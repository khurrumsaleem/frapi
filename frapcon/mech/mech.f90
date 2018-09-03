MODULE FEModel
    USE Kinds
    USE Conversions
    USE common_parameters
    USE FEA_IO, ONLY : fileo, write_output
    USE StaticFEA
    USE FEA_Mesh
    IMPLICIT NONE
    !>@brief
    !> This module contains the driver for the FE cladding deformation model and the temporary storage arrays
    LOGICAL, DIMENSION(:), ALLOCATABLE :: closedt
    LOGICAL, DIMENSION(:), ALLOCATABLE :: stickt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dz0t
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: prt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tempt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: epsefft
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ut
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: epsplt
    !
    CONTAINS
    !
    SUBROUTINE mech (nplast, nrelax, feps, eps, epp, CladEffPlasStrain, CreepStrain, OldCladStrn, &
      &              OldFuelStrn, OldGapPress, OldCoolPress, OldFuelDispl, CladAveTemp, &
      &              OldCladAvTemp, IgapIndexOld, sig, sigeff, reps, rfeps, eppsav, &
      &              repsav, rfpsav)
    USE Kinds
    USE Conversions
    USE pressure1d
    USE data_grid
    USE cladding
    USE m5_cladding
    USE nuclear_fuel
    USE solid1d
    USE quad4
    USE hex8
    USE cont1d
    USE Variables, ONLY : icm, ThermalStrain, epsav, tmpfuel, na, nr, crad, dci, dco, im, &
      &                   ProblemTime, nce, ounit, it, cpl, nt, deltaz, CoolantPress, &
      &                   IgapGapIndex, crephr, FastFluence, tplen, BulkCoolantTemp, &
      &                   CladIrradGrowStrn, CladOutSurfTemp, CladInSurfTemp, GapPress, &
      &                   FuelSurfDispl, FuelCladGap, CladDiamHot, CladInSurDisp, RinterfacPress, &
      &                   dlrod, dlrel
    IMPLICIT NONE
    !>@brief
    !> This is the driver for the FEA cladding deformation model (mechan=1)
    !>@author
    !>
    !
    ! Input
    !
    ! nce               - Number of radial cladding elements
    ! IgapGapIndex      - Gap closure index (0=open / 1=closed)
    ! IgapIndexOld      - Gap closure index of previous power step
    ! nplast            - Flag for creep calculation (1=yes / 0=no)
    ! nrelax            - Flag for plastic calculation (1=yes / 0=no)
    ! frcoef            - Friction coefficient (-)
    ! Deltaz            - Axial node length (ft)
    ! cpl               - Cold plenum length (in)
    ! CladOutSurfTemp   - Cladding outer surface temperature (F)
    ! CladInSurfTemp    - Cladding inner surface temperature (F)
    ! CladIrradGrowStrn - Cladding irradiation growth (in/in)
    ! CoolantPress      - Coolant pressure (Psia)
    ! GapPress          - Gass pressure in the gap (Psia)
    ! FuelSurfDispl     - Fuel surface displacement (in)
    ! dp                - Pellet outer diameter (in)
    ! feps              - FRAPCON3 array for total fuel strains (in/in)
    ! FuelCladGap       - FRAPCON3 array for gap width (in)
    ! CladDiamHot       - Cladding hot diameter (in)
    ! CladInSurDisp     - FRAPCON3 array for cladding inner surface displacement (in)
    ! RinterfacPress    - FRAPCON3 array for contact pressure
    ! dlrod             - change in length of the cladding (in)
    ! dlrel             - relative change of the fuel and cladding (in)
    ! ThermalStrain     - FRAPCON3 array for thermal strains
    ! eps               - FRAPCON3 array for total cladding strains
    ! epp               - FRAPCON3 array for plastic strains
    ! CladEffPlasStrain - FRAPCON3 array for effective plastic strains
    ! CreepStrain       - FRAPCON3 array for effective creep strain
    ! OldCladStrn       - Old cladding strains - previous power step
    ! OldFuelStrn       - Old fuel strains of previous power step
    ! OldGapPress       - rod internal gas pressure of previous power step (psia)
    ! OldCoolPress      - coolant channel pressure of previous power step (psia)
    ! OldFuelDispl      - fuel radial displacement of previous power step (in)
    ! CladAveTemp       - Cladding average temperature (F)
    ! OldCladAvTemp     - Old Cladding average temperature (F)
    ! sig               - FRAPCON3 array for stresses
    ! sigeff            - Cladding effective stress
    ! reps              - 
    ! rfeps             - 
    ! eppsav            - 
    ! epsav             - 
    ! repsav            - 
    ! rfpsav            - 
    ! tplen             - Plenum temperature
    ! BulkCoolantTemp   - Coolant temperature
    ! crephr            - Maximum time step
    ! FastFluence       - Fast fluence (n/m2)
    !
    ! Output
    !
    INTEGER(ipk), INTENT(IN) :: nplast, nrelax
    INTEGER(ipk) :: i, j, label, node, nfound, nr_grid
    REAL(r8k):: h_tot, u_forced_r, u_forced_ax, dl_grwth
    INTEGER(ipk), DIMENSION(na) :: IgapIndexOld
    REAL(r8k), DIMENSION(3) :: x_node
    REAL(r8k), DIMENSION(na) :: sigeff, OldCladAvTemp, CladEffPlasStrain, CreepStrain, OldCladStrn, &
      &                         OldFuelStrn, OldGapPress, OldCoolPress, OldFuelDispl, CladAveTemp
    REAL(r8k), DIMENSION(na,3) :: feps, eps, epp, sig, reps, rfeps, eppsav, repsav, rfpsav
    REAL(r8k), DIMENSION(nr+2,na) :: rad
    TYPE(node_type), POINTER :: current_node
    TYPE(solid1d_type), POINTER :: current_solid1d
    TYPE(cont1d_type), POINTER :: current_cont1d

    ! Initialize variables
    quiet = .TRUE.
    lrupture = .FALSE.
    max_rn = 1.0e-5_r8k

    IF (nrelax == 1) THEN
        CALL temp_storage('U')
    ELSE
        ! Time step
        time_end = ProblemTime(it) * sectohr
        time0 = ProblemTime(it-1) * sectohr
        dtime = time_end - time0
        IF (time_end - time0 < 1.0e-10_r8k) THEN
            time_end = time0 + 1.0e-6_r8k
            dtime = 1.0e-6_r8k
        END IF
        dtime = MIN(dtime, crephr)
        dtime0 = dtime
        dtime_output = 1.0e10_r8k

        ! Initialize finite element model
        IF (nnodes == 0) THEN
            ! Reference temperature
            tref = 293.15_r8k

            ! Use British units
            units = 0

            ! open output files
            CALL fileo()

            ! mesh fuel rod
            CALL mesh_fuel_rod

            ! Create temporary storage
            ALLOCATE(elev(na), ut(2,nnodes), tempt(nnodes), epsplt(3,nsolid1d), &
                     epsefft(nsolid1d), closedt(ncont1d), stickt(ncont1d), &
                     dz0t(ncont1d), prt(npressure1d))

            ! Initialize temporary storage variables
            CALL temp_storage('U')

            ! Output initial geometry
            CALL write_output()

            ! Set input data grid for the temperature input
            nr_grid = nr + 2
            elev(1) = deltaz(1) * 6.0_r8k
            DO i = 2, (na - 1)
                elev(i) = elev(i-1) + (deltaz(i-1) + deltaz(i)) * 6.0_r8k
            END DO
            elev(na) = elev(na-1) + cpl / 2.0_r8k
            DO j = 1, (na - 1)
                DO i = 1, nr
                    rad(i,j) = crad(nr+1-i,j)
                END DO
                rad(nr+1,j) = dci(j) / 2.0_r8k
                rad(nr+2,j) = dco(j) / 2.0_r8k
            END DO
            CALL grid_1d_create(1, na, nr_grid, elev, rad)
        END IF

        ! Change of length in the cladding due to irradiation growth
        h_tot = 0.0_r8k
        DO i = 1, (na - 1)
            node = i * 1000 + 6
            current_node => first_node
            DO WHILE (ASSOCIATED(current_node))
                IF (current_node%label == node) EXIT
                current_node => current_node%next
            END DO
            h_tot = h_tot + (1.0_r8k + CladIrradGrowStrn(i)) * deltaz(i) * fttoin
            dl_grwth = h_tot - current_node%x0(2)
            current_node%x0(2) = h_tot
            current_node%u0(2) = current_node%u0(2) - dl_grwth
        END DO
        node = na * 1000 + 6
        current_node => first_node
        DO WHILE (ASSOCIATED(current_node))
            IF (current_node%label == node) EXIT
            current_node => current_node%next
        END DO
        h_tot = h_tot + cpl
        current_node%x0(2) = h_tot
        current_node%u0(2) = current_node%u0(2) - dl_grwth

        ! Initialize variables from temporary storage
        CALL temp_storage('I')

        ! Pressure boundary conditions
        DO i = 1, (na - 1)
            ! Coolant pressure
            label = i * 1000
            CALL pressure1d_setpress(label,CoolantPress(i))

            ! Rod inner pressure
            label = i * 1000 + 1
            CALL pressure1d_setpress(label,GapPress(i))
        END DO

        ! Plenum pressure
        DO i = na, (na + 1)
            label = i * 1000
            CALL pressure1d_setpress(label,CoolantPress(na-1))
            label = i * 1000 + 1
            CALL pressure1d_setpress(label,GapPress(na-1))
        END DO

        ! Temperature
        DO i = 1, na-1
            DO j = 1, nr
                first_grid_1d%data(j,i) = tfk(tmpfuel(nr+1-j,i))
            END DO
            first_grid_1d%data(nr+1,i) = tfk(CladInSurfTemp(i))
            first_grid_1d%data(nr+2,i) = tfk(CladOutSurfTemp(i))
        END DO
        first_grid_1d%data(1:nr+1,na) = tfk(tplen)
        first_grid_1d%data(nr+2,na) = tfk(BulkCoolantTemp(na-1))
        CALL grid_1d_temp(1)

        ! Forced fuel displacements
        u_forced_ax = 0.0_r8k
        DO i = 1, (na - 1)
            node = i * 1000 + 1
            CALL create_displ(node,0.0_r8k)
            node = i * 1000 + 2
            u_forced_r = FuelSurfDispl(i)
            CALL create_displ(node,u_forced_r)
            node = i * 1000 + 5
            u_forced_ax = u_forced_ax + feps(i,2) * deltaz(i) * fttoin
            CALL create_displ(node,u_forced_ax)
        END DO

        ! Solution for the current time step
        CALL comput_static()
    END IF

    ! Output gap condition
    current_cont1d => first_cont1d
    DO WHILE (ASSOCIATED(current_cont1d))
        i = current_cont1d%label / 1000
        IF (current_cont1d%closed) THEN
            IGapgapIndex(i) = 1
            FuelCladGap(i) = 0.0_r8k
            RinterfacPress(i) = Fint(1,current_cont1d%node_numbers(2)) / (2.0_r8k * pi * &
              &                 x(1,current_cont1d%node_numbers(2)) * Deltaz(i) * fttoin)
        ELSE
            IGapgapIndex(i) = 0
            FuelCladGap(i) = (x(1,current_cont1d%node_numbers(2)) - x(1,current_cont1d%node_numbers(1))) !/fttoin
            RinterfacPress(i) = 0.0_r8k
        END IF
        current_cont1d => current_cont1d%next
    END DO

    ! Compute radially averaged values
    DO i = 1, (na - 1)
        ! Cladding hot diameter
        label = i * 1000 + 3
        x_node = node_coords(label,1)
        CladDiamHot(i) = x_node(1) * 2.0_r8k

        ! Total and relative rod length change
        label = (na - 1) * 1000 + 6
        x_node = node_coords(label,2)
        dlrod = x_node(2) !+ dl_grwth
        label = (na - 1) * 1000 + 5
        x_node = node_coords(label,2)
        dlrel = dlrod - x_node(2)

        ! Element data
        ThermalStrain(i,1:3) = 0.0_r8k
        eps(i,1:3) = 0.0_r8k
        epp(i,1:3) = 0.0_r8k
        CladEffPlasStrain(i) = 0.0_r8k
        sig(i,1:3) = 0.0_r8k
        sigeff(i) = 0.0_r8k
        label = i * 1000 + 2
        current_solid1d => first_solid1d
        nfound = 0
        DO WHILE(ASSOCIATED(current_solid1d))
            IF (current_solid1d%egroup == label) THEN
                nfound = nfound + 1
                ThermalStrain(i,1:3) = ThermalStrain(i,1:3) + current_solid1d%epsth
                eps(i,1:3) = eps(i,1:3) + current_solid1d%epstot((/3,2,1/))
                epp(i,1:3) = epp(i,1:3) + current_solid1d%epspl((/3,2,1/))
                CladEffPlasStrain(i) = CladEffPlasStrain(i) + current_solid1d%epseff
                sig(i,1:3) =  sig(i,1:3) + current_solid1d%sigma((/3,2,1/))
                sigeff(i) = sigeff(i) + current_solid1d%sigeff
            END IF
            IF (nfound == nce) EXIT
            current_solid1d => current_solid1d%next
        END DO
        ThermalStrain(i,1:3) = ThermalStrain(i,1:3) / nce
        eps(i,1:3) = eps(i,1:3) / nce
        epp(i,1:3) = epp(i,1:3) / nce
        CladEffPlasStrain(i) = CladEffPlasStrain(i) / nce
        sig(i,1:3) =  sig(i,1:3) / nce
        sigeff(i) = sigeff(i) / nce
        CreepStrain(i) = CladEffPlasStrain(i)
        OldCladStrn(i) = eps(i,2)
        OldFuelStrn(i) = feps(i,2)
        IgapIndexOld(i) = IgapGapIndex(i)
        OldGapPress(i) = GapPress(i)
        OldCoolPress(i) = CoolantPress(i)
        OldFuelDispl(i) = FuelSurfDispl(i)
        OldCladAvTemp(i) = CladAveTemp(i)
        reps(i,1:3) = eps(i,1:3)
        rfeps(i,1:3) = feps(i,1:3)
    END DO

    DO i = 1, (na - 1)
        eppsav(i,1:3) = epp(i,1:3)
        epsav(i) = CladEffPlasStrain(i)
        repsav(i,1:3) = reps(i,1:3)
        rfpsav(i,1:3) = rfeps(i,1:3)
    END DO

    END SUBROUTINE mech  
    !
    !
    !
    SUBROUTINE temp_storage (ic)
    USE Kinds
    USE Conversions
    USE solid1d
    USE cont1d
    USE pressure1d
    IMPLICIT NONE
    !>@brief
    !> This subroutine performs temporary data storage functions for the FEA model
    !
    CHARACTER(LEN=1), INTENT(IN) :: ic
    TYPE(node_type), POINTER :: cnode
    TYPE(solid1d_type), POINTER :: csolid
    TYPE(cont1d_type), POINTER :: ccont
    TYPE(pressure1d_type), POINTER :: cpress
    INTEGER(ipk) :: i

    IF (ic == 'U') THEN
        i = 0
        cnode => first_node
        DO WHILE (ASSOCIATED(cnode))
            i = i + 1
            ut(1:2,i) = cnode%u0(1:2)
            tempt(i) = cnode%temp0
            cnode => cnode%next
        END DO

        i = 0
        csolid => first_solid1d
        DO WHILE (ASSOCIATED(csolid))
            i = i + 1
            epsplt(1:3,i) = csolid%epspl0(1:3)
            epsefft(i) = csolid%epseff0
            csolid => csolid%next
        END DO

        i = 0
        ccont => first_cont1d
        DO WHILE (ASSOCIATED(ccont))
            i = i + 1
            closedt(i) = ccont%closed0
            stickt(i) = ccont%stick0
            dz0t(i) = ccont%dz0
            ccont => ccont%next
        END DO

        i = 0
        cpress => first_pressure1d
        DO WHILE (ASSOCIATED(cpress))
            i = i + 1
            prt(i) = cpress%p0
            cpress => cpress%next
        END DO

    ELSE

        i = 0
        cnode => first_node
        DO WHILE (ASSOCIATED(cnode))
            i = i + 1
            cnode%u0(1:2) = ut(1:2,i)
            cnode%temp0 = tempt(i)
            cnode => cnode%next
        END DO

        i = 0
        csolid => first_solid1d
        DO WHILE (ASSOCIATED(csolid))
            i = i + 1
            csolid%epspl0(1:3) = epsplt(1:3,i)
            csolid%epseff0 = epsefft(i)
            csolid => csolid%next
        END DO

        i = 0
        ccont => first_cont1d
        DO WHILE (ASSOCIATED(ccont))
            i = i + 1
            ccont%closed0 = closedt(i)
            ccont%stick0 = stickt(i)
            ccont%dz0 = dz0t(i)
            ccont => ccont%next
        END DO

        i = 0
        cpress => first_pressure1d
        DO WHILE (ASSOCIATED(cpress))
            i = i + 1
            cpress%p0 = prt(i)
            cpress => cpress%next
        END DO

    END IF

    END SUBROUTINE temp_storage
    !
END MODULE FEModel