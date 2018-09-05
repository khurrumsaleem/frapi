MODULE MechanicalDeform
    USE Kinds
    USE conversions_frapcon
    USE Deformation
    IMPLICIT NONE
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: prevcladstrn, prevfuelstrn, PrevCladEffPlasStrain
    !
    CONTAINS
    !
    SUBROUTINE fracas
    USE Kinds
    USE conversions_frapcon
    USE FEModel
    USE variables_frapcon
    IMPLICIT NONE
    !
    INTEGER(ipk) :: i, ii, l, nrelax, m1, m2, m3, m4, m5, m6, kflag, n1, n22, n3, n4, jjj
    REAL(r8k) :: deltime, atime, zz, urfold, dlrod2, dlrel2
    REAL(r8k), PARAMETER :: bb = 0.5_r8k
    !
    ! Setting up data for fracas (fuel rod and cladding analysis subcode)
    !
    nrelax = 0
    deltime = delh
    atime = ProblemTime(it-1) * sectohr
    zz = deltaz(j-1) * fttoin
    IF (nab == 1) THEN
        nab = 2
        IF (.NOT. ALLOCATED(prevcladstrn)) ALLOCATE(prevcladstrn(1:na))
        IF (.NOT. ALLOCATED(prevfuelstrn)) ALLOCATE(prevfuelstrn(1:na))
        IF (.NOT. ALLOCATED(PrevCladEffPlasStrain)) ALLOCATE(PrevCladEffPlasStrain(1:na))
        prevcladstrn = 0.0_r8k
        prevfuelstrn = 0.0_r8k
        PrevCladEffPlasStrain = 0.0_r8k
        gpthe(1) = cdg(j-1) / 1000.0_r8k
        AxialNodLength(1:nt) = deltaz(1:nt) * fttoin 
        FuelCladGap(1:na) = cdg(j-1) / 2000.0_r8k
    END IF
    ! This was modified by IP 8/13/15
    ! It was determined that HotNodLength was not being calculated before being used for axial nodes > j-1
    ! during the first timestep and for the following timesteps, it is truly using HotNodLength from the previous
    ! timestep because volume (where HotNodLength is calculated) is not called until outside of the
    ! gap convergence loop
    HotNodLength(j-1) = 0.0_r8k
    DO l = 1, nrm1
        hotringl(l,j-1) = coldringl(l,j-1) * (1.0_r8k + uo2exp(l,j-1) + dpw(l,j-1) + densf(l,j-1))
        HotNodLength(j-1) = MAX(HotNodLength(j-1), hotringl(l,j-1))
    END DO
    !
    SELECT CASE (mechan)
    CASE(1)
        DO i = 1, nt
            PrevCladEffPlasStrn(i) = CladEffPlasStrain(i)
            GapPress(i) = press
            ! If the user supplied coolant pressures at each axial node, update CoolantPress here for each axial node
            IF (ifixedcoolp == 1) THEN
                CoolantPress(i) = CoolantPressure(it,i)
            ELSE ! Constant coolant pressure at all axial nodes
                CoolantPress(i) = p2(it)
            END IF
            PrevCladStrain(i) = OldCladStrn(i)
            PrevFuelStrain(i) = OldFuelStrn(i)
            PrevOldCladAvTemp(i) = OldCladAvTemp(i)
            PrevOldCoolPress(i) = OldCoolPress(i)
            PrevOldGapPress(i) = OldGapPress(i)
            PrevOldFuelDispl(i) = OldFuelDispl(i)
            IgapIndexPrevOld(i) = IgapIndexOld(i)
            DO ii = 1, 3
                eppp(i,ii) = epp(i,ii)
                rfepp(i,ii) = rfeps(i,ii)
                repsp(i,ii) = reps(i,ii)
            END DO
        END DO
        urfold = FuelSurfDispl(j-1)
        FuelSurfDispl(j-1) = hrad(1,j-1) - rp
        FuelSurfDispl(j-1) = bb * FuelSurfDispl(j-1) + (1.0_r8k - bb) * urfold
        feps(j-1,1) = FuelSurfDispl(j-1) / rp
        feps(j-1,2) = (HotNodLength(j-1) - zz) / zz
        AxialNodLength(j-1) = zz * (1.0_r8k + CladIrradGrowStrn(j-1))
        nplast = 1
        CALL mech (nplast, nrelax, feps, eps, eppp, PrevCladEffPlasStrain, PrevCreepStrain, PrevCladStrn, &
          &        PrevFuelStrn, PrevOldGapPress, PrevOldCoolPress, PrevOldFuelDispl, CladAveTemp, &
          &        PrevOldCladAvTemp, IgapIndexPrevOld, sig, sigeff, reps, rfeps, eppsv, repsv, rfpsv)
    CASE (2) ! FRACAS-1
        DO i = 1, nt
            PrevCladEffPlasStrn(i) = CladEffPlasStrain(i)
            GapPress(i) = press
            ! If the user supplied coolant pressures at each axial node, update CoolantPress here for each axial node
            IF (ifixedcoolp == 1) THEN
                CoolantPress(i) = CoolantPressure(it,i)
            ELSE ! Constant coolant pressure at all axial nodes
                CoolantPress(i) = p2(it)
            END IF
            PrevCladStrain(i) = OldCladStrn(i)
            PrevFuelStrain(i) = OldFuelStrn(i)
            PrevOldCladAvTemp(i) = OldCladAvTemp(i)
            PrevOldCoolPress(i) = OldCoolPress(i)
            PrevOldGapPress(i) = OldGapPress(i)
            PrevOldFuelDispl(i) = OldFuelDispl(i)
            IgapIndexPrevOld(i) = IgapIndexOld(i)
            DO ii = 1, 3
                eppp(i,ii) = epp(i,ii)
                rfepp(i,ii) = rfeps(i,ii)
                repsp(i,ii) = reps(i,ii)
            END DO
        END DO
        urfold = FuelSurfDispl(j-1)
        FuelSurfDispl(j-1) = hrad(1,j-1) - rp
        FuelSurfDispl(j-1) = bb * FuelSurfDispl(j-1) + (1.0_r8k - bb) * urfold
        feps(j-1,1) = FuelSurfDispl(j-1) / rp
        feps(j-1,2) = (HotNodLength(j-1) - zz) / zz
        AxialNodLength(j-1) = zz * (1.0_r8k + CladIrradGrowStrn(j-1))
        nplast = 1
        ! The following call to fcmi results in cladding mechanical calculations using the Fracas-1 subcode    
        CALL fcmi (IgapGapIndex, eps, eppp, sig, repsp, rfepp, feps, dlrod, dlrel, CreepStrain1, &
          &        PrevCladStrain, PrevFuelStrain, PrevOldCladAvTemp, PrevOldGapPress, PrevOldCoolPress, &
          &        PrevOldFuelDispl, IgapIndexPrevOld, eppsv, deltime, atime, PrevCreepStrain, repsv, &
          &        rfpsv, epp1, CladH2Concen, ExcessH2Concen, UniformAxNodStrn, iquit, j-1, it, nplast, nrelax)
    END SELECT
    !
    END SUBROUTINE fracas
    !
END MODULE MechanicalDeform

