module frapcon4

    use Kinds_frapcon
    use variables_frapcon
    use Decay_frapcon
    use Refabrication_frapcon
    use DatingData_frapcon
    use SetupProblem_frapcon, only : setup, axhef
    use conversions_frapcon
    use RunProperties_frapcon
    use FEA_Setup_frapcon
    use Comde_frapcon
    use Initialization_frapcon, only : check, ResetTimesteps
    use Gas_frapcon, only : Allocate_Gas, ngases
    use GadRadPower_frapcon, only : LoadGadProperties
    use TimeStep_frapcon
    USE CoolantData_frapcon
    USE CorrosionData_frapcon
    USE Output_Data_frapcon
    USE PlotFile_frapcon
    USE Material_Properties_frapcon
    USE FissionGas_frapcon
    USE Restart_frapcon
    USE Burn_frapcon
    USE MechanicalDeform_frapcon
    USE Plenum_frapcon
    USE void_frapcon
    USE FEA_IO_frapcon, ONLY : write_output
    USE CladDeformation_frapcon
    USE FuelDeformation_frapcon
    USE Temperature_frapcon
    USE ZrModels_frapcon
    USE CladCreep_frapcon
    USE Functions_frapcon, ONLY : terp
    USE Developer_frapcon
    USE SpentFuel_frapcon
    USE FileIO_frapcon, ONLY : Namelist_Read_Error

    implicit none

    logical :: DoFrapconAllocation = .true.
    character (len=200), target :: namerf
    logical, target :: is_kernel_allocated = .false.

    type, public :: frapcon_driver

        include "fc_k_variables_h.f90" ! kernel variables
        include "fc_r_variables_h.f90" ! working driver variables
        include "fc_b_variables_h.f90" ! backup driver variables

        logical :: DoRodRefabrication = .false.
        logical :: Verbose = .false.
        logical :: is_driver_allocated = .false.
        logical, pointer :: is_kernel_allocated

    contains

        procedure :: make => driver_make
        procedure :: next => driver_next
        procedure :: init => driver_init
        procedure :: deft => driver_deft
        procedure :: next0=> driver_next0
        procedure :: copy_r2k => driver_copy_r2k
        procedure :: copy_k2r => driver_copy_k2r
        procedure :: copy_r2b => driver_copy_r2b
        procedure :: copy_b2r => driver_copy_b2r
        procedure :: copy_r2f => driver_copy_r2f
        procedure :: copy_f2r => driver_copy_f2r
        procedure :: destroy => driver_destroy
        procedure :: restfs => driver_restfs

    end type frapcon_driver

contains

    subroutine driver_make(this, na_, ngasr_, nr_, nce_, verbose_)

        class (frapcon_driver), intent(inout) :: this

        logical :: verbose_
        INTEGER(ipk) :: na_, ngasr_, nr_, nce_


        this % verbose = verbose_ ! Print output data

        im      = 1        ! Number of time steps
        na      = na_      ! Number of axial nodes
        ngasr   = ngasr_   ! Number of radial gas release nodes (equal-volume) in the pellet for FGR calculations
        nr      = nr_      ! Number of radial nodes in the pellet for thermal calculations
        nce     = nce_     ! Number of radial elements in the cladding for the FEA model

        it      = 1

        ivardm = 1
        nt = na
        na = na + 1
        nab = 2
        IF (naxim==0) naxim = na*im

        ! The following call sets the pointers and allocates the time-dependent and axial arrays. Coded by IP 1/6/2014
        if (DoFrapconAllocation) then
            CALL Allocate_Time_Arrays
            CALL Allocate_Axial_Arrays
            CALL Allocate_Radial_Arrays
            CALL Allocate_2D_Arrays
            CALL Allocate_3D_Arrays
            CALL Allocate_Comde
            CALL Allocate_Gas
            CALL Allocate_DatingData
            call ALLOCATE_FGR_Variables
            DoFrapconAllocation = .false.
        endif

        include 'fc_k_associate_h.f90'
        include 'fc_r_allocate_h.f90'
        include 'fc_b_allocate_h.f90'

        is_kernel_allocated = .true.
        this % is_driver_allocated = .true.
        this % is_kernel_allocated => is_kernel_allocated

        ! WTF ???
!        if (.not. this % verbose) open(ounit, file='~frapcon.temp', status='unknown', form='formatted')

    end subroutine driver_make

    subroutine driver_copy_r2f(this, filename)

        class (frapcon_driver), intent(inout) :: this

        character(*) :: filename

        integer :: ifile = 142

        open(ifile, file=filename, status='unknown', form='unformatted')

        include 'fc_copy_r2f_h.f90'

        close(ifile)

    end subroutine driver_copy_r2f

    subroutine driver_copy_f2r(this, filename)

        class (frapcon_driver), intent(inout) :: this

        character(*) :: filename

        integer :: ifile = 142

        open(ifile, file=filename, status='unknown', form='unformatted')

        include 'fc_copy_f2r_h.f90'

        close(ifile)

    end subroutine driver_copy_f2r

    subroutine driver_copy_k2r(this)

        class (frapcon_driver), intent(inout) :: this

        include 'fc_copy_k2r_h.f90'

    end subroutine driver_copy_k2r

    subroutine driver_copy_r2k(this)

        class (frapcon_driver), intent(inout) :: this

        include 'fc_copy_r2k_h.f90'

    end subroutine driver_copy_r2k

    subroutine driver_copy_r2b(this)

        class (frapcon_driver), intent(inout) :: this

        include 'fc_copy_r2b_h.f90'

    end subroutine driver_copy_r2b

    subroutine driver_copy_b2r(this)

        class (frapcon_driver), intent(inout) :: this

        include 'fc_copy_b2r_h.f90'

    end subroutine driver_copy_b2r


    subroutine driver_deft(this)

        implicit none

        class (frapcon_driver), intent(inout) :: this

        call Set_Defaults

        include "fc_k_default_h.f90"

    end subroutine driver_deft


    subroutine driver_init(this)

        implicit none

        class (frapcon_driver), intent(inout) :: this

        INTEGER(ipk) :: jj

        im = 1

        jn(:)  = na         ! # of qf, x pairs for each axial power shape

        ProblemTime(0) = 1.d-6 ! <= in order to pass the check routine
        ProblemTime(1) = 2.d-6 !

        ! The following call to inital initializes variables based on input
        call init_check
        ! If modeling gadolinia, intialize variables in GadRadPower
        IF (MAXVAL(gadoln) > 0.0_r8k) CALL LoadGadProperties
        ! Initialization of FE model
        CALL default_values()
        CALL init()
        IF (iquit == 1) stop
        ! The following call to axhef computes the axial power profile
        CALL axhef (qf,x)

        ! Check to see if using Decay Heating during shutdown
        IF (MINVAL(qmpy) < 0.0_r8k) CALL DecayHeatSetup

        TimeIntegration = 0 ! Specify time integration technique (0 = None, 1 = Linear Interpolation, 2 = Histogram)

        dcoBOL = dco(1)
        jfix = jpeak(1)
        itt = 1
        transt = 0.8_r8k * tsint
        frden = den * 0.01_r8k
        jmin = 2
        jmax = ir1

        ! New Problem
        cwkf = cldwks
        coldwk = cldwks
        rstran = 1.0e-05_r8k
        nab = 1
        IF (it > 0) THEN
            m = jst(it)
        ELSE
            m = 1
        END IF
        DO jj = 1, ir1 - 1
            BOSNodeburnup(jj) = buin(jj) * qaxnorm(jj,m) * fa
        END DO

        im = 2
        it = 1

        if ( sum(deltaz) <= 0 ) then
            write(*,*) "ERROR: sum of axial mesh thickness is equal to ", sum(deltaz) / cmtoft
            stop
        endif

    end subroutine driver_init

    subroutine driver_next0(this)

        class (frapcon_driver), intent(in) :: this

        real(r8k) :: d_time_init = 0.001_r8k/daytosec

        it = 1

        call print1()

        call axhef(qf,x)

        ProblemTime(0) = 0.0_r8k

        call this % next(d_time_init)

        it = 2

    end subroutine driver_next0

    subroutine driver_next(this, d_time)

        class (frapcon_driver), intent(in) :: this

        real(r8k) :: d_time

        INTEGER(ipk) :: jj, ii, i, ntime, mtest, nam1, l, ij, m3, ir1m3, ir1m33, ijj, nalfdt, m1, m2
        REAL(r8k) :: wpcm3, wpgm, gaslas, deh, gratio, cinwid, cinrad, cyldst, caxrad, caxstr, deltmp, &
          &          strnyt, strnye, strnie, strnue, stsrpt, strrpe, cyldse, cultse, cbrste, cbrsst, &
          &          ctstrt, cgap, cruf, gap, tcak, u, cdpres, fuelTD, dummy

        ! in order to make a time step with zero power
        if (this % qmpy(it) < 1.D-10) this % qmpy = 1.D-10

        CALL axhef (qf,x)

        ProblemTime(it) = ProblemTime(it-1) + d_time * daytosec

        IF (ProblemTime(it) >= stopox) calcoxide = .FALSE.
        !
        ! Check to see if the rod has been refabricated during this timestep
        !
        !IF (it == irefab + 1) CALL RodRefabrication
        if (this % DoRodRefabrication) call RodRefabrication
        !
        ! Check to see if this is a decay power step
        ! it must be revised due to the usage of the previous time steps from the ProblemTime array
        ! IF (qmpy(it) < 0.0_r8k) CALL DecayHeat
        !
        ! Determine current peak axial node indicator "jpeak(1)"
        !mtest = jst(it)
        !jpeak(1) = jpeak(mtest)
        !IF (mtest == 1) jpeak(1) = jfix
        gasflg = .FALSE.
        m = jst(it)
        qav = (qmpy(it) * BTUhtokW) * (dcoBOL * intoft) * pi / fa
        qpeak = qaxnorm(jpeak(1)-1,m) * qav * fa
        ! Set the time values
        delhs = ProblemTime(it) - ProblemTime(it-1)
        delh = delhs * sectohr
        t = ProblemTime(it) * sectohr
        ! set up the flux arrays
        ! compute the fluence array also
        ! FastFlux() units - n/m**2*sec
        ! flux units - (n/m**2*sec)/(w/gram uo2)
        ! FastFluence units - n/m**2
        nam1 = na - 1
        avflux = 0.0_r8k
        fuelTD = MatProp ('FUEL', 'TDENSITY', dummy) ! Theoretical density (g/cm3)
        DO j = 1, (na - 1)
            ! Fuel LHGR (kW/ft)
            Power(j) = (qmpy(it) * qaxnorm(j,m) * BTuhtokW) * (dcoBOL * intoft) * pi
            ! Power Density (Watts / cm^3 fuel) = LHGR / Fuel cross sectional area, Pi * (outer radius^2 - inner radius^2)
            wpcm3 = (Power(j) / ftocm * 1000.0_r8k) / (pi * (((dp(j) / 2.0_r8k * intocm) ** 2) - ((rc(j) * intocm) ** 2)))
            ! Power Density (Watts / gram fuel)
            wpgm = wpcm3 / (frden * fuelTD)
            ! Flux adjusted by (1/(1-modheat)) to ensure entire flux value is used
            FastFlux(j) = wpgm * flux(j) * (1.0_r8k / (1.0_r8k - modheat))
            FastFluence(j) = FastFluence(j) + FastFlux(j) * delhs
            avflux = avflux + FastFlux(j)
        END DO
        avflux = avflux / REAL(nam1)
        
!        IF (it /= 1 .OR. nread /= 0) THEN
!            !
!            ! Compute effective fluences and effective coldworks
!            !
!            ctemp = tfk(taca(0))
!            rtemp = 0.0_r8k
!            rtemp = (ctemp - tfk(taca(1))) / delhs
!            !
!            CALL caneal
!            !
!            ! Retain converged information needed for the next power-time step
!            !
!            ! Modified by IP to bypass this subroutine during the first FRAPCON loop after reading a restart file.
!            IF (nread /= 1 ) CALL store
!            !
!        END IF

        if (it > 1) then
            ! Compute effective fluences and effective coldworks
            ctemp = tfk(taca(0))
            rtemp = 0.0_r8k
            rtemp = (ctemp - tfk(taca(1))) / delhs
            call caneal
            ! Retain converged information needed for the next power-time step
            ! Modified by IP to bypass this subroutine during the first FRAPCON loop after reading a restart file.
            call store
        END IF



        !
        ! ********************************************************************
        ! *****     START OF GAS RELEASE LOOP, iter = 1, CONVERGENCE     *****
        ! ********************************************************************
        !
        ! Start at first iteration (iter = 1)
        iter = 1
        DO WHILE (.NOT. gasflg)
            !
            ! Check for gas convergence if more than 1 iteration has been made
            !
            IF (iter > 2) THEN
                IF (((ABS(pit(it-1) - gaslas)) / gaslas) <= 0.01_r8k) gasflg = .TRUE.
                IF (iter > 10 .AND. (ABS(pit(it-1) - gaslas) / gaslas) <= 0.05_r8k) gasflg = .TRUE.
                IF (iter == 20) gasflg = .TRUE.
                !IF (iter > 15) WRITE (ounit,820) iter, gaslas, pit(it-1)
            END IF
            gaslas = pit(it-1)
            !
            ! ********************************************************************
            ! *****           START OF AXIAL NODE LOOP, j = 2, na            *****
            ! ********************************************************************
            !
            ! Set the lower axial node bound
            j = jmin
            DO WHILE (j <= jmax)
                qc(j-1) = qmpy(it) * qaxnorm(j-1,m)
                rci = dci(j-1) / 2.0_r8k
                rco = dco(j-1) / 2.0_r8k
                rp = dp(j-1) / 2.0_r8k
                tcc = rco - rci
                deh = de(j-1) * intoft
                cdg(j-1) = 1.0E3_r8k * 2.0_r8k * (rci - rp)
                gratio = (rci - rp) / rp
                ! Update the coolant pressure if ifixedcoolp = 1
                IF (ifixedcoolp == 1) p2(it) = CoolantPressure(it, j-1)
                ! The following call to burnup computes the fuel burnup
                CALL burnup
                !
                ! The following call to turbo calulates the radial form factor
                ! and radial burnup. It also updates the concentration arrays.
                qnode(j-1) = qav * qaxnorm(j-1,m)

                CALL turbo
                !
                ! Update the physical properties of the fuel and cladding (i.e. Fuel Tmelt, Clad Tmelt)
                ! based on the current burnup
                ! Fuel
                ftmelt = MatProp ('FUEL', 'TMELT', dummy)
                ! Cladding
                ctmelt = MatProp ('CLAD', 'TMELT', dummy)
                !
                ! The following call to gaspro computes the fission gas production
                !
                CALL gaspro
                !
                ! Compute the coolant properties (bulk coolant temperature & film/crud temperature rise)
                !
                CALL Coolant
                !
                ! Compute the corrosion thickness, hydrogen uptake in the cladding and tempeature drop across the oxide layer
                !
                CALL Corrosion
                !
                IF (iter == 1) THEN
                    !
                    ! Calculate effect of H2 concentration on uniform strain of cladding.
                    !
                    ! Provide duumy arguments to cmlimt for arguments that do not influence uniform strain.
                    cinwid = 6.0e-4_r8k
                    cinrad = 5.0e-3_r8k
                    cdpres = 1.0e6_r8k
                    caxrad = 10.0_r8k
                    caxstr = 0.0_r8k
                    deltmp = 0.0_r8k
                    !
                    CALL cmlimt (cinwid, cinrad, cdpres, caxrad, caxstr, deltmp, strnyt, strnye, strnue, &
                      &          strnie, stsrpt, strrpe, cyldst, cyldse, cultse, cbrste, cbrsst, ctstrt)
                    !
                    UniformAxNodStrn(j-1) = strnue
                    !
                END IF
                !
                ! Compute the cladding outside, inside and average surface temperatures.
                !
                CALL cladrp
                !
                ! Compute the radial and axial thermal expansion of the cladding
                !
                CALL cexpan
                !
                ! Compute the cladding irradiation growth
                !
                IF (iter == 1) THEN
                    CALL cldgro
                    IF (it == 1) StepNodeburnup(j-1) = 0.0_r8k
                    HotThermalGap(j-1) = gpth(1)
                    StepNodeburnup(j-1) = delbp
                END IF
                CladOutSurfTemp(j-1) = tco
                CladInSurfTemp(j-1) = tci
                GapPress(1:na) = press
                CoolantPress(1:na) = p2(it)
                nconvg = .FALSE.
                ncont = .FALSE.
                ! fuel-cladding contact criteria is based on fuel & cladding roughness
                cruf = 2.0_r8k * (roughc + roughf)
                gapmin = MAX(cruf, 5.0e-5_r8k)
                !
                ! Compute the Fuel Relocation
                !
                CALL gtrloc
                !
                IF (it > 1 .OR. iter > 1) THEN
                    dltgc(1) = EstGapDeltaT(j-1)
                    gpthe(1) = gpthpg(j)
                ELSE
                    dltgc(1) = qc(j-1) / 500.0_r8k
                END IF
                !
                ! ********************************************************************
                ! * ITERATION ON GAP AND PELLET TEMPERATURE DISTRIBUTION BEGINS HERE *
                ! ********************************************************************
                k = 1
                DO WHILE (.NOT. nconvg .AND. .NOT. ncont)
                    !
                    ! Calculate the fuel temperature distribution
                    !
                    CALL tmpsub
                    !
                    ! Compute the fuel thermal expansion
                    !
                    CALL fexpan
                    !
                    ! Compute fuel swelling and densification
                    ! Note that 1/2 the relocation strain (rlcstrn) is added to the pellet radius as extra swelling.
                    !
                    CALL swell
                    !
                    ! Calculate the cladding elastic deformation
                    !
                    nplast = 0
                    !
                    CALL fracas
                    !
                    ! Estimate the gap thickness
                    !
                    ! for Fracas-1, the initial diameteral gap thickness estimate is:
                    gpth(k) = (FuelCladGap(j-1) - Relocation(j-1) * gaprecov) * 2.0_r8k
                    dphfrl = dphf + Relocation(j-1) * gaprecov * 2.0_r8k
                    ! In the above, only 1/2 the relocation is used, since the other
                    ! 1/2 is added to pellet radius as extra swelling in Subroutine swell
                    IF (gpth(k) <= gapmin) THEN
                        gpth(k) = gapmin
                        dphfrl = dci(j-1) * (1.0_r8k + eps(j-1,1)) - gapmin
                    END IF
                    !
                    ! Calculate the new thermal gap
                    !
                    CALL newgap
                    !
                    ! Check for convergence
                    !
                    IF (.NOT. nconvg .AND. .NOT. ncont) THEN
                        !
                        gapt = gpthe(k) * intom / 2.0_r8k
                        hsolid = 0.0_r8k
                        !
                        ! Calculate the gap conductance and the gap temperature drop
                        !
                        CALL conduc
                        !
                    END IF
                    !
                    ! ********************************************************************
                    ! *****               END OF TEMPERATURE ITERATION               *****
                    ! ********************************************************************
                    !
                END DO
                IF (ncont .AND. .NOT. nconvg) THEN
                    !WRITE (ounit,800) j-1, it-1, iter, dltgc(k-1), dltgc(k-2)
                    !WRITE (ounit,810) FuelCladGap(j-1), RinterfacPress(j-1)
                END IF
                Relocation(j-1) = MAX(Relocation(j-1), 0.0_r8k)
                ! New way (IP - use value from k instead of k_frapcon-1)
                EstGapDeltaT(j-1) = dltgc(MIN(k,30))
                gpthpg(j) = gpthe(MIN(k,30))
                HotThermalGap(j-1) = gpthe(MIN(k,30))
                GapAveTemp(j-1) = tci + 0.5_r8k * dltgc(MIN(k,30))
                CladDiamHot(j-1) = 0.5_r8k * (dco(j-1) + dci(j-1)) * eps(j-1,1) - &
                  &                0.5_r8k * (dco(j-1) - dci(j-1)) * eps(j-1,3) + dci(j-1)
                tmpfuel(1:nr,j-1) = tfuelr(1:nr)
                GapCond(j-1) = hgapt
                !
                ! Set peak values
                !
                IF (j == jpeak(1)) CALL SetPeakPowerValues ('AxialLoop')
                !
                ! Compute the void volumes
                !
                CALL volume
                !
                ! Compute gas release
                !
                CALL fgasre
                !
                ! Compute the fuel stored energy
                !
                CALL energy
                !
                ! Convert to british units
                !
                StoredEnergy(j-1) = StoredEnergy(j-1) * JkgtoBTUlb
                TotalHgap(j-1) = hgapt
                SolidHgap(j-1) = hsolid
                GasHgap(j-1) = hgap
                RadHgap(j-1) = hgapr
                IF (iquit == 1) RETURN
                tcak = tfk(CladAveTemp(j-1))
                ctmax(j) = MAX(ctmax(j), tcak)
                j = j + 1
                ! ********************************************************************
                ! *****                 END OF AXIAL NODE LOOP                   *****
                ! ********************************************************************
            END DO
            !
            ! Compute the total gas in the rod
            !
            CALL totgas
            !
            ! The following statement eliminates cladding plastic deformation when
            !  the cladding dimensional change switch is specified.
            !
            nplast = 1
            !
            ! Compute the plenum temperature and hot state plenum volume
            !
            CALL plnt
            !
            ! Compute the rod internal pressure
            !
            CALL gspres
            !
            IF (igascal == 0) press = p1(it)
            !
            ! If the refabrication timestep is currently being calculated, update the gas composition with the refabricated gases.
            !
            !IF (it == irefab + 1) CALL UpdateRefabGases
            if (this % DoRodrefabrication) call UpdateRefabGases
            !
            iter = iter + 1
        END DO
        !
        ! ********************************************************************
        ! *****                END OF GAS RELEASE LOOP                   *****
        ! ********************************************************************
        !
        ! Compute the cladding creep
        !
        CALL ccreep
        !
        ! ********************************************************************
        ! *****                      OUTPUT DATA                         *****
        ! ********************************************************************
        !
        ! gasplt outputs a plot and table describing the release of short-live
        ! radioactive gases.  The information is calculated by ans54.
        !IF (ngasr == 10 .AND. ProblemTime(it) > oneday) CALL gasplt
        IF (ngasr == 10 .AND. ProblemTime(it) > oneday) CALL gasplt
        !
        ! Set peak values
        !
        CALL SetPeakPowerValues ('TimestepLoop')
        !
        ! Provide the axial node output. Check to see if need to output this timestep
        !
        IF (nopt == 0 .AND. gasflg) THEN
            DO ij = jmin, jmax + 1
                j = ij
                PrintType = 'Axial Loop'
                if (this % verbose) CALL print2
            END DO
        END IF
        !
        ! This is the FRAPCON to TRACE Data.
        !
        IF (nfrttr == 1) CALL fraptotrace
        !
        ! The following call to grafout stores the plot information for use with the Excel plot package Aplotter_frapcon.xls.
        !
        !IF (nplot >= 1 .AND. it > 1) THEN
            !CALL grafout
            !IF (mechan == 1) CALL write_output()
        !END IF
        !
        ! Write the FRAPTRAN restart information
        !
        IF (ntape > 0) THEN
            DO ij = 1, nt
                u = 0.0_r8k
                DO i = 2, nr
                    ii = nr - i + 2
                    ijj = nr * ij + ii - 2
                    u = u + (dpw(ii-1,ij) + densf(ii-1,ij)) * (crad(ii-1,ij) - crad(ii,ij)) * intoft
                END DO
                FuelPorosity(ij) = PorosityVolume(ij) / FuelVolume(ij)
                PermFuelDispl(ij) = u
            END DO
            CALL restfs
        END IF
        !
        ! Write the FRAPCON restart information
        ! This is the new FRAPCON restart file. Still under construction by Ian Porter
        !
        !IF (nrestr > 0) CALL WriteRestart
        !
        IF (idatingcreep > 0) CALL DatingDriver

        ! Copy values from the last time step to the first one
        qend(1)                   = qend(2)                 
        avgqi(1)                  = avgqi(2)                
        go(1)                     = go(2)                   
        p2(1)                     = p2(2)                   
        qmpy(1)                   = qmpy(2)                 
        tw(1)                     = tw(2)                   
        p1(1)                     = p1(2)                   
        jn(1)                     = jn(2)                   
        jnsurftemp(1)             = jnsurftemp(2)           
        jpeak(1)                  = jpeak(2)                
        jst(1)                    = jst(2)                  
        jstsurftemp(1)            = jstsurftemp(2)          
        acmfg(1)                  = acmfg(2)                
        acmhe(1)                  = acmhe(2)                
        acmH2(1)                  = acmH2(2)                
        acmH2O(1)                 = acmH2O(2)               
        acmn2(1)                  = acmn2(2)                
        amgpt(1)                  = amgpt(2)                
        gasmo(0)                  = gasmo(1)                
        hmgpt(1)                  = hmgpt(2)                
        tafa(1)                   = tafa(2)                 
        taga(1)                   = taga(2)                 
        tsfa(1)                   = tsfa(2)                 
        taca(0)                   = taca(1)                 
        pkODCladTemp(0)           = pkODCladTemp(1)         
        pkPelAveTemp(0)           = pkPelAveTemp(1)         
        pkPower(0)                = pkPower(1)              
        pkAveCladTemp(0)          = pkAveCladTemp(1)        
        pkIDCladTemp(0)           = pkIDCladTemp(1)         
        pkGap(0)                  = pkGap(1)                
        pkFisGasRelFrac(0)        = pkFisGasRelFrac(1)      
        pkPelSurfTemp(0)          = pkPelSurfTemp(1)        
        pkH2up(0)                 = pkH2up(1)               
        pkPelCentTemp(0)          = pkPelCentTemp(1)        
        pkIntefacePres(0)         = pkIntefacePres(1)       
        pit(0)                    = pit(1)                  
        pkHoopStres(0)            = pkHoopStres(1)          
        pkAxlStres(0)             = pkAxlStres(1)           
        pkBurnup(0)               = pkBurnup(1)             
        pkHoopStrain(0)           = pkHoopStrain(1)         
        pkFuelPelOD(0)            = pkFuelPelOD(1)          
        pkGapCond(0)              = pkGapCond(1)            
        pkZrO2(0)                 = pkZrO2(1)               
        HeatFlux(1)               = HeatFlux(2)             
        buavearray(1)             = buavearray(2)           
        voidvolarray(1)           = voidvolarray(2)         
        he_ifba(1)                = he_ifba(2)              
        addgmles(1)               = addgmles(2)             
        addswell(1)               = addswell(2)             
        CycleSteps(1)             = CycleSteps(2)           
        !tcoolant(1:na)            = tcoolant(na:2*na)
        !pcoolant(1:na)            = pcoolant(na:2*na)
        !x(1:na)                   = x(na:2*na)
        !qf(1:na)                  = qf(na:2*na)
        !xt(1:na)                  = xt(na:2*na)
        !cladt(1:na)               = cladt(na:2*na)
        coolanttemp(1,1:na+1)     = coolanttemp(2,1:na+1)   
        coolantpressure(1,1:na+1) = coolantpressure(2,1:na+1)
        storedearray(1,1:na)      = storedearray(2,1:na)    
        cltemparray(1,1:na)       = cltemparray(2,1:na)     
        buarray(1,1:na)           = buarray(2,1:na)         
        strainarray(1,1:na)       = strainarray(2,1:na)     
        straindiffarray(1,1:na)   = straindiffarray(2,1:na) 
        dpwxarray(1,1:na)         = dpwxarray(2,1:na)       
        creaparray(1,1:na)        = creaparray(2,1:na)      
        dumarray3(1,1:na)         = dumarray3(2,1:na)       
        cladtarray(1,1:na+1)      = cladtarray(2,1:na+1)    
        
        RB_rod(15,1)              = RB_rod(15,2)            
        prdct(1:ngasr,1:na,1)     = prdct(1:ngasr,1:na,2)   
        ansd(1:ngasr,1:na,1)      = ansd(1:ngasr,1:na,2)    

        pfU235 (1:na-1,0)         = pfU235 (1:na-1,1)      
        pfU238 (1:na-1,0)         = pfU238 (1:na-1,1)      
        pfPu239(1:na-1,0)         = pfPu239(1:na-1,1)       
        pfPu240(1:na-1,0)         = pfPu240(1:na-1,1)       
        pfPu241(1:na-1,0)         = pfPu241(1:na-1,1)       
        pfPu242(1:na-1,0)         = pfPu242(1:na-1,1) 

        ProblemTime(it-1)         = ProblemTime(it)

    end subroutine driver_next

    subroutine driver_destroy(this)

        class (frapcon_driver), intent(inout) :: this

        if (this % is_driver_allocated) then
            include "fc_r_deallocate_h.f90"
            include "fc_b_deallocate_h.f90"
            this % is_driver_allocated = .false.
        endif

        !if (.not. this % verbose) close(ounit, status='delete')

    end subroutine driver_destroy

    SUBROUTINE init_check

    IMPLICIT NONE
    !> @brief
    !> Subroutine inital declares the default values before the input
    !> file is read, then reads the input blocks $frpcon & $frpmox.
    !> It then increases the user-defined time-dependent values by 1 timestep
    !> to allow for FRAPCON's 'initial' timestep calculation of 0.001 days.
    !
    INTEGER(ipk) :: oligp2, ifixtedtsurf, ncoolanthist, npairs, ncoolz, i, ii, mt, kz, kt, &
      &             nap1, icreep, jnsurftemp_max, numshapes, jj, nstart, ij, l, InputStat = 0
    REAL(r8k) :: rchmfr, rcrack, axheight, beta, dishsg, u, gden, hi, qmpy_tmp, dummy, cmol, &
      &          GasDensity, MWtMix, vs, dspg, dspgw
    CHARACTER(LEN=100) :: errorline
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dumarray2, dumarray1

    jnsurftemp_max      = 0                           !
    beta                = 1.d0                        ! it is not used 
    nap1                = na + 1                      !

    vs = Spring%vs
    dspgw = Spring%dspgw
    dspg = Spring%dspg

    qmpy(1) = qmpy(1) / kWtoBTUh * intoft * pi * totl / sum(deltaz(1:nt)/dco(1:nt)) ! kW/ft

    ! Check to see if $SpentFuel exists
    !CALL SpentFuelRead
    IF (IsModelingSpentFuel) THEN
        !CALL Allocate_DatingData
        CALL SpentFuelErrorCheck
    ENDIF
    !
    ! Check to see if user is a developer using developer options
    !CALL DeveloperOptions
    !
    ! Set j = 2 to correspond to bottom axial node
    j = 2
    ! Fix ProblemTime
    DO i = im + 1, 1, -1
        ProblemTime(i) = ProblemTime(i-1)
    END DO
    ProblemTime(0) = 0.0_r8k
    ! Check to see if user wants to re-define # of timesteps
    IF (TimeIntegration > 0) CALL ResetTimesteps
    ! Redefine the dimensions for jnsurftemp
    jnsurftemp_max = MAXVAL(jnsurftemp)
    ! Allocate temporary arrays
    ALLOCATE (dumarray1(1:naxim))
    ALLOCATE (dumarray2(1:naxim))
    dumarray1 = 0.0_r8k
    dumarray2 = 0.0_r8k
    ! If fraptran start tape made, change jdlpr to 0
    IF (ntape > 0) jdlpr = 0
    ! For HWR Use rprm1_frapcon=2.21 based on JNM 255,222-233
    IF (iplant == -4) rprm1 = 2.21_r8k
    ! Make grainsize be 10 micrometers, regardless of user input value
    grnsize = 10.0_r8k
    !
    ! The following arrays may be input as either a single value or na
    ! Ensure that if they were put in as a single value that the value is applied to all axial nodes
    ! Note that they are initialized to -1.0 to allow for a user-input value of 0.0
    !
    ! Cladding Outer Diameter
    IF (dco(2) < 0.0_r8k) THEN ! Single value input
        dco = dco(1)
    ELSE ! User-defined input values
        IF (MINVAL(dco) <= 0.0_r8k) THEN
            i = MINLOC(dco,DIM=1)
            IF (i /= na) THEN
                WRITE (0,101) dco(i), i
                WRITE (ounit,101) dco(i), i
101             FORMAT ('Warning: Cladding diameter set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            IF (dco(na) <= 0.0_r8k) dco(na) = dco(na - 1)
        END IF
    END IF
    ! Cladding thickness
    IF (thkcld(2) < 0.0_r8k) THEN ! Single value input
        thkcld = thkcld(1)
    ELSE ! User-defined input values
        IF (MINVAL(thkcld) <= 0.0_r8k) THEN
            i = MINLOC(thkcld,DIM=1)
            IF (i /= na) THEN
                WRITE (0,102) thkcld(i), i
                WRITE (ounit,102) thkcld(i), i
102             FORMAT ('Warning: Cladding thickness set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            IF (thkcld(na) <= 0.0_r8k) thkcld(na) = thkcld(na - 1)
        END IF
    END IF
    ! Gap Thickness
    IF (thkgap(2) < 0.0_r8k) THEN ! Single value input
        thkgap = thkgap(1)
    ELSE ! User-defined input values
        IF (MINVAL(thkgap) <= 0.0_r8k) THEN
            i = MINLOC(thkgap,DIM=1)
            IF (i /= na) THEN
                WRITE (0,103) thkgap(i), i
                WRITE (ounit,103) thkgap(i), i
103             FORMAT ('Warning: Gap thickness set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>=0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            ! Allow for a gap of 0.0 but not negative
            IF (thkgap(na) < 0.0_r8k) thkgap(na) = thkgap(na - 1)
        END IF
    END IF
    ! Central hole
    IF (rc(2) < 0.0_r8k) rc = rc(1)
    DO i = 1, na
        IF (rc(i) < 0.0_r8k) rc(i) = 0.0_r8k
    END DO
    ! Gadolinia content
    IF (gadoln(2) < 0.0_r8k) gadoln = gadoln(1)
    DO i = 1, nt
        IF (gadoln(i) < 0.0_r8k) gadoln(i) = 0.0_r8k
    END DO
    ! U-235 enrichment
    IF (enrch(2) < 0.0_r8k) enrch = enrch(1)
    DO i = 1, nt
        IF (enrch(i) <= 0.0_r8k) THEN
            WRITE (0,104) enrch(i), i
            WRITE (ounit,104) enrch(i), i
104         FORMAT ('Warning: Enrichment set to ',e10.3,' at axial node ',i4, ' which is outside', &
              &     ' of the bounds (>0.0).',/,9x,'Will reset to 1.0E-10 to ensure code stability')
            enrch(i) = 1.0E-10_r8k
        END IF
    END DO
    ! Pu content
    IF (comp(2) < 0.0_r8k) comp = comp(1)
    DO i = 1, nt
        IF (comp(i) < 0.0_r8k) THEN
            WRITE (0,105) comp(i), i
            WRITE (ounit,105) comp(i), i
105         FORMAT ('Warning: Pu content set to ',e10.3,' at axial node ',i4, ' which is outside', &
              &     ' of the bounds (>=0.0).',/,9x,'Will reset to 0.0')
            comp(i) = 0.0_r8k
        END IF
    END DO
    ! Account for gamma heating (if supplied)
    IF (modheat > 0.0_r8k) qmpy = qmpy * (1.0_r8k - modheat)
    ! Set axial values for hydraulic diameter (de), clad outer diameter (dci), pellet diameter (dp) and cold gap (cdg)
    DO i = 1, na
        de(i) = 4.0_r8k * ((pitch ** 2) - (pi / 4.0_r8k) * dco(i) ** 2) / (pi * dco(i))
        dci(i) = dco(i) - 2.0_r8k * thkcld(i)
        dp(i) = dci(i) - 2.0_r8k * thkgap(i)
        cdg(i) = (dci(i) - dp(i)) * 1.0e3_r8k
    END DO
    ! Start the crud film at 0.0 for user specified crud growth
    IF (icor == 2) crdt = 0.0_r8k
    !
    buin = buin(1)
    ! Check to see that ivardm was properly set in the input file
    IF (ivardm == 0) deltaz(1:nt) = totl / nt
    ! Plenum height
    deltaz(na) = cpl
    ! Set the conversion factors based on the input units
    IF (nunits == 0) pitch = 100.0_r8k * pitch ! (cm)
    ! If fixed coolant conditions (nsp=0), apply 1st timestep value to all timesteps (im)
    IF (nsp == 0) THEN
        go = go(1)
        p2 = p2(1)
        tw = tw(1)
    END IF
    ! Set spring variables
    Spring%vs = vs
    Spring%dspgw = dspgw
    Spring%dspg = dspg
    ! The following call to check edits the input information for obvious errors
    CALL check
    !
    IF (iquit == 1) THEN
        DEALLOCATE(dumarray1)
        DEALLOCATE(dumarray2)
        RETURN
    END IF
    DO ii = 1, nt
        ij = nt - ii + 1
        ctmax(ij+1) = ctmax(ij)
    END DO
    IF (nunits == 0) THEN
        IF (crdt == 1.0_r8k) crdt = 2.54e-5_r8k
        IF (crdtr == 1.1415525e-4_r8k) crdtr = 8.05e-13_r8k
        IF (roughc == 1.97e-5_r8k) roughc = 5.0e-7_r8k
        IF (roughf == 7.87e-5_r8k) roughf = 2.0e-6_r8k
        IF (tref == 77.0_r8k) tref = 298.15_r8k
        IF( tsint == 2911.0_r8k) tsint = 1872.59_r8k
        IF (TGasFab == 77.0_r8k) TGasFab = 298.15_r8k
        cpl = cpl * mtoin
        cplrefab = cplrefab * mtoin
        cdg = cdg * mtoin
        crdt = crdt * mtomil
        crdtr = crdtr * mtomil * hrtosec
        dishsd = dishsd * mtoin
        dspg = dspg * mtoin
        Spring%dspg = Spring%dspg * mtoin
        dspgrefab = dspgrefab * mtoin
        dspgw = dspgw * mtoin
        Spring%dspgw = Spring%dspgw * mtoin
        dspgwrefab = dspgwrefab * mtoin
        fgpav = fgpav * PatoPSI
        fgpavrefab = fgpavrefab * PatoPSI
        hdish = hdish * mtoin
        chmfrh = chmfrh * mtoin
        chmfrw = chmfrw * mtoin
        hplt = hplt * mtoin
        rc = rc * mtoin
        thkcld = mtoin * thkcld
        thkgap = mtoin * thkgap
        zrb2thick = zrb2thick * mtoin
        tcc = tcc * mtoin
        totl = totl * mtoft
        deltaz = deltaz * mtoft
        roughf = roughf * mtoin
        roughc = roughc * mtoin
        tref = tkf(tref)
        tsint = tkf(tsint)
        TGasFab = tkf(TGasFab)
        DO ii = 1, (nt + 1)
            buin(ii) = buin(ii) * MWskgUtoMWdMTU
            dco(ii) = dco(ii) * mtoin
            dci(ii) = dci(ii) * mtoin
            dp(ii) = dp(ii) * mtoin
            de(ii) = de(ii) * mtoin
            ctmax(ii) = tkf(ctmax(ii))
        END DO
        DO ii = 1, im
            go(ii) = go(ii) * ksm2tolbhrft2
            IF (igascal == 0) p1(ii) = p1(ii) * PatoPSI
            p2(ii) = p2(ii) * PatoPSI
            qmpy(ii) = qmpy(ii) / mtoft
            tw(ii) = tkf(tw(ii))
        END DO
        x = x * mtoft
    END IF
    ! Convert creep storage temperature & pressure used in Dating Model
    IF (idatingcreep > 0 .AND. nunits == 1) THEN !Convert from British to SI
        DO icreep = 1, ncreeptab
            creeptabtemp(icreep) = tfc(creeptabtemp(icreep))
            creeptabstress(icreep) = creeptabstress(icreep) * PSItoMPa
        END DO
    END IF
    ! Set up array for fixed cladding temperatures
    IF (ifixedtsurf == 1) THEN
        axheight = totl / nt
        DO kt = 1, nt + 1
            IF (kt == 1) THEN
                axialnode(kt) = 0.0_r8k
            ELSE
                axialnode(kt) = axialnode(kt-1) + axheight
            END IF
        END DO
        numshapes = MAXVAL(jstsurftemp)
        IF (nunits == 0) THEN
            DO kt = 1, naxim
                xt(kt) = xt(kt) * mtoft
                cladt(kt) = tkf(cladt(kt))
            END DO
        END IF
        nstart = 0
        DO mt = 1, numshapes
            DO kt = 1, jnsurftemp(mt)
                dumarray1(kt) = xt(kt + nstart)
                dumarray2(kt) = cladt(kt + nstart)
            END DO
            nstart = nstart + jnsurftemp(mt)
            DO kt = 1, nt + 1
                dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, jnsurftemp(mt))
            END DO
        END DO
        DO mt = 2, im + 1
            IF (jstsurftemp(mt-1) > 0) THEN
                DO kt = 1, nt + 1
                    cladtarray(mt,kt) = dumarray3(jstsurftemp(mt-1),kt)
                END DO
                cladtarray(mt,na+1) = dumarray3(jstsurftemp(mt-1),na)
            END IF
        END DO
    END IF
    ! Check to see if fixed coolant temperatures or pressures
    IF (ifixedcoolt == 1 .OR. ifixedcoolp == 1) THEN
        ! Determine the axial node increments used by the code
        axheight = totl / nt
        axialnode(1) = 0.0_r8k
        DO kz = 2, na
            axialnode(kz) = axialnode(kz-1) + deltaz(kz-1)
        END DO
        ! Determine the # of axial nodes supplied by the user
        ncoolz = COUNT(zcool(2:na) > 0.0_r8k) + 1
        IF (ncoolz < 3) THEN
            WRITE (ounit,*) 'Error: # of zcool pairs is < 3. Code Stopped'
            STOP
        END IF
        ! Reduce the array size to save memory
        CALL ReAllocateArray(na, ncoolz, 1, zcool)
        ! Convert axial heights (m to ft)
        IF (nunits == 0) zcool(1:ncoolz) = zcool(1:ncoolz) * mtoft ! Convert m to feet
        DO kz = 2, ncoolz
            ! Determine the # of axial nodes supplied by the user
            IF (zcool(kz) <= 0.0_r8k) THEN
                WRITE (ounit,*) 'Error: Axial heights for zcool pairs is not increasing. Code Stopped'
                STOP
            ELSE IF (zcool(ncoolz) /= totl) THEN
                WRITE (ounit,*) 'User supplied axial heights zcool not equal to total length of fuel rod, totl'
                WRITE (ounit,*) 'zcool(top) = ',zcool(kz-1),'totl = ',totl
                WRITE (ounit,*) 'This will be reset to equal totl.'
                zcool(ncoolz) = totl  ! Force the last node to be equal to the total fuel length
            END IF
        END DO
        ! Fixed coolant temperatures
        IF (ifixedcoolt == 1) THEN
            ! Determine the # of temperature/history pairs
            ncoolanthist = COUNT(tcoolant(1:(im*ncoolz)) > 0.0_r8k)
            npairs = ncoolanthist / ncoolz
            ! Reduce the array size to save memory
            CALL ReAllocateArray(im*na, ncoolanthist, 1, tcoolant)
            ! Convert coolant temperatures from K to F if necessary
            IF (nunits == 0) THEN
                DO kt = 1, (ncoolanthist)
                    tcoolant(kt) = tkf(tcoolant(kt))
                END DO
            END IF
            DO mt = 1, im
                DO kt = 1, ncoolz
                    dumarray1(kt) = zcool(kt)
                    IF (mt <= npairs) THEN
                        dumarray2(kt) = tcoolant(mt + (kt-1) * npairs)
                    ELSE ! Last state is assumed to happen for all remaining timesteps
                        dumarray2(kt) = tcoolant(kt * npairs)
                    END IF
                END DO
                DO kt = 1, na
                    dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, ncoolz)
                END DO
            END DO
            DO mt = 2, (im + 1)
                DO kt = 1, na
                    coolanttemp(1,kt) = dumarray3(1,kt)
                    coolanttemp(mt,kt) = dumarray3(mt-1,kt)
                END DO
                coolanttemp(1,na+1) = dumarray3(1,na)
                coolanttemp(mt,na+1) = dumarray3(mt-1,na)
            END DO
        END IF
        ! Check to see if fixed coolant pressures
        IF (ifixedcoolp == 1) THEN
            ! Determine the # of temperature/history pairs
            ncoolanthist = COUNT(Pcoolant(1:(im*ncoolz)) > 0)
            npairs = ncoolanthist / ncoolz
            ! Reduce the array size to save memory
            CALL ReAllocateArray(im*na, ncoolanthist, 1, pcoolant)
            ! Convert coolant pressures from Pa to Psi
            IF (nunits == 0) pcoolant(1:ncoolanthist) = pcoolant(1:ncoolanthist) * PatoPSI
            DO mt = 1, im
                DO kt = 1, ncoolz
                    dumarray1(kt) = zcool(kt)
                    IF (mt <= npairs) THEN
                        dumarray2(kt) = Pcoolant(mt + (kt-1) * npairs)
                    ELSE ! Last state is assumed to happen for all remaining timesteps
                        dumarray2(kt) = Pcoolant(kt * npairs)
                    END IF
                END DO
                DO kt = 1, na
                    dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, ncoolz)
                END DO
            END DO
            DO mt = 2, im + 1
                DO kt = 1, na
                    coolantpressure(1,kt) = dumarray3(1,kt)
                    coolantpressure(mt,kt) = dumarray3(mt-1,kt)
                END DO
            END DO
        END IF
    END IF
    ! Crud multiplier. If not supplied, set equal to 1.0
    IF (crudmult(1) == -1.0_r8k) crudmult(1:na) = 1.0_r8k
    ! If not supplied for plenum (by default, not supplied) set equal to value for cladding node directly below plenum.
    IF (crudmult(na) == -1.0_r8k) crudmult(na) = crudmult(na-1)
    ! Crud thickness = crud thickness * axial crud rate multiplier
    crdtt(1:na) = crdt * crudmult(1:na)
    ! # of axial nodes + 1
    ir1 = nt + 1
    ! Check to ensure LHGR > 0 and an axial power shape is provided
    DO ii = 1, im
        IF (jst(ii) == 0) jst(ii) = 1
        IF (qmpy(ii) == 0.0_r8k) qmpy(ii) = 1.0e-6_r8k
    END DO
    ! # of axial power shapes
    jjc = MAXVAL(jst)
    ! Resize qaxnorm if jjc < im
    IF (jjc < im) CALL ReAllocateArray2D(na, na, 1, im+1, jjc, 1, qaxnorm)
    ! As-fabricated fuel density (g/in^3)
    rhofuel = MatProp ('FUEL', 'ASFABDENSITY', dummy)
    ! Calculate the empty volume of the plenum
    cpv = pi * dci(na) ** 2 * cpl / 4.0_r8k
    ! Calculate the volume of the spring
    Spring%Vcold = vs * (pi * (dspgw / 2.0_r8k) ** 2) * pi * (dspg - dspgw)
    ! Calculate the volume fraction of the plenum occupied by the spring
    Spring%Vf = Spring%Vcold / cpv
    ! Calculate the empty volume of the plenum
    cpv = cpv - Spring%Vcold
    ! Calculation of initial fuel porosity
    prty = (100.0_r8k - den) / 100.0_r8k
    nrm1 = nr - 1
    ! Fuel porosity
    FORALL (jj=1:na-1,i=1:nr-1) porosold(i,jj) = prty / 3.0_r8k
    ! Calculation of the radial node placement in the fuel region. This function will place more nodes
    ! near the surface of the pellet to account for rim effects. node 1 is on the fuel surface.
    DO jj = 1, nt
        DO i = 1, nr
            crad(i,jj) = (1.0_r8k - (REAL(i-1) / REAL(nr-1)) ** 3) * (dp(jj) / 2.0_r8k - rc(jj)) + rc(jj)
            ! Subroutines fueltp and tubrnp use a reversed nodalization_frapcon
            rrev(nr-(i-1),jj) = crad(i,jj)
        END DO
    END DO
    ! cfva - total cold fuel volume assuming cylindrical pellets
    cfva = 0.0_r8k
    DO jj = 1, nt
        cfva = cfva + pi * dp(jj) ** 2 * deltaz(jj) * 3.0_r8k
    END DO
    cfv = 0.0_r8k
    ! Determine the cold free volume (cfv)
    DO jj = 1, nt
        IF (hdish <= 0.0_r8k .AND. chmfrh <= 0.0_r8k) THEN !No dish or chamfers are modeled
            DO l = 1, nrm1
                ringvol(l,jj) = (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * pi * deltaz(jj) * fttoin
                coldringl(l,jj) = deltaz(jj) * fttoin
            END DO
            cfv = cfva - pi * rc(jj) * rc(jj) * totl * fttoin
        ELSE
            ! dish radius calculation
            dishsg = dp(jj) / 2.0_r8k - dishsd
            ! dish radius of curvature
            IF (hdish <= 0.0_r8k) THEN
                rdish = 0.0_r8k
            ELSE
                rdish = (hdish * hdish + dishsg * dishsg) / (2.0_r8k * hdish)
            END IF
            ! chamfer inner radius
            rchmfr = dp(jj) / 2.0_r8k - chmfrw
            ! volume of pellet annulus
            vplt = pi * rc(jj) * rc(jj) * deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
            ! radial pellet ring height calculation
            DO l = nrm1, 1, -1
                IF (rdish < crad(l,jj)) THEN
                    hi = hdish
                ELSE
                    hi = rdish - MAX(SQRT(rdish ** 2 - crad(l,jj) ** 2), (rdish - hdish))
                END IF
                ringvol(l,jj) = (deltaz(jj) / hplt) * pi * (crad(l,jj) ** 2 * (hplt + 2.0_r8k * (hi - hdish)) - &
                  &              2.0_r8k * hi ** 2 * (rdish - hi / 3.0_r8k)) * fttoin - vplt
                IF (chmfrh > 0.0_r8k .AND. chmfrw > 0.0_r8k) THEN
                    IF (crad(l,jj) > rchmfr) ringvol(l,jj)   = &
                      & (deltaz(jj) / hplt) * (pi * (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * hplt - &
                      &  2.0_r8k * pi * crad(l,jj) ** 2 * chmfrh * (crad(l,jj) - rchmfr) / chmfrw + &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l,jj) - rchmfr) / chmfrw * &
                      &  (crad(l,jj) ** 2 + (rchmfr) ** 2 + crad(l,jj) * (rchmfr))) * fttoin
                    IF (crad(l+1,jj) > rchmfr) ringvol(l,jj) = &
                      & (deltaz(jj) / hplt) * (pi * (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * hplt - &
                      &  2.0_r8k * pi * crad(l,jj) ** 2 * chmfrh * (crad(l,jj) - rchmfr) / chmfrw + &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l,jj) - rchmfr) / chmfrw * &
                      &  (crad(l,jj) ** 2 + (rchmfr) ** 2 + crad(l,jj) * (rchmfr)) + &
                      &  2.0_r8k * pi * crad(l+1,jj) ** 2 * chmfrh * (crad(l+1,jj) - rchmfr) / chmfrw - &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l+1,jj) - rchmfr) / chmfrw * &
                      &  (crad(l+1,jj) ** 2 + (rchmfr) ** 2 + crad(l+1,jj) * (rchmfr))) * fttoin
                END IF
                coldringl(l,jj) = ringvol(l,jj) / ((crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * pi)
                vplt = vplt + ringvol(l,jj)
            END DO
            cfv = cfv + (vplt - pi * rc(jj) * rc(jj) * deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish))
            IF (rc(jj) > 0.0_r8k) coldringl(nr-1,jj) = deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
        END IF
    END DO
    pecdh = (cfva - cfv) / cfva
    ! Calculation of pellet open porosity
    gden = den - 1.25_r8k
    IF (gden >= 94.0_r8k) THEN
        fpor1 = 0.0_r8k
    ELSE IF (gden > 91.25_r8k) THEN
        fpor1 = 0.0012019636_r8k * (94.0_r8k - gden)
    ELSE
        fpor1 = 16.929682_r8k - 0.23285470_r8k * gden - 8.7183686e-4_r8k * gden ** 2 + 1.5242216e-5_r8k * gden ** 3
    END IF
    ! Calculation of as-fabricated roughness volume (rfnvff) and free volume (cvv) in in^3
    rfnvff = 0.0_r8k
    cvv = 0.0_r8k
    DO jj = 1, nt
        rfnvff = rfnvff + 14.4e-6_r8k * pi * dp(jj) * deltaz(jj) * ft2toin2 / cfv
        cvv = cvv + (dci(jj) ** 2 - (1.0_r8k - pecdh) * dp(jj) ** 2) * pi * deltaz(jj) * 3.0_r8k
    END DO
    cvv = cvv + cpv + rfnvff * cfv + fpor1 * cfv
    ! Initial rod pressure = fill gas pressure
    press = fgpav
    ! Set default gas value based on user input
    SELECT CASE (idxgas)
    CASE (1)
        amfhe = 1.0_r8k
    CASE (2)
        amfair = 1.0_r8k
    CASE (3)
        amfn2 = 1.0_r8k
    CASE (4)
        amffg = 1.0_r8k
    CASE (5)
        amfarg = 1.0_r8k
    END SELECT
    ! Set values for Xe & Kr
    IF (amffg > 0.0_r8k) THEN
        SELECT CASE (imox)
        CASE (0) !UO2 Xe/Kr Ratio
            amfkry = 0.15_r8k * amffg
            amfxe = 0.85_r8k * amffg
        CASE (1, 2) !MOX Xe/Kr Ratio
            amfkry = 0.0588_r8k * amffg
            amfxe = 0.9412_r8k * amffg
        CASE DEFAULT ! STOP
            WRITE (0,601) imox
            WRITE (ounit,601) imox
601         FORMAT ('Wrong value for imox called in subroutine inital for determing Xe/Kr Ratio. imox = ',i3)
            STOP
        END SELECT
    END IF
    ! Set gas molecular weight ratios
    gases(1) = amfhe
    gases(2) = amfarg
    gases(3) = amfkry
    gases(4) = amfxe
    gases(5) = amfh2
    gases(6) = amfn2
    gases(7) = amfair
    gases(8) = amfh2o
    ! Calculate initial gas moles (cmol)
    ! From PV=nRT, n = PV / R_igc*T = V/MWt_mix * (P/R_mix*T) where P/R_mix*T = Density
    GasDensity = MatProp ('GAS', 'DENSITY', tfk(TgasFab)) ! kg/m3
    MWtMix = MatProp ('GAS', 'MOLWT', tfk(TgasFab)) ! g/mol
    cmol = cvv * in3tom3 * GasDensity * kgtog / MWtMix
    ! Set input # of moles of each gas species
    hein = amfhe * cmol
    argin = amfarg * cmol
    IF (amffg > 0.0_r8k) THEN ! Specified fission gas fraction in input
        fgin = cmol * amffg
    ELSE ! Fission gas fraction is sum of Xe and Kr
        fgin = (amfxe + amfkry) * cmol
    END IF
    kryin = amfkry * cmol
    xein = amfxe * cmol
    h2in = amfh2 * cmol
    an2in = amfn2 * cmol
    airin = amfair * cmol
    h2oin = amfh2o * cmol
    ! Calculation of initial concentration of nitrogen in fuel
    angi = ppmn2 * cfv * rhofuel / (N2MWt * 1.0e6_r8k)
    DO i = 1, nt
        ang(i,1) = angi * (deltaz(i) / totl)
    END DO
    ! Calculation of initial concentration of water in fuel
    h2omi = ppmh2o * cfv * rhofuel / (H2OMWt * 1.0e6_r8k)
    DO i = 1, nt
        ah2og(i,1) = h2omi * (deltaz(i) / totl)
    END DO
    ! Fixed coolant conditions.
    IF (nsp == 0) THEN
        IF (ifixedcoolt == 1) THEN ! User supplied axial coolant temperatures
            tw(1:im) = coolanttemp(2:im+1,1)
        ELSE ! User supplied only one value to be used for all timesteps
            tw = tw(1)
        END IF
        IF (ifixedcoolp == 1) THEN ! User supplied axial coolant pressures
            p2(1:im) = coolantpressure(2:im+1,1)
        ELSE ! User supplied only one value to be used for all timesteps
            p2 = p2(1)
        END IF
        go = go(1)
    END IF
    ! Calculate initial concentrations of U-235, U-238, Pu-239, Pu-240, Pu-241 and Pu-242.
    CALL turbin
    ! Write some input file data to output file
    !CALL print1
    ! Set sintering temperature, convert from F to K
    tsint = tfk(tsint)
    DO ii = 1, im
        qmpy_tmp = 0.0_r8k
        ! Normalize the heat flux over all axial cladding dimensions
        DO i = 1, nt
            qmpy_tmp = qmpy_tmp + (qmpy(ii) * kWtoBTUh / ((dco(i) * intoft) * pi)) * (deltaz(i) / totl)
        END DO
        qmpy(ii) = qmpy_tmp
    END DO
    ! Convert problem time from days to seconds
    ProblemTime = ProblemTime * daytosec
    ! Convert stopox from days to seconds
    stopox = stopox * daytosec
    ! The following coding creates an inital time step with small values of time and power to avoid instabilities
    DO ij = 1, im
        ii = im - ij + 1
        IF (igascal == 0) p1(ii+1) = p1(ii)
        p2(ii+1) = p2(ii)
        go(ii+1) = go(ii)
        tw(ii+1) = tw(ii)
        qmpy(ii+1) = qmpy(ii)
        ProblemTime(ii+1) = ProblemTime(ii)
        jst(ii+1) = jst(ii)
        jstsurftemp(ii+1) = jstsurftemp(ii)
        ! If the user option for addgmles(it) or addswell(it) > 0, it won't be applied until the first true timestep value (it = 2)
        addgmles(ii+1) = addgmles(ii)
        addswell(ii+1) = addswell(ii)
    END DO
    ! Set values for initial power step
    !qmpy(1) = 3000.0_r8k
    ProblemTime(1) = 0.001_r8k
    jstsurftemp(1) = 0
    addgmles(1) = 0.0_r8k
    addswell(1) = 0.0_r8k
    ! Incriment im by 1 to account for initial power step
    !im = im + 1
    !
    DEALLOCATE(dumarray1)
    DEALLOCATE(dumarray2)
    !
    END SUBROUTINE init_check

    subroutine FRAPCON_4_0_Patch_1
        USE Kinds_frapcon
        USE conversions_frapcon
        USE variables_frapcon, ONLY : ProblemTime, ounit, it
        USE FileIO_frapcon, ONLY : iofiles, IOEcho
        USE TimeStep_frapcon
        USE, INTRINSIC :: ISO_FORTRAN_ENV
        IMPLICIT NONE
        !>@brief
        !> This is the driver for the FRAPCON Code. Version 4.0 Patch 1.
        !
        REAL(r8k) :: LastTime

        CALL iofiles
        CALL IOEcho
        !
        CALL frpcon
        !
        CLOSE (ounit)
        ! Print the time at which the code execution stopped (Convert from seconds to day)
        LastTime = ProblemTime(it) * sectoday
        WRITE (0,10) LastTime
10      FORMAT ('FRAPCON code execution finished with a problem time of ',f11.4,' days.')
        !
    end subroutine FRAPCON_4_0_Patch_1

    subroutine driver_restfs(this)

        class (frapcon_driver), intent(inout) :: this

        logical :: is_open

        inquire (unit = ftunit, opened = is_open)
        if (is_open) close (unit = ftunit)
        open (unit = ftunit, file = namerf)
        call restfs
        close (ftunit)
    end subroutine driver_restfs

end module frapcon4




