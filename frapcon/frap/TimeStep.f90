MODULE TimeStep
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to advance the problem in time.
    !> Subroutines include frpcon, newgap, store
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE frpcon
    USE Kinds
    USE Conversions
    USE CoolantData
    USE CorrosionData
    USE Output_Data
    USE PlotFile
    USE Material_Properties
    USE Variables
    USE Refabrication
    USE DatingData
    USE FissionGas
    USE Restart
    USE Burn
    USE Decay
    USE MechanicalDeform
    USE SetupProblem, ONLY : setup
    USE FileIO, ONLY : Namelist_Read_Error
    USE Plenum
    USE void
    USE FEA_IO, ONLY : write_output
    USE CladDeformation
    USE FuelDeformation
    USE Temperature
    USE ZrModels
    USE CladCreep
    IMPLICIT NONE
    !>@brief
    !> frpcon is called from driver and calls all of the major subcodes
    !
    INTEGER(ipk) :: jj, ii, i, ntime, mtest, nam1, l, ij, m3, ir1m3, ir1m33, ijj, nalfdt, m1, m2
    REAL(r8k) :: wpcm3, wpgm, gaslas, deh, gratio, cinwid, cinrad, cyldst, caxrad, caxstr, deltmp, &
      &          strnyt, strnye, strnie, strnue, stsrpt, strrpe, cyldse, cultse, cbrste, cbrsst, &
      &          ctstrt, cgap, cruf, gap, tcak, u, cdpres, fuelTD, dummy
    !
    ! Read and processes input and set up the dynamic dimensioning pointers
    !
    CALL setup
    !
    IF (iquit == 1) RETURN
    ! define dcoBOL
    dcoBOL = dco(1)
    jfix = jpeak(1)
    itt = 1
    transt = 0.8_r8k * tsint
    frden = den * 0.01_r8k
    jmin = 2
    jmax = ir1
    !
    IF (nread == 0) THEN
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
        it = 0 
        ! new = 2
    ELSE
        ! Read the restart information for a frapcon restart
        CALL ReadRestart
    END IF
    ! If error occured, stop the code execution
    IF (iquit == 1) RETURN
    !
    ! Print informational data to plot file
    !
    IF (nplot > 0) CALL grafini
    !
    WRITE (ounit,*)'Finished input processing. Start of time step loop'
    !
    ! ********************************************************************
    ! *****           START OF TIME STEP LOOP, it = 1, im            *****
    ! ********************************************************************
    !
    DO WHILE (it < im)
        it = it + 1
        IF (ProblemTime(it) >= stopox) calcoxide = .FALSE.
        !
        ! Check to see if the rod has been refabricated during this timestep
        !
        IF (it == irefab + 1) CALL RodRefabrication
        !
        ! Check to see if this is a decay power step
        !
        IF (qmpy(it) < 0.0_r8k) CALL DecayHeat
        !
        ! Determine current peak axial node indicator "jpeak(1)"
        mtest = jst(it)
        jpeak(1) = jpeak(mtest)
        IF (mtest == 1) jpeak(1) = jfix
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
        IF (it /= 1 .OR. nread /= 0) THEN
            !
            ! Compute effective fluences and effective coldworks
            !
            ctemp = tfk(taca(it-1))
            rtemp = 0.0_r8k
            IF (it > 2) rtemp = (ctemp - tfk(taca(it-2))) / delhs
            !
            CALL caneal
            !
            ! Retain converged information needed for the next power-time step
            !
            ! Modified by IP to bypass this subroutine during the first FRAPCON loop after reading a restart file.
            IF (nread /= 1 ) CALL store
            !
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
                IF (iter > 15) WRITE (ounit,820) iter, gaslas, pit(it-1)
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
                    WRITE (ounit,800) j-1, it-1, iter, dltgc(k-1), dltgc(k-2)
                    WRITE (ounit,810) FuelCladGap(j-1), RinterfacPress(j-1)
                END IF
                Relocation(j-1) = MAX(Relocation(j-1), 0.0_r8k)
                ! New way (IP - use value from k instead of k-1)
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
            IF (it == irefab + 1) CALL UpdateRefabGases
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
                CALL print2
            END DO
        END IF
        !
        ! This is the FRAPCON to TRACE Data.
        !
        IF (nfrttr == 1) CALL fraptotrace
        !
        ! The following call to grafout stores the plot information for use with the Excel plot package Aplotter.xls.
        !
        IF (nplot >= 1 .AND. it > 1) THEN
            CALL grafout
            IF (mechan == 1) CALL write_output()
        END IF
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
        IF (nrestr > 0) CALL WriteRestart
        !
    END DO
    !
    ! ********************************************************************
    ! *****                 END OF TIME STEP LOOP                    *****
    ! ********************************************************************
    !
    ! Printout the summary page as well as EOL strain range, cumulative fission gas release, and the zircaloy-oxide weight gain.
    !
    PrintType = 'Summary'
    !
    CALL print2
    !
    ! Call the DATING subprogram to calculate spent fuel creep
    !
    IF (idatingcreep > 0) CALL DatingDriver
    !
    !
    !
800 FORMAT (10x,'No convergence on gap temperature drop for axial node', &
      &     i3, ', Time step',i3,' Iteration number, iter= ',i3/10x, &
      &     'The temperature drop the last two iterations were', &
      &     1x,f7.2,' and',1x,f7.2,' degrees F.')
810 FORMAT (10x,'Gap = ',e12.5,5x,'Interface pressure = ',e12.5)
820 FORMAT (/' Gas loop iter = ',i2,10x,'Pressure last two loops',2f10.2)
    !
    END SUBROUTINE frpcon
    !
    !
    !
    SUBROUTINE newgap
    USE Kinds
    USE Conversions
    USE Variables, ONLY : gpthe, gpth, dltgc, j, k, nconvg, ncont, gapmin, convc
    IMPLICIT NONE
    !>@brief
    !> newgap is called from frpcon and computes the new estimate of gap and interfacial pressure.
    !> It also checks on convergence of the inner loop based on gap temperature drop.
    !
    ! Input
    !
    ! dltgc  - Gap temperature drop (F)
    ! gapmin - Minimum diameteral gap thickness from Subroutine gaprs (in)
    ! gpth   - Thermal diameteral gap thickness (in)
    ! j      - Axial node index
    ! k      - Gap iteration indicator
    !
    ! Output
    !
    ! gpthe  - estimated gap thickness (in)
    ! nconvg - convergence index
    ! ncont  - non-convergence index (30 iterations)
    !
    REAL(r8k) :: cos
    !
    IF (k > 2) THEN
        ! Check for convergence on the gap temperature drop, dltgc
        convc = MAX(0.01_r8k * dltgc(k), 0.5_r8k)
        IF (ABS(dltgc(k) - dltgc(k-1)) <= convc) nconvg = .TRUE.
    END IF
    !
    IF (.NOT. nconvg) THEN
        ! Calculation of estimated gap
        k = k + 1
        IF (k <= 30) THEN
            gpthe(k) = 0.5_r8k * (gpthe(k-1) + gpth(k-1))
            IF (gpthe(k) < gapmin) gpthe(k) = (gpthe(k-1) + gapmin) / 2.0_r8k
        ELSE
            ncont = .TRUE.
        END IF
        ! ********************************
        !
        ! IP-This section of code should be removed as it can never be called due to above criteria on k values
        !
        ! IF (gpth(k-1) /= gpth(k-2)) THEN
        !     cos = 1.0_r8k - (gpthe(k-1) - gpthe(k-2)) / (gpth(k-1) - gpth(k-2))
        !     IF (ABS(cos) > 0.1_r8k) gpthe(k) = (gpthe(k-2) * gpth(k-1) - gpthe(k-1) * gpth(k-2)) / &
        !       &                                (gpthe(k-2) + gpth(k-1) - gpthe(k-1) - gpth(k-2))
        ! ENDIF
        ! gpthe(k) = 0.5_r8k * (gpthe(k-1) + gpth(k-1))
        ! IF (gpthe(k) < gapmin) gpthe(k) = (gpthe(k-1) + gapmin) / 2.0_r8k
        !
        ! ********************************
    END IF
    !
    END SUBROUTINE newgap
    !
    !
    !
    SUBROUTINE store
    USE Kinds
    USE Conversions
    USE Variables, ONLY : BOSNodeburnup, EOSNodeburnup, BOSZrO2Thk, StartofStepPickupH2Con, &
      &                   EndofStepPickupH2Con, EOSZrO2Thk, deltaz, CladH2Concen, &
      &                   OldHealedCrackRadius, HealedCrackRadius, dco, porosold, porosnew, &
      &                   dpw, dpwpp, dpw2, dpwpp2, densf, na, nr, densp, tmpfuel, &
      &                   StartofStepH2Con, ang, totl, buold, bup, ir1, nheal, jmin, jmax
    IMPLICIT NONE
    !>@brief
    !> This Subroutine is called from frpcon and stores converged values of parameters which are necessary for
    !> the next power-time step.
    !>@author
    !> g a berna
    !>@date
    !> November 1977.
    !
    ! Input
    !
    ! ang           - cumulative n2 concentration in the fuel (moles)
    ! bup           - rod burnup to end of power-time step (MWd/mtU)
    ! EOSNodeburnup - nodal burnup at End End of step (MWd/mtU)
    ! deltaz        - axial node length (ft)
    ! densf         - densification (unitless fraction)
    ! dpw           - swelling (unitless fraction)
    ! ir1           - number of axial nodes plus one
    ! na            - maximum number of axial nodes
    ! nheal         - flag to turn on permanent crack healing 0 = Off  1 = On
    ! nr            - maximum number of radial fuel nodes
    ! porosnew      - porosity (unitless fraction)
    ! tmpfuel       - fuel temperature distribution (F)
    ! totl          - total fuel stack length (ft)
    ! EOSZrO2Thk    - zircaloy oxide thickness (ft)
    ! HealedCrackRadius         - radius to where cracking occurs  (m)
    ! StartofStepH2Con(k)       - start of time step H2 concentration in cladding at axial node k (ppm)
    ! CladH2Concen(k)           - End of time step H2 concentration in cladding at axial node k (ppm)
    ! StartofStepPickupH2Con(k) - start of time step H2 concentration in cladding at axial node k due to pickup from coolant (ppm)
    ! EndofStepPickupH2Con(k)   - End of time step H2 concentration in cladding at axial node k due to pickup from coolant (ppm)
    !
    ! Output
    !
    ! ang                  - cumulative n2 concentration in the fuel (moles)
    ! buold                - rod average burnup at beginning of power-time step (MWd/mtU
    ! BOSNodeburnup        - rod nodal burnup at start of step (MWd/mtu)
    ! densp                - fuel densification (unitless fraction)
    ! dpwpp                - fuel swelling (unitless fraction)
    ! dpwpp2               - fuel creep (unitless fraction)
    ! porosnew             - porosity (unitless fraction)
    ! OldHealedCrackRadius - previous radius to where cracking occurs  (m)
    ! BOSZrO2Thk           - zircaloy oxide thickness (ft)
    !
    INTEGER(ipk) :: i, ii
    REAL(r8k) :: bur
    !    
    bur = 0.0_r8k
    DO ii = jmin, jmax
        BOSNodeburnup(ii-1) = EOSNodeburnup(ii-1)
        bur = EOSNodeburnup(ii-1) * deltaz(ii-1) + bur
        dco(ii-1) = dco(ii-1) - (EOSZrO2Thk(ii-1) - BOSZrO2Thk(ii-1)) * fttoin / 1.56_r8k * 2.0_r8k
        BOSZrO2Thk(ii-1) = EOSZrO2Thk(ii-1)
        StartofStepH2Con(ii-1) = CladH2Concen(ii-1)
        StartofStepPickupH2Con(ii-1) = EndofStepPickupH2Con(ii-1)
        IF (nheal == 1) OldHealedCrackRadius(ii-1) = MAX(OldHealedCrackRadius(ii-1), HealedCrackRadius(ii-1))
        ang(ii-1,1) = ang(ii-1,2)
        DO i = 1, nr
            porosold(i,ii-1) = porosnew(i,ii-1)
            densp(i,ii-1) = densf(i,ii-1)
            dpwpp(i,ii-1) = dpw(i,ii-1)
            dpwpp2(i,ii-1) = dpw2(i,ii-1)
        END DO
    END DO
    ! Plenum values
    dco(jmax) = dco(jmax) - (EOSZrO2Thk(jmax) - BOSZrO2Thk(jmax)) * fttoin / 1.56_r8k * 2.0_r8k
    BOSZrO2Thk(jmax) = EOSZrO2Thk(jmax)
    StartofStepH2Con(jmax) = CladH2Concen(jmax)
    StartofStepPickupH2Con(jmax) = EndofStepPickupH2Con(jmax)
    !
    buold = bur / totl
    !
    END SUBROUTINE store
    !
    !
    !
    SUBROUTINE SetPeakPowerValues (SaveLoop)
    USE Kinds
    USE Conversions
    USE Variables
    IMPLICIT NONE
    !>@brief
    !> Subroutine sets the values for the peak power node
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> August 13, 2015
    !
    ! Input
    !
    ! SaveLoop   - Indicator for which data to save
    !            = 'AxialLoop' for values set during the axial convergence loop
    !            = 'TimestepLoop' for value set during the time convergence loop
    ! jpeak(1)   - Peak power node
    !
    ! Internal
    !
    ! PeakNode   - Peak node indicator
    !
    INTEGER(ipk) :: PeakNode
    CHARACTER(LEN=*), INTENT(IN) :: SaveLoop
    !
    PeakNode = jpeak(1) - 1
    !
    SELECT CASE (SaveLoop)
    CASE ('AxialLoop')
      
        pkPower(it-1) = Power(PeakNode)
        pkBurnup(it-1) = EOSNodeburnup(PeakNode)
        pkODCladTemp(it-1) = CladOutSurfTemp(PeakNode)
        pkIDCladTemp(it-1) = CladInSurfTemp(PeakNode)
        pkGap(it-1) = gpthe(MIN(k,30)) * 1000.0_r8k * 0.5_r8k
        pkPelCentTemp(it-1) = tmpfuel(nr,j-1)
        pkFuelPelOD(it-1) = dphfrl
        pkGapCond(it-1) = GapCond(PeakNode)
        pkZrO2(it-1) = EOSZrO2Thk(j-1) * fttomil
        pkZrO2WtGain = zro2wg
        pkH2up(it-1) = CladH2Concen(PeakNode)
   
    CASE ('TimestepLoop')
    
        pkAveCladTemp(it-1) = CladAveTemp(PeakNode)
        pkPelSurfTemp(it-1) = PelSurfTemp(PeakNode)
        pkPelAveTemp(it-1) = PelAveTemp(PeakNode)
        pkFisGasRelFrac(it-1) = tfgfr
        pkHoopStres(it-1) = sig(PeakNode,1)
        pkAxlStres(it-1) = sig(PeakNode,2)
        pkHoopStrain(it-1) = eps(PeakNode,1)
        pkIntefacePres(it-1) = RinterfacPress(PeakNode)
    
    END SELECT
    !
    END SUBROUTINE SetPeakPowerValues
    !
END MODULE TimeStep