MODULE void_fraptran
    USE Kinds
    USE conversions_fraptran
    USE functions_fraptran
    IMPLICIT NONE
    !>@brief
    !>
    CONTAINS
    !
    SUBROUTINE gsflow
    USE Kinds
    USE conversions_fraptran
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !> @brief
    !> Calculates the pressure history in the plenum and swelling zone of a fuel rod. Laminar flowthrough an annulus is modeled.
    !>@author
    !> model by J. A. Dearien + w. a. yuill, aerojet nuclear
    !> programmed by J. A. Dearien, january,1974
    !
    ! Input/Output
    !
    ! vplenc               - plenum volume
    ! tplen                - plenum temperature
    ! GasAx(i)             - moles of gas at axial node i
    ! GapThick(i)          - vector of nodal gap thickness
    ! AxNodElevat(i)       - elevation of axial node i (ft)
    ! TimeIncrement        - driver program time step
    ! ndtgsf               - number of numerical intergration steps (10-20)
    ! naxn                 - number of axial fuel nodes
    ! Ifail                - rod failure index: 0 = rod not failed. 1 = rod failed
    ! ngaspr               - gas pressure specification index
    !                    0 = gas pressure not input
    !                     >= 1, gas pressure calculated
    ! Kswell               - node number of swelling zone
    ! PelletRad(i)         - radius to outer surface of fuel at node i (ft)
    ! GapTemp(i)           - temperature of gap gas at axial node i (F)
    ! TotalGasMoles        - total gram-moles of gas in fuel rod
    ! VolAveGasTemp        - volume averaged temperature of gas in gas gap
    ! FuelSurfT(i)         - fuel surface temperature at axial node i (F)
    ! ies(1)               - switch indicating whether or not cladding strain at some elevation has surpassed instability strain
    !                    0 = No
    !                    1 = Yes
    ! dvdtp                - rate of gas volume increase needed to hold pressure constant in cladding swelling region 
    !                        even though gas is flowing into region  (ft**3/sec)
    !                        (not computed if ies(1)=0) (evaluated at time step midpoint)
    ! pswllz               - pressure in swelling region at start of time step (psia)
    ! GasPress(Kswell)     - pressure in swell region at end of time step as computed by balloon model (psia)
    !                        cladding stress equal to ultimate stress (psia) (not used If ies(1) = 0)
    ! pswll0(1)            - value at start of time step, GasPress(Kswell) = value at end of time step
    ! roi(1)               - inside cladding radius at Ends of localized swelling region (ft) (input)
    ! vs0(1)               - volume for gas occupancy generated by localized cladding swelling (ft**3)  (input)
    ! GasFraction(n)       - respective gas fractions of: 1 helium, 2 argon, 3 krypton, 4 xenon, 5 hydrogen, 6 nitrogen, 
    !                        7 air, 8 water vapor.
    ! flowg(1)             - gas flow rate from plenum, gm-moles/sec (computed)
    ! DishVperL(i)         - pellet dish volume per unit length at axial node i (ft**3/ft). If a central void is present
    !                        at axial node i, DishVperL(i) contains void volume per unit length at axial node i
    ! FuelCenterT(i)       - fuel centerline temperature   (F)
    ! VoverTRatio          - summation of volume/temperature terms, including plenum, in SS ideal gas equation (liters/k)
    !                        (output for use in converged p estimate)
    ! swllfr               - gas flow model switch. If # em20, on. otherwise off
    ! NSteadyTrans         - 1 = steady-state calculations. NSteadyTrans = 2 = transient
    ! CrackTemp(k)         - temperature of gas in fuel cracks at axial node k (F)
    ! gfloa1               - coefficient to extrapolate gas pressure in balloon model with cladding swelling.
    !                        Extrapolation in balloon. Model assumes plenum pressure remains constant during time step. 
    !                        Flow is calculated over the time step TimeIncrement. A new plenum pressure, pp, swelling pressure,
    !                        ps, and new values for pmol and smol are calculated and returned to the driver program.
    ! OpenPorTemp(k)       - temperature of gas in fuel open pores at node k (F)
    ! VolOpenPor(k)        - volume of fuel Open pores at axial node k (ft3/ft)
    ! TotalVoidVol         - total void volume of fuel rod (output) (ft**3)
    ! VoidVolumeRatio(1)   - ratio of plenum volume to total gas volume
    ! VoidVolumeRatio(2)   - ratio of crack  volume to total gas volume
    ! VoidVolumeRatio(3)   - ratio of gap    volume to total gas volume
    ! VoidVolumeRatio(4)   - ratio of pores  volume to total gas volume
    ! VoidVolumeRatio(5)   - ratio of dish   volume to total gas volume
    ! VoidVolumeRatio(6)   - ratio of lower plenum vol. to total gas volume
    ! VoidVolumeRatio(7)   - ratio of central void vol. to total gas volume
    ! VoidVolumeRatio(8)   - ratio of fuel surface roughness vol. to total gas volume
    ! VoidVolumeRatio(9)   - ratio of clad surface roughness vol. to total gas volume
    ! VoidVolumeRatio(10)  - ratio of external plenum volume to total gas volume
    ! VentRod              - indicator of ventilated fuel rod. 9=yes , otherwise no
    ! vplenb               - volume of bottom fuel rod plenum  (ft3)
    ! tplenb               - temperature of gas in bottom plenum (ft3)
    ! BottomPlenumGasMoles - amount of gas in bottom plenum (gram-moles)
    ! CentVoidVol(k)       - volume of fuel central void at axial node k (ft3/ft)
    ! roughc               - arithmetic mean surface roughness of cladding (cm)
    ! roughf               - arithmetic mean surface roughness of fuel     (cm)
    ! AveDishTemp(k)       - area averaged temperature of fuel pellet dishes(F)
    !
    ! Note:
    !
    ! Many of the variables used in this subroutine have their name and number of subcripts changed from that used elsewhere
    !
    INTEGER(ipk) :: VentRod, imx1, index, i, j, k
    REAL(r8k) :: dt1, gstlbs, pmol, anp0, smol, smol0, tgas, drag, vovertratio, vgap, tovervratio, &
      &          pp0, ps0, visc, alx, tg, hyddm, hagen, alxx, ck, ps, ap, bp, cp, &
      &          flow1, q, sqrq, an, ad, argln, aln, exv, pp, dltap, d, gaspin, visco
    REAL(r8k), PARAMETER :: grmlbs = 453.5924277_r8k
    REAL(r8k), PARAMETER :: convis = 0.0209_r8k
    REAL(r8k), PARAMETER :: cnvkr = 1.8_r8k
    REAL(r8k), PARAMETER :: em20 = 1.0e-20_r8k
    REAL(r8k), PARAMETER :: drgmax = 1.0e30_r8k
    REAL(r8k), PARAMETER :: GasConstant = 1545.0_r8k
    REAL(r8k), PARAMETER :: expmax = 35.0_r8k
    !
    VentRod = lhtc / 10
    dt1 = TimeIncrement
    IF (Ifail == 1 .OR. (ngaspr < 1 .AND. VentRod == 9)) THEN
        GasPress(1:naxn) = CoolPress(1:naxn)
        GasPress(naxn+1) = CoolPress(naxn)
        flowg(1) = 0.0_r8k
        RETURN
    ENDIF
    IF (ngaspr >= 1) THEN
        ! Gas pressure history is prescribed by input
        gaspin = polate (gasphs, Time, ngaspr)
        GasPress(1:naxn+1) = gaspin
        flowg(1) = 0.0_r8k
        RETURN
    ENDIF
    IF (NSteadyTrans /= 1 .AND. swllfr >= em20) THEN
        Kswell = MAX(1, Kswell)
        GasAx(Kswell) = TotalGasMoles - GasAx(naxn+1)
        gstlbs = TotalGasMoles / grmlbs
        pmol = GasAx(naxn+1) / grmlbs
        anp0 = pmol
        smol = gstlbs - pmol
        smol0 = smol
        tgas = VolAveGasTemp
        imx1 = MIN(Kswell + 1, naxn)
        drag = 0.0_r8k
        dvdtp = 0.0_r8k
        !
        Index = 4
        CALL gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, CrackVolume, &
          &          CoolPress, AxialNodLen, Ifail, Index, GasAx(naxn+1), FuelSurfT, GasPress, VoverTRatio, &
          &          roi(1), vs0(1), ies(1), Kswell, DishVperL, FuelCenterT, vgap, CrackTemp, OpenPorTemp, &
          &          VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, tplenb, BottomPlenumGasMoles, &
          &          CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
        !
        ! Evaluate sum of T/V terms in ideal gas equation for gas gap
        ! Must be done same as VoverTRatio calculation in Subroutine gapprs
        Index = 3
        CALL gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, CrackVolume, &
          &          CoolPress, AxialNodLen, Ifail, Index, GasAx(naxn+1), FuelSurfT, GasPress, VoverTRatio, &
          &          roi(1), vs0(1), ies(1), Kswell, DishVperL, FuelCenterT, vgap, CrackTemp, OpenPorTemp, &
          &          VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, tplenb, BottomPlenumGasMoles, &
          &          CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
        
        ! Convert VoverTRatio from ft**3/K to ft**3/R
        VoverTRatio = VoverTRatio / cnvkr
        ToverVRatio = 1.0_r8k / VoverTRatio
        pp0 = (pmol * GasConstant * tfr(tplen) / vplenc) / psIft2
        ps0 = (smol * GasConstant / VoverTRatio) / psIft2
        
        ! Gas viscosity
        visco = MatProperty (Material='GAS', Property='VISCOSITY', Temperature=tfk(tgas), GasComposition=GasFraction) * convis
        
        ! Check to see if gap small. If so, set flow rate equal to zero
        alx = AxNodElevat(naxn) - AxNodElevat(Kswell)
        alx = 0.0_r8k
        !
        DO i = imx1, naxn
            tg = MAX(GapThick(i), gapmin)
            hyddm = 2.0_r8k * tg * 12.0_r8k
            IF (hyddm >= 0.001_r8k) THEN
                hagen = 22.0_r8k + 0.24558_r8k / (hyddm - 0.0007874_r8k)
            ELSE
                hagen = 1177.0_r8k
            ENDIF
            IF (hagen < 64.0_r8k) hagen = 64.0_r8k
            tg = MAX(GapThick(i), gapmin)
            alxx = 0.5_r8k * (AxNodElevat(i) - AxNodElevat(i-1))
            IF (i /= naxn) alxx = alxx + 0.5_r8k * (AxNodElevat(i+1) - AxNodElevat(i))
            drag = drag + alxx * tfr(GapTemp(i)) * hagen / ((2.0_r8k * PelletRad(i) + tg) * hyddm ** 3)
        ENDDO
        !
        IF (drag > drgmax) drag = drgmax
        alx = AxNodElevat(naxn) - AxNodElevat(Kswell)
        IF (alx <= 1.0e-20_r8k) alx = 0.5_r8k * (AxNodElevat(naxn) - AxNodElevat(naxn-1))
        ck = pi / (GasConstant * visc * drag)
        
        IF (ies(1) == 1) THEN
            
            ! Compute pressure in rod if at equilibrium. Keep pressure in swelling region from exceeding this pressure.
            Index = 1
            CALL gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, CrackVolume, &
              &          CoolPress, AxialNodLen, Ifail, Index, GasAx(naxn+1), FuelSurfT, GasPress, VoverTRatio, &
              &          roi(1), vs0(1), ies(1), Kswell, DishVperL, FuelCenterT, vgap, CrackTemp, OpenPorTemp, &
              &          VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, tplenb, BottomPlenumGasMoles, &
              &          CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
            !
            IF (GasPress(Kswell) >= GasPress(naxn+1)) THEN
                dvdtp = 0.0_r8k
                flowg(1) = 0.0_r8k
                RETURN
            END IF
            !
            ps = GasPress(Kswell) * psift2
            ap = ck * ps ** 2
            bp = 0.0_r8k
            cp = -ck * GasConstant ** 2 * (tfr(tplen) ** 2) / (vplenc ** 2)
            flow1 = -ck * ((pp0 * psIft2) ** 2 - (pswll0(1) * psIft2) ** 2)
            IF (flow1 > 0.0_r8k) flow1 = 0.0_r8k
            
        ELSE
            
            ! Flow is calculated in lb-moles/sec
            ! Amount of gas which flowed from plenum to swelled cladding region during time span dt1 will determined 
            ! by plugging values into analytical solution to gas flow equation
            ! evaluate constants in analytical solution
            ap = ck * ((GasConstant * gstlbs * ToverVRatio) ** 2)
            bp = -2.0_r8k * ck * gstlbs * ((GasConstant * ToverVRatio) ** 2)
            cp = -ck * (GasConstant ** 2) * ((tfr(tplen) / vplenc) ** 2 - ToverVRatio ** 2)
        END IF
        
        !
        q = 4.0_r8k * ap * cp - bp ** 2
        IF (q > 0.0_r8k) q = -1.0e-12_r8k
        sqrq = SQRT(-q)
        an = 2.0_r8k * cp * anp0 + bp - sqrq
        ad = 2.0_r8k * cp * anp0 + bp + sqrq
        IF (ad == 0.0_r8k) ad = 1.0e-20_r8k
        argln = ABS(an / ad)
        aln = LOG(argln)
        IF (ABS(ad) <= 1.1e-20_r8k) aln = expmax + 1.0_r8k - sqrq * dt1
        IF ((sqrq * dt1 + aln) > expmax) THEN
            ! Equation resulting if exv approaches infinity
            pmol = -(bp + sqrq) / (2.0_r8k * cp)
        ELSE
            exv = EXP(sqrq * dt1 + aln)
            IF (ABS(1.0_r8k - exv) < 1.0e-20_r8k) exv = 1.0_r8k + 1.0e-20_r8k
            pmol = (-bp + sqrq + (bp + sqrq) * exv) / (2.0_r8k * cp * (1.0_r8k - exv))
        END IF
        smol = (TotalGasMoles / grmlbs) - pmol
        GasAx(naxn+1) = pmol * grmlbs
        !
        Index = 2
        CALL gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, CrackVolume, &
          &          CoolPress, AxialNodLen, Ifail, Index, GasAx(naxn+1), FuelSurfT, GasPress, VoverTRatio, &
          &          roi(1), vs0(1), ies(1), Kswell, DishVperL, FuelCenterT, vgap, CrackTemp, OpenPorTemp, &
          &          VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, tplenb, BottomPlenumGasMoles, &
          &          CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
        !
        pp = GasPress(naxn+1) * psift2
        ps = GasPress(naxn+1) * psift2
        IF (ies(1) == 1) ps = GasPress(Kswell) * psift2
        flowg(1) = grmlbs * ck * (pp ** 2 - ps ** 2)
        gfloa1 = 4.0_r8k / (ck * TimeIncrement * GasConstant * tfr(GapTemp(Kswell)))
        ! pp0 is in psi, pp in psf, ps0 in psi
        dltap = pp - ps
        !
        DO i = Kswell, (naxn - 1)
            d = AxNodElevat(i+1) - AxNodElevat(Kswell)
            GasPress(i+1) = (ps + dltap * d / alx) / psIft2
        ENDDO
        !
        GasPress(1:naxn) = ps / psIft2
        !
        GasAx(naxn+1) = pmol * grmlbs
        GasAx(Kswell) = smol * grmlbs
        IF (ies(1) >= 1) THEN
            tgas = tfk(GapTemp(Kswell))
            dvdtp = 0.0_r8k
        ENDIF
        
    ELSE
        
        ! Steady-state gas pressure calculation same internal gas pressure assumed throughout rod
        Index = 1
        CALL gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, CrackVolume, &
          &          CoolPress, AxialNodLen, Ifail, Index, GasAx(naxn+1), FuelSurfT, GasPress, VoverTRatio, &
          &          roi(1), vs0(1), ies(1), Kswell, DishVperL, FuelCenterT, vgap, CrackTemp, OpenPorTemp, &
          &          VolOpenPor, TotalVoidVol, VoidVolumeRatio, vplenb, tplenb, BottomPlenumGasMoles, &
          &          CentVoidVol, roughc, roughf, AveDishTemp, TimeofGBSep)
        !
        GasPress(naxn+1) = GasPress(naxn)
        flowg(1) = 0.0_r8k

        ! Compute mass of gas in each volume
        IF (swllfr > em20) CALL swlchk (vplenc, GasPress, TotalGasMoles, GasAx, tplen, naxn, Kswell, k)
        
        IF (ies(1) == 1) THEN
            dvdtp = tfr(GapTemp(Kswell)) * ((TotalGasMoles / grmlbs) * GasConstant / (GasPress(naxn+1) * psift2) - &
              &     VoverTRatio / cnvkr)
            dvdtp = dvdtp / dt1
        END IF
        
    END IF
    
    END SUBROUTINE gsflow
    !
    !
    !
    SUBROUTINE gapprs (TotalGasMoles, vplenc, tplen, GapTemp, GapThick, PelletRad, naxn, pctop, &
      &                CrackVolume, CoolPress, AxialNodLen, Ifail, Index, PlenumGasMoles, FuelSurfT, &
      &                GasPress, VoverTRatio, rcEnd, vs0, iesloc, Kswell, DishVperL, FuelCenterT, &
      &                vgap, CrackTemp, OpenPorTemp, VolOpenPor, TotalVoidVol, VoidVolumeRatio, &
      &                vplenb, tplenb, BottomPlenumGasMoles, CentVoidVol, roughc, roughf, &
      &                AveDishTemp, TimeofGBSep)
    USE Kinds
    USE conversions_fraptran, ONLY : pi, tfk, tkf
    USE variables_fraptran, ONLY : ounit, explenumv, explenumt, npair, Time, ndebug, n_void_volumes
    USE collct_h
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes pressure along entire length of fuel rod gas gap - small fuel deformation assumed
    !>@author
    !> Modified by Ian Porter, April 2014.
    !
    ! TotalGasMoles is output, all other variables in argument list are input or dummy
    ! TotalGasMoles        - total moles of gas in plenum and gas gap
    ! vplenc               - fuel rod plenum volume (ft**3)
    ! tplen                - plenum temperature (F)
    ! GapTemp(k)           - gap temperature at axial node k (F)
    ! GapThick(k)          - gap thickness at axial node k (ft)
    ! PelletRad(k)         - outside radius of fuel at axial node k (ft)
    ! naxn                 - number of axial nodes
    ! pctop                - coolant pressure at top of rod (psia)
    ! CrackVolume(k)       - volume of fuel radial cracks per unit length(ft3/ft)
    ! CoolPress(k)         - coolant pressure at axial node k (psia)
    ! AxialNodLen(k)       - length associated with axial node k (ft)
    ! PlenumGasMoles       - gram-moles of gas in plenum
    ! Index                - Indicator. (1 = Steady-State, 2 = Transient, 5 = # of moles of gas in fuel rod computed)
    ! FuelSurfT(k)         - fuel surface temperature at axial node k (F)
    ! DishVperL(k)         - dish volume per unit length at axial node k (ft**3/ft)
    !                        *If central void at axial node k, DishVperL(k) contains void volume per unit length at axial node k*
    ! FuelCenterT(k)       - fuel centerline temperature (F)
    ! Output
    ! GasPress(k)          - fuel rod gas pressure at axial node k (psia)
    ! VoverTRatio          - sum of volume over temperature terms in gas gap pressure equation (ft**3/K)
    ! rcend                - cladding inside radius at Ends of swelling region (ft)  (input)
    ! vs0                  - volume generated by localized clad swelling (ft**3) (input)  value at start of time step
    ! iesloc               - swelling indicator (0 = No, 1 = Yes)  (input)
    ! Kswell               - axial node number at which localized swelling is occurring (input)
    ! vgap                 - gas volume excluding plenum and localized bulging (ft**3)
    ! CrackTemp(k)         - temperature of gas in fuel cracks at axial node k (F)
    ! OpenPorTemp(k)       - average temperature of gas in fuel Open pores (F)
    ! VolOpenPor(k)        - volume of fuel Open pores per unit length (ft3/ft)
    ! TotalVoidVol         - total volume occupied by gas in fuel rod (ft**3)
    ! VoidVolumeRatio(1)   - ratio of plenum volume to total gas volume
    ! VoidVolumeRatio(2)   - ratio of crack volume to total gas volume
    ! VoidVolumeRatio(3)   - ratio of gap volume to total gas volume
    ! VoidVolumeRatio(4)   - ratio of pores volume to total gas volume
    ! VoidVolumeRatio(5)   - ratio of dish volume to total gas volume
    ! VoidVolumeRatio(6)   - ratio of lower plenum vol. to total gas vol.
    ! VoidVolumeRatio(7)   - ratio of central void vol. to total vol.
    ! VoidVolumeRatio(8)   - ratio of fuel surface roughness vol. to total
    ! VoidVolumeRatio(9)   - ratio of clad surface roughness vol. to total
    ! VoidVolumeRatio(10)  - ratio of external plenum volume to total
    ! vplenb               - volume of bottom plenum  (ft**3)
    ! tplenb               - gas temperature in bottom plenum (F)
    ! BottomPlenumGasMoles - gram-moles of gas in bottom plenum  (gram-moles)
    ! CentVoidVol(k)       - volume of central void at axial node k  (ft3/ft)
    ! roughc               - arithmetic mean surface roughness of cladding (cm)
    ! roughf               - arithmetic mean surface roughness of fuel     (cm)
    ! AveDishTemp(k)       - area averaged pellet dish temperature, node k (F)
    ! TimeofGBSep(k)       - time at which grain boundary separation occurred at axial node k (s) disintegrated (psia).
    ! alpdis               - volume fraction of fuel
    !
    INTEGER(ipk) :: k, i
    INTEGER(ipk), INTENT(IN) :: ifail, iesloc, naxn, kswell, index
    REAL(r8k) :: gasdsm, tdisin, treper, tdiseq, aphdis, vgap, vovertratio, volcrk, volgap,&
      &          volopp, voldsh, rsc, roughc, rsf, roughf, volvd, volrf, volcf, pgpmin, deltl,&
      &          tm,tcrk, top,  vdish, tdish, fuelsurftk, tvoid, tcldif, tcladi, r1, r2, vgg, &
      &          vop, vrc, rf1, rc1, vfsr, vcsr, tmb, vplenb, totalvoidvol, vplenc, vs0, tplenb, &
      &          total, totalgasmoles, vplen, tplen, votswl, p, gastot, tdisnfrincld, voldis, &
      &          treles, gasdis, votbot, bottomplenumgasmoles, gapgasmoles, plenumgasmoles, &
      &          pctop, rcend, tdisnf, rincld
    REAL(r8k), PARAMETER :: atmpsi = 14.6969_r8k
    REAL(r8k), PARAMETER :: ftl = 0.035316_r8k
    REAL(r8k), PARAMETER :: UniversalGasConstant = 0.08205_r8k
    REAL(r8k), PARAMETER :: vmin = 1.0e-20_r8k
    REAL(r8k), PARAMETER :: cnvcm = 0.0328084_r8k  ! conversion factor from cm to feet = cnvcm
    REAL(r8k), PARAMETER :: cnvmol = 304.8006_r8k ! cmvmol = conversion of kg-moles/m of gas to gram-moles/ft
    REAL(r8k), DIMENSION(:) :: GapTemp, GapThick, CoolPress, GasPress, PelletRad, CrackVolume, AxialNodLen, FuelSurfT, &
      &                        DishVperL, FuelCenterT, CrackTemp, AveDishTemp, OpenPorTemp, VolOpenPor, CentVoidVol, &
      &                        VoidVolumeRatio, TimeofGBSep

    ! Check for cladding rupture
    IF (Ifail == 1) THEN
        ! Cladding ruptured. Pressure inside cladding assumed equal to coolant pressure
        GasPress(1:naxn) = CoolPress(k)
        GasPress(naxn+1) = CoolPress(naxn)
        VoidVolumeRatio(1:5) = 0.0_r8k
        RETURN
    END IF
    
    ! gasdsm = gas at grain boundaries (kg-moles/m length). For burnup of 50 MWd/kg.U, gasdsm ~ 5.0e-4
    gasdsm = gbse(1)
    ! tdisin = Pellet surface temperature at which grains disintegrate (K)
    tdisin = gbse(2)
    ! treper = time through grain disintegration process occurs (s)
    treper = gbse(3)
    ! tdiseq = time for gas pressure due to grain boundary disintegration to equilibriate with plenum pressure (s)
    tdiseq = gbse(4)
    ! aphdis = initial fraction of volume of fuel that contained fission that are released due to grain disintegration
    aphdis = gbse(5)
    
    ! Debugging
    IF (ndebug) THEN
        WRITE(ounit,*) ' GAPPRS: Time = ',Time
9007    FORMAT(8(2x,e11.4))
        WRITE(ounit,9009)
9009    FORMAT('  TimeofGBSep array')
        WRITE(ounit,9007) (TimeofGBSep(k),k = 1,naxn)
        WRITE(ounit,9011)
9011    FORMAT('  FuelSurfT array')
        WRITE(ounit,9007) (FuelSurfT(k),k = 1,naxn)
        WRITE(ounit,9016)
9016    FORMAT('  GasPress array')
        WRITE(ounit,9007) (GasPress(k),k = 1,naxn)
        WRITE(ounit,9018)
9018    FORMAT('  GapThick array')
        WRITE(ounit,9007) (GapThick(k),k = 1,naxn)
        WRITE(ounit,9020)
9020    FORMAT('  PelletRad array')
        WRITE(ounit,9007) (PelletRad(k),k = 1,naxn)
    ENDIF
    
    ! Fuel rod gas pressure computed using ideal gas law
    ! Compute summation of gap volume/gap temperature terms. convert temperature from F to K
    vgap = 0.0_r8k
    VoverTRatio = 0.0_r8k
    volcrk = 0.0_r8k
    volgap = 0.0_r8k
    volopp = 0.0_r8k
    voldsh = 0.0_r8k
    ! Convert surface roughness from cm to ft
    rsc = cnvcm * roughc
    rsf = cnvcm * roughf
    volvd = 0.0_r8k
    volrf = 0.0_r8k
    volcf = 0.0_r8k
    pgpmin = 1.0e20_r8k
    !
    DO k = 1, naxn
        IF (GasPress(k) < pgpmin) pgpmin = GasPress(k)
        deltl = AxialNodLen(k)
        tm = tfk(GapTemp(k))
        tcrk = tfk(CrackTemp(k))
        top = tfk(OpenPorTemp(k))
        vdish = DishVperL(k) * deltl
        tdish = tfk(AveDishTemp(k))
        FuelSurfTk = tfk(FuelSurfT(k))
        tvoid = tfk(FuelCenterT(k))
        tcldIf = (GapTemp(k) - 0.5_r8k * FuelSurfT(k)) / 0.5_r8k
        tcladi = tfk(tcldIf)
        r1 = PelletRad(k)
        r2 = PelletRad(k) + GapThick(k)
        vgg = pi * (r2 ** 2 - r1 ** 2) * deltl
        vop = VolOpenPor(k) * deltl
        vrc = CrackVolume(k) * deltl
        volcrk = volcrk + vrc
        volgap = volgap + vgg
        volopp = volopp + vop
        ! Add on volume due to surface roughness
        rf1 = r1 - rsf
        rc1 = r2 + rsc
        vfsr = pi * (r1 ** 2 - rf1 ** 2) * deltl
        vcsr = pi * (rc1 ** 2 - r2 ** 2) * deltl
        volrf = volrf + vfsr
        volcf = volcf + vcsr
        voldsh = voldsh + vdish
        volvd = volvd  + CentVoidVol(k) * deltl
        VoverTRatio = VoverTRatio + vgg / tm + vrc / tcrk + vdish / tdish + vop / top + &
          &           (CentVoidVol(k) * deltl) / tvoid + vfsr / FuelSurfTk + vcsr / tcladi
        vgap = vgg + vrc + vdish + vop + vgap + CentVoidVol(k) * deltl + vfsr + vcsr
        IF (ndebug) WRITE(ounit,901) k, vgg, tm, vrc, tcrk, vdish, tdish, vop, top
901     FORMAT(' k = ',i3,' vgg = ',e11.4,' tm = ',e11.4,' vrc = ',e11.4,' tcrk = ',e11.4/' vdish = ',e11.4, &
          &    ' tdish = ',e11.4,' vop = ', e11.4,' top = ',e11.4)
    ENDDO
    tmb = tfk(tplenb)
    VoverTRatio = VoverTRatio + vplenb / tmb
    TotalVoidVol = vgap + vplenc + vplenb + explenumv
    IF (iesloc == 1) THEN
        ! Add on localized swelling contribution
        VoverTRatio = VoverTRatio + vs0 / tfk(GapTemp(Kswell))
        TotalVoidVol = TotalVoidVol + vs0
    ENDIF
    IF (Index == 3) RETURN
    !
    VoidVolumeRatio(1) = vplenc / TotalVoidVol
    VoidVolumeRatio(2) = volcrk / TotalVoidVol
    VoidVolumeRatio(3) = volgap / TotalVoidVol
    VoidVolumeRatio(4) = volopp / TotalVoidVol
    VoidVolumeRatio(5) = voldsh / TotalVoidVol
    VoidVolumeRatio(6) = vplenb / TotalVoidVol
    VoidVolumeRatio(7) = volvd / TotalVoidVol
    VoidVolumeRatio(8) = volrf / TotalVoidVol
    VoidVolumeRatio(9) = volcf / TotalVoidVol
    VoidVolumeRatio(10) = explenumv / TotalVoidVol
    Total = SUM(VoidVolumeRatio(1:n_void_volumes))
    
    ! Error check
    IF (ABS(Total - 1.0_r8k) >= 1.0e-10_r8k) THEN
        WRITE(ounit,800) Total
800     FORMAT('The sum of the void volume ratios is not equal to 1.0. Sum = ',es12.4, &
          &    'Execution terminated in Subroutine: gapprs')
        ERROR STOP 'The sum of the void volume ratios is not equal to 1.0. Execution terminated in Subroutine: gapprs'
    ENDIF
    
    ! Transient gas flow
    IF (Index == 2) THEN
        GapGasMoles = TotalGasMoles - PlenumGasMoles
        VoverTRatio = VoverTRatio / ftl
        GasPress(1:naxn) = (GapGasMoles * UniversalGasConstant / VoverTRatio) * atmpsi
        GasPress(naxn+1) = (PlenumGasMoles * UniversalGasConstant * (tfk(tplen)) / (vplenc / ftl)) * atmpsi
        IF (ndebug) THEN
            WRITE(ounit,905) Index
            WRITE(ounit,907) (GasPress(k),k = 1,naxn)
        ENDIF
        RETURN
    END IF
    
    ! Debugging
    IF (ndebug) WRITE(ounit,903) vplenc, tm, TotalGasMoles
903 FORMAT(' vplen = ',e11.4,' tm = ',e11.4,2x,' TotalGasMoles = ',e11.4)
    
    ! Add plenum term
    vplen = MAX(vplenc, vmin)
    tm = tfk(tplen)

    SELECT CASE (index)
    CASE (5)
        VoverTRatio = VoverTRatio + vplen / tm + explenumv / tm
        votswl = VoverTRatio
        ! Convert from ft**3 to liters
        VoverTRatio = VoverTRatio / ftl
        p = GasPress(1) / atmpsi
        TotalGasMoles = p * VoverTRatio / UniversalGasConstant
        RETURN
    CASE DEFAULT
        VoverTRatio = VoverTRatio + vplen / tm + explenumv / tfk(polate(explenumt,Time,npair))
        votswl = VoverTRatio
        ! Convert from ft**3 to liters
        VoverTRatio = VoverTRatio / ftl
        IF (Index == 4) RETURN
    END SELECT
    
    ! Compute pressure in atm
    p = TotalGasMoles * UniversalGasConstant / VoverTRatio
    ! convert to psia
    p = p * atmpsi
    p = MAX(0.0_r8k, p)
    GasPress(1:naxn) = p
    !***  TESTING
    ! convert gas inventory from kg-moles/m to gram-moles/ft
    gastot = cnvmol * gasdsm
    ! Calculate cumulative release from grain boundaries and grains.
    DO k = 1, naxn
        ! Convert disintegration temperature from K to F
        tdisnf = tkf(tdisin)
        IF (TimeofGBSep(k) <= 1.0e-6_r8k .AND. FuelSurfT(k) > tdisnf) THEN
            ! grain boundaries shatter
            TimeofGBSep(k) = Time
            WRITE(ounit,9012) Time, k
9012        FORMAT(' GAPPRS - grain boundary disintegration occurs Time  = ',e13.6,' axial node  = ',i3)
            WRITE(ounit,9014) FuelSurfT(k)
9014        FORMAT(' temperature of pellet surfsace (F)  = ',e11.4)
        ENDIF
    ENDDO
    
    ! 
    DO k = 1, naxn
        IF (TimeofGBSep(k) >= 1.0e-6_r8k .AND. (Time - TimeofGBSep(k)) <= tdiseq) THEN
            rincld = PelletRad(k) + GapThick(k)
            voldis = pi * AxialNodLen(k) * (rincld ** 2 - PelletRad(k) ** 2)
            ! Add on initial volume
            ! aphdis = initial volume fraction of fuel containing gas at grain boundaries.
            voldis = voldis + aphdis * pi * AxialNodLen(k) * PelletRad(k) ** 2
            ! Convert volume for released gas from ft3 to liters
            voldis = voldis / ftl
            ! Convert temperature from F to K.
            tm = tfk(FuelSurfT(k))
            ! Calculate cumulative release of gas from fuel.
            treles = (Time - TimeofGBSep(k)) / treper
            IF (treles > 1.0_r8k) treles = 1.0_r8k
            gasdis = treles * gastot
            ! Calculate gas release at axial node, so multiply by height of node.
            gasdis = gasdis * AxialNodLen(k)
            GasPress(k) = UniversalGasConstant * gasdis / (voldis / tm)
            ! convert pressure to psia units
            GasPress(k) = GasPress(k) * atmpsi
            IF (GasPress(k) < pgpmin) GasPress(k) = pgpmin
            IF (GasPress(k) > 20.0e3_r8k) GasPress(k) = 20.0e3_r8k
        ENDIF
    ENDDO
    
    ! Calculate local gas pressure
    VoverTRatio = votswl
    
    ! Compute moles of gas in bottom plenum
    IF (vplenb > 1.0e-10_r8k) THEN
        votbot = vplenb / tmb
        votbot = votbot / ftl
        BottomPlenumGasMoles = (p / atmpsi) * votbot / UniversalGasConstant
    ELSE
        BottomPlenumGasMoles = 0.0_r8k
    ENDIF
    
    ! Debugging
    IF (ndebug) THEN
        WRITE(ounit,905) Index
905     FORMAT(' calculated gap pressure Index = ',i3)
        WRITE(ounit,907) (GasPress(k),k = 1,naxn)
907     FORMAT(8(2x,e11.4))
    ENDIF
    ! 
    END SUBROUTINE gapprs
    !
    !
    !
    SUBROUTINE swlchk (vplenc, GasPress, TotalGasMoles, GasAx, tplen, naxn, Kswell, k)
    USE Kinds
    USE conversions_fraptran, ONLY : tfk
    IMPLICIT NONE
    !>@brief
    !> Subroutine checks to see if local fuel rod swelling has occurred. If swelling has occurred, 
    !> compute moles of gas associated with each axial node for initial conditions to gas flow code.
    !
    ! Input
    !
    ! vplenc        -  plenum volume (ft**3)
    ! GasPress(k)   -  gas pressure at axial node k (psia)
    ! naxn          -  number of axial nodes
    ! k             -  axial node number
    ! TotalGasMoles -  total moles of gas in fuel rod
    ! tplen         -  temperature of gas in plenum (F)
    !
    ! Output
    !
    ! GasAx(k)      -  moles of gas associated with axial node k
    ! GasAx(naxn+1) -  moles of gas in plenum
    ! Kswell        -  axial node number where swelling occurred. (0 = No local swelling has occurred)
    !
    INTEGER(ipk), INTENT(IN) :: naxn, k
    INTEGER(ipk), INTENT(OUT) :: Kswell
    REAL(r8k) :: tgas, vgasp, p
    REAL(r8k), INTENT(IN) :: vplenc, TotalGasMoles, tplen
    REAL(r8k), DIMENSION(:), INTENT(IN) :: GasPress
    REAL(r8k), DIMENSION(:), INTENT(OUT) :: GasAx
    REAL(r8k), PARAMETER :: ftl = 0.035316_r8k
    REAL(r8k), PARAMETER :: R = 0.08205_r8k
    REAL(r8k), PARAMETER :: atmpsi = 14.6969_r8k
    !
    Kswell = naxn + 1
    
    ! Initialize moles of gas at each axial node
    tgas = tfk (tplen)
    vgasp = vplenc / ftl
    p = GasPress(naxn) / atmpsi
    GasAx(naxn+1) = p * vgasp / (R * tgas)
    GasAx(Kswell) = TotalGasMoles - GasAx(naxn+1)
    !
    END SUBROUTINE swlchk
    !
END MODULE void_fraptran












