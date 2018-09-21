MODULE HeatSolution_fraptran
    USE Kinds_fraptran
    USE Oxidation_fraptran, ONLY : metwtb, chitox
    USE HeatTransferCoefficient_fraptran, ONLY : htrc, gaphtc
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE heat (Gscale)
    USE Kinds_fraptran
    USE conversions_fraptran
    USE GammaHeating_fraptran
    USE variables_fraptran
    USE Uncertainty_Vals_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE HeatCond_fraptran
    USE Coolant_fraptran
    USE RadialNodes_fraptran, ONLY : Radheatsource, weights
    USE AxialPower_fraptran, ONLY : power
    IMPLICIT NONE
    !
    !>@brief
    !>This is the top level subroutine for calculation of fuel rod temperature distribution
    !
    ! Input
    !
    ! Gscale - Scale factor for rod average LHGR
    !
    INTEGER(ipk) :: zzz, kthta, k, j, jchan, ntheta, nza, kz, kt, nvoids, kfail, nchfsw, iht, ibypass, &
      &             l, ngaps, ngapi, npowch, m, mpmax, kk, iii, iiip1, iiim2, nphgap = 0
    REAL(r8k) :: theta, tmpacd, delz, achnl, qcool, dthr, pkw, powr, powr0, pkw0, &
      &          voidtk, voidk, voidcp, qctot, tsurf, tsurfa, tsurf0, dmw11, drodmw, drmax, dmw22, &
      &          emetal, emeti, buoxide, tsi, tsi0, drodmi, vmwi, tk1, tk2, w1, drdmw2, w2, tsurfs, &
      &          hcoefs, qcrits, ps, hflux, tempcm, hcoef, qcrit, frac, conoxide, &
      &          zroft, zroi, roufih, roucih, ftemp, ctemp, ctempk, gptemp, frmlt, rci, &
      &          angle, dofst, cosang, tshftm, pshftm, fngaps, frgseg, cruf, fnx, tgshft, pofset, &
      &          frcsum, hgpsum, gasgpi, pfci, fulori, bulocal, hgap, hgpmax, rgpmid, hgplbd, dhidho, &
      &          hgapgs, fitcnt, hgpmxi, hgapmn, pmax, totpow, afuel, aclad, tsatk, cpfuel, &
      &          cpclad, tmpacc, tmpact, rt, asum, dps, hfluxs, sum, fecool,Gscale, &
      &          DefClID, DefClOD, DefFuRd, DefVoRd, gpthkt, rhoce, rhofe, dum1=0.0_r8k, dum2=0.0_r8k
    REAL(r8k), SAVE :: tsat
    REAL(r8k), PARAMETER :: dloxi = 0.25_r8k
    REAL(r8k), PARAMETER :: em03 = 1.0e-3_r8k
    REAL(r8k), PARAMETER :: em20 = 1.0e-20_r8k
    REAL(r8k), PARAMETER :: gapcv = 0.012_r8k
    REAL(r8k), PARAMETER :: thcond = 6.2306448e3_r8k
    REAL(r8k), PARAMETER :: thkoxm = 2.5e-7_r8k
    REAL(r8k), PARAMETER :: two = 2.0_r8k
    REAL(r8k), DIMENSION(naxn) :: delgap, delpfc, frcgap, hgapin1, hgapin2, hgapout1, hgapout2
    REAL(r8k), DIMENSION(nmesh) :: HeatFlow, rwa1
    
    ! Begin loop which steps up each axial node k. This loop computes temperature distribution at each axial node 
    kthta = 0
    hfluxs = 0.0_r8k
    sum = 0.0_r8k
    !
    DO k = 1, naxn
        hfluxs = hfluxs + HeatFlux0(k) * AxialNodLen(k)
        sum = sum + AxialNodLen(k)
    ENDDO
    htflxa = hfluxs / sum
    j = 1
    jchan = 1
    Nchan = nrc(jchan,j)

    ! Convert surface roughness to inches and define minimum gap (ft)
    roufih = roughf / 2.54_r8k
    roucih = roughc / 2.54_r8k
    cruf   = roufih + roucih
    gapmin = MAX(cruf, 0.5e-05_r8k) / 12.0_r8k
    !
    DO k = 1, naxn
        !
        !LOJ qt15: Modifications to cater for effects of fuel rod deformation 
        !          on heat transfer. Use deformed geometry dimensions_fraptran, 
        !          and re-calculate weights and radial heat source.
        !
        ! Key deformed dimensions of the fuel rod
        DefClID = 2.0_r8k * EOSRad(ncladi,k) ! Clad inner diameter
        DefClOD = 2.0_r8k * EOSRad(nmesh,k)  ! Clad outer diameter
        DefFuRd = EOSRad(igpnod,k)     ! Fuel pellet (outer) radius
        DefVoRd = EOSRad(2,k)          ! Fuel pellet centre void radius
        !
        ! True pellet-clad gap thickness
        gpthkt = MAX(EOSRad(ncladi,k) - EOSRad(igpnod,k), gapmin)
        
        ! Check to see if central void exists in the pellet. Also calculate 
        ! effective fuel pellet density, considering radial fuel deformation:
         IF ((AxNodElevat(k) >= zvoid1) .AND. (AxNodElevat(k) <= zvoid2)) THEN
            nvoids = 1
            rhofe = rhof * (RadialBound(igpnod) ** 2 - RadialBound(2) ** 2) / (DefFuRd ** 2 - DefVoRd ** 2)
         ELSE
            nvoids = 0 
            rhofe = rhof * (RadialBound(igpnod) / DefFuRd) ** 2
         ENDIF
        
        ! Calculate effective cladding tube density, considering radial deformation:
        rhoce = rhoc * (RadialBound(nmesh) ** 2 - RadialBound(ncladi) ** 2) * 4.0_r8k / (DefClOD ** 2 - DefClID ** 2)
        
        ! Volumetric and area weights for the deformed configuration
        CALL Weights (nmesh, gapmin, EOSRad(:,k), VolumeWeightL(:,k), VolumeWeightR(:,k), AreaWeight(:,k))
        
        ! Radial distribution of heat source in deformed pellet and cladding
        CALL Radheatsource (nmesh, igpnod, nvoid, k, CladdingPower(k), AxNodElevat(k), zvoid1, zvoid2, rhofe/rhof, &
          &                 EOSRad(:,k), VolumeWeightL, VolumeWeightR, radsrco, radsrc)
        
        ! Setup calculation type
        SELECT CASE (ndim)
        CASE (0) ! Stacked 1-D radial calculations
            ntheta = 1
            nza = 1
        CASE (1) ! r-theta calculations at specified axial nodes
            ! IF NumAzmuthNod(k)>1, r-theta calculations at axial node k
            nza = 1
            ntheta = NumAzmuthNod(k)
            IF (nswmd == 0) ntheta = 1
            IF (ntheta > 1) IndexTempConverg(k) = 0
        CASE (2) ! Floating refined mesh r-z calculation
            ntheta = 1
            nza = 1
            IF (k == kbot) nza = nzmesh
            IF (nswmd == 0) nza = 1
        CASE DEFAULT ! Floating r-theta-z calculations with r-theta at each axial node
            ntheta = NumAzmuthNod(k)
            nza = 1
            IF (k == 1) nza = nzmesh
            IF (nswmd == 0) THEN
                nza = 1
                ntheta = 1
            ENDIF
        END SELECT
        IF (ntheta > 1) kthta = kthta + 1
        ! Begin loop over axial sub-segments
        DO kz = 1, nza
            IF (nza > 1) THEN
                z = zbot + (kz - 1) * dzmesh
            ELSE
                z = AxNodElevat(k)
            ENDIF
            ! dzmesh = mesh axial spacing in refined floating r-z calculations
            ! zbot   = elevation of bottom edge of refined mesh calculations
            DO kt = 1, ntheta
                theta = (kt - 1) * dtheta
                !
                ! determine coolant conditions surrounding fuel rods
                ! IF into the refill-reflood mode, skip the Call to cool
                !
                IF (time0 > empytm) gum = 0.0_r8k
                IF (time0 > empytm) GOTO 455
                tmpacd = tmpac1
                IF (ndtred >= 1) tmpacd = 1.0e20_r8k
                IF (IterationCount /= 2 .AND. tmpacd < 1.0_r8k) GOTO 450
                IF (ntstep == 1 .AND. IterationCount /= 2) GOTO 450
                IF (Time < (tpo - em20) .AND. Time < tmax) GOTO 450
                IF (ntstep == 0) GOTO 450
                ! IF ncall < 0, coolant conditions printed
                IF (kt == 1) ncall = - ncall
450             CONTINUE
                !
                ! IF coolant conditions not calculated by FrapTran,
                ! bypass calculation of heat input to coolant
                !
                IF (nqchn == 7) GOTO 451
                IF (nqchn /= 0) GOTO 452
451             CONTINUE
                IF (k == 1) THEN
                    delz = AxNodElevat(k)
                ELSE
                    delz = AxNodElevat(k) - AxNodElevat(k-1)
                ENDIF
                achn = acond(9,Nchan)
                ! LOJ qt15: Use deformed rod geometry _fraptran(Previously used RodOD(k) rather than DefClOD)
                achnl = achn / (pi * DefClOD)
                ! Determine the moderator heating fraction. It will come from either (by rank):
                ! (1) the user, (2) Using the built-in coolant density correlation, (3) the original default value of 0.02.
                SELECT CASE (ModHeatModel)
                CASE ('CoolantDensity')
                    fecool = GammaCoolant(CoolDensity(k))
                CASE ('UserValue')
                    fecool = modheat
                CASE DEFAULT
                    fecool = 0.02_r8k
                END SELECT
                !
                IF (k == 1) THEN ! The power is assumed to be zero at elevation z = 0.0
                    qcool = sechr * HeatFlux(k) / (2.0_r8k * achnl) + btuhkw * fecool * AxialPowr(k) / (2.0_r8k * achn)
                ELSE
                    qcool = sechr * (HeatFlux(k) + HeatFlux(k-1)) / (2.0_r8k * achnl) + btuhkw * fecool * &
                      &     (AxialPowr(k) + AxialPowr(k-1)) / (2.0_r8k * achn)
                ENDIF
                dthr = TimeIncrement / sechr
452             CONTINUE
                !
                CALL cool (k, theta, rl, qcool, delz, dthr, tpo)
455             CONTINUE
457             IF (ndebug) WRITE(ounit,936) Time, theta, acond(12,Nchan), acond(19,Nchan), acond(16,Nchan)
936             FORMAT(6x,' after cool call, Time = ',e11.4,' theta = ', e11.4,' g = ',e11.4,' x = ',e11.4,' p = ',e11.4)
                !
                IF (Time >= empytm) gum = 0.0_r8k
                rmassflux(k) = gum
                coolqual(k) = acond(19,Nchan)
                BulkCoolTemp(k) = acond(17,Nchan)
                ncall = 1
                IF (IndexTempConverg(k) == 0) GOTO 270
                ! Determine steady state heat generation source in each fuel rod at each axial node
                !
                CALL power (nptha, npaxp, z, Time, TimeIncrement, DefFuRd, pkw, powr, powr0, &
                  &         powave(j), powop, timop, fpdcay, mpdcay, NSteadyTrans, powict, powimx, &
                  &         ntstep, ExtentOfBow(k), nqbow, pkw0, Gscale, dum1, dum2, DefVoRd)
                !
                IF (NSteadyTrans == 1) GOTO 23
                IF (ntheta > 1 .OR. nza > 1) GOTO 26
                !
23              CONTINUE
                HeatFlow(1:nmesh) = 0.0_r8k
                GOTO 22
                ! HeatFlow(l)= net heat flow by conduction into mesh interval l (btu/ft**3-sec)
                ! Check to see IF central void
26              CONTINUE
                IF (nvoid >= 1) THEN
                    IF (nvoids == 1) THEN
                        ! Void thermal conductivity
                        voidk =  MatProperty (Material='GAS', Property='THERMCOND', Temperature=tfk(EOSTemp(1,k)), &
                          &                   Pressure=(GasPress(k)/psinm2), GasComposition=GasFraction) / thcond
                        ! Void specific heat ! NOTE: This is a fixed value but should be calculated! IP 5/17/2016
                        voidcp = gapcv
                        ! To not reduce size of stable time step,set rho*cp of gas to about that of uo2
                        voidcp = 4.2e3_r8k * voidcp
                        ! Should use this _fraptran(with proper unit conversion)
                        !voidcp = MatProperty (Material='GAS', Property='SPECHEAT', Temperature=tfk(EOSTemp(1,k)), &
                        !  &                   Pressure=(GasPress(k)/psinm2), GasComposition=GasFraction)
                        ! End of note
                    ENDIF
                ENDIF
                !
                CALL qcon (a1, indxjk, EOSRad(:,k), dtheta, nmesh, igpnod, ncladi, naxn, naz, &
                  &        nzmesh, kt, kz, ndim, nsymm, IterationCount, nnaz, kthta, k, j, HeatFlow, &
                  &        ntstep, tmelt(1), rhofe, TimeIncrement, nvoids, voidcp, fotmtl, frden, &
                  &        qcold, qctot, gadolin, burad)
                !
22              CONTINUE
                IF (IterationCount <= 1 .AND. ntstep <= 1) SurfHtFlux(k) = powcnv * sechr * pkw / (pi * DefClOD)
                AxialPowr(k)  = pkw
                AxialPowr0(k) = pkw0
                Ih(k) = Ihtreg(k)
                ! Specify initial temperature distribution of rod of axial node k
                ! NSteadyTrans = 1, steady state; NSteadyTrans = 2, transient
                IF (NSteadyTrans == 1) GOTO 62
                DO l = 1, nmesh
                    tsurf = BOSTemp(l,k)
                    PrevTemp(l) = tsurf
                    tsurfa = EOSTemp(l,k)
                    rwa1(l) = tsurfa
                ENDDO
                deltox(k) = 0.0_r8k
                ! Compute metal-water chemical reaction effects
                IF (modmw /= 2) GOTO 640
                ! Baker-Just oxidation model (METWB) - to be used for licensing calculations
                tsurf = EOSTemp(nmesh,k)
                tsurf0 = BOSTemp(nmesh,k)
                dmw11 = BOSOxideThick(k)
                IF (ProtectiveOxide == 1) dmw11 = BOSOxideThick(k) - oxideod(k) / 0.0254_r8k
                drodmw = DefClOD
                ! Baker-Just model computes thickness of reacted zr, while the cathcart-pawel model computes thickness of zro2.
                drmax = RadialBound(nmesh) - RadialBound(ncladi) - OxiThk1(k) / 12.0_r8k
                !
                CALL metwtb (tsurf0, tsurf, dmw11, dmw22, drodmw, TimeIncrement, emetal, drmax)
                !
                EOSOxideThick(k) = dmw22
                IF (ProtectiveOxide == 1) EOSOxideThick(k) = dmw22 + oxideod(k) / 0.0254_r8k
                WatrMetlEnrgy(k) = emetal / powcnv
                ! Check to see if metal-water reaction occur at inside surface of cladding.
                ! Ifaila stores node at which failure occurred.
                emeti = 0.0_r8k
                IF (Ifaila < 1 .AND. nIDoxide == 0) GOTO 64
                kfail = ifaila
                ! Failed Cladding
                IF (ifaila > 0) THEN
                    ! Check to see if node k is within the zone that receives water through the cladding breach
                    IF (AxNodElevat(k) < (AxNodElevat(kfail) - dloxi) .OR. AxNodElevat(k) > (AxNodElevat(kfail) + dloxi)) GOTO 777
                    drmax = RadialBound(nmesh) - RadialBound(ncladi) - EOSOxideThick(k) / 12.0_r8k
                    tsi = EOSTemp(ncladi,k)
                    tsi0 = BOSTemp(ncladi,k)
                    dmw11 = OxiThk1(k)
                    ! Use deformed inner diameter_fraptran
                    drodmi = DefClID
                    !
                    CALL metwtb (tsi0, tsi, dmw11, dmw22, drodmi, TimeIncrement, emeti, drmax)
                    !
                    OxiThk2(k) = dmw22
                    WatrMetlEnrgy(k) = WatrMetlEnrgy(k) + emeti / powcnv
                    ! Convert metal-water reaction energy to btu/ft**3-sec of energy generation concentrated 
                    ! in mesh bordering cladding surface
                    ! Consider cladding deformation
                    vmwi = pi * (EOSRad(ncladi+1,k) ** 2 - EOSRad(ncladi,k) ** 2)
                    emeti = emeti / vmwi
                    GOTO 64
                    !
777                 CONTINUE
                    ! High-burnup oxidation:
                ELSE IF ((nIDoxide == 1) .AND. (AxBurnup(k) / 86400.0_r8k >= BuOxide)) THEN
                    drmax  = RadialBound(nmesh) - RadialBound(ncladi) - EOSOxideThick(k) / 12.0_r8k
                    tsi    = EOSTemp(ncladi,k)
                    tsi0   = BOSTemp(ncladi,k)
                    dmw11  = OxiThk1(k)
                    !
                    ! Use deformed inner diameter_fraptran
                    drodmi = DefClID
                    call metwtb (tsi0, tsi, dmw11, dmw22, drodmi, TimeIncrement, emeti, drmax)
                    !
                    OxiThk2(k) = dmw22
                    WatrMetlEnrgy(k) = WatrMetlEnrgy(k) + emeti / powcnv
                    ! Convert metal-water reaction energy to btu/ft**3-sec of energy
                    ! generation concentrated in mesh bordering cladding surface
                    ! Consider cladding deformation
                    vmwi = pi * (EOSRad(ncladi+1,k) ** 2 - EOSRad(ncladi,k) ** 2)
                    !
                    emeti = emeti / vmwi
                ENDIF
                GO TO 64
640             CONTINUE
                ! Cathcart-Pawel equations/calculation
                ! Convert oxidation energy from watts/m to kW/ft
                emetal = powcnv * OxiPowerGen(k) / (3.28084_r8k * 1000.0_r8k)
                WatrMetlEnrgy(k) = emetal / powcnv
                emeti = 0.0_r8k
                IF (Ifaila < 1 .AND. nIDoxide == 0) GOTO 64
                ! Check to see IF metal-water reaction occurring at inside surface of cladding
                ! Use Cathcart_fraptran-Pawel equations in CHITOX to calculate ID oxidation
                kfail = Ifaila
                ! Failed Cladding
                IF (ifaila > 0) THEN
                    IF (AxNodElevat(k) < (AxNodElevat(kfail) - dloxi) .OR. AxNodElevat(k) > (AxNodElevat(kfail) + dloxi)) GOTO 888
                    tk1 = tfk(BOSTemp(ncladi,k))
                    tk2 = tfk(EOSTemp(ncladi,k))
                    dmw11 = OxiThk1(k) * 0.0254_r8k
                    w1 = OxUptakeID1(k) / 10.0_r8k
                    ! Use deformed inner diameter_fraptran
                    drodmw = DefClID / 3.28084_r8k
                    IF (dmw11 < thkoxm) dmw11 = thkoxm
                    !
                    CALL chitox (tk1, tk2, dmw22, dmw11, TimeIncrement, AlphaThk22(k), AlphaThk11(k), &
                      &          drdmw2, drodmw, emeti, w1, w2, iStoicGrad)
                    !
                    IF (modmw == 1) emeti = 0.0_r8k
                    OxiThk2(k) = dmw22 / 0.0254_r8k
                    OxUptakeID2(k) = w2 * 10.0_r8k
                    IF (modmw == 1) OxiThk2(k) = 0.0_r8k
                    ! Convert metal-water reaction energy from watts/meter to btu/ft**3-sec
                    emeti = powcnv * emeti / (3.28084_r8k * 1000.0_r8k)
                    ! Add energy at inside to energy at outside (kW/ft)
                    WatrMetlEnrgy(k) = WatrMetlEnrgy(k) + emeti / powcnv
                    !Consider cladding deformation
                    vmwi = pi * (EOSRad(ncladi+1,k) ** 2 - EOSRad(ncladi,k) ** 2)
                    emeti = emeti / vmwi
                    GOTO 64
888                 CONTINUE
                    !
                    ! High-burnup oxidation:
                ELSE IF ((nIDoxide == 1) .AND. (AxBurnup(k) / 86400.0_r8k >= BuOxide)) THEN
                    tk1 = tfk(BOSTemp(ncladi,k))
                    tk2 = tfk(EOSTemp(ncladi,k))
                    dmw11 = OxiThk1(k) * 0.0254_r8k
                    w1 = OxUptakeID1(k) / 10.0_r8k
                    ! Use deformed inner diameter_fraptran
                    drodmw = DefClID / 3.28084_r8k
                    !
                    IF (dmw11 < thkoxm) dmw11 = thkoxm
                    CALL chitox (tk1, tk2, dmw22, dmw11, TimeIncrement, AlphaThk22(k), AlphaThk11(k), drdmw2, drodmw, &
                      &          emeti, w1, w2, iStoicGrad)
                    !
                    IF (modmw == 1) emeti = 0.0_r8k
                    OxiThk2(k) = dmw22 / 0.0254_r8k
                    OxUptakeID2(k) = w2 * 10.0_r8k
                    IF (modmw == 1) OxiThk2(k) = 0.0_r8k
                    !convert metal-water reaction energy from watts/meter to btu/ft**3-sec
                    emeti = powcnv * emeti / (3.28084_r8k * 1000.0_r8k)
                    ! add energy at inside to energy at outside (kW/ft)
                    WatrMetlEnrgy(k) = WatrMetlEnrgy(k) + emeti / powcnv
                    !Consider cladding deformation
                    vmwi = pi * (EOSRad(ncladi+1,k) ** 2 - EOSRad(ncladi,k) ** 2)
                    emeti = emeti / vmwi
                ENDIF
                GOTO 64
62              CONTINUE
                !
                DO l = 1, nmesh
                    tsurf = BOSTemp(l,k)
                    IF (IterationCount > 1) tsurf = EOSTemp(l,k)
                    PrevTemp(l) = tsurf
                ENDDO
                !
                WatrMetlEnrgy(k) = 0.0_r8k
                ! Store oxide layer thickness in units of meters
                zro = EOSOxideThick(k) * 0.0254_r8k
                ! Set initial constants for surface boundary conditions
                AAHT1(k) = 0.0_r8k
                ! Use deformed cladding dimensions only_fraptran. Also add power generated in cladding.
                BBHT1(k) = powr * (DefFuRd ** 2 + CladdingPower(k) * (rhof / rhofe) * &
                  &        (EOSRad(nmesh, k) ** 2 - EOSRad(ncladi,k) ** 2)) / DefClOD
                !
                ! If a central void exist, the power should be reduced accordingly
                IF (nvoids == 1) BBHT1(k) = BBHT1(k) * (1.0_r8k - (EOSRad(2,k) / EOSRad(nmesh,k)) ** 2)
                ! nvoids = 0
                ! IF (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
                ! IF (nvoids /= 1) GOTO 210
                ! reduce surface heat flux to account for central void
                ! plvoid = (RadialBound(2) ** 2) * src1 * AxialPowr(k) / RadialBound(nmesh) ** 2
                ! modify BBHT1(k) to reflect azimuthal power variation
                ! BBHT1(k) = ((AxialPowr(k) - plvoid)/AxialPowr(k)) * BBHT1(k)
                ! 210                 IF (ntheta <= 1) GOTO 215
                ! faz = 1.0_r8k
                ! BBHT1(k) = faz * BBHT1(k)
                ! for steady-state case, determine fuel rod surface temperature
                ! 215                     CONTINUE
                nchfsw = -1
                IF (nchfmd >= 1) THEN
                    DO l = 1, nchfmd
                        IF (nodchf(l) == k) THEN
                            nchfsw = 0
                            IF (Time >= tschf(l) .AND. Time <= techf(l)) nchfsw = 1
                            IF (Time > techf(l)) nchfsw = 2
                        ENDIF
                    ENDDO
                ENDIF
                sum = 0.0_r8k
                tsurfs = 0.0_r8k
                hcoefs = 0.0_r8k
                hfluxs = 0.0_r8k
                qcrits = 0.0_r8k
                ps = 0.0_r8k
                tsat = 0.0_r8k
                hflux = HeatFlux0(k)
                IF (Nchan >= 1) THEN
                    iht = Ih(k)
                    tempcm = CladMaxT(k)
                    !
                    ! Use deformed cladding diameter_fraptran
                    CALL htrc (DefClOD, hcoef, hflux, qcrit, tsurf, iht, k, rl, nchfsw, tempcm, zro, tmelt(2))
                    !
                    Ih(k) = iht
                    CladMaxT(k) = tempcm
                    frac = pchn(jchan,j)
                    sum = sum + frac
                    tsurfs = tsurfs + frac * tsurf
                    hcoefs = hcoefs + frac * hcoef
                    hfluxs = hfluxs + frac * hflux
                    qcrits = qcrits + frac * qcrit
                    ps = ps + frac * acond(16,Nchan)
                    tsat = tsat + frac * acond(21,Nchan)
                END IF
                tsurf = tsurfs / sum
                hcoef = hcoefs / sum
                hflux = hfluxs / sum
                HeatFlux(k) = hflux / sechr
                FilmCoeffAv(k) = hcoef
                ! Multiply by Uncertainty coefficient
                FilmCoeffAv(k) = FilmCoeffAv(k) * sigsurfhtc
                CritHtFlux(k) = qcrits / (sum * sechr)
                CoolPress(k) = ps / sum
                tsat = tsat / sum
                HeatFlux0(k) = HeatFlux(k)
                ! Calculate oxide thermal conductivity and convert from W/m-K to btu/ft2-hr-F
                conoxide = MatProperty (Material='OXIDE', Property='THERMCOND', Temperature=tfk(tsurf)) * 0.57803_r8k
                ! Convert oxide thickness from m to ft
                zroft = zro * 3.28084_r8k
                ! Calculate delta T across oxide (F)
                deltox(k) = hflux * zroft / conoxide
                ! Add oxide delta T to surface T for boundary condition
64              CONTINUE
                BoundaryCondition(6) = tsurf + deltox(k)
                !
                IF (ndebug) WRITE(ounit,889) BoundaryCondition(6), tsurf, deltox(k), emeti
889             FORMAT(' HEAT: BC(6) = ',e11.4,' tsurf = ',e11.4,' deltox(k) = ',e11.4,' emeti = ',e11.4)
                ! zroi = inside layer oxide thickness (meters)
                zroi = OxiThk2(k) * 0.0254_r8k
                ! Compute gap conductance of j-th fuel rod at k-th axial node
                ! zro = oxide layer thickness (meters)
                zro = EOSOxideThick(k) * 0.0254_r8k
                ! ftemp = FinalTemp(igpnod)
                ftemp = EOSTemp(igpnod,k)
                ! ctemp = FinalTemp(ncladi)
                ctemp = EOSTemp(ncladi,k)
                ctempk = tfk(ctemp)
                gptemp = (ftemp + ctemp) / 2.0_r8k
                IF (ndebug) nphgap = 1
                IF (GapThick(k) > 1.0e-6_r8k) GOTO 718
                ! Check to see IF outer mesh of fuel pellet has melted. IF so, set gap conductance to arbitrarily large value.
                IF (NSteadyTrans == 1) GOTO 718
                frmlt = EnrgyMeltZ(igpnod,k) / qmaxmelt(igpnod)
                IF (frmlt < 0.99_r8k .AND. ftemp < (tmelt(1) + 0.5_r8k)) GOTO 718
                HGapAV(k) = 1.0e4_r8k
                ! Use true _fraptran(deformed) pellet-cladding gap width
                GapConductivity = gpthkt * HGapAV(k) / sechr
                GOTO 76
                
718             CONTINUE
                
                ! IF r-theta heat conduction, modify gap thickness for offset fuel pellet
                IF (ntheta > 1) THEN
                    ! Offset pellet
                    ! Use deformed geometry_fraptran
                    rci = DefFuRd + GapThick(k)
                    angle = pi * (180.0_r8k - theta + dofang) / 180.0_r8k
                    dofst = dofset
                    cosang = COS(angle)
                    IF (dofst > GapThick(k)) dofst = GapThick(k)
                    ! Use deformed geometry_fraptran
                    GapThick(k) = dofst * cosang + 0.5_r8k * SQRT((2.0_r8k * dofst * cosang) ** 2 - &
                      &           4.0_r8k * (dofst ** 2 - rci ** 2)) - DefFuRd
                    IF (GapThick(k) < 0.0_r8k) GapThick(k) = gapmin
                END IF

                ! 2/3/99 bypass non-uniform gap model for all cases
                ibypass = 1
                IF (ibypass /= 1 .AND. ndim < 1) THEN
                    ! IF collapsed cladding, assume uniform gap
                    IF (ctempk < 1500.0_r8k .OR. GasPress(k) >= CoolPress(k)) THEN
                        ! IF cladding effective stress > yield stress, assume uniform gap
                        ngaps = 8
                        ! Set maximum pellet offset from uniform gap
                        ! Assume fuel pellets are fully offset, even in ballooned rod.
                        tshftm = GapThick(k)
                        IF (tshftm < gpthk0) tshftm = gpthk0
                        ! pshftm = maximum offset of fuel-cladding interface pressure from nominal  (psia)
                        ! Do not vary interface pressure for offset pellet model.
                        pshftm = 0.0_r8k
                        ! Assume ngaps dIfferent circumferential segments of equal length
                        fngaps = ngaps
                        frgseg = 1.0_r8k / fngaps
                        !
                        DO l = 1, ngaps
                            ! Store fraction of circumferential segment to total circumference
                            frcgap(l) = frgseg
                            fnx = l
                            ! Compute multiplier on offset, which varies from -1. to 1.
                            tgshft = -1.0_r8k + 2.0_r8k * (fnx - 1.0_r8k) / (fngaps - 1.0_r8k)
                            delgap(l) = tshftm * (-1.0_r8k + (2.0_r8k * fnx - 1.0_r8k) / fngaps)
                            ! nphgap = switch to print details of gap conductance calculations
                            IF (nphgap == 1) WRITE(ounit,935) l, frcgap(l), delgap(l), GapThick(k), tgshft
935                         FORMAT(' l = ',i6,' frcgap = ',e11.4,' delgap = ',e11.4,' GapThick(k) = ',e11.4,' tgshft = ',e11.4)
                            IF (RInterfacPrs(k) > 0.0_r8k) THEN
                                ! Multiplier on pshftm varies from (-1.+pofset) to (1.+pofset)
                                pofset = 0.0_r8k
                                delpfc(l) = (tgshft - pofset) * pshftm
                            ELSE
                                delpfc(l) = 0.0_r8k
                            ENDIF
                        ENDDO
                    ELSE
                        ngaps = 1
                        frcgap(1) = 1.0_r8k
                        delgap(1) = 0.0_r8k
                        delpfc(1) = 0.0_r8k
                    ENDIF
                ELSE
                    ngaps = 1
                    frcgap(1) = 1.0_r8k
                    delgap(1) = 0.0_r8k
                    delpfc(1) = 0.0_r8k
                ENDIF
                frcsum = 0.0_r8k
                hgpsum = 0.0_r8k
                !
                DO ngapi = 1, ngaps
                    frcsum = frcsum + frcgap(ngapi)
                    gasgpi = GapThick(k) + delgap(ngapi)
                    IF (gasgpi < 0.0_r8k) gasgpi = gapmin
                    IF (gasgpi > (2.0_r8k * GapThick(k))) gasgpi = 2.0_r8k * GapThick(k)
                    pfci = RInterfacPrs(k) + delpfc(ngapi)
                    !
                    IF (nphgap == 1) WRITE(ounit,926) ngapi, RInterfacPrs(k), delpfc(ngapi), pfci
926                 FORMAT( ' ngapi = ',i5,2x,'RInterfacPrs = ',e11.4,' delpfc = ',e11.4,' pfci = ',e11.4)
                    !
                    pfci = MAX(0.0_r8k, pfci)
                    ! Use deformed geometry_fraptran
                    fulori = DefFuRd - delgap(ngapi)
                    ! Compute gap conductance
                    bulocal = burad(k,igpnod)
                    !
                    CALL gaphtc (gasgpi, fulori, pfci, gptemp, ftemp, ctemp, GasPress(k), GasFraction, &
                      &          FastFlux(k), tflux, roucih, roufih, frden, coldw, zroi, fotmtl, &
                      &          CladMaxT(k), modfd, hgap, gadolin(k), bulocal, gapmin, k)
                    !
                    IF (naxn <= 20) hgap = fdial(k) * hgap
                    IF (nphgap == 1) WRITE(ounit,933) GapThick(k), gasgpi, frcgap(ngapi), hgap
933                 FORMAT(' GapThick(k) = ',e11.4,' gasgpi = ',e11.4,' frcgap = ',e11.4,' hgap = ',e11.4)
                    !
                    hgpsum = hgpsum + frcgap(ngapi) * hgap
                ENDDO
                hgap = hgpsum / frcsum
                ! SSHgap(k) stores start of time step gap conductance.
                IF (IterationCount == 1) SSHgap(k) = hgap
                IF (nphgap == 1) WRITE(ounit,948) IterationCount, AxialPowr(k), NSteadyTrans, GapThick(k), SSHgap(k)
948             FORMAT(' after stat. 735, IterationCount = ',i3,' AxialPowr = ',e11.4,' NSteadyTrans = ',i2, &
                  &    ' GapThick(k) = ',e11.4,' SSHgap(k) = ',e11.4)
                IF (IterationCount > 1) hgapout1(k) = hgapout2(k)
                hgapout2(k) = hgap
                ! Calculate maximum possible gap conductance based upon
                ! assumption that fuel surface temperature cannot decrease
                ! during time step in which power increased under condition
                ! of steady state heat transfer.
                IF (IterationCount == 1) hgpmax = hgap
                IF (IterationCount > 1) GOTO 738
                IF (NSteadyTrans > 1) GOTO 738
                IF (AxialPowr(k) < 5.1_r8k) GOTO 738
                IF (AxialPowr(k) < (AxialPowr0(k) + 0.001_r8k)) GOTO 738
                IF (GapThick(k) > 2.0e-4_r8k) GOTO 738
                ! Use deformed geometry_fraptran
                rgpmid = 0.5_r8k * (EOSRad(igpnod,k) + EOSRad(ncladi,k))
                ! hgpmax has units of btu/hr.ft**2
                hgpmax = powcnv * sechr * AxialPowr(k) / (2.0_r8k * pi * rgpmid * (BOSTemp(igpnod,k) - BOSTemp(ncladi,k)))
                hgpmax = MIN(2.0e4_r8k, hgpmax)
                IF (nphgap == 1) WRITE(ounit,941) hgpmax, SSHgap(k), k, IterationCount
941             FORMAT(' hgpmax = ',e11.4,' SSHgap(k) = ',e11.4,' k = ',i5, ' IterationCount = ',i5)
738             CONTINUE
                IF (IterationCount == 1) HmaxInitial(k) = hgpmax
                IF (IterationCount > 1) hgpmax = HmaxInitial(k)
                !
                IF (nphgap == 1) WRITE(ounit,937) hgap, hgpsum, frcsum
937             FORMAT(' hgap = ',e11.4,' hgpsum = ',e11.4,' frcsum = ',e11.4)
                IF (ndebug) WRITE(ounit,913) GapConductivity, fdial(k)
913             FORMAT(' after gaphtc call, GapConductivity = ',e11.4,' fdial = ',e11.4)
                ! Check to see IF method of newton should be used to accelerate convergence on gap conductance
                IF (IterationCount > 1) GOTO 765
                ! IF steady state and increasing power, guess gap conductance for first iteration
                ! by assuming it to be average of upper and lower bound.
                IF (NSteadyTrans > 1) GOTO 763
                IF (AxialPowr(k) < 5.1_r8k) GOTO 763
                IF (AxialPowr(k) < (AxialPowr0(k) + 0.001_r8k)) GOTO 763
                hgap = hgpmax
763             CONTINUE
                !Use true pellet_fraptran-cladding gap width
                GapConductivity = gpthkt * hgap / sechr
                hgapin1(k) = (GapConductivity / gpthkt) * sechr
                IF (nphgap == 1) WRITE(ounit,945) hgapin1(k)
945             FORMAT('for 1st iteration input hgap = ',e11.4)
                GOTO 790
                !
765             CONTINUE
                IF (IterationCount > 2) GOTO 770
                IF (NSteadyTrans > 1) GOTO 768
                IF (AxialPowr(k) < 5.1_r8k) GOTO 768
                IF (AxialPowr(k) < (AxialPowr0(k) + 0.001_r8k)) GOTO 768
                IF (GapThick(k) > 2.0e-4_r8k) GOTO 768
                hgplbd = 0.5_r8k * (SSHgap(k) + hgpmax)
                ! IF power not changed with time step, reduce hgplbd
                npowch = 1
                IF (AxialPowr(k) > (0.99_r8k * AxialPowr0(k)) .AND. AxialPowr(k) < (1.01_r8k * AxialPowr0(k))) npowch = 0
                IF (npowch == 0) hgplbd = 0.95_r8k * hgplbd
                IF (hgap < hgplbd) hgap = hgplbd
768             CONTINUE
                IF (nphgap == 1) WRITE(ounit,949) hgap, hgpmax, SSHgap(k)
949             FORMAT(' after stat. 768, hgap = ',e11.4,' hgpmax = ',e11.4,' SSHgap(k) = ',e11.4)
                hgap = MIN((1.1_r8k * hgpmax), hgap)
                ! Use true pellet_fraptran-cladding gap width
                GapConductivity = gpthkt * hgap / sechr
                hgapin2(k) = (GapConductivity / gpthkt) * sechr
                IF (nphgap == 1) WRITE(ounit,947) hgapin2(k)
947             FORMAT('for 2nd iteration input hgap = ',e11.4)
                GOTO 790
                !
770             CONTINUE
                IF (ABS(hgapin2(k) - hgapin1(k)) < 1.0e-5_r8k) GOTO 790
                hgapout2(k) = hgap
                dhidho = (hgapout2(k) - hgapout1(k)) / (hgapin2(k) - hgapin1(k))
                hgapgs = (hgapout1(k) - dhidho * hgapin1(k)) / (1.0_r8k - dhidho)
                !
                ! **** Bypass method of newton on third iteration ****
                !
                IF (IterationCount > (MaximumIterations + 5)) hgapgs = 0.5_r8k * (hgapout1(k) + hgapout2(k))
                IF (nphgap == 1) WRITE(ounit,938) IterationCount, hgapgs, hgapin1(k), hgapin2(k), &
                  &                               hgapout1(k), hgapout2(k), dhidho
938             FORMAT(' IterationCount =',i3,' hgapgs = ',e11.4, ' hi1 = ',e11.4,' hi2 = ',e11.4, &
                  &    ' ho1 = ',e11.4,' ho2 = ',e11.4,' dhidho = ',e11.4)
                fitcnt = IterationCount
                ! Establish upper and lower bounds on possible values of hgap.
                ! In addition, force value of hgapgs to fall in an ever narrowing range.
                hgpmxi = hgpmax + 0.05_r8k * fitcnt * hgpmax
                IF (hgapgs > hgpmxi) hgapgs = hgpmxi
                ! See IF lower bound can be imposed upon hgapgs
                IF (NSteadyTrans > 1) GOTO 772
                IF (AxialPowr(k) < 5.1_r8k) GOTO 772
                IF (AxialPowr(k) < AxialPowr0(k)) GOTO 772
                IF (GapThick(k) > 2.0e-4_r8k) GOTO 772
                hgplbd = SSHgap(k) - 0.02_r8k * fitcnt * SSHgap(k)
                IF (hgapgs < hgplbd) hgapgs = hgplbd
772             CONTINUE
                IF (nphgap == 1) WRITE(ounit,943) hgapgs, hgapout1(k), hgapout2(k), IterationCount
943             FORMAT(' hgapgs = ',e11.4,' hgapout1 = ',e11.4,' hgapout2 = ',e11.4,' IterationCount = ',i5)
                !Use true pellet_fraptran-cladding gap width
                GapConductivity = hgapgs * gpthkt / sechr
                hgapout1(k) = hgapout2(k)
                hgapin1(k) = hgapin2(k)
                hgapin2(k) = hgapgs
790             CONTINUE
                !Use true pellet_fraptran-cladding gap width
                HGapAV(k) = (GapConductivity / gpthkt) * sechr
                ! This lower limit for the pellet-cladding gap heat transfer coefficient
                ! was found to be reached when simulating integral LOCA tests in the
                ! Halden IFA-650 series of tests. These tests were done with 95 vol% Ar
                ! in the pellet-cladding gap, which lead to a very low calculated heat 
                ! transfer coefficients as the pellet-cladding gap widened by ballooning
                ! of the cladding tube. The lower cut-off limit affected the calculated 
                ! results significantly, and was therefore reduced (to 10 from 100).
                ! hgapmn= 100.0_r8k
                hgapmn = 10.0_r8k
                IF (HGapAV(k) <= hgapmn) THEN
                    ! WRITE(ounit,934)
934                 FORMAT(/' *** convergence problems in gap conductance iteration. so gap conductance set to 568 W/K-m2 ***')
                    ! WRITE(ounit,939) k, Time, IterationCount
939                 FORMAT(' *** convergence problems were at axial node #',i3,' time(s) = ',e13.6,' iteration #',i4)
                    !Use true pellet_fraptran-cladding gap width
                    GapConductivity = hgapmn * gpthkt / sechr
                    HGapAV(k) = hgapmn
                ENDIF
                IF (nphgap == 1) WRITE(ounit,912) k, IterationCount, HGapAV(k), GapConductivity, gpthk0, sechr
912             FORMAT(' k = ',i2,' IterationCount = ',i3,' HGapAV = ',e11.4 &
                  &    ,' GapConductivity = ',e11.4,' gpthk0 = ',e11.4,' sechr = ',e11.4)
76              CONTINUE
                ! Check to see if film boiling specified for axial node k at time Time
                nchfsw = -1
                IF (nchfmd < 1) GOTO 760
                !
                DO l = 1, nchfmd
                    IF (nodchf(l) == k) THEN
                        nchfsw = 0
                        IF (Time >= tschf(l) .AND. Time <= techf(l)) nchfsw = 1
                        IF (Time > techf(l)) nchfsw = 2
                    ENDIF
                ENDDO
                !
760             CONTINUE
                IF (NSteadyTrans == 1) THEN
                    rwa1(1:nmesh) = PrevIterateTemp(1:nmesh,k)
                ELSE
                    EnrgyMeltZp1(1:nmesh,k) = EnrgyMeltp1(1:nmesh,k)
                    EnrgyMeltZ(1:nmesh,k) = EnrgyMelt(1:nmesh,k)
                    tsurf = tsurfa
                END IF
                ! Check to see if current axial node overlays central void region.
                ! IF so, turn on switch and modIfy source term for mesh # 1.
                ! Also compute thermal conductivity and specific heat of gas in void
                ! nvoids = 0
                !IF (nvoid >= 1 .AND. AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) THEN
                IF (nvoid >= 1 .AND. nvoids == 1) THEN
                    ! turn on flag
                    !nvoids = 1
                    ! Compute gas thermal conductivity of central void
                    ! voidtk = tfk(FinalTemp(1))
                    voidk =  MatProperty (Material='GAS', Property='THERMCOND', Temperature=tfk(EOSTemp(1,k)), &
                      &                   Pressure=(GasPress(k)/psinm2), GasComposition=GasFraction) / thcond
                    ! Override radial source term input for mesh # 1
                    voidcp = gapcv
                ENDIF
                IF (NSteadyTrans > 1) GOTO 67
                !
                CALL ht1sst (powr, HeatFlow, nerr, RadialBound, pazp, azpang, nrazp, naazp, theta, &
                  &          nvoids, voidk, GasPress(k), AxBurnup(k), GasFraction, Vreloc(k), &
                  &          modfd, igpnod, frden, tsntrk, ncladi, modkf,k, cnfsol, tmelt(1), &
                  &          fotmtl, NodeSinterTemp(k), nmesh, rhofe, gadolin(k), burad, radsrc, &
                  &          coldw, rhoce)
                !
                IF (nerr == 1 .OR. nerr == 2) WRITE(ounit,78) k, HGapAV(k), AxialPowr(k), GapThick(k), RInterfacPrs(k)
78              FORMAT(' k = ',i5,' hagp = ',e11.4,' pkw = ',e11.4,2x,'GapThick = ',e11.4,2x,' RInterfacPrs = ' ,e11.4)
                !
                IF (nerr == 1) CALL error1 (1,0)
                IF (nerr == 2) CALL error1 (2,0)
                !
                reflpr = refdtm
                ! Branch from end of ht1sst to end of ht1tdp
                GOTO 68
                !
67              CONTINUE
                ihData(2) = nrazp
                ihData(4) = naazp
                ihData(5) = nqchn
                ihData(6) = nvoids
                ihData(7) = nchfsw
                ihData(8) = igpnod
                ihData(9) = naxn
                ihData(12) = indx
                ihData(13) = length
                ihData(15) = nerr
                ihData(18) = IterationCount
                ihData(19) = lhtc
                ihData(21) = modfd
                ihData(22) = ncladi
                ihData(23) = modkf
                ihData(24) = ntheta
                ihData(25) = nchfmd
                hcoef = FilmCoeffAv(k)
                ! At the beginning of reflood establish the flecht equivalent power and set the maximum
                ! clad temperature and the radiation factor. These values are calculated only once.
                IF (qmax /= 0.0_r8k .OR. Time < refdtm) GOTO 52
                ! Pick up the maximum power
                pmax = 0.0_r8k
                mpmax = 0
                totpow = 0.0_r8k
                !
                DO kk = 1, naxn
                    IF (AxialPowr(kk) > pmax) mpmax = kk
                    IF (pmax < AxialPowr(kk)) pmax = AxialPowr(kk)
                    totpow = totpow + AxialPowr(kk) * AxialNodLen(kk)
                ENDDO
                !
                IF (nflec == 0) THEN
                    ! Compute qmax for lin generalized version of flecht
                    qmax = pmax
                ELSE
                    ! Compute qmax for t4 version of flecht
                    ! the average rod power, kW/ft
                    pavg = totpow / rl
                    ! The flecht power factor
                    qmax = pmax
                END IF
                
                ! Pick out the maximum clad surface temperature
                tempmx = 0.0_r8k
                ! **************************************************
                ! * create an initial temperature table used for   *
                ! * the flt-c-set correlation                  *
                ! **************************************************
                iii = 1
                DO kk = 1, naxn
                    iii = iii + 2
                    iiip1 = iii + 1
                    tempfc(iii) = EOSTemp(nmesh,kk)
                    tempfc(iiip1) = AxNodElevat(kk)
                    IF (EOSTemp(nmesh,kk) > tempmx) tempmx = EOSTemp(nmesh,kk)
                ENDDO
                !
                tempfc(1) = tempfc(3)
                tempfc(2) = 0.0_r8k
                iii = iii + 2
                iiim2 = iii - 2
                iiip1 = iii + 1
                tempfc(iii) = tempfc(iiim2)
                tempfc(iiip1) = rl
                ntempf = naxn + 2
                ! If generalized flecht is used, set tempmx to cladding temperature at start of reflood at elevation of peak power
                tempmx = BOSTemp(nmesh,mpmax)
                
                ! * Define the initial average power and a rod heat capacity quantity used in the flt-c-set correlation *
                IF (mflt == 0) tempmx = MAX(700.0_r8k, tempmx)
                pavgft = totpow / rl
                afuel = pi * EOSRad(igpnod,1) ** 2
                aclad = pi * EOSRad(nmesh,1) ** 2 - pi * EOSRad(ncladi,1) ** 2
                ! Fuel and cladding temperatures
                tsatk = tfk(tsat)
                ! Fuel specific heat
                cpfuel =  MatProperty (Material='FUEL', Property='SPECHEAT', Temperature=tsatk, OMRatio=fotmtl, &
                  &                    Pu=compmt, Fraction_TD=frden, Fraction_melt=0.0_r8k) / 4.1868e3_r8k
                ! Cladding specific heat
                cpclad = MatProperty (Material='CLAD', Property='SPECHEAT', Temperature=tsatk) / 4.1868e3_r8k
                ! LOJ qt15: REMARK!
                ! These properties are calculated for the lowermost axial segment,
                ! and without consideration of possible central hole in the pellet. 
                rcpar = rhof * cpfuel * afuel + rhoc * cpclad * aclad
                drflt = 2.0_r8k * EOSRad(nmesh,1)
                ! LOJ qt15: End Remark
                ! Calculate hrad as per the toodee2 model
                IF (hrad < 0.0_r8k) hrad = 3.67_r8k * qmax * (1.0_r8k - EXP((700.0_r8k - tempmx) / 435.0_r8k))
                !
52              CONTINUE
                ! **************************************************
                ! * surface rod temperature used in flt-c-set cor. *
                ! **************************************************
                trodfc = EOSTemp(nmesh,k)
                iht = Ih(k)
                !
                CALL ht1tdp (pchn, powr, time0, TimeIncrement, tsurf, qcrit, hflux, hcoef, &
                  &          CoolPress(k), iht, DefClOD, HeatFlux0(k), emetal, EnrgyMeltZ, EnrgyMeltZp1, &
                  &          tmelt, hfusn, qmaxmelt, qmaxmeltp1, powr0, SurfHtFlux, AxNodElevat, rl, &
                  &          HeatFlow, EOSRad(:,k), pazp, azpang, theta, fqcrit, voidk, voidcp, &
                  &          CladMaxT(k), zro, AxialPowr, AxialNodLen, RuptFailIndex, rhoce, empytm, &
                  &          emeti, AAHT1, BBHT1, GasPress(k), AxBurnup(k), GasFraction, Vreloc(k), &
                  &          frden, tsntrk, cnfsol, cnfliq, fotmtl, NodeSinterTemp(k), nmesh, rhofe, &
                  &          gadolin(k), burad, k, radsrc, deltox(k), coldw, IndexInitTemp, BOSTemp)
                !
                Ih(k) = iht
                !
                IF (nerr /= 0) CALL error1 (nerr+2,1)
                !
                IF (nerr == 1 .OR. nerr == 2) RETURN
                !
                CritHtFlux(k) = qcrit / sechr
                HeatFlux(k) = hflux / sechr
                FilmCoeffAv(k) = hcoef
                ! Multiply by Uncertainty coefficient
                FilmCoeffAv(k) = FilmCoeffAv(k) * sigsurfhtc
                reflpr = refdtm
                IF (Time >= empytm) rmassflux(k) = 0.0_r8k
                IF (time0 >= refdtm) THEN
                    rmassflux(k) = acond(12,Nchan)
                    CoolPress(k) = acond(16,Nchan)
                    BulkCoolTemp(k) = acond(17,Nchan)
                    crfpr  = crf
                    zqchpr = zqch
                    fldrpr = fldrte
                ENDIF
                ! Locate first index of temperature array
68              CONTINUE
                ! IF new surface temperature indicates nucleate boiling, require more accuracy for temperature calculations
                tmpacc = tmpac1
                ! IF two time step reductions without advance, force explicit solution
                IF (ndtred >= 1) tmpacc = 1.0e20_r8k
                IF (ntstep == 1 .AND. tmpacc > em03) tmpacc = em03
                ! Check for convergence
                ! average calculated temperatures of past two iterations
                itswt = 0
                !
                DO l = 1, nmesh
                    tmpact = tmpacc
                    IF (ntheta > 1 .AND. l == 1) tmpact = tmpacc / 5.0_r8k
                    IF (IterationCount > 10) tmpact = tmpacc
                    ! rt = ABS((FinalTemp(l) - rwa1(l)) / rwa1(l))
                    rt = ABS((EOSTemp(l,k) - rwa1(l)) / rwa1(l))
                    IF (rt > tmpact) itswt = 1
                ENDDO
                !
                IF (itswt == 0) IndexTempConverg(k) = 0
                ! FuelCenterT(k) = FinalTemp(1)
                FuelCenterT(k) = EOSTemp(1,k)
                ! CladSurfT(k) = FinalTemp(nmesh)
                CladSurfT(k) = EOSTemp(nmesh,k)
                !
                EOSTemp(1:nmesh,k) = FinalTemp(1:nmesh)
                IF (ndebug) WRITE(ounit,*)'in heat after 830: k,FinalTemp(ncladi),EOSTemp(k,ncladi)', &
                  &                        k, FinalTemp(ncladi), EOSTemp(ncladi,k)
                ! Store gap temperatures
                ! FuelSurfT(k) = FinalTemp(igpnod)
                FuelSurfT(k) = EOSTemp(igpnod,k)
                ! GapTemp(k) = 0.5_r8k * (FinalTemp(igpnod) + FinalTemp(ncladi))
                GapTemp(k) = 0.5_r8k * (EOSTemp(igpnod,k) + EOSTemp(ncladi,k))
                ! Compute average fuel temperature
                sum = 0.0_r8k
                asum = 0.0_r8k
                !
                DO l = 1, igpnod
                    IF (l == 1) THEN
                        ps = (0.5_r8k * (RadialBound(2) + RadialBound(1))) ** 2
                    ELSE IF (l == igpnod) THEN
                        ps = RadialBound(igpnod) ** 2 - (0.5_r8k * (RadialBound(igpnod) + RadialBound(igpnod-1))) ** 2
                    ELSE
                        ps = (0.5_r8k * (RadialBound(l) + RadialBound(l+1))) ** 2 - &
                          &  (0.5_r8k * (RadialBound(l) + RadialBound(l-1))) ** 2
                    ENDIF
                    ! sum = sum + ps * FinalTemp(l)
                    sum = sum + ps * EOSTemp(l,k)
                    asum = asum + ps
                ENDDO
                !
                OpenPorTemp(k) = sum / asum
                CrackTemp(k) = OpenPorTemp(k)
                ! Compute average cladding temperature
                sum = 0.0_r8k
                ps = 0.0_r8k
                !
                DO l = ncladi, nmesh
                    dps = 1.0_r8k
                    IF (l == ncladi .OR. l == nmesh) dps = 0.5_r8k
                    ps = ps + dps
                    ! sum = sum + dps * FinalTemp(l)
                    sum = sum + dps * EOSTemp(l,k)
                ENDDO
                !
                CladAveTemp(k) = sum / ps
                IF (CladMaxT(k) < CladAveTemp(k)) CladMaxT(k) = CladAveTemp(k)
                ! End of loop on j , k
270         ENDDO
        ENDDO
    ENDDO
    !
    END SUBROUTINE heat
    !
    !
    !
    SUBROUTINE ht1sst (power, HeatFlow, errsw, RadialBound, paz, azpang, nrazp, naazp, theta, nvoids, &
      &                voidk, pres, bu, GasFraction, vrelc, modfd, igpnod, frden, tsntrk, ncladi, &
      &                modkf, k, cnfsol, tmeltf, fotmtl, NodeSinterTemp, nmesh, rhof, gadolin, burad, &
      &                radsrc, coldw, rhoc)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit, IndexThermcon, iflago, iflagn, noiter, indexfintemp, indexbc, maxit, &
      &                   ndebug, Time, FinalTemp, PrevIterateTemp, ArrayE, ArrayF, BoundaryCondition, &
      &                   ThermalConductivity, AreaWeight, areao, VolumeWeightR, VolumeWeightL, arean, epsht1
    USE NCGases_fraptran, ONLY : ngases
    USE conversions_fraptran, ONLY : pi
    IMPLICIT NONE
    !> @brief
    !> Subroutine ht1sst solves the 1-d steady-state heat problem
    !
    !
    ! bdcond   - Subroutine to supply boundary conditions as ordered pairs of numbers (ao,bo), (an,bn)
    !            such that surface heat flux given by q(s)=a*t(s) + b
    ! power    - steady-state power level
    ! HeatFlow(l) - net heat flow by conduction into mesh interval l (btu/ft**3-sec)
    ! errsw    - error switch  (0 =# no error,  1 =# error)
    ! nvoids   - switch to model first mesh as a void, 0=off , 1=on
    ! voidk    - thermal conductivity of central void gas, btu/ft-F-sec
    ! pres     - fuel rod gas pressure (psia)
    ! bu       - fuel burnup  (mW-sec/kg)
    ! rhoc     - density of cladding    (lb/ft3)
    ! GasFraction(k) - fraction of gas consisting of k-th component
    ! vrelc    - crack volume internal to fuel generated by fuel relocation. ft3/ft3 , i.e., it is a void fraction
    ! modfd    - indicator of fuel deformation model.
    ! igpnod   - radial node number at fuel pellet surface
    ! frden    - fraction of fuel density to theoretical maximum density
    ! tsntrk   - fuel sintering temperature (K)
    ! ncladi   - radial node at cladding inside surface
    ! modkf    - indicator of correction formula for cracked fuel conduct.
    ! cnfsol   - thermal conductivity of fuel just before melting (btu/s-ft-F)
    ! tmeltf   - melting temperature of fuel (F)
    ! frden    - fuel density divided by theoretical fuel density.
    ! fotmtl   - oxygen to metal ratio for fuel, normally 2.0.
    ! NodeSinterTemp - number of mesh fartherest from fuel center that has been sintered any time in past
    !
    ! epsht1  thermal convergence criterion
    ! areao   surface area at xo
    ! arean   surcace area at xn
    ! ssflag  true If heat capacities are not required (ss case)
    !
    INTEGER(ipk) :: k, indexrows, indexvolheatcapacity, iterationcountss, indexadv, &
      &             l, igpnod, modfd, ncladi, modkf, nodesintertemp, nvoids, nrazp, naazp, i
    INTEGER(ipk), INTENT(IN) :: nmesh
    INTEGER(ipk), INTENT(OUT) :: errsw
    REAL(r8k) :: faz, power, tmeltf, cnfsol, powin, cflux, cldtime, frden, fotmtl, rhof, den, &
      &          gadolin, coldw, rhoc, pres, bu, vrelc, tsntrk, tmpmsh, ccm1, theta, rbkt, &
      &          powinp, aam1, voidk, r, bkt, arealoj
    REAL(r8k), PARAMETER :: em10 = 10.0e-10_r8k
    LOGICAL :: ssflag, Finished, bcflag
    REAL(r8k), DIMENSION(ngases) :: GasFraction
    REAL(r8k), DIMENSION(:) :: HeatFlow, RadialBound, azpang
    REAL(r8k), DIMENSION(:,:) :: paz, burad, radsrc
    !
    Finished = .FALSE.
    faz = 1.0_r8k
    IF (power > 1.0e25_r8k) ndebug = .TRUE.
    IF (ndebug) WRITE(ounit,909) k, tmeltf, cnfsol, power
909 FORMAT(' ht1sst entered, k = ',i5,' tmeltf = ',e11.4,' cnfsol = ',e11.4,' power = ',e11.4)
    errsw = 0
    IndexRows = 1
    ssflag = .TRUE.
    powin = power
    ! Indexes for left and right volume weights of mesh 1
    IndexVolHeatCapacity = IndexThermCon + nmesh + 3
    IterationCountSS = 0
    !
    ! Use true _fraptran(deformed) cladding perimeter instead of areas, which is not updated with time.
    !
    arealoj = 2.0_r8k * pi * RadialBound(nmesh)
    !
    CALL ftbmov (FinalTemp, PrevIterateTemp(1:nmesh,k), nmesh)
    !
    CALL bdcond (BoundaryCondition, Iflago, Iflagn)
    !
    IF (ndebug) WRITE(ounit,911) noiter
911 FORMAT('noiter = ',i8)
    !
    ! Convergence loop
    ConvLoop: DO
        IF (ndebug) THEN
            errsw = 1
        ELSE
            errsw = 0
        END IF
        !
        IndexAdv = 0
        cflux = 0.0_r8k
        cldtime = 0.0_r8k
        !
        CALL maData (IndexFinTemp, IndexThermCon, IndexBC, IndexVolHeatCapacity, nmesh, IndexRows, &
          &          ssflag, errsw, frden, fotmtl, rhof, gaDOlin, burad, coldw, cflux, cldtime, k, &
          &          rhoc, IndexAdv)
        !
        IF (ndebug) THEN
            WRITE(ounit,913)
913         FORMAT(' thcon before kmod call ')
            WRITE(ounit,903) (ThermalConductivity(l),l = 1,nmesh)
        ENDIF
        !
        CALL kmod (ThermalConductivity, RadialBound, igpnod, pres, bu, GasFraction, vrelc, modfd,  &
          &        FinalTemp, frden, tsntrk, ncladi, modkf, NodeSinterTemp)
        !
        IF (ndebug) THEN
            WRITE(ounit,901) IterationCountSS
901         FORMAT(' from ht1sst, temperature vector IterationCountSS = ',i6)
            WRITE(ounit,903) (FinalTemp(l), l = 1,nmesh)
903         FORMAT(8(2x,e11.4))
            WRITE(ounit,905)
905         FORMAT(' conductivity vector - btu/s-ft-F ')
            WRITE(ounit,903) (ThermalConductivity(l),l = 1,nmesh)
        ENDIF
        ! If fuel temperature above melting temp, override maData thermal conductivity
        DO l = 1, (igpnod - 1)
            tmpmsh = 0.5_r8k * (FinalTemp(l) + FinalTemp(l+1))
            IF (tmpmsh > (tmeltf - 10.0_r8k)) ThermalConductivity(l) = cnfsol
        ENDDO
        ! If central void, override thermal conductivity for mesh #1
        IF (nvoids == 1) ThermalConductivity(1) = voidk
        IF (errsw == 1) RETURN
        IF (Finished) RETURN
        ! set up arrays for gaussian elimination
        bcflag = .FALSE.
        ccm1 = -ThermalConductivity(1) * AreaWeight(1,k)
        IF (Iflago == 1 .OR. Iflago == 2 .OR. Iflago == 3) THEN
            ! Special case
            SELECT CASE (iflago)
            CASE (1)
                ! Surface temperature = constant
                ArrayE(1) = 0.0_r8k
                ArrayF(1) = BoundaryCondition(6)
            CASE (2)
                ! Heat flux = constant
                bkt = ThermalConductivity(1) * areao
                ArrayE(1) = -1.0_r8k
                den = -ccm1
                bcflag = .TRUE.
                powinp = faz * powin * radsrc(k,1)
                ArrayF(1) = (powinp + (VolumeWeightR(1,k) + VolumeWeightL(1,k)) * HeatFlow(1) + bkt * BoundaryCondition(6)) / den
            CASE (3)
                ! Convective bc
                bkt = areao
                den = bkt * BoundaryCondition(1) - ccm1
                IF (den < em10) THEN
                    WRITE(ounit,198) den, bkt, ccm1, BoundaryCondition(1)
                    STOP
                ENDIF
                ArrayE(1) = ccm1 / den
                ! HeatFlow(1) = rate at which heat is conducted into mesh 1 from azimuthal and axial directions (btu/ft**3-sec)
                powinp = faz * powin * radsrc(k,1)
                ArrayF(1) = (powinp + (VolumeWeightR(1,k) + VolumeWeightL(1,k)) * HeatFlow(1) + bkt * BoundaryCondition(6)) / den
            END SELECT
        ELSE IF (BoundaryCondition(2) == 0.0_r8k) THEN
            ! general case - test If bo=0.0
            ! bo=0.0  =# surface temperature supplied
            ArrayE(1) = 0.0_r8k
            ArrayF(1) = BoundaryCondition(6) / BoundaryCondition(1)
        ELSE
            ! test if surface heat flux specified
            IF (BoundaryCondition(1) == 0.0_r8k) bcflag = .TRUE.
            bkt = ThermalConductivity(1) * areao / BoundaryCondition(2)
            den = bkt * BoundaryCondition(1) - ccm1
            IF (den < em10) THEN
                WRITE(ounit,198) den, bkt, ccm1, BoundaryCondition(1)
198             FORMAT(//'**** error: den cannot be zero. den = ',e11.4,' bkt = ',e11.4,' ccm1 = ',e11.4, &
                  &      ' BoundaryCondition(1) = ',e11.4)
                STOP
            ENDIF
            ArrayE(1) = ccm1 / den
            ! HeatFlow(1) = rate at which heat is conducted into mesh 1 from azimuthal and axial directions (btu/ft**3-sec)
            powinp = faz * powin * radsrc(k,1)
            ArrayF(1) = (powinp + (VolumeWeightR(1,k) + VolumeWeightL(1,k)) * HeatFlow(1) + bkt * BoundaryCondition(6)) / den
        END IF
        IF (bcflag) THEN
            DO i = 2, (nmesh - 1)
                aam1 = ccm1
                ccm1 = - ThermalConductivity(i) * AreaWeight(i,k)
                ArrayE(i) = -1.0_r8k
                powinp = faz * powin * radsrc(k,i)
                ArrayF(i) = -(powinp + (VolumeWeightR(i,k) + VolumeWeightL(i,k)) * HeatFlow(i) - aam1 * ArrayF(i-1)) / ccm1
                !
                IF (ndebug) THEN
                    WRITE(ounit,2903)
                    WRITE(ounit,2902) i, powinp, HeatFlow(i), den, ccm1, aam1, AreaWeight(i,k), radsrc(k,i)
2903                FORMAT(' HT1SST: 132:',/,4x,' i',2x,'powinp',4x,'HeatFlow(i)',5x,'den',9x,'ccm1',8x, &
                      &    'aam1',3x,'AreaWeight(i,k)',2x,'radsrc(k,i)')
                ENDIF
            ENDDO
        ELSE
            DO i = 2, (nmesh - 1)
                aam1 = ccm1
                ccm1 = - ThermalConductivity(i) * AreaWeight(i,k)
                den  = - ccm1 - aam1 * (1.0_r8k + ArrayE(i-1))
                ArrayE(i) = ccm1 / den
                powinp = faz * powin * radsrc(k,i)
                ArrayF(i) = (powinp + (VolumeWeightR(i,k) + VolumeWeightL(i,k)) * HeatFlow(i) - aam1 * ArrayF(i-1)) / den
                !
                IF (ndebug) THEN
                    WRITE(ounit,2901)
                    WRITE(ounit,2902) i, powinp, HeatFlow(i), den, ccm1, aam1, AreaWeight(i,k), radsrc(k,i)
2901                FORMAT(' HT1SST: 92:',/,4x,' i',2x,'powinp',4x,'HeatFlow(i)',5x,'den',9x,'ccm1',8x,'aam1',3x, &
                      &    'AreaWeight(i,k)',2x,'radsrc(k,i)')
2902                FORMAT(3x,i3,8(1x,e11.4))
                ENDIF
            ENDDO
        ENDIF
        ! bc coef an, bn in BoundaryCondition(1), BoundaryCondition(2)
        ! bc coef cn in BoundaryCondition(6)
        ! test bc flag for special case
        IF (Iflagn == 1 .OR. Iflagn == 2 .OR. Iflagn == 3) THEN
            ! special case
            SELECT CASE (iflagn)
            CASE (1)
                ! case 1  surface temperature = cn
                FinalTemp(nmesh) = BoundaryCondition(6)
            CASE (2)
                ! case 2  heat flux = cn
                bkt = ThermalConductivity(nmesh-1) * arealoj
                den = -ccm1 * (1.0_r8k + ArrayE(nmesh-1))
                IF (bcflag) EXIT ConvLoop
                powinp = powin*radsrc(k,nmesh)
                FinalTemp(nmesh) = (powinp + bkt * BoundaryCondition(6) - ccm1 * ArrayF(nmesh-1)) / den
                IF (ndebug) WRITE(ounit,4906) FinalTemp(nmesh), powinp, bkt, den, ccm1, BoundaryCondition(6), ArrayF(nmesh-1)
                FinalTemp(nmesh) = BoundaryCondition(6)
            CASE (3)
                ! case 3  convective bc
                bkt = arealoj
                den = bkt * BoundaryCondition(4) - ccm1 * (1.0_r8k + ArrayE(nmesh-1))
                powinp = powin*radsrc(k,nmesh)
                FinalTemp(nmesh) = (powinp + bkt * BoundaryCondition(6) - ccm1 * ArrayF(nmesh-1)) / den
                IF (ndebug) WRITE(ounit,4906) FinalTemp(nmesh), powinp, bkt, den, ccm1, BoundaryCondition(6), ArrayF(nmesh-1)
                FinalTemp(nmesh) = BoundaryCondition(6)
            END SELECT
        ELSE IF (BoundaryCondition(2) == 0.0_r8k) THEN
            ! general case - test If bn=0.0
            ! bn=0.0  =# surface temperature specified
            FinalTemp(nmesh) = BoundaryCondition(6) / BoundaryCondition(4)
        ELSE
            ! test if surface heat flux specified - error if heat flux given at both boundaries
            IF (bcflag .AND. BoundaryCondition(4) == 0.0_r8k) EXIT ConvLoop
            bkt = ThermalConductivity(nmesh-1) * arealoj / BoundaryCondition(2)
            IF (ndebug) WRITE(ounit,*)' arean, BC(2) = ', arean, BoundaryCondition(2)
            den = bkt * BoundaryCondition(4) - ccm1 * (1.0_r8k + ArrayE(nmesh-1))
            powinp = powin * radsrc(k,nmesh)
            FinalTemp(nmesh) = (powinp + bkt * BoundaryCondition(6) - ccm1 * ArrayF(nmesh-1)) / den
            IF (ndebug) WRITE(ounit,4906) FinalTemp(nmesh), powinp, bkt, den, ccm1, BoundaryCondition(6), ArrayF(nmesh-1)
4906        FORMAT('At 107: FinalTemp(nmesh) = ',e11.4,' powinp = ',e11.4,' bkt = ',e11.4, &
              &    ' den = ',e11.4,/' ccm1 = ',e11.4,' BC(6) = ',e11.4,' ArrayF(nmesh-1) = ',e11.4)
            FinalTemp(nmesh) = BoundaryCondition(6)
        END IF
        !
        IF (ndebug) THEN
            WRITE(ounit,*) ' HT1SST:: radsrc(k,i) at k = ',k
            WRITE(ounit,906) (radsrc(k,i),i = 1,nmesh)
            WRITE(ounit,*) '    ArrayE'
            WRITE(ounit,906) (ArrayE(i),i = 1,nmesh)
            WRITE(ounit,*) '    ArrayF'
            WRITE(ounit,906) (ArrayF(i),i = 1,nmesh)
            WRITE(ounit,*) 'FinalTemp(nmesh) = ', FinalTemp(nmesh)
            WRITE(ounit,*) '    BC(6) = ',BoundaryCondition(6)
906         FORMAT(8(2x,e11.4))
        ENDIF
        !
        DO i = 1, (nmesh - 1)
            FinalTemp(nmesh-i) = ArrayF(nmesh-i) - ArrayE(nmesh-i) * FinalTemp(nmesh-i+1)
        ENDDO
        !
        IF (ndebug) THEN
            WRITE(ounit,391) IterationCountSS
            WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
            WRITE(ounit,*) '  PrevIterateTemps '
            WRITE(ounit,906) (PrevIterateTemp(i,k),i = 1,nmesh)
391         FORMAT(' from ht1sst 114, temperature vector IterationCountSS = ',i4)
        ENDIF
        !
        IterationCountSS = IterationCountSS + 1
        Finished = .TRUE.
        !
        DO i = 1, (nmesh - 1)
            IF (ABS(FinalTemp(nmesh-i) - PrevIterateTemp(nmesh-i,k)) > epsht1) Finished = .FALSE.
            PrevIterateTemp(nmesh-i,k) = FinalTemp(nmesh-i)
        ENDDO
        PrevIterateTemp(nmesh,k) = FinalTemp(nmesh)
        ! IF solution converged, set up material property arrays for transient problem and RETURN
        IF (IterationCountSS <= maxit .AND. (.NOT. Finished)) CYCLE
        IF (.NOT. Finished) THEN
            WRITE(ounit,117) IterationCountSS
117         FORMAT(' ******** maximum no. of iterations reached before convergence in heat1 steady state',' IterationCountSS = ',i4)
            WRITE(ounit,118) k, Time
118         FORMAT('nonconvergence at axial node # ',i3,' Time(sec) = ',e11.4)
        END IF
        Finished = .TRUE.
        ssflag = .FALSE.
    END DO ConvLoop
    ! Will not reach this point in code unless error occurred
    IF (bcflag) THEN
        ! no unique solution - problem has gradient specified at both boundaries
        WRITE(ounit, 125)
125     FORMAT('******** surface heat flux specified at both boundaries    no unique solution')
        errsw = 1
    END IF
    !
    END SUBROUTINE ht1sst
    !
    !
    !
    SUBROUTINE ht1tdp (pchn, power, Time0, TimeIncrement, tsurf, qcrit, hflux, hcoef, CoolPress, iht, &
      &                drod, HeatFlux0, emetal, EnrgyMeltZ, EnrgyMeltZp1, tmelt, hfusn, qmaxmelt, &
      &                qmaxmeltp1, powero, SurfHtFlux, AxNodElevat, rl, HeatFlow, RadialBound, paz, &
      &                azpang, theta, fqcrit, voidk, voidcp, tempcm, zroxid, AxialPowr, AxialNodLen, &
      &                RuptFailIndex, rhoc, empytm, emeti, AAHT1, BBHT1, GasPress, bu, GasFraction, &
      &                vrelc, frden, tsntrk, cnfsol, cnfliq, fotmtl, NodeSinterTemp, nmesh, rhof, &
      &                gadolin, burad, k, radsrc, deltox_k, coldw, IndexInitTemp, BOSTemp)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, tfk
    USE variables_fraptran, ONLY : ounit, imaterials, ihData, naxn, nqchn, igpnod, nchfmd, lhtc, IndexThermCon, IndexThermConAdv, &
      &                   iflago, iflagn, IndexBC, noiter, Indexfintemp, ndebug, epsht1, areao, BoundaryCondition, &
      &                   ThermalConductivity, FinalTemp, rhocp, ArrayE, ArrayF, PrevTemp, AreaWeight, arean, acond, &
      &                   nchan, VolumeWeightR, VolumeWeightL, PrevIterateTemp, rhocp0, ThermalConductAdv
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE ZircSpecHeat_fraptran
    USE Reflood_Conditions_fraptran, ONLY : reflood
    USE NCGases_fraptran, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Subroutine ht1tdp solves the 1-d transient heat problem for a time step TimeIncrement long.
    !
    ! Input/Output
    !
    ! bdcond          - Subroutine to supply boundary conditions as ordered pairs of numbers (ao,bo), (an,bn)
    !                   such that the surface heat flux is given by  q(s)=a*t(s) + b
    ! pchn            - fraction of rod power to each channel
    ! power           - power level at time Time0
    ! HeatFlow(l)     - net heat flow by conduction into mesh interval l (btu/ft**3-sec)
    ! Time0           - time at beginning of current time step - t0
    ! TimeIncrement   - length of current time step  (TimeIncrement= Time0 - t0)
    ! errsw           - error switch and problem termination flag
    !                  (0 =# no error, 1 =# error found, 2 =# noiter exceeded, 3 =# temperature limit reached)
    ! acond           - storage area with channel Data and coolant conditions for coolant channel n
    ! acond(16,Nchan) - p = coolant pressure,  psia
    ! tsurf           - fuel rod surface temperature (input and output) (F)
    ! qcrit           - critical heat flux (output) (btu/hr-ft**2)
    ! hflux           - fuel rod surface heat flux (output) (btu/hr-ft**2)
    ! hcoef           - fuel rod surface heat transfer coefficient (output)
    ! CoolPress       - average pressure of coolant surrounding fuel rod (output)
    ! iht             - designates heat transfer mode from fuel rod surface
    ! rl              - fuel rod length (axial z) (ft)
    ! drod            - fuel rod diameter (ft)
    ! HeatFlux0       - surface heat flux from axial node k, calculated in previous time step (btu/ft**2-sec)
    ! emetal          - metal-water reaction heat source at fuel rod surface (btu/sec-foot length)
    ! k               - axial node being analyzed
    ! sp              - source terms to account for phase change (work array)
    ! SurfHtFlux      - vector containing surface heat flux at axial nodes
    ! AxNodElevat     - array of elevations (ft)
    ! IterationCount  - number of past iterations on surface temperature
    ! nvoids          - switch to model central void, 0=off, 1=on
    ! voidk           - thermal conductivity of central void, btu/ft-F-sec
    ! voidcp          - volumetric heat capacity of central void, btu/ft3
    ! nchfsw          - switch to force film boiling , 0=no , 1=yes
    ! RadialBound(l)  - radial coordinate of radial node l (ft)
    ! theta           - azimuthal coordinate (degrees)
    ! igpnod          - radial node at fuel pellet surface
    ! arguments GasPress thru frden are defined in argument dictionary for Subroutine ht1sst
    ! tsntrk          - fuel fabrication sintering temperature (K)
    ! cnfsol          - thermal conductivity of solid phase at melt (btu/s-F-ft)
    ! cnfliq          - liquid phase
    ! frden           - fuel density divided by theoretcal fuel density.
    ! fotmtl          - ratio of oxygen atoms to uranium atoms in fuel.
    ! NodeSinterTemp  - number of mesh fartherest from fuel center that has been sintered any time in past
    ! nmesh           - radial node number at cladding surface.
    ! rhoc            - density of cladding    (lb/ft3)
    ! epsht1          - thermal convergence criterion
    ! Time0           - time at beginning of time step
    ! areao           - surface area at xo
    ! arean           - surcace area at xn time dependent surface area = pi * drod
    ! totsrc          - integrated source
    ! powero          - power level at beginning of time step
    ! matpas          - material Data bypass switch (matpas=1, bypass Call to maData)
    ! power           - power level at End of time step
    ! ssflag          - steady state flag: false for time dependent case
    !
    INTEGER(ipk), PARAMETER :: itfix = 20
    INTEGER(ipk) :: k, nrazp, naazp, nvoids, nchfsw, iterationcount, nodfd, &
      &             ncladi, modfd,modkf, ntheta, jchan, indexrows, iterationcounttrans, &
      &             nmesh, matpas, j, indexvolheatcapacity, newrho, indexadv, indexinittemp, &
      &             nodesintertemp, l, i, iht, mhi, lm, errsw
    REAL(r8k) :: time, time0, timeincrement, faz, dtovr2, &
      &          powin, powero, power, aotov2, cflux, cldtime, frden, fotmtl, rhof, gadolin,&
      &          coldw, rhoc, gaspress, bu, vrelc, tsntrk, rq1, rq2, qmltav, cnfsol, cnfliq, &
      &          voidk, voidcp, temp, tp, ccm1, bkt, bcao, gg, den, r, theta, powinp, qoxl, &
      &          qoxr, emeti, aam1, tm, dr, ro, drod, dv, ai, qovdt, tn0, tnm10, thk, dvo, &
      &          an, bn, emetal, heatflux0, sum, hflux0, frac, hcoef0, hcoef, tsurfo, tsurf, &
      &          hflux, tsurfs, hcoefs, qcrits, hfluxs, ps, empytm, rl, qcrit, tempcm, zroxid, &
      &          conoxide, zroft, deltox_k, hflx0, coolpress, v, qmax, dqmlt, cladflux, &
      &          cladtime, surfdt, ccm2, bkt1, bkt2, bcao1, bcao2, aam2, fqcrit, arealoj,clthlo
    LOGICAL :: ssflag, Finished
    INTEGER(ipk), DIMENSION(:) :: RuptFailIndex
    REAL(r8k), DIMENSION(1) :: hfchn0
    REAL(r8k), DIMENSION(3) :: hfusn, tmelt
    REAL(r8k), DIMENSION(ngases) :: GasFraction
    REAL(r8k), DIMENSION(nmesh) :: qmeltarray
    REAL(r8k), DIMENSION(:) :: qmaxmelt, qmaxmeltp1, HeatFlow, RadialBound, SurfHtFlux, AxNodElevat, &
      &                        AAHT1, AxialPowr, BBHT1, AxialNodLen, azpang
    REAL(r8k), DIMENSION(:,:) :: pchn, paz, EnrgyMeltZ, EnrgyMeltZp1, BOSTemp, burad, radsrc
    !
    Time = Time0 + TimeIncrement
    IF (ndebug) WRITE(ounit,95) k, Time0, TimeIncrement, epsht1
95  FORMAT(' HT1TDP, k = ',i3,' Time0 = ',e13.6,' TimeIncrement = ',e13.6,' epsht1 = ',e11.4)
    !
    nrazp   =     ihData(2)
    naazp   =     ihData(4)
    !nqchn   =     ihData(5)
    nvoids  =     ihData(6)
    nchfsw  =     ihData(7)
    !igpnod  =     ihData(8)
    !naxn    =     ihData(9)
    errsw   =     ihData(15)
    IterationCount   =     ihData(18)
    !lhtc    =     ihData(19)
    modfd   =     ihData(21)
    ! radial node at cladding inside surface
    ncladi  =     ihData(22)
    ! indicator for formula to correct fuel conductivity for cracking
    modkf   =     ihData(23)
    ! ntheta  = # of azimuthal sectors
    ntheta  =     ihData(24)
    !nchfmd  =     ihData(25)
    faz = 1.0_r8k
    errsw = 0
    jchan = 1
    hfchn0(jchan) = SurfHtFlux(k)
    IndexRows = 1
    IterationCountTrans = 0
    ssflag = .FALSE.
    matpas = 0
    ! Use true _fraptran(deformed) cladding perimeter instead of arean, which is not updated with time
    arealoj = 2.0_r8k * pi * RadialBound(nmesh)
    !
    qmeltarray(1:nmesh) = 0.0_r8k
    !
    IndexVolHeatCapacity = IndexThermCon + nmesh + 3
    dtovr2 = TimeIncrement * 0.5_r8k
    powin  = (powero + power) * dtovr2
    aotov2 = areao * dtovr2
    newrho = IndexThermConAdv + nmesh + 3
    ! check If first Call to subcode
    IF (matpas > 0) GOTO 101
    !
    CALL bdcond (BoundaryCondition, Iflago, Iflagn)
    ! evaluate properties at beginning of time step
    IndexAdv = 0
    cflux = 0.0_r8k
    cldtime = 0.0_r8k
    CALL maData (IndexInitTemp, IndexThermCon, IndexBC, IndexVolHeatCapacity, nmesh, IndexRows, &
      &          ssflag, errsw, frden, fotmtl, rhof, gadolin, burad, coldw, cflux, cldtime, k, &
      &          rhoc, IndexAdv)
    !
    CALL kmod (ThermalConductivity, RadialBound, igpnod, GasPress, bu, GasFraction, vrelc, &
      &        modfd, FinalTemp, frden, tsntrk, ncladi, modkf, NodeSinterTemp)
    ! If mesh undergoing phase change, proportion the fuel conductivity according to completion of phase change
    !
    DO l = 1, (igpnod - 1)
        rq1 = EnrgyMeltZp1(l,k) / qmaxmeltp1(l)
        rq2 = EnrgyMeltZ(l+1,k) / qmaxmelt(l+1)
        qmltav = 0.5_r8k * (rq1 + rq2)
        IF (qmltav <= 1.0e-6_r8k) GOTO 80
        IF (qmltav >= (1.0_r8k - 1.0e-6_r8k)) GOTO 80
        ! units in eqn below are btu/s-ft-F
        ThermalConductivity(l) = cnfsol + qmltav * (cnfliq - cnfsol)
80  ENDDO
    ! If central void, override thermal conductivity & volumetric heat capacity for mesh # 1
    IF (nvoids == 1) THEN
        ThermalConductivity(1) = voidk
        RhoCp(1,k) = voidcp
    ENDIF
    IF (errsw == 1) THEN
        WRITE(ounit,905)
905     FORMAT(//, ' Program stopped in ht1tdp after first call to madata ')
        ERROR STOP 'Program stopped in ht1tdp after first call to madata'
    ENDIF
    matpas = 1
    ! set up arrays for gaussian elimination
101 CONTINUE
    IF (ndebug) THEN
        WRITE(ounit,902) IterationCountTrans
        WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
902     FORMAT(' HT1TDP 101, temperature vector IterationCountTrans = ',i4)
    ENDIF
    temp = PrevTemp(1)
    tp = PrevTemp(2)
    ccm1 = -ThermalConductivity(1) * AreaWeight(1,k) * dtovr2
    ! bc coef ao,bo in BoundaryCondition(1), BoundaryCondition(2), resp.
    ! bc coef co in BoundaryCondition(6)
    ! test bc flag for special case
    IF (Iflago > 0 .AND. Iflago <= 3) GOTO 105
    ! general case - test If bo=0.0
    IF (BoundaryCondition(2) == 0.0_r8k) GOTO 104
    bkt = ThermalConductivity(1) * aotov2 / BoundaryCondition(2)
102 CONTINUE
    bcao = bkt * BoundaryCondition(1)
103 CONTINUE
    gg = RhoCp(1,k) * VolumeWeightR(1,k)
    den = gg - ccm1 + bcao
    ArrayE(1) = ccm1 / den
    powinp = faz * powin * radsrc(k,1)
    ArrayF(1) = (powinp + (VolumeWeightR(1,k) + VolumeWeightL(1,k)) * HeatFlow(1) * TimeIncrement - ccm1 * tp + &
      &         (gg + ccm1 - bcao) * temp + bkt * BoundaryCondition(6) * 2.0_r8k + qmeltarray(1)) / den
    GOTO 109
    ! bo=0.0  surface temperature supplied
104 CONTINUE
    ArrayE(1) = 0.0_r8k
    ArrayF(1) = BoundaryCondition(6) / BoundaryCondition(1)
    GOTO 109
    ! special case
105 CONTINUE
    IF (Iflago-2 < 0) GOTO 106
    IF (Iflago-2 == 0) GOTO 107
    IF (Iflago-2 > 0) GOTO 108
    ! case 1  surface temperature = co
106 CONTINUE
    ArrayE(1) = 0.0_r8k
    ArrayF(1) = BoundaryCondition(6)
    GOTO 109
    ! case 2  surface flux = co
107 CONTINUE
    bkt = ThermalConductivity(1) * aotov2
    bcao = 0.0_r8k
    GOTO 103
    ! case 3  convective bc
108 CONTINUE
    bkt = aotov2
    GOTO 102
    !
109 CONTINUE
    !
    IF (ndebug) WRITE(ounit,291)
291 FORMAT(/' HT1TDP 91:'/4x,' i',4x,'powinp',4x,'HeatFlow(i)',4x,'temp',9x,'tp',9x,'ccm1',8x,'aam1',9x,'gg')
    DO i = 2, (nmesh - 1)
        qoxl = 0.0_r8k
        qoxr = 0.0_r8k
        IF (i == (igpnod+1)) qoxr = emeti
        IF (i == (igpnod+2)) qoxl = emeti
        aam1 = ccm1
        tm = temp
        temp = tp
        tp = PrevTemp(i+1)
        ccm1 = - ThermalConductivity(i) * AreaWeight(i,k) * dtovr2
        gg = RhoCp(i-1,k) * VolumeWeightL(i,k) + RhoCp(i,k) * VolumeWeightR(i,k)
        den = gg - ccm1 - aam1 * (1.0_r8k + ArrayE(i-1))
        ArrayE(i) = ccm1 / den
        powinp = faz * powin * radsrc(k,i)
        ArrayF(i) = (powinp + (VolumeWeightR(i,k) + VolumeWeightL(i,k)) * HeatFlow(i) * TimeIncrement + VolumeWeightR(i,k) * &
          &          qoxr * TimeIncrement + VolumeWeightL(i,k) * qoxl * TimeIncrement - aam1 * tm + (gg + aam1 + ccm1) * &
          &          temp - ccm1 * tp - aam1 * ArrayF(i-1) + qmeltarray(i)) / den
        IF (ndebug) WRITE(ounit,292) i, powinp, HeatFlow(i), temp, tp, ccm1, aam1, gg
292     FORMAT(3x,i3,7(1x,e11.4))
    ENDDO
    !
    dr = RadialBound(nmesh) - RadialBound(nmesh-1)
    ro = drod / 2.0_r8k
    dv = pi * (ro * dr - dr ** 2 / 4.0_r8k)
    ai = 2.0_r8k * pi * (ro - dr / 2.0_r8k)
    qovdt = RhoCp(nmesh-1,k) * dv / TimeIncrement
    tn0 = PrevTemp(nmesh)
    tnm10 = PrevTemp(nmesh-1)
    ! compute coefficients an, bn in n-th finite difference equation
    ! compute thickness of outer half-mesh
    thk = (RadialBound(nmesh) - RadialBound(nmesh-1)) / 2.0_r8k
    ! compute area of outer half mesh
    dvo = pi * (ro ** 2 - (ro - thk) ** 2)
    ! HeatFlow(nmesh-1) * dvo = rate at which heat is conducted into outer half mesh  (btu/sec)
    an = -0.5_r8k * ai * ThermalConductivity(nmesh-1) / dr
    bn = qovdt - an
    ! a * (surface temperature) + b = surface flux
60  CONTINUE
    IF (ndebug) THEN
        WRITE(ounit,904) IterationCountTrans
        WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
904     FORMAT(' HT1TDP 60, temperature vector IterationCountTrans = ',i4)
    ENDIF
    !
    clthlo = 0.5_r8k * (powero + power) * radsrc(k,nmesh)
    AAHT1(k) = -(bn - an * ArrayE(nmesh-1)) / (0.5_r8k * arealoj)
    BBHT1(k) = -(an * ArrayF(nmesh-1) - qovdt * tn0 - an * (tn0 - tnm10) - emetal - clthlo - &
      &          HeatFlow(nmesh-1) * dvo + 0.5_r8k * arealoj * HeatFlux0) / (0.5_r8k * arealoj)
    sum = 0.0_r8k
    hflx0 = 0.0_r8k
    jchan = 1
    frac = pchn(jchan,1)
    hflx0 = hflx0 + SurfHtFlux(k) * frac
    sum = sum + frac
    hflx0 = hflx0 / sum
    ! evaluate coolant conditions needed for surface temperature calculation
    hcoef0 = hcoef
    tsurfo = tsurf
    hflux = HeatFlux0
    sum = 0.0_r8k
    tsurfs = 0.0_r8k
    hcoefs = 0.0_r8k
    qcrits = 0.0_r8k
    hfluxs = 0.0_r8k
    ps = 0.0_r8k
    hflux = HeatFlux0
    ! the reflood heat transfer package is called here
    IF (Time0 >= empytm) THEN
        CALL reflood (AxNodElevat, AAHT1(k), BBHT1(k), drod, AxialNodLen, Time, TimeIncrement, &
          &           AxialPowr, naxn, rl, RuptFailIndex, acond, tsurf, hflux, hcoef, qcrit, iht, k, Nchan)
    ELSE
        CALL htrc (drod, hcoef, hflux, qcrit, tsurf, iht, k, rl, nchfsw, tempcm, zroxid, tmelt(2))
    ENDIF
    !
    IF (ndebug) THEN
        WRITE(ounit,903) IterationCountTrans
        WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
903     FORMAT(' HT1TDP 40, temperature vector IterationCountTrans = ',i4)
    ENDIF
    frac = pchn(jchan,1)
    sum = sum + frac
    tsurfs = tsurfs + frac * tsurf
    hcoefs = hcoefs + frac * hcoef
    hfluxs = hfluxs + frac * hflux
    ps = ps + frac * acond(16,Nchan)
    qcrits = qcrits + frac * qcrit
    tsurf = tsurfs / sum
    ! calculate delta T across outer oxide layer and convert tsurf from F to K
    ! calculate oxide thermal conductivity and convert from W/m-K to btu/ft2-hr-F
    conoxide = MatProperty (Material='OXIDE', Property='THERMCOND', Temperature=tfk(tsurf)) * 0.57803_r8k
    ! convert oxide thickness from m to ft
    zroft = zroxid * 3.28084_r8k
    ! calculate deltat T across oxide (F)
    deltox_k = hflux * zroft / conoxide
    ! add oxide delta T to surface T for boundary condition
    BoundaryCondition(6) = tsurf + deltox_k
    ! BoundaryCondition(6) = tsurf
    hflux = hfluxs / sum
    hcoef = hcoefs / sum
    CoolPress = ps / sum
    qcrit = qcrits / sum
    IF (IterationCount < itfix .OR. tsurf > tsurfo) THEN
        ! add oxide delta T to surface T for boundary condition
        BoundaryCondition(6) = tsurf + deltox_k
        IF (ndebug) WRITE(ounit,1111) IterationCount, IterationCountTrans, BoundaryCondition(6), tsurf, deltox_k
1111    FORMAT(' HT1TDP: IterationCount = ',i3,' IterationCountTrans = ',i3, &
          &    ' BC(6) = ',e11.4,' tsurf = ',e11.4,' deltox = ',e11.4)
        FinalTemp(nmesh) = BoundaryCondition(6)
        FinalTemp(nmesh) = tsurf
    ELSE
        ! after itfix iterations, tsurf not changed unless value just
        ! computed is higer than value computed in past iteration
        tsurf = tsurfo
        hflux = hflx0
        hcoef = hcoef0
        jchan = 1
        SurfHtFlux(k) = hfchn0(jchan)
    END IF
    ! Calculate temperatures
    IF (ndebug) THEN
        WRITE(ounit,*) ' HT1TDP: ArrayE'
        WRITE(ounit,906) (ArrayE(i),i = 1,nmesh)
        WRITE(ounit,*) ' HT1TDP: ArrayF'
        WRITE(ounit,906) (ArrayF(i),i = 1,nmesh)
    ENDIF
    !
    DO i = 1, (nmesh - 1)
        FinalTemp(nmesh-i) = ArrayF(nmesh-i) - ArrayE(nmesh-i) * FinalTemp(nmesh-i+1)
    ENDDO
    !
    IF (ndebug) THEN
        WRITE(ounit,901) IterationCountTrans
        WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
901     FORMAT(' HT1TDP 119, temperature vector IterationCountTrans = ',i4)
906     FORMAT(8(2x,e11.4))
    ENDIF
    ! check for melting or freezing
    mhi = 0
    !
    DO i = 1, (nmesh - 1)
        DO j = 1, 2
            IF (mhi == 0) GOTO 222
            ! check for melting in half interval
            lm = imaterials(i+j-2)
            IF (j == 1) THEN
              v = VolumeWeightL(i,k)
            ELSE
              v = VolumeWeightR(i,k)  
            ENDIF
            qmax = v * hfusn(lm)
            IF (j == 1) qmaxmelt(i) = qmax
            IF (j == 2) qmaxmeltp1(i) = qmax
            IF (FinalTemp(i) < tmelt(lm) .AND. EnrgyMeltZ(i,k) < 1.0e-20_r8k .AND. j == 1) GOTO 215
            IF (FinalTemp(i) > tmelt(lm) .AND. EnrgyMeltZ(i,k) >= qmax .AND. j == 1) GOTO 215
            IF (FinalTemp(i) < tmelt(lm) .AND. EnrgyMeltZp1(i,k) < 1.0e-20_r8k .AND. j == 2) GOTO 215
            IF (FinalTemp(i) > tmelt(lm) .AND. EnrgyMeltZp1(i,k) >= qmax .AND. j == 2) GOTO 215
            !
            dqmlt = RhoCp(i+j-2,k) * v * (FinalTemp(i) - tmelt(lm))
            IF (j == 1) EnrgyMeltZ(i,k) = EnrgyMeltZ(i,k) + dqmlt
            IF (j == 2) EnrgyMeltZp1(i,k) = EnrgyMeltZp1(i,k) + dqmlt
            IF (EnrgyMeltZ(i,k) < 0.0_r8k .AND. j == 1) GOTO 212
            IF (EnrgyMeltZp1(i,k) < 0.0_r8k .AND. j == 2) GOTO 212
            IF (EnrgyMeltZ(i,k) <= qmax .AND. j == 1) GOTO 220
            IF (EnrgyMeltZp1(i,k) <= qmax .AND. j == 2) GOTO 220
            ! Complete melting
            SELECT CASE (j)
            CASE (1)
                dqmlt = qmax - EnrgyMeltZ(i,k)
                EnrgyMeltZ(i,k) = qmax
            CASE (2)
                dqmlt = qmax - EnrgyMeltZp1(i,k)
                EnrgyMeltZp1(i,k) = qmax
            END SELECT
            GOTO 220
            ! complete freezing
212         CONTINUE
            IF (j == 1) dqmlt = EnrgyMeltZ(i,k)
            IF (j == 2) dqmlt = EnrgyMeltZp1(i,k)
            IF (j == 1) EnrgyMeltZ(i,k) = 0.0_r8k
            IF (j == 2) EnrgyMeltZp1(i,k) = 0.0_r8k
            GOTO 220
215         CONTINUE
            dqmlt = 0.0_r8k
220         CONTINUE
            qmeltarray(i) = qmeltarray(i) - dqmlt
222         CONTINUE
            mhi = mhi + 1
        ENDDO
    ENDDO
    !
    IF (IterationCountTrans == noiter) RETURN
    IterationCountTrans = IterationCountTrans + 1
    errsw  = 0
    Finished = .TRUE.
    ! Test for convergence
    DO i = 1, nmesh
        IF (ABS(PrevIterateTemp(i,k) - FinalTemp(i)) > epsht1) Finished = .FALSE.
        PrevIterateTemp(i,k) = FinalTemp(i)
    ENDDO
    PrevIterateTemp(nmesh,k) = FinalTemp(nmesh)
    ! is solution converged? - RETURN
    IF (Finished) RETURN
    errsw = 2
    ! get updated boundary conditions
    CALL bdcond (BoundaryCondition, Iflago, Iflagn)
    !
    IndexAdv = 1
    CALL maData (IndexFinTemp, IndexThermConAdv, IndexBC, newrho, nmesh, IndexRows, ssflag, errsw, &
      &          frden, fotmtl, rhof, gadolin, burad, coldw, cladflux, cladtime, k, rhoc, IndexAdv)
    ! Call Subroutine which computes cladding specific heat more rigorously in case of large temperature increase during time step
    ! in alpha-beta phase transition
    CALL ccpmod (RhoCp, BOSTemp, FinalTemp, k, rhoc, RhoCp0)
    !
    CALL kmod (ThermalConductAdv, RadialBound, igpnod, GasPress, bu, GasFraction, vrelc, modfd, &
      &        FinalTemp, frden, tsntrk, ncladi, modkf, NodeSinterTemp)
    ! If mesh undergoing phase change, proportion the fuel conductivity according to completion of phase change
    DO l = 1, (igpnod - 1)
      rq1 = EnrgyMeltZp1(l,k) / qmaxmeltp1(l)
      rq2 = EnrgyMeltZ(l+1,k) / qmaxmelt(l+1)
      qmltav = 0.5_r8k * (rq1 + rq2)
      IF (qmltav > 1.0e-6_r8k .AND. qmltav < (1.0_r8k - 1.0e-6_r8k)) &
        &  ThermalConductivity(l) = cnfsol + qmltav * (cnfliq - cnfsol) ! store fuel conductivity in units of btu/s-ft-F
    ENDDO
    ! If central void, override thermal conductivity & volumetric heat capacity for mesh # 1
    IF (nvoids == 1) THEN
        ThermalConductAdv(1) = voidk
        RhoCp(1,k) = voidcp
    ENDIF
    IF (errsw == 1) THEN
        WRITE(ounit,9052)
9052    FORMAT(//, ' Program stopped in ht1tdp after second call to madata ')
        ERROR STOP 'Program stopped in ht1tdp after second call to madata'
    ENDIF
    ! set up gaussian elimination using advanced propeties
    temp = PrevTemp(1)
    tp = PrevTemp(2)
    surfdt = AreaWeight(1,k) * dtovr2
    ccm1 = -ThermalConductivity(1) * surfdt
    ccm2 = -ThermalConductAdv(1) * surfdt
    ! bc coef ao, bo in BoundaryCondition(1), BoundaryCondition(2), resp.
    ! bc coef co in BoundaryCondition(6)
    ! test bc flag for special case
    IF (Iflago > 0 .AND. Iflago <= 3) GOTO 124
    ! general case - test If bo=0.0
    IF (BoundaryCondition(2) == 0.0_r8k) GOTO 123
    bkt1 = ThermalConductivity(1) * aotov2 / BoundaryCondition(2)
    bkt2 = ThermalConductAdv(1) * aotov2 / BoundaryCondition(2)
121 CONTINUE
    bcao1 = bkt1 * ThermalConductivity(1)
    bcao2 = bkt2 * BoundaryCondition(1)
122 CONTINUE
    gg = (RhoCp(1,k) + RhoCp0(1,k)) * VolumeWeightR(1,k) * 0.5_r8k
    den = gg - ccm2 + bcao2
    ArrayE(1) = ccm2 / den
    powinp = faz * powin * radsrc(k,1)
    ArrayF(1) = (powinp + (VolumeWeightR(1,k) + VolumeWeightL(1,k)) * HeatFlow(1) * TimeIncrement - ccm1 * tp + &
      &          bkt1 * BoundaryCondition(6) + bkt2 * BoundaryCondition(6) + (gg + ccm1 - bcao1) * temp + qmeltarray(1)) / den
    GOTO 128
    ! bc bo=0.0  surface temp specified
123 CONTINUE
    ArrayE(1) = 0.0_r8k
    ArrayF(1) = BoundaryCondition(6) / BoundaryCondition(1)
    GOTO 128
    ! special case
124 CONTINUE
    IF (Iflago-2 < 0) GOTO 125
    IF (Iflago-2 == 0) GOTO 126
    IF (Iflago-2 > 0) GOTO 127
    ! case 1  surface temperature = co
125 CONTINUE
    ArrayE(1) = 0.0_r8k
    ArrayF(1) = BoundaryCondition(6)
    GOTO 128
    ! case 2  surface flux = co
126 CONTINUE
    bkt1  = ThermalConductivity(1) * aotov2
    bkt2  = ThermalConductAdv(1) * aotov2
    bcao1 = 0.0_r8k
    bcao2 = 0.0_r8k
    GOTO 122
    ! case 3  convective bc
127 CONTINUE
    bkt1 = aotov2
    bkt2 = aotov2
    GOTO 121
    !
128 CONTINUE
    !
    IF (ndebug) WRITE(ounit,293)
293 FORMAT(/' HT1TDP 93:'/4x,' i',4x,'powinp',4x,'HeatFlow(i)',4x,'temp',9x,'tp',9x,'ccm1',8x,'aam1',9x,'gg')
    DO i = 2, (nmesh - 1)
        qoxl = 0.0_r8k
        qoxr = 0.0_r8k
        IF (i == (igpnod+1)) qoxr = emeti
        IF (i == (igpnod+2)) qoxl = emeti
        aam1 = ccm1
        aam2 = ccm2
        tm = temp
        temp = tp
        tp = PrevTemp(i+1)
        surfdt = AreaWeight(i,k) * dtovr2
        ccm1 = - ThermalConductivity(i) * surfdt
        ccm2 = - ThermalConductAdv(i) * surfdt
        gg = ((RhoCp(i-1,k) + RhoCp0(i-1,k)) * VolumeWeightL(i,k) + (RhoCp(i,k) + RhoCp0(i,k)) * VolumeWeightR(i,k)) * 0.5_r8k
        den = gg - ccm2 - aam2 * (1.0_r8k + ArrayE(i-1))
        ArrayE(i) = ccm2 / den
        powinp = faz * powin * radsrc(k,i)
        ArrayF(i) = (powinp + (VolumeWeightR(i,k) + VolumeWeightL(i,k)) * HeatFlow(i) * TimeIncrement + &
          &          VolumeWeightR(i,k) * qoxr * TimeIncrement + VolumeWeightL(i,k) * qoxl * TimeIncrement - &
          &          aam1 * tm - ccm1 * tp + (gg + aam1 + ccm1) * temp - aam2 * ArrayF(i-1) + qmeltarray(i)) / den
        !
        IF (ndebug) WRITE(ounit,292) i, powinp, HeatFlow(i), temp, tp, ccm1, aam1, gg
    ENDDO
    !
    qovdt = 0.5_r8k * (RhoCp(nmesh-1,k) + RhoCp0(nmesh-1,k)) * dv / TimeIncrement
    an = - 0.25_r8k * ai * (ThermalConductivity(nmesh-1) + ThermalConductAdv(nmesh-1)) / dr
    bn = qovdt - an
    GOTO 60
    !
    END SUBROUTINE ht1tdp
    !
    !
    !
    SUBROUTINE maData (IndexFinTemp, IndexThermCon, IndexBC, IndexVolHeatCapacity, nmesh, IndexRows, &
      &                ssflag, errsw, frden, fotmtl, rhof, gadolin, burad, coldw, cflux, cldtime, k, &
      &                rhoc, IndexAdv)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit, nomat, imaterials, naxn, ndebug
    USE phypro_h_fraptran
    USE heatconduction_h_fraptran
    USE Material_Properties_fraptran, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> This subcode computes the thermal conductivity and volumetric heat
    !> capacity for each mesh element evaluated at the average temperature of the element.
    !> boundary conditions are of the form q(s) = a*t(s)+b where a and b are specified for each surface element
    !
    ! Input
    !
    ! array                - floating point storage space passed to heat1
    ! rhoc                 - density of cladding (lb/ft3)
    ! IndexFinTemp         - index of temperatures
    ! IndexThermCon        - index in array where thermal conductivities are to be stored
    ! IndexVolHeatCapacity - index in array where volumetric heat capacities are to be stored. Boundary condition parameters
    !                        a, b are stored in the thcon and Rodrhocp arrays, resp., at the surface locations.
    ! IndexBC              - index in array of boundary conditions. Boundary condition parameters are specified as ordered pairs
    !                        of values (a,b) for each surface element beginning at the origin and proceeding by rows along column
    !                        bndry 1, Then column bndry cols. If 2-d, Then along row bndry 1 and row bndry rows, arrange data 
    !                        in the following order
    ! nmesh                - no. of mesh points in x-direction  (columns)
    ! IndexRows            - no. of mesh points in y-direction  (1 If 1-d problem)
    ! ssflag               - If true => no heat capacities to be calculated such as in ss prob
    ! errsw                - errsw switch  (0 = no error,  1 = error)
    !
    ! Output
    !
    ! ThermalConductivity  - Thermal conductivity
    ! Rhocp                - Volumetric heat capacity Returned
    !
    ! Internal
    !
    ! thcon                - Thermal conductivity
    ! Rodrhocp             - value of volumetric heat capacity
    ! Material             - material no. for which properties are requested
    ! tempn                - current value of temperature
    !
    INTEGER(ipk) :: IndexColumnBoundary, IndexRowBoundary, Material, i, icol, l1, krtrn, &
      &             indexthermconductivity, indexheatcapacity, indexvolheatcapacity, midx, &
      &             indextemp, indexfintemp, mtrlo, indx, indexbc, lridx, jrow, mrtn, indexadv
    INTEGER(ipk), INTENT(IN) :: k, nmesh, IndexRows, IndexThermCon
    INTEGER(ipk), INTENT(INOUT) :: errsw
    REAL(r8k) :: tempn, bulocal, tempnk, cldtime, cflux, coldw, ccon, frden, fotmtl, &
      &          gadolin, rhof, rhoc, Rodrhocp = 0.0_r8k, thcon = 0.0_r8k
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    LOGICAL, INTENT(IN) :: ssflag
    REAL(r8k), DIMENSION(1) :: array
    REAL(r8k), DIMENSION(:,:) :: burad
    !
    errsw = 0
    IF (ndebug) WRITE(ounit,904) k
904 FORMAT(' madata entered, k = ',i6)
    IndexColumnBoundary = nmesh - 1
    IndexRowBoundary = IndexRows - 1
    IndexThermConductivity = IndexThermCon
    IndexHeatCapacity = IndexVolHeatCapacity
    midx = 1
    IndexTemp = IndexFinTemp
    mtrlo = 0
    IF (IndexRows == 1) GOTO 102
    ! 2-d problem - get boundary condition parameter for row 1
    indx = IndexBC + 4 * IndexRowBoundary
    array(IndexThermConductivity) = 0
    array(IndexHeatCapacity) = 0
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    !
    DO icol = 1, IndexColumnBoundary
        array(IndexThermConductivity) = array(indx)
        array(IndexHeatCapacity) = array(indx+1)
        IndexThermConductivity = IndexThermConductivity + 1
        IndexHeatCapacity = IndexHeatCapacity + 1
        indx = indx + 2
    ENDDO
    !
    array(IndexThermConductivity) = 0
    array(IndexHeatCapacity) = 0
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    lridx = IndexFinTemp + nmesh
    jrow = 1
1100 CONTINUE
    ! Store left bc parameter ao or bo
    array(IndexThermConductivity) = array(indx)
    array(IndexHeatCapacity) = array(indx+1)
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    indx = indx + 2
    icol = 1
999 CONTINUE
    Material = imaterials(midx)
    tempn = 0.25_r8k * (array(IndexTemp) + array(IndexTemp+1) + array(lridx) + array(lridx+1))
    !
    mrtn = 97
    krtrn = 98
    !
    IF (mtrlo == Material) GOTO 108
    mtrlo = Material
    GOTO 106
    !
97  CONTINUE
    RhoCp(icol,k) = Rodrhocp
98  CONTINUE
    IF (IndexAdv == 0) THEN
        ThermalConductivity(icol) = thcon
    ELSE
        ThermalConductAdv(icol) = thcon
    ENDIF
    !
    IndexTemp = IndexTemp + 1
    lridx = lridx + 1
    midx = midx + 1
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    icol = icol + 1
    IF (icol <= IndexColumnBoundary) GOTO 999
    IndexTemp = IndexTemp + 1
    lridx = lridx + 1
    ! Store right bc parameter an or bn
    array(IndexThermConductivity) = array(indx)
    array(IndexHeatCapacity) = array(indx+1)
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    indx = indx + 2
    jrow = jrow + 1
    IF (jrow <= IndexRowBoundary) GOTO 1100
    ! Store bc parameter for row bndry IndexRows
    indx = indx + 2 * IndexColumnBoundary
    array(IndexThermConductivity) = 0
    array(IndexHeatCapacity) = 0
    IndexThermConductivity = IndexThermConductivity + 1
    IndexHeatCapacity = IndexHeatCapacity + 1
    !
    DO icol = 1, IndexColumnBoundary
        array(IndexThermConductivity) = array(indx)
        array(IndexHeatCapacity) = array(indx+1)
        IndexThermConductivity = IndexThermConductivity + 1
        IndexHeatCapacity = IndexHeatCapacity + 1
        indx = indx + 2
    ENDDO
    !
    array(IndexThermConductivity) = 0
    array(IndexHeatCapacity) = 0
    RETURN
    !
    ! bc given as a(t,time) * t + b(t,time) * grad t.n = c(t,time)
    ! store left bc parameters  (ao, bo in thcon array, co, Do in Rodrhocp array)
    !
    !     1-D problem
    !
102 CONTINUE
    icol = 1
    l1 = IndexFinTemp
    IF (ndebug) THEN
        WRITE(ounit,902)
        WRITE(ounit,906) (FinalTemp(i),i = 1,nmesh)
906     FORMAT(8(2x,e11.4))
902     FORMAT('from madata, temperature vector')
    ENDIF
1105 CONTINUE
    Material = imaterials(midx)
    tempn = 0.50_r8k * (FinalTemp(icol) + FinalTemp(icol+1))
    IF (ndebug) WRITE(ounit,907) icol, IndexAdv, tempn
907 FORMAT(' icol = ',i4,' IndexAdv = ',i4,' tempn = ',e11.4)
    IF (Material /= 1) GOTO 9000
    !
    bulocal = 0.5_r8k * (burad(k,icol) + burad(k,icol+1))
9000 CONTINUE
    !
    mrtn = 103
    krtrn = 104
    !
    IF (mtrlo == Material) GOTO 108
    mtrlo = Material
    GOTO 106
    !
103 CONTINUE
    RhoCp(icol,k) = Rodrhocp
104 CONTINUE
    IF (IndexAdv == 0) THEN
        ThermalConductivity(icol) = thcon
    ELSE
        ThermalConductAdv(icol) = thcon
    ENDIF
    !
    midx = midx + 1
    IndexTemp = IndexTemp + 1
    icol = icol + 1
    IF (icol <= IndexColumnBoundary) GOTO 1105
    RhoCp(nmesh,k) = BoundaryCondition(3)
    IF (IndexAdv == 0) THEN
        ThermalConductivity(nmesh) = BoundaryCondition(4)
    ELSE
        ThermalConductAdv(nmesh) = BoundaryCondition(4)
    ENDIF
    !
    RETURN
    !
106 CONTINUE
    !
    IF (Material <= 0 .OR. Material > nomat) THEN
        WRITE(ounit,126) Material, nomat
126     FORMAT('******* material no. ',i2,' is not within expected ',/,10x,'range from 1-',i1)
        errsw = 1
        RETURN
    END IF
    !
108 CONTINUE
    ! Calculate thermal conductivity for each mesh
    tempnk = tfk(tempn)
    SELECT CASE (Material)
    CASE (3)
        ! Gas
        thcon = GapConductivity
        IF (ndebug) WRITE(ounit,*) 'Gap thcon, tempn = ',thcon, tempn
    CASE (2)
        ! Clad
        thcon = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=tempnk, Flux=cflux, ColdWork=coldw) * 1.6056e-04_r8k
        
        IF (ndebug) WRITE(ounit,*) 'Clad thcon, tempn = ',thcon, tempn
    CASE (1)
        ! Fuel
        thcon = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tempnk, Burnup=bulocal, OMRatio=fotmtl, &
          &                  Fraction_TD=frden, Pu=compmt, Gadolinia=gadolin) * 1.6056e-04_r8k
        IF (ndebug) WRITE(ounit,*) 'Fuel thcon, tempn = ',thcon, tempn
    END SELECT
    !
    IF (ssflag) THEN
        IF (krtrn == 98) GOTO 98
        IF (krtrn == 104) GOTO 104
    ENDIF
    ! Calculate heat capacity for each mesh
    SELECT CASE (Material)
    CASE (1)
        Rodrhocp = MatProperty (Material='FUEL', Property='SPECHEAT', Temperature=tempnk, OMRatio=fotmtl, &
          &                     Pu=compmt, Fraction_TD=frden, Fraction_Melt=zero) / 4.1868e3_r8k * rhof
    CASE (2)
        Rodrhocp = MatProperty (Material='CLAD', Property='SPECHEAT', Temperature=tempnk) / 4.186e3_r8k * rhoc
    CASE (3)
        Rodrhocp = 0.0_r8k
    END SELECT
    IF (mrtn == 97) GOTO 97
    IF (mrtn == 103) GOTO 103
    !
    END SUBROUTINE madata
    !
    !
    !
    PURE SUBROUTINE bdcond (BoundaryCondition, Iflago, Iflagn)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes boundary conditions required for heat-1 subcodes
    !
    INTEGER(ipk), INTENT(OUT) :: Iflago, Iflagn
    REAL(r8k), DIMENSION(6), INTENT(INOUT) :: BoundaryCondition
    !
    Iflago = 0
    Iflagn = 0
    BoundaryCondition(1) = 0.0_r8k
    BoundaryCondition(2) = 1.0_r8k
    BoundaryCondition(3) = 0.0_r8k
    BoundaryCondition(4) = 1.0_r8k
    BoundaryCondition(5) = 0.0_r8k
    !
    END SUBROUTINE bdcond
    !
    !
    SUBROUTINE ftbmov (a, b, n)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Subroutine moves n words from a to b in memory. If n is positive, the move is forward,
    !> i.e., a(1)->b(1), a(2)->b(2)... If n is negative, the move is backward,
    !> i.e., a(n)->b(n), a(n-1)->b(n-1)...
    !>@author
    !> $Id: envrl.s,v 2.102 1999/05/19 23:08:13 randyt Exp randyt $
    !> Cognizant engineer: rjw
    !> Modified by Ian Porter, NRC, April 2014, to clean coding and convert to .f90
    !***********************************************************************
    !
    ! Data dictionary for local variables
    !
    ! Number of local variables =    4
    !
    ! i=INTEGER(ipk) r=Real l=LOGICAL c=CHARACTER
    !***********************************************************************
    ! Type    Name                              Definition
    !-----------------------------------------------------------------------
    !  r      a(1)                             =
    !  r*8    a(1)                             =
    !  r      b(1)                             =
    !  r*8    b(1)                             =
    !***********************************************************************
    !
    INTEGER(ipk) :: j, i
    INTEGER(ipk), INTENT(IN) :: n
    REAL(r8k), INTENT(INOUT) :: a(ABS(n)), b(ABS(n))
    !
    IF (n < 0) THEN
        j = -n
        DO i = 1, ABS(n)
            b(j) = a(j)
            j = j - 1
        ENDDO
    ELSE IF (n > 0) THEN
            DO i = 1, n
                b(i) = a(i)
            ENDDO
    ELSE ! n = 0
        WRITE (ounit,*) 'Error in subroutine ftbmov. n = 0. Code Execution Stopped.'
        ERROR STOP 'Error in subroutine ftbmov. n = 0. Code Execution Stopped.'
    ENDIF
    !
    END SUBROUTINE ftbmov
    !
    !
    SUBROUTINE error1 (nerr, nstop)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit
    USE ErrorMsg_fraptran, ONLY : fabend
    IMPLICIT NONE
    !>@brief
    !> Subroutine prints out specified error messages
    !
    INTEGER(ipk) :: nerr, nstop
    !
    WRITE(ounit,5)
5   FORMAT(5(/' *******'),' Error detected *******' )
    !
    SELECT CASE (nerr)
    CASE (1)
        WRITE(ounit,12)
12      FORMAT(/' Error found during execution of Subroutine ht1sst ')
        GOTO 900
    CASE (2)
        WRITE(ounit,22)
22      FORMAT(/' Maximum number of iterations in Subroutine ht1sst exceeded ')
        GOTO 900
    CASE (3)
        WRITE(ounit,32)
32      FORMAT(' Error returned from routine ht1tdp')
        WRITE(ounit,902)
        STOP 10
    CASE (4)
        WRITE(ounit,42)
42      FORMAT(/' Maximum number of iterations in Subroutine ht1tdp exceeded ')
        GOTO 900
    CASE (5)
        WRITE(ounit,52)
52      FORMAT(' Temperature limit exceeded in routine ht1tdp')
        GOTO 910
    CASE (6)
        WRITE(ounit,62)
62      FORMAT(' ******* The wrong number of variables was passed to routine plotw for writing to the plot data set.',/, &
            &  ' ******* The plots should be discarded or checked carefully.',/,5(' *******'/))
        GOTO 900
    CASE DEFAULT
        WRITE(ounit,202) nerr
202     FORMAT(' Undefined error, no.',i5)
        GOTO 910
    END SELECT
900 If (nstop < 1) GOTO 910
    IF (nstop >= 1) GOTO 920
910 WRITE(ounit,902)
902 FORMAT(/' Execution stopped ')
    !
    CALL fabEnd
    ERROR STOP
    !
920 WRITE(ounit,922)
922 FORMAT(/' Execution continuing ')
    !
    END SUBROUTINE error1
    !
    !
    SUBROUTINE kmod (ThermalConductivity, RadialBound, igpnod, GasPress, bu, GasFraction, vrelc, &
      &              modfd, FinalTemp, frden, tsntrk, ncladi, modkf, NodeSinterTemp)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, ftmetr, psinm2, tfk
    USE variables_fraptran, ONLY : ounit, Time, ndebug
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE NCGases_fraptran, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Subroutine modifies fuel thermal conductivity to account for fuel circumferential cracks and burnup
    !
    ! Input
    !
    ! ThermalConductivity(l) - conductivity of l-th radial mesh (btu/ft-F-sec) input unmodified conductivity
    ! RadialBound(l+1)       - outside radius of l-th radial mesh  (ft)
    ! igpnod                 - radial node number at fuel pellet surface
    ! GasPress               - fuel rod gas pressure  (psia)
    ! bu                     - fuel burnup (mw-sec/kg)
    ! GasFraction(n)         - fraction of gas that belongs to n-th component
    ! vrelc                  - void volume generated by fuel relocation (void fraction, radially averaged over fuel cross-section.)
    ! modfd                  - fuel deformation model type.
    ! FinalTemp(l+1)         - temperature at outside radius of l-th mesh
    ! frden                  - fraction fuel density to maximum theoretical density
    ! tsntrk                 - fuel sintering temperature (K)
    ! ncladi                 - radial node number at cladding inside surface
    ! modkf                  - switch to specify formula for correction factor to account for effect of cracks
    !                          upon fuel thermal conductivity
    !                      0 = Frap-s3 empirical correlation
    !                      1 = theoretical formula
    !
    ! Output
    !
    ! NodeSinterTemp         - number of mesh fartherest from fuel center that has been sintered any time in past
    ! ThermalConductivity(l) - conductivity of l-th radial mesh (btu/ft-F-sec) output modified conductivity.
    !
    INTEGER(ipk) :: m, nmesh, nchang
    INTEGER(ipk), INTENT(IN) :: igpnod, modfd, ncladi, modkf !Note: modkf not used
    INTEGER(ipk), INTENT(INOUT) :: NodeSinterTemp
    REAL(r8k) :: alpha, gap0, c1, crel, tempf, tempk, pressi, congas, congsi, rcor
    REAL(r8k), INTENT(IN) :: vrelc, GasPress, bu, frden, tsntrk !Note: bu not used
    REAL(r8k), PARAMETER :: thcond = 6.2306448e3_r8k
    REAL(r8k), DIMENSION(ngases), INTENT(IN) :: GasFraction
    REAL(r8k), DIMENSION(:), INTENT(IN) :: FinalTemp, RadialBound
    REAL(r8k), DIMENSION(:), INTENT(INOUT) :: ThermalConductivity
    !
    IF (ndebug) WRITE(ounit,907) Time, vrelc, NodeSinterTemp
907 FORMAT(' in kmod, Time = ',e13.6,' vrelc = ',e13.6,' NodeSinterTemp = ',i3)
    !
80  CONTINUE
    nmesh = igpnod - 1
    !  4/29/03: no longer using KMOD
    IF (modfd == 0) RETURN
    GOTO 2000
    ! Frap-s3 correction for fuel cracking. Calculate void fraction generated by azimuthal cracking of fuel.
    ! Assume same void fraction thruout fuel pellet.
2000 CONTINUE
    WRITE(ounit,*)'STOP: error in KMOD - non-zero value of modfd'
    IF (modfd /= 0) STOP
    !
    alpha = vrelc
    IF (alpha < 0.0_r8k) alpha = 0.0_r8k
    gap0 = RadialBound(ncladi) - RadialBound(igpnod)
    c1 = 0.3_r8k
    ! Calculate (structural gap) - (thermal gap) = gapdIf
    ! Alpha = volstr of Frapcon2
    IF (alpha < 1.0e-10_r8k) THEN
        crel = 0.125_r8k
    ELSE
        crel = alpha / (4.0_r8k * gap0 / RadialBound(igpnod))
        crel = crel * ((gap0 / ftmetr) / 0.8e-4_r8k)
        IF (crel < 0.125_r8k) crel = 0.125_r8k
    ENDIF
    nmesh = igpnod - 1
    nchang = 0
    !
    DO m = 1, nmesh
        tempf = 0.5_r8k * (FinalTemp(m) + FinalTemp(m+1))
        tempk = tfk(tempf)
        pressi = GasPress / psinm2
        ! Gas thermal conductivity
        congas = MatProperty (Material='GAS', Property='THERMCOND', Temperature=tempk, &
          &                   Pressure=pressi, GasComposition=GasFraction) / thcond
        !
        congsi = congas * thcond
        IF (ndebug) WRITE(ounit,916) tempk, pressi, congsi
916     FORMAT(' in kmod, tempk = ',e11.4,' pressi = ',e11.4,' congsi = ',e11.4)
        IF (m <= NodeSinterTemp) THEN
            rcor = 1.0_r8k
        ELSE
            IF (tempk >= tsntrk) nchang = nchang + 1
            ! Permit maximum of one new mesh to be sintered per call to kmod
            IF (tempk > tsntrk .AND. nchang == 1) NodeSinterTemp = NodeSinterTemp + 1
            IF (tempk >= tsntrk .AND. nchang == 1) THEN
                rcor = 1.0_r8k
            ELSE
                rcor = 1.0_r8k - c1 * crel * (1.0_r8k - congas / ThermalConductivity(m))
                IF (rcor < 0.4_r8k) rcor = 0.4_r8k
            ENDIF
        ENDIF
        ThermalConductivity(m) = rcor * ThermalConductivity(m)
        IF (ndebug) WRITE(ounit,952) m, tempk, crel, alpha, rcor
952     FORMAT(' for mesh #',i3,' tempk = ',e11.4,' crel = ',e11.4,' alpha = ',e11.4,' rcor = ',e11.4)
        !
    ENDDO
    !
    END SUBROUTINE kmod
    !
END MODULE HeatSolution_fraptran













