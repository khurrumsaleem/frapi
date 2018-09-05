MODULE CorrosionData
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : calcoxide, icm, iplant, ounit, sigh2, zr2vintage, EOSZrO2Thk, sigcor, &
      &                   FastFlux, dco, dci, dp, StartofStepPickupH2Con, EndofStepPickupH2Con, &
      &                   CladH2Concen, ExcessH2Concen, ZrO2ThkNoAd, EOSNodeBurnup, &
      &                   qc, CladOutSurfTemp, j, iter, zro2wg, cladavgtemp, delhs, &
      &                   chorg, ppmh2o, zro2i, zro2o, zoxk, excesh2, SurfTempOxide
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> Module CorrosionData calculates the oxide layer thickness, hydrogen uptake and temperature rise across from the oxide layer
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/16/2014
    !
    CONTAINS
    !
    SUBROUTINE Corrosion
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from frpcon and calculates the corrosion layer thickness, 
    !> hydrogen concentration in the cladding and temperature rise across oxide layer.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/16/2014
    !
    REAL(r8k) :: tcok, zoxki, dcom, dcim, dpm, qcim, testr, rgascn, tssh2
    !
    tcok = tfk(SurfTempOxide(j-1))
    ! Compute the zircaloy-oxide thermal conductivity, W/m*K
    zoxki = MatProp ('OXIDE', 'THERMCOND', tcok)
    ! Convert to Btu/h*ft*F
    zoxk = zoxki * WmKtoBhftF
    ! Convert btu/hr*ft^2 to W/m^2
    qcim = qc(j-1) * Bhft2toWm2
    zro2i = ZrO2ThkNoAd(j-1) * ftom
    !
    IF (iter == 1) THEN
        !
        ! Compute the corrosion thickness
        !
        CALL corros (tcok, qcim, zoxki)
        ! Update cladding hydrogen content.
        dcom = dco(j-1) * intom
        dcim = dci(j-1) * intom
        dpm = dp(j-1) * intom
        zro2wg = EOSZrO2Thk(j-1) / 0.225e-5_r8k
        cladavgtemp = tfk(CladOutSurfTemp(j-1))
        zoxki = MatProp ('OXIDE', 'THERMCOND', cladavgtemp)
        !
        ! Calculate hydrogen uptake
        !
        CALL chuptk (dpm, dcom, dcim, cladavgtemp, qcim, zoxki)
        !
        ! Calculate concentration above solubility limit.
        ! Kearns' correlation is used to calculate "Terminal Solid Solubility", 
        ! referenced in Journal of Nuclear Materials, 22 (1967), pages 292-303.
        ! rgascn = gas constant in units of gram.calories/gram.mole.K
        rgascn = 1.985887_r8k
        tssh2 = (1.2e5_r8k) * EXP(-8550.0_r8k / (rgascn * cladavgtemp))
        ExcessH2Concen(j-1) = MAX(CladH2Concen(j-1) - tssh2, 0.0_r8k)
        testr = 550.0_r8k
        excesh2 = MIN(ExcessH2Concen(j-1), testr)
    END IF
    !
    END SUBROUTINE Corrosion
    !
    !
    !
    SUBROUTINE corros (tcoi, qci, zoxki)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Subroutine corros calculates the ZrO2 water-side corrosion oxide thickness
    !
    ! Input
    !
    ! iplant  = flag for reactor type
    !      -2 - PWR
    !      -3 - BWR
    !      -4 - HBWR
    ! tcoi    = cladding oxide-water interface temperature, K
    ! qci     = cladding surface heat flux, W/m^2
    ! zro2i   = previous oxide thickness, meters
    ! zoxki   = cladding thermal conductivity, W/m-K
    ! delhs   = time step size in seconds
    ! phi     = current fast neutron flux, n/m^2-sec
    ! icm     = flag for cladding type
    !       2 - Zircaloy-2
    !       4 - Zircaloy-4
    !       5 - M5
    !       6 - ZIRLO(TM)
    !       7 - Optimized ZIRLO(TM)
    !
    ! Output
    !
    ! zro2o   - oxide thickness, meters
    !
    ! Internal
    !
    ! SpallLimit - Maximum cladding thickness before spallation is assumed to occur (150 microns)
    ! tcok    - cladding-oxide interface temperature, K
    !
    REAL(r8k) :: tcok, zotc, phic, deld, r, gama, ra, rk, rq, rq1, oxlim, wt, qc, rp, rm, u, rkt, &
      &          rold, rnewthk, term, term1, term2, term3, diff, adrwt, addthk, rc
    REAL(r8k), INTENT(IN) :: tcoi, qci, zoxki
    REAL(r8k), PARAMETER :: SpallLimit = 1.5e-4_r8k
    LOGICAL :: diffstop
    !
    diffstop = .FALSE.
    tcok = tcoi + qci * zro2i / zoxki
    ! Convert to conductivity to W/cm-K, heat flux to W/cm^2, neutron
    ! flux to n/cm^2-s, and time to days to match derived equation in EPRI-NP-1472
    zotc = zoxki / 100.0_r8k
    SELECT CASE (icm)
    CASE (6)
        qc = MIN(qci / 1.0e4_r8k, 41.0_r8k)
    CASE (7)
        qc = MIN(qci / 1.0e4_r8k, 41.0_r8k)
    CASE DEFAULT
        qc = qci / 1.0e4_r8k
    END SELECT
    phic = FastFlux(j-1) / 1.0e4_r8k
    deld = delhs * sectoday
    ! r = gas constant, cal/mole-K; gama converts weight gain in g/cm^2 to cm oxide thickness.
    r = 1.987_r8k
    gama = 0.6789_r8k
    ! Set values based on plant type
    SELECT CASE (iplant)
    CASE (-2) ! coefficients for PWR; note rk and u are converted from microns per day to g/cm^2 per day.
        ra = 6.3e9_r8k
        rk = 11863.0_r8k
        SELECT CASE (icm)
        CASE (5)
            rq = 27354.0_r8k * 1.09_r8k
            rq1 = 32289.0_r8k * 0.85_r8k
            oxlim = 7.0e-6_r8k
            wt = zro2i * 100.0_r8k / gama
        CASE (6)
            rq = 27354.0_r8k * 0.99_r8k
            rq1 = 32289.0_r8k
            oxlim = 2.0e-6_r8k
            wt = zro2i * 2.0_r8k * 100.0_r8k / gama
        CASE (7)
            rq = 27354.0_r8k * 1.0_r8k
            rq1 = 32289.0_r8k
            oxlim = 2.0e-6_r8k
            wt = zro2i * 2.0_r8k * 100.0_r8k / gama
        CASE DEFAULT
            rq = 27354.0_r8k
            rq1 = 32289.0_r8k
            oxlim = 2.0e-6_r8k
            wt = zro2i * 100.0_r8k / gama
        END SELECT
        rp = 0.24_r8k
        rm = 1.91e-15_r8k
        u = 3.5e4_r8k
        rkt = rk + u * ((rm * phic) ** rp)
        ! Pre transition oxidation
        rold = zro2i * 1.0e6_r8k
        rnewthk = (3.0_r8k * ra * EXP(-rq1 / (tcok * r)) * deld + (rold ** 3)) ** 0.33333_r8k
        rnewthk = rnewthk * 1.0e-6_r8k
        zro2o = rnewthk
        IF (zro2i >= oxlim) THEN
            ! add post transition oxidation  (EPRI-NP-5100 Model)
            term = r * (tcoi ** 2) * zotc / (gama * rq * qc)
            term1 = 1.0_r8k / term
            term2 = term1 * rkt * EXP(-rq / (r * tcoi))
            term3 = EXP(term1 * wt)
            diff = 1.0_r8k - term2 * term3 * deld
            ! Protect against 0 or negative values of 'diff'
            IF (diff <= 0.0_r8k) THEN
                diffstop = .TRUE.
            ELSE
                ! adrwt is post transition wt. gain in grams per square centemeter
                adrwt = term * LOG(1.0_r8k / diff)
                ! addthk is added thickness, in meters
                addthk = adrwt * gama / 100.0_r8k
                ! for ZIRLO(TM), reduce oxidation by a factor of 2
                IF (icm == 6 .OR. icm == 7) addthk = addthk / 2.0_r8k
                zro2o = zro2i + addthk
            END IF
        END IF
    CASE DEFAULT !BWR (iplant = -2 or iplant = -3)
        rk = 11800.0_r8k
        rq = 27350.0_r8k
        rc = 2.5e-12_r8k
        !
        wt = zro2i * 100.0_r8k / gama
        term = r * (tcoi ** 2) * zotc / (gama * rq * qc)
        term1 = 1.0_r8k / term
        term2 = term1 * rk * EXP(-rq / (r * tcoi))
        term3 = EXP(term1 * wt)
        diff = 1.0_r8k - term2 * term3 * deld
        ! Protect against 0 or negative values of 'diff'
        IF (diff <= 0.0_r8k) THEN
            diffstop = .TRUE.
        ELSE
            ! adrwt is post transition wt. gain in grams per square centemeter
            adrwt = term * LOG(1.0_r8k / diff) + rc * rk * deld * qc
            ! addthk is added thickness, in meters
            addthk = adrwt * gama / 100.0_r8k
            zro2o = zro2i + addthk
        END IF
    END SELECT
    ! Check for spallation.
    ! The oxide thickness is assumed to spall away once it is greater than 150 microns.
    IF (zro2o >= SpallLimit) THEN
        WRITE (ounit,75) zro2o
75      FORMAT(2x,'***** the oxide thickness has reached or exceeded 150 microns and is assumed to spall',2x,e11.4)
        zro2o = 1.5e-4_r8k
    END IF
    ! If the oxide thickness is too large, stop code execution.
    IF (diffstop) THEN
        WRITE (ounit,100)
100     FORMAT(2x, 'Oxide thickness or time step size or both are too great. Review input and output.')
        STOP
    END IF
    !
    ! If oxide calculation is turned off, set final oxide thickness equal to initial oxide thickness.
    IF (.NOT. calcoxide) zro2o = zro2i
    !
    EOSZrO2Thk(j-1) = zro2o * mtoft
    ZrO2ThkNoAd(j-1) = EOSZrO2Thk(j-1)
    ! Add uncertianties
    SELECT CASE (icm)
    CASE (2)
        EOSZrO2Thk(j-1) = MAX(0.0_r8k, EOSZrO2Thk(j-1) + sigcor * 2.49e-5_r8k)
    CASE (4)
        EOSZrO2Thk(j-1) = MAX(0.0_r8k, EOSZrO2Thk(j-1) + sigcor * 5.02e-5_r8k)
    CASE (5)
        EOSZrO2Thk(j-1) = MAX(0.0_r8k, EOSZrO2Thk(j-1) + sigcor * 1.64e-5_r8k)
    CASE (6)
        EOSZrO2Thk(j-1) = MAX(0.0_r8k, EOSZrO2Thk(j-1) + sigcor * 4.92e-5_r8k)
    CASE (7)
        EOSZrO2Thk(j-1) = MAX(0.0_r8k, EOSZrO2Thk(j-1) + sigcor * 4.92e-5_r8k)
    END SELECT
    !
    END SUBROUTINE corros
    !
    !
    !
    SUBROUTINE chuptk (dp, dco, dci, tcoi, qci, zoxki)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> chuptk calculates average concentration of hydrogen in zircaloy cladding.
    !>@author
    !> chuptk coded by d. l. hagrman february 1977
    !> Modified by d. l. hagrman june 1978
    !
    ! Input
    !
    ! chorg  - initial hydrogen in cladding (ppm)
    ! ppmh2o - initial fuel water content (ppm)
    ! dp     - fuel pellet diameter (m)
    ! icm    - cladding material flag 
    ! dco    - cladding outside diameter (m)
    ! dci    - cladding inside diameter (m)
    ! iplant - reator type -2 for PWR, -3 or -4 for BWR
    ! zro2i  - oxide thickness at start of current time step (m)
    ! zro2o  - oxide thickness at End of current time step (m)
    ! tcoi   - zro2-coolant interface temperature (k)
    ! qci    - axial increment heat flux (watt/meter**2)
    ! zoxki  - zircaloy oxide thermal conductivity (w/(m-k))
    ! StartofStepPickupH2Con - Hydrogen from coolant in cladding at start of time step (ppm)
    !
    ! Output
    !
    ! CladH2Concen         - Average concentration of hydrogen in cladding at end of time step (ppm)
    ! EndofStepPickupH2Con - Hydrogen from coolant in cladding at end of time step (ppm)
    !
    ! Reference:
    !
    ! (1) [Insert D. D. Lanning's references here]
    !
    ! Note:
    !
    ! This model should not be used outside the temperature range 523.15 - 673.15 k (250 - 400 c).**
    !
    REAL(r8k), INTENT(IN) :: dp, dco, dci, tcoi, qci, zoxki
    REAL(r8k) :: b, c, d, chupc, tcoic, wtran, burnup, huptak, chydai, chydbi
    !
    burnup = EOSNodeburnup(j-1) / 1000.0_r8k
    chydai = StartofStepPickupH2Con(j-1)
    !
    IF (tcoi <= 366.5_r8k) THEN
        chupc = 0.0_r8k
    ELSE
        IF (zro2i <= 0.0_r8k) zro2i = 1.0e-10_r8k
        tcoic = tcoi + qci * zro2i / zoxki
        wtran = 7.7490e-06_r8k * EXP(-7.90e2_r8k / tcoic)
        IF (iplant <- 2) THEN ! bwr environment
            b = 0.12_r8k
            IF (icm <= 2) b = 0.29_r8k
        ELSE ! pwr environment
            SELECT CASE (icm)
            CASE DEFAULT
                b = 0.15_r8k
            CASE (2)
                b = 0.48_r8k
            CASE (5) ! for M5, pickup fraction is 0.01
                b = 0.1_r8k
            CASE (6) ! for ZIRLO(TM), pickup fraction is 0.175
                b = 0.175_r8k
            CASE (7) ! for Optimized ZIRLO(TM), pickup fraction is 0.175
                b = 0.175_r8k
            END SELECT
        END IF
        c = 1.0_r8k
        ! b is the pickup fraction in pre-transition and post-transition
        ! c is always 1
        ! find hydrogen uptake from coolant during current time step
        d = (9.0E5_r8k) * dco / ((dco ** 2) - (dci ** 2))
        IF (zro2i > wtran) THEN
            ! all oxidation post - transition
            chupc = d * (c * b / 8.0_r8k) * (zro2o - zro2i)
        ELSE IF (zro2o >= wtran) THEN
            ! part of oxidation pretransition
            chupc = d * ((b / 8.0_r8k) * (wtran - zro2i) + (c * b / 8.0_r8k) * (zro2o - wtran))
        ELSE
            ! all oxidation pretransition
            chupc = d * (b / 8.0_r8k) * (zro2o - zro2i)
        END IF
    END IF
    ! sum hydrogen from coolant
    chydbi = chydai + chupc
    IF (icm == 2) THEN
        SELECT CASE (zr2vintage)
        CASE (0)
            IF (burnup <= 50.0_r8k) THEN
                chydbi = 47.8_r8k * EXP(-1.3_r8k / (1.0_r8k + burnup)) + 0.316_r8k * burnup
            ELSE
                chydbi = 22.8_r8k + EXP(0.117_r8k * (burnup - 20.0_r8k)) + 6.1_r8k
            END IF
        CASE (1)
            chydbi = 22.8_r8k + EXP(0.117_r8k * (burnup - 20.0_r8k))
        END SELECT
    END IF
    ! Check to see if oxide calculation has been stopped
    IF (.NOT. calcoxide) chydbi = chydai
    ! sum hydrogen from alloy, fuel moisture and coolant
    huptak = chorg + 0.186_r8k * ppmh2o * ((dp ** 2) / (dco ** 2 - dci ** 2)) + chydbi
    ! add uncertianty
    SELECT CASE (icm)
    CASE (2)
        IF (burnup <= 50.0_r8k) THEN
            IF (zr2vintage == 0) huptak = MAX(0.0_r8k, huptak + sigh2 * 10.0_r8k)
            IF (zr2vintage == 1) huptak = MAX(0.0_r8k, huptak + sigh2 * 13.0_r8k)
        ELSE
            IF (zr2vintage == 0) huptak = MAX(0.0_r8k, huptak + sigh2 * 54.0_r8k)
            IF (zr2vintage == 1) huptak = MAX(0.0_r8k, huptak + sigh2 * 60.0_r8k)
        END IF
    CASE (4)
        huptak = MAX(0.0_r8k, huptak + sigh2 * 94.0_r8k)
    CASE (5)
        huptak = MAX(0.0_r8k, huptak + sigh2 * 23.0_r8k)
    CASE (6)
        huptak = MAX(0.0_r8k, huptak + sigh2 * 110.0_r8k)
    CASE (7)
        huptak = MAX(0.0_r8k, huptak + sigh2 * 110.0_r8k)
    END SELECT
    !
    CladH2Concen(j-1) = huptak
    EndofStepPickupH2Con(j-1) = chydbi
    !
    END SUBROUTINE chuptk
    !
END MODULE CorrosionData

