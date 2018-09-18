MODULE HeatFluxCorrelations
    USE Kinds
    USE sth2x, ONLY : surten
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE pchf (MassFlowRate, hfp, hgp, hin, CoolPress, Quality, cl, dr, dh, hd, chf, icwf, &
      &             HtFlxFac, SurfHtFlux, AxNodElevat, k, acond, j1, tsat, rf, rg, alpha, cpg, &
      &             twall, cpf, tbulk, naxn, Nchan, r, cp)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    USE conversions_fraptran, ONLY : sechr
    IMPLICIT NONE
    !
    ! Input
    !
    ! MassFlowRate - mass velocity, lb/ft**2/hr
    ! CoolPress    - pressure, lb/in**2
    ! Quality      - quality
    ! hfp          - enthalpy, saturated liquid, btu/lb
    ! hgp          - enthalpy, saturated gas, btu/lb
    ! hin          - enthalpy, inlet, btu/lb
    ! cl           - local core channel length, in
    ! dr           - fuel rod diameter, in
    ! dh           - heated equivalent diameter, in
    ! hd           - hydraulic equivalent diameter, in
    ! icwf         - flag for aux. factors in hi pressure range
    !            0 = calculate HtFlxFac and cold wall effect
    !            1 = calculate only HtFlxFac
    !            2 = calculate only cold wall influence
    !            3 = use neither HtFlxFac nor cold wall factor
    ! HtFlxFac     - non-unIform axial heat flux factor (calculated only in steady state, reset to 1.0 at flow reversal)
    ! SurfHtFlux   - surface heat fluxes at each axial node
    !
    ! Output
    !
    ! chf         - critical heat flux, btu/ft**2/hr
    ! tchf        - critical heat flux temperature (F)
    ! HtFlxFac    - non-uniform axial heat flux factor (calculated only in steady state, reset to 1.0 at flow reversal)
    ! AxNodElevat - elevations of each axial node (ft)
    ! k           - axial node number
    ! acond       - array for functions of coolant channel geometry
    !               acond(1,Nchan) must be zero initially, not used if j = 1 or 2
    ! j1 = j = correlation indicator.            (input)
    ! If changed during a run, acond(1,Nchan) must also be reset to zero.
    !     j = 0
    ! these correlations are based on empirical Data derived from rod
    ! bundles at steady state conditions.
    ! jmc
    ! jmc -- new chf correlations --
    ! jmc
    ! jmc
    ! jmc  flag    old correlation         new correlation
    ! jmc  j=0    B&W #2 correlation        epri-1 correlation (default)
    ! jmc  j=1    general electric corr.    Bowring's mixed cluster corr.
    ! jmc  j=2    savannah river corr.      MacBeth's correlation
    ! jmc  j=3    comb. hughes and w-3(tong)  Biasi correlation
    ! jmc  j=4    ce-1 correlation        modified Zuber correlation
    ! jmc  j=5    loft correlations         set j=0, epri-1 (default)
    ! jmc  j=6    retran4,mod7 corr.        set j=0, epri-1 (default)
    ! jmc
    !
    ! tsat   - saturation temperature (F)
    ! rf     - saturated liquid density (lb/ft**3)
    ! rg     - saturated vapor density  (lb/ft**3)
    ! alpha  - void fraction
    ! cpg    - specific heat of saturated vapor  (btu/lb-F)
    ! twall  - cladding surface temperature from last iteration  (F)
    ! cpf    - heat capacity of saturated liquid water  (btu/lb-F)
    ! tbulk  - coolant temperature (F)
    !
    INTEGER(ipk) :: jj, imc, ll, l, iextrp, ii, iabv, iblo, jend, j1
    INTEGER(ipk), INTENT(IN) :: k, naxn, nchan, icwf
    REAL(r8k) :: chf, hfg, hgp, hfp, g, hij, xx, xxin, hin, dhft, dh, hdft, hd, achn, cw, pratio, &
      &          aterm, cterm, fluxn, faxial, fgrd, fwall, fabwr, fcbwr, cgrid, sumq, xstart, fluxnj, &
      &          xl, yterm, atermp, ctermp, de, sci, pr, fpx, dfp, qbar, f1, f2, a1, a2, b1, c, &
      &          fvane, chflv, factr, aprime, cprime, chfhv, chfa, chfb, slope, tsat, sigma, gravcn, &
      &          gravac, qlowg, alpha, rg, rf, tbulk, fsubc, cpf, gbiasi, pbiasi, dhbsi, biasin, fpterm, &
      &          fhterm, qb1, qb2, cl, dr, htflxfac, cpg, twall, r, cp, MassFlowRate, Quality, &
      &          CoolPress
    REAL(r8k), DIMENSION(:) :: SurfHtFlux, AxNodElevat
    REAL(r8k), DIMENSION(:,:) :: acond
    
    REAL(r8k) :: tsubmn = 35.0_r8k, fvmin = 2.0_r8k
    REAL(r8k) :: sra = 188000.0_r8k, srb = 0.0515_r8k, src = 0.069_r8k
    ! pressure range boundaries
    REAL(r8k) :: pr12 = 725.0_r8k
    REAL(r8k) :: pr21 = 1000.0_r8k
    REAL(r8k) :: pr23 = 1300.0_r8k
    REAL(r8k) :: pr32 = 15.0e2_r8k
    REAL(r8k) :: epsd = 0.1e-4_r8k
    REAL(r8k) :: dp8 = 0.8_r8k
    REAL(r8k) :: dp4 = 0.4_r8k
    REAL(r8k) :: dp023 = 0.023_r8k
    
    ! CHF Limit. This value needs to be set! IP 5/13/16
    REAL(r8k), PARAMETER :: chflim = 0.0_r8k
    !
    ! constants for chf correlations
    !
    ! Coefficients for epri-1 correlation
    !
    REAL(r8k), PARAMETER :: p1 = 0.5328_r8k
    REAL(r8k), PARAMETER :: p2 = 0.1212_r8k
    REAL(r8k), PARAMETER :: p3 = 1.6151_r8k
    REAL(r8k), PARAMETER :: p4 = 1.4066_r8k
    REAL(r8k), PARAMETER :: p5 = -0.3040_r8k
    REAL(r8k), PARAMETER :: p6 = 0.4843_r8k
    REAL(r8k), PARAMETER :: p7 = -0.3285_r8k
    REAL(r8k), PARAMETER :: p8 = -2.0749_r8k
    !
    ! Coefficents for Bowring's correlation
    !
    REAL(r8k), PARAMETER :: f1a = 1.0_r8k
    REAL(r8k), PARAMETER :: f1b = -0.04_r8k
    REAL(r8k), PARAMETER :: f1c = 1.0_r8k
    REAL(r8k), PARAMETER :: f1d = 0.47_r8k
    REAL(r8k), PARAMETER :: f1e = 2.0_r8k
    REAL(r8k), PARAMETER :: f1f = 0.5_r8k
    REAL(r8k), PARAMETER :: f1g = 2.0_r8k
    REAL(r8k), PARAMETER :: f2a1 = 3.2_r8k
    REAL(r8k), PARAMETER :: f2b1 = 0.32_r8k
    REAL(r8k), PARAMETER :: f2c1 = 0.135_r8k
    REAL(r8k), PARAMETER :: f2a2 = 0.424_r8k
    REAL(r8k), PARAMETER :: f2b2 = 1.959_r8k
    REAL(r8k), PARAMETER :: f2c2 = -1.556_r8k
    REAL(r8k), PARAMETER :: f2d2 = 2.0_r8k
    REAL(r8k), PARAMETER :: f2a3 = 0.45_r8k
    REAL(r8k), PARAMETER :: f2b3 = 1.25_r8k
    REAL(r8k), PARAMETER :: f1a4 = 0.8726_r8k
    REAL(r8k), PARAMETER :: f2a4 = 0.953_r8k
    REAL(r8k), PARAMETER :: a1a = 242.4_r8k
    REAL(r8k), PARAMETER :: a1b = 1.0_r8k
    REAL(r8k), PARAMETER :: a1c = 1.52_r8k
    REAL(r8k), PARAMETER :: a1d = 2.0_r8k
    REAL(r8k), PARAMETER :: a1e = 1.3_r8k
    REAL(r8k), PARAMETER :: a1f = 1.0_r8k
    REAL(r8k), PARAMETER :: a1g = 0.8_r8k
    REAL(r8k), PARAMETER :: a1h = 1.0_r8k
    REAL(r8k), PARAMETER :: a1i = 2.250_r8k
    REAL(r8k), PARAMETER :: a2a = 18.0_r8k
    REAL(r8k), PARAMETER :: a2b = 9.5_r8k
    REAL(r8k), PARAMETER :: a2c = 0.1_r8k
    REAL(r8k), PARAMETER :: brb1a = 0.25_r8k
    REAL(r8k), PARAMETER :: brb1b = -0.2_r8k
    REAL(r8k), PARAMETER :: c1a = 60.0_r8k
    REAL(r8k), PARAMETER :: c1b = 0.57_r8k
    REAL(r8k), PARAMETER :: c1c = 0.27_r8k
    REAL(r8k), PARAMETER :: c1d = 1.0_r8k
    REAL(r8k), PARAMETER :: c1e = -1.0_r8k
    REAL(r8k), PARAMETER :: c1f = 1.0_r8k
    !
    ! Coefficients for MacBeth's correlation
    !
    REAL(r8k), DIMENSION(6), PARAMETER :: ay0  = [   106.5_r8k,   123.5_r8k,  124.5_r8k,   59.9_r8k,   67.5_r8k,    1.3_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ay1 =  [   0.847_r8k,   0.834_r8k,  0.913_r8k,  0.873_r8k,   1.13_r8k,  -0.05_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ay2 =  [   0.677_r8k,   0.408_r8k,  0.376_r8k,  0.120_r8k,  0.535_r8k,   1.02_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ay3 =  [    60.3_r8k,    78.8_r8k,  118.0_r8k,   82.7_r8k,  108.0_r8k,  103.0_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ay4 =  [     1.4_r8k,     1.4_r8k,    1.4_r8k,    1.4_r8k,    1.4_r8k,    1.4_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ay5 =  [   0.937_r8k,   0.737_r8k,  0.555_r8k,  0.096_r8k,  0.343_r8k,  0.529_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: ayp =  [   250.0_r8k,   530.0_r8k, 1000.0_r8k, 1570.0_r8k, 2000.0_r8k, 2700.0_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by0 =  [   237.0_r8k,   114.0_r8k,   36.0_r8k,   65.5_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by1 =  [     1.2_r8k,   0.811_r8k,  0.509_r8k,   1.19_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by2 =  [   0.425_r8k,   0.221_r8k, -0.109_r8k,  0.376_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by3 =  [  -0.940_r8k,  -0.128_r8k, -0.190_r8k, -0.577_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by4 =  [ -0.0324_r8k,  0.0274_r8k,  0.024_r8k,  0.220_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by5 =  [   0.111_r8k, -0.0667_r8k,  0.463_r8k, -0.373_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by6 =  [    19.3_r8k,   127.0_r8k,   41.7_r8k,   17.1_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by7 =  [   0.959_r8k,    1.32_r8k,  0.953_r8k,   1.18_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by8 =  [   0.831_r8k,   0.411_r8k, 0.0191_r8k, -0.456_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by9 =  [    2.61_r8k,  -0.274_r8k,  0.231_r8k,  -1.53_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by10 = [ -0.0578_r8k, -0.0397_r8k, 0.0767_r8k,   2.75_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: by11 = [   0.124_r8k, -0.0221_r8k,  0.117_r8k,   2.24_r8k ]
    REAL(r8k), DIMENSION(4), PARAMETER :: byp =  [   560.0_r8k,  1000.0_r8k, 1550.0_r8k, 2000.0_r8k ]
    REAL(r8k), PARAMETER :: vlow1 = 0.00633_r8k
    REAL(r8k), PARAMETER :: vlow2 = -0.1_r8k
    REAL(r8k), PARAMETER :: vlow3 = 0.51_r8k
    !
    ! Coefficients for Biasi's correlation
    !
    REAL(r8k), PARAMETER :: b1a = 5.9695e6_r8k
    REAL(r8k), PARAMETER :: b1b = -0.166667_r8k
    REAL(r8k), PARAMETER :: b2a = 11.98e6_r8k
    REAL(r8k), PARAMETER :: b2b = -0.60_r8k
    REAL(r8k), PARAMETER :: fpa = 0.7249_r8k
    REAL(r8k), PARAMETER :: fpb = 0.099_r8k
    REAL(r8k), PARAMETER :: fpc = -0.032_r8k
    REAL(r8k), PARAMETER :: hpa = -1.159_r8k
    REAL(r8k), PARAMETER :: hpb = 0.149_r8k
    REAL(r8k), PARAMETER :: hpc = -0.019_r8k
    REAL(r8k), PARAMETER :: hpd = 8.99_r8k
    REAL(r8k), PARAMETER :: hpe = 10.0_r8k
    REAL(r8k), DIMENSION(6) :: y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, ypr
    !
    cgrid = 0.0_r8k
    chf = 0.0_r8k
    acond(1,Nchan) = 0.0_r8k
    hfg = hgp - hfp
    imc = 0
    !
    !  chf meaningless above critical point
    !
    IF (hfg <= 0.0_r8k) THEN
        chf = 1.0e20_r8k
        GOTO 900
    ENDIF
    !
    IF (j1 /= 0) GOTO 200
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    !  epri-1 chf correlation
    !  base correlation can be modified at the user's option, with
    !  three correction factors;
    !    - a 2-part cold wall correction for corner-
    !    peaked bundles modeled with subchannels
    !    - a grid spacer correction factor for
    !    bundles with loss coefficients that are
    !    not close to one,
    !    - a nonunIform axial power correction
    !    factor (note: this is same as bowring's,
    !    except it uses local flux rather than
    !    radially averaged values in the subchannel).
    !
    ! nominal range:
    ! 200 <= P <= 2450 psia
    ! 0.2 <= G <= 4.5 Mlbm/hr-ft2
    ! -0.25 < x < 0.75
    ! 
    !   Note: Pressure depEndence of this correlation is such that
    !       it can be extrapolated to pressures above 2450 psia 
    !       and still produce reasonable predictions of chf.
    !       Similarly, quality depEndence can be extrapolated to
    !       subcooled conditions below -0.25, and still give
    !       reasonable predictions of chf.
    !
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    ! check range on pressure
    !   If P < 200 psia, use Biasi chf correlation
    !
    IF (CoolPress < 200.0_r8k) GOTO 600
    !
    g = MassFlowRate / 1.0E6
    !
    ! check range on mass flux
    !   If g > 4.5 Mlbm/hr-ft2, use Biasi chf correlation
    !   If g < 0.2 Mlbm/hr-ft2, use Zuber with GrIffith modIfication
    !
    IF (g > 4.5_r8k) GOTO 600
    IF (g < 0.2_r8k) GOTO 510
    !
    hij = acond(13,Nchan)
    xx = (hij - hfp) / hfg
    xxin = (hin - hfp) / hfg
    !
    ! check range on quality
    !   If xx > 0.75, use Biasi chf correlation
    !
    IF (xx > 0.75_r8k) GOTO 600
    !
    !  convert equiv. diameters from inches to feet
    !
    dhft = dh / 12.0_r8k
    hdft = hd / 12.0_r8k
    !
    achn = acond(9,Nchan)
    cw = 4.0_r8k * achn * ((1 / dhft) - (1 / hdft))
    pratio = CoolPress / 3208.2_r8k
    aterm = p1 * (pratio ** p2) * g ** (p5 + p7 * pratio)
    cterm = p3 * (pratio ** p4) * g ** (p6 + p8 * pratio)
    fluxn = SurfHtFlux(k)
    !
    ! jmc convert fluxn from Btu/hr-ft2 to MBtu/hr-ft2
    !
    fluxn = fluxn / 1.0E6_r8k
    faxial = 1.0_r8k
    fgrd = 1.0_r8k
    fwall = 0.0_r8k
    fabwr = 1.0_r8k
    fcbwr = 1.0_r8k
    
    ! Cold wall correction factor (applicable to subchannels only)
    IF (((icwf == 0) .OR. (icwf == 2)) .AND. (ABS(cw) > 1.0e-6_r8k)) THEN
        fabwr = g ** 0.1_r8k
        fcbwr = 1.183_r8k * fabwr
        fwall = fabwr
    END IF
    
    ! Grid loss coefficient correction factor
    IF (cgrid > 0.0_r8k) THEN
        fgrd = 1.3_r8k - 0.3_r8k * cgrid
        IF (fgrd <= 0.0_r8k) fgrd = 1.0_r8k
    ENDIF
    !
    IF (icwf == 0 .OR. icwf == 1) THEN
        !
        ! Nonuniform axial flux correction factor
        !
        ! Find beginning of heated length
        !
        sumq = 0.0_r8k
        xstart = 0.0_r8k
        DO jj = 2, k
            fluxnj = SurfHtFlux(jj)
            IF (fluxnj > 0.0_r8k) THEN
                sumq = sumq + fluxnj * (AxNodElevat(jj) - AxNodElevat(jj - 1))
            ELSE
                xstart = AxNodElevat(jj)
            ENDIF
        ENDDO
        !
        xl = AxNodElevat(k) - xstart
        yterm = sumq / (fluxnj * xl)
        faxial = 1.0_r8k + (yterm - 1.0_r8k) / (1.0_r8k + g)
    END IF
    
    ! Combine the correction factors with the a and c terms
    atermp = aterm * fabwr
    ctermp = cterm * fcbwr * fgrd * faxial
    !
    chf = (atermp - xxin) / (ctermp + (xx - xxin) / fluxn)
    !
    !  convert from MBtu / hr - ft2 to Btu / hr - ft2
    !
    chf = chf * 1.0E6_r8k
    !
    GOTO 900
    !
    ! jmc  End of section for epri - 1 correlation
    !
    !* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   * 
    !
200 CONTINUE
    !
    IF (j1 /= 1) GOTO 300
    !
    !* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
    !
    ! Bowring's mixed - cluster dryout correlation 
    !    for channels modeling whole bundles or sections of symmetry
    !
    ! nominal range:
    ! 90 <= P <= 2250 psia
    ! 0.04 <= G <= 3.0 Mlbm / hr - ft2
    ! 
    !   Note: Pressure dependence of this correlation is such that
    !       it can be extrapolated to pressures above 2450 psia 
    !       and still produce reasonable predictions of chf.
    !
    !* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
    !
    ! check range on pressure
    !    If p < 90 psia, use Biasi chf correlation
    !
    IF (CoolPress < 90.0_r8k) GOTO 600
    !
    g = MassFlowRate / 1.0E6_r8k
    !
    ! check range on mass flux
    !    If g > 3 Mlbm / hr - ft2, use Biasi correlation
    !    If g < 0.04 Mlbm / hr - ft2, use Zuber with GrIffith modIfication
    !
    IF (g > 3.0_r8k) GOTO 600
    IF (g < 0.04_r8k) GOTO 510    
    !
    de = hd
    g = MassFlowRate / 1.0E6_r8k
    hij = acond(13,Nchan)
    xx = (hij - hfp) / hfg
    IF (xx < 0.0_r8k) xx = 0.0_r8k
    IF (xx > 1.0_r8k) xx = 1.0_r8k
    sci = hfp - hin
    pr = CoolPress * 0.001_r8k
    IF (fpx <= 0.0_r8k) fpx = 1.0_r8k
    dfp = fpx * dh
    fgrd = fvane
    faxial = 1.0_r8k
    !
    sumq = 0.0_r8k
    DO jj = 2, k
        qbar = SurfHtFlux(jj) / 1.0E6_r8k
        sumq = sumq + qbar * (AxNodElevat(jj) - AxNodElevat(jj-1)) * 12.0_r8k
    ENDDO
    
    ! Axial flux factor
    faxial = sumq / (qbar * AxNodElevat(k) * 12.0_r8k)
    !
    ! jmc  -  -  keep original temporarily, for double - check of Data statements
    ! jmc    f1=(1.0_r8k - 0.04_r8k * pr * (1.0_r8k + 0.47_r8k * pr ** 2) ** .5_r8k) ** 2
    ! jmc    f2=(3.2_r8k - pr) * (0.32_r8k + 0.135_r8k * pr)
    ! jmc    IF (pref <= 650.0_r8k) f2=0.424_r8k + 1.959_r8k * pr - 1.556_r8k * pr ** 2
    ! jmc    IF (pref <= 415.0_r8k) f2=0.45_r8k + 1.25_r8k * pr
    ! jmc    IF (pref <= 1250.0_r8k) GOTO 510
    ! jmc    f1=0.8726_r8k
    ! jmc    f2=0.953_r8k
    !
    f1 = (f1a + f1b * pr * (f1c + f1d * pr ** f1e) ** f1f) ** f1g
    f2 = (f2a1 - pr) * (f2b1 + f2c1 * pr)
    IF (CoolPress <= 650.0_r8k) f2 = f2a2 + f2b2 * pr + f2c2 * pr ** f2d2
    IF (CoolPress <= 415.0_r8k) f2 = f2a3 + f2b3 * pr
    IF (CoolPress > 1250.0_r8k) THEN
        f1 = f1a4
        f2 = f2a4
    END IF
    !
    ! jmc  -  -  keep original temporarily, for double - check of Data statements
    ! jmc    a1=242.4 * f1 * g * dh / (1. + 1.52 * (fpx * dh) ** 2 * g / (f2 * de ** 1.3
    ! jmc     1   * (1. + g * (.8 * fpx * dh / de - 1.))))
    ! jmc    a2=18. * g + 9.5 * g * dh / (0.1 + g)
    ! jmc    IF (pref > 1250.) a1=a2 + (2.250 - pr) * (a1 - a2)
    ! jmc    b1=0.25 * g * dh * EXP( - .2 * g)
    ! jmc    c=60. * de ** .57 * g ** .27 * (1. + (faxial - 1.) / (g + 1.))
    !
    a1 = a1a * f1 * g * dh / (a1b + a1c * ((fpx * dh) ** a1d) * g / (f2 * (de ** a1e) * (a1f + g * (a1g * fpx * dh / de - a1h))))
    a2 = a2a * g + a2b * g * dh / (a2c + g)
    IF (CoolPress > 1250.0_r8k) a1 = a2 + (a1i - pr) * (a1 - a2)
    b1 = brb1a * g * dh * EXP(brb1b * g)
    c = c1a * (de ** c1b) * (g ** c1c) * (c1d + (faxial + c1e) / (g + c1f))
    !
    chf = (a1 + b1 * sci) / (c + AxNodElevat(k) * 12.0_r8k * faxial)
    !
    !  convert from mbtu/hr-ft2 to btu/hr-ft2
    !
    chf = chf * 1.0E6_r8k
    !
    GOTO 900
    !
    ! jmc  End of section for Bowring's mixed cluster correlation
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
300 CONTINUE
    !
    IF (j1 /= 2) GOTO 500
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    ! MacBeth's dryout correlation
    !  (Note: this correlation has no correction factors for
    !   non-unIform axial flux, unheated walls, or grids.)
    !
    ! nominal range:
    ! 15 <= P <= 2700 psia
    ! 0.0073 <= G <= 13.7 Mlbm/hr-ft2
    !
    !   based on Data obtained for vertical upflow in round tubes;
    !   can be extrapolated to annuli and (cautiously) to rod arrays
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    ! check range on pressure
    !   If p < 15 psia or p > 2700 psia, use Biasi chf correlation
    !
    IF (CoolPress < 15.0_r8k .OR. CoolPress > 2700.0_r8k) GOTO 600
    !
    g = MassFlowRate / 1.0E6_r8k
    !
    ! check range on mass flux
    !   If g > 13.7 Mlbm/hr-ft2, use Biasi chf correlation
    !   If g < 0.0073 Mlbm/hr-ft2, use Zuber with GrIffith modIfication
    !
    IF (g > 13.7_r8k) GOTO 600
    IF (g < 0.0073_r8k) GOTO 510    
    !
    de = hd
    hij = acond(13,Nchan)
    xx = (hij - hfp) / hfg
    IF (xx > 1.0_r8k) xx = 1.0_r8k
    faxial = 1.0_r8k
    !
    IF (imc /= 0 .AND. imc /= 1) imc = 1
    ! Note: I modified the above statement from icm to imc (Original below). Need to verify. IP, 6/7/2014
    ! If (icm /= 0 .AND. icm /= 1) icm = 1
    !
    IF (imc == 0) THEN
        
        ! Coefficients for the 6-coefficient formulation
        ll = 6
        DO l = 1,ll
            y0(l) = ay0(l)
            y1(l) = ay1(l)
            y2(l) = ay2(l)
            y3(l) = 0.0_r8k
            y4(l) = 0.0_r8k
            y5(l) = 0.0_r8k
            y6(l) = ay3(l)
            y7(l) = ay4(l)
            y8(l) = ay5(l)
            y9(l) = 0.0_r8k
            y10(l) = 0.0_r8k
            y11(l) = 0.0_r8k
            ypr(l) = ayp(l)
        ENDDO
        
    ELSE
        
        ! Coefficients for the 12-coefficient formulation
        ll = 4
        DO l = 1,ll
            y0(l) = by0(l)
            y1(l) = by1(l)
            y2(l) = by2(l)
            y3(l) = by3(l)
            y4(l) = by4(l)
            y5(l) = by5(l)
            y6(l) = by6(l)
            y7(l) = by7(l)
            y8(l) = by8(l)
            y9(l) = by9(l)
            y10(l) = by10(l)
            y11(l) = by11(l)
            ypr(l) = byp(l)
        ENDDO
    END IF
    
    !
    !  low velocity regime
    !
    ! jmc    chflv = 0.00633_r8k * hfg * de ** (-0.1_r8k) * g ** 0.51_r8k * (1.0_r8k - xx)
    chflv = vlow1 * hfg *(de ** (vlow2)) * (g ** vlow3) * (1.0_r8k - xx)
    
    ! Determine the interpolation factor for this pressure
    iextrp = 0
    factr = 0.0_r8k
    IF (CoolPress < 250.0_r8k .AND. imc == 0) GOTO 375
    IF (imc == 0) GOTO 355
    IF (CoolPress < 560.0_r8k) GOTO 375
    IF (CoolPress > 2000.0_r8k) GOTO 400
355 CONTINUE
    
    DO ii = 1, ll
        iabv = ii
        l = iabv
        iblo = ii - 1
        IF (CoolPress > ypr(ii)) CYCLE
        factr = (CoolPress - ypr(ii-1)) / (ypr(ii) - ypr(ii-1))
        GOTO 450
    END DO
    
    !
    GOTO 450
    !
375 CONTINUE
    
    ! If pressure is below lower limit of 6-coefficient array, extrapolate the high velocity regime equation
    iabv = 2
    l = iabv
    iblo = 1
    iextrp = 1
    jEnd = 1
    GOTO 450
    
    ! If pressure is above upper limit of 12-coefficient array and imc=1, extrapolate the high velocity regime
400 CONTINUE
    iabv = 4
    !
    l = iabv
    iblo = 3
    jEnd = 4
    !
450 CONTINUE
    
    ! High velocity regime
    High_Velocity_Loop: DO
        aprime = y0(l) * de ** y1(l) * g ** y2(l) * (1.0_r8k + y3(l) * de + y4(l) * g + y5(l) * de * g)
        cprime = y6(l) * de ** y7(l) * g ** y8(l) * (1.0_r8k + y9(l) * de + y10(l) * g + y11(l) * de * g)
        !
        chfhv = (aprime-de/4.0_r8k*g*hfg*xx)/cprime
        IF (chfhv <= 0.0_r8k) chfhv=0.000001_r8k
        IF (l /= iabv) EXIT High_Velocity_Loop
        chfa = chfhv
        l = iblo
    END DO High_Velocity_Loop

    !
    chfb = chfhv
    IF (iextrp <= 0) THEN
        !
        chfhv = chfb + factr * (chfa - chfb)
    ELSE
        !
        slope = (chfa - chfb) / (ypr(iabv) - ypr(iblo))
        IF (jEnd == 4) chfhv = chfa + slope * (CoolPress - ypr(jEnd))
        IF (jEnd == 1) chfhv = chfa - slope * (ypr(iabv) - CoolPress)
        IF (chfhv <= 0.0_r8k) chfhv = 0.000001_r8k
    END IF
    !
    chf = MIN(chflv, chfhv)
    !
    !  convert chf from MBtu/hr-ft2 to Btu/hr-ft2
    chf = chf * 1.0e6_r8k
    !
    GOTO 900
    !
    ! jmc  End of section for MacBeth's correlation
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
500 CONTINUE
    !
    IF (j1 /= 3) GOTO 600
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    !  Zuber chf correlation, with GrIffith modIfication
    !
    ! nominal range:
    ! no restriction on pressure
    ! G <= 0.2 Mlbm/hr-ft2
    !
    !     Note: modified Zuber correlation is used to calculate chf 
    !         when G is below the lower limit of the applicable
    !         range of a given chf correlation, regardless of
    !         the chf correlation selected by input.  It is also
    !         used If void fraction is above 0.8, regardless of
    !         G and P ranges.
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    g = MassFlowRate / 1.0E6_r8k
    !
    ! check range on mass flux
    !   If g > 0.2 Mlbm/hr-ft2, use Biasi chf correlation
    !
    IF (g > 0.2_r8k) GOTO 600
    !
510 CONTINUE
    
    !
    CALL surten (tsat, sigma)
    !
    gravcn = 32.2_r8k
    gravac = 32.2_r8k
    qlowg = 0.9_r8k * (1.0_r8k - alpha) * 0.13090_r8k * (hgp - hfp) * SQRT(rg) * ((sigma * gravcn * gravac * (rf - rg)) ** 0.25_r8k)
    !
    !  convert from btu/sec-ft**2 to btu/hr-ft**2
    !
    qlowg = sechr * qlowg
    !
    !  If subcooled coolant, apply correction factor using equation
    !  from jaeri code nsr-77
    !
    IF (tbulk < (tsat - 1.0_r8k)) THEN
        fsubc = 1.0_r8k + 0.065_r8k * ((rf / rg) ** 0.8_r8k) * (cpf * (tsat - tbulk) / (hgp - hfp))
        qlowg = fsubc * qlowg
    END IF
    !
    chf = qlowg
    !
    GOTO 900
    !
    ! jmc  End of section for modified Zuber correlation
    !
600 CONTINUE
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    !  Biasi's chf correlation
    !
    ! nominal range:
    ! no restriction on pressure
    ! G > 0.2 Mlbm/hr-ft2
    ! alpha <= 0.8
    !
    !     Note: Biasi's correlation is used to calculate chf when
    !         P is above or below the user-selected correlation's
    !         range, and when G is above the upper limit of the
    !         the user-selected chf correlation's range, regardless
    !         of the chf correlation selected by input.
    !
    ! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    !
    g = MassFlowRate / 1.0E6
    !
    ! check range on mass flux: If g < 0.2 Mlbm/hr-ft2, use modified Zuber correlation
    ! check range on void fraction: alpha > 0.8 indicates annular film flow; must use modified Zuber to define chf
    !
    IF (g <= 0.2_r8k .OR. alpha > 0.8_r8k) GOTO 510
    
    ! Convert units on mass flux from lbm/hr-ft2 to g/cm2-sec
    gbiasi = MassFlowRate * 453.5924_r8k / (sechr * 929.0_r8k)
    
    ! Convert units on pressure from psia to bars
    pbiasi = CoolPress * 0.068046_r8k
    
    ! Convert units on hydraulic diameter from inches to centimeters
    dhbsi = hd * 2.54_r8k
    
    !
    IF (dhbsi >= 1.0_r8k) THEN
        biasin = 0.4_r8k
    ELSE
        biasin = 0.6_r8k
    END IF
    
    !
    fpterm = fpa + fpb * pbiasi * EXP(fpc * pbiasi)
    fhterm = hpa + hpb * pbiasi * EXP(hpc * pbiasi) + hpd * pbiasi / (hpe + pbiasi ** 2)
    !
    qb1 = b1a * (gbiasi ** b1b) * (fpterm * (gbiasi ** b1b) - Quality) * dhbsi ** (-biasin)
    qb2 = b2a * fhterm * (1.0_r8k - Quality) * (dhbsi ** (-biasin)) * (gbiasi ** b2b)
    chf = MAX(qb1, qb2)
    !
    ! jmc  End of section for Biasi correlation
    !
900 CONTINUE
    
    ! Set lower limit for CHF
    IF (chf < 0.0_r8k) chf = chflim
    !
    END SUBROUTINE pchf

END MODULE HeatFluxCorrelations












