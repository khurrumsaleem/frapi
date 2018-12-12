MODULE HeatTransferCoefficient_fraptran
    !>@brief
    !> This module contains the subroutines used to calculate the cladding to coolant _fraptran
    !> heat transfer coefficient and the cladding to pellet gap heat transfer coefficient.
    !> Subroutines include htrc, qdot, root1, and gaphtc
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    USE Kinds_fraptran
    USE ErrorMsg_fraptran, ONLY : fabend
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE htrc (dr, hcoef, hflux, qcrit, tsurf, iht, k, rl, nchfsw, tempcm, zroxid, tmeltc)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : sechr, ftin, tfk, tfr
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran, ONLY : ounit, Time, ndebug, fdial, timeincrement
    USE Dyna_h_fraptran
    USE CoolantProperties_fraptran, ONLY : Prop, aasth, elvrad, nelrad, vfrad1, vfrad2, vfrad3, trad1, trad2, trad3, tshrda, nsrad3
    USE resti_h_fraptran
    USE htcb_h_fraptran
    USE heatconduction_h_fraptran
    USE sth2x_fraptran, ONLY : sth2x2, sth2x3, VoidRatio, thcon
    USE scalr_h_fraptran, ONLY : fqcrit
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE HeatFluxCorrelations_fraptran
    IMPLICIT NONE
    !
    ! htflxa          = axially averaged rod heat flux past time step,btu/sec-ft2 (surface heat flux at past time step input)
    ! tshtc           = time in sec after which computed surface HTC at node kaxhtc multiplied by fhtc 
    !                   to account for cladding ballooning on neighboring fuel rod
    ! nradsh          = number of flow shroud temp. vs time input for radiation
    ! tshrda          = array of f.s. temp vs time pairs
    ! rshrd           = flow shroud inner diameter (ft)
    ! tshtc           = time in sec after which computed surface h.t.c. at node kaxhtc multiplied by fhtc to account for cladding
    !                   ballooning on neighboring rod. values set in main.
    ! nfhtc           = switch for this. 0=off , 1=on
    ! acond           = storage area with channel Data and coolant conditions for coolant channel being processed -
    ! acond(nn,Nchan) = chfv  - vector of storage for pchf routine. nn=1,8
    ! acond( 9,Nchan) = achn  - flow area,  ft**2
    ! acond(10,Nchan) = dhe   - heated equivalent diameter, ft
    ! acond(11,Nchan) = dhy   - channel hydraulic diameter, ft
    ! acond(12,Nchan) = g     - mass flow rate, lbm/(hr.ft**2)
    ! acond(13,Nchan) = h     - local coolant enthalpy,  btu/lbm
    ! acond(14,Nchan) = hin   - enthalpy at normal inlet, btu/lbm
    ! acond(15,Nchan) = hup   - enthalpy at outlet (upper plenum)
    ! acond(16,Nchan) = p     - coolant pressure,  psia
    ! acond(17,Nchan) = tbulk - local coolant temperature,  F
    ! acond(18,Nchan) = v     - specific volume,  ft**3/lbm
    ! acond(19,Nchan) = x     - coolant quality
    ! acond(20,Nchan) = cp    - heat capacity at constant pressure, btu/(lbm.F)
    ! acond(21,Nchan) = tsat  - saturation temperature,  F
    ! acond(22,Nchan) = hf    - saturation enthalpy (liquid/fluid)
    ! acond(23,Nchan) = hg    - saturation enthalpy (vapor/gas)
    ! acond(24,Nchan) = vf    - saturation specific volume (liquid)
    ! acond(25,Nchan) = vg    - saturation specific volume (vapor)
    ! acond(26,Nchan) = cpf   - saturation heat capacity (liquid)
    ! acond(27,Nchan) = cpg   - saturation heat capacity (vapor)
    !
    ! aasth           = table of water properties initialized by routine sth2xi
    ! AAHT1,BBHT1     = constants such that - AAHT1*tsurf + BBHT1 = flux where tsurf=surface temperature.
    ! equations for evaluating AAHT1 and BBHT1 found by triangu
    ! system of equation used to solve for fuel rod temperature distribution
    ! dr              = fuel rod outer diameter, feet
    ! hcoef           = surface heat transfer coeficient, btu/ft**2-hr-F output
    ! hflux           = surface flux, btu/ft**2-hr                       output
    ! qcrit           = critical heat flux, btu/ft**2-hr                 output
    ! tsurf           = surface temperature, F                 input and output
    ! iht             = heat transfer mode code                          output
    !               1 - forced convection to liquid
    !                   Dittus-Boelter htc
    !               2 - subcooled nucleate boiling
    !                   Thom+D.B. htc (default)
    !                   Chen htc
    !               3 - saturated nucleate boiling
    !                   Thom+D.B. htc (default)
    !                   Chen htc
    !               4 - post-CHF transition boiling (G>0.2 Mlbm/hr-ft2)
    !                   modified Tong-Young (default)
    !                   modified Condie-Benston
    !                   Bjornard-GrIffith
    !               5 - post-CHF film boiling (G>0.2 Mlbm/hr-ft2)
    !                   Groeneveld 5.9 (default)
    !                   Groeneveld 5.7
    !                   Bishop-Sandburg-Tong
    !                   Groeneveld-Delorme
    !               6 - low-flow post-CHF transition boiling (G<0.2 Mlbm/hr-ft2) modified Bromley
    !               7 - low-flow post-CHF film boiling (G<0.2 Mlbm/hr-ft2) modified Bromley
    !               8 - forced convection to superheated steam Dittus-Boelter htc
    !               9 - (no longer used)
    !              10 - zero flow, minimum htc defined as 5.0 Btu/hr-ft2-F
    !              11 - adabatic heatup with zero htc
    !              12 - flecht reflood heat transfer
    !              80 - single-phase convection to helium gas
    !
    ! Note: If fluid is helium, not water: jchf = 9 and mode = 80;
    ! Heat transfer mode selection logic is bypassed; Use Dittus_fraptran-Boelter-type htc everywhere
    !
    ! lhtc            = indicator for heat transfer correlations
    ! ndebug          = debug print flag
    ! nqchn           = indicator of method used to compute coolant conditions
    !               0 - coolant conditions computed in Subroutine cool
    !               3 - coolant conditions specified by input
    !               4 - coolant conditions specified by input
    !               5 - coolant conditions specified by input
    ! hcoef set equal to 5.0 If g=0 and nqchn=0
    ! k               = local axial node number                   input
    ! AxNodElevat     = array of axial positions, ft              input
    ! rl              = fuel rod length, ft
    ! Ichf(k)         = cold wall and axial factor control for chf. input & output
    ! HtFlxFac        = chf factor for non-unIform axial heat flux. input & output
    ! SurfHtFlux      = surface heat fluxes at each axial node.     input & output
    ! fqcrit          = multiplicative factor on critical heat flux (input specified) (default is 1.0)
    ! nchfsw          = switch to force film boiling at axial node k.  0=no, 1=yes
    ! tempcm          = maximum attained cladding temperature (degrees F )
    ! zroxid          = oxide layer thickness  (m)
    ! elaptm          = elapsed problem time (sec)
    ! dt              = time step (sec)
    ! maxn            = number of axial nodes in the fuel rod
    ! AxialPowr       = axial power array   kW/ft
    ! AxialNodLen     = axial node length array
    ! tmeltc          = cladding melting temperature (F)
    ! prop            = array in which state properties are passed to and from steam table Subroutines.  
    !                   Values are in SI units and appear in CoolProperties.
    ! prop(x)       1 - temperature, K
    !               2 - pressure, n/m2
    !               3 - specific volume, m3/kg
    !               8 - single phase csubp, j/kg-K
    !
    INTEGER(ipk), INTENT(IN) :: k, nchfsw
    INTEGER(ipk), INTENT(INOUT) :: iht
    INTEGER(ipk) :: nfflg, nht, it, iz, i, iend, irads, itte, lpool, j, jchf
    REAL(r8k) :: rg, rtmp, tkloc, hmin, cpf, cpg, delbow, ts, tchf, hcdum, qa, tsa, qb, tsb, htcnb, beta, tave, cpm, &
      &          rfm, qbl, tsbl, tamb, dtemp, emscld, tshrd, tshrdk, zroxsh, emsshd, fcs, rk, c1, tts, c2, vf1, vf2, &
      &          vf3, trd1, trd2, trd3, tsr, tcon, tvis, cphe, re, prn, anu, bnu, cnu, dnu, fnu, aa, bb, aflow, dh, &
      &          hd, g, h, hin, z, pr, tbulk, fluxk, r, x, cp, tsat, hf, hg, xc, vf, rf, vg, sr, alpha, &
      &          dum = 0.0_r8k, dum1 = 0.0_r8k
    REAL(r8k), INTENT(IN) :: tmeltc, zroxid, tempcm, rl, dr
    REAL(r8k), INTENT(OUT) :: hcoef, hflux, qcrit
    REAL(r8k), INTENT(INOUT) :: tsurf
    REAL(r8k), PARAMETER :: sipr = 6894.76_r8k       ! sipr   = pressure multiplier   lbf/in2 to n/m2
    REAL(r8k), PARAMETER :: sisvol = 6.24279606e-2_r8k ! sisvol = specific volume multiplier   ft3/lbm to m3/kg
    REAL(r8k), PARAMETER :: encp = 2.388458966e-4_r8k  ! encp   = heat capacity multiplier   j/kg-K to btu/lbm-F
    REAL(r8k), PARAMETER :: five = 5.0_r8k
    REAL(r8k), PARAMETER :: twoht = 200000.0_r8k
    REAL(r8k), PARAMETER :: exm20 = 1.0e-20_r8k
    REAL(r8k), PARAMETER :: xnear1 = 0.999_r8k
    REAL(r8k), PARAMETER :: d1p05 = 1.05_r8k
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), PARAMETER :: one = 1.0_r8k
    REAL(r8k), PARAMETER :: fthun = 1500.0_r8k
    REAL(r8k), PARAMETER :: gzero = 2.0_r8k
    REAL(r8k), PARAMETER :: sigma = 1.73e-09_r8k
    REAL(r8k), PARAMETER :: ftor = 459.67_r8k
    REAL(r8k), PARAMETER :: tradth = 700.0_r8k
    LOGICAL :: err, CalcShroud
    REAL(r8k), DIMENSION(nsrad3*2) :: dtrad
    CalcShroud = .FALSE.
    !
    ! jmc -- calculate hmin for Nu = 1
    !
    aa = AAHT1(k) * sechr
    bb = BBHT1(k) * sechr
    IF (ndebug) WRITE(ounit,921) tsurf, AAHT1(k), BBHT1(k), iht, k
921 FORMAT(' HTRC: tsurf = ',e13.6,' AAHT1 = ',e13.6,' BBHT1 = ',e13.6,' iht = ',i3,' k = ',i3)
    IF (nqchn == 3 .OR. nqchn == 4 .OR. nqchn == 5) GOTO 2000
    j = MOD(lhtc,10)
    jchf = lhtc / 10
    IF (ndebug) WRITE(ounit,911) j, jchf
911 FORMAT(7x,' film boiling indicator j = ',i3,' chf indicator jchf = ',i3)
    aflow = acond(9,Nchan) ! aflow = flow cross-sectional area, ft**2
    dh = acond(10,Nchan)
    hd = acond(11,Nchan)
    g = ABS(acond(12,Nchan))
    h = acond(13,Nchan)
    hin = acond(14,Nchan)
    z = AxNodElevat(k)
    IF (acond(15,Nchan) < hin) THEN
        hin = acond(15,Nchan)
        HtFlxFac(k) = one
        z = rl - z
    ENDIF
    z = z * ftin
    pr = acond(16,Nchan)
    tbulk = acond(17,Nchan)
    fluxk = SurfHtFlux(k)
    ! If jchf = 9 , coolant is helium 
    ! Skip all mode selection logic -- single-phase convection the is only possibility
    IF (jchf == 9) GOTO 3000
    !
    r = one / acond(18,Nchan)
    x = acond(19,Nchan)
    cp = acond(20,Nchan)
    tsat = acond(21,Nchan)
    hf = acond(22,Nchan)
    hg = acond(23,Nchan)
    xc = (h - hf) / (hg - hf)
    vf = acond(24,Nchan)
    rf = one / vf
    vg = acond(25,Nchan)
    rg = one / vg
    !
    CALL voidratio (x, rf, rg, dh * 12.0_r8k, sr, alpha)
    ! jmc
    ! jmc define hmin for Nu = 1 (hmin = k/Dh)
    ! jmc
    IF (tbulk <= tsat) THEN
        nfflg = 1
        rtmp = rf
    ELSE
        nfflg = 2
        rtmp = rg
    ENDIF
    tkloc = thcon(nfflg, tbulk, rtmp)
    hmin = tkloc / dh
    ! jmc
    cpf = acond(26,Nchan)
    IF (ndebug) WRITE(ounit,907) g, x, tsat, iht, hd, pr, h, hin
907 FORMAT(7x,' g = ',e13.6,' x = ',e13.6,' tsat = ',e13.6, &
     &    ' iht = ',i3, ' hd = ',e13.6,/' pr = ',e13.6,' h = ',e13.6,' hin = ',e13.6)
    cpg = acond(27,Nchan)
    ! If flow is stagnant (g = 0), skip heat transfer mode selection, assume minimum htc = 5.0 Btu/hr-ft2-F
    ! jmc
    IF (g <= gzero .AND. nqchn == 0) GOTO 1000
    ! Compute critical heat flux
    CALL pchf (g, hf, hg, hin, pr, xc, z, dr * ftin, dh * ftin, hd * ftin, qcrit, Ichf(k), &
      &        HtFlxFac(k), SurfHtFlux, AxNodElevat, k, acond, jchf, tsat, rf, rg, alpha, cpg, &
      &        tsurf, cpf, tbulk, naxn, Nchan, r, cp)
    !
    qcrit = fqcrit * qcrit
    IF (nqbow < 1) GOTO 70
    ! Account for effect of row bowing on chf
    IF (ExtentOfBow(k) < bowthr) GOTO 70
    delbow = ffchf * ((ExtentOfBow(k) - bowthr) / (1.0_r8k - bowthr))
    qcrit = (1.0_r8k - delbow) * qcrit
70  CONTINUE
    IF (nchfsw == 1) GOTO 600
    ! **Determine heat transfer mode**
    ! Check for single-phase forced convection to subcooled liquid (mode 1) or superheated vapor (mode 8)
    IF (tbulk >= tsat .AND. x >= xnear1) GOTO 140
    IF (tbulk < d1p05 * tsat) GOTO 120
    WRITE(ounit,9001) x, tbulk, tsat
9001 FORMAT(/// ' *** ERROR STOP ***  in Subroutine: htrc ',/, &
       &        ' Quality, coolant temperature, and saturation temperatures are inconsistent.',/, &
       &        ' Values = ',3(1pe15.6))
    !
    CALL fabEnd
    !
120 CONTINUE
    IF (tsurf < tsat) GOTO 130
    GOTO 160
    !
140 CONTINUE
    iht = 8
    IF (x < one) cp = cpg
    GOTO 150
    !
130 CONTINUE
    iht = 1
    IF (x > zero) cp = cpf
150 CONTINUE
    ! Single-phase convection (mode 1 or mode 8)
    CALL qdot (aa, bb, cpf, cpg, cp, rf, rg, r, g, x, hd, hf, hg, pr, qcrit, hflux, tbulk, h, ts, &
      &        tsat, tsurf, tchf, iht, j, lpool, tempcm, zroxid, dh, aflow, dr, dum1, fluxk, hcdum)
    !
    GOTO 890
    !
160 CONTINUE
    ! Initial boiling heat transfer mode selection based on previous time-step solution
    IF (iht > 9) iht = 9
    SELECT CASE (iht)
        ! Mode = 1 2 3 4 5 6 7 8 9
    CASE(0, 1, 2)
        GOTO 200
    CASE(3, 4, 5, 6, 7)
        GOTO 300
    CASE(8, 9)
        GOTO 600
    CASE DEFAULT
        WRITE(ounit,*) 'Execution terminated in htrc due to bad value for iht. iht =',iht
        ERROR STOP 'Execution terminated in htrc due to bad value for iht'
    END SELECT
200 CONTINUE
    !
    IF (tbulk >= tsat) GOTO 300
    ! If fluid is subcooled, calculate both single-phase forced convecton to liquid and subcooled nucleate boiling htcs
    ! jmc
    IF (x > zero) cp = cpf
    ! jmc assume mode = 1 -- single-phase forced convection to liquid
    nht = 1
    CALL qdot (aa, bb, cpf, cpg, cp, rf, rg, r, g, x, hd, hf, hg, pr, qcrit, qa, tbulk, h, tsa, &
      &        tsat, tsurf, tchf, nht, j, lpool, tempcm, zroxid, dh, aflow, dr, dum1, fluxk, hcdum)
    ! jmc -- debug
    ! jmc    WRITE(ounit,1907) nht, qcrit, qa, k, tbulk, tsa, tchf, h
    ! jmc 1907 FORMAT(' HTRC: nht = ',i3,' qcrit = ',e13.6,' qa = ',e13.6,
    ! jmc     &    ' k = ',i3, ' tbulk = ',e13.6,
    ! jmc     &    /' tsa = ',e13.6' tchf = ',e13.6,
    ! jmc     &    ' h = ',e13.6)
    !
    ! jmc assume mode = 2 -- subcooled nucleate boiling
    ! (Note: no distinction between modes 2 and 3 for Thom or Chen correlations)
300 CONTINUE
    !
    nht = 2
    CALL qdot (aa, bb, cpf, cpg, cp, rf, rg, r, g, x, hd, hf, hg, pr, qcrit, qb, tbulk, h, tsb, &
      &        tsat, tsurf, tchf, nht, j, lpool, tempcm, zroxid, dh, aflow, dr, dum1, fluxk, htcnb)
    ! jmc -- debug
    ! jmc    WRITE(ounit,2907) nht, qcrit, qb, k, tbulk, tsa, tsb, tchf, h
    ! jmc 2907 FORMAT('HTRC: nht = ',i3,' qcrit = ',e13.6,' qb = ',e13.6,
    ! jmc     &    ' k = ',i3, ' tbulk = ',e13.6,
    ! jmc     &    /' tsa = ',e13.6,' tsb = ',e13.6,' tchf = ',e13.6,
    ! jmc     &    ' h = ',e13.6)
    ! new selection is mode 1 If
    ! a.) quality is below zero,
    ! AND
    ! [ b.) single-phase htc predicts higher twall than boiling htc
    ! OR
    ! c.) boiling htc predicts twall below tsat]
    !
    ! jmc If (tsb < tsa .AND. tsb > (tsat + 0.1_r8k)) GOTO 500
    IF (x > zero) GOTO 500
    IF (tsb < tsa .AND. tsb > tsat) GOTO 500
    iht = 1
    hflux  = qa
    ts  = tsa
    GOTO 890
    !
500 CONTINUE
    ! jmc calculate tchf for qcrit, using mode 2 (nucleate boiling) htc
    ! (Note: no distinction between modes 2 and 3 for Thom or Chen correlations)
    tchf = tbulk + qcrit / htcnb
    ! Check for flags to force transition to post-CHF heat transfer regime
    IF (nchfmd >= 1 .AND. nchfsw == 0) GOTO 596
    IF (nchfmd >= 1 .AND. nchfsw == 1) GOTO 600
    ! Check nucleate boiling heat flux against predicted critical heat flux
    IF (qb > qcrit) GOTO 600
    ! New selection is mode 2 If nucleate boiling heat flux is less than the critical heat flux
    ! jmc
596 CONTINUE
    hflux = qb
    ts = tsb
    iht = 2
    GOTO 890
    !
600 CONTINUE
    ! jmc
    ! If appropriate input flags are specified, force mode to post-CHF by
    ! 1) defining wall temperature just above saturation (tsurf = tsat + 0.1)
    ! 2) defining critical heat flux as very small (qcrit = 0.000001*qcrit)
    ! jmc
    IF (nchfsw == 1 .AND. tsurf < (tsat + 0.1_r8k)) tsurf = tsat + 0.1_r8k
    IF (nchfsw == 1) qcrit = 1.0e-6_r8k * qcrit
    ! If nucleate boiling heat flux exceeds critical heat flux--
    ! first, check for low flow regime (mode 6 or 7) 
    IF (g < twoht) GOTO 621
    ! second, calculate surface heat flux assuming post-CHF transition boiling (mode = 4)
    nht = 4
    !
    CALL qdot (aa, bb, cpf, cpg, dum, rf, rg, dum, g, x, hd, hf, hg, pr, qcrit, qa, tbulk, h, tsa, &
      &        tsat, tsurf, tchf, nht, j, lpool, tempcm, zroxid, dh, aflow, dr, dum1, fluxk, hcdum)
    !
    IF (tsurf > fthun) GOTO 619
    prop(1) = tfk(tsurf)
    prop(2) = pr * sipr
    !
    CALL sth2x3 (aasth, prop, it, err)
    !
    IF (err) THEN
        WRITE(ounit,615) tsurf, pr
615     FORMAT(' failure in Call to sth203 from htrc.  p, t = ',2e15.6)
        CALL fabEnd
    ENDIF
    ! jmc
    ! calculate cp and r based on local wall temperature
    cp = prop(8) * encp
    r = sisvol / prop(3)
619 CONTINUE
    ! third, calculate surface heat flux assuming post-CHF film boiling (mode = 5)
    nht = 5
    !
    CALL qdot (aa, bb, cpf, cpg, cp, rf, rg, r, g, x, hd, hf, hg, pr, qcrit, qb, tbulk, h, tsb, &
      &        tsat, tsurf, tchf, nht, j, lpool, tempcm, zroxid, dh, aflow, dr, dum1, fluxk, hcdum)
    ! jmc
    ! select new heat transfer mode based on maximum surface heat flux 
    ! predicted with transition boiling and film boiling correlations
    ! If q_film_boiling < q_tr_boiling (qb < qa), transition boiling (mode 4)
    ! If q_film_boiling >= q_tr_boiling (qb >= qa), film boiling (mode 5)
    IF (qb >= qa) GOTO 610
    ! Current mode is transition boiling
    iht = 4
    hflux = qa
    ts = tsa
    GOTO 620
    !
610 CONTINUE
    ! Current mode is film boiling
    iht = 5
    hflux = qb
    ts = tsb
    !
620 CONTINUE
    ! jmc
    IF (g >= twoht) GOTO 890
    ! Low flow film boiling regime (mode 6 or 7)
    ! Note: one correlation covers both transition and film boiling
621 CONTINUE
    ! Compute coefficient of thermal expansion of saturated water vapor (1/F)
    prop(2) = pr * sipr
    prop(9) = x
    !
    CALL sth2x2 (aasth, prop, err)
    !
    IF (err) THEN
        WRITE(ounit,728)
728     FORMAT(' *** Call to Subroutine sth2x2 of wagner water property', &
          & ' package not successful in try for coefficient of thermal ', &
          & 'expansion ***',/,' Subroutine htrc,  program stopped '  )
        ERROR STOP 'Bad call to Subroutine: sth2x2. Called from Subroutine: htrc'
    ENDIF
    ! Store coefficient of thermal expansion in qcrit
    beta = prop(18) * 0.55555556_r8k
    tave = (tsurf + tsat) * 0.5_r8k
    prop(1) = tfk(tave)
    prop(2) = pr * sipr
    !
    CALL sth2x3 (aasth, prop, it, err)
    !
    IF (err) THEN
        WRITE(ounit,733)
733     FORMAT(//,   ' *** error found in sth2x3. called from htrc to', &
          & ' find heat capacity for free convection to vapor. program stopped*** ')
        ERROR STOP 'Bad call to Subroutine: sth2x3. Called from Subroutine: htrc'
    ENDIF
    cpm = prop(8) * encp
    rfm = sisvol / prop(3)
    ! jmc assume mode 6 -- low flow post-CHF heat transfer
    nht = 6
    CALL qdot (aa, bb, cpf, cpg, cpm, rf, rg, rfm, g, x, hd, hf, hg, pr, qcrit, qbl, tbulk, h, tsbl, &
      &        tsat, tsurf, tchf, nht, j, lpool, tempcm, zroxid, dh, aflow, dr, beta, fluxk, hcdum)
    ! jmc 
    ! original code logic evaluates mode 4 or mode 5 heat flux even If
    ! g<0.2 Mlbm/hr-ft2; If heat flux from that calculation is greater than  
    ! mode 6 heat flux, mode 6 prediction is ignored.
    !
    ! current mods skip the mode 4, 5 calc If g < 0.2, so mode 6 is used if in low flow region.  
    ! jmc
    ! jmc    IF (hflux >= qbl) GOTO 890
    iht = 6
    hflux = qbl
    ts = tsbl
    !
890 CONTINUE
    !
    tamb = tsat
    IF ((iht == 1) .OR. (iht == 8)) tamb = tbulk
    dtemp = ts - tamb
    IF (ABS(dtemp) < exm20) dtemp = exm20
    hcoef = ABS(hflux / dtemp)
    IF (hcoef < hmin) hcoef = hmin
    IF (iht == 6 .OR. iht == 7) GOTO 975
    ! jmc
    ! modify heat transfer coefficient to account for local flow effects due to clad  ballooning
    IF (nfhtc /= 1 .OR. k /= kaxhtc) GOTO 975
    hcoef = fhtc * hcoef
    ts = (bb + hcoef * tamb) / (hcoef - aa)
    dtemp = ts - tamb
    IF (ABS(dtemp) < exm20) dtemp = exm20
975 CONTINUE
    IF (iht /= 4) GOTO 810
    ! jmc
    ! Clad ballooning correction factor for transition boiling (mode = 4)
    ! jmc
    IF (fdial(22) == 1.0_r8k) GOTO 880
    hcoef = fdial(22) * hcoef
805 CONTINUE
    ts = (bb + hcoef * tamb) / (hcoef - aa)
    dtemp = ts - tamb
    IF (ABS(dtemp) < exm20) dtemp = exm20
    GOTO 880
    !
810 CONTINUE
    IF (iht /= 5) GOTO 820
    ! jmc
    ! Clad ballooning correction factor for film boiling (mode = 5)
    ! jmc
    IF (fdial(23) == 1.0_r8k) GOTO 880
    hcoef = fdial(23) * hcoef
    ! jmc
    GOTO 805
    !
820 CONTINUE
    IF (iht /= 6) GOTO 830
    ! jmc
    ! Clad ballooning correction factor for low flow post-CHF conditions (mode = 6 or 7)
    ! jmc
    IF (fdial(24) == 1.0_r8k) GOTO 880
    hcoef = fdial(24) * hcoef
    GOTO 805
    !
830 CONTINUE
    IF (iht /= 7) GOTO 880
    IF (fdial(25) == 1.0_r8k) GOTO 880
    hcoef = fdial(25) * hcoef
    ! jmc
    GOTO 805
    !
880 CONTINUE
    hflux = hcoef * dtemp
    SurfHtFlux(k) = hflux
    ! To perform rewetting calculations, limit cladding temperature to melting temperature to avoid program STOP
    IF (ts < tmeltc) GOTO 885
    ts = tmeltc
    dtemp = ts - tamb
    hcoef = ABS(hflux / dtemp)
885 CONTINUE
    tsurf = ts
    IF (ndebug) THEN
        WRITE(ounit,909) hcoef, iht
909     FORMAT(' HTRC for plnt: hcoef = ',e13.6,' iht = ',i3)
        WRITE(ounit,985) iht, hflux, tsurf, dtemp
985     FORMAT(7x,' iht = ',i5,' hflux = ',e12.5,' tsurf =' ,e12.5,' dtemp = ',e12.5)
    END IF
    IF (nradsh >= 1) GOTO 725
    IF (nsrad3 >= 1) GOTO 725
    ! The following statement should be removed here and in three other
    ! places in htrc.f once the reason for spurious negative heat fluxes is fixed
    ! jmc -- debug: see what happens If this is removed...
    ! jmc    (I am not convinced all negative heat fluxes are spurious for this code)
    ! jmc    IF (hflux < 0.0_r8k) hflux = 0.0_r8k
    IF (ndebug) THEN
        WRITE(ounit,987) k, iht
        WRITE(ounit,988) g, hcoef, hd, hflux, qcrit, pr, h, hf, hg, vf, vg, tsat, tbulk, x, alpha
    ENDIF
    !
    RETURN
    ! Option to add heat transfer due to radiation to flow shroud
725 CONTINUE
    tbulk = tamb
    IF (nsrad3 >= 1) GOTO 2040
    IF (tsurf < tradth) RETURN
    CalcShroud = .TRUE.
    GOTO 2040
    ! jmc mode 10 -- zero flow; minimum heat transfer coefficient, chf not applicable
1000 CONTINUE
    hcoef = five
    ts = (bb + hcoef * tbulk) / (hcoef - aa)
    hflux = hcoef * (ts - tbulk)
    SurfHtFlux(k) = hflux
    tsurf = ts
    qcrit = 1.0_r8k
    iht = 10
    ! The following statement should be removed here and in three other
    ! places in htrc.f once the reason for spurious negative heat fluxes is fixed
    ! jmc -- debug: see what happens If this is removed...
    ! jmc    (I am not convinced all negative heat fluxes are spurious for this code)
    ! jmc    IF (hflux < 0.0_r8k) hflux = 0.0_r8k
    !
    IF (ndebug) THEN
        WRITE(ounit,987) k, iht
        WRITE(ounit,988) g, hcoef, hd, hflux, qcrit, pr, h, hf, hg, vf, vg, tsat, tbulk, x, alpha
    ENDIF
    !
    RETURN
    !
    ! jmc option for heat transfer coefficient specified by input
    ! jmc  (coolant solution not used; heat transfer mode selection logic not used)
    ! jmc
2000 CONTINUE
    !
    hcoef = acond(13,Nchan)
    IF (nfhtc == 1 .AND. k == kaxhtc) hcoef = fhtc * hcoef
    tbulk = acond(17,Nchan)
    ! Check for option to include radiation to flow shroud
2040 IF (nsrad3 >= 1) THEN
        ! Calculate c1 and c2 for multiple radiation option
        IF (AxNodElevat(k) < elvrad(1)) THEN
            iz = 1
        ELSE IF (AxNodElevat(k) > elvrad(nelrad)) THEN
            iz = nelrad
        ELSE
            iz = 0
            DO i = 1, nelrad
                iz = iz + 1
                IF (AxNodElevat(k) < elvrad(iz)) EXIT
            ENDDO
        END IF
        vf1 = vfrad1(iz)
        vf2 = vfrad2(iz)
        vf3 = vfrad3(iz)
        iEnd = nsrad3 * 2
        !
        DO irads = 1, 3
            DO itte = 1, iEnd
                IF (irads == 1) dtrad(itte) = trad1(itte,iz)
                IF (irads == 2) dtrad(itte) = trad2(itte,iz)
                IF (irads == 3) dtrad(itte) = trad3(itte,iz)
            ENDDO
            IF (irads == 1) trd1 = tfr (polate (dtrad, Time, nsrad3))
            IF (irads == 2) trd2 = tfr (polate (dtrad, Time, nsrad3))
            IF (irads == 3) trd3 = tfr (polate (dtrad, Time, nsrad3))
        ENDDO
        !
        c1 = -(aa - hcoef) / (sigma * (vf1 + vf2 + vf3))
        c2 = -(bb + hcoef * (tbulk + ftor) + sigma * (vf1 * trd1 ** 4 + vf2 * trd2 ** 4 + &
          &    vf3 * trd3 ** 4) - ftor * aa) / (sigma * (vf1 + vf2 + vf3))
        !
        CALL root1 (c1, c2, tsurf+ftor, tsr)
        !
        tsurf = tsr - ftor
        hflux = aa * tsurf + bb
        hcoef = hflux / (tsurf - tbulk)
    ELSE IF ((nradsh <= 0 .OR. tsurf < tradth) .AND. (.NOT. CalcShroud)) THEN
        tsurf = (bb + hcoef * tbulk) / (hcoef - aa)
        hflux = hcoef * (tsurf - tbulk)
    ELSE
        ! Find emissivities of cladding surface and flow shroud
        
        ! Cladding emissivity
        emscld = MatProperty (Material='CLAD', Property='EMISS', Temperature=tfk(tsurf), Oxide=zroxid)
        
        ! Flow shroud emissivity
        tshrd = polate (tshrda, Time, nradsh) ! Interpolate for temperature
        tshrdk = tfk(tshrd) ! Convert from F to K
        zroxsh = 1.4e-6_r8k ! Fixed oxide thickness
        emsshd = MatProperty (Material='CLAD', Property='EMISS', Temperature=tshrdk, Oxide=zroxsh)
        
        !
        fcs = 1.0_r8k / (1.0_r8k / emscld + (dr / (2.0_r8k * rshrd)) * (1.0_r8k / emsshd - 1.0_r8k))
        rk = fcs * sigma
        c1 = (hcoef - aa) / rk
        tts = (tshrd + ftor) * (tshrd + ftor)
        c2 = -(tts * tts + (hcoef * (tbulk + ftor) + bb - ftor * aa) / rk)
        !
        CALL root1 (c1, c2, tsurf+ftor, tsr)
        !
        tsurf = tsr - ftor
        hflux = aa * tsurf + bb
        hcoef = hflux/(tsurf - tbulk)
    END IF

    IF (nqchn == 3 .OR. nqchn == 4 .OR. nqchn == 5) qcrit = 0.0_r8k
    SurfHtFlux(k) = hflux
    !
    ! the following statement should be removed here and in three other
    ! places in htrc.f once the reason for spurious negative heat fluxes is fixed
    !
    ! jmc -- debug: see what happens If this is removed...
    ! jmc    (I am not convinced all negative heat fluxes are spurious for this code)
    ! jmc    IF (hflux < 0.0_r8k) hflux = 0.0_r8k
    !
    IF (ndebug) THEN
        WRITE(ounit,987) k, iht
        WRITE(ounit,988) g, hcoef, hd, hflux, qcrit, pr, h, hf, hg, vf, vg, tsat, tbulk, x, alpha
    ENDIF
    !
    RETURN
    ! option for helium coolant -- heat transfer coefficient calculated using modified Dittus-Boelter correlation
    ! tcon   = thermal conductivity of helium      (btu/ft-hr-F)
    ! tvis   = dynamic viscosity of helium         (lbm/hr-ft)
3000 CONTINUE
    ! Gas thermal conductivity
    tcon = 0.577902_r8k * MatProperty (Material='GAS', Property='THERMCOND', Temperature=tfk(tbulk), &
      &                                Pressure=(6.897e3_r8k*pr), GasComposition=GasFraction)
    ! Gas viscosity
    tvis  = 2419.09_r8k * MatProperty (Material='GAS', Property='VISCOSITY', Temperature=tfk(tbulk), &
      &                                Pressure=(6.897e3_r8k*pr), GasComposition=GasFraction)
    ! cphe = specific heat of helium
    cphe = 1.24_r8k
    ! re = reynolds number
    re = g * hd / tvis
    ! prn = prandtl number
    prn = cphe * tvis / tcon
    ! fnu = nusselt number
    anu = 0.0215_r8k
    bnu = 1.15_r8k
    cnu = 0.8_r8k
    dnu = 0.6_r8k
    fnu = anu * bnu * (re ** cnu) * (prn ** dnu)
    ! hcoef  = calculated heat transfer coefficient  (btu/hr-ft2-F) (for helium coolant option)
    IF (z > zroug1 .AND. z < zroug2) fnu = 2.0_r8k * fnu
    hcoef = fnu * tcon / hd
    tsurf = (bb + hcoef * tbulk) / (hcoef - aa)
    hflux = hcoef * (tsurf - tbulk)
    iht = 80
    qcrit = 0.0_r8k
    ! The following statement should be removed here and in three other
    ! places in htrc.f once the reason for spurious negative heat fluxes is fixed
    ! jmc -- debug: see what happens If this is removed...
    ! jmc    (I am not convinced all negative heat fluxes are spurious for this code)
    ! jmc    IF (hflux < 0.0_r8k) hflux = 0.0_r8k
    IF (ndebug) THEN
        WRITE(ounit,987) k, iht
987     FORMAT(' HTRC: k = ',i5,' iht = ',i5)
        WRITE(ounit,988) g, hcoef, hd, hflux, qcrit, pr, h, hf, hg, vf, vg, tsat, tbulk, x, alpha
988     FORMAT(' HTRC: g     = ',e11.4,' hcoef   = ',e11.4, &
          &    ' hd    = ',e11.4,' hflux = ',e11.4,' qcrit = ',e11.4, &
          &    ' pr     = ',e11.4, /6x,' h     = ',e11.4,' hf    = ', &
          &    e11.4,' hg    = ',e11.4,' vf    = ',e11.4,' vg    = ', &
          &    e11.4,' tsat   = ',e11.4, /6x,' tbulk = ',e11.4, &
          &    ' x     = ',e11.4,' alpha = ',e11.4)
    ENDIF
    !
    END SUBROUTINE htrc
    !
    !
    !
    SUBROUTINE qdot (a, b, cpf, cpg, cp, rf, rg, r, MassFlowRate, Quality, hd, hf, hg, CoolPress, qcrit, qq, tbulk, &
      &              h, ts, tsat, tsur, tchf, ih, j, l, tempcm, zroxid, dh, aflow, dr, beta, fluxk, hcsave)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, sechr, tfk, tfr
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran, ONLY : ounit, Time, ndebug, Radiation, timeincrement
    USE CoolantProperties_fraptran, ONLY : Prop, aasth
    USE bcdcom_h_fraptran
    USE emssf_fraptran, ONLY : emssf1
    USE sth2x_fraptran, ONLY : sth2x0, sth2x3, surten, VoidRatio, thcon, visc, viscol
    IMPLICIT NONE
    !> @brief
    !> qdot calculates surface temperature, flux, and h.t.c., given a specific heat transfer correlation and conduction values.
    !>@author
    !> Re-written by Ian Porter, NRC
    !>@date
    !> 4/21/2015
    !
    ! new heat trans props, from tkcon, visc, steam tables.  7/74  gaj
    !
    ! a, b         - coefficients from the conduction solution in the form qq - a*tsur + b
    ! cpf          - specific heat capacity for saturated liquid
    ! cpg          - specific heat capacity for saturated gas
    ! cp           - specific heat capacity for single phase
    ! rf           - density for saturated liquid, lbm/ft**3
    ! rg           - density for saturated gas
    ! r            - single phase density
    ! MassFlowRate - coolant flow rate
    ! Quality      - volume average quality
    ! hd           - hydraulic diameter, ft
    ! hf           - saturation liquid enthalpy   (btu/lb)
    ! hg           - saturation gas enthalpy
    ! CoolPress    - volume pressure
    ! qcrit        - critical heat flux  (btu/hr-ft2)
    ! tchf         - critical heat flux temperature (F)
    ! qq           - surface heat flux (btu/hr-ft2)     output
    ! tbulk        - bulk coolant temperature  (F)
    ! h            - local coolant enthalpy (Btu/lbm)
    ! ts           - surface temperature (F)            output
    ! tsat         - saturation temperature (F)
    ! tsur         - surface temperature guess          input
    ! ih           - heat transfer mode --
    !            1 = forced convection to single-phase liquid
    !            2 = subcooled nucleate boiling
    !            3 = saturated nucleate boiling
    !                Note: Thom+D.B and Chen correlations for nucleate boiling are each applicable to both modes.
    !                      Mode is defined based on tbulk and tsat:
    !                      If tbulk < tsat, mode = 2
    !                      If tbulk = tsat, mode = 3
    !            4 = post-CHF transition boiling
    !            5 = post-CHF film boiling
    !            6 = low flow (g < 0.2 Mlbm/ft-ft2) transition boiling
    !            7 = low flow (g < 0.2 Mlbm/ft-ft2) film boiling
    !            8 = forced convection to single-phase vapor (or gas)
    !            9 = (not used)
    ! jfb          - indicator for film boiling correlation
    !            0 = Groeneveld 5.9 for mode 5
    !            1 = Groeneveld 5.7 for mode 5
    !            2 = Bishop-Sandburg-Tong for mode 5
    !            3 = Groeneveld-Delorme for mode 5
    !
    ! l            - flag for type of low-flow post-CHF boiling (used only for modes 6 and 7)
    !            1 = low flow transition boiling                  output
    !            2 = low flow post-CHF boiling, with laminar flow
    !
    ! jtr          - indicator for transition boiling correlation
    !            0 = use modified Tong_fraptran-Young for mode 4
    !            1 = use modified Condie_fraptran-Bengston for mode 4
    !            2 = use Bjornard_fraptran-GrIffith for mode 4
    !
    ! tempcm       - peak clad temperature (F)
    ! zroxid       - oxide layer thickness
    ! dh           - equivalent heated diameter (ft)
    ! aflow        - cross-sectional flow area  (ft**2)
    ! dr           - cladding outer diameter (ft)
    ! beta         - thermal coefficient of expansion of sat. vapor phase (1/F) Note: beta is defined only If ih=6
    ! hcsave       - heat transfer coefficient for current node
    !
    INTEGER(ipk) :: j, itn, k, l, it, ireyn = 1, ifsup = 1
    INTEGER(ipk), INTENT(IN) :: ih
    INTEGER(ipk), PARAMETER :: n1 = 1
    INTEGER(ipk), PARAMETER :: n2 = 2
    INTEGER(ipk), PARAMETER :: nreyn = 15
    INTEGER(ipk), PARAMETER :: nfsup = 13
    REAL(r8k) :: coolpress, pressi, tbulkk, rhosi, r, tsatk, viscsi, vsf, &
      &          prf, cp, ref, hd, hc, hlam, hturb, hspl, ts, b, tbulk, a, hcsave, dtf, tsat, tsur, &
      &          fp1, qnb, hnb, twall2, tin1, twgs, tin2, tout1, tout2, dtidto, sig, cpf, vsg, &
      &          freyn, xgam, gamma, rf, rg, fsupin, fsupf, fmic, hg, hf, hmac, tsurk, psatsi, psatw, &
      &          dpsat, dtsat, hmic, xsave, yyy, tp1, tkg, tk, vs, prw, reg, hgroen, hfilmb, qfb, &
      &          tfilmk, cpfilm, rfilm, tkfilm, tkfv, vsfilm, prfilm, refilm, alfa, rb, hcv, sr, &
      &          tfilm, alpha, hfg, tkgv, rehom, prg, cpg, twall, a1, fluxk, hvap, tan, xvap, px, &
      &          hx, vsv, vvap, tvap, rvap, gdnu, dtsat1, al, hbrom, hspv, qtot, htot, xe, qchfv, &
      &          qcrit, hfbv, tchf, dtchf, qfbv, qcty, cty1, cty2, qtbv, hcmin, qtb, htranb, hpchf, &
      &          qqfb, htbv, rfg, gcdumy, dtmina, fluxn, tmin, qmin, delta, qmindb, prn, re, &
      &          rdtest, dh, tshrd, toxshr, fe, rk, c1, tts,c2, h, tempcm, zroxid, aflow, dr, beta, &
      &          MassFlowRate, Quality, tkf, coef_a, coef_b, coef_c, discr
    REAL(r8k), INTENT(OUT) :: qq
    REAL(r8k), PARAMETER :: dp01 = 0.01_r8k
    REAL(r8k), PARAMETER :: dp023 = 0.023_r8k
    REAL(r8k), PARAMETER :: dp072 = 0.072_r8k
    REAL(r8k), PARAMETER :: dp1 = 0.1_r8k
    REAL(r8k), PARAMETER :: dp4 = 0.4_r8k
    REAL(r8k), PARAMETER :: dp8 = 0.8_r8k
    REAL(r8k), PARAMETER :: one = 1.0_r8k
    REAL(r8k), PARAMETER :: five = 5.0_r8k
    REAL(r8k), PARAMETER :: d400 = 400.0_r8k
    REAL(r8k), PARAMETER :: d800 = 800.0_r8k
    REAL(r8k), PARAMETER :: d1200 = 1200.0_r8k
    REAL(r8k), PARAMETER :: d1260 = 1260.0_r8k
    ! Goeneveld 5.9 coefficients
    REAL(r8k), PARAMETER :: gr9a = 0.00327_r8k
    REAL(r8k), PARAMETER :: gr9b = 0.901_r8k
    REAL(r8k), PARAMETER :: gr9c = 1.32_r8k
    REAL(r8k), PARAMETER :: gr9d = -1.50_r8k
    ! Groeneveld 5.7coefficients
    REAL(r8k), PARAMETER :: gr7a = 0.052_r8k
    REAL(r8k), PARAMETER :: gr7b = 0.688_r8k
    REAL(r8k), PARAMETER :: gr7c = 1.26_r8k
    REAL(r8k), PARAMETER :: gr7d = -1.06_r8k
    REAL(r8k), PARAMETER :: xgrmx = 0.950_r8k !This parameter is not used
    REAL(r8k), PARAMETER :: sigma = 1.73e-9_r8k
    REAL(r8k), PARAMETER :: fthun = 1500.0_r8k
    REAL(r8k), PARAMETER :: prhi = 0.8695652174_r8k
    REAL(r8k), PARAMETER :: xx = 979.2_r8k !This parameter is not used
    REAL(r8k), PARAMETER :: yy = 1180.8_r8k !This parameter is not used
    REAL(r8k), PARAMETER :: zz = 1501.2_r8k !This parameter is not used
    REAL(r8k), PARAMETER :: ftor = 459.67_r8k
    REAL(r8k), PARAMETER :: tradcb = 1800.0_r8k
    REAL(r8k), PARAMETER :: xminty = 0.05_r8k
    REAL(r8k), PARAMETER :: hmin = 0.01_r8k
    REAL(r8k), PARAMETER :: ft = 12.0_r8k
    REAL(r8k), PARAMETER :: gravc = 4.173e8_r8k
    REAL(r8k), PARAMETER :: sipr = 6894.76_r8k
    REAL(r8k), PARAMETER :: encp = 2.388458966e-4_r8k
    REAL(r8k), PARAMETER :: sisvol = 6.24279606e-2_r8k
    LOGICAL :: err, Converged
    ! Constants for vapor temperature equation
    REAL(r8k), DIMENSION(5,5), PARAMETER :: ctvs = reshape ([ -0.1179100862e5_r8k, 0.1256160907e3_r8k, -0.1083713369_r8k, &
      &                                               0.3278071846e-4_r8k, -0.3425564927e-8_r8k, 0.2829274345e2_r8k, &
      &                                              -0.3333448850_r8k, 0.2928177730e-3_r8k, -0.8970959364e-7_r8k, &
      &                                               0.9527692453e-11_r8k, -0.2678181564e-1_r8k, 0.3326901268e-3_r8k, &
      &                                              -0.2972436458e-6_r8k, 0.9246248312e-10_r8k, -0.1001409043e-13_r8k, &
      &                                               0.1218742752e-4_r8k, -0.1477890326e-6_r8k, 0.1342639113e-9_r8k, &
      &                                              -0.4249155515e-13_r8k, 0.4703914404e-17_r8k, -0.2092033147e-8_r8k, &
      &                                               0.2463258371e-10_r8k, -0.2275585718e-13_r8k, 0.7338316751e-17_r8k, &
      &                                              -0.8315044742e-21_r8k ], [5, 5])
    ! Constants for vapor temperature equation above critical pressure
    REAL(r8k), DIMENSION(5,5), PARAMETER :: ctvc = reshape ([ 0.3795256853e4_r8k, -0.3910086240e1_r8k, 0.3410500159e-4_r8k, &
      &                                              0.1527377542e-6_r8k, -0.1437179752e-10_r8k, -6.347031007_r8k, &
      &                                              0.1222747819e-1_r8k, 0.7010900113e-9_r8k, -0.5356866315e-9_r8k, &
      &                                              0.5006731336e-13_r8k, 0.2867228326e-2_r8k, -0.1404664699e-4_r8k, &
      &                                             -0.1030201866e-9_r8k, 0.6823225984e-12_r8k, -0.6365519546e-16_r8k, &
      &                                              0.5953599813e-8_r8k, 0.7505679464e-8_r8k, 0.5731099333e-14_r8k, &
      &                                             -0.3668096142e-15_r8k, 0.3473711350e-19_r8k, 0.4798207438e-10_r8k, &
      &                                             -0.1608693653e-11_r8k, 0.3720795449e-16_r8k, 0.6946004624e-19_r8k, &
      &                                             -0.6842306083e-23_r8k ], [5, 5])
    ! Constants for vapor specific volume equation
    REAL(r8k), DIMENSION(4,3), PARAMETER :: cnvg = reshape ([ -0.1403086182e4_r8k,       0.3817195017_r8k,  -0.6449501159e-4_r8k, &
      &                                               0.7823817858e-8_r8k,    0.1802594763e1_r8k,  -0.5394444747e-3_r8k, &
      &                                               0.8437637660e-7_r8k, -0.1053834646e-10_r8k,  -0.2097279215e-3_r8k, &
      &                                               0.1855203702e-6_r8k, -0.2713755001e-10_r8k,  0.3629590764e-14_r8k ], &
      &                                               [4, 3])
    ! Define reynolds #  factor table for chen correlation for nucleate boiling
    REAL(r8k), DIMENSION(30), PARAMETER :: reyn =  [ 1.07_r8k,     0.1_r8k,  1.21_r8k,  0.2_r8k,  1.42_r8k, 0.3_r8k, &
      &                                              1.63_r8k,     0.4_r8k,  2.02_r8k,  0.6_r8k,  2.75_r8k, 1.0_r8k, &
      &                                              4.30_r8k,     2.0_r8k,  5.60_r8k,  3.0_r8k,  6.75_r8k, 4.0_r8k, &
      &                                              9.10_r8k,     6.0_r8k, 12.10_r8k, 10.0_r8k, 22.00_r8k, &
      &                                              20.0_r8k,    44.7_r8k,  50.0_r8k, 76.0_r8k, 100.0_r8k, &
      &                                              200.00_r8k, 400.0_r8k ]
    ! Define suppression factor for chen correlation for nucleate boil.
    REAL(r8k), DIMENSION(26), PARAMETER :: fsup =  [ 1.000_r8k, 1.0e3_r8k, 0.893_r8k, 1.0e4_r8k, 0.793_r8k, &
      &                                              2.0e4_r8k, 0.703_r8k, 3.0e4_r8k, 0.629_r8k, 4.0e4_r8k, &
      &                                              0.513_r8k, 6.0e4_r8k, 0.375_r8k, 1.0e5_r8k, 0.213_r8k, &
      &                                              2.0e5_r8k, 0.142_r8k, 3.0e5_r8k, 0.115_r8k, 4.0e5_r8k, &
      &                                              0.093_r8k, 6.0e5_r8k, 0.083_r8k, 1.0e6_r8k, 0.000_r8k, &
      &                                              1.0e8_r8k ]
    !
    IF (ndebug) WRITE(ounit,905) Time, ih, Quality, MassFlowRate, CoolPress, j
905 FORMAT(' QDOT: Time = ',e11.4,' ih = ',i3, ' Quality = ',e11.4, &
      &    ' MassFlowRate = ',e11.4, ' CoolPress = ',e11.4,' j = ',i3)
    !
    ! jmc   Note: mode =  9 -- eliminated (new post-CHF htc correlations extended to low pressure conditions)
    ! jmc       mode = 10 -- stagnant conditions (g < 0.000002 Mlbm/hr-ft2), assume minimum htc = 5.0 Btu/hr-ft2-F
    ! jmc       mode = 11 -- adiabatic heat-up, htc = 0.0
    ! jmc       mode = 12 -- reflood, using flecht heat transfer models
    !

    SELECT CASE (ih)
    CASE (1)
        !
        ! Mode 1 -- single-phase liquid forced convection: Dittus-Boelter
        !
        tkf = thcon (n1, tbulk, r)
        ! Compute liquid viscosity using relap5 function
        pressi = CoolPress * 6.896552e3_r8k !Note: This conversion is wrong (Pa to psi). Should be 6.89475729e+3_r8k
        tbulkk = tfk(tbulk)
        rhosi = r / 0.0624279606_r8k
        tsatk = tfk(tsat)
        !
        viscsi = viscol (pressi, tbulkk, rhosi, tsatk)
        !
        vsf = viscsi * 2419.09_r8k
        prf = cp * vsf / tkf
        ref = MassFlowRate * hd / vsf
        hc = (dp023 * tkf / hd) * ref ** dp8 * prf ** dp4
        !
        ! lower limit laminar Nu = 7.86 (from Sparrow, et al.)
        !
        hlam = 0.0_r8k
        hturb = hc
        IF (ref < 2000.0_r8k) hlam = 7.86_r8k * tkf / hd
        hc = MAX(hturb, hlam)
        IF (hc < five) hc = five
        hspl = hc
        !
        ts = (b + hc * tbulk) / (hc - a)
        qq = hc * (ts - tbulk)
        hcsave = hc
    CASE (2, 3)
        !
        ! Mode 2, 3 -- subcooled and saturated nucleate boiling: Thom Correlation (nbhtc = 0)
        !                                                        Chen correlation (nbhtc = 1)
        !
        tkf = thcon (n1, tbulk, r)
        ! Compute liquid viscosity using relap5 function
        pressi = CoolPress * 6.896552e3_r8k !Note: This conversion is wrong (Pa to psi). Should be 6.89475729e+3_r8k
        tbulkk = tfk(tbulk)
        rhosi = r / 0.0624279606_r8k
        tsatk = tfk(tsat)
        !
        viscsi = viscol (pressi, tbulkk, rhosi, tsatk)
        !
        vsf = viscsi * 2419.09_r8k
        prf = cp * vsf / tkf
        ref = MassFlowRate * hd / vsf
        hc = (dp023 * tkf / hd) * ref ** dp8 * prf ** dp4
        !
        ! lower limit laminar Nu = 7.86 (from Sparrow, et al.)
        !
        hlam = 0.0_r8k
        hturb = hc
        IF (ref < 2000.0_r8k) hlam = 7.86_r8k * tkf / hd
        hc = MAX(hturb, hlam)
        IF (hc < five) hc = five
        hspl = hc
        !
        itn = 1
        !
        SELECT CASE (nbhtc)
        CASE (0)
            !
            ! Thom correlation
            !
!            Thom_ConvgLoop: DO
!                !
!                dtf = MAX(0.001_r8k, tsur - tsat)
!                fp1 = EXP(CoolPress / 1260.0_r8k) / 0.072_r8k
!                qnb = ((fp1 * dtf) ** 2)
!                hnb = qnb / (tsur - tbulk)
!                hc = MAX(hnb + hspl, five)
!                twall2 = (b + hc * tbulk) / (hc - a)
!                IF (ABS(twall2 - tsur) < 0.1_r8k) EXIT Thom_ConvgLoop
!                !
!                SELECT CASE (itn)
!                CASE (1)
!                    tin1 = twall2
!                    twgs = twall2
!                    IF (ndebug) WRITE(ounit,12944) itn, twgs, twall2, tin1, tsur, hspl, hnb, qnb
!12944               FORMAT(' QDOT for mode 2, itn = ',i5,' twgs = ',e13.6, ' twall2 = ',e13.6,' tin1 = ',e13.6, &
!                      &    ' tsur = ',e13.6,' hspl =',e13.6,' hnb =',e13.6,' qnb =',e13.6)
!                CASE (2)
!                    tin2 = twall2
!                    tout1 = tin2
!                    twgs = twall2
!                    IF (ndebug) WRITE(ounit,22944) itn, twgs, tin2, tout1
!22944               FORMAT(' QDOT for mode 2, itn = ',i5,' twgs = ',e13.6,' tin2 = ',e13.6,' tout1 = ',e13.6)
!                CASE DEFAULT
!                    ! Exit if convergence has been reached
!                    IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT Thom_ConvgLoop
!                    tout2 = twall2
!                    ! Get improved guess of surface temperature using method of newton
!                    dtidto = (tout2 - tout1) / (tin2 - tin1)
!                    twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
!                    tout1 = tout2
!                    tin1 = tin2
!                    tin2 = twgs
!                    IF (ndebug) WRITE(ounit,2944) itn, twgs, tout2, tin2, tout1, tin1, tsur
!2944                FORMAT(' QDOT for mode 2, itn = ',i5,' twgs = ',e13.6,' tout2 = ',e13.6,' tin2 = ',e13.6, &
!                      &    ' tout1 = ',e13.6,' tin1 = ',e13.6,' tsur = ',e13.6)
!                END SELECT
!                tsur = twgs
!                itn = itn + 1
!                IF (itn == 100) THEN ! Max # of iteration reached. Code execution stopped.
!                    WRITE(ounit,2277) twall2, tsur
!2277                FORMAT(////,' Iteration on cladding surface temperature in Thom_ConvgLoop for nucleate boiling mode did', &
!                      &         ' not converge. Program stopped in Subroutine: qdot',/,' twall2 = ',e13.6,' twall1 = ',e13.6)
!                    ERROR STOP ' Code execution stopped in Subroutine: qdot. No convergence found in loop: Thom_ConvgLoop'
!                ELSE
!
!                ENDIF
!            ENDDO Thom_ConvgLoop

            fp1 = EXP(CoolPress / 1260.0_r8k) / 0.072_r8k
            coef_b = (2*tsat*fp1**2 + a - hspl) / fp1**2
            coef_c = (tsat**2 * fp1**2 - hspl * tbulk - b) / fp1**2
            discr = coef_b**2 - 4 * coef_c
            if (discr < 0) then
                write(*,*) "ERROR: can not find tsur for given parameters"
                write(*,*) a, b, tsat, hspl, tbulk
                stop
            else
                tsur = (coef_b + discr**0.5) / 2
                hc = hspl + fp1**2 * (tsur - tsat)**2 / (tsur - tbulk)
                if (hc < 5.d0) then
                    hc = 5.d0
                    tsur = (b + hc * tbulk) / (hc - a)
                endif
                twall2 = tsur
                coef_a = tsur
            endif
            !
            ! Iteration converged
            !
            ts = twall2
            tsur = ts
            ! jmc
            qq = a * ts + b
            hcsave = qq / (ts - tbulk)
        CASE (1)
            !
            ! Chen correlation
            !
            ! First compute the required water properties
            !
            tkf = thcon (n1, tsat, r)
            !
            ! If liquid subcooled, use relaps viscosity Subroutine_fraptran
            !
            IF (Quality <= 0.0_r8k) THEN
                pressi = CoolPress * 6.896552e3_r8k !Note: This conversion is wrong. Should be 6.89475729e+3_r8k Pa/psi
                tbulkk = tfk(tbulk)
                rhosi = r / 0.0624279609_r8k
                tsatk = tfk (tsat)
                !
                viscsi = viscol (pressi, tbulkk, rhosi, tsatk)
                !
                vsf = viscsi * 2419.09_r8k
            ELSE
                vsf = visc (n1, tsat, r)
            ENDIF
            !
            CALL surten (tsat, sig)
            !
            prf = cpf * vsf / tkf
            ref = MassFlowRate * hd / vsf
            !
            vsg = visc (n2, tsat, rg)
            !
            ! check to see if subcooled or saturated
            !
            IF (Quality <= 0.0_r8k) THEN
                !
                ! Subcooled. reynolds number factor = 1.0
                !
                freyn = 1.0_r8k
            ELSE
                !
                ! Compute gamma for input argument to reynolds number table
                !
                xgam = MIN(0.99_r8k, Quality)
                gamma = ((xgam / (1.0_r8k - xgam)) ** 0.9_r8k) * ((rf / rg) ** 0.5_r8k) * ((vsg / vsf) ** 0.1_r8k)
                !
                IF (gamma <= 0.1_r8k) THEN
                    freyn = 1.07_r8k
                ELSEIF (gamma < 400.0_r8k) THEN
                    freyn = polate (reyn, gamma, nreyn, ireyn)
                ELSE
                    freyn = 200.0_r8k
                ENDIF
            ENDIF
            !
            ! compute suppression factor
            !
            fsupin = ref * freyn ** 1.25_r8k
            IF (fsupin <= 1.0e3_r8k) THEN
                fsupf = 1.0_r8k
            ELSEIF (fsupin < 1.0e8_r8k) THEN
                fsupf = polate (fsup, fsupin, nfsup, ifsup)
            ELSE
                fsupf = 0.0_r8k
            ENDIF
            !
            ! compute material property factors for hmic and hmac
            !
            fmic = 0.00122_r8k * (((tkf ** 0.79_r8k) * (cpf ** 0.45_r8k) * (rf ** 0.49_r8k) * &
              &    (gravc ** 0.25_r8k)) / ((sig ** 0.5_r8k) * (vsf ** 0.29_r8k) * ((hg - hf) ** 0.24_r8k) * &
              &    (rg ** 0.24_r8k))) * fsupf
            hmac = 0.023_r8k * (tkf / hd) * (prf ** 0.4_r8k) * (ref ** 0.8_r8k) * freyn
            !
            ! compute vapor pressure at cladding surface temperature
            !
            Converged = .FALSE.
            Chen_ConvgLoop: DO
                ! limit temperature to critical temperature
                tsurk = MIN(640.0_r8k, tfk(tsur))
                !
                CALL sth2x0 (tsurk, psatsi, err)
                !
                IF (err) THEN
                    WRITE(ounit,242) tsurk, psatsi
242                 FORMAT(//' Error in call to sth2x0 in qdot for nucleate boiling heat transfer. ', &
                      &      'Program Stopped: tsurk = ',e11.4,' psatsi = ',e11.4)
                    ERROR STOP ' Code stopped in Subroutine: qdot, after call to sth2x0 for nuclear boiling heat transfer.'
                ENDIF
                !
                ! convert psat from Pa to psia
                psatw = 0.000145_r8k * psatsi
                !
                ! since tsat was evaluated at pressure CoolPress,
                ! psat(tsat), dpsat has units of lbf/ft**2
                !
                dpsat = MAX(0.0_r8k, (psatw - CoolPress) * 144.0_r8k)
                dtsat = MAX(0.0_r8k, tsur - tsat)
                hmic = fmic * (dtsat ** 0.24_r8k) * (dpsat ** 0.75_r8k)
                ! Check to see if subcooled or saturated
                IF (Quality <= 0.0_r8k) THEN
                    ! Subcooled
                    twall2 = (b + hmic * tsat + hmac * tbulk) / (hmic + hmac - a)
                ELSE
                    ! Saturated
                    twall2 = (b + (hmic + hmac) * tsat) / (hmic + hmac - a)
                ENDIF
                IF (ndebug) WRITE(ounit,942) itn, tsur, twall2, hmic, hmac, dtsat
942             FORMAT(' QDOT for mode 2, itn = ',i5,' tsur = ',e13.6,' twall2 = ',e13.6,' hmic = ',e11.4, &
                  &    ' hmac = ',e11.4,' dtsat = ',e13.6)
                IF (ABS(twall2 - tsur) < 0.1_r8k) EXIT Chen_ConvgLoop
                SELECT CASE (itn)
                CASE (1)
                    tin1 = twall2
                    twgs = twall2
                CASE (2)
                    tin2 = twall2
                    tout1 = tin2
                    twgs = twall2
                CASE DEFAULT
                    IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT Chen_ConvgLoop
                    tout2 = twall2
                    ! Get improved guess of surface temperature using method of newton
                    dtidto = (tout2 - tout1) / (tin2 - tin1)
                    twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
                    tout1 = tout2
                    tin1 = tin2
                    tin2 = twgs
                    IF (ndebug) WRITE(ounit,944) itn, twgs, tout2, tin2, tout1, tin1
944                 FORMAT(' QDOT for mode 2, itn = ',i5,' twgs = ',e13.6,' tout2 = ',e13.6,' tin2 = ',e13.6, &
                      &    ' tout1 = ',e13.6,' tin1 = ',e13.6)
                END SELECT
                tsur = twgs
                itn = itn + 1
                IF (itn == 100) THEN  ! Max # of iterations reached. Force convergence
                    WRITE(ounit,277) twall2, tsur
!                    WRITE(0,277) twall2, tsur
277                 FORMAT(////,' iteration on cladding surface temperature in qdot for nucleate boiling mode did', &
                      &         'not converge. Program stopped.', /,' twall2 = ',e14.5,' twall1 = ',e12.5)
                    twall2 = 0.5_r8k * (twall2 + tsur)
                    EXIT Chen_ConvgLoop
                ENDIF
            ENDDO Chen_ConvgLoop
            
            ts = twall2
            IF (ts < tsat) ts = tsat
            qq = a * ts + b
            hcsave = qq / (ts - tbulk)
            !
        CASE DEFAULT
            WRITE (0,*) 'Error in Subroutine: qdot. Bad value for nbhtc. nbhtc = ',nbhtc
            ERROR STOP 'Error in Subroutine: qdot. Bad value for nbhtc. Execution termianted.'
        END SELECT
    CASE (4, 5, 8)
        !
        ! Mode 4: Post-CHF boiling heat transfer
        ! Mode 5: Film boiling
        ! Mode 8: Forced convection to superheated vapor
        !
        ! Notes:
        ! 1) film boiling must be calculated first, even when in transition boiling region, as most transition
        !    correlations need film boiling heat flux, heat transfer coefficient, or onset of stable film
        !    boiling temperature
        ! 2) film boiling must be calculated for mode 8, to ensure proper transition from film boiling to
        !    single-phase convection to steam
        !
        !   mode 5 -- film boiling is needed for mode 4
        !
        ! correlation selection based on user input
        ! current options: j = jfb (from input)
        !                 = 0 -- Groeneveld 5.9 (default)
        !                 = 1 -- Groeneveld 5.7 
        !                 = 2 -- Bishop-Sandberg-Tong
        !                 = 3 -- Groeneveld-Delorme
        SELECT CASE (j)
        CASE (0, 1)
            !
            ! Final correlation based on value of j
            ! 0: Groeneveld correlation (default, 5.9 formulation)
            ! 1: Groeneveld correlation (default, 5.7 formulation)
            !
            xsave = Quality
            IF (Quality < dp1) Quality = dp1
            yyy = one - dp1 * ((one - Quality) * (rf / rg - one)) ** dp4
            IF (yyy < dp1) yyy = dp1
            tp1 = Quality + (one - Quality) * rg / rf
            Quality = xsave
            !
            tkg = thcon (n2, tbulk, rg)
            !
            vsg = visc (n2, tbulk, rg)
            ! Prandtl no. eval. at wall temp.
            IF (tsur <= fthun) THEN
                tk = thcon (n2, tsur, r)
                vs = visc (n2, tsur, r)
                prw = cp * vs / tk
            ELSE
                prw = prhi
            ENDIF
            !
            ! jmc (this is incorrect; vs should be at Tsat, not Twall)
            ! reg = MassFlowRate * hd / vs
            reg = MassFlowRate * hd / vsg 
            !
            IF (j == 0) THEN
                hc = (gr9a * tkg / hd) * (reg * tp1) ** gr9b * prw ** gr9c * yyy ** gr9d
            ELSE
                hc = (gr7a * tkg / hd) * (reg * tp1) ** gr7b * prw ** gr7c * yyy ** gr7d
            END IF
            !
            IF (hc < five) hc = five
            hgroen = hc
            hfilmb = hc
            IF (ih == 5) THEN
                ts = (b + hc * tsat) / (hc - a)
                qq = hc * (ts - tsat)
                qfb = qq
                RETURN
            END IF
        CASE (2)
            !
            ! Bishop-Sandberg-Tong film boiling correlation
            ! jmc
            xsave = Quality
            IF (Quality < dp1) Quality = dp1
            tp1 = Quality + (one - Quality) * rg / rf
            !
            tfilm = 0.5_r8k * (tsur + tsat)
            ! jmc
            ! Density and specific heat of vapor at film temperature
            tfilmk = MIN(1800.0_r8k, tfk(tfilm))
            prop(1) = tfilmk
            prop(2) = CoolPress * sipr
            !
            CALL sth2x3 (aasth, prop, itn, err)
            !
            IF (err) THEN
                WRITE(ounit,1461)
1461            FORMAT(///,' failure in Call to sth2x3 from qdot for heat transfer mode 4')
                WRITE(ounit,1468) tfilm, CoolPress
1468            FORMAT(' tfilm = ',e11.4,' CoolPress = ',e11.4)
                CALL fabend
            ENDIF
            ! cpfilm = specific heat of vapor at film temperature (btu/lb-F)
            cpfilm = prop(8) * encp
            ! rfilm = vapor density at film temperature    (lb/ft**3)
            rfilm = sisvol / prop(3)
            ! thermal conductivity of vapor at film temperature
            tkfilm = thcon (n2, tfilm, r)
            ! convert units on tkfilm from Btu/hr-ft-F to Btu/sec-ft-F
            tkfv = tkfilm / sechr
            ! viscosity of vapor phase at film temperature
            vsfilm = visc (n2, tfilm, r)
            ! jmc
            rfilm = MIN(rg, rfilm)
            !
            prfilm = cpfilm * vsfilm / tkfilm
            refilm = MassFlowRate * hd / vsfilm
            !
            ! calculate local equilibrium void fraction and bulk density 
            !
            alfa = xsave / tp1
            rb = rg * alfa + (1.0_r8k - alfa) * rf
            hcv = (0.0193_r8k * tkfv / hd) * ((refilm) ** dp8) * (prfilm ** 1.23_r8k) * &
              &   ((rg / rb) ** 0.68_r8k) * (rg / rf) ** 0.068_r8k
            hc = MAX(hcv * sechr, five)
            hfilmb = hc
            IF (ih == 5) THEN
                ts = (b + hc * tsat) / (hc - a)
                qq = hc * (ts - tsat)
                qfb = qq
                RETURN
            END IF
        CASE (3)
            !
            ! Groeneveld-Delorme film boiling correlation
            !
            CALL voidratio (Quality, rf, rg, hd * ft, sr, alpha)
            xsave = Quality
            hfg = hg - hf
            tkg = thcon (n2, tsat, rg)
            !
            ! convert units on tkg from Btu/hr-ft-F to Btu/sec-ft-F
            !
            tkgv = tkg / sechr
            vsg = visc (n2, tsat, rg)
            rehom = xsave * MassFlowRate * hd / (vsg * alpha)
            prg = cpg * vsg / tkg
            !
            tfilm = 0.5_r8k * (tsur + tsat)
            ! jmc
            ! density and specific heat of vapor at film temperature
            !
            tfilmk = MIN(1800.0_r8k, tfk(tfilm))
            prop(1) = tfilmk
            prop(2) = CoolPress * sipr
            !
            CALL sth2x3 (aasth, prop, itn, err)
            !
            IF (err) THEN
                WRITE(ounit,2461)
2461            FORMAT(///,' failure in Call to sth2x3 from qdot for heat transfer mode 4 ')
                WRITE(ounit,2465) tfilm, CoolPress
2465            FORMAT(' tfilm = ',e11.4,' CoolPress = ',e11.4)
                CALL fabEnd
            ENDIF
            ! cpfilm = specific heat of vapor at film temperature (btu/lb-F)
            cpfilm = prop(8) * encp
            ! rfilm = vapor density at film temperature (lb/ft**3)
            rfilm = sisvol / prop(3)
            ! thermal conductivity of vapor at film temperature
            tkfilm = thcon (n2, twall, r)
            ! viscosity of vapor phase at film temperature
            vsfilm = visc (n2, twall, r)
            ! jmc
            rfilm = MIN(rg, rfilm)
            ! jmc
            prfilm = cpfilm * vsfilm / tkfilm
            a1 = 0.13864_r8k * (prg ** 0.2031_r8k) * (rehom ** 0.20006_r8k) / (fluxk * hd * cpg / &
              &  (tkg * (hg - hf))) ** 0.09232_r8k
            a1 = a1 * (1.3072_r8k - 1.0833_r8k * xsave + 0.8455_r8k * xsave ** 2)
            a1 = MAX(0.0_r8k, MIN(1.55_r8k, a1))
            hvap = hg + hfg * EXP(-TAN(a1)) * EXP(-1.0_r8k / (3.0_r8k * alpha) ** 4)
            hvap = MIN(hvap, 1800.0_r8k)
            xvap = hfg * xsave / (hvap - hf)
            ! jmc
            !
            ! vapor specific volume at superheated vapor enthalpy (hvap)
            ! (from EPRI water properties functions: vapor specific volume as a function of pressure and enthalpy)
            !
            px = CoolPress
            hx = hvap
            vsv = cnvg(1,3) / px + cnvg(2,3) + px * (cnvg(3,3) + cnvg(4,3) * px)
            DO j = 1, 2
                k = 3 - j
                vsv = vsv * hx + cnvg(1,k) / px + cnvg(2,k) + px * (cnvg(3,k) + cnvg(4,k) * px)
            ENDDO
            vvap = vsv
            IF (px <= 3208.2_r8k) THEN
                !
                ! Superheated vapor temperature at superheated vapor enthalpy (hvap)
                ! (from EPRI water properties functions: vapor temperature as a function of pressure and enthalpy)
                vsv = ctvs(1,5) + px * (ctvs(2,5) + px * (ctvs(3,5) + px * (ctvs(4,5) + px * ctvs(5,5))))
                DO j = 1, 4
                    k = 5 - j
                    vsv = vsv * hx + ctvs(1,k) + px * (ctvs(2,k) + px * (ctvs(3,k) + px * (ctvs(4,k) + px * ctvs(5,k))))
                ENDDO
                tvap = vsv
            ELSE
                !
                ! Supercritical vapor
                vsv = ctvc(1,5) + px * (ctvc(2,5) + px * (ctvc(3,5) + px * (ctvc(4,5) + px * ctvc(5,5))))
                DO j = 1, 4
                    k = 5 - j
                    vsv = vsv * hx + ctvc(1,k) + px * (ctvc(2,k) + px * (ctvc(3,k) + px * (ctvc(4,k) + px * ctvc(5,k))))
                ENDDO
            ENDIF
            tvap = MIN(tvap, tsur - 0.1_r8k)
            rvap = 1.0_r8k / vvap
            gdnu = 0.008348_r8k * ((MassFlowRate * hd / vsfilm) * (xvap + (one - xvap) * rvap / rf)) ** 0.8774_r8k * &
              &    (prfilm ** 0.6112_r8k)
            !
            hc = MAX((tkfilm / hd) * gdnu, five)
            hfilmb = hc
            IF (ndebug) WRITE(ounit,10330) hf, hg, hvap, tvap, rvap, xvap
10330       FORMAT(' Gr-D in QDOT: hf = ',e12.4,' hg = ',e12.4,' hvap = ',e12.4,' tvap = ',e12.4,' rvap = ',e12.4, &
              &    ' xvap = ',e12.4)
            IF (ih == 5) THEN
                ts = (b + hc * tsat) / (hc - a)
                qq = hc * (ts - tsat)
                qfb = qq
                RETURN
            END IF
        CASE DEFAULT
            WRITE (0, 5251) j
            WRITE (ounit, 5251) j
5251        FORMAT ('Code execution stopped in subroutine qdot for heat transfer mode 4. ', &
              &     'Wrong value for j called. j = ',i3)
            ERROR STOP 'Code execution stopped in subroutine qdot. Bad value supplied for j in heat transfer mode 4.'
        END SELECT
        ! Limit transition boiling region to range of wall temperatures
        ! based on reasonable range of expected minimum film boiling temperature.
        ! If current wall surface temperature is above 900 F, assume film boiling
        !
        IF (ih == 8) THEN
            !
            !  mode 8 -- forced convection to superheated vapor
            !
            tk = thcon (n2, tbulk, r)
            !
            vs = visc (n2, tbulk, r)
            !
            prn = cp * vs / tk
            IF (tbulk > fthun) prn = prhi
            re = MassFlowRate * hd / vs
            hspv = (dp023 * tk / hd) * re ** dp8 * prn ** dp4
            IF (hspv < hmin) hspv = hmin
            hc = MIN(hspv, hfilmb)
            ! Ken  hc=hspv
            !
            ! check for radiation switch (for radiation to shroud)
            ! IP - 4/21/2015
            IF (Radiation == 'ON') THEN
                ! radiation switch turned on
                ! This model assumes fuel rod is surrounded by zircaloy annular flow shroud with inside wall temperature of tbulk
                ! Radiation is assumed negligible if dh > 0.9 * hd
                rdtest = 0.90_r8k * hd
                IF (dh < rdtest) THEN
                    alpha = 1.0_r8k
                    ! Assume maximum shroud surface temp. = tbulk
                    tshrd = tbulk
                    toxshr = 2.5e-7_r8k
                    !
                    CALL emssf1 (tshrd, alpha, toxshr, fe)
                    !
                    rk = fe * sigma
                    c1 = (hc - a) / rk
                    tts = tfr(tbulk) ** 2
                    c2 = - (tts ** 2 + (hc * tfr(tbulk) + b - ftor * a) / rk)
                    !
                    CALL root1 (c1, c2, tfr(tsur), ts)
                    !
                    ts = ts - ftor
                    qq = a * ts + b
                ENDIF
            ELSE ! Radiation is turned off
                ts = (b + hc * tbulk) / (hc - a)
                qq = hc * (ts - tbulk)
            ENDIF
            RETURN
        END IF
        !
        IF (tsur > 900.0_r8k) THEN
            IF (ndebug) WRITE(ounit,999) ts, qq
            RETURN
        ENDIF
        ! correlation selection based on user input
        ! current options:
        !  jtr (input flag) = 0 -- modified Tong-Young (default) (?)
        !                   = 1 -- modified Condie-Bengston
        !                   = 2 -- Bjornard-Griffith
        SELECT CASE (jtr)
        CASE (0)
            !
            ! Modified Tong-Young
            !
            xe = MAX(xminty, Quality)
            ! convert chf to Btu/sec-ft2 and film boiling htc to Btu/sec-ft2-F
            qchfv = qcrit / sechr
            hfbv = hfilmb / sechr
            ! thermal conductivity of saturated vapor
            tkg = thcon (n2, tsat, rg)
            ! viscosity of saturated vapor
            vsg = visc (n2, tsat, rg)
            ! compute reynolds number of saturated vapor
            reg = MassFlowRate * hd / vsg
            ! calculate wall superheat at critical heat flux temperature
            IF (tchf < tsat) tchf = tsat + 0.01_r8k
            dtchf = tchf - tsat
            ! calculate film boiling heat flux at the critical heat flux temperature
            qfbv = MIN(qchfv - 0.001_r8k, hfbv * dtchf)
            !
            twall = tsur
            !
            itn = 1
            TongYoung_ConvgLoop: DO
                !
                ! jmc    IF (twall < (tsat + 1.0_r8k)) twall = tsat + 1.0_r8k
                IF (twall < tchf) twall = tchf + 1.0_r8k
                dtsat = twall - tsat
                !
                qcty = a * twall + b
                fluxn = qcty / sechr
                !
                cty1 = 0.666666667_r8k
                cty2 = 1.0_r8k + 0.0016_r8k * dtsat
                !
                a1 = 0.012_r8k * xe ** cty1 * (hd * (MassFlowRate / sechr) * (hg - hf) / (4.0_r8k * fluxn)) * &
                  &  (0.01_r8k * dtsat) ** cty2
                ! jmc  restrict a1 to maximum value of 15, minimum value of 0
                !
                a1 = MAX(0.0_r8k, MIN(a1, 15.0_r8k))
                ! jmc
                qtbv = (qchfv - qfbv) * EXP(-a1)
                ! jmc
                ! transition boiling heat flux cannot exceed critical heat flux (by definition)
                !
                qtbv = MIN(qtbv, qchfv)
                ! jmc
                hcmin = five / sechr
                qtbv = MAX(qtbv, hcmin * (twall - tbulk))
                !
                qtb = qtbv * sechr
                htranb = qtb / (twall - tbulk)
                !
                hpchf = htranb + hfilmb
                !
                twall2 = (b + hpchf * tbulk) / (hpchf - a)
                !
                IF (ndebug) WRITE(ounit,981) itn, twall, twall2, tin1, tin2, tout2, qcrit, a1, qtb
981             FORMAT(' QDOT, itn = ',i5,' twall = ',e13.6,' twall2 = ',e13.6,' tin1 = ',e13.6, &
                  &    ' tin2 = ',e13.6,' tout2 = ',e13.6,' qcrit = ',e11.4,' a1 = ',e11.4,' qtb = ',e11.4)
                ! jmc IF (twall2 < (tsat + 1.0_r8k)) twall2 = tsat + 1.0_r8k
                IF (twall2 < tchf) twall2 = tchf + 1.0_r8k
                SELECT CASE (itn)
                CASE (1)
                    tin1 = twall2
                    twgs = twall2
                CASE (2)
                    tin2 = twall2
                    tout1 = tin2
                    twgs = twall2
                CASE DEFAULT
                    IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT TongYoung_ConvgLoop
                    tout2 = twall
                    ! jmc IF (ABS(tout2 - tin2) < 1.0_r8k) EXIT TongYoung_ConvgLoop
                    IF (ABS(tout2 - tin2) < 0.10_r8k) EXIT TongYoung_ConvgLoop
                    tout2 = twall2
                    ! Get improved guess of twall using method of newton
                    dtidto = (tout2 - tout1) / (tin2 - tin1)
                    twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
                    ! jmc IF (twgs < (tsat + 1.0_r8k)) twgs = tsat + 1.0_r8k
                    IF (twgs < (tchf + 1.0_r8k)) twgs = tchf + 1.0_r8k
                    tout1 = tout2
                    tin1 = tin2
                    tin2 = twgs
                    IF (ndebug) WRITE(ounit,983) itn, twgs, tout2, tin2, tout1, tin1, Time
983                 FORMAT(' itn = ',i5,' twgs =',e13.6,' tout2 =',e13.6,' tin2 = ',e13.6,' tout1 = ',e13.6, &
                      &    ' tin1 = ',e13.6,' Time = ',e13.6)
                END SELECT
                twall = twgs
                itn = itn + 1
                IF (itn == 100) THEN  ! Max # of iterations reached. Force convergence
                    WRITE(ounit,477) itn, twall2, tin1
477                 FORMAT(///' iteration on wall temperature in qdot with modified tong-young for transition ', &
                      &       'boiling. Program stopped.',/,' itn=',i5,' twall2 = ',e12.5,' twall = ',e12.5)
                    IF (twall < twall2) twall2 = twall
                    EXIT TongYoung_ConvgLoop
                ENDIF
            END DO TongYoung_ConvgLoop
            !
            ts = twall2
            ! jmc IF (ts < tsat) ts = tsat
            IF (ts < tchf) ts = tchf
            qq = a * ts + b
            ! calculate film boiling heat flux at the new ts
            qqfb = hfilmb * (ts - tbulk)
            RETURN
        CASE (1)
            !
            ! Modified Condie-Bengston correlation
            !
            ! Convert chf and film boiling htc to Btu/sec-ft2 and Btu/sec-ft2-F
            qchfv = qcrit / sechr
            hfbv = hfilmb / sechr
            !
            ! Calculate critical heat flux temperature (from nucleate boiling htc)
            !
            dtchf = tchf - tsat
            IF (tchf < tsat) THEN
               tchf = tsat + 0.01_r8k
               dtchf = tchf - tsat
            ENDIF
            !
            twall = tsur
            !
            itn = 1
            !
            CondieBengston_ConvgLoop: DO
                
                IF (twall < (tsat + 1.0_r8k)) twall = tsat + 1.0_r8k
                dtsat = twall - tsat
                ! jmc
                qfbv = MIN(hfbv * dtchf, qchfv - 0.001_r8k)
                a1 = EXP(LOG(qchfv - qfbv) + 0.5_r8k * SQRT(dtchf) - LOG(dtchf))
                htbv = a1 * EXP(-SQRT(dtf) / 2.0_r8k)
                hc = htbv * sechr
                hpchf = hc + hfilmb
                !
                twall2 = (b + hpchf * tbulk) / (hpchf - a)
                !
                IF (ndebug) WRITE(ounit,3981) itn, twall, twall2, tin1, tin2, tout2, qcrit, a1, qtb
3981            FORMAT(' QDOT, itn = ',i5,' twall = ',e13.6,' twall2 = ',e13.6,' tin1 = ',e13.6, &
                  &    ' tin2 = ',e13.6,' tout2 = ',e13.6,' qcrit = ',e11.4,' a1 = ',e11.4,' qtb = ',e11.4)
                ! jmc IF (twall2 < (tsat + 1.0_r8k)) twall2 = tsat + 1.0_r8k
                IF (twall2 < tchf) twall2 = tchf + 1.0_r8k
                SELECT CASE (itn)
                CASE (1)
                    tin1 = twall2
                    twgs = twall2
                CASE (2)
                    tin2 = twall2
                    tout1 = tin2
                    twgs = twall2
                CASE DEFAULT
                    IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT CondieBengston_ConvgLoop
                    tout2 = twall2
                    ! jmc IF (ABS(tout2 - tin2) < 1.0_r8k) EXIT CondieBengston_ConvgLoop
                    IF (ABS(tout2 - tin2) < 0.10_r8k) EXIT CondieBengston_ConvgLoop
                    ! Get improved guess of twall using method of newton
                    dtidto = (tout2 - tout1) / (tin2 - tin1)
                    twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
                    ! jmc IF (twgs < (tsat + 1.0_r8k)) twgs = tsat + 1.0_r8k
                    IF (twgs < (tchf + 1.0_r8k)) twgs = tchf + 1.0_r8k
                    tout1 = tout2
                    tin1  = tin2
                    tin2  = twgs
                    IF (ndebug) WRITE(ounit,3983) itn, twgs, tout2, tin2, tout1, tin1, Time
3983                FORMAT(' itn = ',i5,' twgs =',e13.6,' tout2 =',e13.6,' tin2 = ',e13.6,' tout1 = ',e13.6, &
                      &    ' tin1 = ',e13.6,' Time = ',e13.6)
                END SELECT
                twall = twgs
                itn = itn + 1
                IF (itn == 100) THEN ! Max # of iterations reached. Force convergence
                    WRITE(ounit,3477) itn, twall2, tin1
3477                FORMAT(///' iteration on wall temperature in qdot with modified Condie-Bengston for transition', &
                      &       ' boiling. program stopped.',/,' itn=',i5,' twall2 = ',e13.6,' twall = ',e13.6)
                    IF (twall < twall2) twall2 = twall
                    EXIT CondieBengston_ConvgLoop
                ENDIF
            END DO CondieBengston_ConvgLoop
            !
            ts = MAX(tchf, twall2)
            qq = a * ts + b
            ! calculate film boiling heat flux at the new ts
            qqfb = hfilmb * (ts - tbulk)
            RETURN
        CASE (2)
            !
            ! Bjornard-Griffith (interpolation from critical heat flux temperature (tchf) to minimum
            ! film boiling temperature (tmin); uses Iloeje correlation to define minimum film boiling temperature
            !
            ! sig = surface tension
            CALL surten (tsat, sig)
            !
            ! Density and specific heat of vapor at film temperature
            twall = tsur
            !
            itn = 1
            BjornardGriffith_ConvgLoop: DO
                !
                tfilm = (twall + tsat) / 2.0_r8k
                dtsat = twall - tsat
                !
                tfilmk = MIN(1800.0_r8k, tfk(tfilm))
                prop(1) = tfilmk
                prop(2) = CoolPress * sipr
                !
                CALL sth2x3 (aasth, prop, it, err)
                !
                IF (err) THEN
                    WRITE(ounit,3461)
3461                FORMAT(///,' failure in Call to sth2x3 from qdot ')
                    WRITE(ounit,3468) tfilm, CoolPress
3468                FORMAT(' tfilm = ',e11.4,' CoolPress = ',e11.4)
                    CALL fabend
                ENDIF
                ! cpfilm = specific heat of vapor at film temperature (btu/lb-F)
                cpfilm = prop(8) * encp
                ! rfilm = vapor density at film temperature    (lb/ft**3)
                rfilm = sisvol / prop(3)
                ! thermal conductivity of vapor at film temperature
                tkfilm = thcon (n2, twall, r)
                ! convert units on thermal conductivity from Btu/hr-ft-F to Btu/sec-ft-F
                tkfv = tkfilm / sechr
                ! viscosity of vapor phase at film temperature
                vsfilm = visc (n2, twall, r)
                ! convert units on viscosity from lbm/hr-ft to lbm/sec-ft
                vsfilm = vsfilm / sechr
                ! jmc
                rfilm = MIN(rg, rfilm)
                ! jmc
                rfg = rf - rg
                !
                !  gravc is used mainly as force-to-mass units conversion, and is in lbm-ft/(lbf-hr^2); 
                !  for dtmina, gravc is gravitational acceleration, so units are simply ft/hr^2
                !  convert dummy variable to ft/sec^2
                !
                gcdumy = gravc / (sechr ** 2)
                !
                ! jmc
                !  convert chf and film boiling htc to Btu/sec-ft2 and Btu/sec-ft2-F
                !
                qchfv = qcrit / sechr
                hfbv = hfilmb / sechr
                !
                xe = Quality
                !
                dtmina = 0.127_r8k * (rfilm * (hg - hf) / tkfv) * (gcdumy * rfg / (rf + rg)) ** 0.6666667_r8k * &
                  &      SQRT(sig / rfg) * (vsfilm / (gcdumy * rfg)) ** 0.3333333_r8k
                dtmina = MIN(dtmina, 705.3_r8k - tsat)
                dtmina = 0.29_r8k * dtmina * (1.0_r8k - 0.295_r8k * xe ** 2.45_r8k) * (1.0_r8k + (0.360_r8k * &
                  &      ABS(MassFlowRate) / sechr) ** 0.49_r8k)
                tmin = MAX(710.0_r8k, MIN(1000.0_r8k, dtmina + tsat))
                IF (twall > tmin) THEN
                    hc = hfilmb
                    ts = twall
                    qq = a * ts + b
                    !
                    IF (ndebug) WRITE(ounit,999) ts, qq
                    RETURN
                ENDIF
                qmin = hfbv * (tmin - tsat)
                delta = ((tmin - twall) / (tmin - tchf)) ** 2
                delta = MIN(1.0_r8k, delta)
                qtbv = delta * qchfv + (1.0_r8k - delta) * qmin
                qtb = qtbv * sechr
                htranb = qtb / dtsat
                hc = htranb
                !
                hpchf = hc + hfilmb
                !
                twall2 = (b + hpchf * tbulk) / (hpchf - a)
                !
                qmindb = qmin * sechr
                !
                IF (ndebug) WRITE(ounit,4982) itn, twall, twall2, tin1, tin2, tout2
4982            FORMAT(' QDOT, itn = ',i5,' twall = ',e13.6,' twall2 = ',e13.6,' tin1 = ',e13.6, &
                  &     ' tin2 = ',e13.6,' tout2 = ',e13.6)
                ! jmc    IF (twall2 < (tsat + 1.0_r8k)) twall2 = tsat + 1.0_r8k
                IF (twall2 < tchf) twall2 = tchf + 1.0_r8k
                SELECT CASE (itn)
                CASE (1)
                    tin1 = twall2
                    twgs = twall2
                CASE (2)
                    tin2 = twall2
                    tout1 = tin2
                    twgs = twall2
                CASE DEFAULT
                    IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT BjornardGriffith_ConvgLoop
                    tout2 = twall2
                    IF (ABS(tout2 - tin2) < 1.0_r8k) EXIT BjornardGriffith_ConvgLoop
                    ! Get improved guess of twall using method of newton
                    dtidto = (tout2 - tout1) / (tin2 - tin1)
                    twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
                    ! jmc IF (twgs < (tsat + 1.0_r8k)) twgs = tsat + 1.0_r8k
                    IF (twgs < (tchf + 1.0_r8k)) twgs = tchf + 1.0_r8k
                    tout1 = tout2
                    tin1 = tin2
                    tin2 = twgs
                    IF (ndebug) WRITE(ounit,4983) itn, twgs, tout2, tin2, tout1, tin1, Time
4983                FORMAT(' itn = ',i5,' twgs =',e13.6,' tout2 =',e13.6,' tin2 = ',e13.6,' tout1 = ',e13.6, &
                      &    ' tin1 = ',e13.6,' Time = ',e13.6)
                END SELECT
                twall = twgs
                itn = itn + 1
                IF (itn == 100) THEN ! Max # of iterations reached. Force convergence
                    WRITE(ounit,4477) itn, twall2, tin1
4477                FORMAT(///' iteration on wall temperature in qdot with Bjornard-GrIffith interpolation for ', &
                      &        'transition boiling. program stopped.',/,' itn=',i5,' twall2 = ',e13.6,' twall = ',e13.6)
                    IF (twall < twall2) twall2 = twall
                    EXIT BjornardGriffith_ConvgLoop
                ENDIF
            END DO BjornardGriffith_ConvgLoop
            !
            ts = twall2
            ! jmc IF (ts < tsat) ts = tsat
            IF (ts < tchf) ts = tchf
            qq = a * ts + b
            ! calculate film boiling heat flux at the new ts
            qqfb = hfilmb * (ts - tbulk)
            RETURN
        CASE DEFAULT
            WRITE (0, 5252) jtr
            WRITE (ounit, 5252) jtr
5252        FORMAT ('Code execution stopped in subroutine qdot for heat transfer mode 4. ', &
              &     'Wrong value for jtr called. jtr = ',i3)
            ERROR STOP 'Code execution stopped in Subroutine: qdot. Bad value for (jtr) in heat transfer mode 4.'
        END SELECT
    CASE (6, 7)
        !
        ! Mode 6 or 7 -- low flow post-CHF boiling (G < 0.2 Mlbm/hr-ft2): Modified Bromley correlation
        !
        CALL voidratio (Quality, rf, rg, hd * ft, sr, alpha)
        !
        itn = 1
        !
        Mode6_ConvgLoop: DO
            !
            IF (tsur < (tsat + 1.0_r8k)) tsur = tsat + 1.0_r8k
            tfilm = 0.5_r8k * (tsur + tsat)
            !
            vsfilm = visc (n2, tfilm, rg)
            !
            vsg = visc (n2, tsat , rg)
            !
            tkfilm = thcon (n2, tfilm, rg)
            !
            prfilm = cp * vsfilm / tkfilm
            dtsat1 = MAX(0.0_r8k, tsur - tsat)
            ! jmc
            ! sig = surface tension
            CALL surten (tsat, sig)
            ! jmc
            ! Density and specific heat of vapor at film temperature
            tfilmk = MIN(1800.0_r8k, tfk(tfilm))
            prop(1) = tfilmk
            prop(2) = CoolPress * sipr
            !
            CALL sth2x3 (aasth, prop, itn, err)
            !
            IF (err) THEN
                WRITE(ounit,461)
461             FORMAT(///,' failure in call to sth2x3 from qdot ')
                WRITE(ounit,468) tfilm, CoolPress
468             FORMAT(' tfilm = ',e11.4,' CoolPress = ',e11.4)
                CALL fabend
            ENDIF
            ! cpfilm = specific heat of vapor at film temperature (btu/lb-F)
            cpfilm = prop(8) * encp
            ! rfilm = vapor density at film temperature (lb/ft**3)
            rfilm = sisvol / prop(3)
            ! thermal conductivity of vapor at film temperature
            tkfilm = thcon (n2, twall, r)
            ! viscosity of vapor phase at film temperature
            vsfilm = visc (n2, twall, r)
            ! jmc
            rfilm = MIN(rg, rfilm)
            ! jmc
            al = 2.0_r8k * pi * SQRT(sig / (rf - rfilm))
            ! tkg = thermal conductivity of vapor phase
            tkg = thcon (n2, tsat, rg)
            !
            hbrom = 0.62_r8k * ((hd / al) ** 0.172_r8k) * ((((tkfilm ** 3) * rfilm * (rf - rfilm) * &
              &     (hg - hf) * gravc) / (hd * vsfilm * dtsat1)) ** 0.25_r8k)
            !
            prg = cpg * vsg / tkg
            reg = MassFlowRate * hd / vsg
            hspv = (tkg / hd) * 0.023_r8k * (reg ** dp8) * (prg ** dp4)
            hspv = MAX(hspv, five)
            hc = MAX(hspv, hbrom)
            !
            qtot = hc * dtsat1
            IF (ndebug) WRITE(ounit,30333) itn,tsur,tchf,hspv,hbrom,qtot
30333       FORMAT(' Bromley in QDOT: itn = ',i5,' tsur = ',e12.4,' tchf = ',e12.4,' hspv = ',e12.4, &
              &    ' hbrom = ',e12.4,' qtot = ', e12.4)
            ! From conduction equation, a*tsur + b = qtot
            ! Solve above equation for new surface temperature
            IF (dtsat1 < 0.1_r8k) dtsat1 = 0.1_r8k
            htot = qtot / dtsat1
            twall2 = (b + htot * tsat) / (htot - a)
            IF ((ABS(twall2 - tsur) < 1.0_r8k) .OR. (twall2 < (tsat + 2.0_r8k))) EXIT Mode6_ConvgLoop
            SELECT CASE (itn)
            CASE (1)
                tin1 = twall2
                twgs = twall2
            CASE (2)
                tin2 = twall2
                tout1 = tin2
                twgs = twall2
            CASE DEFAULT
                IF (ABS(tin2 - tin1) < 1.0e-5_r8k) EXIT Mode6_ConvgLoop
                tout2 = twall2
                ! Get improved guess of surface temperature using method of newton
                dtidto = (tout2 - tout1) / (tin2 - tin1)
                twgs = (tout1 - dtidto * tin1) / (1.0_r8k - dtidto)
                tout1 = tout2
                tin1 = tin2
                tin2 = twgs
                IF (ndebug) WRITE(ounit,987) itn, twgs, tout2, tin2, tout1, tin1
987             FORMAT(' itn = ',i4,' twgs = ',e11.4,' tout2 = ',e13.6,' tin2 = ',e13.6,' tout1 = ',e13.6,' tin1 = ',e13.6)
            END SELECT
            tsur = twgs
            itn = itn + 1
            IF (itn == 100) THEN ! Max # of iterations reached. Force convergence
                WRITE(ounit,649) twall2, tsur
649             FORMAT(///' iteration on cladding surface temperature in qdot with modified Bromley htc in low flow film', &
                  &       ' boiling regime (mode = 6) did not converge.',/,'   twall2= ',e11.4, ' twall1= ',e11.4)
                WRITE(ounit,647) Time
647             FORMAT(' Time = ',e13.6)
                WRITE(ounit,648)
648             FORMAT(' temperature set to upper bound value ')
                IF (tsur > twall2) twall2 = tsur
                EXIT Mode6_ConvgLoop
            ENDIF
        ENDDO Mode6_ConvgLoop
        !
        ts = twall2
        IF (ts < tsat) ts = tsat
        qq = a * ts + b
        l = 2
    CASE DEFAULT
        !
        ! Wrong value for ih was chosen.  Code exeuction terminating.
        !
!        WRITE(0,901) ih
        WRITE(ounit,901) ih
901     FORMAT ('Wrong value for ih chosen. Execution terminated in qdot. ih = ',i3)
        ERROR STOP 'Code execution stopped in Subroutine: qdot. Wrong value for ih chosen.'
    END SELECT
    
    IF (ndebug) WRITE(ounit,999) ts, qq
999 FORMAT(' QDOT: surface temperature = ',e11.4,' surface heat flux = ',e11.4)
    !
    END SUBROUTINE qdot
    !
    !
    SUBROUTINE root1 (c1, c2, x1, x2)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes real root of function f(x)=x**4+c1*x+c2. Newton's second order method used.
    !> Method referenced on page 6 of numerical methods in engineering by salvadori
    !
    INTEGER(ipk) :: n
    INTEGER(ipk), PARAMETER :: itmax = 100
    REAL(r8k) :: x, am1, am2, f, fp, fpp, dxi
    REAL(r8k), INTENT(IN) :: c1, c2, x1
    REAL(r8k), INTENT(OUT) :: x2
    REAL(r8k), PARAMETER :: d2 = 2.0_r8k
    REAL(r8k), PARAMETER :: d4 = 4.0_r8k
    REAL(r8k), PARAMETER :: d12 = 12.0_r8k
    REAL(r8k), PARAMETER :: eps = 1.0e7_r8k
    REAL(r8k), PARAMETER :: fmult = 0.5e5_r8k
    !
    n = 0
    x = x1
    am1 = x ** 2
    am1 = am1 ** 2
    am2 = ABS(c1 * x)
    IF (am1 * fmult <= am2) x = -c2 / c1
    DO WHILE (n < itmax)
        n = n + 1
        x2 = x
        am1 = x ** 2
        f = am1 ** 2 + c1 * x + c2
        IF (ABS(f) < eps) RETURN
        ! Evaluate first and second derivatives of f(x)
        fp = d4 * am1 * x + c1
        fpp = d12 * am1
        dxi = -fp / f + (fpp / d2) / fp
        x = x + 1.0_r8k / dxi
    ENDDO
    ! This is only performed if root not found.
    WRITE (ounit,45) f, x2, n
45  FORMAT(//,' root finding scheme in Subroutine: root1 did not converge',/,' f2= ',e11.4,3x,'x2= ',e11.4,3x,'itmax= ',i4)
    !
    END SUBROUTINE root1
    !
    !
    SUBROUTINE gaphtc (gpthki, rf, pfc, tg, tf, tc, pg, GasFraction, flux, tflux, rufc, ruff, frden, &
      &                coldw, zro, fotmtl, tempcm, modfd, hgapt, gadolin, bulocal, gapmin, node)
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : sechr, tfk, tfr
    USE variables_fraptran, ONLY : ounit, Time, ndebug
    USE phypro_h_fraptran
    USE emssf_fraptran, ONLY : emssf2
    USE Material_Properties_fraptran, ONLY : MatProperty
    USE NCGases_fraptran, ONLY : ngases, ncGasProperties
    IMPLICIT NONE
    !>@brief
    !> Model for gap conductance computed by this subroutine is described in report BNWL-1894
    !
    ! Input
    !
    ! coldw  - cladding cold work
    ! flux   - time averaged fast neutron flux (neutrons/m**2-s)
    ! fotmtl - fuel oxygen to metal ratio
    ! frden  - ratio of fuel density to theoretical density
    ! gapmin - minimum gap size (ft)
    ! GasFraction(i) - absolute mole fractions
    !              1 = Helium
    !              2 = Argon
    !              3 = Krypton
    !              4 = Xenon
    !              5 = Hydrogen
    !              6 = Nitrogen
    !              7 = Air
    !              8 = Water vapor
    ! gpthki - input radial gas gap (ft)
    ! modfd  - indicator of fuel deformation model
    ! pfc    - pellet - cladding interfacial pressure (psia) to be limited to max value of 4000 psi for calc.
    ! pg     - pressure of gas in gas gap (psia)
    ! rf     - radius to outside surface of fuel (ft)
    ! rufc   - arithmetic mean roughness height of cladding (in)
    ! ruff   - arithmetic mean roughness height of fuel (in)
    ! tc     - temperature of inside surface of cladding (F)
    ! tempcm - peak historical cladding temperature  (F)
    ! tf     - temperature of outside surface of fuel (F)
    ! tflux  - cladding exposure time to fast neutron flux  (s)
    ! tg     - temperature of gas in gap (F)
    ! zro    - oxide thickness of cladding inner surface (m)
    !
    ! Output
    !
    ! hgapt  - gap conductance (BTU/hr-ft**2-F)
    ! hgap   - conduction contribution to conductance (BTU/hr-ft**2-F)
    ! hgapr  - radiation contribution to conductance (BTU/hr-ft**2-F)
    ! hsolid - contact contribution to conductance (BTU/hr-ft**2-F)
    !
    INTEGER(ipk) :: npass, i, l, modfd, node
    REAL(r8k) :: zro, gpthki, pg, rufc, ruff, rf, pfc, tg, tf, tc, flux, tflux, frden, coldw, &
      &          fotmtl,gpthk, gapmin, tgk, tfsi, tcsi, gascon, fa, gpthm, fe, fueltemprankine, &
      &          cladtemprankine, hgapr, tacom, ahe, axe, slop, rint, sum, adr, ags, djump, &
      &          djmpft, hgap, hgapt, hgapop, pfcinp, conf, bulocal, gadolin, conc, fkm, &
      &          hmeyer, pfc1, prel, r, e, rmult, hsolid, cee, thc, hgapc0, hgapcp, tempcm
    REAL(r8k), PARAMETER :: sbc = 0.4806e-12_r8k       ! (??Stefan-Boltzmann Constant??)
    REAL(r8k), PARAMETER :: convk = 1.73_r8k
    REAL(r8k), PARAMETER :: convl = 0.03281_r8k
    REAL(r8k), PARAMETER :: gpsi = 0.0_r8k
    REAL(r8k), PARAMETER :: gpthkn = 0.0_r8k
    REAL(r8k), DIMENSION(ngases) :: GasFraction
    !
    IF (ndebug) THEN
        WRITE(ounit,901) Time, zro, gpthki, pg, rufc, ruff
901     FORMAT(/' GAPHTC; Time = ',e11.4,' zro = ',e11.4,' gpthki = ',e11.4,' pg = ',e11.4,' rufc = ',e11.4,' ruff = ',e11.4)
        WRITE(ounit,903) rf, pfc, tg, tf, tc
903     FORMAT('      rf = ',e11.4,' pfc = ',e11.4,' tg = ',e11.4,' tf = ',e11.4,' tc = ',e11.4)
        WRITE(ounit,905) flux, tflux, frden, coldw, fotmtl
905     FORMAT('      flux = ',e11.4,' tflux = ',e11.4,' frden = ',e11.4,' coldw = ',e11.4,' fotmtl = ',e11.4)
        WRITE(ounit,907)
907     FORMAT('    gas fraction array ')
        WRITE(ounit,908) (GasFraction(l),l=1,ngases)
908     FORMAT(2x,e11.4)
    ENDIF
    ! Set minimum radial gap based on fuel and cladding roughness (ft)
    gpthk = MAX(gapmin, gpthki)
    ! Convert gas gap, fuel and cladding surface temperatures from F to K
    tgk = tfk(tg)
    tfsi = tfk(tf)
    tcsi = tfk(tc)
    
    ! Compute conductivity of gas in gap, convert from W/m-K to BTU/ft-F-sec
    gascon = MatProperty (Material='GAS', Property='THERMCOND', Temperature=tgk, Pressure=gpsi, GasComposition=GasFraction) / &
      &      (convk * sechr)
    !
    IF (ndebug) WRITE(ounit,910) gascon, tgk, gpsi, gpthkn
910 FORMAT(' GAPHTC; after gthcon call, gascon = ',e11.4,' tgk = ',e11.4,' gps1 = ',e11.4,' gpthkn = ',e11.4)
    ! Compute radiation heat transfer across gas gap. transform to an effective conductivity.
    ! Assume configuration factor equals 1
    fa = 1.0_r8k
    !
    gpthm = MAX(gpthk, gapmin)
    ! Compute emissivity factor fe
    CALL emssf2 (tf, tc, rf, gpthm, zro, fe)
    ! Convert temperature to rankine to calculate radiation term in English units.
    ! hgapr is in BTU/s-ft2-F
    FuelTempRankine = tfr(tf)
    CladTempRankine = tfr(tc)
    hgapr = sbc * fa * fe * (FuelTempRankine ** 2 + CladTempRankine ** 2) * (FuelTempRankine + CladTempRankine)
    !
    IF (ndebug) WRITE(ounit,911) fe, hgapr, rf, gpthm
911 FORMAT(' GAPHTC; fe = ',e11.4,' hgapr = ',e11.4,' rf = ',e11.4,' gpthm = ',e11.4)
    ! Compute temperature jump distance, using recommended form in report BNWL-1984
    tacom = tgk
    ! Since correlation for acomodation coefficient is not valid above 1000 K, limit tacom to 1000 K
    IF (tacom > 1000.0_r8k) tacom = 1000.0_r8k
    ahe = 0.425_r8k - 2.3e-4_r8k * tacom
    axe = 0.749_r8k - 2.5e-4_r8k * tacom
    slop = (axe - ahe) / 127.0_r8k
    rint = ahe - 4.0_r8k * slop
    sum = 0.0_r8k
    !
    DO i = 1, ngases
        adr = SQRT(ncGasProperties(i)%MWt)
        ags = slop * ncGasProperties(i)%MWt + rint
        sum = sum + ags * GasFraction(i) / adr
    ENDDO
    !
    djump = 2.23_r8k * (gascon * SQRT(tgk) / pg) / sum
    ! Convert djump from cm to ft
    djmpft = djump * convl
    !
    IF (ndebug) WRITE(ounit,913) djump, sum
913 FORMAT(6x,'djump = ',e11.4,' sum = ',e11.4)
    ! Conductance for open gap
    ! Include roughness factor (in ft) to have continuous transition going
    ! Calculation for open gap to closed gap
    ! hgap is in BTU/s-ft2-F
    ! hgap = gascon/(gpthk + djmpft + cruf/12.)
    hgap = gascon / (gpthk + djmpft)
    ! Sum up Open gap conductance contributions and convert to BTU/hr-ft2-F
    hgapt = (hgap + hgapr) * sechr
    !
    IF (ndebug) WRITE(ounit,915) hgapt, hgap, gapmin, djmpft
915 FORMAT('     Open Gap: hgapt = ',e11.4,' hgap = ',e11.4,' gapmin = ',e11.4,' djmpft = ',e11.4)
    !
    ! If interfacial pressure is ~0, then return
    IF (pfc < 1.0e-10_r8k) RETURN
    !
    ! Conductance for closed gap
    hgapop = hgapt
    ! Limit interface pressure to 4000 psi to match FRAPCON3
    pfcinp = MIN(pfc, 4000.0_r8k)
    ! Compute solid-solid contact conductance, using recommended formula in NUREG/CR-6534 (same as for FRAPCON3)
    ! Conductivity values in W/m-K

    ! Fuel thermal conductivity
    conf = MatProperty (Material='FUEL', Property='THERMCOND', Temperature=tfsi, Burnup=bulocal, Fraction_TD=frden, &
      &                 Pu=compmt, OMRatio=fotmtl, Gadolinia=gadolin)
    
    ! Cladding thermal conductivity
    conc = MatProperty (Material='CLAD', Property='THERMCOND', Temperature=tcsi, Flux=flux, ColdWork=coldw)
    !
    fkm = 2.0_r8k * conf * conc / (conf + conc)
    ! Determine meyer hardness and convert from N/m2 to psi
    hmeyer = MatProperty (Material='CLAD', Property='MEYER_HARD', Temperature=tcsi) / 6895.0_r8k
    !
    DO npass = 1, 2
        ! npass = 1 - contact conductance with zero interface pressure
        ! npass = 2 - contact conductance with input interface pressure
        IF (npass == 1) pfc = 0.0_r8k
        IF (npass == 2) pfc = pfcinp
        ! hsolid will be calculated in units of W/m2-K and then converted
        ! pfc is in psi, convert to kg/cm2 for use in cee_fraptran
        pfc1 = pfc / 14.2232_r8k
        prel = MAX(pfc / hmeyer, 0.0_r8k)
        ! mean roughness in m
        r = SQRT(rufc ** 2 + ruff ** 2) / 39.37_r8k
        ! e term uses fuel roughness in micro-inch
        e = EXP(5.738_r8k - 0.528_r8k * LOG(ruff * 1.0e6_r8k))
        !
        ! Compute solid conductance based on prel
        IF (prel > 0.003_r8k) THEN ! high prel
            IF (prel <= 0.0087_r8k) THEN
                rmult = 333.3_r8k * prel
            ELSE
                rmult = 2.9_r8k
            ENDIF
            hsolid = 0.4166_r8k * fkm * prel * rmult / r / e
        ELSE IF (prel < 9.0e-06_r8k) THEN ! Low prel
            hsolid = 0.4166_r8k * fkm * SQRT(prel) / r / e
        ELSE ! Intermediate prel
            hsolid = 0.00125_r8k * fkm / r / e
        ENDIF
        ! convert hsolid to BTU/s-ft2-F
        hsolid = hsolid * 0.1761_r8k / sechr
        ! calculate gas conduction under contact conditions
        ! thc in ft, hgap in BTU/s-ft2-F
        cee = 1.0_r8k * EXP(-0.00125_r8k * pfc1)
        thc = (cee * (ruff + rufc) + djump / 2.54_r8k) / 12.0_r8k
        IF (thc < gapmin) thc = gapmin
        hgap = gascon / thc
        ! Sum up closed gap contributions and convert to BTU/hr-ft2-F
        hgapt = (hgap + hgapr + hsolid) * sechr
        IF (ndebug) WRITE(ounit,919) hgap, hgapr, hsolid
919     FORMAT(' GAPHTC; hgap = ',e11.4,' hgapr = ',e11.4,' hsolid = ',e11.4)
        !
        IF (npass == 1) hgapc0 = hgapt
        IF (npass == 2) hgapcp = hgapt
    END DO
    ! Force continuity of gap conductance by making open conductance with zero gap
    ! equal to closed conductance with zero interface pressure
    hgapt = hgapcp - (hgapc0 - hgapop)
    !
    IF (ndebug) WRITE(ounit,917) hgapt, hgapop, hgapc0, hgapcp
917 FORMAT(' GAPHTC; hgapt = ',e11.4,' hgapop = ',e11.4,' hgapc0 = ',e11.4,' hgapcp = ',e11.4)
    !
    END SUBROUTINE gaphtc
!
END MODULE HeatTransferCoefficient_fraptran













