MODULE Reflood_Conditions
    USE Kinds
    USE Functions, ONLY : polate, simq
    USE sth2x, ONLY : surten, thcon, visc, viscol
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE reflood (AxNodElevat, a, b, drod, AxialNodLen, time, deltat, AxialPowr, naxn, rl, RuptFailIndex, &
     &                 acond, tsurf, qq, htc, qcrit, ihtreg, k,Nchan)
    USE Kinds
    USE conversions_fraptran, ONLY : pi, sechr, tfk, tkf
    USE Functions, ONLY : polate
    USE variables_fraptran, ONLY : ounit,emflag
    USE CoolantProperties
    USE sth2x, ONLY : sth2x2, sth2x3
    IMPLICIT NONE
    !>@brief
    !> Subroutine reflood manages the refill and reflood heat transfer calculations.
    !>@author
    !> reflood and its companion Subroutines were developed, written and installed by  b. w. burnham - september 1977.
    !
    ! Input
    !
    ! a             - heat-1 conduction solution coefficient
    ! acond         - a vector of conditions. See htrc for a complete description
    ! AxialNodLen   - the array of axial node lengths, ft
    ! AxialPowr     - the array of axial node powers, kW
    ! AxNodElevat   - axial node midplane elevation, ft
    ! b             - heat-1 conduction solution coefficient
    ! deltat        - the time step, sec
    ! drod          - fuel pin diameter, ft
    ! k             - current axial node number
    ! naxn          - the number of axial nodes
    ! rl            - the total rod length, ft
    ! RuptFailIndex - clad failure flag
    ! time          - the total problem time, sec
    !
    ! Output
    !
    ! htc    - The heat transfer coefficient used, btu's/ft**2-hr-F
    ! ihtreg - Heat transfer mode flag
    ! qcrit  - Heat transfer mode flag
    ! qq     - Surface heat flux, btu's /ft**2-hr
    ! tsurf  - Fuel pin surface temperature , degrees F
    !
    ! See fldinp for the description of the --flecht-- common variables
    !
    INTEGER(ipk), INTENT(IN) :: naxn
    INTEGER(ipk) :: ihtreg, ipath, nchan, itph, kdbug, i, k, mode, it, itb5=1, n=1, npdc=8, nqax=1
    INTEGER(ipk), PARAMETER :: ithree = 3
    INTEGER(ipk), PARAMETER :: izero = 0
    INTEGER(ipk), PARAMETER :: i26 = 26
    REAL(r8k), PARAMETER :: frthrs = 1.0_r8k ! frthrs = flooding rate threshold
    REAL(r8k), PARAMETER :: fmngen = 0.4_r8k ! set cutoff flooding rate for generalized flecht
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), PARAMETER :: smidgn = 1.0_r8k
    REAL(r8k), PARAMETER :: eh = 4.3e-4_r8k
    REAL(r8k), PARAMETER :: encp = 2.38845896e-4_r8k
    REAL(r8k), PARAMETER :: sisvol = 6.24279606e-2_r8k
    REAL(r8k), PARAMETER :: sipr = 6894.76_r8k
    LOGICAL :: err
    CHARACTER(LEN=8), PARAMETER :: on = 'on'
    CHARACTER(LEN=8), PARAMETER :: off = 'off'
    CHARACTER(LEN=11) :: FlechtCorrelation = ''
    INTEGER(ipk), DIMENSION(:) :: RuptFailIndex
    REAL(r8k) :: tmax, time, htc, tsurf, b, a, qq, qcrit, tsat, tinlet, rhowat, tinsi, deltat, elevn, agflow, &
      &          zqch1, zqch2, power, height, htcflc, qlocal, pdecay, rl, deltaz, pincir, drod
    REAL(r8k), DIMENSION(:) :: AxNodElevat, AxialPowr, AxialNodLen
    REAL(r8k), DIMENSION(:,:) :: acond
    REAL(r8k), DIMENSION(16), PARAMETER :: pdcay = [  0.0_r8k,   0.0_r8k, 38.0e2_r8k,   50.0_r8k, 69.9_r8k,  103.0_r8k, &
      &                                              94.8_r8k, 149.0_r8k,  122.2_r8k,  202.0_r8k, 147.1_r8k, 252.0_r8k, &
      &                                             170.1_r8k, 300.0_r8k,  437.0_r8k, 1000.0_r8k ]
    ! Calculate the time into reflood. Check to see if the problem has advanced past refill and into reflood.
    ! If not, do the adabatic heat up and return.
    tmax = tempmx
    tflood = time - refdtm
    ! If tflood is negative the fuel pin is undergoing adabatic heatup
    IF (tflood <= zero) THEN
        htc = zero
        tsurf = -b/a
        qq = zero
        qcrit = -2.0_r8k
        ihtreg = 11
        ipath = 1
    ENDIF
    ! Check the time advance values to determine whether a new time step is initiated.
    ! If time > oldtim there is a new timestep. Otherwise, skip first of timestep calculations.
    !
    ! Check to see If one pass through the steam tables is called for
    !**************************************************
    !*prevent hcalf from being Call If tflood=0.      *
    !**************************************************
    IF (time == oldtim .AND. tflood <= zero) GOTO 1000
    IF (time > oldtim) THEN
        ! Otherwise, increment the time record
        oldtim = time
        ! Set up the Call to sth2x2 for saturated water properties.
        ! prop -- is the communication vector used by sth2x2. values are in  si  units
        prop = 0.0_r8k
        !
        ! assign the system pressure
        ! 
        ! pressr = polate (prestm, tflood, nbrpst)
        ! Modified by Ian Porter 4/21/2015
        IF (tflood < prestm(2)) THEN
            pressr = prestm(1)
            ! polate (pbh, time, npbh, ih)
        ELSE
            pressr = polate (prestm, tflood, nbrpst)
        ENDIF
        !
        ! convert to si
        !
        pressi = pressr * sipr
        prop(2) = pressi
        !
        ! set the water quality to zero
        !
        prop(9) = zero
        !
        ! set the error flag
        !
        err = .TRUE.
        CALL sth2x2 (aasth, prop, err)
        !
        ! check for error in call
        !
        IF (err) THEN
            WRITE(ounit,10)
10          FORMAT(' error in call to sth2x2 from  reflood execution stopped')
            STOP
        ENDIF
        !
        ! otherwise pick out the saturated properties needed
        !
        tsat = tkf (prop(1))
        tsatt = tsat
        cpstem = prop(22) * encp
        rhostm = (1.0_r8k / prop(12)) * sisvol
        !**************************************************
        !* additional properties are needed for the flt-c-*
        !* set correlation                  *
        !**************************************************
        acond(22,Nchan) = prop(15) * eh
        acond(23,Nchan) = prop(16) * eh
        acond(24,Nchan) = prop(11) / sisvol
        acond(25,Nchan) = prop(12) / sisvol
        acond(26,Nchan) = prop(21) * encp
        tsat = tkf (prop(1))
        !
        ! If in the adabatic mode,  set temphi to tsat and RETURN
        !
        templo = tsat
        temphi = tsat
        !
        IF (tflood <= zero) GOTO 1000
        !
        ! pick out the current flooding rate
        !
        fldrte = polate (fldrat, tflood, nbrfdr)
        !
        ! pick out the current inlet temperature
        tinlet = polate (temptm, tflood, nbrtmp)
        !
        ! calculate density of flood water at core inlet
        !
        IF (tinlet < tsat) THEN
            !
            ! compute density of subcooled water
            !
            tinsi = tfk(tinlet)
            itph = 1
            prop(1) = tinsi
            prop(2) = pressi
            !
            CALL sth2x3 (aasth, prop, itph, err)
            !
            IF (err) THEN
                WRITE(ounit,48)
48              FORMAT(///' temperature or pressure out of range in call to sth2x3 from reflood. execution stopped ')
                STOP
            ENDIF
            rhowat = (1.0_r8k / prop(3)) * sisvol
        ELSE
            rhowat = (1.0_r8k / prop(11)) * sisvol
        ENDIF
        !
        ! calculate the subcooling
        tsub = tsat - tinlet
        !
        IF (nbrliq > 0) GOTO 35
        !
        ! calculate the carry-out rate fraction and quench level
        !
        IF (fldrte < 0.0_r8k) fldrte = 0.0_r8k
        CALL crfzqh (pressr, qmax, fldrte, tsub, zqch, deltat, crf)
        GOTO 39
        !
        ! calculate carryover fraction from input of collapsed liquid level
        !
    35  zqch1 = zqch
        zqch2 = polate (hlqclp, tflood, nbrliq, itb5)
        zqch = zqch2
        gflow = rhowat * sechr * (fldrte/12.0_r8k - (zqch2 - zqch1)/deltat)
        IF (fldrte < 1.0e-5_r8k) crf = 1000.0_r8k
        IF (fldrte < 1.0e-5_r8k) GOTO 41
        crf = 1.0_r8k - (zqch2 - zqch1)/(fldrte * deltat/12.0_r8k)
        GOTO 41
    39  CONTINUE
        !
        ! calculate the outlet mass flow rate,  lbm / ft**2 . hv
        !
        gflow = ((fldrte * rhowat * sechr)/12.0_r8k) * crf
        IF (gflow < 1.0_r8k) gflow = 1.0_r8k
    41  CONTINUE
        kdbug = 0
        IF (kdbug == 1) WRITE(ounit,49) time,rhowat,fldrte,crf,gflow
    49  FORMAT(' sub. reflood start, time = ',e11.4,' rhowat = ',e11.4, &
          &    ' fldrte = ',e11.4,' crf = ',e11.4,' gflow = ',e11.4)
        IF (kdbug == 1) WRITE(ounit,901)flowbk
    901 FORMAT(' flowbk = ',e11.4)
        !
        ! once per time step determine the axial node level zqch
        !
        IF (hliq < 0.0_r8k) hliq = 0.0_r8k
        liqnod = izero
        DO i = 1, naxn
            IF (AxNodElevat(i) > zqch) liqnod = i
            IF (i == naxn) liqnod = i
            IF (liqnod > izero) Exit
        ENDDO
        !
        ! integrate the  "liquid height' --hliq  in feet
        !
        hliq = hliq  + deltat * fldrte / 12.0_r8k
        !
        ! interrogate the rupture flags and compare with axial node height
        ! to track rupture plane for the rupture model
        !
        ! this need be done only until the rod has failed
        !
        IF (rupflg == off) THEN
            DO i = 1, naxn
                IF (RuptFailIndex(i) /= izero) THEN
                    rupflg = on
                    irup = i
                    ruplev = AxNodElevat(i)
                    EXIT
                END IF
            ENDDO
        ENDIF
        !
    ENDIF
    tsat = tsatt
    ! Do the axially dependent calculation for the flecht correlation
    !
    ! Calculate the equivalent flecht height
    !
    ! Find product of (integrated normalized nuclear power)*pfflec/pfnuc
    power = (powing(k, AxialPowr, AxialNodLen) / pavg) * pfflec / pfnuc
    !
    ! Height is the axial elevation for the flt-c-set correlation
    !
    IF (mflt == 1) THEN
        height = AxNodElevat(k)
        FlechtCorrelation = 'Generalized'
    ELSE
        height = polate (flthit, power, nbrfht)
        ! Reset height within the flecht bounds
        IF (nflec == 0) THEN
            IF (height < 2.0_r8k) height = 2.0_r8k
            IF (height > 10.0_r8k) height = 10.0_r8k
            ! If short fuel rods, call short core flecht
            FlechtCorrelation = 'Generalized'
        ELSE
            IF (height < 3.0_r8k) height = 3.0_r8k
            IF (height > 10.0_r8k) height = 10.0_r8k
            FlechtCorrelation = 'PWR_Flecht'
        END IF
    END IF
    !
    SELECT CASE (FlechtCorrelation)
    CASE ('PWR_Flecht')
        !
        ! Call wcorr for the flecht heat transfer coefficient
        !
        CALL wcorr (tsub, pressr, tmax, qmax, height, hrad, fldrte, tflood, hliq, htcflc)
    CASE ('Generalized')
        !
        ! Call Bob Lin's generalized flecht correlation
        !
        qlocal = qmax
        !**************************************************************
        !* the integral of the power history is normalized by a power *
        !* curve integral for use in the flt-c-set correlation        *
        !**************************************************************
        pdecy = polate (faxzq,zpkfc,npaxpf,nqax) * pavgft / qmax
        qaxpk = polate (qaxzq,zpkfc,npaxpf,nqax) * pavgft / qmax
        IF (tflood /= tfoldf) THEN
            pdeint = pdeint + pdecy * deltat
            tfoldf = tflood
        ENDIF
        pdecay = pdeint / polate (pdcay, tflood, npdc, n)
        IF (tflood <= 5.0_r8k) pdecay = 1.0_r8k
        !***********************************************************************
        !* variables are added to the hcalf call for the flt-c-set correlation *
        !***********************************************************************
        CALL hcalf (tmax, qlocal, height, htcflc, acond, pdecay, deltat, mode, rl, Nchan)
    END SELECT
    ! Under extreme conditions, wcorr may return a negative heat transfer coefficient. If so, set to 0.0.
    IF (htcflc < 0.0_r8k) htcflc = 0.0_r8k
    ! The crucial variable returned from wcorr is htcflc
    !
    ! Now decide which of the  l-o-d models to follow.
    ! Check against the input parameter -stmpln- as to liquid level or  rupture plane
    IF (lodmrk == 'liqlev') GOTO 110
    ! Otherwise, CONTINUE with the rupture plane model.
    ! Check the rupture flags and proceed.
    ! If no failure is indicated, use htcflc and RETURN.
    IF (rupflg == on) GOTO 30
    !
    CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
    ! Save the bulk temperature used and RETURN
    htc = htcflc
    templo = tsat
    temphi = tsat
    ihtreg = 12
    ipath = 2
    GOTO 1000
    !
30  CONTINUE
    ! If the rupture flag is on but the axial node is not yet up to the rupture plane, use htcflc and RETURN
    IF (k < irup) GOTO 34
    IF (emflag(15) == off .AND. fldrte >= 0.4_r8k) GOTO 34
    IF (emflag(15) == on  .AND. fldrte >= 1.0_r8k) GOTO 34
    GOTO 40
34  CONTINUE
    !
    CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
    ! Save the bulk temperature used and RETURN.
    htc = htcflc
    templo = tsat
    temphi = tsat
    ihtreg = 12
    ipath = 3
    GOTO 1000
    !
40  CONTINUE
    ! If the axial node lies at the rupture plane make the inital pass through the
    ! steam-flecht interface model to calculate the heat capacity multiplier.
    IF (k <= irup) THEN
        !
        cpmult = dbinvs (tsat, rhostm, hydiam, htcflc, cpstem, gflow)
        ! Save cpmult, call tailnd, save bulk temperature and RETURN.
        CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
        !
        htc = htcflc
        templo = tsat
        temphi = tsat
        ihtreg = 12
        ipath = 4
        GOTO 1000
    END IF
    ! The axial elevation is above the rupture plane. Envoke the enthalpy rise model.
    ! Check the axial node indicator to see If the axial node has been incremented while above the rupture plane.
    ! If not -- kaxnlo = k restore templo to the coolant temperature used on the previous
    ! pass and CONTINUE otherwise, increment the coolant temperature and CONTINUE.
    !
    IF (kaxnlo /= k) THEN
        templo = temphi
        kaxnlo = k
    ENDIF
    ! Using templo and pressr, Call sth2x3 for heat capacity and density of the next lower node coolant
    ! Set the error flag
    err = .TRUE.
    !
    it = ithree
    !
    prop(1:23) = 0.0_r8k
    !
    IF (templo < (tsat + smidgn)) templo = tsat + smidgn
    !
    prop(1) = tfk(templo)
    prop(2) = pressi
    !
    CALL sth2x3 (aasth, prop, it, err)
    !
    ! check for error in call
    !
    IF (err) THEN
        WRITE(ounit,62)
62      FORMAT(' error in Call to sth2x3 from reflood execution stopped ')
        STOP
    ENDIF
    ! Otherwise pick out the vapor quantities and CONTINUE
    cpstem = prop(8) * encp
    rhostm = (1.0_r8k / prop(3)) * sisvol
    !
    ! the axial node height, ft
    !
    deltaz = AxialNodLen(k)
    !
    ! the fuel pin circumference, ft
    !
    pincir = pi * drod
    !
    ! set the local elevation
    !
    elevn = AxNodElevat(k)
    !
    CALL enrise (gflow, cpmult, flxsec, deltaz, deltat, pincir, a, b, htcflc, templo, cpstem, rhostm, hydiam, &
      &          fldrte, elevn ,rupflg, flowbk, ruplev, temphi, tsurf, qq, qcrit, htc, ihtreg, agflow, k)
    ipath = 5
    ! On RETURN from -enrise- htc is the heat transfer coefficient used and temphi is the new coolant bulk temperature.
    ! Set the bulk coolant in the heat-1 array and RETURN.
    GOTO 1000
    !**** End of the rupture plane--l-o-d model.
    !**** Beginning of the liquid level--l-o-d model
110 CONTINUE
    ! Check the reflood rate.  If the reflood rate exceeds 1-inch /sec, use htcflc and exit
    IF (emflag(15) == off .AND. fldrte >= fmngen) GOTO 68
    IF (emflag(15) == on .AND. fldrte >= frthrs) GOTO 68
    GOTO 70
68  CONTINUE
    ! otherwise
    CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
    ! Save the bulk temperature used and RETURN
    htc = htcflc
    templo = tsat
    temphi = tsat
    ihtreg = 12
    ipath = 6
    !
    GOTO 1000
    !
70  CONTINUE
    ! The reflood rate is less than one inch per second.  If the quench level is above the axial node, use htcflc.
    IF (k < liqnod) THEN
        !
        CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
        ! Save the bulk temperature and RETURN.
        htc = htcflc
        templo = tsat
        temphi = tsat
        ihtreg = 12
        ipath = 7
        !
        GOTO 1000
    END IF
    !
    ! the reflood rate is less than one inch per second. the quench
    ! level is at or below the axial node.  If the quench level is
    ! at the node level Call dbinvs for the heat capacity multiplier
    ! and RETURN.  otherwise the node is above the  quench level so
    ! GOTO  the enthalpy rise model.
    !
    IF (k > liqnod) THEN
        ! The axial elevation is above the rupture plane. Envoke the enthalpy rise model
        !
        ! check the axial node indicator to see If the axial node has been
        ! incremented while above the rupture plane
        ! If not -- kaxnlo = k restore templo to the coolant  temperature
        ! used on the previous pass and CONTINUE otherwise, increment the
        ! coolant temperature and CONTINUE
        !
        IF (kaxnlo /= k) THEN
            templo = temphi
            kaxnlo = k
        END IF
        ! using -templo- and pressr  CALL  sth2x3 for heat capacity and density of the next lower node coolant
        ! set the error flag
        err = .TRUE.
        !
        it = ithree
        !
        prop = 0.0_r8k
        !
        IF (templo < (tsat + smidgn)) templo = tsat + smidgn
        !
        prop(1) = tfk(templo)
        prop(2) = pressi
        !
        CALL sth2x3 (aasth, prop, it, err)
        !
        ! check  for  error in  call
        !
        IF (err) THEN
            WRITE(ounit,65)
65          FORMAT(' error in Call to sth2x3 from reflood-execution stopped - second  call')
            STOP
        ENDIF
        !
        ! otherwise pick out the vapor quantites and CONTINUE
        !
        cpstem = prop(8) * encp
        rhostm = (1.0_r8k / prop(3)) * sisvol
        !
        ! the axial node height, ft
        !
        deltaz = AxialNodLen(k)
        !
        ! the fuel pin circumference, ft
        !
        pincir = pi * drod
        !
        ! set the local elevation
        !
        elevn = AxNodElevat(k)
        !
        CALL enrise (gflow, cpmult, flxsec, deltaz, deltat, pincir, a, b, htcflc, templo, cpstem, rhostm, hydiam, &
          &          fldrte, elevn, rupflg, flowbk, ruplev, temphi, tsurf, qq, qcrit, htc, ihtreg, agflow, k)
        !
        ipath = 9
        !
        ! on RETURN from -enrise- htc is the heat transfer coefficient
        ! used and temphi is the new coolant bulk temperature
        ! set the bulk coolant in the heat-1 array and RETURN
        !
        !**** End of the liquid level -- l-o-d model
        !
    ELSE
        !
        cpmult = dbinvs (tsat, rhostm, hydiam, htcflc, cpstem, gflow)
        ! save cpmult, Call tailnd, save bulk temperature and RETURN
        CALL tailnd (a, b, htcflc, tsat, tsurf, qq, qcrit)
        htc = htcflc
        templo = tsat
        temphi = tsat
        ihtreg = 12
        ipath = 8
    END IF
1000 CONTINUE
    !
    ! pass the system parameters back to main
    !
    acond(12,Nchan) = 0.0_r8k
    IF (tflood > zero) THEN
        acond(12,Nchan) = gflow
        ! If above rupture plane, store adjusted flow
        IF (emflag(15) == off .AND. fldrte >= 0.4_r8k) GOTO 1100
        IF (emflag(15) == on .AND. fldrte >= 1.0_r8k) GOTO 1100
        !
        IF (irup > 0 .AND. k > irup) acond(12,Nchan) = agflow
    END IF
1100 acond(16,Nchan) = pressr
    acond(17,Nchan) = temphi
    acond(19,Nchan) = zero
    acond(21,Nchan) = tsat
    qcrit = sechr * qcrit
    !
    ihtreg = ipath
    !***************************************************
    !* output flt-c-set h.t. modes                     *
    !***************************************************
    IF (mflt == 1 .AND. tflood > 0.0_r8k) ihtreg = mode
    !
    END SUBROUTINE reflood
    !
    !
    !
    REAL(r8k) FUNCTION powing (k, AxialPowr, AxialNodLen)
    USE Kinds
    USE resti_h, ONLY : naxn
    IMPLICIT NONE
    !>@brief
    !> Function powing returns the integral of the axial power from the bottom of the rod to the height dza(nodnbr)
    !>@author
    !> Modified by I. Porter, NRC, March 2014 to clean coding and convert to .f90
    INTEGER(ipk) :: k, i
    REAL(r8k), DIMENSION(:) :: AxialPowr, AxialNodLen
    !
    powing =  0.0_r8k
    SELECT CASE (k)
    CASE (1)
        powing = 0.5_r8k * AxialNodLen(k) * AxialPowr(k)
    CASE DEFAULT
        DO i = 1, (k - 1)
            powing =  powing + AxialPowr(i) *  AxialNodLen(i)
        ENDDO
        powing = powing + AxialPowr(k)  * 0.5_r8k * AxialNodLen(k)
    END SELECT
    !
    END FUNCTION powing
    !
        SUBROUTINE crfzqh (pressr, qmax, fldrte, tsub, zqch, deltat, crf)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine crfzqh calculates the carry-out rate fraction and the quench height at each timestep
    !
    ! Input
    !
    ! pressr - System pressure, psia
    ! qmax   - Flecht equivalent power
    ! fldrte - Reflood rate, inchs / second
    ! tsub   - Coolant subcooling, ( tsat - tcoolant), F
    ! deltat - Time step, seconds
    ! zqch   - Calculated quench height, feet
    !
    ! Output
    !
    ! crf    - The carry-out rate fraction
    ! zqch   - Calculated quench height, feet (if too small, reset zqch)
    !
    REAL(r8k) :: a, b, c, d, e, f, g, h
    REAL(r8k), INTENT(IN) :: pressr, qmax, fldrte, tsub, deltat
    REAL(r8k), INTENT(OUT) :: crf
    REAL(r8k), INTENT(INOUT) :: zqch
    !
    zqch = MAX(1.0e-3_r8k, zqch)
    ! The carry-out rate fraction
    a = 1.0_r8k * EXP(-0.1232_r8k * pressr ** 0.308_r8k)
    b = 1.0_r8k - EXP(-1.0831_r8k * qmax ** 0.252_r8k)
    c = 1.0_r8k - EXP(-1.2397_r8k * fldrte ** 0.162_r8k)
    d = EXP(0.3209_r8k * tsub ** 0.086_r8k)
    e = 1.0_r8k - EXP(-1.9293_r8k * zqch)
    f = EXP(-0.2018_r8k * zqch ** 2)
    g = 4.1644_r8k * e
    h = 0.7_r8k * zqch * f
    !
    crf = a * b * c * (g - h) / d
    ! If crf is too small, set minimum crf
    crf = MAX(1.0e-5_r8k, crf)
    ! next calculate the quench level.  zqch is initialized in fldinp
    zqch = zqch + fldrte * deltat * (1.0_r8k - crf) / 12.0_r8k
    !
        END SUBROUTINE crfzqh
!
!
    FUNCTION dbinvs (temp, rhostm, hydiam, htcflc, cpstem, MassFlowRate)
    USE Kinds
    USE variables_fraptran, ONLY : ounit, Time, ndebug
    IMPLICIT NONE
    !>@brief
    !> function dbinvs calculates the heat capacity multiplier which provides a smooth transition from flecht to steam.
    !> dbinvs is part of the reflood package.
    !
    ! Input
    !
    ! temp    - steam temperature-usually set to saturation temperature, F
    ! rhostm  - steam density, lbm/ft**3
    ! hydiam  - channel hydraulic diameter, ft
    ! htcflc  - the flecht heat transfer coefficient, btu/ft**2-hr-F
    ! cpstem  - the sth2x2 heat capacity of saturated vapor at system pressure and temperature - temp, btu/lbm-f
    ! MassFlowRate - mass flux, lbm/ft**2-hr
    !
    ! Output
    !
    ! dbinvs  - heat capacity multiplier, unitless
    !
    INTEGER(ipk), PARAMETER :: n2 = 2
    REAL(r8k) :: dbinvs, thmcon, visco, a, b, c, cpdb
    REAL(r8k), INTENT(IN) :: temp, rhostm, hydiam, htcflc, cpstem, MassFlowRate
    !
    IF (MassFlowRate <= 1.0_r8k .OR. htcflc <= 0.01_r8k) THEN
        dbinvs = 1.0_r8k
    ELSE
        ! Calculate the thermal conductivity and viscosity
        thmcon = thcon (n2, temp, rhostm)
        visco  = visc (n2, temp, rhostm)
        ! Calculate the inverse of the dittus-boelter heat transfer correlation - solving for the heat capacity
        a = 1.0_r8k / (MassFlowRate * hydiam / visco) ** 0.8_r8k
        b = (htcflc * hydiam) / (0.023_r8k * thmcon)
        c = thmcon / visco
        cpdb = c * (a * b) ** 2.5_r8k
        dbinvs = cpdb / cpstem
    ENDIF
    !
    IF (ndebug) THEN
        WRITE(ounit,901) Time, rhostm, htcflc, cpstem, MassFlowRate, hydiam
901     FORMAT('in dbinvs, Time = ',e11.4,' rhostm = ',e11.4,' htcflc = ',e11.4,' cpstem = ',e11.4, &
          &    ' MassFlowRate = ',e11.4,' hydiam = ',e11.4)
        WRITE(ounit,903) thmcon, visco
903     FORMAT('thmcon = ',e11.4,' visco = ',e11.4)
        WRITE(ounit,905) cpdb, dbinvs
905     FORMAT('cp(backed out of dittus boelter) = ',e11.4,' cpmult = ',e11.4)
    ENDIF
    !
    END FUNCTION dbinvs
!
!
    SUBROUTINE enrise (MassFlowRate, cpmult, flxsec, deltaz, deltat, pincir, a, b, htcflc, templo, &
      &                cpstem, rhostm, hydiam, fldrte, AxNodElevat, rupflg, flowbk, ruplev, tcc, &
      &                tsurf, qq, qcrit, htc, ihtreg, agflow, k)
    USE Kinds
    USE conversions_fraptran, ONLY : sechr
    USE variables_fraptran, ONLY : ounit, Time, ndebug
    IMPLICIT NONE
    !>@brief
    !> Subroutine enrise calculates the the enthalpy rise of the reflood vapor coolant and solves
    !> the remaining heat conduction from heat-1. enrise is part of the reflood package.
    !
    ! Input
    !
    ! MassFlowRate - mass flux, lbm/ft**2-hr
    ! cpmult   - heat capacity multiplier, unitless
    ! flxsec   - flow chanel cross-sectional area, ft**2
    ! deltaz   - axial node height, ft
    ! deltat   - time step ,seconds
    ! pincir   - fuel pin outer circumference ,ft
    ! a        - heat-1 conduction solution coefficient
    ! b        - heat-1 conduction solution coefficient
    ! htcflc   - flecht heat transfer coefficient, btu/ft**2-hr-f
    ! templo   - coolant temperature of the next lower axial node, degrees F
    ! pressr   - system pressure , psia
    ! cpstem   - heat capacity of steam from sth2x
    ! rhostm   - specific density of steam from sth2x
    ! hydiam   - channel hydraulic diameter, ft
    ! fldrte   - flood rate, inches per second
    ! AxNodElevat - local elevation, feet
    ! rupflg   - rupture flag
    ! flowbk   - flow blockage, percent
    ! ruplev   - rupture elevation, feet
    !
    ! Output
    !
    ! tcc      - new coolant temperature, degrees F
    ! tsurf    - new surface temperature, degrees F
    ! qq       - heat flux, btu's / ft**2-hr
    ! qcrit    - heat transfer mode flag
    ! htc      - the heat transfer coefficient which yields the smaller heat flux
    !
    INTEGER(ipk) :: ihtreg, npass, nsimq, ks
    INTEGER(ipk), INTENT(IN) :: k
    INTEGER(ipk), PARAMETER :: izero = 0
    INTEGER(ipk), PARAMETER :: ione =1
    INTEGER(ipk), PARAMETER :: itwo = 2
    INTEGER(ipk), PARAMETER :: ithree = 3
    INTEGER(ipk), PARAMETER :: n2 = 2
    REAL(r8k), INTENT(IN) :: MassFlowRate, cpmult, flxsec, deltaz, deltat, pincir
    REAL(r8k) :: a, b, htcflc, thmcon, visco, agflow, deltah, a1, hydiam, b1, &
      &          h1, cpstem, c1, htcdtb, rhostm, templo, f1, f2, f, f3, htc, fldrte, axnodelevat, &
      &          flowbk, ruplev, tcc, qq, tsurf, qcrit, aa, bb
    REAL(r8k), PARAMETER :: gmin = 1000.0_r8k
    CHARACTER(LEN=8) :: rupflg
    REAL(r8k), DIMENSION(3) :: bs, cs
    REAL(r8k), DIMENSION(9) :: as
    !
    aa = a * sechr
    bb = b * sechr
    IF (htcflc <= 0.01_r8k) THEN
        !
        ! Low flow solution - adiabatic heatup assumed
        tsurf = -bb / aa
        tcc = tsurf
        htc = 0.0_r8k
        qcrit = -5.0_r8k
        qq = 0.0_r8k
        WRITE (0,11) time
11      FORMAT ('Low flow solution - adiabatic heatup assumed at time ',e12.4,', seconds.')
        RETURN
    ENDIF
    !
    thmcon = thcon (n2, templo, rhostm)
    !
    visco = visc (n2, templo, rhostm)
    ! set the heat transfer mode
    ihtreg = 13
    ! adjust the mass flow as per licencing prescribed model
    agflow = fiddle (fldrte, AxNodElevat, rupflg, MassFlowRate, flowbk, ruplev)
    !  convert the timestep to hours
    deltah = deltat / sechr
    ! convert  a and b
    ! calculate the dittus-boelter heat transfer coefficient
    a1 = (0.023_r8k * thmcon) / hydiam
    b1 = (agflow * hydiam / visco) ** 0.8_r8k
    h1 = cpmult * cpstem
    IF (ndebug) THEN
        WRITE(ounit,901) Time, k
901     FORMAT(' in enrise at Time(sec) = ',e11.4,' k = ',i3)
        WRITE(ounit,903) MassFlowRate, agflow, thmcon, hydiam, visco, cpstem
903     FORMAT(' MassFlowRate = ',e11.4,' agflow = ',e11.4,' thmcon = ',e11.4,' hydiam =',e11.4,' visco =',e11.4,' cpstem =',e11.4)
    ENDIF
    !
    h1 = MIN(1.0_r8k, h1)
    !
    c1 = (h1 * visco / thmcon) ** 0.4_r8k
    !
    htcdtb = a1 * b1 * c1
    IF (ndebug) THEN
        WRITE(ounit,905) cpmult, htcdtb, h1
905     FORMAT(' cpmult = ',e11.4,' htcdtb = ',e11.4,' cpmod = ',e11.4)
        WRITE(ounit,907) htcflc, rhostm, pincir, deltaz, deltah, aa, bb
907     FORMAT(' htcflc = ',e11.4,' rhostm = ',e11.4,' pincir = ',e11.4,' deltaz = ',e11.4, &
          &    ' deltah = ',e11.4,' aa = ',e11.4,' bb = ',e11.4)
        WRITE(ounit,909) flxsec,templo
909     FORMAT(' flxsec = ',e11.4,' templo = ',e11.4)
    ENDIF
    ! Next make two passes through the enthalpy rise equations, one pass with htcflc and one with htcdtb.
    ! RETURN the results from the pass resulting in the smaller heat flux
    f1 = flxsec / (pincir * deltaz * deltah)
    f2 = rhostm * deltaz * cpmult * cpstem + agflow * deltah
    f = f1 * f2
    f3 = f * templo
    ! The following system of equations is solved for tc, ts, qs
    ! Parameters:
    ! tc = coolant temperature at axial node k, F
    ! ts = cladding surface temperature, F
    ! qs = cladding surface heat flux, btu/(ft2-hr)
    ! (cpmult*cpstem*agflow*flxsec)*tc -pincir*deltaz*qs +0.0*ts=cpmult*cpstem*agflow*flxsec*templo
    ! htc*tc+qs-htc*ts = 0.0
    ! 0.0*tc+qs-aa*ts  = bb
    ! If virtually no flow, assume htc=0.0 and solve adiabatic heatup case
    IF (agflow < gmin) THEN
        tsurf = -bb / aa
        tcc = tsurf
        htc = 0.0_r8k
        qcrit = -5.0_r8k
        qq = 0.0_r8k
        WRITE (0,12) time
12      FORMAT ('Virtually no flow - adiabatic heatup assumed at time ',e12.4,', seconds.')
        RETURN
    ENDIF
    ! Set up the two solution passes with simq
    npass   = ione
10  CONTINUE
    IF (npass == ione) htc = htcflc
    IF (npass == itwo) htc = htcdtb
    as(1) = cpmult * cpstem * agflow * flxsec
    as(2) = htc
    as(3) = 0.0_r8k
    as(4) = -pincir * deltaz
    as(5) = 1.0_r8k
    as(6) = 1.0_r8k
    as(7) = 0.0_r8k
    as(8) = -htc
    as(9) = -aa
    !
    bs(1) = cpmult * cpstem * agflow * templo * flxsec
    bs(2) = 0.0_r8k
    bs(3) = bb
    nsimq = ithree
    ks = izero
    !
    CALL simq ( as, bs, nsimq, ks )
    !
    IF (ndebug) WRITE(ounit,911) npass,bs(1),bs(2),bs(3)
911 FORMAT(' for simq solution #',i2, ' tcc= ',e11.4,' qq= ',e11.4,' tsurf = ',e11.4)
    ! Check for solution failure
    IF (ks /= izero) THEN
        WRITE(ounit,20)
20      FORMAT(' solution failure in enrise execution stopped')
        STOP
    ENDIF
    IF (npass == itwo) GOTO 30
    ! Otherwise
    cs(1) = bs(1)
    cs(2) = bs(2)
    cs(3) = bs(3)
    ! Set up second pass
    npass = itwo
    GOTO 10
30  CONTINUE
    ! Pick out the smaller heat flux and RETURN the corresponding values
    IF (cs(2) <= bs(2)) THEN
        ! Otherwise  cs(2)  <=  bs(2)  and  RETURN  the  cs  values for htc = htcflc
        tcc = cs(1)
        qq = cs(2)
        tsurf = cs(3)
        qcrit = -3.0_r8k
        htc = htcflc
    ELSE
        ! The second pass values provide the smaller heat flux with htc = htcdtb. RETURN the bs values
        tcc = bs(1)
        qq = bs(2)
        tsurf = bs(3)
        qcrit = -4.0_r8k
        htc = htcdtb
    ENDIF
    !
      END SUBROUTINE enrise
!
!
    FUNCTION fiddle (fldrte, AxNodElevat, rupflg, MassFlowRate, blkagp, ruplev)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    USE conversions_fraptran, ONLY : pi
    IMPLICIT NONE
    !>@brief
    !> this Subroutine adjusts the mass flow of the reflood steam as per the licencing specifications of october 1977.
    !> The model is more or less that of toodee2 as recommEnded by norm laubin
    !> It checks the rupture flag. If no rupture inoicated, RETURN the flow
    !>@author
    !> Modified by Ian Porter, NRC, April 2014 to clean coding and convert to .f90
    INTEGER(ipk) :: kdbug
    REAL(r8k) :: fiddle, fudge1, fudge2, blkage
    REAL(r8k), INTENT(IN) :: MassFlowRate, AxNodElevat, fldrte, blkagp, ruplev
    CHARACTER(LEN=8), INTENT(IN) :: rupflg
    CHARACTER(LEN=8), PARAMETER :: off = 'off'
    ! convert blockage from percentage to fraction
    blkage = blkagp / 100.0_r8k
    !
    IF (rupflg == off .OR. fldrte > 1.0_r8k) THEN
        fiddle = MassFlowRate
        GOTO 100
    ENDIF
    !
    kdbug=0
    IF (kdbug == 1) WRITE(ounit,901)AxNodElevat,ruplev,blkage,fldrte,MassFlowRate
901 FORMAT(' in fiddle, AxNodElevat=',e11.4,' ruplev=',e11.4,' blkage=',e11.4,' fldrte=',e11.4,' MassFlowRate=',e11.4)
    ! otherwise the pin has failed and the flow is partially blocked
    ! If the elevation is less than ruplev - .91667 no modification is made
    IF (AxNodElevat <=  ruplev - 0.91667_r8k) THEN
        fiddle = MassFlowRate
        GOTO 100
    ENDIF
    ! otherwise the elevation extEnds into the flow blockage region
    fudge1 = 1.01_r8k - 0.51_r8k * fldrte
    fudge2 =  1.0_r8k - (1.0_r8k - fudge1) * AxNodElevat / ruplev
    IF (AxNodElevat > ruplev) fudge2 = fudge1
    IF (AxNodElevat > (ruplev + 2.0_r8k )) fudge2 = fudge2 + (1.0_r8k - fudge2) * (AxNodElevat - &
      &                                             ruplev - 2.0_r8k) ** 2 / (10.0_r8k - ruplev)
    ! set adjusted flow
    IF (AxNodElevat > (ruplev - 0.91667_r8k)) fiddle = fudge2 * MassFlowRate * (blkage * 0.5_r8k * &
      &                                                (SIN(pi * (ruplev - AxNodElevat - 0.41667_r8k)) &
      &                                                -1.0_r8k) + 1.0_r8k)
    ! If (AxNodElevat > (ruplev + .58333))
    fiddle = fudge2 * MassFlowRate * ( 1.0_r8k - blkage * 0.5_r8k * EXP(2.0_r8k * &
      &      (ruplev - AxNodElevat + 0.58333_r8k)))
    !
100 CONTINUE
    ! The current reflood model does not gracefully handle reverse or negative flow rates.
    ! Therefore it will reset itself to 0.0_r8k
    IF (fiddle < 0.0_r8k) fiddle = 0.0_r8k
    ! prevent increase of mass flux
    IF (fiddle > MassFlowRate) fiddle = MassFlowRate
    IF (kdbug == 1) WRITE(ounit,905) fiddle, fudge1, fudge2
905 FORMAT (' fiddle=',e11.4,' fudge1=',e11.4,' fudge2=',e11.4)
    !
    END FUNCTION fiddle
!
!
    SUBROUTINE hcalf (tinit, qlocal, z, h, acond, pdecay, deltat, mode, RodLength, Nchan)
    USE Kinds
    USE conversions_fraptran, ONLY : sechr
    USE Functions, ONLY : polate
    USE CoolantProperties
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calculates cladding surface heat transfer coefficient during reflooding of core
    !> according to Bob Lin's generalized version of flecht
    !
    ! Input
    !
    ! dtsub  - subcooling of flood water at inlet, F
    ! vin    - flooding rate (inches/s)
    ! p      - reactor vessel internal pressure, psia
    ! tinit  - cladding surface temperature at start of reflooding of core at elevation of peak power, F
    ! qmax   - maximum fuel rod power at start of reflood
    ! qlocal - qmax in current version. when effect of radial power profile modeled, qlocal dIfferent from qmax. kW/ft
    ! z      - equivalent elevation in flecht test that gives same integral of power with z as nuclear rod, ft
    ! b      - flow blockage
    ! tflood - time since start of reflood, s
    ! hliq   - height of liquid level above bottom of core assuming no carry out, ft
    ! hrad   - radiation heat transfer coefficient, btu/hr-ft2-F
    !
    ! Output
    !
    ! h      - heat transfer coefficient at cladding surface, btu/hr-ft2-F
    !
    INTEGER(ipk) :: npax = 1, nqax = 1, ntemp = 1, nchan, ivq, k, mode
    REAL(r8k) :: p, dtsub, vin, tsat, sig, b, shift, xi, tp, z, f11, e, fttq3, ett, tinit, dtshif, &
      &          f1, f4, f2, f3, f5, f61, ptq6ft, tq6ft, etq6ft, qlocal, h0, h1, h12a, h12b, &
      &          h12c, h12d, h12, xa, xb, xab, fqmax5, fqmax1, fqmax2, fvin, ttq2, ttq3, x2, h2, &
      &          fc, b1amp, b1a, b1b, b1c, b1d, b1e, b1, b2a, b2b, b2c, b2, b3, b4, y3, h3, c, f, &
      &          h00, h, ttq, tt2, xx, u, hb, hc, ha, y, ex1, qmaxfc, mr, hg, volg, rhog, volf, &
      &          rhof, rhogf, ct, cpf, hf, hfg, visf, a, de, dr, rcpaf, qdnb, epso, &
      &          deltat, rodlength, dzq, fax, qax, qeq1, qeq, tinite, re, fdtsub, fvin2, fvin3, &
      &          fvin4, fvin5, fp1, fp2, fp3, fp4, ft1, ft2, fvsub, qdls, cq, fvq1, fvq2, fvq, fq, &
      &          tq, fr1, fr2, qr, pdecay, zq1, tq1, zq2, tq2, vq, dtc, dte, te, tez, qeffz, fh, &
      &          tinitz, kf, kg, nu
    REAL(r8k), DIMENSION(30,10) :: acond
    !
    p = pressr
    dtsub = tsub
    vin = fldrte
    tsat = tsatt
    !
    CALL surten (tsat, sig)
    !
    IF (mflt == 1) GOTO 66
    b = flowbk
    b = 0.0_r8k
    !
    IF (vin < 0.4_r8k ) vin = 0.4_r8k
    IF (vin > 10.0_r8k) vin = 10.0_r8k
    shift = hliq * 12.0_r8k
    xi = shift / vin - tflood
    IF (vin > 1.0_r8k) THEN
        ! Adjusted time convolution
        tp = tflood + (0.214_r8k * z - 0.386_r8k) * xi
    ELSE
        tp = tflood + 224.0_r8k * (1.0_r8k - (1.0_r8k + 0.01528_r8k * xi) * EXP(-0.01389_r8k * xi)) + &
          &  (2.2_r8k * (z ** 2.13_r8k) - 100.0_r8k) * (1.0_r8k - EXP(-0.01416_r8k * xi) - &
          &  0.01331_r8k * xi * EXP(-0.0002_r8k * xi * xi))
    ENDIF
    IF (tp < 1.0e-5_r8k) tp = 1.0e-5_r8k
    f11 = 0.3_r8k
    e = 60.0_r8k
    fttq3 = 0.26_r8k
    ett = EXP(-2.77e-6_r8k * tinit ** 2)
    dtshif = 274.0_r8k * EXP(-0.0034_r8k * tinit) * EXP(-0.465_r8k * vin) * &
     &       EXP(-1.25_r8k * qmax) / (1.0_r8k + 50.0_r8k ** (-0.2_r8k * (p - 30.0_r8k)))
    f1 = EXP(-0.0107_r8k * dtsub) * (1.0_r8k - EXP(-0.667_r8k * vin))
    f4 = 1.0_r8k + 0.5_r8k * EXP(-0.000037_r8k * p ** 3)
    f2 = f4 + 1.3_r8k * EXP(-0.111_r8k * vin ** 2) + 17.3_r8k * EXP(-.000037_r8k * p ** 3) * &
      &  EXP(-0.49_r8k * vin ** 2)
    f3 = 3.28_r8k / vin ** 1.1_r8k - 2.8_r8k * EXP(-vin)
    f5 = 1.0_r8k + 0.0000588_r8k * tinit - 1.05_r8k * EXP(-.0025_r8k * tinit) * &
      &  (1.0_r8k + 0.5_r8k / (1.0_r8k + 50.0_r8k ** (2.0_r8k - 0.667_r8k * vin))) * &
      &  (1.0_r8k + 0.32_r8k / (1.0_r8k + 50.0_r8k ** (5.0_r8k - 0.1_r8k * p)))
    ! Note: relap4 multiplies qmax by kpower, which adjusts for difference in reactor decay heat 
    ! and decay heat simulated in flecht facility.
    f61 = 1.207_r8k * qmax ** 1.5_r8k - 0.667_r8k
    ! Compute 6-foot quench time correlation
    ptq6ft = 98.39_r8k * (f1 * f2 * f61 + f3 * f4) * f5
    tq6ft = ptq6ft
    etq6ft = tq6ft - dtshif
    IF (ABS(qlocal) < 0.0001_r8k) qlocal = qmax
    h0 = hrad
    h1 = h0 + 0.0397_r8k * (1.0_r8k - EXP(-0.0025_r8k * dtshif ** 2)) * qmax * (tinit - 100.0_r8k)
    h12a = 22.0_r8k - 0.00303_r8k * z ** 4.1_r8k
    h12b = 1.0_r8k - EXP(-0.0333_r8k * p) - 0.034_r8k * p * EXP(-0.0011_r8k * p ** 2)
    h12c = 1.0_r8k - EXP(-0.2_r8k * vin)
    h12d = 8.0_r8k * (1.0_r8k - EXP(-vin / 0.5_r8k)) * (1.0_r8k - EXP(-b / 25.0_r8k))
    h12 = 2.644_r8k + 1.092_r8k * qmax
    h12 = h12 + (35.7_r8k + h12a * h12b) * h12c + h12d
    xa = 1.0_r8k + 4.37_r8k * EXP(-0.0166_r8k * dtsub)
    xb = 1.0_r8k - EXP(-(0.00075_r8k + 0.0000272_r8k * (vin - 8.0_r8k) * (vin - 8.0_r8k)) * &
      &  (0.5_r8k * ((tinit - 1000.0_r8k) + SQRT(tinit ** 2 - 2000.0_r8k * tinit + 1000100.0_r8k)) + 350.0_r8k))
    xab = 17.6_r8k * xa * xb
    fqmax5 = qmax + (1.24_r8k - qmax) / (1.0_r8k + 50.0_r8k ** (5.0_r8k - 2.0_r8k * vin))
    fqmax1 = 0.436_r8k + 0.455_r8k * fqmax5
    fqmax2 = 0.564_r8k - 0.455_r8k * fqmax5
    fvin = 2.8_r8k - 4.8_r8k * EXP(0.668_r8k - 1.67_r8k * vin)
    ttq2 = 0.62_r8k * ((1.0_r8k - EXP(-0.192_r8k * z)) - 0.115_r8k * z * EXP(-0.0368_r8k * z ** 2))
    ttq3 = 1.55_r8k * ((1.0_r8k - EXP(-0.205_r8k * z)) - 0.154_r8k * z * EXP(-0.0421_r8k * z ** 2))
    ttq3 = ttq3 + fttq3 * ett
    x2 = xab * ttq2 * fqmax1
    h2 = h12 * ((1.0_r8k - EXP(-x2)) - 0.9_r8k * x2 * EXP(-x2 * x2))
    fc = 1.0_r8k - EXP(-(0.026_r8k * p + 1.041_r8k * vin + 10.28_r8k * EXP(-3.01_r8k * qmax) - 0.651_r8k))
    b1amp = EXP(-0.8503_r8k * z * z + 1.0986123_r8k * z + 2.3025851_r8k ) + 1.0_r8k
    b1a = (682.0_r8k - 650.0_r8k * (1.0_r8k - EXP(4.0_r8k - z))) * b1amp
    b1b = 1.0_r8k - EXP(-0.95_r8k * (1.0_r8k - 0.0488_r8k * z) * vin)
    b1c = 1.0_r8k - EXP(-0.0238_r8k * dtsub)
    b1d = 0.696_r8k + 0.304_r8k * EXP(-b / 25.0_r8k)
    b1e = 1.0_r8k + 0.2_r8k * (1.0_r8k - fc)
    b1 = b1a * b1b * b1c * b1d * b1e
    b2a = 1.0_r8k - EXP(-2.0_r8k * (z - 3.5_r8k))
    IF (z < 4.0_r8k) b2a = 0.0_r8k
    b2b = 1.33_r8k * (1.0_r8k - EXP(-0.0227_r8k * p)) - 1.0_r8k
    b2c = 2.9_r8k * (1.0_r8k - EXP(-vin / 2.5_r8k)) * (1.0_r8k - EXP(-b / 25.0_r8k))
    b2 = 0.4_r8k * z * b2a * b2b - b2c
    b3 = 2.55_r8k * (z - 3.7_r8k) * (z - 3.7_r8k) * EXP(3.7_r8k - z)
    IF (z < 4.0_r8k) b3 = 0.0_r8k
    b4 = 87.5_r8k * vin * EXP(-vin * vin) * EXP(-0.036_r8k * dtsub)
    y3 = ttq3 - ttq2
    h3 = h2 + b1 * (y3 * y3 + b2 * (y3 ** 2 - b3 * y3 ** 3))
    h3 = h3 + b1 * b4 * y3 * y3 * EXP(-6.38_r8k * y3)
    c = 420.0_r8k * (1.0_r8k - EXP(-0.00625_r8k * b1)) * fc
    f1 = f11
    f2 = f1 + (1.0_r8k - f1) / (1.0_r8k + 50.0_r8k ** (2.0_r8k - vin))
    f  = f2 + (1.0_r8k - f2) / (1.0_r8k + 50.0_r8k ** (z - 7.0_r8k))
    h00 = f * h0
    IF (tp >= dtshIf) GOTO 8
    h = h00 + 0.0397_r8k * qmax * (tinit - 100.0_r8k) * (1.0_r8k - EXP(-0.0025_r8k * tp ** 2))
    GOTO 40
8   CONTINUE
    ttq = (tp - dtshif) / etq6ft
    IF (ttq > ttq2) GOTO 26
    tt2 = ttq / ttq2
    xx = xab * ttq * fqmax1 * tt2 ** fqmax2
    u = 3.0_r8k * fqmax1 * tt2 ** (1.0_r8k + fqmax2 * fvin)
    u = u * u
    hb = (1.0_r8k - EXP(-xx)) - 0.9_r8k * xx * EXP(-xx * xx)
    hc = 1.0_r8k - 2.21_r8k * EXP(-0.4_r8k * vin) * u * EXP(-u) * EXP(-(0.588_r8k * z - 3.824_r8k) ** 2) &
      &  / (1.0_r8k + 100.0_r8k ** (10.0_r8k * ttq / ttq2 - 9.0_r8k))
    ha = h1 * (1.0_r8k - hb * hc) * (1.0_r8k - EXP(-10.0_r8k * (x2 - xx) / x2))
    h = ha + h12 * hb * hc
    GOTO 30
26  IF (ttq > ttq3) GOTO 28
    y = ttq - ttq2
    h = h2 + b1 * (y ** 2 + b2 * (y ** 2 - b3 * y ** 3))
    h = h + b1 * b4 * y ** 2 * EXP(-6.38_r8k * y)
    h = h + e * ett * y/y3 * EXP(-2.25_r8k * (y / y3) ** 2)
    GOTO 30
28  h = h3 + c * (ttq - ttq3)
30  CONTINUE
    f1 = f11 + (1.0_r8k - f11) * (1.0_r8k - EXP(-1.5_r8k * ttq))
    f2 = f1 + (1.0_r8k - f1) / (1.0_r8k + 50.0_r8k ** (2.0_r8k - vin))
    f = f2 + (1.0_r8k - f2) / (1.0_r8k + 50.0_r8k ** (z - 7.0_r8k))
    h = f * h
    ex1 = (ttq2 - ttq) * etq6ft
    IF (ex1 < (-2.0_r8k)) GOTO 40
    IF (ex1 > 2.0_r8k) h = 0.978_r8k * h
    IF (ex1 > 2.0_r8k) GOTO 40
    h = h * (0.978_r8k + 0.022_r8k / (1.0_r8k + 30.0_r8k ** ((ttq2 - ttq) * etq6ft)))
40  CONTINUE
    IF (h > 50.0_r8k) h = 50.0_r8k
    GOTO 68
66  CONTINUE
    !**************************************************
    !* flt-c-set reflood heat transfer correlation    *
    !* reference the 161 rod unblocked bundle Data    *
    !* analysis and evaluation report             *
    !* the correlation was adapted to frap-t6 by    *
    !* d. m. ogden                        *
    !**************************************************
    qmaxfc = qmax
    mr = 2
    hg = acond(23,Nchan)
    volg = acond(25,Nchan)
    rhog = 1.0_r8k/volg
    volf = acond(24,Nchan)
    rhof = 1.0_r8k/volf
    rhogf = rhog/rhof
    ct = (tinit - tsat) / (500.0_r8k - tsat)
    cpf = acond(26,Nchan)
    hf = acond(22,Nchan)
    hfg = hg - hf
    visf = visc(1,tsat,rhof) / sechr
    kf = thcon(1,tsat,rhof) / sechr
    kg = thcon(2,tsat,rhog) / sechr
    a = 0.00123_r8k
    de = 0.04451_r8k
    dr = 0.03517_r8k
    IF (mbdl == 17) THEN
        de = 0.03863_r8k
        dr = 0.03117_r8k
        a = 0.0009455_r8k
    ENDIF
    rcpaf = rhof * cpf * a
    !
    CALL surten (tsat, sig)
    !
    b = 0.00353_r8k * p ** 0.187_r8k
    qdnb = sechr * 0.13_r8k * hfg * SQRT(rhog) * (sig * (rhof- rhog) * 1035.0_r8k) ** 0.25_r8k * SQRT((rhof / (rhof + rhog)))
    !
    vin = MAX(0.4_r8k, polate (fldrat, toldfc, nbrfdr))
    !
    epso = deltat / 100.0_r8k
    IF (mzq1 /= 1) GOTO 500
    toldfc = tflood
    zqflt = zqch
    IF (zqflt > RodLength) zqflt = RodLength
    GOTO 1005
500 If (ABS(toldfc - tflood) <= epso .OR. tflood == 0.0_r8k) GOTO 1005
15  CONTINUE
    dzq = 0.005_r8k
19  CONTINUE
    !**************************************************
    !* compute quench front elevation             *
    !**************************************************
    zqflt = zqflt + dzq
    IF (zqflt > RodLength) zqflt = RodLength
    IF (zqflt >= RodLength) GOTO 1005
    DO ivq = 1, 2
        IF (ivq == 1) zqflt = zqflt - 0.0005_r8k
        IF (ivq == 2) zqflt = zqflt + 0.0005_r8k
        IF (zqflt <= 0.0_r8k) zqflt = 0.0_r8k
        !
        fax = polate (faxzq, zqflt, npaxpf, npax) * pavgft / qmaxfc
        !
        qax = polate (qaxzq, zqflt, npaxpf, nqax) * pavgft / qmaxfc
        !
        qeq1 = qmaxfc
        IF (mr == 2) qeq1 = qeq1 * 1.1_r8k
        qeq = qeq1
        tinite = (tinit - tsat) * fax + tsat
        !
        vin = MAX(0.4_r8k, polate (fldrat, toldfc, nbrfdr))
        !
        re = vin / 12.0_r8k * rhof * de / visf
        fdtsub = EXP(-10.09_r8k * (cpf * dtsub / hfg))
        fvin2 = 1.3_r8k * EXP(-1.652e-9_r8k * re ** 2 / rhogf ** 0.524_r8k)
        fvin3 = EXP(-7.293e-9_r8k * re ** 2 / rhogf ** 0.524_r8k)
        fvin4 = 66203.0_r8k * rhogf ** 0.2882_r8k / re ** 1.1_r8k - 2.8_r8k * &
          &     EXP(-0.000122_r8k * re / rhogf ** 0.262_r8k)
        fvin5 = 1.0_r8k + 0.5_r8k / (1.0_r8k + 50.0_r8k ** (2.0_r8k - 0.00008137_r8k * &
          &     re / rhogf ** 0.262_r8k))
        fp1 = 1.0_r8k + 0.5_r8k * EXP(-5.6251e8_r8k * rhogf ** 3)
        fp2 = 17.3_r8k * EXP(-5.6251e8_r8k * rhogf ** 3)
        fp3 = fp1
        fp4 = 1.0_r8k + 0.32_r8k / (1.0_r8k + 50.0_r8k ** (5.0_r8k - 2520.0_r8k * rhogf))
        ct  = (tinite - tsat) / (500.0_r8k - tsat)
        ft1 = 1.01552_r8k + 0.01388_r8k * ct
        ft2 = 1.05_r8k * EXP(-0.66_r8k - 0.59_r8k * ct)
        fvsub = 0.3_r8k + 0.7_r8k * (1.0_r8k - EXP(-10.31e-8_r8k * re ** 2 / rhogf ** 0.524_r8k)) - &
          &     2.9e-11_r8k * re ** 2 * re / rhogf ** 0.786_r8k * EXP(-9.3e-8_r8k * re ** 2 / &
          &     rhogf ** 0.524_r8k) / (1.0_r8k + 50.0_r8k ** (-15.75_r8k * (cpf * dtsub / hfg) + 1.333_r8k))
        DO k = 1, 3
            qdls = 0.9481_r8k * qaxpk / rhof / a / vin * 12.0_r8k / hfg
            cq = qeq * qdls
            fvq1 = -0.7_r8k * (1.0_r8k - EXP(-0.0000801_r8k * re / rhogf ** 0.262_r8k))
            fvq2 = 6.458e-5_r8k * re ** 1.938_r8k / rhogf ** 0.5078_r8k * (cq * dr / zpkfc) ** 1.5_r8k
            fvq = fvq1 + fvq2
            fq  = 1.0_r8k - 0.16_r8k / (1.0_r8k + 70.0_r8k ** (1250.0_r8k * (dr / zpkfc) - 5.45_r8k)) / &
              &   (1.0_r8k + 80.0_r8k ** (7.14_r8k * cq - 4.93_r8k))
            tq  = (fdtsub * fvq * (fp1 + fvin2 + fp2 * fvin3) + fvin4 * fp3) * (ft1 - ft2 * fvin5 * fp4) * fvsub * fq
            tq  = zpkfc / vin * 0.00228_r8k * re * rhogf ** (-0.262_r8k) * tq
            fr1 = 0.5_r8k
            fr2 = 9.0_r8k
            qr  = qax / qaxpk
            fq  = qr + fr1 * qr * EXP(-fr2 * qr * qr)
            tq  = tq * fq
            tq  = zqflt / vin * 12.0_r8k + (tq - zqflt / vin * 12.0_r8k) / (1.0_r8k + &
              &   50.0_r8k ** (-(tinite - 400.0_r8k) / (400.0_r8k - tsat)))
            IF (tq < (zqflt / vin * 12.0_r8k)) tq = zqflt / vin * 12.0_r8k
            qeq = qeq1 * pdecay
        ENDDO
        SELECT CASE (ivq)
        CASE (1)
            zq1 = zqflt
            tq1 = tq
        CASE (2)
            zq2 = zqflt
            tq2 = tq
        END SELECT
    ENDDO
    vq = (zq2 - zq1) / (tq2 - tq1)
    IF (vq < 0.0_r8k .AND. zqflt < zpkfc) vq = ABS(vq)
    IF (vq < 0.0_r8k) zqflt = RodLength
    toldfc = toldfc + dzq/vq
    IF (ABS(toldfc - tflood) <= epso .OR. tflood == 0.0_r8k .OR. zqflt >= RodLength) GOTO 1005
    IF (toldfc < tflood) GOTO 15
    zqflt = zqflt - dzq
    toldfc = toldfc - dzq / vq
    dzq = dzq / 2.0_r8k
    GOTO 19
    !**************************************************
    !*  compute heat transfer coefficient         *
    !**************************************************
1005 CONTINUE
    dtc = 800.0_r8k
    dte = dtc / (1.0_r8k + 60.0_r8k ** (1.08_r8k * (tinit - tsat) / dtc - 1.26_r8k))
    te = tinit + dte
    !
    fax = polate (faxzq, zqflt, npaxpf, npax) * pavgft / qmaxfc
    !
    tez = tsat + (te - tsat) * fax
    qeffz = 0.7_r8k * fax * 0.9481_r8k
    !
    vin = MAX(0.4_r8k, polate (fldrat, toldfc, nbrfdr))
    !
    re = vin / 12.0_r8k * rhof * de / visf
    fh = 1.0_r8k / (1.0_r8k + 70.0_r8k ** (1.0_r8k - 0.0133_r8k * (zpkfc / dr)))
    IF (izadfg == 1) GOTO 600
    zs = 6329.0_r8k * (re + 4000.0_r8k) ** (-1.468_r8k) * vin / 12.0_r8k * rhof * cpf * de ** 2 / kf * fh
    zad = 51.0_r8k * rcpaf * dtsub * vin / 12.0_r8k / qmaxfc / 0.9481_r8k / rhof / cpf - 0.234_r8k &
      &   * rcpar * (tinit - tsat) * vin / 12.0_r8k / qmaxfc / 0.9481_r8k + 1.147_r8k * fh
    IF (zad <= 0.0_r8k) zad = 0.0_r8k
600 CONTINUE
    !
    fax = polate (faxzq, z, npaxpf, npax) * pavgft/qmaxfc
    !
    tinitz = polate (tempfc, z, ntempf, ntemp)
    !
    h1 = 0.215_r8k * qmaxfc * 0.9481_r8k * fax / rcpar * (1.0_r8k - EXP(-(tinitz - 700.0_r8k) / 435.0_r8k))
    IF (tinitz < 700.0_r8k) h1 = 0.0_r8k
    h = h1
    nu1 = h1 / sechr * de / kg
    xx = 4.0_r8k * (zqflt - zad) / zs
    h3 = qeffz / (tez - tsat) / dr * 1.21_r8k * (1.0_r8k - EXP(-0.0000305_r8k * re / rhogf ** 0.262_r8k)) &
      &  * (0.714_r8k + 0.286_r8k * (1.0_r8k - EXP(-3.05e-4_r8k * rhogf ** 1.524_r8k / re / re)))
    nu3 = h3 * de / kg
    nu2 = nu3 + 108.0_r8k * EXP(-0.0000183_r8k * re / rhogf ** 0.262_r8k) * EXP(-0.0534_r8k * (z - zqflt) / de)
    IF (zqflt <= zad) nu = nu1
    IF (zqflt < (zs + zad) .AND. zqflt >= zad) nu = nu1 * (1.0_r8k - EXP(2.5_r8k * xx - 10.0_r8k)) + &
      &                                             (nu2 - nu1 * (1.0_r8k - EXP(2.5_r8k * xx - 10.0_r8k))) * &
      &                                             (1.0_r8k - EXP(-xx) - 0.9_r8k * xx * EXP(-xx * xx))
    IF (zqflt >= (zs + zad)) nu = nu2
    IF (z > zpkfc) nu = nu - 44.2_r8k * (1.0_r8k - fax) * EXP(-0.00304_r8k * (z - zpkfc) / de)
    h = nu * kg * sechr / de
    IF (zqflt >= z) h = h + qdnb * b * EXP(1.0_r8k - b * (trodfc - tsat))
    IF (h > 2000.0_r8k) h = 2000.0_r8k
    IF (zqflt <= zad) mode = 1
    IF (zqflt < (zs + zad) .AND. zqflt >= zad) mode = 2
    IF (zqflt >= (zs + zad)) mode = 3
    IF (zqflt >= z) mode = 4
    IF (h >= 2000.0_r8k) mode = 5
    IF (z > zpkfc) mode = 10 * mode
68  CONTINUE
    !
    END SUBROUTINE hcalf
!
!
    SUBROUTINE tailnd (a, b, htc, temp, tsurf, qq, qcrit)
    USE Kinds
    USE conversions_fraptran, ONLY : sechr
    IMPLICIT NONE
    !>@brief
    !> Subroutine tailnd calculates the tail end of the heat conduction calculation. It is part of the reflood package.
    !>@author
    !
    !
    ! Input
    !
    ! a
    ! b
    ! htc
    ! temp
    !
    ! Output
    !
    ! tsurf
    ! qq
    ! qcrit
    !
    REAL(r8k), INTENT(IN) :: a, b, htc, temp
    REAL(r8k), INTENT(OUT) :: tsurf, qq, qcrit
    REAL(r8k) :: aa, bb
    !
    aa = a * sechr
    bb = b * sechr
    ! It solves the following two equations
    tsurf = (bb + htc * temp) / (htc - aa)
    !
    qq = (tsurf - temp) * htc
    ! and sets the qcrit flag to -3.0
    qcrit = -3.0_r8k
    !
    END SUBROUTINE tailnd
!
!
    SUBROUTINE wcorr (dtsub, p, tinit, qmax, AxNodElevat, hrad, floodr, Time, hliq, flecht)
    USE Kinds
    USE Functions, ONLY : polate
    IMPLICIT NONE
    !>@brief
    !> Subroutine to evaluate pwr flecht heat transfer correlation
    !
    ! Input
    !
    ! Time        - total time (sec)
    ! xi          - 
    ! AxNodElevat - elevation (ft)
    ! dtsub       - inlet coolant subcooling (F)
    ! p           - system pressure (psia)
    ! tinit       - initial clad temperature (F)
    ! qmax        - peak power density (kW/ft)
    ! i           - time step number
    ! hrad        - initial heat transfer coefficient
    !
    ! Output
    !
    ! flecht      - heat transfer coefficient (btu/hr/ft**2)
    ! vin         - flooding rate (in/sec)
    ! hrad        - radiation heat transfer coeff (input or computed)
    ! h           - heat transfer coefficient (btu/hr/ft**2)
    ! AxNodElevat - elevation for flecht ht correlation
    ! qmax        - power input - flecht ht correlation
    ! dtsub       - subcooling tempperature (F) - flecht ht correlation
    ! tinit       - initial cladding temperature (F) - flecht ht correlation
    ! b           - blockage - flecht ht correlation
    !
    ! limit vin -- .4 le vin le 10.0
    !
    INTEGER(ipk) :: ix, iu, i, j
    REAL(r8k) :: b, vin, f1, f2, f3, f4, f5, f61, tq, shift, xi, tp, ttq2, ttq3, h12, h12a, h12b, &
      &          h12c, h12d, xa, xb, xab, x2, h2, b1a, b1b, b1c, b1d, b1, b2a, b2b, b2c, b2, b3, &
      &          b4, y3, h3, c, ttq, x, u, hb, hc, ha, h, zz, y
    REAL(r8k), INTENT(IN) :: hliq, Time, floodr, qmax, tinit, p, dtsub, hrad  !Check to see if hrad should be (inout) or just (out)
    REAL(r8k), INTENT(OUT) :: flecht
    REAL(r8k), INTENT(INOUT) :: AxNodElevat
    REAL(r8k), DIMENSION(14) :: ftl, fth
    !
    REAL(r8k), DIMENSION(9,7), PARAMETER :: fxl = reshape([ &
      &     4.14_r8k, 5.69_r8k, 3.89_r8k, 2.97_r8k, 3.52_r8k, 2.77_r8k, 3.20_r8k, 1.00_r8k, 1.00_r8k, &
      &     4.14_r8k, 5.69_r8k, 3.89_r8k, 2.97_r8k, 3.52_r8k, 2.77_r8k, 3.20_r8k, 1.00_r8k, 1.00_r8k, &
      &     0.93_r8k, 1.05_r8k, 1.03_r8k, 1.17_r8k, 1.58_r8k, 1.40_r8k, 1.49_r8k, 0.82_r8k, 0.55_r8k, &
      &     1.21_r8k, 1.25_r8k, 0.97_r8k, 0.94_r8k, 1.13_r8k, 1.05_r8k, 1.01_r8k, 0.94_r8k, 0.77_r8k, &
      &     1.05_r8k, 1.03_r8k, 0.75_r8k, 0.87_r8k, 0.84_r8k, 0.75_r8k, 0.92_r8k, 1.00_r8k, 0.79_r8k, &
      &     0.69_r8k, 0.43_r8k, 0.48_r8k, 0.55_r8k, 0.51_r8k, 0.64_r8k, 1.24_r8k, 1.68_r8k, 1.10_r8k, &
      &     0.69_r8k, 0.43_r8k, 0.48_r8k, 0.55_r8k, 0.51_r8k, 0.64_r8k, 1.24_r8k, 1.68_r8k, 1.10_r8k ], [9, 7])
    REAL(r8k), DIMENSION(9,7), PARAMETER :: fxh = reshape([ &
      &     6.07_r8k, 4.35_r8k, 2.85_r8k, 2.34_r8k, 3.20_r8k, 2.94_r8k, 2.97_r8k, 4.90_r8k, 3.17_r8k, &
      &     6.07_r8k, 4.35_r8k, 2.85_r8k, 2.34_r8k, 3.20_r8k, 2.94_r8k, 2.97_r8k, 4.90_r8k, 3.17_r8k, &
      &     1.17_r8k, 0.86_r8k, 0.92_r8k, 1.10_r8k, 1.45_r8k, 1.37_r8k, 1.40_r8k, 1.58_r8k, 1.68_r8k, &
      &     0.97_r8k, 0.98_r8k, 0.83_r8k, 0.90_r8k, 1.06_r8k, 1.04_r8k, 1.14_r8k, 1.43_r8k, 1.33_r8k, &
      &     0.46_r8k, 0.54_r8k, 0.68_r8k, 0.68_r8k, 0.80_r8k, 0.88_r8k, 1.11_r8k, 1.15_r8k, 0.91_r8k, &
      &     0.15_r8k, 1.04_r8k, 0.29_r8k, 0.54_r8k, 0.75_r8k, 1.04_r8k, 1.28_r8k, 1.35_r8k, 1.27_r8k, &
      &     0.15_r8k, 1.04_r8k, 0.29_r8k, 0.54_r8k, 0.75_r8k, 1.04_r8k, 1.28_r8k, 1.35_r8k, 1.27_r8k ], [9, 7])
    ! Force blockage to be zero in flecht correlation use
    b = 0.0_r8k
    ix = 7
    iu = 1
    vin = floodr
    vin = MAX(0.4_r8k, vin)
    vin = MIN(10.0_r8k, vin)
    shift = hliq * 12.0_r8k
    xi = shift / vin - Time
    IF (vin <= 1.0_r8k) THEN
        tp = Time + 224.0_r8k * (1.0_r8k - (1.0_r8k + 0.01528_r8k * xi) * EXP(-0.01389_r8k * xi)) &
          &  + (2.2_r8k * (AxNodElevat ** 2.13_r8k) - 100.0_r8k) * (1.0_r8k - EXP(-.01416_r8k * xi) &
          &  - 0.01331_r8k * xi * EXP(-0.0002_r8k * xi ** 2))
    ELSE
        ! Adjusted time convolution
        tp = Time + (0.214_r8k * AxNodElevat - 0.386_r8k) * xi
    ENDIF
    ! Compute 6-foot quench time correlation
    f1 = EXP(-0.0107_r8k * dtsub) * (1.0_r8k - EXP(-0.667_r8k * vin))
    f4 = 1.0_r8k + 0.5_r8k * EXP(-0.000037_r8k * p ** 3)
    f2 = f4 + 1.3_r8k * EXP(-0.111_r8k * vin ** 2) + 17.3_r8k * EXP(-0.000037_r8k * p ** 3) &
      &  * EXP(-0.49_r8k * vin ** 2)
    f3 = 3.28_r8k / vin ** 1.1_r8k - 2.8_r8k * EXP(-vin)
    f5 = 1.0_r8k + 0.0000588_r8k * tinit
    f61= 1.207_r8k * qmax ** 1.5_r8k - 0.667_r8k
    tq = 98.39_r8k * (f1 * f2 * f61 + f3 * f4) * f5
    ! Heat transfer coefficient correlation
    ! Compute ttq2 and ttq3 for periods 1 and 2
    ttq2 = 0.62_r8k * ((1.0_r8k - EXP(-0.192_r8k * AxNodElevat)) - 0.115_r8k * AxNodElevat &
      &    * EXP(-0.0368_r8k * AxNodElevat * AxNodElevat))
    ttq3 = 1.55_r8k * ((1.0_r8k - EXP(-0.205_r8k * AxNodElevat)) - 0.154_r8k * AxNodElevat &
      &    * EXP(-0.0421_r8k * AxNodElevat * AxNodElevat))
    ! Heat transfer coefficient correlation
    h12a = 22.0_r8k - 0.00303_r8k * AxNodElevat ** 4.1_r8k
    h12b = 1.0_r8k - EXP(-0.0333_r8k * p) - 0.034_r8k * p * EXP(-0.0011_r8k * p ** 2)
    h12c = 1.0_r8k - EXP(-0.2_r8k * vin)
    h12d = 8.0_r8k * (1.0_r8k - EXP(-vin / 0.5_r8k)) * (1.0_r8k - EXP(-b / 25.0_r8k))
    h12  = 4.0_r8k + (35.7_r8k + h12a * h12b) * h12c + h12d
    ! Compute x2 for period 1
    xa = 1.0_r8k + 4.37_r8k * EXP(-0.0166_r8k * dtsub)
    xb = 1.0_r8k - EXP(-(0.00075_r8k + 0.0000272_r8k * (vin - 8.0_r8k) ** 2) *  (tinit - 650.0_r8k))
    xab= 17.6_r8k * xa * xb
    x2 = xab * ttq2
    ! Compute h2 for period 2
    IF (x2 - 13.1_r8k <= 0.0) THEN
        h2 = h12 * ((1.0_r8k - EXP(-x2)) - 0.9_r8k * x2 * EXP(-x2 ** 2))
    ELSE
        h2 = h12 *  (1.0_r8k - EXP(-x2))
    ENDIF
    ! Compute b1, b2, b3, b4 for period 2 and period 3
    b1a = 682.0_r8k - 650.0_r8k * (1.0_r8k - EXP(4.0_r8k - AxNodElevat))
    b1b = 1.0_r8k - EXP(-0.95_r8k * (1.0_r8k - 0.0488_r8k * AxNodElevat) * vin)
    b1c = 1.0_r8k - EXP(-0.0238_r8k * dtsub)
    b1d = 0.696_r8k + 0.304_r8k * EXP(-b / 25.0_r8k)
    b1  = b1a * b1b * b1c * b1d
    b2a = 1.0_r8k - EXP(-2.0_r8k * (AxNodElevat - 3.5_r8k))
    b2b = 1.33_r8k * (1.0_r8k - EXP(-0.0227_r8k * p)) - 1.0_r8k
    b2c = 2.9_r8k * (1.0_r8k - EXP(-vin / 2.5_r8k)) * (1.0_r8k - EXP(-b / 25.0_r8k))
    b2  = 0.4_r8k * AxNodElevat * b2a * b2b - b2c
    b3  = 2.55_r8k * (AxNodElevat - 3.7_r8k) **2 * EXP(3.7_r8k - AxNodElevat)
    IF (vin - 13.1_r8k <= 0.0) THEN
        b4 = 87.5_r8k * vin * EXP(-vin * vin) * EXP(-0.036_r8k * dtsub)
    ELSE
        b4 = 0.0_r8k
    ENDIF
    ! Compute y3 for period 3
    y3 = ttq3 - ttq2
    ! Compute h3 and c for period 3
    h3 = h2 + b1 * (y3 ** 2 + b2 * (y3 ** 2 - b3 * y3 ** 3))
    IF (6.38_r8k * y3 - 172.0_r8k <= 0.0_r8k) h3 = h3 + b1 * b4 * y3 ** 2 * EXP(-6.38_r8k * y3)
    c = 420.0_r8k * (1.0_r8k - EXP(-0.00625_r8k * b1))
    ttq = tp / tq
    IF (ttq <= ttq2) THEN
        !
        ! Period 1
        x = xab * ttq
        u = (9.0_r8k * ttq ** 2 / ttq2) / ttq2
        IF (x - 13.1_r8k <= 0.0) THEN
            hb = (1.0_r8k - EXP(-x)) - 0.9_r8k * x * EXP(-x * x)
        ELSE
            hb = (1.0_r8k - EXP(-x))
        ENDIF
        hc = 1.0_r8k - 2.21_r8k * EXP(-0.4_r8k * vin) * u * EXP(-u) * EXP(-(0.588_r8k * AxNodElevat - 3.824_r8k) ** 2)
        ha = hrad * (1.0_r8k - hb * hc) * (1.0_r8k - EXP(-10.0_r8k * (x2 - x) / x2))
        ! Heat transfer coefficient for period 1
        h = ha + h12 * hb * hc
        zz = 1.0_r8k
    ELSE IF (ttq <= ttq3) THEN
        !
        ! Period 2
        y = ttq - ttq2
        ! Heat transfer coefficient for period 2
        h = h2 + b1 * (y ** 2 + b2 * (y ** 2 - b3 * y ** 3))
        IF (6.38_r8k * y - 172.0_r8k <= 0.0_r8k) h = (h + b1 * b4 * y ** 2 * EXP(-6.38_r8k * y))
        zz = 2.0_r8k
    ELSE
        !
        ! Period 3
        ! Heat transfer coefficient for period 3
        h = h3 + c * (ttq - ttq3)
        zz = 3.0_r8k
    ENDIF
    i = 9
    IF (ttq < ttq2) i = 1 + INT(4.0_r8k * ttq / ttq2)
    IF (ttq < ttq3) i = 5 + INT(4.0_r8k * (ttq - ttq2) / (ttq3 - ttq2))
    DO j = 1, 7
        ftl(2*j-1) = fxl(i,j)
        fth(2*j-1) = fxh(i,j)
        fth(2*j) = 2 * (j - 1)
        ftl(2*j) = 2 * (j - 1)
    ENDDO
    h = h * 0.05_r8k * ((p - 20.0_r8k) * polate (fth, AxNodElevat, ix, iu) - (p - 40.0_r8k) * polate (ftl, AxNodElevat, ix, iu))
    AxNodElevat = zz
    h = MIN(50.0_r8k, h)
    flecht = h
    !
    END SUBROUTINE wcorr
!
!
END MODULE Reflood_Conditions

