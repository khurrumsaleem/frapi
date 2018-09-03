MODULE Coolant
    USE Kinds
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE cool (k, theta, RodLength, qcool, delz, dthr, tpo)
    USE Kinds
    USE Conversions, ONLY : sechr, psift2, tkf, tfr
    USE Functions, ONLY : polate
    USE Variables, ONLY : ounit, unit, ndebug, Time
    USE collct_h
    USE CoolantProperties
    USE resti_h
    USE heatconduction_h
    USE sth2x, ONLY : sth2x2, sth2x3, sth2x5
    USE Dyna_h, ONLY : CoolEnthalpy, CoolEnthalpy0, CoolDensity, CoolDensity0, CoolMassFlx
    IMPLICIT NONE
    !>@brief
    !> Subroutine specifies coolant conditions for each coolant subchannel at time, Time, and elevation, z
    !
    ! acndaz          - array storing azimuthal factors on coolant conditions. 
    !                   It is part of a1 array storage scheme - starting and ending indexes
    ! factor on mass flux   -  locations 1 - ithymx * nvol * 2
    ! time tc2 mass flux factor first , Then tc1 factors
    ! factor on enthalpy    - ithymx*nvol*2     - 2*ithymx*nvol * 2
    ! factor on temperature - 2 * (         )  -  3 * (          )
    ! upper bound of angle for which n-th t/h sector holds - 3 * (       )  -  3 * (      )  + ithymx
    ! ithymx          - number of t/h sectors (sum of dIfferent sectors touching each rod ) minus 1
    ! nvol            - number of t/h axial sectors  (assumed same for all rods
    !
    ! icglob(1,nconn) - global index in acndaz for 1,nconn pairing
    ! theta           - azimuthal position  (degrees)
    ! RodLength       - fuel stack length (ft)
    ! CoolEnthalpy0   - beginning of time step enthalpy for all axial nodes (btu/lbm)
    ! CoolDensity0    - beginning of time step density for all axial nodes (lbm/ft**3)
    ! qcool           - total average volumetric heat source (btu/hr*ft**3)
    !                   qcool includes clad surface heate flux and direct energy deposition in the coolant for node k
    ! delz            - node length (ft)
    ! dthr            - time step (hr)
    ! CoolEnthalpy    - updated enthalpy for all axial nodes (btu/lbm)
    ! CoolMassFlx     - updated mass flux for all axial nodes (lbm/hr*ft**2)
    ! CoolDensity     - updated density for all axial nodes (lbm/ft**3)
    !
    ! the following arrays can be Dimensioned to length of 1 for
    ! relap4 type link with thermal/hydraulic code. otherwise, set length to 22
    !
    ! common block /coold/  contains arrays that store coolant condition versus time Data
    !
    ! aasth       = Data array for sth2x package (coolant properties)
    ! nchn        = number of coolant subchannels
    ! k           = axial node number corresponding to elevation z (ft)
    ! ntco        = fortran LOGICAL unit number for data set with coolant conditions (used only If nqchn = 2)
    ! Time        = time (sec)
    ! z           = fuel rod elevation (ft)
    ! ncall       = -1 If coolant conditions are to be printed,
    !             =  0 on first Call to routine,  = 1 otherwise.
    ! RodLength   = fuel stack height of fuel rods (ft)
    ! acond       = storage area with channel Data and coolant conditions for coolant channel j
    ! acond(nn,j) = chfv = vector of storage for pchf routine. nn=1,8
    ! acond( 9,j) = achn = flow area,  ft**2
    ! acond(10,j) = dhe  = heated equivalent diameter, ft
    ! acond(11,j) = dhy  = channel hydraulic diameter, ft
    ! acond(12,j) = g    = mass flow rate, lbm/(hr.ft**2)
    ! acond(13,j) = h    = local coolant enthalpy,  btu/lbm
    ! acond(14,j) = hin  = enthalpy at normal inlet, btu/lbm
    ! acond(15,j) = hup  = enthalpy at outlet (upper plenum)
    ! acond(16,j) = p    = coolant pressure,  psia
    ! acond(17,j) = tbulk= local coolant temperature,  F
    ! acond(18,j) = v    = specific volume,  ft**3/lbm
    ! acond(19,j) = coolant quality
    ! acond(20,j) = cp   = heat capacity at constant pressure, btu/(lbm.F)
    ! acond(21,j) = tsat = saturation temperature,  F
    ! acond(22,j) = hf   = saturation enthalpy (liquid/fluid)
    ! acond(23,j) = hg   = saturation enthalpy (vapor/gas)
    ! acond(24,j) = vf   = saturation specific volume (liquid)
    ! acond(25,j) = vg   = saturation specific volume (vapor)
    ! acond(26,j) = cpf  = saturation heat capacity (liquid)
    ! acond(27,j) = cpg  = saturation heat capacity (vapor)
    ! acond(28,j) = memory indices for saturation state
    ! acond(29,j) = memory indices for single phase
    ! acond(30,j) = rate of change of coolant quality with respect to elevation (1/ft)
    !
    ! pbh - gbh & npbh - ngbh are defined in Sub. coolin. they specify transient conditions of coolant at flow inlet
    !
    ! unit        = .TRUE. if print units are same as internal
    ! nqchn       = Coolant calculation type switch.
    !           0 - heat absorbed by coolant as it flows through core computed by frap
    !           1 - inlet enthalpy history specified by array hinta
    !           1 - array hbh contains average coolant enthalpy history. Also, arrays pbh, tbh, and gbh are Then average core values
    !           2 - transient coolant conditions along entire rod length are read from disk or tape.
    !           3 - heat transfer coefficients are specified by card input
    !           4 - heat transfer coefficients read from file
    !           5 - h.t.c., tbulk, CoolantPress from dirct relap link
    !           6 - coolant conditions from cobra type link
    !           7 - coolant conditions read similar to nqchn=2, but mass flux and enthalpy are not used. Instead, they are
    !               calculated in Subroutine transh on basis of the read lower plenum g and h, and heat flux from fuel rod
    ! nvol        = number of volumes used in thermal/hydraulic code to compute coolant conditions surrounding fuel rod
    ! nhtcz       = number of zones for which heat transfer coefficients specified when nqchn = 3
    ! htca(nhprs*2,nhtcz)=array storing heat transfer coefficients when nqchn=3 (btu/hr-ft2-F)
    ! htclev(j)   = elevation of top of h.t.c. zone j when nqchn = 3  (ft)
    ! nhprs(j)    = number of h.t.c. - time pairs in j-th column of htca
    ! ntprs(j)    = number of sink temperature - time pairs when nqchn=3
    ! tblka(n,j)  = array of sink temperature-time pairs when nqchn = 3(F)
    ! rwa5(j)     = working space array
    ! tc1         = time of second to last read of coolant condition file (sec)
    ! tc2         = time of last file read of coolant conditions, sec (only used when nqchn = 2) ( input and output)
    ! gum         = unmodified mass flux (not given absolute value) (lbm/ft2-hr)
    ! lhtc/10     = switch to indicate whether or not coolant is helium If lhtc/10 = 9 , yes.  otherwise no.
    ! pz1 through z2 are arrays that are set in sub. cool but which must be stored in main for use in future calls to sub. cool
    ! prop    = array in which state properties are passed to and  from steam table Subroutines.  values are in si units
    ! prop(1) = coolant temperature  K
    !     (2) = coolant pressure     n/m2
    !     (3) = specific volume   m3/kg
    !     (4) = specific internal energy   j/kg
    !     (5) = specific enthalpy   j/kg
    ! quantities 1-5 are input and output to state routines
    !     (6) = single phase beta
    !     (7) = single phase kappa
    !     (8) = single phase csubp
    !     (9) = quality If two-phase
    !    (10) = saturation pressure
    !    (11) = liquid specific volume
    !    (12) = vapor  specific volume
    !    (13) = liquid specific internal energy
    !    (14) = vapor  specific internal energy
    !    (15) = liquid specific enthalpy
    !    (16) = vapor  specific enthalpy
    !    (17) = liquid beta
    !    (18) = vapor  beta
    !    (19) = liquid kappa
    !    (20) = vapor  kappa
    !    (21) = liquid csubp
    !    (22) = vapor  csubp
    !    (23) = indexes
    !
    ! nogo    = .TRUE. means problem failure
    ! heflag  = .TRUE.  means helium coolant
    !
    ! limits on water property tables
    ! 273.15 lt t lt 5000. in deg K,    32.018 lt t lt 8540.312 in F
    ! 2.0e3_r8k  ) p ) 2.5e7_r8k p in n/sqm,   0.0886 ) p ) 3626   p in psi
    !
    INTEGER(ipk) :: ig = 1, ipx = 1, j, it, npass, isgz2, isgz1, ishz2, ishz1, istz2, &
      &             istz1, isangl, imax, iup, nthymx, i, n, icon, iglbl, iaz, iazp1, ig1, ig2, &
      &             i1, l, iazmul, nangl
    INTEGER(ipk), INTENT(IN) :: k
    REAL(r8k) :: tim0, Time_in, hi, hin1, pri, hup1, delth, tlast, hi0km1, hikm1, gikm1, rhkm1, &
        &       tcool, angle, b1, b2, b3, b4, a1, a2, a3, a4, hzfac, tzfac, gzfac, angthy, htc, &
        &       hlm1, hl, htc0, tbulk, tbulk0, dhdz = 0.0_r8k
    REAL(r8k), INTENT(IN) :: theta, RodLength, qcool, delz, dthr, tpo
    REAL(r8k), PARAMETER :: ranf = 459.67_r8k
    REAL(r8k), PARAMETER :: psi = 6.89476e3_r8k
    REAL(r8k), PARAMETER  :: dxdlmn = 0.001_r8k
    REAL(r8k), PARAMETER  :: cphe = 1.24_r8k
    REAL(r8k), PARAMETER  :: rsubhe = 386.04_r8k
    REAL(r8k), PARAMETER :: dens = 16.018463373959_r8k
    REAL(r8k), PARAMETER :: spen = 2326.0_r8k
    REAL(r8k), PARAMETER :: htcap = 4.1868e3_r8k
    LOGICAL :: nogo, heflag
    CHARACTER(LEN=8) :: w2nogo
    CHARACTER(LEN=8), PARAMETER :: w2o2 = 'sth2x2'
    CHARACTER(LEN=8), PARAMETER :: w2o5 = 'sth2x5'
    REAL(r8k) :: zu, zl, htcu, templ, zloc, htcc, htcl, tempc, tempu
    !
    REAL(r8k), DIMENSION(1) :: acndaz ! This array is improperly sized (> 1)
    REAL(r8k), DIMENSION(4) :: tbh
    !
    IF (ndebug) WRITE(ounit,923) Time, z
923 FORMAT(' COOL: Time = ',e12.5,' z = ',e11.4)
    heflag = ((lhtc/10) == 9)
    SELECT CASE (nqchn)
    CASE (0)
        GOTO 60
    CASE (2)
        GOTO 2000
    CASE (3)
        GOTO 3000
    CASE (4)
        GOTO 4000
    CASE (5)
        GOTO 4500
    CASE (6)
        GOTO 2500
    CASE (7)
        GOTO 2000
    CASE DEFAULT
        WRITE (0,1) nqchn
1       FORMAT ('Wrong value supplied for nqchn. Execution stopped in Subroutine: cool. nqchn = ',i3)
        ERROR STOP 'Wrong value supplied for nqchn. Execution stopped in Subroutine: cool'
    END SELECT
    ! Interpolate to find coolant conditions at time Time
50  CONTINUE
    tim0 = hbh(2)
    Time_in = Time
    IF (Time_in < tim0) Time_in = tim0
    !
    hi = polate (hbh, Time_in, nhbh)
    !
60  CONTINUE
    tim0 = hinta(2)
    Time_in = Time
    IF (Time_in < tim0) Time_in = tim0
    !
    hin1 = polate (hinta, Time_in, nhinta)
    !
    tim0 = gbh(2)
    Time_in = Time
    IF (Time_in < tim0) Time_in = tim0
    !
    gum =  polate (gbh, Time, ngbh, ig)
    !
    tim0 = pbh(2)
    Time_in = Time
    IF (Time_in < tim0) Time_in = tim0
    !
    pri = polate (pbh, Time_in, npbh, ipx)
    !
    tim0 = hupta(2)
    Time_in = Time
    IF (Time_in < tim0) Time_in = tim0
    !
    hup1 = polate (hupta, Time_in, nhupta)
    !
    IF (ndebug) WRITE(ounit,926) gum, hi, pri, hinta(1)
926 FORMAT(' g = ',e11.4,' h = ',e11.4,' p = ',e11.4, ' hin = ',e11.4)
    ! Determine coolant conditions in each channel at time Time
    DO 1000 j = 1,nchn
        acond(14,j) = hin1
        acond(15,j) = hup1
        acond(16,j) = pri
        IF (nqchn == 7) GOTO 62
        IF (nqchn /= 0) GOTO 90
        !
62      CONTINUE
        IF (NSteadyTrans == 2) GOTO 65
        ! Steady state enthalpy and mass flux calculations
        IF (heflag) hin1 = cphe * (tfr(acond(14,j)))
        delth = qcool * delz / gum
        hi = hin1 + delth
        IF (k /= 1) hi = CoolEnthalpy(k-1) + delth
        CoolEnthalpy(k) = hi
        CoolMassFlx(k) = gum
        GOTO 90
        ! Transient enthalpy and mass flux calculations
65      CONTINUE
        IF (heflag) GOTO 66
        ! If reverse flow, bypass transient enthalpy rise calculation and conservatively set local enthalpy to lower plenum enthalpy
        IF (nqchn /= 7) GOTO 66
        IF (gum > 0.0_r8k) GOTO 66
        CoolEnthalpy(k) = hin1
        CoolMassFlx(k) = gum
        GOTO 88
66      CONTINUE
        tlast = Time - dthr * sechr
        IF (tlast < pbh(2)) tlast = pbh(2)
        !
        CoolantPress = polate (pbh, tlast, npbh, ipx) * psi
        !
        IF (k /= 1) THEN
            hi0km1 = CoolEnthalpy0(k-1)
            hikm1  = CoolEnthalpy(k-1)
            gikm1  = CoolMassFlx(k-1)
            rhkm1  = CoolDensity0(k-1)
        ELSE
            IF (tlast < hinta(2)) tlast = hinta(2)
            !
            hi0km1 = polate (hinta, tlast, nhinta)
            !
            gikm1 = gum
            IF (heflag) THEN
                hi0km1 = cphe * tfr(hi0km1)
                hikm1  = cphe * (tfr(acond(14,j)))
                rhkm1  = pri  * psift2/(rsubhe * (tfr(acond(14,j))))
            ELSE
                hikm1 = hin1
                hbar  = hi0km1 * spen
                !
                CALL sth2x5 (aasth, prop, it, nogo)
                !
                w2nogo = w2o5
                IF (nogo) GOTO 1111
                rhkm1 = 1.0_r8k/(dens * v)
            ENDIF
        ENDIF
        !
        CALL transh (hi0km1, CoolEnthalpy0(k), hikm1, gikm1, rhkm1, CoolDensity0(k), dthr, delz, &
          &          qcool, CoolEnthalpy(k), CoolMassFlx(k), aasth, nogo)
        !
        w2nogo = w2o5
        IF (nogo) GOTO 1111
        IF (ndebug) WRITE(ounit,907) k, CoolEnthalpy(k), hup1, CoolMassFlx(k)
907     FORMAT(6x,'after transh call, k = ',i2,' CoolEnthalpy(k) = ', e11.4,' hup1 = ',e11.4,' CoolMassFlx(k) =',e11.4)
        IF ((nqchn == 7) .AND. (CoolEnthalpy(k) < hup1)) CoolEnthalpy(k) = hup1
88      CONTINUE
        gum = CoolMassFlx(k)
        hi = CoolEnthalpy(k)
        delth = CoolEnthalpy(k) - hikm1
90      acond(12,j) = gum
        acond(13,j) = hi
        IF (ndebug) WRITE(ounit,925) gum, hi
925     FORMAT(6x,' CoolMassFlx(k) = ',e11.4,' CoolEnthalpy(k) = ', e11.4)
        IF (.NOT. heflag) GOTO 100
        !**** compute and store coolant conditions assuming coolant is helium
        !**** assume inlet temperature instead of inlet enthalpy input
        IF (nqchn == 0 .OR. nqchn == 7) THEN
            tcool = CoolEnthalpy(k) / cphe - ranf
        ELSE
            tcool = hi
        ENDIF
        CoolDensity(k) = pri * psift2 / (rsubhe * (tfr(tcool)))
        acond(17,j) = tcool
        acond(18,j) = 0.0_r8k
        acond(19,j) = 0.0_r8k
        acond(20,j) = 0.0_r8k
        acond(21,j) = 0.0_r8k
        acond(22,j) = 0.0_r8k
        acond(23,j) = 0.0_r8k
        acond(24,j) = 0.0_r8k
        acond(25,j) = 0.0_r8k
        acond(26,j) = 0.0_r8k
        acond(27,j) = 0.0_r8k
        GOTO 1000
        ! Use coolant properties to calculate other conditions from pri, hi.
        ! This section uses si units
100     CONTINUE
        !
        prop(1:22) = 0.0_r8k
        !
        CoolantPress = pri * psi
        hbar = hi * spen
        ! Calculate water properties from pressure and enthalpy
        prop(23) = acond(28,j)
        !
        CALL sth2x5 (aasth, prop, it, nogo)
        !
        w2nogo = w2o5
        IF (nogo) GOTO 1111
        CoolDensity(k) = 1.0_r8k / (dens * v)
        acond(17,j) = tkf(tt)
        acond(18,j) = v * dens
        acond(19,j) = x
        IF (ndebug) WRITE(ounit,888) k, pri, hi, acond(17,j), x
888     FORMAT(' COOL: k = ',i3,' coolant pressure = ',e11.4,' coolant enthalpy = ',e11.4,' coolant temp = ',e11.4, &
          &    ' coolant quality = ',e11.4)
        IF (it == 2) csubp = csubpf * (1 - x) + csubpg * x
        acond(20,j) = csubp/htcap
        IF (it == 4) THEN
            prop(1) = 647.3_r8k
            prop(2) = 2.2e+07_r8k
        END IF
        acond(21,j) = tkf(prop(10))
        IF (it /= 2) THEN
            !
            CALL sth2x2 (aasth, prop, nogo)
            !
            w2nogo = w2o2
            IF (nogo) GOTO 1111
        END IF
        !
        acond(22,j) = hsubf / spen
        acond(23,j) = hsubg / spen
        acond(24,j) = vsubf * dens
        acond(25,j) = vsubg * dens
        acond(26,j) = csubpf / htcap
        acond(27,j) = csubpg / htcap
        acond(28,j) = prop(23)
        IF (nqchn == 2 .OR. nqchn == 6) GOTO 140
        IF (nqchn == 1) GOTO 150
        acond(30,j) = MAX(dxdlmn, ABS(delth / ((hsubg - hsubf) / spen)))
        GOTO 1000
140     CONTINUE
        acond(30,j) = dhdz / ((hsubg - hsubf) / spen)
        GOTO 1000
150     CONTINUE
        IF (hup1 < hin1) THEN
            acond(30,j) = ((hi - hup1) / ((hsubg - hsubf) / spen)) / (RodLength / 2.0_r8k)
        ELSE
            acond(30,j) = ((hi - hin1) / ((hsubg - hsubf) / spen)) / (RodLength / 2.0_r8k)
        ENDIF
1000 CONTINUE     
    !
    IF (ntstep == 0) RETURN
    !
    IF ((IterationCount == 1) .AND. (Time >= (tpo - 1.0e-6_r8k))) CALL prntc (acond, z, Time, nchn, k, unit)
    !
    RETURN
    ! Fatal error in coolant properties
1111 CONTINUE
    WRITE(ounit,910) w2nogo
910 FORMAT('******** steam table failure from property routine ',a6)
    WRITE(ounit,950) ncall, gum, prop
950 FORMAT(' ncall = ',i3,'  gum = ',e10.3, &
      &  / ' prop(1)= temperature  K', &
      &  / '     (2)= pressure     n/m2', &
      &  / '     (3)= specific volume   m3/kg', &
      &  / '     (4)= specific internal energy   j/kg', &
      &  / '     (5)= specific enthalpy   j/kg', &
      &  / 'quantities 1-5 are input and output to state routines', &
      &  / '     (6)= single phase beta', &
      &  / '     (7)= single phase kappa', &
      &  / '     (8)= single phase csubp', &
      &  / '     (9)= quality If two-phase', &
      &  / '    (10)= saturation pressure', &
      &  / '    (11)= liquid specific volume', &
      &  / '    (12)= vapor  specific volume', &
      &  / '    (13)= liquid specific internal energy', &
      &  / '    (14)= vapor  specific internal energy', &
      &  / '    (15)= liquid specific enthalpy', &
      &  / '    (16)= vapor  specific enthalpy', &
      &  / '    (17)= liquid beta', &
      &  / '    (18)= vapor  beta', &
      &  / '    (19)= liquid kappa', &
      &  / '    (20)= vapor  kappa', &
      &  / '    (21)= liquid csubp', &
      &  / '    (22)= vapor  csubp', &
      &  / '    (23)= indexes', &
      &  //,5x,'(1,9,17,25)(2,10,18,26)(3,11,19)  (4,12,20)  (5,13,21)', &
      &    '  (6,14,22)  (7,15,23)  (8,16,24)'/4(4x,8e11.3,/))
    !
    ERROR STOP 'coolant failure'
    ! Transient coolant conditions read from disk or tape
2000 CONTINUE
    npass = 0
    IF (ithymx > 0) THEN
        ! Compute starting indexes for azimuthally varying variables
        isgz2 = 1
        isgz1 = isgz2 + (nvol * ithymx)
        ishz2 = isgz2 + (ithymx * nvol) * 2
        ishz1 = ishz2 + (ithymx * nvol)
        istz2 = ishz2 + (ithymx * nvol) * 2
        istz1 = istz2 + (ithymx * nvol)
        isangl = istz2 + (ithymx * nvol) * 2
    ENDIF
    imax = nvol + 1
    iup = imax + 1
    IF (ncall /= 0) GOTO 2020
    REWIND ntco
    ! If multi-coolant channels, read rod to channel connection data
    ! first number read is - sum over all rods of number channels connecting each rod
    IF (ithymx <= 0) GOTO 2012
    READ(ntco,End = 10000) nthymx
10000 CONTINUE
    IF (ithymx == (nthymx - 1)) GOTO 2014
    WRITE(ounit,903)
903 FORMAT(////,' error detected reading coolant condition tape in Subroutine cool')
    WRITE(ounit,904)
904 FORMAT(//,' from read of tape, number of rod to coolant channel connections =',i5,1x, &
      &        ' while input deck says the number is',i5)
    !
    ERROR STOP
    ! read angular upper bound for each rod to channel connection
    ! angle in degrees
    ! read - rod number , local connection number, upper bound angle
2014 CONTINUE
    DO i = 1, nthymx
        READ(ntco,End = 10001) n, icon, angle
10001   IF (n > 12 .OR. n < 1) WRITE(ounit,912)
912     FORMAT(' *** error - rod number out of range reading coolant tape  *** '  )
        IF (icon < 1 .OR. icon > 8) WRITE(ounit,914)
914     FORMAT(///'*** error - rod channel connection number out of range reading coolant tape *** ' )
        icglob(n,icon) = i - 1
        iglbl = icglob(n,icon)
        acndaz(isangl+iglbl) = angle
    ENDDO
    !
2012 CONTINUE
2010 Read(ntco,End = 10002) tc2
10002 Read(ntco,End = 10003) pz2(1), hz2(1), tz2(1)
10003 CONTINUE
    ! Constants are arbitrairly set not knowing their function
    b1 = 1.0_r8k
    b2 = 1.0_r8k
    b3 = 1.0_r8k
    b4 = 1.0_r8k
    a1 = 1.0_r8k
    a2 = 1.0_r8k
    a3 = 1.0_r8k
    a4 = 1.0_r8k
    !
    DO i = 2, imax
        READ(ntco,End = 10004) z1(i), z2(i), pz2(i), hz2(i), tz2(i), gz2(i)
10004   IF (ABS(b2) == 1.0_r8k) GOTO 20
        CoolantPress = (pz2(i) + a1) * b1 * psi
        hbar = hz2(i) * spen * (1.0_r8k + 0.01_r8k * (b2/ABS(b2)))
        !
        CALL sth2x5 (aasth, prop, it, nogo)
        !
        IF (x <= 0.15_r8k) GOTO 10
        hbar = (hz2(i) + a2) * spen * b2
        !
        CALL sth2x5 (aasth, prop, it, nogo)
        !
10      CONTINUE
        pz2(i) = CoolantPress/psi
        hz2(i) = hbar/spen
        tz2(i) = tkf(tt)
        gz2(i) = (gz2(i) + a4) * b4
        GOTO 30
20      CONTINUE
        pz2(i) = (pz2(i) + a1) * b1
        hz2(i) = (hz2(i) + a2) * b2
        tz2(i) = (tz2(i) + a3) * b3
        gz2(i) = (gz2(i) + a4) * b4
30      CONTINUE
        !
        IF (ithymx > 0) THEN
            ! Read multirod and azimuthally varying coolant condition factors
            DO iaz = 1,ithymx
                Read(ntco,End = 10005) n, icon, hzfac, tzfac, gzfac
10005           iglbl = icglob(n,icon) - 1
                acndaz(ishz2 + (i-2) * ithymx + iglbl) = hzfac
                acndaz(istz2 + (i-2) * ithymx + iglbl) = tzfac
                acndaz(isgz2 + (i-2) * ithymx + iglbl) = gzfac
            ENDDO
        ENDIF
    ENDDO
    !
    READ(ntco,End=10006) pz2(iup), hz2(iup), tz2(iup)
10006 npass = npass + 1
    IF (ncall == 0 .AND. npass <= 1) GOTO 2100
2020 CONTINUE
    IF (ncall == 0) GOTO 2030
    IF (Time >= tc1) GOTO 2030
    GOTO 2500
2030 CONTINUE
    IF (Time <= tc2) GOTO 2500
2100 CONTINUE
    DO i = 2, imax
        pz1(i) = pz2(i)
        hz1(i) = hz2(i)
        tz1(i) = tz2(i)
        gz1(i) = gz2(i)
        IF (ithymx >= 1) THEN
            ! set azimuthal variation factors for time tc1
            DO iazp1 = 1, ithymx
                iaz = iazp1 - 1
                ig1 = ishz1 + (i-2) * ithymx + iaz
                ig2 = ishz2 + (i-2) * ithymx + iaz
                acndaz(ig1) = acndaz(ig2)
                ig1 = isgz1 + (i-2) * ithymx + iaz
                ig2 = isgz2 + (i-2) * ithymx + iaz
                acndaz(ig1) = acndaz(ig2)
                ig1 = istz1 + (i-2) * ithymx + iaz
                ig2 = istz2 + (i-2) * ithymx + iaz
                acndaz(ig1) = acndaz(ig2)
            ENDDO
        ENDIF
    ENDDO
    !
    hz1(1) = hz2(1)
    hz1(iup) = hz2(iup)
    tc1 = tc2
    GOTO 2010
    ! Find coolant conditions for elevation z
2500 CONTINUE
    imax = nvol + 1
    iup = imax + 1
    IF (Time >= tc1) THEN
        ! tc1 and tc2 bracket Time
        npbh = 2
        nhbh = 2
        ngbh = 2
        nhinta = 2
        nhupta = 2
    ELSE
        ! First time on coolant condition Data set greater than problem initial time. assume this time currently applies
        npbh = 1
        nhbh = 1
        ngbh = 1
        nhinta = 1
        nhupta = 1
    ENDIF
    ! If calculating local x, only use coolant conditions for first zone
    IF (nqchn == 7) THEN
        i1 = 2
    ELSE
        DO i = 2, imax
            i1 = i
            IF (z <= z2(i)) EXIT
        ENDDO
        IF (i1 == (imax + 1)) THEN
            WRITE(ounit,2550) z, z2(i1)
2550        FORMAT(' *** coolant conditions needed at higher elevation than stored on tape ***',/,' z(needed)  ',e11.4, &
              &    ' z(stored) = ',e11.4)
            STOP
        ENDIF
    ENDIF
    hinta(1) = hz1(1)
    hinta(2) = tc1
    hinta(3) = hz2(1)
    hinta(4) = tc2
    l = i1
    iazmul = 0
    IF (ithymx == 0) GOTO 2850
    ! check to see If current azimuthal position is beyond angular boundary of first t/h code sector
    iglbl = icglob(1,1)
    angthy = acndaz(isangl+iglbl)
    IF (theta < angthy) GOTO 2850
    iazmul = 1
    ! Find t/h code sector that is tangent with current azimuthal sector
    nangl = 1
2820 CONTINUE
    nangl = nangl + 1
    iglbl = icglob(1,nangl) - 1
    ! angle also stored for 1st channel, so add 1 to iglbl
    angthy = acndaz(isangl+iglbl+1)
    IF (theta <= (angthy + 1.0e-6_r8k)) GOTO 2830
    IF (nangl < 9 ) GOTO 2820
    WRITE(ounit,918)
918 FORMAT(////,' *** error in Subroutine cool finding t/h code sector that touches rod azimuthal sector. program stopped *** ')
    STOP
    !
2830 CONTINUE
2850 CONTINUE
    hbh(1) = hz1(l-1)
    IF (l <= 2) GOTO 2860
    IF (iazmul < 1) GOTO 2860
    hbh(1) = hbh(1) * acndaz(ishz1 + (l-3) * ithymx + iglbl)
2860 CONTINUE
    hbh(2) = tc1
    hbh(3) = hz2(l-1)
    IF (l <= 2) GOTO 2862
    IF (iazmul < 1) GOTO 2862
    hbh(3) = hbh(3) * acndaz(ishz2 + (l-3) * ithymx + iglbl)
2862 CONTINUE
    hbh(4) = tc2
    !
    hlm1 = polate (hbh, Time, nhbh)
    !
    hbh(1) = hz1(l)
    IF (iazmul < 1) GOTO 2864
    hbh(1) = hbh(1) * acndaz(ishz1 + (l-2) * ithymx + iglbl)
2864 CONTINUE
    hbh(2) = tc1
    hbh(3) = hz2(l)
    IF (iazmul < 1) GOTO 2866
    hbh(3) = hbh(3) * acndaz(ishz2 + (l-2) * ithymx + iglbl)
2866 CONTINUE
    hbh(4) = tc2
    !
    hl = polate (hbh, Time, nhbh)
    !
    dhdz = ABS((hl - hlm1) / (z2(l) - z1(l)))
    gbh(1) = gz1(l)
    IF (iazmul < 1) GOTO 2868
    gbh(1) = gbh(1) * acndaz(isgz1 + (l-2) * ithymx + iglbl)
2868 CONTINUE
    gbh(2) = tc1
    gbh(3) = gz2(l)
    IF (iazmul < 1) GOTO 2870
    gbh(3) = gbh(3) * acndaz(isgz2 + (l-2) * ithymx + iglbl)
2870 CONTINUE
    gbh(4) = tc2
    pbh(1) = pz1(l)
    pbh(2) = tc1
    pbh(3) = pz2(l)
    pbh(4) = tc2
    tbh(1) = tz1(l)
    IF (iazmul < 1) GOTO 2872
    tbh(1) = tbh(1) * acndaz(istz1 + (l-2) * ithymx + iglbl)
2872 CONTINUE
    tbh(2) = tc1
    tbh(3) = tz2(l)
    IF (iazmul < 1) GOTO 2874
    tbh(3) = tbh(3) * acndaz(istz2 + (l-2) * ithymx + iglbl)
2874 CONTINUE
    tbh(4) = tc2
    hupta(1) = hz1(iup)
    hupta(2) = tc1
    hupta(3) = hz2(iup)
    hupta(4) = tc2
    ! At end of read of cool(t,z), switch to calculate local x
    IF (nqchn == 7) GOTO 60
    GOTO 50
    ! determine heat transfer coefficient for elevation z , time Time
3000 CONTINUE
    l = 0
3020 CONTINUE
    l = l + 1
    IF (z < htclev(l)) GOTO 3200
    IF (l < nhtcz) GOTO 3020
    WRITE(ounit,3050) l, nhtcz, z, htclev(l)
3050 FORMAT(' *** Heat transfer coefficients not specified along entire length of fuel rods. Program stopped *** ', &
       &    ' l = ',i4,' nhtcz = ',i4,' z = ',f8.4,' htclev(l) = ',f8.4)
    STOP
    !
3200 CONTINUE
    
    ! User supplied Coolant Conditions (HTC, Temp & Pressure)
    

        
        ! Pressure Value
    
        ! Set the interpolation time
        Time_in = MAX(Time, pbh(2))
        ! Linearly interpolate between user-supplied time values
        ! Note: Pressure is supplied as inlet pressure and assummed constant over all axial nodes (no axial depenency)
        pri = polate (pbh, Time_in, npbh)
        
        ! Current heat transfer coefficients in up to three coolant zones:
        
        ! Centered coolant zone (housing the current axial node): 
        Time_in = MAX(Time, htca(2,l))
        htcc = polate (htca(1:,l), Time_in, nhprs(l))
        zu = htclev(l)
        
        ! Coolant zone below:
        IF (l > 1) THEN
           Time_in = MAX(Time, htca(2,l-1))
           htcl = polate(htca(1:,l-1), Time_in, nhprs(l-1))
           zl = htclev(l-1)
        ELSE
           htcl = htcc
           zl = 0.0_r8k
        END IF
        
        ! Coolant zone above:
        IF (l < nhtcz) THEN
           Time_in = MAX(Time, htca(2,l+1))
           htcu = polate (htca(1:,l+1), Time_in, nhprs(l+1))
        ELSE
           htcu = htcc
        END IF

        ! Current coolant temperature in up to three coolant zones:
        
        ! Centered coolant zone (housing the current axial node): 
        Time_in = MAX(Time, tblka(2,l))
        tempc = polate (tblka(1:,l), Time_in, ntprs(l))
        
        ! Coolant zone below:
        IF (l > 1) THEN
           Time_in = MAX(Time, tblka(2,l-1))
           templ = polate (tblka(1:,l-1), Time_in, ntprs(l-1))
        ELSE
           templ = tempc
        END IF
        
        ! Coolant zone above:
        IF (l < nhtcz) THEN
           Time_in = MAX(Time, tblka(2,l+1))
           tempu = polate (tblka(1:,l+1), Time_in, ntprs(l+1))
        ELSE
           tempu = tempc
        END IF
        
        ! Interpolation with regard to local position within the coolant zone:
        zloc = (z - zl) / (zu - zl)
        htcl = 0.5_r8k * (htcl + htcc)
        htcu = 0.5_r8k * (htcu + htcc)
        templ = 0.5_r8k * (templ + tempc)
        tempu = 0.5_r8k * (tempu + tempc)
        
        IF (zloc <= 0.25_r8k) THEN
            
            ! Lower quarter of zone:
            htc = htcl + 4.0_r8k * zloc * (htcc - htcl)
            tbulk = templ + 4.0_r8k * zloc * (tempc - templ)
            
        ELSE IF (zloc >= 0.75_r8k) THEN
            
            ! Upper quarter of zone:
            htc = htcc + 4.0_r8k * (zloc - 0.75_r8k) * (htcu - htcc)
            tbulk = tempc + 4.0_r8k * (zloc - 0.75_r8k) * (tempu - tempc)
            
        ELSE
            
            ! Central half of zone:
            htc = htcc
            tbulk = tempc
           
        END IF
        
    
    !
    DO j = 1,nchn
        acond(12,j) = 0.0_r8k
        acond(13,j) = htc   ! Store heat transfer coefficient in acond(13,j)
        acond(14,j) = 0.0_r8k
        acond(15,j) = 0.0_r8k
        acond(16,j) = pri   ! Store
        acond(17,j) = tbulk ! Store bulk coolant temperature in acond(17,j)
        acond(18,j) = 0.0_r8k
        acond(19,j) = 0.0_r8k
        acond(20,j) = 0.0_r8k
        acond(21,j) = 0.0_r8k
        acond(22,j) = 0.0_r8k
        acond(23,j) = 0.0_r8k
        acond(24,j) = 0.0_r8k
        acond(25,j) = 0.0_r8k
        acond(26,j) = 0.0_r8k
        acond(27,j) = 0.0_r8k
        acond(28,j) = 0.0_r8k
        acond(29,j) = 0.0_r8k
        gum = 0.0_r8k
    ENDDO
    !
    IF (ncall < 0) Call prntmp (tbulk, pri, Time, z, unit, k)
    !
    RETURN
    !
4000 CONTINUE
    npass = 0
    imax = nvol
    IF (ncall /= 0) GOTO 4020
    REWIND ntco
4010 CONTINUE
    READ(ntco,End = 10007) tc2
10007 WRITE(ounit,905) Time, tc2
905 FORMAT('for time(sec) = ',e13.6,' coolant tape read. time on tape = ',e13.6)
    !
    DO i = 1, imax
        READ(ntco,End = 4060) z1(i), z2(i), hz2(i), tz2(i), pz2(i)
4060 ENDDO
    !
    npass = npass + 1
    IF (ncall == 0 .AND. npass <= 1) GOTO 4100
4020 CONTINUE
    IF (ncall == 0) GOTO 4030
    IF (Time >= tc1) GOTO 4030
    ncall = 0
    GOTO 4000
4030 CONTINUE
    IF (Time <= tc2) GOTO 4500
    !
4100 CONTINUE
    !
    DO i = 1, imax
        pz1(i) = pz2(i)
        hz1(i) = hz2(i)
        tz1(i) = tz2(i)
    ENDDO
    !
    tc1 = tc2
    GOTO 4010
4500 CONTINUE
    imax = nvol
    DO i = 1, imax
        i1 = i
        IF (z <= z2(i)) GOTO 4850
    ENDDO
    !
    If(ntstep > 0 .AND. Time >= (tpo - 1.0e-8_r8k)) WRITE(ounit,4550) z, z2(i-1)
4550 FORMAT(1x,'Warning: FRAPTRAN Height (z) above T/H Node Height(z2) in coolt calculation. z= ',f10.5,3x,' z2(i)= ',f10.5)
4850 CONTINUE
    l = i1
    hbh(1) = hz1(l)
    hbh(2) = tc1
    hbh(3) = hz2(l)
    hbh(4) = tc2
    ! Ensure that tc2-tc1 does not cause a problem
    IF (tc2 == tc1) hbh(4) = hbh(4) + 1.0E-8
    !
    htc = polate (hbh, Time, nhbh)
    !
    pbh(1) = pz1(l)
    pbh(2) = tc1
    pbh(3) = pz2(l)
    pbh(4) = tc2
    ! Ensure that tc2-tc1 does not cause a problem
    IF (tc2 == tc1) pbh(4) = pbh(4) + 1.0E-8
    !
    pri = polate (pbh, Time, nhbh)
    !
    tbh(1) = tz1(l)
    tbh(2) = tc1
    tbh(3) = tz2(l)
    tbh(4) = tc2
    ! Ensure that tc2-tc1 does not cause a problem
    IF (tc2 == tc1) tbh(4) = tbh(4) + 1.0E-8
    !
    tbulk = polate (tbh, Time, nhbh)
    !
    DO j = 1, nchn
        acond(13,j) = htc
        acond(16,j) = pri
        acond(17,j) = tbulk
        acond(12,j) = 0.0_r8k
        acond(14,j) = 0.0_r8k
        acond(15,j) = 0.0_r8k
        acond(18:29,j) = 0.0_r8k
    ENDDO
    !
    gum = 0.0_r8k
    IF (ncall < 0) CALL prntmp (tbulk, pri, Time, z, unit, k)
    !
    END SUBROUTINE cool
    !
    !
    !
    SUBROUTINE transh (h0km1, h0k,  hkm1, gkm1, rhokm1, rhok, delt, delz, qtav, hk, gk, aasth, nogo)
    USE Kinds
    USE CoolantProperties, ONLY : Prop
    USE sth2x, ONLY : sth2x5
    USE resti_h, ONLY : nqchn
    IMPLICIT NONE
    !
    ! Input
    !
    ! h0km1  - Beginning of time step enthalpy at node k-1 (btu/lbm)
    ! hok    - Beginning of time step enthalpy at node k (btu/lbm)
    ! hkm1   - End of time step enthalpy at node k-1 (btu/lbm)
    ! gkm1   - Mass flux at node k-1 (lbm/hr*ft**2)
    ! rhokm1 - Density at node k-1 (lbm/ft**3)
    ! rhok   - Density at node k (lbm/ft**3)
    ! delt   - Time step (hr)
    ! delz   - Node length (ft)
    ! qtav   - Total average volumetric heat source (btu/hr*ft**3).
    !          Includes clad surface heate flux and direct energy deposition in the coolant
    ! nqchn  - Coolant calculation type switch. If nqchn=7, local enthalpy calculated by FrapTran, 
    !          but inlet conditions read from relap tape.
    ! Output
    !
    ! hk     - End of time step enthalpy at node k (btu/lbm)
    ! gk     - Mass flux at node k (lbm/hr*ft**2)
    ! nogo   - Logical flag for determining if error caused in subroutine sth2x5
    !
    INTEGER(ipk) :: it
    REAL(r8k) :: delh, delrho, rhoAveIn, drhodh, a, gavein, alpha, h0kp1
    REAL(r8k), INTENT(IN) :: h0km1, h0k, hkm1, gkm1, rhokm1, rhok, delt, delz, qtav
    REAL(r8k), INTENT(OUT) :: hk, gk
    LOGICAL, INTENT(OUT) :: nogo
    REAL(r8k), DIMENSION(:), INTENT(INOUT) :: aasth
    !
    delh = h0k - h0km1
    delrho = rhok - rhokm1
    rhoAveIn = (rhok + rhokm1) / 2.0_r8k
    IF (ABS(delh) >= 0.01_r8k) THEN
        drhodh = delrho / delh
    ELSE
        prop(5) = (h0k + 0.01_r8k) * 2326.3_r8k
        CALL sth2x5 (aasth, prop, it, nogo)
        IF (nogo) RETURN
        drhodh = ((1.0_r8k / (prop(3) * 16.02_r8k)) - rhok) / 0.01_r8k
    ENDIF
    a = delh * drhodh / rhoAveIn
    IF (nqchn == 7) THEN
        gk = gkm1
    ELSE
        gk = gkm1 * (2.0_r8k + a) / (2.0_r8k - a) + 2.0_r8k * drhodh * qtav * delz / (rhoAveIn * (a - 2.0_r8k))
    ENDIF
    IF (delz == 0.0_r8k) THEN
        hk = hkm1
    ELSE
        gAveIn = (gk + gkm1) / 2.0_r8k
        alpha = gAveIn * delt / (rhoAveIn * delz)
        IF (alpha == -1.0_r8k) THEN
            h0kp1 = h0k + delh
            hk = h0kp1 + delt * qtav / rhoAveIn
        ELSE
            hk = h0km1 - (hkm1 - h0k) * (1.0_r8k - alpha) / (1.0_r8k + alpha) + &
              &  2.0_r8k * delt * qtav / (rhoAveIn * (1.0_r8k + alpha))
        ENDIF
    ENDIF
    !
    END SUBROUTINE transh
    !
    !
    SUBROUTINE prntc (acond, z, Time, nchn, k, unit)
    USE Kinds
    USE Conversions, ONLY : tfk
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine prints out coolant conditions used as boundary conditions by FrapTran.
    !> Subroutine arguments are defined in Subroutine cool.
    !>@author
    !>
    !
    ! Input
    !
    ! acond - Coolant condition values
    ! z     - Elevation corresponding with tbulk value (ft)
    ! Time  - Time (sec)
    ! nchn  - # of T/H channels
    ! k     - Axial node numer
    ! unit  - .TRUE. (british units), .FALSE. = (SI units)
    !
    INTEGER(ipk) :: iu, i
    INTEGER(ipk), INTENT(IN) :: nchn, k
    REAL(r8k), INTENT(IN) :: z, Time
    REAL(r8k), PARAMETER :: ft = 0.3048_r8k
    REAL(r8k), PARAMETER :: spen = 2326.0_r8k 
    REAL(r8k), PARAMETER :: psi = 6.89476e3_r8k
    REAL(r8k), PARAMETER :: aflow = 1.35623e-3_r8k
    REAL(r8k), PARAMETER :: dens = 16.01846337_r8k
    LOGICAL, INTENT(IN) :: unit
    REAL(r8k), DIMENSION(7) :: a
    REAL(r8k), DIMENSION(30,10), INTENT(IN) :: acond
    CHARACTER(LEN=8), DIMENSION(2,8), PARAMETER :: Units = reshape([ &
      &          '(ft)    ', '(m)     ', &
      &          '(btu/lb)', '(J/kg)  ', &
      &          ' (psia) ', '(n/m**2)', &
      &          '(lbm/hr.', '(kg/sec.', &
      &          'ft**2)  ', 'm**2)   ', &
      &          '(F)     ', '(K)     ', &
      &          '(ft**3/l', '(m**3/kg', &
      &          'bm)     ', ')       ' ], [2, 8])
    !
    IF (unit) THEN
        iu = 1
    ELSE
        iu = 2
    ENDIF
    !
    IF (k == 1) THEN
        WRITE(ounit,50) Time
50      FORMAT(/,20x,'*** Coolant conditions at time = ',1pe13.6,' sec ***')
        WRITE(ounit,70) Units(iu,2), Units(iu,2)
70      FORMAT(/,25x,'Lower plenum enthalpy   Upper plenum enthalpy',/,32x,a8,16x,a8)
        a(1) = acond(14,1)
        a(2) = acond(15,1)
        IF (.NOT. unit) THEN !Convert units
            a(1) = a(1) * spen
            a(2) = a(2) * spen
        END IF
        WRITE(ounit,80) a(1), a(2)
80      FORMAT(16x,2(1pe24.4))
    END IF
    IF (k == 0 .OR. k == 1) WRITE(ounit,120) (Units(iu,i), i=1,8)
120 FORMAT(/,' Channel no.',3x,'Elevation',7x,'Enthalpy',8x,'Pressure',8x,'Mass flux',7x,'Temperature',5x, &
      &      'Specific vol.',3x,'Quality',/,15x,a8,8x,a8,8x,a8,8x,a8,a8,a8,8x,a8,a8 /)
    DO i = 1, nchn
        a(1) = z
        a(2) = acond(13,i)
        a(3) = acond(16,i)
        a(4) = acond(12,i)
        a(5) = acond(17,i)
        a(6) = acond(18,i)
        a(7) = acond(19,i)
        IF (.NOT. unit) THEN !Convert units
            a(1) = a(1) * ft
            a(2) = a(2) * spen
            a(3) = a(3) * psi
            a(4) = a(4) * aflow
            a(5) = tfk(a(5))
            a(6) = a(6) / dens
        END IF
        WRITE(ounit,250) i, a
250     FORMAT(4x,i5,7(2x,1pe14.4))
    END DO
    !
    END SUBROUTINE prntc
    !
    !
    !
    SUBROUTINE prntmp (tbulk, CoolPress, Time, z, unit, k)
    USE Kinds
    USE Conversions, ONLY : tfk
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine prints out bulk temperatures when heat transfer coefficients for cladding surface prescribed (nqchn = 3)
    !>@author
    !>
    !
    ! Input
    !
    ! tbulk     -  coolant temperature (F)
    ! CoolPress -  coolant pressre (psia)
    ! Time      -  time (sec)
    ! z         -  elevation corresponding with tbulk value (ft)
    ! unit      - .TRUE. (British units), .FALSE. = (SI units)
    ! k         -  axial node numer
    !
    INTEGER(ipk), INTENT(IN) :: k
    REAL(r8k) :: dum1, dum2, dum3
    REAL(r8k), INTENT(IN) :: tbulk, CoolPress, Time, z
    REAL(r8k), PARAMETER :: cvz = 0.3048_r8k
    REAL(r8k), PARAMETER :: cvp = 6.894756e3_r8k
    LOGICAL, INTENT(IN) :: unit
    !
    IF (k == 1) THEN
        WRITE(ounit,15) Time
15      FORMAT(///,'     Coolant conditions at time = ',e12.5,' sec ')
        IF (unit) THEN
            WRITE(ounit,20)
20          FORMAT(/,' Elevation(ft)    Coolant Temperature(F)  Pressure(psia) ')
        ELSE
            WRITE(ounit,52)
52          FORMAT(/,' Elevation(m)     Coolant Temperature(K)  Pressure(n/m**2) ')
        ENDIF
    ENDIF
    IF (unit) THEN
        dum1 = z
        dum2 = tbulk
        dum3 = CoolPress
    ELSE
        dum1 = cvz * z
        dum2 = tfk(tbulk)
        dum3 = cvp * CoolPress
    ENDIF
    WRITE(ounit,151) dum1, dum2, dum3
151 FORMAT(2x,e11.4,12x,e11.4,9x,e11.4)
    !
    END SUBROUTINE prntmp
    !
    !
END MODULE Coolant