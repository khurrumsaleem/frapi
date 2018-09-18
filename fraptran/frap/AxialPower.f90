MODULE AxialPower
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to calculate axial power as a function of time.
    !> Subroutines include power, fans
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    !
    CONTAINS
    !
    SUBROUTINE power (nptha, npaxp, AxNodElevat, Time, TimeIncrement, rf, pkw, p2, p1, powave, powopi, timop, &
      &               fpdcay, mpdcay, NSteadyTrans, powict, powimx, ntstep, ExtentOfBow, nqbow, pkw0, Gscale, &
      &               powavend, pkwnd, rvoid)
    USE Kinds
    USE conversions_fraptran, ONLY : pi, powcnv, sechr, ftin
    USE functions_fraptran, ONLY : polate
    USE variables_fraptran, ONLY : RodAvePower, AxPowProfile, zvoid1, zvoid2, naxn, nprofile
    IMPLICIT NONE
    !>@brief
    !> Subroutine specifies fuel rod power as function of time and axial elevation
    !
    ! Input/Output
    !
    ! RodAvePower(odd)  - averaged-over-length fuel rod linear power (kW/ft)
    ! RodAvePower(even) - time (sec)
    ! AxPowProfile(:,:) - (First dimension) : Ratio of linear fuel rod power (Odd #s) to average fuel rod linear power 
    !                                         at axial level AxPowProfile (Even #s)
    !                   - (Second dimension) : Axial Profile #
    ! nptha             - number of linear power versus time pairs
    ! npaxp             - number of axial power ratio versus elevation pairs
    ! nprofile          - Axial profile #
    ! AxNodElevat       - axial elevation (ft)
    ! Time              - time (sec)
    ! rf                - cold state radius to outside surface of fuel pellets (ft)
    ! pkw               - fuel rod linear power at axial elevation y (kW/ft)
    ! p2                - averaged radially fuel rod power density (btu/ft**3) at time Time
    ! p1                - power at time Time - TimeIncrement (btu/ft**3)
    ! powop             - operating power at time of loca initiation (kW/ft) (averaged over fuel rod length)
    ! timop             - time at which fuel rods were at operating power (sec)
    ! fpdcay            - factor applied to ans standard decay heat formula
    ! NSteadyTrans      - steady-state or transient indicator (1 = Steady-state, 2 = Transient)
    ! npramp            - number of the power step being performed
    ! ExtentOfBow       - bowing extent at elevation AxNodElevat (0 = None, 1 = Contact[full bow])
    ! nqbow             - switch to model effect of bowing on power (0 = No, 1 = Yes)
    ! pkw0              - linear power at start of time step (kW/ft)
    !
    INTEGER(ipk) :: nqbow, ntstep, nsteadytrans, mpdcay, npaxp, nptha
    REAL(r8k) :: axf, extentofbow, framp, powimx, powict, timin, time, tim0, powave, pkw, &
      &          pkw0, axnodelevat, p2, rf, tm1, timeincrement, p1, powop, powopi, pkwn, pkwn0, &
      &          p1n, p2n, powavn, t0, timop, frac, t1, ta, fpdcay, fp, ts, a1, a2, b1, b2,Gscale, &
      &          powavend, pkwnd, rvoid
    !
    axf = polate (AxPowProfile(1:2*npaxp,nprofile), AxNodElevat, npaxp) * Gscale
    IF (nqbow == 1 .AND. ExtentOfBow >= 0.3_r8k) axf = &
      &     axf * (1.0_r8k + 0.01_r8k * (0.94_r8k * ExtentOfBow - 2.84_r8k * ExtentOfBow ** 2))
    framp = 1.0_r8k
    IF (ntstep < 1 .AND. powimx >= 1.0e-10_r8k) framp = powict / powimx
    timin = Time
    tim0 = RodAvePower(2)
    IF (timin < tim0) timin = tim0
    powave = framp * polate (RodAvePower, timin, nptha)
    IF (powave < 1.0e-20_r8k) powave = 1.0e-20_r8k
    pkw = powave * axf
    pkw0 = pkw
    IF (AxNodElevat > zvoid2 .OR. AxNodElevat < zvoid1) THEN
        p2 = powcnv * pkw / (pi * rf ** 2)
    ELSE
        p2 = powcnv * pkw / (pi * (rf ** 2 - rvoid ** 2))
    ENDIF
    IF (NSteadyTrans /= 1) THEN
        tm1 = Time - TimeIncrement
        timin = tm1
        IF (timin < tim0) timin = tim0
        pkw0 = polate (RodAvePower, timin, nptha) * axf
        IF (pkw0 < 1.0e-20_r8k) pkw0 = 1.0e-20_r8k
        IF (AxNodElevat > zvoid2 .OR. AxNodElevat < zvoid1) THEN
          p1 = powcnv * pkw0 / (pi * rf ** 2)
        ELSE
          p1 = powcnv * pkw0 / (pi * (rf ** 2 - rvoid ** 2))
        ENDIF
    ENDIF
    ! Save global and local LHGR without contributions from decay heat. 
    ! These data are used for subsequent scaling.
    powavend = powave
    pkwnd = pkw
    IF (mpdcay /= 1) RETURN
    ! power transient specified by ans standard decay formula in power
    powop = framp * powopi
    pkwn = pkw
    pkwn0 = pkw0
    IF (NSteadyTrans /= 1) p1n = p1
    p2n = p2
    powavn = powave
    ! check to see if time includes total operating time or only time from start of transient
    IF (Time >= timop) THEN
        ts = Time - timop
        t0 = timop
    ELSE
        ts = Time
        t0 = timop
    ENDIF
    IF (ts < 0.001_r8k) ts = 0.001_r8k
    frac = fans (t0, ts)
    pkw = frac * axf * powop
    powave = pkw / axf
    IF (AxNodElevat > zvoid2 .OR. AxNodElevat < zvoid1) THEN
        p2 = powcnv * pkw / (pi * rf * rf)
    ELSE
        p2 = powcnv * pkw / (pi * (rf ** 2 - rvoid ** 2))
    ENDIF
    IF (NSteadyTrans /= 1) THEN
        t1 = ts - TimeIncrement
        ta = t1
        IF (t1 < 0.001_r8k) t1 = 0.001_r8k
        IF (ta < 0.0_r8k) ta = 0.0_r8k
        frac = fans (t0, t1)
        pkw0 = frac * axf * powop
        IF (AxNodElevat > zvoid2 .OR. AxNodElevat < zvoid1) THEN
            p1 = powcnv * pkw0 / (pi * rf ** 2)
        ELSE
            p1 = powcnv * pkw0 / (pi * (rf ** 2 - rvoid ** 2))
        ENDIF
    ENDIF
    ! apply factor to ans standard formula
    IF (fpdcay <= 0.9999_r8k .OR. fpdcay >= 1.0001_r8k) THEN
        fp = fpdcay
        pkw = fp * pkw
        powave = pkw / axf
        p2 = fp * p2
        IF (NSteadyTrans /= 1) THEN
            pkw0 = fp * pkw0
            p1 = fp * p1
        ENDIF
    ENDIF
    IF (NSteadyTrans /= 1) p1 = p1 + p1n
    p2 = p2 + p2n
    pkw = pkw + pkwn
    pkw0 = pkw0 + pkwn0
    IF (NSteadyTrans == 1) pkw0 = pkw
    powave = powave + powavn
    !
    END SUBROUTINE power
    !
    !
    !
    PURE FUNCTION fans (t0, ts)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This is the function for the ANS 5.1 decay heat.
    !>@author
    !> Developed by Ken Geelhood, PNNL
    !>@date
    !> May 2015
    !
    REAL(r8k), INTENT(IN) :: t0, ts
    REAL(r8k) :: t00, sum, fans
    ! t0 is time of operation (s)
    ! ts is time since reactor shutdown (s)
    ! alpha and lambda taken from Table 9 of ANS-5.1-2004
    REAL(r8k), DIMENSION(23), PARAMETER :: alpha = [ 5.28E-04_r8k, 6.86E-01_r8k, 4.08E-01_r8k, 2.19E-01_r8k, &
      &    5.77E-02_r8k, 2.25E-02_r8k, 3.34E-03_r8k, 9.37E-04_r8k, 8.09E-04_r8k, 1.96E-04_r8k, 3.26E-05_r8k, &
      &    7.58E-06_r8k, 2.52E-06_r8k, 4.98E-07_r8k, 1.85E-07_r8k, 2.66E-08_r8k, 2.24E-09_r8k, 8.96E-12_r8k, &
      &    8.60E-11_r8k, 2.11E-14_r8k, 7.12E-16_r8k, 8.11E-17_r8k, 9.47E-17_r8k ]
    REAL(r8k), DIMENSION(23), PARAMETER :: lambda = [2.72E+00_r8k, 1.03E+00_r8k, 3.14E-01_r8k, 1.18E-01_r8k, &
      &    3.44E-02_r8k, 1.18E-02_r8k, 3.61E-03_r8k, 1.40E-03_r8k, 6.26E-04_r8k, 1.89E-04_r8k, 5.51E-05_r8k, &
      &    2.10E-05_r8k, 9.99E-06_r8k, 2.54E-06_r8k, 6.63E-07_r8k, 1.23E-07_r8k, 2.72E-08_r8k, 3.30E-09_r8k, &
      &    7.42E-10_r8k, 2.47E-10_r8k, 1.56E-13_r8k, 2.26E-14_r8k, 2.05E-14_r8k ]
    INTEGER(ipk) :: i
    ! note (a) in table 9 of ANS-5.1-2004 says to use 1e13 for any time over 1e13
    t00 = t0
    IF (t0 > 1.0e13_r8k) t00 = 1.0e13_r8k
    sum = 0.0_r8k
    ! use equation for F(t,T) in note (a) of table 9 in ANS-5.1-2004
    DO i = 1, 23
        sum = sum + alpha(i) / lambda(i) * EXP(-lambda(i) * ts) * (1.0_r8k - EXP(-lambda(i) * t00))
    END DO
    ! use 200 MeV/fission to convert to fraction of previous reactor power
    fans = sum / 200.0_r8k
    !
    END FUNCTION fans  
    !
END MODULE AxialPower












