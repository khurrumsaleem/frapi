MODULE sth2x
    USE Kinds
    USE variables_fraptran, ONLY : h2ounit, ounit, nt, np, ns, ns2, klp, klp2, llp, nt5, jpl
    USE WaterPropertyArray
    !>@brief
    !> This module contains all of the water properties
    !>@author
    !> Coded by Ian Porter, NRC April, 2014
    !
    PRIVATE
    PUBLIC :: sth2x0, sth2xi, sth2x3, sth2x2, sth2x5, surten, visc, thcon, viscol, voidratio
    !
    CONTAINS
    !
    SUBROUTINE sth2x0 (t, press, err)
    USE Kinds
    IMPLICIT NONE
    !>@author
    !> Id: envrl.s,v 2.102 1999 / 05 / 19 23:08:13 randyt Exp randyt
    !>@brief
    !> Given temperature (t), returns saturation pressure (press) and err (error).
    !> Set (err) true if temperature is below triple point or over critical temperature.
    REAL(r8k) :: fr, fr1
    REAL(r8k), INTENT(IN) :: t
    REAL(r8k), INTENT(OUT) :: press
    REAL(r8k), PARAMETER :: crt = 647.3_r8k
    REAL(r8k), PARAMETER :: crp = 22120000.0_r8k
    REAL(r8k), PARAMETER :: ctrp = 273.16_r8k
    LOGICAL, INTENT(OUT) :: err
    REAL(r8k), DIMENSION(9), PARAMETER :: k = [  -7.691234564_r8k,   -26.08023696_r8k, -168.1706546_r8k, &
      &                                         6.423285504e1_r8k, -1.189646225e2_r8k,  4.167117320_r8k, &
      &                                         2.097506760e1_r8k,          1.0e9_r8k,          6.0_r8k ]
    !
    IF (t < ctrp .OR. t > crt) THEN
        ! Temperature is outside of range. Return an error.
        err = .TRUE.
    ELSE
        fr = t / crt
        fr1 = 1.0_r8k - fr
        press = crp * EXP((((((k(5) * fr1 + k(4)) * fr1 + k(3)) * fr1 + k(2)) * fr1 + k(1)) * fr1) / &
          &     (((k(7) * fr1 + k(6)) * fr1 + 1.0_r8k) * fr) - fr1 / (k(8) * fr1 * fr1 + k(9)))
        err = .FALSE.
    ENDIF
    !
    END SUBROUTINE sth2x0
    !
    !
    !
    SUBROUTINE sth2x2 (a, prop, err)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Compute water thermodynamic properties as a function of temperature and quality
    !>@author
    !> Id: envrl.s,v 2.102 1999 / 05 / 19 23:08:13 randyt Exp randyt
    !> Updated by Ian Porter, NRC, 05/19/2016 to conform with Fortran 2015 standard
    INTEGER(ipk) :: ip, jp, jpp, kp2, kp, ia, ib
    REAL(r8k) :: unp, fr, fr1, pa, ta, pb, tb, hfg1, hfg2, dpdt1, dpdt2, f1, f2, d1,d2, c0, c1, c2, c3
    REAL(r8k), PARAMETER :: crt = 647.3_r8k
    REAL(r8k), PARAMETER :: crp = 22120000.0_r8k
    REAL(r8k), PARAMETER :: pxxx = 1378.951459_r8k
    REAL(r8k), PARAMETER :: pxxy = 3102640.782_r8k
    REAL(r8k), PARAMETER :: pxx1 = 1.450377377e-3_r8k
    REAL(r8k), PARAMETER :: pxx2 = 1.450377377e-4_r8k
    REAL(r8k), PARAMETER :: plow = 611.2444_r8k
    LOGICAL :: s1, s2, s3
    LOGICAL, INTENT(OUT) :: err
    INTEGER(ipk), DIMENSION(2) :: iunp
    REAL(r8k), DIMENSION(:) :: a
    REAL(r8k), DIMENSION(:) :: prop
    REAL(r8k), DIMENSION(3), PARAMETER :: cc = [   0.84488898_r8k,       2.9056480_r8k,      219.74589_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: b = [   6669.352222_r8k,       -4658.899_r8k,    1376.536722_r8k, &
      &                                          -201.9126167_r8k,     14.82832111_r8k,  -0.4337434056_r8k ]
    REAL(r8k), DIMENSION(9), PARAMETER :: k = [  -7.691234564_r8k,    -26.08023696_r8k,   -168.1706546_r8k, &
      &                                         6.423285504e1_r8k,  -1.189646225e2_r8k,    4.167117320_r8k, &
      &                                         2.097506760e1_r8k,           1.0e9_r8k,            6.0_r8k ]
    REAL(r8k), DIMENSION(9), PARAMETER :: c = [   274.9043833_r8k,     13.66254889_r8k,    1.176781611_r8k, &
      &                                             -0.189693_r8k,   8.74535666e-2_r8k,  -1.7405325e-2_r8k, &
      &                                        2.147682333e-3_r8k, -1.383432444e-4_r8k, 3.800086611e-6_r8k ]
    EQUIVALENCE (unp, iunp(1))

    ! Compute water thermodynamic properties as a function of pressure and quality
    s1 = .TRUE.
    
    ! Temporary patch to be able to do ice condenser debug runs
    prop(2) = MAX(prop(2), plow)
    prop(10) = prop(2)
    IF (prop(2) < plow .OR. prop(2) > crp) THEN
        err = .TRUE.
        RETURN
    END IF
    
    ! Check based on coolant pressure
    IF (prop(2) < pxxx) THEN
        fr = LOG(prop(2))
        prop(1) = (cc(1) * fr + cc(2)) * fr + cc(3)
    ELSE IF (prop(2) > pxxy) THEN
        fr = LOG(pxx2 * prop(2))
        prop(1) = ((((fr * b(6) + b(5)) * fr + b(4)) * fr + b(3)) * fr + b(2)) * fr + b(1)
        prop(1) = MIN(prop(1), crt)
    ELSE
        fr = LOG(pxx1 * prop(2))
        prop(1) = (((((((fr * c(9) + c(8)) * fr + c(7)) * fr + c(6)) * fr + c(5)) * fr + c(4)) * fr + &
          &       c(3)) * fr + c(2)) * fr + c(1)
    END IF
    
    fr = prop(1) / crt
    fr1 = 1.0_r8k - fr
    d1 = ((((k(5) * fr1 + k(4)) * fr1 + k(3)) * fr1 + k(2)) * fr1 + k(1)) * fr1
    d2 = (((5.0_r8k * k(5) * fr1 + 4.0_r8k * k(4)) * fr1 + 3.0_r8k * k(3)) * fr1 + 2.0_r8k * k(2)) * fr1 + k(1)
    c2 = k(7) * fr1
    c1 = (c2 + k(6)) * fr1 + 1.0_r8k
    c2 = 2.0_r8k * c2 + k(6)
    f2 = k(8) * fr1
    f1 = 1.0_r8k / (f2 * fr1 + k(9))
    f2 = 2.0_r8k * f2
    hfg1 = 1.0_r8k / (fr * c1)
    hfg2 = fr1 * f1
    pa = crp * EXP(d1 * hfg1 - hfg2)
    prop(1) = MAX(prop(1) + (prop(2) - pa) * crt / (pa * ((d1 * hfg1 * (fr * c2 - c1) - d2) * hfg1 + &
      &          (1.0_r8k - hfg2 * f2) * f1)), 273.16_r8k)
    
    ! Check for valid input
    IF (prop(1) < a(1) .OR. prop(1) > a(ns))  THEN
        err = .TRUE.
        RETURN
    END IF
    ! Check for valid input
    IF (prop(9) < 0.0_r8k .OR. prop(9) > 1.0_r8k)  THEN
        err = .TRUE.
        RETURN
    END IF
    
    IF (.NOT. s1) THEN
        fr = prop(1) / crt
        fr1 = 1.0_r8k - fr
        prop(10) = crp * EXP((((((k(5) * fr1 + k(4)) * fr1 + k(3)) * fr1 + k(2)) * fr1 + k(1)) * fr1) / &
          &        (((k(7) * fr1 + k(6)) * fr1 + 1.0_r8k) * fr) - fr1 / (k(8) * fr1 * fr1 + k(9)))
        prop(2) = prop(10)
    END IF
    
    unp = prop(23)
    ip = iunp(1)
    jp = iunp(2)
    s2 = .FALSE.
    s3 = .FALSE.
    IF (ip <= 0 .OR. ip >= ns) ip = 1
    IF (jp <= 0 .OR. jp >= ns2) jp = 1
    
    ! Set indexes in temperature tables for saturation computations
    Temp_Loop_1: DO
        IF (prop(1) >= a(ip)) EXIT Temp_Loop_1
        ip = ip - 1
    END DO Temp_Loop_1
    
    Temp_Loop_2: DO
        IF (prop(1) < a(ip + 1)) EXIT Temp_Loop_2
        ip = ip + 1
    END DO Temp_Loop_2
    
    ! Set indexes in pressure tables for saturation computations
    jpp = jp + nt
    Press_Loop_1: DO
        IF (prop(10) >= a(jpp)) GOTO 110
        jpp = jpp - 1
        IF (jpp > nt) THEN
            CYCLE Press_Loop_1
        ELSE
            s3 = .TRUE.
            GOTO 112
        END IF
    END DO Press_Loop_1
    
110 Press_Loop_2: DO
        IF (prop(10) < a(jpp + 1)) EXIT Press_Loop_2
        jpp = jpp + 1
        IF (jpp < jpl) THEN
            CYCLE Press_Loop_2
        ELSE
            s2 = .TRUE.
            EXIT Press_Loop_2
        END IF
    END DO Press_Loop_2
    
112 jp = jpp - nt
    kp2 = klp2 + jp * 13
    kp = klp + ip * 13
    
    IF (s3 .OR. a(jpp) <= a(kp)) THEN
        ta = a(ip)
        pa = a(kp)
        ia = kp
    ELSE
        pa = a(jpp)
        ta = a(kp2)
        ia = kp2
    END IF
    
    IF (s2 .OR. a(jpp + 1) >= a(kp + 13)) THEN
        tb = a(ip + 1)
        pb = a(kp + 13)
        ib = kp + 13
    ELSE
        pb = a(jpp + 1)
        tb = a(kp2 + 13)
        ib = kp2 + 13
    END IF
    
    fr1 = prop(1) - ta
    fr = fr1 / (tb - ta)
    ! Two phase fluid.
    hfg1 = a(ia + 8) - a(ia + 2) + pa * (a(ia + 7) - a(ia + 1))
    hfg2 = a(ib + 8) - a(ib + 2) + pb * (a(ib + 7) - a(ib + 1))
    dpdt1 = hfg1 / (ta * (a(ia + 7) - a(ia + 1)))
    dpdt2 = hfg2 / (tb * (a(ib + 7) - a(ib + 1)))
    f1 = a(ia + 1) * (a(ia + 3) - a(ia + 4) * dpdt1)
    f2 = a(ib + 1) * (a(ib + 3) - a(ib + 4) * dpdt2)
    d1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 1)
    c1 = d1
    c2 = 3.0_r8k * (a(ib + 1) - a(ia + 1)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(ib + 1) - a(ia + 1))
    prop(11) = c0 + fr * (c1 + fr * (c2 + fr * c3))
    f1 = a(ia + 7) * (a(ia + 9) - a(ia + 10) * dpdt1)
    f2 = a(ib + 7) * (a(ib + 9) - a(ib + 10) * dpdt2)
    d1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 7)
    c1 = d1
    c2 = 3.0_r8k * (a(ib + 7) - a(ia + 7)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(ib + 7) - a(ia + 7))
    prop (12) = c0 + fr * (c1 + fr * (c2 + fr * c3))
    ! Two phase fluid.
    prop(13) = a(ia + 2) + (a(ib + 2) - a(ia + 2)) * fr
    prop(14) = a(ia + 8) + (a(ib + 8) - a(ia + 8)) * fr
    prop(15) = prop(13) + prop(10) * prop(11)
    prop(16) = prop(14) + prop(10) * prop(12)
    prop(17) = a(ia + 3) + (a(ib + 3) - a(ia + 3)) * fr
    prop(18) = a(ia + 9) + fr * tb / prop(1) * (a(ib + 9) - a(ia + 9))
    prop(19) = a(ia + 4) + (a(ib + 4) - a(ia + 4)) * fr
    prop(20) = a(ia + 10) + (prop(10) - pa) / (pb - pa) * pb / prop(10) * (a(ib + 10) - a(ia + 10))
    prop(21) = a(ia + 5) + (a(ib + 5) - a(ia + 5)) * fr
    prop(22) = a(ia + 11) + (a(ib + 11) - a(ia + 11)) * fr
    prop(25) = a(ia + 6) + (a(ib + 6) - a(ia + 6)) * fr
    prop(26) = a(ia + 12) + (a(ib + 12) - a(ia + 12)) * fr
    fr = 1.0_r8k - prop(9)
    prop(3) = fr * prop(11) + prop(9) * prop(12)
    prop(4) = fr * prop(13) + prop(9) * prop(14)
    prop(5) = fr * prop(15) + prop(9) * prop(16)
    prop(24) = fr * prop(25) + prop(9) * prop(26)
    iunp(1) = ip
    iunp(2) = jp
    prop(23) = unp
    err = .FALSE.
    
    END SUBROUTINE sth2x2
    !
    !
    !
    SUBROUTINE sth2x3 (a, prop, it, err)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Compute water thermodynamic properties as a function of temperature and pressure
    !>@author
    !> Id: envrl.s,v 2.102 1999 / 05 / 19 23:08:13 randyt Exp randyt
    INTEGER(ipk) :: ip, jp, jpp, kp2, kp, ia, ib, it, jpq, lpp, lqq, ic, id
    REAL(r8k) :: unp, fr, fr1, pa, ta, pb, tb, frn, px, frc, frc2, frd, frd2, hfg1, hfg2, dpdt1, &
      &          dpdt2, f1, f2, d1, d2, c0, c1, c2, c3, cv, ren
    REAL(r8k), PARAMETER :: crt = 647.3_r8k
    REAL(r8k), PARAMETER :: crp = 22120000.0_r8k
    LOGICAL :: err, s1, s2
    INTEGER(ipk), DIMENSION(2) :: iunp
    REAL(r8k), DIMENSION(:) :: a
    REAL(r8k), DIMENSION(:) :: prop
    REAL(r8k), DIMENSION(9), PARAMETER :: k = [   -7.691234564_r8k, -2.608023696e1_r8k, -1.681706546e2_r8k, &
      &                                          6.423285504e1_r8k, -1.189646225e2_r8k,    4.167117320_r8k, &
      &                                          2.097506760e1_r8k,          1.0e9_r8k,            6.0_r8k ]
    EQUIVALENCE (unp,iunp(1))
    ! Check for valid input
    IF (prop(1) < a(1) .OR. prop(1) > 5000.0_r8k) GOTO 101
    IF (prop(2) <= 0.0_r8k .OR. prop(2) > a(jpl)) GOTO 101
    unp = prop(23)
    ip = iunp(1)
    jp = iunp(2)
    IF (ip <= 0 .OR. ip > nt) ip = 1
    IF (jp <= 0 .OR. jp >= np) jp = 1
    IF (prop(1) >= a(nt)) GOTO 46
    ! Set indexes in temperature and pressure tables for saturation computations
11  IF (prop(1) >= a(ip)) GOTO 10
    ip = ip - 1
    GOTO 11
10  IF (prop(1) < a(ip + 1)) GOTO 12
    ip = ip + 1
    GOTO 10
12  IF (ip >= ns) GOTO 44
    s1 = .FALSE.
    s2 = .FALSE.
    fr = prop(1) / crt
    fr1 = 1.0_r8k - fr
    prop(10) = crp *  EXP((((((k(5) * fr1 + k(4)) * fr1 + k(3)) * fr1 + k(2)) * fr1 + k(1)) * fr1) / &
      &     (((k(7) * fr1 + k(6)) * fr1 + 1.0_r8k) * fr) - fr1 / (k(8) * fr1 * fr1 + k(9)))
    jpp = jp + nt
13  IF (prop(10) >= a(jpp)) GOTO 14
    jpp = jpp - 1
    IF (jpp > nt) GOTO 13
    s2 = .TRUE.
    GOTO 15
14  IF (prop(10) < a(jpp + 1)) GOTO 15
    jpp = jpp + 1
    IF (jpp < jpl) GOTO 14
    s1 = .TRUE.
15  kp2 = klp2 + (jpp - nt) * 13
    kp = klp + ip * 13
    IF (s2 .OR. a(jpp) <= a(kp)) GOTO 16
    pa = a(jpp)
    ta = a(kp2)
    ia = kp2
    GOTO 17
16  ta = a(ip)
    pa = a(kp)
    ia = kp
17  IF (s1 .OR. a(jpp + 1) >= a(kp + 13)) GOTO 18
    pb = a(jpp + 1)
    tb = a(kp2 + 13)
    ib = kp2 + 13
    GOTO 19
18  tb = a(ip + 1)
    pb = a(kp + 13)
    ib = kp + 13
19  fr1 = prop(1) - ta
    ! Compare input pressure to saturation pressure to determine vapor or liquid
    fr = fr1 / (tb - ta)
    IF (prop(2) <= prop(10)) GOTO 43
    it = 1
    prop(9)=0.0_r8k
    GOTO 50
46  ip = nt
44  it = 4
    GOTO 45
43  it = 3
45  prop(9)=1.0_r8k
50  jpq = jp + nt
    ! Search for single phase indexes.
51  IF (prop(2) >= a(jpq)) GOTO 53
    jpq = jpq - 1
    IF (jpq > nt) GOTO 51
    GOTO 90
53  IF (prop(2) < a(jpq + 1)) GOTO 54
    jpq = jpq + 1
    IF (jpq < jpl) GOTO 53
    GOTO 101
54  jp = jpq - nt
    lpp = llp + jp * nt5 + ip * 6
    lqq = lpp + nt5
    kp2 = klp2 + jp * 13
    frn = prop(1) - a(ip)
    IF (it - 3 < 0) GOTO 60
    IF (it - 3 == 0) GOTO 70
    IF (it - 3 > 0) GOTO 73
    ! Liquid phase.
60  IF (a(jpq) >= prop(10)) GOTO 61
    px = prop (10)
    s2 = .FALSE.
    GOTO 62
61  px = a(jpq)
    s2 = .TRUE.
    IF (jp > ns2) GOTO 63
    IF (a(kp2) > a(ip + 1)) GOTO 63
    frc = frn / (a(kp2) - a(ip))
    frc2 = prop(1) - a(kp2)
    ic = kp2 + 1
    GOTO 62
63  frc = frn / (a(ip + 1) - a(ip))
    frc2 = prop(1) - a(ip + 1)
    ic = lpp + 6
    frd = frc
    frd2 = frc2
    GOTO 65
62  IF (jp >= ns2) GOTO 67
    IF (a(kp2 + 13) > a(ip + 1)) GOTO 67
    frd = frn / (a(kp2 + 13) - a(ip))
    frd2 = prop(1) - a(kp2 + 13)
    id = kp2 + 14
    GOTO 66
67  frd = frn / (a(ip + 1) - a(ip))
    frd2 = prop(1) - a(ip + 1)
65  id = lqq + 6
66  IF (s2) GOTO 69
    hfg1 = a(ia + 8) - a(ia + 2) + pa * (a(ia + 7) - a(ia + 1))
    hfg2 = a(ib + 8) - a(ib + 2) + pb * (a(ib + 7) - a(ib + 1))
    dpdt1 = hfg1 / (ta * (a(ia + 7) - a(ia + 1)))
    dpdt2 = hfg2 / (tb * (a(ib + 7) - a(ib + 1)))
    f1 = a(ia + 1) * (a(ia + 3) - a(ia + 4) * dpdt1)
    f2 = a(ib + 1) * (a(ib + 3) - a(ib + 4) * dpdt2)
    d1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 1)
    c1 = d1
    c2 = 3.0_r8k * (a(ib + 1) - a(ia + 1)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(ib + 1) - a(ia + 1))
    prop (11) = c0 + fr * (c1 + fr * (c2 + fr * c3))
    prop(13) = a(ia + 2) + (a(ib + 2) - a(ia + 2)) * fr
    prop(17) = a(ia + 3) + (a(ib + 3) - a(ia + 3)) * fr
    prop(19) = a(ia + 4) + (a(ib + 4) - a(ia + 4)) * fr
    prop(21) = a(ia + 5) + (a(ib + 5) - a(ia + 5)) * fr
    prop(25) = a(ia + 6) + (a(ib + 6) - a(ia + 6)) * fr
    GOTO 56
69  d1 = a(lpp + 2) * a(lpp) * (frn - frc2)
    d2 = a(ic + 2) * a(ic) * (frn - frc2)
    c0 = a(lpp)
    c1 = d1
    c2 = 3.0_r8k * (a(ic) - a(lpp)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(ic) - a(lpp))
    prop (11) = c0 + frc * (c1 + frc * (c2 + frc * c3))
    prop(13) = a(lpp + 1) + (a(ic + 1) - a(lpp + 1)) * frc
    prop(17) = a(lpp + 2) + (a(ic + 2) - a(lpp + 2)) * frc
    prop(19) = a(lpp + 3) + (a(ic + 3) - a(lpp + 3)) * frc
    prop(21) = a(lpp + 4) + (a(ic + 4) - a(lpp + 4)) * frc
    prop(25) = a(lpp + 5) + (a(ic + 5) - a(lpp + 5)) * frc
56  d1 = a(lqq + 2) * a(lqq) * (frn - frd2)
    d2 = a(id + 2) * a(id) * (frn - frd2)
    c0 = a(lqq)
    c1 = d1
    c2 = 3.0_r8k * (a(id) - a(lqq)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(id) - a(lqq))
    prop (12) = c0 + frd * (c1 + frd * (c2 + frd * c3))
    prop(14) = a(lqq + 1) + (a(id + 1) - a(lqq + 1)) * frd
    prop(18) = a(lqq + 2) + (a(id + 2) - a(lqq + 2)) * frd
    prop(20) = a(lqq + 3) + (a(id + 3) - a(lqq + 3)) * frd
    prop(22) = a(lqq + 4) + (a(id + 4) - a(lqq + 4)) * frd
    prop(26) = a(lqq + 5) + (a(id + 5) - a(lqq + 5)) * frd
    IF (prop(11) > prop(12)) GOTO 83
    prop(3) = prop(11)
    fr = 0.0_r8k
    GOTO 84
83  prop(15) = prop(19) * prop(11)
    prop(16) = prop(20) * prop(12)
    fr1 = prop(16) - prop(15)
    IF (ABS(fr1) < 1.0e-10_r8k) GOTO 81
    fr = prop(11) + prop(12) - (a(jpq + 1) - px) * prop(15) * prop(16) / fr1
    fr1 = SQRT(fr * fr - 4.0_r8k * prop(11) * prop(12) * (prop(16) * (1.0_r8k - prop(19) * (prop(2) - px)) - prop(15) &
      &   * (1.0_r8k - prop(20) * (prop(2) - a(jpq + 1)))) / fr1)
    prop(3) = 0.5_r8k * (fr + fr1)
    IF (prop(3) > prop(11)) prop(3) = 0.5_r8k * (fr - fr1)
    IF (prop(3) >= prop(12)) GOTO 82
81  fr = (prop(2) - px) / (a(jpq + 1) - px)
    prop(3) = prop(11) * (1.0_r8k - fr) + prop(12) * fr
82  fr = (prop(3) - prop(11)) / (prop(12) - prop(11))
84  prop(4) = prop(13) + (prop(14) - prop(13)) * fr
    prop(5) = prop(4) + prop(2) * prop(3)
    prop(6) = prop(17) + (prop(18) - prop(17)) * fr
    prop(7) = prop(19) + (prop(20) - prop(19)) * fr
    prop(8) = prop(21) + (prop(22) - prop(21)) * fr
    prop(24) = prop(25) + (prop(26) - prop(25)) * fr
    GOTO 20
    ! Vapor phase.
70  s1 = .FALSE.
    IF (a(jpq + 1) <= prop(10)) GOTO 71
    frc = prop(10)
    hfg1 = a(ia + 8) - a(ia + 2) + pa * (a(ia + 7) - a(ia + 1))
    hfg2 = a(ib + 8) - a(ib + 2) + pb * (a(ib + 7) - a(ib + 1))
    dpdt1 = hfg1 / (ta * (a(ia + 7) - a(ia + 1)))
    dpdt2 = hfg2 / (tb * (a(ib + 7) - a(ib + 1)))
    f1 = a(ia + 7) * (a(ia + 9) - a(ia + 10) * dpdt1)
    f2 = a(ib + 7) * (a(ib + 9) - a(ib + 10) * dpdt2)
    d1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 7)
    c1 = d1
    c2 = 3.0_r8k * (a(ib + 7) - a(ia + 7)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(ib + 7) - a(ia + 7))
    prop (12) = c0 + fr * (c1 + fr * (c2 + fr * c3))
    prop(14) = a(ia + 8) + (a(ib + 8) - a(ia + 8)) * fr
    prop(18) = a(ia + 9) + fr * tb / prop(1) * (a(ib + 9) - a(ia + 9))
    prop(20) = a(ia + 10) + (prop(10) - pa) / (pb - pa) * pb / prop(10) * (a(ib + 10) - a(ia + 10))
    prop(22) = a(ia + 11) + (a(ib + 11) - a(ia + 11)) * fr
    prop(26) = a(ia + 12) + (a(ib + 12) - a(ia + 12)) * fr
    GOTO 72
71  IF (a(kp2 + 13) < a(ip)) GOTO 73
    frd = (prop(1) - a(kp2 + 13)) / (a(ip + 1) - a(kp2 + 13))
    ic = kp2 + 20
    GOTO 74
73  IF (ip == nt) GOTO 80
    frd = frn / (a(ip + 1) - a(ip))
    s1 = .TRUE.
    ic = lqq
74  frc = a(jpq + 1)
    c0 = a(ic)
    d1 = a(ic) * a(ic + 2) * (a(ip + 1) - a(ip))
    d2 = a(lqq + 8) * a(lqq + 6) * (a(ip + 1) - a(ip))
    c1 = d1
    c2 = 3.0_r8k * (a(lqq + 6) - a(ic)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(lqq + 6) - a(ic))
    prop (12) = c0 + frd * (c1 + frd * (c2 + frd * c3))
    prop(14) = a(ic + 1) + (a(lqq + 7) - a(ic + 1)) * frd
    prop(18) = a(ic + 2) + frd * a(ip + 1) / prop(1) * (a(lqq + 8) - a(ic + 2))
    prop(20) = a(ic + 3) + (a(lqq + 9) - a(ic + 3)) * frd
    prop(22) = a(ic + 4) + (a(lqq + 10) - a(ic + 4)) * frd
    prop(26) = a(ic + 5) + (a(lqq + 11) - a(ic + 5)) * frd
72  IF (s1) GOTO 75
    IF (a(kp2) < a(ip)) GOTO 77
    frd = (prop(1) - a(kp2)) / (a(ip + 1) - a(kp2))
    ia = kp2 + 7
    GOTO 76
77  frd = frn / (a(ip + 1) - a(ip))
75  ia = lpp
76  c0 = a(ia)
    d1 = a(ia) * a(ia + 2) * (a(ip + 1) - a(ip))
    d2 = a(lpp + 8) * a(lpp + 6) * (a(ip + 1) - a(ip))
    c1 = d1
    c2 = 3.0_r8k * (a(lpp + 6) - a(ia)) - d2 - 2.0_r8k * d1
    c3 = d2 + d1 - 2.0_r8k * (a(lpp + 6) - a(ia))
    prop (11) = c0 + frd * (c1 + frd * (c2 + frd * c3))
    prop(13) = a(ia + 1) + (a(lpp + 7) - a(ia + 1)) * frd
    prop(17) = a(ia + 2) + frd * a(ip + 1) / prop(1) * (a(lpp + 8) - a(ia + 2))
    prop(19) = a(ia + 3) + (a(lpp + 9) - a(ia + 3)) * frd
    prop(21) = a(ia + 4) + (a(lpp + 10) - a(ia + 4)) * frd
    prop(25) = a(ia + 5) + (a(lpp + 11) - a(ia + 5)) * frd
    fr = prop(12) * (frc - a(jpq))
    prop(3) = prop(11) * fr / (fr + (prop(11) - prop(12)) * (prop(2) - a(jpq)))
    fr = (prop(3) - prop(11)) / (prop(12) - prop(11))
    frn = fr * prop(12) / prop(3)
    prop(4) = prop(13) + (prop(14) - prop(13)) * fr
    prop(5) = prop(4) + prop(2) * prop(3)
    prop(6) = prop(17) + frn * (prop(18) - prop(17))
    prop(7) = prop(19) + (prop(20) - prop(19)) * fr
    prop(8) = prop(21) + frn * (prop(22) - prop(21))
    prop(24) = prop(25) + (prop(26) - prop(25)) * fr
20  iunp(1) = ip
    iunp(2) = jp
    prop(23) = unp
    err = .FALSE.
    RETURN
    ! Vapor phase, temperature greater than highest table temperature.
80  fr = a(lqq) * (a(jpq + 1) - a(jpq))
    prop(3) = a(lpp) * fr / (fr + (a(lpp) - a(lqq)) * (prop(2) - a(jpq)))
    fr = (prop(3) - a(lpp)) / (a(lqq) - a(lpp))
    frc = fr * a(lqq) / prop(3)
    prop(5) = a(lpp + 1) + (a(lqq + 1) - a(lpp + 1)) * fr + prop(2) * prop(3)
    prop(8) = a(lpp + 4) + frc * (a(lqq + 4) - a(lpp + 4))
    frd = prop(1) / a(nt)
    prop(3) = prop(3) * frd
    prop(5) = prop(5) + prop(8) * frn
    prop(4) = prop(5) - prop(2) * prop(3)
    prop(6) = (a(lpp + 2) + frc * (a(lqq + 2) - a(lpp + 2))) / frd
    prop(7) = a(lpp + 3) + (a(lqq + 3) - a(lpp + 3)) * fr
    prop(24) = a(lpp + 5) + (a(lqq + 5) - a(lpp + 5)) * fr
    cv = prop(8) - a(nt) * prop(6) * prop(6) * prop(3) / prop(7)
    prop(24) = prop(24) + cv * LOG(frd ** (prop(8) / cv))
    GOTO 20
    ! Vapor phase, pressure less than lowest table pressure
90  IF (it == 1) GOTO 101
    IF (prop(1) < a(klp2 + 13)) GOTO 92
    lpp = llp + nt5 + ip * 6
    IF (ip == nt) GOTO 95
    IF (a(ip) < a(klp2 + 13)) GOTO 93
    ia = ip
    lqq = lpp
    GOTO 91
93  ia = klp2 + 13
    lqq = ia + 7
91  fr = (prop(1) - a(ia)) / (a(ip + 1) - a(ia))
    prop(3) = (fr * a(lpp + 6) / a(ip + 1) + (1.0_r8k - fr) * a(lqq) / a(ia)) * a(jpq + 1) * prop(1) / prop(2)
    prop(4) = a(lqq + 1) + (a(lpp + 7) - a(lqq + 1)) * fr
    prop (6) = a(lqq + 2) + (a(lpp + 8) - a(lqq + 2)) * fr * a(ip + 1) / prop(1)
    prop(8) = a(lqq + 4) + (a(lpp + 10) - a(lqq + 4)) * fr
    prop(24) = a(lqq + 5) + (a(lpp + 11) - a(lqq + 5)) * fr
    ren = prop (2) * prop (3) / prop(1)
    prop(24) = prop(24) - ren * LOG(prop(2) / a(jpq + 1))
94  prop(5) = prop(4) + prop(2) * prop(3)
    prop(7) = 1.0_r8k / prop(2)
    jp = 1
    GOTO 20
92  prop(3) = (fr * pb * a(ib + 7) / tb + (1.0_r8k - fr) * pa * a(ia + 7) / ta) * prop(1) / prop(2)
    prop(4) = a(ia + 8) + (a(ib + 8) - a(ia + 8)) * fr
    prop(6) = 1.0_r8k / prop(1)
    prop(8) = a(ia + 11) + (a(ib + 11) - a(ia + 11)) * fr
    prop(24) = a(ia + 12) + (a(ib + 12) - a(ia + 12)) * fr
    ren = prop (2) * prop (3) / prop(1)
    prop(24) = prop(24) - ren * LOG(prop(2) / prop(10))
    GOTO 94
95  frd = prop(1) / a(nt)
    frc = a(nt + 1) * a(lpp)
    prop(3) = frc * frd / prop(2)
    prop(8) = a(lpp + 4)
    prop(5) = a(lpp + 1) + frc + prop(8) * (prop(1) - a(nt))
    prop(4) = prop(5) - prop(2) * prop(3)
    prop(6) = a(lpp + 2) / frd
    prop(7) = a(lpp + 3) * a(nt + 1) / prop(2)
    ren = prop (2) * prop (3) / prop(1)
    prop(24) = a(lpp + 5) + prop(8) * LOG(prop(1) / a(nt)) - ren * LOG(prop(2) / a(jpq + 1))
    GOTO 20
101 err = .TRUE.
    !
    END SUBROUTINE sth2x3
    !
    !
    !
    SUBROUTINE sth2x5 (a, prop, it, err)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Compute water thermodynamic properties as a function of pressure and total enthalpy
    !>@author
    !> Id: envrl.s,v 2.102 1999 / 05 / 19 23:08:13 randyt Exp randyt
    !
    INTEGER(ipk) :: it, ip, jp, jpp, ic, kp, kp2, ia, ib, lpp, lqq, id
    REAL(r8k) :: unp, fr, fr1, d1, d2, c2, c1, f2, f1, hfg1, hfg2, pa, ta, pb, tb, dpdt1, dpdt2, &
      &          c0, c3, ht, frn, frc2, ut, vt, frc3, px, frc, frd3, frd, pr, r2, s11i, s12i, fr2, &
      &          tr, frd2, cv, ren
    INTEGER(ipk) :: iunp(2)
    REAL(r8k), PARAMETER :: crt = 647.3_r8k
    REAL(r8k), PARAMETER :: crp = 22120000.0_r8k
    REAL(r8k), PARAMETER :: ctrp = 273.16_r8k
    REAL(r8k), PARAMETER :: pxxx = 1378.951459_r8k
    REAL(r8k), PARAMETER :: pxxy = 3102640.782_r8k
    REAL(r8k), PARAMETER :: pxx1 = 1.450377377e-3_r8k
    REAL(r8k), PARAMETER :: pxx2 = 1.450377377e-4_r8k
    LOGICAL :: err, s1, s2, s3
    REAL(r8k), DIMENSION(:) :: a
    REAL(r8k), DIMENSION(:) :: prop
    REAL(r8k), DIMENSION(3), PARAMETER :: cc = [  0.84488898_r8k,       2.9056480_r8k,      219.74589_r8k ]
    REAL(r8k), DIMENSION(6), PARAMETER :: b =  [ 6669.352222_r8k,       -4658.899_r8k,    1376.536722_r8k, &
      &                                         -201.9126167_r8k,     14.82832111_r8k,  -0.4337434056_r8k ]
    REAL(r8k), DIMENSION(9), PARAMETER :: c = [  274.9043833_r8k,     13.66254889_r8k,    1.176781611_r8k, &
      &                                            -0.189693_r8k,   8.74535666e-2_r8k,  -1.7405325e-2_r8k, &
      &                                       2.147682333e-3_r8k, -1.383432444e-4_r8k, 3.800086611e-6_r8k ]
    REAL(r8k), DIMENSION(9), PARAMETER :: k = [ -7.691234564_r8k,    -26.08023696_r8k,   -168.1706546_r8k, &
      &                                        6.423285504e1_r8k,  -1.189646225e2_r8k,    4.167117320_r8k, &
      &                                        2.097506760e1_r8k,           1.0e9_r8k,            6.0_r8k ]
    EQUIVALENCE (unp,iunp(1))
    !
    s3 = .FALSE.
    ! Check for valid input
23  IF (prop(2) <= 0.0_r8k .OR. prop(2) > a(jpl)) GOTO 1001
    unp = prop(23)
    ip = iunp(1)
    jp = iunp(2)
    IF (ip <= 0 .OR. ip >= nt) ip = 1
    IF (jp <= 0 .OR. jp >= np) jp = 1
    jpp = jp + nt
    s1 = .FALSE.
    ! set indexes in temperature and pressure tables for saturation computations
11  IF (prop(2) >= a(jpp)) GOTO 10
    jpp = jpp - 1
    IF (jpp > nt) GOTO 11
    jpp = jpp + 1
    jp = 1
    s1 = .TRUE.
    IF (prop(2) < a(klp + 13)) GOTO 44
    GOTO 12
10  IF (prop(2) < a(jpp + 1)) GOTO 12
    jpp = jpp + 1
    GOTO 10
12  jp = jpp - nt
    IF (prop(2) >= crp) GOTO 44
    IF (s3) GOTO 15
    IF (prop(2) < pxxx) GOTO 18
    IF (prop(2) > pxxy) GOTO 17
    fr = LOG(pxx1 * prop(2))
    prop(10) = (((((((fr * c(9) + c(8)) * fr + c(7)) * fr + c(6)) * fr + c(5)) * fr + c(4)) * fr &
      &     + c(3)) * fr + c(2)) * fr + c(1)
    GOTO 27
17  fr = LOG(pxx2 * prop(2))
    prop(10) = ((((fr * b(6) + b(5)) * fr + b(4)) * fr + b(3)) * fr + b(2)) * fr + b(1)
    GOTO 27
18  fr = LOG(prop(2))
    prop(10) = (cc(1) * fr + cc(2)) * fr + cc(3)
27  fr = prop(10) / crt
    fr1 = 1.0_r8k - fr
    d1 = ((((k(5) * fr1 + k(4)) * fr1 + k(3)) * fr1 + k(2)) * fr1 + k(1)) * fr1
    d2 = (((5.0_r8k * k(5) * fr1 + 4.0_r8k * k(4)) * fr1 + 3.0_r8k * k(3)) * fr1 + 2.0_r8k * &
      &  k(2)) * fr1 + k(1)
    c2 = k(7) * fr1
    c1 = (c2 + k(6)) * fr1 + 1.0_r8k
    c2 = 2.0_r8k * c2 + k(6)
    f2 = k(8) * fr1
    f1 = 1.0_r8k / (f2 * fr1 + k(9))
    f2 = 2.0_r8k * f2
    hfg1 = 1.0_r8k / (fr * c1)
    hfg2 = fr1 * f1
    pa = crp * EXP(d1 * hfg1 - hfg2)
    prop(10) = MAX(prop(10) + (prop(2) - pa) * crt / (pa * ((d1 * hfg1 * (fr * c2 - c1) - d2) * hfg1 &
      &     + (1.0_r8k - hfg2 * f2) * f1)),ctrp)
15  ic = ip
16  IF (prop(10) >= a(ic)) GOTO 13
    ic = ic - 1
    IF (ic > 0) GOTO 16
    ic = 1
    GOTO 14
13  IF (prop(10) < a(ic + 1)) GOTO 14
    ic = ic + 1
    IF (ic < ns) GOTO 13
    ic = ic - 1
14  kp = klp + ic * 13
    kp2 = klp2 + jp * 13
    IF (s1 .OR. a(jpp) <= a(kp)) GOTO 19
    pa = a(jpp)
    ta = a(kp2)
    ia = kp2
    GOTO 20
19  ta = a(ic)
    pa = a(kp)
    ia = kp
20  IF (a(jpp + 1) >= a(kp + 13)) GOTO 21
    pb = a(jpp + 1)
    tb = a(kp2 + 13)
    ib = kp2 + 13
    GOTO 22
21  pb = a(kp + 13)
    tb = a(ic + 1)
    ib = kp + 13
22  fr1 = prop(10) - ta
    fr = fr1 / (tb - ta)
    ! compute vsubf and vsubg to determine liquid, two phase, or vapor state
    hfg1 = a(ia + 8) - a(ia + 2) + pa * (a(ia + 7) - a(ia + 1))
    hfg2 = a(ib + 8) - a(ib + 2) + pb * (a(ib + 7) - a(ib + 1))
    dpdt1 = hfg1 / (ta * (a(ia + 7) - a(ia + 1)))
    dpdt2 = hfg2 / (tb * (a(ib + 7) - a(ib + 1)))
    f1 = a(ia + 1) * (a(ia + 3) - a(ia + 4) * dpdt1)
    f2 = a(ib + 1) * (a(ib + 3) - a(ib + 4) * dpdt2)
    c1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 1)
    c2 = 3.0_r8k * (a(ib + 1) - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (a(ib + 1) - c0)
    prop(11) = c0 + fr * (c1 + fr * (c2 + fr * c3))
24  prop(13) = a(ia + 2) + (a(ib + 2) - a(ia + 2)) * fr
    prop(15) = prop(13) + prop(2) * prop(11)
    IF (prop(5) <= prop(15)) GOTO 41
    f1 = a(ia + 7) * (a(ia + 9) - a(ia + 10) * dpdt1)
    f2 = a(ib + 7) * (a(ib + 9) - a(ib + 10) * dpdt2)
    c1 = f1 * (tb - ta)
    d2 = f2 * (tb - ta)
    c0 = a(ia + 7)
    c2 = 3.0_r8k * (a(ib + 7) - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (a(ib + 7) - c0)
    prop(12) = c0 + fr * (c1 + fr * (c2 + fr * c3))
    prop(14) = a(ia + 8) + (a(ib + 8) - a(ia + 8)) * fr
    prop(16) = prop(14) + prop(2) * prop(12)
    IF (prop(5) >= prop(16)) GOTO 43
    ! Two phase fluid.
    it = 2
    prop(17) = a(ia + 3) + (a(ib + 3) - a(ia + 3)) * fr
    prop(18) = a(ia + 9) + fr * tb / prop(10) * (a(ib + 9) - a(ia + 9))
    prop(19) = a(ia + 4) + (a(ib + 4) - a(ia + 4)) * fr
    prop(20) = a(ia + 10) + (prop(2) - pa) / (pb - pa) * pb / prop(2) * (a(ib + 10) - a(ia + 10))
    prop(21) = a(ia + 5) + (a(ib + 5) - a(ia + 5)) * fr
    prop(22) = a(ia + 11) + (a(ib + 11) - a(ia + 11)) * fr
    prop(25) = a(ia + 6) + (a(ib + 6) - a(ia + 6)) * fr
    prop(26) = a(ia + 12) + (a(ib + 12) - a(ia + 12)) * fr
    prop(9) = (prop(5) - prop(15)) / (prop(16) - prop(15))
    fr = 1.0_r8k - prop(9)
    prop(1) = prop(10)
    prop(3) = fr * prop(11) + prop(9) * prop(12)
    prop(4) = fr * prop(13) + prop(9) * prop(14)
    prop(24) = fr * prop(25) + prop(9) * prop(26)
    ip = ic
25  iunp(1) = ip
    iunp(2) = jp
    prop(23) = unp
    err = .FALSE.
    RETURN
44  it = 4
    GOTO 50
    ! Single phase fluid, search for single phase indexes.
41  it = 1
    IF (s1) GOTO 1001
    GOTO 50
43  it = 3
50  lpp = llp + jp * nt5 + ip * 6
51  ht = a(lpp + 1) + a(jpp) * a(lpp)
    IF (prop(5) >= ht) GOTO 52
    lpp = lpp - 6
    ip = ip - 1
    IF (ip > 0) GOTO 51
    lpp = lpp + 6
    ip = ip + 1
    GOTO 54
52  ht = a(lpp + 7) + a(jpp) * a(lpp + 6)
    IF (prop(5) <= ht) GOTO 54
    lpp = lpp + 6
    ip = ip + 1
    IF (ip < nt) GOTO 52
    IF (s1) GOTO 95
    lpp = lpp - 6
    ip = ip - 1
    GOTO 53
54  IF (s1) GOTO 96
53  kp = klp + ip * 13
    lqq = lpp + nt5
    frn = prop(2) - a(jpp)
    frc2 = prop(2) - a(jpp + 1)
    s3 = .FALSE.
    IF (it - 3 < 0) GOTO 58
    IF (it - 3 == 0) GOTO 70
    IF (it - 3 > 0) GOTO 48
48  IF (ip >= ns) GOTO 157
    GOTO 57
    ! Liquid phase.
58  ht = prop(15)
    ut = prop(13)
    vt = prop(11)
60  IF (a(ip) < prop(10)) GOTO 57
    ip = ip - 1
    IF (ip <= 0) GOTO 1001
    kp = kp - 13
    lpp = lpp - 6
    lqq = lqq - 6
    GOTO 60
57  s1 = .FALSE.
    IF (it == 4) GOTO 61
    IF (a(ip + 1) <= prop(10)) GOTO 61
    ta = prop(10)
    s2 = .FALSE.
    GOTO 62
61  ta = a(ip + 1)
    s2 = .TRUE.
    IF (ip + 1 > ns) GOTO 63
    IF (a(kp + 13) <= a(jpp)) GOTO 63
    frc3 = prop(2) - a(kp + 13)
    px = a(kp + 13)
    frc = a(jpp + 1) - a(kp + 13)
    ic = kp + 14
    GOTO 64
63  frc3 = frn
    frc = a(jpp + 1) - a(jpp)
    px = a(jpp)
    ic = lpp + 6
    frd3 = frc3
    frd = frc
    s1 = .TRUE.
64  pr = (prop(2) - px) / (a(jpp + 1) - px)
    c0 = 1.0_r8k / a(ic)
    r2 = 1.0_r8k / a(lqq + 6)
    c1 = c0 * a(ic + 3) * (a(jpp + 1) - px)
    d2 = r2 * a(lqq + 9) * (a(jpp + 1) - px)
    c2 = 3.0_r8k * (r2 - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (r2 - c0)
    s11i = c0 + pr * (c1 + pr * (c2 + pr * c3))
    prop(11) = 1.0_r8k / s11i
82  fr1 = (prop(11) - a(ic)) / (a(lqq + 6) - a(ic))
    prop(13) = a(ic + 1) + (a(lqq + 7) - a(ic + 1)) * fr1
    prop(15) = prop(13) + prop(2) * prop(11)
62  IF (s1) GOTO 65
    IF (ip > ns) GOTO 67
    IF (a(kp) <= a(jpp)) GOTO 67
    frd3 = prop(2) - a(kp)
    frd = a(jpp + 1) - a(kp)
    px = a(kp)
    id = kp + 1
    GOTO 66
67  frd3 = frn
    frd = a(jpp + 1) - a(jpp)
    px = a(jpp)
    s1 = .TRUE.
65  id = lpp
66  IF (s3) GOTO 68
    pr = (prop(2) - px) / (a(jpp + 1) - px)
    c0 = 1.0_r8k / a(id)
    r2 = 1.0_r8k / a(lqq)
    c1 = c0 * a(id + 3) * (a(jpp + 1) - px)
    d2 = r2 * a(lqq + 3) * (a(jpp + 1) - px)
    c2 = 3.0_r8k * (r2 - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (r2 - c0)
    s12i = c0 + pr * (c1 + pr * (c2 + pr * c3))
    prop (12) = 1.0_r8k / s12i
182 fr2 = (prop(12) - a(id)) / (a(lqq) - a(id))
    prop(14) = a(id + 1) + (a(lqq + 1) - a(id + 1)) * fr2
    prop(16) = prop(14) + prop(2) * prop(12)
    IF (prop(16) <= prop(5)) GOTO 68
    s2 = .TRUE.
    prop(11) = prop(12)
    prop(13) = prop(14)
    prop(15) = prop(16)
    fr1 = fr2
    ip = ip - 1
    ta = a(ip + 1)
    IF (ip <= 0) GOTO 1001
    kp = kp - 13
    lqq = lqq - 6
    lpp = lpp - 6
    ic = id
    GOTO 62
68  IF (prop(15) >= prop(5)) GOTO 59
    prop(12) = prop(11)
    prop(11) = vt
    prop(14) = prop(13)
    prop(13) = ut
    prop(16) = prop(15)
    prop(15) = ht
    fr2 = fr1
    lqq = lqq + 6
    lpp = lpp + 6
    kp = kp + 13
    ip = ip + 1
    IF (ip < ns) GOTO 158
    s3 = .FALSE.
    GOTO 157
158 id = ic
    s3 = .TRUE.
    GOTO 57
59  IF (s2) GOTO 69
    prop(17) = a(ia + 3) + (a(ib + 3) - a(ia + 3)) * fr
    prop(19) = a(ia + 4) + (a(ib + 4) - a(ia + 4)) * fr
    prop(21) = a(ia + 5) + (a(ib + 5) - a(ia + 5)) * fr
    prop(25) = a(ia + 6) + (a(ib + 6) - a(ia + 6)) * fr
    GOTO 56
69  prop(17) = a(ic + 2) + (a(lqq + 8) - a(ic + 2)) * fr1
    prop(19) = a(ic + 3) + (a(lqq + 9) - a(ic + 3)) * fr1
    prop(21) = a(ic + 4) + (a(lqq + 10) - a(ic + 4)) * fr1
    prop(25) = a(ic + 5) + (a(lqq + 11) - a(ic + 5)) * fr1
56  prop(18) = a(id + 2) + (a(lqq + 2) - a(id + 2)) * fr2
    prop(20) = a(id + 3) + (a(lqq + 3) - a(id + 3)) * fr2
    prop(22) = a(id + 4) + (a(lqq + 4) - a(id + 4)) * fr2
    prop(26) = a(id + 5) + (a(lqq + 5) - a(id + 5)) * fr2
    fr = (prop(5) - prop(16)) / (prop(15) - prop(16))
    frd = ta - a(ip)
    fr2 = frd * fr
    prop(1) = a(ip) + fr2
    fr1 = fr2 / frd
    tr = (prop(1) - a(ip)) / frd
    c0 = prop (12)
    c1 = prop(12) * prop(18) * frd
    d2 = prop(11) * prop(17) * frd
    c2 = 3.0_r8k * (prop(11) - prop(12)) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (prop(11) - prop(12))
    prop (3) = c0 + tr * (c1 + tr * (c2 + tr * c3))
    prop(4) = prop(5) - prop(2) * prop(3)
    prop (6) = (c1 + tr * (2.0_r8k * c2 + 3.0_r8k * tr * c3)) / (frd * prop(3))
    prop(7) = prop(20) + (prop(19) - prop(20)) * fr1
    prop(8) = prop(22) + (prop(21) - prop(22)) * fr1
    prop(24) = prop(26) + (prop(25) - prop(26)) * fr1
    prop(9) = 0.0_r8k
    GOTO 25
70  ht = prop(16)
    ut = prop(14)
    vt = prop(12)
    ! Vapor phase.
160 If (a(ip + 1) > prop(10)) GOTO 157
    ip = ip + 1
    kp = kp + 13
    lpp = lpp + 6
    lqq = lqq + 6
    GOTO 160
157 s1 = .FALSE.
    IF (it == 4) GOTO 161
    IF (a(ip) >= prop(10)) GOTO 161
    ta = prop(10)
    s2 = .FALSE.
    GOTO 162
161 ta = a(ip)
    s2 = .TRUE.
    IF (ip >= ns) GOTO 163
    IF (a(kp) >= a(jpp + 1)) GOTO 163
    frc3 = a(kp) - a(jpp)
    px = a(kp)
    ic = kp + 7
    GOTO 164
163 frc3 = a(jpp + 1) - a(jpp)
    px = a(jpp + 1)
    ic = lqq
    frd3 = frc3
    s1 = .TRUE.
164 fr1 = a(ic) * frc3
    pr = (prop(2) - a(jpp)) / (px - a(jpp))
    c0 = 1.0_r8k / a(lpp)
    r2 = 1.0_r8k / a(ic)
    c1 = c0 * a(lpp + 3) * (px - a(jpp))
    d2 = r2 * a(ic + 3) * (px - a(jpp))
    c2 = 3.0_r8k * (r2 - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (r2 - c0)
    s12i = c0 + pr * (c1 + pr * (c2 + pr * c3))
    prop (12) = 1.0_r8k / s12i
    frc2 = (prop(12) - a(lpp)) / (a(ic) - a(lpp))
    frc = frc2 * a(ic) / prop(12)
    prop(14) = a(lpp + 1) + (a(ic + 1) - a(lpp + 1)) * frc2
    prop(16) = prop(14) + prop(2) * prop(12)
162 If (s1) GOTO 165
    IF (ip + 1 > ns) GOTO 167
    IF (a(kp + 13) >= a(jpp + 1)) GOTO 167
    frd3 = a(kp + 13) - a(jpp)
    px = a(kp + 13)
    id = kp + 20
    GOTO 166
167 frd3 = a(jpp + 1) - a(jpp)
    px = a(jpp + 1)
    s1 = .TRUE.
165 id = lqq + 6
166 IF (s3) GOTO 168
    fr1 = a(id) * frd3
    pr = (prop(2) - a(jpp)) / (px - a(jpp))
    c0 = 1.0_r8k / a(lpp + 6)
    r2 = 1.0_r8k / a(id)
    c1 = c0 * a(lpp + 9) * (px - a(jpp))
    d2 = r2 * a(id + 3) * (px - a(jpp))
    c2 = 3.0_r8k * (r2 - c0) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (r2 - c0)
    s11i = c0 + pr * (c1 + pr * (c2 + pr * c3))
    prop (11) = 1.0_r8k / s11i
    frd2 = (prop(11) - a(lpp + 6)) / (a(id) - a(lpp + 6))
    frd = frd2 * a(id) / prop(11)
    prop(13) = a(lpp + 7) + (a(id + 1) - a(lpp + 7)) * frd2
    prop(15) = prop(13) + prop(2) * prop(11)
    IF (prop(15) >= prop(5)) GOTO 168
    s2 = .TRUE.
    ip = ip + 1
    lqq = lqq + 6
    lpp = lpp + 6
    IF (ip == nt) GOTO 80
    ta = a(ip)
    kp = kp + 13
    ic = id
    prop(12) = prop(11)
    prop(14) = prop(13)
    prop(16) = prop(15)
    frc2 = frd2
    frc = frd
    GOTO 162
168 If (prop(16) <= prop(5)) GOTO 159
    s3 = .TRUE.
    prop(11) = prop(12)
    prop(12) = vt
    prop(13) = prop(14)
    prop(14) = ut
    prop(15) = prop(16)
    prop(16) = ht
    frd2 = frc2
    frd = frc
    ip = ip - 1
    IF (ip <= 0) GOTO 1001
    kp = kp - 13
    lpp = lpp - 6
    lqq = lqq - 6
    IF (ip < ns) GOTO 57
    GOTO 157
159 If (s2) GOTO 169
    prop(18) = a(ia + 9) + fr * tb / prop(10) * (a(ib + 9) - a(ia + 9))
    prop(20) = a(ia + 10) + (prop(2) - pa) / (pb - pa) * pb / prop(2) * (a(ib + 10) - a(ia + 10))
    prop(22) = a(ia + 11) + (a(ib + 11) - a(ia + 11)) * fr
    prop(26) = a(ia + 12) + (a(ib + 12) - a(ia + 12)) * fr
    GOTO 156
169 prop(18) = a(lpp + 2) + frc * (a(ic + 2) - a(lpp + 2))
    prop(20) = a(lpp + 3) + (a(ic + 3) - a(lpp + 3)) * frc2
    prop(22) = a(lpp + 4) + frc * (a(ic + 4) - a(lpp + 4))
    prop(26) = a(lpp + 5) + (a(ic + 5) - a(lpp + 5)) * frc2
156 prop(17) = a(lpp + 8) + frd * (a(id + 2) - a(lpp + 8))
    prop(19) = a(lpp + 9) + (a(id + 3) - a(lpp + 9)) * frd2
    prop(21) = a(lpp + 10) + frd * (a(id + 4) - a(lpp + 10))
    prop(25) = a(lpp + 11) + (a(id + 5) - a(lpp + 11)) * frd2
    fr = (prop(5) - prop(16)) / (prop(15) - prop(16))
    frd = a(ip + 1) - ta
    fr2 = frd * fr
    prop(1) = ta + fr2
    fr1 = fr2 / frd
    tr = (prop(1) - ta) / frd
    c0 = prop (12)
    c1 = prop(12) * prop(18) * frd
    d2 = prop(11) * prop(17) * frd
    c2 = 3.0_r8k * (prop(11) - prop(12)) - d2 - 2.0_r8k * c1
    c3 = d2 + c1 - 2.0_r8k * (prop(11) - prop(12))
    prop (3) = c0 + tr * (c1 + tr * (c2 + tr * c3))
    prop(4) = prop(5) - prop(2) * prop(3)
    prop (6) = (c1 + tr * (2.0_r8k * c2 + 3.0_r8k * tr * c3)) / (frd * prop(3))
    prop(7) = prop(20) + (prop(19) - prop(20)) * fr1
    prop(8) = prop(22) + (prop(21) - prop(22)) * fr1
    prop(24) = prop(26) + (prop(25) - prop(26)) * fr1
99  prop(9) = 1.0_r8k
    GOTO 25
    ! Vapor phase, temperature greater than highest table temperature.
80  fr = a(lqq) * (a(jpp + 1) - a(jpp))
    prop(3) = a(lpp) * fr / (fr + (a(lpp) - a(lqq)) * frn)
    fr = (prop(3) - a(lpp)) / (a(lqq) - a(lpp))
    frc = fr * a(lqq) / prop(3)
    ht = a(lpp + 1) + (a(lqq + 1) - a(lpp + 1)) * fr + prop(2) * prop(3)
    prop(8) = a(lpp + 4) + frc * (a(lqq + 4) - a(lpp + 4))
    prop(1) = (prop(5) - ht + prop(8) * a(nt)) / prop(8)
    frd = prop(1) / a(nt)
    prop(3) = prop(3) * frd
    prop(4) = prop(5) - prop(2) * prop(3)
    prop(6) = (a(lpp + 2) + frc * (a(lqq + 2) - a(lpp + 2))) / frd
    prop(7) = a(lpp + 3) + (a(lqq + 3) - a(lpp + 3)) * fr
    prop(24) = a(lpp + 5) + (a(lqq + 5) - a(lpp + 5)) * fr
    cv = prop(8) - a(nt) * prop(6) * prop(6) * prop(3) / prop(7)
    prop(24) = prop(24) + cv * LOG(frd ** (prop(8) / cv))
    GOTO 99
    ! Vapor phase, pressure less than lowest table pressure
96  ht = a(klp2 + 21) + a(nt + 1) * a(klp2 + 20)
    IF (prop(5) < ht) GOTO 90
    IF (a(ip) < a(klp2 + 13)) GOTO 93
    ia = ip
    lqq = lpp
    ht = a(lqq + 1) + a(nt + 1) * a(lqq)
    GOTO 91
93  ia = klp2 + 13
    lqq = ia + 7
91  fr = (prop(5) - ht) / (a(lpp + 7) + a(nt + 1) * a(lpp + 6) - ht)
    fr1 = a(ip + 1) - a(ia)
    fr2 = fr1 * fr
    prop(1) = a(ia) + fr2
    fr1 = fr2 / fr1
    prop(3) = (fr1 * a(lpp + 6) / a(ip + 1) + (1.0_r8k - fr1) * a(lqq) / a(ia)) * a(nt + 1) * prop(1) / prop(2)
    prop (6) = a(lqq + 2) + (a(lpp + 8) - a(lqq + 2)) * fr * a(ip + 1) / prop(1)
    prop(8) = a(lqq + 4) + (a(lpp + 10) - a(lqq + 4)) * fr
    prop(24) = a(lqq + 5) + (a(lpp + 11) - a(lqq + 5)) * fr
    ren = prop (2) * prop (3) / prop(1)
    prop(24) = prop(24) - ren * LOG(prop(2) / a(nt + 1))
94  prop(7) = 1.0_r8k / prop(2)
98  prop(4) = prop(5) - prop(2) * prop(3)
    GOTO 99
95  prop(8) = a(lpp + 4)
    prop(1) = (prop(5) - a(lpp + 1) - a(nt + 1) * a(lpp) + prop(8) * a(nt)) / prop(8)
    frd = prop(1) / a(nt)
    frc = a(nt + 1) * a(lpp)
    prop(3) = frc * frd / prop(2)
    prop(6) = a(lpp + 2) / frd
    prop(7) = a(lpp + 3) * a(nt + 1) / prop(2)
    ren = prop (2) * prop (3) / prop(1)
    IF (prop(1) <= 0.0_r8k) GOTO 1001
    prop(24) = a(lpp + 5) + prop(8) * LOG(prop(1) / a(nt)) - ren * LOG(prop(2) / a(nt + 1))
    GOTO 98
90  ht = a(klp + 21) + a(klp + 13) * a(klp + 20)
    IF (prop(5) < ht) GOTO 1001
    ip = 1
    kp = klp + 13
202 frd = a(kp + 21) + a(kp + 13) * a(kp + 20)
    IF (prop(5) <= frd) GOTO 201
    ip = ip + 1
    kp = kp + 13
    ht = frd
    GOTO 202
201 fr = (prop(5) - ht) / (frd - ht)
    prop(1) = a(ip) + fr * (a(ip + 1) - a(ip))
    prop(3) = (fr * a(kp + 13) * a(kp + 20) / a(ip + 1) + (1.0_r8k - fr) * a(kp) * a(kp + 7) / a(ip)) * prop(1) / prop(2)
    prop(6) = 1.0_r8k / prop(1)
    prop(8) = a(kp + 11) + (a(kp + 24) - a(kp + 11)) * fr
    ren = prop(2) * prop(3) / prop(1)
    IF (prop(1) <= 0.0_r8k) GOTO 1001
    prop(24) = a(kp + 12) + prop(8) * LOG(prop(1) / a(ip)) - ren * LOG(prop(2) / a(kp))
    GOTO 94
1001 err = .TRUE.
    !
    END SUBROUTINE sth2x5
    !
    !
    !
    SUBROUTINE sth2xi (a, nuse)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Reads the water properties file
    !>@author
    !> envrl.s,v 2.102 1999 / 05 / 19 23:08:13 randyt Exp randyt
    !> Updated by Ian Porter, 2015
    INTEGER(ipk) :: i, jp1, nsize, nuse
    LOGICAL :: L_Exists, I_Opened, UseWaterProp
    REAL(r8k), DIMENSION(:) :: a
    !
    L_Exists = .FALSE. ! Specifies if the file exists
    I_Opened = .FALSE. ! Specifies if the file is open. It should be opened by now if it exists
    UseWaterProp = .FALSE. ! Specifies whether to use the built - in water properties.
    REWIND h2ounit
    INQUIRE (h2ounit, Opened = I_Opened, Exist = L_Exists)
    ! Check to see if the water file was supplied by the user
    IF (I_Opened) THEN ! The water file is supplied by the user
        READ (h2ounit,End=100,err=101) nt,np,ns,ns2
        nt5 = nt * 6
        jpl = nt + np
        klp = jpl - 12
        jp1 = jpl + ns * 13
        klp2 = jp1 - 12
        jp1 = jp1 + ns2 * 13
        llp = jp1 - nt5 - 5
        nsize = jp1 + 6 * nt * np
        IF (nsize > nuse) GOTO 102
        nuse = nsize
        READ (h2ounit,End=100,err=100) (a(i),i=1,nsize)
    ELSE
        UseWaterProp = .TRUE.
    ENDIF
    GOTO 300
    !
100 WRITE (ounit,200)
200 FORMAT ('0 ******** End of data encountered reading water thermodynamic property file.')
    UseWaterProp = .TRUE.
    GOTO 300
101 WRITE (ounit,201)
201 FORMAT ('0 ******** Read error encountered reading water thermodynamic property file.')
    UseWaterProp = .TRUE.
    GOTO 300
102 WRITE (ounit,202)
202 FORMAT ('0 ******** Insufficient space furnished for water thermodynamic property file.')
    UseWaterProp = .TRUE.
    ! No water file is supplied by the user or else the file supplied by user has problems. Use default water properties.
300 If (UseWaterProp) THEN
        WRITE (ounit, 203)
        WRITE (0, 203)
        nt = 66
        np = 38
        ns = 47
        ns2 = 29
        nt5 = nt * 6
        jpl = nt + np
        klp = jpl - 12
        jp1 = jpl + ns * 13
        klp2 = jp1 - 12
        jp1 = jp1 + ns2 * 13
        llp = jp1 - nt5 - 5
        nsize = jp1 + 6 * nt * np
        nuse = nsize
        a(:) = Water(:)
    ENDIF
    !
203 FORMAT (/10x,'Default Water Properties Used.', /)
    !
    END SUBROUTINE sth2xi
    !
    !
    SUBROUTINE surten (tin, sigma)
    USE Kinds
    USE conversions_fraptran, ONLY : tfk
    IMPLICIT NONE
    !>@brief
    !> This Subroutine Returns surface tension (lbf/ft) when given steam temperature (F)
    !
    ! Input
    !
    ! tin   - Temperature (F)
    !
    ! Output
    !
    ! sigma - Surface tension (lbf/ft)
    !
    ! Reference:
    !
    ! (1) Properties of water and steam in si units, schmidt, 1969
    !
    REAL(r8k) :: t, dt
    REAL(r8k), INTENT(IN) :: tin
    REAL(r8k), INTENT(OUT) :: sigma
    REAL(r8k), PARAMETER :: tk = 647.3_r8k
    REAL(r8k), PARAMETER :: b  = 0.83_r8k
    REAL(r8k), PARAMETER :: a1 = 1.160936807e-1_r8k
    REAL(r8k), PARAMETER :: a2 = 1.121404688e-3_r8k
    REAL(r8k), PARAMETER :: a3 = -5.75280518e-6_r8k
    REAL(r8k), PARAMETER :: a4 = 1.286274650e-8_r8k
    REAL(r8k), PARAMETER :: a5 = -1.14971929e-11_r8k
    ! Convert temperature from F to K
    t = tfk(tin)
    IF (t < 100.0_r8k) THEN
        sigma  =  58.0_r8k
    ELSE IF (t > 639.0_r8k) THEN
        sigma  =  1.0_r8k
    ELSE
        dt = tk - t
        sigma = a1 * dt ** 2 / (1.0_r8k + b * dt) + a2 * dt ** 2 + a3 * dt ** 3 + a4 * dt ** 4 + a5 * dt ** 5
    END IF
    ! convert from si to english units (lbf/ft)
    sigma = sigma * 30.479_r8k / 444820.0_r8k
    !
    END SUBROUTINE surten
!
!
    FUNCTION thcon (n, t1, rho1)
    USE Kinds
    USE conversions_fraptran, ONLY : sechr
    IMPLICIT NONE
    !>@brief
    !> Function thcon computes the thermal conductivity of saturated liquid and saturated and superheated vapor
    !
    ! Inputs
    !
    ! n    - Water state indicator (1 - Saturated liquid, 2 - Superheated vapor)
    ! t1   - Water tempearture (F)
    ! rho1 - Water density (lbm/ft^3)
    !
    ! Output
    !
    ! thcon - Water thermal conductivity (btu/ft-hr-F)
    !
    ! Reference:
    !
    ! (1) 1967 steam tables
    !
    ! Note:
    !
    ! Thermal conductivity units are (btu/ft-sec-F) but output value is in hours (btu/ft-hr-F)
    !
    INTEGER(ipk) :: i
    INTEGER(ipk), INTENT(IN) :: n
    REAL(r8k) :: thcon, tc, rho2, z0, z1, z2, z3
    REAL(r8k), INTENT(IN) :: t1, rho1
    REAL(r8k), DIMENSION(11), PARAMETER :: ta = [ &
      &     32.0_r8k, 100.0_r8k, 200.0_r8k, 250.0_r8k, 300.0_r8k, &
      &    350.0_r8k, 400.0_r8k, 500.0_r8k, 600.0_r8k, 650.0_r8k, 705.0_r8k ]
    REAL(r8k), DIMENSION(11), PARAMETER :: ak = [ &
      &   0.91e-4_r8k, 1.01e-4_r8k, 1.09e-4_r8k, 1.10e-4_r8k, 1.10e-4_r8k, &
      &   1.09e-4_r8k, 1.06e-4_r8k, 0.97e-4_r8k, 0.83e-4_r8k, 0.72e-4_r8k, 0.44e-4_r8k ]
    !
    SELECT CASE (n)
    CASE (1) ! Saturated liquid
        IF (t1 > ta(1)) THEN
            DO i = 2, 11
                IF (t1 <= ta(i)) GOTO 130
            ENDDO
            i = 11
130         thcon = ak(i-1) + ((t1 - ta(i-1))/(ta(i) - ta(i-1))) * (ak(i) - ak(i-1))
        ELSE
            thcon = ak(1)
        ENDIF
    CASE (2) ! Saturated and superheated vapor
        tc = 5.0_r8k * (t1 - 32.0_r8k) / 9.0_r8k
        rho2 = rho1 / 62.4_r8k
        z1 = (5.87e-2_r8k) * tc
        z2 = (1.04e-4_r8k) * (tc ** 2)
        z3 = (4.51e-8_r8k) * (tc ** 3)
        z0 = 17.6_r8k + z1 + z2 - z3
        z1 = 103.51_r8k + (0.4198_r8k) * tc - (2.771e-5_r8k) * (tc ** 2)
        z2 = (2.1482e14_r8k) / (tc ** 4.20_r8k)
        z3 = z0 + z1 * rho2 + z2 * (rho2 ** 2)
        thcon = (1.605e-7_r8k) * z3
    END SELECT
    !
    thcon = MAX(thcon, 1.0e-8_r8k)
    thcon = thcon * sechr
    !
    END FUNCTION thcon
!
!
    FUNCTION visc (n, t1, rho1)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    USE conversions_fraptran, ONLY : sechr
    IMPLICIT NONE
    !>@brief
    !> Function visc computes viscosity of saturated liquid and saturated and superheated vapor - 1967 steam tables
    ! Viscosity table units are lbm/sec-ft, but output value is in hours
    !
    INTEGER(ipk) :: i, m
    INTEGER(ipk), INTENT(IN) :: n
    INTEGER(ipk), PARAMETER :: TableVals = 17
    REAL(r8k) :: visc, rho2, tc, z0, z1, z2, z3
    REAL(r8k), INTENT(IN) :: t1, rho1
    REAL(r8k), DIMENSION(TableVals), PARAMETER :: ta = [ &
      &            32.0_r8k,  50.0_r8k, 100.0_r8k, 150.0_r8k, 200.0_r8k, &
      &           250.0_r8k, 300.0_r8k, 350.0_r8k, 400.0_r8k, 450.0_r8k, &
      &           500.0_r8k, 550.0_r8k, 600.0_r8k, 635.8_r8k, 650.0_r8k, &
      &           680.0_r8k, 705.0_r8k ]
    REAL(r8k), DIMENSION(TableVals), PARAMETER :: v1 = [ &
      &       11.780e-4_r8k, 8.730e-4_r8k, 4.570e-4_r8k, 2.870e-4_r8k, 2.010e-4_r8k, &
      &        1.530e-4_r8k, 1.230e-4_r8k, 1.020e-4_r8k, 0.879e-4_r8k, 0.773e-4_r8k, &
      &        0.692e-4_r8k, 0.629e-4_r8k, 0.571e-4_r8k, 0.524e-4_r8k, 0.510e-4_r8k, &
      &        0.470e-4_r8k, 0.240e-4_r8k ]
    !
    SELECT CASE (n)
    CASE (1) ! Saturated liquid
        IF (t1 <= ta(1)) THEN
            visc = v1(1)
        ELSE
            DO i = 2, TableVals
                IF (t1 <= ta(i)) EXIT
            ENDDO
            i = MIN(i,TableVals)
            visc = v1(i-1) + ((t1 - ta(i-1)) / (ta(i)- ta(i-1))) * (v1(i) - v1(i-1))
        ENDIF
    CASE (2) ! Saturated and superheated vapor
        tc = 5.0_r8k * (t1 - 32.0_r8k) / 9.0_r8k
        rho2 = rho1 / 62.4_r8k
        z1 = 0.407_r8k * tc + 80.4_r8k
        m = 1
        IF (tc > 340.0_r8k) m = 2
        IF (tc <= 365.0_r8k) GOTO 205
        m = 3
        GOTO 210
205     z2 = z1 + rho2 * (5.90_r8k * tc - 1858.0_r8k)
        z0 = z2
        IF (m == 1) GOTO 220
210     CONTINUE
        z3 = z1 + rho2 * (353.0_r8k + rho2 * (676.5_r8k + rho2 * 102.1_r8k))
        z0 = z3
        IF (m == 2) z0 = 0.5_r8k * (z2 + z3)
220     visc = 0.0672e-6_r8k * z0
    CASE DEFAULT
        WRITE(ounit,*) ' Execution terminated in visc. Wrong value for n supplied.'
        STOP
    END SELECT
    ! Convert to lbm/hr*ft
    visc = visc * sechr
    !
    END FUNCTION visc
!
!
    FUNCTION viscol (pres, temp, rhol, tsatp)
    USE Kinds
    USE ErrorMsg, ONLY : fabend
    IMPLICIT NONE
    !>@brief
    !> Calculates liquid water viscosity.
    !
    ! Input
    !
    ! pres   - Pressure (Pa)
    ! temp   - Temperature (K)
    ! rhol   - Liquid density (kg/m3)
    !          If (pres >= critical pressure) THEN rhol is for super-critical liquid at (pres, temp).
    !          If (pres < critical pressure) and:
    !          1. If (pres > psat(temp)) rhol is for subcooled liquid at (pres, temp).
    !          2. If (pres <= psat(temp)) rhol is for saturated liquid at pres.
    ! tsatp  - Saturation temperature (K) at pres
    !
    ! Output
    !
    ! viscol = liquid viscosity (kg/m-sec) 
    !
    ! Reference:
    !
    ! (1) The american society of mechanical engineers, thermodynamic and transport properties of steam,
    !     united engineering center, 345 east 47-th street, new york, n.y., 10017, (1967).
    !
    ! Notes:
    !
    ! If (pres >= critical pressure), viscol is calculated for super-critical liquid at (pres, temp).
    ! If (pres. lt. critical pressure) and:
    ! 1. If (pres > psat(temp)), viscol is calculated for subcooled liquid at (pres, temp).
    ! 2. If (pres <= psat(temp)), viscol is calculated for saturated liquid at pres.
    !
    REAL(r8k) :: viscol, psat, templ, tflim
    REAL(r8k), INTENT(IN) :: pres, temp, rhol, tsatp
    LOGICAL :: err
    ! Define liquid state
    psat   = pres
    templ  = temp
    IF (pres < 2.212e+7_r8k) templ = MIN(temp, tsatp)
    IF (templ < 647.3_r8k) THEN
        CALL sth2x0 (templ, psat, err)
        IF (err) Call fabEnd
    ENDIF
    ! Subcooled and saturated liquid viscosity
    tflim  = MAX(573.15_r8k, MIN(647.3_r8k, templ))
    viscol = (647.3_r8k - tflim) * EXP(570.58059_r8k / (templ - 140.0_r8k)) * (3.2555630478758e-7_r8k + 3.4088115981118e-18_r8k * &
      &       MAX((pres - psat), 0.0_r8k) * (templ - 305.0_r8k)) + (tflim - 573.15_r8k) * (1.0842886041807e-7_r8k + &
      &       5.488873904248e-10_r8k * (templ - 273.15_r8k) + rhol * (4.7606203631266e-10_r8k + rhol * &
      &      (9.1233984665203e-13_r8k + rhol * 1.3769386378961e-16_r8k)))
    !
    END FUNCTION viscol
!
!
    SUBROUTINE voidratio (Quality, rf, rg, de, sr, alpha)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine computes void ratio and vapor-liquid velocity ratio
    !
    ! Input
    !
    ! Quality - coolant quality
    ! rf      - density of liquid phase of coolant (lbm/ft**3)
    ! rg      - density of vapor phase of coolant (lbm/ft**3)
    ! de      - equivalent diameter (inches)
    !
    ! Output
    !
    ! alpha   - void ratio
    ! sr      - vapor-liquid velocity ratio (slip ratio) - Not used by any part of FRAPTRAN
    !
    INTEGER(ipk) :: QualityCase, itn
    INTEGER(ipk), PARAMETER :: itmax = 100
    REAL(r8k) :: sr2, vf, vg
    REAL(r8k), INTENT(IN) :: Quality, rf, rg, de
    REAL(r8k), INTENT(OUT) :: sr, alpha
    REAL(r8k), PARAMETER :: eps = 0.005_r8k
    REAL(r8k), PARAMETER :: em02 = 0.01_r8k
    !
    IF (Quality <= em02) QualityCase = 1
    IF (Quality > em02) QualityCase = 2
    IF (Quality > em02 .AND. Quality < 0.95_r8k) QualityCase = 3
    !
    SELECT CASE (QualityCase)
    CASE (1)
        sr = 1.0_r8k
        alpha = 0.0_r8k
    CASE (2)
        sr = 1.0_r8k
        alpha = 1.0_r8k
    CASE (3) ! on first iteration, assume slip ratio is two!
        sr = 1.0_r8k
        vf = 1.0_r8k / rf
        vg = 1.0_r8k / rg
        itn = 0
        ConvergenceLoop: DO
            alpha = Quality * vg / ((1.0_r8k - Quality) * vf * sr + Quality * vg)
            IF (alpha < em02) alpha = em02
            ! compute slip ratio by correlation
            sr2 = 1.0_r8k
            ! check for convergence
            IF (ABS((sr2 - sr) / sr) < eps) EXIT ConvergenceLoop ! Convergence achieved
            itn = itn + 1
            IF (itn > itmax) THEN
                WRITE(ounit,301)
301             FORMAT(' *** Convergence did not occur in iteration for slip ratio and void fraction in subroutine void. ', &
                  &    ' Execution stopped ***')
                WRITE(ounit,302) sr, sr2, alpha, Quality, rf, rg
302             FORMAT(//' sr = ',e11.4,'  sr2 = ',e11.4,'  alpha = ',e11.4,'  Quality = ',e11.4,'  rf = ',e11.4,'  rg = ',e11.4)
                STOP
            ENDIF
            sr = sr2
        END DO ConvergenceLoop
        ! convergence
        sr = sr2
    END SELECT
    !
    END SUBROUTINE voidratio
    !
    !
END MODULE sth2x












