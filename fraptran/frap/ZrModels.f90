MODULE ZrModels
    USE Kinds
    USE cnvt
    !>@brief
    !> This module contains the Zircaloy caldding models for annealing,  
    !> yield stress, mechanical limits, coefficients of anisotropy,
    !> cladding true stress and strain.
    !> Subroutines include caneal, caniso, ckmn, cmlimt, cstran, cstres, cstrni
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2015
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE caneal (FastFlux, delh, ctemp, rtemp, fnck, fncn, cwkf, cwnf)
    USE Kinds
    USE variables_fraptran, ONLY : Time
    IMPLICIT NONE
    !>@brief
    !> caneal calculates the change in effective fluence and effective cold work during a time step
    !>@author
    !> coded by d. l. hagrman october 1977
    !> last modified by d. l. hagrman july 1982 (cdf-mp-10)
    !
    ! Input
    !
    ! FastFlux - Fast neutron flux ((neutrons/m**2)/s)
    ! delh     - Time step size (s)
    ! ctemp    - Cladding temperature at start of time step (k)
    ! rtemp    - Average rate of change of temperature (k/s)
    ! fnck     - Effective fast fluence for strength coefficient at time step start (neutrons/(m**2))
    ! fncn     - Effective fast fluence for strain hardening exponent at time step start (neutrons/(m**2))
    ! cwkf     - Effective cold work for strength coefficient at time step start (unitless ratio of areas)
    ! cwnf     - Effective cold work for strain hardening exponent at time step start (unitless ratio of areas)
    !
    ! Output
    !
    ! fnck     - Effective fast fluence for strength coefficient at time step finish (neutrons/(m**2))
    ! fncn     - Effective fast fluence for strain hardening exponent at time step finish (neutrons/(m**2))
    ! cwkf     - Effective cold work for strength coefficient at time step finish (unitless ratio of areas)
    ! cwnf     - Effective cold work for strain hardening exponent at time step finish (unitless ratio of areas)
    !
    ! Reference:
    !
    ! (1) a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !     Quarterly progress report for april through june 1976, bmi-nureg-1956 (july 1976)
    ! (2) a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !     Quarterly progress report for july through september, 1976, bmi-nureg-1961 (october 1976)
    ! (3) a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !     Quarterly progress report for october through december 1976, bmi-nureg-1967 (january 1977)
    !
    REAL(r8k) :: t, te, qk, bk, disok, fnisok, qn, bn, dison, fnison, bki, qki, disoki, fniski, &
      &          bni, qni, fnisni
    REAL(r8k), INTENT(IN) :: FastFlux, delh, ctemp, rtemp
    REAL(r8k), INTENT(INOUT) :: fnck, fncn, cwkf, cwnf
    !
    t = ctemp
    ! update fluence
    fnck = fnck + FastFlux * delh
    fncn = fncn + FastFlux * delh
    ! annealing model
    IF (t <= 500.0_r8k) RETURN
    IF (t >= 1255.0_r8k .OR. ((t + rtemp * delh) >= 1255.0_r8k)) THEN
        fncn = 0.0_r8k
        fnck = 0.0_r8k
        cwkf = 0.0_r8k
        cwnf = 0.0_r8k
        RETURN
    ENDIF
    IF (cwkf > 1.0e-20_r8k) THEN
        qk =  2.33e18_r8k
        bk = 1.504_r8k * (1.0_r8k + (2.2e-25_r8k) * fnck)
        disok = qk * rtemp * delh * (t ** (-7)) * 6.0_r8k
        IF (ABS(disok) < 1.0e-4_r8k) fnisok = 1.0_r8k
        IF (ABS(disok) >= 1.5e2_r8k) GOTO 2
        IF (ABS(disok) >= 1.0e-4_r8k) fnisok = (EXP(disok) - 1.0_r8k) / disok
        cwkf = EXP(-bk * delh * EXP(-qk * (t ** (-6))) * fnisok) * cwkf
    END IF
    !
    IF (cwnf > 1.0e-20_r8k) THEN
         qn =  2.33e18_r8k
        bn = 1.504_r8k * (1.0_r8k + (2.2e-25_r8k) * fncn) * 8.0_r8k
        dison = qn * rtemp * delh * (t ** (-7)) * 6.0_r8k
        IF (ABS(dison) < 1.0e-04_r8k) THEN
            fnison = 1.0_r8k
        ELSE
            fnison = (EXP(dison) - 1.0_r8k) / dison
        ENDIF
        IF (ABS(dison) >= 1.50e2_r8k) GOTO 2
        cwnf = EXP(-bn * delh * EXP(-qn * (t ** (-6))) * fnison) * cwnf
    END IF
    !
    IF (fnck > 1.0e14_r8k) THEN
        bki = 2.49e-06_r8k
        qki = 5.35e23_r8k
        disoki = qki * rtemp * delh * (t ** (-9)) * 8.0_r8k
        IF (ABS(disoki) < 1.0e-4_r8k) fniski = 1.0_r8k
        IF (ABS(disoki) >= 1.5e2_r8k) GOTO 2
        IF (ABS(disoki) >= 1.0e-04_r8k) fniski = (EXP(disoki) - 1.0_r8k) / disoki
        fnck = fnck / 1.0e20_r8k
        fnck = (bki * delh * (EXP(-qki * (t ** (-8)))) * fniski + fnck ** (-1)) ** (-1)
        fnck = fnck * 1.0e20_r8k
    END IF
    !
    IF (fncn > 1.0e14_r8k) THEN
        bni = bki * 1000.0_r8k
        qni = qki
        fnisni = fniski
        fncn = fncn / 1.0e20_r8k
        fncn = (bni * delh * (EXP(-qni * (t ** (-8)))) * fnisni + fncn ** (-1)) ** (-1)
        fncn = fncn * 1.0e20_r8k
    END IF
    !
    RETURN
    !
2   WRITE (*, 901) Time
901 FORMAT('Time step too large for cladding annealing model (under flow)',/, &
      &    'Effective coldworks and fluences set to zero',/,'at Problemtime =',e12.4)
    !
    cwkf = 0.0_r8k
    cwnf = 0.0_r8k
    fnck = 0.0_r8k
    fncn = 0.0_r8k
    !
    END SUBROUTINE caneal
    !
    !
    !
    SUBROUTINE caniso (dep1, dep2, dep3, ctemp, f1p, f2p, f3p, a1d, a2d, a3d, a1s, a2s, a3s, a1e, a2e, a3e)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> caniso calculates coefficients of anisotropy for relating effective stress to stress components and relating
    !> effective strain to strain components. This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FrapTran
    !>@author
    !> caniso was coded by d. l. hagrman october 1978
    !> modified by d. l. hagrman in june 1979
    !> modified by d. l. hagrman july 1981 to keep a1d,a2d,a3d,a1p,a2p, and a3p in the range 0 - 1.5 (cdr-mp-04)
    !> Modified by PNNL, January 1997, to clean up coding by deleting unu WRITE and FORMAT statements
    !
    ! Input
    !
    ! ctemp  - cladding temperature (k)
    ! dep(i) - cladding plastic strain increment along axis (i, i= 1 to 3) durring the current time step (unitless)
    ! f(i)p  - volume weighted average cosine of the angle between basal poles and coordinate axis (i, i = 1 to 3) 
    !          at the start of the current time step
    ! a(i)d  - high temperature strain anisotropy coefficient along axis (i, i= 1 to 3) at the start of the current time step.
    !          (= 0.5 for the first step)
    !
    ! Output
    !
    ! a1s   - coefficient of anisotropy for calculating effective stress (unitless)
    ! a2s   - coefficient of anisotropy for calculating effective stress (unitless)
    ! a3s   - coefficient of anisotropy for calculating effective stress (unitless)
    ! The form of the equation for effective stress is assumed to be:
    ! effective stress = SQRT(a1s * (sigma 1 - sigma 2) ** 2 + a2s * (sigma 2 - sigma 3) ** 2 + a3s * (sigma 3 - sigma 1) ** 2)
    ! Where sigma 1, sigma 2 and sigma 3 are principal axis stress components
    !
    ! a1e   - coefficient of anisotropy for calculating effective strain
    ! a2e   - coefficient of anisotropy for calculating effective strain
    ! a3e   - coefficient of anisotropy for calculating effective strain
    ! The form of the equation for effective strain is assumed to be:
    ! effective strain = sum of effective strain increments
    ! With the strain increments defined in terms of strain increment components by
    !
    ! del effective strain = (1.0/(a1e*a2e + a2e*a3e + a3ea1e)) *
    !                        a1e * (a2e * dep1 - a3e * dep2)**2) +
    !                        a2e * (a3e * dep2 - a1e * dep3)**2) +
    !                        a3e * (a1e * dep3 - a2e * dep1)**2))**0.5
    !
    ! Where dep1, dep2 and dep3 are plastic strain increment components
    !
    ! f(i)p  - volume weighted average cosine of the angle between basal poles and coordinate axis (i, i = 1 to 3) 
    !          at the end of the current time step
    ! a(i)d  - high temperature strain anisotropy coefficient along axis (i, i= 1 to 3) at the end of the current time step.
    !
    ! Reference:
    !
    ! (1) c. r. hann et. al., Transient deformation properties of zircaloy for loca simulation, np-526, volume 3 (march 1978).
    !
    ! convert f factors to appropriate anisotropy coeffecients
    !
    REAL(r8k) :: sao, spo, rac, apn, adn, a1p, a2p, a3p
    REAL(r8k), INTENT(IN) :: ctemp, dep1, dep2, dep3
    REAL(r8k), INTENT(OUT) :: a1e, a2e, a3e, a1s, a2s, a3s
    REAL(r8k), INTENT(INOUT) :: f1p, f2p, f3p, a1d, a2d, a3d
    !
    a3p = 1.5_r8k * f2p
    a2p = 1.5_r8k * f1p
    a1p = 1.5_r8k * f3p
    !
    IF (ctemp >= 1255.0_r8k) THEN
        sao = 0.0_r8k
    ELSE IF (ctemp >= 1090.0_r8k) THEN
        sao = 7.6060606_r8k - ctemp * 6.060606e-03_r8k
    ELSE
        sao = 1.0_r8k
    END IF    
    spo = 1.0_r8k / (EXP((ctemp - 725.0_r8k) / 18.0_r8k) + 1.0_r8k)
    IF (ctemp >= 1090.0_r8k) THEN
        rac = 6.5174_r8k
    ELSE IF (ctemp > 644.0_r8k) THEN
        rac = -2.757_r8k + ctemp * 1.343e-02_r8k
    ELSE
        rac = 0.52762_r8k
    END IF
    apn = 1.0_r8k
    IF (a1p > 1.48_r8k) apn = 0.0_r8k
    IF (a2p > 1.48_r8k) apn = 0.0_r8k
    IF (a3p > 1.48_r8k) apn = 0.0_r8k
    IF (a1p < 0.02_r8k) apn = 0.0_r8k
    IF (a2p < 0.02_r8k) apn = 0.0_r8k
    IF (a3p < 0.02_r8k) apn = 0.0_r8k
    adn = 1.0_r8k
    IF (a1d > 1.48_r8k) adn = 0.0_r8k
    IF (a2d > 1.48_r8k) adn = 0.0_r8k
    IF (a3d > 1.48_r8k) adn = 0.0_r8k
    IF (a1d < 0.02_r8k) adn = 0.0_r8k
    IF (a2d < 0.02_r8k) adn = 0.0_r8k
    IF (a3d < 0.02_r8k) adn = 0.0_r8k
    a1p = a1p - rac * apn * dep3
    a2p = a2p - rac * apn * dep1
    a3p = a3p - rac * apn * dep2
    IF (a1p > 1.5_r8k) a1p = 1.49_r8k
    IF (a2p > 1.5_r8k) a2p = 1.49_r8k
    IF (a3p > 1.5_r8k) a3p = 1.49_r8k
    IF (a1p < 0.00_r8k) a1p = 0.01_r8k
    IF (a2p < 0.00_r8k) a2p = 0.01_r8k
    IF (a3p < 0.00_r8k) a3p = 0.01_r8k
    a1d = a1d - rac * adn * dep3
    a2d = a2d - rac * adn * dep1
    a3d = a3d - rac * adn * dep2
    IF (a1d > 1.5_r8k) a1d = 1.49_r8k
    IF (a2d > 1.5_r8k) a2d = 1.49_r8k
    IF (a3d > 1.5_r8k) a3d = 1.49_r8k
    IF (a1d < 0.00_r8k) a1d = 0.01_r8k
    IF (a2d < 0.00_r8k) a2d = 0.01_r8k
    IF (a3d < 0.00_r8k) a3d = 0.01_r8k
    a1e = (a1p * spo + a1d * (1.0_r8k - spo)) * sao + 0.5_r8k * (1.0_r8k - sao)
    a2e = (a2p * spo + a2d * (1.0_r8k - spo)) * sao + 0.5_r8k * (1.0_r8k - sao)
    a3e = (a3p * spo + a3d * (1.0_r8k - spo)) * sao + 0.5_r8k * (1.0_r8k - sao)
    a1s = a1p * sao + 0.5_r8k * (1.0_r8k - sao)
    a2s = a2p * sao + 0.5_r8k * (1.0_r8k - sao)
    a3s = a3p * sao + 0.5_r8k * (1.0_r8k - sao)
    ! Convert anisotropy coefficients to f factors
    f1p = a2p / 1.5_r8k
    f2p = a3p / 1.5_r8k
    f3p = a1p / 1.5_r8k
    !
    END SUBROUTINE caniso
    !
    !
    !
    SUBROUTINE ckmn (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am)
    USE Kinds
    USE variables_fraptran, ONLY : CladType, cexh2l, ounit
    IMPLICIT NONE
    !>@brief
    !> ckmn calculates parameters for the cladding equation of state as a function of temperature, 
    !> average oxygen concentration, fast neutron fluence, and cold work.
    !>@author
    !> Coded by d. l. hagrman august 1977
    !> Modified by g. a. reymann and m. a. morgan march 1978
    !> Modified by d. l. hagrman october, 1978
    !
    ! Input
    !
    ! ctemp  - cladding meshpoint temperature (K)
    ! deloxy - average oxygen concentration excluding
    ! oxide layer - average oxygen concentration of as received cladding  (kg oxygen/kg zircaloy)
    ! fnck   - effective fast fluence for strength coefficient (neutrons/(m**2))
    ! fncn   - effective fast fluence for strain hardening exponent (neutrons/(m**2))
    ! cwkf   - effective cold work for strength coefficient (unitless ratio of areas)
    ! cwnf   - effective cold work for strain hardening exponent (unitless ratio of areas)
    ! rstran - effective true plastic strain rate (s**(-1))
    ! cexh2l - local hydrogen concentration that is in excess of solubility limit (ppm).
    !
    ! Output
    !
    ! ak    - strength coefficient (Pa)
    ! an    - strain hardening exponent (unitless)
    ! am    - strain rate sensitivity exponent (unitless)
    ! um    - estimated  uncertainty of am (not currently returned)
    ! un    - estimated uncertainty of an (not currently returned) Note: an - un is negative near 1000k **
    ! uk    - estimated uncertainty of ak (not currently returned)
    !
    ! Reference:
    !
    ! (1) c.r. woods, properties of zircaloy-4 tubing, wapd-tm-585 (1966)
    ! (2) ultimate strength Data of h.c. brassfield,et al. gemp-482(1968)
    ! (3) a.l. bement, jr., effects of cold work and neutron irradiation on the tensile properties of zircaloy-2, hw-74955
    ! (4) a. cowan and w.j. langford j.nuclear mater. 30 (1969) pp 271-281
    ! (5) l.m. howe and w.r. thomas, j. nuclear  mater. 1 (1960) p 248
    ! (6) a. m. garde light-water-reactor safety research program quarterly progress report april - june 1975 anl-75-58
    ! (7) a. m. garde light-water-reactor safety research program quarterly progress report july - september 1975 anl-75-72
    ! (8) r.l. mehan and f.w. wiesinger, mechanical properties of zircaloy-2, kapl-2110
    ! (9) d. lee and w.a. backofen tms-aime 239  (1967) pp 1034-1040
    ! (10) p. j. crescimanno, high strain rate tensile tests of zircaloy at 550f, wapd-tm-1263 (february 1976).
    ! (11) c. r. hann et al, transient deformation properties of zircaloy for  loca simulation, epri np-526 volume 3 (march 1978)
    !
    REAL(r8k) :: t, fluence, h2solubility, h2excess, antm, anphi, anzry, aktm, akcw, &
      &     fcwt, akphi, akzry, anh2, akcwf, fkcc, akh2, cwnf0, a, b, c, d
    REAL(r8k), INTENT(IN) :: ctemp, deloxy, fnck, fncn, cwkf, cwnf
    REAL(r8k), INTENT(OUT) :: ak, an, am
    REAL(r8k), INTENT(INOUT) :: rstran
    !
    t = ctemp
    ! Limit strain rate to a minimum of 1.0e-05
    rstran = MAX(1.0e-05_r8k, rstran)
    ! 2/5/03 limit fluence to 1.0e26_r8k n/m2
    fluence = MIN(1.0e26_r8k, fnck)
    !
    SELECT CASE (CladType)
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry Properties
        a = 20.63172161_r8k
        b = -0.07704552983_r8k
        c = 9.504843067e-05_r8k
        d = -3.860960716e-08_r8k
        ! Excess hydrogen not explicitly used for RIA model
        ! Excess hydrogen limited to 400 ppm for NUREG/CR-6534, Vol. 1, model
        ! Calculate excess H2 based on resolution
        H2solubility = 1.99e5_r8k * EXP(-9300.0_r8k / (1.99_r8k * t))
        H2excess = MIN(400.0_r8k, MAX(0.0_r8k, cexh2l - H2solubility))
        ! 7/2/04 model for Zircaloy-2 and Zircaloy-4
        ! Strain rate coefficient m (coding variable am), fixed value
        IF (t > 800.0_r8k) THEN
            am = 0.000324124_r8k * t - 0.20701_r8k
        ELSE IF (t > 750.0_r8k) THEN
            am = 0.0007458_r8k * t - 0.544338_r8k
        ELSE
            am = 0.015_r8k
        END IF
        ! am = am * EXP(-69.0_r8k * deloxy)
        ! Strain hardening coefficient n (coding variable an)
        ! Temperature term: antm constant from 300 to 419.4K before decreasing
        IF (t >= 1600.0_r8k) THEN
            antm = 0.17344880_r8k
        ELSE IF (t >= 1099.0722_r8k) THEN
            antm = -0.22655119_r8k + 2.5e-04_r8k * t
        ELSE IF (t >= 419.4_r8k) THEN
            antm = -9.490e-02_r8k + t * (1.165e-03_r8k + t * (-1.992e-06_r8k + t * 9.558e-10_r8k))
        ELSE
            antm = 0.11405_r8k
        ENDIF
        ! Fluence term
        IF (fluence >= 7.5e25_r8k) THEN
            anphi = 1.608953_r8k
        ELSE IF (fluence >= 2.0e25_r8k) THEN
            anphi = 1.5435_r8k + 0.008727e-25_r8k * fluence
        ELSE IF (fluence >= 0.1e25_r8k) THEN
            anphi = 1.369_r8k + 0.096e-25_r8k * fluence
        ELSE
            anphi = 1.321_r8k + 0.48e-25_r8k * fluence
        ENDIF
        ! Cladding type term
        anzry = 1.0_r8k
        IF (CladType == 2) anzry = 1.6_r8k
        ! Combine terms
        an = antm * anphi / anzry
        ! Strength coefficient K (coding variable ak)
        ! temperature term
        IF (t > 2100.0_r8k) THEN
            aktm = 1.0_r8k
        ELSE IF (t >= 1255.0_r8k) THEN
            aktm = 4.330e7_r8k + t * (-6.685e4_r8k + t * (37.579_r8k - t * 7.33e-03_r8k))
        ELSE IF (t >= 1090.0_r8k) THEN
            aktm = 184.1376039e6_r8k - 1.4345448e5_r8k * t
        ELSE IF (t >= 750.0_r8k) THEN
            aktm = 2.5224880e6_r8k * EXP(2.8500027e6_r8k / (t ** 2))
        ELSE
            aktm = 1.17628e9_r8k + t * (4.54859e5_r8k + t * (-3.28185e3_r8k + t * 1.72752_r8k))
        ENDIF
        ! Cold work term
        akcw = 0.546_r8k * cwkf
        ! Fluence term
        fcwt = 2.25_r8k * EXP(-cwkf * 20.0_r8k) * MIN(1.0_r8k, EXP((t - 550.0_r8k) / 10.0_r8k)) + 1.0_r8k
        IF (fluence >= 7.5e25_r8k) THEN
            akphi = 0.731995_r8k
        ELSEIF (fluence >= 2.0e25_r8k) THEN
            akphi = 0.53236_r8k + 2.6618e-27_r8k * fluence
        ELSEIF (fluence >= 0.1e25_r8k) THEN
            akphi = 2.928e-26_r8k * fluence
        ELSE
            akphi = (-0.1464_r8k + 1.464e-25_r8k * fluence) * fcwt
        ENDIF
        ! Cladding type term
        akzry = 1.0_r8k
        IF (CladType == 2) akzry = 1.305_r8k
        ! Combine terms
        ak = aktm * (1.0_r8k + akcw + akphi) / akzry
        ! VTT  prevent small values at very high temperature
        ! ak = MAX(2.0e6_r8k, ak)
    CASE (6, 8) ! Zr-1%Nb properties from RRC-KI
        a = -2.534966886_r8k
        b =  0.006626767224_r8k
        c = -5.303091629e-6_r8k
        d =  1.34653092e-9_r8k
        ! Find am (the same as Zry at strain rate > 0.01/s)
        ! No irradiation dependency
        IF (t >= 902.0_r8k) THEN
            am = -0.1619955889_r8k + 3.080302048e-4_r8k * t
        ELSEIF (t > 752.0_r8k) THEN
            am = a + t * (b + t * (c + t * d))
        ELSE
            am = 0.02280034483_r8k - 3.448275862e-7_r8k * t
        ENDIF
        ! check whether use non-irradiated or irradiated properties
        IF (fluence > 1.0e25_r8k) THEN
            ! irradiated Zr-1%Nb parameters
            ! strain hardening exponent, n
            IF (t > 879.0_r8k) THEN
                an = 0.04628421012_r8k + 0.000197951907_r8k * t - 3.314868215e-7_r8k * t ** 2 + &
                  &  1.3913294e-10_r8k * t ** 3
            ELSEIF (t > 759.0_r8k) THEN
                an = -0.239614587_r8k + 0.002839248035_r8k * t - 8.226160457e-6_r8k * t ** 2 + &
                  &  9.276772204e-9_r8k * t ** 3 - 3.588141876e-12_r8k * t ** 4
            ELSE
                an = -0.1255447757_r8k + 0.001350416112_r8k * t - 3.536814687e-6_r8k * t ** 2 + &
                  &  3.734672258e-9_r8k * t ** 3 - 1.365014312e-12_r8k * t ** 4
            ENDIF
            ! strength coefficient, k
            IF (t > 1223.0_r8k) THEN ! limit K value above 1223K
                ! VTT  ak = EXP(-0.005608069738_r8k*1223.0_r8k)*15180.65748_r8k
                ak = EXP(-0.005608069738_r8k * t) * 15180.65748_r8k
            ELSEIF (t > 859.4_r8k) THEN
                ak = EXP(-0.005608069738_r8k * t) * 15180.65748_r8k
            ELSEIF (t > 763.0_r8k) THEN
                ak = EXP(-0.00965027547_r8k * t) * 491246.9131_r8k
            ELSE
                ak = 916.8547193_r8k - 0.6046334417_r8k * t - 0.0002474820043_r8k * t ** 2
            ENDIF
            ak = ak * 1.0e6_r8k
        ELSE
            ! Non-irradiated Zr-1%Nb parameters
            ! strain hardening exponent, an
            IF (t > 1223.0_r8k) THEN
                an = 0.047_r8k
            ELSE
                an = 0.04628421012_r8k + 0.000197951907_r8k * t - 3.314868215e-7_r8k * t ** 2 &
                  &  + 1.3913294e-10_r8k * t ** 3
            ENDIF
            ! strength coefficient, K
            IF (t > 1223.0_r8k) THEN ! limit K value above 1223K
                ak = EXP(-0.005608069738_r8k * t) * 15180.65748_r8k
                ! VTT ak = EXP(-0.005608069738_r8k * 1223.0_r8k) * 15180.65748_r8k ! VTT modified E110 correlation
            ELSEIF (t > 797.9_r8k) THEN
                ak = EXP(-0.005608069738_r8k * t) * 15180.65748_r8k
            ELSE
                ak = 898.3710095_r8k - 1.911883946_r8k * t + 0.002024675204_r8k * t ** 2 - 9.628259856e-7_r8k * t ** 3
            ENDIF
            ak = ak * 1.0e6_r8k
        ENDIF
        ! VTT prevent small values at very high temperature
        ak = MAX(15.0e6_r8k, ak)
    CASE DEFAULT
        WRITE (ounit, *) 'Execution terminated in Subroutine: ckmn; Wrong value of Cladtype. CladType =',CladType
        ERROR STOP 'Execution terminated in Subroutine: ckmn; Wrong value of CladType selected.'
    END SELECT
    !
    END SUBROUTINE ckmn
    !
    !
    !
    SUBROUTINE cmlimt (ctemp, deloxy, fnck, fncn, cwkf, cwnf, cinwid, cinrad, cdpres, caxrad, caxstr, &
      &                rstran, deltmp, strnyt, strnye, strnue, strnie, stsrpt, strrpe, cyldst, cyldse, &
      &                cultse, cbrste, cbrsst, ctstrt, ak, an, am, elmod)
    USE Kinds
    USE variables_fraptran, ONLY : CladType, cexh2l, ounit, ndebug
    USE Material_Properties, ONLY : MatProperty
    IMPLICIT NONE
    !>@brief
    !> cmlimt calculates the following limit points of mechanical deformation:
    !> (1) true tangential stress at burst is calculated as a function of temperature, fast neutron fluence and cold work.
    !> (2) true tangential strain at failure for azimuthally symmetric deformation is calculated from the true tangential 
    !>     burst stress, the pressure at burst, the axial radius of curvature at burst, the axial stress at burst and the 
    !>     initial midwall radius.
    !> (3) cladding stress and strain at the transition between elastic and plastic deformation (yield) are calculated as
    !>     a function of temperature, fast neutron fluence, cold work, average oxygen concentration and strain rate. 
    !> (4) typical instability strains, burst pressures (engineering hoop stress) and circumferential elongations at failure
    !>     are also calculated and returned.
    !>@author
    !> last modified by d. l. hagrman july 1982 (cdf-mp-10)
    !> Modified by PNNL, February 1997, to delete sensitivity uncertainty analysis coding
    !
    ! Input
    !
    ! ctemp  - average cladding temperature (K)
    ! deloxy - average oxygen concentration excluding oxide layer - average oxygen concentration of as received cladding 
    !          (kg oxygen/kg zircaloy)
    ! fnck   - effective fast fluence for strength coefficient (neutrons/(m**2))
    ! fncn   - effective fast fluence for strain hardening exponent (neutrons/(m**2))
    ! cwkf   - effective cold work for strength coefficient (unitless ratio of areas)
    ! cwnf   - effective cold work for strain hardening exponent (unitless ratio of areas)
    ! cinwid - initial cladding wall thickness (m)
    ! cinrad - initial cladding midwall radius (m)
    ! cdpres - pressure dIfferential across cladding at burst (Pa)
    ! caxrad - axial radius of curvature (m)
    ! caxstr - true axial stress (m).  set equal to zero in the subcode if caxrad is greater than 10 meters
    ! rstran - strain rate. (1/s)
    ! deltmp - circumferential temperature variation. used only to find typical values (K)
    !
    ! Output
    !
    ! strnyt - true strain at yield (m/m)
    ! strnye - engineering strain at yield (m/m)
    ! strnue - unIform strain (m/m)
    ! strnie - typical circumferential engineering strain at instability (m/m)
    ! stsrpt - true tangential failure strain for azimuthally symmetric deformation (m/m)
    ! strrpe - typical circumferential engineering strain at rupture(m/m)
    ! cyldst - true yield strength (Pa)
    ! cyldse - engineering yield strength (Pa)
    ! cultse - engineering ultimate strength (Pa)
    ! cbrsst - effective true tangential stress at burst for idealized symmetric deformation with circumference 
    !          equal to the actual cladding circumference (Pa)
    ! ctstrt - true tangential component of stress at burst (Pa)
    ! cbrste - typical engineering hoop stress at burst (Pa)
    !
    ! estimated values of deltmp are
    !  (a) for cladding with external thermocouples and in film boiling  deltmp = 150 K
    !  (b) for cladding with internal heating in steam and without external thermocouples
    !      deltmp = 10 K If the temperature is above 1255 K
    !             = 50 * (1255 - temperature) / 165 + 10 If temperature is between 1090 and 1255 K
    !             = 50 K If temperature is below 1090 K
    !  (c) for cladding in a furnace deltmp = 10 K
    !  (d) for self - resistance heated cladding with cool filler
    !      deltmp = (heating rate)/1000 K/s) * values of (b)
    !
    !
    ! Reference:
    !
    ! (1) r. h. chapman, multirod burst test program quarterly report for april - june 1977,
    !     ornl/nureg/tm-135 (1977).
    ! (2) r. h. chapman, j. l. crowley. a. w. longest and e. g. sewell, effect of creep time and heating rate on
    !     deformation of zircaloy-4 tubes tested in steam with internal heaters, ornl/nureg/tm-245 and nureg/cr-0345 (1978).
    ! (3) r. h. chapman, multirod burst test program quarterly progress report for april - june 1976, 
    !     ornl/nureg/tm-74,  (1977)
    ! (4) r. h. chapman, multirod burst test program progress report for july - December 1977,
    !     ornl/nureg/tm-200 and nureg/cr-0103  (1978).
    ! (5) r. h. chapman, multirod burst test program progress report for january - march 1978,
    !     ornl/nureg/tm-217 and nureg/cr-0225  (1978).
    ! (6) r. h. chapman, multirod burst test program quarterly progress report for january - march 1976,
    !     ornl/nureg/tm-36  (1976).
    ! (7) r. h. chapman, multirod burst test program quarterly progress report for october - december 1976,
    !     ornl/nureg/tm-95  (1977).
    ! (8) r. h. chapman, multirod burst test program quarterly progress report for january - march 1977, 
    !     ornl/nureg/tm-108  (1977).
    ! (9) d. o. hobson and p. l. rittenhouse, deformation and rupture behavior of light water reactor fuel cladding,
    !     ornl-4727  (1971)
    ! (10) h. m. chung and t. f. kassner, deformation characteristics of zircaloy cladding in vacuum and
    !      steam under transient-heating conditions.  summary report, anl-77-31 and nureg/cr-0344 (1978).
    ! (11) a. a. bauer, w. j. gallagher, l. m. lowry and a. j. markworth, evaluating strength and ductility
    !      of irradiated zircaloy. quarterly progress report july through september, 1977, bmi-nureg-1985  (1977).
    ! (12) t. f. cook, s. a. ploger and r. r. hobbins, postirradiation examination results for the irradiation 
    !      effects test ie-5, tree-nureg-1201  (1978).
    ! (13) e. h. karb, results of the fr-2 nuclear tests on the behavior of zircaloy clad fuel rods, paper presented
    !      at the 6th water reactor safety research inFormation meeting, gaithersburg, md,  (1978).
    ! (14) k. wiehr and he. schmidt, out-of-pile-versuche zum aufblahvorgang von zirkaloy-hullen, kfk 2345, (1977).
    ! (15) k. wiehr, f. erbacher, u. harten, w. just, h. j. neitzel, p. schaffner and h. schmidt, jahreskolloquium 1977 des
    !      project nukleare sicherheit, kfk report  (1977).
    !
    ! The correlation for typical enginering hoop stress at burst was taken from:
    ! (1)  j. d. kerrigan: frail. a fuel rod subcode, srd-137-76  (1976).
    !
    INTEGER(ipk) :: iv21
    REAL(r8k), INTENT(IN) :: deltmp
    REAL(r8k) :: fnck, fluence, ctemp, deloxy, fncn, cwkf, cwnf, rstran, ak, an, am, &
      &          elmod, t, ag, strnyt, strnye, cyldst, cyldse, strnue, cultse, ratio, aka, &
      &          akcw, fcwt, strnut, akphi, ctstrt, ctstrt723, ctstrt973, caxrad, caxstr, &
      &          axfac, cinwid, cdpres, stsrpt, cinrad, strrpe, strnie, cbrsst, cbrste, ctemp_F
    
    ! Cladding fluence
    IF (ctemp > 1255.0_r8k) THEN
        fluence = 0.0_r8k
    ELSE
        fluence = MIN(fnck, 1.0e26_r8k)
    END IF
    !
    CALL ckmn (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am)
    !
    elmod = MatProperty (Material='CLAD', Property='YOUNG_MOD', Temperature=ctemp, Fluence=fnck, ColdWork=cwkf, OxygenConcen=deloxy)
    t = ctemp
    ag = ak * ((rstran / 1.0e-03_r8k) ** am)
    ! Calculate strain at yield
    strnyt = ((ag / elmod) ** (1.0_r8k / (1.0_r8k - an)))
    strnye = EXP(strnyt) - 1.0_r8k
    ! Calculate yield strength
    cyldst = (ag / (elmod ** an)) ** (1.0_r8k / (1.0_r8k - an))
    IF (ndebug) WRITE(ounit,101) cyldst, ctemp, ak, an, am, rstran, elmod
101 FORMAT('In Subroutine: CMLIMT,',/,'cyldst,ctemp,ak,an,am,rstran,elmod=',7e13.4)
    cyldse = cyldst / EXP(strnyt)
    ! calculate strain at maximum one Dimensional load
    strnut = an / (1.0_r8k + am)
    iv21 = 0
    IF (iv21 == 1) THEN ! V21 model for strnue
        strnue = EXP(strnut) - 1.0_r8k
    ELSE ! New model for strnue from CL Painter, PNNL, August 3, 1995
        strnue = 0.096_r8k - 1.142e-04_r8k * ctemp + 0.01856_r8k * EXP(-fnck / 1.0e25_r8k) - SQRT(cexh2l / 804976.0_r8k)
    ENDIF
    IF (strnue < 0.0_r8k) strnue = 0.0_r8k
    ! Calculate ultimate engineering strength
    cultse = (ag * (strnue ** an)) / EXP(strnue)
    ! Calculate tangential component of true stress at burst
    IF (t >= 1050.0_r8k) THEN
        ratio = 7.7_r8k
    ELSE IF (t > 750.0_r8k) THEN
        ratio = 46.861429_r8k * EXP(-(1.9901087e6_r8k / (t ** 2)))
    ELSE ! For t < 750
        ratio = 1.36_r8k
    ENDIF
    !
    SELECT CASE (CladType)
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry Properties
        ! 2/6/03  change aka equation to work with current model
        ! aka is subtracting off fluence, cold work, hydrogen effects
        ! aka = (ak-250.e6_r8k)/(1.0_r8k+0.546_r8k*cwkf + 9.76d-27*fluence) + 250.e6_r8k
        ! 3/3/03 change per KJG
        ! aka = ak/(1.0_r8k + 0.546_r8k*cwkf + 9.76d-27*fluence)
        ! 7/2/04 change per KJG
        IF (t > 2100.0_r8k) THEN
            aka = 1.0_r8k
        ELSE IF (t >= 1255.0_r8k) THEN
            aka = 4.330e+07_r8k + t * (-6.685e+04_r8k + t * (37.579_r8k - t * 7.33e-03_r8k))
        ELSE IF (t >= 1090.0_r8k) THEN
            aka = 184.1376039e+06_r8k - 1.4345448e+05_r8k * t
        ELSE IF (t >= 750.0_r8k) THEN
            aka = 2.5224880e+06_r8k * EXP(2.8500027e+06_r8k / (t ** 2))
        ELSE
            aka = 1.17628e+09_r8k + t * (4.54859e+05_r8k + t * (-3.28185e+03_r8k + t * 1.72752_r8k))
        ENDIF
        !
        IF (CladType == 2) aka = aka / 1.305_r8k
        ! Cold work term
        akcw = 0.546_r8k * cwkf
        ! Fluence term
        fcwt = 2.25_r8k * EXP(-cwkf * 20.0_r8k) * MIN(1.0_r8k, EXP((t - 550.0_r8k) / 10.0_r8k)) + 1.0_r8k
        IF (fluence >= 7.5e25_r8k) THEN
            akphi = 0.731995_r8k
        ELSE If (fluence >= 2.0e25_r8k) THEN
            akphi = 0.53236_r8k + 2.6618e-27_r8k * fluence
        ELSE If (fluence >= 0.1e25_r8k) THEN
            akphi = 2.928e-26_r8k * fluence
        ELSE
            akphi = (-0.1464_r8k + 1.464e-25_r8k * fluence) * fcwt
        ENDIF
        !
        ctstrt = ratio * aka + 0.4_r8k * (akphi + akcw) * aka         
    CASE (6, 8) ! Zr-1%Nb from RRC-KI
        akcw = 0.0_r8k
        IF (fluence <= 1.e25_r8k) THEN
            ! Non-irradiated values for T<973K
            ! Temperature < 723K
            ctstrt = 2016.268_r8k - 5.2948_r8k * t + 0.00627_r8k * t ** 2 - 2.8233e-6_r8k * t ** 3
            ! 723 < temperature < 973K
            ctstrt723 = 2016.268_r8k - 5.2948_r8k * 723.0_r8k + 0.00627_r8k * 723.0_r8k ** 2 - &
              &         2.8233e-6_r8k * 723.0_r8k ** 3
            ctstrt973 = 116139.02_r8k * EXP(-0.0065753_r8k * 973.0_r8k)
            ctstrt = ctstrt973 + (ctstrt723 - ctstrt973) * (973.0_r8k - t) / (973.0_r8k - 723.0_r8k)
        ELSE
            ! Irradiated values for T<973K
            ! Temperature < 723K
            ctstrt = 4178.356_r8k - 12.894_r8k * t + 0.0154_r8k * t ** 2 - 6.5545e-6_r8k * t ** 3
            ! 723 < temperature < 973K
            ctstrt723 = 4178.356_r8k - 12.894_r8k * 723.0_r8k + 0.0154_r8k * 723.0_r8k ** 2 - &
              &         6.5545e-6_r8k * 723.0_r8k ** 3
            ctstrt973 = 116139.02_r8k * EXP(-0.0065753_r8k * 973.0_r8k)
            ctstrt = ctstrt973 + (ctstrt723 - ctstrt973) * (973.0_r8k - t) / (973.0_r8k - 723.0_r8k)
        ENDIF
        ! Values for T>973K (same for non-irradiated and irradiated)
        IF (t >= 1190.0_r8k) THEN ! 1190 < temperature < 1473K
            ctstrt = 7611.82_r8k * EXP(-0.4283_r8k * t)
        ELSE IF (t >= 973.0_r8k) THEN ! 973 < temperature < 1190K
            ctstrt = 116139.02_r8k * EXP(-0.0065753_r8k * t)
        ENDIF
        ! Convert to Pa
        ctstrt = 1.0e6_r8k * ctstrt
    CASE DEFAULT
        WRITE (ounit, *) 'Execution terminated in Subroutine: cmlimt; Wrong value of Cladtype. CladType =',CladType
        ERROR STOP 'Execution terminated in Subroutine: cmlimt; Wrong value of CladType selected.'
    END SELECT
    ! Find true tangential failure strain for azimuthally symmetric deformation
    IF (caxrad < 1.0e-03_r8k) caxrad = 1.0e-03_r8k
    IF (caxrad > 10.0_r8k) caxstr = 0.0_r8k
    axfac  = caxstr * cinwid / (2.0_r8k * cdpres * caxrad)
    IF (axfac == 0.0_r8k .AND. caxstr == 0.0_r8k) THEN
        stsrpt = 0.0_r8k
    ELSE
        stsrpt = LOG(SQRT(ctstrt * cinwid / (cdpres * cinrad)) + axfac * (1.0_r8k + 0.5_r8k * axfac))
    END IF
    ! Find typical circumferential engineering strain at rupture
    strrpe = (SQRT(ctstrt * cinwid / (cdpres * cinrad)) - 1.0_r8k) * EXP(-0.01_r8k * deltmp)
    ! Find typical circumferential engineering strain at instability
    strnie = SQRT(ak * cinwid * (10.0_r8k ** (2.0_r8k * am)) / (cdpres * cinrad * (0.866_r8k ** &
      &      (1.0_r8k + am + an)))) - 1.0_r8k
    IF (strnie < 0.05_r8k) strnie = 0.05_r8k
    IF (strnie > 0.5_r8k) strnie = 0.5_r8k
    IF (strrpe < strnie) strrpe = strnie
    cbrsst = (cdpres * cinrad / cinwid) * ((1.0_r8k + strrpe) ** 2)
    strnie = strnie * EXP(-0.01_r8k * deltmp)
    ! Find typical engineering hoop stress at burst (Equations uses temp in Fahrenheit)
    ctemp_F = tkf(ctemp)
    cbrste = (10.0_r8k ** (5.00_r8k + ctemp_F * (3.27e-04_r8k - ctemp_F * (1.14e-06_r8k - ctemp_F * 2.56e-10_r8k)))) / &
        &     1.4505e-04_r8k
    !
    END SUBROUTINE cmlimt
    !
    !
    !
    SUBROUTINE cstres (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, strant, strest)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> cstres calculates cladding effective stress as a function of true cladding strain, true cladding strain rate,
    !> cladding temperature, average oxygen concentration in cladding, fast neutron fluence, and cold work.
    !>@author
    !> cstres was adapted from cstran (a Subroutine by r.l. miller and r.r. hobbins) by g.a. berna in april 1975
    !> modified by d.l. hagrman  april, 1981
    !> modified by e. r. carlson  may, 1984 (remove small strain fix)
    !
    ! Input
    !
    ! strant - Effective true plastic strain (unitless)
    ! rstran - Effective true plastic strain rate (s**(-1))
    ! ctemp  - Cladding meshpoint temperature (k)
    ! deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as received cladding
    !          (kg oxygen/kg zircaloy)
    ! fnck   - Effective fast fluence for strength coefficient (neutrons/(m**2))
    ! fncn   - Effective fast fluence for strain hardening exponent (neutrons/(m**2))
    ! cwkf   - Effective cold work for strength coefficient (unitless ratio of areas)
    ! cwnf   - Effective cold work for strain hardening exponent (unitless ratio of areas)
    !
    ! Output
    !
    ! strest - Effective true stress (Pa)
    !
    ! Note:
    !
    ! Conversion from Pa  to  psi  is 1.4505e-04 (psi/Pa)
    !
    REAL(r8k) :: ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am, arg1, arg, strant, strest
    !
    CALL CKMN (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am)
    !
    IF (rstran < 0.0_r8k) rstran = ABS(rstran)
    arg1 = MAX(1.0E-8_r8k, rstran / 1.0e-3_r8k)
    arg = ak * arg1 ** am
    IF (strant < 0.0_r8k) strant = 1.0e-20_r8k
    strest = arg * strant ** an
    !
    END SUBROUTINE cstres
    !
    !
    !
    SUBROUTINE cstran (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, strest, strant)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> cstran calculates cladding strain as a function of true cladding stress, true cladding strain rate,
    !> cladding temperature, average oxygen concentration in cladding, fast neutron fluence, and cold work.
    !> no internal iterations are contained in cstran.  It is assumed that the user will iterate the input value
    !> of rstran. See subcode cstrni for iterated version.
    !>@author
    !> cstran was coded by r. l. miller and r. r. hobbins in march 1974
    !> modified by d. l. hagrman  april, 1981
    !> modified by e. r. carlson  may, 1984 (remove small strain fix)
    !
    ! Input
    !
    ! strest - Effective true stress (Pa)
    ! rstran - Effective true plastic strain rate (s**(-1))
    ! ctemp  - Cladding meshpoint temperature (k)
    ! deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as received cladding
    !          (kg oxygen/kg zircaloy)
    ! fnck   - Effective fast fluence for strength coefficient (neutrons/(m**2))
    ! fncn   - Effective fast fluence for strain hardening exponent (neutrons/(m**2))
    ! cwkf   - Effective cold work for strength coefficient (unitless ratio of areas)
    ! cwnf   - Effective cold work for strain hardening exponent (unitless ratio of areas)
    !
    ! Output
    !
    ! strant - Effective true plastic strain (unitless)
    !
    ! Note:
    !
    ! Conversion from Pa  to  psi  is 1.4505e-04 (psi/Pa)
    !
    REAL(r8k) :: arg1, arg2, ak, an, am
    REAL(r8k), INTENT(IN) :: ctemp, deloxy, fnck, fncn, cwkf, cwnf, strest
    REAL(r8k), INTENT(OUT) :: strant
    REAL(r8k), INTENT(INOUT) :: rstran
    !
    CALL ckmn (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am)
    !
    arg1 = rstran / 1.0e-03_r8k
    arg2 = MAX(0.0_r8k, strest / (ak * (arg1 ** am)))
    strant = arg2 ** (1.0_r8k / an)
    !
    END SUBROUTINE cstran
    !
    !
    !
    SUBROUTINE cstrni (delh, ctemp, deloxy, fnck, fncn, cwkf, cwnf, strest, strant)
    USE Kinds
    USE variables_fraptran, ONLY : ounit, CladType, ndebug
    IMPLICIT NONE
    !>@brief
    !> cstrni calculates cladding strain as a function of true cladding stress, initial true cladding strain,
    !> time step size, cladding temperature, average oxygen concentration in cladding, fast neutron fluence, and cold work.
    !>@author
    !> cstrni was coded by d. l. hagrman october 1977
    !> modified by d.l. hagrman  april, 1981
    !> modified by e. r. carlson  may, 1984 (remove small strain fix)
    !
    ! Input
    !
    ! strest - Effective true stress (Pa)
    ! strant - Effective true plastic strain at the start of the current time step (unitless)
    ! delh   - Time step size (s)
    ! ctemp  - Cladding meshpoint temperature (k)
    ! deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as received cladding
    !          (kg oxygen/kg zircaloy)
    ! fnck   - Effective fast fluence for strength coefficient (neutrons/(m**2))
    ! fncn   - Effective fast fluence for strain hardening exponent (neutrons/(m**2))
    ! cwkf   - Effective cold work for strength coefficient (unitless ratio of areas)
    ! cwnf   - Effective cold work for strain hardening exponent (unitless ratio of areas)
    ! ndebug - Debug indicator (TRUE/FALSE)
    !
    ! Output
    !
    ! strant - Effective true plastic strain at the end of the current time step (unitless)
    !
    ! Note:
    !
    ! Conversion from Pa  to  psi  is 1.4505e-04 (psi/Pa)
    !
    INTEGER(ipk) :: ii
    REAL(r8k) :: rstran, ak, an, am, stran4
    REAL(r8k), INTENT(IN) :: delh, ctemp, deloxy, fnck, fncn, cwkf, cwnf, strest
    REAL(r8k), INTENT(OUT) :: strant
    !
    IF (ndebug) THEN
        WRITE(ounit,901) delh, ctemp, deloxy, strest, strant
901     FORMAT(' delh=',e11.4,' ctemp=',e11.4,' deloxy=',e11.4,' strest = ',e11.4,' strant = ',e11.4)
        WRITE(ounit,903) fnck, fncn, cwkf, cwnf
903     FORMAT(' fnck = ',e11.4,' fncn = ',e11.4,' cwkf = ',e11.4,' cwnf = ',e11.4)
    ENDIF
    rstran = 1.0e-03_r8k
    CALL ckmn (ctemp, deloxy, fnck, fncn, cwkf, cwnf, rstran, ak, an, am)
    IF (ndebug) WRITE(ounit,905) ak, an, am
905 FORMAT(' from ckmn, ak = ',e13.6,' an = ',e13.6,' am = ',e13.6)
    !
    stran4 = strant
    ii = 0
104 CONTINUE
    IF (ndebug) WRITE(ounit,907) ii, stran4, an, am, ak
907 FORMAT(' after stat 104, i1 = ',i3,' stran4 = ',e11.4,' an = ',e11.4,' am = ',e11.4,' ak = ',e11.4)
    IF (stran4 < 0.0_r8k ) stran4 = 1.0e-20_r8k
    strant = ((an / am + 1.0_r8k) * (1.0e-3_r8k) * ((strest / ak) ** (1.0_r8k / am)) * delh + &
      &      stran4 ** (an / am + 1.0_r8k)) ** (am / (an + am))
    rstran = (strant - stran4) / delh
    ! correct am for revised rstran if necessary
    RETURN
    ! The following lines are not computed because the flag itransient was removed (set to 1)
    IF (rstran < 1.0e-5_r8k) rstran = 1.0e-5_r8k
    IF (rstran > 6.34e-03_r8k) RETURN
    ii = ii + 1
    am = -6.4e-02_r8k + ctemp * 2.203e-04_r8k
    IF (ctemp - 1172.5_r8k <= 0.0_r8k) THEN
        am = am + 6.78e-02_r8k * LOG(6.34e-03_r8k / rstran) * ((ctemp - 1090.0_r8k) / 82.5_r8k)
    ELSE
        am = am + 6.78e-02_r8k * LOG(6.34e-03_r8k / rstran) * ((1255.0_r8k - ctemp) / 82.5_r8k)
    ENDIF
    am = am + EXP(-69.0_r8k * deloxy)
    IF (ii < 10) GOTO 104
    !
    END SUBROUTINE cstrni
!
END MODULE ZrModels

