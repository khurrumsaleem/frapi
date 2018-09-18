MODULE ZrModels_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the Zircaloy caldding models for annealing_frapcon,  
    !> yield stress, mechanical limits, cladding true stress and strain.
    !> Subroutines include caneal, ckmn, cmlimt, cstran, cstres
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE caneal
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit, cwkf, cwnf, fncn, fnck, delhs, avflux, ctemp, rtemp
    IMPLICIT NONE
    !>@brief
    !> Caneal calculates the change in effective fluence and effective cold work during a time step
    !>@author
    !> Coded by d. l. hagrman october 1977
    !> Modified by g. a. reymann and m. a. morgan march 1978
    !> Modified by d. l. hagrman june 1978
    !
    ! Input
    !
    ! avflux - fast neutron flux ((neutrons/m**2)/s)
    ! delhs  - time step size (s)
    ! ctemp  - cladding temperature at start of time step (k)
    ! rtemp  - average rate of change of temperature (k/s)
    ! fnck   - effective fast fluence for strength coefficient at time step start (neutrons/(m**2))
    ! fncn   - effective fast fluence for strain hardening exponent at time step start (neutrons/(m**2))
    ! cwkf   - effective cold work for strength coefficient at time step start (unitless ratio of areas)
    ! cwnf   - effective cold work for strain hardening exponent at time step start (unitless ratio of areas)
    !
    ! Output
    !
    ! fnck   - effective fast fluence for strength coefficient at time step finish (neutrons/(m**2))
    ! fncn   - effective fast fluence for strain hardening exponent at time step finish (neutrons/(m**2))
    ! cwkf   - effective cold work for strength coefficient at time step finish (unitless ratio of areas)
    ! cwnf   - effective cold work for strain hardening exponent at time step finish (unitless ratio of areas)
    !
    ! Reference:
    !
    ! (1)  a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !      Quartely progress report for april through june 1976, bmi-nureg-1956 (july 1976)
    ! (2)  a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !      Quarterly progress report for july through september, 1976, bmi-nureg-1961 (october 1976)
    ! (3)  a. a. bauer, l. m. lowry, and j. s. perrin, evaluating strength and ductility of irradiated zircaloy.
    !      Quartly progress report for october through december 1976, bmi-nureg-1967 (january 1977)
    !
    REAL(r8k) :: t, disok, qk, bk, qn, bn, dison, bki, qki, bni, qni, disoki, fniski, fnisni, fnisok, fnison
    LOGICAL :: WarningMsg
    !
    WarningMsg = .FALSE.
    t = ctemp
    ! Update fluence
    fnck = fnck + avflux * delhs
    fncn = fncn + avflux * delhs
    ! Annealing model
    IF (t > 500.0_r8k) THEN
        IF (cwkf > 1.0e-20_r8k) THEN
            qk = 2.33e18_r8k
            bk = (1.504_r8k) * (1.0_r8k + (2.2e-25_r8k) * fnck)
            disok = qk * rtemp * delhs * (t ** (-7)) * 6.0_r8k
            IF (ABS(disok) < 1.0e-04_r8k) fnisok = 1.0_r8k
            IF (ABS(disok) >= 150.0_r8k) THEN
                WarningMsg = .TRUE.
            ELSE
                IF (ABS(disok) >= 1.0e-04_r8k) fnisok = (EXP(disok) - 1.0_r8k) / disok
                cwkf = EXP(-bk * delhs * EXP(-qk * (t ** (-6))) * fnisok) * cwkf
            END IF
        END IF
        IF (cwnf > 1.0e-20_r8k .AND. .NOT. WarningMsg) THEN
            qn = 2.33e18_r8k
            bn = (1.504_r8k) * (1.0_r8k + (2.2e-25_r8k) * fncn) * 8.0_r8k
            dison = qn * rtemp * delhs * (t ** (-7)) * 6.0_r8k
            IF (ABS(dison) < 1.0e-04_r8k) fnison = 1.0_r8k
            IF (ABS(dison) >= 1.0e-04_r8k) fnison = (EXP(dison) - 1.0_r8k) / dison
            IF (ABS(dison) >= 150.0_r8k) THEN
                WarningMsg = .TRUE.
            ELSE
                cwnf = EXP(-bn * delhs * EXP(-qn * (t ** (-6))) * fnison) * cwnf
            END IF
        END IF
        IF (fnck > 1.0e14_r8k .AND. .NOT. WarningMsg) THEN
            bki = 2.49e-06_r8k
            qki = 5.35e23_r8k
            disoki = qki * rtemp * delhs * (t ** (-9)) * 8.0_r8k
            IF (ABS(disoki) < 1.0e-04_r8k) fniski = 1.0_r8k
            IF (ABS(disoki) >= 150.0_r8k) THEN
                WarningMsg = .TRUE.
            ELSE
                IF (ABS(disoki) >= 1.0e-04_r8k) fniski = (EXP(disoki) - 1.0_r8k) / disoki
                fnck = fnck / 1.0e20_r8k
                fnck = (bki * delhs * (EXP(-qki * (t ** (-8)))) * fniski + (fnck) ** (-1)) ** (-1)
                fnck = fnck * 1.0e20_r8k
            END IF
        END IF
        IF (fncn > 1.0e14_r8k .AND. .NOT. WarningMsg) THEN
            bni = bki * 1000.0_r8k
            qni = qki
            fnisni = fniski
            fncn = fncn / 1.0e20_r8k
            fncn = (bni * delhs * (EXP(-qni * (t ** (-8)))) * fnisni + (fncn) ** (-1)) ** (-1)
            fncn = fncn * 1.0e20_r8k
        END IF
        ! If warning was made during executing model, write to output file
        IF (WarningMsg) THEN
            WRITE (ounit, 150)
            WRITE (0, 150)
150         FORMAT ('  time step too large for cladding annealing model ')
        END IF
    END IF
    !
    END SUBROUTINE caneal
    !
    !
    !
    SUBROUTINE ckmn (ctemp, cexh2, ak, an, am)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : icm, cwkf, fnck, rstran
    IMPLICIT NONE
    !>@brief
    !> ckmn calculates parameters for the cladding equation of state as a function of temperature, 
    !> average oxygen concentration, fast neutron fluence, and cold work.
    !>@author
    !> coded by d. l. hagrman august 1977
    !> modified by g. a. reymann and m. a. morgan march 1978
    !> modified by d. l. hagrman october, 1978
    !
    ! Input
    !
    ! ctemp  - cladding meshpoint temperature (k)
    ! fnck   - effective fast fluence for strength coefficient (neutrons/(m**2))
    ! cwkf   - effective cold work for strength coefficient (unitless ratio of areas)
    !
    ! Output
    !
    ! ak     - strength coefficient (pa)
    ! an     - strain hardening exponent (unitless)
    ! am     - strain rate sensitivity exponent (unitless)
    ! um     - estimated  uncertainty of am (not currently returned)
    ! un     - estimated uncertainty of an (not currently returned)
    !          Note: an - un is negative near 1000k
    ! uk     - estimated uncertainty of ak (not currently returned)
    ! cexh2  - hydrogen concentration that is in excess of solubility limit (ppm)
    !
    ! Reference:
    !
    ! (1)  c.r. woods, properties of zircaloy-4 tubing, wapd-tm-585-1966
    ! (2)  ultimate strength data of h.c. brassfield,et al. gemp-482(1968)
    ! (3)  a.l. bement, jr., effects of cold work and neutron irradiation on the tensile properties of zircaloy-2, hw-74955
    ! (4)  a. cowan and w.j. langford j.nuclear mater. 30 (1969) pp 271-281
    ! (5)  l.m. howe and w.r. thomas, j. nuclear  mater. 1 (1960) p 248
    ! (6)  a. m. garde light-water-reactor safety research program quarterly progress report april - june 1975 anl-75-58
    ! (7)  a. m. garde light-water-reactor safety research program quarterly progress report july - september 1975 anl-75-72
    ! (8)  r.l. mehan and f.w. wiesinger, mechanical properties of zircaloy-2, kapl-2110
    ! (9)  d. lee and w.a. backofen tms-aime 239  (1967) pp 1034-1040
    ! (10) p. j. crescimanno, high strain rate tensile tests of zircaloy at 550f, wapd-tm-1263 (february 1976).
    ! (11) c. r. hann et al, transient deformation properties of zircaloy for  loca simulation, epri np-526 volume 3 (march 1978)
    !
    REAL(r8k) :: t, fluence, fcwt, anzry, antm, anphi, akzry, aktm, akphi, akcw
    REAL(r8k), INTENT(IN) :: ctemp, cexh2
    REAL(r8k), INTENT(OUT) :: ak, an, am
    !
    t = ctemp
    ! limit strain rate to a minimum of 1.0e-05
    rstran = MAX(rstran, 1.0e-05_r8k)
    ! limit fluence to 1.0d26 n/m2
    fluence = MIN(1.0e26_r8k, fnck)
    ! find strain rate exponent, am
    IF (t > 800.0_r8k) THEN
        am = 0.000324124_r8k * t - 0.20701_r8k
    ELSE IF (t > 750.0_r8k) THEN
        am = 0.0007458_r8k * t - 0.544338_r8k
    ELSE
        am = 0.015_r8k
    END IF
    ! find strain hardening exponent, an
    IF (t >= 1600.0_r8k) THEN
        antm = 0.17344880_r8k
    ELSE IF (t >= 1099.0722_r8k) THEN
        antm = -0.22655119_r8k + 2.5e-04_r8k * t
    ELSE IF (t >= 419.4_r8k) THEN
        antm = -9.490e-02_r8k + t * (1.165e-03_r8k + t * (-1.992e-06_r8k + t * 9.558e-10_r8k))
    ELSE
        antm = 0.11405_r8k
    END IF
    ! fluence term
    IF (fluence >= 7.5e25_r8k) THEN
        anphi = 1.608953_r8k
    ELSE IF (fluence >= 2.0e25_r8k) THEN
        anphi = 1.5435_r8k + 0.008727e-25_r8k * fluence
    ELSE IF (fluence >= 0.1e25_r8k) THEN
        anphi = 1.369_r8k + 0.096e-25_r8k * fluence
    ELSE 
        anphi = 1.321_r8k + 0.48e-25_r8k * fluence
    END IF
    ! cladding type term
    IF (icm == 2) THEN
        anzry = 1.6_r8k
    ELSE
        anzry = 1.0_r8k
    END IF
    ! combine terms
    an = antm * anphi / anzry
    ! find strength coefficient, ak
    IF (t > 2100.0_r8k) THEN
        aktm = 1.0_r8k
    ELSE IF (t >= 1255.0_r8k) THEN
        aktm = 4.330e7_r8k + t * (-6.685e4_r8k + t * (37.579_r8k - t * 7.33e-3_r8k))
    ELSE IF (t >= 1090.0_r8k) THEN
        aktm = 184.1376039e6_r8k - 1.4345448e5_r8k * t
    ELSE IF (t >= 750.0_r8k) THEN
        aktm = 2.5224880e6_r8k * EXP(2.8500027e6_r8k / (t ** 2))
    ELSE
        aktm = 1.17628e9_r8k + t * (4.54859e5_r8k + t * (-3.28185e3_r8k + t * 1.72752_r8k))
    END IF
    ! cold work term
    akcw = 0.546_r8k * cwkf
    ! fluence term
    IF (fluence >= 7.5e25_r8k) THEN
        akphi = 0.731995_r8k
    ELSE IF (fluence >= 2.0e25_r8k) THEN
        akphi = 0.53236_r8k + 2.6618e-27_r8k * fluence
    ELSE IF (fluence >= 0.1e25_r8k) THEN
        akphi = 2.928e-26_r8k * fluence
    ELSE
        fcwt = 2.25_r8k * EXP(-cwkf * 20.0_r8k) * MIN(1.0_r8k, EXP((t - 550.0_r8k) / 10.0_r8k)) + 1.0_r8k
        akphi = (-0.1464_r8k + 1.464e-25_r8k * fluence) * fcwt
    END IF
    ! cladding type term
    IF (icm == 2) THEN
        akzry = 1.305_r8k
    ELSE
        akzry = 1.0_r8k
    END IF
    ! combine terms
    ak = aktm * (1.0_r8k + akcw + akphi) / akzry
    !
    END SUBROUTINE ckmn
    !
    !
    !
    SUBROUTINE cmlimt (cinwid, cinrad, cdpres, caxrad, caxstr, deltmp, strnyt, strnye, strnue, strnie, &
      &                stsrpt, strrpe, cyldst, cyldse, cultse, cbrste, cbrsst, ctstrt)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : icm, fastfluence, excesh2, cladavgtemp, fnck, rstran, cwkf
    USE Material_Properties_frapcon, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> cmlimt calculates the following limit points of mechanical deformation. (1) true tangential stress at burst is
    !> calculated as a function of temperature, fast neutron fluence and cold work.  (2) true tangential strain at failure
    !> for azimuthally symmetric deformation is calculated from the true tangential burst stress, the pressure at burst, the axial
    !> radius of curvature at burst, the axial stress at burst and the initial midwall radius.  (3) cladding stress and strain
    !> at the transition between elastic and plastic deformation (yield) are calculated as a function of temperature,
    !> fast neutron fluence, cold work, average oxygen concentration and strain rate.  (4) typical instability strains,
    !> burst pressures (engineering hoop stress) and circumferential elongations at failure are also calculated and returned.
    !>@author
    !> modified by d. l. hagrman may 1979
    !
    ! Input
    !
    ! cladavgtemp - average cladding temperature (k)
    ! fnck        - effective fast fluence for strength coefficient (neutrons/(m**2))
    ! cinwid      - initial cladding wall thickness (m)
    ! cinrad      - initial cladding midwall radius (m)
    ! cdpres      - pressure differential across cladding at burst (pa)
    ! caxrad      - axial radius of curvature (m)
    ! caxstr      - true axial stress (m).  set equal to zero in the subcode If caxrad is greater than 10 meters
    ! rstran      - strain rate. (1./s)
    ! excesh2     - hydrogen concentration in excess of solubility limit (ppm)
    ! deltmp      - circumferential temperature variation. used only to find typical values (k)
    !               Estimated values of deltmp are:
    !               (a) for cladding with external thermocouples and in film boiling  deltmp - 150k
    !               (b) for cladding with internal heating in steam and without external thermocouples
    !               deltmp - 10k If the temperature is above 1255k, deltmp - 50 * (1255 - temperature) / 165 + 10.
    !               If temperature is between 1090 and 1255k, deltmp - 50k If temperature is below 1090k
    !               (c) for cladding in a furnace, deltmp - 10k
    !               (d) for self - resistance heated cladding with cool filler
    !               deltmp - (heating rate)/1000k/s) * values of (b)
    !
    ! Output
    !
    ! strnyt      - true strain at yield (m/m)
    ! strnye      - engineering strain at yield (m/m)
    ! strnue      - uniform strain (m/m)
    ! strnie      - typical circumferential engineering strain at instability (m/m)
    ! stsrpt      - true tangential failure strain for azimuthally symmetric deformation (m/m)
    ! strrpe      - typical circumferential engineering strain at rupture(m/m)
    ! cyldst      - true yield strength (pa)
    ! cyldse      - engineering yield strength (pa)
    ! cultse      - engineering ultimate strength (pa)
    ! cbrsst      - effective true tangential stress at burst for idealized symmetric deformation with 
    !               circumference equal to the actual cladding circumference (pa)
    ! ctstrt      - true tangential component of stress at burst (pa)
    ! cbrste      - typical engineering hoop stress at burst (pa)
    ! uctstt      - estimated uncertainty of ctstrt (pa) (not currently returned)
    !
    ! Reference:
    !
    ! (1)  r. h. chapman, multirod burst test program quarterly report for april - june 1977, ornl/nureg/tm-135 (1977).
    ! (2)  r. h. chapman, j. l. crowley. a. w. longest and e. g. sewell, effect of creep time and heating rate on
    !      deformation of zircaloy-4 tubes tested in steam with internal heaters, ornl/nureg/tm-245 and nureg/cr-0345 (1978).
    ! (3)  r. h. chapman, multirod burst test program quarterly progress report for:
    !      january - march 1976, ornl/nureg/tm-36 (1976)
    !      april - june 1976, ornl/nureg/tm-74, (1977)
    !      october - december 1976, ornl/nureg/tm-95 (1977)
    !      january - march 1977, ornl/nureg/tm-108 (1977)
    !      july - December 1977, ornl/nureg/tm-200 and nureg/cr-0103 (1978)
    !      january - March 1978, ornl/nureg/tm-217 and nureg/cr-0225 (1978)
    ! (4)  d. o. hobson and p. l. rittenhouse, deformation and rupture behavior of LWR fuel cladding, ornl-4727 (1971)
    ! (5)  h. m. chung and t. f. kassner, deformation characteristics of zircaloy cladding in vacuum and
    !      steam under transient-heating conditions.  summary report, anl-77-31 and nureg/cr-0344 (1978).
    ! (6)  a. a. bauer, w. j. gallagher, l. m. lowry and a. j. markworth, evaluating strength and ductility
    !      of irradiated zircaloy. quarterly progress report july through september, 1977, bmi-nureg-1985 (1977).
    ! (7)  t. f. cook, s. a. ploger and r. r. hobbins, postirradiation examination results for the irradiation
    !      effects test ie-5, tree-nureg-1201 (1978).
    ! (8)  e. h. karb, results of the fr-2 nuclear tests on the behavior of zircaloy clad fuel rods, paper presented
    !      meeting, gaithersburg, md, (1978).
    ! (9)  k. wiehr and he. schmidt, out-of-pile-versuche zum aufblahvorgang von zirkaloy-hullen, kfk 2345,  (1977).
    ! (10) k. wiehr, f. erbacher, u. harten, w. just, h. j. neitzel, p. schaffner and h. schmidt, jahreskolloquium
    !      des project nukleare sicherheit, kfk report (1977).
    !
    ! The correlation for typical enginering hoop stress at burst was taken from:
    ! (1)  j. d. kerrigan: frail. a fuel rod subcode, srd-137-76 (1976).
    !
    REAL(r8k) :: fluence, ak, an, am, t, ag, strnut, ratio, aka, akcw, fcwt, akphi, uctstt, axfac, ft, elmod
    REAL(r8k), INTENT(IN) :: cinwid, cinrad, cdpres, deltmp
    REAL(r8k), INTENT(INOUT) :: caxrad, caxstr
    REAL(r8k), INTENT(OUT) :: strnyt, strnye, strnue, strnie, stsrpt, strrpe, cyldst, cyldse, &
      &                       cultse, cbrste, cbrsst, ctstrt
    !
    IF (cladavgtemp > 1255.0_r8k) THEN
        fluence = 0.0_r8k
    ELSE
        fluence = MIN(fnck, 1.0e26_r8k)
    END IF
    !
    CALL ckmn (cladavgtemp, excesh2, ak, an, am)
    elmod = MatProp ('CLAD', 'YOUNG_MOD', cladavgtemp)
    t = cladavgtemp
    ag = ak * ((rstran / 1.0e-03_r8k) ** am)
    ! calculate strain at yield
    strnyt = ((ag / elmod) ** (1.0_r8k / (1.0_r8k - an)))
    strnye = EXP(strnyt) - 1.0_r8k
    ! calculate yield strength
    cyldst = (ag / (elmod ** an)) ** (1.0_r8k / (1.0_r8k - an))
    cyldse = cyldst / EXP(strnyt)
    ! calculate strain at maximum one dimensional load
    strnut = an / (1.0_r8k + am)
    ! ***previous model***
    ! strnue = EXP(strnut) - 1.0_r8k
    ! new model from Chad Painter, PNL, August 3, 1995.
    strnue = 0.096_r8k - (1.142e-4_r8k) * cladavgtemp + 0.01856_r8k * EXP(-fnck / 1.0e25_r8k) - SQRT(excesh2 / 804976.0_r8k)
    strnue = MAX(strnue, 0.0_r8k)
    ! calculate ultimate engineering strength
    cultse = (ag * (strnut ** an)) / EXP(strnut)
    ! calculate tangential component of true stress at burst
    IF (t <= 750.0_r8k) THEN
        ratio = 1.36_r8k
    ELSE IF (t < 1050.0_r8k) THEN
        ratio = 46.861429_r8k * EXP(-(1.9901087e6_r8k / (t ** 2)))
    ELSE
        ratio = 7.7_r8k
    END IF
    ! Find annealed state strength coefficient (aka)
    IF (t < 750.0_r8k) THEN
        aka = 1.17628e9_r8k + t * (4.54859e5_r8k + t * (-3.28185e3_r8k + t * 1.72752_r8k))
    ELSE IF (t < 1090.0_r8k) THEN
        aka = 2.522488e6_r8k * EXP(2.8500027e6_r8k / (t ** 2))
    ELSE IF (t < 1255.0_r8k) THEN
        aka = 184.1376039e6_r8k - 1.4345448e5_r8k * t
    ELSE IF (t <= 2100.0_r8k) THEN
        aka = 4.33e7_r8k + t * (-6.685e4_r8k + t * (3.7579e1_r8k - t * 7.33e-3_r8k))
    ELSE
        aka = 1.0_r8k
    END IF
    IF (icm == 2) aka = aka / 1.305_r8k
    ! cold work term
    akcw = 0.546_r8k * cwkf
    ! fluence term
    IF (fluence < 0.1e25_r8k) THEN
        fcwt = 2.25_r8k * EXP(-cwkf * 20.0_r8k) * MIN(1.0_r8k, EXP((t - 550.0_r8k) / 10.0_r8k)) + 1.0_r8k
        akphi = (-0.1464_r8k + 1.464e-25_r8k * fluence) * fcwt
    ELSE IF (fluence < 2.0e25_r8k) THEN
        akphi = 2.928e-26_r8k * fluence
    ELSE IF (fluence < 7.5e25_r8k) THEN
        akphi = 0.53236_r8k + 2.6618e-27_r8k * fluence
    ELSE
        akphi = 0.731995_r8k
    END IF
    ctstrt = ratio * aka + 0.4_r8k * (akphi + akcw) * aka
    ! Uncertainty estimate for ctstrt
    uctstt = 0.17_r8k * ctstrt
    ! True tangential failure strain for azimuthally symmetric deformation
    caxrad = MAX(caxrad, 1.0e-03_r8k)
    IF (caxrad > 10.0_r8k) caxstr = 0.0_r8k
    axfac = caxstr * cinwid / (2.0_r8k * cdpres * caxrad)
    stsrpt =  LOG(SQRT((ctstrt * cinwid) / (cdpres * cinrad)) + axfac * (1.0_r8k + 0.5_r8k * axfac))
    ! Typical circumferential engineering strain at rupture
    strrpe = (SQRT((ctstrt * cinwid) / (cdpres * cinrad)) - 1.0_r8k) * EXP(-0.01_r8k * deltmp)
    ! Effective true tangential stress at burst for idealized symmetric deformation and typical circumferential strain
    cbrsst = (cdpres * cinrad / cinwid) * ((1.0_r8k + strrpe) ** 2.0_r8k)
    ! Typical circumferential engineering strain at instability
    strnie = MAX(SQRT(ak * cinwid * (10.0_r8k ** (2.0_r8k * am)) / (cdpres * cinrad * &
      &      (0.866_r8k ** (1.0_r8k + am + an)))) - 1.0_r8k, 0.05_r8k)
    strnie = strnie * EXP(-0.01_r8k * deltmp)
    ! Typical engineering hoop stress at burst
    ft = tkf(cladavgtemp)
    cbrste = (10.0_r8k ** (5.0_r8k + ft * (3.27e-04_r8k - ft * (1.14e-06_r8k - ft * 2.56e-10_r8k)))) / 1.4505e-04_r8k
    !
    END SUBROUTINE cmlimt
    !
    !
    !
    SUBROUTINE cstran (ctemp, strest, cexh2, strant)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : rstran
    IMPLICIT NONE
    !>@brief
    !> cstran calculates cladding strain as a function of true cladding stress, true cladding strain rate,
    !> cladding temperature, average oxygen concentration in cladding, fast neutron fluence, and cold work.
    !> no internal iterations are contained in cstran. It is assumed that the user will iterate the input value
    !> of rstran.
    !>@author
    !> cstran was coded by r. l. miller and r. r. hobbins in march 1974
    !> modified by d. l. hagrman  april, 1981
    !
    ! Input
    !
    ! strest - effective true stress (pa)
    ! rstran - effective true plastic strain rate (s**(-1))
    ! ctemp  - cladding meshpoint temperature (k)
    ! cexh2  - concentration of H2 in cladding above solubility limit, (ppm)
    !
    ! Output
    !
    ! strant - effective true plastic strain (unitless)
    !
    REAL(r8k) :: ak, an, am, arg1, arg2
    REAL(r8k), INTENT(IN) :: ctemp, strest, cexh2
    REAL(r8k), INTENT(OUT) :: strant
    !
    CALL ckmn (ctemp, cexh2, ak, an, am)
    arg1 = rstran / 1.0e-03_r8k
    arg2 = MAX(strest / (ak * ((arg1) ** am)), 0.0_r8k)
    strant = arg2 ** (1.0_r8k / an)
    ! strant = (strest / (ak * ((rstran / 1.0e-03_r8k) ** am))) ** (1.0_r8k / an)
    ! Small strain correction is commented out.
    ! strant = MAX(strant, 1.0e-06_r8k)
    ! revise 'an' for small strains
    ! IF (strant < (an / (1.0_r8k + am))) THEN
    !     an = an ** 2 / ((1.0_r8k + am) * strant)
    !     IF (ctemp >= 780.0_r8k) THEN
    !         anl = 0.95_r8k
    !     ELSE IF (ctemp > 730.0_r8k) THEN
    !         anl = 1.56e-02_r8k * ctemp - 11.218_r8k
    !     ELSE
    !         anl = 0.17_r8k
    !     ENDIF
    !     IF (an > anl) an = anl
    !     strant = (strest / (ak * ((rstran / 1.0e-03_r8k) ** am))) ** (1.0_r8k / an)
    ! END IF
    END SUBROUTINE cstran
    !
    !
    !
    SUBROUTINE cstres (ctemp, strant, cexh2, strest)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : rstran
    IMPLICIT NONE
    !>@brief
    !> cstres calculates cladding effective stress as a function of true cladding strain, true cladding strain rate,
    !> cladding temperature, average oxygen concentration in cladding, fast neutron fluence, and cold work.
    !>@author
    !> cstres was adapted from cstran (a Subroutine by r.l. miller and r.r. hobbins) by g.a. berna in april 1975
    !> modified by d.l. hagrman  april, 1981
    !
    ! Input
    !
    ! strant - effective true plastic strain (unitless)
    ! rstran - effective true plastic strain rate (s**(-1))
    ! ctemp  - cladding meshpoint temperature (k)
    ! cexh2  - H2 concentration above solubility limit (ppm)
    !
    ! Output
    !
    ! strest - effective true stress (pa)
    !
    REAL(r8k) :: anl, arg, ak, an, am
    REAL(r8k), INTENT(IN) :: ctemp, cexh2
    REAL(r8k), INTENT(OUT) :: strest
    REAL(r8k), INTENT(INOUT) :: strant
    !
    CALL ckmn (ctemp, cexh2, ak, an, am)
    strant = MAX(strant, 1.0e-06_r8k)
    ! revise 'an' for small strains
    IF (strant < (an / (1.0_r8k + am))) an = (an ** 2) / ((1.0_r8k + am) * strant)
    IF (ctemp >= 780.0_r8k) THEN
        anl = 0.95_r8k
    ELSE IF (ctemp > 730.0_r8k) THEN
        anl = 1.56e-02_r8k * ctemp - 11.218_r8k
    ELSE
        anl = 0.17_r8k
    END IF
    IF (an > anl) an = anl
    arg = ak * ((rstran / 1.0e-3_r8k) ** am)
    strest = arg * (strant ** an)
    !
    END SUBROUTINE cstres
    !
END MODULE ZrModels_frapcon



