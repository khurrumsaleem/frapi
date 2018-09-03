MODULE Zirconium
    USE Kinds
    USE Conversions
    USE Functions
    USE Variables, ONLY : siggro, RinterfacPress, ProblemTime, icm, cladelmod, ctmelt, &
      &                   chefus, ctranb, ctrane, ctranz, deloxy, j, it, coldwk, zro2o, cwkf, &
      &                   fnck, FastFlux, FastFluence, delhs, ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains all Zirconium Properties
    !>@author
    !> Ian Porter, NRC
    !
    ! Theoretical Density (g/cm3)
    REAL(r8k), PARAMETER :: ZircTD = 6.52_r8k
    ! Melting temperature (K)
    REAL(r8k), PARAMETER :: ZircTmelt = 2098.15_r8k
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatPropZirc (property, temp) RESULT (mat_prop)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This function calls all of the Zirconium Material Properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    ! Inputs
    !
    ! property    - material property to be calculated
    ! temp        - material temperature
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    REAL(r8k) :: thexp_ax, thexp_rad
    REAL(r8k), INTENT(IN) :: temp
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THERMCOND')  !Thermal Conductivity
        mat_prop = cthcon (temp)
    CASE ('TDENSITY') ! Theoretical Density, (g/cm^3)
        mat_prop = ZircTD
    CASE ('TMELT')  ! Melting Temperature
        CALL ZircPhysProp
        mat_prop = ZircTMelt
    CASE ('SPECHEAT')        ! Specific Heat (J/kg*K)
        mat_prop = 0.0_r8k
    CASE ('SWELLING')        ! Swelling
        mat_prop = 0.0_r8k
    CASE ('THEXP_AXIAL')     ! Axial Thermal Expansion
        CALL cthexp (temp, thexp_ax, thexp_rad)
        mat_prop = thexp_ax
    CASE ('THEXP_RADIAL')    ! Radial Thermal Expansion
        CALL cthexp (temp, thexp_ax, thexp_rad)
        mat_prop = thexp_rad
    CASE ('EMISS')           ! Emissivity
        mat_prop = zoemis (temp, zro2o)
        ! Note: We are aware that emissivity for gap conductance is being calculated as a function 
        !       of the outer cladding oxide thickness. However, we do not currently model the ID 
        !       oxidation and will leave it as such until a new model or better data is available.
        !       IP, June 2015
    CASE ('DENSIFICATION')   ! Densification
        mat_prop = 0.0_r8k
    CASE ('ENTHALPY')        ! Enthalpy
        mat_prop = 0.0_r8k
    CASE ('RELOCATION')      ! Relocation
        mat_prop = 0.0_r8k
    CASE ('POIS_RATIO')      ! Poisson's Ratio
        mat_prop = 0.0_r8k
    CASE ('YOUNG_MOD')       ! Young's Modulus
        mat_prop = celmod (temp)
    CASE ('YIELD_STRESS')    ! Yield stress
        mat_prop = 0.0_r8k
    CASE ('THEXP_COEF')      ! Thermal expansion coefficient
        mat_prop = 0.0_r8k
    CASE ('CREEP')           ! Creep
        mat_prop = 0.0_r8k
    CASE ('AXIAL_GROW')      ! Axial Growth
        mat_prop = cagrow ()
    CASE ('SHEAR_MOD')       ! Shear Modulus
        mat_prop = cshear (temp)
    CASE ('MEYER_HARD')      ! Meyer's Hardness
        mat_prop = cmhard (temp)
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module Zirconium. Invalid material property ID. Material Property ID = ',a20)
        STOP
    END SELECT
    !
    END FUNCTION MatPropZirc
    !
    !
    !
    REAL(r8k) FUNCTION cthcon (ctemp)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> cthcon calculates cladding thermal conductivity as a function of temperature, time, flux and cold work.
    !>@author
    !> This version of cthcon was developed by r.l. miller, dec 1975
    !> Last modified by e.r. carlson in june 1978.
    !
    ! Input
    !
    ! ctemp  - Cladding meshpoint temperature (k)
    !
    ! Output
    !
    ! cthcon - Thermal conductivity of Zircaloy-4 (w/m-k)
    !
    ! Reference:
    !
    ! (1) w.k. anderson, c.j. beck, a.r. kephart and j.s. theilacker, astm-stp-314, 1962, pp 62 - 93
    ! (2) j.n. chirigos, c. kass, w.s. kirk and g.j. salvaggio. Fuel element fabrication, academic press, 1961, pp 19-55
    ! (3) a.d. feith, gemp-669 (oct 1966)
    ! (4) c.f. lucks and h.w. deem, bmi-1273 (1958) pp 7-9
    ! (5) a.e. powers, kapl-2146 (1961)
    ! (6) d.b. scott, wcap-3269-41 (1965) pp 5-9
    !
    ! Notes:
    !
    ! This version of cthcon does not use time, flux or coldwork as parameters in calculation of zircaloy thermal conductivity.
    ! one standard deviation of this function = 1.01 w/m-k
    !
    REAL(r8k), INTENT(IN) :: ctemp
    !
    cthcon = 7.511_r8k + ctemp * (2.088e-2_r8k + ctemp * (-1.450e-5_r8k + ctemp * 7.668e-09_r8k))
    !
    END FUNCTION cthcon
    !
    !
    !
    SUBROUTINE ZircPhysProp
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> Physical Properties of Zirconium based alloys. (Tmelt, TD, density)
    !> Note that this subroutine does not return any parameters directly but rather stores into the Module Variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> August 2015
    !
    ! Input
    !
    ! bup     - fuel burnup
    ! comp    - plutonia weight percent (wt%)
    !
    ! Output
    !
    ! chefus  - Heat of fusion
    ! ctranb  - Start of alpha-beta transition temperature
    ! ctrane  - End of alpha-beta transition temperature
    !
    REAL(r8k) :: wfox
    !
    ! The following are not used:
    ! Heat of fusion
    chefus = 22.5e4_r8k
    !
    ! Alpha - Beta Transition Temperature
    !
    ! Start of alpha-beta transition temperature
    wfox = deloxy + 0.0012_r8k
    IF (wfox < 0.025_r8k) THEN
        ctranb = 1094.0_r8k + wfox * (-1.289e3_r8k + wfox * 7.914e5_r8k)
    ELSE
        ctranb = 1556.4_r8k + 3.8281e4_r8k * (wfox - 0.025_r8k)
    END IF
    ! End of alpha-beta transition temperature
    IF (deloxy < 4.7308937e-03_r8k) THEN
        ctrane = 392.46_r8k * ((100.0_r8k * deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
    ELSE
        ctrane = (100.0_r8k * deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
    END IF
    ! Isothermal alpha-beta transition temperature
    ctranz = 1135.15_r8k
    !
    END SUBROUTINE ZircPhysProp
    !
    !
    !
    REAL(r8k) FUNCTION ccp (ctemp)
    USE Kinds
    USE Conversions
    USE Functions, ONLY : polate
    IMPLICIT NONE
    !>@brief
    !> ccp calculates the specific heat at constant pressure for zircaloys
    !>@author
    !> ccp coded by r. l. miller october 1974
    !> modified by d. l. hagrman may 1976
    !
    ! Input
    !
    ! ctemp - Cladding temperature (k)
    !
    ! Output
    !
    ! ccp   - Cladding specific heat at constant pressure (j/kg-k)
    !
    ! Reference:
    !
    ! (1) c. r. brooks and e. e. stansbury, "The specific heat of zircaloy-2 from 50 to 700 C", 
    !     journal of nuclear materials 18 (1966) p 233
    ! (2) e. a. eldridge and h. w. deem, specific heats and heats of transformation of zircaloy-2 and 
    !     low nickel zircaloy-2 bmi-1803 (may 31, 1967)
    !
    ! Note:
    !
    ! The estimated standard error of the prediction of ccp for the specific heat of zircaloy cladding samples
    ! -- (sum of squared residuals/(number of residuals-degrees of freedom))**0.5 -- is:
    ! (1) for temperature less than 1090 k, 10j/kg-k)
    ! (2) for temperature 1090 k to 1300 k, 25j/kg-k)
    ! (3) for temperature above 1300 k ,   100j/kg-k)
    ! 
    INTEGER(ipk) :: iu = 1
    INTEGER(ipk), PARAMETER :: npcp = 13
    REAL(r8k), INTENT(IN) :: ctemp
    REAL(r8k), PARAMETER :: Tmax = 1248.0_r8k
    REAL(r8k), PARAMETER :: Tmin = 300.0_r8k
    REAL(r8k), DIMENSION(2*npcp), PARAMETER :: cpdata = (/ 281.0_r8k,  300.0_r8k, 302.0_r8k,  400.0_r8k, 331.0_r8k, 640.0_r8k, &
      &                                                    375.0_r8k, 1090.0_r8k, 502.0_r8k, 1093.0_r8k, 590.0_r8k, 1113.0_r8k, &
      &                                                    615.0_r8k, 1133.0_r8k, 719.0_r8k, 1153.0_r8k, 816.0_r8k, 1173.0_r8k, &
      &                                                    770.0_r8k, 1193.0_r8k, 619.0_r8k, 1213.0_r8k, 469.0_r8k, 1233.0_r8k, &
      &                                                    356.0_r8k, 1248.0_r8k /)
    !
    IF (ctemp >= Tmax) THEN
        ccp = 356.0_r8k
    ELSE IF (ctemp <= Tmin) THEN
        ccp = 281.0_r8k
    ELSE
        ccp = polate(cpdata,ctemp,npcp,iu)
    END IF
    !
    END FUNCTION ccp
    !
    !
    !
    SUBROUTINE cthexp (ctemp, cathex, cdthex)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> cthexp calculates axial and diametral thermal expansion of zircaloy lwr cladding.
    !>@author
    !> cthexp was originally coded by r.l. miller in nov. 1974.
    !> last updated by g.a. reymann in june 1978.
    !
    ! Input
    !
    ! ctemp  - Cladding meshpoint temperature (k)
    !
    ! Output
    !
    ! cathex - Axial thermal expansion of zircaloy (m/m)
    ! cdthex - Diametral thermal expansion of zircaloy (m/m)
    !
    ! Reference:
    !
    ! Correlation from room temperature to 1273k
    ! (1) r.l. mehan and f.w. wiesinger, "mechanical properties of zircaloy-2", usaec report kapl-2110 (1961)
    ! (2) d.b. scott, "physical and mechanical properties of zircaloy-2 and -4", wcap-3269-41 (1965) 
    ! (3) j.j. kearns, "thermal expansion and preferred orientation in zircaloy", wapd-tm-472 (1965)
    !
    ! Above 1273k the coefficient of thermal expansion used is the constant value 9.7e-06/k 
    ! (1) b. lustman and f. kerze, "the metallurgy of zirconium", mc graw-hill book company, new york (1955) page 355.
    !
    ! Between 1073 and 1273k (approximately the alpha-beta transition range for zircaloy) cthexp uses 
    ! the linear interpolation routine polate to find the thermal expansion.
    !
    INTEGER(ipk) :: npcatx = 22, npdexp = 22, iu = 1
    REAL(r8k), INTENT(IN) :: ctemp
    REAL(r8k), INTENT(OUT) :: cathex, cdthex
    REAL(r8k), DIMENSION(44), PARAMETER :: cathxd = (/ &
      &            0.0_r8k,   293.15_r8k, 3.52774e-03_r8k, 1073.15_r8k, 3.5300e-03_r8k, 1083.15_r8k, &
      &       3.50e-03_r8k,  1093.15_r8k, 3.46000e-03_r8k, 1103.15_r8k, 3.4100e-03_r8k, 1113.15_r8k, &
      &       3.330e-03_r8k,  1123.15_r8k, 3.21000e-03_r8k, 1133.15_r8k, 3.0700e-03_r8k, 1143.15_r8k, &
      &       2.800e-03_r8k,  1153.15_r8k, 2.50000e-03_r8k, 1163.15_r8k, 2.0000e-03_r8k, 1173.15_r8k, &
      &       1.500e-03_r8k,  1183.15_r8k, 1.30000e-03_r8k, 1193.15_r8k, 1.1600e-03_r8k, 1203.15_r8k, &
      &       1.130e-03_r8k,  1213.15_r8k, 1.10000e-03_r8k, 1223.15_r8k, 1.1100e-03_r8k, 1233.15_r8k, &
      &       1.130e-03_r8k,  1243.15_r8k, 1.20000e-03_r8k, 1253.15_r8k, 1.3000e-03_r8k, 1263.15_r8k, &
      &       1.400e-03_r8k,  1273.15_r8k /)
    REAL(r8k), DIMENSION(44), PARAMETER :: dthexp = (/ &
      &       4.348e-04_r8k,   373.15_r8k, 5.13950e-03_r8k, 1073.15_r8k, 5.2200e-03_r8k, 1083.15_r8k, &
      &       5.250e-03_r8k,  1093.15_r8k, 5.28000e-03_r8k, 1103.15_r8k, 5.2800e-03_r8k, 1113.15_r8k, &
      &       5.240e-03_r8k,  1123.15_r8k, 5.22000e-03_r8k, 1133.15_r8k, 5.1500e-03_r8k, 1143.15_r8k, &
      &       5.080e-03_r8k,  1153.15_r8k, 4.90000e-03_r8k, 1163.15_r8k, 4.7000e-03_r8k, 1173.15_r8k, &
      &       4.450e-03_r8k,  1183.15_r8k, 4.10000e-03_r8k, 1193.15_r8k, 3.5000e-03_r8k, 1203.15_r8k, &
      &       3.130e-03_r8k,  1213.15_r8k, 2.97000e-03_r8k, 1223.15_r8k, 2.9200e-03_r8k, 1233.15_r8k, &
      &       2.870e-03_r8k,  1243.15_r8k, 2.86000e-03_r8k, 1253.15_r8k, 2.8800e-03_r8k, 1263.15_r8k, &
      &       2.900e-03_r8k,  1273.15_r8k /)
    !
    IF (ctemp <= 1073.15_r8k) THEN
        cathex = -2.5060e-05_r8k + tkc(ctemp) * 4.4410e-06_r8k
        cdthex = -2.3730e-04_r8k + tkc(ctemp) * 6.7210e-06_r8k
    ELSE IF (ctemp >= 1273.15_r8k) THEN
        cathex = -8.300e-03_r8k + tkc(ctemp) * 9.70e-06_r8k
        cdthex = -6.800e-03_r8k + tkc(ctemp) * 9.70e-06_r8k
    ELSE
        cathex = polate(cathxd,ctemp,npcatx,iu)
        cdthex = polate(dthexp,ctemp,npdexp,iu)
    END IF
    !
    END SUBROUTINE cthexp
    !
    !
    !
    REAL(r8k) FUNCTION zoemis (ctmax, zroxid) RESULT (emissv)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> zoemis calculates the emissivity of the cladding surface as a function of maximum cladding temperature and
    !> oxide thickness.
    !>@author
    !> zoemis coded by r. l. miller sept 1974
    !> modified by d. l. hagrman october 1976
    !
    ! Input
    !
    ! ctmax  - Maximum cladding temperature (k)
    ! zroxid - Oxide layer thickness (m)
    !
    ! Output
    !
    ! emissv - Cladding surface emissivity (unitless)
    ! puemis - Positive standard error expected in emissv when compared to in-reactor data (not currently returned)
    ! uuemis - Negative standard error expected in emissv when compared to in-reactor data (not currently returned)
    !
    ! Reference:
    !
    ! (1) aec fuels and materials development program progress report no. 76, usaec report gemp - 1008 (1968). section by
    !     e. f. juenke and s. j. sjodahl, p  239
    ! (2) t. b. burgoyne and a. garlick, paper presented at specialists meeting on the behaviour of water reactor
    !     fuel elements under accident conditions, spatind norway (september 1976)
    ! (3) e. v. murphy and f. havelock, emissivity of zirconium alloys in air in the temperature range 100 - 400 c
    !     j. nuc. mat., 60 (1976) pp 167-176
    !
    REAL(r8k) :: puemis, uuemis
    REAL(r8k), INTENT(IN) :: ctmax, zroxid
    ! Model for temperatures below 1500k follows
    IF (zroxid >= 3.88e-06_r8k) THEN
        emissv = 0.808642_r8k - 50.0_r8k * zroxid
    ELSE
        emissv = 0.325_r8k + 1.246e05_r8k * zroxid
    END IF
    !
    IF (ctmax > 1500.0_r8k) THEN
        ! Modification for maximum temperatures above 1500 k follows
        emissv = emissv * EXP((1500.0_r8k - ctmax) / 300.0_r8k)
        IF (emissv < 0.325_r8k) emissv = 0.325_r8k
        ! Error
        puemis = 0.1_r8k / EXP((1500.0_r8k - ctmax) / 300.0_r8k)
        uuemis = puemis
        ! Standard error cut off at impossible values follows
        IF (puemis > (1.0_r8k - emissv)) puemis = 1.0_r8k - emissv
        IF (uuemis > emissv) uuemis = emissv
    ELSE
        puemis = 0.1_r8k
        uuemis = 0.1_r8k
    END IF
    !
    END FUNCTION zoemis
    !
    !
    !
    REAL(r8k) FUNCTION celmod (ctemp)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> celmod calculates cladding young's modulus as a function of temperature, fast neutron fluence, cold work,
    !> and average oxygen concentration. Grain orientation is assumed random.
    !>@author
    !> celmod was coded by r. l. miller in march 1974.
    !> updated and corrected by b. w. burnham in october 1975
    !> modified by b. w. burnham october 1977
    !> modified by d. l. hagrman december 1977
    !
    ! Input
    !
    ! ctemp  - Cladding temperature (k)
    ! fnck   - Effective fast fluence (neutrons/(m**2))
    ! cwkf   - Effective cold work (unitless ratio of areas)
    ! deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as-received cladding
    !          (kg oxygen/kg zircaloy)
    !
    ! Output
    !
    ! celmod - Young's modulus for zircaloy 2 and 4 with random texture (pa)
    ! ucelmd - Standard error expected in celmod (pa) (not currently returned)
    !
    ! Reference:
    !
    ! The young's modulus calculated by this function is based on data from the following references
    ! (1) l. r. bunnell, g. b. mellinger  and j. l. bates, high temperature properties of zircaloy - oxygen alloys, 
    !     EPRI report np - 524 (1977).
    ! (2) e. s. fisher and c. j. renken, physical review 135 2a (20 july 1964) pp a482 - 494.
    ! (3) p. e. armstrong and h. l. brown, transactions of the  metallurgical society of aime 230 (august 1964) pp 962 - 966
    ! (4) a. padel and a. groff, journal of nuclear materials 59 (1976) pp 325-326
    ! (5) w. r. smalley, saxton core ii fuel performance evaluation. part i: materials, wcap-3385-56 (september 1971)
    !
    ! The expected standard error was derived using additional data from the following references:
    ! (1) c. c. busby and c.r. woods (eds.) "properties of zircaloy-4 tubing", usaec report wapd-tm-585 (december 1966) p 65
    ! (2) z. spasic, m. pavlovic and g. simis, conference on the use of zirconium alloys in nuclear reactors, marianske
    !     lanze, czech. conf-681086 (1968) pp 277 - 284
    ! (3) r. l. mehan, modulus of elasticity of zircaloy-2 between room temperature and 1000 f, kapl-m-rlm-16 (july 1958)
    ! (4) d. o. northwood, i. m london, and l. e. bahen, journal of nuclear materials 55 (1975) pp 299-310
    ! (5) f. r. shober, j. a. van echo, l. l. marsh jr. and j. r. keeler, the mechanical properties of zirconium and zircaloy-2
    !     bmi-1168 (1957)
    !
    REAL(r8k) :: c1, c2, c3, wfox, taab, tabb, amodl, amodr, ucelmd
    REAL(r8k), INTENT(IN) :: ctemp
    CHARACTER(LEN=7) :: CaseType
    !
    CaseType = 'Default'
    IF (cladelmod > 0) CaseType = 'UserVal'
    !
    SELECT CASE (CaseType)
    Case ('UserVal') ! User supplied value for elastic modulus
        celmod = cladelmod
    CASE DEFAULT ! Value based off of cladding type
        ! best estimate model young's modulus
        c1 = (1.16e+11_r8k + ctemp * 1.037e+08_r8k) * 5.7015_r8k
        c2 = 1.0_r8k
        IF (fnck > 1.0e+22_r8k) c2 = 0.88_r8k * (1.0_r8k - EXP(-fnck / 1.0e+25_r8k)) + EXP(-fnck / 1.0e+25_r8k)
        c3 = -2.6e+10_r8k
        celmod = (1.088e+11_r8k - 5.475e+07_r8k * ctemp + c1 * deloxy + c3 * cwkf) / c2
        IF (ctemp >= 1090.0_r8k) THEN
            ! calculate a to a + b and a + b to b boundaries
            wfox = deloxy + 0.0012_r8k
            IF (wfox < 0.025_r8k) THEN
                taab = 1094.0_r8k + wfox * (-1.289e+03_r8k + wfox * 7.914e+05_r8k)
            ELSE
                taab = 1556.4_r8k + 3.8281e+04_r8k * (wfox - 0.025_r8k)
            END IF
            IF (deloxy < 4.7308937e-03_r8k) THEN
                tabb = 392.46_r8k * ((100_r8k * deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
            ELSE
                tabb = (100_r8k * deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
            END IF
            IF (ctemp >= taab) THEN
                IF (ctemp > tabb) THEN
                    celmod = 9.21e+10_r8k - ctemp * 4.05e+07_r8k
                ELSE
                    amodl = (1.088e+11_r8k - 5.475e+07_r8k * taab + c1 * deloxy + c3 * cwkf) / c2
                    amodr = 9.21e+10_r8k - tabb * 4.05e+07_r8k
                    celmod = amodl + (ctemp - taab) * (amodr - amodl) / (tabb - taab)
                END IF
            END IF
        END IF
    END SELECT
    !
    celmod = MAX(celmod, 1.0_r8k)
    ucelmd = 6.4e+09_r8k
    !
    END FUNCTION celmod
    !
    !
    !
    REAL(r8k) FUNCTION cagrow ()
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> cagrow calculates axial cladding growth strain increment as a function of fluence
    !>@author
    !> cagrow coded by D. L. Hagrman, August 1975
    !> revised April, 1995 by D. D. Lanning and K. J. Geelhood to conform to EPRI model (Ref. 1)
    !
    ! Input
    !
    ! flux   - Fast neutron flux ((neutrons/m**2)/s)
    ! fluenc - Fast neutron fluence (neutrons/m**2)
    ! delhs  - Time increment at temperature (s)
    ! icm    - Cladding material index
    !      2 = Zircaloy 2
    !      4 = Zircaloy 4
    !      5 = M5
    !      6 = ZIRLO
    !      7 = Optimized ZIRLO
    ! siggro - Uncertainty in cladding growth
    !
    ! Output
    !
    ! cagrow - Axial cladding growth straini increment (m/m)
    !
    ! Reference:
    !
    ! (1) D. G. Franklin, "Zircaloy-4 Cladding Deformation During Power Reactor Irradiation," ASTM STP 754, 1982, pp.235-267.
    !
    REAL(r8k) :: f1, f2, ax1, ax2
    !
    f2 = Fastfluence(j-1) / 10000.0_r8k
    f1 = (Fastfluence(j-1) - FastFlux(j-1) * delhs) / 10000.0_r8k
    ! Choose Cladding Type
    SELECT CASE (icm)
    CASE (2, 4) ! Zircaloy-2 (2) and ! Zircaloy-4 (4)
        ax1 = 2.18e-21_r8k * f1 ** 0.845_r8k
        ax2 = 2.18e-21_r8k * f2 ** 0.845_r8k
        IF (icm == 2) THEN ! Zircaloy-2 is half of Zircaloy-4
            ax1 = 0.5_r8k * ax1
            ax2 = 0.5_r8k * ax2
        END IF
    CASE (5) ! M5
        ax1 = 7.013e-21_r8k * f1 ** 0.81787_r8k
        ax2 = 7.013e-21_r8k * f2 ** 0.81787_r8k
    CASE (6, 7) ! ZIRLO (6) and Optimized ZIRLO (7)
        ax1 = 9.7893e-25_r8k * f1 ** 0.98239_r8k
        ax2 = 9.7893e-25_r8k * f2 ** 0.98239_r8k
    CASE DEFAULT ! Zircaloy-4
        ax1 = 2.18e-21_r8k * f1 ** 0.845_r8k
        ax2 = 2.18e-21_r8k * f2 ** 0.845_r8k
    END SELECT
    !
    cagrow = ax2 - ax1
    ! Add on uncertainty
    SELECT CASE (icm)
    CASE (2) ! Zircaloy-2
        IF (siggro > 0.0_r8k) cagrow = cagrow * (1.0_r8k + siggro * 0.203_r8k)
        IF (siggro < 0.0_r8k) cagrow = cagrow / (1.0_r8k - siggro * 0.203_r8k)
    CASE (4) ! Zircaloy-4
        IF (siggro > 0.0_r8k) cagrow = cagrow * (1.0_r8k + siggro * 0.223_r8k)
        IF (siggro < 0.0_r8k) cagrow = cagrow / (1.0_r8k - siggro * 0.223_r8k)
    CASE (5) ! M5
        IF (siggro > 0.0_r8k) cagrow = cagrow * (1.0_r8k + siggro * 0.186_r8k)
        IF (siggro < 0.0_r8k) cagrow = cagrow / (1.0_r8k - siggro * 0.186_r8k)
    CASE (6, 7) ! ZIRLO (6) and Optimized ZIRLO (7)
        cagrow = MAX(0.0_r8k, cagrow + siggro * 0.0005_r8k)
    END SELECT
    !
    END FUNCTION cagrow
    !
    !
    !
    REAL(r8k) FUNCTION cshear (ctemp)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> cshear calculates the shear modulus of zircaloy-2 and -4 as a function of temperature, fast neutron fluence,
    !> cold work, and average oxygen concentration.  Grain orientation is assumed random.
    !>@author
    !> cshear was coded by r. l. miller, june 1974
    !> modified by d. l. hagrman december 1977
    !
    ! Input
    !
    ! ctemp  - Cladding temperature (k)
    ! fnck   - Effective fast fluence (neutrons/(m**2))
    ! cwkf   - Effective cold work (unitless ratio of areas)
    ! deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as-received cladding
    !          (kg oxygen/kg zircaloy)
    !
    ! Output
    !
    ! cshear - shear modulus for zircaloy-2 and -4 with random texture (pa)
    ! ucsher - standard error expected in cshear (pa) (not currently returned)
    !
    ! Reference:
    !
    ! (1) l. r. bunnell, g. b. mellinger  and j. l. bates, high temperature properties of zircaloy - oxygen alloys, 
    !     epri report np - 524 (1977)
    ! (2) e. s. fisher and c. j. renken, physical review 135 2a (20 july 1964) pp a482 - 494.
    ! (3) p. e. armstrong and h. l. brown, transactions of the metallurgical society of aime 230 (august 1964) pp 962 - 966
    ! (4) a. padel and a. groff, journal of nuclear materials 59 (1976) pp 325 - 326.
    ! (5) w. r. smalley, saxton core ii fuel performance evaluation. part i: materials, wcap-3385-56 (september 1971)
    !
    REAL(r8k), INTENT(IN) :: ctemp
    REAL(r8k) :: ucsher, c1, c2, c3, wfox, taab, tabb, amodl, amodr
    !
    c1 = 7.07e+11_r8k - ctemp * 2.315e+08_r8k
    c2 = 1.0_r8k
    IF (fnck > 1.0e+22_r8k) c2 = 0.88_r8k * (1.0_r8k - EXP(-fnck / 1.0e+25_r8k)) + EXP(-fnck / 1.0e+25_r8k)
    c3 = -0.867e+10_r8k
    cshear = (4.04e+10_r8k - 2.168e+07_r8k * ctemp + c1 * deloxy + c3 * cwkf) / c2
    IF (ctemp >= 1090.0_r8k) THEN
        ! calculate a to a + b and a + b to b boundaries
        wfox = deloxy + 0.0012_r8k
        IF (wfox < 0.025_r8k) THEN
            taab = 1094.0_r8k + wfox * (-1.289e+03_r8k + wfox * 7.914e+05_r8k)
        ELSE
            taab = 1556.4_r8k + 3.8281e+04_r8k * (wfox - 0.025_r8k)
        END IF
        !
        IF (deloxy < 4.7308937e-03_r8k) THEN
            tabb = 392.46_r8k * ((100.0_r8k * deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
        ELSE
            tabb = (100.0_r8k * deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
        END IF
        IF (ctemp >= taab) THEN
            IF (ctemp > tabb) THEN
                cshear = 3.49e+10_r8k - ctemp * 1.66e+07_r8k
            ELSE
                amodl = (4.04e+10_r8k - 2.168e+07_r8k * taab + c1 * deloxy + c3 * cwkf) / c2
                amodr = 3.49e+10_r8k - tabb * 1.66e+07_r8k
                cshear = amodl + (ctemp - taab) * (amodr - amodl) / (tabb - taab)
            END IF
        END IF
    END IF
    !
    cshear = MAX(cshear, 1.0_r8k)
    ucsher = 9.0E9_r8k
    !
    END FUNCTION cshear
    !
    !
    !
    REAL(r8k) FUNCTION cmhard (ctemp)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    ! the routine cmhard calculates meyer hardness as a function of cladding temperature.
    !>@author
    !> cmhard was coded by v.f.baston in may 1974.
    !> modified by m. a. morgan june 1978
    !
    ! Input
    !
    ! ctemp  - Cladding temperature (k)
    !
    ! Output
    !
    ! cmhard - Meyer hardness of zircaloy cladding (n/m**2)
    !
    ! Reference:
    !
    ! (1) a. m. ross and r. l. stoute, heat transfer coefficient between uo2 and zircaloy - 2, aecl - 1552 (june 1962)
    ! (2) i. d. peggs and d. p. godin, the yield strength - hot hardness relationship of zircaloy - 4, 
    !     Journal of Nuclear Materials 57 pp 246 - 248 (1975)
    !
    REAL(r8k), INTENT(IN) :: ctemp
    !
    cmhard = EXP(26.034_r8k - 2.6394e-02_r8k * ctemp + 4.3502e-05_r8k * ctemp ** 2.0_r8k - 2.5621e-08_r8k * ctemp ** 3.0_r8k)
    !
    END FUNCTION cmhard
    !
END MODULE Zirconium