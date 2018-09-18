MODULE zirconium_fraptran_fraptran
    USE Kinds
    USE conversions_fraptran
    USE functions_fraptran
    USE variables_fraptran, ONLY : CladType
    USE Uncertainty_Vals
    IMPLICIT NONE
    !>@brief
    !> This Module contains all Zirconium Properties
    !>@author
    !> Ian Porter, NRC
    !
    PRIVATE
    PUBLIC :: Zircaloy
    ! Theoretical Density (g/cm3)
    REAL(r8k), PARAMETER :: ZircTD = 6.52_r8k
    ! Melting temperature (K)
    REAL(r8k), PARAMETER :: ZircTmelt = 2098.15_r8k
    !
    TYPE Zircaloy
        ! Temperature
        REAL(r8k) :: Temp
        ! Oxide thickness
        REAL(r8k) :: Oxide
        ! Oxygen concentration
        REAL(r8k) :: deloxy
        ! Fluence
        REAL(r8k) :: fnck
        ! Cold work
        REAL(r8k) :: cwkf
        ! Flux
        REAL(r8k) :: Flux
        ! Melting point
        REAL(r8k) :: Tmelt
        ! Heat of fusion
        REAL(r8k) :: hefus
        ! Theoretical Density
        REAL(r8k) :: TD = ZircTD
        ! Axial thermal expansion
        REAL(r8k) :: athexp
        ! Diametral thermal expansion
        REAL(r8k) :: dthexp
        ! Specific heat
        REAL(r8k) :: SpecHeat
        ! Thermal conductivity
        REAL(r8k) :: Thcon
        ! Derivative of thermal conductivity with respect to temperature
        REAL(r8k) :: dkdt
    CONTAINS
        PROCEDURE :: MatProp
        PROCEDURE, PRIVATE :: ccp
        PROCEDURE, PRIVATE :: celmod
        PROCEDURE, PRIVATE :: cmhard
        PROCEDURE, PRIVATE :: cshear
        PROCEDURE, PRIVATE :: cthcon
        PROCEDURE, PRIVATE :: cthexp
        PROCEDURE, PRIVATE :: Physprop
        PROCEDURE, PRIVATE :: zoemis
    END TYPE Zircaloy
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (Zirc, property) RESULT (mat_prop)
    USE Kinds
    USE conversions_fraptran
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
    CLASS(Zircaloy), INTENT(INOUT) :: Zirc
    REAL(r8k) :: thexp_ax, thexp_rad
    CHARACTER(LEN=*), INTENT(IN) :: property
    
    ! Call material property
    SELECT CASE (property)
    CASE ('THERMCOND')  !Thermal Conductivity
        mat_prop = Zirc%cthcon ()
    CASE ('TDENSITY') ! Theoretical Density, (g/cm^3)
        mat_prop = Zirc%TD
    CASE ('TMELT')  ! Melting Temperature
        CALL Zirc%PhysProp
        mat_prop = Zirc%TMelt
    CASE ('HEAT_FUSION')  ! Melting Temperature
        CALL Zirc%PhysProp
        mat_prop = Zirc%hefus
    CASE ('SPECHEAT')        ! Specific Heat (J/kg*K)
        mat_prop = Zirc%ccp ()
    CASE ('SWELLING')        ! Swelling
        mat_prop = 0.0_r8k
    CASE ('THEXP_AXIAL')     ! Axial Thermal Expansion
        CALL Zirc%cthexp ()
        mat_prop = Zirc%athexp
    CASE ('THEXP_RADIAL')    ! Radial Thermal Expansion
        CALL Zirc%cthexp ()
        mat_prop = Zirc%dthexp
    CASE ('EMISS')           ! Emissivity
        mat_prop = Zirc%zoemis ()
    CASE ('DENSIFICATION')   ! Densification
        mat_prop = 0.0_r8k
    CASE ('ENTHALPY')        ! Enthalpy
        mat_prop = 0.0_r8k
    CASE ('RELOCATION')      ! Relocation
        mat_prop = 0.0_r8k
    CASE ('POIS_RATIO')      ! Poisson's Ratio
        mat_prop = 0.0_r8k
    CASE ('YOUNG_MOD')       ! Young's Modulus
        mat_prop = Zirc%celmod ()
    CASE ('YIELD_STRESS')    ! Yield stress
        mat_prop = 0.0_r8k
    CASE ('THEXP_COEF')      ! Thermal expansion coefficient
        mat_prop = 0.0_r8k
    CASE ('CREEP')           ! Creep
        mat_prop = 0.0_r8k
!    CASE ('AXIAL_GROW')      ! Axial Growth
!        mat_prop = cagrow ()
    CASE ('SHEAR_MOD')       ! Shear Modulus
        mat_prop = Zirc%cshear ()
    CASE ('MEYER_HARD')      ! Meyer's Hardness
        mat_prop = Zirc%cmhard ()
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module zirconium_fraptran_fraptran. Invalid material property ID. Material Property ID = ',a20)
        ERROR STOP 'Error in Module zirconium_fraptran_fraptran. Invalid material property ID called.'
    END SELECT
    !
    END FUNCTION MatProp
    !
    !
    !
    REAL(r8k) FUNCTION ccp (Zirc)
    USE variables_fraptran, ONLY : CladType
    USE Kinds
    IMPLICIT NONE
    !>@brief 
    !> ccp calculates the specific heat at constant pressure for zircaloys.
    !>@author
    !> ccp coded by r. l. miller october 1974
    !> modified by d. l. hagrman may 1976
    !> Modified by PNNL, January 1997, to clean up coding, including
    !> removed licensing assistance coding and sensitivity uncertainty coding
    !
    ! Input
    !
    ! Zirc%temp - Cladding temperature (k)
    ! CladType  - Cladding type
    !
    ! Output
    !
    ! ccp       - Cladding specific heat at constant pressure (j/kg-k)
    !
    ! Reference:
    ! (1) c. r. brooks and e. e. stansbury, "the specific heat of zircaloy-2 from 50 to 700 C",
    !     journal of nuclear materials 18 (1966) p 233
    ! (2) e. a. eldridge and h. w. deem, specific heats and heats of transformation of zircaloy-2
    !     and low nickel zircaloy-2 bmi-1803 (may 31, 1967)
    !
    ! Note:
    ! Conversion from j/(kg*k) to cal/(gm*c) is 2.39006e-4_r8k (cal/(gm*c))/(j/(kg*k))
    ! 
    ! estimated standard error of the prediction of ccp for the specific heat of zircaloy cladding
    ! samples -- (sum of squared residuals/(number of residuals-degrees of freedom))**0.5 -- is
    ! (1) for temperature less than 1090 k, 10 J/kg-k)
    ! (2) for temperature 1090 k to 1300 k, 25 J/kg-k)
    ! (3) for temperature above 1300 k,    100 J/kg-k)
    !
    CLASS (Zircaloy) :: Zirc
    INTEGER(ipk) :: iwwer
    INTEGER(ipk), PARAMETER :: npcp = 14
    INTEGER(ipk), PARAMETER :: npww = 12
    INTEGER(ipk), PARAMETER :: npw2 = 13
    ! Zry properties
    REAL(r8k), DIMENSION(28), PARAMETER :: cpData = [ &
      &    279.0_r8k, 290.0_r8k, 281.0_r8k, 300.0_r8k, 302.0_r8k, 400.0_r8k, 331.0_r8k, 640.0_r8k, &
      &    375.0_r8k, 1090.0_r8k, 502.0_r8k, 1093.0_r8k, 590.0_r8k, 1113.0_r8k, 615.0_r8k, 1133.0_r8k, &
      &    719.0_r8k, 1153.0_r8k, 816.0_r8k, 1173.0_r8k, 770.0_r8k, 1193.0_r8k, 619.0_r8k, 1213.0_r8k, &
      &    469.0_r8k, 1233.0_r8k, 356.0_r8k, 1248.0_r8k ]
    ! Zr-1%Nb properties
    REAL(r8k), DIMENSION(24), PARAMETER :: cpwwer = [ & ! Low heat rate (<1000K/s)
      &   345.0_r8k, 280.0_r8k, 360.0_r8k, 473.0_r8k, 370.0_r8k, 573.0_r8k, 380.0_r8k, 673.0_r8k, &
      &   383.0_r8k, 773.0_r8k, 385.0_r8k, 873.0_r8k, 448.0_r8k, 883.0_r8k, 680.0_r8k, 973.0_r8k, &
      &   816.0_r8k, 1025.0_r8k, 770.0_r8k, 1073.0_r8k, 400.0_r8k, 1153.0_r8k, 392.0_r8k, 1173.0_r8k ]
    REAL(r8k), DIMENSION(26), PARAMETER :: cpwwr2 = [ & ! High heat rate (>1000K/s)
      &    412.6_r8k, 1100.0_r8k, 420.0_r8k, 1110.0_r8k, 480.0_r8k, 1120.0_r8k, 600.0_r8k, 1134.0_r8k, &
      &    1000.0_r8k, 1142.0_r8k, 1400.0_r8k, 1150.0_r8k, 1600.0_r8k, 1155.0_r8k, 1400.0_r8k, 1161.0_r8k, &
      &    1000.0_r8k, 1168.0_r8k, 600.0_r8k, 1177.0_r8k, 400.0_r8k, 1180.0_r8k, 360.0_r8k, 1190.0_r8k, &
      &    348.0_r8k, 1200.0_r8k ]
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! RRC-KI Zr-1%Nb property
        ! This block calculates the specific heat at constant pressure for
        ! H1-alloy cladding. If iwwer = 1 Then Data is derived from
        ! Volkov B.Yu. et. al."Material property library for H1-alloy
        ! cladding", Preprint IAE-4941/11, 1989. This property are induced
        ! by low heating rate. If iwwer = 2 Then Data is derived from
        ! Ljusternik V.E. et. al."Experimental research of zirconium reactor
        ! materials thermal properties:110-alloy". M.,J. High Temperature
        ! Thermal Physics." v.31 n.4, 1993. This property are induced by high-rate heating>=1000 K/s.
        !   5/5/03: default to iwwer=1 until can set this flag up
        iwwer = 1
        SELECT CASE (iwwer)
        CASE (1) ! Low heat rate
            IF (Zirc%Temp < 1173.0_r8k) THEN
                ccp = polate(cpwwer, Zirc%Temp, npww)
            ELSE
                ccp = 392.0_r8k
            ENDIF
        CASE (2) ! High heat rate
            ccp = 237.5_r8k + 15.91e-2_r8k * Zirc%Temp
            IF (Zirc%Temp >= 1100.0_r8k .AND. Zirc%Temp <= 1200.0_r8k) ccp = polate(cpwwr2, Zirc%Temp, npw2)
            IF (Zirc%Temp > 1200.0_r8k) ccp = 199.7_r8k + 12.364e-2_r8k * Zirc%Temp
        END SELECT
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry
        IF (Zirc%Temp >= 1248.0_r8k) THEN
            ccp = 356.0_r8k
        ELSE IF (Zirc%Temp >= 290.0_r8k .AND. Zirc%Temp <= 1248.0_r8k) THEN
            ccp = polate (cpData, Zirc%Temp, npcp)
        ELSE
            ccp = 279.0_r8k
        ENDIF
    END SELECT
    
    END FUNCTION ccp
    !
    !
    !
    REAL(r8k) PURE FUNCTION celmod (Zirc)
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran, ONLY : CladType
    IMPLICIT NONE
    !>@brief
    !> celmod calculates cladding young's modulus as a function of temperature, fast neutron fluence, cold work,
    !> and average oxygen concentration. Grain orientation is assumed random.
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT
    !>@author
    !> celmod was coded by r. l. miller in march 1974.
    !> updated and corrected by b. w. burnham in october 1975
    !> modified by b. w. burnham october 1977
    !> modified by d. l. hagrman december 1977
    !> Modified by PNNL, January 1997, to clean up coding, delete licensing analysis and sensitivity analysis coding
    !
    ! Input
    !
    ! CladType    - cladding type
    ! Zirc%Temp   - Cladding temperature (k)
    ! Zirc%fnck   - Effective fast fluence (neutrons/(m**2))
    ! Zirc%cwkf   - Effective cold work (unitless ratio of areas)
    ! Zirc%deloxy - Average oxygen concentration excluding oxide layer - average oxygen concentration of as-received cladding
    !              (kg oxygen/kg zircaloy)
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
    CLASS (Zircaloy), INTENT(IN) :: Zirc
    REAL(r8k) :: c1, c2, c3, wfox, taab, tabb, amodl, amodr, ucelmd
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! Zr-1%Nb model from RRC-KI
        ! This code block calculates cladding Young's modulus as a function of temperature ; Data is derived from
        ! Volkov B.Yu. et.al."Material property library for H1-alloy cladding", Preprint IAE-4941/11, 1989.
        ! Fedorov et.al."Influence of oxygen content and gamma-irradiation on the elastic module and internal
        ! friction pure zirconium" Metal Physics vol.32(3) p.621, 1971
        IF (Zirc%Temp > 1073.0_r8k) THEN
            celmod = 9.129e10_r8k - 4.5e7_r8k * Zirc%Temp
        ELSE
            celmod = 1.121e11_r8k - 6.438e07_r8k * Zirc%Temp
            celmod = celmod + 3.021e12_r8k * Zirc%deloxy
        ENDIF
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry model. Best estimate model young's modulus
        c1 = (1.16e11_r8k + Zirc%Temp * 1.037e08_r8k) * 5.7015_r8k
        c2 = 1.0_r8k
        IF (Zirc%fnck > 1.0e22_r8k) c2 = 0.88_r8k * (1.0_r8k - EXP(-Zirc%fnck / 1.0e25_r8k)) + EXP(-Zirc%fnck/1.0e25_r8k)
        c3 = -2.6e10_r8k
        celmod = (1.088e11_r8k - 5.475e7_r8k * Zirc%Temp + c1 * Zirc%deloxy + c3 * Zirc%cwkf) / c2
        IF (Zirc%Temp >= 1090.0_r8k) THEN
            ! calculate a to a + b and a + b to b boundaries
            wfox = Zirc%deloxy + 0.0012_r8k
            taab = 1094.0_r8k + wfox * (-1.289e3_r8k + wfox * 7.914e5_r8k)
            IF (wfox >= 0.025_r8k) taab = 1556.4_r8k + 3.8281e4_r8k * (wfox - 0.025_r8k)
            tabb = 392.46_r8k * ((100 * Zirc%deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
            IF (Zirc%deloxy >= 4.7308937e-03_r8k) tabb = (100 * Zirc%deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
            !
            IF (Zirc%Temp >= taab) THEN
                IF (Zirc%Temp > tabb) THEN
                    celmod = 9.21e10_r8k - Zirc%Temp * 4.05e7_r8k
                ELSE
                    amodl = (1.088e11_r8k - 5.475e7_r8k * taab + c1 * Zirc%deloxy + c3 * Zirc%cwkf) /c2
                    amodr = 9.21e10_r8k - tabb * 4.05e7_r8k
                    celmod = amodl + (Zirc%Temp - taab) * (amodr - amodl) / (tabb - taab)
                ENDIF
            ENDIF
        ENDIF
    END SELECT
    !
    celmod = MAX(celmod, 1.0_r8k)
    ucelmd = 6.4e+09_r8k
    !
    END FUNCTION celmod
    !
    !
    !
    REAL(r8k) PURE FUNCTION cmhard (Zirc)
    USE Kinds
    USE variables_fraptran, ONLY : CladType
    IMPLICIT NONE
    !>@brief
    !> The routine cmhard calculates meyer hardness as a function of cladding temperature.
    !>@author
    !> cmhard was coded by v.f.baston in may 1974.
    !> modified by m. a. morgan june 1978
    !> Modified by PNNL, January 1997, to clean up coding, delete sensitivity uncertainty coding
    !
    ! Input
    !
    ! Zirc%Temp - Cladding temperature (k)
    ! CladType  - Cladding type
    !
    ! Output
    !
    ! cmhard    - Meyer hardness of zircaloy cladding (n/m**2)
    !
    ! Reference:
    !
    ! (1) a. m. ross and r. l. stoute, heat transfer coefficient between uo2 and zircaloy - 2, aecl - 1552 (june 1962)
    ! (2) i. d. peggs and d. p. godin, the yield strength - hot hardness relationship of zircaloy - 4, journal of nuclear
    !     materials 57 pp 246 - 248 (1975)
    !
    CLASS (Zircaloy), INTENT(IN) :: Zirc
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! Zr-1%Nb from RRC-KI
        IF (Zirc%Temp < 800.0_r8k) THEN
            cmhard = 1.0e6_r8k * (2172.1_r8k - 10.7055_r8k * Zirc%Temp + 0.02765_r8k * Zirc%Temp ** 2 - &
              &      3.278e-5_r8k * Zirc%Temp ** 3 + 1.423e-8_r8k * Zirc%Temp ** 4)
        ELSE
            cmhard = EXP(26.034_r8k - 2.6394e-2_r8k * Zirc%Temp + 4.3502e-5_r8k * Zirc%Temp ** 2 - &
              &      2.5621e-8_r8k * Zirc%Temp ** 3)
        ENDIF
        cmhard = MAX(1.0E5_r8k, cmhard)
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry property.
        cmhard = EXP(26.034_r8k - 2.6394e-02_r8k * Zirc%Temp + 4.3502e-05_r8k * Zirc%Temp ** 2 - &
          &          2.5621e-08_r8k * Zirc%Temp ** 3)
        cmhard = MAX(1.94E8_r8k, cmhard)
    END SELECT
    !
    END FUNCTION cmhard
    !
    !
    !
    REAL(r8k) PURE FUNCTION cshear (Zirc)
    USE Kinds
    USE variables_fraptran, ONLY : CladType
    IMPLICIT NONE
    !>@brief
    !> cshear calculates the shear modulus of zircaloy-2 and -4 as a function of temperature, fast neutron fluence,
    !> cold work, and average oxygen concentration.  grain orientation is assumed random.
    !> This is a MATPRO-11, Rev. 1 routine modified by PNNL for use in FRAPT
    !>@author
    !> cshear was coded by r. l. miller, june 1974
    !> modified by d. l. hagrman december 1977
    !> Modified by PNNL, January 1997, to clean up coding and delete sensitivity uncertainty analysis coding
    !
    ! Input
    !
    ! Zirc%Temp   - cladding temperature (k)
    ! Zirc%fnck   - effective fast fluence (neutrons/(m**2))
    ! Zirc%cwkf   - effective cold work (unitless ratio of areas)
    ! Zirc%deloxy - average oxygen concentration excluding oxide layer - average oxygen concentration of as-received cladding
    !              (kg oxygen/kg zircaloy)
    ! CladType    - Cladding Type
    !
    ! Output
    !
    ! cshear   - Shear modulus for zircaloy-2 and -4 with random texture (Pa)
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
    CLASS (Zircaloy), INTENT(IN) :: Zirc
    REAL(r8k) :: c1, c2, c3, wfox, taab, tabb, amodl, amodr, ey, xnu
    !
    SELECT CASE (CladType)
    CASE (6, 8) !  Zr-1%Nb from RRC-KI
        ! This code block calculates cladding young's modulus and  poisson's ratio as a function of temperature.
        ! Data is derived from Volkov B.Yu. et.al."Material property library for H1-alloy cladding", Preprint IAE-4941/11, 1989.
        ! Elastic modulus
        ey = Zirc%celmod()
        ! Poisson's ratio:
        xnu = 0.42628_r8k - 5.556e-5_r8k * Zirc%Temp
        ! Shear modulus
        cshear = 0.5_r8k * ey / (1.0_r8k + xnu)
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry generic Zry properties
        c1 = 7.07e11_r8k - Zirc%Temp * 2.315e8_r8k
        IF (Zirc%fnck > 1.0e22_r8k) THEN
            c2 = 0.88_r8k * (1.0_r8k - EXP(-Zirc%fnck / 1.0e25_r8k)) + EXP(-Zirc%fnck / 1.0e25_r8k)
        ELSE
            c2 = 1.0_r8k
        ENDIF
        c3 = -0.867e+10_r8k
        cshear = (4.04e10_r8k - 2.168e7_r8k * Zirc%Temp + c1 * Zirc%deloxy + c3 * Zirc%cwkf) / c2
        IF (Zirc%Temp >= 1090.0_r8k) THEN
            ! calculate a to a + b and a + b to b boundaries
            wfox = Zirc%deloxy + 0.0012_r8k
            IF (wfox < 0.025_r8k) THEN
                taab = 1094.0_r8k + wfox * (-1.289e3_r8k + wfox * 7.914e5_r8k)
            ELSE
                taab = 1556.4_r8k + 3.8281e4_r8k * (wfox - 0.025_r8k)
            ENDIF
            IF (Zirc%deloxy < 4.7308937e-03_r8k) THEN
                tabb = 392.46_r8k * ((100.0_r8k * Zirc%deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
            ELSE
                tabb = (100.0_r8k * Zirc%deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
            ENDIF
            IF (Zirc%Temp >= taab) THEN
                IF (Zirc%Temp > tabb) THEN
                    cshear = 3.49e10_r8k - Zirc%Temp * 1.66e7_r8k
                ELSE
                    amodl = (4.04e10_r8k - 2.168e7_r8k * taab  + c1 * Zirc%deloxy + c3 * Zirc%cwkf) / c2
                    amodr = 3.49e10_r8k - tabb * 1.66e7_r8k
                    cshear = amodl + (Zirc%Temp - taab ) * (amodr - amodl ) / (tabb - taab)
                ENDIF
            ENDIF
        ENDIF
    END SELECT
    !
    cshear = MAX(1.0_r8k, cshear)
    !
    END FUNCTION cshear
    !
    !
    !
    REAL(r8k) FUNCTION cthcon (Zirc)
    USE Kinds
    USE variables_fraptran, ONLY : CladType
    USE Uncertainty_Vals
    IMPLICIT NONE
    !>@brief
    !> cthcon calculates cladding thermal conductivity as a function of temperature, time, flux, and cold work
    !> This is a MATPRO-11, Rev. 2 routine modified by PNNL for use in FRAPT
    !>@author
    !> This version of cthcon was developed by r.l. miller, dec 1975
    !> last modified by e.r. carlson in june 1978.
    !> Modified by PNNL, January 1997, to clean up coding and delete licening analysis and sensitivity uncertainty coding
    !
    ! Input
    !
    ! Zirc%Temp - Cladding meshpoint temperature (k)
    !
    ! Output
    !
    ! cthcon      - Thermal conductivity of zircaloy-4 (w/m-k)
    ! cdkdt     - Derivative of thermal conductivity with respect to temperature
    !
    ! Reference:
    !
    ! Zr-1%Nb:
    ! (1) Volkov B.Yu. et. al."Material property library for H1-alloy cladding", Preprint IAE-4941/11, 1989.
    ! Generic Zircaloy:
    ! (2) w.k. anderson, c.j. beck, a.r. kephart and j.s. theilacker astm-stp-314, 1962, pp 62 - 93
    ! (3) j.n. chirigos, c. kass, w.s. kirk and g.j. salvaggio fuel element fabrication, academic press, 1961, pp 19 - 55
    ! (4) a.d. feith, gemp-669 (oct 1966)
    ! (5) c.f. lucks and h.w. deem, bmi-1273 (1958) pp 7-9
    ! (6) a.e. powers, kapl-2146 (1961)
    ! (7) d.b. scott, wcap-3269-41 (1965) pp 5-9
    !
    CLASS (Zircaloy), INTENT(INOUT) :: Zirc
    REAL(r8k) :: cdkdt
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! E-110 property as coded by RRC-KI and provided to PNNL
        IF (Zirc%Temp <= 2133.0_r8k) THEN
            cthcon = EXP(0.000461843_r8k * Zirc%Temp) * 15.0636_r8k
            cdkdt = 0.006957018215_r8k * EXP(0.000461843_r8k * Zirc%Temp)
        ELSE
            cthcon = 36.0_r8k
            cdkdt = 0.0_r8k
        ENDIF
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zircaloy property from MATPRO
        ! This version of cthcon does not use time, flux or coldwork as parameters in calculation of zircaloy thermal conductivity
        ! one standard deviation of this function = 1.01 w/m-k
        cthcon  = 7.511_r8k + Zirc%Temp * (2.088e-2_r8k + Zirc%Temp * (-1.450e-5_r8k + Zirc%Temp * 7.668e-09_r8k))
        cdkdt = 2.088e-2_r8k + Zirc%Temp * (-2.9e-5_r8k + Zirc%Temp * 2.3e-8_r8k)
    END SELECT
    ! Multiply by Uncertainty coefficient
    cthcon = cthcon * sigcladthermcond
    cdkdt = cdkdt * sigcladthermcond
    Zirc%Thcon = cthcon
    Zirc%dkdt = cdkdt
    !
    END FUNCTION cthcon
    !
    !
    !
    SUBROUTINE cthexp (Zirc)
    USE Kinds
    USE variables_fraptran, ONLY : CladType
    USE Uncertainty_Vals
    IMPLICIT NONE
    !>@brief
    !> cthexp calculates axial and diametral thermal expansion of  zircaloy lwr cladding.
    !>@author
    !> cthexp was originally coded by r.l. miller in nov. 1974.
    !> last updated by g.a. reymann in june 1978.
    !> Licensing analysis coding deleted 9/23/97 by ME Cunningham, PNNL
    !
    ! Input
    !
    ! CladType  - Cladding type
    ! Zirc%Temp - Cladding meshpoint temperature (K)
    !
    ! Output
    !
    ! cathex   = Axial thermal expansion of zircaloy (m/m)
    ! cdthex   = Diametral thermal expansion of zircaloy (m/m)
    !
    ! Reference:
    ! Room temperature to 1273K:
    ! (1) r.l. mehan and f.w. wiesinger, "mechanical properties of zircaloy-2", usaec report kapl-2110 (1961)
    ! (2) d.b. scott, "physical and mechanical properties of zircaloy-2 and -4", wcap-3269-41 (1965) 
    ! (3) j.j. kearns, "thermal expansion and preferred orientation in zircaloy", wapd-tm-472 (1965)
    !
    ! Above 1273k the coefficient of thermal expansion used is the constant value 9.7e-06/k recommended by 
    ! (1) b. lustman and f. kerze, "the metallurgy of zirconium", mc graw-hill book company, new york (1955) page 355.
    !
    ! Between 1073 and 1273k (approximately the alpha-beta transition range for zircaloy) cthexp uses
    ! linear interpolation routine polate to find the thermal expansion.
    !
    CLASS (Zircaloy), INTENT(INOUT) :: Zirc
    INTEGER(ipk) :: iu = 1
    INTEGER(ipk), PARAMETER :: npcatx = 22
    INTEGER(ipk), PARAMETER :: npdexp = 22
    REAL(r8k) :: cathex, cdthex
    REAL(r8k), DIMENSION(44), PARAMETER :: cathxd = [ 0.0_r8k, 293.15_r8k, 3.52774e-03_r8k, 1073.15_r8k, &
      & 3.5300e-03_r8k, 1083.15_r8k, 3.5000e-03_r8k, 1093.15_r8k, 3.460e-03_r8k, 1103.15_r8k, &
      & 3.4100e-03_r8k, 1113.15_r8k, 3.3300e-03_r8k, 1123.15_r8k, 3.210e-03_r8k, 1133.15_r8k, &
      & 3.0700e-03_r8k, 1143.15_r8k, 2.8000e-03_r8k, 1153.15_r8k, 2.500e-03_r8k, 1163.15_r8k, &
      & 2.0000e-03_r8k, 1173.15_r8k, 1.5000e-03_r8k, 1183.15_r8k, 1.300e-03_r8k, 1193.15_r8k, &
      & 1.1600e-03_r8k, 1203.15_r8k, 1.1300e-03_r8k, 1213.15_r8k, 1.100e-03_r8k, 1223.15_r8k, &
      & 1.1100e-03_r8k, 1233.15_r8k, 1.1300e-03_r8k, 1243.15_r8k, 1.200e-03_r8k, 1253.15_r8k, &
      & 1.3000e-03_r8k, 1263.15_r8k, 1.4000e-03_r8k, 1273.15_r8k ]
    REAL(r8k), DIMENSION(44), PARAMETER :: dthexp = [ 4.3480e-04_r8k, 373.15_r8k, &
      & 5.1395e-03_r8k, 1073.15_r8k, 5.2200e-03_r8k, 1083.15_r8k, 5.2500e-03_r8k, 1093.15_r8k, &
      & 5.2800e-03_r8k, 1103.15_r8k, 5.2800e-03_r8k, 1113.15_r8k, 5.2400e-03_r8k, 1123.15_r8k, &
      & 5.2200e-03_r8k, 1133.15_r8k, 5.1500e-03_r8k, 1143.15_r8k, 5.0800e-03_r8k, 1153.15_r8k, &
      & 4.9000e-03_r8k, 1163.15_r8k, 4.7000e-03_r8k, 1173.15_r8k, 4.4500e-03_r8k, 1183.15_r8k, &
      & 4.1000e-03_r8k, 1193.15_r8k, 3.5000e-03_r8k, 1203.15_r8k, 3.1300e-03_r8k, 1213.15_r8k, &
      & 2.9700e-03_r8k, 1223.15_r8k, 2.9200e-03_r8k, 1233.15_r8k, 2.8700e-03_r8k, 1243.15_r8k, &
      & 2.8600e-03_r8k, 1253.15_r8k, 2.8800e-03_r8k, 1263.15_r8k, 2.9000e-03_r8k, 1273.15_r8k ]
    !
    SELECT CASE (CladType)
    CASE (6, 8) !  RRC-KI thermal expansion
        IF (Zirc%Temp >= 2133.0_r8k) THEN
           cathex = 1.0582459e-2_r8k
           cdthex = 1.3133600e-2_r8k
        ELSE If (Zirc%Temp  > 1153.0_r8k) THEN
           cathex = 1.076459e-3_r8k + 9.7e-6_r8k * (Zirc%Temp - 1153.0_r8k)
           cdthex = 3.627600e-3_r8k + 9.7e-6_r8k * (Zirc%Temp - 1153.0_r8k)
        ELSE If (Zirc%Temp > 883.0_r8k) THEN
           cathex = 3.0465577e-3_r8k + 2.312e-8_r8k*(Zirc%Temp-883.0_r8k) - 7.358e-8_r8k * (Zirc%Temp - 883.0_r8k) ** 2 &
             &      + 1.7211e-10_r8k * (Zirc%Temp - 883.0_r8k) ** 3
           cdthex = 5.5977000e-3_r8k + 2.312e-8_r8k*(Zirc%Temp-883.0_r8k) - 7.358e-8_r8k * (Zirc%Temp - 883.0_r8k) ** 2 &
             &      + 1.7211e-10_r8k * (Zirc%Temp - 883.0_r8k) ** 3
        ELSE If (Zirc%Temp > 573.0_r8k) THEN
           cathex = 0.13725577e-2_r8k + 5.4e-6_r8k * (Zirc%Temp - 573.0_r8k)
           cdthex = 0.3336985e-8_r8k * Zirc%Temp ** 2 + 5.65390e-6_r8k * Zirc%Temp - 0.199649865e-2_r8k
        ELSE
           cathex = 0.1338985e-8_r8k * Zirc%Temp ** 2 + 3.85875e-6_r8k * Zirc%Temp - 0.127813365e-2_r8k
           cdthex = 0.3336985e-8_r8k * Zirc%Temp ** 2 + 5.65390e-6_r8k * Zirc%Temp - 0.199649865e-2_r8k
        ENDIF
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zircaloy thermal expansion
        IF (Zirc%Temp <= 1073.15_r8k) THEN
            cathex = -2.5060e-05_r8k + (Zirc%Temp - 273.15_r8k) * 4.4410e-06_r8k
            cdthex = -2.3730e-04_r8k + (Zirc%Temp - 273.15_r8k) * 6.7210e-06_r8k
        ENDIF
        IF (Zirc%Temp >= 1273.15_r8k) THEN
            cathex = -8.300e-03_r8k + (Zirc%Temp - 273.15_r8k) * 9.70e-06_r8k
            cdthex = -6.800e-03_r8k + (Zirc%Temp - 273.15_r8k) * 9.70e-06_r8k
        ENDIF
        IF (Zirc%Temp > 1073.15_r8k .AND. Zirc%Temp < 1273.15_r8k) THEN
            cathex = polate(cathxd,Zirc%Temp,npcatx,iu)
            cdthex = polate(dthexp,Zirc%Temp,npdexp,iu)
        ENDIF
    END SELECT
    ! Multiply by Uncertainty coefficient
    Zirc%athexp = cathex * sigcladthermexp
    Zirc%dthexp = cdthex * sigcladthermexp
    !
    END SUBROUTINE cthexp
    !
    !
    !
    SUBROUTINE PhysProp (Zirc)
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran, ONLY : CladType
    IMPLICIT NONE
    !>@brief
    !> Physical Properties of Zirconium based alloys. (Tmelt, TD, density)
    !> Note that this subroutine does not return any parameters directly but rather stores into the Module variables_fraptran
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> January 6, 2016
    !
    ! Input
    !
    ! CladType    - Cladding type
    !
    ! Output
    !
    ! Zirc%chefus - Heat of fusion
    ! Zirc%Tmelt  - Melting temperature
    ! Zirc%ctranb - Start of alpha-beta transition temperature (Not used)
    ! Zirc%ctrane - End of alpha-beta transition temperature (Not used)
    !
    CLASS (Zircaloy), INTENT(INOUT) :: Zirc
    REAL(r8k) :: wfox
    !
    SELECT CASE (CladType)
    CASE (6, 8) ! Zr-1%Nb properties from RRC-KI
      Zirc%TMelt = 2133.0_r8k !ctmelt
      Zirc%hefus = 21.0e4_r8k !chefus
    CASE (1, 2, 3, 4, 5, 7, 9, 10, 11) ! Generic Zry Properties
      Zirc%TMelt = 2098.15_r8k !ctmelt
      Zirc%hefus = 22.5e4_r8k !chefus
    END SELECT
    !
    ! Alpha - Beta Transition Temperature
    !
    ! 5/7/03: ctranb, ctrane, ctranz phase transition temperatures not currently used in FRAPTRAN; 
    ! RRC-KI does have different transition temperatures for Zr-1%Nb
    ! Start of alpha-beta transition temperature
    !
    !wfox = Zirc%deloxy + 0.0012_r8k
    !IF (wfox > 0.025_r8k) THEN
    !    Zirc%ctranb = 1556.4_r8k + 3.8281e4_r8k * (wfox - 0.025_r8k)
    !ELSE
    !    Zirc%ctranb = 1094.0_r8k + wfox * (-1.289e3_r8k + wfox * 7.914E5_r8k)
    !ENDIF
    ! End of alpha-beta transition temperature
    !IF (Zirc%deloxy >= 4.7308937e-03_r8k) THEN
    !    Zirc%ctrane = (100.0_r8k * Zirc%deloxy + 0.12_r8k) * 491.157_r8k + 1081.7413_r8k
    !ELSE
    !    Zirc%ctrane = 392.46_r8k * ((100.0_r8k * Zirc%deloxy + 0.1242807_r8k) ** 2 + 3.1417_r8k)
    !ENDIF
    ! Isothermal alpha-beta transition temperature
    !Zirc%ctranz = 1135.15_r8k
    !
    END SUBROUTINE PhysProp
    !
    !
    !
    REAL(r8k) PURE FUNCTION zoemis (Zirc) RESULT (emissv)
    USE Kinds
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> zoemis calculates the emissivity of the cladding surface as a function of maximum cladding temperature and
    !> oxide thickness.
    !>@author
    !> zoemis coded by r. l. miller sept 1974
    !> modified by d. l. hagrman october 1976
    !> Modified by PNNL, January 1997, to clean up coding, delete sensitivity uncertainty coding, 
    !> and correct error in puemis for T > 1500K
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
    CLASS (Zircaloy), INTENT(IN) :: Zirc
    REAL(r8k) :: puemis, uuemis
    ! Model for temperatures below 1500k follows
    IF (Zirc%Oxide >= 3.88e-06_r8k) THEN
        emissv = 0.808642_r8k - 50.0_r8k * Zirc%Oxide
    ELSE
        emissv = 0.325_r8k + 1.246e05_r8k * Zirc%Oxide
    END IF
    !
    IF (Zirc%Temp > 1500.0_r8k) THEN
        ! Modification for maximum temperatures above 1500 k follows
        emissv = emissv * EXP((1500.0_r8k - Zirc%Temp) / 300.0_r8k)
        IF (emissv < 0.325_r8k) emissv = 0.325_r8k
        ! Error
        puemis = 0.1_r8k / EXP((1500.0_r8k - Zirc%Temp) / 300.0_r8k)
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
END MODULE zirconium_fraptran_fraptran












