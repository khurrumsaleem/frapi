MODULE Conversions
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This file contains the subroutines used to perform unit conversions and holds the conversion factors used in the code.
    !> These conversions are identical to those used in FRAPCON-4.0.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/13/2016
    !
    ! ** Constants **
    !
    ! Avogadro's Number
    REAL(r8K), PARAMETER :: Avogadro = 6.0221413E23_r8k              ! (molecules / mol)
    ! Boltzmann's Constant
    REAL(r8k), PARAMETER :: Boltzmann = 1.3806505E-23_r8k            ! (J / K)
    ! Gravitational Acceleration
    REAL(r8K), PARAMETER :: Gravity_SI = 9.80665_r8k                 ! (m / s)
    ! Ideal Gas Constant
    REAL(r8K), PARAMETER :: R_JmolK = 8.3144621_r8k                  ! (J / mol - K)
    ! Ideal Gas Constant
    REAL(r8k), PARAMETER :: R_in3psiRlbmol = 10.73159_r8k            ! (in^3*psi)/(Rankine*lb-mol)
    ! Pi
    REAL(r8k), PARAMETER :: pi = 3.1415926535897932_r8k              ! Value for pi
    ! Speed of Light
    REAL(r8K), PARAMETER :: speedlight = 2.99792458E8_r8k            ! (m / s)
    

    REAL(r8k), PARAMETER :: ftin = 12.0_r8k              ! ( inch / ft )
    REAL(r8k), PARAMETER :: ftom = 0.3048_r8k            ! ( ft / meter )
    REAL(r8k), PARAMETER :: ftmetr = 3.28084_r8k         ! ( m / ft )
    REAL(r8k), PARAMETER :: sechr = 3600.0_r8k           ! ( second / hour )
    REAL(r8k), PARAMETER :: btuhkw = 3412.874_r8k        ! ( btu/hour / kilowatt )
    REAL(r8k), PARAMETER :: powcnv = 0.9478_r8k          ! ( Btu/second / kilowatt) !.94781712
    REAL(r8k), PARAMETER :: psinm2 = 1.4503774e-4_r8k    ! ( psi / n/m2 )
    REAL(r8k), PARAMETER :: psift2 = 144.0_r8k           ! ( psi / lb/ft^2 )
    REAL(r8k), PARAMETER :: gtolb = 453.6_r8k            ! ( grams / lb )
    REAL(r8k), PARAMETER :: hgpcnv = 5.67826_r8k         ! Convert gap HTC (Btu/hr*ft^2*F to W/m^2*K)
    !
    !
    ! ** Energy **
    !
    ! Convert BTU/lb to J/kg
    REAL(r8k), PARAMETER :: BTUlbtoJkg = 2326.000292_r8k             ! (J/kg / BTU/lb)
    ! Convert J/kg to BTU/lb
    REAL(r8k), PARAMETER :: JkgtoBTUlb = 1.0_r8k / BTUlbtoJkg        ! (BTU/lb / J/kg)
    !
    ! ** Heat Conductance (Thermal Conductivity) **
    !
    ! Convert Btu/(hr*ft*F) to W/(m*K)
    REAL(r8k), PARAMETER :: BhftFtoWmK = 1.73073467_r8k              ! Convert gap HTC (W/m*K / Btu/hr*ft*F)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBhftF = 1.0_r8k / BhftFtoWmK        ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBsftF = WmKtoBhftF / 3600.0_r8k     ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    !
    ! ** Heat Capacity **
    !
    ! Convert Btu/(lb*F) to J/(kg*K)
    REAL(r8k), PARAMETER :: BtulbFtoJkgK = 4183.995381_r8k           ! (J/kg*K / Btu/lb*F) or (J/kg*C)
    ! Convert J/(kg*K) to Btu/(lb*F)
    REAL(r8k), PARAMETER :: JkgKtoBtulbF = 1.0_r8k / BtulbFtoJkgK    ! (Btu/lb*F / J/kg*K) or (J/kg*C)
    !
    ! ** Pressure **
    !
    ! Convert atm to psi
    REAL(r8k), PARAMETER :: ATMtoPSI = 14.6959488_r8k                ! (psi / atm)
    ! Convert psi to atm
    REAL(r8k), PARAMETER :: PSItoATM = 1.0_r8k / ATMtoPSI            ! (atm / psi)
    ! Convert psi to lb/ft^2
    REAL(r8k), PARAMETER :: PSItolbft2 = 144.0_r8k                   ! (lb/in^2 / lb/ft^2)
    ! Convert kg/cm^2 to psi
    REAL(r8k), PARAMETER :: kgcm2toPSI = 14.2233433_r8k              ! (psi / kg-Force/cm^2)
    ! Convert psi to kg/cm^2
    REAL(r8k), PARAMETER :: PSItokgcm2 = 1.0_r8k / kgcm2toPSI        ! (kg-Force/cm^2 / psi)
    ! Convert psi to kPa
    REAL(r8k), PARAMETER :: PSItokPa = 6.89475728_r8k                ! (kPa / psi)
    ! Convert psi to Pa
    REAL(r8k), PARAMETER :: PSItoPa = 1.0E3_r8k * PSItokPa           ! (Pa / psi)
    ! Convert psi to MPa
    REAL(r8k), PARAMETER :: PSItoMPa = 1.0E-3_r8k * PSItokPa         ! (MPa / psi)
    ! Convert Pa to psi
    REAL(r8k), PARAMETER :: PatoPSI = 1.0_r8k / PSItoPa              ! (Pa / psi)
    ! Convert MPa to psi
    REAL(r8k), PARAMETER :: MPatoPSI = 1.0_r8k / PSItoMPa            ! (Pa / psi)

    REAL(r8k), PARAMETER :: MWskgUtoMWdMTU = 1000.0_r8k / 86400.0_r8k! ((1000 kg * 1 day) / (1 Tonne * 86400s))
    !
    ! ** Distance **
    !
    ! Convert feet to inches
    REAL(r8k), PARAMETER :: fttoin = 12.0_r8k                        ! (inch / ft)
    ! Convert inches to cm
    REAL(r8k), PARAMETER :: intocm = 2.54_r8k                        ! (cm / in)
    ! Convert feet to cm
    REAL(r8k), PARAMETER :: ftocm = (fttoin * intocm)                ! (cm / ft)
    ! Convert inches to to feet
    REAL(r8k), PARAMETER :: intoft = 1.0_r8k / fttoin                ! (ft / in)
    ! Convert inches to mm
    REAL(r8k), PARAMETER :: intomm = 10.0_r8k * intocm               ! (mm / in)
    ! Convert inches to meter
    REAL(r8k), PARAMETER :: intom = 0.01_r8k * intocm                ! (m / in)
    ! Convert meter to feet
    REAL(r8k), PARAMETER :: mtoft = 1.0_r8k / ftom                   ! (ft / m)
    ! Convert cm to feet
    REAL(r8k), PARAMETER :: cmtoft = 1.0_r8k / ftocm                 ! (ft / cm)
    ! Convert cm to inches
    REAL(r8k), PARAMETER :: cmtoin = 1.0_r8k / intocm                ! (in / cm)
    ! Convert cm to meter
    REAL(r8k), PARAMETER :: cmtom = 0.01_r8k                         ! (m / cm)
    ! Convert meter to inches
    REAL(r8k), PARAMETER :: mtoin = 100.0_r8k * cmtoin               ! (in / m)
    ! Convert meter to cm
    REAL(r8k), PARAMETER :: mtocm = 1.0_r8k / cmtom                  ! (cm / m)
    ! Convert meter to mil
    REAL(r8k), PARAMETER :: mtomil = 1000.0_r8k  * mtoin             ! (m / mil)
    ! Convert mil to foot
    REAL(r8k), PARAMETER :: miltoft = 0.001_r8k * intoft             ! (ft / mil)
    ! Convert foot to mil
    REAL(r8k), PARAMETER :: fttomil = 1.0_r8k / miltoft              ! (mil / ft)
    ! Convert mil to millimeter
    REAL(r8k), PARAMETER :: miltomm = 0.01_r8k * intocm              ! (mm / mil)
    ! Convert mil to micrometer
    REAL(r8k), PARAMETER :: miltoum = 10.0_r8k * intocm              ! (micrometer / mil)
    ! Convert micrometer to mil
    REAL(r8k), PARAMETER :: umtomil = 0.1_r8k * cmtoin               ! (mil / micrometer)
    ! Convert micrometer to meter
    REAL(r8k), PARAMETER :: umtom = 1.0E-6_r8k                       ! (meter / micrometer)
    !
    ! ** Area **
    !
    ! Convert ft^2 to in^2
    REAL(r8k), PARAMETER :: ft2toin2 = fttoin ** 2                   ! (in^2 / ft^2)
    ! Convert in^2 to cm^2
    REAL(r8k), PARAMETER :: in2tocm2 = intocm ** 2                   ! (cm^2 / in^2)
    ! Convert in^2 to ft^2
    REAL(r8k), PARAMETER :: in2toft2 = intoft ** 2                   ! (ft^2 / in^2)
    !
    ! ** Volume **
    !
    ! Convert ft^3 to in^3
    REAL(r8k), PARAMETER :: ft3toin3 = fttoin ** 3                   ! (in^3 / ft^3)
    ! Convert in^3 to cm^3
    REAL(r8k), PARAMETER :: in3tocm3 = intocm ** 3                   ! (cm^3 / in^3)
    ! Convert in^3 to mm^3
    REAL(r8k), PARAMETER :: in3tomm3 = intomm ** 3                   ! (m^3 / in^3)
    ! Convert in^3 to m^3
    REAL(r8k), PARAMETER :: in3tom3 = intom ** 3                     ! (m^3 / in^3)
    ! Convert in^3 to ft^3
    REAL(r8k), PARAMETER :: in3toft3 = intoft ** 3                   ! (ft^3 / in^3)
    ! Convert cm^3 to in^3
    REAL(r8k), PARAMETER :: cm3toin3 = cmtoin ** 3                   ! (in^3 / cm^3)
    ! Convert cm^3 to m^3
    REAL(r8k), PARAMETER :: cm3tom3 = cmtom ** 3                     ! (m^3 / cm^3)
    ! Convert m^3 to cm^3
    REAL(r8k), PARAMETER :: m3tocm3 = mtocm ** 3                     ! (cm^3 / m^3)
    ! Convert m^3 to in^3
    REAL(r8k), PARAMETER :: m3toin3 = mtoin ** 3                     ! (in^3 / m^3)
    !
    ! ** Time **
    !
    ! Convert day to hour
    REAL(r8k), PARAMETER :: daytohr = 24.0_r8k                       ! (hour / day)
    ! Convert day to seconds
    REAL(r8k), PARAMETER :: daytosec = 86400.0_r8k                   ! (second / day)
    ! 1 day
    REAL(r8k), PARAMETER :: oneday = daytosec                        ! 1 day in seconds
    ! Convert day to year
    REAL(r8k), PARAMETER :: daytoyr = 1.0_r8k / 365.25_r8k           ! (yr / day)
    ! Convert hour to day
    REAL(r8k), PARAMETER :: hrtoday = 1.0_r8k / daytohr              ! (day / hour)
    ! Convert hour to seconds
    REAL(r8k), PARAMETER :: hrtosec = 3600.0_r8k                     ! (second / hour)
    ! Convert second to hour
    REAL(r8k), PARAMETER :: sectohr = 1.0_r8k / hrtosec              ! (hour / second)
    ! Convert second to day
    REAL(r8k), PARAMETER :: sectoday = 1.0_r8k / daytosec            ! (second / day)
    ! Convert years to days
    REAL(r8k), PARAMETER :: yrtoday = 1.0_r8k / daytoyr              ! (day / year)
    ! Convert years to seconds
    REAL(r8k), PARAMETER :: yrtosec = 3.1536E7_r8k                   ! (second / year)
    !
    ! ** Energy **
    !
    !
    ! ** Heat Transfer Coefficients **
    !
    ! Convert Btu/(hr*ft^2*F) to W/(m^2*K)
    REAL(r8k), PARAMETER :: Bhft2FtoWm2K = 5.678263_r8k              ! Convert gap HTC (W/(m^2*K) / Btu/hr*ft^2*F)
    ! Convert W/(m^2*K) to Btu/(hr*ft^2*F)
    REAL(r8k), PARAMETER :: Wm2KtoBhft2F = 1.0_r8k / Bhft2FtoWm2K    ! Convert gap HTC (Btu/(hr*ft^2*F) / W/(m^2*K))
    !
    ! ** Mass **
    !
    ! Convert lb to grams
    REAL(r8k), PARAMETER :: lbtog = 453.5923699997481_r8k            ! (grams / lb)
    ! Convert kg to grams
    REAL(r8k), PARAMETER :: kgtog = 1000.0_r8k                       ! (grams / kg)
    ! Convert grams to kg
    REAL(r8k), PARAMETER :: gtokg = 1.0_r8k / kgtog                  ! (kg / gram)
    !
    ! ** Mass Flux **
    !
    ! Convert kg/(s*m^2) to lbm/(hr*ft^2)
    REAL(r8k), PARAMETER :: ksm2tolbhrft2 = 737.3422919_r8k          ! (lbm/(hr*ft^2) / kg/(s*m^2))
    ! Convert kg/(s*m^2) to lbm/(hr*ft^2)
    REAL(r8k), PARAMETER :: lbhrft2toksm2 = 1.0_r8k / ksm2tolbhrft2  ! (kg/(s*m^2) / lbm/(hr*ft^2))
    !
    ! ** Density **
    !
    ! Convert lb/ft3 to kg/m3
    REAL(r8k), PARAMETER :: lbft3tokgm3 = 16.0184634_r8k             ! (kg/m^3 / lb/ft^3)
    ! Convert kg/m3 to lb/ft3
    REAL(r8k), PARAMETER :: kgm3tolbft3 = 1.0_r8k / lbft3tokgm3      ! (lb/ft^3 / kg/m^3)
    ! Convert kg/m3 to g/in3
    REAL(r8k), PARAMETER :: kgm3togin3 = kgtog / m3toin3             ! (g/in^3 / kg/m^3)
    !
    ! ** Power **
    !
    ! Convert watt to btu/hr
    REAL(r8k), PARAMETER :: WtoBTUh = 3.41214163513_r8k              ! (Btu/hr / W)
    ! Convert watt to btu/s
    REAL(r8k), PARAMETER :: WtoBTUs = WtoBTUh / hrtosec              ! (Btu/second / W)
    ! Convert kilowatt to btu/hr
    REAL(r8k), PARAMETER :: kWtoBTUh = WtoBTUh * 1.0E3_r8k           ! (Btu/hr / kW)
    ! Convert kilowatt to btu/s
    REAL(r8k), PARAMETER :: kWtoBTUs = kWtoBTUh / hrtosec            ! (Btu/second / kW)
    ! Convert megawatt to btu/hr
    REAL(r8k), PARAMETER :: MWtoBTUh = WtoBTUh * 1.0E6_r8k           ! (Btu/hr / MW)
    ! Convert megawatt to btu/s
    REAL(r8k), PARAMETER :: MWtoBTUs = MWtoBTUh / hrtosec            ! (Btu/second / MW)
    ! Convert btu/hr to watt
    REAL(r8k), PARAMETER :: BTUhtoW = 1.0_r8k / WtoBTUh              ! (W / Btu/hr)
    ! Convert btu/hr to watt
    REAL(r8k), PARAMETER :: BTUhtokW = 1.0_r8k / kWtoBTUh            ! (kW / Btu/hr)
    ! Convert btu/hr to watt
    REAL(r8k), PARAMETER :: BTUhtoMW = 1.0_r8k / MWtoBTUh            ! (MW / Btu/hr)
    ! Convert btu/s to watt
    REAL(r8k), PARAMETER :: BTUstoW = 1.0_r8k / WtoBTUs              ! (W / Btu/sec)
    ! Convert btu/s to watt
    REAL(r8k), PARAMETER :: BTUstokW = 1.0_r8k / kWtoBTUs            ! (kW / Btu/sec)
    ! Convert btu/s to watt
    REAL(r8k), PARAMETER :: BTUstoMW = 1.0_r8k / MWtoBTUs            ! (MW / Btu/sec)
    ! Convert kW/m to kW/ft
    REAL(r8k), PARAMETER :: kWmtokWft = ftom                         ! (kW/ft / kW/m)
    ! Convert kW/ft to kW/m
    REAL(r8k), PARAMETER :: kWfttokWm = 1.0_r8k / kWmtokWft          ! (kW/m / kW/ft)
    ! Convert kW/ft to W/m
    REAL(r8k), PARAMETER :: kWfttoWm = 1.0E3_r8k * kWfttokWm         ! (W/m / kW/ft)
    !
    ! ** Pressure **
    !
    !
    CONTAINS
    !
    PURE FUNCTION TO_UPPERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes lowercase text in a string to uppercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(LOWL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = CAPL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_UPPERCASE
    !
    !
    !
    PURE FUNCTION TO_LOWERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes uppercase text in a string to lowercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(CAPL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = LOWL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_LOWERCASE
    !
    ! ** Temperature Conversion Functions **
    !
    PURE ELEMENTAL FUNCTION tfr (T_Fahrenheit) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  R
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Rankine
    
    T_Rankine = T_Fahrenheit + 459.67_r8k
    
    END FUNCTION tfr
    !
    !
    !
    !
    !
    PURE ELEMENTAL FUNCTION tfk (T_Fahrenheit) RESULT (T_Kelvin)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion: F to K
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/14/2016
    REAL(r8k), INTENT(IN) :: T_Fahrenheit
    REAL(r8k) :: T_Kelvin
    
    T_Kelvin = (T_Fahrenheit + 459.67_r8k) / 1.8_r8k
    
    END FUNCTION tfk
    !
    !
    !
    PURE ELEMENTAL FUNCTION tcr (T_Celcius) RESULT (T_Rankine)
    USE Kinds, ONLY : r8k
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  R
    REAL(r8k), INTENT(IN) :: T_Celcius
    REAL(r8k) :: T_Rankine
    
    T_Rankine = ((T_Celcius * 1.8_r8k) + 32.0_r8k) + 459.67_r8k
    
    END FUNCTION tcr
    !
    !
    !
    !
    !
    SUBROUTINE coneu (xin, xout, itype)
    USE Kinds, ONLY : ipk, r8k
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> coneu - convert engineering units.
    !> This routine converts engineering units using the itype argument to specify the type of conversion needed.
    !> For units with itypes less than 11, a multiplication and addition factor is needed.
    !> For itypes greater than or equal to 11, only a multiplication factor is needed.  (y = (x * a) + b)
    !>@author
    !> h.d. stewart     07/15/80
    !
    ! Input
    !
    ! p1 - Number to be converted
    ! p3 - Type of conversion
    !
    ! Output
    !
    ! p2 - Converted number
    !
    INTEGER(ipk), INTENT(IN) :: itype
    INTEGER(ipk), PARAMETER :: itotal = 28
    REAL(r8k), INTENT(IN) :: xin
    REAL(r8k), INTENT(OUT) :: xout
    REAL(r8k) :: a, x
    REAL(r8k), DIMENSION(itotal), PARAMETER :: contbl = &
      &             [ 0.555555556_r8k,   255.37222_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                       0.0_r8k,         0.0_r8k, &
      &                     1.2e4_r8k,    11348.93_r8k, &
      &                    5.6785_r8k,      0.3048_r8k, &
      &                     25.40_r8k,      0.0254_r8k, &
      &              1.6387064e+4_r8k,  6.89476e-3_r8k, &
      &                   3.28084_r8k,  1.35623e-3_r8k, &
      &                  2.326e+3_r8k,       304.8_r8k, &
      &                    1728.0_r8k,        12.0_r8k, &
      &                   1055.87_r8k,    11348.93_r8k, &
      &                16.0184625_r8k,   0.4535924_r8k ]
    !
    !    The conversion code is as follows:
    !
    !    TYPE   from                to
    !
    !     1     K                   F
    !    11     ft                  mils
    !    12     btu/(s.ft2)         W/m2
    !    13     btu/((hr)(ft2)(F))  W/((m2)(K))
    !    14     ft                  m
    !    15     in                  mm
    !    16     mils                mm
    !    17     in3                 mm3
    !    18     psia                mpa
    !    19     kW/ft               kW/m
    !    20     lb/((hr)(ft2))      kg/((s)(m2))
    !    21     btu/lb              j/kg
    !    22     ft                  mm
    !    23     ft3                 in3
    !    24     ft                  in
    !    25     btu                 j
    !    26     btu/((s)(ft2))      W/m2
    !    27     lbm/ft3             kg/m3
    !    28     lbm                 kg
    !
    xout = xin
    !
    IF (itype <= 0 .OR. itype > itotal) THEN
        WRITE(ounit,10) itype
10      FORMAT(/'itype = ',i6,' is not in expected range of 1 to itotal')
        STOP
    ENDIF
    !
    x = contbl(itype)
    a = 0.0_r8k
    IF (itype <= 9) a = contbl(itype+1)
    !
    xout = (xin * x) + a
    !
    END SUBROUTINE coneu
    !
    ! ** Temperature Conversion Functions **
    !
    REAL(r8k) FUNCTION tkf (tk)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function : convert K  to  F
    !
    REAL(r8k) :: tk
    !
    tkf = (tk * 1.8_r8k) - 459.67_r8k
    !
    END FUNCTION tkf
    !
    !
    !
    REAL(r8k) FUNCTION tkc(tk)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  K  to  C
    !
    REAL(r8k) :: tk
    !
    tkc = tk - 273.15_r8k
    !
    END FUNCTION tkc
    !
    !
    !
    !
    !
    !
    REAL(r8k) FUNCTION trc (tr)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  R  to  C
    !
    REAL(r8k) :: tr
    !
    trc = (tr / 1.8_r8k) - 273.15_r8k
    !
    END FUNCTION trc
    !
    !
    !
    !
    !
    !
    !
    !
    !
    REAL(r8k) FUNCTION tfc (tf)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  C
    !
    REAL(r8k) :: tf
    !
    tfc = (tf - 32.0_r8k) / 1.8_r8k
    !
    END FUNCTION tfc
    !
    !
    !
    REAL(r8k) FUNCTION tcf(tc)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  F
    !
    REAL(r8k) :: tc
    !
    tcf = (tc * 1.8_r8k) + 32.0_r8k
    !
    END FUNCTION tcf
    !
    !
    !
    REAL(r8k) FUNCTION tck(tc)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  K
    !
    REAL(r8k) :: tc
    !
    tck = tc + 273.15_r8k
    !
    END FUNCTION tck
    !
END MODULE Conversions