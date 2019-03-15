MODULE conversions_frapcon
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This file contains the subroutines used to perform unit conversions and holds the conversion factors used in the code.
    !>@author
    !> Developed by Ian Porter, NRC
    !>@date
    !> April, 2014
    !
    ! ** Burnup **
    !
    REAL(r8k), PARAMETER :: MWskgUtoMWdMTU = 1000.0_r8k / 86400.0_r8k! ((1000 kg * 1 day) / (1 Tonne * 86400s))
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
    !
    ! ** Distance **
    !
    ! Convert feet to inches
    REAL(r8k), PARAMETER :: fttoin = 12.0_r8k                        ! (inch / ft)
    ! Convert inches to cm
    REAL(r8k), PARAMETER :: intocm = 2.54_r8k                        ! (cm / in)
    ! Convert feet to cm
    REAL(r8k), PARAMETER :: ftocm = (fttoin * intocm)                ! (cm / ft)
    ! Convert feet to mm
    REAL(r8k), PARAMETER :: ftomm = (fttoin * intocm) * 1.D+1        ! (mm / ft)
    ! Convert feet to um
    REAL(r8k), PARAMETER :: ftoum = (fttoin * intocm) * 1.D+4        ! (um / ft)
    REAL(r8k), PARAMETER :: fttoum = (fttoin * intocm) * 1.D+4        ! (um / ft)
    ! Convert feet to m
    REAL(r8k), PARAMETER :: ftom = ftocm / 100.0_r8k                 ! (meter / ft)
    ! Convert inches to to feet
    REAL(r8k), PARAMETER :: intoft = 1.0_r8k / fttoin                ! (ft / in)
    ! Convert inches to mm
    REAL(r8k), PARAMETER :: intomm = 10.0_r8k * intocm               ! (mm / in)
    REAL(r8k), PARAMETER :: mmtoin = 1.d0 / intomm
    ! Convert inches to meter
    REAL(r8k), PARAMETER :: intom = 0.01_r8k * intocm                ! (m / in)
    REAL(r8k), PARAMETER :: intoum = 1000.d0 * intomm                ! (m / in)
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
    REAL(r8k), PARAMETER :: mmtoft = 1.d0 / (miltomm * fttomil)
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
    ! ** Heat Flux **
    !
    ! Convert (Btu/(hr*ft^2)) to (W/m^2)
    REAL(r8k), PARAMETER :: Bhft2toWm2 = 3.152481_r8k                ! (W/m2) / (Btu/h*ft2)
    ! Note:  3.154590745 (BTU(IT)) or 3.152481 (BTU(TH))
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
    ! Convert grams to lb
    REAL(r8k), PARAMETER :: gtolb = 1.0_r8k / lbtog                  ! (lb / gram)
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
    !
    !
    !
    CONTAINS
    !
    ! ** Temperature Conversion Functions **
    !
    REAL(r8k) FUNCTION tkf (tk)
    USE Kinds_frapcon
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
    USE Kinds_frapcon
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
    REAL(r8k) FUNCTION tfr (tf)
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  R
    !
    REAL(r8k) :: tf
    !
    tfr = tf + 459.67_r8k
    !
    END FUNCTION tfr
    !
    !
    !
    REAL(r8k) FUNCTION trc (tr)
    USE Kinds_frapcon
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
    REAL(r8k) FUNCTION tfk(tf)
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  F  to  K
    !
    REAL(r8k) :: tf
    !
    tfk = (tf + 459.67_r8k) / 1.8_r8k
    !
    END FUNCTION tfk
    !
    !
    !
    REAL(r8k) FUNCTION tcr (tc)
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> Temperature conversion function: convert  C  to  R
    !
    REAL(r8k) :: tc
    !
    tcr = ((tc * 1.8_r8k) + 32.0_r8k) + 459.67_r8k
    !
    END FUNCTION tcr
    !
    !
    !
    REAL(r8k) FUNCTION tfc (tf)
    USE Kinds_frapcon
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
    USE Kinds_frapcon
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
    USE Kinds_frapcon
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
END MODULE conversions_frapcon



