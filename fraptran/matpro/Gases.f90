MODULE NCGases_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    USE functions_fraptran
    USE variables_fraptran, ONLY : GasFraction, ounit, GasPress, gsmol0
    USE FissionGasRelease_h_fraptran, ONLY : gsmol0
    IMPLICIT NONE
    !>@brief
    !> This Module contains all Gas Properties_fraptran
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> Updated 4/15/2015
    !
    ! Keep Functions Private
    !
    PRIVATE
    PUBLIC :: NonCondGases, Allocate_Gas, ngases, HeIndex, SteamIndex, ncGasProperties
    !
    ! Data type for fit function parameters for specific heat at constant volume for non-condensable gases.
    TYPE SpecHeatFitT
        ! Fit function for specific heat at constant volume for non-condensable gases in the mixture of non-condensable gas model.
        ! cv = co                   for T < to
        ! cv = co + do * (T - to)   for T >= to
        ! where T is gas phase temperature and cv is specific heat at constant volume for gas.
        REAL(r8k) :: c0         ! Specific heat at constant volume constant parameter.
        REAL(r8k) :: d0         ! Specific heat at constant volume linear temperature coefficient.
        REAL(r8k) :: t0         ! Specific heat at constant volume temperature.
    END TYPE SpecHeatFitT
    !
    ! Data type for fit function for viscosity of non-condensable gases.
    TYPE ViscosityFitT
        ! Fit function for viscosity for non-condensable gases in the mixture of non-condensable gas model.
        ! viscosity = muo * T ** 1.5 / (T + to)
        REAL(r8k) :: muo        ! muo constant in viscosity fit.
        REAL(r8k) :: t0         ! to constant in viscosity fit.
        REAL(r8k) :: cld        ! Collision diameter (m)
        REAL(r8k) :: eok        ! maximum energy of attraction between a pair of molecules (J/molecule) 
                                ! divided by Boltzmann's constant
    END TYPE ViscosityFitT
    !
    ! Data type for fit function for conductivity of non-condensable gases.
    TYPE ConductivityFitT
        ! Fit function for conductivity for non-condensable gases in the mixture of non-condensable gas model.
        ! conductivity = aa * T ^ bb
        REAL(r8k) :: aa      ! aa coefficient in the non-condensable gas conductivity fit.
        REAL(r8k) :: bb      ! bb exponent in the non-condensable gas conductivity fit.
    END TYPE ConductivityFitT
    !
    ! Non-condensable gas properties
    TYPE ncGasPropertiesT
        !
        REAL(r8k) :: mWt                    ! Molecular weight.
        REAL(r8k) :: adv                    ! Atomic diffusion volume.
        REAL(r8k) :: rGas                   ! Species gas constant. (Universal Gas Constant / Molecular Weight) (J/(kg*K))
        CHARACTER(LEN=3) :: Name            ! Gas name.
        TYPE(SpecHeatFitT) :: cvFit         ! Parameters for the specific heat at constant volume fit function.
        TYPE(ViscosityFitT) :: muFit        ! Parameters for the viscosity fit function.
        TYPE(ConductivityFitT) :: kFit      ! Parameters for the conducitivity fit function.
    END TYPE ncGasPropertiesT
    !
    ! Current gas species modeled
    INTEGER(ipk), PARAMETER :: HeIndex = 1       ! Helium
    INTEGER(ipk), PARAMETER :: ArIndex = 2       ! Argon
    INTEGER(ipk), PARAMETER :: KrIndex = 3       ! Krypton
    INTEGER(ipk), PARAMETER :: XeIndex = 4       ! Xenon
    INTEGER(ipk), PARAMETER :: H2Index = 5       ! Hydrogen
    INTEGER(ipk), PARAMETER :: N2Index = 6       ! Nitrogen
    INTEGER(ipk), PARAMETER :: AirIndex = 7      ! Air
    INTEGER(ipk), PARAMETER :: SteamIndex = 8    ! Steam
    INTEGER(ipk), PARAMETER :: FGIndex = 9       ! Fission Gas. Currently only used for molar mass
    !
    INTEGER(ipk), PARAMETER :: ngases = 8        ! # of different gases in gas gap
    ! Molecular weight of He.
    REAL(r8k), PARAMETER :: HeMWt = 4.0026_r8k
    ! Molecular weight of Ar.
    REAL(r8k), PARAMETER :: ArMWt = 39.94766_r8k
    ! Molecular weight of Kr.
    REAL(r8k), PARAMETER :: KrMWt = 83.8_r8k
    ! Molecular weight of Xe.
    REAL(r8k), PARAMETER :: XeMWt = 131.3_r8k
    ! Molecular weight of H2.
    REAL(r8k), PARAMETER :: H2MWt = 2.01594_r8k
    ! Molecular weight of N2.
    REAL(r8k), PARAMETER :: N2MWt = 28.01403_r8k
    ! The following are needed to calculate the Molar Mass of air
    ! Molecular weight of O2.
    REAL(r8k), PARAMETER :: O2MWt = 31.99874_r8k
    ! Molecular weight of CO2.
    REAL(r8k), PARAMETER :: CO2MWt = 44.00989_r8k
    ! Molecular weight of Ne.
    REAL(r8k), PARAMETER :: NeMWt = 20.17135_r8k
    ! Molecular weight of air. (= mole fraction * molecular weight sum)
    !   = 78.084% N2, 20.946% O2, 0.934% Ar, 0.033% CO2, 0.002% Ne, 0.001% He
    REAL(r8k), PARAMETER :: airMWt = 0.78084_r8k * N2MWt + 0.20946_r8k * O2MWt + 9.34E-3_r8k * ArMWt + &
      &                              3.3E-4_r8k * CO2MWt + 2.0E-5_r8k + 1.0E-5
    ! Molecular weight of steam.
    REAL(r8k), PARAMETER :: H2OMWt = 18.016_r8k
    ! Molecular weight of fission gas.
    REAL(r8k), PARAMETER :: fgMWt = 124.17_r8k
    !
    ! Function of prediction of transport properties of gases at low densities
    ! Transport Phenomena, Bird, Stewart and Lightfoot 1960. Table B-2
    ! Interpolation table of (Omega, kT/e) where Omega is for viscosity & thermal conductivity
    INTEGER(ipk), PARAMETER, PRIVATE :: nt = 82
    REAL(r8k), DIMENSION(2*nt), PARAMETER, PRIVATE :: Omegavk = [  &
      &     2.785_r8k,  0.30_r8k,  2.628_r8k,  0.35_r8k,  2.492_r8k,  0.40_r8k,  2.368_r8k,  0.45_r8k, &
      &     2.257_r8k,  0.5_r8k,   2.156_r8k,  0.55_r8k,  2.065_r8k,  0.60_r8k,  1.982_r8k,  0.65_r8k, &
      &     1.908_r8k,  0.70_r8k,  1.841_r8k,  0.75_r8k,  1.780_r8k,  0.80_r8k,  1.725_r8k,  0.85_r8k, &
      &     1.675_r8k,  0.90_r8k,  1.629_r8k,  0.95_r8k,  1.587_r8k,  1.0_r8k,   1.549_r8k,  1.05_r8k, &
      &     1.514_r8k,  1.1_r8k,   1.482_r8k,  1.15_r8k,  1.452_r8k,  1.2_r8k,   1.424_r8k,  1.25_r8k, &
      &     1.399_r8k,  1.3_r8k,   1.375_r8k,  1.35_r8k,  1.353_r8k,  1.4_r8k,   1.333_r8k,  1.45_r8k, &
      &     1.314_r8k,  1.5_r8k,   1.296_r8k,  1.55_r8k,  1.279_r8k,  1.6_r8k,   1.264_r8k,  1.65_r8k, &
      &     1.248_r8k,  1.7_r8k,   1.234_r8k,  1.75_r8k,  1.221_r8k,  1.8_r8k,   1.209_r8k,  1.85_r8k, &
      &     1.197_r8k,  1.9_r8k,   1.186_r8k,  1.95_r8k,  1.175_r8k,  2.0_r8k,   1.156_r8k,  2.1_r8k,  &
      &     1.138_r8k,  2.2_r8k,   1.122_r8k,  2.3_r8k,   1.107_r8k,  2.4_r8k,   1.093_r8k,  2.5_r8k,  &
      &     1.081_r8k,  2.6_r8k,   1.069_r8k,  2.7_r8k,   1.058_r8k,  2.8_r8k,   1.048_r8k,  2.9_r8k,  &
      &     1.039_r8k,  3.0_r8k,   1.030_r8k,  3.1_r8k,   1.022_r8k,  3.2_r8k,   1.014_r8k,  3.3_r8k,  &
      &     1.007_r8k,  3.4_r8k,   0.9999_r8k, 3.5_r8k,   0.9932_r8k, 3.6_r8k,   0.9870_r8k, 3.7_r8k,  &
      &     0.9811_r8k, 3.8_r8k,   0.9755_r8k, 3.9_r8k,   0.9700_r8k, 4.0_r8k,   0.9649_r8k, 4.1_r8k,  &
      &     0.9600_r8k, 4.2_r8k,   0.9553_r8k, 4.3_r8k,   0.9507_r8k, 4.4_r8k,   0.9464_r8k, 4.5_r8k,  &
      &     0.9422_r8k, 4.6_r8k,   0.9382_r8k, 4.7_r8k,   0.9343_r8k, 4.8_r8k,   0.9305_r8k, 4.9_r8k,  &
      &     0.9269_r8k, 5.0_r8k,   0.8963_r8k, 6.0_r8k,   0.8727_r8k, 7.0_r8k,   0.8538_r8k, 8.0_r8k,  &
      &     0.8379_r8k, 9.0_r8k,   0.8242_r8k, 10.0_r8k,  0.7432_r8k, 20.0_r8k,  0.7005_r8k, 30.0_r8k,  &
      &     0.6718_r8k, 40.0_r8k,  0.6504_r8k, 50.0_r8k,  0.6335_r8k, 60.0_r8k,  0.6194_r8k, 70.0_r8k,  &
      &     0.6076_r8k, 80.0_r8k,  0.5973_r8k, 90.0_r8k,  0.5882_r8k, 100.0_r8k, 0.5320_r8k, 200.0_r8k, &
      &     0.5016_r8k, 300.0_r8k, 0.4811_r8k, 400.0_r8k ]
    !
    ! Atomic Diffusion Volume. Not currently used.
    ! Ref:  Eq. 11-4.1 of "Properties of Gases and Liquids" by Reid, Praudnitz & Sherwood.
    !       3rd edition, McGraw-Hill Book Co., 1977.
    !
    REAL(r8k), PARAMETER :: advHe  = 2.67_r8k
    REAL(r8k), PARAMETER :: advAr  = 16.2_r8k
    REAL(r8k), PARAMETER :: advKr  = 24.5_r8k
    REAL(r8k), PARAMETER :: advXe  = 32.7_r8k
    REAL(r8k), PARAMETER :: advH2  = 6.12_r8k
    REAL(r8k), PARAMETER :: advN2  = 18.5_r8k
    REAL(r8k), PARAMETER :: advAir = 19.7_r8k
    REAL(r8k), PARAMETER :: advVap = 13.1_r8k
    !
    ! Maximum number of non-condensable gases allowed in a user defined mixture of non-condensable gases.
    INTEGER(ipk), PARAMETER :: maxNCGasMix = 7
    TYPE(ncGasPropertiesT), DIMENSION(maxNCGasMix + 1) :: ncGasProperties  ! non-condensable gas individual species properties
    !
    ! Minimum mole fraction for gas to be considered in calculation
    REAL(r8k), PARAMETER, PRIVATE :: MinMoles = 1.0E-9_r8k
    ! Limits on NCG temperatures (K).
    REAL(r8k), PARAMETER, PRIVATE :: tvLimitMin = 0.1_r8k
    REAL(r8k), PARAMETER, PRIVATE :: tvLimitMax = 5555.0_r8k
    !
    ! Flag to indicate the call to LoadncGasProperties
    LOGICAL, PRIVATE :: FirstCall = .TRUE.
    !
    ! Theoretical Density (g/cm3)
    REAL(r8k), PARAMETER :: ZircTD = 6.52_r8k
    ! Melting temperature (K)
    REAL(r8k), PARAMETER :: ZircTmelt = 2098.15_r8k
    !
    TYPE NonCondGases
        ! Name
        CHARACTER(LEN=10) :: MatID = 'GAS'
        ! Temperature
        REAL(r8k) :: Temp
        ! Oxide thickness
        REAL(r8k) :: Press
        ! Gap width
        REAL(r8k) :: Width
        ! Gas composition
        REAL(r8k), DIMENSION(ngases) :: Composition
    CONTAINS
        PROCEDURE :: MatProp
        PROCEDURE, PRIVATE :: gthcon
        PROCEDURE, PRIVATE :: CP
        PROCEDURE, PRIVATE :: gvisco
        PROCEDURE, PRIVATE :: Rho
        PROCEDURE, PRIVATE :: MWt
        PROCEDURE, PRIVATE :: gconr2
    END TYPE NonCondGases
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (Gas, property) RESULT (mat_prop)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calls all of the Rod Gas Material Properties
    CLASS (NonCondGases) :: Gas
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    IF (FirstCall) CALL LoadGases
    !
    SELECT CASE (property)
    CASE ('THERMCOND')       ! Thermal Conductivity
        mat_prop = Gas%gthcon ()
    CASE ('SPECHEAT')        ! Specific Heat
        mat_prop = Gas%CP ()
    CASE ('VISCOSITY')       ! Viscosity
        mat_prop = Gas%gvisco ()
    CASE ('DENSITY')         ! Density
        mat_prop = Gas%Rho ()
    CASE ('MOLWT')           ! Molecular Weight
        mat_prop = Gas%MWt ()
    CASE ('GCONR2')
        mat_prop = Gas%gconr2 ()
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module Gas_fraptran. Invalid material property ID. Material Property ID = ',a20)
        STOP
    END SELECT
    !
    END FUNCTION MatProp
    !
    !
    !
    SUBROUTINE LoadGases ()
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This subroutine loads the properties needed for the non-condensible fill gases
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/15/2015
    !
    ! Helium
    ncGasProperties(HeIndex)%MWt = HeMWt
    ncGasProperties(HeIndex)%adv = advHe
    ncGasProperties(HeIndex)%Rgas = 2077.235_r8k
    ncGasProperties(HeIndex)%Name = 'He '
    ncGasProperties(HeIndex)%cvFit%c0 = 3115.839_r8k
    ncGasProperties(HeIndex)%cvFit%d0 = 0.003455924_r8k
    ncGasProperties(HeIndex)%cvFit%t0 = 250.0_r8k
    ncGasProperties(HeIndex)%muFit%muo = 1.473E-6_r8k
    ncGasProperties(HeIndex)%muFit%t0 = 80.3_r8k
    ncGasProperties(HeIndex)%muFit%cld = 2.576_r8k
    ncGasProperties(HeIndex)%muFit%eok = 10.2_r8k
    ncGasProperties(HeIndex)%KFit%aa = 2.531E-3_r8k
    ncGasProperties(HeIndex)%KFit%bb = 0.7146_r8k
    !
    ! Argon
    ncGasProperties(ArIndex)%MWt = ArMWt
    ncGasProperties(ArIndex)%adv = advAr
    ncGasProperties(ArIndex)%Rgas = 208.129_r8k
    ncGasProperties(ArIndex)%Name = 'Ar '
    ncGasProperties(ArIndex)%cvFit%c0 = 312.192_r8k
    ncGasProperties(ArIndex)%cvFit%d0 = 0.003517_r8k
    ncGasProperties(ArIndex)%cvFit%t0 = 250.0_r8k
    ncGasProperties(ArIndex)%muFit%muo = 1.935E-6_r8k
    ncGasProperties(ArIndex)%muFit%t0= 147.0_r8k
    ncGasProperties(ArIndex)%muFit%cld = 3.418_r8k
    ncGasProperties(ArIndex)%muFit%eok = 124.0_r8k
    ncGasProperties(ArIndex)%KFit%aa = 4.092E-4_r8k
    ncGasProperties(ArIndex)%KFit%bb = 0.6748_r8k
    !
    ! Krypton
    ncGasProperties(KrIndex)%MWt = KrMWt
    ncGasProperties(KrIndex)%adv = advKr
    ncGasProperties(KrIndex)%Rgas = 99.216_r8k
    ncGasProperties(KrIndex)%Name = 'Kr '
    ncGasProperties(KrIndex)%cvFit%c0 = 148.824_r8k
    ncGasProperties(KrIndex)%cvFit%d0 = 0.0035_r8k
    ncGasProperties(KrIndex)%cvFit%t0 = 250.0_r8k
    ncGasProperties(KrIndex)%muFit%muo = 2.386E-6_r8k
    ncGasProperties(KrIndex)%muFit%t0= 188.0_r8k
    ncGasProperties(KrIndex)%muFit%cld = 3.61_r8k
    ncGasProperties(KrIndex)%muFit%eok = 190.0_r8k ! FRAPCON-3.5 used 225.0
    ncGasProperties(KrIndex)%KFit%aa = 1.966E-4_r8k
    ncGasProperties(KrIndex)%KFit%bb = 0.7006_r8k
    !
    ! Xenon
    ncGasProperties(XeIndex)%MWt = XeMWt
    ncGasProperties(XeIndex)%adv = advXe
    ncGasProperties(XeIndex)%Rgas = 63.323_r8k
    ncGasProperties(XeIndex)%Name = 'Xe '
    ncGasProperties(XeIndex)%cvFit%c0 = 94.9085_r8k
    ncGasProperties(XeIndex)%cvFit%d0 = 0.0035_r8k
    ncGasProperties(XeIndex)%cvFit%t0 = 250.0_r8k
    ncGasProperties(XeIndex)%muFit%muo = 2.455E-6_r8k
    ncGasProperties(XeIndex)%muFit%t0= 252.1_r8k
    ncGasProperties(XeIndex)%muFit%cld = 4.055_r8k
    ncGasProperties(XeIndex)%muFit%eok = 229.0_r8k
    ncGasProperties(XeIndex)%KFit%aa = 9.825E-5_r8k
    ncGasProperties(XeIndex)%KFit%bb = 0.7334_r8k
    !
    ! Hydrogen
    ncGasProperties(H2Index)%MWt = H2MWt
    ncGasProperties(H2Index)%adv = advH2
    ncGasProperties(H2Index)%Rgas = 4124.299_r8k
    ncGasProperties(H2Index)%Name = 'H2 '
    ncGasProperties(H2Index)%cvFit%c0 = 10310.75_r8k
    ncGasProperties(H2Index)%cvFit%d0 = 0.522573_r8k
    ncGasProperties(H2Index)%cvFit%t0 = 250.0_r8k
    ncGasProperties(H2Index)%muFit%muo = 6.675E-7_r8k
    ncGasProperties(H2Index)%muFit%t0= 83.0_r8k
    ncGasProperties(H2Index)%muFit%cld = 2.915_r8k
    ncGasProperties(H2Index)%muFit%eok = 38.0_r8k
    ncGasProperties(H2Index)%KFit%aa = 1.349E-3_r8k
    ncGasProperties(H2Index)%KFit%bb = 0.8408_r8k
    !
    ! Nitrogen
    ncGasProperties(N2Index)%MWt = N2MWt
    ncGasProperties(N2Index)%adv = advN2
    ncGasProperties(N2Index)%Rgas = 296.792_r8k
    ncGasProperties(N2Index)%Name = 'N2 '
    ncGasProperties(N2Index)%cvFit%c0 = 741.976_r8k
    ncGasProperties(N2Index)%cvFit%d0 = 0.1184518_r8k
    ncGasProperties(N2Index)%cvFit%t0 = 250.0_r8k
    ncGasProperties(N2Index)%muFit%muo = 1.381E-6_r8k
    ncGasProperties(N2Index)%muFit%t0= 102.7_r8k
    ncGasProperties(N2Index)%muFit%cld = 3.681_r8k
    ncGasProperties(N2Index)%muFit%eok = 91.5_r8k
    ncGasProperties(N2Index)%KFit%aa = 2.984E-4_r8k
    ncGasProperties(N2Index)%KFit%bb = 0.7799_r8k
    !
    ! Air
    ncGasProperties(AirIndex)%MWt = AirMWt
    ncGasProperties(AirIndex)%adv = advAir
    ncGasProperties(AirIndex)%Rgas = 287.052_r8k
    ncGasProperties(AirIndex)%Name = 'Air'
    ncGasProperties(AirIndex)%cvFit%c0 = 715.0_r8k
    ncGasProperties(AirIndex)%cvFit%d0 = 0.10329037_r8k
    ncGasProperties(AirIndex)%cvFit%t0 = 250.0_r8k
    ncGasProperties(AirIndex)%muFit%muo = 1.492E-6_r8k
    ncGasProperties(AirIndex)%muFit%t0= 114.0_r8k
    ncGasProperties(AirIndex)%muFit%cld = 3.617_r8k
    ncGasProperties(AirIndex)%muFit%eok = 97.0_r8k
    ncGasProperties(AirIndex)%KFit%aa = 1.945E-4_r8k
    ncGasProperties(AirIndex)%KFit%bb = 0.8586_r8k
    !
    ! Steam
    ncGasProperties(SteamIndex)%MWt = H2OMWt
    ncGasProperties(SteamIndex)%adv = AdvVap
    ncGasProperties(SteamIndex)%Rgas = 461.4975_r8k
    ncGasProperties(SteamIndex)%Name = 'H2O'
    !
    FirstCall = .FALSE.
    !
    END SUBROUTINE LoadGases
    !
    !
    !
    REAL(r8k) FUNCTION gthcon (Gas)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> gthcon calculates gas thermal conductivity as a function of temperature and gas fraction
    !>@author
    !> gthcon coded by r.c.young  march 1975
    !> adapted from routine cmix by p.e.macdonald
    !> Updated by W.G. Luscher 10-17-07
    !> Updated by I. Porter, NRC May 2015
    !
    ! Input
    !
    ! GasFraction(i) - mole fractions of the gas mixture.
    ! Gas%Temp       - gas temperature (k)
    ! Gas%Press      - gas pressure (psi) used for knudsen domain correction and for steam
    !
    ! Output
    !
    ! gthcon   - Gas thermal conductivity (W/m-k)
    !
    ! Internal
    !
    ! aa       - fitting paramter, for equation gthcon = aa * gtemp ^ bb
    ! bb       - fitting paramter, for equation gthcon = aa * gtemp ^ bb
    !
    ! Reference:
    !
    ! Formula for gas mixtures is from: r.s.brokaw, report nasa tr r-81 (1960)
    ! Conductivity of rare gases is based on  j.m.gandhi and s.c.saxena, jour. chem. and eng. data, vol.13, no.3 (1968)
    ! Wisconsin electric power co., docket no. 50-301 (jan 1973)
    ! The accommodation factor is from  r.a.dean, cvna-127 (1962)
    ! Steam equation is from  meyer, et.al., "thermodynamic and transport properties of steam", 
    ! the american society of mechanical engineers (1967)
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: i, j
    REAL(r8k) :: gtempC, gpres, fij, gij, kmix, rmoles, sum
    REAL(r8k), PARAMETER :: SQRT8 = SQRT(8.0_r8k)
    REAL(r8k), DIMENSION(ngases) :: kgas, KroneckerDelta
    ! Check for input errors. Input range requires 0.1K < Gas Temperature < 5555K
    ! and the molar fraction of gases must be > 0.0
    IF (Gas%Temp < tvLimitMin .OR. Gas%Temp > tvLimitMax .OR. MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0,180) Gas%Temp, GasFraction
        WRITE (ounit,180) Gas%Temp, GasFraction
180     FORMAT (' Error in gthcon. Bad input. Gas%Temp= ',e12.4,' K',/,' Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    kmix = 0.0_r8k
    !
    ! Calculate the thermal conductivity of each gas species
    DO i = 1, ngases
        !
        SELECT CASE (i)
        CASE (SteamIndex)
            ! Steam
            gtempC = MAX(0.1_r8k, tkc(Gas%Temp))
            gpres = Gas%Press * PsitoPa
            IF (Gas%Temp <= 973.15_r8k) THEN
                kgas(i) = gpres / Gas%Temp * (-2.8516e-8_r8k + 9.424e-10_r8k * Gas%Temp - 6.005e-14_r8k * Gas%Temp ** 2) + &
                  &       1.009_r8k * gpres ** 2 / ((Gas%Temp ** 2) * (gtempC ** 4.2_r8k)) + (17.6e-4_r8k + 5.87e-5_r8k * &
                  &       (gtempC) + 1.08e-7_r8k * (gtempC) ** 2 - 4.51e-11_r8k * (gtempC) ** 3)
            ELSE
                kgas(i) = 4.44e-6_r8k * Gas%Temp ** 1.45_r8k + 9.45e-5_r8k * (2.1668e-9_r8k * gpres / Gas%Temp) ** 1.3_r8k
            END IF
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            !
            ! Conductivity = aa * temp ^ bb
            kgas(i) = ncGasProperties(i)%KFit%aa * Gas%Temp ** ncGasProperties(i)%KFit%bb
        CASE DEFAULT
            WRITE (0,101) i
            WRITE (ounit,101) i
101         FORMAT ('Error in subroutine gthcon. Wrong Gas ID called. GasID = ',i7)
            STOP
        END SELECT
    END DO
    !
    ! Calculate thermal conductivity of gas mixture
    DO i = 1, ngases
        sum = 0.0_r8k
        KroneckerDelta = 0.0_r8k
        DO j = 1, ngases
            IF (j == i) KroneckerDelta(j) = 1.0_r8k
            rmoles = ncGasProperties(i)%MWt / ncGasProperties(j)%MWt
            gij = (1.0_r8k + SQRT(kgas(i) / kgas(j)) * (rmoles) ** 0.25_r8k) ** 2 / (SQRT8 * SQRT(1.0_r8k + rmoles))
            fij = gij * (1.0_r8k + 2.41_r8k * (ncGasProperties(i)%MWt - ncGasProperties(j)%MWt) * &
              &   (ncGasProperties(i)%MWt - 0.142_r8k * ncGasProperties(j)%MWt) / &
              &   (ncGasProperties(i)%MWt + ncGasProperties(j)%MWt) ** 2)
            sum = sum + (1.0_r8k - KroneckerDelta(j)) * fij * GasFraction(j)
        END DO
        kmix = kmix + kgas(i) * GasFraction(i) / (GasFraction(i) + sum)
    END DO
    !
    gthcon = kmix
    !
    END FUNCTION gthcon
    !
    !
    !
    REAL(r8k) FUNCTION gconr2 (Gas)
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> gconr2 calculates the conduction part of gas gap heat conductance as a function of temperature, gas
    !> component fractions, gap width and gas pressure. Gap surface effects are considered.
    !>@author
    !> gconr2 was coded by r. c. young march 1975
    !> modified by d. l. hagrman october 1979
    !> Modified by PNNL, February 1997, to clean up coding and delete licensing audit coding.
    !> Modified by Ian Porter, NRC, 1/13/2016 to allow the code to use updated gas properties_fraptran
    !
    ! Input
    !
    ! GasFraction(i) - absolute mole fractions
    !              1 = Helium
    !              2 = Argon
    !              3 = Krypton
    !              4 = Xenon
    !              5 = Hydrogen
    !              6 = Nitrogen
    !              7 = Air
    !              8 = Water vapor
    ! gtemp          - gas temperature (K)
    ! gpres          - gas pressure (Pa)
    ! Gas%Width      - gap width (m) 
    !                  (A minimum gap width of 4.4e-6 is suggested to account for typical fuel surface roughness)
    !
    ! Output
    !
    ! gconr2         - conduction part of gas gap heat conductance (W/((m**2)*K))
    !
    ! Reference:
    !
    ! (1)  g. k. white, experimental techniques in low temperature physics, oxford press (1959) pp 181 - 183.
    ! (2)  m. w. zemansky, heat and thermodynamics, mcgraw - hill book company, inc (1957).
    ! (3)  c. d. hodgman (ed), handbook of chemistry and physics. thirty - eighth edition, chemical rubber publishing co. (1956).
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: i, j
    REAL(r8k) :: gtempC, gpres, fij, gij, kmix, rmoles, sum, sphi, yi
    REAL(r8k), PARAMETER :: SQRT8 = SQRT(8.0_r8k)
    REAL(r8k), DIMENSION(ngases) :: kgas, KroneckerDelta
    ! Note: (IP)
    ! I am not sure what these properties represent. Assumption was made that properties are the
    ! same for Nitrogen as they are for air.
    REAL(r8k), DIMENSION(ngases), PARAMETER :: ac = &
      &     [ 0.06_r8k, 0.15_r8k, 0.74_r8k, 0.74_r8k, 0.66_r8k, 0.19_r8k, 0.19_r8k, 0.19_r8k ]
    REAL(r8k), DIMENSION(ngases), PARAMETER :: ar = &
      &     [ 1.6669_r8k, 1.6667_r8k, 1.6667_r8k, 1.6667_r8k, 1.4045_r8k, 1.4006_r8k, 1.4006_r8k, 1.2857_r8k ]
    REAL(r8k), DIMENSION(ngases), PARAMETER :: br = &
      &     [ -2.0e-9_r8k, 3.53e-8_r8k, 3.53e-8_r8k, 3.53e-8_r8k, 2.5e-9_r8k, 2.21e-8_r8k, 2.21e-8_r8k, 6.29e-8_r8k ]
    ! Check for input errors. Input range requires 0.1K < Gas Temperature < 5555K
    ! and the molar fraction of gases must be > 0.0
    IF (Gas%Temp < tvLimitMin .OR. Gas%Temp > tvLimitMax .OR. MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0,180) Gas%Temp, GasFraction
        WRITE (ounit,180) Gas%Temp, GasFraction
180     FORMAT (' Error in gthcon. Bad input. Gas%Temp= ',e12.4,' K',/,' Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    kmix = 0.0_r8k
    !
    ! Calculate the thermal conductivity of each gas species
    DO i = 1, ngases
        !
        SELECT CASE (i)
        CASE (SteamIndex)
            ! Steam
            gtempC = MAX(0.1_r8k, tkc(Gas%Temp))
            gpres = Gas%Press * PsitoPa
            IF (Gas%Temp <= 973.15_r8k) THEN
                kgas(i) = gpres / Gas%Temp * (-2.8516e-8_r8k + 9.424e-10_r8k * Gas%Temp - 6.005e-14_r8k * Gas%Temp ** 2) + &
                  &       1.009_r8k * gpres ** 2 / ((Gas%Temp ** 2) * (gtempC ** 4.2_r8k)) + (17.6e-4_r8k + 5.87e-5_r8k * &
                  &       (gtempC) + 1.08e-7_r8k * (gtempC) ** 2 - 4.51e-11_r8k * (gtempC) ** 3)
            ELSE
                kgas(i) = 4.44e-6_r8k * Gas%Temp ** 1.45_r8k + 9.45e-5_r8k * (2.1668e-9_r8k * gpres / Gas%Temp) ** 1.3_r8k
            END IF
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            !
            ! Conductivity = aa * temp ^ bb
            kgas(i) = ncGasProperties(i)%KFit%aa * Gas%Temp ** ncGasProperties(i)%KFit%bb
        CASE DEFAULT
            WRITE (0,101) i
            WRITE (ounit,101) i
101         FORMAT ('Error in subroutine gthcon. Wrong Gas ID called. GasID = ',i7)
            STOP
        END SELECT
    END DO
    !
    ! Calculate thermal conductivity of gas mixture
    DO i = 1, ngases
        sum = 0.0_r8k
        KroneckerDelta = 0.0_r8k
        DO j = 1, ngases
            IF (j == i) KroneckerDelta(j) = 1.0_r8k
            rmoles = ncGasProperties(i)%MWt / ncGasProperties(j)%MWt
            gij = (1.0_r8k + SQRT(kgas(i) / kgas(j)) * (rmoles) ** 0.25_r8k) ** 2 / (SQRT8 * SQRT(1.0_r8k + rmoles))
            fij = gij * (1.0_r8k + 2.41_r8k * (ncGasProperties(i)%MWt - ncGasProperties(j)%MWt) * &
              &   (ncGasProperties(i)%MWt - 0.142_r8k * ncGasProperties(j)%MWt) / &
              &   (ncGasProperties(i)%MWt + ncGasProperties(j)%MWt) ** 2)
            sum = sum + (1.0_r8k - KroneckerDelta(j)) * fij * GasFraction(j)
        END DO
        sphi = ar(i) + br(i) * Gas%Press
        yi = SQRT(ncGasProperties(i)%MWt * Gas%Temp) * (sphi - 1.0_r8k) * kgas(i) / &
          &      (18.0_r8k * (sphi + 1.0_r8k) * ac(i) * Gas%Press)
        kmix = kmix + kgas(i) * GasFraction(i) / ((GasFraction(i) + sum) * Gas%Width + yi)
    END DO
    !
    gconr2 = kmix
    !
    END FUNCTION gconr2
    !
    !
    !
    REAL(r8k) FUNCTION gvisco (Gas)
    USE Kinds_fraptran
    USE conversions_fraptran
    USE variables_fraptran, ONLY : ounit, GasFraction
    IMPLICIT NONE
    !>@brief
    !> gvisco calculates the gas viscosity as a function of temperature and gas mole fractions.
    !>@author
    !> coded by r.c.young, March 1975
    !> Updated by I. Porter, NRC May 2015
    !
    ! Input
    !
    ! GasFraction(i) - mole fractions of the gas mixture. The seven elements of gases must sum to 1.0
    !              1 = helium
    !              2 = argon
    !              3 = krypton
    !              4 = xenon
    !              5 = hydrogen
    !              6 = nitrogen
    !              7 = air
    !              8 = steam - from 1967 asme at 1 bar pressure
    ! Gas%Temp       - gas temperature (K)
    !
    ! Output
    !
    ! gvisco         - Gas viscosity (kg/m-s)
    !
    ! Reference:
    !
    ! The gas viscosity relationships used in this function are those stated by:
    ! 1) bird,stewart,and lightfoot, "transport phenomena",p 26 and p 258 (1954).
    ! 2) bird, hirschfelder, curtiss, trans. asme 76 (1954) 1011-1038,
    ! 3) handbook of physics, section 5.5 (mcgraw-hill) 1954.
    ! Steam equation:
    ! 4) mayer, et al, "thermodynamic and transport properties of steam", the american society of mechanical engineers (1967)
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: l, i, j
    REAL(r8k) :: sum, rv, phi, vmix, rmoles, gij, tkoe, omega
    REAL(r8k), PARAMETER :: SQRT8 = SQRT(8.0_r8k)
    REAL(r8k), DIMENSION(ngases) :: vgas
    !
    ! Check for input errors. Input range requires 0.1K < Gas Temperature < 5555K
    ! and the molar fraction of gases must be > 0.0
    IF (Gas%Temp < tvLimitMin .OR. Gas%Temp > tvLimitMax .OR. MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0,180) Gas%Temp, GasFraction
        WRITE (ounit,180) Gas%Temp, GasFraction
180     FORMAT (' Error in gvisco2. Bad input. Gas%Temp= ',e12.4,' K',/,' Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    vmix = 0.0_r8k
    !
    ! Calculate the viscosity of each gas species
    DO i = 1, ngases
        !
        SELECT CASE (i)
        CASE (SteamIndex)
            ! Steam
            ! Set limit on minimum temperature allowed so that viscosity remains > 0.
            vgas(i) = (0.407_r8k * MAX(Gas%Temp, 75.61_r8k) - 30.77_r8k) * 1.0E-7_r8k
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            ! Viscosity = 2.6693E-6 * (MolWt * Temp) ^ 0.5 / (collision diameter ^2 * Omega)
            tkoe = MIN(Gas%Temp / ncGasProperties(i)%mufit%eok, 400.0_r8k)
            omega = polate(Omegavk, tkoe, nt)
            vgas(i) = 2.6693E-6_r8k * SQRT(ncGasProperties(i)%MWt * Gas%Temp) / ((ncGasProperties(i)%mufit%cld ** 2) * omega)
        CASE DEFAULT
            WRITE (0,101) i
            WRITE (ounit,101) i
101         FORMAT ('Error in subroutine gvisco. Wrong Gas ID called. GasID = ',i7)
            STOP
        END SELECT
    END DO
    !
    ! Calculate viscosity of gas mixture
    DO i = 1, ngases
        sum = 0.0_r8k
        DO j = 1, ngases
            ! MATPRO document does not say to use the i _fraptran/= j but calculations are inconsistent for single gas otherwise (IP)
            IF (i /= j) THEN
                rmoles = ncGasProperties(j)%MWt / ncGasProperties(i)%MWt
                gij = (1.0_r8k + SQRT(vgas(i) / vgas(j)) * (ncGasProperties(j)%MWt / ncGasProperties(i)%MWt) ** 0.25_r8k) ** 2 &
                  &   / (SQRT8 * SQRT(1.0_r8k + ncGasProperties(i)%MWt / ncGasProperties(j)%MWt))
                sum = sum + gij * GasFraction(j)
            END IF
        END DO
        vmix = vmix + vgas(i) * GasFraction(i) / (GasFraction(i) + sum)
    END DO
    !
    gvisco = vmix
    !
    END FUNCTION gvisco
    !
    !
    !
    REAL(r8k) FUNCTION CP (Gas)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calculates the specific heat of the fill gas
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/15/2015
    !
    ! Input
    !
    ! Gas%Temp  - Gas temperature, K
    ! Gas%Press - Plenum pressure, psi
    !
    ! Output
    !
    ! GasCP     - Gas specific heat, J/kg*K
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: i
    REAL(r8k) :: sum, cpa, PressPa, cpH2O, Temp
    REAL(r8k), PARAMETER :: C1 = 1.68835968E3_r8k
    REAL(r8k), PARAMETER :: C2 = 0.6029856_r8k
    REAL(r8k), PARAMETER :: C3 = 482.0979623_r8k
    REAL(r8k), PARAMETER :: C4 = 2.95317905e7_r8k
    REAL(r8k), PARAMETER :: C5 = 1.8_r8k
    REAL(r8k), PARAMETER :: C6 = 460.0_r8k
    REAL(r8k), PARAMETER :: TempMin = 273.15_r8k
    REAL(r8k), PARAMETER :: cpH2OMax = 1.0_r8k * BtulbFtoJkgK
    REAL(r8k), DIMENSION(ngases) :: cvgas
    !
    ! Check for errors
    IF (Gas%Temp < tvLimitMin .OR. Gas%Temp > tvLimitMax .OR. MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0, 180) Gas%Temp, GasFraction
        WRITE (ounit, 180) Gas%Temp, GasFraction
180     FORMAT (' Error in gascp. Bad input. Gas%Temp= ',e12.4,' K',/,' Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    ! Unit Conversions
    PressPa = Gas%Press * PSItoPa
    !
    ! Calculate specific heat at constant volume for each individual gas
    DO i = 1, ngases
        SELECT CASE  (i)
        CASE (SteamIndex)
            cvgas(SteamIndex) = 0.0_r8k
            ! Ensure temperature is above minimum temperature threshold
            Temp = MAX(Gas%Temp, TempMin)
            ! Specific heat of water
            cpH2O = C1 + C2 * Temp + (C3 * PressPa) / (C5 * Temp - C6) ** 2.4_r8k + (C4 * PressPa ** 3) / (C5 * Temp - C6) ** 9
            ! Prevent values outside of bounds of formula, caused by high pressures at low temperatures
            cpH2O = MIN(cpH2OMax, cpH2O)
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            IF (Gas%Temp <= ncGasProperties(i)%cvFit%T0) THEN
                ! Ideal specific heat. cv = c0
                cvgas(i) = ncGasProperties(i)%cvFit%c0
            ELSE
                ! Non-ideal specific heat. cv = c0 + d0 * (temp - T0)
                cvgas(i) = ncGasProperties(i)%cvFit%c0 + ncGasProperties(i)%cvFit%d0 * (Gas%Temp - ncGasProperties(i)%cvFit%T0)
            END IF
        CASE DEFAULT
            WRITE (0,101) i
            WRITE (ounit,101) i
101         FORMAT ('Error in subroutine GasCP. Wrong Gas ID called. GasID = ',i7)
            STOP
        END SELECT
    END DO
    cpa = 0.0_r8k
    !
    ! Calculate specific heat of gas mixture
    DO i = 1, ngases
        !
        SELECT CASE (i)
        CASE (SteamIndex)
            ! Steam
            ! If steam were NOT modeled as a NCGas, this would be bypassed.
            ! sum = 0.0_r8k
            ! However, steam is currenly modeled as a NCGas
            sum = GasFraction(i) * cpH2O
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            ! cp = Mole Fraction(i) * (spec heat @ constant volume(i) * gas constant(i))
            sum = GasFraction(i) * (cvgas(i) + ncGasProperties(i)%Rgas)
        END SELECT
        ! Cpa = SUM(cp(i),i=1,ngases)
        cpa = cpa + sum
    END DO
    !
    ! Account for contribution from steam
    IF (GasFraction(SteamIndex) > MinMoles) THEN
        ! Steam Density
        ! rhoH2O = GasRho (Gas%Temp, 'Steam')
        ! Gas Density
        ! rhoGas = GasRho (Gas%Temp, 'NCGases')
        ! If steam was NOT modeled as a NCGas, would need to use a weighted average_fraptran.
        ! Gascp = (rhoH2O * cpH2O + rhoGas * cpa) / (rhoH2O + rhoGas)
        ! However, steam is currently modeled as a NCGas
        CP = cpa
    ELSE
        ! No steam present.
        CP = cpa
    END IF
    !
    END FUNCTION CP
    !
    !
    !
    REAL(r8k) FUNCTION Rho (Gas)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calculates the gas density
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/23/2015
    !
    ! Input
    !
    ! Gas%Temp - plenum temperature, K
    !
    ! Output
    !
    ! GasRho   - gas density (kg/m^3)
    !
    ! Internal
    !
    ! x        - mass fraction
    !
    ! Gas constant calculated using the following formula:
    ! Rgas = SUM(Ra_i * x_i)
    !
    ! Gas Density calculated using the following formula:
    ! Density = Gas Pressure(Pa) / (Rgas(J/kg*K) * Gas Temperature(K)) 
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: i
    REAL(r8k) :: sum, Rgas, PressPa, x_tot
    REAL(r8k), DIMENSION(ngases) :: x
    !
    ! Check for errors
    IF (Gas%Temp < tvLimitMin .OR. Gas%Temp > tvLimitMax .OR. MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0,180) Gas%Temp, GasFraction
        WRITE (ounit,180) Gas%Temp, GasFraction
180     FORMAT (' Error in GasRho. Bad input. Gas%Temp= ',e12.4,' K',/,' Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    ! Unit Conversions
    PressPa = Gas%Press * PSItoPa
    !
    ! Calculate mass fraction
    x_tot = Gas%MWt ()
    DO i = 1, ngases
        x(i) = GasFraction(i) * ncGasProperties(i)%MWt / x_tot
    END DO
    !
    ! Calculate effective gas constant
    Rgas = 0.0_r8k
    DO i = 1, ngases
        SELECT CASE (i)
        CASE (SteamIndex)
            ! Steam is currently being modeled as a NCGas
            sum = ncGasProperties(i)%Rgas * x(i)
        CASE (HeIndex, ArIndex, KrIndex, XeIndex, H2Index, N2Index, AirIndex)
            sum = ncGasProperties(i)%Rgas * x(i)
        END SELECT
        Rgas = Rgas + sum
    END DO
    !
    Rho = PressPa / (Rgas * Gas%Temp)
    !
    END FUNCTION Rho
    !
    !
    !
    REAL(r8k) FUNCTION MWt (Gas)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calculates the gas molecular weight
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/19/2015
    !
    ! Input
    !
    ! GasFraction - Array for storing the molar fraction of each gas species
    ! ngases      - Number of gas species modeled
    !
    ! Output
    !
    ! GasMWt      - Gas molecular weight (g/mol)
    !
    ! Molecular weight calculated using the following formula:
    ! MWt = SUM(MWt_i * MoleFraction_i)
    !
    CLASS (NonCondGases) :: Gas
    INTEGER(ipk) :: i
    REAL(r8k) :: MWt_i
    !
    ! Check for errors
    IF (MAXVAL(GasFraction) <= MinMoles) THEN
        WRITE (0,180) GasFraction
        WRITE (ounit,180) GasFraction
180     FORMAT (' Error in GasMWt. Bad input. Gas Fractions ',7(e12.4,3x))
        STOP
    END IF
    !
    MWt = 0.0_r8k
    DO i = 1, ngases
        MWt_i = ncGasProperties(i)%MWt * GasFraction(i)
        MWt = MWt + MWt_i
    END DO
    !
    END FUNCTION MWt
    !
    !
    !
    SUBROUTINE Allocate_Gas
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This subroutine allocates the gas array
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/15/2015
    !
    ! Input
    !
    ! GasFraction - Array for storing the molar fraction of each gas species
    ! i           - Rod ID being modeled
    ! ngases      - Number of gas species modeled
    
    ! Allocate the arrays
    ALLOCATE (GasFraction(1:ngases))
    ALLOCATE (gsmol0(1:ngases))
    
    ! Set the default values
    GasFraction = 0.0_r8k    
    gsmol0 = 0.0_r8k
    
    END SUBROUTINE Allocate_Gas
    !
END MODULE NCGases_fraptran













