MODULE Dyna_h_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comdeck Dyna_fraptran
    TYPE dyna_var
        ! Temperature convergence indicator at each axial node
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexTempConverg
        ! Radial power distribution at a given node
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadialPower
        ! Radial bounding fuel & cladding dimensions at a given node
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadialBound
        ! maximum energy half mesh ll can absorb in melting (btu/ft**3)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: qmaxmelt
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: qmaxmeltp1
        ! Coolant mass flux
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: rmassflux                           
        ! Coolant quality
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coolqual
        ! Part of surface B.C. equation
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AAHT1
        ! Part of surface B.C. equation
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: BBHT1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasPress0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolPress
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasMolesAx
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasMolesAx0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasPress
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSurfT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelCenterT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: VolOpenPor
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapThick
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapThick0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelletRad
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelletRad0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HeatFlux
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HeatFlux0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SurfHtFlux
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladMaxT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Vreloc
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Vreloc0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelSrfDispl
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldPelDis
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldPelDis0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiThk1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiThk2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: DishVperL
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFlux
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk11
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk22
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: oxideid
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrain0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: oxideod
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermStrn
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermStrn0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SSHgap
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladAveTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldGasPrs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldGasPrs0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HtFlxFac
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelAxStrn
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodOD
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldAxStrn
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldAxStrn0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodOD0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladT0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelAxStrn0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: TimeofGBSep
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SwellDispl
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HmaxInitial
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacPrs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacPrs0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HGapAv
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FilmCoeffAv
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialPowr
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialPowr0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CritHtFlux
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSrfStrRat
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WatrMetlEnrgy
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacGap
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladSurfT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EDotFZ
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelSrfStrn0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSrfStrRat0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EDotFZ0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EnergyPerL
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HFXSum
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEnrgPerL
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolMassFlx
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Enthl
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelRadDeviat
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermAxStrn
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: VoidVolumeRatio
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermHoopStrn 
        ! Equivalent Cladding Reacted
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ECR
        ! Equals OxUpTakeID2
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxUptakeID1
        ! Oxygen Uptake for B-J or C-P Model
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxUpTakeID2
        ! SED using PNNL E-P eff. strain, MJ/m3
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDPNNL
        ! PNNL elastic-plastic effective strain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrainPNNL
        ! k coefficient
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefk
        ! n coefficient
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefn
        ! m coefficient
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefm
        ! Elastic Modulus
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Emodulus
        ! Strain rate coefficient
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: strainrateterm
        ! SED based on EPRI's formulation
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDEPRI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffFastFluStrnHardExp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: BetaThickness
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffColdWkStrnHardExp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxygenConcenAve
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffColdWkStrenCoef
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxygenUptake
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxStAlphaThkRemain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxStAlphaThkNearFuel
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffFastFluStrenCoef
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiPowerGen
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrcntSatBeta
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAAO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenABO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenACO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenADO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAEO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAFO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAGO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAHO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAIO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAAI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenABI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenACI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenADI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAEI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAFI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAGI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAHI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAII
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackVolume
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AveDishTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CentVoidVol
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ExtentOfBow
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialNodLen
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: TerfacePres
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackWidth
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EinstabilStrain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxBurnup
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSOxideThick
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSOxideThick
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorVol
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorosity
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEffStress
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: BulkCoolTemp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladYieldStress
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressAtInstStrain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: techf
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CesiumContent
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HydrogenContent
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tschf
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceGAPI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceEP1
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceURC
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceTCMx
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceGAP
        ! User option in namelist $boundary to specify axial nodes at which film boiling is prescribed.
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: nodchf
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: BOSTemp
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EOSTemp
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EOSRad
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: BOSRad
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltP1
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltZ
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMelt
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltZp1
        ! Radial node coordinates for each radial node x axial node
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RadialBoundO
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: bufrad
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: DeformedRadiusOfMesh
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: WorkSpaceEPP1
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: buradv
        !  Mechanical Models Variables
        REAL(r8k) :: gapmin
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpacePINT
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceRCI
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceRCO
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceReloc
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxNodElevat
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ureloc
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: a1
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldElStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldPlasStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldPlasStrn0
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrnRat
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrnRat0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HydrostatPress
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FuelResidStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FuelResidStrn0
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: Baln2Twall
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldresidStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldResidStrn0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: IodineContent
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: PelSrfStrn
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStress
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldThermStrn
        REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: GRsv
        INTEGER(ipk) :: Ifchk
        ! # of radial nodes in cladding
        INTEGER(ipk) :: nce
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexPCMI
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexPCMIOnce
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: NumAzmuthNod
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ichf
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ih
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ihtreg
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: GapIndex
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: BOSGapIndex
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: BOSGapIndex0
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: RuptFailIndex
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: CladCollapseIndex
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: CladCollIndx0
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: OldGapIndex
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: NodeSinterTemp
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: RodFailIndex
    END TYPE dyna_var
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexTempConverg
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadialPower
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadialBound
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qmaxmelt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qmaxmeltp1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rmassflux
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: coolqual
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AAHT1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BBHT1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasPress0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolPress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasMolesAx
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasMolesAx0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasPress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSurfT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelCenterT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: VolOpenPor
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapThick0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GapTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelletRad
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelletRad0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HeatFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HeatFlux0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SurfHtFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladMaxT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Vreloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Vreloc0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelSrfDispl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldPelDis
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldPelDis0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiThk1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiThk2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: DishVperL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FastFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk11
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AlphaThk22
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oxideid
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrain0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oxideod
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SSHgap
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCoolPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladAveTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldGasPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldGasPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HtFlxFac
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodOD
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldAxStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodOD0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCladT0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldFuelAxStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: TimeofGBSep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SwellDispl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HmaxInitial
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HGapAv
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FilmCoeffAv
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialPowr
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialPowr0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CritHtFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSrfStrRat
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WatrMetlEnrgy
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RInterfacGap
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladSurfT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EDotFZ
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelSrfStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelSrfStrRat0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EDotFZ0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EnergyPerL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HFXSum
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEnrgPerL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolEnthalpy0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolMassFlx
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Enthl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CoolDensity0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PelRadDeviat
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: VoidVolumeRatio
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CldPermHoopStrn 
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ECR
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxUptakeID1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxUpTakeID2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDPNNL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffStrainPNNL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefk
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: coefm
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: Emodulus
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: strainrateterm
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDEPRI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffFastFluStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BetaThickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffColdWkStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxygenConcenAve
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffColdWkStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxygenUptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxStAlphaThkRemain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxStAlphaThkNearFuel
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EffFastFluStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxiPowerGen
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrcntSatBeta
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAAO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenABO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenACO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenADO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAEO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAFO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAGO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAHO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAIO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAAI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenABI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenACI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenADI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAEI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAFI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAGI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAHI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OxConcenAII
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackVolume
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AveDishTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CentVoidVol
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ExtentOfBow
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxialNodLen
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: TerfacePres
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CrackWidth
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EinstabilStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxBurnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BOSOxideThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EOSOxideThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorVol
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OpenPorosity
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEffStress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BulkCoolTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladYieldStress
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressAtInstStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: techf
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CesiumContent
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HydrogenContent
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tschf
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceGAPI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceEP1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceURC
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceTCMx
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceGAP
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: nodchf
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: BOSTemp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EOSTemp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EOSRad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: BOSRad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltP1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltZ
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMelt
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: EnrgyMeltZp1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RadialBoundO
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: bufrad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: DeformedRadiusOfMesh
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: WorkSpaceEPP1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: buradv
    REAL(r8k) :: gapmin
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpacePINT
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceRCI
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceRCO
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: WorkSpaceReloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: AxNodElevat
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ureloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: a1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldElStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldPlasStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldPlasStrn0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrnRat
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStrnRat0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HydrostatPress
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FuelResidStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FuelResidStrn0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: Baln2Twall
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldresidStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldResidStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: IodineContent
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: PelSrfStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldStress
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: CldThermStrn
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE ::GRsv
    INTEGER(ipk) :: Ifchk
    INTEGER(ipk) :: nce
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexPCMI
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: IndexPCMIOnce
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: NumAzmuthNod
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ichf
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ih
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Ihtreg
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: GapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: BOSGapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: BOSGapIndex0
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: RuptFailIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: CladCollapseIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: CladCollIndx0
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: OldGapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: NodeSinterTemp
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: RodFailIndex
    !
    END MODULE Dyna_h_fraptran













