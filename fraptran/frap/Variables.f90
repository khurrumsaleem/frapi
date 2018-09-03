MODULE Variables
    USE Kinds
    USE bcdcom_h
    USE bloon_h
    USE collct_h
    USE CoolantProperties
    USE Dyna_h
    USE excb_h
    USE FissionGasRelease_h
    USE frapc
    USE HeatConduction_h
    USE htcb_h
    USE modcom_h
    USE phypro_h
    USE resti_h
    USE scalr_h
    IMPLICIT NONE
    !
    REAL(r8k) :: maxfueltemp = 0.0_r8k
    !
    REAL(r8k) :: maxcladtemp = 0.0_r8k
    !
    REAL(r8k) :: maxgaspressure = 0.0_r8k
    !
    REAL(r8k) :: maxenthalpy = 0.0_r8k
    ! axial nodes, radial nodes, and input variable array sizes
    INTEGER(ipk) :: pre_na, pre_nr, pre_nt
    ! # of void volumes
    INTEGER(ipk), PARAMETER :: n_void_volumes = 10_ipk
    ! Indicates which fuel rod is currently being modeled
    INTEGER(ipk) :: idx
    ! Units
    ! Input File. Default = 55
    INTEGER(ipk), PARAMETER :: iunit = 55
    ! Scratch unit. Fixed to 5
    INTEGER(ipk), PARAMETER :: scrunit = 5
    ! Output File. Default = 6
    INTEGER(ipk), PARAMETER :: ounit = 6
    ! Plot File. Default = 66
    INTEGER(ipk), PARAMETER :: plotunit = 66
    ! FRAPTRAN Restart File. Default = 2
    INTEGER(ipk), PARAMETER :: frtrunit = 2
    ! FRAPCN Restart File. Default = 22
    INTEGER(ipk), PARAMETER :: fcunit = 22
    ! Water Properties File. Default = 15
    INTEGER(ipk), PARAMETER :: h2ounit = 15
    ! Dakota File. Default = 77
    INTEGER(ipk), PARAMETER :: dakotaunit = 77
    !
    CHARACTER(LEN=*), PARAMETER :: codeid = &                       ! Stores the code ID. replaces common block CodeIdent
      &  'FRAPTRAN-2.0 May 26, 2016'
    TYPE Variables_var
        ! INTEGERs
        INTEGER(ipk) :: ntimesteps                          ! # of timesteps
        INTEGER(ipk) :: naxialnodes                         ! # of axial nodes
        INTEGER(ipk) :: nradialnodes                        ! # of radial nodes
        INTEGER(ipk) :: iplant                              ! Plant type flag.
        INTEGER(ipk) :: gammait                             ! # of iterations for gamma ray heating calculation
        INTEGER(ipk) :: maxidx                              ! Comdeck lacmdl
        INTEGER(ipk) :: nthermex                            ! Comdeck nthermex
        ! Real
        ! Elastic Modulus
        REAL(r8k) :: e
        ! Poisson's Ratio
        REAL(r8k) :: pois                                   
        REAL(r8k) :: modheat                                ! Moderator heating fraction
        REAL(r8k) :: DefaultTemp                            ! Default Temp. = 77.0F
        REAL(r8k) :: dtmpcl                                 ! Comdeck matprc
        REAL(r8k) :: tref                                   ! Comdeck reftemp
        ! LOGICAL
        ! Print rod burst
        LOGICAL :: printrodburst
        ! Print ballooning
        LOGICAL :: printballoon
        ! Character
        ! Moderator heating model
        CHARACTER(LEN=15) :: ModHeatModel
        ! **Arrays**
        ! INTEGER
        ! DIMENSION = 3
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nm
        ! User option in namelist $boundary to specify coolant temperature history for zones
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: htco
        ! User option in namelist $boundary to specify heat transfer coefficient history for zones
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: tem
        ! Real
        ! User specified gadolinia content (axially dimensioned) in namelist $design
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gadoln
        ! Used in prntot to store old effective plastic strain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldepeff
        ! Used in prntot to store old cladding hoop strain
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldeps
        ! User supplied in namelist %boundary to specify coolant pressure and time data pairs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh1
        ! User supplied in namelist %boundary to specify coolant pressure and time data pairs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh2
        ! User supplied in namelist %boundary to specify collapsed liquid level and time data pairs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hlqcl
        ! User supplied in namelist %boundary to specify flow shroud temperature and time data pairs
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ts
        ! comdeck frpsto. 40000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: afrap
        ! 1. Comdeck matprc. Used only in restart files.
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: amatpc
        ! 11. Comdeck indx. Not used
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ispace
        ! DIMENSION = 21. Comdeck lacmdl
        CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE :: emflag
        ! Used in prntot
        INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: iok
        ! Used in prntot
        INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: iokold
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FrapconTemp
        ! 20, 20. Comdeck qconb
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: qcold
    END TYPE Variables_var
    !
    INTEGER(ipk) :: ntimesteps
    INTEGER(ipk) :: naxialnodes
    INTEGER(ipk) :: nradialnodes
    ! Integers
    INTEGER(ipk) :: iplant
    INTEGER(ipk) :: gammait
    INTEGER(ipk) :: maxidx
    INTEGER(ipk) :: nthermex
    ! Reals
    REAL(r8k) :: e
    REAL(r8k) :: pois
    REAL(r8k) :: modheat
    REAL(r8k) :: DefaultTemp
    REAL(r8k) :: dtmpcl
    REAL(r8k) :: tref
    ! Logicals
    LOGICAL :: printrodburst
    LOGICAL :: printballoon
    ! Characters
    ! Moderator heating model
    CHARACTER(LEN=15) :: ModHeatModel
    ! **Arrays**
    ! Timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pbh2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hlqcl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ts
    ! Axial Nodes
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: htco
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: tem
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gadoln
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldepeff
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldeps
    ! 2-D Arrays
    ! Used in prntot
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: iok
    !
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: iokold
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: FrapconTemp
    ! 20,20. Comdeck qconb
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: qcold
    ! Fixed Arrays
    ! Dimension = 3
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nm
    ! comdeck frpsto. DIMENSION = 40000
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: afrap
    ! 1. Comdeck matprc. Used only in restart files. Unclear If Real or INTEGER
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: amatpc
    ! Comdeck indx. Not used.
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ispace
    ! Dimension = 21. Comdeck lacmdl
    CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE :: emflag
    !comdeck thcntl
    TYPE thcntl_var
        !
        INTEGER(ipk) :: ncards
        !
        INTEGER(ipk) :: NRestart
        INTEGER(ipk) :: defsize
        ! Indicates type of coolant option to use. Default ncool=1. If ncool=7, coupled with T/H Code
        INTEGER(ipk) :: ncool
        !
        INTEGER(ipk) :: ndtad
        !
        REAL(r8k) :: t1
        !
        REAL(r8k) :: t2
        !
        LOGICAL :: unit
    END TYPE thcntl_var
    !
    INTEGER(ipk) :: ncards
    !
    INTEGER(ipk) :: NRestart
    INTEGER(ipk) :: defsize
    ! Indicates type of coolant option to use. Default ncool=1. If ncool=7, coupled with T/H Code
    INTEGER(ipk) :: ncool
    !
    INTEGER(ipk) :: ndtad
    !
    REAL(r8k) :: t1
    !
    REAL(r8k) :: t2
    !
    LOGICAL :: unit
    !comdeck materials
    TYPE material_var
        INTEGER(ipk) :: nomat                               ! # of materials
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imaterials!100
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imatflag
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imatflag1
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: iDatapairs
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: iheattablen
    END TYPE material_var
    ! # of materials
    INTEGER(ipk) :: nomat
    ! 100
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imaterials
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imatflag
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: imatflag1
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: iDatapairs
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: iheattablen
    ! The parameters below are used to identify code specifics for output information.
    ! Namely, the problem end time, print & graphics intervals
    TYPE Code_var
        INTEGER(ipk) :: ntimes                              ! # of time history pairs from T/H Code
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tEnd        ! End Time
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gfInt       ! Time interval to print to plot file
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: edInt       ! Time interval to print to output file
        CHARACTER(LEN=100) :: Title                         ! Stores the title information
    END TYPE Code_var
    ! Note: ntimes, tEnd, gfInt & edInt are stored in frapc
    CHARACTER(LEN=100) :: Title                             ! Stores the title information
    ! These are from sth2x
    TYPE waterprop_var
        INTEGER(ipk) :: nt
        INTEGER(ipk) :: np
        INTEGER(ipk) :: ns
        INTEGER(ipk) :: ns2
        INTEGER(ipk) :: klp
        INTEGER(ipk) :: klp2
        INTEGER(ipk) :: llp
        INTEGER(ipk) :: nt5
        INTEGER(ipk) :: jpl
    END TYPE waterprop_var
    !
    INTEGER(ipk) :: nt
    INTEGER(ipk) :: np
    INTEGER(ipk) :: ns
    INTEGER(ipk) :: ns2
    INTEGER(ipk) :: klp
    INTEGER(ipk) :: klp2
    INTEGER(ipk) :: llp
    INTEGER(ipk) :: nt5
    INTEGER(ipk) :: jpl
    ! These are used in the store6 subroutine
    TYPE store6_var
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDPNNLold
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldEffStrainPNNL
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDEPRIold
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain1old
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain2old
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain3old
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressHoopOld
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressRadialOld
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressAxialOld
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEffStressOld
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hsolold
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldPlasStrn
    END TYPE store6_var
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDPNNLold
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: oldEffStrainPNNL
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: SEDEPRIold
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain1old
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain2old
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: EPStrain3old
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressHoopOld
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressRadialOld
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: StressAxialOld
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladEffStressOld
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hsolold
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: OldCldPlasStrn
    !
    !
    !
    !>@brief
    !> This Module replaces the comdeck numcom
    TYPE numcom_var
        INTEGER(ipk) :: idebug         !
        INTEGER(ipk) :: ncmesh         ! User specified # of equal area nodes in the cladding
        INTEGER(ipk) :: nfmesh         ! User specified # of equal area nodes in the fuel
        INTEGER(ipk) :: nsc            ! =36. # of specific heat vs temperature pairs for clad
        INTEGER(ipk) :: nsf            ! =20. # of specific heat vs temperature pairs for fuel
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: cmesh          ! User specified cladding radial node spacing
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: fmesh          ! User specified fuel radial node spacing 
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: zelev          ! User specified axial node spacing
    END TYPE numcom_var
    !
    INTEGER(ipk) :: idebug
    INTEGER(ipk) :: ncmesh
    INTEGER(ipk) :: nfmesh
    INTEGER(ipk) :: nsc
    INTEGER(ipk) :: nsf
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cmesh
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fmesh
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: zelev
    !
    !
    !
    TYPE powrd_var
        !
        INTEGER(ipk) :: nprofile
        !
        INTEGER(ipk) :: NumAxProfiles
        !
        REAL(r8k) :: apowrd !Used only in restart files but unclear if real or integer
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ProfileStartTime
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodAvePower
        ! Dimension is product of 2*151 - 151 pairs to define each axial profile (one per axial node)
        ! and up to 25 profiles
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: AxPowProfile
        ! Dimension is product of 2*51 - 51 pairs to define each radial  profile, and up to 151 profiles (one per axial node)
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: pazp
    END TYPE powrd_var
    !
    INTEGER(ipk) :: nprofile
    INTEGER(ipk) :: NumAxProfiles
    REAL(r8k) :: apowrd
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ProfileStartTime
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RodAvePower
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: AxPowProfile
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: pazp
    !
    !>@brief
    !> This module replaces the powcom common block.
    !> It holds the radial power profile & cladding power.
    TYPE powcom_var
        ! # of pairs of radial power profiles values supplied in RadPowProfile
        INTEGER(ipk) :: nprad
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadPowProfile
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladdingPower
    END TYPE powcom_var
    !
    INTEGER(ipk) :: nprad
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadPowProfile
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: CladdingPower
    !
    !
    !
    TYPE prntb_var
        !
        INTEGER(ipk) :: itcntd
        !
        REAL(r8k) :: crfpr
        !
        REAL(r8k) :: zqchpr
        !
        REAL(r8k) :: reflpr
        !
        REAL(r8k) :: fldrpr
        !
        REAL(r8k) :: TimeIncrement
        !
        REAL(r8k) :: tplenb
        ! Only used in init6 & restrw to store/read a variable but is not used anywhere else so unsure if real or integer
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: aprntb
    END TYPE prntb_var
    !
    INTEGER(ipk) :: itcntd
    REAL(r8k) :: crfpr
    REAL(r8k) :: zqchpr
    REAL(r8k) :: reflpr
    REAL(r8k) :: fldrpr
    REAL(r8k) :: TimeIncrement
    REAL(r8k) :: tplenb
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: aprntb
    !
    !
    !
    TYPE presb_var
        INTEGER(ipk) :: Kswell
        INTEGER(ipk) :: Ifail
        REAL(r8k) :: vplenc
        REAL(r8k) :: tplen
        REAL(r8k) :: VolAveGasTemp
        REAL(r8k) :: pctop
        REAL(r8k) :: dvdtp
        REAL(r8k) :: vplenb
        REAL(r8k) :: gfloa1
        REAL(r8k) :: roughf
        REAL(r8k) :: roughc
        REAL(r8k) :: swllfr
        REAL(r8k) :: TotalVoidVol
        ! Arrays
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ies
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pswll0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: roi
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vs0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: flowg
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasAx
    END TYPE presb_var
    !
    INTEGER(ipk) :: Kswell
    INTEGER(ipk) :: Ifail
    REAL(r8k) :: vplenc
    REAL(r8k) :: tplen
    REAL(r8k) :: VolAveGasTemp
    REAL(r8k) :: pctop
    REAL(r8k) :: dvdtp
    REAL(r8k) :: vplenb
    REAL(r8k) :: gfloa1
    REAL(r8k) :: roughf
    REAL(r8k) :: roughc
    REAL(r8k) :: swllfr
    REAL(r8k) :: TotalVoidVol
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ies
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pswll0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: roi
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: flowg
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: GasAx
    !
    !
    !
    TYPE Oxidation_var
        INTEGER(ipk) :: CladType
        INTEGER(ipk) :: ProtectiveOxide
        INTEGER(ipk) :: npair
        INTEGER(ipk) :: nIDoxide
        REAL(r8k) :: cexh2l
        REAL(r8k) :: explenumv
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: cexh2a
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltox
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: explenumt
    END TYPE Oxidation_var
    !
    ! User specified cladding type
    INTEGER(ipk) :: CladType
    ! User option to model initial oxide as protective(0) or non-protective(1)
    INTEGER(ipk) :: ProtectiveOxide
    ! # of pairs for user supplied plenum temperature
    INTEGER(ipk) :: npair
    ! User option for something. Not described in FRAPTRAN-1.4 manual
    INTEGER(ipk) :: nIDoxide
    ! Cladding hydrogen concentration for axial node k (ppm)
    REAL(r8k) :: cexh2l
    ! External plenum volume
    REAL(r8k) :: explenumv
    ! User specified cladding hydrogen concentration for each axial node (ppm)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: cexh2a
    ! Temperature drop across the oxide layer
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: deltox
    ! User option for exterior plenum temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: explenumt
    !
    !
    !
    TYPE inpdat_var
        INTEGER(ipk) :: maxit            ! User Specified maximum # of iterations in SS temperature solution
        INTEGER(ipk) :: IndexFinTemp     ! POINTER to final temperature array
        INTEGER(ipk) :: indxpr           ! Not assigned a value. All arrays that used this DIMENSION are commented out
        INTEGER(ipk) :: IndexGeom        ! POINTER to geometry array
        INTEGER(ipk) :: IndexThermCon    ! POINTER to thermal conductivity array
        INTEGER(ipk) :: IndexThermConAdv ! POINTER to advanced conductivity array
        INTEGER(ipk) :: IndexBC          ! POINTER to boundary condition array
        INTEGER(ipk) :: noiter           ! User Specified maximum # of iterations in transient temperature solution
        INTEGER(ipk) :: Iflago
        INTEGER(ipk) :: Iflagn
    END TYPE inpdat_var
    !
    INTEGER(ipk) :: maxit
    INTEGER(ipk) :: IndexFinTemp
    INTEGER(ipk) :: indxpr
    INTEGER(ipk) :: IndexGeom
    INTEGER(ipk) :: IndexThermCon
    INTEGER(ipk) :: IndexThermConAdv
    INTEGER(ipk) :: IndexBC
    INTEGER(ipk) :: noiter
    INTEGER(ipk) :: Iflago
    INTEGER(ipk) :: Iflagn
    !
    !
    !
    TYPE debug_var
        REAL(r8k) :: Time
        REAL(r8k) :: DebugTime
        REAL(r8k) :: DebugTimeStop
    END TYPE debug_var
    !
    REAL(r8k) :: Time
    REAL(r8k) :: DebugTime
    REAL(r8k) :: DebugTimeStop
    LOGICAL :: Ndebug ! Flag for specifying if debug information should be written
    !
    !
    !
    TYPE carcom_var
        INTEGER(ipk) :: ierr
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ibnopt  !33
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nopt    !8
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nunopt  !14
    END TYPE carcom_var
    !
    INTEGER(ipk) :: ierr
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ibnopt
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nopt
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: nunopt
    !
    !
    !
    TYPE iocom_var
        INTEGER(ipk) :: npltn     !
        REAL(r8k) :: tplot        ! Next plotting time
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtplta    ! Plot interval vs time pairs
    END TYPE iocom_var
    !
    INTEGER(ipk) :: npltn     !
    REAL(r8k) :: tplot        !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtplta    ! 40
    !
    !
    !
    TYPE dialb_var
        ! Gap conductance multiplier (1 - 20) and ballooning HTC correction factor (22 - 25)
        ! This needs to be re-dimensioned to naxn + 10
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: fdial
    END TYPE dialb_var
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fdial
    !
    !
    !
    TYPE intcom_var
        INTEGER(ipk) :: nepp0
        REAL(r8k) :: cladid
        REAL(r8k) :: cladod
        REAL(r8k) :: cladtk
        REAL(r8k) :: rf
        REAL(r8k) :: FuelPelDiam
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: eppinp
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: radpel
    END TYPE intcom_var
    !
    INTEGER(ipk) :: nepp0
    REAL(r8k) :: cladid
    REAL(r8k) :: cladod
    REAL(r8k) :: cladtk
    REAL(r8k) :: rf
    REAL(r8k) :: FuelPelDiam
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: eppinp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: radpel
    !
    TYPE Output_var
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: efuelref
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ein0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: eout0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: efuel0
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: eclad0
    END TYPE Output_var
    !
    CONTAINS
    !
    SUBROUTINE Allocate_Variables (ntimepairs, radial, axial)
    !>@brief
    !> This subroutine allocates the FRAPTRAN variables
    !>@author
    !> Coded by Ian Porter, NRC  May 2014
    !
    INTEGER(ipk), INTENT(IN) :: ntimepairs, radial, axial
    !
    CALL Allocate_Variable_Sizes
    CALL Allocate_Variables_var
    CALL Allocate_Oxidation_var
    CALL Allocate_Code_var (ntimepairs)
    CALL Allocate_Phypro_var
    CALL Allocate_Powcom_var
    CALL Allocate_Debug_var
    CALL Allocate_Inpdat_var
    CALL Allocate_Modcom_var
    CALL Allocate_Bcdcom_var
    CALL Allocate_Dialb_var
    CALL Allocate_Carcom_var
    CALL Allocate_WaterProp_var
    CALL Allocate_Scalr_var
    CALL Allocate_Prntb_var
    CALL Allocate_Presb_var
    CALL Allocate_Powrd_var
    CALL Allocate_Numcom_var
    CALL Allocate_Bloon_var
    CALL Allocate_Collct_var
    CALL Allocate_CoolantProperties
    CALL Allocate_Dyna_var
    CALL Allocate_Excb_var
    CALL Allocate_FGR
    CALL Allocate_Heatconduction_var
    CALL Allocate_Htcb_var
    CALL Allocate_Intcom_var
    CALL Allocate_Iocom_var
    CALL Allocate_Resti_var
    CALL Allocate_Material_var
    CALL Allocate_store6_var
    CALL Allocate_thcntl_var
    CALL Allocate_frapc_var (radial, axial)
    !
    END SUBROUTINE Allocate_Variables
    !
    !
    !
    SUBROUTINE Allocate_Oxidation_Var
    !>@brief
    !> This subroutine allocates the oxidation variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/8/2014

    ! Allocate arrays
    ALLOCATE (cexh2a(1:naxialnodes))
    ALLOCATE (deltox(1:naxialnodes))
    ALLOCATE (explenumt(1:2*ntimesteps))
    
    ! Set the default values
    CladType = 0
    ProtectiveOxide = 0
    npair = 0
    nIDoxide = 0
    cexh2l = 0.0_r8k
    explenumv = 0.0_r8k
    cexh2a = 0.0_r8k
    deltox = 0.0_r8k
    explenumt = -1.0_r8k
    
    END SUBROUTINE Allocate_Oxidation_Var
    !
    SUBROUTINE Allocate_Code_var (ntimepairs)
    !>@brief
    !> This subroutine allocates the code variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/9/2014
    
    INTEGER(ipk) :: ntimepairs !# of timesteps set by T/H Code
    
    ! Allocate arrays
    ALLOCATE (tEnd(1:ntimepairs))
    ALLOCATE (gfInt(1:ntimepairs))
    ALLOCATE (edInt(1:ntimepairs))
    
    ! Set default values
    ntimes = ntimepairs
    tEnd = 0.0_r8k
    gfInt = 0.0_r8k
    edInt = 0.0_r8k
    title = ' '
    
    END SUBROUTINE Allocate_Code_var
    !
    SUBROUTINE Allocate_Phypro_var
    !>@brief
    !> This subroutine allocates the phypro variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/9/2014

    ! Set default values
    imox = 0_ipk
    ftmelt = 0.0_r8k
    fhefus = 0.0_r8k
    ctmelt = 0.0_r8k
    chefus = 0.0_r8k
    fdelta = 0.0_r8k
    bumtp = 0.0_r8k
    compmt = 0.0_r8k
    deloxy = 0.0_r8k
    
    END SUBROUTINE Allocate_Phypro_var
    !
    SUBROUTINE Allocate_Powcom_var
    !>@brief
    !> This subroutine allocates the powcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/9/2014
    
    INTEGER(ipk):: size1
    
    ! Set size
    size1 = 2 * naxialnodes * ntimesteps
    
    ! Allocate the arrays
    ALLOCATE (CladdingPower(1:naxialnodes))
    ALLOCATE (RadPowProfile(1:size1))
    
    ! Set default values
    nprad = 0
    CladdingPower = 0.0_r8k
    RadPowProfile = 0.0_r8k
    
    END SUBROUTINE Allocate_Powcom_var
    !
    SUBROUTINE Allocate_Debug_var
    !>@brief
    !> This subroutine allocates the debug variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/9/2014

    ! Set default values
    Time = 0.0_r8k
    DebugTime = 1.0E20_r8k
    DebugTimeStop = 0.0_r8k
    
    END SUBROUTINE Allocate_Debug_var
    !
    SUBROUTINE AllocatE_Inpdat_var
    !>@brief
    !> This subroutine allocates the inpdat variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/9/2014
    
    ! Set default values
    maxit = 200
    IndexFinTemp = 0
    indxpr = 0
    IndexGeom = 0
    IndexThermCon = 0
    IndexThermConAdv = 0
    IndexBC = 0
    noiter = 0
    Iflago = 0
    Iflagn = 0
    
    END SUBROUTINE Allocate_Inpdat_var
    !
    SUBROUTINE Allocate_Modcom_var
    !>@brief
    !> This subroutine allocates the modcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/11/2014
    
    ! Set default values
    mwork = 0
    mtabl1 = 0
    mtabl2 = 0
    mtabl3 = 0
    mgaspr = 0
    trise = 10.0_r8k
    
    END SUBROUTINE Allocate_Modcom_var
    !
    SUBROUTINE Allocate_bcdcom_var
    !>@brief
    !> This subroutine allocates the bcdcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (ndchfi(1:naxialnodes))
    ALLOCATE (bowrat(1:naxialnodes))
    ALLOCATE (techfi(1:naxialnodes))
    ALLOCATE (tschfi(1:naxialnodes))
    ALLOCATE (hlqcl1(1:ntimesteps))
    
    ! Set default values
    jchf = 0
    mbowr = 0
    mgbh = 0
    mhbh = 0
    mhinta = 0
    mhlqc1 = 0
    mhlqcp = 0
    mhupta = 0
    mndchf = 0
    mpbh = 0
    mprest = 0
    mtempt = 0
    nbowr = 0
    ncooli = 0
    nhtc = 0
    nbhtc = 0
    jtr = 0
    achn = 0.0_r8k
    dhe = 0.0_r8k
    dhy = 0.0_r8k
    jhtc = 0.0_r8k
    Radiation = 'OFF'
    ndchfi = 0
    bowrat = 0.0_r8k
    techfi = 0.0_r8k
    tschfi = 0.0_r8k
    hlqcl1 = 0.0_r8k
    
    END SUBROUTINE Allocate_bcdcom_var
    !
    SUBROUTINE Allocate_dialb_var
    !>@brief
    !> This subroutine allocates the Dialb variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (fdial(1:30))
    
    ! Set the default values
    fdial = 1.0_r8k
    
    END SUBROUTINE Allocate_dialb_var
    !
    SUBROUTINE Allocate_carcom_var
    !>@brief
    !> This subroutine allocates the Carcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (ibnopt(1:33))
    ALLOCATE (nopt(1:8))
    ALLOCATE (nunopt(1:14))
    
    ! Set the default values
    ierr = 0
    ibnopt = 0
    nopt = 0
    nunopt = 0
    
    END SUBROUTINE Allocate_Carcom_var
    !
    SUBROUTINE Allocate_WaterProp_var
    !>@brief
    !> This subroutine allocates the sth2x variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Set the default values
    nt = 0
    np = 0
    ns = 0
    ns2 = 0
    klp = 0
    klp2 = 0
    llp = 0
    nt5 = 0
    jpl = 0
    
    END SUBROUTINE Allocate_WaterProp_var
    !
    SUBROUTINE Allocate_scalr_var
    !>@brief
    !> This subroutine allocates the Scalr variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (idumr1(1:7))
    ALLOCATE (vplen(1))
    ALLOCATE (apln0(1))
    ALLOCATE (bpln0(1))
    ALLOCATE (vsn(1))
    ALLOCATE (bu(1))
    ALLOCATE (gappr0(1))
    ALLOCATE (scd(1))
    ALLOCATE (spl(1))
    ALLOCATE (swd(1))
    ALLOCATE (qpln(1))
    ALLOCATE (qpln0(1))
    ALLOCATE (flwblk(1))
    ALLOCATE (tp(1))
    ALLOCATE (powave(1))
    ALLOCATE (dvdt(1))
    ALLOCATE (apln(1))
    ALLOCATE (bpln(1))
    ALLOCATE (dvdt0(1))
    ALLOCATE (hfusn(1:3))
    ALLOCATE (tmelt(1:3))
    ALLOCATE (ascal3(1:25))
    ALLOCATE (ascal2(1:57))
    ALLOCATE (ascal1(1:29))
    ALLOCATE (gadolin(1:naxialnodes))
    ALLOCATE (pchn(1,1))
    ALLOCATE (burad(1:naxialnodes,1:nradialnodes))
    ALLOCATE (radpow(1:naxialnodes,1:nradialnodes))
    ALLOCATE (radsrc(1:naxialnodes,1:nradialnodes))
    ALLOCATE (radsrco(1:naxialnodes,1:nradialnodes))
    
    ! Set the default values
    Ifaila = 0
    iagap = 0
    iagap0 = 0
    m5fc2 = 0
    m6fc2 = 0
    IndexFC2Print = 0
    mechan = 0
    irupt = 0
    irefine = 0
    Ifailue = 0
    nradq = 0
    nbuq = 0
    atop = 0.0_r8k
    dcldh = 0.0_r8k
    delth = 0.0_r8k
    gpthk0 = 0.0_r8k
    cnfsol = 0.0_r8k
    cnfliq = 0.0_r8k
    tmax = 0.0_r8k
    trecrd = 0.0_r8k
    t0 = 0.0_r8k
    dtenfb = 1.0e+10_r8k
    dtenfo = 1.0e+10_r8k
    frcoef = 0.0_r8k
    ruptstrain = 0.0_r8k
    refine = 0.0_r8k
    cfluxa = 0.0_r8k
    coldw = 0.0_r8k
    dishd = 0.0_r8k
    dishv0 = 0.0_r8k
    bup = 0.0_r8k
    beta1 = 0.0_r8k
    fqcrit = 0.0_r8k
    frden = 0.0_r8k
    frpo2 = 0.0_r8k
    drod = 0.0_r8k
    dtpo = 0.0_r8k
    dtss = 0.0_r8k
    fpdcay = 0.0_r8k
    TotalGasMoles = 0.0_r8k
    pelh = 0.0_r8k
    pfail = 0.0_r8k
    powop = 0.0_r8k
    PlenumGasMoles = 0.0_r8k
    prsacc = 0.0_r8k
    zvoid1 = 0.0_r8k
    rhof = 0.0_r8k
    rl = 0.0_r8k
    rshd = 0.0_r8k
    rhoc = 0.0_r8k
    rvoid = 0.0_r8k
    srcv2 = 0.0_r8k
    src1 = 0.0_r8k
    src2 = 0.0_r8k
    tdbugd = 0.0_r8k
    tempcs = 0.0_r8k
    tflux = 0.0_r8k
    tgas0 = 0.0_r8k
    timop = 0.0_r8k
    tmaxc = 5140.0_r8k
    tmaxf = 3320.0_r8k
    tmpac1 = 0.0_r8k
    tpowf = 0.0_r8k
    trest = 0.0_r8k
    t0c = 0.0_r8k
    t0f = 0.0_r8k
    zvoid2 = 0.0_r8k
    OpenPorosityFraction = 0.0_r8k
    spl = 0.0_r8k
    scd = 0.0_r8k
    swd = 0.0_r8k
    dcldh0 = 0.0_r8k
    delth0 = 0.0_r8k
    dtheta = 0.0_r8k
    dtold = 0.0_r8k
    fr = 0.0_r8k
    frchdt = 0.0_r8k
    frchmx = 0.0_r8k
    fuelrd = 0.0_r8k
    time0 = 0.0_r8k
    tpo = 0.0_r8k
    zbot = 0.0_r8k
    zmesh = 0.0_r8k
    zro = 0.0_r8k
    dppowi = 0.0_r8k
    powimx = 0.0_r8k
    powict = 0.0_r8k
    aidtot = 0.0_r8k
    aidlng = 0.0_r8k
    aidsht = 0.0_r8k
    aidsur = 0.0_r8k
    relocmodel = 'FRAPCON-3.3'
    idumr1 = 0
    vplen = 0.0_r8k
    apln0 = 0.0_r8k
    bpln0 = 0.0_r8k
    vsn = 0.0_r8k
    bu = 0.0_r8k
    gappr0 = 0.0_r8k
    scd = 0.0_r8k
    spl = 0.0_r8k
    swd = 0.0_r8k
    qpln = 0.0_r8k
    qpln0 = 0.0_r8k
    flwblk = 0.0_r8k
    tp = 0.0_r8k
    powave = 0.0_r8k
    dvdt = 0.0_r8k
    apln = 0.0_r8k
    bpln = 0.0_r8k
    dvdt0 = 0.0_r8k
    hfusn = 0.0_r8k
    tmelt = 0.0_r8k
    ascal3 = 0.0_r8k
    ascal2 = 0.0_r8k
    ascal1 = 0.0_r8k
    gadolin = 0.0_r8k
    pchn = 0.0_r8k
    burad = 0.0_r8k
    radpow = 0.0_r8k
    radsrc = 0.0_r8k
    radsrco = 0.0_r8k
    !
    END SUBROUTINE Allocate_scalr_var
    !
    SUBROUTINE Allocate_prntb_var
    !>@brief
    !> This subroutine allocates the Prntb variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (aprntb(1))
    
    ! Set the default values
    itcntd = 0
    crfpr = 0.0_r8k
    zqchpr = 0.0_r8k
    reflpr = 0.0_r8k
    fldrpr = 0.0_r8k
    TimeIncrement = 0.0_r8k
    tplenb = 0.0_r8k
    aprntb = 0.0_r8k
    
    END SUBROUTINE Allocate_prntb_var
    !
    SUBROUTINE Allocate_presb_var
    !>@brief
    !> This subroutine allocates the Presb variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (ies(1))
    ALLOCATE (pswll0(1))
    ALLOCATE (roi(1))
    ALLOCATE (vs0(1))
    ALLOCATE (flowg(1))
    ALLOCATE (GasAx(1:naxialnodes))
    
    ! Set the default values
    Kswell = 0
    Ifail = 0
    vplenc = 0.0_r8k
    tplen = 0.0_r8k
    VolAveGasTemp = 0.0_r8k
    pctop = 0.0_r8k
    dvdtp = 0.0_r8k
    vplenb = 0.0_r8k
    gfloa1 = 0.0_r8k
    roughf = 0.0_r8k
    roughc = 0.0_r8k
    swllfr = 0.0_r8k
    TotalVoidVol = 0.0_r8k
    ies = 0
    pswll0 = 0.0_r8k
    roi = 0.0_r8k
    vs0 = 0.0_r8k
    flowg = 0.0_r8k
    GasAx = 0.0_r8k
    
    END SUBROUTINE Allocate_presb_var
    !
    SUBROUTINE Allocate_powrd_var
    !>@brief
    !> This subroutine allocates the Powrd variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (ProfileStartTime(1:ntimesteps))
    ALLOCATE (RodAvePower(1:ntimesteps))
    ALLOCATE (AxPowProfile(1:2*naxialnodes,1:ntimesteps))
    ALLOCATE (pazp(1:2*nradialnodes, 1:naxialnodes))
    
    ! Set the default values
    nprofile = 0
    NumAxProfiles = 0
    apowrd = 0.0_r8k
    ProfileStartTime = 0.0_r8k
    RodAvePower = 0.0_r8k
    AxPowProfile = 0.0_r8k
    pazp = 0.0_r8k
    
    END SUBROUTINE Allocate_Powrd_var
    !
    SUBROUTINE Allocate_Numcom_var
    !>@brief
    !> This subroutine allocates the Numcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (cmesh(1:nradialnodes))
    ALLOCATE (fmesh(1:nradialnodes))
    ALLOCATE (zelev(1:naxialnodes))
    
    ! Set the default values
    idebug = 0
    ncmesh = 0
    nfmesh = 0
    nsc = 0
    nsf = 0
    cmesh = 0.0_r8k
    fmesh = 0.0_r8k
    zelev = 0.0_r8k
    
    END SUBROUTINE Allocate_Numcom_var
    !
    SUBROUTINE Allocate_htcb_var
    !>@brief
    !> This subroutine allocates the Htcb variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Set the default values
    kaxhtc = 0
    nfhtc = 0
    nradsh = 0
    nqbow = 0
    tshtc = 0.0_r8k
    fhtc = 0.0_r8k
    rshrd = 0.0_r8k
    zroug1 = 0.0_r8k
    zroug2 = 0.0_r8k
    ffchf = 0.0_r8k
    htflxa = 0.0_r8k
    bowthr = 0.0_r8k
    
    END SUBROUTINE Allocate_htcb_var
    !
    SUBROUTINE Allocate_HeatConduction_var
    !>@brief
    !> This subroutine allocates the HeatConduction variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (BoundaryCondition(1:6))
    ALLOCATE (ThermalConductAdv(1:nradialnodes))
    ALLOCATE (FinalTemp(1:nradialnodes))
    ALLOCATE (PrevTemp(1:nradialnodes))
    ALLOCATE (ArrayE(1:nradialnodes))
    ALLOCATE (ArrayF(1:nradialnodes))
    ALLOCATE (VolumeWeightL(1:nradialnodes,1:naxialnodes))
    ALLOCATE (VolumeWeightR(1:nradialnodes,1:naxialnodes))
    ALLOCATE (AreaWeight(1:nradialnodes,1:naxialnodes))
    ALLOCATE (ThermalConductivity(1:nradialnodes))
    ALLOCATE (acond(1:30,1:10))
    ALLOCATE (RhoCp(1:nradialnodes,1:naxialnodes))
    ALLOCATE (RhoCp0(1:nradialnodes,1:naxialnodes))
    ALLOCATE (PrevIterateTemp(1:nradialnodes,1:naxialnodes))
    
    ! Set the default values
    Nchan = 0
    areao = 0.0_r8k
    arean = 0.0_r8k
    epsht1 = 0.0_r8k
    GapConductivity = 0.0_r8k
    BoundaryCondition = 0.0_r8k
    ThermalConductAdv = 0.0_r8k
    FinalTemp = 0.0_r8k
    PrevTemp = 0.0_r8k
    ArrayE = 0.0_r8k
    ArrayF = 0.0_r8k
    VolumeWeightL = 0.0_r8k
    VolumeWeightR = 0.0_r8k
    AreaWeight = 0.0_r8k
    ThermalConductivity = 0.0_r8k
    acond = 0.0_r8k
    RhoCp = 0.0_r8k
    RhoCp0 = 0.0_r8k
    PrevIterateTemp = 0.0_r8k
    
    END SUBROUTINE Allocate_HeatConduction_var
    !
    SUBROUTINE Allocate_FGR
    !>@brief
    !> This subroutine allocates the FissionGasRelease variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (relfraca(1:2*ntimesteps))
    ALLOCATE (FuelGasSwell(1:2*ntimesteps))
    ALLOCATE (ansr(1:nradialnodes))
    ALLOCATE (fmgp(1:naxialnodes))
    ALLOCATE (gasavail1(1:naxialnodes,1:nradialnodes)) ! 1:ngasr
    ALLOCATE (gasavail2(1:naxialnodes,1:nradialnodes)) ! 1:ngasr
    
    ! Set the default values
    TranSwell = 0
    presfgr = 0
    nFuelSwellPairs = 0
    nFGRpairs = 0
    GasMoles0 = 0.0_r8k
    prodxe = 0.0_r8k
    prodkr = 0.0_r8k
    relfract = 0.0_r8k
    TranFuelSwell = 0.0_r8k
    relfraca = 0.0_r8k
    FuelGasSwell = 0.0_r8k
    ngasr = 0
    ansr = 0.0_r8k
    fmgp = 0.0_r8k
    gasavail1 = 0.0_r8k
    gasavail2 = 0.0_r8k
    
    END SUBROUTINE Allocate_FGR
    !
    SUBROUTINE Allocate_Bloon_var
    !>@brief
    !> This subroutine allocates the Bloon variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (farbal(1:20))
    ALLOCATE (sdfar(1:20))
    ALLOCATE (zfarbl(1:20))
    ALLOCATE (ablona(1:73))
    
    ! Set the default values
    ifbaln = 0
    jmnbal = 0
    kntbal = 0
    nbncal = 0
    nodprm = 0
    kbaln = 0
    modbal = 0
    nprntb = 0
    chstrs = 0.0_r8k
    frbal = 0.0_r8k
    pmxbal = 0.0_r8k
    r8bal = 0.0_r8k
    tcebal = 0.0_r8k
    pdrato = 0.0_r8k
    rnbnt = 0.0_r8k
    totnb = 0.0_r8k
    trabal = 0.0_r8k
    taxbal = 0.0_r8k
    dtbal = 0.0_r8k
    dtobal = 0.0_r8k
    emwbal = 0.0_r8k
    fabal = 0.0_r8k
    flxbal = 0.0_r8k
    htcbal = 0.0_r8k
    h0bal = 0.0_r8k
    pcbal = 0.0_r8k
    psbal = 0.0_r8k
    qbal = 0.0_r8k
    qlbal = 0.0_r8k
    rfbal = 0.0_r8k
    rmpbal = 0.0_r8k
    r0bal = 0.0_r8k
    tbkbal = 0.0_r8k
    tc0bal = 0.0_r8k
    tf0bal = 0.0_r8k
    tgbal = 0.0_r8k
    timbal = 0.0_r8k
    tm1bal = 0.0_r8k
    tp1bal = 0.0_r8k
    ztmax = 0.0_r8k
    zm1bal = 0.0_r8k
    zp1bal = 0.0_r8k
    zndbal = 0.0_r8k
    htcgba = 0.0_r8k
    tfavba = 0.0_r8k
    zbaln = 0.0_r8k
    farbal = 0.0_r8k
    sdfar = 0.0_r8k
    zfarbl = 0.0_r8k
    ablona = 0.0_r8k
    
    END SUBROUTINE Allocate_Bloon_var
    !
    SUBROUTINE Allocate_dyna_var
    !>@brief
    !> This subroutine allocates the Dyna variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/11/2014
    
    ! Allocate the arrays
    ALLOCATE (IndexTempConverg(1:naxialnodes))
    ALLOCATE (RadialPower(1:nradialnodes))
    ALLOCATE (RadialBound(1:nradialnodes))
    ALLOCATE (qmaxmelt(1:nradialnodes))
    ALLOCATE (qmaxmeltp1(1:nradialnodes))
    ALLOCATE (rmassflux(1:naxialnodes))
    ALLOCATE (coolqual(1:naxialnodes))
    ALLOCATE (AAHT1(1:naxialnodes))
    ALLOCATE (BBHT1(1:naxialnodes))
    ALLOCATE (GasPress0(1:naxialnodes))
    ALLOCATE (CoolPress(1:naxialnodes))
    ALLOCATE (GasMolesAx(1:naxialnodes))
    ALLOCATE (GasMolesAx0(1:naxialnodes))
    ALLOCATE (GasPress(1:naxialnodes))
    ALLOCATE (FuelSurfT(1:naxialnodes))
    ALLOCATE (FuelCenterT(1:naxialnodes))
    ALLOCATE (CrackTemp(1:naxialnodes))
    ALLOCATE (VolOpenPor(1:naxialnodes))
    ALLOCATE (GapThick(1:naxialnodes))
    ALLOCATE (GapThick0(1:naxialnodes))
    ALLOCATE (GapTemp(1:naxialnodes))
    ALLOCATE (PelletRad(1:naxialnodes))
    ALLOCATE (PelletRad0(1:naxialnodes))
    ALLOCATE (HeatFlux(1:naxialnodes))
    ALLOCATE (HeatFlux0(1:naxialnodes))
    ALLOCATE (SurfHtFlux(1:naxialnodes))
    ALLOCATE (CladMaxT(1:naxialnodes))
    ALLOCATE (Vreloc(1:naxialnodes))
    ALLOCATE (Vreloc0(1:naxialnodes))
    ALLOCATE (PelSrfDispl(1:naxialnodes))
    ALLOCATE (OldPelDis(1:naxialnodes))
    ALLOCATE (OldPelDis0(1:naxialnodes))
    ALLOCATE (OxiThk1(1:naxialnodes))
    ALLOCATE (OxiThk2(1:naxialnodes))
    ALLOCATE (DishVperL(1:naxialnodes))
    ALLOCATE (FastFlux(1:naxialnodes))
    ALLOCATE (AlphaThk1(1:naxialnodes))
    ALLOCATE (AlphaThk2(1:naxialnodes))
    ALLOCATE (AlphaThk11(1:naxialnodes))
    ALLOCATE (AlphaThk22(1:naxialnodes))
    ALLOCATE (oxideid(1:naxialnodes))
    ALLOCATE (EffStrain(1:naxialnodes))
    ALLOCATE (EffStrain0(1:naxialnodes))
    ALLOCATE (oxideod(1:naxialnodes))
    ALLOCATE (CldPermStrn(1:naxialnodes))
    ALLOCATE (CldPermStrn0(1:naxialnodes))
    ALLOCATE (SSHgap(1:naxialnodes))
    ALLOCATE (OldCoolPrs(1:naxialnodes))
    ALLOCATE (OldCoolPrs0(1:naxialnodes))
    ALLOCATE (CladAveTemp(1:naxialnodes))
    ALLOCATE (OldGasPrs(1:naxialnodes))
    ALLOCATE (OldGasPrs0(1:naxialnodes))
    ALLOCATE (HtFlxFac(1:naxialnodes))
    ALLOCATE (OldCladT(1:naxialnodes))
    ALLOCATE (OldFuelAxStrn(1:naxialnodes))
    ALLOCATE (RodOD(1:naxialnodes))
    ALLOCATE (OldCldAxStrn(1:naxialnodes))
    ALLOCATE (OldCldAxStrn0(1:naxialnodes))
    ALLOCATE (RodOD0(1:naxialnodes))
    ALLOCATE (OldCladT0(1:naxialnodes))
    ALLOCATE (OldFuelAxStrn0(1:naxialnodes))
    ALLOCATE (TimeofGBSep(1:naxialnodes))
    ALLOCATE (SwellDispl(1:naxialnodes))
    ALLOCATE (HmaxInitial(1:naxialnodes))
    ALLOCATE (CoolEnthalpy(1:naxialnodes))
    ALLOCATE (RInterfacPrs(1:naxialnodes))
    ALLOCATE (RInterfacPrs0(1:naxialnodes))
    ALLOCATE (HGapAv(1:naxialnodes))
    ALLOCATE (FilmCoeffAv(1:naxialnodes))
    ALLOCATE (AxialPowr(1:naxialnodes))
    ALLOCATE (AxialPowr0(1:naxialnodes))
    ALLOCATE (CritHtFlux(1:naxialnodes))
    ALLOCATE (FuelSrfStrRat(1:naxialnodes))
    ALLOCATE (WatrMetlEnrgy(1:naxialnodes))
    ALLOCATE (RInterfacGap(1:naxialnodes))
    ALLOCATE (CladSurfT(1:naxialnodes))
    ALLOCATE (EDotFZ(1:naxialnodes))
    ALLOCATE (PelSrfStrn0(1:naxialnodes))
    ALLOCATE (FuelSrfStrRat0(1:naxialnodes))
    ALLOCATE (EDotFZ0(1:naxialnodes))
    ALLOCATE (EnergyPerL(1:naxialnodes))
    ALLOCATE (HFXSum(1:naxialnodes))
    ALLOCATE (CladEnrgPerL(1:naxialnodes))
    ALLOCATE (CoolEnthalpy0(1:naxialnodes))
    ALLOCATE (CoolMassFlx(1:naxialnodes))
    ALLOCATE (Enthl(1:naxialnodes))
    ALLOCATE (CoolDensity(1:naxialnodes))
    ALLOCATE (CoolDensity0(1:naxialnodes))
    ALLOCATE (PelRadDeviat(1:naxialnodes))
    ALLOCATE (CldPermAxStrn(1:naxialnodes))
    ALLOCATE (VoidVolumeRatio(1:n_void_volumes))
    ALLOCATE (CldPermHoopStrn(1:naxialnodes))
    ALLOCATE (ECR(1:naxialnodes))
    ALLOCATE (OxUptakeID1(1:naxialnodes))
    ALLOCATE (OxUpTakeID2(1:naxialnodes))
    ALLOCATE (SEDPNNL(1:naxialnodes))
    ALLOCATE (EffStrainPNNL(1:naxialnodes))
    ALLOCATE (coefk(1:naxialnodes))
    ALLOCATE (coefn(1:naxialnodes))
    ALLOCATE (coefm(1:naxialnodes))
    ALLOCATE (Emodulus(1:naxialnodes))
    ALLOCATE (strainrateterm(1:naxialnodes))
    ALLOCATE (SEDEPRI(1:naxialnodes))
    ALLOCATE (EffFastFluStrnHardExp(1:naxialnodes))
    ALLOCATE (BetaThickness(1:naxialnodes))
    ALLOCATE (EffColdWkStrnHardExp(1:naxialnodes))
    ALLOCATE (OxygenConcenAve(1:naxialnodes))
    ALLOCATE (EffColdWkStrenCoef(1:naxialnodes))
    ALLOCATE (OxygenUptake(1:naxialnodes))
    ALLOCATE (OxStAlphaThkRemain(1:naxialnodes))
    ALLOCATE (OxStAlphaThkNearFuel(1:naxialnodes))
    ALLOCATE (EffFastFluStrenCoef(1:naxialnodes))
    ALLOCATE (OxiPowerGen(1:naxialnodes))
    ALLOCATE (PrcntSatBeta(1:naxialnodes))
    ALLOCATE (OxConcenAAO(1:naxialnodes))
    ALLOCATE (OxConcenABO(1:naxialnodes))
    ALLOCATE (OxConcenACO(1:naxialnodes))
    ALLOCATE (OxConcenADO(1:naxialnodes))
    ALLOCATE (OxConcenAEO(1:naxialnodes))
    ALLOCATE (OxConcenAFO(1:naxialnodes))
    ALLOCATE (OxConcenAGO(1:naxialnodes))
    ALLOCATE (OxConcenAHO(1:naxialnodes))
    ALLOCATE (OxConcenAIO(1:naxialnodes))
    ALLOCATE (OxConcenAAI(1:naxialnodes))
    ALLOCATE (OxConcenABI(1:naxialnodes))
    ALLOCATE (OxConcenACI(1:naxialnodes))
    ALLOCATE (OxConcenADI(1:naxialnodes))
    ALLOCATE (OxConcenAEI(1:naxialnodes))
    ALLOCATE (OxConcenAFI(1:naxialnodes))
    ALLOCATE (OxConcenAGI(1:naxialnodes))
    ALLOCATE (OxConcenAHI(1:naxialnodes))
    ALLOCATE (OxConcenAII(1:naxialnodes))
    ALLOCATE (CrackVolume(1:naxialnodes))
    ALLOCATE (OpenPorTemp(1:naxialnodes))
    ALLOCATE (AveDishTemp(1:naxialnodes))
    ALLOCATE (CentVoidVol(1:naxialnodes))
    ALLOCATE (ExtentOfBow(1:naxialnodes))
    ALLOCATE (AxialNodLen(1:naxialnodes))
    ALLOCATE (TerfacePres(1:naxialnodes))
    ALLOCATE (CrackWidth(1:naxialnodes))
    ALLOCATE (EinstabilStrain(1:naxialnodes))
    ALLOCATE (AxBurnup(1:naxialnodes))
    ALLOCATE (BOSOxideThick(1:naxialnodes))
    ALLOCATE (EOSOxideThick(1:naxialnodes))
    ALLOCATE (OpenPorVol(1:naxialnodes))
    ALLOCATE (OpenPorosity(1:naxialnodes))
    ALLOCATE (CladEffStress(1:naxialnodes))
    ALLOCATE (BulkCoolTemp(1:naxialnodes))
    ALLOCATE (CladYieldStress(1:naxialnodes))
    ALLOCATE (StressAtInstStrain(1:naxialnodes))
    ALLOCATE (techf(1:naxialnodes))
    ALLOCATE (CesiumContent(1:naxialnodes))
    ALLOCATE (HydrogenContent(1:naxialnodes))
    ALLOCATE (tschf(1:naxialnodes))
    ALLOCATE (WorkSpaceGAPI(1:naxialnodes))
    ALLOCATE (WorkSpaceEP1(1:naxialnodes))
    ALLOCATE (WorkSpaceURC(1:naxialnodes))
    ALLOCATE (WorkSpaceTCMx(1:naxialnodes))
    ALLOCATE (WorkSpaceGAP(1:naxialnodes))
    ALLOCATE (nodchf(1:naxialnodes+1))
    ALLOCATE (BOSTemp(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EOSTemp(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EOSRad(1:nradialnodes,1:naxialnodes))
    ALLOCATE (BOSRad(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EnrgyMeltP1(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EnrgyMeltZ(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EnrgyMelt(1:nradialnodes,1:naxialnodes))
    ALLOCATE (EnrgyMeltZp1(1:nradialnodes,1:naxialnodes))
    ALLOCATE (RadialBoundO(1:nradialnodes,1:naxialnodes))
    ALLOCATE (DeformedRadiusOfMesh(1:nradialnodes,1:naxialnodes))
    ALLOCATE (WorkSpaceEPP1(1:naxialnodes,1:3))
    ALLOCATE (WorkSpacePINT(1:naxialnodes))
    ALLOCATE (WorkSpaceRCI(1:naxialnodes))
    ALLOCATE (WorkSpaceRCO(1:naxialnodes))
    ALLOCATE (WorkSpaceReloc(1:naxialnodes))
    ALLOCATE (AxNodElevat(1:naxialnodes))
    ALLOCATE (ureloc(1:naxialnodes))
    ALLOCATE (a1(1:10000))
    ALLOCATE (CldElStrn(1:naxialnodes,1:3))
    ALLOCATE (CldPlasStrn(1:naxialnodes,1:3))
    ALLOCATE (CldPlasStrn0(1:naxialnodes,1:3))
    ALLOCATE (CldStrn(1:naxialnodes,1:3))
    ALLOCATE (CldStrnRat(1:naxialnodes,1:3))
    ALLOCATE (CldStrnRat0(1:naxialnodes,1:3))
    ALLOCATE (HydrostatPress(1:naxialnodes))
    ALLOCATE (FuelResidStrn(1:naxialnodes,1:3))
    ALLOCATE (FuelResidStrn0(1:naxialnodes,1:3))
    ALLOCATE (Baln2Twall(1:16,1:16))
    ALLOCATE (CldresidStrn(1:naxialnodes,1:3))
    ALLOCATE (CldResidStrn0(1:naxialnodes,1:3))
    ALLOCATE (IodineContent(1:naxialnodes))
    ALLOCATE (PelSrfStrn(1:naxialnodes,1:3))
    ALLOCATE (CldStress(1:naxialnodes,1:3))
    ALLOCATE (CldThermStrn(1:naxialnodes,1:3))
    ALLOCATE (GRsv(4,1:nradialnodes,1:nradialnodes))
    ALLOCATE (IndexPCMI(1:naxialnodes))
    ALLOCATE (IndexPCMIOnce(1:naxialnodes))
    ALLOCATE (NumAzmuthNod(1:naxialnodes))
    ALLOCATE (Ichf(1:naxialnodes))
    ALLOCATE (Ih(1:naxialnodes))
    ALLOCATE (Ihtreg(1:naxialnodes))
    ALLOCATE (GapIndex(1:naxialnodes))
    ALLOCATE (BOSGapIndex(1:naxialnodes))
    ALLOCATE (BOSGapIndex0(1:naxialnodes))
    ALLOCATE (RuptFailIndex(1:naxialnodes))
    ALLOCATE (CladCollapseIndex(1:naxialnodes))
    ALLOCATE (CladCollIndx0(1:naxialnodes))
    ALLOCATE (OldGapIndex(1:naxialnodes))
    ALLOCATE (NodeSinterTemp(1:naxialnodes))
    ALLOCATE (RodFailIndex(1:naxialnodes))
    
    ! Set default values
    IndexTempConverg = 0
    RadialPower = 0.0_r8k
    RadialBound = 0.0_r8k
    qmaxmelt = 0.0_r8k
    qmaxmeltp1 = 0.0_r8k
    rmassflux = 0.0_r8k
    coolqual = 0.0_r8k
    AAHT1 = 0.0_r8k
    BBHT1 = 0.0_r8k
    GasPress0 = 0.0_r8k
    CoolPress = 0.0_r8k
    GasMolesAx = 0.0_r8k
    GasMolesAx0 = 0.0_r8k
    GasPress = 0.0_r8k
    FuelSurfT = 0.0_r8k
    FuelCenterT = 0.0_r8k
    CrackTemp = 0.0_r8k
    VolOpenPor = 0.0_r8k
    GapThick = 0.0_r8k
    GapThick0 = 0.0_r8k
    GapTemp = 0.0_r8k
    PelletRad = 0.0_r8k
    PelletRad0 = 0.0_r8k
    HeatFlux = 0.0_r8k
    HeatFlux0 = 0.0_r8k
    SurfHtFlux = 0.0_r8k
    CladMaxT = 0.0_r8k
    Vreloc = 0.0_r8k
    Vreloc0 = 0.0_r8k
    PelSrfDispl = 0.0_r8k
    OldPelDis = 0.0_r8k
    OldPelDis0 = 0.0_r8k
    OxiThk1 = 0.0_r8k
    OxiThk2 = 0.0_r8k
    DishVperL = 0.0_r8k
    FastFlux = 0.0_r8k
    AlphaThk1 = 0.0_r8k
    AlphaThk2 = 0.0_r8k
    AlphaThk11 = 0.0_r8k
    AlphaThk22 = 0.0_r8k
    oxideid = 0.0_r8k
    EffStrain = 0.0_r8k
    EffStrain0 = 0.0_r8k
    oxideod = 0.0_r8k
    CldPermStrn = 0.0_r8k
    CldPermStrn0 = 0.0_r8k
    SSHgap = 0.0_r8k
    OldCoolPrs = 0.0_r8k
    OldCoolPrs0 = 0.0_r8k
    CladAveTemp = 0.0_r8k
    OldGasPrs = 0.0_r8k
    OldGasPrs0 = 0.0_r8k
    HtFlxFac = 0.0_r8k
    OldCladT = 0.0_r8k
    OldFuelAxStrn = 0.0_r8k
    RodOD = 0.0_r8k
    OldCldAxStrn = 0.0_r8k
    OldCldAxStrn0 = 0.0_r8k
    RodOD0 = 0.0_r8k
    OldCladT0 = 0.0_r8k
    OldFuelAxStrn0 = 0.0_r8k
    TimeofGBSep = 0.0_r8k
    SwellDispl = 0.0_r8k
    HmaxInitial = 0.0_r8k
    CoolEnthalpy = 0.0_r8k
    RInterfacPrs = 0.0_r8k
    RInterfacPrs0 = 0.0_r8k
    HGapAv = 0.0_r8k
    FilmCoeffAv = 0.0_r8k
    AxialPowr = 0.0_r8k
    AxialPowr0 = 0.0_r8k
    CritHtFlux = 0.0_r8k
    FuelSrfStrRat = 0.0_r8k
    WatrMetlEnrgy = 0.0_r8k
    RInterfacGap = 0.0_r8k
    CladSurfT = 0.0_r8k
    EDotFZ = 0.0_r8k
    PelSrfStrn0 = 0.0_r8k
    FuelSrfStrRat0 = 0.0_r8k
    EDotFZ0 = 0.0_r8k
    EnergyPerL = 0.0_r8k
    HFXSum = 0.0_r8k
    CladEnrgPerL = 0.0_r8k
    CoolEnthalpy0 = 0.0_r8k
    CoolMassFlx = 0.0_r8k
    Enthl = 0.0_r8k
    CoolDensity = 0.0_r8k
    CoolDensity0 = 0.0_r8k
    PelRadDeviat = 0.0_r8k
    CldPermAxStrn = 0.0_r8k
    VoidVolumeRatio = 0.0_r8k
    CldPermHoopStrn = 0.0_r8k
    ECR = 0.0_r8k
    OxUptakeID1 = 0.0_r8k
    OxUpTakeID2 = 0.0_r8k
    SEDPNNL = 0.0_r8k
    EffStrainPNNL = 0.0_r8k
    coefk = 0.0_r8k
    coefn = 0.0_r8k
    coefm = 0.0_r8k
    Emodulus = 0.0_r8k
    strainrateterm = 0.0_r8k
    SEDEPRI = 0.0_r8k
    EffFastFluStrnHardExp = 0.0_r8k
    BetaThickness = 0.0_r8k
    EffColdWkStrnHardExp = 0.0_r8k
    OxygenConcenAve = 0.0_r8k
    EffColdWkStrenCoef = 0.0_r8k
    OxygenUptake = 0.0_r8k
    OxStAlphaThkRemain = 0.0_r8k
    OxStAlphaThkNearFuel = 0.0_r8k
    EffFastFluStrenCoef = 0.0_r8k
    OxiPowerGen = 0.0_r8k
    PrcntSatBeta = 0.0_r8k
    OxConcenAAO = 0.0_r8k
    OxConcenABO = 0.0_r8k
    OxConcenACO = 0.0_r8k
    OxConcenADO = 0.0_r8k
    OxConcenAEO = 0.0_r8k
    OxConcenAFO = 0.0_r8k
    OxConcenAGO = 0.0_r8k
    OxConcenAHO = 0.0_r8k
    OxConcenAIO = 0.0_r8k
    OxConcenAAI = 0.0_r8k
    OxConcenABI = 0.0_r8k
    OxConcenACI = 0.0_r8k
    OxConcenADI = 0.0_r8k
    OxConcenAEI = 0.0_r8k
    OxConcenAFI = 0.0_r8k
    OxConcenAGI = 0.0_r8k
    OxConcenAHI = 0.0_r8k
    OxConcenAII = 0.0_r8k
    CrackVolume = 0.0_r8k
    OpenPorTemp = 0.0_r8k
    AveDishTemp = 0.0_r8k
    CentVoidVol = 0.0_r8k
    ExtentOfBow = 0.0_r8k
    AxialNodLen = 0.0_r8k
    TerfacePres = 0.0_r8k
    CrackWidth = 0.0_r8k
    EinstabilStrain = 0.0_r8k
    AxBurnup = 0.0_r8k
    BOSOxideThick = 0.0_r8k
    EOSOxideThick = 0.0_r8k
    OpenPorVol = 0.0_r8k
    OpenPorosity = 0.0_r8k
    CladEffStress = 0.0_r8k
    BulkCoolTemp = 0.0_r8k
    CladYieldStress = 0.0_r8k
    StressAtInstStrain = 0.0_r8k
    techf = 0.0_r8k
    CesiumContent = 0.0_r8k
    HydrogenContent = 0.0_r8k
    tschf = 0.0_r8k
    WorkSpaceGAPI = 0.0_r8k
    WorkSpaceEP1 = 0.0_r8k
    WorkSpaceURC = 0.0_r8k
    WorkSpaceTCMx = 0.0_r8k
    WorkSpaceGAP = 0.0_r8k
    nodchf = 0.0_r8k
    BOSTemp = 0.0_r8k
    EOSTemp = 0.0_r8k
    EOSRad = 0.0_r8k
    BOSRad = 0.0_r8k
    EnrgyMeltP1 = 0.0_r8k
    EnrgyMeltZ = 0.0_r8k
    EnrgyMelt = 0.0_r8k
    EnrgyMeltZp1 = 0.0_r8k
    RadialBoundO = 0.0_r8k
    DeformedRadiusOfMesh = 0.0_r8k
    WorkSpaceEPP1 = 0.0_r8k
    gapmin = 0.0_r8k
    WorkSpacePINT = 0.0_r8k
    WorkSpaceRCI = 0.0_r8k
    WorkSpaceRCO = 0.0_r8k
    WorkSpaceReloc = 0.0_r8k
    AxNodElevat = 0.0_r8k
    ureloc = 0.0_r8k
    a1 = 0.0_r8k
    CldElStrn = 0.0_r8k
    CldPlasStrn = 0.0_r8k
    CldPlasStrn0 = 0.0_r8k
    CldStrn = 0.0_r8k
    CldStrnRat = 0.0_r8k
    CldStrnRat0 = 0.0_r8k
    HydrostatPress = 0.0_r8k
    FuelResidStrn = 0.0_r8k
    FuelResidStrn0 = 0.0_r8k
    Baln2Twall = 0.0_r8k
    CldresidStrn = 0.0_r8k
    CldResidStrn0 = 0.0_r8k
    IodineContent = 0.0_r8k
    PelSrfStrn = 0.0_r8k
    CldStress = 0.0_r8k
    CldThermStrn = 0.0_r8k
    GRsv = 0.0_r8k
    Ifchk = 0
    nce = 0
    IndexPCMI = 0
    IndexPCMIOnce = 0
    NumAzmuthNod = 0
    Ichf = 0
    Ih = 0
    Ihtreg = 0
    GapIndex = 0
    BOSGapIndex = 0
    BOSGapIndex0 = 0
    RuptFailIndex = 0
    CladCollapseIndex = 0
    CladCollIndx0 = 0
    OldGapIndex = 0
    NodeSinterTemp = 0
    RodFailIndex = 0
    
    END SUBROUTINE Allocate_dyna_var
    !
    SUBROUTINE Allocate_collct_var
    !>@brief
    !> This subroutine allocates the Collct variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (Ifstor(1:30))
    ALLOCATE (ihData(1:30))
    ALLOCATE (icglob(1:12,1:8))
    ALLOCATE (acolct(1:665))
    ALLOCATE (gasphs(1:2*ntimesteps))
    ALLOCATE (gasths(1:2*ntimesteps,1:2))
    ALLOCATE (gbse(1:5))
    ALLOCATE (pelprm(1:40))
    ALLOCATE (pinput(1:60))
    ALLOCATE (potput(1:60))
    ALLOCATE (tpln(1:6,1:2))
    ALLOCATE (tplbt1(1:6,1:2))
    
    ! Set the default values
    ntapou = 0
    la1max = 0
    MaximumIterations = 0
    n1 = 0
    la1fil = 0
    ntplot = 0
    ntabl1 = 0
    ntabl2 = 0
    ntabl3 = 0
    lexcb = 0
    lprntb = 0
    lflect = 0
    ldialb = 0
    lhtcb = 0
    lresr1 = 0
    lresr2 = 0
    lsclr1 = 0
    lsclr2 = 0
    lsclr3 = 0
    lcolct = 0
    lcoold = 0
    lcombk = 0
    lresi2 = 0
    lresi3 = 0
    lafrap = 0
    npramp = 0
    itswt = 0
    lmatpc = 0
    nswmd = 0
    n3 = 0
    ndap1 = 0
    IterationCount = 0
    ntco = 0
    kdbug = 0
    lblona = 0
    lphypr = 0
    lthyd = 0
    prestmp = 0
    dtp = 0.0_r8k
    z = 0.0_r8k
    gum = 0.0_r8k
    Ifstor = 0
    ihData = 0
    icglob = 0
    acolct = 0.0_r8k
    gasphs = 0.0_r8k
    gasths = 0.0_r8k
    gbse = 0.0_r8k
    pelprm = 0.0_r8k
    pinput = 0.0_r8k
    potput = 0.0_r8k
    tpln = 0.0_r8k
    tplbt1 = 0.0_r8k
    
    END SUBROUTINE Allocate_collct_var
    !
    SUBROUTINE Allocate_excb_var
    !>@brief
    !> This subroutine allocates the Excb variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (aexcb(1))
    ALLOCATE (dtpoa(1:pre_nt+2))
    ALLOCATE (tplbot(1:6,1:2))
    
    ! Set the default values
    ndim = 0
    nzmesh = 0
    n2 = 1
    n4 = 0
    nsymm = 0
    nconsw = 0
    naz = 0
    nnaz = 0
    npdtpo = 0
    nbalsw = 0
    nrefld = 0
    nlac = 0
    nprsw = 0
    modkf = 0
    nbotpl = 0
    ncolbp = 0
    dzmesh = 0.0_r8k
    timmd = 0.0_r8k
    dofset = 0.0_r8k
    dofang = 0.0_r8k
    pitch = 0.0_r8k
    fgrns = 0.0_r8k
    fotmtl = 0.0_r8k
    rsntr = 0.0_r8k
    tsntrk = 0.0_r8k
    volbp = 0.0_r8k
    splbp = 0.0_r8k
    coldbp = 0.0_r8k
    spdbp = 0.0_r8k
    BottomPlenumGasMoles = 0.0_r8k
    cldwdc = 0.0_r8k
    aexcb = 0.0_r8k
    dtpoa = 0.0_r8k
    tplbot = 0.0_r8k
    
    END SUBROUTINE Allocate_excb_var
    !
    SUBROUTINE Allocate_CoolantProperties
    !>@brief
    !> This subroutine allocates the CoolProperties variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/18/2014
    
    INTEGER(ipk) :: Total
    
    ! Set the total array size
    Total = 10 * naxialnodes + 6
    
    ! Allocate the arrays
    ALLOCATE (Prop(1:26))
    ALLOCATE (nhprs(1:naxialnodes))
    ALLOCATE (ntprs(1:naxialnodes))
    ALLOCATE (nvprs(1:naxialnodes))
    ALLOCATE (npprs(1:naxialnodes))
    ALLOCATE (acoold(1))
    ALLOCATE (vfrad1(1:5))
    ALLOCATE (vfrad2(1:5))
    ALLOCATE (vfrad3(1:5))
    ALLOCATE (elvrad(1:5))
    ALLOCATE (htclev(1:naxialnodes))
    ALLOCATE (gbh(1:ntimesteps))
    ALLOCATE (hbh(1:ntimesteps))
    ALLOCATE (hinta(1:ntimesteps))
    ALLOCATE (hupta(1:ntimesteps))
    ALLOCATE (pbh(1:ntimesteps))
    ALLOCATE (tshrda(1:ntimesteps))
    ALLOCATE (htca(1:ntimesteps,1:naxialnodes))
    ALLOCATE (tblka(1:ntimesteps,1:naxialnodes))
    ALLOCATE (trad1(1:20,1:5))
    ALLOCATE (trad2(1:20,1:5))
    ALLOCATE (trad3(1:20,1:5))
    ALLOCATE (flthit(1:40))
    ALLOCATE (faxzq(1:2*naxialnodes))
    ALLOCATE (qaxzq(1:2*naxialnodes))
    ALLOCATE (tempfc(1:2*naxialnodes))
    ALLOCATE (aflcht(1:630))
    ALLOCATE (prestm(1:ntimesteps))
    ALLOCATE (hlqclp(1:ntimesteps))
    ALLOCATE (temptm(1:ntimesteps))
    ALLOCATE (fldrat(1:ntimesteps))
    ALLOCATE (tz2(1:naxialnodes))
    ALLOCATE (z1(1:naxialnodes))
    ALLOCATE (z2(1:naxialnodes))
    ALLOCATE (gz1(1:naxialnodes))
    ALLOCATE (gz2(1:naxialnodes))
    ALLOCATE (hz1(1:naxialnodes))
    ALLOCATE (hz2(1:naxialnodes))
    ALLOCATE (pz1(1:naxialnodes))
    ALLOCATE (pz2(1:naxialnodes))
    ALLOCATE (tz1(1:Total))
    ALLOCATE (aasth(1:17000))
    
    ! Set the default values
    aasth = 0.0_r8k
    nvol = 0
    ithymx = 0
    ixazim = 0
    ncall = 0
    tc1 = 0.0_r8k
    tc2 = 0.0_r8k
    tz2 = 0.0_r8k
    z1 = 0.0_r8k
    z2 = 0.0_r8k
    gz1 = 0.0_r8k
    gz2 = 0.0_r8k
    hz1 = 0.0_r8k
    hz2 = 0.0_r8k
    pz1 = 0.0_r8k
    pz2 = 0.0_r8k
    tz1 = 0.0_r8k
    nbrtmp = 0
    nbrfdr = 0
    nbrfht = 0
    kaxnlo = 0
    liqnod = 0
    izadfg = 0
    irup = 0
    nbrpst = 0
    nflec = 0
    nbrliq = 0
    npaxpf = 0
    mzq1 = 0
    mflt = 0
    mbdl = 0
    ntempf = 0
    hliq = 0.0_r8k
    qmax = 0.0_r8k
    empytm = 0.0_r8k
    hrad = 0.0_r8k
    fldrte = 0.0_r8k
    zqch = 0.0_r8k
    oldtim = 0.0_r8k
    tflood = 0.0_r8k
    crf = 0.0_r8k
    templo = 0.0_r8k
    rhostm = 0.0_r8k
    cpstem = 0.0_r8k
    tsatt = 0.0_r8k
    pressr = 0.0_r8k
    pressi = 0.0_r8k
    cpmult = 0.0_r8k
    gflow = 0.0_r8k
    temphi = 0.0_r8k
    ruplev = 0.0_r8k
    pavg = 0.0_r8k
    refdtm = 0.0_r8k
    hydiam = 0.0_r8k
    flxsec = 0.0_r8k
    tsub = 0.0_r8k
    pdeint = 0.0_r8k
    flowbk = 0.0_r8k
    tempmx = 0.0_r8k
    pfflec = 0.0_r8k
    tfoldf = 0.0_r8k
    pdecy = 0.0_r8k
    drflt = 0.0_r8k
    pfnuc = 0.0_r8k
    toldfc = 0.0_r8k
    zqflt = 0.0_r8k
    qaxpk = 0.0_r8k
    zpkfc = 0.0_r8k
    fltgap = 0.0_r8k
    pavgft = 0.0_r8k
    rcpar = 0.0_r8k
    zad = 0.0_r8k
    zs = 0.0_r8k
    trodfc = 0.0_r8k
    nu1 = 0.0_r8k
    nu2 = 0.0_r8k
    nu3 = 0.0_r8k
    rupflg = ' '
    lodmrk = ' '
    flthit = 0.0_r8k
    faxzq = 0.0_r8k
    qaxzq = 0.0_r8k
    tempfc = 0.0_r8k
    aflcht = 0.0_r8k
    prestm = 0.0_r8k
    hlqclp = 0.0_r8k
    temptm = 0.0_r8k
    fldrat = 0.0_r8k
    Prop = 0.0_r8k
    tt => prop(1)
    CoolantPress => prop(2)
    v => prop(3)
    ubar => prop(4)
    hbar => prop(5)
    beta => prop(6)
    kappa => prop(7)
    csubp => prop(8)
    x => prop(9)
    psat => prop(10)
    vsubf => prop(11)
    vsubg => prop(12)
    usubf => prop(13)
    usubg => prop(14)
    hsubf => prop(15)
    hsubg => prop(16)
    betaf => prop(17)
    betag => prop(18)
    kappaf => prop(19)
    kappag => prop(20)
    csubpf => prop(21)
    csubpg => prop(22)
    nsrad3 = 0
    nelrad = 0
    nhprs = 0
    ntprs = 0
    nvprs = 0
    npprs = 0
    acoold = 0.0_r8k
    vfrad1 = 0.0_r8k
    vfrad2 = 0.0_r8k
    vfrad3 = 0.0_r8k
    elvrad = 0.0_r8k
    htclev = 0.0_r8k
    gbh = 0.0_r8k
    hbh = 0.0_r8k
    hinta = 0.0_r8k
    hupta = 0.0_r8k
    pbh = 0.0_r8k
    tshrda = 0.0_r8k
    htca = 0.0_r8k
    tblka = 0.0_r8k
    tblka(1,1) = 77.0_r8k
    trad1 = 0.0_r8k
    trad2 = 0.0_r8k
    trad3 = 0.0_r8k
    
    END SUBROUTINE Allocate_CoolantProperties
    !
    SUBROUTINE Allocate_Intcom_var
    !>@brief
    !> This subroutine allocates the Intcom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (eppinp(1:2*naxialnodes))
    ALLOCATE (radpel(1:2*naxialnodes))
    
    ! Set the default values
    nepp0 = 0
    cladid = 0.0_r8k
    cladod = 0.0_r8k
    cladtk = 0.0_r8k
    rf = 0.0_r8k
    FuelPelDiam = 0.0_r8k
    eppinp = 0.0_r8k
    radpel = 0.0_r8k
    
    END SUBROUTINE Allocate_Intcom_var
    !
    SUBROUTINE Allocate_iocom_var
    !>@brief
    !> This subroutine allocates the Iocom variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/18/2014
    
    ! Allocate the arrays
    ALLOCATE (dtplta(1:pre_nt+2))
    
    ! Set the default values
    npltn = 0
    tplot = 0.0_r8k
    dtplta = 0.0_r8k
    
    END SUBROUTINE Allocate_iocom_var
    !
    SUBROUTINE Allocate_resti_var
    !>@brief
    !> This subroutine allocates the Resti variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/18/2014
    
    ! Allocate the arrays
    ALLOCATE (ngastmp(1:2))
    ALLOCATE (modfal(1))
    ALLOCATE (nswpm(1))
    ALLOCATE (irest2(1))
    ALLOCATE (ncs(1))
    ALLOCATE (irest3(1))
    ALLOCATE (indxjk(1:nradialnodes))
    ALLOCATE (nrc(1,1))
    ALLOCATE (dtmaxa(1:pre_nt+2))
    ALLOCATE (arest1(1:136))
    ALLOCATE (azpang(1:naxialnodes))
    ALLOCATE (fluxz(1:2*naxialnodes))
    ALLOCATE (tplna(1:6,1:2,1))
    
    ! Set the default values
    indxmd = 0
    kflgb = 0
    ldir = 0
    lengmd = 0
    ncladi = 0
    nmesh = 0
    nodpln = 0
    ntstep = 0
    Ifcmi = 0
    igpnod = 0
    indx = 0
    indxkc = 0
    indxkf = 0
    inpfil = 0
    la1req = 0
    lchf = 0
    length = 0
    lhtc = 0
    luout = 0
    modfd = 0
    modmw = 0
    mpdcay = 0
    naazp = 0
    naazpd = 0
    naxn = 0
    nkf = 0
    nchn = 0
    nchfmd = 0
    ndtmax = 0
    nedtsw = 0
    nerr = 0
    nfastf = 0
    ngbh = 0
    nhbh = 0
    nhinta = 0
    nhtca = 0
    nhtclv = 0
    nhtcz = 0
    nhupta = 0
    nitmin = 0
    nkc = 0
    npaxp = 0
    npaxp1 = 0
    npbh = 0
    npid = 0
    nplnt = 0
    nptha = 0
    nptha1 = 0
    nvoid = 0
    ixazmn = 0
    nswinw = 0
    NFrapconInitialization = 0
    nrazp = 0
    nrazpd = 0
    ngaspr = 0
    ngastmp = 0
    iStoicGrad = 0
    itdmx = 0
    kbot = 0
    knonue = 0
    IndexInitTemp = 0
    nitdt = 0
    NSteadyTrans = 0
    numdta = 0
    ndtred = 0
    nPelRadDeviat = 0
    nqchn = 0
    xtime = 0.0_r8k
    modfal = 0
    nswpm = 0
    irest2 = 0
    ncs = 0
    irest3 = 0
    indxjk = 0
    nrc = 0
    dtmaxa = 0.0_r8k
    arest1 = 0.0_r8k
    azpang = 0.0_r8k
    fluxz = 0.0_r8k
    tplna = 0.0_r8k
    
    END SUBROUTINE Allocate_resti_var
    !
    SUBROUTINE Allocate_material_var
    !>@brief
    !> This subroutine allocates the Material variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/12/2014
    
    ! Allocate the arrays
    ALLOCATE (imaterials(1:nradialnodes))
    ALLOCATE (imatflag(1:nradialnodes))
    ALLOCATE (imatflag1(1:nradialnodes))
    ALLOCATE (iDatapairs(1:nradialnodes))
    ALLOCATE (iheattablen(1:nradialnodes))
    
    ! Set the default values
    nomat = 0
    imaterials = 0
    imatflag = 0
    imatflag1 = 0
    iDatapairs = 0
    iheattablen = 0
    
    END SUBROUTINE Allocate_material_var
    !
    SUBROUTINE Allocate_Variable_Sizes
    !>@brief
    !> This subroutine allocates the sizes used to define dimensions for dynamically allocated arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/14/2014
    
    ! Set the default values
    ntimesteps = pre_nt
    naxialnodes = pre_na
    nradialnodes = pre_nr
    
    END SUBROUTINE Allocate_Variable_Sizes
    !
    SUBROUTINE Allocate_Variables_var
    !>@brief
    !> This subroutine allocates the Variable variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/14/2014
    
    ! Allocate the arrays
    ALLOCATE (nm(1:3))
    ALLOCATE (htco(1:naxialnodes))
    ALLOCATE (tem(1:naxialnodes))
    ALLOCATE (pbh1(1:ntimesteps))
    ALLOCATE (pbh2(1:ntimesteps))
    ALLOCATE (hlqcl(1:ntimesteps))
    ALLOCATE (ts(1:ntimesteps))
    ALLOCATE (gadoln(1:naxialnodes))
    ALLOCATE (oldepeff(1:naxialnodes))
    ALLOCATE (oldeps(1:naxialnodes))
    ALLOCATE (afrap(1:40000))
    ALLOCATE (amatpc(1))
    ALLOCATE (ispace(1:11))
    ALLOCATE (emflag(1:21))
    ALLOCATE (iok(1:5,1:naxialnodes))
    ALLOCATE (iokold(1:5,1:naxialnodes))
    ALLOCATE (FrapconTemp(1:nradialnodes,1:naxialnodes))
    ALLOCATE (qcold(1:20,1:20))
    
    ! Set the default values
    iplant = -4
    gammait = 0
    maxidx = 0
    nthermex = 0
    e = 0.0_r8k
    pois = 0.0_r8k
    modheat = 0.02_r8k
    DefaultTemp = 77.0_r8k
    dtmpcl = 0.0_r8k
    tref = 0.0_r8k
    printrodburst = .TRUE.
    printballoon = .TRUE.
    ModHeatModel = 'Default'
    nm = 0
    htco = 0
    tem = 0
    pbh1 = 0.0_r8k
    pbh2 = 0.0_r8k
    hlqcl = 0.0_r8k
    ts = 0.0_r8k
    gadoln = 0.0_r8k
    oldepeff = 0.0_r8k
    oldeps = 0.0_r8k
    afrap = 0.0_r8k
    amatpc = 0.0_r8k
    ispace = 0.0_r8k
    emflag = ' '
    iok = 0
    iokold = 0
    FrapconTemp = 0.0_r8k
    qcold = 0.0_r8k
    
    END SUBROUTINE Allocate_Variables_var
    !
    SUBROUTINE Allocate_store6_var
    !>@brief
    !> This subroutine allocates the variables used in the store6 subroutine
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/14/2014
    
    ! Allocate the arrays
    ALLOCATE (SEDPNNLold(1:naxialnodes))
    ALLOCATE (oldEffStrainPNNL(1:naxialnodes))
    ALLOCATE (SEDEPRIold(1:naxialnodes))
    ALLOCATE (EPStrain1old(1:naxialnodes))
    ALLOCATE (EPStrain2old(1:naxialnodes))
    ALLOCATE (EPStrain3old(1:naxialnodes))
    ALLOCATE (StressHoopOld(1:naxialnodes))
    ALLOCATE (StressRadialOld(1:naxialnodes))
    ALLOCATE (StressAxialOld(1:naxialnodes))
    ALLOCATE (CladEffStressOld(1:naxialnodes))
    ALLOCATE (hsolold(1:naxialnodes))
    ALLOCATE (OldCldPlasStrn(1:naxialnodes))
    
    ! Set the default values
    SEDPNNLold = 0.0_r8k
    oldEffStrainPNNL = 0.0_r8k
    SEDEPRIold = 0.0_r8k
    EPStrain1old = 0.0_r8k
    EPStrain2old = 0.0_r8k
    EPStrain3old = 0.0_r8k
    StressHoopOld = 0.0_r8k
    StressRadialOld = 0.0_r8k
    StressAxialOld = 0.0_r8k
    CladEffStressOld = 0.0_r8k
    hsolold = 0.0_r8k
    OldCldPlasStrn = 0.0_r8k
    
    END SUBROUTINE Allocate_store6_var
    !
    SUBROUTINE Allocate_thcntl_var
    !>@brief
    !> This subroutine allocates the Thcntl variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/19/2014

    ! Set the default values
    ncards = 0
    NRestart = 0
    defsize = 400
    ncool = 0
    ndtad = 0
    t1 = 0.0_r8k
    t2 = 0.0_r8k
    unit = .FALSE.
    
    END SUBROUTINE Allocate_thcntl_var
    !
    SUBROUTINE Allocate_frapc_var (radial, axial)
    IMPLICIT NONE
    !>@brief
    !> This subroutine allocates the POINTERs for the fuel rod that is being initialized and assigns default values.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/16/2014
    !
    INTEGER(ipk), INTENT(IN) :: radial, axial
    
    ! Allocate the arrays
    ALLOCATE (Iffrp(1:axial))
    ALLOCATE (pclrlp(1:axial))
    ALLOCATE (rmrlp(1:radial))
    ALLOCATE (hgpfrp(1:axial))
    ALLOCATE (ElevatThermHydr(1:axial))
    ALLOCATE (tmprlp(1:radial,1:axial))
    ALLOCATE (bufrp(1:axial))
    ALLOCATE (pgpfrp(1:axial))
    ALLOCATE (vrlfrp(1:axial))
    ALLOCATE (drdfrp(1:axial))
    ALLOCATE (ElevatFrap(1:axial))
    ALLOCATE (HTC_L(1:axial))
    ALLOCATE (HTC_V(1:axial))
    ALLOCATE (TBulk_L(1:axial))
    ALLOCATE (TBulk_V(1:axial))
    
    ! Set the default values
    idx2 = 0
    ncorlp = 0
    ncirlp = 0
    nforlp = 0
    kmxrlp = 0
    kmxfrp = 0
    ncool2 = 0
    ndtadv = 0
    ncard2 = 0
    nrest2 = 0
    tprlp = 0.0_r8k
    Allocate_Arrays_FT = .FALSE.
    convert_units = .TRUE.
    setunits = .FALSE.
    first_pass = .TRUE.
    FrapTranFN = 'fraptran.inp'
    pclrlp = 0.0_r8k
    rmrlp = 0.0_r8k
    hgpfrp = 0.0_r8k
    ElevatThermHydr = 0.0_r8k
    tmprlp = 0.0_r8k
    HTC_L = 0.0_r8k
    HTC_V = 0.0_r8k
    TBulk_L = 0.0_r8k
    TBulk_V = 0.0_r8k
    bufrp = 0.0_r8k
    pgpfrp = 0.0_r8k
    vrlfrp = 0.0_r8k
    drdfrp = 0.0_r8k
    ElevatFrap = 0.0_r8k
    
    END SUBROUTINE Allocate_frapc_var
    !
END MODULE Variables