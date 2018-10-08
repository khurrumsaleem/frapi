module tp_frstate

    implicit none

    type frstate
    logical :: is_export = .false. ! Flag denotes whether fraptran is used as an exported function or not
    REAL(8) :: maxfueltemp = 0.0_r8k
    REAL(8) :: maxcladtemp = 0.0_r8k
    REAL(8) :: maxgaspressure = 0.0_r8k
    REAL(8) :: maxenthalpy = 0.0_r8k
    INTEGER(4) :: pre_na, pre_nr, pre_nt
    INTEGER(4) :: idx
    INTEGER(4) , target :: ntimesteps
    INTEGER(4) , target :: naxialnodes
    INTEGER(4) , target :: nradialnodes
    INTEGER(4) , target :: iplant
    INTEGER(4) , target :: gammait
    INTEGER(4) , target :: maxidx
    INTEGER(4) , target :: nthermex
    REAL(8) , target :: e
    REAL(8) , target :: pois
    REAL(8) , target :: modheat
    REAL(8) , target :: DefaultTemp
    REAL(8) , target :: dtmpcl
    REAL(8) , target :: tref
    LOGICAL , target :: printrodburst
    LOGICAL , target :: printballoon
    CHARACTER(LEN=15) , target :: ModHeatModel
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: pbh1
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: pbh2
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: hlqcl
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: ts
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: htco
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: tem
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: gadoln
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: oldepeff
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: oldeps
    INTEGER(4), DIMENSION(:,:), ALLOCATABLE , target :: iok
    INTEGER(4), DIMENSION(:,:), ALLOCATABLE , target :: iokold
    REAL(8), DIMENSION(:,:), ALLOCATABLE , target :: FrapconTemp
    REAL(8), DIMENSION(:,:), ALLOCATABLE , target :: qcold
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: nm
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: afrap
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: amatpc
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: ispace
    CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE , target :: emflag
    INTEGER(4) , target :: ncards
    INTEGER(4) , target :: NRestart
    INTEGER(4) , target :: defsize
    INTEGER(4) , target :: ncool
    INTEGER(4) , target :: ndtad
    REAL(8) , target :: t1
    REAL(8) , target :: t2
    LOGICAL , target :: unit
    INTEGER(4) , target :: nomat
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: imaterials
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: imatflag
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: imatflag1
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: iDatapairs
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: iheattablen
    INTEGER(4) :: nt
    INTEGER(4) :: np
    INTEGER(4) :: ns
    INTEGER(4) :: ns2
    INTEGER(4) :: klp
    INTEGER(4) :: klp2
    INTEGER(4) :: llp
    INTEGER(4) :: nt5
    INTEGER(4) :: jpl
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: SEDPNNLold
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: oldEffStrainPNNL
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: SEDEPRIold
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: EPStrain1old
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: EPStrain2old
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: EPStrain3old
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: StressHoopOld
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: StressRadialOld
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: StressAxialOld
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: CladEffStressOld
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: hsolold
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: OldCldPlasStrn
    INTEGER(4) , target :: idebug
    INTEGER(4) , target :: ncmesh
    INTEGER(4) , target :: nfmesh
    INTEGER(4) , target :: nsc
    INTEGER(4) , target :: nsf
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: cmesh
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: fmesh
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: zelev
    INTEGER(4) , target :: nprofile
    INTEGER(4) , target :: NumAxProfiles
    REAL(8) , target :: apowrd
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: ProfileStartTime
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: RodAvePower
    REAL(8), DIMENSION(:,:), ALLOCATABLE , target :: AxPowProfile
    REAL(8), DIMENSION(:,:), ALLOCATABLE , target :: pazp
    INTEGER(4) , target :: nprad
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: RadPowProfile
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: CladdingPower
    INTEGER(4) , target :: itcntd
    REAL(8) , target :: crfpr
    REAL(8) , target :: zqchpr
    REAL(8) , target :: reflpr
    REAL(8) , target :: fldrpr
    REAL(8) , target :: TimeIncrement
    REAL(8) , target :: tplenb
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: aprntb
    INTEGER(4) , target :: Kswell
    INTEGER(4) , target :: Ifail
    REAL(8) , target :: vplenc
    REAL(8) , target :: tplen
    REAL(8) , target :: VolAveGasTemp
    REAL(8) , target :: pctop
    REAL(8) , target :: dvdtp
    REAL(8) , target :: vplenb
    REAL(8) , target :: gfloa1
    REAL(8) , target :: roughf
    REAL(8) , target :: roughc
    REAL(8) , target :: swllfr
    REAL(8) , target :: TotalVoidVol
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: ies
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: pswll0
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: roi
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: vs0
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: flowg
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: GasAx
    INTEGER(4) , target :: CladType
    INTEGER(4) , target :: ProtectiveOxide
    INTEGER(4) , target :: npair
    INTEGER(4) , target :: nIDoxide
    REAL(8) , target :: cexh2l
    REAL(8) , target :: explenumv
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: cexh2a
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: deltox
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: explenumt
    INTEGER(4) , target :: maxit
    INTEGER(4) , target :: IndexFinTemp
    INTEGER(4) , target :: indxpr
    INTEGER(4) , target :: IndexGeom
    INTEGER(4) , target :: IndexThermCon
    INTEGER(4) , target :: IndexThermConAdv
    INTEGER(4) , target :: IndexBC
    INTEGER(4) , target :: noiter
    INTEGER(4) , target :: Iflago
    INTEGER(4) , target :: Iflagn
    REAL(8) , target :: Time
    REAL(8) , target :: DebugTime
    REAL(8) , target :: DebugTimeStop
    LOGICAL , target :: Ndebug ! Flag for specifying if debug information should be written
    INTEGER(4) , target :: ierr
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: ibnopt
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: nopt
    INTEGER(4), DIMENSION(:), ALLOCATABLE , target :: nunopt
    INTEGER(4) , target :: npltn     !
    REAL(8) , target :: tplot        !
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: dtplta    ! 40
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: fdial
    INTEGER(4) , target :: nepp0
    REAL(8) , target :: cladid
    REAL(8) , target :: cladod
    REAL(8) , target :: cladtk
    REAL(8) , target :: rf
    REAL(8) , target :: FuelPelDiam
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: eppinp
    REAL(8), DIMENSION(:), ALLOCATABLE , target :: radpel

        character(len=200)                , target :: namerf               ! 
        character(len=3)                  , target :: reflood              ! 
        character(len=12)                 , target :: relocmodel           ! 
        character(len=3)                  , target :: deformation          ! 
        character(len=10)                 , target :: inst                 ! 
        character(len=3)                  , target :: metal                ! 
        character(len=3)                  , target :: mheat                ! 
        character(len=3)                  , target :: bheat                ! 
        character(len=3)                  , target :: coolant              ! 
        character(len=3)                  , target :: internal             ! 
        character(len=3)                  , target :: radiation            ! 
        integer(4)                      , target :: reflo                ! Suboption to specify reflood rate as a function of time
        integer(4)                      , target :: naxn                 ! 
        integer(4)                      , target :: cathca               ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
        integer(4)                      , target :: prestmp              ! 
        integer                           , target :: lowpl                ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
        integer(4)                      , target :: nbhtc                ! 
        integer(4)                      , target :: inp                  ! 
        integer(4)                      , target :: rtheta               ! 
        integer(4)                      , target :: geom                 ! Suboption to specify the inner radius of the flow shroud.
        integer(4)                      , target :: iStoicGrad           ! 
        integer                           , target :: nvol1                ! Number of coolant zones stacked on top of each other and surrounding fuel rod
        integer(4)                      , target :: jchf                 ! 
        integer(4)                      , target :: gasflo               ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
        integer(4)                      , target :: noball               ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
        integer(4)                      , target :: prescri              ! 
        integer(4)                      , target :: mechan               ! 
        integer(4)                      , target :: nfmesh               ! 
        integer(4)                      , target :: NumAxProfiles        ! 
        integer(4)                      , target :: azang                ! 
        integer(4)                      , target :: idoxid               ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
        integer(4)                      , target :: tape1                ! 
        integer(4)                      , target :: jtr                  ! 
        integer(4)                      , target :: NRestart             ! 
        integer(4)                      , target :: ncards               ! 
        integer(4)                      , target :: CladType             ! 
        integer                           , target :: massfl               ! Suboption to specify the coolant mass flux history
        integer(4)                      , target :: odoxid               ! 
        integer(4)                      , target :: nchn                 ! 
        integer(4)                      , target :: TranSwell            ! 
        integer(4)                      , target :: unitin               ! Option to specify that the input data are in SI units
        integer(4)                      , target :: geometry             ! Suboption to specify geometry parameters
        integer                           , target :: filmbo               ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
        integer(4)                      , target :: noiter               ! 
        integer(4)                      , target :: nthermex             ! 
        integer(4)                      , target :: ncmesh               ! 
        integer(4)                      , target :: IndexFC2Print        ! 
        integer(4)                      , target :: chf                  ! Suboption to select the CHF correlation to be used
        integer(4)                      , target :: irefine              ! 
        integer(4)                      , target :: ProtectiveOxide      ! 
        integer(4)                      , target :: presfgr              ! 
        integer                           , target :: coreav               ! Suboption to specify the core average coolant enthalpy history
        integer(4)                      , target :: cenvoi               ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
        integer(4)                      , target :: grass                ! 
        integer(4)                      , target :: baker                ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
        integer(4)                      , target :: ncolbp               ! 
        integer(4)                      , target :: geomet               ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
        integer(4)                      , target :: soltyp               ! Option to specify an explicit solution
        integer(4)                      , target :: naz                  ! 
        integer(4)                      , target :: profile              ! 
        integer(4)                      , target :: nsym                 ! 
        integer(4)                      , target :: unitout              ! Option to specify that the output is to be in SI units even though the input is in British units
        integer(4)                      , target :: tape2                !                
        integer(4)                      , target :: nvol2                !                 
        integer(4)                      , target :: zone                 !                
        integer(4)                      , target :: upppl                !                 
        integer(4)                      , target :: jfb                  !              
        integer(4)                      , target :: nucbo                !                
        integer(4)                      , target :: plenumtemp           !                     
        integer(4)                      , target :: liquid               ! Suboption to specify the collapsed liquid level as the line of demarcation instead of the rupture plane
        integer(4)                      , target :: inlet                ! Suboption to specify the fraction of flooding water carried out of the core
        integer(4)                      , target :: radiat               ! Suboption to specify the radiation heat transfer at the cladding surface during reflood.
        integer(4)                      , target :: axpow                !                
        integer(4)                      , target :: bowing               !                
        integer(4)                      , target :: spefbz               !                  
        integer(4)                      , target :: nbundl               !                  
        integer(4)                      , target :: refloodtime          !                      
        integer(4)                      , target :: collaps              !                  
        integer(4)                      , target :: frapt4               !                  
        integer(4)                      , target :: maxit                !     
        integer(4)                      , target :: irupt                !     
        integer(4)                      , target :: nIDoxide             !     
        integer(4)                      , target :: IndexGrainBndSep     !     
        integer(4)                      , target :: coldwa               !     
        integer(4)                      , target :: ruptur               !     
        integer(4)                      , target :: pressu               !     
        integer(4)                      , target :: pow                  !     
        integer(4)                      , target :: press                !     
        integer(4)                      , target :: res                  !     
        integer(4)                      , target :: pressure             !     
        integer(4)                      , target :: nce                  !     
        integer(4)                      , target :: temp                 !     
        real(8)                         , target :: pl                   ! 
        real(8)                         , target :: epsht1               ! 
        real(8)                         , target :: ph                   ! 
        real(8)                         , target :: buoxide              !                  
        real(8)                         , target :: splbp                ! 
        real(8)                         , target :: tpowf                ! 
        real(8)                         , target :: ruptstrain           ! 
        real(8)                         , target :: frcoef               ! 
        real(8)                         , target :: CladPower            ! 
        real(8)                         , target :: pitch                ! 
        real(8)                         , target :: bowthr               ! 
        real(8)                         , target :: hrad                 ! 
        real(8)                         , target :: RodDiameter          ! 
        real(8)                         , target :: refdtm               ! 
        real(8)                         , target :: totnb                ! 
        real(8)                         , target :: FuelPelDiam          ! 
        real(8)                         , target :: gapthk               ! 
        real(8)                         , target :: flxsec               ! 
        real(8)                         , target :: ffch                 ! 
        real(8)                         , target :: roughc               ! 
        real(8)                         , target :: roughf               ! 
        real(8)                         , target :: prsacc               ! 
        real(8)                         , target :: fpowr                ! 
        real(8)                         , target :: dofset               ! 
        real(8)                         , target :: zs                   ! 
        real(8)                         , target :: spdbp                ! 
        real(8)                         , target :: pelh                 ! 
        real(8)                         , target :: pdrato               ! 
        real(8)                         , target :: dofang               ! 
        real(8)                         , target :: tgas0                ! 
        real(8)                         , target :: tsntrk               ! 
        real(8)                         , target :: achn                 ! 
        real(8)                         , target :: doffst               ! 
        real(8)                         , target :: RodLength            ! 
        real(8)                         , target :: OpenPorosityFraction ! 
        real(8)                         , target :: zad                  ! 
        real(8)                         , target :: rshrd                ! 
        real(8)                         , target :: tref                 ! 
        real(8)                         , target :: tflux                ! 
        real(8)                         , target :: emptm                ! 
        real(8)                         , target :: trise                ! 
        real(8)                         , target :: fltgap2              ! 
        real(8)                         , target :: hydiam               ! 
        real(8)                         , target :: dishd                ! 
        real(8)                         , target :: dtss                 ! 
        real(8)                         , target :: bup                  ! 
        real(8)                         , target :: cldwdc               ! 
        real(8)                         , target :: timop                ! 
        real(8)                         , target :: coldbp               ! 
        real(8)                         , target :: cfluxa               ! 
        real(8)                         , target :: rvoid                ! 
        real(8)                         , target :: fltgap               ! 
        real(8)                         , target :: frpo2                ! 
        real(8)                         , target :: trest                ! 
        real(8)                         , target :: refine               ! 
        real(8)                         , target :: modheat              ! 
        real(8)                         , target :: tmpac1               ! 
        real(8)                         , target :: coldw                ! 
        real(8)                         , target :: dhe                  ! 
        real(8)                         , target :: explenumv            ! 
        real(8)                         , target :: dhy                  ! 
        real(8)                         , target :: volbp                ! 
        real(8)                         , target :: powop                ! 
        real(8)                         , target :: fotmtl               ! 
        real(8)                         , target :: gsms                 ! 
        real(8)                         , target :: dishv0               ! 
        real(8)                         , target :: rnbnt                ! 
        real(8)                         , target :: zvoid2               ! 
        real(8)                         , target :: zvoid1               ! 
        real(8)                         , target :: fgrns                ! 
        real(8)                         , target :: rshd                 ! 
        real(8)                         , target :: gasmoles0            !     
        real(8)                         , target :: frden                !     
        real(8)                         , target :: fpdcay               !     
        integer(4) , dimension(:)       , target :: tem                  !     
        integer(4) , dimension(:)       , target :: ngastmp              ! 
        integer(4) , dimension(:)       , target :: htco                 ! 
        integer(4) , dimension(:)       , target :: ncs                  ! 
        real(8)    , dimension(:)       , target :: hupta                !     
        real(8)    , dimension(:)       , target :: hinta                !     
        real(8)    , dimension(:)       , target :: hbh                  !     
        real(8)    , dimension(:)       , target :: gbh                  ! 
        real(8)    , dimension(:,:)     , target :: htca                 ! 
        real(8)    , dimension(:)       , target :: gbse                 ! 
        real(8)    , dimension(:)       , target :: scd                  ! 
        real(8)    , dimension(:)       , target :: dtplta               ! 
        real(8)    , dimension(:)       , target :: ProfileStartTime     ! 
        real(8)    , dimension(:)       , target :: radpel               ! 
        real(8)    , dimension(:)       , target :: azpang               ! 
        real(8)    , dimension(:)       , target :: htclev               ! 
        real(8)    , dimension(:)       , target :: dtmaxa               ! 
        real(8)    , dimension(:)       , target :: gadoln               ! 
        real(8)    , dimension(:,:)     , target :: radtemp              ! 
        real(8)    , dimension(:,:)     , target :: fuelrad              ! 
        real(8)    , dimension(:)       , target :: ExtentOfBow          ! 
        real(8)    , dimension(:)       , target :: gfrac                ! 
        real(8)    , dimension(:)       , target :: FuelGasSwell         ! 
        real(8)    , dimension(:)       , target :: temptm               ! 
        real(8)    , dimension(:)       , target :: fmesh                ! 
        real(8)    , dimension(:)       , target :: pbh2                 ! 
        real(8)    , dimension(:)       , target :: pbh1                 ! 
        real(8)    , dimension(:)       , target :: fluxz                ! 
        real(8)    , dimension(:)       , target :: hlqcl                ! 
        real(8)    , dimension(:)       , target :: nodchf               ! 
        real(8)    , dimension(:,:)     , target :: AxPowProfile         ! 
        real(8)    , dimension(:,:)     , target :: gasths               ! 
        real(8)    , dimension(:)       , target :: RodAvePower          ! 
        real(8)    , dimension(:)       , target :: swd                  ! 
        real(8)    , dimension(:)       , target :: oxideod              ! 
        real(8)    , dimension(:)       , target :: cexh2a               ! 
        real(8)    , dimension(:,:)     , target :: pazp                 ! 
        real(8)    , dimension(:)       , target :: fldrat               ! 
        real(8)    , dimension(:)       , target :: cmesh                ! 
        real(8)    , dimension(:)       , target :: butemp               ! 
        real(8)    , dimension(:)       , target :: oxideid              ! 
        real(8)    , dimension(:)       , target :: gasphs               ! 
        real(8)    , dimension(:)       , target :: spl                  ! 
        real(8)    , dimension(:)       , target :: eppinp               ! 
        real(8)    , dimension(:)       , target :: explenumt            ! 
        real(8)    , dimension(:)       , target :: dtpoa                ! 
        real(8)    , dimension(:)       , target :: techf                ! 
        real(8)    , dimension(:,:)     , target :: tblka                ! 
        real(8)    , dimension(:)       , target :: relfraca             ! 
        real(8)    , dimension(:)       , target :: zelev                ! 
        real(8)    , dimension(:)       , target :: tschf                ! 
        real(8)    , dimension(:)       , target :: gappr0               ! 
        real(8)    , dimension(:)       , target :: prestm               ! 
        real(8)    , dimension(:)       , target :: vplen                ! 

        contains

        procedure alloc
        procedure save
        procedure load

    end type frstate

    contains

    subroutine alloc
        ALLOCATE (cexh2a(1:naxialnodes))
        ALLOCATE (deltox(1:naxialnodes))
        ALLOCATE (explenumt(1:2*ntimesteps))
        ALLOCATE (tEnd(1:ntimepairs))
        ALLOCATE (gfInt(1:ntimepairs))
        ALLOCATE (edInt(1:ntimepairs))
        ALLOCATE (CladdingPower(1:naxialnodes))
        ALLOCATE (RadPowProfile(1:size1))
        ALLOCATE (radtemp(1:naxialnodes,1:ntimesteps))
        ALLOCATE (fuelrad(1:naxialnodes,1:ntimesteps))
        ALLOCATE (ndchfi(1:naxialnodes))
        ALLOCATE (bowrat(1:naxialnodes))
        ALLOCATE (techfi(1:naxialnodes))
        ALLOCATE (tschfi(1:naxialnodes))
        ALLOCATE (hlqcl1(1:ntimesteps))
        ALLOCATE (fdial(1:30))
        ALLOCATE (ibnopt(1:33))
        ALLOCATE (nopt(1:8))
        ALLOCATE (nunopt(1:14))
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
        ALLOCATE (butemp(1:naxialnodes*nradialnodes))
        ALLOCATE (aprntb(1))
        ALLOCATE (ies(1))
        ALLOCATE (pswll0(1))
        ALLOCATE (roi(1))
        ALLOCATE (vs0(1))
        ALLOCATE (flowg(1))
        ALLOCATE (GasAx(1:naxialnodes))
        ALLOCATE (ProfileStartTime(1:ntimesteps))
        ALLOCATE (RodAvePower(1:ntimesteps))
        ALLOCATE (AxPowProfile(1:2*naxialnodes,1:ntimesteps))
        ALLOCATE (pazp(1:2*nradialnodes, 1:naxialnodes))
        ALLOCATE (cmesh(1:nradialnodes))
        ALLOCATE (fmesh(1:nradialnodes))
        ALLOCATE (zelev(1:naxialnodes))
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
        ALLOCATE (relfraca(1:2*ntimesteps))
        ALLOCATE (FuelGasSwell(1:2*ntimesteps))
        ALLOCATE (ansr(1:nradialnodes))
        ALLOCATE (fmgp(1:naxialnodes))
        ALLOCATE (gasavail1(1:naxialnodes,1:nradialnodes)) ! 1:ngasr
        ALLOCATE (gasavail2(1:naxialnodes,1:nradialnodes)) ! 1:ngasr
        ALLOCATE (farbal(1:20))
        ALLOCATE (sdfar(1:20))
        ALLOCATE (zfarbl(1:20))
        ALLOCATE (ablona(1:73))
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
        ALLOCATE (aexcb(1))
        ALLOCATE (dtpoa(1:pre_nt+2))
        ALLOCATE (tplbot(1:6,1:2))
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
        ALLOCATE (eppinp(1:2*naxialnodes))
        ALLOCATE (radpel(1:2*naxialnodes))
        ALLOCATE (dtplta(1:pre_nt+2))
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
        ALLOCATE (imaterials(1:nradialnodes))
        ALLOCATE (imatflag(1:nradialnodes))
        ALLOCATE (imatflag1(1:nradialnodes))
        ALLOCATE (iDatapairs(1:nradialnodes))
        ALLOCATE (iheattablen(1:nradialnodes))
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

    end subroutine alloc


end module frstate