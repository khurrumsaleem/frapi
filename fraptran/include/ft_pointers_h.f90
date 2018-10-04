    character(len=200)                , pointer :: restart_file_name    ! 
    character(len=3)                  , pointer :: reflood              ! 
    character(len=12)                 , pointer :: relocmodel           ! 
    character(len=3)                  , pointer :: deformation          ! 
    character(len=10)                 , pointer :: inst                 ! 
    character(len=3)                  , pointer :: metal                ! 
    character(len=3)                  , pointer :: mheat                ! 
    character(len=3)                  , pointer :: bheat                ! 
    character(len=3)                  , pointer :: coolant              ! 
    character(len=3)                  , pointer :: internal             ! 
    character(len=3)                  , pointer :: radiation            ! 
    integer(ipk)                      , pointer :: reflo                ! Suboption to specify reflood rate as a function of time
    integer(ipk)                      , pointer :: naxn                 ! 
    integer(ipk)                      , pointer :: cathca               ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
    integer(ipk)                      , pointer :: prestmp              ! 
    integer                           , pointer :: lowpl                ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
    integer(ipk)                      , pointer :: nbhtc                ! 
    integer(ipk)                      , pointer :: inp                  ! 
    integer(ipk)                      , pointer :: rtheta               ! 
    integer(ipk)                      , pointer :: geom                 ! Suboption to specify the inner radius of the flow shroud.
    integer(ipk)                      , pointer :: iStoicGrad           ! 
    integer                           , pointer :: nvol1                ! Number of coolant zones stacked on top of each other and surrounding fuel rod
    integer(ipk)                      , pointer :: jchf                 ! 
    integer(ipk)                      , pointer :: gasflo               ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
    integer(ipk)                      , pointer :: noball               ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
    integer(ipk)                      , pointer :: prescri              ! 
    integer(ipk)                      , pointer :: mechan               ! 
    integer(ipk)                      , pointer :: nfmesh               ! 
    integer(ipk)                      , pointer :: NumAxProfiles        ! 
    integer(ipk)                      , pointer :: azang                ! 
    integer(ipk)                      , pointer :: idoxid               ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
    integer(ipk)                      , pointer :: tape1                ! 
    integer(ipk)                      , pointer :: jtr                  ! 
    integer(ipk)                      , pointer :: NRestart             ! 
    integer(ipk)                      , pointer :: ncards               ! 
    integer(ipk)                      , pointer :: CladType             ! 
    integer                           , pointer :: massfl               ! Suboption to specify the coolant mass flux history
    integer(ipk)                      , pointer :: odoxid               ! 
    integer(ipk)                      , pointer :: nchn                 ! 
    integer(ipk)                      , pointer :: TranSwell            ! 
    integer(ipk)                      , pointer :: unitin               ! Option to specify that the input data are in SI units
    integer(ipk)                      , pointer :: geometry             ! Suboption to specify geometry parameters
    integer                           , pointer :: filmbo               ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
    integer(ipk)                      , pointer :: noiter               ! 
    integer(ipk)                      , pointer :: nthermex             ! 
    integer(ipk)                      , pointer :: ncmesh               ! 
    integer(ipk)                      , pointer :: IndexFC2Print        ! 
    integer(ipk)                      , pointer :: chf                  ! Suboption to select the CHF correlation to be used
    integer(ipk)                      , pointer :: irefine              ! 
    integer(ipk)                      , pointer :: ProtectiveOxide      ! 
    integer(ipk)                      , pointer :: presfgr              ! 
    integer                           , pointer :: coreav               ! Suboption to specify the core average coolant enthalpy history
    integer(ipk)                      , pointer :: cenvoi               ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
    integer(ipk)                      , pointer :: grass                ! 
    integer(ipk)                      , pointer :: baker                ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
    integer(ipk)                      , pointer :: ncolbp               ! 
    integer(ipk)                      , pointer :: geomet               ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
    integer(ipk)                      , pointer :: soltyp               ! Option to specify an explicit solution
    integer(ipk)                      , pointer :: naz                  ! 
    integer(ipk)                      , pointer :: profile              ! 
    integer(ipk)                      , pointer :: nsym                 ! 
    integer(ipk)                      , pointer :: unitout              ! Option to specify that the output is to be in SI units even though the input is in British units
    integer(ipk)                      , pointer :: tape2                !                
    integer(ipk)                      , pointer :: nvol2                !                 
    integer(ipk)                      , pointer :: zone                 !                
    integer(ipk)                      , pointer :: upppl                !                 
    integer(ipk)                      , pointer :: jfb                  !              
    integer(ipk)                      , pointer :: nucbo                !                
    integer(ipk)                      , pointer :: plenumtemp           !                     
    integer(ipk)                      , pointer :: liquid               ! Suboption to specify the collapsed liquid level as the line of demarcation instead of the rupture plane
    integer(ipk)                      , pointer :: inlet                ! Suboption to specify the fraction of flooding water carried out of the core
    integer(ipk)                      , pointer :: radiat               ! Suboption to specify the radiation heat transfer at the cladding surface during reflood.
    integer(ipk)                      , pointer :: axpow                !                
    integer(ipk)                      , pointer :: bowing               !                
    integer(ipk)                      , pointer :: spefbz               !                  
    integer(ipk)                      , pointer :: nbundl               !                  
    integer(ipk)                      , pointer :: refloodtime          !                      
    integer(ipk)                      , pointer :: collaps              !                  
    integer(ipk)                      , pointer :: frapt4               !                  
    integer(ipk)                      , pointer :: maxit                !     
    integer(ipk)                      , pointer :: irupt                !     
    integer(ipk)                      , pointer :: nIDoxide             !     
    integer(ipk)                      , pointer :: IndexGrainBndSep     !     
    integer(ipk)                      , pointer :: coldwa               !     
    integer(ipk)                      , pointer :: ruptur               !     
    integer(ipk)                      , pointer :: pressu               !     
    integer(ipk)                      , pointer :: pow                  !     
    integer(ipk)                      , pointer :: press                !     
    integer(ipk)                      , pointer :: res                  !     
    integer(ipk)                      , pointer :: pressure             !     
    integer(ipk)                      , pointer :: nce                  !     
    integer(ipk)                      , pointer :: temp                 !     
    real(r8k)                         , pointer :: pl                   ! 
    real(r8k)                         , pointer :: epsht1               ! 
    real(r8k)                         , pointer :: ph                   ! 
    real(r8k)                         , pointer :: buoxide              !                  
    real(r8k)                         , pointer :: splbp                ! 
    real(r8k)                         , pointer :: tpowf                ! 
    real(r8k)                         , pointer :: ruptstrain           ! 
    real(r8k)                         , pointer :: frcoef               ! 
    real(r8k)                         , pointer :: CladPower            ! 
    real(r8k)                         , pointer :: pitch                ! 
    real(r8k)                         , pointer :: bowthr               ! 
    real(r8k)                         , pointer :: hrad                 ! 
    real(r8k)                         , pointer :: RodDiameter          ! 
    real(r8k)                         , pointer :: refdtm               ! 
    real(r8k)                         , pointer :: totnb                ! 
    real(r8k)                         , pointer :: FuelPelDiam          ! 
    real(r8k)                         , pointer :: gapthk               ! 
    real(r8k)                         , pointer :: flxsec               ! 
    real(r8k)                         , pointer :: ffch                 ! 
    real(r8k)                         , pointer :: roughc               ! 
    real(r8k)                         , pointer :: roughf               ! 
    real(r8k)                         , pointer :: prsacc               ! 
    real(r8k)                         , pointer :: fpowr                ! 
    real(r8k)                         , pointer :: dofset               ! 
    real(r8k)                         , pointer :: zs                   ! 
    real(r8k)                         , pointer :: spdbp                ! 
    real(r8k)                         , pointer :: pelh                 ! 
    real(r8k)                         , pointer :: pdrato               ! 
    real(r8k)                         , pointer :: dofang               ! 
    real(r8k)                         , pointer :: tgas0                ! 
    real(r8k)                         , pointer :: tsntrk               ! 
    real(r8k)                         , pointer :: achn                 ! 
    real(r8k)                         , pointer :: doffst               ! 
    real(r8k)                         , pointer :: RodLength            ! 
    real(r8k)                         , pointer :: OpenPorosityFraction ! 
    real(r8k)                         , pointer :: zad                  ! 
    real(r8k)                         , pointer :: rshrd                ! 
    real(r8k)                         , pointer :: tref                 ! 
    real(r8k)                         , pointer :: tflux                ! 
    real(r8k)                         , pointer :: emptm                ! 
    real(r8k)                         , pointer :: trise                ! 
    real(r8k)                         , pointer :: fltgap2              ! 
    real(r8k)                         , pointer :: hydiam               ! 
    real(r8k)                         , pointer :: dishd                ! 
    real(r8k)                         , pointer :: dtss                 ! 
    real(r8k)                         , pointer :: bup                  ! 
    real(r8k)                         , pointer :: cldwdc               ! 
    real(r8k)                         , pointer :: timop                ! 
    real(r8k)                         , pointer :: coldbp               ! 
    real(r8k)                         , pointer :: cfluxa               ! 
    real(r8k)                         , pointer :: rvoid                ! 
    real(r8k)                         , pointer :: fltgap               ! 
    real(r8k)                         , pointer :: frpo2                ! 
    real(r8k)                         , pointer :: trest                ! 
    real(r8k)                         , pointer :: refine               ! 
    real(r8k)                         , pointer :: modheat              ! 
    real(r8k)                         , pointer :: tmpac1               ! 
    real(r8k)                         , pointer :: coldw                ! 
    real(r8k)                         , pointer :: dhe                  ! 
    real(r8k)                         , pointer :: explenumv            ! 
    real(r8k)                         , pointer :: dhy                  ! 
    real(r8k)                         , pointer :: volbp                ! 
    real(r8k)                         , pointer :: powop                ! 
    real(r8k)                         , pointer :: fotmtl               ! 
    real(r8k)                         , pointer :: gsms                 ! 
    real(r8k)                         , pointer :: dishv0               ! 
    real(r8k)                         , pointer :: rnbnt                ! 
    real(r8k)                         , pointer :: zvoid2               ! 
    real(r8k)                         , pointer :: zvoid1               ! 
    real(r8k)                         , pointer :: fgrns                ! 
    real(r8k)                         , pointer :: rshd                 ! 
    real(r8k)                         , pointer :: gasmoles0            !     
    real(r8k)                         , pointer :: frden                !     
    real(r8k)                         , pointer :: fpdcay               !     
    integer(ipk) , dimension(:)       , pointer :: tem                  !     
    integer(ipk) , dimension(:)       , pointer :: ngastmp              ! 
    integer(ipk) , dimension(:)       , pointer :: htco                 ! 
    integer(ipk) , dimension(:)       , pointer :: ncs                  ! 
    real(r8k)    , dimension(:)       , pointer :: hupta                !     
    real(r8k)    , dimension(:)       , pointer :: hinta                !     
    real(r8k)    , dimension(:)       , pointer :: hbh                  !     
    real(r8k)    , dimension(:)       , pointer :: gbh                  ! 
    real(r8k)    , dimension(:,:)     , pointer :: htca                 ! 
    real(r8k)    , dimension(:)       , pointer :: gbse                 ! 
    real(r8k)    , dimension(:)       , pointer :: scd                  ! 
    real(r8k)    , dimension(:)       , pointer :: dtplta               ! 
    real(r8k)    , dimension(:)       , pointer :: ProfileStartTime     ! 
    real(r8k)    , dimension(:)       , pointer :: radpel               ! 
    real(r8k)    , dimension(:)       , pointer :: azpang               ! 
    real(r8k)    , dimension(:)       , pointer :: htclev               ! 
    real(r8k)    , dimension(:)       , pointer :: dtmaxa               ! 
    real(r8k)    , dimension(:)       , pointer :: gadoln               ! 
    real(r8k)    , dimension(:,:)     , pointer :: radtemp              ! 
    real(r8k)    , dimension(:,:)     , pointer :: fuelrad              ! 
    real(r8k)    , dimension(:)       , pointer :: ExtentOfBow          ! 
    real(r8k)    , dimension(:)       , pointer :: gfrac                ! 
    real(r8k)    , dimension(:)       , pointer :: FuelGasSwell         ! 
    real(r8k)    , dimension(:)       , pointer :: temptm               ! 
    real(r8k)    , dimension(:)       , pointer :: fmesh                ! 
    real(r8k)    , dimension(:)       , pointer :: pbh2                 ! 
    real(r8k)    , dimension(:)       , pointer :: pbh1                 ! 
    real(r8k)    , dimension(:)       , pointer :: fluxz                ! 
    real(r8k)    , dimension(:)       , pointer :: hlqcl                ! 
    real(r8k)    , dimension(:)       , pointer :: nodchf               ! 
    real(r8k)    , dimension(:,:)     , pointer :: AxPowProfile         ! 
    real(r8k)    , dimension(:,:)     , pointer :: gasths               ! 
    real(r8k)    , dimension(:)       , pointer :: RodAvePower          ! 
    real(r8k)    , dimension(:)       , pointer :: swd                  ! 
    real(r8k)    , dimension(:)       , pointer :: oxideod              ! 
    real(r8k)    , dimension(:)       , pointer :: cexh2a               ! 
    real(r8k)    , dimension(:,:)     , pointer :: pazp                 ! 
    real(r8k)    , dimension(:)       , pointer :: fldrat               ! 
    real(r8k)    , dimension(:)       , pointer :: cmesh                ! 
    real(r8k)    , dimension(:)       , pointer :: butemp               ! 
    real(r8k)    , dimension(:)       , pointer :: oxideid              ! 
    real(r8k)    , dimension(:)       , pointer :: gasphs               ! 
    real(r8k)    , dimension(:)       , pointer :: spl                  ! 
    real(r8k)    , dimension(:)       , pointer :: eppinp               ! 
    real(r8k)    , dimension(:)       , pointer :: explenumt            ! 
    real(r8k)    , dimension(:)       , pointer :: dtpoa                ! 
    real(r8k)    , dimension(:)       , pointer :: techf                ! 
    real(r8k)    , dimension(:,:)     , pointer :: tblka                ! 
    real(r8k)    , dimension(:)       , pointer :: relfraca             ! 
    real(r8k)    , dimension(:)       , pointer :: zelev                ! 
    real(r8k)    , dimension(:)       , pointer :: tschf                ! 
    real(r8k)    , dimension(:)       , pointer :: gappr0               ! 
    real(r8k)    , dimension(:)       , pointer :: prestm               ! 
    real(r8k)    , dimension(:)       , pointer :: vplen                ! 
