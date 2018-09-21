    LOGICAL              ,                    POINTER :: calcoxide                               ! Flag to specify whether or not to calculate the oxidation reaction
    LOGICAL              ,                    POINTER :: updated_restart                         ! Indicates whether or not to write an updated FRAPTRAN restart file
    LOGICAL              ,                    POINTER :: gasflg                                  ! Convergence flag on gas pressure iteration
    LOGICAL              ,                    POINTER :: nconvg                                  ! Convergence flag on gap and pellet temperature distribution iteration
    LOGICAL              ,                    POINTER :: ncont                                   ! non-convergence index (30 iterations) on gap and pellet temperature distribution iteration
    LOGICAL              ,                    POINTER :: IsModelingSpentFuel                     !
    LOGICAL              ,                    POINTER :: hgapt_flag                              ! YU JIANKAI
    LOGICAL              ,                    POINTER :: flag_iapws                              ! YU JIANKAI
    Character   (LEN=80) ,                    POINTER :: title                                   ! Stores the title card information
    CHARACTER   (LEN=10) ,                    POINTER :: PrintType                               ! Identifier to print axial node loop or summary printout
    CHARACTER   (LEN=12) ,                    POINTER :: RelocModel                              ! Specifies fuel relocation model
    INTEGER     (ipk)    ,                    POINTER :: irefab                                  ! Timestep to start using refabricated values (Default = 10,000)
    INTEGER     (ipk)    ,                    POINTER :: nrefab1                                 ! Lower axial node for refabrication
    INTEGER     (ipk)    ,                    POINTER :: nrefab2                                 ! Upper axial node for refabrication
    INTEGER     (ipk)    ,                    POINTER :: idatingcreep                            ! Creep model choice (BE vs. conservative creep and Monkman-Grant)
    INTEGER     (ipk)    ,                    POINTER :: ncreephist                              ! Type of solution (1: helium cooling, 2: nitrogen cooling, 3: specified temperature, 4: specified temperature and stress
    INTEGER     (ipk)    ,                    POINTER :: ncreepstep                              ! # of creep output steps
    INTEGER     (ipk)    ,                    POINTER :: ncreeptab                               ! # of input pairs for ncreephist = 3 or 4
    INTEGER     (ipk)    ,                    POINTER :: DecayModel                              ! Decay model used. 1 = ANS-5.1-2004 (Default)
    INTEGER     (ipk)    ,                    POINTER :: lschni                                  !
    INTEGER     (ipk)    ,                    POINTER :: iquit                                   ! Indicator to terminate program
    INTEGER     (ipk)    ,                    POINTER :: ir1                                     ! Number of axial nodes + 1
    INTEGER     (ipk)    ,                    POINTER :: it                                      ! Time-step index
    INTEGER     (ipk)    ,                    POINTER :: itt                                     ! First time through burnup subroutine indicator
    INTEGER     (ipk)    ,                    POINTER :: iter                                    ! Gas loop iteration index
    INTEGER     (ipk)    ,                    POINTER :: jmin                                    ! Minimum (lowest) axial node being modeled
    INTEGER     (ipk)    ,                    POINTER :: jmax                                    ! Maximum (highest) axial node being modeled
    INTEGER     (ipk)    ,                    POINTER :: j                                       ! Axial node index currently being modeled
    INTEGER     (ipk)    ,                    POINTER :: jjc                                     ! Total number of axial power shapes
    INTEGER     (ipk)    ,                    POINTER :: jfix                                    ! Peak node used for output printing
    INTEGER     (ipk)    ,                    POINTER :: k                                       ! Gap iteration indicator
    INTEGER     (ipk)    ,                    POINTER :: m                                       ! Axial power shape index
    INTEGER     (ipk)    ,                    POINTER :: nab                                     ! Indicator for first tiem step to declare variables in fracas
    INTEGER     (ipk)    ,                    POINTER :: nplast                                  ! Elastic/Plastic flag (0 = Elastic calculation performed, 1 = Plastic calculation performed)
    INTEGER     (ipk)    ,                    POINTER :: nrm1                                    ! Number of radial nodes (nr) minus 1
    INTEGER     (ipk)    ,                    POINTER :: icor                                    ! Crud model (0 or 1 = constant layer, 2= time dependent)
    INTEGER     (ipk)    ,                    POINTER :: idxgas                                  ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
    INTEGER     (ipk)    ,                    POINTER :: ivardm                                  ! Option to specify variable axial node length (1 = ON, 0 = OFF(Default))
    INTEGER     (ipk)    ,                    POINTER :: ncreep                                  ! Creep flag (0 = no creep, 1 = creep)
    INTEGER     (ipk)    ,                    POINTER :: newprb                                  ! New problem indicator
    INTEGER     (ipk)    ,                    POINTER :: nheal                                   ! Flag to turn on permanent fuel crack healing (0 = Off,  1 = On)
    INTEGER     (ipk)    ,                    POINTER :: nt                                      ! Number of axial nodes minus 1
    INTEGER     (ipk)    ,                    POINTER :: nvoid                                   ! Central Void Index. (0 = No central void, 1 = Central void exists)   
    INTEGER     (ipk)    ,                    POINTER :: nplot                                   ! Output options Specifies whether to print plot information for use with Excel package FRAPlot or APT Plot (0 = no, 1 = limited, 2 = detailed)
    INTEGER     (ipk)    ,                    POINTER :: coupled                                 ! Specifies if coupled to T/H code  (0 = no, 1 = yes) [Not used]
    INTEGER     (ipk)    ,                    POINTER :: nfrttr                                  ! Specifies to create the output file read by the NRC's Internal AIG for TRACE Runs (0 = no, 1 = yes)
    INTEGER     (ipk)    ,                    POINTER :: nrestr                                  ! Specifies to write a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER     (ipk)    ,                    POINTER :: jdlpr                                   ! Specifies the output file printing option (-1 = axial summary, 0 = peak-power node, 1 = all axial nodes)
    INTEGER     (ipk)    ,                    POINTER :: nopt                                    ! Specifies the output file print control (0 = each timestep, 1 = input & summary only)
    INTEGER     (ipk)    ,                    POINTER :: nread                                   ! Specifies to start from a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER     (ipk)    ,                    POINTER :: ntape                                   ! Specifies to write a FRAPCON-to-FRAPTRAN restart tape  (0 = no, 1 = yes)
    INTEGER     (ipk)    ,                    POINTER :: ounit                                   ! Output File
    INTEGER     (ipk)    ,                    POINTER :: iunit                                   ! Input File  
    INTEGER     (ipk)    ,                    POINTER :: scrunit                                 ! Scratch file
    INTEGER     (ipk)    ,                    POINTER :: funit                                   ! .frttr file
    INTEGER     (ipk)    ,                    POINTER :: punit                                   ! FRAPCON-to-FRAPCON restart file WRITE
    INTEGER     (ipk)    ,                    POINTER :: ntaps                                   ! FRAPCON-to-FRAPCON restart file READ
    INTEGER     (ipk)    ,                    POINTER :: ntapi                                   ! FRAPCON-to-FRAPTRAN restart file
    INTEGER     (ipk)    ,                    POINTER :: ftunit                                  ! Dating file
    INTEGER     (ipk)    ,                    POINTER :: ddunit                                  ! Input/Output units (0 = SI, 1 = British)
    INTEGER     (ipk)    ,                    POINTER :: nunits                                  ! Signal for units system to be used for input and output: 1 = British units 0 = SI units
    INTEGER     (ipk)    ,                    POINTER :: graphna                                 ! Number of graphing axial nodes [Not used]
    INTEGER     (ipk)    ,                    POINTER :: graphnr                                 ! Number of graphing radial nodes [Not used]
    INTEGER     (ipk)    ,                    POINTER :: icm                                     ! Cladding type
    INTEGER     (ipk)    ,                    POINTER :: imox                                    ! Fuel type
    INTEGER     (ipk)    ,                    POINTER :: iplant                                  ! Plant type
    INTEGER     (ipk)    ,                    POINTER :: zr2vintage                              ! Zircaloy-2 vintage (Used when icm = 2) (0 = pre-1998, 1 = post-1998)
    INTEGER     (ipk)    ,                    POINTER :: iq                                      ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
    INTEGER     (ipk)    ,                    POINTER :: igas                                    ! Timestep to begin calculation of fission gas release
    INTEGER     (ipk)    ,                    POINTER :: igascal                                 ! Internal pressure calculation for FEA model
    INTEGER     (ipk)    ,                    POINTER :: ifixedtsurf                             ! Specify to use fixed cladding surface temperatures
    INTEGER     (ipk)    ,                    POINTER :: nsp                                     ! Specify which type of coolant conditions to use (0 = constant, 1 = time-dependent)
    INTEGER     (ipk)    ,                    POINTER :: ifixedcoolt                             ! Specify whether to use user-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER     (ipk)    ,                    POINTER :: ifixedcoolp                             ! Specify whether to use user-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER     (ipk)    ,                    POINTER :: TimeIntegration                         ! Specify time integration technique (0 = None, 1 = Linear Interpolation, 2 = Histogram)
    INTEGER     (ipk)    ,                    POINTER :: moxtype                                 ! Flag for type of Pu used in MOX (Used if icm = 1 or 2)
    INTEGER     (ipk)    ,                    POINTER :: cooltype                                ! Coolant type flag
    INTEGER     (ipk)    ,                    POINTER :: geom                                    ! Assembly geometry flag
    INTEGER     (ipk)    ,                    POINTER :: im_old                                  ! Original number of timesteps when invoking timestep reduction
    INTEGER     (ipk)    ,                    POINTER :: mechan                                  ! Cladding mechanical model
    INTEGER     (ipk)    ,                    POINTER :: ngasmod                                 ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
    REAL        (r8k)    ,                    POINTER :: gapmin                                  ! Minimum diametral gap thickness
    REAL        (r8k)    ,                    POINTER :: gaph                                    ! Hot state unrelocated radial gap (m)
    REAL        (r8k)    ,                    POINTER :: gapt                                    ! Hot state relocated radial gap (m)
    REAL        (r8k)    ,                    POINTER :: airin                                   ! Initial moles of air in the rod
    REAL        (r8k)    ,                    POINTER :: angi                                    ! Initial nitrogen concentration in the fuel (moles)
    REAL        (r8k)    ,                    POINTER :: an2in                                   ! Initial moles of N2 in the rod
    REAL        (r8k)    ,                    POINTER :: argin                                   ! Initial moles of Argon in the rod
    REAL        (r8k)    ,                    POINTER :: avflux                                  ! Average fast neutron flux
    REAL        (r8k)    ,                    POINTER :: bu                                      ! Rod average burnup to end of power-time stpe (MWd/mtU)
    REAL        (r8k)    ,                    POINTER :: buold                                   ! Rod average burnup to beginning of power-time stpe (MWd/mtU)
    REAL        (r8k)    ,                    POINTER :: cfv                                     ! Cold free volume including dishes and chamfers
    REAL        (r8k)    ,                    POINTER :: cfva                                    ! Cold free volume assuming cylindrical pellets
    REAL        (r8k)    ,                    POINTER :: convc                                   ! Convergence criteria on gap temperature drop
    REAL        (r8k)    ,                    POINTER :: cpv                                     ! Empty volume of the plenum
    REAL        (r8k)    ,                    POINTER :: ctemp                                   ! Average cladding temperature for axial node (j)
    REAL        (r8k)    ,                    POINTER :: dcobol                                  ! Outer clad diameter at beginning of life
    REAL        (r8k)    ,                    POINTER :: delbp                                   ! Average burnup during power-time step for node j (MWd/mtU)
    REAL        (r8k)    ,                    POINTER :: delbu                                   ! Rod average burnup during power-time step (MWd/mtU)
    REAL        (r8k)    ,                    POINTER :: delh                                    ! Time step size (hours)
    REAL        (r8k)    ,                    POINTER :: deltcr                                  ! Temeprature drop across the crud (F)
    REAL        (r8k)    ,                    POINTER :: deltdb                                  ! Temperature drop acros the film using Dittus-Boelter (F)
    REAL        (r8k)    ,                    POINTER :: deltfc                                  ! Temperature drop acros the film total using forced convection (F)
    REAL        (r8k)    ,                    POINTER :: deltjl                                  ! Temperature drop across the film using Jens-Lottes (F)
    REAL        (r8k)    ,                    POINTER :: delhs                                   ! Time step size (seconds)
    REAL        (r8k)    ,                    POINTER :: dlrel                                   ! Relative change in length with respect to the fuel length change (in)
    REAL        (r8k)    ,                    POINTER :: dlrod                                   ! Change in length of the active cladding length (in)
    REAL        (r8k)    ,                    POINTER :: dphf                                    ! Diameter of the fuel pellet hot and swelled
    REAL        (r8k)    ,                    POINTER :: dphfrl                                  ! Diameter of the fuel pellet hot and swelled plus relocation
    REAL        (r8k)    ,                    POINTER :: dhfll                                   ! Fuel stack axial extension (in)
    REAL        (r8k)    ,                    POINTER :: fgin                                    ! Initial moles of fission gas in the rod
    REAL        (r8k)    ,                    POINTER :: fpor1                                   ! Cold fuel open porosity fraction
    REAL        (r8k)    ,                    POINTER :: hcrv                                    ! Total hot crack volume (in^3)
    REAL        (r8k)    ,                    POINTER :: hcv                                     ! Total hot clad volume (in^3)
    REAL        (r8k)    ,                    POINTER :: hdshv                                   ! Total hot dish + interface volume (in**3) or just interface vol. if rc > 0.0 or hdish = 0.0
    REAL        (r8k)    ,                    POINTER :: hein                                    ! Initial moles of helium in the rod
    REAL        (r8k)    ,                    POINTER :: h2oin                                   ! Initial moles of water vapor in the rod
    REAL        (r8k)    ,                    POINTER :: h2in                                    ! Initial moles of hydrogen in the rod
    REAL        (r8k)    ,                    POINTER :: hfll                                    ! Maximum hot fuel stack length (in)
    REAL        (r8k)    ,                    POINTER :: hflmdb                                  ! Dittus-Boelter film heat transfer coefficient (btu/hr-ft**2-F)
    REAL        (r8k)    ,                    POINTER :: hflmp                                   ! Forced convection film coefficient (btu/hr-ft**2-F)
    REAL        (r8k)    ,                    POINTER :: hfper                                   ! Increase of fuel stack height (%)
    REAL        (r8k)    ,                    POINTER :: hfv                                     ! Total hot fuel volume (in^3)
    REAL        (r8k)    ,                    POINTER :: hgapt                                   ! Total gap conductance (btu/hr-ft^2-F)
    REAL        (r8k)    ,                    POINTER :: hgv                                     ! Total hot gap volume (in^3)
    REAL        (r8k)    ,                    POINTER :: hgap                                    ! Conduction contribution to conductance (btu/hr-ft**2-F)
    REAL        (r8k)    ,                    POINTER :: hsolid                                  ! Contact contribution to conductance (btu/hr-ft**2-F)
    REAL        (r8k)    ,                    POINTER :: hgapr                                   ! Radiation contribution to conductance (btu/hr-ft**2-F)
    REAL        (r8k)    ,                    POINTER :: hpl                                     ! Hot plenum length (in)
    REAL        (r8k)    ,                    POINTER :: hporv                                   ! Total hot porosity volume (in**3)
    REAL        (r8k)    ,                    POINTER :: hpv                                     ! Hot plenum volume (in**3)
    REAL        (r8k)    ,                    POINTER :: hva                                     ! Total annulus volume (in**3)
    REAL        (r8k)    ,                    POINTER :: h2omi                                   ! Initial water content in the fuel (moles)
    REAL        (r8k)    ,                    POINTER :: kryin                                   ! Initial moles of krypton in the rod
    REAL        (r8k)    ,                    POINTER :: nu                                      ! Nusselt number of plentum gas mixture
    REAL        (r8k)    ,                    POINTER :: pecdh                                   ! Fuel dish volume fraction
    REAL        (r8k)    ,                    POINTER :: rprm1                                   ! Unknown. Used in burnup calculations
    REAL        (r8k)    ,                    POINTER :: press                                   ! Rod intenal gas pressure (psia)
    REAL        (r8k)    ,                    POINTER :: prty                                    ! Initial fuel porosity
    REAL        (r8k)    ,                    POINTER :: rci                                     ! Cladding inner radius
    REAL        (r8k)    ,                    POINTER :: rco                                     ! Cladding outer radius
    REAL        (r8k)    ,                    POINTER :: rtemp                                   ! Average rate of change of temperature (K/s)
    REAL        (r8k)    ,                    POINTER :: qav                                     ! Average linear heat generation rate (kW/ft)
    REAL        (r8k)    ,                    POINTER :: qpeak                                   ! Peak linear heat generation rate (kW/ft)
    REAL        (r8k)    ,                    POINTER :: pkZrO2WtGain                            ! Peak ZrO2 weight gain (gram/m^2)
    REAL        (r8k)    ,                    POINTER :: rfnvff                                  ! Fuel roughness volume (%)
    REAL        (r8k)    ,                    POINTER :: rhofuel                                 ! As-fabricated fuel density (grams/in^3)
    REAL        (r8k)    ,                    POINTER :: rp                                      ! As-fabricated pellet radius (in)
    REAL        (r8k)    ,                    POINTER :: rstran                                  ! True cladding strain rate (1/s)
    REAL        (r8k)    ,                    POINTER :: rtran                                   ! Radius out to the region where crack begins (m)
    REAL        (r8k)    ,                    POINTER :: sumck                                   ! Sum of axial crack volumes / crack temperature (in^3/F)
    REAL        (r8k)    ,                    POINTER :: sumdh                                   ! Sum of axial dish volumes / fuel avg. temp. (in**3/F)
    REAL        (r8k)    ,                    POINTER :: sumexp                                  ! Total fuel surface displacement due to thermal expan (in)
    REAL        (r8k)    ,                    POINTER :: sumgp                                   ! Sum of axial gap volumes / gap temperatures (in**3/F)
    REAL        (r8k)    ,                    POINTER :: sumpor                                  ! Sum of axial porosity volumes / porosity temps (in**3/F)
    REAL        (r8k)    ,                    POINTER :: sumrg                                   ! Sum of axial roughness volumes / roughness temperature
    REAL        (r8k)    ,                    POINTER :: t                                       ! Total problem time (hours)
    REAL        (r8k)    ,                    POINTER :: thvv                                    ! Total free gas volume
    REAL        (r8k)    ,                    POINTER :: tci                                     ! Cladding inner surface temperature
    REAL        (r8k)    ,                    POINTER :: tco                                     ! Cladding outer surface temperature
    REAL        (r8k)    ,                    POINTER :: tfgfr                                   ! Cumulative fission gas release fraction
    REAL        (r8k)    ,                    POINTER :: thefr                                   ! Cumulative helium release fraction
    REAL        (r8k)    ,                    POINTER :: th2ofr                                  ! Cumulative water vapor release fraction
    REAL        (r8k)    ,                    POINTER :: tn2fr                                   ! Cumulative nitrogen release fraction
    REAL        (r8k)    ,                    POINTER :: tpa                                     ! Pellet average temperature
    REAL        (r8k)    ,                    POINTER :: tplen                                   ! Plenum temperature (F)
    REAL        (r8k)    ,                    POINTER :: transt                                  ! Temperature above which no cracking is assumed (K). Set to 80% of tsint
    REAL        (r8k)    ,                    POINTER :: tsat                                    ! Coolant saturation temperature (F)
    REAL        (r8k)    ,                    POINTER :: txa                                     ! Clad thermal expansion coefficient in radial direction (in/in-F)
    REAL        (r8k)    ,                    POINTER :: txal                                    ! Clad thermal expansion coefficient in axial direction (in/in-F)
    REAL        (r8k)    ,                    POINTER :: txc                                     ! Cladding thermal expansion coefficient (1/F)
    REAL        (r8k)    ,                    POINTER :: visc                                    ! Gas viscosity used in plenum temperature calculation
    REAL        (r8k)    ,                    POINTER :: vplt                                    ! Volume of pellet annulus
    REAL        (r8k)    ,                    POINTER :: wt                                      ! Average bulk coolant temperature at node j (F)
    REAL        (r8k)    ,                    POINTER :: xein                                    ! Initial moles of xenon in the rod
    REAL        (r8k)    ,                    POINTER :: zro2wg                                  ! ZrO2 Weight Gain
    REAL        (r8k)    ,                    POINTER :: cladavgtemp                             ! Average claddimg temperature
    REAL        (r8k)    ,                    POINTER :: tpca                                    ! Axial average centerline temperature (F)
    REAL        (r8k)    ,                    POINTER :: tntera                                  ! Axial averaged interface temperature (F)
    REAL        (r8k)    ,                    POINTER :: fnck                                    ! Input effective fast fluence for strength coefficient (neutrons/(m**2))
    REAL        (r8k)    ,                    POINTER :: fncn                                    ! Input effective fast fluence for strain hardening exponent at time step start (neutrons/(m**2))
    REAL        (r8k)    ,                    POINTER :: cwkf                                    ! Input effective cold work for strength coefficient (unitless ratio of areas)
    REAL        (r8k)    ,                    POINTER :: cwnf                                    ! Input effective cold work for strain hardening exponent at time step start (unitless ratio of areas)
    REAL        (r8k)    ,                    POINTER :: coldwk                                  ! Cladding cold work
    REAL        (r8k)    ,                    POINTER :: frcoef                                  ! Fuel/Cladding friction coefficient for FEA model
    REAL        (r8k)    ,                    POINTER :: afal                                    ! Additional fuel thermal expansion (multiplier)
    REAL        (r8k)    ,                    POINTER :: afdn                                    ! Additional fuel densification factor (multiplier)
    REAL        (r8k)    ,                    POINTER :: afgr                                    ! Additional fractional gas release factor
    REAL        (r8k)    ,                    POINTER :: amfair                                  ! Mole fraction of air
    REAL        (r8k)    ,                    POINTER :: amffg                                   ! Mole fraction of fission gas
    REAL        (r8k)    ,                    POINTER :: catexf                                  ! Clad texture factor
    REAL        (r8k)    ,                    POINTER :: chorg                                   ! As-fabricated clad hydrogen content (wt.ppm)
    REAL        (r8k)    ,                    POINTER :: cldwks                                  ! Clad cold work (0.5= SRA, 0.0= RXA)
    REAL        (r8k)    ,                    POINTER :: cpl                                     ! Cold plenum length
    REAL        (r8k)    ,                    POINTER :: crephr                                  ! Creep step duration (Defualt = 10hr)
    REAL        (r8k)    ,                    POINTER :: crdt                                    ! Constant crud thickness
    REAL        (r8k)    ,                    POINTER :: crdtr                                   ! Crud accumulation rate
    REAL        (r8k)    ,                    POINTER :: dishsd                                  ! Dish shoulder width
    REAL        (r8k)    ,                    POINTER :: fa                                      ! Peak-to-average power ratio (fa = 1 if iq = 0)
    REAL        (r8k)    ,                    POINTER :: fgpav                                   ! Fill gas pressure
    REAL        (r8k)    ,                    POINTER :: fotmtl                                  ! Fuel oxygen-to-metal ratio (Default = 2.0)
    REAL        (r8k)    ,                    POINTER :: chmfrh                                  ! Chamfer height
    REAL        (r8k)    ,                    POINTER :: chmfrw                                  ! Chamfer width
    REAL        (r8k)    ,                    POINTER :: hdish                                   ! Dish height
    REAL        (r8k)    ,                    POINTER :: hplt                                    ! Pellet height
    REAL        (r8k)    ,                    POINTER :: pitch                                   ! Center to center rod distance
    REAL        (r8k)    ,                    POINTER :: ppmh2o                                  ! Weight ppm H2O in fuel (wt.ppm)
    REAL        (r8k)    ,                    POINTER :: ppmn2                                   ! Weight ppm N2 in fuel (wt. ppm)
    REAL        (r8k)    ,                    POINTER :: rdish                                   ! Dish radius of curvature
    REAL        (r8k)    ,                    POINTER :: roughc                                  ! Clad roughness
    REAL        (r8k)    ,                    POINTER :: roughf                                  ! Fuel roughness
    REAL        (r8k)    ,                    POINTER :: sgapf                                   ! Fision Gas atoms per 100 fissions (Default = 31)
    REAL        (r8k)    ,                    POINTER :: tcc                                     ! As-fabricated cladding thickness
    REAL        (r8k)    ,                    POINTER :: tref                                    ! Reference temperature upon which the stored energy is based (F)
    REAL        (r8k)    ,                    POINTER :: TGasFab                                 ! Reference temeprature for specified gas fabrication pressure
    REAL        (r8k)    ,                    POINTER :: rsntr                                   ! Expected resintering density increase
    REAL        (r8k)    ,                    POINTER :: tsint                                   ! Pellet centering temperature
    REAL        (r8k)    ,                    POINTER :: grnsize                                 ! Input grain size (effective diameter) of the fuel (Default = 10.0 microns)
    REAL        (r8k)    ,                    POINTER :: b10                                     ! Boron-10 enrichment (atom %) in ZrB2
    REAL        (r8k)    ,                    POINTER :: zrb2thick                               ! ZrB2 thickness
    REAL        (r8k)    ,                    POINTER :: zrb2den                                 ! ZrB2 density
    REAL        (r8k)    ,                    POINTER :: prvden                                  ! Input total densification from previous time step (%) (For radial region l, axial node j)
    REAL        (r8k)    ,                    POINTER :: ifba                                    ! Percent IFBA rods in core (%)
    REAL        (r8k)    ,                    POINTER :: ftmelt                                  ! Fuel melting temperature
    REAL        (r8k)    ,                    POINTER :: fhefus                                  ! Fuel heat of fusion
    REAL        (r8k)    ,                    POINTER :: ctmelt                                  ! Cladding melting temperture
    REAL        (r8k)    ,                    POINTER :: chefus                                  ! Cladding heat of fusion
    REAL        (r8k)    ,                    POINTER :: ctranb                                  ! Beginning of alpha-beta transition temperature
    REAL        (r8k)    ,                    POINTER :: ctrane                                  ! End of alpha-beta transition temperature
    REAL        (r8k)    ,                    POINTER :: ctranz                                  ! Isothermal alpha-beta transition temperature
    REAL        (r8k)    ,                    POINTER :: fdelta                                  ! Variation in fuel melting temperature
    REAL        (r8k)    ,                    POINTER :: bup                                     ! Rod average burnup for node j (MWs/kgU)
    REAL        (r8k)    ,                    POINTER :: deloxy                                  ! Input average oxygen concentration excluding oxide layer - average oxygen concentration of
    REAL        (r8k)    ,                    POINTER :: zro2i                                   ! Previous oxide thickness, m
    REAL        (r8k)    ,                    POINTER :: zro2o                                   ! Current oxide thickness, m
    REAL        (r8k)    ,                    POINTER :: zoxk                                    ! ZrO2 thermal conductivity
    REAL        (r8k)    ,                    POINTER :: excesh2                                 ! Excess hydrogen in cladding
    REAL        (r8k)    ,                    POINTER :: frden                                   ! Fraction of theoretical density (= 0.01 * den)
    REAL        (r8k)    ,                    POINTER :: totl                                    ! Total length of active fuel
    REAL        (r8k)    ,                    POINTER :: rdot                                    ! Gas release fraction per axial node per cumulative time
    REAL        (r8k)    ,                    POINTER :: cvv                                     ! As-Fabricated Free Volume (in3)
    REAL        (r8k)    ,                    POINTER :: gmlesAsFab                              ! As-fabricated # of moles of gas
    REAL        (r8k)    ,                    POINTER :: gmlesReleased                           ! Released # of moles of gas
    REAL        (r8k)    ,                    POINTER :: vfrcpl                                  ! Volume fraction of gases contained in the plenum
    REAL        (r8k)    ,                    POINTER :: tplens                                  ! Plenum temperature (K)
    REAL        (r8k)    ,                    POINTER :: slim                                    ! User supplied swelling limit (vol fraction) (Default = 0.05)
    REAL        (r8k)    ,                    POINTER :: gaphtcmult                              ! User supplied gap heat transfer coefficient multiplier
    REAL        (r8k)    ,                    POINTER :: RestartTime                             ! Problem Time (s) to use for a restart calculation
    REAL        (r8k)    ,                    POINTER :: sigftc                                  ! Bias on fuel thermal conductivity model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigftex                                 ! Bias on fuel thermal expansion model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigfgr                                  ! Bias on fission gas release model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigswell                                ! Bias on fuel swelling model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigcreep                                ! Bias on cladding creep model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: siggro                                  ! Bias on cladding axial growth model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigcor                                  ! Bias on cladding corrosion model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: sigh2                                   ! Bias on cladding hydrogen pickup model (# of standard deviations)
    REAL        (r8k)    ,                    POINTER :: newtimestep                             ! The new timestep value to use for the calculation. (days) Used when TimeIntegration = 1 or 2
    REAL        (r8k)    ,                    POINTER :: stopox                                  ! Flag to specify when to stop the oxidation calcualtion
    REAL        (r8k)    ,                    POINTER :: deng                                    ! Fuel open porosity fraction (%TD)
    REAL        (r8k)    ,                    POINTER :: den                                     ! As-fabricated apparent fuel density (%TD)
    REAL        (r8k)    ,                    POINTER :: amfhe                                   ! Molar fraction of helium
    REAL        (r8k)    ,                    POINTER :: amfh2                                   ! Molar fraction of hydrogen
    REAL        (r8k)    ,                    POINTER :: amfn2                                   ! Molar fraction of nitrogen
    REAL        (r8k)    ,                    POINTER :: amfarg                                  ! Molar fraction of argon
    REAL        (r8k)    ,                    POINTER :: amfkry                                  ! Molar fraction of krypton
    REAL        (r8k)    ,                    POINTER :: amfxe                                   ! Molar fraction of xenon
    REAL        (r8k)    ,                    POINTER :: amfh2o                                  ! Molar fraction of water
    REAL        (r8k)    ,                    POINTER :: enrpu39                                 ! Fuel pellet Pu-239 content
    REAL        (r8k)    ,                    POINTER :: enrpu40                                 ! Fuel pellet Pu-240 content
    REAL        (r8k)    ,                    POINTER :: enrpu41                                 ! Fuel pellet Pu-241 content
    REAL        (r8k)    ,                    POINTER :: enrpu42                                 ! Fuel pellet Pu-242 content
    REAL        (r8k)    ,                    POINTER :: facmot                                  ! Input fraction of molten fuel (0.0 - solid, 1.0 - molten)
    REAL        (r8k)    ,                    POINTER :: DebugTime                               ! User supplied time for printing debug information
    REAL        (r8k)    ,                    POINTER :: modheat                                 ! Moderator heating fraction
    REAL        (r8k)    ,                    POINTER :: cladelmod                               ! User-supplied value for cladding elastic modulus (Pa)
    REAL        (r8k)    ,                    POINTER :: fuelreloc                               ! User-supplied value for fuel relocation
    REAL        (r8k)    ,                    POINTER :: gaprecov                                ! User-supplied value for gap recovery
    REAL        (r8k)    ,                    POINTER :: relocm_true                             ! Fuel relocation that is added to fuel surface displacement (*.5)
    REAL        (r8k)    ,                    POINTER :: relocm_mod                              ! Fuel relocation after accounting for gap thermal conductivity
    REAL        (r8k)    ,                    POINTER :: cplrefab                                ! Refabricated upper plenum length
    REAL        (r8k)    ,                    POINTER :: vsrefab                                 ! Number of spring turns in refabricated upper plenum
    REAL        (r8k)    ,                    POINTER :: dspgrefab                               ! New plenum spring coil diameter
    REAL        (r8k)    ,                    POINTER :: dspgwrefab                              ! New plenum spring wire diameter
    REAL        (r8k)    ,                    POINTER :: fgpavrefab                              ! Fill gas pressure at time step of refabrication
    REAL        (r8k)    ,                    POINTER :: airrefab                                ! Fraction of air in refabricated rod
    REAL        (r8k)    ,                    POINTER :: n2refab                                 ! Fraction of nitrogen in refabricated rod
    REAL        (r8k)    ,                    POINTER :: arrefab                                 ! Fraction of argon in refabricated rod
    REAL        (r8k)    ,                    POINTER :: fgrefab                                 ! Fraction of fission gas in refabricated rod
    REAL        (r8k)    ,                    POINTER :: herefab                                 ! Fraction of helium in refabricated rod (Default = 1.0)
    REAL        (r8k)    ,                    POINTER :: krrefab                                 ! Fraction of krypton in refabricated rod
    REAL        (r8k)    ,                    POINTER :: xerefab                                 ! Fraction of xenon in refabricated rod
    REAL        (r8k)    ,                    POINTER :: creeppooltime                           ! Time spent in pool after reactor discharge (years)
    REAL        (r8k)    ,                    POINTER :: creeptime                               ! Time duration of creep calculation (years)
    REAL        (r8k)    ,                    POINTER :: datingtstep                             ! Timestep size to be used in creep calculation (s)
    REAL        (r8k)    ,                    POINTER :: pfave                                   !                        
    REAL        (r8k)    ,                    POINTER :: por000                                  !
    REAL        (r8k)    ,                    POINTER :: ntot                                    !
    REAL        (r8k)    ,                    POINTER :: fpdcay                                  ! User specified decay heat multiplier (Default = 1.0)
    REAL        (r8k)    ,                    POINTER :: vs                                      ! Number of spring turns
    REAL        (r8k)    ,                    POINTER :: dspg                                    ! Spring diameter
    REAL        (r8k)    ,                    POINTER :: dspgw                                   ! Spring wire diameter
    REAL        (r8k)    ,                    POINTER :: vcold                                   ! 
    INTEGER     (ipk)    ,  DIMENSION(:,:,:), POINTER :: openp                                   !                           
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: IgapGapIndex                            ! Axial-Dependent Arrays, dimensioned (na)
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: IgapIndexOld                            !
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: IgapIndexPrevOld                        !
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: jn                                      ! # of qf, x pairs for each axial power shape
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: jnsurftemp                              ! # of cladt, xt pairs for each axial temperature distribution
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: jpeak                                   ! Maximum power node for each power shape
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: jst                                     ! Sequential # of the power shape to be used for each timestep
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: jstsurftemp                             ! Sequential # of the cladding temperature profile to be used for each timestep
    INTEGER     (ipk)    ,  DIMENSION(:),     POINTER :: CycleSteps                              ! Keeps track of the cycle steps, used for increased # of timestep runs   
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gases                                   ! Mole fraction of gas constituents. Fixed to ngases
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: releas                                  ! Fraction short-lived radioactive gases released
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dltgc                                   ! Gap temperature rise used for temperature convergence loop
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: totinner                                ! Total inner cladding surface displacement, mils
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: totcrl                                  ! Total outer cladding surface displacement, mils
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FastFluxd                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FastFluenced                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: BulkCoolantTemp                         ! Bulk Coolant Temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rhof                                    ! Density of the coolant
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dpwxrate                                !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: axialnode                               ! Axial node dimensions
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PelletRad                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladAveTemp                             ! Average cladding temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PelAveTemp                              ! Average fuel pellet temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: GapAveTemp                              ! Average gap temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PelSurfTemp                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PelCentTemp                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: sigeff                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelSurfDispl                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladInSurDisp                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladInPermDef                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: sigy                                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: AxialNodLength                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladEffPlasStrain                       !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: RinterfacPress                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelCladGap                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: GapPress                                !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CoolantPress                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PlastStrnep1                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldCladStrn                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldFuelStrn                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldGapPress                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldCoolPress                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldFuelDispl                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldCladAvTemp                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CreepStrain                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CreepStrain1                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladDiamHot                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevCladEffPlasStrn                     !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevFuelStrain                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: HotNodLength                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PermFuelDispl                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: BOSNodeburnup                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: EOSNodeburnup                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: StepNodeburnup                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: TotalHgap                               ! Gap conductance
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: SolidHgap                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: GasHgap                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: RadHgap                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FastFlux                                !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FastFluence                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelPorosity                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FilmCoefficient                         !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: EstGapDeltaT                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: BOSZrO2Thk                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: EOSZrO2Thk                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ZrO2ThkNoAd                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelCondFactor                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: Relocation                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: RinternalVolume                         !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladVolume                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CrackVolume                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: RinterfacVolume                         !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelVolume                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fden                                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: GapVolume                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PorosityVolume                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: SurfTempOxide                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: AnnulusVolume                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gapplot                                 ! Thermal gap
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevOldCoolPress                        !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevOldGapPress                         !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevOldCladAvTemp                       !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevOldFuelDispl                        !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevCreepStrain                         !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladIrradGrowStrn                       !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: UniformAxNodStrn                        !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladH2Concen                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ExcessH2Concen                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: PrevCladStrain                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: StartofStepH2Con                        !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: StartofStepPickupH2Con                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: EndofStepPickupH2Con                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FuelTempRestruRad                       !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: OldHealedCrackRadius                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: StoredEnergy                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: HealedCrackRadius                       !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: HotThermalGap                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: WorkArray1                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: WorkArray2                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: WorkArray3                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladInSurfTemp                          ! Cladding inner surface temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: CladOutSurfTemp                         ! Cladding outer surface temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: Power                                   !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gpthe                                   !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gpth                                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gpthpg                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: qc                                      ! Node heat flux (btu/hr-ft**2)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: totdef                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: buin                                    ! Input fuel burnup
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dci                                     ! Inner cladding diameter
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dco                                     ! Outer cladding diameter
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: thkcld                                  ! As-fabricated cladding thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: cdg                                     ! Diametral gap thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: thkgap                                  ! As-fabricated gap thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: de                                      ! Hydraulic diameter
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: deltaz                                  ! Axial node length (ft)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: comp                                    ! PuO2 weight percent if MOX fuel (wt%)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ctmax                                   !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dp                                      !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: flux                                    !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: qend                                    ! End-node to plenum heat transfer fraction (Default = 0.03). 1 value per power shape
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: crudmult                                ! Axial crud thickness multiplier
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: crdtt                                   ! Crud thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rdotwrt                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: FDItave                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: creapratearray                          !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: GapCond                                 ! BTU/(hr-ft^2-F) W/(m^2K) Gap Conductance
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fuelexptot                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fuelswltot                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fuelcreeptot                            !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fueldentot                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fuelburntot                             !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: cladcrptot                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gapHTC                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: oxidelayer                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gapmech                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gapthrm                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: fuelrelmod                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ifbarel                                 !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: NodalMoles                              ! Void volume per unit temperature at axial node
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: NodalGMLES                              ! Moles at each axial node
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rlcstrn                                 ! Relocation strain (unitless) (1/2 is added to the radial strain as permanent outward strain)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rlcstrnold                              ! Relocation strain at previous timestep
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rc                                      ! Radius of the fuel pellet central annulus (in)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gadoln                                  ! Gadolinia content at each axial node
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: colddef                                 ! Net permanent fuel displacement due to swelling and densification
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: colddec                                 ! Net cladding displacement due to creep/plastic deformation
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: qnode                                   ! Rod power at node j
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: heprod                                  ! Used in fgasre
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: he                                      ! Used in fgasre
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: heold                                   ! Used in fgasre
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: boron10                                 ! Used in fgasre
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enrch                                   ! Fuel U-235 enrichment (at%)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: epsav                                   ! Cladding strain
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: wimsburnup                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: oldwimsburnup                           !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: stold                                   !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: stnew                                   !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: deltimeold                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: deltimenew                              !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: sagold                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: sagnew                                  !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: delsagold                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: delsagnew                               !
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: delst                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: angr                                    ! Nitrogen release per node and power step
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: ah2ogr                                  ! Water release per node and power step
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: fgmgp                                   ! Fission gas production for each node (moles)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: fmgr                                    ! Cumulative fission gas release per node and power step
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: hemgp                                   ! Cumulative helium gas production per node and power step
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: hmgr                                    ! Cumulative helium release per node and power step
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: ang                                     ! Cumulative nitrogen concentration in fuel per power step (moles)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: ah2og                                   ! Cumulative water concentration in fuel per power step (moles)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: epp                                     !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: eppp                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: eppsv                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: eps                                     !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: feps                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: reps                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: repsv                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: repsp                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rfeps                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rfepp                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rfpsv                                   !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: sig                                     !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: epp1                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: sig1                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: eps1                                    !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: ThermalStrain                           !
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: RB_axial                                ! ANS 5.4 2011 axial release fractions array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: RB_rod                                  ! ANS 5.4 2011 total rod release fractions array
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: avgqi                                   ! Average of the q'' for each axial power shape
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: go                                      ! Coolant mass flux around fuel rod, input for each timestep if nsp = 1
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: p2                                      ! Coolant System Pressure, input for each timestep if nsp = 1
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ProblemTime                             ! Cumulative time at the end of each timestep
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ProblemTime_Prev                        ! Cumulative time at the end of each timestep. Set equal to problemtime when timestep reduction is used
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: qmpy                                    ! Rod average heat flux for power-time step (btu/hr-ft**2)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tw                                      ! Coolant inlet temperature, input for each timestep if nsp = 1
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: p1                                      ! Rod internal pressure for each time tep for FEA model
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: acmfg                                   ! Cumulative fission gas release
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: acmhe                                   ! Cumulative helium release
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: acmH2                                   ! Cumulative hydrogen release
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: acmH2O                                  ! Cumulative water vapor release
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: acmn2                                   ! Cumulative nitrogen release
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: amgpt                                   ! Cumulative fission gas produced
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: gasmo                                   ! Gram-moles of gas in rod
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: hmgpt                                   ! Cumulative helium production (gram-moles)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tafa                                    ! Radial and axial fuel averaged temperature (F)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: taga                                    ! Radial and axial gap averaged temperature (F)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tsfa                                    ! Axial averaged fuel surface temperature (F)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: taca                                    ! Radial and axial averaged cladding temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkPelAveTemp                            ! Peak power node average temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkPower                                 ! Peak power node power
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkAveCladTemp                           ! Peak power node average cladding temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkGap                                   ! Peak power node gap thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkFisGasRelFrac                         ! Peak power node fissio gas release fraction
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkPelSurfTemp                           ! Peak power node fuel surface temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkPelCentTemp                           ! Peak power node fuel centerline temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkH2up                                  ! Peak power node hydrogen pickup
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkIntefacePres                          ! Peak power node fuel/clad interfacial pressure
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkIDCladTemp                            ! Peak power node inner surface cladding temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkODCladTemp                            ! Peak power node outer surface cladding temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pit                                     ! Rod internal gas pressure
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkHoopStres                             ! Peak power node cladding hoop stress
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkAxlStres                              ! Peak power node cladding axial stress
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkBurnup                                ! Peak power node burnup
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkHoopStrain                            ! Peak power node cladding hoop strain
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkFuelPelOD                             ! Peak power node fuel pellet OD
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkGapCond                               ! Peak power node gap conductance
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pkZrO2                                  ! Peak power node oxide thickness
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: zcool                                   ! Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1.
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tcoolant                                ! Bulk coolant temperature at each axial node & timestep
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: pcoolant                                ! Bulk coolant pressure at each axial node & timestep
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: x                                       ! Elevation in each qf, x array defining a power shape
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: qf                                      ! Ratio of linear power at x(n) elevation to axially average value for each M-th power shape
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: xt                                      ! Elevation in each cladt, xt array defining a cladding temperature profile
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: cladt                                   ! Cladding surface temperature at xt(n) elevation for each M-th power shape
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: addgmles                                ! Additional gmles added to the total # of gmles released from the fuel
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: addswell                                ! Additional swelling added to the fuel pellet
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: HeatFlux                                ! HeatFlux, used in Subroutine burnup
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: dt                                      ! Time increment, used in Subroutine ans54
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: buavearray                              ! Used in Subroutine print2
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: voidvolarray                            ! Used in Subroutine print2
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: he_ifba                                 ! Used in Subroutine totgas
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tfuelr                                  ! Fuel radial temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tfuelr2                                 ! Fuel radial temperature
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: tfring                                  ! Fuel ring temperature (F)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: rrapow                                  ! Reversed array to match nodaliztion in fueltp
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: coolanttemp                             ! User-defined coolant temperature
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: coolantpressure                         ! User-defined coolant pressure
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: cladtarray                              ! User-defined cladding temperature
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: storedearray                            ! Fuel stored energy array (timestep, axial node)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: cltemparray                             ! Fuel centerline temperature array (timestep, axial node)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: buarray                                 ! Burnup array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: strainarray                             ! Strain array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: straindiffarray                         ! Change in strain from previous timestep
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dpwxarray                               ! Fuel swelling rate array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: creaparray                              ! Cladding creep rate array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dumarray3                               ! Dummy array used in inital
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: formf                                   ! 2-D array (axial,radial) of normalized form factors, correlated to the radial boundaries. (dimensionless).
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: ringvol                                 ! Fuel volume per increment + ring (in^3)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: coldringl                               ! Cold ring length (in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: porosold                                ! Porosity left at beginning of power step (m/m)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: porosnew                                ! Porosity left after power step (m/m)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rrev                                    ! Fuel mesh point radii array (in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: qaxnorm                                 ! Normalized axial power profile
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: crad                                    ! Cold fuel ring radius (m)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: hrad                                    ! Hot fuel ring radius (m)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rapow                                   ! Normalized radial power profile array
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: uo2exp                                  ! Thermal expansion of fuel ring (in/in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dpw                                     ! Fuel sweling (in/in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dpwpp                                   ! Fuel swelling at previous timestep (in/in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dpw2                                    ! Fuel creep (in/in) [Not used]
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: dpwpp2                                  ! Fuel creep at previous timestep (in/in) [Not used]
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: densf                                   ! Fuel densification (in/in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: densp                                   ! Fuel densification at previous timestep (in/in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: hotringl                                ! Hot pellet ring length (in)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: tmpfuel                                 ! Fuel temperature
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: gasavail1                               ! Gas available for transient FGR
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: gasavail2                               ! Gas available for transient FGR
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: prdct                                   ! HeatFlux, used in Subroutine ans54
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: brnup3                                  ! Burnup from comde.h file
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: ansd                                    ! Diffusion parameter, used in Subroutine ans54 (im,ngasr,na)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: creeptabtime                            ! Time input for ncreephist = 3 or 4
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: creeptabtemp                            ! Temperature input for ncreephist = 3 or 4
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: creeptabstress                          ! Stress input for ncreephist = 4
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conU235                                 ! Boundary-specific concentrations of U-235 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conU238                                 ! Boundary-specific concentrations of U-238 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conPu239                                ! Boundary-specific concentrations of Pu-239 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conPu240                                ! Boundary-specific concentrations of Pu-240 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conPu241                                ! Boundary-specific concentrations of Pu-241 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: conPu242                                ! Boundary-specific concentrations of Pu-242 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coU235av                                ! Volume average concentration of U-235 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coU238av                                ! Volume average concentration of U-238 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coPu239av                               ! Volume average concentration of Pu-239 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coPu240av                               ! Volume average concentration of Pu-240 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coPu241av                               ! Volume average concentration of Pu-241 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: coPu242av                               ! Volume average concentration of Pu-242 (atoms per cm^3)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriU235                                ! Enrichment of U-235 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriU238                                ! Enrichment of U-238 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriPu239                               ! Enrichment of Pu-239 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriPu240                               ! Enrichment of Pu-240 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriPu241                               ! Enrichment of Pu-241 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: enriPu242                               ! Enrichment of Pu-242 (atom% in HM)
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfU235                                  ! Power fraction coming from U-235
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfU238                                  ! Power fraction coming from U-238
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfPu239                                 ! Power fraction coming from Pu-239
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfPu240                                 ! Power fraction coming from Pu-240
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfPu241                                 ! Power fraction coming from Pu-241
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pfPu242                                 ! Power fraction coming from Pu-242
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: pf                                      !                        
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: decay                                   !                           
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: half                                    !                          
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: brnup1                                  !                            
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: ansr                                    !                          
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: gp                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: gpold                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rls                                     !                         
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: rlsold                                  !                            
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: brn                                     !                         
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: brnrefab                                !                              
    REAL        (r8k)    ,  DIMENSION(:),     POINTER :: EOSNodeburnuprefab                      !                                        
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: brnold                                  !                            
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: grnold                                  !                            
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: frdenold                                !                              
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: gbtot                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:),   POINTER :: tempkold                                !                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: gb                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: grs                                     !                         
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: gg                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: gbold                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: ggold                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: grsold                                  !                            
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g1                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g2                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g3                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g4                                      !                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g1old                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g2old                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g3old                                   !                           
    REAL        (r8k)    ,  DIMENSION(:,:,:), POINTER :: g4old                                   !                           
