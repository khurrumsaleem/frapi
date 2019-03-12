MODULE variables_frapcon
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains all of the common variables that are set by the input file and used within the code_frapcon.
    !>@author
    !> Ian Porter, NRC
    !
    ! Scalars
    !
    ! Indicator to terminate program
    INTEGER(ipk), TARGET :: iquit
    ! Number of axial nodes + 1
    INTEGER(ipk), TARGET :: ir1
    ! Time-step index
    INTEGER(ipk), TARGET :: it
    ! First time through burnup subroutine indicator
    INTEGER(ipk), TARGET :: itt
    ! Gas loop iteration index
    INTEGER(ipk), TARGET :: iter
    ! Minimum (lowest) axial node being modeled
    INTEGER(ipk), TARGET :: jmin
    ! Maximum (highest) axial node being modeled
    INTEGER(ipk), TARGET :: jmax
    ! Axial node index currently being modeled
    INTEGER(ipk), TARGET :: j
    ! Total number of axial power shapes
    INTEGER(ipk), TARGET :: jjc
    ! Peak node used for output printing
    INTEGER(ipk), TARGET :: jfix
    ! Gap iteration indicator
    INTEGER(ipk), TARGET :: k
    ! Axial power shape index
    INTEGER(ipk), TARGET :: m
    ! Indicator for first tiem step to declare variables in fracas
    INTEGER(ipk), TARGET :: nab
    ! Elastic/Plastic flag (0 = Elastic calculation performed, 1 = Plastic calculation performed)
    INTEGER(ipk), TARGET :: nplast
    ! Number of radial nodes (nr) minus 1
    INTEGER(ipk), TARGET :: nrm1
    ! Crud model (0 or 1 = constant layer, 2= time dependent)
    INTEGER(ipk), TARGET :: icor
    ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
    INTEGER(ipk), TARGET :: idxgas
    ! Option to specify variable axial node length (1 = ON, 0 = OFF(Default))
    INTEGER(ipk), TARGET :: ivardm
    ! Creep flag (0 = no creep, 1 = creep)
    INTEGER(ipk), TARGET :: ncreep
    ! New problem indicator
    INTEGER(ipk), TARGET :: newprb
    ! Flag to turn on permanent fuel crack healing (0 = Off,  1 = On)
    INTEGER(ipk), TARGET :: nheal
    ! Number of axial nodes minus 1
    INTEGER(ipk), TARGET :: nt
    ! Central Void Index. (0 = No central void, 1 = Central void exists)
    INTEGER(ipk), TARGET :: nvoid
    ! Output options
    ! Specifies whether to print plot information for use with Excel package FRAPlot or APT Plot_frapcon
    ! (0 = no, 1 = limited, 2 = detailed)
    INTEGER(ipk), TARGET :: nplot
    ! Specifies if coupled to T/H code  (0 = no, 1 = yes) [Not used]
    INTEGER(ipk), TARGET :: coupled
    ! Specifies to create the output file read by the NRC's Internal AIG for TRACE Runs (0 = no, 1 = yes)
    INTEGER(ipk), TARGET :: nfrttr
    ! Specifies to write a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER(ipk), TARGET :: nrestr
    ! Specifies the output file printing option (-1 = axial summary, 0 = peak-power node, 1 = all axial nodes)
    INTEGER(ipk), TARGET :: jdlpr
    ! Specifies the output file print control (0 = each timestep, 1 = input & summary only)
    INTEGER(ipk), TARGET :: nopt
    ! Specifies to start from a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
    INTEGER(ipk), TARGET :: nread
    ! Specifies to write a FRAPCON-to-FRAPTRAN restart tape  (0 = no, 1 = yes)
    INTEGER(ipk), TARGET :: ntape
    ! File unit #'s
    ! Output File
    INTEGER(ipk), TARGET :: ounit = 73 ! old: 6 YU JIANKAI
    ! Input File
    INTEGER(ipk), TARGET :: iunit = 55
    ! Scratch file
    INTEGER(ipk), TARGET :: scrunit = 5
    ! .frttr file
    INTEGER(ipk), TARGET :: funit = 50
    ! Plot File
    INTEGER(ipk), TARGET :: punit = 66
    ! FRAPCON-to-FRAPCON restart file WRITE
    INTEGER(ipk), TARGET :: ntaps = 12
    ! FRAPCON-to-FRAPCON restart file READ
    INTEGER(ipk), TARGET :: ntapi = 13
    ! FRAPCON-to-FRAPTRAN restart file
    INTEGER(ipk), TARGET :: ftunit = 22
    ! Dating file
    INTEGER(ipk), TARGET :: ddunit = 8
    ! Input/Output units (0 = SI, 1 = British)
    INTEGER(ipk), TARGET :: nunits
    ! Number of timesteps
    INTEGER(ipk), TARGET :: im
    ! Cladding mechanical model
    INTEGER(ipk), TARGET :: mechan
    ! Number of axial nodes
    INTEGER(ipk), TARGET :: na
    ! Number of radial gas release nodes (equal-volume) in the pellet for FGR calculations
    INTEGER(ipk), TARGET :: ngasr
    ! Number of radial nodes in the pellet for thermal calculations
    INTEGER(ipk), TARGET :: nr
    ! Number of radial elements in the cladding for the FEA model
    INTEGER(ipk), TARGET :: nce
    ! Number of axial nodes (na) * number of timesteps (im)
    INTEGER(ipk), TARGET :: naxim
    ! Number of graphing axial nodes [Not used]
    INTEGER(ipk), TARGET :: graphna
    ! Number of graphing radial nodes [Not used]
    INTEGER(ipk), TARGET :: graphnr
    ! Cladding type
    INTEGER(ipk), TARGET :: icm
    ! Fuel type
    INTEGER(ipk), TARGET :: imox
    ! Plant type
    INTEGER(ipk), TARGET :: iplant
    ! Zircaloy-2 vintage (Used when icm = 2) (0 = pre-1998, 1 = post-1998)
    INTEGER(ipk), TARGET :: zr2vintage
    ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
    INTEGER(ipk), TARGET :: ngasmod
    ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
    INTEGER(ipk), TARGET :: iq
    ! Timestep to begin calculation of fission gas release
    INTEGER(ipk), TARGET :: igas
    ! Internal pressure calculation for FEA model
    INTEGER(ipk), TARGET :: igascal
    ! Specify to use fixed cladding surface temperatures_frapcon
    INTEGER(ipk), TARGET :: ifixedtsurf
    ! Specify
    INTEGER(ipk), TARGET :: nsp
    ! Specify whether to use user_frapcon-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER(ipk), TARGET :: ifixedcoolt
    ! Specify whether to use user_frapcon-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)
    INTEGER(ipk), TARGET :: ifixedcoolp
    ! Specify time integration technique (0 = None, 1 = Linear Interpolation, 2 = Histogram)
    INTEGER(ipk), TARGET :: TimeIntegration
    ! Flag for type of Pu used in MOX (Used if icm = 1 or 2)
    INTEGER(ipk), TARGET :: moxtype
    ! Coolant type flag
    INTEGER(ipk), TARGET :: cooltype
    ! Assembly geometry flag
    INTEGER(ipk), TARGET :: geom
    ! Original number of timesteps when invoking timestep reduction
    INTEGER(ipk), TARGET :: im_old
    ! Minimum diametral gap thickness
    REAL(r8k), TARGET :: gapmin
    ! Hot state unrelocated radial gap (m)
    REAL(r8k), TARGET :: gaph
    ! Hot state relocated radial gap (m)
    REAL(r8k), TARGET :: gapt
    ! Initial moles of air in the rod
    REAL(r8k), TARGET :: airin
    ! Initial nitrogen concentration in the fuel (moles)
    REAL(r8k), TARGET :: angi
    ! Initial moles of N2 in the rod
    REAL(r8k), TARGET :: an2in
    ! Initial moles of Argon in the rod
    REAL(r8k), TARGET :: argin
    ! Average fast neutron flux
    REAL(r8k), TARGET :: avflux
    ! Rod average burnup to end of power-time stpe (MWd/mtU)
    REAL(r8k), TARGET :: bu
    ! Rod average burnup to beginning of power-time stpe (MWd/mtU)
    REAL(r8k), TARGET :: buold
    ! Cold free volume including dishes and chamfers
    REAL(r8k), TARGET :: cfv
    ! Cold free volume assuming cylindrical pellets
    REAL(r8k), TARGET :: cfva
    ! Convergence criteria on gap temperature drop
    REAL(r8k), TARGET :: convc
    ! Empty volume of the plenum
    REAL(r8k), TARGET :: cpv
    ! Average cladding temperature for axial node (j)
    REAL(r8k), TARGET :: ctemp
    ! Outer clad diameter at beginning of life
    REAL(r8k), TARGET :: dcobol
    ! Average burnup during power-time step for node j (MWd/mtU)
    REAL(r8k), TARGET :: delbp
    ! Rod average burnup during power-time step (MWd/mtU)
    REAL(r8k), TARGET :: delbu
    ! Time step size (hours)
    REAL(r8k), TARGET :: delh
    ! Temeprature drop across the crud (F)
    REAL(r8k), TARGET :: deltcr
    ! Temperature drop acros the film using Dittus-Boelter (F)
    REAL(r8k), TARGET :: deltdb
    ! Temperature drop acros the film total using forced convection (F)
    REAL(r8k), TARGET :: deltfc
    ! Temperature drop across the film using Jens-Lottes (F)
    REAL(r8k), TARGET :: deltjl
    ! Time step size (seconds)
    REAL(r8k), TARGET :: delhs
    ! Relative change in length with respect to the fuel length change (in)
    REAL(r8k), TARGET :: dlrel
    ! Change in length of the active cladding length (in)
    REAL(r8k), TARGET :: dlrod
    ! Diameter of the fuel pellet hot and swelled
    REAL(r8k), TARGET :: dphf
    ! Diameter of the fuel pellet hot and swelled plus relocation
    REAL(r8k), TARGET :: dphfrl
    ! Fuel stack axial extension (in)
    REAL(r8k), TARGET :: dhfll
    ! Initial moles of fission gas in the rod
    REAL(r8k), TARGET :: fgin
    ! Cold fuel open porosity fraction
    REAL(r8k), TARGET :: fpor1
    ! Total hot crack volume (in^3)
    REAL(r8k), TARGET :: hcrv
    ! Total hot clad volume (in^3)
    REAL(r8k), TARGET :: hcv
    ! Total hot dish + interface volume (in**3) or just interface vol. if rc > 0.0 or hdish = 0.0
    REAL(r8k), TARGET :: hdshv
    ! Initial moles of helium in the rod
    REAL(r8k), TARGET :: hein
    ! Initial moles of water vapor in the rod
    REAL(r8k), TARGET :: h2oin
    ! Initial moles of hydrogen in the rod
    REAL(r8k), TARGET :: h2in
    ! Maximum hot fuel stack length (in)
    REAL(r8k), TARGET :: hfll
    ! Dittus-Boelter film heat transfer coefficient (btu/hr-ft**2-F)
    REAL(r8k), TARGET :: hflmdb
    ! Forced convection film coefficient (btu/hr-ft**2-F)
    REAL(r8k), TARGET :: hflmp
    ! Increase of fuel stack height (%)
    REAL(r8k), TARGET :: hfper
    ! Total hot fuel volume (in^3)
    REAL(r8k), TARGET :: hfv
    ! Total gap conductance (btu/hr-ft^2-F)
    REAL(r8k), TARGET :: hgapt
    ! Total hot gap volume (in^3)
    REAL(r8k), TARGET :: hgv
    ! Conduction contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k), TARGET :: hgap
    ! Contact contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k), TARGET :: hsolid
    ! Radiation contribution to conductance (btu/hr-ft**2-F)
    REAL(r8k), TARGET :: hgapr
    ! Hot plenum length (in)
    REAL(r8k), TARGET :: hpl
    ! Total hot porosity volume (in**3)
    REAL(r8k), TARGET :: hporv
    ! Hot plenum volume (in**3)
    REAL(r8k), TARGET :: hpv
    ! Total annulus volume (in**3)
    REAL(r8k), TARGET :: hva
    ! Initial water content in the fuel (moles)
    REAL(r8k), TARGET :: h2omi
    ! Initial moles of krypton in the rod
    REAL(r8k), TARGET :: kryin
    ! Nusselt number of plentum gas mixture
    REAL(r8k), TARGET :: nu
    ! Fuel dish volume fraction
    REAL(r8k), TARGET :: pecdh
    ! Unknown. Used in burnup calculations
    REAL(r8k), TARGET :: rprm1
    ! Rod intenal gas pressure (psia)
    REAL(r8k), TARGET :: press
    ! Initial fuel porosity
    REAL(r8k), TARGET :: prty
    ! Cladding inner radius
    REAL(r8k), TARGET :: rci
    ! Cladding outer radius
    REAL(r8k), TARGET :: rco
    ! Average rate of change of temperature (K/s)
    REAL(r8k), TARGET :: rtemp
    ! Average linear heat generation rate (kW/ft)
    REAL(r8k), TARGET :: qav
    ! Peak linear heat generation rate (kW/ft)
    REAL(r8k), TARGET :: qpeak
    ! Peak ZrO2 weight gain (gram/m^2)
    REAL(r8k), TARGET :: pkZrO2WtGain
    ! Fuel roughness volume (%)
    REAL(r8k), TARGET :: rfnvff
    ! As-fabricated fuel density (grams/in^3)
    REAL(r8k), TARGET :: rhofuel
    ! As-fabricated pellet radius (in)
    REAL(r8k), TARGET :: rp
    ! True cladding strain rate (1/s)
    REAL(r8k), TARGET :: rstran
    ! Radius out to the region where crack begins (m)
    REAL(r8k), TARGET :: rtran
    ! Sum of axial crack volumes / crack temperature (in^3/F)
    REAL(r8k), TARGET :: sumck
    ! Sum of axial dish volumes / fuel avg. temp. (in**3/F)
    REAL(r8k), TARGET :: sumdh
    ! Total fuel surface displacement due to thermal expan (in)
    REAL(r8k), TARGET :: sumexp
    ! Sum of axial gap volumes / gap temperatures (in**3/F)
    REAL(r8k), TARGET :: sumgp
    ! Sum of axial porosity volumes / porosity temps (in**3/F)
    REAL(r8k), TARGET :: sumpor
    ! Sum of axial roughness volumes / roughness temperature
    REAL(r8k), TARGET :: sumrg
    ! Total problem time (hours)
    REAL(r8k), TARGET :: t
    ! Total free gas volume
    REAL(r8k), TARGET :: thvv
    ! Cladding inner surface temperature
    REAL(r8k), TARGET :: tci
    ! Cladding outer surface temperature
    REAL(r8k), TARGET :: tco
    ! Cumulative fission gas release fraction
    REAL(r8k), TARGET :: tfgfr
    ! Cumulative helium release fraction
    REAL(r8k), TARGET :: thefr
    ! Cumulative water vapor release fraction
    REAL(r8k), TARGET :: th2ofr
    ! Cumulative nitrogen release fraction
    REAL(r8k), TARGET :: tn2fr
    ! Pellet average temperature
    REAL(r8k), TARGET :: tpa
    ! Plenum temperature (F)
    REAL(r8k), TARGET :: tplen
    ! Temperature above which no cracking is assumed (K). Set to 80% of tsint
    REAL(r8k), TARGET :: transt
    ! Coolant saturation temperature (F)
    REAL(r8k), TARGET :: tsat
    ! Clad thermal expansion coefficient in radial direction (in/in-F)
    REAL(r8k), TARGET :: txa
    ! Clad thermal expansion coefficient in axial direction (in/in-F)
    REAL(r8k), TARGET :: txal
    ! Cladding thermal expansion coefficient (1/F)
    REAL(r8k), TARGET :: txc
    ! Gas viscosity used in plenum temperature calculation
    REAL(r8k), TARGET :: visc
    ! Volume of pellet annulus
    REAL(r8k), TARGET :: vplt
    ! Average bulk coolant temperature at node j (F)
    REAL(r8k), TARGET :: wt
    ! Initial moles of xenon in the rod
    REAL(r8k), TARGET :: xein
    ! ZrO2 Weight Gain
    REAL(r8k), TARGET :: zro2wg
    ! Average claddimg temperature
    REAL(r8k), TARGET :: cladavgtemp
    ! Axial average centerline temperature (F)
    REAL(r8k), TARGET :: tpca
    ! Axial averaged interface temperature (F)
    REAL(r8k), TARGET :: tntera
    ! Input effective fast fluence for strength coefficient (neutrons/(m**2))
    REAL(r8k), TARGET :: fnck
    ! Input effective fast fluence for strain hardening exponent at time step start (neutrons/(m**2))
    REAL(r8k), TARGET :: fncn
    ! Input effective cold work for strength coefficient (unitless ratio of areas)
    REAL(r8k), TARGET :: cwkf
    ! Input effective cold work for strain hardening exponent at time step start (unitless ratio of areas)
    REAL(r8k), TARGET :: cwnf
    ! Cladding cold work
    REAL(r8k), TARGET :: coldwk
    ! Fuel/Cladding friction coefficient for FEA model
    REAL(r8k), TARGET :: frcoef
    ! Additional fuel thermal expansion (multiplier)
    REAL(r8k), TARGET :: afal
    ! Additional fuel densification factor (multiplier)
    REAL(r8k), TARGET :: afdn
    ! Additional fractional gas release factor
    REAL(r8k), TARGET :: afgr
    ! Mole fraction of air
    REAL(r8k), TARGET :: amfair
    ! Mole fraction of fission gas
    REAL(r8k), TARGET :: amffg
    ! Clad texture factor
    REAL(r8k), TARGET :: catexf
    ! As-fabricated clad hydrogen content (wt.ppm)
    REAL(r8k), TARGET :: chorg
    ! Clad cold work (0.5= SRA, 0.0= RXA)
    REAL(r8k), TARGET :: cldwks
    ! Cold plenum length
    REAL(r8k), TARGET :: cpl
    ! Creep step duration (Defualt = 10hr)
    REAL(r8k), TARGET :: crephr
    ! Constant crud thickness
    REAL(r8k), TARGET :: crdt
    ! Crud accumulation rate
    REAL(r8k), TARGET :: crdtr
    ! Dish shoulder width
    REAL(r8k), TARGET :: dishsd
    ! Peak-to-average power ratio (fa = 1 if iq = 0)
    REAL(r8k), TARGET :: fa
    ! Fill gas pressure
    REAL(r8k), TARGET :: fgpav
    ! Fuel oxygen-to-metal ratio (Default = 2.0)
    REAL(r8k), TARGET :: fotmtl
    ! Chamfer height
    REAL(r8k), TARGET :: chmfrh
    ! Chamfer width
    REAL(r8k), TARGET :: chmfrw
    ! Dish height
    REAL(r8k), TARGET :: hdish
    ! Pellet height
    REAL(r8k), TARGET :: hplt
    ! Center to center rod distance
    REAL(r8k), TARGET :: pitch
    ! Weight ppm H2O in fuel (wt.ppm)
    REAL(r8k), TARGET :: ppmh2o
    ! Weight ppm N2 in fuel (wt. ppm)
    REAL(r8k), TARGET :: ppmn2
    ! Dish radius of curvature
    REAL(r8k), TARGET :: rdish
    ! Clad roughness
    REAL(r8k), TARGET :: roughc
    ! Fuel roughness
    REAL(r8k), TARGET :: roughf
    ! Fision Gas atoms per 100 fissions (Default = 31)
    REAL(r8k), TARGET :: sgapf
    ! As-fabricated cladding thickness
    REAL(r8k), TARGET :: tcc
    ! Reference temperature upon which the stored energy is based (F)
    REAL(r8k), TARGET :: tref
    ! Reference temeprature for specified gas fabrication pressure
    REAL(r8k), TARGET :: TGasFab
    ! Expected resintering density increase
    REAL(r8k), TARGET :: rsntr
    ! Pellet centering temperature
    REAL(r8k), TARGET :: tsint
    ! Input grain size (effective diameter) of the fuel (Default = 10.0 microns)
    REAL(r8k), TARGET :: grnsize
    ! Boron-10 enrichment (atom %) in ZrB2
    REAL(r8k), TARGET :: b10
    ! ZrB2 thickness
    REAL(r8k), TARGET :: zrb2thick
    ! ZrB2 density
    REAL(r8k), TARGET :: zrb2den
    ! Input total densification from previous time step (%) (For radial region l, axial node j)
    REAL(r8k), TARGET :: prvden
    ! Percent IFBA rods in core (%)
    REAL(r8k), TARGET :: ifba
    ! Fuel melting temperature
    REAL(r8k), TARGET :: ftmelt
    ! Fuel heat of fusion
    REAL(r8k), TARGET :: fhefus
    ! Cladding melting temperture
    REAL(r8k), TARGET :: ctmelt
    ! Cladding heat of fusion
    REAL(r8k), TARGET :: chefus
    ! Beginning of alpha-beta transition temperature
    REAL(r8k), TARGET :: ctranb
    ! End of alpha-beta transition temperature
    REAL(r8k), TARGET :: ctrane
    ! Isothermal alpha-beta transition temperature
    REAL(r8k), TARGET :: ctranz
    ! Variation in fuel melting temperature
    REAL(r8k), TARGET :: fdelta
    ! Rod average burnup for node j (MWs/kgU)
    REAL(r8k), TARGET :: bup
    ! Input average oxygen concentration excluding oxide layer - average oxygen concentration of
    ! as-received cladding  (kg oxygen/kg zircaloy) [Not used]
    REAL(r8k), TARGET :: deloxy
    ! Previous oxide thickness, m
    REAL(r8k), TARGET :: zro2i
    ! Current oxide thickness, m
    REAL(r8k), TARGET :: zro2o
    ! ZrO2 thermal conductivity
    REAL(r8k), TARGET :: zoxk
    ! Excess hydrogen in cladding
    REAL(r8k), TARGET :: excesh2
    ! Fraction of theoretical density (= 0.01 * den)
    REAL(r8k), TARGET :: frden
    ! Total length of active fuel
    REAL(r8k), TARGET :: totl
    ! Gas release fraction per axial node per cumulative time
    REAL(r8k), TARGET :: rdot
    ! As-Fabricated Free Volume (in3)
    REAL(r8k), TARGET :: cvv
    ! As-fabricated # of moles of gas
    REAL(r8k), TARGET :: gmlesAsFab
    ! Released # of moles of gas
    REAL(r8k), TARGET :: gmlesReleased
    ! Volume fraction of gases contained in the plenum
    REAL(r8k), TARGET :: vfrcpl
    ! Plenum temperature (K)
    REAL(r8k), TARGET :: tplens
    ! User supplied swelling limit (vol fraction) (Default = 0.05)
    REAL(r8k), TARGET :: slim
    ! User supplied gap heat transfer coefficient multiplier
    REAL(r8k), TARGET :: gaphtcmult
    ! Problem Time (s) to use for a restart calculation_frapcon
    REAL(r8k), TARGET :: RestartTime
    ! Uncertainty options
    ! Bias on fuel thermal conductivity model (# of standard deviations)
    REAL(r8k), TARGET :: sigftc
    ! Bias on fuel thermal expansion model (# of standard deviations)
    REAL(r8k), TARGET :: sigftex
    ! Bias on fission gas release model (# of standard deviations)
    REAL(r8k), TARGET :: sigfgr
    ! Bias on fuel swelling model (# of standard deviations)
    REAL(r8k), TARGET :: sigswell
    ! Bias on cladding creep model (# of standard deviations)
    REAL(r8k), TARGET :: sigcreep
    ! Bias on cladding axial growth model (# of standard deviations)
    REAL(r8k), TARGET :: siggro
    ! Bias on cladding corrosion model (# of standard deviations)
    REAL(r8k), TARGET :: sigcor
    ! Bias on cladding hydrogen pickup model (# of standard deviations)
    REAL(r8k), TARGET :: sigh2
    ! The new timestep value to use for the calculation_frapcon. (days) Used when TimeIntegration = 1 or 2
    REAL(r8k), TARGET :: newtimestep
    ! Flag to specify when to stop the oxidation calcualtion
    REAL(r8k), TARGET :: stopox
    ! Fuel open porosity fraction (%TD)
    REAL(r8k), TARGET :: deng
    ! As-fabricated apparent fuel density (%TD)
    REAL(r8k), TARGET :: den
    ! Molar fraction of helium
    REAL(r8k), TARGET :: amfhe
    ! Molar fraction of hydrogen
    REAL(r8k), TARGET :: amfh2
    ! Molar fraction of nitrogen
    REAL(r8k), TARGET :: amfn2
    ! Molar fraction of argon
    REAL(r8k), TARGET :: amfarg
    ! Molar fraction of krypton
    REAL(r8k), TARGET :: amfkry
    ! Molar fraction of xenon
    REAL(r8k), TARGET :: amfxe
    ! Molar fraction of water
    REAL(r8k), TARGET :: amfh2o
    ! Fuel pellet Pu-239 content
    REAL(r8k), TARGET :: enrpu39
    ! Fuel pellet Pu-240 content
    REAL(r8k), TARGET :: enrpu40
    ! Fuel pellet Pu-241 content
    REAL(r8k), TARGET :: enrpu41
    ! Fuel pellet Pu-242 content
    REAL(r8k), TARGET :: enrpu42
    ! Input fraction of molten fuel (0.0 - solid, 1.0 - molten)
    REAL(r8k), TARGET :: facmot
    ! User supplied time for printing debug information
    REAL(r8k), TARGET :: DebugTime
    ! Moderator heating fraction
    REAL(r8k), TARGET :: modheat
    ! User-supplied value for cladding elastic modulus (Pa)
    REAL(r8k), TARGET :: cladelmod
    ! User-supplied value for fuel relocation
    REAL(r8k), TARGET :: fuelreloc
    ! User-supplied value for gap recovery
    REAL(r8k), TARGET :: gaprecov
    ! Fuel relocation that is added to fuel surface displacement (*.5)
    REAL(r8k), TARGET :: relocm_true
    ! Fuel relocation after accounting for gap thermal conductivity
    REAL(r8k), TARGET :: relocm_mod
    ! Spent fuel modeling (came from SpentFuel module)
    LOGICAL, TARGET :: IsModelingSpentFuel = .FALSE.
    ! Flag to specify whether or not to calculate the oxidation reaction
    LOGICAL, TARGET :: calcoxide
    ! Indicates whether or not to write an updated FRAPTRAN restart file
    LOGICAL, TARGET :: updated_restart = .FALSE.
    ! Convergence flag on gas pressure iteration
    LOGICAL, TARGET :: gasflg
    ! Convergence flag on gap and pellet temperature distribution iteration
    LOGICAL, TARGET :: nconvg
    ! non-convergence index (30 iterations) on gap and pellet temperature distribution iteration
    LOGICAL, TARGET :: ncont
    ! The following is for storing the code ID Information
    ! The code version identifier (should be changed each time changes are made in the code)
    CHARACTER(LEN=*),PARAMETER :: codeid = 'FRAPCON-4.0 Patch 1'
    ! The code build date identifier (should be changed each time changes are made in the code)
    CHARACTER(LEN=*), PARAMETER :: buildid = 'Built May 19, 2016'
    ! Stores the title card information
    Character(LEN=80), TARGET :: title
    ! Identifier to print axial node loop or summary printout
    CHARACTER(LEN=10), TARGET :: PrintType
    ! Specifies fuel relocation model
    CHARACTER(LEN=12), TARGET :: RelocModel
    !
	! modified by YU JIANKAI
    ! flag to indicate the user defined total gap conductance(default : false)
    LOGICAL ,TARGET :: hgapt_flag = .false.
    ! 
    ! flag to indicate the iapws-if97 version of steam table  to be used (default : true)
    LOGICAL ,TARGET :: flag_iapws = .true.
    !
    LOGICAL, TARGET :: FirstCall = .TRUE.
    ! 
    ! Arrays
    !
    ! Fixed dimension arrays
    ! Mole fraction of gas constituents. Fixed to ngases
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gases
    ! Fraction short-lived radioactive gases released
    REAL(r8k), DIMENSION(11), TARGET :: releas = 0.0_r8k
    ! Gap temperature rise used for temperature convergence loop
    REAL(r8k), DIMENSION(31), TARGET :: dltgc = 0.0_r8k
    ! Axial-Dependent Arrays, dimensioned (na)
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: IgapGapIndex
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: IgapIndexOld
    !
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: IgapIndexPrevOld 
    ! Total inner cladding surface displacement, mils
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: totinner
    ! Total outer cladding surface displacement, mils
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: totcrl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FastFluxd
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FastFluenced
    ! Bulk Coolant Temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: BulkCoolantTemp
    ! Density of the coolant
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rhof
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: dpwxrate
    ! Axial node dimensions
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: axialnode
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PelletRad
    ! Average cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladAveTemp
    ! Average fuel pellet temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PelAveTemp
    ! Average gap temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: GapAveTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PelSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PelCentTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: sigeff     
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelSurfDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladInSurDisp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladInPermDef
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: sigy       
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: AxialNodLength
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladEffPlasStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: RinterfacPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelCladGap
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: GapPress   
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CoolantPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PlastStrnep1
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldCladStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldFuelStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldGapPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldCoolPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldCladAvTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CreepStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CreepStrain1
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladDiamHot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevCladEffPlasStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevFuelStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: HotNodLength
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PermFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: BOSNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: EOSNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: StepNodeburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: TotalHgap  
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: SolidHgap  
    
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: GasHgap    
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: RadHgap    
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FastFlux   
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FastFluence
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelPorosity
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FilmCoefficient
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: EstGapDeltaT
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: BOSZrO2Thk 
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: EOSZrO2Thk 
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ZrO2ThkNoAd
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelCondFactor
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: Relocation 
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: RinternalVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladVolume 
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CrackVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: RinterfacVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelVolume 
    ! Fuel average fraction of theoretical density (current, not as-fab)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fden       
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: GapVolume  
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PorosityVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: SurfTempOxide
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: AnnulusVolume
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gapplot    
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevOldCoolPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevOldGapPress
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevOldCladAvTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevOldFuelDispl
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevCreepStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladIrradGrowStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: UniformAxNodStrn
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladH2Concen
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ExcessH2Concen
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: PrevCladStrain
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: StartofStepH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: StartofStepPickupH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: EndofStepPickupH2Con
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FuelTempRestruRad
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: OldHealedCrackRadius
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: StoredEnergy
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: HealedCrackRadius
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: HotThermalGap
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: WorkArray1
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: WorkArray2
    ! Axhef array
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: WorkArray3
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladInSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: CladOutSurfTemp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: Power      
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gpthe      
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gpth       
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gpthpg     
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: qc         
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: totdef
    ! Input fuel burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: buin
    ! Inner cladding diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: dci
    ! Outer cladding diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: dco
    ! As-fabricated cladding thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: thkcld
    ! Diametral gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: cdg
    ! As-fabricated gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: thkgap
    ! Hydraulic diameter
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: de
    ! Axial node length (ft)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: deltaz
    ! PuO2 weight percent if MOX fuel (wt%)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: comp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ctmax
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: dp
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: flux
    ! End-node to plenum heat transfer fraction (Default = 0.03). 1 value per power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: qend
    ! Axial crud thickness multiplier
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: crudmult
    ! Crud thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: crdtt
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rdotwrt
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: FDItave
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: creapratearray
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: GapCond
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fuelexptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fuelswltot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fuelcreeptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fueldentot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fuelburntot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: cladcrptot
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gapHTC
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: oxidelayer
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gapmech
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gapthrm
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: fuelrelmod
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ifbarel
    ! Void volume per unit temperature at axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: NodalMoles
    ! Moles at each axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: NodalGMLES
    ! Relocation strain (unitless) (1/2 is added to the radial strain as permanent outward strain)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rlcstrn
    ! Relocation strain at previous timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rlcstrnold
    ! Radius of the fuel pellet central annulus (in)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rc
    ! Gadolinia content at each axial node
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gadoln
    ! Net permanent fuel displacement due to swelling and densification
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: colddef
    ! Net cladding displacement due to creep/plastic deformation
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: colddec
    ! Rod power at node j
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: qnode
    ! Used in fgasre
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: heprod
    ! Used in fgasre
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: he
    ! Used in fgasre
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: heold
    ! Used in fgasre
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: boron10
    ! Fuel U-235 enrichment (at%)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: enrch
    ! Cladding strain
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: epsav
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: wimsburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: oldwimsburnup
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: stold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: stnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: deltimeold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: deltimenew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: sagold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: sagnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: delsagold
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: delsagnew
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: delst
    !
    ! ***These arrays are dimensioned (na, 2)***
    !
    ! Nitrogen release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: angr
    ! Water release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: ah2ogr
    ! Fission gas production for each node (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: fgmgp
    ! Cumulative fission gas release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: fmgr
    ! Cumulative helium gas production per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: hemgp
    ! Cumulative helium release per node and power step
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: hmgr
    ! Cumulative nitrogen concentration in fuel per power step (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: ang
    ! Cumulative water concentration in fuel per power step (moles)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: ah2og
    !
    ! ***These arrays are dimensioned (na, 3)***
    !
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: epp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: eppp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: eppsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: eps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: feps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: reps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: repsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: repsp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: rfeps
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: rfepp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: rfpsv
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: sig
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: epp1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: sig1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: eps1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: ThermalStrain
    ! ANS 5.4 2011 axial release fractions array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: RB_axial
    ! ANS 5.4 2011 total rod release fractions array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: RB_rod
    !
    ! ***Time-Dependent***
    !
    ! # of qf, x pairs for each axial power shape
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: jn
    ! # of cladt, xt pairs for each axial temperature distribution
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: jnsurftemp
    ! Maximum power node for each power shape
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: jpeak
    ! Sequential # of the power shape to be used for each timestep
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: jst
    ! Sequential # of the cladding temperature profile to be used for each timestep
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: jstsurftemp
    ! Keeps track of the cycle steps, used for increased # of timestep runs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, TARGET :: CycleSteps
    ! Average of the q'' for each axial power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: avgqi
    ! Coolant mass flux around fuel rod, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: go
    ! Coolant System Pressure, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: p2
    ! Cumulative time at the end of each timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ProblemTime
    ! Cumulative time at the end of each timestep. Set equal to problemtime when timestep reduction is used
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: ProblemTime_Prev
    ! LHGR at each timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: qmpy
    ! Coolant inlet temperature, input for each timestep if nsp = 1
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tw
    ! Rod internal pressure for each time tep for FEA model
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: p1
    ! Cumulative fission gas release
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: acmfg
    ! Cumulative helium release
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: acmhe
    ! Cumulative hydrogen release
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: acmH2
    ! Cumulative water vapor release
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: acmH2O
    ! Cumulative nitrogen release
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: acmn2
    ! Cumulative fission gas produced
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: amgpt
    ! Gram-moles of gas in rod
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: gasmo
    ! Cumulative helium production (gram-moles)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: hmgpt
    ! Radial and axial fuel averaged temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tafa
    ! Radial and axial gap averaged temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: taga
    ! Axial averaged fuel surface temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tsfa
    ! Radial and axial averaged cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: taca
    ! Peak power node average temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkPelAveTemp
    ! Peak power node power
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkPower
    ! Peak power node average cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkAveCladTemp
    ! Peak power node gap thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkGap
    ! Peak power node fissio gas release fraction
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkFisGasRelFrac
    ! Peak power node fuel surface temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkPelSurfTemp
    ! Peak power node fuel centerline temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkPelCentTemp
    ! Peak power node hydrogen pickup
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkH2up
    ! Peak power node fuel/clad interfacial pressure
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkIntefacePres
    ! Peak power node inner surface cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkIDCladTemp
    ! Peak power node outer surface cladding temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkODCladTemp
    ! Rod internal gas pressure
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pit
    ! Peak power node cladding hoop stress
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkHoopStres
    ! Peak power node cladding axial stress
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkAxlStres
    ! Peak power node burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkBurnup
    ! Peak power node cladding hoop strain
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkHoopStrain
    ! Peak power node fuel pellet OD
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkFuelPelOD
    ! Peak power node gap conductance
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkGapCond
    ! Peak power node oxide thickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pkZrO2
    ! Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1.
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: zcool
    ! Bulk coolant temperature at each axial node & timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tcoolant
    ! Bulk coolant pressure at each axial node & timestep
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: pcoolant
    ! Elevation in each qf, x array defining a power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: x
    ! Ratio of linear power at x(n) elevation to axially average value for each M-th power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: qf
    ! Elevation in each cladt, xt array defining a cladding temperature profile
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: xt
    ! Cladding surface temperature at xt(n) elevation for each M-th power shape
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: cladt
    ! Additional gmles added to the total # of gmles released from the fuel
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: addgmles
    ! Additional swelling added to the fuel pellet
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: addswell
    ! HeatFlux, used in Subroutine burnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: HeatFlux
    ! Time increment, used in Subroutine ans54
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: dt
    ! Used in Subroutine print2
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: buavearray
    ! Used in Subroutine print2
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: voidvolarray
    ! Used in Subroutine totgas
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: he_ifba
    !
    ! *** Radial-Dependent Arrays, dimensioned (nr)***
    !
    ! Fuel radial temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tfuelr
    ! Fuel radial temperature
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tfuelr2
    ! Fuel ring temperature (F)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: tfring
    ! Reversed array to match nodaliztion in fueltp
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: rrapow
    !
    ! 2-D Arrays
    !
    ! User-defined coolant temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: coolanttemp
    ! User-defined coolant pressure
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: coolantpressure
    ! User-defined cladding temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: cladtarray
    ! Fuel stored energy array (timestep, axial node)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: storedearray
    ! Fuel centerline temperature array (timestep, axial node)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: cltemparray
    ! Burnup array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: buarray
    ! Strain array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: strainarray
    ! Change in strain from previous timestep
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: straindiffarray
    ! Fuel swelling rate array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dpwxarray
    ! Cladding creep rate array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: creaparray
    ! Dummy array used in inital
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dumarray3
    ! 2-D array (axial,radial) of normalized form factors, correlated to the radial boundaries. (dimensionless).
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: formf                  
    ! Fuel volume per increment + ring (in^3)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: ringvol
    ! Cold ring length (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: coldringl
    ! Porosity left at beginning of power step (m/m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: porosold
    ! Porosity left after power step (m/m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: porosnew
    ! Fuel mesh point radii array (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: rrev
    ! Normalized axial power profile
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: qaxnorm
    ! Cold fuel ring radius (m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: crad
    ! Hot fuel ring radius (m)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: hrad
    ! Normalized radial power profile array
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: rapow
    ! Thermal expansion of fuel ring (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: uo2exp
    ! Fuel sweling (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dpw
    ! Fuel swelling at previous timestep (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dpwpp
    ! Fuel creep (in/in) [Not used]
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dpw2
    ! Fuel creep at previous timestep (in/in) [Not used]
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: dpwpp2
    ! Fuel densification (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: densf
    ! Fuel densification at previous timestep (in/in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: densp
    ! Hot pellet ring length (in)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: hotringl
    ! Fuel temperature
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: tmpfuel
    ! Gas available for transient FGR
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: gasavail1
    ! Gas available for transient FGR
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: gasavail2
    !
    ! 3-D Arrays
    !
    ! HeatFlux, used in Subroutine ans54
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: prdct
    ! Burnup from comde.h file
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: brnup3
    ! Diffusion parameter, used in Subroutine ans54 (im,ngasr,na)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: ansd
    !
    ! Spring Data
    TYPE SpringProp
        REAL(r8k) :: vs = 0.0_r8k           ! Number of spring turns
        REAL(r8k) :: dspg = 0.0_r8k         ! Spring diameter
        REAL(r8k) :: dspgw = 0.0_r8k        ! Spring wire diameter
        REAL(r8k) :: Vcold = 0.0_r8k        ! Hot volume
        REAL(r8k) :: Vhot = 0.0_r8k         ! Cold Volume
        REAL(r8k) :: Vf = 0.0_r8k           ! (Plenum) Volume fraction occupied by spring
    END TYPE SpringProp
    !
    TYPE (SpringProp), SAVE, TARGET :: Spring
    !
    CONTAINS
    !
    SUBROUTINE Set_Defaults
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine sets default values for scalars
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/19/2015
    nvoid = 0_ipk
    rdot = 0.0_r8k
    fhefus = 1.0_r8k
    gaph = 1.0e-2_r8k
    gapt = 1.0e-2_r8k
    fdelta = 0.0_r8k
    ctmelt = 1.0_r8k
    ctranb = 1.0_r8k
    ctrane = 1.0_r8k
    ctranz = 1.0_r8k
    chefus = 1.0_r8k
    deloxy = 0.0_r8k
    fnck = 0.0_r8k
    fncn = 0.0_r8k
    cwnf = 0.0_r8k
    !
    END SUBROUTINE Set_Defaults
    !
    !
    !
    SUBROUTINE Allocate_Time_Arrays
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the time-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (qend(1:im+1))
    ALLOCATE (avgqi(1:im+1))
    ALLOCATE (go(1:im+1))
    ALLOCATE (p2(1:im+1))
    ALLOCATE (ProblemTime(0:im+1))
    ALLOCATE (ProblemTime_Prev(0:im+1))
    ALLOCATE (qmpy(1:im+1))
    ALLOCATE (tw(1:im+1))
    ALLOCATE (p1(1:im+1))
    ALLOCATE (jn(1:im+1))
    ALLOCATE (jnsurftemp(1:im+1))
    ALLOCATE (jpeak(1:im+1))
    ALLOCATE (jst(1:im+1))
    ALLOCATE (jstsurftemp(1:im+1))
    ALLOCATE (acmfg(1:im+1))
    ALLOCATE (acmhe(1:im+1))
    ALLOCATE (acmH2(1:im+1))
    ALLOCATE (acmH2O(1:im+1))
    ALLOCATE (acmn2(1:im+1))
    ALLOCATE (amgpt(1:im+1))
    ALLOCATE (gasmo(0:im+1))
    ALLOCATE (hmgpt(1:im+1))
    ALLOCATE (tafa(1:im+1))
    ALLOCATE (taga(1:im+1))
    ALLOCATE (tsfa(1:im+1))
    ALLOCATE (taca(0:im+1))
    ALLOCATE (pkODCladTemp(0:im+1))
    ALLOCATE (pkPelAveTemp(0:im+1))
    ALLOCATE (pkPower(0:im+1))
    ALLOCATE (pkAveCladTemp(0:im+1))
    ALLOCATE (pkIDCladTemp(0:im+1))
    ALLOCATE (pkGap(0:im+1))
    ALLOCATE (pkFisGasRelFrac(0:im+1))
    ALLOCATE (pkPelSurfTemp(0:im+1))
    ALLOCATE (pkH2up(0:im+1))
    ALLOCATE (pkPelCentTemp(0:im+1))
    ALLOCATE (pkIntefacePres(0:im+1))    
    ALLOCATE (pit(0:im+1))
    ALLOCATE (pkHoopStres(0:im+1))
    ALLOCATE (pkAxlStres(0:im+1))
    ALLOCATE (pkBurnup(0:im+1))
    ALLOCATE (pkHoopStrain(0:im+1))
    ALLOCATE (pkFuelPelOD(0:im+1))
    ALLOCATE (pkGapCond(0:im+1))
    ALLOCATE (pkZrO2(0:im+1))
    ALLOCATE (HeatFlux(1:im+1))
    ALLOCATE (dt(1:im+2))
    ALLOCATE (buavearray(1:im+1))
    ALLOCATE (voidvolarray(1:im+1))
    ALLOCATE (he_ifba(1:im+1))
    ALLOCATE (addgmles(1:im+1))
    ALLOCATE (addswell(1:im+1))
    ALLOCATE (CycleSteps(1:im+1))
    !
    ! Assign a value to each array
    !
    qend = 0.0_r8k
    avgqi = 0.0_r8k
    go = 0.0_r8k
    p2 = 0.0_r8k
    ProblemTime = 0.0_r8k
    ProblemTime_Prev = 0.0_r8k
    qmpy = 0.0_r8k
    tw = 0.0_r8k
    p1 = 0.0_r8k
    jn = 0_ipk
    jnsurftemp = 0_ipk
    jpeak = 0_ipk
    jst = 0_ipk
    acmfg = 0.0_r8k
    acmhe = 0.0_r8k
    acmH2 = 0.0_r8k
    acmH2O = 0.0_r8k
    acmn2 = 0.0_r8k
    amgpt = 0.0_r8k
    gasmo = 0.0_r8k
    hmgpt = 0.0_r8k
    tafa = 0.0_r8k
    taga = 0.0_r8k
    tsfa = 0.0_r8k
    taca = 0.0_r8k
    pkODCladTemp = 0.0_r8k
    pkPelAveTemp = 0.0_r8k
    pkPower = 0.0_r8k
    pkAveCladTemp = 0.0_r8k
    pkIDCladTemp = 0.0_r8k
    pkGap = 0.0_r8k
    pkFisGasRelFrac = 0.0_r8k
    pkPelSurfTemp = 0.0_r8k
    pkH2up = 0.0_r8k
    pkPelCentTemp = 0.0_r8k
    pkIntefacePres = 0.0_r8k
    pit = 0.0_r8k
    pkHoopStres = 0.0_r8k
    pkAxlStres = 0.0_r8k
    pkBurnup = 0.0_r8k
    pkHoopStrain = 0.0_r8k
    pkFuelPelOD = 0.0_r8k
    pkGapCond = 0.0_r8k
    pkZrO2 = 0.0_r8k
    HeatFlux = 0.0_r8k
    dt = 0.0_r8k
    jstsurftemp = 0
    buavearray = 0.0_r8k
    voidvolarray = 0.0_r8k
    he_ifba = 0.0_r8k
    addgmles = 0.0_r8k
    addswell = 0.0_r8k
    CycleSteps = 0_ipk
    !
    END SUBROUTINE Allocate_Time_Arrays
    !
    !
    SUBROUTINE Allocate_Axial_Arrays
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the axial-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    !naxim = na * im (already did this)
    !
    ALLOCATE (BulkCoolantTemp(1:na+1))
    ALLOCATE (rhof(1:na))
    ALLOCATE (dpwxrate(1:na))
    ALLOCATE (axialnode(1:na))
    ALLOCATE (PelletRad(1:na))
    ALLOCATE (FastFluxd(1:na))
    ALLOCATE (FastFluenced(1:na))
    ALLOCATE (totinner(1:na))
    ALLOCATE (totcrl(1:na))
    ALLOCATE (CladAveTemp(1:na))
    ALLOCATE (PelAveTemp(1:na))
    ALLOCATE (GapAveTemp(1:na))
    ALLOCATE (PelSurfTemp(1:na))
    ALLOCATE (PelCentTemp(1:na))
    ALLOCATE (sigeff(1:na))
    ALLOCATE (FuelSurfDispl(1:na))
    ALLOCATE (CladInSurDisp(1:na))
    ALLOCATE (CladInPermDef(1:na))
    ALLOCATE (sigy(1:na))
    ALLOCATE (AxialNodLength(1:na))
    ALLOCATE (CladEffPlasStrain(1:na))
    ALLOCATE (RinterfacPress(1:na))
    ALLOCATE (FuelCladGap(1:na))
    ALLOCATE (GapPress(1:na))
    ALLOCATE (CoolantPress(1:na))
    ALLOCATE (PlastStrnep1(1:na))
    ALLOCATE (OldCladStrn(1:na))
    ALLOCATE (OldFuelStrn(1:na))
    ALLOCATE (OldGapPress(1:na))
    ALLOCATE (OldCoolPress(1:na))
    ALLOCATE (OldFuelDispl(1:na))
    ALLOCATE (OldCladAvTemp(1:na))
    ALLOCATE (CreepStrain(1:na))
    ALLOCATE (CreepStrain1(1:na))
    ALLOCATE (CladDiamHot(1:na))
    ALLOCATE (PrevCladEffPlasStrn(1:na))
    ALLOCATE (PrevFuelStrain(1:na))
    ALLOCATE (HotNodLength(1:na))
    ALLOCATE (PermFuelDispl(1:na))
    ALLOCATE (BOSNodeburnup(1:na))
    ALLOCATE (EOSNodeburnup(1:na))
    ALLOCATE (StepNodeburnup(1:na))
    ALLOCATE (TotalHgap(1:na))
    ALLOCATE (SolidHgap(1:na))
    ALLOCATE (GasHgap(1:na))
    ALLOCATE (RadHgap(1:na))
    ALLOCATE (FastFlux(1:na))
    ALLOCATE (FastFluence(1:na))
    ALLOCATE (FuelPorosity(1:na))
    ALLOCATE (FilmCoefficient(1:na))
    ALLOCATE (EstGapDeltaT(1:na))
    ALLOCATE (BOSZrO2Thk(1:na))
    ALLOCATE (EOSZrO2Thk(1:na))
    ALLOCATE (ZrO2ThkNoAd(1:na))
    ALLOCATE (FuelCondFactor(1:na))
    ALLOCATE (Relocation(1:na))
    ALLOCATE (RinternalVolume(1:na))
    ALLOCATE (CladVolume(1:na))
    ALLOCATE (CrackVolume(1:na))
    ALLOCATE (RinterfacVolume(1:na))
    ALLOCATE (FuelVolume(1:na))
    ALLOCATE (fden(1:na))
    ALLOCATE (GapVolume(1:na))
    ALLOCATE (PorosityVolume(1:na))
    ALLOCATE (SurfTempOxide(1:na))
    ALLOCATE (AnnulusVolume(1:na))
    ALLOCATE (gapplot(1:na))
    ALLOCATE (PrevOldCoolPress(1:na))
    ALLOCATE (PrevOldGapPress(1:na))
    ALLOCATE (PrevOldCladAvTemp(1:na))
    ALLOCATE (PrevOldFuelDispl(1:na))
    ALLOCATE (PrevCreepStrain(1:na))
    ALLOCATE (CladIrradGrowStrn(1:na))
    ALLOCATE (UniformAxNodStrn(1:na))
    ALLOCATE (CladH2Concen(1:na))
    ALLOCATE (ExcessH2Concen(1:na))
    ALLOCATE (PrevCladStrain(1:na))
    ALLOCATE (StartofStepH2Con(1:na))
    ALLOCATE (StartofStepPickupH2Con(1:na))
    ALLOCATE (EndofStepPickupH2Con(1:na))
    ALLOCATE (FuelTempRestruRad(1:na))
    ALLOCATE (OldHealedCrackRadius(1:na))
    ALLOCATE (StoredEnergy(1:na))
    ALLOCATE (HealedCrackRadius(1:na))
    ALLOCATE (HotThermalGap(1:na))
    ALLOCATE (WorkArray1(1:na))
    ALLOCATE (WorkArray2(1:na))
    ALLOCATE (WorkArray3(1:na))
    ALLOCATE (CladInSurfTemp(1:na))
    ALLOCATE (CladOutSurfTemp(1:na))
    ALLOCATE (Power(1:na))
    ! Note: 30 is the number of iterations on the gap convergence
    ALLOCATE (gpthe(1:30))
    ALLOCATE (gpth(1:30))
    ALLOCATE (gpthpg(1:na))
    ALLOCATE (qc(1:na))
    ALLOCATE (totdef(1:na))
    ALLOCATE (IgapGapIndex(1:na))
    ALLOCATE (IgapIndexOld(1:na))
    ALLOCATE (IgapIndexPrevOld(1:na))
    ALLOCATE (buin(1:na))
    ALLOCATE (dci(1:na))
    ALLOCATE (dco(1:na))
    ALLOCATE (cdg(1:na))
    ALLOCATE (thkcld(1:na))
    ALLOCATE (thkgap(1:na))
    ALLOCATE (de(1:na))
    ALLOCATE (deltaz(1:na))
    ALLOCATE (comp(1:na))
    ALLOCATE (ctmax(1:na))
    ALLOCATE (dp(1:na))
    ALLOCATE (flux(1:na))
    ALLOCATE (crudmult(1:na))
    ALLOCATE (crdtt(1:na))
    ALLOCATE (rdotwrt(1:na))
    ALLOCATE (FDItave(1:na))
    ALLOCATE (creapratearray(1:na))
    ALLOCATE (GapCond(1:na))
    ALLOCATE (fuelexptot(1:na))
    ALLOCATE (fuelswltot(1:na))
    ALLOCATE (fuelcreeptot(1:na))
    ALLOCATE (fueldentot(1:na))
    ALLOCATE (fuelburntot(1:na))
    ALLOCATE (cladcrptot(1:na))
    ALLOCATE (gapHTC(1:na))
    ALLOCATE (oxidelayer(1:na))
    ALLOCATE (gapmech(1:na))
    ALLOCATE (gapthrm(1:na))
    ALLOCATE (fuelrelmod(1:na))
    ALLOCATE (ifbarel(1:na))
    ALLOCATE (zcool(1:na))
    ALLOCATE (tcoolant(1:naxim))
    ALLOCATE (pcoolant(1:naxim))
    ALLOCATE (x(1:naxim))
    ALLOCATE (qf(1:naxim))
    ALLOCATE (xt(1:naxim))
    ALLOCATE (cladt(1:naxim))
    ALLOCATE (NodalMoles(1:na))
    ALLOCATE (NodalGMLES(1:na))
    ALLOCATE (rlcstrn(1:na))
    ALLOCATE (rlcstrnold(1:na))
    ALLOCATE (rc(1:na))
    ALLOCATE (gadoln(1:na))
    ALLOCATE (stold(1:na))
    ALLOCATE (stnew(1:na))
    ALLOCATE (deltimeold(1:na))
    ALLOCATE (deltimenew(1:na))
    ALLOCATE (sagold(1:na))
    ALLOCATE (sagnew(1:na))
    ALLOCATE (delsagold(1:na))
    ALLOCATE (delsagnew(1:na))
    ALLOCATE (delst(1:na))
    ALLOCATE (colddef(1:na))
    ALLOCATE (colddec(1:na))
    ALLOCATE (qnode(1:na))
    ALLOCATE (heprod(1:na))
    ALLOCATE (he(1:na))
    ALLOCATE (heold(1:na))
    ALLOCATE (boron10(1:na))
    ALLOCATE (enrch(1:na))
    ALLOCATE (epsav(1:na))
    ALLOCATE (wimsburnup(1:na))
    ALLOCATE (oldwimsburnup(1:na))
    !
    ! Assign a value to each array
    !
    totinner = 0.0_r8k
    totcrl = 0.0_r8k
    BulkCoolantTemp = 0.0_r8k
    PelletRad = 0.0_r8k
    dpwxrate = 0.0_r8k
    rhof = 0.0_r8k
    axialnode = 0.0_r8k
    FastFluxd = 0.0_r8k
    FastFluenced = 0.0_r8k
    CladAveTemp = 0.0_r8k!500.0_r8k
    PelAveTemp = 0.0_r8k
    GapAveTemp = 0.0_r8k
    PelSurfTemp = 0.0_r8k
    PelCentTemp = 0.0_r8k
    sigeff = 0.0_r8k
    FuelSurfDispl = 0.0_r8k
    CladInSurDisp = 0.0_r8k
    CladInPermDef = 0.0_r8k
    sigy = 0.0_r8k
    AxialNodLength = 0.0_r8k
    CladEffPlasStrain = 0.0_r8k
    RinterfacPress = 0.0_r8k
    FuelCladGap = 0.0_r8k
    GapPress = 0.0_r8k
    CoolantPress = 0.0_r8k
    PlastStrnep1 = 0.0_r8k
    OldCladStrn = 0.0_r8k
    OldFuelStrn = 0.0_r8k
    OldGapPress = 0.0_r8k
    OldCoolPress = 0.0_r8k
    OldFuelDispl = 0.0_r8k
    OldCladAvTemp = 0.0_r8k
    CreepStrain = 0.0_r8k
    CreepStrain1 = 0.0_r8k
    CladDiamHot = 0.0_r8k
    PrevCladEffPlasStrn = 0.0_r8k
    PrevFuelStrain = 0.0_r8k
    HotNodLength = 0.0_r8k
    PermFuelDispl = 0.0_r8k
    BOSNodeburnup = 0.0_r8k
    EOSNodeburnup = 0.0_r8k
    StepNodeburnup = 0.0_r8k
    TotalHgap = 0.0_r8k
    SolidHgap = 0.0_r8k
    GasHgap = 0.0_r8k
    RadHgap = 0.0_r8k
    FastFlux = 0.0_r8k
    FastFluence = 0.0_r8k
    FuelPorosity = 0.0_r8k
    FilmCoefficient = 0.0_r8k
    EstGapDeltaT = 0.0_r8k
    BOSZrO2Thk = 0.0_r8k
    EOSZrO2Thk = 0.0_r8k
    ZrO2ThkNoAd = 0.0_r8k
    FuelCondFactor = 0.0_r8k
    Relocation = 0.0_r8k
    RinternalVolume = 0.0_r8k
    CladVolume = 0.0_r8k
    CrackVolume = 0.0_r8k
    RinterfacVolume = 0.0_r8k
    FuelVolume = 0.0_r8k
    fden = 0.0_r8k
    GapVolume = 0.0_r8k
    PorosityVolume = 0.0_r8k
    SurfTempOxide = 0.0_r8k
    AnnulusVolume = 0.0_r8k
    gapplot = 0.0_r8k
    PrevOldCoolPress = 0.0_r8k
    PrevOldGapPress = 0.0_r8k
    PrevOldCladAvTemp = 0.0_r8k
    PrevOldFuelDispl = 0.0_r8k
    PrevCreepStrain = 0.0_r8k
    CladIrradGrowStrn = 0.0_r8k
    UniformAxNodStrn = 0.0_r8k
    CladH2Concen = 10.0_r8k
    ExcessH2Concen = 0.0_r8k
    PrevCladStrain = 0.0_r8k
    StartofStepH2Con = 10.0_r8k
    StartofStepPickupH2Con = 0.0_r8k
    EndofStepPickupH2Con = 0.0_r8k
    FuelTempRestruRad = 0.0_r8k
    OldHealedCrackRadius = 0.0_r8k
    StoredEnergy = 0.0_r8k
    HealedCrackRadius = 0.0_r8k
    HotThermalGap = 0.0_r8k
    WorkArray1 = 0.0_r8k
    WorkArray2 = 0.0_r8k
    WorkArray3 = 0.0_r8k
    CladInSurfTemp = 0.0_r8k
    CladOutSurfTemp = 0.0_r8k
    Power = 0.0_r8k
    gpthe = 0.0_r8k
    gpth = 0.0_r8k
    gpthpg = 0.0_r8k
    qc = 0.0_r8k
    totdef = 0.0_r8k
    IgapGapIndex = 0.0_r8k
    IgapIndexOld = 0.0_r8k
    IgapIndexPrevOld = 0.0_r8k
    buin = 0.0_r8k
    dci = 0.0_r8k
    dco = -1.0_r8k
    cdg = 0.0_r8k
    thkcld = -1.0_r8k
    thkgap = -1.0_r8k
    de = 0.0_r8k
    deltaz = 0.0_r8k
    comp = -1.0_r8k
    comp(1) = 0.0_r8k
    ctmax = 0.0_r8k
    dp = 0.0_r8k
    flux = 0.0_r8k
    crudmult = 0.0_r8k
    crdtt = 0.0_r8k
    rdotwrt = 0.0_r8k
    FDItave = 0.0_r8k
    creapratearray = 0.0_r8k
    GapCond = 0.0_r8k
    fuelexptot = 0.0_r8k
    fuelswltot = 0.0_r8k
    fuelcreeptot = 0.0_r8k
    fueldentot = 0.0_r8k
    fuelburntot = 0.0_r8k
    cladcrptot = 0.0_r8k
    gapHTC = 0.0_r8k
    oxidelayer = 0.0_r8k
    gapmech = 0.0_r8k
    gapthrm = 0.0_r8k
    fuelrelmod = 0.0_r8k
    ifbarel = 0.0_r8k
    zcool = 0.0_r8k
    tcoolant = 0.0_r8k
    pcoolant = 0.0_r8k
    x = 0.0_r8k
    qf = 0.0_r8k
    xt = 0.0_r8k
    cladt = 0.0_r8k
    NodalMoles = 0.0_r8k
    NodalGMLES = 0.0_r8k
    rlcstrn = 0.0_r8k
    rlcstrnold = 0.0_r8k
    rc = 0.0_r8k
    gadoln = 0.0_r8k
    stold = 0.0_r8k
    stnew = 0.0_r8k
    deltimeold = 0.0_r8k
    deltimenew = 0.0_r8k
    sagold = 0.0_r8k
    sagnew = 0.0_r8k
    delsagold = 0.0_r8k
    delsagnew = 0.0_r8k
    delst = 0.0_r8k
    colddef = 0.0_r8k
    colddec = 0.0_r8k
    qnode = 0.0_r8k
    heprod = 0.0_r8k
    he = 0.0_r8k
    heold = 0.0_r8k
    boron10 = 0.0_r8k
    enrch = 0.0_r8k
    epsav = 0.0_r8k
    wimsburnup = 0.0_r8k
    oldwimsburnup = 0.0_r8k
    !
    END SUBROUTINE Allocate_Axial_Arrays
    !
    !
    !
    SUBROUTINE Allocate_Radial_Arrays
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates the radial-dependent arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (tfuelr(1:nr))
    ALLOCATE (tfuelr2(1:nr))
    ALLOCATE (tfring(1:nr))
    ALLOCATE (rrapow(1:nr))
    !
    tfuelr = 500.0_r8k
    tfuelr2 = 500.0_r8k
    tfring = 0.0_r8k
    rrapow = 0.0_r8k
    !
    END SUBROUTINE Allocate_Radial_Arrays
    !
    !
    !
    SUBROUTINE Allocate_2D_Arrays
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates 2D arrays for various combinations of na, nr and im
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ALLOCATE (coolanttemp(1:im+1,1:na+1))
    ALLOCATE (coolantpressure(1:im+1,1:na+1))
    ALLOCATE (storedearray(1:im+1,1:na))
    ALLOCATE (cltemparray(1:im+1,1:na))
    ALLOCATE (buarray(1:im+1,1:na))
    ALLOCATE (strainarray(1:im+1,1:na))
    ALLOCATE (straindiffarray(1:im+1,1:na))
    ALLOCATE (dpwxarray(1:im+1,1:na))
    ALLOCATE (creaparray(1:im+1,1:na))
    ALLOCATE (dumarray3(1:im+1,1:na))
    ALLOCATE (formf(1:na,1:nr))
    ALLOCATE (ringvol(1:nr,1:na))
    ALLOCATE (coldringl(1:nr,1:na))
    ALLOCATE (porosold(1:nr,1:na))
    ALLOCATE (porosnew(1:nr,1:na))
    ALLOCATE (cladtarray(1:im+1,1:na+1))
    ALLOCATE (rrev(1:nr,1:na))
    ALLOCATE (qaxnorm(1:na,1:im+1))
    ALLOCATE (crad(1:nr,1:na))
    ALLOCATE (hrad(1:nr,1:na))
    ALLOCATE (rapow(1:nr,1:na))
    ALLOCATE (uo2exp(1:nr,1:na))
    ALLOCATE (dpw(1:nr,1:na))
    ALLOCATE (dpwpp(1:nr,1:na))
    ALLOCATE (dpw2(1:nr,1:na))
    ALLOCATE (dpwpp2(1:nr,1:na))
    ALLOCATE (densf(1:nr,1:na))
    ALLOCATE (densp(1:nr,1:na))
    ALLOCATE (hotringl(1:nr,1:na))
    ALLOCATE (tmpfuel(1:nr,1:na))
    ALLOCATE (gasavail1(1:ngasr,1:na))
    ALLOCATE (gasavail2(1:ngasr,1:na))
    ALLOCATE (angr(1:na,2))
    ALLOCATE (ah2ogr(1:na,2))
    ALLOCATE (fgmgp(1:na,2))
    ALLOCATE (fmgr(1:na,2))
    ALLOCATE (hemgp(1:na,2))
    ALLOCATE (hmgr(1:na,2))
    ALLOCATE (ang(1:na,2))
    ALLOCATE (ah2og(1:na,2))
    ALLOCATE (epp(1:na,3))
    ALLOCATE (eppp(1:na,3))
    ALLOCATE (eppsv(1:na,3))
    ALLOCATE (eps(1:na,3))
    ALLOCATE (feps(1:na,3))
    ALLOCATE (reps(1:na,3))
    ALLOCATE (repsv(1:na,3))
    ALLOCATE (repsp(1:na,3))
    ALLOCATE (rfeps(1:na,3))
    ALLOCATE (rfepp(1:na,3))
    ALLOCATE (rfpsv(1:na,3))
    ALLOCATE (sig(1:na,3))
    ALLOCATE (epp1(1:na,3))
    ALLOCATE (sig1(1:na,3))
    ALLOCATE (eps1(1:na,3))
    ALLOCATE (ThermalStrain(1:na,3))
    ! Note: 15 is the number of radioactive gases being tracked by ANS-5.4
    ALLOCATE (RB_axial(15,1:na))
    ALLOCATE (RB_rod(15,1:im+1))
    !
    ! Assign a value to each array
    !
    coolanttemp = 0.0_r8k
    coolantpressure = 0.0_r8k
    storedearray = 0.0_r8k
    cltemparray = 0.0_r8k
    buarray = 0.0_r8k
    strainarray = 0.0_r8k
    straindiffarray = 0.0_r8k
    dpwxarray = 0.0_r8k
    creaparray = 0.0_r8k
    dumarray3 = 0.0_r8k
    formf = 0.0_r8k
    ringvol = 0.0_r8k
    coldringl = 0.0_r8k
    porosold = 0.0_r8k
    porosnew = 0.0_r8k
    cladtarray = 500.0_r8k
    rrev = 0.0_r8k
    qaxnorm = 0.0_r8k
    crad = 0.0_r8k
    hrad = 0.0_r8k
    rapow = 0.0_r8k
    uo2exp = 0.0_r8k
    dpw = 0.0_r8k
    dpwpp = 0.0_r8k
    dpw2 = 0.0_r8k
    dpwpp2 = 0.0_r8k
    densf = 0.0_r8k
    densp = 0.0_r8k
    hotringl = 0.0_r8k
    angr = 0.0_r8k
    ah2ogr = 0.0_r8k
    fgmgp = 0.0_r8k
    fmgr = 0.0_r8k
    hemgp = 0.0_r8k
    hmgr = 0.0_r8k
    ang = 0.0_r8k
    ah2og = 0.0_r8k
    epp = 0.0_r8k
    eppp = 0.0_r8k
    eppsv = 0.0_r8k
    eps = 0.0_r8k
    feps = 0.0_r8k
    reps = 0.0_r8k
    repsv = 0.0_r8k
    repsp = 0.0_r8k
    rfeps = 0.0_r8k
    rfepp = 0.0_r8k
    rfpsv = 0.0_r8k
    sig = 0.0_r8k
    epp1 = 0.0_r8k
    sig1 = 0.0_r8k
    eps1 = 0.0_r8k
    ThermalStrain = 0.0_r8k
    tmpfuel = 0.0_r8k
    gasavail1 = 0.0_r8k
    gasavail2 = 0.0_r8k
    RB_axial = 0.0_r8k
    RB_rod = 0.0_r8k
    !
    END SUBROUTINE Allocate_2D_Arrays
    !
    !
    !
    SUBROUTINE Allocate_3D_Arrays
    USE Kinds_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine allocates 3D arrays
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2014
    !
    ! FGR Radial Nodes, Axial Nodes, Timesteps
    ALLOCATE (prdct(1:ngasr,1:na,1:im+1))
    ! Axial Nodes, Radial Nodes, 2
    ALLOCATE (brnup3(1:na,1:nr,2))
    ! FGR Radial Nodes, Axial Nodes, Timesteps
    ALLOCATE (ansd(1:ngasr,1:na,1:im+1))
    !
    ! Assign a value to each array
    !
    prdct = 0.0_r8k
    brnup3 = 0.0_r8k
    ansd = 0.0_r8k
    !
    END SUBROUTINE Allocate_3D_Arrays
    !
END MODULE variables_frapcon



