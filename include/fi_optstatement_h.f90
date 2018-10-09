character(len=20), optional :: frapmode    ! 'frapcon' (default) or 'fraptran' calculations

! FRAPCON optional input variables
integer,           optional :: nr          ! number of radial segments
integer,           optional :: na          ! number of axial segments
integer,           optional :: ngasr       ! number of radial gas release nodes
integer,           optional :: nce         ! number of radial elements in the cladding for the FEA model
integer,           optional :: mechan      ! Cladding mechanical model (1 = FEA, 2 = FRACAS-I)
integer,           optional :: ngasmod     ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
integer,           optional :: icm         ! cladding type 4: Zircaloy-4
integer,           optional :: icor        ! crud model (0 or 1 = constant layer, 2= time dependent)
integer,           optional :: iplant      ! plant type Plant type, -2: PWR, -3: BWR, -4: HBWR
integer,           optional :: imox        ! fuel type (Fuel type, 0: UO_2)
integer,           optional :: igascal     ! Internal pressure calculation for FEA model igascal=1 normal pressure calculation igascal=0 use prescribed pressure set by p1
integer,           optional :: zr2vintage  ! zircaloy-2 vintage
integer,           optional :: moxtype     ! flag for type of Pu used in MOX
integer,           optional :: idxgas      ! fill gas type
integer,           optional :: iq          ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
integer,           optional :: ivardm      ! Option to specify variable axial node length (1 is on, 0 is off (default))
integer,           optional :: ifixedcoolt ! Specify whether to use user-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)
integer,           optional :: ifixedcoolp ! Specify whether to use user-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)
integer,           optional :: ifixedtsurf ! Specify to use fixed cladding surface temperatures
logical,           optional :: verbose     ! Print the output data in terminal
logical,           optional :: flag_iapws  ! flag to indicate the iapws-if97 version of steam table to be used (default : true)

! FRAPTRAN optional input variables
character(len=3) , optional :: coolant     ! 
character(len=3) , optional :: bheat       ! 
character(len=3) , optional :: mheat       ! 
character(len=3) , optional :: reflood     ! 
character(len=3) , optional :: internal    ! 
character(len=3) , optional :: metal       ! 
character(len=3) , optional :: deformation ! 
character(len=10), optional :: inst        ! 
character(len=12), optional :: relocmodel  ! relocation model: FRAPCON-3.3, FRAPCON-3.4 or FRAPCON-3.5
integer          , optional :: geomet      ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
integer          , optional :: nvol1       ! Number of coolant zones stacked on top of each other and surrounding fuel rod
integer          , optional :: lowpl       ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
integer          , optional :: pressu      ! Suboption to specify the coolant pressure history
integer          , optional :: massfl      ! Suboption to specify the coolant mass flux history
integer          , optional :: coreav      ! Suboption to specify the core average coolant enthalpy history
integer          , optional :: chf         ! Suboption to select the CHF correlation to be used
integer          , optional :: filmbo      ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
integer          , optional :: coldwa      ! Suboption to modify the critical heat flux for cold wall effect
integer          , optional :: axpow       ! Suboption to modify the critical heat flux for effect of axially varying power
integer          , optional :: bowing      ! Suboption to modify the critical heat flux as calculated according to the chf correlation suboption for fuel rod bowing effect.
integer          , optional :: spefbz      ! Suboption to prescribe film boiling over part of fuel rod.
integer          , optional :: geometry    ! Suboption to specify geometry parameters
integer          , optional :: nbundl      ! About a FLECHT correlation
integer          , optional :: refloodtime ! Suboption to specify start time of reactor core reflooding.
integer          , optional :: radiat      ! Suboption to specify the radiation heat transfer at the cladding surface during reflood.
integer          , optional :: ruptur      ! Suboption to specify the rupture plane as the line of demarcation between the FLECHT and steam cooling models
integer          , optional :: liquid      ! Suboption to specify the collapsed liquid level as the line of demarcation instead of the rupture plane
integer          , optional :: inlet       ! Suboption to specify the fraction of flooding water carried out of the core
integer          , optional :: reflo       ! Suboption to specify reflood rate as a function of time
integer          , optional :: pressure    ! Suboption to specify reactor vessel pressure as a function of time.
integer          , optional :: collaps     ! Suboption to specify the fraction of flooding water carried out of the core.
integer          , optional :: frapt4      ! Suboption to specify the FRAP-T4 FLECHT correlation instead of the generalized FLECHT correlation
integer          , optional :: geom        ! Suboption to specify the inner radius of the flow shroud. 
integer          , optional :: temp        ! Suboption to specify temperature history of flow shroud
integer          , optional :: tape2       ! Suboption to specify that the heat transfer coefficients, coolant temperature, and pressure are input on tape
integer          , optional :: nvol2       ! Number of heat transfer coefficient zones stacked on top of each other.
integer          , optional :: zone        ! Suboption to specify the elevation of heat transfer coefficient zone 1
integer          , optional :: upppl       ! Suboption to specify the enthalpy history of coolant at the top of the fuel rod(exit enthalpy)
integer          , optional :: jfb         ! Is the indicator of the film boiling correlation to be used.
integer          , optional :: nucbo       ! Suboption to select the nucleate boiling heat transfer correlation to be used
integer          , optional :: unitin      ! Option to specify that the input data are in SI units
integer          , optional :: unitout     ! Option to specify that the output is to be in SI units even though the input is in British units
integer          , optional :: res         ! Option to specify that a restart file is to be created
integer          , optional :: pow         ! Option to specify the printout of the fuel rod state at each step of the first power ramp
integer          , optional :: gasflo      ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
integer          , optional :: idoxid      ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
integer          , optional :: cathca      ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
integer          , optional :: baker       ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
integer          , optional :: noball      ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
integer          , optional :: cenvoi      ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
integer          , optional :: soltyp      ! Option to specify an explicit solution
