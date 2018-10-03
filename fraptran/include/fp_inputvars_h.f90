!character(len=3) :: coolant
!character(len=3) :: heat
!character(len=3) :: reflood
!character(len=3) :: internal
!character(len=3) :: metal
!character(len=3) :: deformation
!character(len=3) :: heat
!character(len=10):: inst
!
!integer :: geomet   ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
!integer :: nvol1    ! Number of coolant zones stacked on top of each other and surrounding fuel rod
!integer :: lowpl    ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
!integer :: pressu   ! Suboption to specify the coolant pressure history
!integer :: massfl   ! Suboption to specify the coolant mass flux history
!integer :: coreav   ! Suboption to specify the core average coolant enthalpy history
!integer :: chf      ! Suboption to select the CHF correlation to be used
!integer :: filmbo   ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
!integer :: coldwa   ! Suboption to modify the critical heat flux for cold wall effect
!integer :: axpow    ! Suboption to modify the critical heat flux for effect of axially varying power
!integer :: bowing   ! Suboption to modify the critical heat flux as calculated according to the chf correlation suboption for fuel rod bowing effect.
!integer :: spefbz   ! Suboption to prescribe film boiling over part of fuel rod.
!integer :: geometry ! Suboption to specify geometry parameters
!integer :: nbundl   ! About a FLECHT correlation
!integer :: refloodtime ! Suboption to specify start time of reactor core reflooding.
!integer :: radiat   ! Suboption to specify the radiation heat transfer at the cladding surface during reflood.
!integer :: ruptur   ! Suboption to specify the rupture plane as the line of demarcation between the FLECHT and steam cooling models
!integer :: liquid   ! Suboption to specify the collapsed liquid level as the line of demarcation instead of the rupture plane
!integer :: inlet    ! Suboption to specify the fraction of flooding water carried out of the core
!integer :: reflo    ! Suboption to specify reflood rate as a function of time
!integer :: pressure ! Suboption to specify reactor vessel pressure as a function of time.
!integer :: collaps  ! Suboption to specify the fraction of flooding water carried out of the core.
!integer :: frapt4   ! Suboption to specify the FRAP-T4 FLECHT correlation instead of the generalized FLECHT correlation
!integer :: geom     ! Suboption to specify the inner radius of the flow shroud. 
!integer :: temp     ! Suboption to specify temperature history of flow shroud
!integer :: tape2    ! Suboption to specify that the heat transfer coefficients, coolant temperature, and pressure are input on tape
!integer :: nvol2    ! Number of heat transfer coefficient zones stacked on top of each other.
!integer :: press    ! Suboption to specify coolant pressure.
!integer :: zone     ! Suboption to specify the elevation of heat transfer coefficient zone 1
!integer :: upppl    ! Suboption to specify the enthalpy history of coolant at the top of the fuel rod(exit enthalpy)
!integer :: jfb      ! Is the indicator of the film boiling correlation to be used.
!integer :: nucbo    ! Suboption to select the nucleate boiling heat transfer correlation to be used
!integer :: unitin   ! Option to specify that the input data are in SI units
!integer :: unitout  ! Option to specify that the output is to be in SI units even though the input is in British units
!integer :: res      ! Option to specify that a restart file is to be created
!integer :: pow      ! Option to specify the printout of the fuel rod state at each step of the first power ramp
!integer :: gasflo   ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
!integer :: idoxid   ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
!integer :: cathca   ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
!integer :: baker    ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
!integer :: noball   ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
!integer :: cenvoi   ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
!integer :: soltyp   ! Option to specify an explicit solution
!
!real(8) :: ffch        ! User-supplied multiplier in equation for CHF reduction due to bowing
!real(8) :: emptm       ! Time at which reactor core is empty of coolant and adiabatic heatup begins, s
!real(8) :: ftgap2      ! Gap multiplier
!real(8) :: roddiameter ! Cladding outer diameter, m
!real(8) :: rodlength   ! Fuel pellet stack length, m
!real(8) :: gapthk      ! Radial fuel-cladding gap thickness, m
!real(8) :: gsms        ! Quantity of gas in fuel rod, g-moles
!real(8) :: buoxide     ! Burnup at which double sided oxidation should be calculated, GWd/MTU
!real(8) :: fpowr       ! Multiplicative factor for power
!real(8) :: cladpower   ! Option to specify heating of the cladding by gamma radiation
!
!real(8), dimension(:), allocatable :: gfrac  ! Fraction of gas that is helium, mole-fraction
!real(8), dimension(:), allocatable :: butemp ! Radial burnup profiles for each axial node, MWd/MTM