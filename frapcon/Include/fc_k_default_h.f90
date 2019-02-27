        this % mechan              = 2                           ! Cladding mechanical model (1 = FEA, 2 = FRACAS-I)
        this % ngasmod             = 2                           ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
        this % jst(1)              = 1                           ! Sequential # of the power shape to be used for each timestep
        this % nplot               = 0                           ! Output options Specifies whether to print plot information for use with Excel package FRAPlot or APT Plot _frapcon(0 = no, 1 = limited, 2 = detailed)
        this % iq                  = 1                           ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
        this % jdlpr               =-1                           ! Specifies the output file printing option (-1 = axial summary, 0 = peak-power node, 1 = all axial nodes)
        this % nunits              = 1                           ! Signal for units system to be used for input and output: 1 = British units 0 = SI units
        this % ivardm              = 0                           ! Option to specify variable axial node length (1 = ON, 0 = OFF(Default))
        this % nsp                 = 1                           !
        this % chmfrh              = 0.d0                        ! Chamfer height
        this % chmfrw              = 0.d0                        ! Chamfer width
        this % comp                = 0.d0                        ! PuO2 weight percent if MOX fuel (wt%)
        this % moxtype             = 0                           ! Flag for type of Pu used in MOX (Used if icm = 1 or 2)       ???WTF: compare with imox
        this % ifba                = 0.0d0                       ! Percent IFBA rods in core (%)
        this % b10                 = 0.0d0                       ! Boron-10 enrichment (atom %) in ZrB2
        this % zrb2thick           = 0.0d0                       ! ZrB2 thickness
        this % zrb2den             = 90.0d0                      ! ZrB2 density
        this % ppmh2o              = 0.0d0                       ! Weight ppm H2O in fuel (wt.ppm)
        this % ppmn2               = 0.0d0                       ! Weight ppm N_2 in fuel (wt.ppm)
        this % deng                = 0.0d0                       ! Fuel open porosity fraction (%TD)
        this % tsint               = 2911.0d0                    ! Pellet centering temperature, F
        this % zr2vintage          = 1                           ! Zircaloy-2 vintage (Used when icm = 2) (0 = pre-1998, 1 = post-1998)
        this % cldwks              = 0.2d0                       ! Clad cold work (0.5= SRA, 0.0= RXA)
        this % catexf              = 0.05d0                      ! Clad texture factor
        this % chorg               = 10.0d0                      ! As-fabricated clad hydrogen content (wt.ppm)
        this % amfair              = 0.0d0                       ! Molar fraction of air
        this % amfarg              = 0.0d0                       ! Molar fraction of argon
        this % amffg               = 0.0d0                       ! Molar fraction of fission gas
        this % amfhe               = 0.0d0                       ! Molar fraction of hellium
        this % amfh2               = 0.0d0                       ! Molar fraction of hydrogen
        this % amfkry              = 0.0d0                       ! Molar fraction of krypton
        this % amfn2               = 0.0d0                       ! Molar fraction of nitrogen
        this % amfxe               = 0.0d0                       ! Molar fraction of xenon
        this % icor                = 0                           ! Crud model (0 or 1 = constant layer, 2= time dependent)
        this % crdtr               = 0.0d0                       ! Crud accumulation rate
        this % crudmult            =-1.0d0                       ! Axial crud thickness multiplier
        this % flux                = 0.221d+17                   ! ???WTF: 
        this % DecayModel          = 1                           ! Decay heat model to use. Currently only one model is implemented, and is the default model. 1 – ANS
        this % fpdcay              = 1.0d0                       ! Multiplicative factor applied to power given by decay heat model
        this % ifixedtsurf         = 0                           ! ???WTF: there is the same in proc
        this % xt                  = 0.0d0                       ! Elevation in each cladt, xt array defining a cladding temperature profile
        this % cladt               = 0.0d0                       ! Cladding surface temperature at xt(n) elevation for each M-th power shape
        this % jnsurftemp          = 0                           ! # of cladt, xt pairs for each axial temperature distribution
        this % ifixedcoolt         = 0                           ! ???WTF: there is the same in proc
        this % zcool               = 0.0d0                       ! Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1.
        this % ifixedcoolp         = 0                           ! ???WTF: there is the same in proc
        this % crephr              = 10.0d0                      ! Subdivision for internal creep steps
        this % sgapf               = 31.0d0                      ! Number of fission gas atoms formed per 100 fissions
        this % qend                = 0.3d0                       ! Fraction of end-node heat that transfers to the plenum gas
        this % igas                = 0                           ! Time step to begin calculation of fission gas release
        this % frcoef              = 0.015d0                     ! Coulomb friction coefficient between the cladding and the fuel pellet
        this % igascal             = 1                           ! Internal pressure calculation for FEA model igascal=1 normal pressure calculation igascal=0 use prescribed pressure set by p1_frapcon
        this % sigftc              = 0.0d0                       ! Bias on fuel thermal conductivity model (# of standard deviations)
        this % sigftex             = 0.0d0                       ! Bias on fuel thermal expansion model (# of standard deviations)
        this % sigfgr              = 0.0d0                       ! Bias on fission gas release model (# of standard deviations)
        this % sigswell            = 0.0d0                       ! Bias on fuel swelling model (# of standard deviations)
        this % sigcreep            = 0.0d0                       ! Bias on cladding creep model (# of standard deviations)
        this % siggro              = 0.0d0                       ! Bias on cladding axial growth model (# of standard deviations)
        this % sigcor              = 0.0d0                       ! Bias on cladding corrosion model (# of standard deviations)
        this % sigh2               = 0.0d0                       ! Bias on cladding hydrogen pickup model (# of standard deviations)
        this % irefab              = 10000                       ! Timestep to start using refabricated values (Default = 10,000)
        this % nrefab1             = 0                           ! Lower axial node for refabrication
        this % nrefab2             = 0                           ! Upper axial node for refabrication
        this % cplrefab            = 0.0d0                       ! Refabricated upper plenum length
        this % vsrefab             = 0.0d0                       ! Number of spring turns in refabricated upper plenum
        this % dspgrefab           = 0.0d0                       ! New plenum spring coil diameter
        this % dspgwrefab          = 0.0d0                       ! New plenum spring wire diameter
        this % fgpavrefab          = 0.0d0                       ! Fill gas pressure at time step of refabrication
        this % airrefab            = 0.0d0                       ! Fraction of air in refabricated rod
        this % n2refab             = 0.0d0                       ! Fraction of nitrogen in refabricated rod
        this % arrefab             = 0.0d0                       ! Fraction of argon in refabricated rod
        this % fgrefab             = 0.0d0                       ! Fraction of fission gas in refabricated rod
        this % herefab             = 1.0d0                       ! Fraction of helium in refabricated rod (Default = 1.0)
        this % krrefab             = 0.0d0                       ! Fraction of krypton in refabricated rod
        this % xerefab             = 0.0d0                       ! Fraction of xenon in refabricated rod
        this % IsModelingSpentFuel = .false.                     ! ???WTF:
        this % idatingcreep        = 0                           ! Creep model choice (BE vs. conservative creep and Monkman-Grant)
        this % ncreephist          = 1                           ! Type of solution (1: helium cooling, 2: nitrogen cooling, 3: specified temperature, 4: specified temperature and stress
        this % creeptime           = 0.0d0                       ! Time duration of creep calculation (years)
        this % creeppooltime       = 5.0d0                       ! Time spent in pool after reactor discharge (years)
        this % ncreepstep          = 1                           ! # of creep output steps
        this % ncreeptab           = 1                           ! # of input pairs for ncreephist = 3 or 4
        this % creeptabtime        = 0.0d0                       ! Time input for ncreephist = 3 or 4
        this % creeptabtemp        = 0.0d0                       ! Temperature input for ncreephist = 3 or 4
        this % creeptabstress      = 0.0d0                       ! Stress input for ncreephist = 4
        this % stopox              = 1.0d+10                     ! Indicator for when to stop the oxide calculation (days).
        this % nopt                = 1                           ! Specifies the output file print control (0 = each timestep, 1 = input & summary only)
        this % nplot               = 0                           ! Specifies whether to print plot information
        this % ntape               = 0                           ! Specifies to write a FRAPCON-to-FRAPTRAN restart tape
        this % calcoxide           = .true.                      ! Flag to specify whether or not to calculate the oxidation reaction
        this % gaphtcmult          = 1.0d0                       ! User supplied gap heat transfer coefficient multiplier
        this % modheat             = 0.0d0                       ! Moderator heating fraction
        this % cladelmod           =-1.0d0                       ! User-supplied value for cladding elastic modulus (Pa)
        this % relocmodel          = 'FRAPCON-3.5'               ! Specifies fuel relocation model
        this % fuelreloc           =-1.0d0                       ! User-supplied value for fuel relocation
        this % gaprecov            = 0.5d0                       ! User-supplied value for gap recovery
        this % TimeIntegration     = 0                           ! Specify time integration technique (0 = None, 1 = Linear Interpolation, 2 = Histogram)
        this % newtimestep         = 1.0d0                       ! The new timestep value to use for the calculation_frapcon. (days) Used when TimeIntegration = 1 or 2
        this % RestartTime         =-1.0d0                       ! Problem Time (s) to use for a restart calculation_frapcon
        this % nfrttr              = 0                           ! Specifies to create the output file read by the NRC's Internal AIG for TRACE Runs (0 = no, 1 = yes)
        this % nread               = 0                           ! Specifies to start from a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
        this % nrestr              = 0                           ! Specifies to write a FRAPCON-to-FRAPCON restart tape (0 = no, 1 = yes)
        this % cpl                 = 6.3386d0                    ! Cold plenum length, in
        this % crdt                = 0.d0                        ! Crud thickness, in
        this % thkcld              = 0.0224d0                    ! Thickness of cladding, in
        this % thkgap              = 0.0033d0                    ! Thickness of gap, in
        this % dco                 = 0.3740d0                    ! Outer cladding diameter, in
        this % pitch               = 0.5_r8k                     ! Center to center rod distance, in
        this % rc                  = 0.0_r8k                     ! Radius of the fuel pellet central annulus, in
        this % fotmtl              = 2.d0                        ! Fuel oxygen-to-metal ratio
        this % dishsd              = 0.0d0                       ! Dish shoulder width, in
        this % den                 = 94.43_r8k                   ! As-fabricated apparent fuel density, %TD
        this % dspg                = 0.3                         ! Spring diameter, in
        this % fa                  = 1.d0                        ! Peak-to-average power ratio
        this % dspgw               = 0.0394                      ! Spring wire diameter, in
        this % enrch               = 1.0_r8k                     ! Fuel U-235 enrichment (atom per total heavy metal atoms)
        this % fgpav               = 340.84                      ! Fill gas pressure, psi
        this % hdish               = 0.d0                        ! Dish height, in
        this % hplt                = 0.387d0                     ! Pellet height, in
        this % icm                 = 4                           ! Cladding type, 4: Zircaloy-4
        this % idxgas              = 1                           ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
        this % iplant              =-2                           ! Plant type, -2: PWR, -3: BWR, -4: HBWR
        this % imox                = 0                           ! Fuel type, 0: UO_2
        this % totl                = 1.3100_r8k                  ! Total length of active fuel, ft
        this % roughc              = 1.97d-5                     ! Clad roughness, in
        this % roughf              = 7.87d-5                     ! Fuel roughness, in
        this % vs                  = 30.d0                       ! Number of spring turns
        this % rsntr               = 100.d0                      ! Expected resintering density increase, kg/m**3
        this % nsp                 = 0                           !
        this % slim                = 0.05d0                      ! User supplied swelling limit (vol fraction) (Default = 0.05)
        this % enrpu39             = 0.d0                        ! Fuel pellet Pu-239 content
        this % enrpu40             = 0.d0                        ! Fuel pellet Pu-240 content
        this % enrpu41             = 0.d0                        ! Fuel pellet Pu-241 content
        this % enrpu42             = 0.d0                        ! Fuel pellet Pu-242 content
        this % tw(:)               = 600.                        ! Coolant inlet temperature, F
        this % p2(:)               = 2000.                       ! Coolant System Pressure, Psi
        this % go(:)               = 2.D+6                       ! Coolant mass flux around fuel rod, lb/hr * ft^2
        this % qf(:)               = 1.                          ! Ratio of linear power
        this % qmpy                = 6.                          ! The linear heat generation rate, kW/ft (after make it turns to rod average heat flux, BTU/(hr*ft^2) )
        this % gadoln(:)           = 0.d0                        ! Weight fraction of gadolinia in the fuel
        this % x(:)                = 0.d0                        ! Axial evaluation, ft
        this % deltaz(:)           = 0.d0                        ! Axial node thickness, ft
        this % stold               = 0.d0                        !
        this % deltimeold          = 0.d0                        !
        this % sagold              = 0.d0                        !
        this % delsagold           = 0.d0                        !
        this % stnew               = 0.d0                        !
        this % deltimenew          = 0.d0                        !
        this % sagnew              = 0.d0                        !
        this % delsagnew           = 0.d0                        !
        this % delst               = 0.d0                        !
        this % afdn                = 1.d0                        !
        this % afgr                = 1.d0                        !
        this % afal                = 1.d0                        !
        this % amfh2o              = 0.d0                        !
        this % coldwk              = 0.1d0                       !
        this % ncreep              = 0                           !
        this % nheal               = 1                           !
        this % cooltype            = 0                           !
        this % geom                = 0                           !
        this % datingtstep         = 1.0d-2                      !
        this % tcc                 = 0.d0                        !
        this % tref                = 77.d0                       !
        this % TgasFab             = 77.d0                       !
        this % rprm1               = 3.45d0                      !
        this % ctmax               = 386.33d0                    !
        this % namerf              = 'rf.txt'                    !