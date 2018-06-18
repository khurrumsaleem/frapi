case("Additional fuel densification factor")
    this % driver % r__afdn         = var(1)                       
case("Cladding type")
    this % driver % r__icm          = var(1)                       
case("Crud model")
    this % driver % r__icor         = var(1)                       
case("Clad texture factor")
    this % driver % r__catexf       = var(1)                       
case("As-fabricated clad hydrogen content, wt.ppm")
    this % driver % r__chorg        = var(1)                       
case("Clad cold work")
    this % driver % r__cldwks       = var(1)                       
case("Cold plenum length")
    this % driver % r__cpl          = var(1)                       
case("Constant crud thickness")
    this % driver % r__crdt         = var(1)                       
case("Crud accumulation rate")
    this % driver % r__crdtr        = var(1)                       
case("Creep step duration, hr")
    this % driver % r__crephr       = var(1)                       
case("As-fabricated apparent fuel density, %TD")
    this % driver % r__den          = var(1)                       
case("Fuel open porosity fraction, %TD")
    this % driver % r__deng         = var(1)                       
case("Spring diameter, mm")
    this % driver % r__dspg         = var(1) * mmtoin              
case("Spring wire diameter, mm")
    this % driver % r__dspgw        = var(1) * mmtoin              
case("Number of spring turns")
    this % driver % r__vs           = var(1)                       
case("Peak-to-average power ratio")
    this % driver % r__fa           = var(1)                       
case("Fill gas pressure, Pa")
    this % driver % r__fgpav        = var(1) * PatoPSI             
case("Fuel oxygen-to-metal ratio")
    this % driver % r__fotmtl       = var(1)                       
case("Fill gas type")
    this % driver % r__idxgas       = var(1)                       
case("Plant type")
    this % driver % r__iplant       = var(1)                       
case("Fuel type")
    this % driver % r__imox         = var(1)                       
case("Weight ppm H2O in fuel, wt.ppm")
    this % driver % r__ppmh2o       = var(1)                       
case("Weight ppm N2 in fuel, wt. ppm")
    this % driver % r__ppmn2        = var(1)                       
case("Expected resintering density increase, kg/m^3")
    this % driver % r__rsntr        = var(1)                       
case("Fision Gas atoms per 100 fissions")
    this % driver % r__sgapf        = var(1)                       
case("swelling limit")
    this % driver % r__slim         = var(1)                       
case("Pellet centering temperature, K")
    this % driver % r__tsint        = tkf(var(1))                  
case("grain size of the fuel, um")
    this % driver % r__grnsize      = var(1)                       
case("Fuel/Cladding friction coefficient for FEA model")
    this % driver % r__frcoef       = var(1)                       
case("Internal pressure calculation for FEA model")
    this % driver % r__igascal      = var(1)                       
case("Percent IFBA rods in core, %")
    this % driver % r__ifba         = var(1)                       
case("Boron-10 enrichment in ZrB2, atom %")
    this % driver % r__b10          = var(1)                       
case("ZrB2 thickness, mm")
    this % driver % r__zrb2thick    = var(1) * mm2in               
case("ZrB2 density, mm")
    this % driver % r__zrb2den      = var(1) * mm2in               
case("Zircaloy-2 vintage")
    this % driver % r__zr2vintage   = var(1)                       
case("User specified decay heat multiplier")
    this % driver % r__fpdcay       = var(1)                       
case("Flag for type of Pu used in MOX")
    this % driver % r__moxtype      = var(1)                       
case("Mole fraction of air")
    this % driver % r__amfair       = var(1)                       
case("Molar fraction of argon")
    this % driver % r__amfarg       = var(1)                       
case("Mole fraction of fission gas")
    this % driver % r__amffg        = var(1)                       
case("Molar fraction of helium")
    this % driver % r__amfhe        = var(1)                       
case("Molar fraction of hydrogen")
    this % driver % r__amfh2        = var(1)                       
case("Molar fraction of water")
    this % driver % r__amfh2o       = var(1)                       
case("Molar fraction of krypton")
    this % driver % r__amfkry       = var(1)                       
case("Molar fraction of nitrogen")
    this % driver % r__amfn2        = var(1)                       
case("Molar fraction of xenon")
    this % driver % r__amfxe        = var(1)                       
case("Bias on fuel thermal conductivity model")
    this % driver % r__sigftc       = var(1)                       
case("Bias on fuel thermal expansion model")
    this % driver % r__sigftex      = var(1)                       
case("Bias on fission gas release model")
    this % driver % r__sigfgr       = var(1)                       
case("Bias on fuel swelling model")
    this % driver % r__sigswell     = var(1)                       
case("Bias on cladding creep model")
    this % driver % r__sigcreep     = var(1)                       
case("Bias on cladding axial growth model")
    this % driver % r__siggro       = var(1)                       
case("Bias on cladding corrosion model")
    this % driver % r__sigcor       = var(1)                       
case("Bias on cladding hydrogen pickup model")
    this % driver % r__sigh2        = var(1)                       
case("Fuel pellet Pu-239 content")
    this % driver % r__enrpu39      = var(1)                       
case("Fuel pellet Pu-240 content")
    this % driver % r__enrpu40      = var(1)                       
case("Fuel pellet Pu-241 content")
    this % driver % r__enrpu41      = var(1)                       
case("Fuel pellet Pu-242 content")
    this % driver % r__enrpu42      = var(1)                       
case("Pellet height, mm")
    this % driver % r__hplt         = var(1) * mm2in               
case("Chamfer height, mm")
    this % driver % r__chmfrh       = var(1) * mm2in               
case("Chamfer width, mm")
    this % driver % r__chmfrw       = var(1) * mm2in               
case("Dish shoulder width, mm")
    this % driver % r__dishsd       = var(1) * mm2in               
case("Clad roughness, mm")
    this % driver % r__roughc       = var(1) * mm2in               
case("Fuel roughness, mm")
    this % driver % r__roughf       = var(1) * mm2in               
case("Input fuel burnup")
    this % driver % r__buin         = var(:)                       
case("PuO2 weight percent if MOX fuel, wt%")
    this % driver % r__comp         = var(:)                       
case("Node heat flux, W/m^2")
    this % driver % r__qc           = var(:) / Bhft2toWm2          
case("Gadolinia content at each axial node")
    this % driver % r__gadoln       = var(:)                       
case("End-node to plenum heat transfer fraction")
    this % driver % r__qend         = var(1)                       
case("Radius of the fuel pellet central annulus, mm")
    this % driver % r__rc           = var(1) * mm2in               
case("Cladding surface temperature, K")
    this % driver % r__cladt        = var(:)                       
case("Rod internal pressure for each time tep for FEA model, MPa")
    this % driver % r__p1           = var(1) * patoPSI             
case("Axial crud thickness multiplier")
    this % driver % r__crudmult     = var(1)                       


!NOT IN USED
ivardm                                  "Option to specify variable axial node length (1 = ON, 0 = OFF(Default))"

irefab                                  "Timestep to start using refabricated values (Default = 10,000)"
nrefab1                                 "Lower axial node for refabrication"
nrefab2                                 "Upper axial node for refabrication"
cplrefab                                "Refabricated upper plenum length"
vsrefab                                 "Number of spring turns in refabricated upper plenum"
dspgrefab                               "New plenum spring coil diameter"
dspgwrefab                              "New plenum spring wire diameter"
fgpavrefab                              "Fill gas pressure at time step of refabrication"
airrefab                                "Fraction of air in refabricated rod"
n2refab                                 "Fraction of nitrogen in refabricated rod"
arrefab                                 "Fraction of argon in refabricated rod"
fgrefab                                 "Fraction of fission gas in refabricated rod"
herefab                                 "Fraction of helium in refabricated rod (Default = 1.0)"
krrefab                                 "Fraction of krypton in refabricated rod"
xerefab                                 "Fraction of xenon in refabricated rod"

thkcld                                  "As-fabricated cladding thickness"
thkgap                                  "As-fabricated gap thickness"
enrch                                   "Fuel U-235 enrichment (at%)"
ProblemTime                             "Cumulative time at the end of each timestep"
jst                                     "Sequential # of the power shape to be used for each timestep"
totl                                    "Total length of active fuel"
ngasmod                                 "Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)"
qf                                      "Ratio of linear power at x(n) elevation to axially average value for each M-th power shape"
qmpy                                    "Rod average heat flux for power-time step (btu/hr-ft**2)"
zcool                                   "Elevation defining the coolant temperature profile. Used when ifixedcoolt = 1."
jstsurftemp                             "Sequential # of the cladding temperature profile to be used for each timestep"
jnsurftemp                              "# of cladt, xt pairs for each axial temperature distribution"
jn                                      "# of qf, x pairs for each axial power shape"
ctmax                                   ""
tcoolant                                "Bulk coolant temperature at each axial node & timestep"
pcoolant                                "Bulk coolant pressure at each axial node & timestep"
p2                                      "Coolant System Pressure, input for each timestep if nsp = 1"
tw                                      "Coolant inlet temperature, input for each timestep if nsp = 1"
go                                      "Coolant mass flux around fuel rod, input for each timestep if nsp = 1"
dco                                     "Outer cladding diameter"
deltaz                                  "Axial node length (ft)"
iq                                      "Axial power shape indicator (0 = user-input, 1 = chopped cosine)"
this % driver % r__igas         = var(1)                           case("Timestep to begin calculation of fission gas release")
this % driver % r__pitch        = var(1)                           case("Center to center rod distance")
this % driver % r__ifixedcoolt  = var(1)                           case("Specify whether to use user-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)")
this % driver % r__ifixedcoolp  = var(1)                           case("Specify whether to use user-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)")
case("Specify to use fixed cladding surface temperatures")
    this % driver % r__ifixedtsurf  = var(1)                       
