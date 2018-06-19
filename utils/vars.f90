                                                               case("additional fuel densification factor")
this % driver % r__afdn         = var(1)                       case("cladding type")
this % driver % r__icm          = var(1)                       case("crud model")
this % driver % r__icor         = var(1)                       case("clad texture factor")
this % driver % r__catexf       = var(1)                       case("as-fabricated clad hydrogen content, wt.ppm")
this % driver % r__chorg        = var(1)                       case("clad cold work")
this % driver % r__cldwks       = var(1)                       case("cold plenum length")
this % driver % r__cpl          = var(1)                       case("constant crud thickness")
this % driver % r__crdt         = var(1)                       case("crud accumulation rate")
this % driver % r__crdtr        = var(1)                       case("creep step duration, hr")
this % driver % r__crephr       = var(1)                       case("fuel open porosity fraction, %TD")
this % driver % r__deng         = var(1)                       case("spring diameter, mm")
this % driver % r__dspg         = var(1) * mmtoin              case("spring wire diameter, mm")
this % driver % r__dspgw        = var(1) * mmtoin              case("number of spring turns")
this % driver % r__vs           = var(1)                       case("peak-to-average power ratio")
this % driver % r__fa           = var(1)                       case("fill gas pressure, Pa")
this % driver % r__fgpav        = var(1) * PatoPSI             case("fuel oxygen-to-metal ratio")
this % driver % r__fotmtl       = var(1)                       case("fill gas type")
this % driver % r__idxgas       = var(1)                       case("plant type")
this % driver % r__iplant       = var(1)                       case("fuel type")
this % driver % r__imox         = var(1)                       case("weight ppm H2O in fuel, wt.ppm")
this % driver % r__ppmh2o       = var(1)                       case("weight ppm N2 in fuel, wt. ppm")
this % driver % r__ppmn2        = var(1)                       case("expected resintering density increase, kg/m^3")
this % driver % r__rsntr        = var(1)                       case("fision gas atoms per 100 fissions")
this % driver % r__sgapf        = var(1)                       case("swelling limit")
this % driver % r__slim         = var(1)                       case("pellet centering temperature, K")
this % driver % r__tsint        = tkf(var(1))                  case("grain size of the fuel, um")
this % driver % r__grnsize      = var(1)                       case("FEA friction coefficient")
this % driver % r__frcoef       = var(1)                       case("FEA internal pressure flag")
this % driver % r__igascal      = var(1)                       case("percent IFBA rods in core, %")
this % driver % r__ifba         = var(1)                       case("boron-10 enrichment in ZrB2, atom %")
this % driver % r__b10          = var(1)                       case("ZrB2 thickness, mm")
this % driver % r__zrb2thick    = var(1) * mmtoin               case("ZrB2 density, mm")
this % driver % r__zrb2den      = var(1) * mmtoin               case("zircaloy-2 vintage")
this % driver % r__zr2vintage   = var(1)                       case("decay heat multiplier")
this % driver % r__fpdcay       = var(1)                       case("flag for type of Pu used in MOX")
this % driver % r__moxtype      = var(1)                       case("mole fraction of air")
this % driver % r__amfair       = var(1)                       case("molar fraction of argon")
this % driver % r__amfarg       = var(1)                       case("mole fraction of fission gas")
this % driver % r__amffg        = var(1)                       case("molar fraction of helium")
this % driver % r__amfhe        = var(1)                       case("molar fraction of hydrogen")
this % driver % r__amfh2        = var(1)                       case("molar fraction of water")
this % driver % r__amfh2o       = var(1)                       case("molar fraction of krypton")
this % driver % r__amfkry       = var(1)                       case("molar fraction of nitrogen")
this % driver % r__amfn2        = var(1)                       case("molar fraction of xenon")
this % driver % r__amfxe        = var(1)                       case("Bias on fuel thermal conductivity")
this % driver % r__sigftc       = var(1)                       case("Bias on fuel thermal expansion")
this % driver % r__sigftex      = var(1)                       case("Bias on fission gas release")
this % driver % r__sigfgr       = var(1)                       case("Bias on fuel swelling")
this % driver % r__sigswell     = var(1)                       case("Bias on cladding creep")
this % driver % r__sigcreep     = var(1)                       case("Bias on cladding axial growth")
this % driver % r__siggro       = var(1)                       case("Bias on cladding corrosion")
this % driver % r__sigcor       = var(1)                       case("Bias on cladding hydrogen pickup")
this % driver % r__sigh2        = var(1)                       case("fuel pellet Pu-239 content")
this % driver % r__enrpu39      = var(1)                       case("fuel pellet Pu-240 content")
this % driver % r__enrpu40      = var(1)                       case("fuel pellet Pu-241 content")
this % driver % r__enrpu41      = var(1)                       case("fuel pellet Pu-242 content")
this % driver % r__enrpu42      = var(1)                       case("pellet height, mm")
this % driver % r__hplt         = var(1) * mmtoin              case("chamfer height, mm")
this % driver % r__chmfrh       = var(1) * mmtoin              case("chamfer width, mm")
this % driver % r__chmfrw       = var(1) * mmtoin              case("dish shoulder width, mm")
this % driver % r__dishsd       = var(1) * mmtoin              case("clad roughness, mm")
this % driver % r__roughc       = var(1) * mmtoin              case("fuel roughness, mm")
this % driver % r__roughf       = var(1) * mmtoin              case("input fuel burnup")
this % driver % r__buin(:)      = var(:) * MWskgUtoMWdMTU      case("PuO2 weight percent if MOX fuel, wt%")
this % driver % r__comp(:)      = var(:)                       case("Heat flux, W/m^2")
this % driver % r__qc(:)        = var(:) / Bhft2toWm2          case("gadolinia content at each axial node")
this % driver % r__gadoln(:)    = var(:)                       case("end-node to plenum heat transfer fraction")
this % driver % r__qend(it)     = var(1)                       case("radius of the fuel pellet central annulus, mm")
this % driver % r__rc(:)        = var(:) * mmtoin              case("cladding surface temperature, K")
this % driver % r__cladt(:)     = (/( tkf(var(i)), i = 1, n )/)case("rod internal pressure for each time tep for FEA model, MPa")
this % driver % r__p1(it)       = var(1) * patoPSIcase("axial crud thickness multiplier")
this % driver % r__crudmult(:)  = var(:)



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
