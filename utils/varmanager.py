data0 = """
fraptran_rod
namerf
reflood              
reflo                
relocmodel           
deformation          
inp                  
inst                 
metal                
naxn                 
cathca               
prestmp              
lowpl                
nbhtc                
rtheta               
geom                 
iStoicGrad           
nvol1                
jchf                 
gasflo               
noball               
prescri              
mechan               
nfmesh               
NumAxProfiles        
azang                
gbh                  
ph                   
idoxid               
maxit                
tape1                
jtr                  
NRestart             
ncards               
CladType             
massfl               
odoxid               
nchn                 
TranSwell            
unitin               
geometry             
filmbo               
noiter               
nthermex             
ncmesh               
IndexFC2Print        
chf                  
irefine              
ProtectiveOxide      
presfgr              
hupta                
coreav               
irupt                
hinta                
nIDoxide             
bheat                 
mheat                 
hbh                  
IndexGrainBndSep     
cenvoi               
grass                
baker                
ncolbp               
geomet               
soltyp               
naz                  
profile              
nsym                 
unitout              
tape2                
nvol2                
zone               
upppl                
jfb            
nucbo              
plenumtemp                   
coldwa               
axpow                
bowing               
spefbz                 
nbundl                 
refloodtime                    
collaps                
frapt4                 
gasmoles0                  
buoxide                
splbp                
tpowf                
ruptstrain           
frcoef               
CladPower            
pitch                
bowthr               
hrad                 
inlet                
frden                
RodDiameter          
refdtm               
totnb                
FuelPelDiam          
gapthk               
flxsec               
radiat               
ffch                 
fpdcay               
roughc               
roughf               
gbse                 
prsacc               
fpowr                
dofset               
zs                   
spdbp                
pelh                 
pdrato               
dofang               
tgas0                
tsntrk               
achn                 
doffst               
RodLength            
ruptur               
epsht1               
OpenPorosityFraction 
zad                  
rshrd                
tref                 
tflux                
emptm                
trise                
htca                 
fltgap2              
hydiam               
dishd                
dtss                 
bup                  
cldwdc               
timop                
coldbp               
cfluxa               
rvoid                
fltgap               
frpo2                
trest                
refine               
modheat              
tmpac1               
coldw                
dhe                  
explenumv            
radiation            
dhy                  
volbp                
powop                
fotmtl               
gsms                 
dishv0               
rnbnt                
zvoid2               
zvoid1               
fgrns                
rshd                 
ngastmp              
htco                 
ncs                  
scd                  
dtplta               
ProfileStartTime     
pressu               
internal             
radpel               
pow                  
azpang               
gadoln               
press                
htclev               
radtemp              
fuelrad              
ExtentOfBow          
gfrac                
FuelGasSwell         
tem                  
dtmaxa               
temptm               
res                  
fmesh                
pbh2                 
pbh1                 
pressure             
fluxz                
hlqcl                
nodchf               
AxPowProfile         
gasths               
RodAvePower          
swd                  
oxideod              
cexh2a               
pazp                 
fldrat               
nce                  
temp                 
cmesh                
butemp               
oxideid              
gasphs               
coolant              
spl                  
eppinp               
explenumt            
dtpoa                
techf                
pl                   
tblka                
relfraca             
zelev                
tschf                
gappr0               
prestm               
liquid               
vplen                
"""
data = """
        FuelRod%Variables%ntimesteps = ntimesteps
        FuelRod%Variables%naxialnodes = naxialnodes
        FuelRod%Variables%nradialnodes = nradialnodes
        FuelRod%Variables%iplant = iplant
        FuelRod%Variables%gammait = gammait
        FuelRod%Variables%maxidx = maxidx
        FuelRod%Variables%nthermex = nthermex
        FuelRod%Variables%e = e
        FuelRod%Variables%pois = pois
        FuelRod%Variables%modheat = modheat
        FuelRod%Variables%DefaultTemp = DefaultTemp
        FuelRod%Variables%dtmpcl = dtmpcl
        FuelRod%Variables%tref = tref
        FuelRod%Variables%printrodburst = printrodburst
        FuelRod%Variables%printballoon = printballoon
        FuelRod%Variables%ModHeatModel = ModHeatModel
        FuelRod%Variables%nm = nm
        FuelRod%Variables%htco = htco
        FuelRod%Variables%tem = tem
        FuelRod%Variables%pbh1 = pbh1
        FuelRod%Variables%pbh2 = pbh2
        FuelRod%Variables%hlqcl = hlqcl
        FuelRod%Variables%ts = ts
        FuelRod%Variables%gadoln = gadoln
        FuelRod%Variables%oldepeff = oldepeff
        FuelRod%Variables%oldeps = oldeps
        FuelRod%Variables%afrap = afrap
        FuelRod%Variables%amatpc = amatpc
        FuelRod%Variables%ispace = ispace
        FuelRod%Variables%emflag = emflag
        FuelRod%Variables%iok = iok
        FuelRod%Variables%iokold = iokold
        FuelRod%Variables%FrapconTemp = FrapconTemp
        FuelRod%Variables%qcold = qcold
        FuelRod%Oxidation%CladType = CladType
        FuelRod%Oxidation%ProtectiveOxide = ProtectiveOxide
        FuelRod%Oxidation%npair = npair
        FuelRod%Oxidation%nIDoxide = nIDoxide
        FuelRod%Oxidation%cexh2l = cexh2l
        FuelRod%Oxidation%explenumv = explenumv
        FuelRod%Oxidation%cexh2a = cexh2a
        FuelRod%Oxidation%deltox = deltox
        FuelRod%Oxidation%explenumt = explenumt
        FuelRod%code%ntimes = ntimes
        FuelRod%code%tEnd = tend
        FuelRod%code%gfInt = gfint
        FuelRod%code%edInt = edint
        FuelRod%code%title = title
        FuelRod%phypro%imox = imox
        FuelRod%phypro%ftmelt = ftmelt
        FuelRod%phypro%fhefus = fhefus
        FuelRod%phypro%ctmelt = ctmelt
        FuelRod%phypro%chefus = chefus
        FuelRod%phypro%fdelta = fdelta
        FuelRod%phypro%bumtp = bumtp
        FuelRod%phypro%compmt = compmt
        FuelRod%phypro%deloxy = deloxy
        FuelRod%debug%time = time
        FuelRod%debug%DebugTime = DebugTime
        FuelRod%debug%DebugTimeStop = DebugTimeStop
        FuelRod%inpdat%maxit = maxit
        FuelRod%inpdat%IndexFinTemp = IndexFinTemp
        FuelRod%inpdat%indxpr = indxpr
        FuelRod%inpdat%IndexGeom = IndexGeom
        FuelRod%inpdat%IndexThermCon = IndexThermCon
        FuelRod%inpdat%IndexThermConAdv = IndexThermConAdv
        FuelRod%inpdat%IndexBC = IndexBC
        FuelRod%inpdat%noiter = noiter
        FuelRod%inpdat%Iflago = Iflago
        FuelRod%inpdat%Iflagn = Iflagn
        FuelRod%modcom%mwork = mwork
        FuelRod%modcom%mtabl1 = mtabl1
        FuelRod%modcom%mtabl2 = mtabl2
        FuelRod%modcom%mtabl3 = mtabl3
        FuelRod%modcom%mgaspr = mgaspr
        FuelRod%modcom%trise = trise
        FuelRod%bcdcom%jchf = jchf
        FuelRod%bcdcom%mbowr = mbowr
        FuelRod%bcdcom%mgbh = mgbh
        FuelRod%bcdcom%mhbh = mhbh
        FuelRod%bcdcom%mhinta = mhinta
        FuelRod%bcdcom%mhlqc1 = mhlqc1
        FuelRod%bcdcom%mhlqcp = mhlqcp
        FuelRod%bcdcom%mhupta = mhupta
        FuelRod%bcdcom%mndchf = mndchf
        FuelRod%bcdcom%mpbh = mpbh
        FuelRod%bcdcom%mprest = mprest
        FuelRod%bcdcom%mtempt = mtempt
        FuelRod%bcdcom%nbowr = nbowr
        FuelRod%bcdcom%ncooli = ncooli
        FuelRod%bcdcom%nhtc = nhtc
        FuelRod%bcdcom%nbhtc = nbhtc
        FuelRod%bcdcom%jtr = jtr
        FuelRod%bcdcom%achn = achn
        FuelRod%bcdcom%dhe = dhe
        FuelRod%bcdcom%dhy = dhy
        FuelRod%bcdcom%jhtc = jhtc
        FuelRod%bcdcom%Radiation = Radiation
        FuelRod%bcdcom%ndchfi = ndchfi
        FuelRod%bcdcom%bowrat = bowrat
        FuelRod%bcdcom%techfi = techfi
        FuelRod%bcdcom%tschfi = tschfi
        FuelRod%bcdcom%hlqcl1 = hlqcl1
        FuelRod%dialb%fdial = fdial
        FuelRod%carcom%ierr = ierr
        FuelRod%carcom%ibnopt = ibnopt
        FuelRod%carcom%nopt = nopt
        FuelRod%carcom%nunopt = nunopt
        FuelRod%WaterProp%nt = nt
        FuelRod%WaterProp%np = np
        FuelRod%WaterProp%ns = ns
        FuelRod%WaterProp%ns2 = ns2
        FuelRod%WaterProp%klp = klp
        FuelRod%WaterProp%klp2 = klp2
        FuelRod%WaterProp%llp = llp
        FuelRod%WaterProp%nt5 = nt5
        FuelRod%WaterProp%jpl = jpl
        FuelRod%scalr%Ifaila = Ifaila
        FuelRod%scalr%iagap = iagap
        FuelRod%scalr%iagap0 = iagap0
        FuelRod%scalr%m5fc2 = m5fc2
        FuelRod%scalr%m6fc2 = m6fc2
        FuelRod%scalr%IndexFC2Print = IndexFC2Print
        FuelRod%scalr%mechan = mechan
        FuelRod%scalr%irupt = irupt
        FuelRod%scalr%irefine = irefine
        FuelRod%scalr%Ifailue = Ifailue
        FuelRod%scalr%nradq = nradq
        FuelRod%scalr%nbuq = nbuq
        FuelRod%scalr%atop = atop
        FuelRod%scalr%dcldh = dcldh
        FuelRod%scalr%delth = delth
        FuelRod%scalr%gpthk0 = gpthk0
        FuelRod%scalr%cnfsol = cnfsol
        FuelRod%scalr%cnfliq = cnfliq
        FuelRod%scalr%tmax = tmax
        FuelRod%scalr%trecrd = trecrd
        FuelRod%scalr%t0 = t0
        FuelRod%scalr%dtenfb = dtenfb
        FuelRod%scalr%dtenfo = dtenfo
        FuelRod%scalr%frcoef = frcoef
        FuelRod%scalr%ruptstrain = ruptstrain
        FuelRod%scalr%refine = refine
        FuelRod%scalr%cfluxa = cfluxa
        FuelRod%scalr%coldw = coldw
        FuelRod%scalr%dishd = dishd
        FuelRod%scalr%dishv0 = dishv0
        FuelRod%scalr%bup = bup
        FuelRod%scalr%beta1 = beta1
        FuelRod%scalr%fqcrit = fqcrit
        FuelRod%scalr%frden = frden
        FuelRod%scalr%frpo2 = frpo2
        FuelRod%scalr%drod = drod
        FuelRod%scalr%dtpo = dtpo
        FuelRod%scalr%dtss = dtss
        FuelRod%scalr%fpdcay = fpdcay
        FuelRod%scalr%TotalGasMoles = TotalGasMoles
        FuelRod%scalr%pelh = pelh
        FuelRod%scalr%pfail = pfail
        FuelRod%scalr%powop = powop
        FuelRod%scalr%PlenumGasMoles = PlenumGasMoles
        FuelRod%scalr%prsacc = prsacc
        FuelRod%scalr%zvoid1 = zvoid1
        FuelRod%scalr%rhof = rhof
        FuelRod%scalr%rl = rl
        FuelRod%scalr%rshd = rshd
        FuelRod%scalr%rhoc = rhoc
        FuelRod%scalr%rvoid = rvoid
        FuelRod%scalr%srcv2 = srcv2
        FuelRod%scalr%src1 = src1
        FuelRod%scalr%src2 = src2
        FuelRod%scalr%tdbugd = tdbugd
        FuelRod%scalr%tempcs = tempcs
        FuelRod%scalr%tflux = tflux
        FuelRod%scalr%tgas0 = tgas0
        FuelRod%scalr%timop = timop
        FuelRod%scalr%tmaxc = tmaxc
        FuelRod%scalr%tmaxf = tmaxf
        FuelRod%scalr%tmpac1 = tmpac1
        FuelRod%scalr%tpowf = tpowf
        FuelRod%scalr%trest = trest
        FuelRod%scalr%t0c = t0c
        FuelRod%scalr%t0f = t0f
        FuelRod%scalr%zvoid2 = zvoid2
        FuelRod%scalr%OpenPorosityFraction = OpenPorosityFraction
        FuelRod%scalr%spl = spl
        FuelRod%scalr%scd = scd
        FuelRod%scalr%swd = swd
        FuelRod%scalr%dcldh0 = dcldh0
        FuelRod%scalr%delth0 = delth0
        FuelRod%scalr%dtheta = dtheta
        FuelRod%scalr%dtold = dtold
        FuelRod%scalr%fr = fr
        FuelRod%scalr%frchdt = frchdt
        FuelRod%scalr%frchmx = frchmx
        FuelRod%scalr%fuelrd = fuelrd
        FuelRod%scalr%time0 = time0
        FuelRod%scalr%tpo = tpo
        FuelRod%scalr%zbot = zbot
        FuelRod%scalr%zmesh = zmesh
        FuelRod%scalr%zro = zro
        FuelRod%scalr%dppowi = dppowi
        FuelRod%scalr%powimx = powimx
        FuelRod%scalr%powict = powict
        FuelRod%scalr%aidtot = aidtot
        FuelRod%scalr%aidlng = aidlng
        FuelRod%scalr%aidsht = aidsht
        FuelRod%scalr%aidsur = aidsur
        FuelRod%scalr%relocmodel = relocmodel
        FuelRod%scalr%idumr1 = idumr1
        FuelRod%scalr%vplen = vplen
        FuelRod%scalr%apln0 = apln0
        FuelRod%scalr%bpln0 = bpln0
        FuelRod%scalr%vsn = vsn
        FuelRod%scalr%bu = bu
        FuelRod%scalr%gappr0 = gappr0
        FuelRod%scalr%scd = scd
        FuelRod%scalr%spl = spl
        FuelRod%scalr%swd = swd
        FuelRod%scalr%qpln = qpln
        FuelRod%scalr%qpln0 = qpln0
        FuelRod%scalr%flwblk = flwblk
        FuelRod%scalr%tp = tp
        FuelRod%scalr%powave = powave
        FuelRod%scalr%dvdt = dvdt
        FuelRod%scalr%apln = apln
        FuelRod%scalr%bpln = bpln
        FuelRod%scalr%dvdt0 = dvdt0
        FuelRod%scalr%hfusn = hfusn
        FuelRod%scalr%tmelt = tmelt
        FuelRod%scalr%ascal3 = ascal3
        FuelRod%scalr%ascal2 = ascal2
        FuelRod%scalr%ascal1 = ascal1
        FuelRod%scalr%gadolin = gadolin
        FuelRod%scalr%pchn = pchn
        FuelRod%scalr%burad = burad
        FuelRod%scalr%radpow = radpow
        FuelRod%scalr%radsrc = radsrc
        FuelRod%scalr%radsrco = radsrco
        FuelRod%prntb%itcntd = itcntd
        FuelRod%prntb%crfpr = crfpr
        FuelRod%prntb%zqchpr = zqchpr
        FuelRod%prntb%reflpr = reflpr
        FuelRod%prntb%fldrpr = fldrpr
        FuelRod%prntb%TimeIncrement = TimeIncrement
        FuelRod%prntb%tplenb = tplenb
        FuelRod%prntb%aprntb = aprntb
        FuelRod%presb%Kswell = Kswell
        FuelRod%presb%Ifail = Ifail
        FuelRod%presb%vplenc = vplenc
        FuelRod%presb%tplen = tplen
        FuelRod%presb%VolAveGasTemp = VolAveGasTemp
        FuelRod%presb%pctop = pctop
        FuelRod%presb%dvdtp = dvdtp
        FuelRod%presb%vplenb = vplenb
        FuelRod%presb%gfloa1 = gfloa1
        FuelRod%presb%roughf = roughf
        FuelRod%presb%roughc = roughc
        FuelRod%presb%swllfr = swllfr
        FuelRod%presb%TotalVoidVol = TotalVoidVol
        FuelRod%presb%ies = ies
        FuelRod%presb%pswll0 = pswll0
        FuelRod%presb%roi = roi
        FuelRod%presb%vs0 = vs0
        FuelRod%presb%flowg = flowg
        FuelRod%presb%GasAx = GasAx
        FuelRod%powrd%nprofile = nprofile
        FuelRod%powrd%NumAxProfiles = NumAxProfiles
        FuelRod%powrd%apowrd = apowrd
        FuelRod%powrd%ProfileStartTime = ProfileStartTime
        FuelRod%powrd%RodAvePower = RodAvePower
        FuelRod%powrd%AxPowProfile = AxPowProfile
        FuelRod%powrd%pazp = pazp
        FuelRod%numcom%idebug = idebug
        FuelRod%numcom%ncmesh = ncmesh
        FuelRod%numcom%nfmesh = nfmesh
        FuelRod%numcom%nsc = nsc
        FuelRod%numcom%nsf = nsf
        FuelRod%numcom%cmesh = cmesh
        FuelRod%numcom%fmesh = fmesh
        FuelRod%numcom%zelev = zelev
        FuelRod%htcb%kaxhtc = kaxhtc
        FuelRod%htcb%nfhtc = nfhtc
        FuelRod%htcb%nradsh = nradsh
        FuelRod%htcb%nqbow = nqbow
        FuelRod%htcb%tshtc = tshtc
        FuelRod%htcb%fhtc = fhtc
        FuelRod%htcb%rshrd = rshrd
        FuelRod%htcb%zroug1 = zroug1
        FuelRod%htcb%zroug2 = zroug2
        FuelRod%htcb%ffchf = ffchf
        FuelRod%htcb%htflxa = htflxa
        FuelRod%htcb%bowthr = bowthr
        FuelRod%HeatConduction%Nchan = Nchan
        FuelRod%HeatConduction%areao = areao
        FuelRod%HeatConduction%arean = arean
        FuelRod%HeatConduction%epsht1 = epsht1
        FuelRod%HeatConduction%GapConductivity = GapConductivity
        FuelRod%HeatConduction%BoundaryCondition = BoundaryCondition
        FuelRod%HeatConduction%ThermalConductAdv = ThermalConductAdv
        FuelRod%HeatConduction%FinalTemp = FinalTemp
        FuelRod%HeatConduction%PrevTemp = PrevTemp
        FuelRod%HeatConduction%ArrayE = ArrayE
        FuelRod%HeatConduction%ArrayF = ArrayF
        FuelRod%HeatConduction%VolumeWeightL = VolumeWeightL
        FuelRod%HeatConduction%VolumeWeightR = VolumeWeightR
        FuelRod%HeatConduction%AreaWeight = AreaWeight
        FuelRod%HeatConduction%ThermalConductivity = ThermalConductivity
        FuelRod%HeatConduction%acond = acond
        FuelRod%HeatConduction%RhoCp = RhoCp
        FuelRod%HeatConduction%RhoCp0 = RhoCp0
        FuelRod%HeatConduction%PrevIterateTemp = PrevIterateTemp
        FuelRod%FGR%TranSwell = TranSwell
        FuelRod%FGR%presfgr = presfgr
        FuelRod%FGR%nFuelSwellPairs = nFuelSwellPairs
        FuelRod%FGR%nFGRpairs = nFGRpairs
        FuelRod%FGR%GasMoles0 = GasMoles0
        FuelRod%FGR%prodxe = prodxe
        FuelRod%FGR%prodkr = prodkr
        FuelRod%FGR%relfract = relfract
        FuelRod%FGR%TranFuelSwell = TranFuelSwell
        FuelRod%FGR%relfraca = relfraca
        FuelRod%FGR%FuelGasSwell = FuelGasSwell
        FuelRod%FGR%ngasr = ngasr
        FuelRod%FGR%ansr = ansr
        FuelRod%FGR%fmgp = fmgp
        FuelRod%FGR%gasavail1 = gasavail1
        FuelRod%FGR%gasavail2 = gasavail2
        FuelRod%bloon%ifbaln = ifbaln
        FuelRod%bloon%jmnbal = jmnbal
        FuelRod%bloon%kntbal = kntbal
        FuelRod%bloon%nbncal = nbncal
        FuelRod%bloon%nodprm = nodprm
        FuelRod%bloon%kbaln = kbaln
        FuelRod%bloon%modbal = modbal
        FuelRod%bloon%nprntb = nprntb
        FuelRod%bloon%chstrs = chstrs
        FuelRod%bloon%frbal = frbal
        FuelRod%bloon%pmxbal = pmxbal
        FuelRod%bloon%r8bal = r8bal
        FuelRod%bloon%tcebal = tcebal
        FuelRod%bloon%pdrato = pdrato
        FuelRod%bloon%rnbnt = rnbnt
        FuelRod%bloon%totnb = totnb
        FuelRod%bloon%trabal = trabal
        FuelRod%bloon%taxbal = taxbal
        FuelRod%bloon%dtbal = dtbal
        FuelRod%bloon%dtobal = dtobal
        FuelRod%bloon%emwbal = emwbal
        FuelRod%bloon%fabal = fabal
        FuelRod%bloon%flxbal = flxbal
        FuelRod%bloon%htcbal = htcbal
        FuelRod%bloon%h0bal = h0bal
        FuelRod%bloon%pcbal = pcbal
        FuelRod%bloon%psbal = psbal
        FuelRod%bloon%qbal = qbal
        FuelRod%bloon%qlbal = qlbal
        FuelRod%bloon%rfbal = rfbal
        FuelRod%bloon%rmpbal = rmpbal
        FuelRod%bloon%r0bal = r0bal
        FuelRod%bloon%tbkbal = tbkbal
        FuelRod%bloon%tc0bal = tc0bal
        FuelRod%bloon%tf0bal = tf0bal
        FuelRod%bloon%tgbal = tgbal
        FuelRod%bloon%timbal = timbal
        FuelRod%bloon%tm1bal = tm1bal
        FuelRod%bloon%tp1bal = tp1bal
        FuelRod%bloon%ztmax = ztmax
        FuelRod%bloon%zm1bal = zm1bal
        FuelRod%bloon%zp1bal = zp1bal
        FuelRod%bloon%zndbal = zndbal
        FuelRod%bloon%htcgba = htcgba
        FuelRod%bloon%tfavba = tfavba
        FuelRod%bloon%zbaln = zbaln
        FuelRod%bloon%farbal = farbal
        FuelRod%bloon%sdfar = sdfar
        FuelRod%bloon%zfarbl = zfarbl
        FuelRod%bloon%ablona = ablona
        FuelRod%dyna%IndexTempConverg = IndexTempConverg
        FuelRod%dyna%RadialPower = RadialPower
        FuelRod%dyna%RadialBound = RadialBound
        FuelRod%dyna%qmaxmelt = qmaxmelt
        FuelRod%dyna%qmaxmeltp1 = qmaxmeltp1
        FuelRod%dyna%rmassflux = rmassflux
        FuelRod%dyna%coolqual = coolqual
        FuelRod%dyna%AAHT1 = AAHT1
        FuelRod%dyna%BBHT1 = BBHT1
        FuelRod%dyna%GasPress0 = GasPress0
        FuelRod%dyna%CoolPress = CoolPress
        FuelRod%dyna%GasMolesAx = GasMolesAx
        FuelRod%dyna%GasMolesAx0 = GasMolesAx0
        FuelRod%dyna%GasPress = GasPress
        FuelRod%dyna%FuelSurfT = FuelSurfT
        FuelRod%dyna%FuelCenterT = FuelCenterT
        FuelRod%dyna%CrackTemp = CrackTemp
        FuelRod%dyna%VolOpenPor = VolOpenPor
        FuelRod%dyna%GapThick = GapThick
        FuelRod%dyna%GapThick0 = GapThick0
        FuelRod%dyna%GapTemp = GapTemp
        FuelRod%dyna%PelletRad = PelletRad
        FuelRod%dyna%PelletRad0 = PelletRad0
        FuelRod%dyna%HeatFlux = HeatFlux
        FuelRod%dyna%HeatFlux0 = HeatFlux0
        FuelRod%dyna%SurfHtFlux = SurfHtFlux
        FuelRod%dyna%CladMaxT = CladMaxT
        FuelRod%dyna%Vreloc = Vreloc
        FuelRod%dyna%Vreloc0 = Vreloc0
        FuelRod%dyna%PelSrfDispl = PelSrfDispl
        FuelRod%dyna%OldPelDis = OldPelDis
        FuelRod%dyna%OldPelDis0 = OldPelDis0
        FuelRod%dyna%OxiThk1 = OxiThk1
        FuelRod%dyna%OxiThk2 = OxiThk2
        FuelRod%dyna%DishVperL = DishVperL
        FuelRod%dyna%FastFlux = FastFlux
        FuelRod%dyna%AlphaThk1 = AlphaThk1
        FuelRod%dyna%AlphaThk2 = AlphaThk2
        FuelRod%dyna%AlphaThk11 = AlphaThk11
        FuelRod%dyna%AlphaThk22 = AlphaThk22
        FuelRod%dyna%oxideid = oxideid
        FuelRod%dyna%EffStrain = EffStrain
        FuelRod%dyna%EffStrain0 = EffStrain0
        FuelRod%dyna%oxideod = oxideod
        FuelRod%dyna%CldPermStrn = CldPermStrn
        FuelRod%dyna%CldPermStrn0 = CldPermStrn0
        FuelRod%dyna%SSHgap = SSHgap
        FuelRod%dyna%OldCoolPrs = OldCoolPrs
        FuelRod%dyna%OldCoolPrs0 = OldCoolPrs0
        FuelRod%dyna%CladAveTemp = CladAveTemp
        FuelRod%dyna%OldGasPrs = OldGasPrs
        FuelRod%dyna%OldGasPrs0 = OldGasPrs0
        FuelRod%dyna%HtFlxFac = HtFlxFac
        FuelRod%dyna%OldCladT = OldCladT
        FuelRod%dyna%OldFuelAxStrn = OldFuelAxStrn
        FuelRod%dyna%RodOD = RodOD
        FuelRod%dyna%OldCldAxStrn = OldCldAxStrn
        FuelRod%dyna%OldCldAxStrn0 = OldCldAxStrn0
        FuelRod%dyna%RodOD0 = RodOD0
        FuelRod%dyna%OldCladT0 = OldCladT0
        FuelRod%dyna%OldFuelAxStrn0 = OldFuelAxStrn0
        FuelRod%dyna%TimeofGBSep = TimeofGBSep
        FuelRod%dyna%SwellDispl = SwellDispl
        FuelRod%dyna%HmaxInitial = HmaxInitial
        FuelRod%dyna%CoolEnthalpy = CoolEnthalpy
        FuelRod%dyna%RInterfacPrs = RInterfacPrs
        FuelRod%dyna%RInterfacPrs0 = RInterfacPrs0
        FuelRod%dyna%HGapAv = HGapAv
        FuelRod%dyna%FilmCoeffAv = FilmCoeffAv
        FuelRod%dyna%AxialPowr = AxialPowr
        FuelRod%dyna%AxialPowr0 = AxialPowr0
        FuelRod%dyna%CritHtFlux = CritHtFlux
        FuelRod%dyna%FuelSrfStrRat = FuelSrfStrRat
        FuelRod%dyna%WatrMetlEnrgy = WatrMetlEnrgy
        FuelRod%dyna%RInterfacGap = RInterfacGap
        FuelRod%dyna%CladSurfT = CladSurfT
        FuelRod%dyna%EDotFZ = EDotFZ
        FuelRod%dyna%PelSrfStrn0 = PelSrfStrn0
        FuelRod%dyna%FuelSrfStrRat0 = FuelSrfStrRat0
        FuelRod%dyna%EDotFZ0 = EDotFZ0
        FuelRod%dyna%EnergyPerL = EnergyPerL
        FuelRod%dyna%HFXSum = HFXSum
        FuelRod%dyna%CladEnrgPerL = CladEnrgPerL
        FuelRod%dyna%CoolEnthalpy0 = CoolEnthalpy0
        FuelRod%dyna%CoolMassFlx = CoolMassFlx
        FuelRod%dyna%Enthl = Enthl
        FuelRod%dyna%CoolDensity = CoolDensity
        FuelRod%dyna%CoolDensity0 = CoolDensity0
        FuelRod%dyna%PelRadDeviat = PelRadDeviat
        FuelRod%dyna%CldPermAxStrn = CldPermAxStrn
        FuelRod%dyna%VoidVolumeRatio = VoidVolumeRatio
        FuelRod%dyna%CldPermHoopStrn = CldPermHoopStrn
        FuelRod%dyna%ECR = ECR
        FuelRod%dyna%OxUptakeID1 = OxUptakeID1
        FuelRod%dyna%OxUpTakeID2 = OxUpTakeID2
        FuelRod%dyna%SEDPNNL = SEDPNNL
        FuelRod%dyna%EffStrainPNNL = EffStrainPNNL
        FuelRod%dyna%coefk = coefk
        FuelRod%dyna%coefn = coefn
        FuelRod%dyna%coefm = coefm
        FuelRod%dyna%Emodulus = Emodulus
        FuelRod%dyna%strainrateterm = strainrateterm
        FuelRod%dyna%SEDEPRI = SEDEPRI
        FuelRod%dyna%EffFastFluStrnHardExp = EffFastFluStrnHardExp
        FuelRod%dyna%BetaThickness = BetaThickness
        FuelRod%dyna%EffColdWkStrnHardExp = EffColdWkStrnHardExp
        FuelRod%dyna%OxygenConcenAve = OxygenConcenAve
        FuelRod%dyna%EffColdWkStrenCoef = EffColdWkStrenCoef
        FuelRod%dyna%OxygenUptake = OxygenUptake
        FuelRod%dyna%OxStAlphaThkRemain = OxStAlphaThkRemain
        FuelRod%dyna%OxStAlphaThkNearFuel = OxStAlphaThkNearFuel
        FuelRod%dyna%EffFastFluStrenCoef = EffFastFluStrenCoef
        FuelRod%dyna%OxiPowerGen = OxiPowerGen
        FuelRod%dyna%PrcntSatBeta = PrcntSatBeta
        FuelRod%dyna%OxConcenAAO = OxConcenAAO
        FuelRod%dyna%OxConcenABO = OxConcenABO
        FuelRod%dyna%OxConcenACO = OxConcenACO
        FuelRod%dyna%OxConcenADO = OxConcenADO
        FuelRod%dyna%OxConcenAEO = OxConcenAEO
        FuelRod%dyna%OxConcenAFO = OxConcenAFO
        FuelRod%dyna%OxConcenAGO = OxConcenAGO
        FuelRod%dyna%OxConcenAHO = OxConcenAHO
        FuelRod%dyna%OxConcenAIO = OxConcenAIO
        FuelRod%dyna%OxConcenAAI = OxConcenAAI
        FuelRod%dyna%OxConcenABI = OxConcenABI
        FuelRod%dyna%OxConcenACI = OxConcenACI
        FuelRod%dyna%OxConcenADI = OxConcenADI
        FuelRod%dyna%OxConcenAEI = OxConcenAEI
        FuelRod%dyna%OxConcenAFI = OxConcenAFI
        FuelRod%dyna%OxConcenAGI = OxConcenAGI
        FuelRod%dyna%OxConcenAHI = OxConcenAHI
        FuelRod%dyna%OxConcenAII = OxConcenAII
        FuelRod%dyna%CrackVolume = CrackVolume
        FuelRod%dyna%OpenPorTemp = OpenPorTemp
        FuelRod%dyna%AveDishTemp = AveDishTemp
        FuelRod%dyna%CentVoidVol = CentVoidVol
        FuelRod%dyna%ExtentOfBow = ExtentOfBow
        FuelRod%dyna%AxialNodLen = AxialNodLen
        FuelRod%dyna%TerfacePres = TerfacePres
        FuelRod%dyna%CrackWidth = CrackWidth
        FuelRod%dyna%EinstabilStrain = EinstabilStrain
        FuelRod%dyna%AxBurnup = AxBurnup
        FuelRod%dyna%BOSOxideThick = BOSOxideThick
        FuelRod%dyna%EOSOxideThick = EOSOxideThick
        FuelRod%dyna%OpenPorVol = OpenPorVol
        FuelRod%dyna%OpenPorosity = OpenPorosity
        FuelRod%dyna%CladEffStress = CladEffStress
        FuelRod%dyna%BulkCoolTemp = BulkCoolTemp
        FuelRod%dyna%CladYieldStress = CladYieldStress
        FuelRod%dyna%StressAtInstStrain = StressAtInstStrain
        FuelRod%dyna%techf = techf
        FuelRod%dyna%CesiumContent = CesiumContent
        FuelRod%dyna%HydrogenContent = HydrogenContent
        FuelRod%dyna%tschf = tschf
        FuelRod%dyna%WorkSpaceGAPI = WorkSpaceGAPI
        FuelRod%dyna%WorkSpaceEP1 = WorkSpaceEP1
        FuelRod%dyna%WorkSpaceURC = WorkSpaceURC
        FuelRod%dyna%WorkSpaceTCMx = WorkSpaceTCMx
        FuelRod%dyna%WorkSpaceGAP = WorkSpaceGAP
        FuelRod%dyna%nodchf = nodchf
        FuelRod%dyna%BOSTemp = BOSTemp
        FuelRod%dyna%EOSTemp = EOSTemp
        FuelRod%dyna%EOSRad = EOSRad
        FuelRod%dyna%BOSRad = BOSRad
        FuelRod%dyna%EnrgyMeltP1 = EnrgyMeltP1
        FuelRod%dyna%EnrgyMeltZ = EnrgyMeltZ
        FuelRod%dyna%EnrgyMelt = EnrgyMelt
        FuelRod%dyna%EnrgyMeltZp1 = EnrgyMeltZp1
        FuelRod%dyna%RadialBoundO = RadialBoundO
        FuelRod%dyna%DeformedRadiusOfMesh = DeformedRadiusOfMesh
        FuelRod%dyna%WorkSpaceEPP1 = WorkSpaceEPP1
        FuelRod%dyna%gapmin = gapmin
        FuelRod%dyna%WorkSpacePINT = WorkSpacePINT
        FuelRod%dyna%WorkSpaceRCI = WorkSpaceRCI
        FuelRod%dyna%WorkSpaceRCO = WorkSpaceRCO
        FuelRod%dyna%WorkSpaceReloc = WorkSpaceReloc
        FuelRod%dyna%AxNodElevat = AxNodElevat
        FuelRod%dyna%ureloc = ureloc
        FuelRod%dyna%a1 = a1
        FuelRod%dyna%CldElStrn = CldElStrn
        FuelRod%dyna%CldPlasStrn = CldPlasStrn
        FuelRod%dyna%CldPlasStrn0 = CldPlasStrn0
        FuelRod%dyna%CldStrn = CldStrn
        FuelRod%dyna%CldStrnRat = CldStrnRat
        FuelRod%dyna%CldStrnRat0 = CldStrnRat0
        FuelRod%dyna%HydrostatPress = HydrostatPress
        FuelRod%dyna%FuelResidStrn = FuelResidStrn
        FuelRod%dyna%FuelResidStrn0 = FuelResidStrn0
        FuelRod%dyna%Baln2Twall = Baln2Twall
        FuelRod%dyna%CldresidStrn = CldresidStrn
        FuelRod%dyna%CldResidStrn0 = CldResidStrn0
        FuelRod%dyna%IodineContent = IodineContent
        FuelRod%dyna%PelSrfStrn = PelSrfStrn
        FuelRod%dyna%CldStress = CldStress
        FuelRod%dyna%CldThermStrn = CldThermStrn
        FuelRod%dyna%GRsv = GRsv
        FuelRod%dyna%Ifchk = Ifchk
        FuelRod%dyna%nce = nce
        FuelRod%dyna%IndexPCMI = IndexPCMI
        FuelRod%dyna%IndexPCMIOnce = IndexPCMIOnce
        FuelRod%dyna%NumAzmuthNod = NumAzmuthNod
        FuelRod%dyna%Ichf = Ichf
        FuelRod%dyna%Ih = Ih
        FuelRod%dyna%Ihtreg = Ihtreg
        FuelRod%dyna%GapIndex = GapIndex
        FuelRod%dyna%BOSGapIndex = BOSGapIndex
        FuelRod%dyna%BOSGapIndex0 = BOSGapIndex0
        FuelRod%dyna%RuptFailIndex = RuptFailIndex
        FuelRod%dyna%CladCollapseIndex = CladCollapseIndex
        FuelRod%dyna%CladCollIndx0 = CladCollIndx0
        FuelRod%dyna%OldGapIndex = OldGapIndex
        FuelRod%dyna%NodeSinterTemp = NodeSinterTemp
        FuelRod%dyna%RodFailIndex = RodFailIndex
        FuelRod%collct%ntapou = ntapou
        FuelRod%collct%la1max = la1max
        FuelRod%collct%MaximumIterations = MaximumIterations
        FuelRod%collct%n1 = n1
        FuelRod%collct%la1fil = la1fil
        FuelRod%collct%ntplot = ntplot
        FuelRod%collct%ntabl1 = ntabl1
        FuelRod%collct%ntabl2 = ntabl2
        FuelRod%collct%ntabl3 = ntabl3
        FuelRod%collct%lexcb = lexcb
        FuelRod%collct%lprntb = lprntb
        FuelRod%collct%lflect = lflect
        FuelRod%collct%ldialb = ldialb
        FuelRod%collct%lhtcb = lhtcb
        FuelRod%collct%lresr1 = lresr1
        FuelRod%collct%lresr2 = lresr2
        FuelRod%collct%lsclr1 = lsclr1
        FuelRod%collct%lsclr2 = lsclr2
        FuelRod%collct%lsclr3 = lsclr3
        FuelRod%collct%lcolct = lcolct
        FuelRod%collct%lcoold = lcoold
        FuelRod%collct%lcombk = lcombk
        FuelRod%collct%lresi2 = lresi2
        FuelRod%collct%lresi3 = lresi3
        FuelRod%collct%lafrap = lafrap
        FuelRod%collct%npramp = npramp
        FuelRod%collct%itswt = itswt
        FuelRod%collct%lmatpc = lmatpc
        FuelRod%collct%nswmd = nswmd
        FuelRod%collct%n3 = n3
        FuelRod%collct%ndap1 = ndap1
        FuelRod%collct%IterationCount = IterationCount
        FuelRod%collct%ntco = ntco
        FuelRod%collct%kdbug = kdbug
        FuelRod%collct%lblona = lblona
        FuelRod%collct%lphypr = lphypr
        FuelRod%collct%lthyd = lthyd
        FuelRod%collct%prestmp = prestmp
        FuelRod%collct%dtp = dtp
        FuelRod%collct%z = z
        FuelRod%collct%gum = gum
        FuelRod%collct%Ifstor = Ifstor
        FuelRod%collct%ihData = ihData
        FuelRod%collct%icglob = icglob
        FuelRod%collct%acolct = acolct
        FuelRod%collct%gasphs = gasphs
        FuelRod%collct%gasths = gasths
        FuelRod%collct%gbse = gbse
        FuelRod%collct%pelprm = pelprm
        FuelRod%collct%pinput = pinput
        FuelRod%collct%potput = potput
        FuelRod%collct%tpln = tpln
        FuelRod%collct%tplbt1 = tplbt1
        FuelRod%excb%ndim = ndim
        FuelRod%excb%nzmesh = nzmesh
        FuelRod%excb%n2 = n2
        FuelRod%excb%n4 = n4
        FuelRod%excb%nsymm = nsymm
        FuelRod%excb%nconsw = nconsw
        FuelRod%excb%naz = naz
        FuelRod%excb%nnaz = nnaz
        FuelRod%excb%npdtpo = npdtpo
        FuelRod%excb%nbalsw = nbalsw
        FuelRod%excb%nrefld = nrefld
        FuelRod%excb%nlac = nlac
        FuelRod%excb%nprsw = nprsw
        FuelRod%excb%modkf = modkf
        FuelRod%excb%nbotpl = nbotpl
        FuelRod%excb%ncolbp = ncolbp
        FuelRod%excb%dzmesh = dzmesh
        FuelRod%excb%timmd = timmd
        FuelRod%excb%dofset = dofset
        FuelRod%excb%dofang = dofang
        FuelRod%excb%pitch = pitch
        FuelRod%excb%fgrns = fgrns
        FuelRod%excb%fotmtl = fotmtl
        FuelRod%excb%rsntr = rsntr
        FuelRod%excb%tsntrk = tsntrk
        FuelRod%excb%volbp = volbp
        FuelRod%excb%splbp = splbp
        FuelRod%excb%coldbp = coldbp
        FuelRod%excb%spdbp = spdbp
        FuelRod%excb%BottomPlenumGasMoles = BottomPlenumGasMoles
        FuelRod%excb%cldwdc = cldwdc
        FuelRod%excb%aexcb = aexcb
        FuelRod%excb%dtpoa = dtpoa
        FuelRod%excb%tplbot = tplbot
        FuelRod%Coolant%Prop = Prop
        FuelRod%Coolant%nsrad3 = nsrad3
        FuelRod%Coolant%nelrad = nelrad
        FuelRod%Coolant%nhprs = nhprs
        FuelRod%Coolant%ntprs = ntprs
        FuelRod%Coolant%nvprs = nvprs
        FuelRod%Coolant%npprs = npprs
        FuelRod%Coolant%acoold = acoold
        FuelRod%Coolant%vfrad1 = vfrad1
        FuelRod%Coolant%vfrad2 = vfrad2
        FuelRod%Coolant%vfrad3 = vfrad3
        FuelRod%Coolant%elvrad = elvrad
        FuelRod%Coolant%htclev = htclev
        FuelRod%Coolant%gbh = gbh
        FuelRod%Coolant%hbh = hbh
        FuelRod%Coolant%hinta = hinta
        FuelRod%Coolant%hupta = hupta
        FuelRod%Coolant%pbh = pbh
        FuelRod%Coolant%tshrda = tshrda
        FuelRod%Coolant%htca = htca
        FuelRod%Coolant%tblka = tblka
        FuelRod%Coolant%trad1 = trad1
        FuelRod%Coolant%trad2 = trad2
        FuelRod%Coolant%trad3 = trad3
        FuelRod%Coolant%nbrtmp = nbrtmp
        FuelRod%Coolant%nbrfdr = nbrfdr
        FuelRod%Coolant%nbrfht = nbrfht
        FuelRod%Coolant%kaxnlo = kaxnlo
        FuelRod%Coolant%liqnod = liqnod
        FuelRod%Coolant%izadfg = izadfg
        FuelRod%Coolant%irup = irup
        FuelRod%Coolant%nbrpst = nbrpst
        FuelRod%Coolant%nflec = nflec
        FuelRod%Coolant%nbrliq = nbrliq
        FuelRod%Coolant%npaxpf = npaxpf
        FuelRod%Coolant%mzq1 = mzq1
        FuelRod%Coolant%mflt = mflt
        FuelRod%Coolant%mbdl = mbdl
        FuelRod%Coolant%ntempf = ntempf
        FuelRod%Coolant%hliq = hliq
        FuelRod%Coolant%qmax = qmax
        FuelRod%Coolant%empytm = empytm
        FuelRod%Coolant%hrad = hrad
        FuelRod%Coolant%fldrte = fldrte
        FuelRod%Coolant%zqch = zqch
        FuelRod%Coolant%oldtim = oldtim
        FuelRod%Coolant%tflood = tflood
        FuelRod%Coolant%crf = crf
        FuelRod%Coolant%templo = templo
        FuelRod%Coolant%rhostm = rhostm
        FuelRod%Coolant%cpstem = cpstem
        FuelRod%Coolant%tsatt = tsatt
        FuelRod%Coolant%pressr = pressr
        FuelRod%Coolant%pressi = pressi
        FuelRod%Coolant%cpmult = cpmult
        FuelRod%Coolant%gflow = gflow
        FuelRod%Coolant%temphi = temphi
        FuelRod%Coolant%ruplev = ruplev
        FuelRod%Coolant%pavg = pavg
        FuelRod%Coolant%refdtm = refdtm
        FuelRod%Coolant%hydiam = hydiam
        FuelRod%Coolant%flxsec = flxsec
        FuelRod%Coolant%tsub = tsub
        FuelRod%Coolant%pdeint = pdeint
        FuelRod%Coolant%flowbk = flowbk
        FuelRod%Coolant%tempmx = tempmx
        FuelRod%Coolant%pfflec = pfflec
        FuelRod%Coolant%tfoldf = tfoldf
        FuelRod%Coolant%pdecy = pdecy
        FuelRod%Coolant%drflt = drflt
        FuelRod%Coolant%pfnuc = pfnuc
        FuelRod%Coolant%toldfc = toldfc
        FuelRod%Coolant%zqflt = zqflt
        FuelRod%Coolant%qaxpk = qaxpk
        FuelRod%Coolant%zpkfc = zpkfc
        FuelRod%Coolant%fltgap = fltgap
        FuelRod%Coolant%pavgft = pavgft
        FuelRod%Coolant%rcpar = rcpar
        FuelRod%Coolant%zad = zad
        FuelRod%Coolant%zs = zs
        FuelRod%Coolant%trodfc = trodfc
        FuelRod%Coolant%nu1 = nu1
        FuelRod%Coolant%nu2 = nu2
        FuelRod%Coolant%nu3 = nu3
        FuelRod%Coolant%rupflg = rupflg
        FuelRod%Coolant%lodmrk = lodmrk
        FuelRod%Coolant%flthit = flthit
        FuelRod%Coolant%faxzq = faxzq
        FuelRod%Coolant%qaxzq = qaxzq
        FuelRod%Coolant%tempfc = tempfc
        FuelRod%Coolant%aflcht = aflcht
        FuelRod%Coolant%prestm = prestm
        FuelRod%Coolant%hlqclp = hlqclp
        FuelRod%Coolant%temptm = temptm
        FuelRod%Coolant%fldrat = fldrat
        FuelRod%Coolant%nvol = nvol
        FuelRod%Coolant%ithymx = ithymx
        FuelRod%Coolant%ixazim = ixazim
        FuelRod%Coolant%ncall = ncall
        FuelRod%Coolant%tc1 = tc1
        FuelRod%Coolant%tc2 = tc2
        FuelRod%Coolant%tz2 = tz2
        FuelRod%Coolant%z1 = z1
        FuelRod%Coolant%z2 = z2
        FuelRod%Coolant%gz1 = gz1
        FuelRod%Coolant%gz2 = gz2
        FuelRod%Coolant%hz1 = hz1
        FuelRod%Coolant%hz2 = hz2
        FuelRod%Coolant%pz1 = pz1
        FuelRod%Coolant%pz2 = pz2
        FuelRod%Coolant%tz1 = tz1
        FuelRod%Coolant%aasth = aasth
        FuelRod%intcom%nepp0 = nepp0
        FuelRod%intcom%cladid = cladid
        FuelRod%intcom%cladod = cladod
        FuelRod%intcom%cladtk = cladtk
        FuelRod%intcom%rf = rf
        FuelRod%intcom%FuelPelDiam = FuelPelDiam
        FuelRod%intcom%eppinp = eppinp
        FuelRod%intcom%radpel = radpel
        FuelRod%iocom%npltn = npltn
        FuelRod%iocom%tplot = tplot
        FuelRod%iocom%dtplta = dtplta
        FuelRod%resti%indxmd = indxmd
        FuelRod%resti%kflgb = kflgb
        FuelRod%resti%ldir = ldir
        FuelRod%resti%lengmd = lengmd
        FuelRod%resti%ncladi = ncladi
        FuelRod%resti%nmesh = nmesh
        FuelRod%resti%nodpln = nodpln
        FuelRod%resti%ntstep = ntstep
        FuelRod%resti%Ifcmi = Ifcmi
        FuelRod%resti%igpnod = igpnod
        FuelRod%resti%indx = indx
        FuelRod%resti%indxkc = indxkc
        FuelRod%resti%indxkf = indxkf
        FuelRod%resti%inpfil = inpfil
        FuelRod%resti%la1req = la1req
        FuelRod%resti%lchf = lchf
        FuelRod%resti%length = length
        FuelRod%resti%lhtc = lhtc
        FuelRod%resti%luout = luout
        FuelRod%resti%modfd = modfd
        FuelRod%resti%modmw = modmw
        FuelRod%resti%mpdcay = mpdcay
        FuelRod%resti%naazp = naazp
        FuelRod%resti%naazpd = naazpd
        FuelRod%resti%naxn = naxn
        FuelRod%resti%nkf = nkf
        FuelRod%resti%nchn = nchn
        FuelRod%resti%nchfmd = nchfmd
        FuelRod%resti%ndtmax = ndtmax
        FuelRod%resti%nedtsw = nedtsw
        FuelRod%resti%nerr = nerr
        FuelRod%resti%nfastf = nfastf
        FuelRod%resti%ngbh = ngbh
        FuelRod%resti%nhbh = nhbh
        FuelRod%resti%nhinta = nhinta
        FuelRod%resti%nhtca = nhtca
        FuelRod%resti%nhtclv = nhtclv
        FuelRod%resti%nhtcz = nhtcz
        FuelRod%resti%nhupta = nhupta
        FuelRod%resti%nitmin = nitmin
        FuelRod%resti%nkc = nkc
        FuelRod%resti%npaxp = npaxp
        FuelRod%resti%npaxp1 = npaxp1
        FuelRod%resti%npbh = npbh
        FuelRod%resti%npid = npid
        FuelRod%resti%nplnt = nplnt
        FuelRod%resti%nptha = nptha
        FuelRod%resti%nptha1 = nptha1
        FuelRod%resti%nvoid = nvoid
        FuelRod%resti%ixazmn = ixazmn
        FuelRod%resti%nswinw = nswinw
        FuelRod%resti%NFrapconInitialization = NFrapconInitialization
        FuelRod%resti%nrazp = nrazp
        FuelRod%resti%nrazpd = nrazpd
        FuelRod%resti%ngaspr = ngaspr
        FuelRod%resti%ngastmp = ngastmp
        FuelRod%resti%iStoicGrad = iStoicGrad
        FuelRod%resti%itdmx = itdmx
        FuelRod%resti%kbot = kbot
        FuelRod%resti%knonue = knonue
        FuelRod%resti%IndexInitTemp = IndexInitTemp
        FuelRod%resti%nitdt = nitdt
        FuelRod%resti%NSteadyTrans = NSteadyTrans
        FuelRod%resti%numdta = numdta
        FuelRod%resti%ndtred = ndtred
        FuelRod%resti%nPelRadDeviat = nPelRadDeviat
        FuelRod%resti%nqchn = nqchn
        FuelRod%resti%xtime = xtime
        FuelRod%resti%modfal = modfal
        FuelRod%resti%nswpm = nswpm
        FuelRod%resti%irest2 = irest2
        FuelRod%resti%ncs = ncs
        FuelRod%resti%irest3 = irest3
        FuelRod%resti%indxjk = indxjk
        FuelRod%resti%nrc = nrc
        FuelRod%resti%dtmaxa = dtmaxa
        FuelRod%resti%arest1 = arest1
        FuelRod%resti%azpang = azpang
        FuelRod%resti%fluxz = fluxz
        FuelRod%resti%tplna = tplna
        FuelRod%materials%nomat = nomat
        FuelRod%materials%imaterials = imaterials
        FuelRod%materials%imatflag = imatflag
        FuelRod%materials%imatflag1 = imatflag1
        FuelRod%materials%iDatapairs = iDatapairs
        FuelRod%materials%iheattablen = iheattablen
        FuelRod%store6vars%SEDPNNLold = SEDPNNLold
        FuelRod%store6vars%oldEffStrainPNNL = oldEffStrainPNNL
        FuelRod%store6vars%SEDEPRIold = SEDEPRIold
        FuelRod%store6vars%EPStrain1old = EPStrain1old
        FuelRod%store6vars%EPStrain2old = EPStrain2old
        FuelRod%store6vars%EPStrain3old = EPStrain3old
        FuelRod%store6vars%StressHoopOld = StressHoopOld
        FuelRod%store6vars%StressRadialOld = StressRadialOld
        FuelRod%store6vars%StressAxialOld = StressAxialOld
        FuelRod%store6vars%CladEffStressOld = CladEffStressOld
        FuelRod%store6vars%hsolold = hsolold
        FuelRod%store6vars%OldCldPlasStrn = OldCldPlasStrn
        FuelRod%thcntl%ncards = ncards
        FuelRod%thcntl%NRestart = NRestart
        FuelRod%thcntl%defsize = defsize
        FuelRod%thcntl%ncool = ncool
        FuelRod%thcntl%ndtad = ndtad
        FuelRod%thcntl%t1 = t1
        FuelRod%thcntl%t2 = t2
        FuelRod%thcntl%unit = unit
        FuelRod%frapc%idx2 = idx2
        FuelRod%frapc%ncorlp = ncorlp
        FuelRod%frapc%ncirlp = ncirlp
        FuelRod%frapc%nforlp = nforlp
        FuelRod%frapc%kmxrlp = kmxrlp
        FuelRod%frapc%kmxfrp = kmxfrp
        FuelRod%frapc%ncool2 = ncool2
        FuelRod%frapc%ndtadv = ndtadv
        FuelRod%frapc%ncard2 = ncard2
        FuelRod%frapc%nrest2 = nrest2
        FuelRod%Variables%maxidx = maxidx
        FuelRod%resti%nqchn = nqchn
        FuelRod%thcntl%ncool = ncool
        FuelRod%frapc%tprlp = tprlp
        FuelRod%frapc%Allocate_Arrays_FT = Allocate_Arrays_FT
        FuelRod%frapc%convert_units = convert_units
        FuelRod%frapc%setunits = setunits
        FuelRod%frapc%first_pass = first_pass
        FuelRod%frapc%FraptranFN = FrapTranFN
        FuelRod%frapc%Iffrp = Iffrp
        FuelRod%frapc%tmprlp = tmprlp
        FuelRod%frapc%rmrlp = rmrlp
        FuelRod%frapc%hgpfrp = hgpfrp
        FuelRod%frapc%pclrlp = pclrlp
        FuelRod%frapc%ElevatThermHydr = ElevatThermHydr
        FuelRod%frapc%ElevatFrap = ElevatFrap
        FuelRod%frapc%drdfrp = drdfrp
        FuelRod%frapc%vrlfrp = vrlfrp
        FuelRod%frapc%pgpfrp = pgpfrp
        FuelRod%frapc%bufrp = bufrp
        FuelRod%frapc%kmxrlp = nvol
        FuelRod%resti%GasFraction = GasFraction
        FuelRod%FGR%gsmol0 = gsmol0
        FuelRod%Output%efuelref = efuelref
        FuelRod%Output%eout0 = eout0
        FuelRod%Output%ein0 = ein0
        FuelRod%Output%efuel0 = efuel0
        FuelRod%Output%eclad0 = eclad0
        FuelRod%Uncertainty%sigfuelthermcond = sigfuelthermcond
        FuelRod%Uncertainty%sigfuelthermexp = sigfuelthermexp
        FuelRod%Uncertainty%sigfuelheatcapa = sigfuelheatcapa
        FuelRod%Uncertainty%sigcladthermcond = sigcladthermcond
        FuelRod%Uncertainty%sigcladthermexp = sigcladthermexp
        FuelRod%Uncertainty%sigcladyieldstr = sigcladyieldstr
        FuelRod%Uncertainty%sigsurfhtc = sigsurfhtc
        FuelRod%Uncertainty%tdkt = tdkt
        FuelRod%Uncertainty%dtdkta = dtdkta
        FuelRod%Uncertainty%dktouts = dktouts
        FuelRod%Uncertainty%dakota = dakota
        FuelRod%Uncertainty%ndktn = ndktn
        FuelRod%Uncertainty%ndktparams = ndktparams
        FuelRod%Uncertainty%dktoutcounter = dktoutcounter"""


b = [x.strip().lower() for x in data0.split()]

z = []
for a in data.split('\n')[1:]:
      x,y = a.split('=')
      y = y.strip().lower()
      if y in b:
            print "{:<100} = {:<}".format(x,y)
            z.append(y)

for x in b:
      if not x in z:
            print x

exit()


data="""
set_r8_0('dtmaxa', dtmaxa(i))
set_r8_0('hbh', hbh(i))
set_r8_0('hupta', hupta(i))
set_r8_0('hinta', hinta(i))
set_r8_0('gbh', gbh(i))
set_r8_0('explenumt', explenumt(i))
set_r8_0('pbh2', pbh2(i))
set_r8_0('dtpoa', dtpoa(i))
set_r8_0('RodAvePower', RodAvePower(i))
set_r8_1('htca', htca(i,:))
set_r8_1('tblka', tblka(i,:))
set_r8_1('RadPowProfile', RadPowProfile(i,:))
set_r8_0('dtplta', dtplta(i))
set_r8_1('gasths', gasths(i,:))
set_r8_0('FuelGasSwell', FuelGasSwell(i))
set_r8_0('temptm', temptm(i))
set_r8_0('relfraca', relfraca(i))
set_r8_0('prestm', prestm(i))
set_r8_0('fldrat', fldrat(i))
set_r8_0('gasphs', gasphs(i))
set_r8_1('AxPowProfile', AxPowProfile(i,:))
set_r8_0('ProfileStartTime', ProfileStartTime(i))
set_r8_0('pbh1', pbh1(i))
set_r8_0('hlqcl', hlqcl(i))
"""




data = [a.split(',')[0].replace("'",'').split('(') for a in data.split('\n')[1:-1]]

notinlist = []
for x,y in data:
      if x == 'set_r8_1':
            print """case("%s")\n    this %% dftran %% %s(:) = var(:)"""%(y,y)
else:
      notinlist.append(x)

print notinlist


exit()

data0="""
coolant
heat
reflood
internal
metal
deformation
heat
inst
geomet   nvol1    lowpl    pressu   massfl   coreav   chf      filmbo   coldwa   axpow    bowing   spefbz   geometry nbundl   refloodtiradiat   ruptur   liquid   inlet    reflo    pressure collaps  frapt4   geom     temp     tape2    nvol2    press    zone     upppl    jfb      nucbo    unitin   unitout  res      pow      gasflo   idoxid   cathca   baker    noball   cenvoi   soltyp   
"""

data1 = """
    character(len=3)                  , pointer :: reflood              
    character(len=12)                 , pointer :: relocmodel           
    character(len=3)                  , pointer :: deformation          
    character(len=10)                 , pointer :: inst                 
    character(len=3)                  , pointer :: metal                
    character(len=3)                  , pointer :: mheat                
    character(len=3)                  , pointer :: bheat                
    character(len=3)                  , pointer :: coolant              
    character(len=3)                  , pointer :: internal             
    character(len=3)                  , pointer :: radiation            
    integer(ipk)                      , pointer :: reflo                
    integer(ipk)                      , pointer :: naxn                 
    integer(ipk)                      , pointer :: cathca               
    integer(ipk)                      , pointer :: prestmp              
    integer                           , pointer :: lowpl                
    integer(ipk)                      , pointer :: nbhtc                
    integer(ipk)                      , pointer :: inp                  
    integer(ipk)                      , pointer :: rtheta               
    integer(ipk)                      , pointer :: geom                 
    integer(ipk)                      , pointer :: iStoicGrad           
    integer                           , pointer :: nvol1                
    integer(ipk)                      , pointer :: jchf                 
    integer(ipk)                      , pointer :: gasflo               
    integer(ipk)                      , pointer :: noball               
    integer(ipk)                      , pointer :: prescri              
    integer(ipk)                      , pointer :: mechan               
    integer(ipk)                      , pointer :: nfmesh               
    integer(ipk)                      , pointer :: NumAxProfiles        
    integer(ipk)                      , pointer :: azang                
    integer(ipk)                      , pointer :: idoxid               
    integer(ipk)                      , pointer :: tape1                
    integer(ipk)                      , pointer :: jtr                  
    integer(ipk)                      , pointer :: NRestart             
    integer(ipk)                      , pointer :: ncards               
    integer(ipk)                      , pointer :: CladType             
    integer                           , pointer :: massfl               
    integer(ipk)                      , pointer :: odoxid               
    integer(ipk)                      , pointer :: nchn                 
    integer(ipk)                      , pointer :: TranSwell            
    integer(ipk)                      , pointer :: unitin               
    integer(ipk)                      , pointer :: geometry             
    integer                           , pointer :: filmbo               
    integer(ipk)                      , pointer :: noiter               
    integer(ipk)                      , pointer :: nthermex             
    integer(ipk)                      , pointer :: ncmesh               
    integer(ipk)                      , pointer :: IndexFC2Print        
    integer(ipk)                      , pointer :: chf                  
    integer(ipk)                      , pointer :: irefine              
    integer(ipk)                      , pointer :: ProtectiveOxide      
    integer(ipk)                      , pointer :: presfgr              
    integer                           , pointer :: coreav               
    integer(ipk)                      , pointer :: cenvoi               
    integer(ipk)                      , pointer :: grass                
    integer(ipk)                      , pointer :: baker                
    integer(ipk)                      , pointer :: ncolbp               
    integer(ipk)                      , pointer :: geomet               
    integer(ipk)                      , pointer :: soltyp               
    integer(ipk)                      , pointer :: naz                  
    integer(ipk)                      , pointer :: profile              
    integer(ipk)                      , pointer :: nsym                 
    integer(ipk)                      , pointer :: unitout              
    integer(ipk)                      , pointer :: tape2                
    integer(ipk)                      , pointer :: nvol2                
    integer(ipk)                      , pointer :: zone                 
    integer(ipk)                      , pointer :: upppl                
    integer(ipk)                      , pointer :: jfb                  
    integer(ipk)                      , pointer :: nucbo                
    integer(ipk)                      , pointer :: plenumtemp           
    integer(ipk)                      , pointer :: liquid               
    integer(ipk)                      , pointer :: inlet                
    integer(ipk)                      , pointer :: radiat               
    integer(ipk)                      , pointer :: axpow                
    integer(ipk)                      , pointer :: bowing               
    integer(ipk)                      , pointer :: spefbz               
    integer(ipk)                      , pointer :: nbundl               
    integer(ipk)                      , pointer :: refloodtime          
    integer(ipk)                      , pointer :: collaps              
    integer(ipk)                      , pointer :: frapt4               
    integer(ipk)                      , pointer :: maxit                
    integer(ipk)                      , pointer :: irupt                
    integer(ipk)                      , pointer :: nIDoxide             
    integer(ipk)                      , pointer :: IndexGrainBndSep     
    integer(ipk)                      , pointer :: coldwa               
    integer(ipk)                      , pointer :: ruptur               
    integer(ipk)                      , pointer :: pressu               
    integer(ipk)                      , pointer :: pow                  
    integer(ipk)                      , pointer :: press                
    integer(ipk)                      , pointer :: res                  
    integer(ipk)                      , pointer :: pressure             
    integer(ipk)                      , pointer :: nce                  
    integer(ipk)                      , pointer :: temp                 
    real(r8k)                         , pointer :: pl                   
    real(r8k)                         , pointer :: epsht1               
    real(r8k)                         , pointer :: ph                   
    real(r8k)                         , pointer :: buoxide              
    real(r8k)                         , pointer :: splbp                
    real(r8k)                         , pointer :: tpowf                
    real(r8k)                         , pointer :: ruptstrain           
    real(r8k)                         , pointer :: frcoef               
    real(r8k)                         , pointer :: CladPower            
    real(r8k)                         , pointer :: pitch                
    real(r8k)                         , pointer :: bowthr               
    real(r8k)                         , pointer :: hrad                 
    real(r8k)                         , pointer :: RodDiameter          
    real(r8k)                         , pointer :: refdtm               
    real(r8k)                         , pointer :: totnb                
    real(r8k)                         , pointer :: FuelPelDiam          
    real(r8k)                         , pointer :: gapthk               
    real(r8k)                         , pointer :: flxsec               
    real(r8k)                         , pointer :: ffch                 
    real(r8k)                         , pointer :: roughc               
    real(r8k)                         , pointer :: roughf               
    real(r8k)                         , pointer :: prsacc               
    real(r8k)                         , pointer :: fpowr                
    real(r8k)                         , pointer :: dofset               
    real(r8k)                         , pointer :: zs                   
    real(r8k)                         , pointer :: spdbp                
    real(r8k)                         , pointer :: pelh                 
    real(r8k)                         , pointer :: pdrato               
    real(r8k)                         , pointer :: dofang               
    real(r8k)                         , pointer :: tgas0                
    real(r8k)                         , pointer :: tsntrk               
    real(r8k)                         , pointer :: achn                 
    real(r8k)                         , pointer :: doffst               
    real(r8k)                         , pointer :: RodLength            
    real(r8k)                         , pointer :: OpenPorosityFraction 
    real(r8k)                         , pointer :: zad                  
    real(r8k)                         , pointer :: rshrd                
    real(r8k)                         , pointer :: tref                 
    real(r8k)                         , pointer :: tflux                
    real(r8k)                         , pointer :: emptm                
    real(r8k)                         , pointer :: trise                
    real(r8k)                         , pointer :: fltgap2              
    real(r8k)                         , pointer :: hydiam               
    real(r8k)                         , pointer :: dishd                
    real(r8k)                         , pointer :: dtss                 
    real(r8k)                         , pointer :: bup                  
    real(r8k)                         , pointer :: cldwdc               
    real(r8k)                         , pointer :: timop                
    real(r8k)                         , pointer :: coldbp               
    real(r8k)                         , pointer :: cfluxa               
    real(r8k)                         , pointer :: rvoid                
    real(r8k)                         , pointer :: fltgap               
    real(r8k)                         , pointer :: frpo2                
    real(r8k)                         , pointer :: trest                
    real(r8k)                         , pointer :: refine               
    real(r8k)                         , pointer :: modheat              
    real(r8k)                         , pointer :: tmpac1               
    real(r8k)                         , pointer :: coldw                
    real(r8k)                         , pointer :: dhe                  
    real(r8k)                         , pointer :: explenumv            
    real(r8k)                         , pointer :: dhy                  
    real(r8k)                         , pointer :: volbp                
    real(r8k)                         , pointer :: powop                
    real(r8k)                         , pointer :: fotmtl               
    real(r8k)                         , pointer :: gsms                 
    real(r8k)                         , pointer :: dishv0               
    real(r8k)                         , pointer :: rnbnt                
    real(r8k)                         , pointer :: zvoid2               
    real(r8k)                         , pointer :: zvoid1               
    real(r8k)                         , pointer :: fgrns                
    real(r8k)                         , pointer :: rshd                 
    real(r8k)                         , pointer :: gasmoles0            
    real(r8k)                         , pointer :: frden                
    real(r8k)                         , pointer :: fpdcay               
    integer(ipk) , dimension(:)       , pointer :: tem                  
    integer(ipk) , dimension(:)       , pointer :: ngastmp              
    integer(ipk) , dimension(:)       , pointer :: htco                 
    integer(ipk) , dimension(:)       , pointer :: ncs                  
    real(r8k)    , dimension(:)       , pointer :: hupta                
    real(r8k)    , dimension(:)       , pointer :: hinta                
    real(r8k)    , dimension(:)       , pointer :: hbh                  
    real(r8k)    , dimension(:)       , pointer :: gbh                  
    real(r8k)    , dimension(:,:)     , pointer :: htca                 
    real(r8k)    , dimension(:)       , pointer :: gbse                 
    real(r8k)    , dimension(:)       , pointer :: scd                  
    real(r8k)    , dimension(:)       , pointer :: dtplta               
    real(r8k)    , dimension(:)       , pointer :: ProfileStartTime     
    real(r8k)    , dimension(:)       , pointer :: radpel               
    real(r8k)    , dimension(:)       , pointer :: azpang               
    real(r8k)    , dimension(:)       , pointer :: htclev               
    real(r8k)    , dimension(:)       , pointer :: dtmaxa               
    real(r8k)    , dimension(:)       , pointer :: gadoln               
    real(r8k)    , dimension(:)       , pointer :: RadPowProfile        
    real(r8k)    , dimension(:)       , pointer :: ExtentOfBow          
    real(r8k)    , dimension(:)       , pointer :: gfrac                
    real(r8k)    , dimension(:)       , pointer :: FuelGasSwell         
    real(r8k)    , dimension(:)       , pointer :: temptm               
    real(r8k)    , dimension(:)       , pointer :: fmesh                
    real(r8k)    , dimension(:)       , pointer :: pbh2                 
    real(r8k)    , dimension(:)       , pointer :: pbh1                 
    real(r8k)    , dimension(:)       , pointer :: fluxz                
    real(r8k)    , dimension(:)       , pointer :: hlqcl                
    real(r8k)    , dimension(:)       , pointer :: nodchf               
    real(r8k)    , dimension(:,:)     , pointer :: AxPowProfile         
    real(r8k)    , dimension(:,:)     , pointer :: gasths               
    real(r8k)    , dimension(:)       , pointer :: RodAvePower          
    real(r8k)    , dimension(:)       , pointer :: swd                  
    real(r8k)    , dimension(:)       , pointer :: oxideod              
    real(r8k)    , dimension(:)       , pointer :: cexh2a               
    real(r8k)    , dimension(:,:)     , pointer :: pazp                 
    real(r8k)    , dimension(:)       , pointer :: fldrat               
    real(r8k)    , dimension(:)       , pointer :: cmesh                
    real(r8k)    , dimension(:)       , pointer :: butemp               
    real(r8k)    , dimension(:)       , pointer :: oxideid              
    real(r8k)    , dimension(:)       , pointer :: gasphs               
    real(r8k)    , dimension(:)       , pointer :: spl                  
    real(r8k)    , dimension(:)       , pointer :: eppinp               
    real(r8k)    , dimension(:)       , pointer :: explenumt            
    real(r8k)    , dimension(:)       , pointer :: dtpoa                
    real(r8k)    , dimension(:)       , pointer :: techf                
    real(r8k)    , dimension(:,:)     , pointer :: tblka                
    real(r8k)    , dimension(:)       , pointer :: relfraca             
    real(r8k)    , dimension(:)       , pointer :: zelev                
    real(r8k)    , dimension(:)       , pointer :: tschf                
    real(r8k)    , dimension(:)       , pointer :: gappr0               
    real(r8k)    , dimension(:)       , pointer :: prestm               
    real(r8k)    , dimension(:)       , pointer :: vplen                
"""

data = """
NRestart, ncards, IndexFC2Print, IndexGrainBndSep, ProblemEndTime, 
ProblemStartTime, gbse,
coolant, reflood, radiation, heat, jchf, jfb, upppl, hupta, zad, zs, 
fltgap, fltgap2, geomet, tape1, nvol1, nchn, lowpl, pressu, massfl, 
  coreav, chf, filmbo, coldwa, axpow, bowing, spefbz, geometry, nbundl, 
  time, radiat, ruptur, liquid, inlet, reflo, pressure, collaps, frapt4, 
  geom, temp, tape2,nvol2, press, zone, htco, nodchf, tem, dhe, dhy, achn, 
  hinta, pbh1, gbh, hbh, ffch, bowthr, ExtentOfBow, tschf, techf, hydiam, 
  flxsec, emptm, refdtm, hrad, temptm, fldrat, prestm, hlqcl, rshrd, ts, 
  pbh2, htclev, htca, tblka, nucbo, nbhtc, jtr,
 pitch, pdrato, rnbnt, CladType, RodLength, RodDiameter, dishd, pelh, dishv0, 
 FuelPelDiam, roughf, frden, bup, rshd, frpo2, fotmtl, tsntrk, fgrns, gadoln, 
 gapthk, coldw, roughc, cfluxa, tflux, cldwdc, spl, scd, swd, vplen, splbp, 
 coldbp, spdbp, volbp, gfrac, gsms, gappr0, tgas0, fluxz, radpel, eppinp, 
 totnb, ncs, ncolbp, OpenPorosityFraction,
 unitin, unitout, trest, inp, res, pow, dtpoa, dtplta,
internal, metal, deformation, heat, inst, nsym, naz, gasphs, oxideid, 
oxideod, zvoid1, zvoid2, rvoid, dofset, dofang, cexh2a, gasflo, grass, prescri, 
idoxid, odoxid, cathca, baker, noball, cenvoi, rtheta, presfgr, relfraca, tref, 
TranSwell, FuelGasSwell, PlenumTemp, nthermex, ProtectiveOxide, frcoef, mechan, 
irupt, ruptstrain, irefine, refine, nIDoxide, BuOxide, explenumv, explenumt, 
iStoicGrad, prestmp, gasths, ngastmp, trise, relocmodel,
 dtmaxa, dtss, prsacc, tmpac1, soltyp, maxit, noiter, epsht1, 
   naxn, zelev, nfmesh, ncmesh, fmesh, cmesh, nce,
RodAvePower, AxPowProfile, RadPowProfile, butemp, azpang, pazp, ph, pl, 
doffst, fpowr, powop, tpowf, timop, fpdcay, CladPower, azang, profile, 
NumAxProfiles, ProfileStartTime, modheat
"""

a = set([a.replace(',', '') for a in data.split()])
b = set(data0.split())
c = a.difference(b)
d = data1.split('::')

d = {}
for x in data1.split('\n')[1:-1]:
      tp, name = x.split('::')
      name = name.strip()
      if not 'dimension' in tp:
            if 'integer' in tp:
                  d[name] = 'i4_0'
            elif 'real' in tp:
                  d[name] = 'r8_0'
            elif 'character' in tp:
                  d[name] = 'ch_0'
            else:
                  print "unknown type"
      else:
            if 'integer' in tp:
                  d[name] = 'i4_1'
            elif 'real' in tp:
                  d[name] = 'r8_1'
            elif 'character' in tp:
                  d[name] = 'ch_1'
            else:
                  print "unknown type"

notinlist = []
for x in c:
      if x in d:
            if d[x] == 'r8_1':
                  print """case("%s")\n    this %% dftran %% %s = var"""%(x,x)
            #print "call frod  set_%s('%s', %s)"%(d[x],x,x)
      else:
            notinlist.append(x)

print notinlist
