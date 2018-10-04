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
