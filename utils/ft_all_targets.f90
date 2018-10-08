logical                                                                :: convert_units 
logical                                                                :: printballoon 
logical                                                                :: printrodburst 
logical                                                                :: allocate_arrays_ft 
logical                                                                :: coupled 
logical                                                                :: ndebug 
logical                                                                :: first_pass 
logical                                                                :: setunits 
logical                                                                :: is_export 
logical                                                                :: first_call 
logical                                                                :: unit 
character(len=3)                                                       :: coolant 
character(len=80)                                                      :: fraptranfn 
character(len=3)                                                       :: metal 
character(len=15)                                                      :: modheatmodel 
character(len=3)                                                       :: deformation 
character(len=12)                                                      :: relocmodel 
character(len=3)                                                       :: reflood 
character(len=200)                                                     :: namerf 
character(len=10)                                                      :: inst 
character(len=3)                                                       :: bheat 
character(len=3)                                                       :: mheat 
character(len=3)                                                       :: radiation 
character(len=3)                                                       :: internal 
integer(4)                                                             :: ndim 
integer(4)                                                             :: ncirlp 
integer(4)                                                             :: numaxprofiles 
integer(4)                                                             :: nprsw 
integer(4)                                                             :: npbh 
integer(4)                                                             :: nbncal 
integer(4)                                                             :: llp 
integer(4)                                                             :: indxkf 
integer(4)                                                             :: ierr 
integer(4)                                                             :: lcoold 
integer(4)                                                             :: nidoxide 
integer(4)                                                             :: press 
integer(4)                                                             :: cathca 
integer(4)                                                             :: klp2 
integer(4)                                                             :: mgaspr 
integer(4)                                                             :: frapt4 
integer(4)                                                             :: ntapou 
integer(4)                                                             :: kaxhtc 
integer(4)                                                             :: indexthermcon 
integer(4)                                                             :: prestmp 
integer(4)                                                             :: irupt 
integer(4)                                                             :: coldwa 
integer(4)                                                             :: nbowr 
integer(4)                                                             :: ldialb 
integer(4)                                                             :: modbal 
integer(4)                                                             :: pressu 
integer(4)                                                             :: inlet 
integer(4)                                                             :: zone 
integer(4)                                                             :: ndap1 
integer(4)                                                             :: nbhtc 
integer(4)                                                             :: protectiveoxide 
integer(4)                                                             :: rtheta 
integer(4)                                                             :: ldir 
integer(4)                                                             :: istoicgrad 
integer(4)                                                             :: ifail 
integer(4)                                                             :: lchf 
integer(4)                                                             :: itswt 
integer(4)                                                             :: pow 
integer(4)                                                             :: nsymm 
integer(4)                                                             :: naazp 
integer(4)                                                             :: nvol2 
integer(4)                                                             :: idebug 
integer(4)                                                             :: nvol1 
integer(4)                                                             :: mhbh 
integer(4)                                                             :: modfd 
integer(4)                                                             :: jchf 
integer(4)                                                             :: ixazmn 
integer(4)                                                             :: gasflo 
integer(4)                                                             :: noball 
integer(4)                                                             :: prescri 
integer(4)                                                             :: mbowr 
integer(4)                                                             :: mtabl3 
integer(4)                                                             :: mechan 
integer(4)                                                             :: nfmesh 
integer(4)                                                             :: mndchf 
integer(4)                                                             :: naazpd 
integer(4)                                                             :: itdmx 
integer(4)                                                             :: nhbh 
integer(4)                                                             :: nfrapconinitialization 
integer(4)                                                             :: ncool 
integer(4)                                                             :: npair 
integer(4)                                                             :: indexgrainbndsep 
integer(4)                                                             :: nedtsw 
integer(4)                                                             :: azang 
integer(4)                                                             :: m5fc2 
integer(4)                                                             :: radiat 
integer(4)                                                             :: mhinta 
integer(4)                                                             :: lcombk 
integer(4)                                                             :: nmesh 
integer(4)                                                             :: iagap 
integer(4)                                                             :: upppl 
integer(4)                                                             :: nitmin 
integer(4)                                                             :: nconsw 
integer(4)                                                             :: numdta 
integer(4)                                                             :: ncard2 
integer(4)                                                             :: nrazp 
integer(4)                                                             :: iplant 
integer(4)                                                             :: indexthermconadv 
integer(4)                                                             :: mhlqc1 
integer(4)                                                             :: nplnt 
integer(4)                                                             :: nswmd 
integer(4)                                                             :: idoxid 
integer(4)                                                             :: nt5 
integer(4)                                                             :: indexgeom 
integer(4)                                                             :: kbaln 
integer(4)                                                             :: nbuq 
integer(4)                                                             :: mtempt 
integer(4)                                                             :: maxit 
integer(4)                                                             :: nbundl 
integer(4)                                                             :: reflo 
integer(4)                                                             :: nlac 
integer(4)                                                             :: nhupta 
integer(4)                                                             :: ntco 
integer(4)                                                             :: kflgb 
integer(4)                                                             :: tape1 
integer(4)                                                             :: lengmd 
integer(4)                                                             :: tape2 
integer(4)                                                             :: nitdt 
integer(4)                                                             :: res 
integer(4)                                                             :: ngaspr 
integer(4)                                                             :: jtr 
integer(4)                                                             :: nqbow 
integer(4)                                                             :: geom 
integer(4)                                                             :: luout 
integer(4)                                                             :: mhlqcp 
integer(4)                                                             :: lowpl 
integer(4)                                                             :: la1fil 
integer(4)                                                             :: ifcmi 
integer(4)                                                             :: nhtcz 
integer(4)                                                             :: indxmd 
integer(4)                                                             :: npdtpo 
integer(4)                                                             :: ncards 
integer(4)                                                             :: massfl 
integer(4)                                                             :: iterationcount 
integer(4)                                                             :: itcntd 
integer(4)                                                             :: iagap0 
integer(4)                                                             :: odoxid 
integer(4)                                                             :: ntstep 
integer(4)                                                             :: kswell 
integer(4)                                                             :: pressure 
integer(4)                                                             :: mhupta 
integer(4)                                                             :: indxkc 
integer(4)                                                             :: nptha 
integer(4)                                                             :: ncorlp 
integer(4)                                                             :: bowing 
integer(4)                                                             :: transwell 
integer(4)                                                             :: unitin 
integer(4)                                                             :: indexfc2print 
integer(4)                                                             :: nepp0 
integer(4)                                                             :: ntimesteps 
integer(4)                                                             :: naxialnodes 
integer(4)                                                             :: nradialnodes 
integer(4)                                                             :: idx 
integer(4)                                                             :: geometry 
integer(4)                                                             :: jfb 
integer(4)                                                             :: ruptur 
integer(4)                                                             :: geomet 
integer(4)                                                             :: nradq 
integer(4)                                                             :: lcolct 
integer(4)                                                             :: noiter 
integer(4)                                                             :: length 
integer(4)                                                             :: nchn 
integer(4)                                                             :: lresr2 
integer(4)                                                             :: ntimes 
integer(4)                                                             :: nfhtc 
integer(4)                                                             :: lresr1 
integer(4)                                                             :: mpbh 
integer(4)                                                             :: ncmesh 
integer(4)                                                             :: nsc 
integer(4)                                                             :: nprofile 
integer(4)                                                             :: ifaila 
integer(4)                                                             :: naxn 
integer(4)                                                             :: lflect 
integer(4)                                                             :: ns 
integer(4)                                                             :: nradsh 
integer(4)                                                             :: lprntb 
integer(4)                                                             :: mprest 
integer(4)                                                             :: nsf 
integer(4)                                                             :: ncooli 
integer(4)                                                             :: m6fc2 
integer(4)                                                             :: indxpr 
integer(4)                                                             :: jmnbal 
integer(4)                                                             :: idx2 
integer(4)                                                             :: kntbal 
integer(4)                                                             :: nprad 
integer(4)                                                             :: nhtca 
integer(4)                                                             :: knonue 
integer(4)                                                             :: chf 
integer(4)                                                             :: irefine 
integer(4)                                                             :: mgbh 
integer(4)                                                             :: lsclr2 
integer(4)                                                             :: lsclr3 
integer(4)                                                             :: lsclr1 
integer(4)                                                             :: lblona 
integer(4)                                                             :: nodpln 
integer(4)                                                             :: nrestart 
integer(4)                                                             :: lafrap 
integer(4)                                                             :: coreav 
integer(4)                                                             :: nvoid 
integer(4)                                                             :: lmatpc 
integer(4)                                                             :: ndtred 
integer(4)                                                             :: nkc 
integer(4)                                                             :: nomat 
integer(4)                                                             :: nkf 
integer(4)                                                             :: nhtclv 
integer(4)                                                             :: npaxp 
integer(4)                                                             :: ncool2 
integer(4)                                                             :: kmxrlp 
integer(4)                                                             :: kmxfrp 
integer(4)                                                             :: indexbc 
integer(4)                                                             :: ndtmax 
integer(4)                                                             :: naz 
integer(4)                                                             :: soltyp 
integer(4)                                                             :: nptha1 
integer(4)                                                             :: ntplot 
integer(4)                                                             :: ns2 
integer(4)                                                             :: nce 
integer(4)                                                             :: modkf 
integer(4)                                                             :: mtabl1 
integer(4)                                                             :: mtabl2 
integer(4)                                                             :: nbalsw 
integer(4)                                                             :: temp 
integer(4)                                                             :: indexinittemp 
integer(4)                                                             :: cenvoi 
integer(4)                                                             :: nrefld 
integer(4)                                                             :: ndtad 
integer(4)                                                             :: nodprm 
integer(4)                                                             :: la1max 
integer(4)                                                             :: klp 
integer(4)                                                             :: npid 
integer(4)                                                             :: nforlp 
integer(4)                                                             :: nswinw 
integer(4)                                                             :: grass 
integer(4)                                                             :: mpdcay 
integer(4)                                                             :: baker 
integer(4)                                                             :: spefbz 
integer(4)                                                             :: ncolbp 
integer(4)                                                             :: mwork 
integer(4)                                                             :: jpl 
integer(4)                                                             :: presfgr 
integer(4)                                                             :: nerr 
integer(4)                                                             :: inp 
integer(4)                                                             :: iflagn 
integer(4)                                                             :: nhinta 
integer(4)                                                             :: lthyd 
integer(4)                                                             :: indexfintemp 
integer(4)                                                             :: nnaz 
integer(4)                                                             :: npaxp1 
integer(4)                                                             :: nrazpd 
integer(4)                                                             :: maximumiterations 
integer(4)                                                             :: npramp 
integer(4)                                                             :: nzmesh 
integer(4)                                                             :: igpnod 
integer(4)                                                             :: unitout 
integer(4)                                                             :: ngbh 
integer(4)                                                             :: lphypr 
integer(4)                                                             :: ifbaln 
integer(4)                                                             :: refloodtime 
integer(4)                                                             :: nrest2 
integer(4)                                                             :: nchfmd 
integer(4)                                                             :: nfastf 
integer(4)                                                             :: nthermex 
integer(4)                                                             :: lhtc 
integer(4)                                                             :: np 
integer(4)                                                             :: imox 
integer(4)                                                             :: nt 
integer(4)                                                             :: maxidx 
integer(4)                                                             :: lhtcb 
integer(4)                                                             :: collaps 
integer(4)                                                             :: profile 
integer(4)                                                             :: nucbo 
integer(4)                                                             :: filmbo 
integer(4)                                                             :: cladtype 
integer(4)                                                             :: ndtadv 
integer(4)                                                             :: ncladi 
integer(4)                                                             :: defsize 
integer(4)                                                             :: nhtc 
integer(4)                                                             :: npelraddeviat 
integer(4)                                                             :: ifailue 
integer(4)                                                             :: axpow 
integer(4)                                                             :: n1 
integer(4)                                                             :: nsteadytrans 
integer(4)                                                             :: modmw 
integer(4)                                                             :: nbotpl 
integer(4)                                                             :: nsym 
integer(4)                                                             :: la1req 
integer(4)                                                             :: kbot 
integer(4)                                                             :: n3 
integer(4)                                                             :: lresi3 
integer(4)                                                             :: lresi2 
integer(4)                                                             :: liquid 
integer(4)                                                             :: ntabl1 
integer(4)                                                             :: ntabl3 
integer(4)                                                             :: ntabl2 
integer(4)                                                             :: iflago 
integer(4)                                                             :: indx 
integer(4)                                                             :: npltn 
integer(4)                                                             :: nqchn 
integer(4)                                                             :: inpfil 
integer(4)                                                             :: plenumtemp 
integer(4)                                                             :: lexcb 
integer(4)                                                             :: kdbug 
integer(4)                                                             :: n2 
integer(4)                                                             :: gammait 
integer(4)                                                             :: n4 
integer(4)                                                             :: nprntb 
real(8)                                                                :: splbp 
real(8)                                                                :: tpowf 
real(8)                                                                :: zbaln 
real(8)                                                                :: dtpo 
real(8)                                                                :: dtheta 
real(8)                                                                :: ruptstrain 
real(8)                                                                :: tplenb 
real(8)                                                                :: defaulttemp 
real(8)                                                                :: pitch 
real(8)                                                                :: dcldh 
real(8)                                                                :: cnfsol 
real(8)                                                                :: gasmoles0 
real(8)                                                                :: bowthr 
real(8)                                                                :: hrad 
real(8)                                                                :: frcoef 
real(8)                                                                :: t0f 
real(8)                                                                :: t0c 
real(8)                                                                :: maxenthalpy 
real(8)                                                                :: tp1bal 
real(8)                                                                :: tmaxf 
real(8)                                                                :: rmpbal 
real(8)                                                                :: plenumgasmoles 
real(8)                                                                :: drod 
real(8)                                                                :: dtbal 
real(8)                                                                :: delth 
real(8)                                                                :: tshtc 
real(8)                                                                :: cexh2l 
real(8)                                                                :: frchmx 
real(8)                                                                :: htcgba 
real(8)                                                                :: frbal 
real(8)                                                                :: reflpr 
real(8)                                                                :: qbal 
real(8)                                                                :: pelh 
real(8)                                                                :: t12 
real(8)                                                                :: debugtime 
real(8)                                                                :: aidsht 
real(8)                                                                :: zro 
real(8)                                                                :: refdtm 
real(8)                                                                :: fdelta 
real(8)                                                                :: totnb 
real(8)                                                                :: qlbal 
real(8)                                                                :: timop 
real(8)                                                                :: src2 
real(8)                                                                :: htcbal 
real(8)                                                                :: powop 
real(8)                                                                :: tgas0 
real(8)                                                                :: gapthk 
real(8)                                                                :: t2 
real(8)                                                                :: t0 
real(8)                                                                :: t1 
real(8)                                                                :: ffch 
real(8)                                                                :: dtenfb 
real(8)                                                                :: fpdcay 
real(8)                                                                :: debugtimestop 
real(8)                                                                :: roughc 
real(8)                                                                :: chstrs 
real(8)                                                                :: roughf 
real(8)                                                                :: dtenfo 
real(8)                                                                :: dppowi 
real(8)                                                                :: flxsec 
real(8)                                                                :: jhtc 
real(8)                                                                :: rfbal 
real(8)                                                                :: fr 
real(8)                                                                :: prsacc 
real(8)                                                                :: fpowr 
real(8)                                                                :: gum 
real(8)                                                                :: gpthk0 
real(8)                                                                :: dofset 
real(8)                                                                :: ftmelt 
real(8)                                                                :: zs 
real(8)                                                                :: tc0bal 
real(8)                                                                :: tgbal 
real(8)                                                                :: apowrd 
real(8)                                                                :: zqchpr 
real(8)                                                                :: time0 
real(8)                                                                :: flxbal 
real(8)                                                                :: fuelrd 
real(8)                                                                :: rf 
real(8)                                                                :: htflxa 
real(8)                                                                :: pdrato 
real(8)                                                                :: rl 
real(8)                                                                :: dofang 
real(8)                                                                :: tplen 
real(8)                                                                :: swllfr 
real(8)                                                                :: tsntrk 
real(8)                                                                :: modheat 
real(8)                                                                :: spdbp 
real(8)                                                                :: pfail 
real(8)                                                                :: cladid 
real(8)                                                                :: fldrpr 
real(8)                                                                :: pmxbal 
real(8)                                                                :: timmd 
real(8)                                                                :: aidtot 
real(8)                                                                :: aidsur 
real(8)                                                                :: maxcladtemp 
real(8)                                                                :: rhoc 
real(8)                                                                :: tflux 
real(8)                                                                :: rhof 
real(8)                                                                :: tdbugd 
real(8)                                                                :: tprlp 
real(8)                                                                :: deloxy 
real(8)                                                                :: r8bal 
real(8)                                                                :: dtp 
real(8)                                                                :: delth0 
real(8)                                                                :: tmax 
real(8)                                                                :: achn 
real(8)                                                                :: chefus 
real(8)                                                                :: trabal 
real(8)                                                                :: t22 
real(8)                                                                :: ffchf 
real(8)                                                                :: roddiameter 
real(8)                                                                :: epsht1 
real(8)                                                                :: h0bal 
real(8)                                                                :: tmaxc 
real(8)                                                                :: zad 
real(8)                                                                :: zndbal 
real(8)                                                                :: cladpower 
real(8)                                                                :: src1 
real(8)                                                                :: rshrd 
real(8)                                                                :: fqcrit 
real(8)                                                                :: fltgap 
real(8)                                                                :: tbkbal 
real(8)                                                                :: timbal 
real(8)                                                                :: pcbal 
real(8)                                                                :: frden 
real(8)                                                                :: xtime 
real(8)                                                                :: doffst 
real(8)                                                                :: powimx 
real(8)                                                                :: zvoid2 
real(8)                                                                :: psbal 
real(8)                                                                :: emptm 
real(8)                                                                :: dvdtp 
real(8)                                                                :: tm1bal 
real(8)                                                                :: trise 
real(8)                                                                :: buoxide 
real(8)                                                                :: fltgap2 
real(8)                                                                :: hydiam 
real(8)                                                                :: dishd 
real(8)                                                                :: beta1 
real(8)                                                                :: cldwdc 
real(8)                                                                :: tplot 
real(8)                                                                :: ph 
real(8)                                                                :: tfavba 
real(8)                                                                :: timeincrement 
real(8)                                                                :: tmpac1 
real(8)                                                                :: crfpr 
real(8)                                                                :: trecrd 
real(8)                                                                :: emwbal 
real(8)                                                                :: cladod 
real(8)                                                                :: pctop 
real(8)                                                                :: zbot 
real(8)                                                                :: rsntr 
real(8)                                                                :: volavegastemp 
real(8)                                                                :: zroug1 
real(8)                                                                :: aidlng 
real(8)                                                                :: coldbp 
real(8)                                                                :: gsmfrp 
real(8)                                                                :: cfluxa 
real(8)                                                                :: dtold 
real(8)                                                                :: atop 
real(8)                                                                :: rvoid 
real(8)                                                                :: tref 
real(8)                                                                :: tf0bal 
real(8)                                                                :: pl 
real(8)                                                                :: fabal 
real(8)                                                                :: z 
real(8)                                                                :: totalgasmoles 
real(8)                                                                :: ctmelt 
real(8)                                                                :: gfloa1 
real(8)                                                                :: frchdt 
real(8)                                                                :: fhtc 
real(8)                                                                :: frpo2 
real(8)                                                                :: trest 
real(8)                                                                :: fgrns 
real(8)                                                                :: dtmpcl 
real(8)                                                                :: refine 
real(8)                                                                :: r0bal 
real(8)                                                                :: fuelpeldiam 
real(8)                                                                :: dcldh0 
real(8)                                                                :: ztmax 
real(8)                                                                :: bottomplenumgasmoles 
real(8)                                                                :: coldw 
real(8)                                                                :: openporosityfraction 
real(8)                                                                :: dhe 
real(8)                                                                :: explenumv 
real(8)                                                                :: compmt 
real(8)                                                                :: zp1bal 
real(8)                                                                :: dtss 
real(8)                                                                :: zroug2 
real(8)                                                                :: dhy 
real(8)                                                                :: volbp 
real(8)                                                                :: bup 
real(8)                                                                :: fhefus 
real(8)                                                                :: powict 
real(8)                                                                :: fotmtl 
real(8)                                                                :: pois 
real(8)                                                                :: dtobal 
real(8)                                                                :: srcv2 
real(8)                                                                :: zmesh 
real(8)                                                                :: gsms 
real(8)                                                                :: time 
real(8)                                                                :: taxbal 
real(8)                                                                :: dishv0 
real(8)                                                                :: tpo 
real(8)                                                                :: cladtk 
real(8)                                                                :: zm1bal 
real(8)                                                                :: dzmesh 
real(8)                                                                :: rnbnt 
real(8)                                                                :: totalvoidvol 
real(8)                                                                :: e 
real(8)                                                                :: rodlength 
real(8)                                                                :: zvoid1 
real(8)                                                                :: vplenc 
real(8)                                                                :: vplenb 
real(8)                                                                :: bumtp 
real(8)                                                                :: maxgaspressure 
real(8)                                                                :: rshd 
real(8)                                                                :: tcebal 
real(8)                                                                :: tempcs 
real(8)                                                                :: maxfueltemp 
real(8)                                                                :: cnfliq 
character(len=8)              , dimension(:)        , allocatable          :: emflag 
integer(ipk)                  , dimension(:)        , allocatable          :: ifstor 
integer(ipk)                  , dimension(:)        , allocatable          :: nswpm 
integer(ipk)                  , dimension(:)        , allocatable          :: tem 
integer(ipk)                  , dimension(:)        , allocatable          :: imaterials 
integer(ipk)                  , dimension(:)        , allocatable          :: modfal 
integer(ipk)                  , dimension(:)        , allocatable          :: idumr1 
integer(ipk)                  , dimension(:)        , allocatable          :: iffrp 
integer(ipk)                  , dimension(:)        , allocatable          :: indxjk 
integer(ipk)                  , dimension(:)        , allocatable          :: ibnopt 
integer(ipk)                  , dimension(:)        , allocatable          :: ihdata 
integer(ipk)                  , dimension(:)        , allocatable          :: ies 
integer(ipk)                  , dimension(:)        , allocatable          :: nopt 
integer(ipk)                  , dimension(:)        , allocatable          :: irest3 
integer(ipk)                  , dimension(:)        , allocatable          :: irest2 
integer(ipk)                  , dimension(:)        , allocatable          :: imatflag 
integer(ipk)                  , dimension(:)        , allocatable          :: htco 
integer(ipk)                  , dimension(:)        , allocatable          :: idatapairs 
integer(ipk)                  , dimension(:)        , allocatable          :: ncs 
integer(ipk)                  , dimension(:)        , allocatable          :: nm 
integer(ipk)                  , dimension(:)        , allocatable          :: imatflag1 
integer(ipk)                  , dimension(:)        , allocatable          :: nunopt 
integer(ipk)                  , dimension(:)        , allocatable          :: ngastmp 
integer(ipk)                  , dimension(:)        , allocatable          :: ndchfi 
integer(ipk)                  , dimension(:)        , allocatable          :: iheattablen 
real(r8k)                     , dimension(:)        , allocatable          :: roi 
real(r8k)                     , dimension(:)        , allocatable          :: tbulk_v 
real(r8k)                     , dimension(:)        , allocatable          :: scd 
real(r8k)                     , dimension(:)        , allocatable          :: dtplta 
real(r8k)                     , dimension(:)        , allocatable          :: apln 
real(r8k)                     , dimension(:)        , allocatable          :: ascal2 
real(r8k)                     , dimension(:)        , allocatable          :: ascal3 
real(r8k)                     , dimension(:)        , allocatable          :: ascal1 
real(r8k)                     , dimension(:)        , allocatable          :: tbulk_l 
real(r8k)                     , dimension(:)        , allocatable          :: flwblk 
real(r8k)                     , dimension(:)        , allocatable          :: oldcldplasstrn 
real(r8k)                     , dimension(:)        , allocatable          :: ts 
real(r8k)                     , dimension(:)        , allocatable          :: tp 
real(r8k)                     , dimension(:)        , allocatable          :: claddingpower 
real(r8k)                     , dimension(:)        , allocatable          :: cexh2a 
real(r8k)                     , dimension(:)        , allocatable          :: epstrain1old 
real(r8k)                     , dimension(:)        , allocatable          :: azpang 
real(r8k)                     , dimension(:)        , allocatable          :: fmesh 
real(r8k)                     , dimension(:)        , allocatable          :: powave 
real(r8k)                     , dimension(:)        , allocatable          :: apln0 
real(r8k)                     , dimension(:)        , allocatable          :: amatpc 
real(r8k)                     , dimension(:)        , allocatable          :: fdial 
real(r8k)                     , dimension(:)        , allocatable          :: sedpnnlold 
real(r8k)                     , dimension(:)        , allocatable          :: dvdt 
real(r8k)                     , dimension(:)        , allocatable          :: table1 
real(r8k)                     , dimension(:)        , allocatable          :: htclev 
real(r8k)                     , dimension(:)        , allocatable          :: farbal 
real(r8k)                     , dimension(:)        , allocatable          :: table2 
real(r8k)                     , dimension(:)        , allocatable          :: table3 
real(r8k)                     , dimension(:)        , allocatable          :: gadoln 
real(r8k)                     , dimension(:)        , allocatable          :: gfrac 
real(r8k)                     , dimension(:)        , allocatable          :: stressaxialold 
real(r8k)                     , dimension(:)        , allocatable          :: gbh 
real(r8k)                     , dimension(:)        , allocatable          :: arest1 
real(r8k)                     , dimension(:)        , allocatable          :: gbse 
real(r8k)                     , dimension(:)        , allocatable          :: drdfrp 
real(r8k)                     , dimension(:)        , allocatable          :: fuelgasswell 
real(r8k)                     , dimension(:)        , allocatable          :: stresshoopold 
real(r8k)                     , dimension(:)        , allocatable          :: dtmaxa 
real(r8k)                     , dimension(:)        , allocatable          :: edint 
real(r8k)                     , dimension(:)        , allocatable          :: oldepeff 
real(r8k)                     , dimension(:)        , allocatable          :: hlqcl1 
real(r8k)                     , dimension(:)        , allocatable          :: tmelt 
real(r8k)                     , dimension(:)        , allocatable          :: temptm 
real(r8k)                     , dimension(:)        , allocatable          :: vsn 
real(r8k)                     , dimension(:)        , allocatable          :: bpln 
real(r8k)                     , dimension(:)        , allocatable          :: tend 
real(r8k)                     , dimension(:)        , allocatable          :: aexcb 
real(r8k)                     , dimension(:)        , allocatable          :: bufrp 
real(r8k)                     , dimension(:)        , allocatable          :: vs0 
real(r8k)                     , dimension(:)        , allocatable          :: hgpfrp 
real(r8k)                     , dimension(:)        , allocatable          :: pbh2 
real(r8k)                     , dimension(:)        , allocatable          :: techfi 
real(r8k)                     , dimension(:)        , allocatable          :: pbh1 
real(r8k)                     , dimension(:)        , allocatable          :: bu 
real(r8k)                     , dimension(:)        , allocatable          :: epstrain2old 
real(r8k)                     , dimension(:)        , allocatable          :: bpln0 
real(r8k)                     , dimension(:)        , allocatable          :: fluxz 
real(r8k)                     , dimension(:)        , allocatable          :: hbh 
real(r8k)                     , dimension(:)        , allocatable          :: oldeffstrainpnnl 
real(r8k)                     , dimension(:)        , allocatable          :: nodchf 
real(r8k)                     , dimension(:)        , allocatable          :: aprntb 
real(r8k)                     , dimension(:)        , allocatable          :: rodavepower 
real(r8k)                     , dimension(:)        , allocatable          :: sdfar 
real(r8k)                     , dimension(:)        , allocatable          :: profilestarttime 
real(r8k)                     , dimension(:)        , allocatable          :: gasfraction 
real(r8k)                     , dimension(:)        , allocatable          :: hfusn 
real(r8k)                     , dimension(:)        , allocatable          :: qpln0 
real(r8k)                     , dimension(:)        , allocatable          :: vrlfrp 
real(r8k)                     , dimension(:)        , allocatable          :: rmrlp 
real(r8k)                     , dimension(:)        , allocatable          :: swd 
real(r8k)                     , dimension(:)        , allocatable          :: tschfi 
real(r8k)                     , dimension(:)        , allocatable          :: hsolold 
real(r8k)                     , dimension(:)        , allocatable          :: pclrlp 
real(r8k)                     , dimension(:)        , allocatable          :: elevatthermhydr 
real(r8k)                     , dimension(:)        , allocatable          :: oxideod 
real(r8k)                     , dimension(:)        , allocatable          :: butemp 
real(r8k)                     , dimension(:)        , allocatable          :: hinta 
real(r8k)                     , dimension(:)        , allocatable          :: stressradialold 
real(r8k)                     , dimension(:)        , allocatable          :: cladeffstressold 
real(r8k)                     , dimension(:)        , allocatable          :: fldrat 
real(r8k)                     , dimension(:)        , allocatable          :: pgpfrp 
real(r8k)                     , dimension(:)        , allocatable          :: radpel 
real(r8k)                     , dimension(:)        , allocatable          :: hlqcl 
real(r8k)                     , dimension(:)        , allocatable          :: deltox 
real(r8k)                     , dimension(:)        , allocatable          :: dvdt0 
real(r8k)                     , dimension(:)        , allocatable          :: pelprm 
real(r8k)                     , dimension(:)        , allocatable          :: hupta 
real(r8k)                     , dimension(:)        , allocatable          :: cmesh 
real(r8k)                     , dimension(:)        , allocatable          :: bowrat 
real(r8k)                     , dimension(:)        , allocatable          :: vplen 
real(r8k)                     , dimension(:)        , allocatable          :: extentofbow 
real(r8k)                     , dimension(:)        , allocatable          :: ablona 
real(r8k)                     , dimension(:)        , allocatable          :: oxideid 
real(r8k)                     , dimension(:)        , allocatable          :: gasphs 
real(r8k)                     , dimension(:)        , allocatable          :: spl 
real(r8k)                     , dimension(:)        , allocatable          :: eppinp 
real(r8k)                     , dimension(:)        , allocatable          :: ispace 
real(r8k)                     , dimension(:)        , allocatable          :: pswll0 
real(r8k)                     , dimension(:)        , allocatable          :: qpln 
real(r8k)                     , dimension(:)        , allocatable          :: explenumt 
real(r8k)                     , dimension(:)        , allocatable          :: dtpoa 
real(r8k)                     , dimension(:)        , allocatable          :: techf 
real(r8k)                     , dimension(:)        , allocatable          :: afrap 
real(r8k)                     , dimension(:)        , allocatable          :: epstrain3old 
real(r8k)                     , dimension(:)        , allocatable          :: radpowprofile 
real(r8k)                     , dimension(:)        , allocatable          :: relfraca 
real(r8k)                     , dimension(:)        , allocatable          :: tschf 
real(r8k)                     , dimension(:)        , allocatable          :: gfint 
real(r8k)                     , dimension(:)        , allocatable          :: gadolin 
real(r8k)                     , dimension(:)        , allocatable          :: zfarbl 
real(r8k)                     , dimension(:)        , allocatable          :: oldeps 
real(r8k)                     , dimension(:)        , allocatable          :: flowg 
real(r8k)                     , dimension(:)        , allocatable          :: gappr0 
real(r8k)                     , dimension(:)        , allocatable          :: prestm 
real(r8k)                     , dimension(:)        , allocatable          :: elevatfrap 
real(r8k)                     , dimension(:)        , allocatable          :: pinput 
real(r8k)                     , dimension(:)        , allocatable          :: htc_v 
real(r8k)                     , dimension(:)        , allocatable          :: potput 
real(r8k)                     , dimension(:)        , allocatable          :: sedepriold 
real(r8k)                     , dimension(:)        , allocatable          :: acolct 
real(r8k)                     , dimension(:)        , allocatable          :: gasax 
real(r8k)                     , dimension(:)        , allocatable          :: zelev 
real(r8k)                     , dimension(:)        , allocatable          :: htc_l 
integer(ipk)                  , dimension(:,:)      , allocatable          :: icglob 
integer(ipk)                  , dimension(:,:)      , allocatable          :: nrc 
integer(ipk)                  , dimension(:,:)      , allocatable          :: iokold 
integer(ipk)                  , dimension(:,:)      , allocatable          :: iok 
real(r8k)                     , dimension(:,:)      , allocatable          :: tmprlp 
real(r8k)                     , dimension(:,:)      , allocatable          :: axpowprofile 
real(r8k)                     , dimension(:,:)      , allocatable          :: radsrc 
real(r8k)                     , dimension(:,:)      , allocatable          :: burad 
real(r8k)                     , dimension(:,:)      , allocatable          :: radtemp 
real(r8k)                     , dimension(:,:)      , allocatable          :: radsrco 
real(r8k)                     , dimension(:,:)      , allocatable          :: tpln 
real(r8k)                     , dimension(:,:)      , allocatable          :: htca 
real(r8k)                     , dimension(:,:)      , allocatable          :: frapcontemp 
real(r8k)                     , dimension(:,:)      , allocatable          :: tplbot 
real(r8k)                     , dimension(:,:)      , allocatable          :: qcold 
real(r8k)                     , dimension(:,:)      , allocatable          :: radpow 
real(r8k)                     , dimension(:,:)      , allocatable          :: tplbt1 
real(r8k)                     , dimension(:,:)      , allocatable          :: pchn 
real(r8k)                     , dimension(:,:)      , allocatable          :: fuelrad 
real(r8k)                     , dimension(:,:)      , allocatable          :: pazp 
real(r8k)                     , dimension(:,:)      , allocatable          :: gasths 
real(r8k)                     , dimension(:,:)      , allocatable          :: tblka 
real(r8k)                     , dimension(:,:,:)    , allocatable          :: tplna 
    ! Avogadro's Number
    REAL(r8K), PARAMETER :: Avogadro = 6.0221413E23_r8k              ! (molecules / mol)
    ! Boltzmann's Constant
    REAL(r8k), PARAMETER :: Boltzmann = 1.3806505E-23_r8k            ! (J / K)
    ! Gravitational Acceleration
    REAL(r8K), PARAMETER :: Gravity_SI = 9.80665_r8k                 ! (m / s)
    ! Ideal Gas Constant
    REAL(r8K), PARAMETER :: R_JmolK = 8.3144621_r8k                  ! (J / mol - K)
    ! Ideal Gas Constant
    REAL(r8k), PARAMETER :: R_in3psiRlbmol = 10.73159_r8k            ! (in^3*psi)/(Rankine*lb-mol)
    ! Pi
    REAL(r8k), PARAMETER :: pi = 3.1415926535897932_r8k              ! Value for pi
    ! Speed of Light
    REAL(r8K), PARAMETER :: speedlight = 2.99792458E8_r8k            ! (m / s)
    

    REAL(r8k), PARAMETER :: ftin = 12.0_r8k              ! ( inch / ft )
    REAL(r8k), PARAMETER :: ftom = 0.3048_r8k            ! ( ft / meter )
    REAL(r8k), PARAMETER :: ftmetr = 3.28084_r8k         ! ( m / ft )
    REAL(r8k), PARAMETER :: sechr = 3600.0_r8k           ! ( second / hour )
    REAL(r8k), PARAMETER :: btuhkw = 3412.874_r8k        ! ( btu/hour / kilowatt )
    REAL(r8k), PARAMETER :: powcnv = 0.9478_r8k          ! ( Btu/second / kilowatt) !.94781712
    REAL(r8k), PARAMETER :: psinm2 = 1.4503774e-4_r8k    ! ( psi / n/m2 )
    REAL(r8k), PARAMETER :: psift2 = 144.0_r8k           ! ( psi / lb/ft^2 )
    REAL(r8k), PARAMETER :: gtolb = 453.6_r8k            ! ( grams / lb )
    REAL(r8k), PARAMETER :: hgpcnv = 5.67826_r8k         ! Convert gap HTC (Btu/hr*ft^2*F to W/m^2*K)
    !
    !
    ! ** Energy **
    !
    ! Convert BTU/lb to J/kg
    REAL(r8k), PARAMETER :: BTUlbtoJkg = 2326.000292_r8k             ! (J/kg / BTU/lb)
    ! Convert J/kg to BTU/lb
    REAL(r8k), PARAMETER :: JkgtoBTUlb = 1.0_r8k / BTUlbtoJkg        ! (BTU/lb / J/kg)
    !
    ! ** Heat Conductance (Thermal Conductivity) **
    !
    ! Convert Btu/(hr*ft*F) to W/(m*K)
    REAL(r8k), PARAMETER :: BhftFtoWmK = 1.73073467_r8k              ! Convert gap HTC (W/m*K / Btu/hr*ft*F)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBhftF = 1.0_r8k / BhftFtoWmK        ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    ! Convert W/(m*K) to Btu/(hr*ft*F)
    REAL(r8k), PARAMETER :: WmKtoBsftF = WmKtoBhftF / 3600.0_r8k     ! Convert gap HTC (Btu/hr*ft*F / W/m*K)
    !
    ! ** Heat Capacity **
    !
    ! Convert Btu/(lb*F) to J/(kg*K)
    REAL(r8k), PARAMETER :: BtulbFtoJkgK = 4183.995381_r8k           ! (J/kg*K / Btu/lb*F) or (J/kg*C)
    ! Convert J/(kg*K) to Btu/(lb*F)
    REAL(r8k), PARAMETER :: JkgKtoBtulbF = 1.0_r8k / BtulbFtoJkgK    ! (Btu/lb*F / J/kg*K) or (J/kg*C)
    !
    ! ** Pressure **
    !
    ! Convert atm to psi
    REAL(r8k), PARAMETER :: ATMtoPSI = 14.6959488_r8k                ! (psi / atm)
    ! Convert psi to atm
    REAL(r8k), PARAMETER :: PSItoATM = 1.0_r8k / ATMtoPSI            ! (atm / psi)
    ! Convert psi to lb/ft^2
    REAL(r8k), PARAMETER :: PSItolbft2 = 144.0_r8k                   ! (lb/in^2 / lb/ft^2)
    ! Convert kg/cm^2 to psi
    REAL(r8k), PARAMETER :: kgcm2toPSI = 14.2233433_r8k              ! (psi / kg-Force/cm^2)
    ! Convert psi to kg/cm^2
    REAL(r8k), PARAMETER :: PSItokgcm2 = 1.0_r8k / kgcm2toPSI        ! (kg-Force/cm^2 / psi)
    ! Convert psi to kPa
    REAL(r8k), PARAMETER :: PSItokPa = 6.89475728_r8k                ! (kPa / psi)
    ! Convert psi to Pa
    REAL(r8k), PARAMETER :: PSItoPa = 1.0E3_r8k * PSItokPa           ! (Pa / psi)
    ! Convert psi to MPa
    REAL(r8k), PARAMETER :: PSItoMPa = 1.0E-3_r8k * PSItokPa         ! (MPa / psi)
    ! Convert Pa to psi
    REAL(r8k), PARAMETER :: PatoPSI = 1.0_r8k / PSItoPa              ! (Pa / psi)
    ! Convert MPa to psi
    REAL(r8k), PARAMETER :: MPatoPSI = 1.0_r8k / PSItoMPa            ! (Pa / psi)
    INTEGER(ipk) , target :: nvol
    INTEGER(ipk) , target :: ithymx
    INTEGER(ipk) , target :: ixazim
    INTEGER(ipk) , target :: ncall
    INTEGER(ipk) , target :: nbrtmp           !
    INTEGER(ipk) , target :: nbrfdr           !
    INTEGER(ipk) , target :: nbrfht           !
    INTEGER(ipk) , target :: kaxnlo           !
    INTEGER(ipk) , target :: liqnod           !
    INTEGER(ipk) , target :: izadfg           !
    INTEGER(ipk) , target :: irup             !
    INTEGER(ipk) , target :: nbrpst           !
    INTEGER(ipk) , target :: nflec            !
    INTEGER(ipk) , target :: nbrliq           !
    INTEGER(ipk) , target :: npaxpf           !
    INTEGER(ipk) , target :: mzq1             !
    INTEGER(ipk) , target :: mflt             !
    INTEGER(ipk) , target :: mbdl             !
    INTEGER(ipk) , target :: ntempf           !
    INTEGER(ipk) , target :: nsrad3
    INTEGER(ipk) , target :: nelrad
    REAL(r8k), POINTER :: tt
    REAL(r8k), POINTER :: CoolantPress
    REAL(r8k), POINTER :: v
    REAL(r8k), POINTER :: ubar
    REAL(r8k), POINTER :: hbar
    REAL(r8k), POINTER :: beta
    REAL(r8k), POINTER :: kappa
    REAL(r8k), POINTER :: csubp
    REAL(r8k), POINTER :: x
    REAL(r8k), POINTER :: psat
    REAL(r8k), POINTER :: vsubf
    REAL(r8k), POINTER :: vsubg
    REAL(r8k), POINTER :: usubf
    REAL(r8k), POINTER :: usubg
    REAL(r8k), POINTER :: hsubf
    REAL(r8k), POINTER :: hsubg
    REAL(r8k), POINTER :: betaf
    REAL(r8k), POINTER :: betag
    REAL(r8k), POINTER :: kappaf
    REAL(r8k), POINTER :: kappag
    REAL(r8k), POINTER :: csubpf
    REAL(r8k), POINTER :: csubpg
    REAL(r8k) , target :: tc1
    REAL(r8k) , target :: tc2
    REAL(r8k) , target :: hliq                !
    REAL(r8k) , target :: qmax                !
    REAL(r8k) , target :: empytm              ! User specified problem time for start of adiabatic heatup, s
    REAL(r8k) , target :: hrad                ! User specified radiation heat transfer coefficient
    REAL(r8k) , target :: fldrte              ! flood rate, in/s
    REAL(r8k) , target :: zqch                !
    REAL(r8k) , target :: oldtim              !
    REAL(r8k) , target :: tflood              ! Time since start of reflood, s
    REAL(r8k) , target :: crf                 ! Carry out rate fraction (reflood)
    REAL(r8k) , target :: templo              ! coolant temp of next lower axial node, F
    REAL(r8k) , target :: rhostm              ! specific density of steam from sth2x
    REAL(r8k) , target :: cpstem              ! heat capacity of steam from sth2x
    REAL(r8k) , target :: tsatt               ! Saturation temperature
    REAL(r8k) , target :: pressr              ! system pressure, psia
    REAL(r8k) , target :: pressi              ! system pressure, SI units
    REAL(r8k) , target :: cpmult              ! heat capacity multiplier, unitless
    REAL(r8k) , target :: gflow               ! outlet mass flow rate
    REAL(r8k) , target :: temphi              ! new coolant bulk temperature
    REAL(r8k) , target :: ruplev              ! rupture elevation, ft
    REAL(r8k) , target :: pavg                ! Average rod power, kw/ft
    REAL(r8k) , target :: refdtm              ! Problem time for initiation of reflood, s
    REAL(r8k) , target :: hydiam              ! Channel hydraulic diameter, ft
    REAL(r8k) , target :: flxsec              ! flow channel cross sectional area, ft^2
    REAL(r8k) , target :: tsub                ! coolant subcooling (tsat - tcoolant), F
    REAL(r8k) , target :: pdeint              ! 
    REAL(r8k) , target :: flowbk              ! Flow blockage, %
    REAL(r8k) , target :: tempmx              ! Max clad surface temperature, F
    REAL(r8k) , target :: pfflec              ! flect axial power peaking factor
    REAL(r8k) , target :: tfoldf              ! Time (maybe for flooding), s
    REAL(r8k) , target :: pdecy               ! 
    REAL(r8k) , target :: drflt               ! 
    REAL(r8k) , target :: pfnuc               !
    REAL(r8k) , target :: toldfc              !
    REAL(r8k) , target :: zqflt               !
    REAL(r8k) , target :: qaxpk               !
    REAL(r8k) , target :: zpkfc               !
    REAL(r8k) , target :: fltgap              !
    REAL(r8k) , target :: pavgft              !
    REAL(r8k) , target :: rcpar               !
    REAL(r8k) , target :: zad
    REAL(r8k) , target :: zs
    REAL(r8k) , target :: trodfc
    REAL(r8k) , target :: nu1
    REAL(r8k) , target :: nu2
    REAL(r8k) , target :: nu3
    CHARACTER(LEN=8) , target :: rupflg
    CHARACTER(LEN=8) , target :: lodmrk
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: nhprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: ntprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: nvprs
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: npprs
    REAL(r8k), DIMENSION(:), ALLOCATABLE,  target :: Prop
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: z1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: z2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pz2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tz1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: flthit
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: faxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qaxzq
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tempfc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: aflcht
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: prestm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hlqclp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: temptm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: fldrat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: aasth
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: acoold
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vfrad3
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: elvrad
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: htclev
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hinta
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hupta
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: pbh
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tshrda
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: htca
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: tblka
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad2
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: trad3
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: IndexTempConverg
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RadialPower
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RadialBound
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qmaxmelt
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qmaxmeltp1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: rmassflux
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: coolqual
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AAHT1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: BBHT1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GasPress0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolPress
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GasMolesAx
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GasMolesAx0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GasPress
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FuelSurfT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FuelCenterT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CrackTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: VolOpenPor
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GapThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GapThick0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: GapTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PelletRad
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PelletRad0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HeatFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HeatFlux0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: SurfHtFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladMaxT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: Vreloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: Vreloc0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PelSrfDispl
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldPelDis
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldPelDis0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxiThk1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxiThk2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: DishVperL
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FastFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AlphaThk1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AlphaThk2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AlphaThk11
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AlphaThk22
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: oxideid
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffStrain0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: oxideod
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CldPermStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CldPermStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: SSHgap
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCoolPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCoolPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladAveTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldGasPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldGasPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HtFlxFac
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCladT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldFuelAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RodOD
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCldAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCldAxStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RodOD0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldCladT0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OldFuelAxStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: TimeofGBSep
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: SwellDispl
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HmaxInitial
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RInterfacPrs
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RInterfacPrs0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HGapAv
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FilmCoeffAv
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AxialPowr
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AxialPowr0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CritHtFlux
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FuelSrfStrRat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WatrMetlEnrgy
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: RInterfacGap
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladSurfT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EDotFZ
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PelSrfStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FuelSrfStrRat0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EDotFZ0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EnergyPerL
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HFXSum
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladEnrgPerL
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolEnthalpy
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolEnthalpy0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolMassFlx
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: Enthl
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolDensity
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CoolDensity0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PelRadDeviat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CldPermAxStrn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: VoidVolumeRatio
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CldPermHoopStrn 
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ECR
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxUptakeID1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxUpTakeID2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: SEDPNNL
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffStrainPNNL
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: coefk
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: coefn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: coefm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: Emodulus
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: strainrateterm
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: SEDEPRI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffFastFluStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: BetaThickness
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffColdWkStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxygenConcenAve
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffColdWkStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxygenUptake
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxStAlphaThkRemain
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxStAlphaThkNearFuel
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EffFastFluStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxiPowerGen
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PrcntSatBeta
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAAO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenABO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenACO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenADO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAEO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAFO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAGO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAHO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAIO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAAI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenABI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenACI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenADI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAEI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAFI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAGI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAHI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OxConcenAII
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CrackVolume
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OpenPorTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AveDishTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CentVoidVol
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ExtentOfBow
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AxialNodLen
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: TerfacePres
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CrackWidth
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EinstabilStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AxBurnup
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: BOSOxideThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: EOSOxideThick
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OpenPorVol
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: OpenPorosity
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladEffStress
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: BulkCoolTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CladYieldStress
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: StressAtInstStrain
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: techf
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: CesiumContent
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HydrogenContent
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tschf
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceGAPI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceEP1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceURC
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceTCMx
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceGAP
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: nodchf
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: BOSTemp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EOSTemp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EOSRad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: BOSRad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EnrgyMeltP1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EnrgyMeltZ
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EnrgyMelt
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: EnrgyMeltZp1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: RadialBoundO
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: bufrad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: DeformedRadiusOfMesh
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: WorkSpaceEPP1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: buradv
    REAL(r8k) , target :: gapmin
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpacePINT
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceRCI
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceRCO
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: WorkSpaceReloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: AxNodElevat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ureloc
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: a1
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldElStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldPlasStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldPlasStrn0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldStrnRat
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldStrnRat0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: HydrostatPress
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: FuelResidStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: FuelResidStrn0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: Baln2Twall
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldresidStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldResidStrn0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: IodineContent
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: PelSrfStrn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldStress
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: CldThermStrn
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE , target ::GRsv
    INTEGER(ipk) , target :: Ifchk
    INTEGER(ipk) , target :: nce
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: IndexPCMI
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: IndexPCMIOnce
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: NumAzmuthNod
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: Ichf
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: Ih
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: Ihtreg
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: GapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: BOSGapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: BOSGapIndex0
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: RuptFailIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: CladCollapseIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: CladCollIndx0
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: OldGapIndex
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: NodeSinterTemp
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: RodFailIndex
    INTEGER(ipk) , target :: ngasr
    !
    INTEGER(ipk) , target :: TranSwell
    ! Suboption for fission gas release history (0=off, 1=user-input pairs, 2=FRAPFGR)
    INTEGER(ipk) , target :: presfgr
    ! # of pairs of fuel swelling values
    INTEGER(ipk) , target :: nFuelSwellPairs
    ! # of pairs of fission gas release values
    INTEGER(ipk) , target :: nFGRpairs
    !
    REAL(r8k) , target :: GasMoles0
    !
    REAL(r8k) , target :: prodxe
    !
    REAL(r8k) , target :: prodkr
    !
    REAL(r8k) , target :: relfract
    !
    REAL(r8k) , target :: TranFuelSwell
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gsmol0
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: relfraca
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FuelGasSwell
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ansr
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: fmgp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: gasavail1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: gasavail2
    INTEGER(ipk) , target :: Nchan
    REAL(r8k) , target :: areao
    REAL(r8k) , target :: arean
    REAL(r8k) , target :: epsht1
    REAL(r8k) , target :: GapConductivity
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: BoundaryCondition
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ThermalConductAdv
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: FinalTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: PrevTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ArrayE
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ArrayF
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: VolumeWeightL
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: VolumeWeightR
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: AreaWeight
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ThermalConductivity
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: acond
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: RhoCp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: RhoCp0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: PrevIterateTemp

    REAL(r8k) , target :: sigfuelthermcond
    REAL(r8k) , target :: sigfuelthermexp
    REAL(r8k) , target :: sigfuelheatcapa
    REAL(r8k) , target :: sigcladthermcond
    REAL(r8k) , target :: sigcladthermexp
    REAL(r8k) , target :: sigcladyieldstr
    REAL(r8k) , target :: sigsurfhtc
    REAL(r8k) , target :: tdkt
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dtdkta
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: dktouts
    LOGICAL , target :: dakota
    INTEGER(ipk) , target :: ndktn
    INTEGER(ipk) , target :: ndktparams
    INTEGER(ipk) , target :: dktoutcounter
    REAL(r8k), target :: maxfueltemp = 0.0_r8k
    !
    REAL(r8k), target  :: maxcladtemp = 0.0_r8k
    !
    REAL(r8k), target  :: maxgaspressure = 0.0_r8k
    !
    REAL(r8k), target  :: maxenthalpy = 0.0_r8k
    character(len=12)                 , pointer :: relocmodel           ! 
    character(len=200)                , pointer :: namerf               ! 
    character(len=3)                  , pointer :: reflood              ! 
    character(len=3)                  , pointer :: deformation          ! 
    character(len=10)                 , pointer :: inst                 ! 
    character(len=3)                  , pointer :: metal                ! 
    character(len=3)                  , pointer :: mheat                ! 
    character(len=3)                  , pointer :: bheat                ! 
    character(len=3)                  , pointer :: coolant              ! 
    character(len=3)                  , pointer :: internal             ! 
    integer(ipk)                      , pointer :: reflo                ! Suboption to specify reflood rate as a function of time
    integer(ipk)                      , pointer :: cathca               ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
    integer                           , pointer :: lowpl                ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
    integer(ipk)                      , pointer :: inp                  ! 
    integer(ipk)                      , pointer :: rtheta               ! 
    integer(ipk)                      , pointer :: geom                 ! Suboption to specify the inner radius of the flow shroud.
    integer                           , pointer :: nvol1                ! Number of coolant zones stacked on top of each other and surrounding fuel rod
    integer(ipk)                      , pointer :: gasflo               ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
    integer(ipk)                      , pointer :: noball               ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
    integer(ipk)                      , pointer :: prescri              ! 
    integer(ipk)                      , pointer :: azang                ! 
    integer(ipk)                      , pointer :: idoxid               ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
    integer(ipk)                      , pointer :: tape1                ! 
    integer                           , pointer :: massfl               ! Suboption to specify the coolant mass flux history
    integer(ipk)                      , pointer :: odoxid               ! 
    integer(ipk)                      , pointer :: unitin               ! Option to specify that the input data are in SI units
    integer(ipk)                      , pointer :: geometry             ! Suboption to specify geometry parameters
    integer                           , pointer :: filmbo               ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
    integer(ipk)                      , pointer :: chf                  ! Suboption to select the CHF correlation to be used
    integer                           , pointer :: coreav               ! Suboption to specify the core average coolant enthalpy history
    integer(ipk)                      , pointer :: cenvoi               ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
    integer(ipk)                      , pointer :: grass                ! 
    integer(ipk)                      , pointer :: baker                ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
    integer(ipk)                      , pointer :: geomet               ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
    integer(ipk)                      , pointer :: soltyp               ! Option to specify an explicit solution
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
    integer(ipk)                      , pointer :: IndexGrainBndSep     !     
    integer(ipk)                      , pointer :: coldwa               !     
    integer(ipk)                      , pointer :: ruptur               !     
    integer(ipk)                      , pointer :: pressu               !     
    integer(ipk)                      , pointer :: pow                  !     
    integer(ipk)                      , pointer :: press                !     
    integer(ipk)                      , pointer :: res                  !     
    integer(ipk)                      , pointer :: pressure             !     
    integer(ipk)                      , pointer :: temp                 !     
    real(r8k)                         , pointer :: pl                   ! 
    real(r8k)                         , pointer :: ph                   ! 
    real(r8k)                         , pointer :: buoxide              !                  
    real(r8k)                         , pointer :: CladPower            ! 
    real(r8k)                         , pointer :: RodDiameter          ! 
    real(r8k)                         , pointer :: gapthk               ! 
    real(r8k)                         , pointer :: ffch                 ! 
    real(r8k)                         , pointer :: fpowr                ! 
    real(r8k)                         , pointer :: doffst               ! 
    real(r8k)                         , pointer :: RodLength            ! 
    real(r8k)                         , pointer :: emptm                ! 
    real(r8k)                         , pointer :: fltgap2              ! 
    real(r8k)                         , pointer :: gsms                 ! 
    real(r8k)    , dimension(:,:)     , pointer :: radtemp              ! 
    real(r8k)    , dimension(:,:)     , pointer :: fuelrad              ! 
    real(r8k)    , dimension(:)       , pointer :: gfrac                ! 
    real(r8k)    , dimension(:)       , pointer :: butemp               ! 