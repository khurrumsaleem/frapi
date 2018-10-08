logical                                         , pointer :: convert_units 
logical                                         , pointer :: printballoon 
logical                                         , pointer :: printrodburst 
logical                                         , pointer :: allocate_arrays_ft 
logical                                         , pointer :: dakota 
logical                                         , pointer :: coupled 
logical                                         , pointer :: ndebug 
logical                                         , pointer :: first_pass 
logical                                         , pointer :: setunits 
logical                                         , pointer :: is_export 
logical                                         , pointer :: first_call 
logical                                         , pointer :: unit 
character(len=8)                                , pointer :: rupflg 
character(len=3)                                , pointer :: coolant 
character(len=80)                               , pointer :: fraptranfn 
character(len=3)                                , pointer :: metal 
character(len=15)                               , pointer :: modheatmodel 
character(len=12)                               , pointer :: relocmodel 
character(len=8)                                , pointer :: lodmrk 
character(len=3)                                , pointer :: reflood 
character(len=200)                              , pointer :: namerf 
character(len=10)                               , pointer :: inst 
character(len=3)                                , pointer :: bheat 
character(len=3)                                , pointer :: internal 
character(len=3)                                , pointer :: mheat 
character(len=3)                                , pointer :: radiation 
character(len=3)                                , pointer :: deformation 
integer(ipk)                                    , pointer :: ifaila 
integer(ipk)                                    , pointer :: kaxnlo 
integer(ipk)                                    , pointer :: ncirlp 
integer(ipk)                                    , pointer :: nprsw 
integer(ipk)                                    , pointer :: npbh 
integer(ipk)                                    , pointer :: nbncal 
integer(ipk)                                    , pointer :: llp 
integer(ipk)                                    , pointer :: ierr 
integer(ipk)                                    , pointer :: lcoold 
integer(ipk)                                    , pointer :: nidoxide 
integer(ipk)                                    , pointer :: press 
integer(ipk)                                    , pointer :: cathca 
integer(ipk)                                    , pointer :: klp2 
integer(ipk)                                    , pointer :: nelrad 
integer(ipk)                                    , pointer :: frapt4 
integer(ipk)                                    , pointer :: ntapou 
integer(ipk)                                    , pointer :: kaxhtc 
integer(ipk)                                    , pointer :: indexthermcon 
integer(ipk)                                    , pointer :: prestmp 
integer(ipk)                                    , pointer :: irupt 
integer(ipk)                                    , pointer :: indxmd 
integer(ipk)                                    , pointer :: nbowr 
integer(ipk)                                    , pointer :: ldialb 
integer(ipk)                                    , pointer :: modbal 
integer(ipk)                                    , pointer :: pressu 
integer(ipk)                                    , pointer :: inlet 
integer(ipk)                                    , pointer :: zone 
integer(ipk)                                    , pointer :: ndap1 
integer(ipk)                                    , pointer :: nbhtc 
integer(ipk)                                    , pointer :: protectiveoxide 
integer(ipk)                                    , pointer :: rtheta 
integer(ipk)                                    , pointer :: ldir 
integer(ipk)                                    , pointer :: istoicgrad 
integer(ipk)                                    , pointer :: ifail 
integer(ipk)                                    , pointer :: lchf 
integer(ipk)                                    , pointer :: itswt 
integer(ipk)                                    , pointer :: pow 
integer(ipk)                                    , pointer :: nsymm 
integer(ipk)                                    , pointer :: naazp 
integer(ipk)                                    , pointer :: nvol2 
integer(ipk)                                    , pointer :: idebug 
integer(ipk)                                    , pointer :: nvol1 
integer(ipk)                                    , pointer :: mhbh 
integer(ipk)                                    , pointer :: modfd 
integer(ipk)                                    , pointer :: ixazmn 
integer(ipk)                                    , pointer :: gasflo 
integer(ipk)                                    , pointer :: nbrtmp 
integer(ipk)                                    , pointer :: nbrfdr 
integer(ipk)                                    , pointer :: prescri 
integer(ipk)                                    , pointer :: mbowr 
integer(ipk)                                    , pointer :: mtabl3 
integer(ipk)                                    , pointer :: mechan 
integer(ipk)                                    , pointer :: nfmesh 
integer(ipk)                                    , pointer :: mndchf 
integer(ipk)                                    , pointer :: naazpd 
integer(ipk)                                    , pointer :: npelraddeviat 
integer(ipk)                                    , pointer :: itdmx 
integer(ipk)                                    , pointer :: nhbh 
integer(ipk)                                    , pointer :: nfrapconinitialization 
integer(ipk)                                    , pointer :: ncool 
integer(ipk)                                    , pointer :: npair 
integer(ipk)                                    , pointer :: indexgrainbndsep 
integer(ipk)                                    , pointer :: nedtsw 
integer(ipk)                                    , pointer :: azang 
integer(ipk)                                    , pointer :: m5fc2 
integer(ipk)                                    , pointer :: radiat 
integer(ipk)                                    , pointer :: mhinta 
integer(ipk)                                    , pointer :: luout 
integer(ipk)                                    , pointer :: nmesh 
integer(ipk)                                    , pointer :: mgaspr 
integer(ipk)                                    , pointer :: iagap 
integer(ipk)                                    , pointer :: indxkf 
integer(ipk)                                    , pointer :: nitmin 
integer(ipk)                                    , pointer :: nconsw 
integer(ipk)                                    , pointer :: numdta 
integer(ipk)                                    , pointer :: nhtcz 
integer(ipk)                                    , pointer :: kntbal 
integer(ipk)                                    , pointer :: nrazp 
integer(ipk)                                    , pointer :: iplant 
integer(ipk)                                    , pointer :: indexthermconadv 
integer(ipk)                                    , pointer :: mhlqc1 
integer(ipk)                                    , pointer :: nplnt 
integer(ipk)                                    , pointer :: nbrliq 
integer(ipk)                                    , pointer :: idoxid 
integer(ipk)                                    , pointer :: lcombk 
integer(ipk)                                    , pointer :: indexgeom 
integer(ipk)                                    , pointer :: kbaln 
integer(ipk)                                    , pointer :: nbuq 
integer(ipk)                                    , pointer :: mtempt 
integer(ipk)                                    , pointer :: maxit 
integer(ipk)                                    , pointer :: nbundl 
integer(ipk)                                    , pointer :: reflo 
integer(ipk)                                    , pointer :: nlac 
integer(ipk)                                    , pointer :: jchf 
integer(ipk)                                    , pointer :: ntco 
integer(ipk)                                    , pointer :: kflgb 
integer(ipk)                                    , pointer :: tape1 
integer(ipk)                                    , pointer :: lengmd 
integer(ipk)                                    , pointer :: tape2 
integer(ipk)                                    , pointer :: nitdt 
integer(ipk)                                    , pointer :: res 
integer(ipk)                                    , pointer :: la1max 
integer(ipk)                                    , pointer :: jtr 
integer(ipk)                                    , pointer :: nqbow 
integer(ipk)                                    , pointer :: idx2 
integer(ipk)                                    , pointer :: geom 
integer(ipk)                                    , pointer :: mhlqcp 
integer(ipk)                                    , pointer :: lowpl 
integer(ipk)                                    , pointer :: la1fil 
integer(ipk)                                    , pointer :: np 
integer(ipk)                                    , pointer :: ifcmi 
integer(ipk)                                    , pointer :: ntempf 
integer(ipk)                                    , pointer :: izadfg 
integer(ipk)                                    , pointer :: npdtpo 
integer(ipk)                                    , pointer :: ncards 
integer(ipk)                                    , pointer :: massfl 
integer(ipk)                                    , pointer :: iterationcount 
integer(ipk)                                    , pointer :: itcntd 
integer(ipk)                                    , pointer :: iagap0 
integer(ipk)                                    , pointer :: odoxid 
integer(ipk)                                    , pointer :: ntstep 
integer(ipk)                                    , pointer :: kswell 
integer(ipk)                                    , pointer :: pressure 
integer(ipk)                                    , pointer :: mhupta 
integer(ipk)                                    , pointer :: ndtred 
integer(ipk)                                    , pointer :: nptha 
integer(ipk)                                    , pointer :: ncorlp 
integer(ipk)                                    , pointer :: bowing 
integer(ipk)                                    , pointer :: transwell 
integer(ipk)                                    , pointer :: unitin 
integer(ipk)                                    , pointer :: indexfc2print 
integer(ipk)                                    , pointer :: nepp0 
integer(ipk)                                    , pointer :: ntimesteps 
integer(ipk)                                    , pointer :: nchan 
integer(ipk)                                    , pointer :: nradialnodes 
integer(ipk)                                    , pointer :: mzq1 
integer(ipk)                                    , pointer :: geometry 
integer(ipk)                                    , pointer :: upppl 
integer(ipk)                                    , pointer :: ruptur 
integer(ipk)                                    , pointer :: geomet 
integer(ipk)                                    , pointer :: nradq 
integer(ipk)                                    , pointer :: lcolct 
integer(ipk)                                    , pointer :: noiter 
integer(ipk)                                    , pointer :: nchfmd 
integer(ipk)                                    , pointer :: nchn 
integer(ipk)                                    , pointer :: lresr2 
integer(ipk)                                    , pointer :: ntimes 
integer(ipk)                                    , pointer :: nfhtc 
integer(ipk)                                    , pointer :: lresr1 
integer(ipk)                                    , pointer :: mpbh 
integer(ipk)                                    , pointer :: ncmesh 
integer(ipk)                                    , pointer :: nsc 
integer(ipk)                                    , pointer :: indxkc 
integer(ipk)                                    , pointer :: ndim 
integer(ipk)                                    , pointer :: naxn 
integer(ipk)                                    , pointer :: lflect 
integer(ipk)                                    , pointer :: ns 
integer(ipk)                                    , pointer :: nradsh 
integer(ipk)                                    , pointer :: lprntb 
integer(ipk)                                    , pointer :: mprest 
integer(ipk)                                    , pointer :: dktoutcounter 
integer(ipk)                                    , pointer :: nsrad3 
integer(ipk)                                    , pointer :: nsf 
integer(ipk)                                    , pointer :: ncooli 
integer(ipk)                                    , pointer :: m6fc2 
integer(ipk)                                    , pointer :: indxpr 
integer(ipk)                                    , pointer :: jmnbal 
integer(ipk)                                    , pointer :: modmw 
integer(ipk)                                    , pointer :: ncard2 
integer(ipk)                                    , pointer :: lsclr1 
integer(ipk)                                    , pointer :: nprad 
integer(ipk)                                    , pointer :: nhtca 
integer(ipk)                                    , pointer :: knonue 
integer(ipk)                                    , pointer :: chf 
integer(ipk)                                    , pointer :: irefine 
integer(ipk)                                    , pointer :: mgbh 
integer(ipk)                                    , pointer :: ndktparams 
integer(ipk)                                    , pointer :: lsclr2 
integer(ipk)                                    , pointer :: lsclr3 
integer(ipk)                                    , pointer :: defsize 
integer(ipk)                                    , pointer :: nflec 
integer(ipk)                                    , pointer :: lblona 
integer(ipk)                                    , pointer :: nodpln 
integer(ipk)                                    , pointer :: nrestart 
integer(ipk)                                    , pointer :: lafrap 
integer(ipk)                                    , pointer :: coreav 
integer(ipk)                                    , pointer :: nfuelswellpairs 
integer(ipk)                                    , pointer :: nswmd 
integer(ipk)                                    , pointer :: nvoid 
integer(ipk)                                    , pointer :: lmatpc 
integer(ipk)                                    , pointer :: inpfil 
integer(ipk)                                    , pointer :: numaxprofiles 
integer(ipk)                                    , pointer :: nkc 
integer(ipk)                                    , pointer :: nomat 
integer(ipk)                                    , pointer :: nkf 
integer(ipk)                                    , pointer :: nfgrpairs 
integer(ipk)                                    , pointer :: nhtclv 
integer(ipk)                                    , pointer :: npaxp 
integer(ipk)                                    , pointer :: ncool2 
integer(ipk)                                    , pointer :: kmxrlp 
integer(ipk)                                    , pointer :: kmxfrp 
integer(ipk)                                    , pointer :: indexbc 
integer(ipk)                                    , pointer :: ncall 
integer(ipk)                                    , pointer :: ndtmax 
integer(ipk)                                    , pointer :: naxialnodes 
integer(ipk)                                    , pointer :: soltyp 
integer(ipk)                                    , pointer :: nptha1 
integer(ipk)                                    , pointer :: ntplot 
integer(ipk)                                    , pointer :: ns2 
integer(ipk)                                    , pointer :: nce 
integer(ipk)                                    , pointer :: modkf 
integer(ipk)                                    , pointer :: mtabl1 
integer(ipk)                                    , pointer :: mtabl2 
integer(ipk)                                    , pointer :: nbalsw 
integer(ipk)                                    , pointer :: ixazim 
integer(ipk)                                    , pointer :: temp 
integer(ipk)                                    , pointer :: indexinittemp 
integer(ipk)                                    , pointer :: cenvoi 
integer(ipk)                                    , pointer :: nrefld 
integer(ipk)                                    , pointer :: ndtad 
integer(ipk)                                    , pointer :: ntabl1 
integer(ipk)                                    , pointer :: nbrfht 
integer(ipk)                                    , pointer :: nqchn 
integer(ipk)                                    , pointer :: klp 
integer(ipk)                                    , pointer :: npid 
integer(ipk)                                    , pointer :: nforlp 
integer(ipk)                                    , pointer :: nswinw 
integer(ipk)                                    , pointer :: grass 
integer(ipk)                                    , pointer :: mpdcay 
integer(ipk)                                    , pointer :: baker 
integer(ipk)                                    , pointer :: spefbz 
integer(ipk)                                    , pointer :: ncolbp 
integer(ipk)                                    , pointer :: nt5 
integer(ipk)                                    , pointer :: mwork 
integer(ipk)                                    , pointer :: nbrpst 
integer(ipk)                                    , pointer :: jpl 
integer(ipk)                                    , pointer :: presfgr 
integer(ipk)                                    , pointer :: mbdl 
integer(ipk)                                    , pointer :: nerr 
integer(ipk)                                    , pointer :: inp 
integer(ipk)                                    , pointer :: iflagn 
integer(ipk)                                    , pointer :: nhinta 
integer(ipk)                                    , pointer :: lthyd 
integer(ipk)                                    , pointer :: indexfintemp 
integer(ipk)                                    , pointer :: nnaz 
integer(ipk)                                    , pointer :: npaxp1 
integer(ipk)                                    , pointer :: nrazpd 
integer(ipk)                                    , pointer :: mflt 
integer(ipk)                                    , pointer :: maximumiterations 
integer(ipk)                                    , pointer :: npramp 
integer(ipk)                                    , pointer :: nzmesh 
integer(ipk)                                    , pointer :: nprofile 
integer(ipk)                                    , pointer :: igpnod 
integer(ipk)                                    , pointer :: coldwa 
integer(ipk)                                    , pointer :: refloodtime 
integer(ipk)                                    , pointer :: ngbh 
integer(ipk)                                    , pointer :: lphypr 
integer(ipk)                                    , pointer :: ifbaln 
integer(ipk)                                    , pointer :: nrest2 
integer(ipk)                                    , pointer :: length 
integer(ipk)                                    , pointer :: nfastf 
integer(ipk)                                    , pointer :: nthermex 
integer(ipk)                                    , pointer :: lhtc 
integer(ipk)                                    , pointer :: naz 
integer(ipk)                                    , pointer :: nvol 
integer(ipk)                                    , pointer :: imox 
integer(ipk)                                    , pointer :: nt 
integer(ipk)                                    , pointer :: maxidx 
integer(ipk)                                    , pointer :: lhtcb 
integer(ipk)                                    , pointer :: collaps 
integer(ipk)                                    , pointer :: profile 
integer(ipk)                                    , pointer :: nucbo 
integer(ipk)                                    , pointer :: filmbo 
integer(ipk)                                    , pointer :: cladtype 
integer(ipk)                                    , pointer :: ndtadv 
integer(ipk)                                    , pointer :: ncladi 
integer(ipk)                                    , pointer :: npaxpf 
integer(ipk)                                    , pointer :: jfb 
integer(ipk)                                    , pointer :: n4 
integer(ipk)                                    , pointer :: nhtc 
integer(ipk)                                    , pointer :: ngasr 
integer(ipk)                                    , pointer :: ithymx 
integer(ipk)                                    , pointer :: ifailue 
integer(ipk)                                    , pointer :: axpow 
integer(ipk)                                    , pointer :: n1 
integer(ipk)                                    , pointer :: nsteadytrans 
integer(ipk)                                    , pointer :: nprntb 
integer(ipk)                                    , pointer :: nbotpl 
integer(ipk)                                    , pointer :: nsym 
integer(ipk)                                    , pointer :: la1req 
integer(ipk)                                    , pointer :: noball 
integer(ipk)                                    , pointer :: kbot 
integer(ipk)                                    , pointer :: n3 
integer(ipk)                                    , pointer :: lresi3 
integer(ipk)                                    , pointer :: lresi2 
integer(ipk)                                    , pointer :: liquid 
integer(ipk)                                    , pointer :: nodprm 
integer(ipk)                                    , pointer :: ngaspr 
integer(ipk)                                    , pointer :: ntabl3 
integer(ipk)                                    , pointer :: ntabl2 
integer(ipk)                                    , pointer :: iflago 
integer(ipk)                                    , pointer :: indx 
integer(ipk)                                    , pointer :: ndktn 
integer(ipk)                                    , pointer :: npltn 
integer(ipk)                                    , pointer :: unitout 
integer(ipk)                                    , pointer :: ifchk 
integer(ipk)                                    , pointer :: plenumtemp 
integer(ipk)                                    , pointer :: lexcb 
integer(ipk)                                    , pointer :: kdbug 
integer(ipk)                                    , pointer :: n2 
integer(ipk)                                    , pointer :: gammait 
integer(ipk)                                    , pointer :: liqnod 
integer(ipk)                                    , pointer :: nhupta 
integer(ipk)                                    , pointer :: irup 
real(r8k)                                       , pointer :: splbp 
real(r8k)                                       , pointer :: tpowf 
real(r8k)                                       , pointer :: kappag 
real(r8k)                                       , pointer :: dtpo 
real(r8k)                                       , pointer :: refdtm 
real(r8k)                                       , pointer :: achn 
real(r8k)                                       , pointer :: prodxe 
real(r8k)                                       , pointer :: pdeint 
real(r8k)                                       , pointer :: zbaln 
real(r8k)                                       , pointer :: tplenb 
real(r8k)                                       , pointer :: tflood 
real(r8k)                                       , pointer :: kappaf 
real(r8k)                                       , pointer :: sigcladthermcond 
real(r8k)                                       , pointer :: pitch 
real(r8k)                                       , pointer :: dcldh 
real(r8k)                                       , pointer :: cnfsol 
real(r8k)                                       , pointer :: gasmoles0 
real(r8k)                                       , pointer :: tfoldf 
real(r8k)                                       , pointer :: bowthr 
real(r8k)                                       , pointer :: hrad 
real(r8k)                                       , pointer :: dofang 
real(r8k)                                       , pointer :: t0f 
real(r8k)                                       , pointer :: pressr 
real(r8k)                                       , pointer :: t0c 
real(r8k)                                       , pointer :: maxenthalpy 
real(r8k)                                       , pointer :: tp1bal 
real(r8k)                                       , pointer :: tmaxf 
real(r8k)                                       , pointer :: tt 
real(r8k)                                       , pointer :: rmpbal 
real(r8k)                                       , pointer :: qlbal 
real(r8k)                                       , pointer :: frden 
real(r8k)                                       , pointer :: vsubg 
real(r8k)                                       , pointer :: vsubf 
real(r8k)                                       , pointer :: dtbal 
real(r8k)                                       , pointer :: delth 
real(r8k)                                       , pointer :: prodkr 
real(r8k)                                       , pointer :: tshtc 
real(r8k)                                       , pointer :: cexh2l 
real(r8k)                                       , pointer :: zpkfc 
real(r8k)                                       , pointer :: htcgba 
real(r8k)                                       , pointer :: frbal 
real(r8k)                                       , pointer :: reflpr 
real(r8k)                                       , pointer :: qbal 
real(r8k)                                       , pointer :: pelh 
real(r8k)                                       , pointer :: arean 
real(r8k)                                       , pointer :: zndbal 
real(r8k)                                       , pointer :: t12 
real(r8k)                                       , pointer :: debugtime 
real(r8k)                                       , pointer :: pavg 
real(r8k)                                       , pointer :: aidsht 
real(r8k)                                       , pointer :: zro 
real(r8k)                                       , pointer :: maxcladtemp 
real(r8k)                                       , pointer :: dtheta 
real(r8k)                                       , pointer :: hsubf 
real(r8k)                                       , pointer :: gapconductivity 
real(r8k)                                       , pointer :: cpmult 
real(r8k)                                       , pointer :: fdelta 
real(r8k)                                       , pointer :: sigfuelthermexp 
real(r8k)                                       , pointer :: totnb 
real(r8k)                                       , pointer :: sigcladthermexp 
real(r8k)                                       , pointer :: timop 
real(r8k)                                       , pointer :: src2 
real(r8k)                                       , pointer :: flowbk 
real(r8k)                                       , pointer :: totalvoidvol 
real(r8k)                                       , pointer :: htcbal 
real(r8k)                                       , pointer :: powop 
real(r8k)                                       , pointer :: tplen 
real(r8k)                                       , pointer :: flxsec 
real(r8k)                                       , pointer :: t2 
real(r8k)                                       , pointer :: t0 
real(r8k)                                       , pointer :: t1 
real(r8k)                                       , pointer :: ffch 
real(r8k)                                       , pointer :: dtenfb 
real(r8k)                                       , pointer :: crf 
real(r8k)                                       , pointer :: fpdcay 
real(r8k)                                       , pointer :: debugtimestop 
real(r8k)                                       , pointer :: roughc 
real(r8k)                                       , pointer :: x 
real(r8k)                                       , pointer :: chstrs 
real(r8k)                                       , pointer :: roughf 
real(r8k)                                       , pointer :: dtenfo 
real(r8k)                                       , pointer :: drod 
real(r8k)                                       , pointer :: jhtc 
real(r8k)                                       , pointer :: hliq 
real(r8k)                                       , pointer :: rfbal 
real(r8k)                                       , pointer :: pctop 
real(r8k)                                       , pointer :: prsacc 
real(r8k)                                       , pointer :: tprlp 
real(r8k)                                       , pointer :: relfract 
real(r8k)                                       , pointer :: fpowr 
real(r8k)                                       , pointer :: gum 
real(r8k)                                       , pointer :: gpthk0 
real(r8k)                                       , pointer :: tref 
real(r8k)                                       , pointer :: ftmelt 
real(r8k)                                       , pointer :: zs 
real(r8k)                                       , pointer :: tc0bal 
real(r8k)                                       , pointer :: tgbal 
real(r8k)                                       , pointer :: apowrd 
real(r8k)                                       , pointer :: rodlength 
real(r8k)                                       , pointer :: psat 
real(r8k)                                       , pointer :: srcv2 
real(r8k)                                       , pointer :: time0 
real(r8k)                                       , pointer :: flxbal 
real(r8k)                                       , pointer :: fuelrd 
real(r8k)                                       , pointer :: zmesh 
real(r8k)                                       , pointer :: rf 
real(r8k)                                       , pointer :: ctmelt 
real(r8k)                                       , pointer :: htflxa 
real(r8k)                                       , pointer :: pdrato 
real(r8k)                                       , pointer :: templo 
real(r8k)                                       , pointer :: rl 
real(r8k)                                       , pointer :: drflt 
real(r8k)                                       , pointer :: tgas0 
real(r8k)                                       , pointer :: swllfr 
real(r8k)                                       , pointer :: gsmfrp 
real(r8k)                                       , pointer :: modheat 
real(r8k)                                       , pointer :: spdbp 
real(r8k)                                       , pointer :: pfail 
real(r8k)                                       , pointer :: cladid 
real(r8k)                                       , pointer :: fldrpr 
real(r8k)                                       , pointer :: nu3 
real(r8k)                                       , pointer :: pmxbal 
real(r8k)                                       , pointer :: nu1 
real(r8k)                                       , pointer :: timmd 
real(r8k)                                       , pointer :: aidtot 
real(r8k)                                       , pointer :: aidsur 
real(r8k)                                       , pointer :: tsub 
real(r8k)                                       , pointer :: sigcladyieldstr 
real(r8k)                                       , pointer :: fabal 
real(r8k)                                       , pointer :: rhoc 
real(r8k)                                       , pointer :: tflux 
real(r8k)                                       , pointer :: rhof 
real(r8k)                                       , pointer :: tdbugd 
real(r8k)                                       , pointer :: sigfuelheatcapa 
real(r8k)                                       , pointer :: deloxy 
real(r8k)                                       , pointer :: r8bal 
real(r8k)                                       , pointer :: z 
real(r8k)                                       , pointer :: delth0 
real(r8k)                                       , pointer :: tmax 
real(r8k)                                       , pointer :: qmax 
real(r8k)                                       , pointer :: chefus 
real(r8k)                                       , pointer :: trabal 
real(r8k)                                       , pointer :: rcpar 
real(r8k)                                       , pointer :: pressi 
real(r8k)                                       , pointer :: t22 
real(r8k)                                       , pointer :: ffchf 
real(r8k)                                       , pointer :: roddiameter 
real(r8k)                                       , pointer :: coolantpress 
real(r8k)                                       , pointer :: ruplev 
real(r8k)                                       , pointer :: epsht1 
real(r8k)                                       , pointer :: h0bal 
real(r8k)                                       , pointer :: tmaxc 
real(r8k)                                       , pointer :: zad 
real(r8k)                                       , pointer :: tranfuelswell 
real(r8k)                                       , pointer :: csubp 
real(r8k)                                       , pointer :: cladpower 
real(r8k)                                       , pointer :: src1 
real(r8k)                                       , pointer :: rshrd 
real(r8k)                                       , pointer :: dppowi 
real(r8k)                                       , pointer :: zqch 
real(r8k)                                       , pointer :: frcoef 
real(r8k)                                       , pointer :: empytm 
real(r8k)                                       , pointer :: zroug2 
real(r8k)                                       , pointer :: frchmx 
real(r8k)                                       , pointer :: tbkbal 
real(r8k)                                       , pointer :: timbal 
real(r8k)                                       , pointer :: pcbal 
real(r8k)                                       , pointer :: plenumgasmoles 
real(r8k)                                       , pointer :: xtime 
real(r8k)                                       , pointer :: doffst 
real(r8k)                                       , pointer :: hsubg 
real(r8k)                                       , pointer :: powimx 
real(r8k)                                       , pointer :: buoxide 
real(r8k)                                       , pointer :: fltgap 
real(r8k)                                       , pointer :: psbal 
real(r8k)                                       , pointer :: emptm 
real(r8k)                                       , pointer :: dvdtp 
real(r8k)                                       , pointer :: tm1bal 
real(r8k)                                       , pointer :: trise 
real(r8k)                                       , pointer :: usubf 
real(r8k)                                       , pointer :: usubg 
real(r8k)                                       , pointer :: fltgap2 
real(r8k)                                       , pointer :: hydiam 
real(r8k)                                       , pointer :: dishd 
real(r8k)                                       , pointer :: beta1 
real(r8k)                                       , pointer :: cldwdc 
real(r8k)                                       , pointer :: cpstem 
real(r8k)                                       , pointer :: tplot 
real(r8k)                                       , pointer :: csubpg 
real(r8k)                                       , pointer :: csubpf 
real(r8k)                                       , pointer :: tfavba 
real(r8k)                                       , pointer :: fotmtl 
real(r8k)                                       , pointer :: timeincrement 
real(r8k)                                       , pointer :: tmpac1 
real(r8k)                                       , pointer :: sigfuelthermcond 
real(r8k)                                       , pointer :: crfpr 
real(r8k)                                       , pointer :: sigsurfhtc 
real(r8k)                                       , pointer :: trecrd 
real(r8k)                                       , pointer :: emwbal 
real(r8k)                                       , pointer :: cladod 
real(r8k)                                       , pointer :: ruptstrain 
real(r8k)                                       , pointer :: zqchpr 
real(r8k)                                       , pointer :: tsntrk 
real(r8k)                                       , pointer :: rsntr 
real(r8k)                                       , pointer :: nu2 
real(r8k)                                       , pointer :: volavegastemp 
real(r8k)                                       , pointer :: zroug1 
real(r8k)                                       , pointer :: aidlng 
real(r8k)                                       , pointer :: rhostm 
real(r8k)                                       , pointer :: ph 
real(r8k)                                       , pointer :: coldbp 
real(r8k)                                       , pointer :: tempmx 
real(r8k)                                       , pointer :: betag 
real(r8k)                                       , pointer :: betaf 
real(r8k)                                       , pointer :: kappa 
real(r8k)                                       , pointer :: cfluxa 
real(r8k)                                       , pointer :: dtold 
real(r8k)                                       , pointer :: zvoid1 
real(r8k)                                       , pointer :: atop 
real(r8k)                                       , pointer :: rvoid 
real(r8k)                                       , pointer :: qaxpk 
real(r8k)                                       , pointer :: dofset 
real(r8k)                                       , pointer :: tf0bal 
real(r8k)                                       , pointer :: ubar 
real(r8k)                                       , pointer :: pl 
real(r8k)                                       , pointer :: v 
real(r8k)                                       , pointer :: tdkt 
real(r8k)                                       , pointer :: dtp 
real(r8k)                                       , pointer :: totalgasmoles 
real(r8k)                                       , pointer :: refine 
real(r8k)                                       , pointer :: gflow 
real(r8k)                                       , pointer :: gfloa1 
real(r8k)                                       , pointer :: temphi 
real(r8k)                                       , pointer :: frchdt 
real(r8k)                                       , pointer :: fhtc 
real(r8k)                                       , pointer :: frpo2 
real(r8k)                                       , pointer :: trest 
real(r8k)                                       , pointer :: fgrns 
real(r8k)                                       , pointer :: dtmpcl 
real(r8k)                                       , pointer :: areao 
real(r8k)                                       , pointer :: pfnuc 
real(r8k)                                       , pointer :: r0bal 
real(r8k)                                       , pointer :: fuelpeldiam 
real(r8k)                                       , pointer :: tsatt 
real(r8k)                                       , pointer :: pdecy 
real(r8k)                                       , pointer :: ztmax 
real(r8k)                                       , pointer :: zqflt 
real(r8k)                                       , pointer :: fldrte 
real(r8k)                                       , pointer :: coldw 
real(r8k)                                       , pointer :: openporosityfraction 
real(r8k)                                       , pointer :: dhe 
real(r8k)                                       , pointer :: explenumv 
real(r8k)                                       , pointer :: compmt 
real(r8k)                                       , pointer :: maxgaspressure 
real(r8k)                                       , pointer :: dtss 
real(r8k)                                       , pointer :: oldtim 
real(r8k)                                       , pointer :: dhy 
real(r8k)                                       , pointer :: tc1 
real(r8k)                                       , pointer :: volbp 
real(r8k)                                       , pointer :: bup 
real(r8k)                                       , pointer :: fhefus 
real(r8k)                                       , pointer :: powict 
real(r8k)                                       , pointer :: fr 
real(r8k)                                       , pointer :: pois 
real(r8k)                                       , pointer :: dtobal 
real(r8k)                                       , pointer :: dcldh0 
real(r8k)                                       , pointer :: pavgft 
real(r8k)                                       , pointer :: tc2 
real(r8k)                                       , pointer :: gsms 
real(r8k)                                       , pointer :: time 
real(r8k)                                       , pointer :: taxbal 
real(r8k)                                       , pointer :: trodfc 
real(r8k)                                       , pointer :: dishv0 
real(r8k)                                       , pointer :: beta 
real(r8k)                                       , pointer :: tpo 
real(r8k)                                       , pointer :: cladtk 
real(r8k)                                       , pointer :: zm1bal 
real(r8k)                                       , pointer :: dzmesh 
real(r8k)                                       , pointer :: toldfc 
real(r8k)                                       , pointer :: rnbnt 
real(r8k)                                       , pointer :: defaulttemp 
real(r8k)                                       , pointer :: hbar 
real(r8k)                                       , pointer :: e 
real(r8k)                                       , pointer :: gapthk 
real(r8k)                                       , pointer :: zbot 
real(r8k)                                       , pointer :: vplenc 
real(r8k)                                       , pointer :: vplenb 
real(r8k)                                       , pointer :: bottomplenumgasmoles 
real(r8k)                                       , pointer :: bumtp 
real(r8k)                                       , pointer :: zvoid2 
real(r8k)                                       , pointer :: zp1bal 
real(r8k)                                       , pointer :: fqcrit 
real(r8k)                                       , pointer :: rshd 
real(r8k)                                       , pointer :: tcebal 
real(r8k)                                       , pointer :: tempcs 
real(r8k)                                       , pointer :: maxfueltemp 
real(r8k)                                       , pointer :: pfflec 
real(r8k)                                       , pointer :: gapmin 
real(r8k)                                       , pointer :: cnfliq 
character(len=8)          , dimension(:)        , pointer :: emflag 
integer(ipk)              , dimension(:)        , pointer :: ifstor 
integer(ipk)              , dimension(:)        , pointer :: ichf 
integer(ipk)              , dimension(:)        , pointer :: nswpm 
integer(ipk)              , dimension(:)        , pointer :: tem 
integer(ipk)              , dimension(:)        , pointer :: imaterials 
integer(ipk)              , dimension(:)        , pointer :: modfal 
integer(ipk)              , dimension(:)        , pointer :: idumr1 
integer(ipk)              , dimension(:)        , pointer :: ih 
integer(ipk)              , dimension(:)        , pointer :: iffrp 
integer(ipk)              , dimension(:)        , pointer :: ihtreg 
integer(ipk)              , dimension(:)        , pointer :: indxjk 
integer(ipk)              , dimension(:)        , pointer :: ibnopt 
integer(ipk)              , dimension(:)        , pointer :: ruptfailindex 
integer(ipk)              , dimension(:)        , pointer :: nm 
integer(ipk)              , dimension(:)        , pointer :: ies 
integer(ipk)              , dimension(:)        , pointer :: nopt 
integer(ipk)              , dimension(:)        , pointer :: indexpcmionce 
integer(ipk)              , dimension(:)        , pointer :: cladcollapseindex 
integer(ipk)              , dimension(:)        , pointer :: rodfailindex 
integer(ipk)              , dimension(:)        , pointer :: gapindex 
integer(ipk)              , dimension(:)        , pointer :: irest2 
integer(ipk)              , dimension(:)        , pointer :: imatflag 
integer(ipk)              , dimension(:)        , pointer :: oldgapindex 
integer(ipk)              , dimension(:)        , pointer :: htco 
integer(ipk)              , dimension(:)        , pointer :: indextempconverg 
integer(ipk)              , dimension(:)        , pointer :: bosgapindex 
integer(ipk)              , dimension(:)        , pointer :: idatapairs 
integer(ipk)              , dimension(:)        , pointer :: ncs 
integer(ipk)              , dimension(:)        , pointer :: nvprs 
integer(ipk)              , dimension(:)        , pointer :: npprs 
integer(ipk)              , dimension(:)        , pointer :: irest3 
integer(ipk)              , dimension(:)        , pointer :: bosgapindex0 
integer(ipk)              , dimension(:)        , pointer :: ntprs 
integer(ipk)              , dimension(:)        , pointer :: ihdata 
integer(ipk)              , dimension(:)        , pointer :: imatflag1 
integer(ipk)              , dimension(:)        , pointer :: cladcollindx0 
integer(ipk)              , dimension(:)        , pointer :: nunopt 
integer(ipk)              , dimension(:)        , pointer :: nhprs 
integer(ipk)              , dimension(:)        , pointer :: nodesintertemp 
integer(ipk)              , dimension(:)        , pointer :: ngastmp 
integer(ipk)              , dimension(:)        , pointer :: ndchfi 
integer(ipk)              , dimension(:)        , pointer :: iheattablen 
integer(ipk)              , dimension(:)        , pointer :: indexpcmi 
integer(ipk)              , dimension(:)        , pointer :: numazmuthnod 
real(r8k)                 , dimension(:)        , pointer :: roi 
real(r8k)                 , dimension(:)        , pointer :: alphathk11 
real(r8k)                 , dimension(:)        , pointer :: tbulk_v 
real(r8k)                 , dimension(:)        , pointer :: cladenrgperl 
real(r8k)                 , dimension(:)        , pointer :: efffastflustrencoef 
real(r8k)                 , dimension(:)        , pointer :: scd 
real(r8k)                 , dimension(:)        , pointer :: watrmetlenrgy 
real(r8k)                 , dimension(:)        , pointer :: coolpress 
real(r8k)                 , dimension(:)        , pointer :: dtplta 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaio 
real(r8k)                 , dimension(:)        , pointer :: apln 
real(r8k)                 , dimension(:)        , pointer :: oxygenuptake 
real(r8k)                 , dimension(:)        , pointer :: thermalconductadv 
real(r8k)                 , dimension(:)        , pointer :: ascal2 
real(r8k)                 , dimension(:)        , pointer :: gasax 
real(r8k)                 , dimension(:)        , pointer :: profilestarttime 
real(r8k)                 , dimension(:)        , pointer :: ascal1 
real(r8k)                 , dimension(:)        , pointer :: htflxfac 
real(r8k)                 , dimension(:)        , pointer :: htclev 
real(r8k)                 , dimension(:)        , pointer :: tbulk_l 
real(r8k)                 , dimension(:)        , pointer :: prop 
real(r8k)                 , dimension(:)        , pointer :: oxconcenado 
real(r8k)                 , dimension(:)        , pointer :: flwblk 
real(r8k)                 , dimension(:)        , pointer :: tempfc 
real(r8k)                 , dimension(:)        , pointer :: oxconcenadi 
real(r8k)                 , dimension(:)        , pointer :: rinterfacgap 
real(r8k)                 , dimension(:)        , pointer :: stressatinststrain 
real(r8k)                 , dimension(:)        , pointer :: rinterfacprs0 
real(r8k)                 , dimension(:)        , pointer :: ts 
real(r8k)                 , dimension(:)        , pointer :: tp 
real(r8k)                 , dimension(:)        , pointer :: claddingpower 
real(r8k)                 , dimension(:)        , pointer :: oldcoolprs0 
real(r8k)                 , dimension(:)        , pointer :: voidvolumeratio 
real(r8k)                 , dimension(:)        , pointer :: techf 
real(r8k)                 , dimension(:)        , pointer :: eosoxidethick 
real(r8k)                 , dimension(:)        , pointer :: cexh2a 
real(r8k)                 , dimension(:)        , pointer :: pelraddeviat 
real(r8k)                 , dimension(:)        , pointer :: coolenthalpy0 
real(r8k)                 , dimension(:)        , pointer :: epstrain1old 
real(r8k)                 , dimension(:)        , pointer :: azpang 
real(r8k)                 , dimension(:)        , pointer :: coolmassflx 
real(r8k)                 , dimension(:)        , pointer :: hmaxinitial 
real(r8k)                 , dimension(:)        , pointer :: betathickness 
real(r8k)                 , dimension(:)        , pointer :: oxstalphathkremain 
real(r8k)                 , dimension(:)        , pointer :: powave 
real(r8k)                 , dimension(:)        , pointer :: axburnup 
real(r8k)                 , dimension(:)        , pointer :: apln0 
real(r8k)                 , dimension(:)        , pointer :: alphathk1 
real(r8k)                 , dimension(:)        , pointer :: amatpc 
real(r8k)                 , dimension(:)        , pointer :: fdial 
real(r8k)                 , dimension(:)        , pointer :: vrlfrp 
real(r8k)                 , dimension(:)        , pointer :: rinterfacprs 
real(r8k)                 , dimension(:)        , pointer :: energyperl 
real(r8k)                 , dimension(:)        , pointer :: dvdt 
real(r8k)                 , dimension(:)        , pointer :: effcoldwkstrnhardexp 
real(r8k)                 , dimension(:)        , pointer :: gapthick0 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaci 
real(r8k)                 , dimension(:)        , pointer :: gbse 
real(r8k)                 , dimension(:)        , pointer :: boundarycondition 
real(r8k)                 , dimension(:)        , pointer :: fmgp 
real(r8k)                 , dimension(:)        , pointer :: oldcoolprs 
real(r8k)                 , dimension(:)        , pointer :: sshgap 
real(r8k)                 , dimension(:)        , pointer :: workspacetcmx 
real(r8k)                 , dimension(:)        , pointer :: sedpnnlold 
real(r8k)                 , dimension(:)        , pointer :: heatflux 
real(r8k)                 , dimension(:)        , pointer :: table2 
real(r8k)                 , dimension(:)        , pointer :: table3 
real(r8k)                 , dimension(:)        , pointer :: gadoln 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaco 
real(r8k)                 , dimension(:)        , pointer :: gfrac 
real(r8k)                 , dimension(:)        , pointer :: radialpower 
real(r8k)                 , dimension(:)        , pointer :: stressaxialold 
real(r8k)                 , dimension(:)        , pointer :: surfhtflux 
real(r8k)                 , dimension(:)        , pointer :: oldcldplasstrn 
real(r8k)                 , dimension(:)        , pointer :: gbh 
real(r8k)                 , dimension(:)        , pointer :: prevtemp 
real(r8k)                 , dimension(:)        , pointer :: workspacereloc 
real(r8k)                 , dimension(:)        , pointer :: pelletrad0 
real(r8k)                 , dimension(:)        , pointer :: rmrlp 
real(r8k)                 , dimension(:)        , pointer :: hz2 
real(r8k)                 , dimension(:)        , pointer :: fastflux 
real(r8k)                 , dimension(:)        , pointer :: bbht1 
real(r8k)                 , dimension(:)        , pointer :: pbh 
real(r8k)                 , dimension(:)        , pointer :: gaptemp 
real(r8k)                 , dimension(:)        , pointer :: arest1 
real(r8k)                 , dimension(:)        , pointer :: dvdt0 
real(r8k)                 , dimension(:)        , pointer :: drdfrp 
real(r8k)                 , dimension(:)        , pointer :: oldgasprs 
real(r8k)                 , dimension(:)        , pointer :: fuelgasswell 
real(r8k)                 , dimension(:)        , pointer :: oxuptakeid2 
real(r8k)                 , dimension(:)        , pointer :: stresshoopold 
real(r8k)                 , dimension(:)        , pointer :: effcoldwkstrencoef 
real(r8k)                 , dimension(:)        , pointer :: oxconcenago 
real(r8k)                 , dimension(:)        , pointer :: rodod0 
real(r8k)                 , dimension(:)        , pointer :: dtmaxa 
real(r8k)                 , dimension(:)        , pointer :: pelletrad 
real(r8k)                 , dimension(:)        , pointer :: edint 
real(r8k)                 , dimension(:)        , pointer :: alphathk2 
real(r8k)                 , dimension(:)        , pointer :: axialpowr 
real(r8k)                 , dimension(:)        , pointer :: pelsrfdispl 
real(r8k)                 , dimension(:)        , pointer :: hgapav 
real(r8k)                 , dimension(:)        , pointer :: tmelt 
real(r8k)                 , dimension(:)        , pointer :: temptm 
real(r8k)                 , dimension(:)        , pointer :: enthl 
real(r8k)                 , dimension(:)        , pointer :: flthit 
real(r8k)                 , dimension(:)        , pointer :: bpln 
real(r8k)                 , dimension(:)        , pointer :: cladmaxt 
real(r8k)                 , dimension(:)        , pointer :: qaxzq 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaii 
real(r8k)                 , dimension(:)        , pointer :: vfrad1 
real(r8k)                 , dimension(:)        , pointer :: vfrad3 
real(r8k)                 , dimension(:)        , pointer :: vfrad2 
real(r8k)                 , dimension(:)        , pointer :: oxconcenafo 
real(r8k)                 , dimension(:)        , pointer :: tend 
real(r8k)                 , dimension(:)        , pointer :: aexcb 
real(r8k)                 , dimension(:)        , pointer :: sedepri 
real(r8k)                 , dimension(:)        , pointer :: alphathk22 
real(r8k)                 , dimension(:)        , pointer :: oxithk1 
real(r8k)                 , dimension(:)        , pointer :: bufrp 
real(r8k)                 , dimension(:)        , pointer :: hz1 
real(r8k)                 , dimension(:)        , pointer :: swelldispl 
real(r8k)                 , dimension(:)        , pointer :: oxithk2 
real(r8k)                 , dimension(:)        , pointer :: tshrda 
real(r8k)                 , dimension(:)        , pointer :: vs0 
real(r8k)                 , dimension(:)        , pointer :: acoold 
real(r8k)                 , dimension(:)        , pointer :: hgpfrp 
real(r8k)                 , dimension(:)        , pointer :: pbh2 
real(r8k)                 , dimension(:)        , pointer :: hlqcl 
real(r8k)                 , dimension(:)        , pointer :: finaltemp 
real(r8k)                 , dimension(:)        , pointer :: techfi 
real(r8k)                 , dimension(:)        , pointer :: pbh1 
real(r8k)                 , dimension(:)        , pointer :: aflcht 
real(r8k)                 , dimension(:)        , pointer :: bu 
real(r8k)                 , dimension(:)        , pointer :: ecr 
real(r8k)                 , dimension(:)        , pointer :: vreloc0 
real(r8k)                 , dimension(:)        , pointer :: epstrain2old 
real(r8k)                 , dimension(:)        , pointer :: hlqclp 
real(r8k)                 , dimension(:)        , pointer :: bpln0 
real(r8k)                 , dimension(:)        , pointer :: pz2 
real(r8k)                 , dimension(:)        , pointer :: cesiumcontent 
real(r8k)                 , dimension(:)        , pointer :: z1 
real(r8k)                 , dimension(:)        , pointer :: z2 
real(r8k)                 , dimension(:)        , pointer :: faxzq 
real(r8k)                 , dimension(:)        , pointer :: fluxz 
real(r8k)                 , dimension(:)        , pointer :: gapthick 
real(r8k)                 , dimension(:)        , pointer :: hbh 
real(r8k)                 , dimension(:)        , pointer :: oldepeff 
real(r8k)                 , dimension(:)        , pointer :: oldeffstrainpnnl 
real(r8k)                 , dimension(:)        , pointer :: cladavetemp 
real(r8k)                 , dimension(:)        , pointer :: nodchf 
real(r8k)                 , dimension(:)        , pointer :: oldgasprs0 
real(r8k)                 , dimension(:)        , pointer :: aasth 
real(r8k)                 , dimension(:)        , pointer :: aprntb 
real(r8k)                 , dimension(:)        , pointer :: volopenpor 
real(r8k)                 , dimension(:)        , pointer :: farbal 
real(r8k)                 , dimension(:)        , pointer :: extentofbow 
real(r8k)                 , dimension(:)        , pointer :: oldcldaxstrn0 
real(r8k)                 , dimension(:)        , pointer :: sdfar 
real(r8k)                 , dimension(:)        , pointer :: ablona 
real(r8k)                 , dimension(:)        , pointer :: cladeffstress 
real(r8k)                 , dimension(:)        , pointer :: rodavepower 
real(r8k)                 , dimension(:)        , pointer :: gasmolesax0 
real(r8k)                 , dimension(:)        , pointer :: cooldensity0 
real(r8k)                 , dimension(:)        , pointer :: ascal3 
real(r8k)                 , dimension(:)        , pointer :: arrayf 
real(r8k)                 , dimension(:)        , pointer :: oldpeldis 
real(r8k)                 , dimension(:)        , pointer :: edotfz 
real(r8k)                 , dimension(:)        , pointer :: workspacerco 
real(r8k)                 , dimension(:)        , pointer :: workspaceep1 
real(r8k)                 , dimension(:)        , pointer :: cldpermstrn 
real(r8k)                 , dimension(:)        , pointer :: hydrogencontent 
real(r8k)                 , dimension(:)        , pointer :: gasfraction 
real(r8k)                 , dimension(:)        , pointer :: tz2 
real(r8k)                 , dimension(:)        , pointer :: workspacerci 
real(r8k)                 , dimension(:)        , pointer :: hfusn 
real(r8k)                 , dimension(:)        , pointer :: prcntsatbeta 
real(r8k)                 , dimension(:)        , pointer :: qpln0 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaai 
real(r8k)                 , dimension(:)        , pointer :: oldpeldis0 
real(r8k)                 , dimension(:)        , pointer :: flowg 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaao 
real(r8k)                 , dimension(:)        , pointer :: iodinecontent 
real(r8k)                 , dimension(:)        , pointer :: oldfuelaxstrn 
real(r8k)                 , dimension(:)        , pointer :: terfacepres 
real(r8k)                 , dimension(:)        , pointer :: axnodelevat 
real(r8k)                 , dimension(:)        , pointer :: cladsurft 
real(r8k)                 , dimension(:)        , pointer :: oxconcenabi 
real(r8k)                 , dimension(:)        , pointer :: cladyieldstress 
real(r8k)                 , dimension(:)        , pointer :: oxideid 
real(r8k)                 , dimension(:)        , pointer :: swd 
real(r8k)                 , dimension(:)        , pointer :: tschfi 
real(r8k)                 , dimension(:)        , pointer :: strainrateterm 
real(r8k)                 , dimension(:)        , pointer :: hsolold 
real(r8k)                 , dimension(:)        , pointer :: gasmolesax 
real(r8k)                 , dimension(:)        , pointer :: hfxsum 
real(r8k)                 , dimension(:)        , pointer :: pclrlp 
real(r8k)                 , dimension(:)        , pointer :: elevatthermhydr 
real(r8k)                 , dimension(:)        , pointer :: timeofgbsep 
real(r8k)                 , dimension(:)        , pointer :: effstrain0 
real(r8k)                 , dimension(:)        , pointer :: oxideod 
real(r8k)                 , dimension(:)        , pointer :: coolqual 
real(r8k)                 , dimension(:)        , pointer :: crithtflux 
real(r8k)                 , dimension(:)        , pointer :: axialnodlen 
real(r8k)                 , dimension(:)        , pointer :: oxconcenahi 
real(r8k)                 , dimension(:)        , pointer :: effstrain 
real(r8k)                 , dimension(:)        , pointer :: filmcoeffav 
real(r8k)                 , dimension(:)        , pointer :: workspacepint 
real(r8k)                 , dimension(:)        , pointer :: sedpnnl 
real(r8k)                 , dimension(:)        , pointer :: heatflux0 
real(r8k)                 , dimension(:)        , pointer :: rmassflux 
real(r8k)                 , dimension(:)        , pointer :: qmaxmelt 
real(r8k)                 , dimension(:)        , pointer :: cldpermhoopstrn 
real(r8k)                 , dimension(:)        , pointer :: stressradialold 
real(r8k)                 , dimension(:)        , pointer :: fuelsrfstrrat 
real(r8k)                 , dimension(:)        , pointer :: cladeffstressold 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaeo 
real(r8k)                 , dimension(:)        , pointer :: cracktemp 
real(r8k)                 , dimension(:)        , pointer :: thermalconductivity 
real(r8k)                 , dimension(:)        , pointer :: fldrat 
real(r8k)                 , dimension(:)        , pointer :: qmaxmeltp1 
real(r8k)                 , dimension(:)        , pointer :: pgpfrp 
real(r8k)                 , dimension(:)        , pointer :: radpel 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaei 
real(r8k)                 , dimension(:)        , pointer :: coefm 
real(r8k)                 , dimension(:)        , pointer :: coefn 
real(r8k)                 , dimension(:)        , pointer :: coefk 
real(r8k)                 , dimension(:)        , pointer :: openportemp 
real(r8k)                 , dimension(:)        , pointer :: rodod 
real(r8k)                 , dimension(:)        , pointer :: fuelsrfstrrat0 
real(r8k)                 , dimension(:)        , pointer :: deltox 
real(r8k)                 , dimension(:)        , pointer :: vreloc 
real(r8k)                 , dimension(:)        , pointer :: cldpermaxstrn 
real(r8k)                 , dimension(:)        , pointer :: fmesh 
real(r8k)                 , dimension(:)        , pointer :: pelprm 
real(r8k)                 , dimension(:)        , pointer :: workspacegap 
real(r8k)                 , dimension(:)        , pointer :: cmesh 
real(r8k)                 , dimension(:)        , pointer :: gsmol0 
real(r8k)                 , dimension(:)        , pointer :: openporvol 
real(r8k)                 , dimension(:)        , pointer :: bowrat 
real(r8k)                 , dimension(:)        , pointer :: gappr0 
real(r8k)                 , dimension(:)        , pointer :: vplen 
real(r8k)                 , dimension(:)        , pointer :: arraye 
real(r8k)                 , dimension(:)        , pointer :: openporosity 
real(r8k)                 , dimension(:)        , pointer :: centvoidvol 
real(r8k)                 , dimension(:)        , pointer :: bulkcooltemp 
real(r8k)                 , dimension(:)        , pointer :: crackvolume 
real(r8k)                 , dimension(:)        , pointer :: butemp 
real(r8k)                 , dimension(:)        , pointer :: hydrostatpress 
real(r8k)                 , dimension(:)        , pointer :: gasphs 
real(r8k)                 , dimension(:)        , pointer :: hlqcl1 
real(r8k)                 , dimension(:)        , pointer :: dtdkta 
real(r8k)                 , dimension(:)        , pointer :: gaspress0 
real(r8k)                 , dimension(:)        , pointer :: pelsrfstrn0 
real(r8k)                 , dimension(:)        , pointer :: workspaceurc 
real(r8k)                 , dimension(:)        , pointer :: hupta 
real(r8k)                 , dimension(:)        , pointer :: spl 
real(r8k)                 , dimension(:)        , pointer :: oxuptakeid1 
real(r8k)                 , dimension(:)        , pointer :: fuelsurft 
real(r8k)                 , dimension(:)        , pointer :: aaht1 
real(r8k)                 , dimension(:)        , pointer :: oxconcenaho 
real(r8k)                 , dimension(:)        , pointer :: eppinp 
real(r8k)                 , dimension(:)        , pointer :: ispace 
real(r8k)                 , dimension(:)        , pointer :: pswll0 
real(r8k)                 , dimension(:)        , pointer :: bosoxidethick 
real(r8k)                 , dimension(:)        , pointer :: oxconcenagi 
real(r8k)                 , dimension(:)        , pointer :: dishvperl 
real(r8k)                 , dimension(:)        , pointer :: qpln 
real(r8k)                 , dimension(:)        , pointer :: sedepriold 
real(r8k)                 , dimension(:)        , pointer :: radialbound 
real(r8k)                 , dimension(:)        , pointer :: explenumt 
real(r8k)                 , dimension(:)        , pointer :: crackwidth 
real(r8k)                 , dimension(:)        , pointer :: edotfz0 
real(r8k)                 , dimension(:)        , pointer :: coolenthalpy 
real(r8k)                 , dimension(:)        , pointer :: dtpoa 
real(r8k)                 , dimension(:)        , pointer :: oldcladt 
real(r8k)                 , dimension(:)        , pointer :: vsn 
real(r8k)                 , dimension(:)        , pointer :: ureloc 
real(r8k)                 , dimension(:)        , pointer :: afrap 
real(r8k)                 , dimension(:)        , pointer :: efffastflustrnhardexp 
real(r8k)                 , dimension(:)        , pointer :: epstrain3old 
real(r8k)                 , dimension(:)        , pointer :: gaspress 
real(r8k)                 , dimension(:)        , pointer :: radpowprofile 
real(r8k)                 , dimension(:)        , pointer :: relfraca 
real(r8k)                 , dimension(:)        , pointer :: effstrainpnnl 
real(r8k)                 , dimension(:)        , pointer :: tz1 
real(r8k)                 , dimension(:)        , pointer :: emodulus 
real(r8k)                 , dimension(:)        , pointer :: einstabilstrain 
real(r8k)                 , dimension(:)        , pointer :: tschf 
real(r8k)                 , dimension(:)        , pointer :: elvrad 
real(r8k)                 , dimension(:)        , pointer :: oldcladt0 
real(r8k)                 , dimension(:)        , pointer :: oldfuelaxstrn0 
real(r8k)                 , dimension(:)        , pointer :: ansr 
real(r8k)                 , dimension(:)        , pointer :: gfint 
real(r8k)                 , dimension(:)        , pointer :: pz1 
real(r8k)                 , dimension(:)        , pointer :: axialpowr0 
real(r8k)                 , dimension(:)        , pointer :: oxygenconcenave 
real(r8k)                 , dimension(:)        , pointer :: a1 
real(r8k)                 , dimension(:)        , pointer :: zfarbl 
real(r8k)                 , dimension(:)        , pointer :: fuelcentert 
real(r8k)                 , dimension(:)        , pointer :: workspacegapi 
real(r8k)                 , dimension(:)        , pointer :: oldeps 
real(r8k)                 , dimension(:)        , pointer :: hinta 
real(r8k)                 , dimension(:)        , pointer :: oxstalphathknearfuel 
real(r8k)                 , dimension(:)        , pointer :: oxconcenabo 
real(r8k)                 , dimension(:)        , pointer :: avedishtemp 
real(r8k)                 , dimension(:)        , pointer :: gz2 
real(r8k)                 , dimension(:)        , pointer :: gz1 
real(r8k)                 , dimension(:)        , pointer :: oldcldaxstrn 
real(r8k)                 , dimension(:)        , pointer :: cooldensity 
real(r8k)                 , dimension(:)        , pointer :: prestm 
real(r8k)                 , dimension(:)        , pointer :: cldpermstrn0 
real(r8k)                 , dimension(:)        , pointer :: elevatfrap 
real(r8k)                 , dimension(:)        , pointer :: pinput 
real(r8k)                 , dimension(:)        , pointer :: htc_v 
real(r8k)                 , dimension(:)        , pointer :: potput 
real(r8k)                 , dimension(:)        , pointer :: oxconcenafi 
real(r8k)                 , dimension(:)        , pointer :: acolct 
real(r8k)                 , dimension(:)        , pointer :: zelev 
real(r8k)                 , dimension(:)        , pointer :: gadolin 
real(r8k)                 , dimension(:)        , pointer :: htc_l 
real(r8k)                 , dimension(:)        , pointer :: table1 
real(r8k)                 , dimension(:)        , pointer :: oxipowergen 
integer(ipk)              , dimension(:,:)      , pointer :: icglob 
integer(ipk)              , dimension(:,:)      , pointer :: nrc 
integer(ipk)              , dimension(:,:)      , pointer :: iokold 
integer(ipk)              , dimension(:,:)      , pointer :: iok 
real(r8k)                 , dimension(:,:)      , pointer :: gasavail1 
real(r8k)                 , dimension(:,:)      , pointer :: tmprlp 
real(r8k)                 , dimension(:,:)      , pointer :: gasavail2 
real(r8k)                 , dimension(:,:)      , pointer :: volumeweightr 
real(r8k)                 , dimension(:,:)      , pointer :: enrgymeltz 
real(r8k)                 , dimension(:,:)      , pointer :: acond 
real(r8k)                 , dimension(:,:)      , pointer :: cldthermstrn 
real(r8k)                 , dimension(:,:)      , pointer :: frapcontemp 
real(r8k)                 , dimension(:,:)      , pointer :: baln2twall 
real(r8k)                 , dimension(:,:)      , pointer :: qcold 
real(r8k)                 , dimension(:,:)      , pointer :: fuelresidstrn 
real(r8k)                 , dimension(:,:)      , pointer :: tplbt1 
real(r8k)                 , dimension(:,:)      , pointer :: previteratetemp 
real(r8k)                 , dimension(:,:)      , pointer :: buradv 
real(r8k)                 , dimension(:,:)      , pointer :: rhocp0 
real(r8k)                 , dimension(:,:)      , pointer :: tblka 
real(r8k)                 , dimension(:,:)      , pointer :: burad 
real(r8k)                 , dimension(:,:)      , pointer :: htca 
real(r8k)                 , dimension(:,:)      , pointer :: enrgymelt 
real(r8k)                 , dimension(:,:)      , pointer :: cldstrnrat 
real(r8k)                 , dimension(:,:)      , pointer :: deformedradiusofmesh 
real(r8k)                 , dimension(:,:)      , pointer :: bosrad 
real(r8k)                 , dimension(:,:)      , pointer :: pelsrfstrn 
real(r8k)                 , dimension(:,:)      , pointer :: eosrad 
real(r8k)                 , dimension(:,:)      , pointer :: radsrc 
real(r8k)                 , dimension(:,:)      , pointer :: cldplasstrn0 
real(r8k)                 , dimension(:,:)      , pointer :: cldstress 
real(r8k)                 , dimension(:,:)      , pointer :: rhocp 
real(r8k)                 , dimension(:,:)      , pointer :: radialboundo 
real(r8k)                 , dimension(:,:)      , pointer :: volumeweightl 
real(r8k)                 , dimension(:,:)      , pointer :: axpowprofile 
real(r8k)                 , dimension(:,:)      , pointer :: enrgymeltzp1 
real(r8k)                 , dimension(:,:)      , pointer :: pazp 
real(r8k)                 , dimension(:,:)      , pointer :: tpln 
real(r8k)                 , dimension(:,:)      , pointer :: radpow 
real(r8k)                 , dimension(:,:)      , pointer :: areaweight 
real(r8k)                 , dimension(:,:)      , pointer :: eostemp 
real(r8k)                 , dimension(:,:)      , pointer :: pchn 
real(r8k)                 , dimension(:,:)      , pointer :: trad2 
real(r8k)                 , dimension(:,:)      , pointer :: trad3 
real(r8k)                 , dimension(:,:)      , pointer :: trad1 
real(r8k)                 , dimension(:,:)      , pointer :: gasths 
real(r8k)                 , dimension(:,:)      , pointer :: fuelrad 
real(r8k)                 , dimension(:,:)      , pointer :: cldresidstrn0 
real(r8k)                 , dimension(:,:)      , pointer :: dktouts 
real(r8k)                 , dimension(:,:)      , pointer :: cldstrnrat0 
real(r8k)                 , dimension(:,:)      , pointer :: cldplasstrn 
real(r8k)                 , dimension(:,:)      , pointer :: bufrad 
real(r8k)                 , dimension(:,:)      , pointer :: radsrco 
real(r8k)                 , dimension(:,:)      , pointer :: cldresidstrn 
real(r8k)                 , dimension(:,:)      , pointer :: radtemp 
real(r8k)                 , dimension(:,:)      , pointer :: workspaceepp1 
real(r8k)                 , dimension(:,:)      , pointer :: fuelresidstrn0 
real(r8k)                 , dimension(:,:)      , pointer :: cldelstrn 
real(r8k)                 , dimension(:,:)      , pointer :: enrgymeltp1 
real(r8k)                 , dimension(:,:)      , pointer :: tplbot 
real(r8k)                 , dimension(:,:)      , pointer :: bostemp 
real(r8k)                 , dimension(:,:)      , pointer :: cldstrn 
real(r8k)                 , dimension(:,:,:)    , pointer :: grsv 
real(r8k)                 , dimension(:,:,:)    , pointer :: tplna 
