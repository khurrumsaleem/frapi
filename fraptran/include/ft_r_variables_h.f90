logical                                                                :: r__convert_units 
logical                                                                :: r__printballoon 
logical                                                                :: r__printrodburst 
logical                                                                :: r__allocate_arrays_ft 
logical                                                                :: r__coupled 
logical                                                                :: r__ndebug 
logical                                                                :: r__dakota 
logical                                                                :: r__setunits 
logical                                                                :: r__first_pass 
logical                                                                :: r__first_call 
logical                                                                :: r__unit 
character(len=8)                                                       :: r__rupflg 
character(len=3)                                                       :: r__coolant 
character(len=80)                                                      :: r__fraptranfn 
character(len=3)                                                       :: r__metal 
character(len=15)                                                      :: r__modheatmodel 
character(len=12)                                                      :: r__relocmodel 
character(len=8)                                                       :: r__lodmrk 
character(len=3)                                                       :: r__reflood 
character(len=200)                                                     :: r__namerf 
character(len=10)                                                      :: r__inst 
character(len=3)                                                       :: r__bheat 
character(len=3)                                                       :: r__internal 
character(len=3)                                                       :: r__mheat 
character(len=3)                                                       :: r__radiation 
character(len=3)                                                       :: r__deformation 
integer(ipk)                                                           :: r__ifaila 
integer(ipk)                                                           :: r__kaxnlo 
integer(ipk)                                                           :: r__ncirlp 
integer(ipk)                                                           :: r__nprsw 
integer(ipk)                                                           :: r__npbh 
integer(ipk)                                                           :: r__nbncal 
integer(ipk)                                                           :: r__llp 
integer(ipk)                                                           :: r__ierr 
integer(ipk)                                                           :: r__lcoold 
integer(ipk)                                                           :: r__nidoxide 
integer(ipk)                                                           :: r__press 
integer(ipk)                                                           :: r__cathca 
integer(ipk)                                                           :: r__klp2 
integer(ipk)                                                           :: r__nelrad 
integer(ipk)                                                           :: r__frapt4 
integer(ipk)                                                           :: r__ntapou 
integer(ipk)                                                           :: r__kaxhtc 
integer(ipk)                                                           :: r__indexthermcon 
integer(ipk)                                                           :: r__prestmp 
integer(ipk)                                                           :: r__irupt 
integer(ipk)                                                           :: r__indxmd 
integer(ipk)                                                           :: r__nbowr 
integer(ipk)                                                           :: r__ldialb 
integer(ipk)                                                           :: r__modbal 
integer(ipk)                                                           :: r__pressu 
integer(ipk)                                                           :: r__inlet 
integer(ipk)                                                           :: r__zone 
integer(ipk)                                                           :: r__ndap1 
integer(ipk)                                                           :: r__nbhtc 
integer(ipk)                                                           :: r__protectiveoxide 
integer(ipk)                                                           :: r__rtheta 
integer(ipk)                                                           :: r__ldir 
integer(ipk)                                                           :: r__istoicgrad 
integer(ipk)                                                           :: r__ifail 
integer(ipk)                                                           :: r__lchf 
integer(ipk)                                                           :: r__itswt 
integer(ipk)                                                           :: r__pow 
integer(ipk)                                                           :: r__nsymm 
integer(ipk)                                                           :: r__naazp 
integer(ipk)                                                           :: r__nvol2 
integer(ipk)                                                           :: r__idebug 
integer(ipk)                                                           :: r__nvol1 
integer(ipk)                                                           :: r__mhbh 
integer(ipk)                                                           :: r__modfd 
integer(ipk)                                                           :: r__ixazmn 
integer(ipk)                                                           :: r__gasflo 
integer(ipk)                                                           :: r__nbrtmp 
integer(ipk)                                                           :: r__nbrfdr 
integer(ipk)                                                           :: r__prescri 
integer(ipk)                                                           :: r__mbowr 
integer(ipk)                                                           :: r__mtabl3 
integer(ipk)                                                           :: r__mechan 
integer(ipk)                                                           :: r__nfmesh 
integer(ipk)                                                           :: r__mndchf 
integer(ipk)                                                           :: r__naazpd 
integer(ipk)                                                           :: r__npelraddeviat 
integer(ipk)                                                           :: r__itdmx 
integer(ipk)                                                           :: r__nhbh 
integer(ipk)                                                           :: r__nfrapconinitialization 
integer(ipk)                                                           :: r__ncool 
integer(ipk)                                                           :: r__nsrad3 
integer(ipk)                                                           :: r__indexgrainbndsep 
integer(ipk)                                                           :: r__nedtsw 
integer(ipk)                                                           :: r__azang 
integer(ipk)                                                           :: r__m5fc2 
integer(ipk)                                                           :: r__radiat 
integer(ipk)                                                           :: r__mhinta 
integer(ipk)                                                           :: r__luout 
integer(ipk)                                                           :: r__nmesh 
integer(ipk)                                                           :: r__mgaspr 
integer(ipk)                                                           :: r__iagap 
integer(ipk)                                                           :: r__indxkf 
integer(ipk)                                                           :: r__nitmin 
integer(ipk)                                                           :: r__nconsw 
integer(ipk)                                                           :: r__numdta 
integer(ipk)                                                           :: r__nhtcz 
integer(ipk)                                                           :: r__kntbal 
integer(ipk)                                                           :: r__nrazp 
integer(ipk)                                                           :: r__iplant 
integer(ipk)                                                           :: r__indexthermconadv 
integer(ipk)                                                           :: r__mhlqc1 
integer(ipk)                                                           :: r__nplnt 
integer(ipk)                                                           :: r__nbrliq 
integer(ipk)                                                           :: r__idoxid 
integer(ipk)                                                           :: r__lcombk 
integer(ipk)                                                           :: r__indexgeom 
integer(ipk)                                                           :: r__kbaln 
integer(ipk)                                                           :: r__nbuq 
integer(ipk)                                                           :: r__mtempt 
integer(ipk)                                                           :: r__maxit 
integer(ipk)                                                           :: r__nbundl 
integer(ipk)                                                           :: r__reflo 
integer(ipk)                                                           :: r__nlac 
integer(ipk)                                                           :: r__jchf 
integer(ipk)                                                           :: r__ntco 
integer(ipk)                                                           :: r__kflgb 
integer(ipk)                                                           :: r__tape1 
integer(ipk)                                                           :: r__lengmd 
integer(ipk)                                                           :: r__tape2 
integer(ipk)                                                           :: r__nitdt 
integer(ipk)                                                           :: r__res 
integer(ipk)                                                           :: r__la1max 
integer(ipk)                                                           :: r__jtr 
integer(ipk)                                                           :: r__nqbow 
integer(ipk)                                                           :: r__idx2 
integer(ipk)                                                           :: r__geom 
integer(ipk)                                                           :: r__mhlqcp 
integer(ipk)                                                           :: r__lowpl 
integer(ipk)                                                           :: r__la1fil 
integer(ipk)                                                           :: r__np 
integer(ipk)                                                           :: r__ifcmi 
integer(ipk)                                                           :: r__ntempf 
integer(ipk)                                                           :: r__izadfg 
integer(ipk)                                                           :: r__npdtpo 
integer(ipk)                                                           :: r__ncards 
integer(ipk)                                                           :: r__massfl 
integer(ipk)                                                           :: r__iterationcount 
integer(ipk)                                                           :: r__itcntd 
integer(ipk)                                                           :: r__iagap0 
integer(ipk)                                                           :: r__odoxid 
integer(ipk)                                                           :: r__ntstep 
integer(ipk)                                                           :: r__kswell 
integer(ipk)                                                           :: r__pressure 
integer(ipk)                                                           :: r__mhupta 
integer(ipk)                                                           :: r__ndtred 
integer(ipk)                                                           :: r__nptha 
integer(ipk)                                                           :: r__ncorlp 
integer(ipk)                                                           :: r__bowing 
integer(ipk)                                                           :: r__transwell 
integer(ipk)                                                           :: r__unitin 
integer(ipk)                                                           :: r__indexfc2print 
integer(ipk)                                                           :: r__nepp0 
integer(ipk)                                                           :: r__ntimesteps 
integer(ipk)                                                           :: r__nchan 
integer(ipk)                                                           :: r__nradialnodes 
integer(ipk)                                                           :: r__mzq1 
integer(ipk)                                                           :: r__geometry 
integer(ipk)                                                           :: r__upppl 
integer(ipk)                                                           :: r__ruptur 
integer(ipk)                                                           :: r__geomet 
integer(ipk)                                                           :: r__nradq 
integer(ipk)                                                           :: r__lcolct 
integer(ipk)                                                           :: r__noiter 
integer(ipk)                                                           :: r__nchfmd 
integer(ipk)                                                           :: r__nchn 
integer(ipk)                                                           :: r__lresr2 
integer(ipk)                                                           :: r__ntimes 
integer(ipk)                                                           :: r__nfhtc 
integer(ipk)                                                           :: r__lresr1 
integer(ipk)                                                           :: r__mpbh 
integer(ipk)                                                           :: r__ncmesh 
integer(ipk)                                                           :: r__nsc 
integer(ipk)                                                           :: r__indxkc 
integer(ipk)                                                           :: r__ndim 
integer(ipk)                                                           :: r__naxn 
integer(ipk)                                                           :: r__lflect 
integer(ipk)                                                           :: r__ns 
integer(ipk)                                                           :: r__nradsh 
integer(ipk)                                                           :: r__lprntb 
integer(ipk)                                                           :: r__mprest 
integer(ipk)                                                           :: r__dktoutcounter 
integer(ipk)                                                           :: r__nsf 
integer(ipk)                                                           :: r__ncooli 
integer(ipk)                                                           :: r__m6fc2 
integer(ipk)                                                           :: r__indxpr 
integer(ipk)                                                           :: r__jmnbal 
integer(ipk)                                                           :: r__modmw 
integer(ipk)                                                           :: r__ncard2 
integer(ipk)                                                           :: r__lsclr1 
integer(ipk)                                                           :: r__nprad 
integer(ipk)                                                           :: r__nhtca 
integer(ipk)                                                           :: r__knonue 
integer(ipk)                                                           :: r__chf 
integer(ipk)                                                           :: r__irefine 
integer(ipk)                                                           :: r__mgbh 
integer(ipk)                                                           :: r__ndktparams 
integer(ipk)                                                           :: r__lsclr2 
integer(ipk)                                                           :: r__lsclr3 
integer(ipk)                                                           :: r__defsize 
integer(ipk)                                                           :: r__nflec 
integer(ipk)                                                           :: r__lblona 
integer(ipk)                                                           :: r__nodpln 
integer(ipk)                                                           :: r__nrestart 
integer(ipk)                                                           :: r__lafrap 
integer(ipk)                                                           :: r__coreav 
integer(ipk)                                                           :: r__nfuelswellpairs 
integer(ipk)                                                           :: r__nswmd 
integer(ipk)                                                           :: r__nvoid 
integer(ipk)                                                           :: r__lmatpc 
integer(ipk)                                                           :: r__inpfil 
integer(ipk)                                                           :: r__numaxprofiles 
integer(ipk)                                                           :: r__nkc 
integer(ipk)                                                           :: r__nomat 
integer(ipk)                                                           :: r__nkf 
integer(ipk)                                                           :: r__nfgrpairs 
integer(ipk)                                                           :: r__nhtclv 
integer(ipk)                                                           :: r__npaxp 
integer(ipk)                                                           :: r__ncool2 
integer(ipk)                                                           :: r__kmxrlp 
integer(ipk)                                                           :: r__kmxfrp 
integer(ipk)                                                           :: r__indexbc 
integer(ipk)                                                           :: r__ncall 
integer(ipk)                                                           :: r__ndtmax 
integer(ipk)                                                           :: r__naxialnodes 
integer(ipk)                                                           :: r__soltyp 
integer(ipk)                                                           :: r__nptha1 
integer(ipk)                                                           :: r__ntplot 
integer(ipk)                                                           :: r__ns2 
integer(ipk)                                                           :: r__nce 
integer(ipk)                                                           :: r__modkf 
integer(ipk)                                                           :: r__mtabl1 
integer(ipk)                                                           :: r__mtabl2 
integer(ipk)                                                           :: r__nbalsw 
integer(ipk)                                                           :: r__ixazim 
integer(ipk)                                                           :: r__temp 
integer(ipk)                                                           :: r__indexinittemp 
integer(ipk)                                                           :: r__cenvoi 
integer(ipk)                                                           :: r__nrefld 
integer(ipk)                                                           :: r__ndtad 
integer(ipk)                                                           :: r__ntabl1 
integer(ipk)                                                           :: r__nbrfht 
integer(ipk)                                                           :: r__nqchn 
integer(ipk)                                                           :: r__klp 
integer(ipk)                                                           :: r__npid 
integer(ipk)                                                           :: r__nforlp 
integer(ipk)                                                           :: r__nswinw 
integer(ipk)                                                           :: r__grass 
integer(ipk)                                                           :: r__mpdcay 
integer(ipk)                                                           :: r__baker 
integer(ipk)                                                           :: r__spefbz 
integer(ipk)                                                           :: r__ncolbp 
integer(ipk)                                                           :: r__nt5 
integer(ipk)                                                           :: r__mwork 
integer(ipk)                                                           :: r__nbrpst 
integer(ipk)                                                           :: r__jpl 
integer(ipk)                                                           :: r__presfgr 
integer(ipk)                                                           :: r__mbdl 
integer(ipk)                                                           :: r__nerr 
integer(ipk)                                                           :: r__inp 
integer(ipk)                                                           :: r__iflagn 
integer(ipk)                                                           :: r__nhinta 
integer(ipk)                                                           :: r__lthyd 
integer(ipk)                                                           :: r__indexfintemp 
integer(ipk)                                                           :: r__nnaz 
integer(ipk)                                                           :: r__npaxp1 
integer(ipk)                                                           :: r__nrazpd 
integer(ipk)                                                           :: r__mflt 
integer(ipk)                                                           :: r__maximumiterations 
integer(ipk)                                                           :: r__npramp 
integer(ipk)                                                           :: r__nzmesh 
integer(ipk)                                                           :: r__nprofile 
integer(ipk)                                                           :: r__igpnod 
integer(ipk)                                                           :: r__npair 
integer(ipk)                                                           :: r__coldwa 
integer(ipk)                                                           :: r__refloodtime 
integer(ipk)                                                           :: r__ngbh 
integer(ipk)                                                           :: r__lphypr 
integer(ipk)                                                           :: r__ifbaln 
integer(ipk)                                                           :: r__nrest2 
integer(ipk)                                                           :: r__length 
integer(ipk)                                                           :: r__nfastf 
integer(ipk)                                                           :: r__nthermex 
integer(ipk)                                                           :: r__lhtc 
integer(ipk)                                                           :: r__naz 
integer(ipk)                                                           :: r__nvol 
integer(ipk)                                                           :: r__imox 
integer(ipk)                                                           :: r__nt 
integer(ipk)                                                           :: r__maxidx 
integer(ipk)                                                           :: r__lhtcb 
integer(ipk)                                                           :: r__collaps 
integer(ipk)                                                           :: r__profile 
integer(ipk)                                                           :: r__nucbo 
integer(ipk)                                                           :: r__filmbo 
integer(ipk)                                                           :: r__cladtype 
integer(ipk)                                                           :: r__ndtadv 
integer(ipk)                                                           :: r__ncladi 
integer(ipk)                                                           :: r__npaxpf 
integer(ipk)                                                           :: r__jfb 
integer(ipk)                                                           :: r__n4 
integer(ipk)                                                           :: r__nhtc 
integer(ipk)                                                           :: r__ngasr 
integer(ipk)                                                           :: r__ithymx 
integer(ipk)                                                           :: r__ifailue 
integer(ipk)                                                           :: r__axpow 
integer(ipk)                                                           :: r__n1 
integer(ipk)                                                           :: r__nsteadytrans 
integer(ipk)                                                           :: r__nprntb 
integer(ipk)                                                           :: r__nbotpl 
integer(ipk)                                                           :: r__nsym 
integer(ipk)                                                           :: r__la1req 
integer(ipk)                                                           :: r__noball 
integer(ipk)                                                           :: r__kbot 
integer(ipk)                                                           :: r__n3 
integer(ipk)                                                           :: r__lresi3 
integer(ipk)                                                           :: r__lresi2 
integer(ipk)                                                           :: r__liquid 
integer(ipk)                                                           :: r__nodprm 
integer(ipk)                                                           :: r__ngaspr 
integer(ipk)                                                           :: r__ntabl3 
integer(ipk)                                                           :: r__ntabl2 
integer(ipk)                                                           :: r__iflago 
integer(ipk)                                                           :: r__indx 
integer(ipk)                                                           :: r__ndktn 
integer(ipk)                                                           :: r__npltn 
integer(ipk)                                                           :: r__unitout 
integer(ipk)                                                           :: r__ifchk 
integer(ipk)                                                           :: r__plenumtemp 
integer(ipk)                                                           :: r__lexcb 
integer(ipk)                                                           :: r__kdbug 
integer(ipk)                                                           :: r__n2 
integer(ipk)                                                           :: r__gammait 
integer(ipk)                                                           :: r__liqnod 
integer(ipk)                                                           :: r__nhupta 
integer(ipk)                                                           :: r__irup 
real(r8k)                                                              :: r__splbp 
real(r8k)                                                              :: r__tpowf 
real(r8k)                                                              :: r__kappag 
real(r8k)                                                              :: r__dtpo 
real(r8k)                                                              :: r__refdtm 
real(r8k)                                                              :: r__achn 
real(r8k)                                                              :: r__prodxe 
real(r8k)                                                              :: r__pdeint 
real(r8k)                                                              :: r__zbaln 
real(r8k)                                                              :: r__tplenb 
real(r8k)                                                              :: r__tflood 
real(r8k)                                                              :: r__kappaf 
real(r8k)                                                              :: r__sigcladthermcond 
real(r8k)                                                              :: r__pitch 
real(r8k)                                                              :: r__dcldh 
real(r8k)                                                              :: r__cnfsol 
real(r8k)                                                              :: r__gasmoles0 
real(r8k)                                                              :: r__tfoldf 
real(r8k)                                                              :: r__bowthr 
real(r8k)                                                              :: r__hrad 
real(r8k)                                                              :: r__dofang 
real(r8k)                                                              :: r__t0f 
real(r8k)                                                              :: r__pressr 
real(r8k)                                                              :: r__t0c 
real(r8k)                                                              :: r__maxenthalpy 
real(r8k)                                                              :: r__tp1bal 
real(r8k)                                                              :: r__tmaxf 
real(r8k)                                                              :: r__tt 
real(r8k)                                                              :: r__rmpbal 
real(r8k)                                                              :: r__qlbal 
real(r8k)                                                              :: r__frden 
real(r8k)                                                              :: r__vsubg 
real(r8k)                                                              :: r__vsubf 
real(r8k)                                                              :: r__dtbal 
real(r8k)                                                              :: r__delth 
real(r8k)                                                              :: r__prodkr 
real(r8k)                                                              :: r__tshtc 
real(r8k)                                                              :: r__cexh2l 
real(r8k)                                                              :: r__zpkfc 
real(r8k)                                                              :: r__htcgba 
real(r8k)                                                              :: r__frbal 
real(r8k)                                                              :: r__reflpr 
real(r8k)                                                              :: r__qbal 
real(r8k)                                                              :: r__pelh 
real(r8k)                                                              :: r__arean 
real(r8k)                                                              :: r__zndbal 
real(r8k)                                                              :: r__t12 
real(r8k)                                                              :: r__debugtime 
real(r8k)                                                              :: r__pavg 
real(r8k)                                                              :: r__aidsht 
real(r8k)                                                              :: r__rl 
real(r8k)                                                              :: r__maxcladtemp 
real(r8k)                                                              :: r__dtheta 
real(r8k)                                                              :: r__hsubf 
real(r8k)                                                              :: r__gapconductivity 
real(r8k)                                                              :: r__cpmult 
real(r8k)                                                              :: r__fdelta 
real(r8k)                                                              :: r__sigfuelthermexp 
real(r8k)                                                              :: r__totnb 
real(r8k)                                                              :: r__sigcladthermexp 
real(r8k)                                                              :: r__timop 
real(r8k)                                                              :: r__src2 
real(r8k)                                                              :: r__flowbk 
real(r8k)                                                              :: r__totalvoidvol 
real(r8k)                                                              :: r__htcbal 
real(r8k)                                                              :: r__powop 
real(r8k)                                                              :: r__tplen 
real(r8k)                                                              :: r__flxsec 
real(r8k)                                                              :: r__t2 
real(r8k)                                                              :: r__t0 
real(r8k)                                                              :: r__t1 
real(r8k)                                                              :: r__ffch 
real(r8k)                                                              :: r__dtenfb 
real(r8k)                                                              :: r__crf 
real(r8k)                                                              :: r__fpdcay 
real(r8k)                                                              :: r__debugtimestop 
real(r8k)                                                              :: r__roughc 
real(r8k)                                                              :: r__x 
real(r8k)                                                              :: r__chstrs 
real(r8k)                                                              :: r__roughf 
real(r8k)                                                              :: r__dtenfo 
real(r8k)                                                              :: r__drod 
real(r8k)                                                              :: r__jhtc 
real(r8k)                                                              :: r__hliq 
real(r8k)                                                              :: r__rfbal 
real(r8k)                                                              :: r__pctop 
real(r8k)                                                              :: r__prsacc 
real(r8k)                                                              :: r__tprlp 
real(r8k)                                                              :: r__relfract 
real(r8k)                                                              :: r__fpowr 
real(r8k)                                                              :: r__gum 
real(r8k)                                                              :: r__gpthk0 
real(r8k)                                                              :: r__tref 
real(r8k)                                                              :: r__ftmelt 
real(r8k)                                                              :: r__zs 
real(r8k)                                                              :: r__tc0bal 
real(r8k)                                                              :: r__tgbal 
real(r8k)                                                              :: r__apowrd 
real(r8k)                                                              :: r__rodlength 
real(r8k)                                                              :: r__psat 
real(r8k)                                                              :: r__srcv2 
real(r8k)                                                              :: r__time0 
real(r8k)                                                              :: r__flxbal 
real(r8k)                                                              :: r__fuelrd 
real(r8k)                                                              :: r__zmesh 
real(r8k)                                                              :: r__rf 
real(r8k)                                                              :: r__ctmelt 
real(r8k)                                                              :: r__htflxa 
real(r8k)                                                              :: r__pdrato 
real(r8k)                                                              :: r__templo 
real(r8k)                                                              :: r__zro 
real(r8k)                                                              :: r__drflt 
real(r8k)                                                              :: r__tgas0 
real(r8k)                                                              :: r__swllfr 
real(r8k)                                                              :: r__gsmfrp 
real(r8k)                                                              :: r__modheat 
real(r8k)                                                              :: r__spdbp 
real(r8k)                                                              :: r__pfail 
real(r8k)                                                              :: r__cladid 
real(r8k)                                                              :: r__fldrpr 
real(r8k)                                                              :: r__nu3 
real(r8k)                                                              :: r__pmxbal 
real(r8k)                                                              :: r__nu1 
real(r8k)                                                              :: r__timmd 
real(r8k)                                                              :: r__aidtot 
real(r8k)                                                              :: r__aidsur 
real(r8k)                                                              :: r__tsub 
real(r8k)                                                              :: r__sigcladyieldstr 
real(r8k)                                                              :: r__fabal 
real(r8k)                                                              :: r__rhoc 
real(r8k)                                                              :: r__tflux 
real(r8k)                                                              :: r__rhof 
real(r8k)                                                              :: r__tdbugd 
real(r8k)                                                              :: r__sigfuelheatcapa 
real(r8k)                                                              :: r__deloxy 
real(r8k)                                                              :: r__r8bal 
real(r8k)                                                              :: r__z 
real(r8k)                                                              :: r__delth0 
real(r8k)                                                              :: r__tmax 
real(r8k)                                                              :: r__qmax 
real(r8k)                                                              :: r__chefus 
real(r8k)                                                              :: r__trabal 
real(r8k)                                                              :: r__rcpar 
real(r8k)                                                              :: r__pressi 
real(r8k)                                                              :: r__t22 
real(r8k)                                                              :: r__ffchf 
real(r8k)                                                              :: r__roddiameter 
real(r8k)                                                              :: r__coolantpress 
real(r8k)                                                              :: r__ruplev 
real(r8k)                                                              :: r__epsht1 
real(r8k)                                                              :: r__h0bal 
real(r8k)                                                              :: r__tmaxc 
real(r8k)                                                              :: r__zad 
real(r8k)                                                              :: r__tranfuelswell 
real(r8k)                                                              :: r__csubp 
real(r8k)                                                              :: r__cladpower 
real(r8k)                                                              :: r__src1 
real(r8k)                                                              :: r__rshrd 
real(r8k)                                                              :: r__dppowi 
real(r8k)                                                              :: r__zqch 
real(r8k)                                                              :: r__frcoef 
real(r8k)                                                              :: r__empytm 
real(r8k)                                                              :: r__zroug2 
real(r8k)                                                              :: r__frchmx 
real(r8k)                                                              :: r__tbkbal 
real(r8k)                                                              :: r__timbal 
real(r8k)                                                              :: r__pcbal 
real(r8k)                                                              :: r__plenumgasmoles 
real(r8k)                                                              :: r__xtime 
real(r8k)                                                              :: r__doffst 
real(r8k)                                                              :: r__hsubg 
real(r8k)                                                              :: r__powimx 
real(r8k)                                                              :: r__buoxide 
real(r8k)                                                              :: r__fltgap 
real(r8k)                                                              :: r__psbal 
real(r8k)                                                              :: r__emptm 
real(r8k)                                                              :: r__dvdtp 
real(r8k)                                                              :: r__tm1bal 
real(r8k)                                                              :: r__trise 
real(r8k)                                                              :: r__usubf 
real(r8k)                                                              :: r__usubg 
real(r8k)                                                              :: r__fltgap2 
real(r8k)                                                              :: r__hydiam 
real(r8k)                                                              :: r__dishd 
real(r8k)                                                              :: r__beta1 
real(r8k)                                                              :: r__cldwdc 
real(r8k)                                                              :: r__cpstem 
real(r8k)                                                              :: r__tplot 
real(r8k)                                                              :: r__csubpg 
real(r8k)                                                              :: r__csubpf 
real(r8k)                                                              :: r__tfavba 
real(r8k)                                                              :: r__fotmtl 
real(r8k)                                                              :: r__timeincrement 
real(r8k)                                                              :: r__tmpac1 
real(r8k)                                                              :: r__sigfuelthermcond 
real(r8k)                                                              :: r__crfpr 
real(r8k)                                                              :: r__sigsurfhtc 
real(r8k)                                                              :: r__trecrd 
real(r8k)                                                              :: r__emwbal 
real(r8k)                                                              :: r__cladod 
real(r8k)                                                              :: r__ruptstrain 
real(r8k)                                                              :: r__zqchpr 
real(r8k)                                                              :: r__tsntrk 
real(r8k)                                                              :: r__rsntr 
real(r8k)                                                              :: r__nu2 
real(r8k)                                                              :: r__volavegastemp 
real(r8k)                                                              :: r__zroug1 
real(r8k)                                                              :: r__aidlng 
real(r8k)                                                              :: r__rhostm 
real(r8k)                                                              :: r__ph 
real(r8k)                                                              :: r__coldbp 
real(r8k)                                                              :: r__tempmx 
real(r8k)                                                              :: r__betag 
real(r8k)                                                              :: r__betaf 
real(r8k)                                                              :: r__kappa 
real(r8k)                                                              :: r__cfluxa 
real(r8k)                                                              :: r__dtold 
real(r8k)                                                              :: r__zvoid1 
real(r8k)                                                              :: r__atop 
real(r8k)                                                              :: r__rvoid 
real(r8k)                                                              :: r__qaxpk 
real(r8k)                                                              :: r__dofset 
real(r8k)                                                              :: r__tf0bal 
real(r8k)                                                              :: r__ubar 
real(r8k)                                                              :: r__pl 
real(r8k)                                                              :: r__v 
real(r8k)                                                              :: r__tdkt 
real(r8k)                                                              :: r__dtp 
real(r8k)                                                              :: r__totalgasmoles 
real(r8k)                                                              :: r__refine 
real(r8k)                                                              :: r__gflow 
real(r8k)                                                              :: r__gfloa1 
real(r8k)                                                              :: r__temphi 
real(r8k)                                                              :: r__frchdt 
real(r8k)                                                              :: r__fhtc 
real(r8k)                                                              :: r__frpo2 
real(r8k)                                                              :: r__trest 
real(r8k)                                                              :: r__fgrns 
real(r8k)                                                              :: r__dtmpcl 
real(r8k)                                                              :: r__areao 
real(r8k)                                                              :: r__pfnuc 
real(r8k)                                                              :: r__r0bal 
real(r8k)                                                              :: r__fuelpeldiam 
real(r8k)                                                              :: r__tsatt 
real(r8k)                                                              :: r__pdecy 
real(r8k)                                                              :: r__ztmax 
real(r8k)                                                              :: r__zqflt 
real(r8k)                                                              :: r__fldrte 
real(r8k)                                                              :: r__coldw 
real(r8k)                                                              :: r__openporosityfraction 
real(r8k)                                                              :: r__dhe 
real(r8k)                                                              :: r__explenumv 
real(r8k)                                                              :: r__compmt 
real(r8k)                                                              :: r__maxgaspressure 
real(r8k)                                                              :: r__dtss 
real(r8k)                                                              :: r__oldtim 
real(r8k)                                                              :: r__dhy 
real(r8k)                                                              :: r__tc1 
real(r8k)                                                              :: r__volbp 
real(r8k)                                                              :: r__bup 
real(r8k)                                                              :: r__fhefus 
real(r8k)                                                              :: r__powict 
real(r8k)                                                              :: r__fr 
real(r8k)                                                              :: r__pois 
real(r8k)                                                              :: r__dtobal 
real(r8k)                                                              :: r__dcldh0 
real(r8k)                                                              :: r__pavgft 
real(r8k)                                                              :: r__tc2 
real(r8k)                                                              :: r__gsms 
real(r8k)                                                              :: r__time 
real(r8k)                                                              :: r__taxbal 
real(r8k)                                                              :: r__trodfc 
real(r8k)                                                              :: r__dishv0 
real(r8k)                                                              :: r__beta 
real(r8k)                                                              :: r__tpo 
real(r8k)                                                              :: r__cladtk 
real(r8k)                                                              :: r__zm1bal 
real(r8k)                                                              :: r__dzmesh 
real(r8k)                                                              :: r__toldfc 
real(r8k)                                                              :: r__rnbnt 
real(r8k)                                                              :: r__defaulttemp 
real(r8k)                                                              :: r__hbar 
real(r8k)                                                              :: r__e 
real(r8k)                                                              :: r__gapthk 
real(r8k)                                                              :: r__zbot 
real(r8k)                                                              :: r__vplenc 
real(r8k)                                                              :: r__vplenb 
real(r8k)                                                              :: r__bottomplenumgasmoles 
real(r8k)                                                              :: r__bumtp 
real(r8k)                                                              :: r__zvoid2 
real(r8k)                                                              :: r__zp1bal 
real(r8k)                                                              :: r__fqcrit 
real(r8k)                                                              :: r__rshd 
real(r8k)                                                              :: r__tcebal 
real(r8k)                                                              :: r__tempcs 
real(r8k)                                                              :: r__maxfueltemp 
real(r8k)                                                              :: r__pfflec 
real(r8k)                                                              :: r__gapmin 
real(r8k)                                                              :: r__cnfliq 
character(len=8)          , dimension(:)        , allocatable          :: r__emflag 
integer(ipk)              , dimension(:)        , allocatable          :: r__ifstor 
integer(ipk)              , dimension(:)        , allocatable          :: r__ichf 
integer(ipk)              , dimension(:)        , allocatable          :: r__nswpm 
integer(ipk)              , dimension(:)        , allocatable          :: r__tem 
integer(ipk)              , dimension(:)        , allocatable          :: r__imaterials 
integer(ipk)              , dimension(:)        , allocatable          :: r__modfal 
integer(ipk)              , dimension(:)        , allocatable          :: r__idumr1 
integer(ipk)              , dimension(:)        , allocatable          :: r__ih 
integer(ipk)              , dimension(:)        , allocatable          :: r__iffrp 
integer(ipk)              , dimension(:)        , allocatable          :: r__ihtreg 
integer(ipk)              , dimension(:)        , allocatable          :: r__indxjk 
integer(ipk)              , dimension(:)        , allocatable          :: r__ibnopt 
integer(ipk)              , dimension(:)        , allocatable          :: r__ruptfailindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__nm 
integer(ipk)              , dimension(:)        , allocatable          :: r__ies 
integer(ipk)              , dimension(:)        , allocatable          :: r__nopt 
integer(ipk)              , dimension(:)        , allocatable          :: r__indexpcmionce 
integer(ipk)              , dimension(:)        , allocatable          :: r__cladcollapseindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__rodfailindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__gapindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__irest2 
integer(ipk)              , dimension(:)        , allocatable          :: r__imatflag 
integer(ipk)              , dimension(:)        , allocatable          :: r__oldgapindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__htco 
integer(ipk)              , dimension(:)        , allocatable          :: r__indextempconverg 
integer(ipk)              , dimension(:)        , allocatable          :: r__bosgapindex 
integer(ipk)              , dimension(:)        , allocatable          :: r__idatapairs 
integer(ipk)              , dimension(:)        , allocatable          :: r__ncs 
integer(ipk)              , dimension(:)        , allocatable          :: r__nvprs 
integer(ipk)              , dimension(:)        , allocatable          :: r__npprs 
integer(ipk)              , dimension(:)        , allocatable          :: r__irest3 
integer(ipk)              , dimension(:)        , allocatable          :: r__bosgapindex0 
integer(ipk)              , dimension(:)        , allocatable          :: r__ntprs 
integer(ipk)              , dimension(:)        , allocatable          :: r__ihdata 
integer(ipk)              , dimension(:)        , allocatable          :: r__imatflag1 
integer(ipk)              , dimension(:)        , allocatable          :: r__cladcollindx0 
integer(ipk)              , dimension(:)        , allocatable          :: r__nunopt 
integer(ipk)              , dimension(:)        , allocatable          :: r__nhprs 
integer(ipk)              , dimension(:)        , allocatable          :: r__nodesintertemp 
integer(ipk)              , dimension(:)        , allocatable          :: r__ngastmp 
integer(ipk)              , dimension(:)        , allocatable          :: r__ndchfi 
integer(ipk)              , dimension(:)        , allocatable          :: r__iheattablen 
integer(ipk)              , dimension(:)        , allocatable          :: r__indexpcmi 
integer(ipk)              , dimension(:)        , allocatable          :: r__numazmuthnod 
real(r8k)                 , dimension(:)        , allocatable          :: r__roi 
real(r8k)                 , dimension(:)        , allocatable          :: r__alphathk11 
real(r8k)                 , dimension(:)        , allocatable          :: r__tbulk_v 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladenrgperl 
real(r8k)                 , dimension(:)        , allocatable          :: r__efffastflustrencoef 
real(r8k)                 , dimension(:)        , allocatable          :: r__scd 
real(r8k)                 , dimension(:)        , allocatable          :: r__watrmetlenrgy 
real(r8k)                 , dimension(:)        , allocatable          :: r__coolpress 
real(r8k)                 , dimension(:)        , allocatable          :: r__dtplta 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaio 
real(r8k)                 , dimension(:)        , allocatable          :: r__apln 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxygenuptake 
real(r8k)                 , dimension(:)        , allocatable          :: r__thermalconductadv 
real(r8k)                 , dimension(:)        , allocatable          :: r__ascal2 
real(r8k)                 , dimension(:)        , allocatable          :: r__gasax 
real(r8k)                 , dimension(:)        , allocatable          :: r__profilestarttime 
real(r8k)                 , dimension(:)        , allocatable          :: r__ascal1 
real(r8k)                 , dimension(:)        , allocatable          :: r__htflxfac 
real(r8k)                 , dimension(:)        , allocatable          :: r__htclev 
real(r8k)                 , dimension(:)        , allocatable          :: r__tbulk_l 
real(r8k)                 , dimension(:)        , allocatable          :: r__prop 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenado 
real(r8k)                 , dimension(:)        , allocatable          :: r__flwblk 
real(r8k)                 , dimension(:)        , allocatable          :: r__tempfc 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenadi 
real(r8k)                 , dimension(:)        , allocatable          :: r__rinterfacgap 
real(r8k)                 , dimension(:)        , allocatable          :: r__stressatinststrain 
real(r8k)                 , dimension(:)        , allocatable          :: r__rinterfacprs0 
real(r8k)                 , dimension(:)        , allocatable          :: r__ts 
real(r8k)                 , dimension(:)        , allocatable          :: r__tp 
real(r8k)                 , dimension(:)        , allocatable          :: r__claddingpower 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcoolprs0 
real(r8k)                 , dimension(:)        , allocatable          :: r__voidvolumeratio 
real(r8k)                 , dimension(:)        , allocatable          :: r__techf 
real(r8k)                 , dimension(:)        , allocatable          :: r__eosoxidethick 
real(r8k)                 , dimension(:)        , allocatable          :: r__cexh2a 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelraddeviat 
real(r8k)                 , dimension(:)        , allocatable          :: r__coolenthalpy0 
real(r8k)                 , dimension(:)        , allocatable          :: r__epstrain1old 
real(r8k)                 , dimension(:)        , allocatable          :: r__azpang 
real(r8k)                 , dimension(:)        , allocatable          :: r__coolmassflx 
real(r8k)                 , dimension(:)        , allocatable          :: r__hmaxinitial 
real(r8k)                 , dimension(:)        , allocatable          :: r__betathickness 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxstalphathkremain 
real(r8k)                 , dimension(:)        , allocatable          :: r__powave 
real(r8k)                 , dimension(:)        , allocatable          :: r__axburnup 
real(r8k)                 , dimension(:)        , allocatable          :: r__apln0 
real(r8k)                 , dimension(:)        , allocatable          :: r__alphathk1 
real(r8k)                 , dimension(:)        , allocatable          :: r__amatpc 
real(r8k)                 , dimension(:)        , allocatable          :: r__fdial 
real(r8k)                 , dimension(:)        , allocatable          :: r__vrlfrp 
real(r8k)                 , dimension(:)        , allocatable          :: r__rinterfacprs 
real(r8k)                 , dimension(:)        , allocatable          :: r__energyperl 
real(r8k)                 , dimension(:)        , allocatable          :: r__dvdt 
real(r8k)                 , dimension(:)        , allocatable          :: r__effcoldwkstrnhardexp 
real(r8k)                 , dimension(:)        , allocatable          :: r__gapthick0 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaci 
real(r8k)                 , dimension(:)        , allocatable          :: r__gbse 
real(r8k)                 , dimension(:)        , allocatable          :: r__fmgp 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcoolprs 
real(r8k)                 , dimension(:)        , allocatable          :: r__sshgap 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacetcmx 
real(r8k)                 , dimension(:)        , allocatable          :: r__sedpnnlold 
real(r8k)                 , dimension(:)        , allocatable          :: r__heatflux 
real(r8k)                 , dimension(:)        , allocatable          :: r__boundarycondition 
real(r8k)                 , dimension(:)        , allocatable          :: r__gadoln 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaco 
real(r8k)                 , dimension(:)        , allocatable          :: r__gfrac 
real(r8k)                 , dimension(:)        , allocatable          :: r__radialpower 
real(r8k)                 , dimension(:)        , allocatable          :: r__stressaxialold 
real(r8k)                 , dimension(:)        , allocatable          :: r__surfhtflux 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcldplasstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__gbh 
real(r8k)                 , dimension(:)        , allocatable          :: r__prevtemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacereloc 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelletrad0 
real(r8k)                 , dimension(:)        , allocatable          :: r__rmrlp 
real(r8k)                 , dimension(:)        , allocatable          :: r__hz2 
real(r8k)                 , dimension(:)        , allocatable          :: r__fastflux 
real(r8k)                 , dimension(:)        , allocatable          :: r__bbht1 
real(r8k)                 , dimension(:)        , allocatable          :: r__pbh 
real(r8k)                 , dimension(:)        , allocatable          :: r__gaptemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__arest1 
real(r8k)                 , dimension(:)        , allocatable          :: r__dvdt0 
real(r8k)                 , dimension(:)        , allocatable          :: r__drdfrp 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldgasprs 
real(r8k)                 , dimension(:)        , allocatable          :: r__fuelgasswell 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxuptakeid2 
real(r8k)                 , dimension(:)        , allocatable          :: r__stresshoopold 
real(r8k)                 , dimension(:)        , allocatable          :: r__effcoldwkstrencoef 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenago 
real(r8k)                 , dimension(:)        , allocatable          :: r__rodod0 
real(r8k)                 , dimension(:)        , allocatable          :: r__dtmaxa 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelletrad 
real(r8k)                 , dimension(:)        , allocatable          :: r__edint 
real(r8k)                 , dimension(:)        , allocatable          :: r__alphathk2 
real(r8k)                 , dimension(:)        , allocatable          :: r__axialpowr 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelsrfdispl 
real(r8k)                 , dimension(:)        , allocatable          :: r__hgapav 
real(r8k)                 , dimension(:)        , allocatable          :: r__tmelt 
real(r8k)                 , dimension(:)        , allocatable          :: r__temptm 
real(r8k)                 , dimension(:)        , allocatable          :: r__enthl 
real(r8k)                 , dimension(:)        , allocatable          :: r__flthit 
real(r8k)                 , dimension(:)        , allocatable          :: r__bpln 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladmaxt 
real(r8k)                 , dimension(:)        , allocatable          :: r__qaxzq 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaii 
real(r8k)                 , dimension(:)        , allocatable          :: r__vfrad1 
real(r8k)                 , dimension(:)        , allocatable          :: r__vfrad3 
real(r8k)                 , dimension(:)        , allocatable          :: r__vfrad2 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenafo 
real(r8k)                 , dimension(:)        , allocatable          :: r__tend 
real(r8k)                 , dimension(:)        , allocatable          :: r__aexcb 
real(r8k)                 , dimension(:)        , allocatable          :: r__sedepri 
real(r8k)                 , dimension(:)        , allocatable          :: r__alphathk22 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxithk1 
real(r8k)                 , dimension(:)        , allocatable          :: r__bufrp 
real(r8k)                 , dimension(:)        , allocatable          :: r__hz1 
real(r8k)                 , dimension(:)        , allocatable          :: r__swelldispl 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxithk2 
real(r8k)                 , dimension(:)        , allocatable          :: r__tshrda 
real(r8k)                 , dimension(:)        , allocatable          :: r__vs0 
real(r8k)                 , dimension(:)        , allocatable          :: r__acoold 
real(r8k)                 , dimension(:)        , allocatable          :: r__hgpfrp 
real(r8k)                 , dimension(:)        , allocatable          :: r__pbh2 
real(r8k)                 , dimension(:)        , allocatable          :: r__hlqcl 
real(r8k)                 , dimension(:)        , allocatable          :: r__finaltemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__techfi 
real(r8k)                 , dimension(:)        , allocatable          :: r__pbh1 
real(r8k)                 , dimension(:)        , allocatable          :: r__aflcht 
real(r8k)                 , dimension(:)        , allocatable          :: r__bu 
real(r8k)                 , dimension(:)        , allocatable          :: r__ecr 
real(r8k)                 , dimension(:)        , allocatable          :: r__vreloc0 
real(r8k)                 , dimension(:)        , allocatable          :: r__epstrain2old 
real(r8k)                 , dimension(:)        , allocatable          :: r__hlqclp 
real(r8k)                 , dimension(:)        , allocatable          :: r__bpln0 
real(r8k)                 , dimension(:)        , allocatable          :: r__pz2 
real(r8k)                 , dimension(:)        , allocatable          :: r__cesiumcontent 
real(r8k)                 , dimension(:)        , allocatable          :: r__z1 
real(r8k)                 , dimension(:)        , allocatable          :: r__z2 
real(r8k)                 , dimension(:)        , allocatable          :: r__faxzq 
real(r8k)                 , dimension(:)        , allocatable          :: r__fluxz 
real(r8k)                 , dimension(:)        , allocatable          :: r__gapthick 
real(r8k)                 , dimension(:)        , allocatable          :: r__hbh 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldepeff 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldeffstrainpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladavetemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__nodchf 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldgasprs0 
real(r8k)                 , dimension(:)        , allocatable          :: r__aasth 
real(r8k)                 , dimension(:)        , allocatable          :: r__aprntb 
real(r8k)                 , dimension(:)        , allocatable          :: r__volopenpor 
real(r8k)                 , dimension(:)        , allocatable          :: r__farbal 
real(r8k)                 , dimension(:)        , allocatable          :: r__extentofbow 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcldaxstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: r__sdfar 
real(r8k)                 , dimension(:)        , allocatable          :: r__ablona 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladeffstress 
real(r8k)                 , dimension(:)        , allocatable          :: r__rodavepower 
real(r8k)                 , dimension(:)        , allocatable          :: r__gasmolesax0 
real(r8k)                 , dimension(:)        , allocatable          :: r__cooldensity0 
real(r8k)                 , dimension(:)        , allocatable          :: r__ascal3 
real(r8k)                 , dimension(:)        , allocatable          :: r__arrayf 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldpeldis 
real(r8k)                 , dimension(:)        , allocatable          :: r__edotfz 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacerco 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspaceep1 
real(r8k)                 , dimension(:)        , allocatable          :: r__cldpermstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__hydrogencontent 
real(r8k)                 , dimension(:)        , allocatable          :: r__gasfraction 
real(r8k)                 , dimension(:)        , allocatable          :: r__tz2 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacerci 
real(r8k)                 , dimension(:)        , allocatable          :: r__hfusn 
real(r8k)                 , dimension(:)        , allocatable          :: r__prcntsatbeta 
real(r8k)                 , dimension(:)        , allocatable          :: r__qpln0 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaai 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldpeldis0 
real(r8k)                 , dimension(:)        , allocatable          :: r__flowg 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaao 
real(r8k)                 , dimension(:)        , allocatable          :: r__iodinecontent 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldfuelaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__terfacepres 
real(r8k)                 , dimension(:)        , allocatable          :: r__axnodelevat 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladsurft 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenabi 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladyieldstress 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxideid 
real(r8k)                 , dimension(:)        , allocatable          :: r__swd 
real(r8k)                 , dimension(:)        , allocatable          :: r__tschfi 
real(r8k)                 , dimension(:)        , allocatable          :: r__strainrateterm 
real(r8k)                 , dimension(:)        , allocatable          :: r__hsolold 
real(r8k)                 , dimension(:)        , allocatable          :: r__gasmolesax 
real(r8k)                 , dimension(:)        , allocatable          :: r__hfxsum 
real(r8k)                 , dimension(:)        , allocatable          :: r__pclrlp 
real(r8k)                 , dimension(:)        , allocatable          :: r__elevatthermhydr 
real(r8k)                 , dimension(:)        , allocatable          :: r__timeofgbsep 
real(r8k)                 , dimension(:)        , allocatable          :: r__effstrain0 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxideod 
real(r8k)                 , dimension(:)        , allocatable          :: r__coolqual 
real(r8k)                 , dimension(:)        , allocatable          :: r__crithtflux 
real(r8k)                 , dimension(:)        , allocatable          :: r__axialnodlen 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenahi 
real(r8k)                 , dimension(:)        , allocatable          :: r__effstrain 
real(r8k)                 , dimension(:)        , allocatable          :: r__filmcoeffav 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacepint 
real(r8k)                 , dimension(:)        , allocatable          :: r__sedpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: r__heatflux0 
real(r8k)                 , dimension(:)        , allocatable          :: r__rmassflux 
real(r8k)                 , dimension(:)        , allocatable          :: r__qmaxmelt 
real(r8k)                 , dimension(:)        , allocatable          :: r__cldpermhoopstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__stressradialold 
real(r8k)                 , dimension(:)        , allocatable          :: r__fuelsrfstrrat 
real(r8k)                 , dimension(:)        , allocatable          :: r__cladeffstressold 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaeo 
real(r8k)                 , dimension(:)        , allocatable          :: r__cracktemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__thermalconductivity 
real(r8k)                 , dimension(:)        , allocatable          :: r__fldrat 
real(r8k)                 , dimension(:)        , allocatable          :: r__qmaxmeltp1 
real(r8k)                 , dimension(:)        , allocatable          :: r__pgpfrp 
real(r8k)                 , dimension(:)        , allocatable          :: r__radpel 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaei 
real(r8k)                 , dimension(:)        , allocatable          :: r__coefm 
real(r8k)                 , dimension(:)        , allocatable          :: r__coefn 
real(r8k)                 , dimension(:)        , allocatable          :: r__coefk 
real(r8k)                 , dimension(:)        , allocatable          :: r__openportemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__rodod 
real(r8k)                 , dimension(:)        , allocatable          :: r__fuelsrfstrrat0 
real(r8k)                 , dimension(:)        , allocatable          :: r__deltox 
real(r8k)                 , dimension(:)        , allocatable          :: r__vreloc 
real(r8k)                 , dimension(:)        , allocatable          :: r__cldpermaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__fmesh 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelprm 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacegap 
real(r8k)                 , dimension(:)        , allocatable          :: r__cmesh 
real(r8k)                 , dimension(:)        , allocatable          :: r__gsmol0 
real(r8k)                 , dimension(:)        , allocatable          :: r__openporvol 
real(r8k)                 , dimension(:)        , allocatable          :: r__bowrat 
real(r8k)                 , dimension(:)        , allocatable          :: r__gappr0 
real(r8k)                 , dimension(:)        , allocatable          :: r__vplen 
real(r8k)                 , dimension(:)        , allocatable          :: r__arraye 
real(r8k)                 , dimension(:)        , allocatable          :: r__openporosity 
real(r8k)                 , dimension(:)        , allocatable          :: r__centvoidvol 
real(r8k)                 , dimension(:)        , allocatable          :: r__bulkcooltemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__crackvolume 
real(r8k)                 , dimension(:)        , allocatable          :: r__butemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__hydrostatpress 
real(r8k)                 , dimension(:)        , allocatable          :: r__gasphs 
real(r8k)                 , dimension(:)        , allocatable          :: r__hlqcl1 
real(r8k)                 , dimension(:)        , allocatable          :: r__dtdkta 
real(r8k)                 , dimension(:)        , allocatable          :: r__gaspress0 
real(r8k)                 , dimension(:)        , allocatable          :: r__pelsrfstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspaceurc 
real(r8k)                 , dimension(:)        , allocatable          :: r__hupta 
real(r8k)                 , dimension(:)        , allocatable          :: r__spl 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxuptakeid1 
real(r8k)                 , dimension(:)        , allocatable          :: r__fuelsurft 
real(r8k)                 , dimension(:)        , allocatable          :: r__aaht1 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenaho 
real(r8k)                 , dimension(:)        , allocatable          :: r__eppinp 
real(r8k)                 , dimension(:)        , allocatable          :: r__ispace 
real(r8k)                 , dimension(:)        , allocatable          :: r__pswll0 
real(r8k)                 , dimension(:)        , allocatable          :: r__bosoxidethick 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenagi 
real(r8k)                 , dimension(:)        , allocatable          :: r__dishvperl 
real(r8k)                 , dimension(:)        , allocatable          :: r__qpln 
real(r8k)                 , dimension(:)        , allocatable          :: r__sedepriold 
real(r8k)                 , dimension(:)        , allocatable          :: r__radialbound 
real(r8k)                 , dimension(:)        , allocatable          :: r__explenumt 
real(r8k)                 , dimension(:)        , allocatable          :: r__crackwidth 
real(r8k)                 , dimension(:)        , allocatable          :: r__edotfz0 
real(r8k)                 , dimension(:)        , allocatable          :: r__coolenthalpy 
real(r8k)                 , dimension(:)        , allocatable          :: r__dtpoa 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcladt 
real(r8k)                 , dimension(:)        , allocatable          :: r__vsn 
real(r8k)                 , dimension(:)        , allocatable          :: r__ureloc 
real(r8k)                 , dimension(:)        , allocatable          :: r__afrap 
real(r8k)                 , dimension(:)        , allocatable          :: r__efffastflustrnhardexp 
real(r8k)                 , dimension(:)        , allocatable          :: r__epstrain3old 
real(r8k)                 , dimension(:)        , allocatable          :: r__gaspress 
real(r8k)                 , dimension(:)        , allocatable          :: r__radpowprofile 
real(r8k)                 , dimension(:)        , allocatable          :: r__relfraca 
real(r8k)                 , dimension(:)        , allocatable          :: r__effstrainpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: r__tz1 
real(r8k)                 , dimension(:)        , allocatable          :: r__emodulus 
real(r8k)                 , dimension(:)        , allocatable          :: r__einstabilstrain 
real(r8k)                 , dimension(:)        , allocatable          :: r__tschf 
real(r8k)                 , dimension(:)        , allocatable          :: r__elvrad 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcladt0 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldfuelaxstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: r__ansr 
real(r8k)                 , dimension(:)        , allocatable          :: r__gfint 
real(r8k)                 , dimension(:)        , allocatable          :: r__pz1 
real(r8k)                 , dimension(:)        , allocatable          :: r__axialpowr0 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxygenconcenave 
real(r8k)                 , dimension(:)        , allocatable          :: r__a1 
real(r8k)                 , dimension(:)        , allocatable          :: r__zfarbl 
real(r8k)                 , dimension(:)        , allocatable          :: r__fuelcentert 
real(r8k)                 , dimension(:)        , allocatable          :: r__workspacegapi 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldeps 
real(r8k)                 , dimension(:)        , allocatable          :: r__hinta 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxstalphathknearfuel 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenabo 
real(r8k)                 , dimension(:)        , allocatable          :: r__avedishtemp 
real(r8k)                 , dimension(:)        , allocatable          :: r__gz2 
real(r8k)                 , dimension(:)        , allocatable          :: r__gz1 
real(r8k)                 , dimension(:)        , allocatable          :: r__oldcldaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: r__cooldensity 
real(r8k)                 , dimension(:)        , allocatable          :: r__prestm 
real(r8k)                 , dimension(:)        , allocatable          :: r__cldpermstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: r__elevatfrap 
real(r8k)                 , dimension(:)        , allocatable          :: r__pinput 
real(r8k)                 , dimension(:)        , allocatable          :: r__htc_v 
real(r8k)                 , dimension(:)        , allocatable          :: r__potput 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxconcenafi 
real(r8k)                 , dimension(:)        , allocatable          :: r__acolct 
real(r8k)                 , dimension(:)        , allocatable          :: r__zelev 
real(r8k)                 , dimension(:)        , allocatable          :: r__gadolin 
real(r8k)                 , dimension(:)        , allocatable          :: r__htc_l 
real(r8k)                 , dimension(:)        , allocatable          :: r__oxipowergen 
real(r8k)                 , dimension(:)        , allocatable          :: r__axlinpower 
integer(ipk)              , dimension(:,:)      , allocatable          :: r__icglob 
integer(ipk)              , dimension(:,:)      , allocatable          :: r__nrc 
integer(ipk)              , dimension(:,:)      , allocatable          :: r__iokold 
integer(ipk)              , dimension(:,:)      , allocatable          :: r__iok 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__gasavail1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__tmprlp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__gasavail2 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__volumeweightr 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__enrgymeltz 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__acond 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldthermstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__frapcontemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__baln2twall 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__qcold 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__fuelresidstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__tplbt1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__previteratetemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__buradv 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__rhocp0 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__tblka 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__burad 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__htca 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__enrgymelt 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldstrnrat 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__deformedradiusofmesh 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__bosrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__pelsrfstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__eosrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__radsrc 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldplasstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldstress 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__rhocp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__radialboundo 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__volumeweightl 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__axpowprofile 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__enrgymeltzp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__pazp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__tpln 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__radpow 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__areaweight 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__eostemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__pchn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__trad2 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__trad3 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__trad1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__gasths 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__fuelrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldresidstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__dktouts 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldstrnrat0 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldplasstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__bufrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__radsrco 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldresidstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__radtemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__workspaceepp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__fuelresidstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldelstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__enrgymeltp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__tplbot 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__bostemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__cldstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: r__htcgap 
real(r8k)                 , dimension(:,:,:)    , allocatable          :: r__grsv 
real(r8k)                 , dimension(:,:,:)    , allocatable          :: r__tplna 
