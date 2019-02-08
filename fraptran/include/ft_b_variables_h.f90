logical                                                                :: b__convert_units 
logical                                                                :: b__printballoon 
logical                                                                :: b__printrodburst 
logical                                                                :: b__allocate_arrays_ft 
logical                                                                :: b__coupled 
logical                                                                :: b__ndebug 
logical                                                                :: b__dakota 
logical                                                                :: b__setunits 
logical                                                                :: b__first_pass 
logical                                                                :: b__first_call 
logical                                                                :: b__unit 
character(len=8)                                                       :: b__rupflg 
character(len=3)                                                       :: b__coolant 
character(len=80)                                                      :: b__fraptranfn 
character(len=3)                                                       :: b__metal 
character(len=15)                                                      :: b__modheatmodel 
character(len=12)                                                      :: b__relocmodel 
character(len=8)                                                       :: b__lodmrk 
character(len=3)                                                       :: b__reflood 
character(len=200)                                                     :: b__namerf 
character(len=10)                                                      :: b__inst 
character(len=3)                                                       :: b__bheat 
character(len=3)                                                       :: b__internal 
character(len=3)                                                       :: b__mheat 
character(len=3)                                                       :: b__radiation 
character(len=3)                                                       :: b__deformation 
integer(ipk)                                                           :: b__ifaila 
integer(ipk)                                                           :: b__kaxnlo 
integer(ipk)                                                           :: b__ncirlp 
integer(ipk)                                                           :: b__nprsw 
integer(ipk)                                                           :: b__npbh 
integer(ipk)                                                           :: b__nbncal 
integer(ipk)                                                           :: b__llp 
integer(ipk)                                                           :: b__ierr 
integer(ipk)                                                           :: b__lcoold 
integer(ipk)                                                           :: b__nidoxide 
integer(ipk)                                                           :: b__press 
integer(ipk)                                                           :: b__cathca 
integer(ipk)                                                           :: b__klp2 
integer(ipk)                                                           :: b__nelrad 
integer(ipk)                                                           :: b__frapt4 
integer(ipk)                                                           :: b__ntapou 
integer(ipk)                                                           :: b__kaxhtc 
integer(ipk)                                                           :: b__indexthermcon 
integer(ipk)                                                           :: b__prestmp 
integer(ipk)                                                           :: b__irupt 
integer(ipk)                                                           :: b__indxmd 
integer(ipk)                                                           :: b__nbowr 
integer(ipk)                                                           :: b__ldialb 
integer(ipk)                                                           :: b__modbal 
integer(ipk)                                                           :: b__pressu 
integer(ipk)                                                           :: b__inlet 
integer(ipk)                                                           :: b__zone 
integer(ipk)                                                           :: b__ndap1 
integer(ipk)                                                           :: b__nbhtc 
integer(ipk)                                                           :: b__protectiveoxide 
integer(ipk)                                                           :: b__rtheta 
integer(ipk)                                                           :: b__ldir 
integer(ipk)                                                           :: b__istoicgrad 
integer(ipk)                                                           :: b__ifail 
integer(ipk)                                                           :: b__lchf 
integer(ipk)                                                           :: b__itswt 
integer(ipk)                                                           :: b__pow 
integer(ipk)                                                           :: b__nsymm 
integer(ipk)                                                           :: b__naazp 
integer(ipk)                                                           :: b__nvol2 
integer(ipk)                                                           :: b__idebug 
integer(ipk)                                                           :: b__nvol1 
integer(ipk)                                                           :: b__mhbh 
integer(ipk)                                                           :: b__modfd 
integer(ipk)                                                           :: b__ixazmn 
integer(ipk)                                                           :: b__gasflo 
integer(ipk)                                                           :: b__nbrtmp 
integer(ipk)                                                           :: b__nbrfdr 
integer(ipk)                                                           :: b__prescri 
integer(ipk)                                                           :: b__mbowr 
integer(ipk)                                                           :: b__mtabl3 
integer(ipk)                                                           :: b__mechan 
integer(ipk)                                                           :: b__nfmesh 
integer(ipk)                                                           :: b__mndchf 
integer(ipk)                                                           :: b__naazpd 
integer(ipk)                                                           :: b__npelraddeviat 
integer(ipk)                                                           :: b__itdmx 
integer(ipk)                                                           :: b__nhbh 
integer(ipk)                                                           :: b__nfrapconinitialization 
integer(ipk)                                                           :: b__ncool 
integer(ipk)                                                           :: b__nsrad3 
integer(ipk)                                                           :: b__indexgrainbndsep 
integer(ipk)                                                           :: b__nedtsw 
integer(ipk)                                                           :: b__azang 
integer(ipk)                                                           :: b__m5fc2 
integer(ipk)                                                           :: b__radiat 
integer(ipk)                                                           :: b__mhinta 
integer(ipk)                                                           :: b__luout 
integer(ipk)                                                           :: b__nmesh 
integer(ipk)                                                           :: b__mgaspr 
integer(ipk)                                                           :: b__iagap 
integer(ipk)                                                           :: b__indxkf 
integer(ipk)                                                           :: b__nitmin 
integer(ipk)                                                           :: b__nconsw 
integer(ipk)                                                           :: b__numdta 
integer(ipk)                                                           :: b__nhtcz 
integer(ipk)                                                           :: b__kntbal 
integer(ipk)                                                           :: b__nrazp 
integer(ipk)                                                           :: b__iplant 
integer(ipk)                                                           :: b__indexthermconadv 
integer(ipk)                                                           :: b__mhlqc1 
integer(ipk)                                                           :: b__nplnt 
integer(ipk)                                                           :: b__nbrliq 
integer(ipk)                                                           :: b__idoxid 
integer(ipk)                                                           :: b__lcombk 
integer(ipk)                                                           :: b__indexgeom 
integer(ipk)                                                           :: b__kbaln 
integer(ipk)                                                           :: b__nbuq 
integer(ipk)                                                           :: b__mtempt 
integer(ipk)                                                           :: b__maxit 
integer(ipk)                                                           :: b__nbundl 
integer(ipk)                                                           :: b__reflo 
integer(ipk)                                                           :: b__nlac 
integer(ipk)                                                           :: b__jchf 
integer(ipk)                                                           :: b__ntco 
integer(ipk)                                                           :: b__kflgb 
integer(ipk)                                                           :: b__tape1 
integer(ipk)                                                           :: b__lengmd 
integer(ipk)                                                           :: b__tape2 
integer(ipk)                                                           :: b__nitdt 
integer(ipk)                                                           :: b__res 
integer(ipk)                                                           :: b__la1max 
integer(ipk)                                                           :: b__jtr 
integer(ipk)                                                           :: b__nqbow 
integer(ipk)                                                           :: b__idx2 
integer(ipk)                                                           :: b__geom 
integer(ipk)                                                           :: b__mhlqcp 
integer(ipk)                                                           :: b__lowpl 
integer(ipk)                                                           :: b__la1fil 
integer(ipk)                                                           :: b__np 
integer(ipk)                                                           :: b__ifcmi 
integer(ipk)                                                           :: b__ntempf 
integer(ipk)                                                           :: b__izadfg 
integer(ipk)                                                           :: b__npdtpo 
integer(ipk)                                                           :: b__ncards 
integer(ipk)                                                           :: b__massfl 
integer(ipk)                                                           :: b__iterationcount 
integer(ipk)                                                           :: b__itcntd 
integer(ipk)                                                           :: b__iagap0 
integer(ipk)                                                           :: b__odoxid 
integer(ipk)                                                           :: b__ntstep 
integer(ipk)                                                           :: b__kswell 
integer(ipk)                                                           :: b__pressure 
integer(ipk)                                                           :: b__mhupta 
integer(ipk)                                                           :: b__ndtred 
integer(ipk)                                                           :: b__nptha 
integer(ipk)                                                           :: b__ncorlp 
integer(ipk)                                                           :: b__bowing 
integer(ipk)                                                           :: b__transwell 
integer(ipk)                                                           :: b__unitin 
integer(ipk)                                                           :: b__indexfc2print 
integer(ipk)                                                           :: b__nepp0 
integer(ipk)                                                           :: b__ntimesteps 
integer(ipk)                                                           :: b__nchan 
integer(ipk)                                                           :: b__nradialnodes 
integer(ipk)                                                           :: b__mzq1 
integer(ipk)                                                           :: b__geometry 
integer(ipk)                                                           :: b__upppl 
integer(ipk)                                                           :: b__ruptur 
integer(ipk)                                                           :: b__geomet 
integer(ipk)                                                           :: b__nradq 
integer(ipk)                                                           :: b__lcolct 
integer(ipk)                                                           :: b__noiter 
integer(ipk)                                                           :: b__nchfmd 
integer(ipk)                                                           :: b__nchn 
integer(ipk)                                                           :: b__lresr2 
integer(ipk)                                                           :: b__ntimes 
integer(ipk)                                                           :: b__nfhtc 
integer(ipk)                                                           :: b__lresr1 
integer(ipk)                                                           :: b__mpbh 
integer(ipk)                                                           :: b__ncmesh 
integer(ipk)                                                           :: b__nsc 
integer(ipk)                                                           :: b__indxkc 
integer(ipk)                                                           :: b__ndim 
integer(ipk)                                                           :: b__naxn 
integer(ipk)                                                           :: b__lflect 
integer(ipk)                                                           :: b__ns 
integer(ipk)                                                           :: b__nradsh 
integer(ipk)                                                           :: b__lprntb 
integer(ipk)                                                           :: b__mprest 
integer(ipk)                                                           :: b__dktoutcounter 
integer(ipk)                                                           :: b__nsf 
integer(ipk)                                                           :: b__ncooli 
integer(ipk)                                                           :: b__m6fc2 
integer(ipk)                                                           :: b__indxpr 
integer(ipk)                                                           :: b__jmnbal 
integer(ipk)                                                           :: b__modmw 
integer(ipk)                                                           :: b__ncard2 
integer(ipk)                                                           :: b__lsclr1 
integer(ipk)                                                           :: b__nprad 
integer(ipk)                                                           :: b__nhtca 
integer(ipk)                                                           :: b__knonue 
integer(ipk)                                                           :: b__chf 
integer(ipk)                                                           :: b__irefine 
integer(ipk)                                                           :: b__mgbh 
integer(ipk)                                                           :: b__ndktparams 
integer(ipk)                                                           :: b__lsclr2 
integer(ipk)                                                           :: b__lsclr3 
integer(ipk)                                                           :: b__defsize 
integer(ipk)                                                           :: b__nflec 
integer(ipk)                                                           :: b__lblona 
integer(ipk)                                                           :: b__nodpln 
integer(ipk)                                                           :: b__nrestart 
integer(ipk)                                                           :: b__lafrap 
integer(ipk)                                                           :: b__coreav 
integer(ipk)                                                           :: b__nfuelswellpairs 
integer(ipk)                                                           :: b__nswmd 
integer(ipk)                                                           :: b__nvoid 
integer(ipk)                                                           :: b__lmatpc 
integer(ipk)                                                           :: b__inpfil 
integer(ipk)                                                           :: b__numaxprofiles 
integer(ipk)                                                           :: b__nkc 
integer(ipk)                                                           :: b__nomat 
integer(ipk)                                                           :: b__nkf 
integer(ipk)                                                           :: b__nfgrpairs 
integer(ipk)                                                           :: b__nhtclv 
integer(ipk)                                                           :: b__npaxp 
integer(ipk)                                                           :: b__ncool2 
integer(ipk)                                                           :: b__kmxrlp 
integer(ipk)                                                           :: b__kmxfrp 
integer(ipk)                                                           :: b__indexbc 
integer(ipk)                                                           :: b__ncall 
integer(ipk)                                                           :: b__ndtmax 
integer(ipk)                                                           :: b__naxialnodes 
integer(ipk)                                                           :: b__soltyp 
integer(ipk)                                                           :: b__nptha1 
integer(ipk)                                                           :: b__ntplot 
integer(ipk)                                                           :: b__ns2 
integer(ipk)                                                           :: b__nce 
integer(ipk)                                                           :: b__modkf 
integer(ipk)                                                           :: b__mtabl1 
integer(ipk)                                                           :: b__mtabl2 
integer(ipk)                                                           :: b__nbalsw 
integer(ipk)                                                           :: b__ixazim 
integer(ipk)                                                           :: b__temp 
integer(ipk)                                                           :: b__indexinittemp 
integer(ipk)                                                           :: b__cenvoi 
integer(ipk)                                                           :: b__nrefld 
integer(ipk)                                                           :: b__ndtad 
integer(ipk)                                                           :: b__ntabl1 
integer(ipk)                                                           :: b__nbrfht 
integer(ipk)                                                           :: b__nqchn 
integer(ipk)                                                           :: b__klp 
integer(ipk)                                                           :: b__npid 
integer(ipk)                                                           :: b__nforlp 
integer(ipk)                                                           :: b__nswinw 
integer(ipk)                                                           :: b__grass 
integer(ipk)                                                           :: b__mpdcay 
integer(ipk)                                                           :: b__baker 
integer(ipk)                                                           :: b__spefbz 
integer(ipk)                                                           :: b__ncolbp 
integer(ipk)                                                           :: b__nt5 
integer(ipk)                                                           :: b__mwork 
integer(ipk)                                                           :: b__nbrpst 
integer(ipk)                                                           :: b__jpl 
integer(ipk)                                                           :: b__presfgr 
integer(ipk)                                                           :: b__mbdl 
integer(ipk)                                                           :: b__nerr 
integer(ipk)                                                           :: b__inp 
integer(ipk)                                                           :: b__iflagn 
integer(ipk)                                                           :: b__nhinta 
integer(ipk)                                                           :: b__lthyd 
integer(ipk)                                                           :: b__indexfintemp 
integer(ipk)                                                           :: b__nnaz 
integer(ipk)                                                           :: b__npaxp1 
integer(ipk)                                                           :: b__nrazpd 
integer(ipk)                                                           :: b__mflt 
integer(ipk)                                                           :: b__maximumiterations 
integer(ipk)                                                           :: b__npramp 
integer(ipk)                                                           :: b__nzmesh 
integer(ipk)                                                           :: b__nprofile 
integer(ipk)                                                           :: b__igpnod 
integer(ipk)                                                           :: b__npair 
integer(ipk)                                                           :: b__coldwa 
integer(ipk)                                                           :: b__refloodtime 
integer(ipk)                                                           :: b__ngbh 
integer(ipk)                                                           :: b__lphypr 
integer(ipk)                                                           :: b__ifbaln 
integer(ipk)                                                           :: b__nrest2 
integer(ipk)                                                           :: b__length 
integer(ipk)                                                           :: b__nfastf 
integer(ipk)                                                           :: b__nthermex 
integer(ipk)                                                           :: b__lhtc 
integer(ipk)                                                           :: b__naz 
integer(ipk)                                                           :: b__nvol 
integer(ipk)                                                           :: b__imox 
integer(ipk)                                                           :: b__nt 
integer(ipk)                                                           :: b__maxidx 
integer(ipk)                                                           :: b__lhtcb 
integer(ipk)                                                           :: b__collaps 
integer(ipk)                                                           :: b__profile 
integer(ipk)                                                           :: b__nucbo 
integer(ipk)                                                           :: b__filmbo 
integer(ipk)                                                           :: b__cladtype 
integer(ipk)                                                           :: b__ndtadv 
integer(ipk)                                                           :: b__ncladi 
integer(ipk)                                                           :: b__npaxpf 
integer(ipk)                                                           :: b__jfb 
integer(ipk)                                                           :: b__n4 
integer(ipk)                                                           :: b__nhtc 
integer(ipk)                                                           :: b__ngasr 
integer(ipk)                                                           :: b__ithymx 
integer(ipk)                                                           :: b__ifailue 
integer(ipk)                                                           :: b__axpow 
integer(ipk)                                                           :: b__n1 
integer(ipk)                                                           :: b__nsteadytrans 
integer(ipk)                                                           :: b__nprntb 
integer(ipk)                                                           :: b__nbotpl 
integer(ipk)                                                           :: b__nsym 
integer(ipk)                                                           :: b__la1req 
integer(ipk)                                                           :: b__noball 
integer(ipk)                                                           :: b__kbot 
integer(ipk)                                                           :: b__n3 
integer(ipk)                                                           :: b__lresi3 
integer(ipk)                                                           :: b__lresi2 
integer(ipk)                                                           :: b__liquid 
integer(ipk)                                                           :: b__nodprm 
integer(ipk)                                                           :: b__ngaspr 
integer(ipk)                                                           :: b__ntabl3 
integer(ipk)                                                           :: b__ntabl2 
integer(ipk)                                                           :: b__iflago 
integer(ipk)                                                           :: b__indx 
integer(ipk)                                                           :: b__ndktn 
integer(ipk)                                                           :: b__npltn 
integer(ipk)                                                           :: b__unitout 
integer(ipk)                                                           :: b__ifchk 
integer(ipk)                                                           :: b__plenumtemp 
integer(ipk)                                                           :: b__lexcb 
integer(ipk)                                                           :: b__kdbug 
integer(ipk)                                                           :: b__n2 
integer(ipk)                                                           :: b__gammait 
integer(ipk)                                                           :: b__liqnod 
integer(ipk)                                                           :: b__nhupta 
integer(ipk)                                                           :: b__irup 
real(r8k)                                                              :: b__splbp 
real(r8k)                                                              :: b__tpowf 
real(r8k)                                                              :: b__kappag 
real(r8k)                                                              :: b__dtpo 
real(r8k)                                                              :: b__refdtm 
real(r8k)                                                              :: b__achn 
real(r8k)                                                              :: b__prodxe 
real(r8k)                                                              :: b__pdeint 
real(r8k)                                                              :: b__zbaln 
real(r8k)                                                              :: b__tplenb 
real(r8k)                                                              :: b__tflood 
real(r8k)                                                              :: b__kappaf 
real(r8k)                                                              :: b__sigcladthermcond 
real(r8k)                                                              :: b__pitch 
real(r8k)                                                              :: b__dcldh 
real(r8k)                                                              :: b__cnfsol 
real(r8k)                                                              :: b__gasmoles0 
real(r8k)                                                              :: b__tfoldf 
real(r8k)                                                              :: b__bowthr 
real(r8k)                                                              :: b__hrad 
real(r8k)                                                              :: b__dofang 
real(r8k)                                                              :: b__t0f 
real(r8k)                                                              :: b__pressr 
real(r8k)                                                              :: b__t0c 
real(r8k)                                                              :: b__maxenthalpy 
real(r8k)                                                              :: b__tp1bal 
real(r8k)                                                              :: b__tmaxf 
real(r8k)                                                              :: b__tt 
real(r8k)                                                              :: b__rmpbal 
real(r8k)                                                              :: b__qlbal 
real(r8k)                                                              :: b__frden 
real(r8k)                                                              :: b__vsubg 
real(r8k)                                                              :: b__vsubf 
real(r8k)                                                              :: b__dtbal 
real(r8k)                                                              :: b__delth 
real(r8k)                                                              :: b__prodkr 
real(r8k)                                                              :: b__tshtc 
real(r8k)                                                              :: b__cexh2l 
real(r8k)                                                              :: b__zpkfc 
real(r8k)                                                              :: b__htcgba 
real(r8k)                                                              :: b__frbal 
real(r8k)                                                              :: b__reflpr 
real(r8k)                                                              :: b__qbal 
real(r8k)                                                              :: b__pelh 
real(r8k)                                                              :: b__arean 
real(r8k)                                                              :: b__zndbal 
real(r8k)                                                              :: b__t12 
real(r8k)                                                              :: b__debugtime 
real(r8k)                                                              :: b__pavg 
real(r8k)                                                              :: b__aidsht 
real(r8k)                                                              :: b__rl 
real(r8k)                                                              :: b__maxcladtemp 
real(r8k)                                                              :: b__dtheta 
real(r8k)                                                              :: b__hsubf 
real(r8k)                                                              :: b__gapconductivity 
real(r8k)                                                              :: b__cpmult 
real(r8k)                                                              :: b__fdelta 
real(r8k)                                                              :: b__sigfuelthermexp 
real(r8k)                                                              :: b__totnb 
real(r8k)                                                              :: b__sigcladthermexp 
real(r8k)                                                              :: b__timop 
real(r8k)                                                              :: b__src2 
real(r8k)                                                              :: b__flowbk 
real(r8k)                                                              :: b__totalvoidvol 
real(r8k)                                                              :: b__htcbal 
real(r8k)                                                              :: b__powop 
real(r8k)                                                              :: b__tplen 
real(r8k)                                                              :: b__flxsec 
real(r8k)                                                              :: b__t2 
real(r8k)                                                              :: b__t0 
real(r8k)                                                              :: b__t1 
real(r8k)                                                              :: b__ffch 
real(r8k)                                                              :: b__dtenfb 
real(r8k)                                                              :: b__crf 
real(r8k)                                                              :: b__fpdcay 
real(r8k)                                                              :: b__debugtimestop 
real(r8k)                                                              :: b__roughc 
real(r8k)                                                              :: b__x 
real(r8k)                                                              :: b__chstrs 
real(r8k)                                                              :: b__roughf 
real(r8k)                                                              :: b__dtenfo 
real(r8k)                                                              :: b__drod 
real(r8k)                                                              :: b__jhtc 
real(r8k)                                                              :: b__hliq 
real(r8k)                                                              :: b__rfbal 
real(r8k)                                                              :: b__pctop 
real(r8k)                                                              :: b__prsacc 
real(r8k)                                                              :: b__tprlp 
real(r8k)                                                              :: b__relfract 
real(r8k)                                                              :: b__fpowr 
real(r8k)                                                              :: b__gum 
real(r8k)                                                              :: b__gpthk0 
real(r8k)                                                              :: b__tref 
real(r8k)                                                              :: b__ftmelt 
real(r8k)                                                              :: b__zs 
real(r8k)                                                              :: b__tc0bal 
real(r8k)                                                              :: b__tgbal 
real(r8k)                                                              :: b__apowrd 
real(r8k)                                                              :: b__rodlength 
real(r8k)                                                              :: b__psat 
real(r8k)                                                              :: b__srcv2 
real(r8k)                                                              :: b__time0 
real(r8k)                                                              :: b__flxbal 
real(r8k)                                                              :: b__fuelrd 
real(r8k)                                                              :: b__zmesh 
real(r8k)                                                              :: b__rf 
real(r8k)                                                              :: b__ctmelt 
real(r8k)                                                              :: b__htflxa 
real(r8k)                                                              :: b__pdrato 
real(r8k)                                                              :: b__templo 
real(r8k)                                                              :: b__zro 
real(r8k)                                                              :: b__drflt 
real(r8k)                                                              :: b__tgas0 
real(r8k)                                                              :: b__swllfr 
real(r8k)                                                              :: b__gsmfrp 
real(r8k)                                                              :: b__modheat 
real(r8k)                                                              :: b__spdbp 
real(r8k)                                                              :: b__pfail 
real(r8k)                                                              :: b__cladid 
real(r8k)                                                              :: b__fldrpr 
real(r8k)                                                              :: b__nu3 
real(r8k)                                                              :: b__pmxbal 
real(r8k)                                                              :: b__nu1 
real(r8k)                                                              :: b__timmd 
real(r8k)                                                              :: b__aidtot 
real(r8k)                                                              :: b__aidsur 
real(r8k)                                                              :: b__tsub 
real(r8k)                                                              :: b__sigcladyieldstr 
real(r8k)                                                              :: b__fabal 
real(r8k)                                                              :: b__rhoc 
real(r8k)                                                              :: b__tflux 
real(r8k)                                                              :: b__rhof 
real(r8k)                                                              :: b__tdbugd 
real(r8k)                                                              :: b__sigfuelheatcapa 
real(r8k)                                                              :: b__deloxy 
real(r8k)                                                              :: b__r8bal 
real(r8k)                                                              :: b__z 
real(r8k)                                                              :: b__delth0 
real(r8k)                                                              :: b__tmax 
real(r8k)                                                              :: b__qmax 
real(r8k)                                                              :: b__chefus 
real(r8k)                                                              :: b__trabal 
real(r8k)                                                              :: b__rcpar 
real(r8k)                                                              :: b__pressi 
real(r8k)                                                              :: b__t22 
real(r8k)                                                              :: b__ffchf 
real(r8k)                                                              :: b__roddiameter 
real(r8k)                                                              :: b__coolantpress 
real(r8k)                                                              :: b__ruplev 
real(r8k)                                                              :: b__epsht1 
real(r8k)                                                              :: b__h0bal 
real(r8k)                                                              :: b__tmaxc 
real(r8k)                                                              :: b__zad 
real(r8k)                                                              :: b__tranfuelswell 
real(r8k)                                                              :: b__csubp 
real(r8k)                                                              :: b__cladpower 
real(r8k)                                                              :: b__src1 
real(r8k)                                                              :: b__rshrd 
real(r8k)                                                              :: b__dppowi 
real(r8k)                                                              :: b__zqch 
real(r8k)                                                              :: b__frcoef 
real(r8k)                                                              :: b__empytm 
real(r8k)                                                              :: b__zroug2 
real(r8k)                                                              :: b__frchmx 
real(r8k)                                                              :: b__tbkbal 
real(r8k)                                                              :: b__timbal 
real(r8k)                                                              :: b__pcbal 
real(r8k)                                                              :: b__plenumgasmoles 
real(r8k)                                                              :: b__xtime 
real(r8k)                                                              :: b__doffst 
real(r8k)                                                              :: b__hsubg 
real(r8k)                                                              :: b__powimx 
real(r8k)                                                              :: b__buoxide 
real(r8k)                                                              :: b__fltgap 
real(r8k)                                                              :: b__psbal 
real(r8k)                                                              :: b__emptm 
real(r8k)                                                              :: b__dvdtp 
real(r8k)                                                              :: b__tm1bal 
real(r8k)                                                              :: b__trise 
real(r8k)                                                              :: b__usubf 
real(r8k)                                                              :: b__usubg 
real(r8k)                                                              :: b__fltgap2 
real(r8k)                                                              :: b__hydiam 
real(r8k)                                                              :: b__dishd 
real(r8k)                                                              :: b__beta1 
real(r8k)                                                              :: b__cldwdc 
real(r8k)                                                              :: b__cpstem 
real(r8k)                                                              :: b__tplot 
real(r8k)                                                              :: b__csubpg 
real(r8k)                                                              :: b__csubpf 
real(r8k)                                                              :: b__tfavba 
real(r8k)                                                              :: b__fotmtl 
real(r8k)                                                              :: b__timeincrement 
real(r8k)                                                              :: b__tmpac1 
real(r8k)                                                              :: b__sigfuelthermcond 
real(r8k)                                                              :: b__crfpr 
real(r8k)                                                              :: b__sigsurfhtc 
real(r8k)                                                              :: b__trecrd 
real(r8k)                                                              :: b__emwbal 
real(r8k)                                                              :: b__cladod 
real(r8k)                                                              :: b__ruptstrain 
real(r8k)                                                              :: b__zqchpr 
real(r8k)                                                              :: b__tsntrk 
real(r8k)                                                              :: b__rsntr 
real(r8k)                                                              :: b__nu2 
real(r8k)                                                              :: b__volavegastemp 
real(r8k)                                                              :: b__zroug1 
real(r8k)                                                              :: b__aidlng 
real(r8k)                                                              :: b__rhostm 
real(r8k)                                                              :: b__ph 
real(r8k)                                                              :: b__coldbp 
real(r8k)                                                              :: b__tempmx 
real(r8k)                                                              :: b__betag 
real(r8k)                                                              :: b__betaf 
real(r8k)                                                              :: b__kappa 
real(r8k)                                                              :: b__cfluxa 
real(r8k)                                                              :: b__dtold 
real(r8k)                                                              :: b__zvoid1 
real(r8k)                                                              :: b__atop 
real(r8k)                                                              :: b__rvoid 
real(r8k)                                                              :: b__qaxpk 
real(r8k)                                                              :: b__dofset 
real(r8k)                                                              :: b__tf0bal 
real(r8k)                                                              :: b__ubar 
real(r8k)                                                              :: b__pl 
real(r8k)                                                              :: b__v 
real(r8k)                                                              :: b__tdkt 
real(r8k)                                                              :: b__dtp 
real(r8k)                                                              :: b__totalgasmoles 
real(r8k)                                                              :: b__refine 
real(r8k)                                                              :: b__gflow 
real(r8k)                                                              :: b__gfloa1 
real(r8k)                                                              :: b__temphi 
real(r8k)                                                              :: b__frchdt 
real(r8k)                                                              :: b__fhtc 
real(r8k)                                                              :: b__frpo2 
real(r8k)                                                              :: b__trest 
real(r8k)                                                              :: b__fgrns 
real(r8k)                                                              :: b__dtmpcl 
real(r8k)                                                              :: b__areao 
real(r8k)                                                              :: b__pfnuc 
real(r8k)                                                              :: b__r0bal 
real(r8k)                                                              :: b__fuelpeldiam 
real(r8k)                                                              :: b__tsatt 
real(r8k)                                                              :: b__pdecy 
real(r8k)                                                              :: b__ztmax 
real(r8k)                                                              :: b__zqflt 
real(r8k)                                                              :: b__fldrte 
real(r8k)                                                              :: b__coldw 
real(r8k)                                                              :: b__openporosityfraction 
real(r8k)                                                              :: b__dhe 
real(r8k)                                                              :: b__explenumv 
real(r8k)                                                              :: b__compmt 
real(r8k)                                                              :: b__maxgaspressure 
real(r8k)                                                              :: b__dtss 
real(r8k)                                                              :: b__oldtim 
real(r8k)                                                              :: b__dhy 
real(r8k)                                                              :: b__tc1 
real(r8k)                                                              :: b__volbp 
real(r8k)                                                              :: b__bup 
real(r8k)                                                              :: b__fhefus 
real(r8k)                                                              :: b__powict 
real(r8k)                                                              :: b__fr 
real(r8k)                                                              :: b__pois 
real(r8k)                                                              :: b__dtobal 
real(r8k)                                                              :: b__dcldh0 
real(r8k)                                                              :: b__pavgft 
real(r8k)                                                              :: b__tc2 
real(r8k)                                                              :: b__gsms 
real(r8k)                                                              :: b__time 
real(r8k)                                                              :: b__taxbal 
real(r8k)                                                              :: b__trodfc 
real(r8k)                                                              :: b__dishv0 
real(r8k)                                                              :: b__beta 
real(r8k)                                                              :: b__tpo 
real(r8k)                                                              :: b__cladtk 
real(r8k)                                                              :: b__zm1bal 
real(r8k)                                                              :: b__dzmesh 
real(r8k)                                                              :: b__toldfc 
real(r8k)                                                              :: b__rnbnt 
real(r8k)                                                              :: b__defaulttemp 
real(r8k)                                                              :: b__hbar 
real(r8k)                                                              :: b__e 
real(r8k)                                                              :: b__gapthk 
real(r8k)                                                              :: b__zbot 
real(r8k)                                                              :: b__vplenc 
real(r8k)                                                              :: b__vplenb 
real(r8k)                                                              :: b__bottomplenumgasmoles 
real(r8k)                                                              :: b__bumtp 
real(r8k)                                                              :: b__zvoid2 
real(r8k)                                                              :: b__zp1bal 
real(r8k)                                                              :: b__fqcrit 
real(r8k)                                                              :: b__rshd 
real(r8k)                                                              :: b__tcebal 
real(r8k)                                                              :: b__tempcs 
real(r8k)                                                              :: b__maxfueltemp 
real(r8k)                                                              :: b__pfflec 
real(r8k)                                                              :: b__gapmin 
real(r8k)                                                              :: b__cnfliq 
character(len=8)          , dimension(:)        , allocatable          :: b__emflag 
integer(ipk)              , dimension(:)        , allocatable          :: b__ifstor 
integer(ipk)              , dimension(:)        , allocatable          :: b__ichf 
integer(ipk)              , dimension(:)        , allocatable          :: b__nswpm 
integer(ipk)              , dimension(:)        , allocatable          :: b__tem 
integer(ipk)              , dimension(:)        , allocatable          :: b__imaterials 
integer(ipk)              , dimension(:)        , allocatable          :: b__modfal 
integer(ipk)              , dimension(:)        , allocatable          :: b__idumr1                ! is not used
integer(ipk)              , dimension(:)        , allocatable          :: b__ih 
integer(ipk)              , dimension(:)        , allocatable          :: b__iffrp                 ! is not used
integer(ipk)              , dimension(:)        , allocatable          :: b__ihtreg 
integer(ipk)              , dimension(:)        , allocatable          :: b__indxjk 
integer(ipk)              , dimension(:)        , allocatable          :: b__ibnopt 
integer(ipk)              , dimension(:)        , allocatable          :: b__ruptfailindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__nm 
integer(ipk)              , dimension(:)        , allocatable          :: b__ies 
integer(ipk)              , dimension(:)        , allocatable          :: b__nopt 
integer(ipk)              , dimension(:)        , allocatable          :: b__indexpcmionce 
integer(ipk)              , dimension(:)        , allocatable          :: b__cladcollapseindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__rodfailindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__gapindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__irest2 
integer(ipk)              , dimension(:)        , allocatable          :: b__imatflag 
integer(ipk)              , dimension(:)        , allocatable          :: b__oldgapindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__htco 
integer(ipk)              , dimension(:)        , allocatable          :: b__indextempconverg 
integer(ipk)              , dimension(:)        , allocatable          :: b__bosgapindex 
integer(ipk)              , dimension(:)        , allocatable          :: b__idatapairs 
integer(ipk)              , dimension(:)        , allocatable          :: b__ncs 
integer(ipk)              , dimension(:)        , allocatable          :: b__nvprs 
integer(ipk)              , dimension(:)        , allocatable          :: b__npprs 
integer(ipk)              , dimension(:)        , allocatable          :: b__irest3 
integer(ipk)              , dimension(:)        , allocatable          :: b__bosgapindex0 
integer(ipk)              , dimension(:)        , allocatable          :: b__ntprs 
integer(ipk)              , dimension(:)        , allocatable          :: b__ihdata 
integer(ipk)              , dimension(:)        , allocatable          :: b__imatflag1 
integer(ipk)              , dimension(:)        , allocatable          :: b__cladcollindx0 
integer(ipk)              , dimension(:)        , allocatable          :: b__nunopt 
integer(ipk)              , dimension(:)        , allocatable          :: b__nhprs 
integer(ipk)              , dimension(:)        , allocatable          :: b__nodesintertemp 
integer(ipk)              , dimension(:)        , allocatable          :: b__ngastmp 
integer(ipk)              , dimension(:)        , allocatable          :: b__ndchfi 
integer(ipk)              , dimension(:)        , allocatable          :: b__iheattablen 
integer(ipk)              , dimension(:)        , allocatable          :: b__indexpcmi 
integer(ipk)              , dimension(:)        , allocatable          :: b__numazmuthnod 
real(r8k)                 , dimension(:)        , allocatable          :: b__roi 
real(r8k)                 , dimension(:)        , allocatable          :: b__alphathk11 
real(r8k)                 , dimension(:)        , allocatable          :: b__tbulk_v 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladenrgperl 
real(r8k)                 , dimension(:)        , allocatable          :: b__efffastflustrencoef 
real(r8k)                 , dimension(:)        , allocatable          :: b__scd 
real(r8k)                 , dimension(:)        , allocatable          :: b__watrmetlenrgy 
real(r8k)                 , dimension(:)        , allocatable          :: b__coolpress 
real(r8k)                 , dimension(:)        , allocatable          :: b__dtplta 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaio 
real(r8k)                 , dimension(:)        , allocatable          :: b__apln 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxygenuptake 
real(r8k)                 , dimension(:)        , allocatable          :: b__thermalconductadv 
real(r8k)                 , dimension(:)        , allocatable          :: b__ascal2 
real(r8k)                 , dimension(:)        , allocatable          :: b__gasax 
real(r8k)                 , dimension(:)        , allocatable          :: b__profilestarttime 
real(r8k)                 , dimension(:)        , allocatable          :: b__ascal1 
real(r8k)                 , dimension(:)        , allocatable          :: b__htflxfac 
real(r8k)                 , dimension(:)        , allocatable          :: b__htclev 
real(r8k)                 , dimension(:)        , allocatable          :: b__tbulk_l 
real(r8k)                 , dimension(:)        , allocatable          :: b__prop 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenado 
real(r8k)                 , dimension(:)        , allocatable          :: b__flwblk 
real(r8k)                 , dimension(:)        , allocatable          :: b__tempfc 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenadi 
real(r8k)                 , dimension(:)        , allocatable          :: b__rinterfacgap 
real(r8k)                 , dimension(:)        , allocatable          :: b__stressatinststrain 
real(r8k)                 , dimension(:)        , allocatable          :: b__rinterfacprs0 
real(r8k)                 , dimension(:)        , allocatable          :: b__ts 
real(r8k)                 , dimension(:)        , allocatable          :: b__tp 
real(r8k)                 , dimension(:)        , allocatable          :: b__claddingpower 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcoolprs0 
real(r8k)                 , dimension(:)        , allocatable          :: b__voidvolumeratio 
real(r8k)                 , dimension(:)        , allocatable          :: b__techf 
real(r8k)                 , dimension(:)        , allocatable          :: b__eosoxidethick 
real(r8k)                 , dimension(:)        , allocatable          :: b__cexh2a 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelraddeviat 
real(r8k)                 , dimension(:)        , allocatable          :: b__coolenthalpy0 
real(r8k)                 , dimension(:)        , allocatable          :: b__epstrain1old 
real(r8k)                 , dimension(:)        , allocatable          :: b__azpang 
real(r8k)                 , dimension(:)        , allocatable          :: b__coolmassflx 
real(r8k)                 , dimension(:)        , allocatable          :: b__hmaxinitial 
real(r8k)                 , dimension(:)        , allocatable          :: b__betathickness 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxstalphathkremain 
real(r8k)                 , dimension(:)        , allocatable          :: b__powave 
real(r8k)                 , dimension(:)        , allocatable          :: b__axburnup 
real(r8k)                 , dimension(:)        , allocatable          :: b__apln0 
real(r8k)                 , dimension(:)        , allocatable          :: b__alphathk1 
real(r8k)                 , dimension(:)        , allocatable          :: b__amatpc 
real(r8k)                 , dimension(:)        , allocatable          :: b__fdial 
real(r8k)                 , dimension(:)        , allocatable          :: b__vrlfrp 
real(r8k)                 , dimension(:)        , allocatable          :: b__rinterfacprs 
real(r8k)                 , dimension(:)        , allocatable          :: b__energyperl 
real(r8k)                 , dimension(:)        , allocatable          :: b__dvdt 
real(r8k)                 , dimension(:)        , allocatable          :: b__effcoldwkstrnhardexp 
real(r8k)                 , dimension(:)        , allocatable          :: b__gapthick0 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaci 
real(r8k)                 , dimension(:)        , allocatable          :: b__gbse 
real(r8k)                 , dimension(:)        , allocatable          :: b__fmgp 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcoolprs 
real(r8k)                 , dimension(:)        , allocatable          :: b__sshgap 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacetcmx 
real(r8k)                 , dimension(:)        , allocatable          :: b__sedpnnlold 
real(r8k)                 , dimension(:)        , allocatable          :: b__heatflux 
real(r8k)                 , dimension(:)        , allocatable          :: b__boundarycondition 
real(r8k)                 , dimension(:)        , allocatable          :: b__gadoln 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaco 
real(r8k)                 , dimension(:)        , allocatable          :: b__gfrac 
real(r8k)                 , dimension(:)        , allocatable          :: b__radialpower 
real(r8k)                 , dimension(:)        , allocatable          :: b__stressaxialold 
real(r8k)                 , dimension(:)        , allocatable          :: b__surfhtflux 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcldplasstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__gbh 
real(r8k)                 , dimension(:)        , allocatable          :: b__prevtemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacereloc 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelletrad0 
real(r8k)                 , dimension(:)        , allocatable          :: b__rmrlp 
real(r8k)                 , dimension(:)        , allocatable          :: b__hz2 
real(r8k)                 , dimension(:)        , allocatable          :: b__fastflux 
real(r8k)                 , dimension(:)        , allocatable          :: b__bbht1 
real(r8k)                 , dimension(:)        , allocatable          :: b__pbh 
real(r8k)                 , dimension(:)        , allocatable          :: b__gaptemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__arest1 
real(r8k)                 , dimension(:)        , allocatable          :: b__dvdt0 
real(r8k)                 , dimension(:)        , allocatable          :: b__drdfrp 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldgasprs 
real(r8k)                 , dimension(:)        , allocatable          :: b__fuelgasswell 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxuptakeid2 
real(r8k)                 , dimension(:)        , allocatable          :: b__stresshoopold 
real(r8k)                 , dimension(:)        , allocatable          :: b__effcoldwkstrencoef 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenago 
real(r8k)                 , dimension(:)        , allocatable          :: b__rodod0 
real(r8k)                 , dimension(:)        , allocatable          :: b__dtmaxa 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelletrad 
real(r8k)                 , dimension(:)        , allocatable          :: b__edint 
real(r8k)                 , dimension(:)        , allocatable          :: b__alphathk2 
real(r8k)                 , dimension(:)        , allocatable          :: b__axialpowr 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelsrfdispl 
real(r8k)                 , dimension(:)        , allocatable          :: b__hgapav 
real(r8k)                 , dimension(:)        , allocatable          :: b__tmelt 
real(r8k)                 , dimension(:)        , allocatable          :: b__temptm 
real(r8k)                 , dimension(:)        , allocatable          :: b__enthl 
real(r8k)                 , dimension(:)        , allocatable          :: b__flthit 
real(r8k)                 , dimension(:)        , allocatable          :: b__bpln 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladmaxt 
real(r8k)                 , dimension(:)        , allocatable          :: b__qaxzq 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaii 
real(r8k)                 , dimension(:)        , allocatable          :: b__vfrad1 
real(r8k)                 , dimension(:)        , allocatable          :: b__vfrad3 
real(r8k)                 , dimension(:)        , allocatable          :: b__vfrad2 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenafo 
real(r8k)                 , dimension(:)        , allocatable          :: b__tend 
real(r8k)                 , dimension(:)        , allocatable          :: b__aexcb 
real(r8k)                 , dimension(:)        , allocatable          :: b__sedepri 
real(r8k)                 , dimension(:)        , allocatable          :: b__alphathk22 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxithk1 
real(r8k)                 , dimension(:)        , allocatable          :: b__bufrp 
real(r8k)                 , dimension(:)        , allocatable          :: b__hz1 
real(r8k)                 , dimension(:)        , allocatable          :: b__swelldispl 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxithk2 
real(r8k)                 , dimension(:)        , allocatable          :: b__tshrda 
real(r8k)                 , dimension(:)        , allocatable          :: b__vs0 
real(r8k)                 , dimension(:)        , allocatable          :: b__acoold 
real(r8k)                 , dimension(:)        , allocatable          :: b__hgpfrp 
real(r8k)                 , dimension(:)        , allocatable          :: b__pbh2 
real(r8k)                 , dimension(:)        , allocatable          :: b__hlqcl 
real(r8k)                 , dimension(:)        , allocatable          :: b__finaltemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__techfi 
real(r8k)                 , dimension(:)        , allocatable          :: b__pbh1 
real(r8k)                 , dimension(:)        , allocatable          :: b__aflcht 
real(r8k)                 , dimension(:)        , allocatable          :: b__bu 
real(r8k)                 , dimension(:)        , allocatable          :: b__ecr 
real(r8k)                 , dimension(:)        , allocatable          :: b__vreloc0 
real(r8k)                 , dimension(:)        , allocatable          :: b__epstrain2old 
real(r8k)                 , dimension(:)        , allocatable          :: b__hlqclp 
real(r8k)                 , dimension(:)        , allocatable          :: b__bpln0 
real(r8k)                 , dimension(:)        , allocatable          :: b__pz2 
real(r8k)                 , dimension(:)        , allocatable          :: b__cesiumcontent 
real(r8k)                 , dimension(:)        , allocatable          :: b__z1 
real(r8k)                 , dimension(:)        , allocatable          :: b__z2 
real(r8k)                 , dimension(:)        , allocatable          :: b__faxzq 
real(r8k)                 , dimension(:)        , allocatable          :: b__fluxz 
real(r8k)                 , dimension(:)        , allocatable          :: b__gapthick 
real(r8k)                 , dimension(:)        , allocatable          :: b__hbh 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldepeff 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldeffstrainpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladavetemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__nodchf 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldgasprs0 
real(r8k)                 , dimension(:)        , allocatable          :: b__aasth 
real(r8k)                 , dimension(:)        , allocatable          :: b__aprntb 
real(r8k)                 , dimension(:)        , allocatable          :: b__volopenpor 
real(r8k)                 , dimension(:)        , allocatable          :: b__farbal 
real(r8k)                 , dimension(:)        , allocatable          :: b__extentofbow 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcldaxstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: b__sdfar 
real(r8k)                 , dimension(:)        , allocatable          :: b__ablona 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladeffstress 
real(r8k)                 , dimension(:)        , allocatable          :: b__rodavepower 
real(r8k)                 , dimension(:)        , allocatable          :: b__gasmolesax0 
real(r8k)                 , dimension(:)        , allocatable          :: b__cooldensity0 
real(r8k)                 , dimension(:)        , allocatable          :: b__ascal3 
real(r8k)                 , dimension(:)        , allocatable          :: b__arrayf 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldpeldis 
real(r8k)                 , dimension(:)        , allocatable          :: b__edotfz 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacerco 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspaceep1 
real(r8k)                 , dimension(:)        , allocatable          :: b__cldpermstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__hydrogencontent 
real(r8k)                 , dimension(:)        , allocatable          :: b__gasfraction 
real(r8k)                 , dimension(:)        , allocatable          :: b__tz2 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacerci 
real(r8k)                 , dimension(:)        , allocatable          :: b__hfusn 
real(r8k)                 , dimension(:)        , allocatable          :: b__prcntsatbeta 
real(r8k)                 , dimension(:)        , allocatable          :: b__qpln0 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaai 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldpeldis0 
real(r8k)                 , dimension(:)        , allocatable          :: b__flowg 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaao 
real(r8k)                 , dimension(:)        , allocatable          :: b__iodinecontent 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldfuelaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__terfacepres 
real(r8k)                 , dimension(:)        , allocatable          :: b__axnodelevat 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladsurft 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenabi 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladyieldstress 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxideid 
real(r8k)                 , dimension(:)        , allocatable          :: b__swd 
real(r8k)                 , dimension(:)        , allocatable          :: b__tschfi 
real(r8k)                 , dimension(:)        , allocatable          :: b__strainrateterm 
real(r8k)                 , dimension(:)        , allocatable          :: b__hsolold 
real(r8k)                 , dimension(:)        , allocatable          :: b__gasmolesax 
real(r8k)                 , dimension(:)        , allocatable          :: b__hfxsum 
real(r8k)                 , dimension(:)        , allocatable          :: b__pclrlp 
real(r8k)                 , dimension(:)        , allocatable          :: b__elevatthermhydr 
real(r8k)                 , dimension(:)        , allocatable          :: b__timeofgbsep 
real(r8k)                 , dimension(:)        , allocatable          :: b__effstrain0 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxideod 
real(r8k)                 , dimension(:)        , allocatable          :: b__coolqual 
real(r8k)                 , dimension(:)        , allocatable          :: b__crithtflux 
real(r8k)                 , dimension(:)        , allocatable          :: b__axialnodlen 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenahi 
real(r8k)                 , dimension(:)        , allocatable          :: b__effstrain 
real(r8k)                 , dimension(:)        , allocatable          :: b__filmcoeffav 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacepint 
real(r8k)                 , dimension(:)        , allocatable          :: b__sedpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: b__heatflux0 
real(r8k)                 , dimension(:)        , allocatable          :: b__rmassflux 
real(r8k)                 , dimension(:)        , allocatable          :: b__qmaxmelt 
real(r8k)                 , dimension(:)        , allocatable          :: b__cldpermhoopstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__stressradialold 
real(r8k)                 , dimension(:)        , allocatable          :: b__fuelsrfstrrat 
real(r8k)                 , dimension(:)        , allocatable          :: b__cladeffstressold 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaeo 
real(r8k)                 , dimension(:)        , allocatable          :: b__cracktemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__thermalconductivity 
real(r8k)                 , dimension(:)        , allocatable          :: b__fldrat 
real(r8k)                 , dimension(:)        , allocatable          :: b__qmaxmeltp1 
real(r8k)                 , dimension(:)        , allocatable          :: b__pgpfrp 
real(r8k)                 , dimension(:)        , allocatable          :: b__radpel 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaei 
real(r8k)                 , dimension(:)        , allocatable          :: b__coefm 
real(r8k)                 , dimension(:)        , allocatable          :: b__coefn 
real(r8k)                 , dimension(:)        , allocatable          :: b__coefk 
real(r8k)                 , dimension(:)        , allocatable          :: b__openportemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__rodod 
real(r8k)                 , dimension(:)        , allocatable          :: b__fuelsrfstrrat0 
real(r8k)                 , dimension(:)        , allocatable          :: b__deltox 
real(r8k)                 , dimension(:)        , allocatable          :: b__vreloc 
real(r8k)                 , dimension(:)        , allocatable          :: b__cldpermaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__fmesh 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelprm 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacegap 
real(r8k)                 , dimension(:)        , allocatable          :: b__cmesh 
real(r8k)                 , dimension(:)        , allocatable          :: b__gsmol0 
real(r8k)                 , dimension(:)        , allocatable          :: b__openporvol 
real(r8k)                 , dimension(:)        , allocatable          :: b__bowrat 
real(r8k)                 , dimension(:)        , allocatable          :: b__gappr0 
real(r8k)                 , dimension(:)        , allocatable          :: b__vplen 
real(r8k)                 , dimension(:)        , allocatable          :: b__arraye 
real(r8k)                 , dimension(:)        , allocatable          :: b__openporosity 
real(r8k)                 , dimension(:)        , allocatable          :: b__centvoidvol 
real(r8k)                 , dimension(:)        , allocatable          :: b__bulkcooltemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__crackvolume 
real(r8k)                 , dimension(:)        , allocatable          :: b__butemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__hydrostatpress 
real(r8k)                 , dimension(:)        , allocatable          :: b__gasphs 
real(r8k)                 , dimension(:)        , allocatable          :: b__hlqcl1 
real(r8k)                 , dimension(:)        , allocatable          :: b__dtdkta 
real(r8k)                 , dimension(:)        , allocatable          :: b__gaspress0 
real(r8k)                 , dimension(:)        , allocatable          :: b__pelsrfstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspaceurc 
real(r8k)                 , dimension(:)        , allocatable          :: b__hupta 
real(r8k)                 , dimension(:)        , allocatable          :: b__spl 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxuptakeid1 
real(r8k)                 , dimension(:)        , allocatable          :: b__fuelsurft 
real(r8k)                 , dimension(:)        , allocatable          :: b__aaht1 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenaho 
real(r8k)                 , dimension(:)        , allocatable          :: b__eppinp 
real(r8k)                 , dimension(:)        , allocatable          :: b__ispace 
real(r8k)                 , dimension(:)        , allocatable          :: b__pswll0 
real(r8k)                 , dimension(:)        , allocatable          :: b__bosoxidethick 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenagi 
real(r8k)                 , dimension(:)        , allocatable          :: b__dishvperl 
real(r8k)                 , dimension(:)        , allocatable          :: b__qpln 
real(r8k)                 , dimension(:)        , allocatable          :: b__sedepriold 
real(r8k)                 , dimension(:)        , allocatable          :: b__radialbound 
real(r8k)                 , dimension(:)        , allocatable          :: b__explenumt 
real(r8k)                 , dimension(:)        , allocatable          :: b__crackwidth 
real(r8k)                 , dimension(:)        , allocatable          :: b__edotfz0 
real(r8k)                 , dimension(:)        , allocatable          :: b__coolenthalpy 
real(r8k)                 , dimension(:)        , allocatable          :: b__dtpoa 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcladt 
real(r8k)                 , dimension(:)        , allocatable          :: b__vsn 
real(r8k)                 , dimension(:)        , allocatable          :: b__ureloc 
real(r8k)                 , dimension(:)        , allocatable          :: b__afrap 
real(r8k)                 , dimension(:)        , allocatable          :: b__efffastflustrnhardexp 
real(r8k)                 , dimension(:)        , allocatable          :: b__epstrain3old 
real(r8k)                 , dimension(:)        , allocatable          :: b__gaspress 
real(r8k)                 , dimension(:)        , allocatable          :: b__radpowprofile 
real(r8k)                 , dimension(:)        , allocatable          :: b__relfraca 
real(r8k)                 , dimension(:)        , allocatable          :: b__effstrainpnnl 
real(r8k)                 , dimension(:)        , allocatable          :: b__tz1 
real(r8k)                 , dimension(:)        , allocatable          :: b__emodulus 
real(r8k)                 , dimension(:)        , allocatable          :: b__einstabilstrain 
real(r8k)                 , dimension(:)        , allocatable          :: b__tschf 
real(r8k)                 , dimension(:)        , allocatable          :: b__elvrad 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcladt0 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldfuelaxstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: b__ansr 
real(r8k)                 , dimension(:)        , allocatable          :: b__gfint 
real(r8k)                 , dimension(:)        , allocatable          :: b__pz1 
real(r8k)                 , dimension(:)        , allocatable          :: b__axialpowr0 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxygenconcenave 
real(r8k)                 , dimension(:)        , allocatable          :: b__a1 
real(r8k)                 , dimension(:)        , allocatable          :: b__zfarbl 
real(r8k)                 , dimension(:)        , allocatable          :: b__fuelcentert 
real(r8k)                 , dimension(:)        , allocatable          :: b__workspacegapi 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldeps 
real(r8k)                 , dimension(:)        , allocatable          :: b__hinta 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxstalphathknearfuel 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenabo 
real(r8k)                 , dimension(:)        , allocatable          :: b__avedishtemp 
real(r8k)                 , dimension(:)        , allocatable          :: b__gz2 
real(r8k)                 , dimension(:)        , allocatable          :: b__gz1 
real(r8k)                 , dimension(:)        , allocatable          :: b__oldcldaxstrn 
real(r8k)                 , dimension(:)        , allocatable          :: b__cooldensity 
real(r8k)                 , dimension(:)        , allocatable          :: b__prestm 
real(r8k)                 , dimension(:)        , allocatable          :: b__cldpermstrn0 
real(r8k)                 , dimension(:)        , allocatable          :: b__elevatfrap 
real(r8k)                 , dimension(:)        , allocatable          :: b__pinput 
real(r8k)                 , dimension(:)        , allocatable          :: b__htc_v 
real(r8k)                 , dimension(:)        , allocatable          :: b__potput 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxconcenafi 
real(r8k)                 , dimension(:)        , allocatable          :: b__acolct 
real(r8k)                 , dimension(:)        , allocatable          :: b__zelev 
real(r8k)                 , dimension(:)        , allocatable          :: b__gadolin 
real(r8k)                 , dimension(:)        , allocatable          :: b__htc_l 
real(r8k)                 , dimension(:)        , allocatable          :: b__oxipowergen 
integer(ipk)              , dimension(:,:)      , allocatable          :: b__icglob 
integer(ipk)              , dimension(:,:)      , allocatable          :: b__nrc 
integer(ipk)              , dimension(:,:)      , allocatable          :: b__iokold 
integer(ipk)              , dimension(:,:)      , allocatable          :: b__iok 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__gasavail1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__tmprlp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__gasavail2 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__volumeweightr 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__enrgymeltz 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__acond 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldthermstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__frapcontemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__baln2twall 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__qcold 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__fuelresidstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__tplbt1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__previteratetemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__buradv 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__rhocp0 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__tblka 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__burad 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__htca 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__enrgymelt 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldstrnrat 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__deformedradiusofmesh 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__bosrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__pelsrfstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__eosrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__radsrc 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldplasstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldstress 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__rhocp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__radialboundo 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__volumeweightl 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__axpowprofile 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__enrgymeltzp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__pazp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__tpln 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__radpow 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__areaweight 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__eostemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__pchn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__trad2 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__trad3 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__trad1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__gasths 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__fuelrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldresidstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__dktouts 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldstrnrat0 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldplasstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__bufrad 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__radsrco 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldresidstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__radtemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__workspaceepp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__fuelresidstrn0 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldelstrn 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__enrgymeltp1 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__tplbot 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__bostemp 
real(r8k)                 , dimension(:,:)      , allocatable          :: b__cldstrn 
real(r8k)                 , dimension(:,:,:)    , allocatable          :: b__grsv 
real(r8k)                 , dimension(:,:,:)    , allocatable          :: b__tplna 

