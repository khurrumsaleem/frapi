    ALLOCATE (this % b__qend(1:im+1))
    ALLOCATE (this % b__avgqi(1:im+1))
    ALLOCATE (this % b__go(1:im+1))
    ALLOCATE (this % b__p2(1:im+1))
    ALLOCATE (this % b__ProblemTime(0:im+1))
    ALLOCATE (this % b__ProblemTime_Prev(0:im+1))
    ALLOCATE (this % b__qmpy(1:im+1))
    ALLOCATE (this % b__tw(1:im+1))
    ALLOCATE (this % b__p1(1:im+1))
    ALLOCATE (this % b__jn(1:im+1))
    ALLOCATE (this % b__jnsurftemp(1:im+1))
    ALLOCATE (this % b__jpeak(1:im+1))
    ALLOCATE (this % b__jst(1:im+1))
    ALLOCATE (this % b__jstsurftemp(1:im+1))
    ALLOCATE (this % b__acmfg(1:im+1))
    ALLOCATE (this % b__acmhe(1:im+1))
    ALLOCATE (this % b__acmH2(1:im+1))
    ALLOCATE (this % b__acmH2O(1:im+1))
    ALLOCATE (this % b__acmn2(1:im+1))
    ALLOCATE (this % b__amgpt(1:im+1))
    ALLOCATE (this % b__gasmo(0:im+1))
    ALLOCATE (this % b__hmgpt(1:im+1))
    ALLOCATE (this % b__tafa(1:im+1))
    ALLOCATE (this % b__taga(1:im+1))
    ALLOCATE (this % b__tsfa(1:im+1))
    ALLOCATE (this % b__taca(0:im+1))
    ALLOCATE (this % b__pkODCladTemp(0:im+1))
    ALLOCATE (this % b__pkPelAveTemp(0:im+1))
    ALLOCATE (this % b__pkPower(0:im+1))
    ALLOCATE (this % b__pkAveCladTemp(0:im+1))
    ALLOCATE (this % b__pkIDCladTemp(0:im+1))
    ALLOCATE (this % b__pkGap(0:im+1))
    ALLOCATE (this % b__pkFisGasRelFrac(0:im+1))
    ALLOCATE (this % b__pkPelSurfTemp(0:im+1))
    ALLOCATE (this % b__pkH2up(0:im+1))
    ALLOCATE (this % b__pkPelCentTemp(0:im+1))
    ALLOCATE (this % b__pkIntefacePres(0:im+1))    
    ALLOCATE (this % b__pit(0:im+1))
    ALLOCATE (this % b__pkHoopStres(0:im+1))
    ALLOCATE (this % b__pkAxlStres(0:im+1))
    ALLOCATE (this % b__pkBurnup(0:im+1))
    ALLOCATE (this % b__pkHoopStrain(0:im+1))
    ALLOCATE (this % b__pkFuelPelOD(0:im+1))
    ALLOCATE (this % b__pkGapCond(0:im+1))
    ALLOCATE (this % b__pkZrO2(0:im+1))
    ALLOCATE (this % b__HeatFlux(1:im+1))
    ALLOCATE (this % b__dt(1:im+2))
    ALLOCATE (this % b__buavearray(1:im+1))
    ALLOCATE (this % b__voidvolarray(1:im+1))
    ALLOCATE (this % b__he_ifba(1:im+1))
    ALLOCATE (this % b__addgmles(1:im+1))
    ALLOCATE (this % b__addswell(1:im+1))
    ALLOCATE (this % b__CycleSteps(1:im+1))
    ALLOCATE (this % b__BulkCoolantTemp(1:na+1))
    ALLOCATE (this % b__rhof(1:na))
    ALLOCATE (this % b__dpwxrate(1:na))
    ALLOCATE (this % b__axialnode(1:na))
    ALLOCATE (this % b__PelletRad(1:na))
    ALLOCATE (this % b__FastFluxd(1:na))
    ALLOCATE (this % b__FastFluenced(1:na))
    ALLOCATE (this % b__totinner(1:na))
    ALLOCATE (this % b__totcrl(1:na))
    ALLOCATE (this % b__CladAveTemp(1:na))
    ALLOCATE (this % b__PelAveTemp(1:na))
    ALLOCATE (this % b__GapAveTemp(1:na))
    ALLOCATE (this % b__PelSurfTemp(1:na))
    ALLOCATE (this % b__PelCentTemp(1:na))
    ALLOCATE (this % b__sigeff(1:na))
    ALLOCATE (this % b__FuelSurfDispl(1:na))
    ALLOCATE (this % b__CladInSurDisp(1:na))
    ALLOCATE (this % b__CladInPermDef(1:na))
    ALLOCATE (this % b__sigy(1:na))
    ALLOCATE (this % b__AxialNodLength(1:na))
    ALLOCATE (this % b__CladEffPlasStrain(1:na))
    ALLOCATE (this % b__RinterfacPress(1:na))
    ALLOCATE (this % b__FuelCladGap(1:na))
    ALLOCATE (this % b__GapPress(1:na))
    ALLOCATE (this % b__CoolantPress(1:na))
    ALLOCATE (this % b__PlastStrnep1(1:na))
    ALLOCATE (this % b__OldCladStrn(1:na))
    ALLOCATE (this % b__OldFuelStrn(1:na))
    ALLOCATE (this % b__OldGapPress(1:na))
    ALLOCATE (this % b__OldCoolPress(1:na))
    ALLOCATE (this % b__OldFuelDispl(1:na))
    ALLOCATE (this % b__OldCladAvTemp(1:na))
    ALLOCATE (this % b__CreepStrain(1:na))
    ALLOCATE (this % b__CreepStrain1(1:na))
    ALLOCATE (this % b__CladDiamHot(1:na))
    ALLOCATE (this % b__PrevCladEffPlasStrn(1:na))
    ALLOCATE (this % b__PrevFuelStrain(1:na))
    ALLOCATE (this % b__HotNodLength(1:na))
    ALLOCATE (this % b__PermFuelDispl(1:na))
    ALLOCATE (this % b__BOSNodeburnup(1:na))
    ALLOCATE (this % b__EOSNodeburnup(1:na))
    ALLOCATE (this % b__StepNodeburnup(1:na))
    ALLOCATE (this % b__TotalHgap(1:na))
    ALLOCATE (this % b__SolidHgap(1:na))
    ALLOCATE (this % b__GasHgap(1:na))
    ALLOCATE (this % b__RadHgap(1:na))
    ALLOCATE (this % b__FastFlux(1:na))
    ALLOCATE (this % b__FastFluence(1:na))
    ALLOCATE (this % b__FuelPorosity(1:na))
    ALLOCATE (this % b__FilmCoefficient(1:na))
    ALLOCATE (this % b__EstGapDeltaT(1:na))
    ALLOCATE (this % b__BOSZrO2Thk(1:na))
    ALLOCATE (this % b__EOSZrO2Thk(1:na))
    ALLOCATE (this % b__ZrO2ThkNoAd(1:na))
    ALLOCATE (this % b__FuelCondFactor(1:na))
    ALLOCATE (this % b__Relocation(1:na))
    ALLOCATE (this % b__RinternalVolume(1:na))
    ALLOCATE (this % b__CladVolume(1:na))
    ALLOCATE (this % b__CrackVolume(1:na))
    ALLOCATE (this % b__RinterfacVolume(1:na))
    ALLOCATE (this % b__FuelVolume(1:na))
    ALLOCATE (this % b__fden(1:na))
    ALLOCATE (this % b__GapVolume(1:na))
    ALLOCATE (this % b__PorosityVolume(1:na))
    ALLOCATE (this % b__SurfTempOxide(1:na))
    ALLOCATE (this % b__AnnulusVolume(1:na))
    ALLOCATE (this % b__gapplot(1:na))
    ALLOCATE (this % b__PrevOldCoolPress(1:na))
    ALLOCATE (this % b__PrevOldGapPress(1:na))
    ALLOCATE (this % b__PrevOldCladAvTemp(1:na))
    ALLOCATE (this % b__PrevOldFuelDispl(1:na))
    ALLOCATE (this % b__PrevCreepStrain(1:na))
    ALLOCATE (this % b__CladIrradGrowStrn(1:na))
    ALLOCATE (this % b__UniformAxNodStrn(1:na))
    ALLOCATE (this % b__CladH2Concen(1:na))
    ALLOCATE (this % b__ExcessH2Concen(1:na))
    ALLOCATE (this % b__PrevCladStrain(1:na))
    ALLOCATE (this % b__StartofStepH2Con(1:na))
    ALLOCATE (this % b__StartofStepPickupH2Con(1:na))
    ALLOCATE (this % b__EndofStepPickupH2Con(1:na))
    ALLOCATE (this % b__FuelTempRestruRad(1:na))
    ALLOCATE (this % b__OldHealedCrackRadius(1:na))
    ALLOCATE (this % b__StoredEnergy(1:na))
    ALLOCATE (this % b__HealedCrackRadius(1:na))
    ALLOCATE (this % b__HotThermalGap(1:na))
    ALLOCATE (this % b__WorkArray1(1:na))
    ALLOCATE (this % b__WorkArray2(1:na))
    ALLOCATE (this % b__WorkArray3(1:na))
    ALLOCATE (this % b__CladInSurfTemp(1:na))
    ALLOCATE (this % b__CladOutSurfTemp(1:na))
    ALLOCATE (this % b__Power(1:na))
    ALLOCATE (this % b__gpthe(1:30))
    ALLOCATE (this % b__gpth(1:30))
    ALLOCATE (this % b__gpthpg(1:na))
    ALLOCATE (this % b__qc(1:na))
    ALLOCATE (this % b__totdef(1:na))
    ALLOCATE (this % b__IgapGapIndex(1:na))
    ALLOCATE (this % b__IgapIndexOld(1:na))
    ALLOCATE (this % b__IgapIndexPrevOld(1:na))
    ALLOCATE (this % b__buin(1:na))
    ALLOCATE (this % b__dci(1:na))
    ALLOCATE (this % b__dco(1:na))
    ALLOCATE (this % b__cdg(1:na))
    ALLOCATE (this % b__thkcld(1:na))
    ALLOCATE (this % b__thkgap(1:na))
    ALLOCATE (this % b__de(1:na))
    ALLOCATE (this % b__deltaz(1:na))
    ALLOCATE (this % b__comp(1:na))
    ALLOCATE (this % b__ctmax(1:na))
    ALLOCATE (this % b__dp(1:na))
    ALLOCATE (this % b__flux(1:na))
    ALLOCATE (this % b__crudmult(1:na))
    ALLOCATE (this % b__crdtt(1:na))
    ALLOCATE (this % b__rdotwrt(1:na))
    ALLOCATE (this % b__FDItave(1:na))
    ALLOCATE (this % b__creapratearray(1:na))
    ALLOCATE (this % b__GapCond(1:na))
    ALLOCATE (this % b__fuelexptot(1:na))
    ALLOCATE (this % b__fuelswltot(1:na))
    ALLOCATE (this % b__fuelcreeptot(1:na))
    ALLOCATE (this % b__fueldentot(1:na))
    ALLOCATE (this % b__fuelburntot(1:na))
    ALLOCATE (this % b__cladcrptot(1:na))
    ALLOCATE (this % b__gapHTC(1:na))
    ALLOCATE (this % b__oxidelayer(1:na))
    ALLOCATE (this % b__gapmech(1:na))
    ALLOCATE (this % b__gapthrm(1:na))
    ALLOCATE (this % b__fuelrelmod(1:na))
    ALLOCATE (this % b__ifbarel(1:na))
    ALLOCATE (this % b__zcool(1:na))
    ALLOCATE (this % b__tcoolant(1:naxim))
    ALLOCATE (this % b__pcoolant(1:naxim))
    ALLOCATE (this % b__x(1:naxim))
    ALLOCATE (this % b__qf(1:naxim))
    ALLOCATE (this % b__xt(1:naxim))
    ALLOCATE (this % b__cladt(1:naxim))
    ALLOCATE (this % b__NodalMoles(1:na))
    ALLOCATE (this % b__NodalGMLES(1:na))
    ALLOCATE (this % b__rlcstrn(1:na))
    ALLOCATE (this % b__rlcstrnold(1:na))
    ALLOCATE (this % b__rc(1:na))
    ALLOCATE (this % b__gadoln(1:na))
    ALLOCATE (this % b__stold(1:na))
    ALLOCATE (this % b__stnew(1:na))
    ALLOCATE (this % b__deltimeold(1:na))
    ALLOCATE (this % b__deltimenew(1:na))
    ALLOCATE (this % b__sagold(1:na))
    ALLOCATE (this % b__sagnew(1:na))
    ALLOCATE (this % b__delsagold(1:na))
    ALLOCATE (this % b__delsagnew(1:na))
    ALLOCATE (this % b__delst(1:na))
    ALLOCATE (this % b__colddef(1:na))
    ALLOCATE (this % b__colddec(1:na))
    ALLOCATE (this % b__qnode(1:na))
    ALLOCATE (this % b__heprod(1:na))
    ALLOCATE (this % b__he(1:na))
    ALLOCATE (this % b__heold(1:na))
    ALLOCATE (this % b__boron10(1:na))
    ALLOCATE (this % b__enrch(1:na))
    ALLOCATE (this % b__epsav(1:na))
    ALLOCATE (this % b__wimsburnup(1:na))
    ALLOCATE (this % b__oldwimsburnup(1:na))
    ALLOCATE (this % b__coolanttemp(1:im+1,1:na+1))
    ALLOCATE (this % b__coolantpressure(1:im+1,1:na+1))
    ALLOCATE (this % b__storedearray(1:im+1,1:na))
    ALLOCATE (this % b__cltemparray(1:im+1,1:na))
    ALLOCATE (this % b__buarray(1:im+1,1:na))
    ALLOCATE (this % b__strainarray(1:im+1,1:na))
    ALLOCATE (this % b__straindiffarray(1:im+1,1:na))
    ALLOCATE (this % b__dpwxarray(1:im+1,1:na))
    ALLOCATE (this % b__creaparray(1:im+1,1:na))
    ALLOCATE (this % b__dumarray3(1:im+1,1:na))
    ALLOCATE (this % b__formf(1:na,1:nr))
    ALLOCATE (this % b__ringvol(1:nr,1:na))
    ALLOCATE (this % b__coldringl(1:nr,1:na))
    ALLOCATE (this % b__porosold(1:nr,1:na))
    ALLOCATE (this % b__porosnew(1:nr,1:na))
    ALLOCATE (this % b__cladtarray(1:im+1,1:na+1))
    ALLOCATE (this % b__rrev(1:nr,1:na))
    ALLOCATE (this % b__qaxnorm(1:na,1:im+1))
    ALLOCATE (this % b__crad(1:nr,1:na))
    ALLOCATE (this % b__hrad(1:nr,1:na))
    ALLOCATE (this % b__rapow(1:nr,1:na))
    ALLOCATE (this % b__uo2exp(1:nr,1:na))
    ALLOCATE (this % b__dpw(1:nr,1:na))
    ALLOCATE (this % b__dpwpp(1:nr,1:na))
    ALLOCATE (this % b__dpw2(1:nr,1:na))
    ALLOCATE (this % b__dpwpp2(1:nr,1:na))
    ALLOCATE (this % b__densf(1:nr,1:na))
    ALLOCATE (this % b__densp(1:nr,1:na))
    ALLOCATE (this % b__hotringl(1:nr,1:na))
    ALLOCATE (this % b__tmpfuel(1:nr,1:na))
    ALLOCATE (this % b__gasavail1(1:ngasr,1:na))
    ALLOCATE (this % b__gasavail2(1:ngasr,1:na))
    ALLOCATE (this % b__angr(1:na,2))
    ALLOCATE (this % b__ah2ogr(1:na,2))
    ALLOCATE (this % b__fgmgp(1:na,2))
    ALLOCATE (this % b__fmgr(1:na,2))
    ALLOCATE (this % b__hemgp(1:na,2))
    ALLOCATE (this % b__hmgr(1:na,2))
    ALLOCATE (this % b__ang(1:na,2))
    ALLOCATE (this % b__ah2og(1:na,2))
    ALLOCATE (this % b__epp(1:na,3))
    ALLOCATE (this % b__eppp(1:na,3))
    ALLOCATE (this % b__eppsv(1:na,3))
    ALLOCATE (this % b__eps(1:na,3))
    ALLOCATE (this % b__feps(1:na,3))
    ALLOCATE (this % b__reps(1:na,3))
    ALLOCATE (this % b__repsv(1:na,3))
    ALLOCATE (this % b__repsp(1:na,3))
    ALLOCATE (this % b__rfeps(1:na,3))
    ALLOCATE (this % b__rfepp(1:na,3))
    ALLOCATE (this % b__rfpsv(1:na,3))
    ALLOCATE (this % b__sig(1:na,3))
    ALLOCATE (this % b__epp1(1:na,3))
    ALLOCATE (this % b__sig1(1:na,3))
    ALLOCATE (this % b__eps1(1:na,3))
    ALLOCATE (this % b__ThermalStrain(1:na,3))
    ALLOCATE (this % b__RB_axial(15,1:na))
    ALLOCATE (this % b__RB_rod(15,1:im+1))
    ALLOCATE (this % b__prdct(1:ngasr,1:na,1:im+1))
    ALLOCATE (this % b__brnup3(1:na,1:nr,2))
    ALLOCATE (this % b__ansd(1:ngasr,1:na,1:im+1))
    ALLOCATE (this % b__ansr(1:ngasr))
    ALLOCATE (this % b__gp(1:ngasr,1:na))
    ALLOCATE (this % b__gpold(1:ngasr,1:na))
    ALLOCATE (this % b__rls(1:ngasr,1:na))
    ALLOCATE (this % b__rlsold(1:ngasr,1:na))
    ALLOCATE (this % b__brn(1:ngasr,1:na))
    ALLOCATE (this % b__brnrefab(1:ngasr,1:na))
    ALLOCATE (this % b__EOSNodeburnuprefab(1:na))
    ALLOCATE (this % b__brnold(1:ngasr,1:na))
    ALLOCATE (this % b__grnold(1:ngasr,1:na))
    ALLOCATE (this % b__frdenold(1:ngasr,1:na))
    ALLOCATE (this % b__gbtot(1:ngasr,1:na))
    ALLOCATE (this % b__tempkold(1:ngasr,1:na))
    ALLOCATE (this % b__pf(1:ngasr,1:na))
    ALLOCATE (this % b__decay(1:11))
    ALLOCATE (this % b__half(1:11))
    ALLOCATE (this % b__openp(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__gb(1:ngasr,1:na,1:3))
    ALLOCATE (this % b__grs(1:ngasr,1:na,1:3))
    ALLOCATE (this % b__gg(1:ngasr,1:na,1:3))
    ALLOCATE (this % b__gbold(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__ggold(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__grsold(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g1(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g2(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g3(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g4(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g1old(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g2old(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g3old(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__g4old(1:ngasr,1:na,1:2))
    ALLOCATE (this % b__brnup1(1:na-1,1:2))
    ALLOCATE (this % b__conU235(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__conU238(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__conPu239(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__conPu240(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__conPu241(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__conPu242(1:na-1,1:nr,1:2))
    ALLOCATE (this % b__coU235av(1:na-1))
    ALLOCATE (this % b__coU238av(1:na-1))
    ALLOCATE (this % b__coPu239av(1:na-1))
    ALLOCATE (this % b__coPu240av(1:na-1))
    ALLOCATE (this % b__coPu241av(1:na-1))
    ALLOCATE (this % b__coPu242av(1:na-1))
    ALLOCATE (this % b__enriU235(1:na-1))
    ALLOCATE (this % b__enriU238(1:na-1))
    ALLOCATE (this % b__enriPu239(1:na-1))
    ALLOCATE (this % b__enriPu240(1:na-1))
    ALLOCATE (this % b__enriPu241(1:na-1))
    ALLOCATE (this % b__enriPu242(1:na-1))
    ALLOCATE (this % b__pfU235(1:na-1,0:im+1))
    ALLOCATE (this % b__pfU238(1:na-1,0:im+1))
    ALLOCATE (this % b__pfPu239(1:na-1,0:im+1))
    ALLOCATE (this % b__pfPu240(1:na-1,0:im+1))
    ALLOCATE (this % b__pfPu241(1:na-1,0:im+1))
    ALLOCATE (this % b__pfPu242(1:na-1,0:im+1))
    ALLOCATE (this % b__creeptabstress(1:ncreeptab))
    ALLOCATE (this % b__tfuelr(1:nr))
    ALLOCATE (this % b__tfring(1:nr))
    ALLOCATE (this % b__gases(1:ngases))
    ALLOCATE (this % b__rrapow(1:nr))
    ALLOCATE (this % b__tfuelr2(1:nr))
    ALLOCATE (this % b__creeptabtemp(1:ncreeptab))
    ALLOCATE (this % b__creeptabtime(1:ncreeptab))
    ALLOCATE (this % b__axlinpower(1:na-1))
