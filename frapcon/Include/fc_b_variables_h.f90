    LOGICAL                                               :: b__calcoxide                             
    LOGICAL                                               :: b__updated_restart                       
    LOGICAL                                               :: b__gasflg                                
    LOGICAL                                               :: b__nconvg                                
    LOGICAL                                               :: b__ncont                                 
    LOGICAL                                               :: b__IsModelingSpentFuel    
    LOGICAL                                               :: b__hgapt_flag
    LOGICAL                                               :: b__flag_iapws
    Character   (LEN=80)                                  :: b__title                                 
    CHARACTER   (LEN=10)                                  :: b__PrintType                             
    CHARACTER   (LEN=12)                                  :: b__RelocModel                            
    CHARACTER   (LEN=200)                                 :: b__namerf                     
    INTEGER     (ipk)                                     :: b__irefab                                
    INTEGER     (ipk)                                     :: b__nrefab1                               
    INTEGER     (ipk)                                     :: b__nrefab2                               
    INTEGER     (ipk)                                     :: b__idatingcreep                          
    INTEGER     (ipk)                                     :: b__ncreephist                            
    INTEGER     (ipk)                                     :: b__ncreepstep                            
    INTEGER     (ipk)                                     :: b__ncreeptab                             
    INTEGER     (ipk)                                     :: b__DecayModel                            
    INTEGER     (ipk)                                     :: b__lschni                                
    INTEGER     (ipk)                                     :: b__iquit                                 
    INTEGER     (ipk)                                     :: b__ir1                                   
    INTEGER     (ipk)                                     :: b__it                                    
    INTEGER     (ipk)                                     :: b__itt                                   
    INTEGER     (ipk)                                     :: b__iter                                  
    INTEGER     (ipk)                                     :: b__jmin                                  
    INTEGER     (ipk)                                     :: b__jmax                                  
    INTEGER     (ipk)                                     :: b__j                                     
    INTEGER     (ipk)                                     :: b__jjc                                   
    INTEGER     (ipk)                                     :: b__jfix                                  
    INTEGER     (ipk)                                     :: b__k                                     
    INTEGER     (ipk)                                     :: b__m                                     
    INTEGER     (ipk)                                     :: b__nab                                   
    INTEGER     (ipk)                                     :: b__nplast                                
    INTEGER     (ipk)                                     :: b__nrm1                                  
    INTEGER     (ipk)                                     :: b__icor                                  
    INTEGER     (ipk)                                     :: b__idxgas                                
    INTEGER     (ipk)                                     :: b__ivardm                                
    INTEGER     (ipk)                                     :: b__ncreep                                
    INTEGER     (ipk)                                     :: b__newprb                                
    INTEGER     (ipk)                                     :: b__nheal                                 
    INTEGER     (ipk)                                     :: b__nt                                    
    INTEGER     (ipk)                                     :: b__nvoid                                 
    INTEGER     (ipk)                                     :: b__nplot                                 
    INTEGER     (ipk)                                     :: b__coupled                               
    INTEGER     (ipk)                                     :: b__nfrttr                                
    INTEGER     (ipk)                                     :: b__nrestr                                
    INTEGER     (ipk)                                     :: b__jdlpr                                 
    INTEGER     (ipk)                                     :: b__nopt                                  
    INTEGER     (ipk)                                     :: b__nread                                 
    INTEGER     (ipk)                                     :: b__ntape                                 
    INTEGER     (ipk)                                     :: b__ounit                                 
    INTEGER     (ipk)                                     :: b__iunit                                 
    INTEGER     (ipk)                                     :: b__scrunit                               
    INTEGER     (ipk)                                     :: b__funit                                 
    INTEGER     (ipk)                                     :: b__punit                                 
    INTEGER     (ipk)                                     :: b__ntaps                                 
    INTEGER     (ipk)                                     :: b__ntapi                                 
    INTEGER     (ipk)                                     :: b__ftunit                                
    INTEGER     (ipk)                                     :: b__ddunit                                
    INTEGER     (ipk)                                     :: b__nunits                                
    INTEGER     (ipk)                                     :: b__graphna                               
    INTEGER     (ipk)                                     :: b__graphnr                               
    INTEGER     (ipk)                                     :: b__icm                                   
    INTEGER     (ipk)                                     :: b__imox                                  
    INTEGER     (ipk)                                     :: b__iplant                                
    INTEGER     (ipk)                                     :: b__zr2vintage                            
    INTEGER     (ipk)                                     :: b__iq                                    
    INTEGER     (ipk)                                     :: b__igas                                  
    INTEGER     (ipk)                                     :: b__igascal                               
    INTEGER     (ipk)                                     :: b__ifixedtsurf                           
    INTEGER     (ipk)                                     :: b__nsp                                   
    INTEGER     (ipk)                                     :: b__ifixedcoolt                           
    INTEGER     (ipk)                                     :: b__ifixedcoolp                           
    INTEGER     (ipk)                                     :: b__TimeIntegration                       
    INTEGER     (ipk)                                     :: b__moxtype                               
    INTEGER     (ipk)                                     :: b__cooltype                              
    INTEGER     (ipk)                                     :: b__geom                                  
    INTEGER     (ipk)                                     :: b__im_old                                
    INTEGER     (ipk)                                     :: b__mechan                                
    INTEGER     (ipk)                                     :: b__ngasmod                               
    REAL        (r8k)                                     :: b__gapmin                                
    REAL        (r8k)                                     :: b__gaph                                  
    REAL        (r8k)                                     :: b__gapt                                  
    REAL        (r8k)                                     :: b__airin                                 
    REAL        (r8k)                                     :: b__angi                                  
    REAL        (r8k)                                     :: b__an2in                                 
    REAL        (r8k)                                     :: b__argin                                 
    REAL        (r8k)                                     :: b__avflux                                
    REAL        (r8k)                                     :: b__bu                                    
    REAL        (r8k)                                     :: b__buold                                 
    REAL        (r8k)                                     :: b__cfv                                   
    REAL        (r8k)                                     :: b__cfva                                  
    REAL        (r8k)                                     :: b__convc                                 
    REAL        (r8k)                                     :: b__cpv                                   
    REAL        (r8k)                                     :: b__ctemp                                 
    REAL        (r8k)                                     :: b__dcobol                                
    REAL        (r8k)                                     :: b__delbp                                 
    REAL        (r8k)                                     :: b__delbu                                 
    REAL        (r8k)                                     :: b__delh                                  
    REAL        (r8k)                                     :: b__deltcr                                
    REAL        (r8k)                                     :: b__deltdb                                
    REAL        (r8k)                                     :: b__deltfc                                
    REAL        (r8k)                                     :: b__deltjl                                
    REAL        (r8k)                                     :: b__delhs                                 
    REAL        (r8k)                                     :: b__dlrel                                 
    REAL        (r8k)                                     :: b__dlrod                                 
    REAL        (r8k)                                     :: b__dphf                                  
    REAL        (r8k)                                     :: b__dphfrl                                
    REAL        (r8k)                                     :: b__dhfll                                 
    REAL        (r8k)                                     :: b__fgin                                  
    REAL        (r8k)                                     :: b__fpor1                                 
    REAL        (r8k)                                     :: b__hcrv                                  
    REAL        (r8k)                                     :: b__hcv                                   
    REAL        (r8k)                                     :: b__hdshv                                 
    REAL        (r8k)                                     :: b__hein                                  
    REAL        (r8k)                                     :: b__h2oin                                 
    REAL        (r8k)                                     :: b__h2in                                  
    REAL        (r8k)                                     :: b__hfll                                  
    REAL        (r8k)                                     :: b__hflmdb                                
    REAL        (r8k)                                     :: b__hflmp                                 
    REAL        (r8k)                                     :: b__hfper                                 
    REAL        (r8k)                                     :: b__hfv                                   
    REAL        (r8k)                                     :: b__hgapt                                 
    REAL        (r8k)                                     :: b__hgv                                   
    REAL        (r8k)                                     :: b__hgap                                  
    REAL        (r8k)                                     :: b__hsolid                                
    REAL        (r8k)                                     :: b__hgapr                                 
    REAL        (r8k)                                     :: b__hpl                                   
    REAL        (r8k)                                     :: b__hporv                                 
    REAL        (r8k)                                     :: b__hpv                                   
    REAL        (r8k)                                     :: b__hva                                   
    REAL        (r8k)                                     :: b__h2omi                                 
    REAL        (r8k)                                     :: b__kryin                                 
    REAL        (r8k)                                     :: b__nu                                    
    REAL        (r8k)                                     :: b__pecdh                                 
    REAL        (r8k)                                     :: b__rprm1                                 
    REAL        (r8k)                                     :: b__press                                 
    REAL        (r8k)                                     :: b__prty                                  
    REAL        (r8k)                                     :: b__rci                                   
    REAL        (r8k)                                     :: b__rco                                   
    REAL        (r8k)                                     :: b__rtemp                                 
    REAL        (r8k)                                     :: b__qav                                   
    REAL        (r8k)                                     :: b__qpeak                                 
    REAL        (r8k)                                     :: b__pkZrO2WtGain                          
    REAL        (r8k)                                     :: b__rfnvff                                
    REAL        (r8k)                                     :: b__rhofuel                               
    REAL        (r8k)                                     :: b__rp                                    
    REAL        (r8k)                                     :: b__rstran                                
    REAL        (r8k)                                     :: b__rtran                                 
    REAL        (r8k)                                     :: b__sumck                                 
    REAL        (r8k)                                     :: b__sumdh                                 
    REAL        (r8k)                                     :: b__sumexp                                
    REAL        (r8k)                                     :: b__sumgp                                 
    REAL        (r8k)                                     :: b__sumpor                                
    REAL        (r8k)                                     :: b__sumrg                                 
    REAL        (r8k)                                     :: b__t                                     
    REAL        (r8k)                                     :: b__thvv                                  
    REAL        (r8k)                                     :: b__tci                                   
    REAL        (r8k)                                     :: b__tco                                   
    REAL        (r8k)                                     :: b__tfgfr                                 
    REAL        (r8k)                                     :: b__thefr                                 
    REAL        (r8k)                                     :: b__th2ofr                                
    REAL        (r8k)                                     :: b__tn2fr                                 
    REAL        (r8k)                                     :: b__tpa                                   
    REAL        (r8k)                                     :: b__tplen                                 
    REAL        (r8k)                                     :: b__transt                                
    REAL        (r8k)                                     :: b__tsat                                  
    REAL        (r8k)                                     :: b__txa                                   
    REAL        (r8k)                                     :: b__txal                                  
    REAL        (r8k)                                     :: b__txc                                   
    REAL        (r8k)                                     :: b__visc                                  
    REAL        (r8k)                                     :: b__vplt                                  
    REAL        (r8k)                                     :: b__wt                                    
    REAL        (r8k)                                     :: b__xein                                  
    REAL        (r8k)                                     :: b__zro2wg                                
    REAL        (r8k)                                     :: b__cladavgtemp                           
    REAL        (r8k)                                     :: b__tpca                                  
    REAL        (r8k)                                     :: b__tntera                                
    REAL        (r8k)                                     :: b__fnck                                  
    REAL        (r8k)                                     :: b__fncn                                  
    REAL        (r8k)                                     :: b__cwkf                                  
    REAL        (r8k)                                     :: b__cwnf                                  
    REAL        (r8k)                                     :: b__coldwk                                
    REAL        (r8k)                                     :: b__frcoef                                
    REAL        (r8k)                                     :: b__afal                                  
    REAL        (r8k)                                     :: b__afdn                                  
    REAL        (r8k)                                     :: b__afgr                                  
    REAL        (r8k)                                     :: b__amfair                                
    REAL        (r8k)                                     :: b__amffg                                 
    REAL        (r8k)                                     :: b__catexf                                
    REAL        (r8k)                                     :: b__chorg                                 
    REAL        (r8k)                                     :: b__cldwks                                
    REAL        (r8k)                                     :: b__cpl                                   
    REAL        (r8k)                                     :: b__crephr                                
    REAL        (r8k)                                     :: b__crdt                                  
    REAL        (r8k)                                     :: b__crdtr                                 
    REAL        (r8k)                                     :: b__dishsd                                
    REAL        (r8k)                                     :: b__fa                                    
    REAL        (r8k)                                     :: b__fgpav                                 
    REAL        (r8k)                                     :: b__fotmtl                                
    REAL        (r8k)                                     :: b__chmfrh                                
    REAL        (r8k)                                     :: b__chmfrw                                
    REAL        (r8k)                                     :: b__hdish                                 
    REAL        (r8k)                                     :: b__hplt                                  
    REAL        (r8k)                                     :: b__pitch                                 
    REAL        (r8k)                                     :: b__ppmh2o                                
    REAL        (r8k)                                     :: b__ppmn2                                 
    REAL        (r8k)                                     :: b__rdish                                 
    REAL        (r8k)                                     :: b__roughc                                
    REAL        (r8k)                                     :: b__roughf                                
    REAL        (r8k)                                     :: b__sgapf                                 
    REAL        (r8k)                                     :: b__tcc                                   
    REAL        (r8k)                                     :: b__tref                                  
    REAL        (r8k)                                     :: b__TGasFab                               
    REAL        (r8k)                                     :: b__rsntr                                 
    REAL        (r8k)                                     :: b__tsint                                 
    REAL        (r8k)                                     :: b__grnsize                               
    REAL        (r8k)                                     :: b__b10                                   
    REAL        (r8k)                                     :: b__zrb2thick                             
    REAL        (r8k)                                     :: b__zrb2den                               
    REAL        (r8k)                                     :: b__prvden                                
    REAL        (r8k)                                     :: b__ifba                                  
    REAL        (r8k)                                     :: b__ftmelt                                
    REAL        (r8k)                                     :: b__fhefus                                
    REAL        (r8k)                                     :: b__ctmelt                                
    REAL        (r8k)                                     :: b__chefus                                
    REAL        (r8k)                                     :: b__ctranb                                
    REAL        (r8k)                                     :: b__ctrane                                
    REAL        (r8k)                                     :: b__ctranz                                
    REAL        (r8k)                                     :: b__fdelta                                
    REAL        (r8k)                                     :: b__bup                                   
    REAL        (r8k)                                     :: b__deloxy                                
    REAL        (r8k)                                     :: b__zro2i                                 
    REAL        (r8k)                                     :: b__zro2o                                 
    REAL        (r8k)                                     :: b__zoxk                                  
    REAL        (r8k)                                     :: b__excesh2                               
    REAL        (r8k)                                     :: b__frden                                 
    REAL        (r8k)                                     :: b__totl                                  
    REAL        (r8k)                                     :: b__rdot                                  
    REAL        (r8k)                                     :: b__cvv                                   
    REAL        (r8k)                                     :: b__gmlesAsFab                            
    REAL        (r8k)                                     :: b__gmlesReleased                         
    REAL        (r8k)                                     :: b__vfrcpl                                
    REAL        (r8k)                                     :: b__tplens                                
    REAL        (r8k)                                     :: b__slim                                  
    REAL        (r8k)                                     :: b__gaphtcmult                            
    REAL        (r8k)                                     :: b__RestartTime                           
    REAL        (r8k)                                     :: b__sigftc                                
    REAL        (r8k)                                     :: b__sigftex                               
    REAL        (r8k)                                     :: b__sigfgr                                
    REAL        (r8k)                                     :: b__sigswell                              
    REAL        (r8k)                                     :: b__sigcreep                              
    REAL        (r8k)                                     :: b__siggro                                
    REAL        (r8k)                                     :: b__sigcor                                
    REAL        (r8k)                                     :: b__sigh2                                 
    REAL        (r8k)                                     :: b__newtimestep                           
    REAL        (r8k)                                     :: b__stopox                                
    REAL        (r8k)                                     :: b__deng                                  
    REAL        (r8k)                                     :: b__den                                   
    REAL        (r8k)                                     :: b__amfhe                                 
    REAL        (r8k)                                     :: b__amfh2                                 
    REAL        (r8k)                                     :: b__amfn2                                 
    REAL        (r8k)                                     :: b__amfarg                                
    REAL        (r8k)                                     :: b__amfkry                                
    REAL        (r8k)                                     :: b__amfxe                                 
    REAL        (r8k)                                     :: b__amfh2o                                
    REAL        (r8k)                                     :: b__enrpu39                               
    REAL        (r8k)                                     :: b__enrpu40                               
    REAL        (r8k)                                     :: b__enrpu41                               
    REAL        (r8k)                                     :: b__enrpu42                               
    REAL        (r8k)                                     :: b__facmot                                
    REAL        (r8k)                                     :: b__DebugTime                             
    REAL        (r8k)                                     :: b__modheat                               
    REAL        (r8k)                                     :: b__cladelmod                             
    REAL        (r8k)                                     :: b__fuelreloc                             
    REAL        (r8k)                                     :: b__gaprecov                              
    REAL        (r8k)                                     :: b__relocm_true                           
    REAL        (r8k)                                     :: b__relocm_mod                            
    REAL        (r8k)                                     :: b__cplrefab                              
    REAL        (r8k)                                     :: b__vsrefab                               
    REAL        (r8k)                                     :: b__dspgrefab                             
    REAL        (r8k)                                     :: b__dspgwrefab                            
    REAL        (r8k)                                     :: b__fgpavrefab                            
    REAL        (r8k)                                     :: b__airrefab                              
    REAL        (r8k)                                     :: b__n2refab                               
    REAL        (r8k)                                     :: b__arrefab                               
    REAL        (r8k)                                     :: b__fgrefab                               
    REAL        (r8k)                                     :: b__herefab                               
    REAL        (r8k)                                     :: b__krrefab                               
    REAL        (r8k)                                     :: b__xerefab                               
    REAL        (r8k)                                     :: b__creeppooltime                         
    REAL        (r8k)                                     :: b__creeptime                             
    REAL        (r8k)                                     :: b__datingtstep                           
    REAL        (r8k)                                     :: b__pfave                                 
    REAL        (r8k)                                     :: b__por000                                
    REAL        (r8k)                                     :: b__ntot                                  
    REAL        (r8k)                                     :: b__fpdcay                                
    REAL        (r8k)                                     :: b__vs                                    
    REAL        (r8k)                                     :: b__dspg                                  
    REAL        (r8k)                                     :: b__dspgw                                 
    REAL        (r8k)                                     :: b__vcold                                 
    INTEGER     (ipk)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__openp                                 
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__IgapGapIndex                          
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__IgapIndexOld                          
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__IgapIndexPrevOld                      
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__jn                                    
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__jnsurftemp                            
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__jpeak                                 
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__jst                                   
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__jstsurftemp                           
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: b__CycleSteps                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gases                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__totinner                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__totcrl                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FastFluxd                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FastFluenced                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__BulkCoolantTemp                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rhof                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__dpwxrate                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__axialnode                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PelletRad                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladAveTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PelAveTemp                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__GapAveTemp                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PelSurfTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PelCentTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__sigeff                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelSurfDispl                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladInSurDisp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladInPermDef                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__sigy                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__AxialNodLength                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladEffPlasStrain                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__RinterfacPress                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelCladGap                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__GapPress                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CoolantPress                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PlastStrnep1                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldCladStrn                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldFuelStrn                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldGapPress                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldCoolPress                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldFuelDispl                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldCladAvTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CreepStrain                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CreepStrain1                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladDiamHot                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevCladEffPlasStrn                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevFuelStrain                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__HotNodLength                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PermFuelDispl                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__BOSNodeburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__EOSNodeburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__StepNodeburnup                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__TotalHgap                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__SolidHgap                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__GasHgap                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__RadHgap                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FastFlux                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FastFluence                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelPorosity                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FilmCoefficient                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__EstGapDeltaT                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__BOSZrO2Thk                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__EOSZrO2Thk                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ZrO2ThkNoAd                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelCondFactor                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__Relocation                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__RinternalVolume                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladVolume                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CrackVolume                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__RinterfacVolume                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelVolume                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fden                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__GapVolume                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PorosityVolume                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__SurfTempOxide                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__AnnulusVolume                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gapplot                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevOldCoolPress                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevOldGapPress                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevOldCladAvTemp                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevOldFuelDispl                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevCreepStrain                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladIrradGrowStrn                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__UniformAxNodStrn                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladH2Concen                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ExcessH2Concen                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__PrevCladStrain                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__StartofStepH2Con                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__StartofStepPickupH2Con                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__EndofStepPickupH2Con                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FuelTempRestruRad                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__OldHealedCrackRadius                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__StoredEnergy                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__HealedCrackRadius                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__HotThermalGap                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__WorkArray1                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__WorkArray2                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__WorkArray3                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladInSurfTemp                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__CladOutSurfTemp                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__Power                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gpthe                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gpth                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gpthpg                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__qc                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__totdef                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__buin                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__dci                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__dco                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__thkcld                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__cdg                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__thkgap                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__de                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__deltaz                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__comp                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ctmax                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__dp                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__flux                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__qend                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__crudmult                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__crdtt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rdotwrt                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__FDItave                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__creapratearray                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__GapCond                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fuelexptot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fuelswltot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fuelcreeptot                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fueldentot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fuelburntot                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__cladcrptot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gapHTC                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__oxidelayer                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gapmech                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gapthrm                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__fuelrelmod                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ifbarel                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__NodalMoles                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__NodalGMLES                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rlcstrn                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rlcstrnold                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rc                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gadoln                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__colddef                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__colddec                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__qnode                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__heprod                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__he                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__heold                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__boron10                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enrch                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__epsav                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__wimsburnup                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__oldwimsburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__stold                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__stnew                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__deltimeold                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__deltimenew                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__sagold                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__sagnew                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__delsagold                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__delsagnew                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__delst                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__angr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__ah2ogr                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__fgmgp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__fmgr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__hemgp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__hmgr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__ang                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__ah2og                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__epp                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__eppp                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__eppsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__eps                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__feps                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__reps                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__repsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__repsp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rfeps                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rfepp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rfpsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__sig                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__epp1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__sig1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__eps1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__ThermalStrain                         
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__RB_axial                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__RB_rod                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__avgqi                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__go                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__p2                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ProblemTime                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ProblemTime_Prev                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__qmpy                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tw                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__p1                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__acmfg                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__acmhe                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__acmH2                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__acmH2O                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__acmn2                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__amgpt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__gasmo                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__hmgpt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tafa                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__taga                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tsfa                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__taca                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkPelAveTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkPower                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkAveCladTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkGap                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkFisGasRelFrac                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkPelSurfTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkPelCentTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkH2up                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkIntefacePres                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkIDCladTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkODCladTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pit                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkHoopStres                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkAxlStres                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkBurnup                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkHoopStrain                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkFuelPelOD                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkGapCond                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pkZrO2                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__zcool                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tcoolant                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__pcoolant                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__x                                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__qf                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__xt                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__cladt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__addgmles                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__addswell                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__HeatFlux                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__dt                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__buavearray                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__voidvolarray                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__he_ifba                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tfuelr                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tfuelr2                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__tfring                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__rrapow                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__coolanttemp                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__coolantpressure                       
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__cladtarray                            
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__storedearray                          
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__cltemparray                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__buarray                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__strainarray                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__straindiffarray                       
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dpwxarray                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__creaparray                            
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dumarray3                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__formf                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__ringvol                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__coldringl                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__porosold                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__porosnew                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rrev                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__qaxnorm                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__crad                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__hrad                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rapow                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__uo2exp                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dpw                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dpwpp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dpw2                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__dpwpp2                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__densf                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__densp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__hotringl                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__tmpfuel                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__gasavail1                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__gasavail2                             
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__prdct                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__brnup3                                
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__ansd                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__creeptabtime                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__creeptabtemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__creeptabstress                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conU235                               
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conU238                               
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conPu239                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conPu240                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conPu241                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__conPu242                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coU235av                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coU238av                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coPu239av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coPu240av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coPu241av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__coPu242av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriU235                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriU238                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriPu239                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriPu240                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriPu241                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__enriPu242                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfU235                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfU238                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfPu239                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfPu240                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfPu241                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pfPu242                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__pf                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__decay                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__half                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__brnup1                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__ansr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__gp                                    
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__gpold                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rls                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__rlsold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__brn                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__brnrefab                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__EOSNodeburnuprefab                    
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__brnold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__grnold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__frdenold                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__gbtot                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: b__tempkold                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__gb                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__grs                                   
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__gg                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__gbold                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__ggold                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__grsold                                
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g1                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g2                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g3                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g4                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g1old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g2old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g3old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: b__g4old                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: b__axlinpower                            