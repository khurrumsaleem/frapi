    LOGICAL                                               :: r__calcoxide                             
    LOGICAL                                               :: r__updated_restart                       
    LOGICAL                                               :: r__gasflg                                
    LOGICAL                                               :: r__nconvg                                
    LOGICAL                                               :: r__ncont                                 
    LOGICAL                                               :: r__IsModelingSpentFuel    
    LOGICAL                                               :: r__hgapt_flag     ! YU JIANKAI 
    LOGICAL                                               :: r__flag_iapws     ! YU JIANKAI	
    Character   (LEN=80)                                  :: r__title                                 
    CHARACTER   (LEN=10)                                  :: r__PrintType                             
    CHARACTER   (LEN=12)                                  :: r__RelocModel                            
    INTEGER     (ipk)                                     :: r__irefab                                
    INTEGER     (ipk)                                     :: r__nrefab1                               
    INTEGER     (ipk)                                     :: r__nrefab2                               
    INTEGER     (ipk)                                     :: r__idatingcreep                          
    INTEGER     (ipk)                                     :: r__ncreephist                            
    INTEGER     (ipk)                                     :: r__ncreepstep                            
    INTEGER     (ipk)                                     :: r__ncreeptab                             
    INTEGER     (ipk)                                     :: r__DecayModel                            
    INTEGER     (ipk)                                     :: r__lschni                                
    INTEGER     (ipk)                                     :: r__iquit                                 
    INTEGER     (ipk)                                     :: r__ir1                                   
    INTEGER     (ipk)                                     :: r__it                                    
    INTEGER     (ipk)                                     :: r__itt                                   
    INTEGER     (ipk)                                     :: r__iter                                  
    INTEGER     (ipk)                                     :: r__jmin                                  
    INTEGER     (ipk)                                     :: r__jmax                                  
    INTEGER     (ipk)                                     :: r__j                                     
    INTEGER     (ipk)                                     :: r__jjc                                   
    INTEGER     (ipk)                                     :: r__jfix                                  
    INTEGER     (ipk)                                     :: r__k                                     
    INTEGER     (ipk)                                     :: r__m                                     
    INTEGER     (ipk)                                     :: r__nab                                   
    INTEGER     (ipk)                                     :: r__nplast                                
    INTEGER     (ipk)                                     :: r__nrm1                                  
    INTEGER     (ipk)                                     :: r__icor                                  
    INTEGER     (ipk)                                     :: r__idxgas                                
    INTEGER     (ipk)                                     :: r__ivardm                                
    INTEGER     (ipk)                                     :: r__ncreep                                
    INTEGER     (ipk)                                     :: r__newprb                                
    INTEGER     (ipk)                                     :: r__nheal                                 
    INTEGER     (ipk)                                     :: r__nt                                    
    INTEGER     (ipk)                                     :: r__nvoid                                 
    INTEGER     (ipk)                                     :: r__nplot                                 
    INTEGER     (ipk)                                     :: r__coupled                               
    INTEGER     (ipk)                                     :: r__nfrttr                                
    INTEGER     (ipk)                                     :: r__nrestr                                
    INTEGER     (ipk)                                     :: r__jdlpr                                 
    INTEGER     (ipk)                                     :: r__nopt                                  
    INTEGER     (ipk)                                     :: r__nread                                 
    INTEGER     (ipk)                                     :: r__ntape                                 
    INTEGER     (ipk)                                     :: r__ounit                                 
    INTEGER     (ipk)                                     :: r__iunit                                 
    INTEGER     (ipk)                                     :: r__scrunit                               
    INTEGER     (ipk)                                     :: r__funit                                 
    INTEGER     (ipk)                                     :: r__punit                                 
    INTEGER     (ipk)                                     :: r__ntaps                                 
    INTEGER     (ipk)                                     :: r__ntapi                                 
    INTEGER     (ipk)                                     :: r__ftunit                                
    INTEGER     (ipk)                                     :: r__ddunit                                
    INTEGER     (ipk)                                     :: r__nunits                                
    INTEGER     (ipk)                                     :: r__graphna                               
    INTEGER     (ipk)                                     :: r__graphnr                               
    INTEGER     (ipk)                                     :: r__icm                                   
    INTEGER     (ipk)                                     :: r__imox                                  
    INTEGER     (ipk)                                     :: r__iplant                                
    INTEGER     (ipk)                                     :: r__zr2vintage                            
    INTEGER     (ipk)                                     :: r__iq                                    
    INTEGER     (ipk)                                     :: r__igas                                  
    INTEGER     (ipk)                                     :: r__igascal                               
    INTEGER     (ipk)                                     :: r__ifixedtsurf                           
    INTEGER     (ipk)                                     :: r__nsp                                   
    INTEGER     (ipk)                                     :: r__ifixedcoolt                           
    INTEGER     (ipk)                                     :: r__ifixedcoolp                           
    INTEGER     (ipk)                                     :: r__TimeIntegration                       
    INTEGER     (ipk)                                     :: r__moxtype                               
    INTEGER     (ipk)                                     :: r__cooltype                              
    INTEGER     (ipk)                                     :: r__geom                                  
    INTEGER     (ipk)                                     :: r__im_old                                
    INTEGER     (ipk)                                     :: r__mechan                                
    INTEGER     (ipk)                                     :: r__ngasmod                               
    REAL        (r8k)                                     :: r__gapmin                                
    REAL        (r8k)                                     :: r__gaph                                  
    REAL        (r8k)                                     :: r__gapt                                  
    REAL        (r8k)                                     :: r__airin                                 
    REAL        (r8k)                                     :: r__angi                                  
    REAL        (r8k)                                     :: r__an2in                                 
    REAL        (r8k)                                     :: r__argin                                 
    REAL        (r8k)                                     :: r__avflux                                
    REAL        (r8k)                                     :: r__bu                                    
    REAL        (r8k)                                     :: r__buold                                 
    REAL        (r8k)                                     :: r__cfv                                   
    REAL        (r8k)                                     :: r__cfva                                  
    REAL        (r8k)                                     :: r__convc                                 
    REAL        (r8k)                                     :: r__cpv                                   
    REAL        (r8k)                                     :: r__ctemp                                 
    REAL        (r8k)                                     :: r__dcobol                                
    REAL        (r8k)                                     :: r__delbp                                 
    REAL        (r8k)                                     :: r__delbu                                 
    REAL        (r8k)                                     :: r__delh                                  
    REAL        (r8k)                                     :: r__deltcr                                
    REAL        (r8k)                                     :: r__deltdb                                
    REAL        (r8k)                                     :: r__deltfc                                
    REAL        (r8k)                                     :: r__deltjl                                
    REAL        (r8k)                                     :: r__delhs                                 
    REAL        (r8k)                                     :: r__dlrel                                 
    REAL        (r8k)                                     :: r__dlrod                                 
    REAL        (r8k)                                     :: r__dphf                                  
    REAL        (r8k)                                     :: r__dphfrl                                
    REAL        (r8k)                                     :: r__dhfll                                 
    REAL        (r8k)                                     :: r__fgin                                  
    REAL        (r8k)                                     :: r__fpor1                                 
    REAL        (r8k)                                     :: r__hcrv                                  
    REAL        (r8k)                                     :: r__hcv                                   
    REAL        (r8k)                                     :: r__hdshv                                 
    REAL        (r8k)                                     :: r__hein                                  
    REAL        (r8k)                                     :: r__h2oin                                 
    REAL        (r8k)                                     :: r__h2in                                  
    REAL        (r8k)                                     :: r__hfll                                  
    REAL        (r8k)                                     :: r__hflmdb                                
    REAL        (r8k)                                     :: r__hflmp                                 
    REAL        (r8k)                                     :: r__hfper                                 
    REAL        (r8k)                                     :: r__hfv                                   
    REAL        (r8k)                                     :: r__hgapt                                 
    REAL        (r8k)                                     :: r__hgv                                   
    REAL        (r8k)                                     :: r__hgap                                  
    REAL        (r8k)                                     :: r__hsolid                                
    REAL        (r8k)                                     :: r__hgapr                                 
    REAL        (r8k)                                     :: r__hpl                                   
    REAL        (r8k)                                     :: r__hporv                                 
    REAL        (r8k)                                     :: r__hpv                                   
    REAL        (r8k)                                     :: r__hva                                   
    REAL        (r8k)                                     :: r__h2omi                                 
    REAL        (r8k)                                     :: r__kryin                                 
    REAL        (r8k)                                     :: r__nu                                    
    REAL        (r8k)                                     :: r__pecdh                                 
    REAL        (r8k)                                     :: r__rprm1                                 
    REAL        (r8k)                                     :: r__press                                 
    REAL        (r8k)                                     :: r__prty                                  
    REAL        (r8k)                                     :: r__rci                                   
    REAL        (r8k)                                     :: r__rco                                   
    REAL        (r8k)                                     :: r__rtemp                                 
    REAL        (r8k)                                     :: r__qav                                   
    REAL        (r8k)                                     :: r__qpeak                                 
    REAL        (r8k)                                     :: r__pkZrO2WtGain                          
    REAL        (r8k)                                     :: r__rfnvff                                
    REAL        (r8k)                                     :: r__rhofuel                               
    REAL        (r8k)                                     :: r__rp                                    
    REAL        (r8k)                                     :: r__rstran                                
    REAL        (r8k)                                     :: r__rtran                                 
    REAL        (r8k)                                     :: r__sumck                                 
    REAL        (r8k)                                     :: r__sumdh                                 
    REAL        (r8k)                                     :: r__sumexp                                
    REAL        (r8k)                                     :: r__sumgp                                 
    REAL        (r8k)                                     :: r__sumpor                                
    REAL        (r8k)                                     :: r__sumrg                                 
    REAL        (r8k)                                     :: r__t                                     
    REAL        (r8k)                                     :: r__thvv                                  
    REAL        (r8k)                                     :: r__tci                                   
    REAL        (r8k)                                     :: r__tco                                   
    REAL        (r8k)                                     :: r__tfgfr                                 
    REAL        (r8k)                                     :: r__thefr                                 
    REAL        (r8k)                                     :: r__th2ofr                                
    REAL        (r8k)                                     :: r__tn2fr                                 
    REAL        (r8k)                                     :: r__tpa                                   
    REAL        (r8k)                                     :: r__tplen                                 
    REAL        (r8k)                                     :: r__transt                                
    REAL        (r8k)                                     :: r__tsat                                  
    REAL        (r8k)                                     :: r__txa                                   
    REAL        (r8k)                                     :: r__txal                                  
    REAL        (r8k)                                     :: r__txc                                   
    REAL        (r8k)                                     :: r__visc                                  
    REAL        (r8k)                                     :: r__vplt                                  
    REAL        (r8k)                                     :: r__wt                                    
    REAL        (r8k)                                     :: r__xein                                  
    REAL        (r8k)                                     :: r__zro2wg                                
    REAL        (r8k)                                     :: r__cladavgtemp                           
    REAL        (r8k)                                     :: r__tpca                                  
    REAL        (r8k)                                     :: r__tntera                                
    REAL        (r8k)                                     :: r__fnck                                  
    REAL        (r8k)                                     :: r__fncn                                  
    REAL        (r8k)                                     :: r__cwkf                                  
    REAL        (r8k)                                     :: r__cwnf                                  
    REAL        (r8k)                                     :: r__coldwk                                
    REAL        (r8k)                                     :: r__frcoef                                
    REAL        (r8k)                                     :: r__afal                                  
    REAL        (r8k)                                     :: r__afdn                                  
    REAL        (r8k)                                     :: r__afgr                                  
    REAL        (r8k)                                     :: r__amfair                                
    REAL        (r8k)                                     :: r__amffg                                 
    REAL        (r8k)                                     :: r__catexf                                
    REAL        (r8k)                                     :: r__chorg                                 
    REAL        (r8k)                                     :: r__cldwks                                
    REAL        (r8k)                                     :: r__cpl                                   
    REAL        (r8k)                                     :: r__crephr                                
    REAL        (r8k)                                     :: r__crdt                                  
    REAL        (r8k)                                     :: r__crdtr                                 
    REAL        (r8k)                                     :: r__dishsd                                
    REAL        (r8k)                                     :: r__fa                                    
    REAL        (r8k)                                     :: r__fgpav                                 
    REAL        (r8k)                                     :: r__fotmtl                                
    REAL        (r8k)                                     :: r__chmfrh                                
    REAL        (r8k)                                     :: r__chmfrw                                
    REAL        (r8k)                                     :: r__hdish                                 
    REAL        (r8k)                                     :: r__hplt                                  
    REAL        (r8k)                                     :: r__pitch                                 
    REAL        (r8k)                                     :: r__ppmh2o                                
    REAL        (r8k)                                     :: r__ppmn2                                 
    REAL        (r8k)                                     :: r__rdish                                 
    REAL        (r8k)                                     :: r__roughc                                
    REAL        (r8k)                                     :: r__roughf                                
    REAL        (r8k)                                     :: r__sgapf                                 
    REAL        (r8k)                                     :: r__tcc                                   
    REAL        (r8k)                                     :: r__tref                                  
    REAL        (r8k)                                     :: r__TGasFab                               
    REAL        (r8k)                                     :: r__rsntr                                 
    REAL        (r8k)                                     :: r__tsint                                 
    REAL        (r8k)                                     :: r__grnsize                               
    REAL        (r8k)                                     :: r__b10                                   
    REAL        (r8k)                                     :: r__zrb2thick                             
    REAL        (r8k)                                     :: r__zrb2den                               
    REAL        (r8k)                                     :: r__prvden                                
    REAL        (r8k)                                     :: r__ifba                                  
    REAL        (r8k)                                     :: r__ftmelt                                
    REAL        (r8k)                                     :: r__fhefus                                
    REAL        (r8k)                                     :: r__ctmelt                                
    REAL        (r8k)                                     :: r__chefus                                
    REAL        (r8k)                                     :: r__ctranb                                
    REAL        (r8k)                                     :: r__ctrane                                
    REAL        (r8k)                                     :: r__ctranz                                
    REAL        (r8k)                                     :: r__fdelta                                
    REAL        (r8k)                                     :: r__bup                                   
    REAL        (r8k)                                     :: r__deloxy                                
    REAL        (r8k)                                     :: r__zro2i                                 
    REAL        (r8k)                                     :: r__zro2o                                 
    REAL        (r8k)                                     :: r__zoxk                                  
    REAL        (r8k)                                     :: r__excesh2                               
    REAL        (r8k)                                     :: r__frden                                 
    REAL        (r8k)                                     :: r__totl                                  
    REAL        (r8k)                                     :: r__rdot                                  
    REAL        (r8k)                                     :: r__cvv                                   
    REAL        (r8k)                                     :: r__gmlesAsFab                            
    REAL        (r8k)                                     :: r__gmlesReleased                         
    REAL        (r8k)                                     :: r__vfrcpl                                
    REAL        (r8k)                                     :: r__tplens                                
    REAL        (r8k)                                     :: r__slim                                  
    REAL        (r8k)                                     :: r__gaphtcmult                            
    REAL        (r8k)                                     :: r__RestartTime                           
    REAL        (r8k)                                     :: r__sigftc                                
    REAL        (r8k)                                     :: r__sigftex                               
    REAL        (r8k)                                     :: r__sigfgr                                
    REAL        (r8k)                                     :: r__sigswell                              
    REAL        (r8k)                                     :: r__sigcreep                              
    REAL        (r8k)                                     :: r__siggro                                
    REAL        (r8k)                                     :: r__sigcor                                
    REAL        (r8k)                                     :: r__sigh2                                 
    REAL        (r8k)                                     :: r__newtimestep                           
    REAL        (r8k)                                     :: r__stopox                                
    REAL        (r8k)                                     :: r__deng                                  
    REAL        (r8k)                                     :: r__den                                   
    REAL        (r8k)                                     :: r__amfhe                                 
    REAL        (r8k)                                     :: r__amfh2                                 
    REAL        (r8k)                                     :: r__amfn2                                 
    REAL        (r8k)                                     :: r__amfarg                                
    REAL        (r8k)                                     :: r__amfkry                                
    REAL        (r8k)                                     :: r__amfxe                                 
    REAL        (r8k)                                     :: r__amfh2o                                
    REAL        (r8k)                                     :: r__enrpu39                               
    REAL        (r8k)                                     :: r__enrpu40                               
    REAL        (r8k)                                     :: r__enrpu41                               
    REAL        (r8k)                                     :: r__enrpu42                               
    REAL        (r8k)                                     :: r__facmot                                
    REAL        (r8k)                                     :: r__DebugTime                             
    REAL        (r8k)                                     :: r__modheat                               
    REAL        (r8k)                                     :: r__cladelmod                             
    REAL        (r8k)                                     :: r__fuelreloc                             
    REAL        (r8k)                                     :: r__gaprecov                              
    REAL        (r8k)                                     :: r__relocm_true                           
    REAL        (r8k)                                     :: r__relocm_mod                            
    REAL        (r8k)                                     :: r__cplrefab                              
    REAL        (r8k)                                     :: r__vsrefab                               
    REAL        (r8k)                                     :: r__dspgrefab                             
    REAL        (r8k)                                     :: r__dspgwrefab                            
    REAL        (r8k)                                     :: r__fgpavrefab                            
    REAL        (r8k)                                     :: r__airrefab                              
    REAL        (r8k)                                     :: r__n2refab                               
    REAL        (r8k)                                     :: r__arrefab                               
    REAL        (r8k)                                     :: r__fgrefab                               
    REAL        (r8k)                                     :: r__herefab                               
    REAL        (r8k)                                     :: r__krrefab                               
    REAL        (r8k)                                     :: r__xerefab                               
    REAL        (r8k)                                     :: r__creeppooltime                         
    REAL        (r8k)                                     :: r__creeptime                             
    REAL        (r8k)                                     :: r__datingtstep                           
    REAL        (r8k)                                     :: r__pfave                                 
    REAL        (r8k)                                     :: r__por000                                
    REAL        (r8k)                                     :: r__ntot                                  
    REAL        (r8k)                                     :: r__fpdcay                                
    REAL        (r8k)                                     :: r__vs                                    
    REAL        (r8k)                                     :: r__dspg                                  
    REAL        (r8k)                                     :: r__dspgw                                 
    REAL        (r8k)                                     :: r__vcold                                 
    INTEGER     (ipk)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__openp                                 
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__IgapGapIndex                          
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__IgapIndexOld                          
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__IgapIndexPrevOld                      
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__jn                                    
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__jnsurftemp                            
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__jpeak                                 
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__jst                                   
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__jstsurftemp                           
    INTEGER     (ipk)    ,  DIMENSION(:),     ALLOCATABLE :: r__CycleSteps                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gases                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__totinner                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__totcrl                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FastFluxd                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FastFluenced                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__BulkCoolantTemp                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rhof                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__dpwxrate                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__axialnode                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PelletRad                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladAveTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PelAveTemp                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__GapAveTemp                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PelSurfTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PelCentTemp                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__sigeff                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelSurfDispl                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladInSurDisp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladInPermDef                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__sigy                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__AxialNodLength                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladEffPlasStrain                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__RinterfacPress                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelCladGap                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__GapPress                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CoolantPress                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PlastStrnep1                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldCladStrn                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldFuelStrn                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldGapPress                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldCoolPress                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldFuelDispl                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldCladAvTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CreepStrain                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CreepStrain1                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladDiamHot                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevCladEffPlasStrn                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevFuelStrain                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__HotNodLength                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PermFuelDispl                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__BOSNodeburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__EOSNodeburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__StepNodeburnup                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__TotalHgap                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__SolidHgap                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__GasHgap                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__RadHgap                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FastFlux                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FastFluence                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelPorosity                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FilmCoefficient                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__EstGapDeltaT                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__BOSZrO2Thk                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__EOSZrO2Thk                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ZrO2ThkNoAd                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelCondFactor                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__Relocation                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__RinternalVolume                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladVolume                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CrackVolume                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__RinterfacVolume                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelVolume                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fden                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__GapVolume                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PorosityVolume                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__SurfTempOxide                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__AnnulusVolume                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gapplot                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevOldCoolPress                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevOldGapPress                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevOldCladAvTemp                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevOldFuelDispl                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevCreepStrain                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladIrradGrowStrn                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__UniformAxNodStrn                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladH2Concen                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ExcessH2Concen                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__PrevCladStrain                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__StartofStepH2Con                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__StartofStepPickupH2Con                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__EndofStepPickupH2Con                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FuelTempRestruRad                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__OldHealedCrackRadius                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__StoredEnergy                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__HealedCrackRadius                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__HotThermalGap                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__WorkArray1                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__WorkArray2                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__WorkArray3                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladInSurfTemp                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__CladOutSurfTemp                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__Power                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gpthe                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gpth                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gpthpg                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__qc                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__totdef                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__buin                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__dci                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__dco                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__thkcld                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__cdg                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__thkgap                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__de                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__deltaz                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__comp                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ctmax                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__dp                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__flux                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__qend                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__crudmult                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__crdtt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rdotwrt                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__FDItave                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__creapratearray                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__GapCond                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fuelexptot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fuelswltot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fuelcreeptot                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fueldentot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fuelburntot                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__cladcrptot                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gapHTC                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__oxidelayer                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gapmech                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gapthrm                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__fuelrelmod                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ifbarel                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__NodalMoles                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__NodalGMLES                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rlcstrn                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rlcstrnold                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rc                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gadoln                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__colddef                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__colddec                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__qnode                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__heprod                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__he                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__heold                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__boron10                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enrch                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__epsav                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__wimsburnup                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__oldwimsburnup                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__stold                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__stnew                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__deltimeold                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__deltimenew                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__sagold                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__sagnew                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__delsagold                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__delsagnew                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__delst                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__angr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__ah2ogr                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__fgmgp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__fmgr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__hemgp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__hmgr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__ang                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__ah2og                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__epp                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__eppp                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__eppsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__eps                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__feps                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__reps                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__repsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__repsp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rfeps                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rfepp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rfpsv                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__sig                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__epp1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__sig1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__eps1                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__ThermalStrain                         
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__RB_axial                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__RB_rod                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__avgqi                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__go                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__p2                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ProblemTime                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ProblemTime_Prev                      
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__qmpy                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tw                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__p1                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__acmfg                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__acmhe                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__acmH2                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__acmH2O                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__acmn2                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__amgpt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__gasmo                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__hmgpt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tafa                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__taga                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tsfa                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__taca                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkPelAveTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkPower                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkAveCladTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkGap                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkFisGasRelFrac                       
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkPelSurfTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkPelCentTemp                         
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkH2up                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkIntefacePres                        
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkIDCladTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkODCladTemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pit                                   
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkHoopStres                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkAxlStres                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkBurnup                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkHoopStrain                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkFuelPelOD                           
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkGapCond                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pkZrO2                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__zcool                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tcoolant                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__pcoolant                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__x                                     
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__qf                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__xt                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__cladt                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__addgmles                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__addswell                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__HeatFlux                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__dt                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__buavearray                            
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__voidvolarray                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__he_ifba                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tfuelr                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tfuelr2                               
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__tfring                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__rrapow                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__coolanttemp                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__coolantpressure                       
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__cladtarray                            
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__storedearray                          
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__cltemparray                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__buarray                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__strainarray                           
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__straindiffarray                       
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dpwxarray                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__creaparray                            
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dumarray3                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__formf                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__ringvol                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__coldringl                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__porosold                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__porosnew                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rrev                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__qaxnorm                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__crad                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__hrad                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rapow                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__uo2exp                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dpw                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dpwpp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dpw2                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__dpwpp2                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__densf                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__densp                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__hotringl                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__tmpfuel                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__gasavail1                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__gasavail2                             
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__prdct                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__brnup3                                
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__ansd                                  
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__creeptabtime                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__creeptabtemp                          
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__creeptabstress                        
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conU235                               
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conU238                               
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conPu239                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conPu240                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conPu241                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__conPu242                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coU235av                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coU238av                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coPu239av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coPu240av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coPu241av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__coPu242av                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriU235                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriU238                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriPu239                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriPu240                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriPu241                             
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__enriPu242                             
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfU235                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfU238                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfPu239                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfPu240                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfPu241                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pfPu242                               
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__pf                                    
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__decay                                 
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__half                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__brnup1                                
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__ansr                                  
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__gp                                    
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__gpold                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rls                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__rlsold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__brn                                   
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__brnrefab                              
    REAL        (r8k)    ,  DIMENSION(:),     ALLOCATABLE :: r__EOSNodeburnuprefab                    
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__brnold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__grnold                                
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__frdenold                              
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__gbtot                                 
    REAL        (r8k)    ,  DIMENSION(:,:),   ALLOCATABLE :: r__tempkold                              
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__gb                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__grs                                   
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__gg                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__gbold                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__ggold                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__grsold                                
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g1                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g2                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g3                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g4                                    
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g1old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g2old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g3old                                 
    REAL        (r8k)    ,  DIMENSION(:,:,:), ALLOCATABLE :: r__g4old                                 

