module fuelrod

    use conversions
    use frapcon41, only : driver_type

    implicit none


    type, public :: frod_type
        type(driver_type) :: driver
    contains
        procedure :: make      => frod_make         ! Initialize the fuel rod
        procedure :: init      => frod_init         ! Set the initial fuel rod state, t = 0
        procedure :: next      => frod_next         ! Perform the trial time step, dt > 0
        procedure :: accept    => frod_accept       ! Reject the last time step
        procedure :: set_value => frod_set_value    ! Set variable value
        procedure :: set_array => frod_set_array    ! Set variable array
        procedure :: get_value => frod_get_value    ! Catch variable value
        procedure :: get_array => frod_get_array    ! Catch variable array
        procedure :: save      => frod_save         ! Save fuel rod state in a file
        procedure :: load      => frod_load         ! Load fuel rod state from a file
        procedure :: destroy   => frod_destroy      ! Deallocate the fuel rod variables
    end type frod_type

    ! TEMPORARY VARIABLES
    integer :: i, j, n, m
    real(8) :: a, b, c, volume
    real(8), allocatable :: weight(:)
    real(8), allocatable :: tmp0(:), tmp1(:), tmp2(:), tmp3(:)

contains

    subroutine frod_make(this, nr, na, ngasr, nce, &
                  mechan, ngasmod, icm, icor, iplant, &
                  imox, igascal, zr2vintage, moxtype, idxgas, iq, ivardm, &
                  ifixedcoolt, ifixedcoolp, ifixedtsurf, verbose)

        class (frod_type), intent(inout) :: this

        integer, optional :: nr          ! number of radial segments
        integer, optional :: na          ! number of axial segments
        integer, optional :: ngasr       ! number of radial gas release nodes
        integer, optional :: nce         ! number of radial elements in the cladding for the FEA model
        integer, optional :: mechan      ! Cladding mechanical model (1 = FEA, 2 = FRACAS-I)
        integer, optional :: ngasmod     ! Fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)
        integer, optional :: icm         ! cladding type 4: Zircaloy-4
        integer, optional :: icor        ! crud model (0 or 1 = constant layer, 2= time dependent)
        integer, optional :: iplant      ! plant type Plant type, -2: PWR, -3: BWR, -4: HBWR
        integer, optional :: imox        ! fuel type (Fuel type, 0: UO_2)
        integer, optional :: igascal     ! Internal pressure calculation for FEA model igascal=1 normal pressure calculation igascal=0 use prescribed pressure set by p1
        integer, optional :: zr2vintage  ! zircaloy-2 vintage
        integer, optional :: moxtype     ! flag for type of Pu used in MOX
        integer, optional :: idxgas      ! fill gas type
        integer, optional :: iq          ! Axial power shape indicator (0 = user-input, 1 = chopped cosine)
        integer, optional :: ivardm      ! Option to specify variable axial node length (1 is on, 0 is off (default))
        integer, optional :: ifixedcoolt ! Specify whether to use user-supplied coolant temperatures at each axial node (0 = No (Default), 1 = User-supplied)
        integer, optional :: ifixedcoolp ! Specify whether to use user-supplied coolant pressures at each axial node (0 = No (Default), 1 = User-supplied)
        integer, optional :: ifixedtsurf ! Specify to use fixed cladding surface temperatures
        logical, optional :: verbose     ! Print the output data in terminal

        integer :: nr_ = 17
        integer :: na_ = 10
        integer :: ngasr_ = 45
        integer :: nce_ = 5
        logical :: verbose_ = .false. 

        n  = na
        m  = nr

        if( present(nr     ) ) nr_      = nr     
        if( present(na     ) ) na_      = na     
        if( present(ngasr  ) ) ngasr_   = ngasr  
        if( present(nce    ) ) nce_     = nce    
        if( present(verbose) ) verbose_ = verbose

        call this % driver % make(na_, ngasr_, nr_+1, nce_, verbose_)

        call this % driver % deft()

        if( present(mechan     ) ) this % driver % mechan      = mechan     
        if( present(ngasmod    ) ) this % driver % ngasmod     = ngasmod    
        if( present(icm        ) ) this % driver % icm         = icm        
        if( present(icor       ) ) this % driver % icor        = icor       
        if( present(iplant     ) ) this % driver % iplant      = iplant     
        if( present(imox       ) ) this % driver % imox        = imox        
        if( present(igascal    ) ) this % driver % igascal     = igascal    
        if( present(zr2vintage ) ) this % driver % zr2vintage  = zr2vintage 
        if( present(moxtype    ) ) this % driver % moxtype     = moxtype    
        if( present(idxgas     ) ) this % driver % idxgas      = idxgas     
        if( present(iq         ) ) this % driver % iq          = iq
        if( present(ivardm     ) ) this % driver % ivardm      = ivardm
        if( present(ifixedcoolt) ) this % driver % ifixedcoolt = ifixedcoolt
        if( present(ifixedcoolp) ) this % driver % ifixedcoolp = ifixedcoolp
        if( present(ifixedtsurf) ) this % driver % ifixedtsurf = ifixedtsurf

        call this % driver % dump()

        ! ALLOCATION OF THE TEMPORARY ARRAYS
        if(.not. allocated(weight)) allocate(weight(m))
        if(.not. allocated(tmp0))   allocate(tmp0(m))
        if(.not. allocated(tmp1))   allocate(tmp1(n))
        if(.not. allocated(tmp2))   allocate(tmp2(m+1))
        if(.not. allocated(tmp3))   allocate(tmp3(n+1))

!        real(8), optional :: thkcld      ! Cladding thickness, cm
!        real(8), optional :: thkgap      ! Gap thickness, cm
!        real(8), optional :: dco         ! Outer cladding diameter, cm
!        real(8), optional :: pitch       ! Fuel rod pitch, cm
!        real(8), optional :: den         ! As-fabricated apparent fuel density, %TD
!        real(8), optional :: enrch       ! Fuel enrichment u-235
!        real(8), optional :: dx(:)       ! Thickness of the axial nodes, cm


    end subroutine frod_make

    subroutine frod_init(this)

        class (frod_type), intent(inout) :: this

        call this % driver % load()
        call this % driver % proc() ! processing and checking of input variables
        call this % driver % init() ! make the very first time step

    end subroutine frod_init

    subroutine frod_next(this, dt)

        class (frod_type), intent(inout) :: this

        real(8) :: dt

        call this % driver % load()
        call this % driver % next(dt)

    end subroutine frod_next

    subroutine frod_save(this, filename)

        class (frod_type), intent(inout) :: this

        character(*) :: filename

        call this % driver % save_state(filename)

    end subroutine frod_save

    subroutine frod_load(this, filename)

        class (frod_type), intent(inout) :: this

        character(*) :: filename
        
        call this % driver % load_state(filename)

    end subroutine frod_load

    subroutine frod_accept(this)

        class (frod_type), intent(inout) :: this

        real(8) :: dt

        call this % driver % dump()

    end subroutine frod_accept

    subroutine frod_set_value(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var
        real(8)      :: mmtoin = 1.d0/intomm

        it = this % driver % r__it

        select case(key)
        case("fuel rod pitch, cm")
            this % driver % r__pitch = var * cmtoin
        case("as-fabricated apparent fuel density, %TD")
            this % driver % r__den = var
        case("coolant mass flux, kg|(s*m^2)")
            this % driver % r__go(it) = var * ksm2tolbhrft2
        case("additional fuel densification factor")
            this % driver % r__afdn         = var
        case("cladding type")
            this % driver % r__icm          = var
        case("crud model")
            this % driver % r__icor         = var
        case("clad texture factor")
            this % driver % r__catexf       = var
        case("as-fabricated clad hydrogen content, wt.ppm")
            this % driver % r__chorg        = var                       
        case("clad cold work")
            this % driver % r__cldwks       = var                       
        case("cold plenum length, m")
            this % driver % r__cpl          = var * mtoin
        case("constant crud thickness, mm")
            this % driver % r__crdt         = var / miltomm
        case("crud accumulation rate")
            this % driver % r__crdtr        = var                       
        case("creep step duration, hr")
            this % driver % r__crephr       = var                       
        case("fuel open porosity fraction, %TD")
            this % driver % r__deng         = var                       
        case("spring diameter, mm")
            this % driver % r__dspg         = var * mmtoin              
        case("spring wire diameter, mm")
            this % driver % r__dspgw        = var * mmtoin              
        case("number of spring turns")
            this % driver % r__vs           = var
        case("peak-to-average power ratio")
            this % driver % r__fa           = var                       
        case("fill gas pressure, Pa")
            this % driver % r__fgpav        = var * PatoPSI             
        case("fuel oxygen-to-metal ratio")
            this % driver % r__fotmtl       = var                       
        case("fill gas type")
            this % driver % r__idxgas       = var                       
        case("plant type")
            this % driver % r__iplant       = var                       
        case("fuel type")
            this % driver % r__imox         = var                       
        case("weight ppm H2O in fuel, wt.ppm")
            this % driver % r__ppmh2o       = var                       
        case("weight ppm N2 in fuel, wt. ppm")
            this % driver % r__ppmn2        = var                       
        case("expected resintering density increase, kg|m^3")
            this % driver % r__rsntr        = var                       
        case("fision gas atoms per 100 fissions")
            this % driver % r__sgapf        = var                       
        case("swelling limit")
            this % driver % r__slim         = var                       
        case("pellet centering temperature, K")
            this % driver % r__tsint        = tkf(var)                  
        case("grain size of the fuel, um")
            this % driver % r__grnsize      = var                       
        case("FEA friction coefficient")
            this % driver % r__frcoef       = var                       
        case("FEA internal pressure flag")
            this % driver % r__igascal      = var                       
        case("percent IFBA rods in core, %")
            this % driver % r__ifba         = var                       
        case("boron-10 enrichment in ZrB2, atom %")
            this % driver % r__b10          = var                       
        case("ZrB2 thickness, mm")
            this % driver % r__zrb2thick    = var * mmtoin               
        case("ZrB2 density, %TD")
            this % driver % r__zrb2den      = var
        case("zircaloy-2 vintage")
            this % driver % r__zr2vintage   = var                       
        case("decay heat multiplier")
            this % driver % r__fpdcay       = var                       
        case("flag for type of Pu used in MOX")
            this % driver % r__moxtype      = var                       
        case("molar fraction of air")
            this % driver % r__amfair       = var                       
        case("molar fraction of argon")
            this % driver % r__amfarg       = var                       
        case("molar fraction of fission gas")
            this % driver % r__amffg        = var                       
        case("molar fraction of helium")
            this % driver % r__amfhe        = var                       
        case("molar fraction of hydrogen")
            this % driver % r__amfh2        = var                       
        case("molar fraction of water")
            this % driver % r__amfh2o       = var                       
        case("molar fraction of krypton")
            this % driver % r__amfkry       = var                       
        case("molar fraction of nitrogen")
            this % driver % r__amfn2        = var                       
        case("molar fraction of xenon")
            this % driver % r__amfxe        = var                       
        case("Bias on fuel thermal conductivity")
            this % driver % r__sigftc       = var                       
        case("Bias on fuel thermal expansion")
            this % driver % r__sigftex      = var                       
        case("Bias on fission gas release")
            this % driver % r__sigfgr       = var                       
        case("Bias on fuel swelling")
            this % driver % r__sigswell     = var                       
        case("Bias on cladding creep")
            this % driver % r__sigcreep     = var                       
        case("Bias on cladding axial growth")
            this % driver % r__siggro       = var                       
        case("Bias on cladding corrosion")
            this % driver % r__sigcor       = var                       
        case("Bias on cladding hydrogen pickup")
            this % driver % r__sigh2        = var                       
        case("fuel pellet Pu-239 content")
            this % driver % r__enrpu39      = var                       
        case("fuel pellet Pu-240 content")
            this % driver % r__enrpu40      = var                       
        case("fuel pellet Pu-241 content")
            this % driver % r__enrpu41      = var                       
        case("fuel pellet Pu-242 content")
            this % driver % r__enrpu42      = var
        case("pellet height, mm")
            this % driver % r__hplt         = var * mmtoin
        case("chamfer height, mm")
            this % driver % r__chmfrh       = var * mmtoin
        case("chamfer width, mm")
            this % driver % r__chmfrw       = var * mmtoin
        case("dish shoulder width, mm")
            this % driver % r__dishsd       = var * mmtoin
        case("dish height, mm")
            this % driver % r__hdish        = var * mmtoin
        case("clad roughness, mm")
            this % driver % r__roughc       = var * mmtoin
        case("fuel roughness, mm")
            this % driver % r__roughf       = var * mmtoin
        case("end-node to plenum heat transfer fraction")
            this % driver % r__qend(it)     = var
        case("rod internal pressure for FEA model, MPa")
            this % driver % r__p1(it)       = var * patoPSI
        case("inlet coolant temperature, C")
            this % driver % r__tw(it) = tcf(var)
        case("inlet coolant pressure, MPa")
            this % driver % r__p2(it) = var * MPatoPSI
        case("fuel enrichment by u-235, %")
            this % driver % r__enrch(:) = var
        case("cladding thickness, cm")
            this % driver % r__thkcld(:) = var * cmtoin
        case("gap thickness, cm")
            this % driver % r__thkgap(:) = var * cmtoin
        case("outer cladding diameter, cm")
            this % driver % r__dco(:) = var * cmtoin
        case("coolant system pressure, MPa")
            this % driver % r__p2(it) = var * MPatoPSI
        case("radius of the fuel pellet central annulus, mm")
            this % driver % r__rc(:) = var * mmtoin           
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_value

    subroutine frod_set_array(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:)
        real(8)      :: mmtoin = 1.d0/intomm

        it = this % driver % r__it

        select case(key)
        case("thickness of the axial nodes, cm")
            this % driver % r__deltaz(1:n) = var(:) * cmtoft            
            this % driver % r__x(1)        = 0.d0                        ! Axial evaluation for linear power distribution, ft
            this % driver % r__x(2:n+1)    = (/( sum(this % driver % r__deltaz(:i)), i = 1, n )/)
            this % driver % r__deltaz(n+1) = this % driver % r__cpl
            this % driver % r__totl        = sum(this % driver % r__deltaz(1:n))            ! Total length of active fuel, ft
            this % driver % r__zcool(:)    = this % driver % r__x(:)        ! Axial evaluation for coolant temperature distribution, ft
        case("cladding thickness, cm")
            this % driver % r__thkcld(1:n) = var(:) * cmtoin
        case("gap thickness, cm")
            this % driver % r__thkgap(1:n) = var(:) * cmtoin
        case("outer cladding diameter, cm")
            this % driver % r__dco(1:n) = var(:) * cmtoin

        case("-linear power, W|cm")
            this % driver % r__qmpy(it) = sum(var) / cmtoft * & 
            sum(this % driver % r__deltaz(1:n) / this % driver % r__dco(1:n)) / pi / intoft * WtoBTUh / this % driver % r__totl
            this % driver % r__qf(:) = var(:) / sum(var)
        case("-coolant temperature, C")
            this % driver % r__coolanttemp(it,1:n+1) = (/( tcf(var(i)), i = 1, n+1 )/)
            this % driver % r__tcoolant(1:n+1) = (/( tcf(var(i)), i = 1, n+1 )/)
        case("-coolant pressure, MPa")
            this % driver % r__p2(it) = var(1) * MPatoPSI
            this % driver % r__coolantpressure(it,1:n+1) = var(:) * MPatoPSI
            this % driver % r__pcoolant(1:n+1) = var(:) * MPatoPSI


        case("linear power, W|cm")
            call linterp(var, this % driver % r__deltaz(1:n), tmp3, n)
            a = sum( var(:) * this % driver % r__deltaz(1:n) ) / this % driver % r__totl /cmtoft ! W/ft
            b = sum( this % driver % r__deltaz(1:n) / this % driver % r__dco(1:n) ) / this % driver % r__totl / intoft ! 1/ft
            this % driver % r__qmpy(it) = a * b / pi * WtoBTUh ! BTUh/ft^2
            this % driver % r__qf(:) = tmp3(:) / sum(tmp3)
        case("coolant temperature, C")
            call linterp(var,  this % driver % r__deltaz(1:n), tmp3, n)
            this % driver % r__coolanttemp(it,1:n+1) = (/( tcf(tmp3(i)), i = 1, n+1 )/)
            this % driver % r__tcoolant(1:n+1) = (/( tcf(tmp3(i)), i = 1, n+1 )/)
        case("coolant pressure, MPa")
            call linterp(var, this % driver % r__deltaz(1:n), tmp3, n)
            this % driver % r__p2(it) = var(1) * MPatoPSI
            this % driver % r__coolantpressure(it,1:n+1) = tmp3(:) * MPatoPSI
            this % driver % r__pcoolant(1:n+1) = tmp3(:) * MPatoPSI
        case("input fuel burnup")
            this % driver % r__buin(:)      = var(:) * MWskgUtoMWdMTU
        case("PuO2 weight percent if MOX fuel, wt%")
            this % driver % r__comp(:)      = var(:)
        case("Heat flux, W|m^2")
            this % driver % r__qc(:)        = var(:) / Bhft2toWm2
        case("gadolinia content at each axial node")
            this % driver % r__gadoln(:)    = var(:)
        case("cladding surface temperature, K")
            this % driver % r__cladt(:)     = (/( tkf(var(i)), i = 1, n )/)
        case("axial crud thickness multiplier")
            this % driver % r__crudmult(:)  = var(:)
        case("neutron flux, 1|(cm^2*s)")
            this % driver % r__flux(:)  = var(:)

        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_array


    subroutine frod_get_value(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var
        real(8)      :: ra, rb, ya, yb, h, temper, volume
        real(8)      :: linteg ! integral of linear function

        it = this % driver % it

        select case(key)
        case('average linear power, W|cm')
            var = this % driver % qmpy(it) * BTUhtokW * &
                 (this % driver % dcoBOL * intoft * pi) / this % driver % fa * &
                  1.D+3 * cmtoft
        case('outlet coolant mass flux, kg|(s*m^2)')
            var = this % driver % go(it) * lbhrft2toksm2
        case('plenum gas temperature, C')
            var = tfc(this % driver % tplen)
        case('plenum gas pressure, MPa')
            var = this % driver % press * PSItoMPa
        case('fission gas release, %')
            if(this % driver % ngasmod == 4) then
                var = sum(this % driver % rb_rod(:,it)) * 100.d0
            else
                var = this % driver % tfgfr * 100.d0
            endif
        case('time, day')
            var = this % driver % ProblemTime(it) * sectoday
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_value

    subroutine frod_get_array(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:) ! array (n,)
        real(8)      :: ra, rb, ya, yb, h, temper, volume
        real(8)      :: linteg ! integral of linear function
        real(8)      :: intoum = intomm * 1.D+3

        it = this % driver % it

        select case(key)
!        case('axial fuel temperature, C')
!            do i = 1, n
!                volume = 0
!                temper = 0
!                do j = 1, m
!                    ya = 1.d0
!                    yb = 1.d0
!                    ra = this % driver % hrad(j+1,i)
!                    rb = this % driver % hrad(j,i)
!                    h = this % driver % x(i+1) - this % driver % x(i)
!                    volume = volume + linteg(ya,yb,ra,rb,h)
!                    ya = this % driver % tmpfuel(j+1,i)
!                    yb = this % driver % tmpfuel(j,i)
!                    temper = temper + linteg(ya,yb,ra,rb,h)
!                enddo
!                var(i) = tfc(temper / volume)
!            enddo
        case('fuel volume average temperature, C')
            var(:) = (/( tfc(this % driver % PelAveTemp(i)), i = 1, n )/)
        case('gap average temperature, C')
            var(:) = (/( tfc(this % driver % GapAveTemp(i)), i = 1, n )/)
        case('cladding average temperature, C')
            var(:) = (/( tfc(this % driver % CladAveTemp(i)), i = 1, n )/)
        case('bulk coolant temperature, C')
            var(:) = 0.5d0 * ( this % driver % BulkCoolantTemp(1:n) + this % driver % BulkCoolantTemp(2:n+1) )
            var(:) = (/( tfc(var(i)), i = 1, n )/)
        case('total gap conductance, W|(m^2*K)')
            var(:) = this % driver % TotalHgap(1:n) * Bhft2FtoWm2K
        case('oxide thickness, um')
            var(:) = this % driver % EOSZrO2Thk(1:n) * fttomil * miltoum
        case('thermal gap thickness, um')
            var(:) = this % driver % gapplot(1:n) * miltoum
        case('mechanical gap thickness, um')
            var(:) = this % driver % FuelCladGap(1:n) * 1.D+3 * miltoum
        case('gap pressure, MPa')
            var(:) = this % driver % GapPress(1:n) * PSItoMPa
        case('cladding hoop strain, %')
            var(:) = this % driver % eps(1:n,1) * 100
        case('cladding axial strain, %')
            var(:) = this % driver % eps(1:n,2) * 100
        case('cladding radial strain, %')
            var(:) = this % driver % eps(1:n,3) * 100
        case('cladding permanent hoop strain, %')
            var(:) = this % driver % epp(1:n,1) * 100
        case('cladding permanent axial strain, %')
            var(:) = this % driver % epp(1:n,2) * 100
        case('cladding permanent radial strain, %')
            var(:) = this % driver % epp(1:n,3) * 100
        case('cladding termal hoop strain, %')
            var(:) = this % driver % ThermalStrain(1:n,1) * 100
        case('cladding termal axial strain, %')
            var(:) = this % driver % ThermalStrain(1:n,2) * 100
        case('cladding termal radial strain, %')
            var(:) = this % driver % ThermalStrain(1:n,3) * 100
        case('cladding hoop stress, MPa')
            var(:) = this % driver % sig(1:n,1) * PSItoMPa
        case('cladding axial stress, MPa')
            var(:) = this % driver % sig(1:n,2) * PSItoMPa
        case('cladding radial stress, MPa')
            var(:) = this % driver % sig(1:n,3) * PSItoMPa
        case('cladding inner radius displacement, mm')
            var(:) = this % driver % totinner(1:n) * intomm
        case('cladding outer radius displacement, mm')
            var(:) = this % driver % totcrl(1:n) * intomm
        case('cladding creep rate')
            var(:) = this % driver % creapratearray(1:n)
        case('fuel surface outward displacement, mm')
            var(:) = this % driver % totdef(1:n) * intomm
        case('fuel thermal expansion, mm')
            var(:) = this % driver % fuelexptot(1:n) * intomm
        case('fuel swelling, um')
            var(:) = this % driver % fuelswltot(1:n) * intoum
        case('fuel creep, mm')
            var(:) = this % driver % fuelcreeptot(1:n) * intomm
        case('fuel densification, mm')
            var(:) = this % driver % fueldentot(1:n) * intomm
        case('fuel relocation, mm')
            var(:) = this % driver % relocation(1:n) * intomm
        case('cladding hydrogen concentration, ppm')
            var(:) = this % driver % CladH2Concen(1:n)
        case('coolant density, kg|m^3')
            var(:) = this % driver % rhof(1:n) * lbft3tokgm3
        case('coolant pressure, MPa')
            var(:) = this % driver % coolantpressure(it,1:n) * PSItoMPa
        case('axial mesh, cm')
            var(:) = 0.5d0 * (this % driver % x(1:n) + this % driver % x(2:n+1)) / cmtoft
        case('gas release fractions')
            var(:) = this % driver % RB_rod(1:11,it)
        case('centerline temperature, C')
            var(:) = (/( tfc(this % driver % tmpfuel(m+1,i)), i = 1, n )/)
        case('fuel stored energy, J|kg')
            var(:) = this % driver % StoredEnergy(1:n) * BTUlbtoJkg
        case('fuel burnup, MW*s|kg')
            var(:) = this % driver % EOSNodeburnup(1:n) * 1.D-3 / MWskgUtoMWdMTU
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_array

    subroutine frod_destroy(this)

        class (frod_type), intent(inout) :: this

        call this % driver % destroy()

    end subroutine frod_destroy

end module fuelrod



