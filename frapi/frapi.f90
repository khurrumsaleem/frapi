module frapi

    use conversions_frapcon
    use frapcon4,  only : frapcon_driver
    use fraptran2, only : fraptran_driver

    implicit none

    type, public :: frod_type
        type( frapcon_driver) :: dfcon          ! Burnup steady-state calculations
        type(fraptran_driver) :: dftran          ! Transient calculations

    contains
        procedure :: make      => frod_make         ! Initialize the fuel rod
        procedure :: init      => frod_init         ! Set the initial fuel rod state, t = 0
        procedure :: next      => frod_next         ! Perform the trial time step, dt > 0
        procedure :: accept    => frod_accept       ! Reject the last time step

        ! old interface: ---------------------------------------------------
        procedure :: set_value => frod_set_r8_0    ! Set variable value
        procedure :: set_array => frod_set_r8_1    ! Set variable array
        procedure :: get_value => frod_get_r8_0    ! Get variable value
        procedure :: get_array => frod_get_r8_1    ! Get variable array
        !--------------------------------------------------------------------

        procedure :: set_r8_0  => frod_set_r8_0     ! set real variable
        procedure :: set_r8_1  => frod_set_r8_1     ! set real array of dimension 1

        procedure :: get_r8_0  => frod_get_r8_0     ! get real variable
        procedure :: get_r8_1  => frod_get_r8_1     ! get real array of dimension 1
        procedure :: get_r8_2  => frod_get_r8_2     ! get real array of dimension 2
        procedure :: get_i4_0  => frod_get_i4_0     ! get integer variable

        procedure :: save      => frod_save         ! Save fuel rod state in a file
        procedure :: load      => frod_load         ! Load fuel rod state from a file
        procedure :: destroy   => frod_destroy      ! Deallocate the fuel rod variables
!        procedure :: transient => p_transient       ! Transient time step
        procedure :: frapc2t   => p_frapc2t         ! Pass data from FRAPCON to FRAPTRAN
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
                  ifixedcoolt, ifixedcoolp, ifixedtsurf, verbose, flag_iapws)

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
        logical, optional :: flag_iapws  ! flag to indicate the iapws-if97 version of steam table to be used (default : true)

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

        call this % dfcon % make(na_, ngasr_, nr_+1, nce_, verbose_)

        call this % dfcon % deft()

        if( present(mechan     ) ) this % dfcon % mechan      = mechan     
        if( present(ngasmod    ) ) this % dfcon % ngasmod     = ngasmod    
        if( present(icm        ) ) this % dfcon % icm         = icm        
        if( present(icor       ) ) this % dfcon % icor        = icor       
        if( present(iplant     ) ) this % dfcon % iplant      = iplant     
        if( present(imox       ) ) this % dfcon % imox        = imox        
        if( present(igascal    ) ) this % dfcon % igascal     = igascal    
        if( present(zr2vintage ) ) this % dfcon % zr2vintage  = zr2vintage 
        if( present(moxtype    ) ) this % dfcon % moxtype     = moxtype    
        if( present(idxgas     ) ) this % dfcon % idxgas      = idxgas     
        if( present(iq         ) ) this % dfcon % iq          = iq
        if( present(ivardm     ) ) this % dfcon % ivardm      = ivardm
        if( present(ifixedcoolt) ) this % dfcon % ifixedcoolt = ifixedcoolt
        if( present(ifixedcoolp) ) this % dfcon % ifixedcoolp = ifixedcoolp
        if( present(ifixedtsurf) ) this % dfcon % ifixedtsurf = ifixedtsurf

        call this % dfcon % dump()

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

! fraptran initialization
!        ! FRAPCON radial nodes at fuel pellet surface, cladding inside surface, cladding outside surface
!        READ(fcunit,*) nfofs, ncifs, ncofs

        ! Radius to each FRAPCON radial node (ft)
!        this % dftran % radfs(1:nr) = this % dfcon % crad(1:nr) ! 
!        this % dftran % radfs(nr+1) = this % dfcon % crad(nr+1)
!        this % dftran % radfs(nr+2) = this % dfcon % crad(nr+2)

!        this % dftran % ngasr = 
!            ALLOCATE (ansr(1:ngasr))
!            ALLOCATE (gasavail1(1:naxn,1:ngasr))
!            ALLOCATE (gasavail2(1:naxn,1:ngasr))

    end subroutine frod_make

    subroutine frod_init(this)

        class (frod_type), intent(inout) :: this

        call this % dfcon % load()
        call this % dfcon % proc() ! processing and checking of input variables
        call this % dfcon % init() ! make the very first time step

    end subroutine frod_init

    subroutine frod_next(this, dt)

        class (frod_type), intent(inout) :: this

        real(8) :: dt

        call this % dfcon % load()
        call this % dfcon % next(dt)

    end subroutine frod_next

    subroutine frod_save(this, filename)

        class (frod_type), intent(inout) :: this

        character(*) :: filename

        call this % dfcon % save_state(filename)

    end subroutine frod_save

    subroutine frod_load(this, filename)

        class (frod_type), intent(inout) :: this

        character(*) :: filename
        
        call this % dfcon % load_state(filename)

    end subroutine frod_load

    subroutine frod_accept(this)

        class (frod_type), intent(inout) :: this

        call this % dfcon % dump()

    end subroutine frod_accept

    subroutine frod_set_r8_0(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var
        real(8)      :: mmtoin = 1.d0/intomm

        it = this % dfcon % r__it

        select case(key)
        case("fuel rod pitch, cm")
            this % dfcon % r__pitch = var * cmtoin
        case("as-fabricated apparent fuel density, %TD")
            this % dfcon % r__den = var
        case("coolant mass flux, kg|(s*m^2)")
            this % dfcon % r__go(it) = var * ksm2tolbhrft2
        case("additional fuel densification factor")
            this % dfcon % r__afdn         = var
        case("clad texture factor")
            this % dfcon % r__catexf       = var
        case("as-fabricated clad hydrogen content, wt.ppm")
            this % dfcon % r__chorg        = var                       
        case("clad cold work")
            this % dfcon % r__cldwks       = var                       
        case("cold plenum length, m")
            this % dfcon % r__cpl          = var * mtoin
        case("constant crud thickness, mm")
            this % dfcon % r__crdt         = var / miltomm
        case("crud accumulation rate")
            this % dfcon % r__crdtr        = var                       
        case("creep step duration, hr")
            this % dfcon % r__crephr       = var                       
        case("fuel open porosity fraction, %TD")
            this % dfcon % r__deng         = var                       
        case("spring diameter, mm")
            this % dfcon % r__dspg         = var * mmtoin              
        case("spring wire diameter, mm")
            this % dfcon % r__dspgw        = var * mmtoin              
        case("number of spring turns")
            this % dfcon % r__vs           = var
        case("peak-to-average power ratio")
            this % dfcon % r__fa           = var                       
        case("fill gas pressure, Pa")
            this % dfcon % r__fgpav        = var * PatoPSI             
        case("fuel oxygen-to-metal ratio")
            this % dfcon % r__fotmtl       = var                       
        case("weight ppm H2O in fuel, wt.ppm")
            this % dfcon % r__ppmh2o       = var                       
        case("weight ppm N2 in fuel, wt. ppm")
            this % dfcon % r__ppmn2        = var                       
        case("expected resintering density increase, kg|m^3")
            this % dfcon % r__rsntr        = var                       
        case("fision gas atoms per 100 fissions")
            this % dfcon % r__sgapf        = var                       
        case("swelling limit")
            this % dfcon % r__slim         = var                       
        case("pellet centering temperature, K")
            this % dfcon % r__tsint        = tkf(var)                  
        case("grain size of the fuel, um")
            this % dfcon % r__grnsize      = var                       
        case("FEA friction coefficient")
            this % dfcon % r__frcoef       = var                       
        case("percent IFBA rods in core, %")
            this % dfcon % r__ifba         = var                       
        case("boron-10 enrichment in ZrB2, atom %")
            this % dfcon % r__b10          = var                       
        case("ZrB2 thickness, mm")
            this % dfcon % r__zrb2thick    = var * mmtoin               
        case("ZrB2 density, %TD")
            this % dfcon % r__zrb2den      = var
        case("decay heat multiplier")
            this % dfcon % r__fpdcay       = var                       
        case("molar fraction of air")
            this % dfcon % r__amfair       = var                       
        case("molar fraction of argon")
            this % dfcon % r__amfarg       = var                       
        case("molar fraction of fission gas")
            this % dfcon % r__amffg        = var                       
        case("molar fraction of helium")
            this % dfcon % r__amfhe        = var                       
        case("molar fraction of hydrogen")
            this % dfcon % r__amfh2        = var                       
        case("molar fraction of water")
            this % dfcon % r__amfh2o       = var                       
        case("molar fraction of krypton")
            this % dfcon % r__amfkry       = var                       
        case("molar fraction of nitrogen")
            this % dfcon % r__amfn2        = var                       
        case("molar fraction of xenon")
            this % dfcon % r__amfxe        = var                       
        case("Bias on fuel thermal conductivity")
            this % dfcon % r__sigftc       = var                       
        case("Bias on fuel thermal expansion")
            this % dfcon % r__sigftex      = var                       
        case("Bias on fission gas release")
            this % dfcon % r__sigfgr       = var                       
        case("Bias on fuel swelling")
            this % dfcon % r__sigswell     = var                       
        case("Bias on cladding creep")
            this % dfcon % r__sigcreep     = var                       
        case("Bias on cladding axial growth")
            this % dfcon % r__siggro       = var                       
        case("Bias on cladding corrosion")
            this % dfcon % r__sigcor       = var                       
        case("Bias on cladding hydrogen pickup")
            this % dfcon % r__sigh2        = var                       
        case("fuel pellet Pu-239 content")
            this % dfcon % r__enrpu39      = var                       
        case("fuel pellet Pu-240 content")
            this % dfcon % r__enrpu40      = var                       
        case("fuel pellet Pu-241 content")
            this % dfcon % r__enrpu41      = var                       
        case("fuel pellet Pu-242 content")
            this % dfcon % r__enrpu42      = var
        case("pellet height, mm")
            this % dfcon % r__hplt         = var * mmtoin
        case("chamfer height, mm")
            this % dfcon % r__chmfrh       = var * mmtoin
        case("chamfer width, mm")
            this % dfcon % r__chmfrw       = var * mmtoin
        case("dish shoulder width, mm")
            this % dfcon % r__dishsd       = var * mmtoin
        case("dish height, mm")
            this % dfcon % r__hdish        = var * mmtoin
        case("clad roughness, mm")
            this % dfcon % r__roughc       = var * mmtoin
        case("fuel roughness, mm")
            this % dfcon % r__roughf       = var * mmtoin
        case("end-node to plenum heat transfer fraction")
            this % dfcon % r__qend(it)     = var
        case("rod internal pressure for FEA model, MPa")
            this % dfcon % r__p1(it)       = var * patoPSI
        case("inlet coolant temperature, C")
            this % dfcon % r__tw(it) = tcf(var)
        case("inlet coolant pressure, MPa")
            this % dfcon % r__p2(it) = var * MPatoPSI
        case("fuel enrichment by u-235, %")
            this % dfcon % r__enrch(:) = var
        case("cladding thickness, cm")
            this % dfcon % r__thkcld(:) = var * cmtoin
        case("gap thickness, cm")
            this % dfcon % r__thkgap(:) = var * cmtoin
        case("outer cladding diameter, cm")
            this % dfcon % r__dco(:) = var * cmtoin
        case("coolant system pressure, MPa")
            this % dfcon % r__p2(it) = var * MPatoPSI
        case("radius of the fuel pellet central annulus, mm")
            this % dfcon % r__rc(:) = var * mmtoin  
        case("total gap conductance, W|(m^2*K)") ! YU JIANKAI
            this % dfcon % r__TotalHgap(:) = var * Wm2KtoBhft2F
            this % dfcon % r__hgapt_flag   = .true.
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_r8_0

    subroutine frod_set_r8_1(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:)
!        real(8)      :: mmtoin = 1.d0/intomm

        it = this % dfcon % r__it

        select case(key)
        case("thickness of the axial nodes, cm")
            this % dfcon % r__deltaz(1:n) = var(:) * cmtoft
            this % dfcon % r__x(1)        = 0.d0                        ! Axial evaluation for linear power distribution, ft
            this % dfcon % r__x(2:n+1)    = (/( sum(this % dfcon % r__deltaz(:i)), i = 1, n )/)
            this % dfcon % r__deltaz(n+1) = this % dfcon % r__cpl
            this % dfcon % r__totl        = sum(this % dfcon % r__deltaz(1:n))            ! Total length of active fuel, ft
            this % dfcon % r__zcool(:)    = this % dfcon % r__x(:)        ! Axial evaluation for coolant temperature distribution, ft
        case("cladding thickness, cm")
            this % dfcon % r__thkcld(1:n) = var(:) * cmtoin
        case("gap thickness, cm")
            this % dfcon % r__thkgap(1:n) = var(:) * cmtoin
        case("outer cladding diameter, cm")
            this % dfcon % r__dco(1:n) = var(:) * cmtoin

        case("FRAPCON FORMAT: linear power, W|cm")
            this % dfcon % r__qmpy(it) = sum(var) / cmtoft * & 
            sum(this % dfcon % r__deltaz(1:n) / this % dfcon % r__dco(1:n)) &
            / pi / intoft * WtoBTUh / this % dfcon % r__totl
            this % dfcon % r__qf(:) = var(:) / sum(var)
        case("FRAPCON FORMAT: coolant temperature, C")
            this % dfcon % r__coolanttemp(it,1:n+1) = (/( tcf(var(i)), i = 1, n+1 )/)
            this % dfcon % r__tcoolant(1:n+1) = (/( tcf(var(i)), i = 1, n+1 )/)
        case("FRAPCON FORMAT: coolant pressure, MPa")
            this % dfcon % r__p2(it) = var(1) * MPatoPSI
            this % dfcon % r__coolantpressure(it,1:n+1) = var(:) * MPatoPSI
            this % dfcon % r__pcoolant(1:n+1) = var(:) * MPatoPSI


        case("linear power, W|cm")
            call linterp(var, this % dfcon % r__deltaz(1:n), tmp3, n)
            a = sum( var(:) * this % dfcon % r__deltaz(1:n) ) / this % dfcon % r__totl /cmtoft ! W/ft
            b = sum( this % dfcon % r__deltaz(1:n) / this % dfcon % r__dco(1:n) ) &
                / this % dfcon % r__totl / intoft ! 1/ft
            this % dfcon % r__qmpy(it) = a * b / pi * WtoBTUh ! BTUh/ft^2
            this % dfcon % r__qf(:) = tmp3(:) / sum(tmp3)
        case("coolant temperature, C")
            call linterp(var,  this % dfcon % r__deltaz(1:n), tmp3, n)
            this % dfcon % r__coolanttemp(it,1:n+1) = (/( tcf(tmp3(i)), i = 1, n+1 )/)
            this % dfcon % r__tcoolant(1:n+1) = (/( tcf(tmp3(i)), i = 1, n+1 )/)
        case("coolant pressure, MPa")
            call linterp(var, this % dfcon % r__deltaz(1:n), tmp3, n)
            this % dfcon % r__p2(it) = var(1) * MPatoPSI
            this % dfcon % r__coolantpressure(it,1:n+1) = tmp3(:) * MPatoPSI
            this % dfcon % r__pcoolant(1:n+1) = tmp3(:) * MPatoPSI
        case("input fuel burnup")
            this % dfcon % r__buin(:)      = var(:) * MWskgUtoMWdMTU
        case("PuO2 weight percent if MOX fuel, wt%")
            this % dfcon % r__comp(:)      = var(:)
        case("Heat flux, W|m^2")
            this % dfcon % r__qc(:)        = var(:) / Bhft2toWm2
        case("gadolinia weight, wt%")
            this % dfcon % r__gadoln(:)    = var(:)
        case("cladding surface temperature, K")
            this % dfcon % r__cladt(:)     = (/( tkf(var(i)), i = 1, n )/)
        case("axial crud thickness multiplier")
            this % dfcon % r__crudmult(:)  = var(:)
        case("neutron flux, 1|(cm^2*s)")
            this % dfcon % r__flux(:)  = var(:)

        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_r8_1

    subroutine frod_get_i4_0(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        integer      :: var

        it = this % dfcon % it

        select case(key)
        case('program terminate')
            var = this % dfcon % iquit
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_i4_0

    subroutine frod_get_r8_0(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var
!        real(8)      :: ra, rb, ya, yb, h, temper, volume
!        real(8)      :: linteg ! integral of linear function

        it = this % dfcon % it

        select case(key)
        case('average linear power, W|cm')
            var = this % dfcon % qmpy(it) * BTUhtokW * &
                 (this % dfcon % dcoBOL * intoft * pi) / this % dfcon % fa * &
                  1.D+3 * cmtoft
        case('outlet coolant mass flux, kg|(s*m^2)')
            var = this % dfcon % go(it) * lbhrft2toksm2
        case('plenum gas temperature, C')
            var = tfc(this % dfcon % tplen)
        case('plenum gas pressure, MPa')
            var = this % dfcon % press * PSItoMPa
        case('fission gas release, %')
            if(this % dfcon % ngasmod == 4) then
                var = sum(this % dfcon % rb_rod(:,it)) * 100.d0
            else
                var = this % dfcon % tfgfr * 100.d0
            endif
        case('time, day')
            var = this % dfcon % ProblemTime(it) * sectoday
        case('average fuel burnup, MW*d|kg')
            var = this % dfcon % bu * 1.D-3
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_r8_0

    subroutine frod_get_r8_1(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:) ! array (n,)
!        real(8)      :: ra, rb, ya, yb, h, temper, volume
!        real(8)      :: linteg ! integral of linear function
        real(8)      :: intoum = intomm * 1.D+3

        it = this % dfcon % it

        select case(key)
!        case('axial fuel temperature, C')
!            do i = 1, n
!                volume = 0
!                temper = 0
!                do j = 1, m
!                    ya = 1.d0
!                    yb = 1.d0
!                    ra = this % dfcon % hrad(j+1,i)
!                    rb = this % dfcon % hrad(j,i)
!                    h = this % dfcon % x(i+1) - this % dfcon % x(i)
!                    volume = volume + linteg(ya,yb,ra,rb,h)
!                    ya = this % dfcon % tmpfuel(j+1,i)
!                    yb = this % dfcon % tmpfuel(j,i)
!                    temper = temper + linteg(ya,yb,ra,rb,h)
!                enddo
!                var(i) = tfc(temper / volume)
!            enddo
        case('fuel volume average temperature, C')
            var(:) = (/( tfc(this % dfcon % PelAveTemp(i)), i = 1, n )/)
        case('gap average temperature, C')
            var(:) = (/( tfc(this % dfcon % GapAveTemp(i)), i = 1, n )/)
        case('cladding average temperature, C')
            var(:) = (/( tfc(this % dfcon % CladAveTemp(i)), i = 1, n )/)
        case('bulk coolant temperature, C')
            var(:) = 0.5d0 * ( this % dfcon % BulkCoolantTemp(1:n) + this % dfcon % BulkCoolantTemp(2:n+1) )
            var(:) = (/( tfc(var(i)), i = 1, n )/)
        case('total gap conductance, W|(m^2*K)')
            var(:) = this % dfcon % TotalHgap(1:n) * Bhft2FtoWm2K
        case('oxide thickness, um')
            var(:) = this % dfcon % EOSZrO2Thk(1:n) * fttomil * miltoum
        case('thermal gap thickness, um')
            var(:) = this % dfcon % gapplot(1:n) * miltoum
        case('mechanical gap thickness, um')
            var(:) = this % dfcon % FuelCladGap(1:n) * 1.D+3 * miltoum
        case('gap pressure, MPa')
            var(:) = this % dfcon % GapPress(1:n) * PSItoMPa
        case('cladding hoop strain, %')
            var(:) = this % dfcon % eps(1:n,1) * 100
        case('cladding axial strain, %')
            var(:) = this % dfcon % eps(1:n,2) * 100
        case('cladding radial strain, %')
            var(:) = this % dfcon % eps(1:n,3) * 100
        case('cladding permanent hoop strain, %')
            var(:) = this % dfcon % epp(1:n,1) * 100
        case('cladding permanent axial strain, %')
            var(:) = this % dfcon % epp(1:n,2) * 100
        case('cladding permanent radial strain, %')
            var(:) = this % dfcon % epp(1:n,3) * 100
        case('cladding termal hoop strain, %')
            var(:) = this % dfcon % ThermalStrain(1:n,1) * 100
        case('cladding termal axial strain, %')
            var(:) = this % dfcon % ThermalStrain(1:n,2) * 100
        case('cladding termal radial strain, %')
            var(:) = this % dfcon % ThermalStrain(1:n,3) * 100
        case('cladding hoop stress, MPa')
            var(:) = this % dfcon % sig(1:n,1) * PSItoMPa
        case('cladding axial stress, MPa')
            var(:) = this % dfcon % sig(1:n,2) * PSItoMPa
        case('cladding radial stress, MPa')
            var(:) = this % dfcon % sig(1:n,3) * PSItoMPa
        case('cladding inner radius displacement, mm')
            var(:) = this % dfcon % totinner(1:n) * intomm
        case('cladding outer radius displacement, mm')
            var(:) = this % dfcon % totcrl(1:n) * intomm
        case('cladding creep rate')
            var(:) = this % dfcon % creapratearray(1:n)
        case('fuel surface outward displacement, mm')
            var(:) = this % dfcon % totdef(1:n) * intomm
        case('fuel thermal expansion, mm')
            var(:) = this % dfcon % fuelexptot(1:n) * intomm
        case('fuel swelling, um')
            var(:) = this % dfcon % fuelswltot(1:n) * intoum
        case('fuel creep, mm')
            var(:) = this % dfcon % fuelcreeptot(1:n) * intomm
        case('fuel densification, mm')
            var(:) = this % dfcon % fueldentot(1:n) * intomm
        case('fuel relocation, mm')
            var(:) = this % dfcon % relocation(1:n) * intomm
        case('cladding hydrogen concentration, ppm')
            var(:) = this % dfcon % CladH2Concen(1:n)
        case('coolant density, kg|m^3')
            var(:) = 0.5d0*(this % dfcon % rhof(1:n) + this % dfcon % rhof(2:n+1)) * lbft3tokgm3 
        case('coolant pressure, MPa')
            var(:) = this % dfcon % coolantpressure(it,1:n) * PSItoMPa
        case('axial mesh, cm')
            var(:) = 0.5d0 * (this % dfcon % x(1:n) + this % dfcon % x(2:n+1)) / cmtoft
        case('gas release fractions')
            var(:) = this % dfcon % RB_rod(1:11,it)
        case('centerline temperature, C')
            var(:) = (/( tfc(this % dfcon % tmpfuel(m+1,i)), i = 1, n )/)
        case('fuel stored energy, J|kg')
            var(:) = this % dfcon % StoredEnergy(1:n) * BTUlbtoJkg
        case('fuel burnup, MW*d|kg')
            var(:) = this % dfcon % EOSNodeburnup(1:n) * 1.D-3 ! / MWskgUtoMWdMTU
        case('cladding inner temperature, C')
            var(:) = (/(tfc(this % dfcon % CladInSurfTemp(i)), i = 1, n )/) 
        case('cladding outer temperature, C')
            var(:) = (/(tfc(this % dfcon % CladOutSurfTemp(i)), i = 1, n )/)  
        case('cladding middle temperature, C')
            var(:) = (/(tfc(this % dfcon % CladOutSurfTemp(i) + this % dfcon % CladOutSurfTemp(i))*0.5d0, i = 1, n )/)  
        case('radial meshes, cm')
            var(:) = (/(this % dfcon % hrad(m - i + 1, 1), i = 0, m )/) 
            var(:) = var(:) * intocm
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_r8_1


    subroutine frod_get_r8_2(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:,:) ! array (n,)
        real(8)      :: intoum = intomm * 1.D+3

        it = this % dfcon % it

        select case(key)
            case('fuel temperature distribution, C') ! YU JIANKAI
                do i = 1, n 
                    do j = 1, m + 1
                        var(j,i) = tfc(this % dfcon % tmpfuel(m + 2 - j, i))
                    enddo
                enddo 
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get_r8_2

    subroutine frod_destroy(this)

        class (frod_type), intent(inout) :: this

        call this % dfcon % destroy()
        call this % dftran % destroy()

    end subroutine frod_destroy

    subroutine p_transient(this, dt)
        !
        ! Make transient time step by FRAPTRAN
        !
        class (frod_type), intent(inout) :: this

        real(8), intent(in) :: dt

        call this % dftran % next(dt)

    end subroutine p_transient

    subroutine p_frapc2t(this)

        class (frod_type), intent(inout) :: this

        call this % dfcon  % restfs()
        call this % dftran % restfs()

    end subroutine p_frapc2t


!    subroutine p_frapc2t(this)

!        class (frod_type), intent(inout) :: this

!        this % dfcon  % restfs()
!        this % dftran % restfs()
!
!        it = this % dfcon % it
!
!        ! Oxidation layer thickness at cladding outside surface, inches
!        this % dftran % BOSOxideThick(1:n) = this % dfcon % EOSZrO2Thk(1:n) * fttoin
!
!        ! Excess H2 concenctration in cladding (ppm)
!        this % dftran % cexh2a(1:n) = this % dfcon % CladH2Concen(1:n)
!
!        ! Peak historic cladding temp. (K) 
!        ! WTF?? << units missmatch !!!
!        this % dftran % CladMaxT(1:n) = this % dfcon % ctmax(1:n)
!
!        ! Open porosity at each axial node (fraction of pellet volume)
!        this % dftran % OpenPorosity(1:n) = this % dfcon % FuelPorosity(1:n)
!
!        ! Fuel burnup  (MW-sec/kg)
!        this % dftran % AxBurnup(1:n) = this % dfcon % EOSNodeburnup(1:n) * 86.4D0
!
!        ! Fast neutron fluence, n/m^2
!        tflux = this % dfcon % ProblemTime(it)
!        this % dftran % restfluence(1:n)= this % dfcon % FastFluence(1:n) / tflux
!
!        ! Gram-moles of gas in fuel rod
!        this % dftran % TotalGasMoles = this % dfcon % gasmo(it)
!
!        ! Fraction of each component of gas
!        ! Assume that nitrogen is set to 0.0 as this was not part of the original restart
!        this % dftran % GasFraction(1) = this % dfcon % gases(1) ! Helium
!        this % dftran % GasFraction(2) = this % dfcon % gases(2) ! Argon
!        this % dftran % GasFraction(3) = this % dfcon % gases(3) ! Krypton
!        this % dftran % GasFraction(4) = this % dfcon % gases(4) ! Xenon
!        this % dftran % GasFraction(5) = this % dfcon % gases(5) ! Hydrogen
!        this % dftran % GasFraction(6) = 0.D0                    ! << WTF??
!        this % dftran % GasFraction(7) = this % dfcon % gases(6) ! Nitrogen
!        this % dftran % GasFraction(8) = this % dfcon % gases(7) ! Air
!
!        ! Normalize radii to match FrapTran value
!        radnorm = RadialBound(igpnod) / radfs(nfofs)
!        radfsn(1:nfofs) = radfs(1:nfofs) * radnorm
!
!        ! Cladding plastic strains
!        this % dftran % CldPlasStrnFrapcon(1:n, 1:3) = this % dfcon % epp(1:n, 1:3)
!
!        ! Cladding effective plastic strain
!        this % dftran % EffStrain(1:n) = this % dfcon % efstrn(1:n)
!
!!        this % dftran % FrapconTemp(1:nfmesh,n) = this % dfcon % tmpfuel(1:n)
!! TODO: interpolation of the fuel/cladding temperatures
!
!        ! Net permanent fuel deformation due to fuel swelling and densification (no relocation); inches, convert to feet
!        this % dftran % SwellDispl(1:n) = this % dfcon % colddef(1:n) / 12.D0
!
!        ! Net permanent cladding deformation; inches, convert to feet
!        this % dftran % colddec(k:n) = this % dfcon % colddec(1:n) / 12.D0
!
!        ! Permanent fuel relocation displacement; inches, convert to feet
!        this % dftran % ureloc(1:n) = this % dfcon % ureloc(1:n) / 12.D0
!
!        ! Gadolinia content of fuel (WTF?? > currently only reads for 1 axial node)
!        this % dftran % gadolin(1:n) = this % dfcon % gadolin(1:n)
!
!        ! Radial burnup profile at each axial node
!        ! this % dftran % burado(1:n,1:m) = this % dfcon % brnup3(1:n,1:m)
!        ! Interpolate to define burnup profile at FrapTran nodes
!        ! TODO: interpolation of the burnup profile
!        ! burad(1:n,igpnod) <= burado(1:n,1:m)
!
!        ! Radial power profile at each axial node
!        ! TODO: interpolate the power profile
!        ! this % dftran % radsrco(1:n,1:igpnod) <= this % dfcon % rapow(1:n,1:m)
!
!        ! FRAPCON fission gas release model
!        this % dftran % ngasmod = this % dfcon % ngasmod
!        this % dftran % ansr(:) = this % dfcon % ansr(:)
!        this % dftran % gasavail1(:,:) = this % dfcon % gasavail1(:,:)
!        this % dftran % gasavail2(:,:) = this % dfcon % gasavail2(:,:)
!        this % dftran % fmgp(:) = this % dfcon % fmgp(:)
!
!    end subroutine p_frapc2t

end module frapi