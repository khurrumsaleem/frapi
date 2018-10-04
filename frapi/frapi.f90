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

        procedure :: set_ch_0  => frod_set_ch_0     ! set variable of type character and rank 0
        procedure :: set_i4_0  => frod_set_i4_0     ! set variable of type integer and rank 0
        procedure :: set_i4_1  => frod_set_i4_1     ! set variable of type integer and rank 1
        procedure :: set_r8_0  => frod_set_r8_0     ! set variable of type real and rank 0
        procedure :: set_r8_1  => frod_set_r8_1     ! set variable of type real and rank 1
        procedure :: set_r8_2  => frod_set_r8_2     ! set variable of type real and rank 2

        procedure :: get_i4_0  => frod_get_i4_0     ! get variable of type integer and rank 0
        procedure :: get_r8_0  => frod_get_r8_0     ! get variable of type real and rank 0
        procedure :: get_r8_1  => frod_get_r8_1     ! get variable of type real and rank 1
        procedure :: get_r8_2  => frod_get_r8_2     ! get variable of type real and rank 2

        procedure :: save      => frod_save         ! Save fuel rod state in a file
        procedure :: load      => frod_load         ! Load fuel rod state from a file
        procedure :: destroy   => frod_destroy      ! Deallocate the fuel rod variables
!        procedure :: transient => p_transient       ! Transient time step
        procedure :: frapc2t   => p_frapc2t         ! Pass data from FRAPCON to FRAPTRAN
    end type frod_type

    ! TEMPORARY VARIABLES
    character(len=20) :: frapmode_
    integer :: i, j, n, m
    real(8) :: a, b, c, volume
    real(8), allocatable :: weight(:)
    real(8), allocatable :: tmp0(:), tmp1(:), tmp2(:), tmp3(:)

contains

    subroutine frod_make(this, nr, na, ngasr, nce, frapmode, &
               mechan, ngasmod, icm, icor, iplant, &
               imox, igascal, zr2vintage, moxtype, idxgas, iq, ivardm, &
               ifixedcoolt, ifixedcoolp, ifixedtsurf, verbose, flag_iapws, relocmodel, &
               coolant, mheat, bheat, reflood, internal, metal, deformation, inst, geomet, &
               nvol1, lowpl, pressu, massfl, coreav, chf, filmbo, coldwa, axpow, bowing, &
               spefbz, geometry, nbundl, refloodtime, radiat, ruptur, liquid, inlet, reflo, &
               pressure, collaps, frapt4, geom, temp, tape2, nvol2, press, zone, upppl, &
               jfb, nucbo, unitin, unitout, res, pow, gasflo, idoxid, cathca, baker, &
               noball, cenvoi, soltyp)

        class (frod_type), intent(inout) :: this

        include "fi_optstatement_h.f90"

        integer :: nr_ = 17
        integer :: na_ = 10
        integer :: ngasr_ = 45
        integer :: nce_ = 5
        logical :: verbose_ = .false. 

        frapmode_ = 'frapcon'

        n  = na
        m  = nr

        if( present(nr          ) ) nr_      = nr
        if( present(na          ) ) na_      = na
        if( present(ngasr       ) ) ngasr_   = ngasr
        if( present(nce         ) ) nce_     = nce
        if( present(verbose     ) ) verbose_ = verbose
        if( present(frapmode    ) ) frapmode_= frapmode

        select case (frapmode_)

        case ('frapcon')

            call this % dfcon % make(na_, ngasr_, nr_+1, nce_, verbose_)

            include "fi_optassignment_h.f90"

            call this % dfcon % deft()

            call this % dfcon % dump()

        case ('fraptran')

            call this % dftran % make(na_, nr_, nce_, verbose_)

            include "fi_optassignment_h.f90"

            call this % dftran % deft()

        case ('frapi')

            call this % dfcon % make(na_, ngasr_, nr_+1, nce_, verbose_)

            call this % dftran % make(na_, nr_, nce_, verbose_)

            include "fi_optassignment_h.f90"

            call this % dfcon % deft()

            call this % dfcon % dump()

            call this % dftran % deft()

        case default
            write(*,*) "ERROR: 'mode' must be 'frapcon' or 'fraptran' "

        end select

        ! ALLOCATION OF THE TEMPORARY ARRAYS
        if(.not. allocated(weight)) allocate(weight(m))
        if(.not. allocated(tmp0))   allocate(tmp0(m))
        if(.not. allocated(tmp1))   allocate(tmp1(n))
        if(.not. allocated(tmp2))   allocate(tmp2(m+1))
        if(.not. allocated(tmp3))   allocate(tmp3(n+1))

    end subroutine frod_make

    subroutine frod_init(this)

        class (frod_type), intent(inout) :: this

        select case (frapmode_)
        case ('frapcon')
            call frod_init_frapcon_(this)
        case ('fraptran')
            call frod_init_fraptran_(this)
        case ('frapi')
            call frod_init_frapcon_(this)
            call frod_init_fraptran_(this)
        end select

    end subroutine frod_init

    subroutine frod_init_frapcon_(this)

        class (frod_type), intent(inout) :: this

        call this % dfcon % load()
        call this % dfcon % proc() ! processing and checking of input variables
        call this % dfcon % init() ! make the very first time step

    end subroutine frod_init_frapcon_

    subroutine frod_init_fraptran_(this)

        class (frod_type), intent(inout) :: this

        call this % dftran % openrf() ! open restart file
        call this % dftran % init()   ! init variables, including reading of the restart file

    end subroutine frod_init_fraptran_

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

    subroutine frod_set_ch_0(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key, var
        integer      :: it

        select case (key)
        case ("restart file")
            this % dftran % restart_file_name = trim(var)
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_ch_0

    subroutine frod_set_i4_0(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        integer(4)   :: var

        select case(key)
        case("azang")
            this % dftran % azang = var
        case("iStoicGrad")
            this % dftran % iStoicGrad = var
        case("prestmp")
            this % dftran % prestmp = var
        case("nbhtc")
            this % dftran % nbhtc = var
        case("rtheta")
            this % dftran % rtheta = var
        case("prescri")
            this % dftran % prescri = var
        case("mechan")
            this % dftran % mechan = var
        case("nfmesh")
            this % dftran % nfmesh = var
        case("NumAxProfiles")
            this % dftran % NumAxProfiles = var
        case("radiat")
            this % dftran % radiat = var
        case("maxit")
            this % dftran % maxit = var
        case("tape1")
            this % dftran % tape1 = var
        case("jtr")
            this % dftran % jtr = var
        case("NRestart")
            this % dftran % NRestart = var
        case("irupt")
            this % dftran % irupt = var
        case("ncards")
            this % dftran % ncards = var
        case("CladType")
            this % dftran % CladType = var
        case("odoxid")
            this % dftran % odoxid = var
        case("nchn")
            this % dftran % nchn = var
        case("TranSwell")
            this % dftran % TranSwell = var
        case("noiter")
            this % dftran % noiter = var
        case("nthermex")
            this % dftran % nthermex = var
        case("ncmesh")
            this % dftran % ncmesh = var
        case("naxn")
            this % dftran % naxn = var
        case("IndexFC2Print")
            this % dftran % IndexFC2Print = var
        case("irefine")
            this % dftran % irefine = var
        case("ProtectiveOxide")
            this % dftran % ProtectiveOxide = var
        case("nIDoxide")
            this % dftran % nIDoxide = var
        case("nce")
            this % dftran % nce = var
        case("IndexGrainBndSep")
            this % dftran % IndexGrainBndSep = var
        case("grass")
            this % dftran % grass = var
        case("ncolbp")
            this % dftran % ncolbp = var
        case("presfgr")
            this % dftran % presfgr = var
        case("inp")
            this % dftran % inp = var
        case("naz")
            this % dftran % naz = var
        case("jchf")
            this % dftran % jchf = var
        case("profile")
            this % dftran % profile = var
        case("nsym")
            this % dftran % nsym = var
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_i4_0


    subroutine frod_set_i4_1(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        integer(4)   :: var(:)

        select case (key)
        case("tem")
            this % dftran % tem(:) = var(:)
        case("ngastmp")
            this % dftran % ngastmp(:) = var(:)
        case("htco")
            this % dftran % htco(:) = var(:)
        case("ncs")
            this % dftran % ncs(:) = var(:)
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_i4_1


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
        case("splbp")
            this % dftran % splbp = var
        case("tpowf")
            this % dftran % tpowf = var
        case("ruptstrain")
            this % dftran % ruptstrain = var
        case("frcoef")
            this % dftran % frcoef = var
        case("epsht1")
            this % dftran % epsht1 = var
        case("CladPower")
            this % dftran % CladPower = var
        case("pitch")
            this % dftran % pitch = var
        case("bowthr")
            this % dftran % bowthr = var
        case("dofang")
            this % dftran % dofang = var
        case("coldbp")
            this % dftran % coldbp = var
        case("frden")
            this % dftran % frden = var
        case("RodDiameter")
            this % dftran % RodDiameter = var
        case("refdtm")
            this % dftran % refdtm = var
        case("totnb")
            this % dftran % totnb = var
        case("powop")
            this % dftran % powop = var
        case("flxsec")
            this % dftran % flxsec = var
        case("ffch")
            this % dftran % ffch = var
        case("fpdcay")
            this % dftran % fpdcay = var
        case("roughc")
            this % dftran % roughc = var
        case("roughf")
            this % dftran % roughf = var
        case("prsacc")
            this % dftran % prsacc = var
        case("fpowr")
            this % dftran % fpowr = var
        case("tref")
            this % dftran % tref = var
        case("pelh")
            this % dftran % pelh = var
        case("pdrato")
            this % dftran % pdrato = var
        case("tgas0")
            this % dftran % tgas0 = var
        case("tsntrk")
            this % dftran % tsntrk = var
        case("spdbp")
            this % dftran % spdbp = var
        case("achn")
            this % dftran % achn = var
        case("tflux")
            this % dftran % tflux = var
        case("RodLength")
            this % dftran % RodLength = var
        case("OpenPorosityFraction")
            this % dftran % OpenPorosityFraction = var
        case("zad")
            this % dftran % zad = var
        case("rshrd")
            this % dftran % rshrd = var
        case("doffst")
            this % dftran % doffst = var
        case("emptm")
            this % dftran % emptm = var
        case("trise")
            this % dftran % trise = var
        case("fltgap2")
            this % dftran % fltgap2 = var
        case("hydiam")
            this % dftran % hydiam = var
        case("dishd")
            this % dftran % dishd = var
        case("ph")
            this % dftran % ph = var
        case("hrad")
            this % dftran % hrad = var
        case("dtss")
            this % dftran % dtss = var
        case("bup")
            this % dftran % bup = var
        case("cldwdc")
            this % dftran % cldwdc = var
        case("timop")
            this % dftran % timop = var
        case("cfluxa")
            this % dftran % cfluxa = var
        case("rvoid")
            this % dftran % rvoid = var
        case("dofset")
            this % dftran % dofset = var
        case("pl")
            this % dftran % pl = var
        case("fltgap")
            this % dftran % fltgap = var
        case("frpo2")
            this % dftran % frpo2 = var
        case("trest")
            this % dftran % trest = var
        case("fgrns")
            this % dftran % fgrns = var
        case("refine")
            this % dftran % refine = var
        case("modheat")
            this % dftran % modheat = var
        case("tmpac1")
            this % dftran % tmpac1 = var
        case("coldw")
            this % dftran % coldw = var
        case("dhe")
            this % dftran % dhe = var
        case("explenumv")
            this % dftran % explenumv = var
        case("dhy")
            this % dftran % dhy = var
        case("volbp")
            this % dftran % volbp = var
        case("rshd")
            this % dftran % rshd = var
        case("fotmtl")
            this % dftran % fotmtl = var
        case("gsms")
            this % dftran % gsms = var
        case("dishv0")
            this % dftran % dishv0 = var
        case("rnbnt")
            this % dftran % rnbnt = var
        case("zvoid2")
            this % dftran % zvoid2 = var
        case("gapthk")
            this % dftran % gapthk = var
        case("zvoid1")
            this % dftran % zvoid1 = var
        case("zs")
            this % dftran % zs = var
        case("FuelPelDiam")
            this % dftran % FuelPelDiam = var
        case("dtmaxa")
            this % dftran % dtmaxa(it) = var
        case("hbh")
            this % dftran % hbh(it) = var
        case("hupta")
            this % dftran % hupta(it) = var
        case("hinta")
            this % dftran % hinta(it) = var
        case("gbh")
            this % dftran % gbh(it) = var
        case("explenumt")
            this % dftran % explenumt(it) = var
        case("pbh2")
            this % dftran % pbh2(it) = var
        case("dtpoa")
            this % dftran % dtpoa(it) = var
        case("RodAvePower")
            this % dftran % RodAvePower(it) = var
        case("dtplta")
            this % dftran % dtplta(it) = var
        case("FuelGasSwell")
            this % dftran % FuelGasSwell(it) = var
        case("temptm")
            this % dftran % temptm(it) = var
        case("relfraca")
            this % dftran % relfraca(it) = var
        case("prestm")
            this % dftran % prestm(it) = var
        case("fldrat")
            this % dftran % fldrat(it) = var
        case("gasphs")
            this % dftran % gasphs(it) = var
        case("ProfileStartTime")
            this % dftran % ProfileStartTime(it) = var
        case("pbh1")
            this % dftran % pbh1(it) = var
        case("hlqcl")
            this % dftran % hlqcl(it) = var
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

        ! Must be removed in the future ---------------------------------------------------------
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
        ! ----------------------------------------------------------------------------------------

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
        case("scd")
            this % dftran % scd(:) = var(:)
        case("azpang")
            this % dftran % azpang(:) = var(:)
        case("fmesh")
            this % dftran % fmesh(:) = var(:)
        case("htclev")
            this % dftran % htclev(:) = var(:)
        case("ExtentOfBow")
            this % dftran % ExtentOfBow(:) = var(:)
        case("vplen")
            this % dftran % vplen(:) = var(:)
        case("gadoln")
            this % dftran % gadoln(:) = var(:)
        case("gfrac")
            this % dftran % gfrac(:) = var(:)
        case("gbse")
            this % dftran % gbse(:) = var(:)
        case("fluxz")
            this % dftran % fluxz(:) = var(:)
        case("nodchf")
            this % dftran % nodchf(:) = var(:)
        case("swd")
            this % dftran % swd(:) = var(:)
        case("oxideod")
            this % dftran % oxideod(:) = var(:)
        case("cexh2a")
            this % dftran % cexh2a(:) = var(:)
        case("radpel")
            this % dftran % radpel(:) = var(:)
        case("cmesh")
            this % dftran % cmesh(:) = var(:)
        case("gappr0")
            this % dftran % gappr0(:) = var(:)
        case("butemp")
            this % dftran % butemp(:) = var(:)
        case("oxideid")
            this % dftran % oxideid(:) = var(:)
        case("spl")
            this % dftran % spl(:) = var(:)
        case("eppinp")
            this % dftran % eppinp(:) = var(:)
        case("techf")
            this % dftran % techf(:) = var(:)
        case("tschf")
            this % dftran % tschf(:) = var(:)
        case("zelev")
            this % dftran % zelev(:) = var(:)
        case("htca")
            this % dftran % htca(it,:) = var(:)
        case("tblka")
            this % dftran % tblka(it,:) = var(:)
        case("gasths")
            this % dftran % gasths(it,:) = var(:)
        case("radtemp")
            this % dftran % radtemp(:,it) = var(:)
        case("fuelrad")
            this % dftran % fuelrad(:,it) = var(:)
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_r8_1


    subroutine frod_set_r8_2(this, key, var)

        class (frod_type), intent(inout) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:,:)

        select case (key)
        case("pazp")
            this % dftran % pazp(:,:) = var(:,:)
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set_r8_2


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

        !call this % dfcon  % restfs()
        !call this % dftran % restfs()

    end subroutine p_frapc2t

end module frapi