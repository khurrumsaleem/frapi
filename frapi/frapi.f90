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

        include "fp_options_h.f90"

        integer :: nr_ = 17
        integer :: na_ = 10
        integer :: ngasr_ = 45
        integer :: nce_ = 5
        logical :: verbose_ = .false. 
        character(len=20) :: frapmode_ = 'frapcon'

        n  = na
        m  = nr

        if( present(nr      ) ) nr_      = nr
        if( present(na      ) ) na_      = na
        if( present(ngasr   ) ) ngasr_   = ngasr
        if( present(nce     ) ) nce_     = nce
        if( present(verbose ) ) verbose_ = verbose
        if( present(frapmode) ) frapmode_= frapmode

        select case (frapmode_)
        case ('frapcon')

            call this % dfcon % make(na_, ngasr_, nr_+1, nce_, verbose_)

            call this % dfcon % deft()

            if( present(mechan      ) ) this % dfcon  % mechan      = mechan
            if( present(ngasmod     ) ) this % dfcon  % ngasmod     = ngasmod
            if( present(icm         ) ) this % dfcon  % icm         = icm
            if( present(icor        ) ) this % dfcon  % icor        = icor
            if( present(iplant      ) ) this % dfcon  % iplant      = iplant
            if( present(imox        ) ) this % dfcon  % imox        = imox
            if( present(igascal     ) ) this % dfcon  % igascal     = igascal
            if( present(zr2vintage  ) ) this % dfcon  % zr2vintage  = zr2vintage
            if( present(moxtype     ) ) this % dfcon  % moxtype     = moxtype
            if( present(idxgas      ) ) this % dfcon  % idxgas      = idxgas
            if( present(iq          ) ) this % dfcon  % iq          = iq
            if( present(ivardm      ) ) this % dfcon  % ivardm      = ivardm
            if( present(ifixedcoolt ) ) this % dfcon  % ifixedcoolt = ifixedcoolt
            if( present(ifixedcoolp ) ) this % dfcon  % ifixedcoolp = ifixedcoolp
            if( present(ifixedtsurf ) ) this % dfcon  % ifixedtsurf = ifixedtsurf

            call this % dfcon % dump()

        case ('fraptran')

            call this % dftran % make(na_, nr_, nce_, verbose_)

            call this % dftran % deft()

            if( present(coolant     ) ) this % dftran % coolant     = coolant
            if( present(bheat       ) ) this % dftran % bheat       = bheat
            if( present(mheat       ) ) this % dftran % mheat       = mheat
            if( present(reflood     ) ) this % dftran % reflood     = reflood
            if( present(internal    ) ) this % dftran % internal    = internal
            if( present(metal       ) ) this % dftran % metal       = metal
            if( present(deformation ) ) this % dftran % deformation = deformation
            if( present(inst        ) ) this % dftran % inst        = inst
            if( present(geomet      ) ) this % dftran % geomet      = geomet
            if( present(nvol1       ) ) this % dftran % nvol1       = nvol1
            if( present(lowpl       ) ) this % dftran % lowpl       = lowpl
            if( present(pressu      ) ) this % dftran % pressu      = pressu
            if( present(massfl      ) ) this % dftran % massfl      = massfl
            if( present(coreav      ) ) this % dftran % coreav      = coreav
            if( present(chf         ) ) this % dftran % chf         = chf
            if( present(filmbo      ) ) this % dftran % filmbo      = filmbo
            if( present(coldwa      ) ) this % dftran % coldwa      = coldwa
            if( present(axpow       ) ) this % dftran % axpow       = axpow
            if( present(bowing      ) ) this % dftran % bowing      = bowing
            if( present(spefbz      ) ) this % dftran % spefbz      = spefbz
            if( present(geometry    ) ) this % dftran % geometry    = geometry
            if( present(nbundl      ) ) this % dftran % nbundl      = nbundl
            if( present(refloodtime ) ) this % dftran % refloodtime = refloodtime
            if( present(radiat      ) ) this % dftran % radiat      = radiat
            if( present(ruptur      ) ) this % dftran % ruptur      = ruptur
            if( present(liquid      ) ) this % dftran % liquid      = liquid
            if( present(inlet       ) ) this % dftran % inlet       = inlet
            if( present(reflo       ) ) this % dftran % reflo       = reflo
            if( present(pressure    ) ) this % dftran % pressure    = pressure
            if( present(collaps     ) ) this % dftran % collaps     = collaps
            if( present(frapt4      ) ) this % dftran % frapt4      = frapt4
            if( present(geom        ) ) this % dftran % geom        = geom
            if( present(temp        ) ) this % dftran % temp        = temp
            if( present(tape2       ) ) this % dftran % tape2       = tape2
            if( present(nvol2       ) ) this % dftran % nvol2       = nvol2
            if( present(press       ) ) this % dftran % press       = press
            if( present(zone        ) ) this % dftran % zone        = zone
            if( present(upppl       ) ) this % dftran % upppl       = upppl
            if( present(jfb         ) ) this % dftran % jfb         = jfb
            if( present(nucbo       ) ) this % dftran % nucbo       = nucbo
            if( present(unitin      ) ) this % dftran % unitin      = unitin
            if( present(unitout     ) ) this % dftran % unitout     = unitout
            if( present(res         ) ) this % dftran % res         = res
            if( present(pow         ) ) this % dftran % pow         = pow
            if( present(gasflo      ) ) this % dftran % gasflo      = gasflo
            if( present(idoxid      ) ) this % dftran % idoxid      = idoxid
            if( present(cathca      ) ) this % dftran % cathca      = cathca
            if( present(baker       ) ) this % dftran % baker       = baker
            if( present(noball      ) ) this % dftran % noball      = noball
            if( present(cenvoi      ) ) this % dftran % cenvoi      = cenvoi
            if( present(soltyp      ) ) this % dftran % soltyp      = soltyp
            if( present(relocmodel  ) ) this % dftran % relocmodel  = relocmodel

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

end module frapi