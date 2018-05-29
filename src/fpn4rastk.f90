module fpn4rastk

    use conversions
    use frapcon41, only : driver_type

    implicit none


    type, public :: frod_type
        type(driver_type) :: driver
    contains
        procedure :: make => frod_make
        procedure :: init => frod_init
        procedure :: next => frod_next
        procedure :: set  => frod_set
        procedure :: get  => frod_get
    end type frod_type

    ! TEMPORARY VARIABLES
    integer :: i, j, n, m
    real(8) :: a, b, c, volume
    real(8), allocatable :: weight(:)
    real(8), allocatable :: tmp0(:), tmp1(:), tmp2(:), tmp3(:)

contains

    subroutine frod_make(this, m_, n_, dx, radfuel, radgap, radclad, pitch, den, enrch)

        class (frod_type), intent(inout) :: this

        integer :: n_          ! number of axial segments
        integer :: m_          ! number of radial segments
        integer :: ngasr = 45  ! number of radial gas release nodes
        integer :: nce = 5     ! number of radial elements in the cladding for the FEA model

        integer :: mechan  = 2 ! cladding mechanical model (1: FEA, 2: FRACAS-I)
        integer :: ngasmod = 2 ! fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)

        real(8) :: dx(:)       ! Thickness of the axial nodes, cm
        real(8) :: radfuel
        real(8) :: radgap
        real(8) :: radclad
        real(8) :: pitch
        real(8) :: den
        real(8) :: enrch
        real(8) :: p2
        real(8) :: qmpy
        real(8) :: tw
        real(8) :: go

        n  = n_
        m  = m_

        call this % driver % init(n, ngasr, m+1, nce, mechan, ngasmod)

        this % driver % cpl     = 6.3386d0                     ! Cold plenum length, in
        this % driver % crdt    = 0.d0 * cmtoin                ! Crud thickness, in
        this % driver % thkcld  = (radclad - radgap) * cmtoin  ! Thickness of cladding, in
        this % driver % thkgap  = (radgap - radfuel) * cmtoin  ! Thickness of gap, in
        this % driver % dco     = 2 * radclad * cmtoin         ! Outer cladding diameter, in
        this % driver % pitch   = pitch * cmtoin               ! Center to center rod distance, in
        this % driver % rc      = 0.d0 * cmtoin                ! Radius of the fuel pellet central annulus, in
        this % driver % fotmtl  = 2.d0                         ! Fuel oxygen-to-metal ratio
        this % driver % dishsd  = 0.064d0                      ! Dish shoulder width, in
        this % driver % den     = den * 10.40d0/10.96d0        ! As-fabricated apparent fuel density, %TD
        this % driver % dspg    = 0.3                          ! Spring diameter, in
        this % driver % fa      = 1.d0                         ! Peak-to-average power ratio
        this % driver % dspgw   = 0.0394                       ! Spring wire diameter, in
        this % driver % enrch   = enrch                        ! Fuel U-235 enrichment (atom per total heavy metal atoms)
        this % driver % fgpav   = 340.84                       ! Fill gas pressure, psi
        this % driver % hdish   = 0.0094d0                     ! Dish height, in
        this % driver % hplt    = 0.387d0                      ! Pellet height, in
        this % driver % icm     = 4                            ! Cladding type, 4: Zircaloy-4
        this % driver % idxgas  = 1                            ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
        this % driver % iplant  =-2                            ! Plant type, -2: PWR, -3: BWR, -4: HBWR
        this % driver % imox    = 0                            ! Fuel type, 0: UO_2
        this % driver % totl    = sum(dx) * cmtoft             ! Total length of active fuel, ft
        this % driver % roughc  = 1.97d-5                      ! Clad roughness, in
        this % driver % roughf  = 7.87d-5                      ! Fuel roughness, in
        this % driver % vs      = 30.d0                        ! Number of spring turns
        this % driver % rsntr   = 100.d0                       ! Expected resintering density increase, kg/m**3
        this % driver % nsp     = 0                            ! Specify which type of coolant conditions to use (0 = constant, 1 = time-dependent)
        this % driver % slim    = 0.05d0                       ! User supplied swelling limit (vol fraction) (Default = 0.05)
        this % driver % enrpu39 = 0.d0                         ! Fuel pellet Pu-239 content
        this % driver % enrpu40 = 0.d0                         ! Fuel pellet Pu-240 content
        this % driver % enrpu41 = 0.d0                         ! Fuel pellet Pu-241 content
        this % driver % enrpu42 = 0.d0                         ! Fuel pellet Pu-242 content
        this % driver % tw(1)   = 600.                         ! Coolant inlet temperature, F
        this % driver % p2(1)   = 2000.                        ! Coolant System Pressure, Psi
        this % driver % go(1)   = 2.D+6                        ! Coolant mass flux around fuel rod, lb/hr * ft^2
        this % driver % qf(:)   = 1.                           ! Ratio of linear power
        this % driver % qmpy    = 6.                           ! The linear heat generation rate, kW/ft (after make it turns to rod average heat flux, BTU/(hr*ft^2) )
        this % driver % gadoln(:) = 0.d0                       ! Weight fraction of gadolinia in the fuel

        ! Elevation in each qf, x array defining a power shape, ft
        this % driver % x(1)     = 0.d0
        this % driver % x(2:n+1) = (/( sum(dx(:i)), i = 1, n )/) * cmtoft
        this % driver % deltaz(1:n)= dx(:) * cmtoft
        this % driver % deltaz(n+1)= this % driver % cpl

        call this % driver % make() ! set default and check input variables

        ! ALLOCATION OF THE TEMPORARY ARRAYS
        allocate(weight(m))
        allocate(tmp0(m))
        allocate(tmp1(n))
        allocate(tmp2(m+1))
        allocate(tmp3(n+1))

    end subroutine frod_make

    subroutine frod_init(this)

        class (frod_type), intent(in) :: this

        call this % driver % stp0()  ! make the very first time step

    end subroutine frod_init

    subroutine frod_next(this, dt)

       class (frod_type), intent(in) :: this

       real(8) :: dt

       call this % driver % next(dt)

    end subroutine frod_next

    subroutine frod_set(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key
        integer      :: it
        real(8)      :: var(:)

        it = this % driver % it

        select case(key)
        case("linear power, W/cm")
            call linterp(var, this % driver % deltaz(1:n), tmp3, n)
            a = sum( var(:) * this % driver % deltaz(1:n) ) / this % driver % totl /cmtoft ! W/ft
            b = sum( this % driver % deltaz(1:n) / this % driver % dco(1:n) ) / this % driver % totl / intoft ! 1/ft
            this % driver % qmpy(it) = a * b / pi * WtoBTUh ! BTUh/ft^2
            this % driver % qf(:) = tmp3(:) / sum(tmp3)
        case("coolant temperature, C")
            call linterp(var,  this % driver % deltaz(1:n), tmp3, n)
            this % driver % tw(it) = tcf(tmp3(1))
            this % driver % coolanttemp(it,1:n+1) = (/( tcf(tmp3(i)), i = 1, n+1 )/)
        case("coolant pressure, MPa")
            call linterp(var, this % driver % deltaz(1:n), tmp3, n)
            this % driver % p2(it) = var(1) * MPatoPSI
            this % driver % coolantpressure(it,1:n+1) = tmp3(:) * MPatoPSI
        case("coolant mass flux, kg/(s*m^2)")
            this % driver % go(it) = var(1) * ksm2tolbhrft2
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_set

    subroutine frod_get(this, key, var)

        class (frod_type), intent(in) :: this

        character(*) :: key

        real(8) :: var(:) ! array (n,)

        real(8) :: ra, rb, ya, yb, h, temper, volume

        real(8) :: linteg ! integral of linear function

        select case(key)
        case('axial fuel temperature, C')
            do i = 1, n
                volume = 0
                temper = 0
                do j = 1, m
                    ya = 1.d0
                    yb = 1.d0
                    ra = this % driver % hrad(j+1,i)
                    rb = this % driver % hrad(j,i)
                    h = this % driver % x(i+1) - this % driver % x(i)
                    volume = volume + linteg(ya,yb,ra,rb,h)
                    ya = this % driver % tmpfuel(j+1,i)
                    yb = this % driver % tmpfuel(j,i)
                    temper = temper + linteg(ya,yb,ra,rb,h)
                enddo
                var(i) = tfc(temper / volume)
            enddo
        case('bulk coolant temperature, C')
            var(:) = 0.5d0 * ( this % driver % BulkCoolantTemp(1:n) + this % driver % BulkCoolantTemp(2:n+1) )
            var(:) = (/( tfc(var(i)), i = 1, n )/)
        case('gap conductance, W/(m^2*K)')
            var(:) = this % driver % GapCond(1:n) * Bhft2FtoWm2K
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
        case('cladding hoop stress, MPa')
            var(:) = this % driver % sig(1:n,1) * PSItoMPa
        case('cladding axial stress, MPa')
            var(:) = this % driver % sig(1:n,2) * PSItoMPa
        case('cladding radial stress, MPa')
            var(:) = this % driver % sig(1:n,3) * PSItoMPa
        case('axial mesh, cm')
            var(:) = 0.5d0 * (this % driver % x(1:n) + this % driver % x(2:n+1)) / cmtoft
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine frod_get

end module fpn4rastk



