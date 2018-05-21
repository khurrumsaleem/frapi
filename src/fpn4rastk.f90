module fpn4rastk

    use conversions
    use frapcon41, only : frapcon_type

    implicit none

    type(frapcon_type) :: fpn

    ! TEMPORARY VARIABLES
    integer :: i, n, m
    real(8) :: a, b, c
    real(8), allocatable :: area(:)
    real(8), allocatable :: weight(:)
    real(8), allocatable :: tmp0(:), tmp1(:), tmp2(:)

contains

    subroutine init(nr, na, x, radfuel, radgap, radclad, pitch, den, enrch, p2, tw, go, qmpy)

        integer :: na          ! number of nodes in the axial mesh
        integer :: nr          ! number of radial nodes in a pellet
        integer :: ngasr = 45  ! number of radial gas release nodes
        integer :: nce = 5     ! number of radial elements in the cladding for the FEA model

        integer :: mechan  = 2 ! cladding mechanical model (1: FEA, 2: FRACAS-I)
        integer :: ngasmod = 2 ! fission gas release model (1 = ANS5.4, 2 = Massih(Default), 3 = FRAPFGR, 4 = ANS5.4_2011)

        real(8) :: x(:)        ! Elevation in each qf, x array defining a power shape
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

        call fpn % init(na, ngasr, nr, nce, mechan, ngasmod)

        fpn % cpl     = 6.3386d0 * cmtoin            ! Cold plenum length, in
        fpn % crdt    = 0.d0 * cmtoin                ! Crud thickness, in
        fpn % thkcld  = (radclad - radgap) * cmtoin  ! Thickness of cladding, in
        fpn % thkgap  = (radgap - radfuel) * cmtoin  ! Thickness of gap, in
        fpn % dco     = 2 * radclad * cmtoin         ! Outer cladding diameter, in
        fpn % pitch   = pitch * cmtoin               ! Center to center rod distance, in
        fpn % rc      = 0.d0 * cmtoin                ! Radius of the fuel pellet central annulus, in
        fpn % fotmtl  = 2.d0                         ! Fuel oxygen-to-metal ratio
        fpn % dishsd  = 0.064d0                      ! Dish shoulder width, in
        fpn % den     = den * 10.40d0/10.96d0        ! As-fabricated apparent fuel density, %TD
        fpn % dspg    = 0.3                          ! Spring diameter, in
        fpn % fa      = 1.d0                         ! Peak-to-average power ratio
        fpn % dspgw   = 0.0394                       ! Spring wire diameter, in
        fpn % enrch   = enrch                        ! Fuel U-235 enrichment (atom per total heavy metal atoms)
        fpn % fgpav   = 340.84                       ! Fill gas pressure, psi
        fpn % hdish   = 0.0094d0                     ! Dish height, in
        fpn % hplt    = 0.387d0                      ! Pellet height, in
        fpn % icm     = 4                            ! Cladding type, 4: Zircaloy-4
        fpn % idxgas  = 1                            ! Fill gas type (1 = He, 2 = Air, 3 = N2, 4 = FG, 5 = Ar, 6 = User-Specified)
        fpn % iplant  =-2                            ! Plant type, -2: PWR, -3: BWR, -4: HBWR
        fpn % imox    = 0                            ! Fuel type, 0: UO_2
        fpn % totl    = x(size(x)) * cmtoft          ! Total length of active fuel, ft
        fpn % roughc  = 1.97d-5                      ! Clad roughness, in
        fpn % roughf  = 7.87d-5                      ! Fuel roughness, in
        fpn % vs      = 10.d0                        ! Number of spring turns
        fpn % rsntr   = 100.d0                       ! Expected resintering density increase, kg/m**3
        fpn % nsp     = 0                            ! Specify which type of coolant conditions to use (0 = constant, 1 = time-dependent)
        fpn % slim    = 0.05d0                       ! User supplied swelling limit (vol fraction) (Default = 0.05)
        fpn % enrpu39 = 0.d0                         ! Fuel pellet Pu-239 content
        fpn % enrpu40 = 0.d0                         ! Fuel pellet Pu-240 content
        fpn % enrpu41 = 0.d0                         ! Fuel pellet Pu-241 content
        fpn % enrpu42 = 0.d0                         ! Fuel pellet Pu-242 content
        fpn % x(:)    = x(:) * cmtoft                ! Elevation in each qf, x array defining a power shape, ft
        fpn % tw(1)   = tcf(tw)                      ! Coolant inlet temperature, F
        fpn % p2(1)   = p2 * MPatoPSI                ! Coolant System Pressure, Psi
        fpn % go(1)   = go * ksm2tolbhrft2           ! Coolant mass flux around fuel rod, lb/hr * ft^2
        fpn % qf(:)   = 1.                           ! Ratio of linear power
        fpn % qmpy    = 1.D-3 * qmpy / mtoft         ! The linear heat generation rate, kW/ft
        fpn % gadoln(:) = 0.D                        ! Weight fraction of gadolinia in the fuel

        call fpn % make() ! set default and check input variables
        call fpn % stp0() ! make the very first time step

        ! ALLOCATION OF THE TEMPORARY ARRAYS

        n = na   ! number of axial segments
        m = nr-1 ! number of radial segments

        allocate(area(n))
        allocate(weight(m))
        allocate(tmp0(m))
        allocate(tmp1(n))
        allocate(tmp2(m+1))

    end subroutine init

    subroutine next(dt)

       real(8) :: dt

       call fpn % next(dt)

    end subroutine next

    subroutine set(key, var)

        character(*) :: key
        real(8)      :: var(:)

        select case(key)
        case("linear power")
            fpn % qf(:) = var(:) / sum(var) * size(var)
            fpn % qmpy  = sum(var) / mtoft
        case("coolant temperature")
            fpn % tcoolant(:) = (/( tcf(var(i)), i = 1, size(var) )/)
        case("coolant pressure")
            fpn % pcoolant(:) = var(:) * MPatoPSI
        case("coolant mass flux")
            fpn % go(1) = var(1) * ksm2tolbhrft2
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine set

    subroutine get(key, var)

        character(*) :: key

        real(8) :: var(:) ! array (n-1,)

        select case(key)
        case('axial fuel temperature')
            area(:) = (/(  fpn % hrad(1,i)**2 - fpn % hrad(m+1,i)**2, i = 1, n )/)
            do i = 1, n
                weight(:) = ( fpn % hrad(1:m,i) - fpn % hrad(2:m+1,i) ) / area(i)
                tmp2(:) = 0.5 * ( fpn % tmpfuel(1:m+1,i) + fpn % tmpfuel(1:m+1,i+1) )
                tmp0(:) = 0.5 * ( tmp2(1:m) + tmp2(2:m+1) )
                tmp1(i) = sum(tmp0(:) * weight(:))
            enddo
            var(:) = (/( tfc(tmp1(i)), i = 1, n )/)
        case('bulk coolant temperature')
            var(:) = 0.5 * ( fpn % BulkCoolantTemp(1:n) + fpn % BulkCoolantTemp(2:n+1) )
            var(:) = (/( tfc(var(i)), i = 1, n )/)
        case('gap conductance')
            var(:) = fpn % GapCond(1:n) * Bhft2FtoWm2K
        case('oxide thickness')
            var(:) = fpn % EOSZrO2Thk(1:n) * intomm
        case('thermal gap thickness')
            var(:) = fpn % gapplot(1:n) * miltomm
        case('mechanical gap thickness')
            var(:) = fpn % FuelCladGap(1:n) * 1.D+3 * miltomm
        case('gap pressure')
            var(:) = fpn % GapPress(1:n) * PSItoMPa
        case('cladding hoop strain')
            var(:) = fpn % eps(1:n,1) * PSItoMPa
        case('cladding hoop stress')
            var(:) = fpn % sig(1:n,1) * PSItoMPa
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine get

end module fpn4rastk



