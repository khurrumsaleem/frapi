module fpn4rastk

    use conversions
    use frapcon41, only : frapcon_type

    implicit none

    type(frapcon_type) :: fpn

    ! TEMPORARY VARIABLES
    integer :: i, j, n, m
    real(8) :: a, b, c, volume
    real(8), allocatable :: weight(:)
    real(8), allocatable :: tmp0(:), tmp1(:), tmp2(:), tmp3(:)

contains

    subroutine init(m_, n_, dx, radfuel, radgap, radclad, pitch, den, enrch)

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

        call fpn % init(n, ngasr, m+1, nce, mechan, ngasmod)

        fpn % cpl     = 6.3386d0                     ! Cold plenum length, in
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
        fpn % totl    = sum(dx) * cmtoft             ! Total length of active fuel, ft
        fpn % roughc  = 1.97d-5                      ! Clad roughness, in
        fpn % roughf  = 7.87d-5                      ! Fuel roughness, in
        fpn % vs      = 30.d0                        ! Number of spring turns
        fpn % rsntr   = 100.d0                       ! Expected resintering density increase, kg/m**3
        fpn % nsp     = 0                            ! Specify which type of coolant conditions to use (0 = constant, 1 = time-dependent)
        fpn % slim    = 0.05d0                       ! User supplied swelling limit (vol fraction) (Default = 0.05)
        fpn % enrpu39 = 0.d0                         ! Fuel pellet Pu-239 content
        fpn % enrpu40 = 0.d0                         ! Fuel pellet Pu-240 content
        fpn % enrpu41 = 0.d0                         ! Fuel pellet Pu-241 content
        fpn % enrpu42 = 0.d0                         ! Fuel pellet Pu-242 content
        fpn % tw(1)   = 600.                         ! Coolant inlet temperature, F
        fpn % p2(1)   = 2000.                        ! Coolant System Pressure, Psi
        fpn % go(1)   = 2.D+6                        ! Coolant mass flux around fuel rod, lb/hr * ft^2
        fpn % qf(:)   = 1.                           ! Ratio of linear power
        fpn % qmpy    = 6.                           ! The linear heat generation rate, kW/ft (after make it turns to rod average heat flux, BTU/(hr*ft^2) )
        fpn % gadoln(:) = 0.d0                       ! Weight fraction of gadolinia in the fuel

        ! Elevation in each qf, x array defining a power shape, ft
        fpn % x(1)     = 0.d0
        fpn % x(2:n+1) = (/( sum(dx(:i)), i = 1, n )/) * cmtoft
        fpn % deltaz(1:n)= dx(:) * cmtoft
        fpn % deltaz(n+1)= fpn % cpl

        call fpn % make() ! set default and check input variables

        ! ALLOCATION OF THE TEMPORARY ARRAYS
        allocate(weight(m))
        allocate(tmp0(m))
        allocate(tmp1(n))
        allocate(tmp2(m+1))
        allocate(tmp3(n+1))

    end subroutine init

    subroutine stp0()

        call fpn % stp0()  ! make the very first time step

    end subroutine stp0

    subroutine next(dt)

       real(8) :: dt

       call fpn % next(dt)

    end subroutine next

    subroutine set(key, var)

        character(*) :: key
        integer      :: it
        real(8)      :: var(:)

        it = fpn % it

        select case(key)
        case("linear power, W/cm")
            call linterp(var, fpn % deltaz(1:n), tmp3, n)
            a = sum( var(:) * fpn % deltaz(1:n) ) / fpn % totl /cmtoft ! W/ft
            b = sum( fpn % deltaz(1:n) / fpn % dco(1:n) ) / fpn % totl / intoft ! 1/ft
            fpn % qmpy(it) = a * b / pi * WtoBTUh ! BTUh/ft^2
            fpn % qf(:) = tmp3(:) / sum(tmp3)
        case("coolant temperature, C")
            call linterp(var,  fpn % deltaz(1:n), tmp3, n)
            fpn % tw(it) = tcf(tmp3(1))
            fpn % coolanttemp(1,1) = tcf(tmp3(1))
            fpn % coolanttemp(1,2:n+2) = (/( tcf(tmp3(i)), i = 1, n )/)
        case("coolant pressure, MPa")
            call linterp(var, fpn % deltaz(1:n), tmp3, n)
            fpn % p2(it) = var(1) * MPatoPSI
            fpn % coolantpressure(1,1) = var(1) * MPatoPSI
            fpn % coolantpressure(1,2:n+2) = tmp3(:) * MPatoPSI
        case("coolant mass flux, kg/(s*m^2)")
            fpn % go(it) = var(1) * ksm2tolbhrft2
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine set

    subroutine get(key, var)

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
                    ra = fpn % hrad(j+1,i)
                    rb = fpn % hrad(j,i)
                    h = fpn % x(i+1) - fpn % x(i)
                    volume = volume + linteg(ya,yb,ra,rb,h)
                    ya = fpn % tmpfuel(j+1,i)
                    yb = fpn % tmpfuel(j,i)
                    temper = temper + linteg(ya,yb,ra,rb,h)
                enddo
                var(i) = tfc(temper / volume)
            enddo
        case('bulk coolant temperature, C')
            var(:) = 0.5 * ( fpn % BulkCoolantTemp(1:n) + fpn % BulkCoolantTemp(2:n+1) )
            var(:) = (/( tfc(var(i)), i = 1, n )/)
        case('gap conductance, W/(m^2*K)')
            var(:) = fpn % GapCond(1:n) * Bhft2FtoWm2K
        case('oxide thickness, um')
            var(:) = fpn % EOSZrO2Thk(1:n) * 12000.d0 * miltoum
        case('thermal gap thickness, um')
            var(:) = fpn % gapplot(1:n) * miltoum
        case('mechanical gap thickness, um')
            var(:) = fpn % FuelCladGap(1:n) * 1.D+3 * miltoum
        case('gap pressure, MPa')
            var(:) = fpn % GapPress(1:n) * PSItoMPa
        case('cladding hoop strain, %')
            var(:) = fpn % eps(1:n,1) * 100
        case('cladding hoop stress, MPa')
            var(:) = fpn % sig(1:n,1) * PSItoMPa
        case('cladding axial stress, MPa')
            var(:) = fpn % sig(1:n,2) * PSItoMPa
        case('cladding radial stress, MPa')
            var(:) = fpn % sig(1:n,3) * PSItoMPa
        case('axial mesh, cm')
            var(:) = 0.5d0 * (fpn % x(1:n) + fpn % x(2:n+1)) / cmtoft
        case default
            write(*,*) 'ERROR: Variable ', key, ' has not been found'
            stop
        end select

    end subroutine get

end module fpn4rastk



