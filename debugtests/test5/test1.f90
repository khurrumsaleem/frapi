program test2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                           !
!   Fuel rod length 3.65 m with the constant linear power 200 W/cm          !
!                                                                           !
!   15.4MPa, 290C, 5000 kg/m2s => | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | !
!                                                                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use frapi, only : t_fuelrod            ! 'fuelrod' class from 'frapi'

    implicit none

    integer, parameter :: na = 8           ! number of axial mesh nodes
    integer, parameter :: nr = 5           ! number of radial mesh nodes in pellet
    integer, parameter :: nc = 3           ! number of radial mesh nodes in cladding
    integer, parameter :: ng = 45          ! number of radial mesh nodes in pellet for gas release model
    integer, parameter :: nt = 1           ! number of time steps

    real(8), parameter :: dt = 1.D-3       ! time step, sec
    real(8), parameter :: tcool = 296.D0   ! inlet coolant temperature, C
    real(8), parameter :: tclad = 310.D0   ! cladding surface temperature, C
    real(8), parameter :: pcool = 15.5D0   ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 3101.d0    ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: dz = 365.D0 / na ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod             ! fuel rod object

    real(8) :: time = 0
    real(8) :: a(na), b(na), c(na), d(na), w(na), filmhtc(na)
    real(8) :: ghtc(na), rhtc(na), thtc(na), shtc(na)
    !real(8), parameter :: power(na) = (/ 18.312220071541269, 35.560080297862605, &
    !                                 41.810513075780570, 41.658526517375705, &
    !                                 36.192058600986449, 23.705567561053176, &
    !                                 8.5339054256230220, 1.7174878679687262 /)
    integer :: q(na)  ! temporary variable
    integer :: i_rod, i = 0

    real(8) :: power(na)

    filmhtc = 3000.d0
    power = 100.d0

    write(*,'(A10,9A12)') '','Time (s)', 'P (W)', 'Tcb (C)', 'Tfs (C)', 'Tfc (C)', 'Dc (kg/cm3)', 'HTC'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPCON Calculation                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, &
                        ifixedcoolp = 0, &
                        ifixedtsurf = 1, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='frapcon')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')
    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_1("linear power, W/cm", power)
!    call fuelrod % set_r8_0("inlet coolant temperature, C", tclad)
!    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
!    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    a = tclad
    call fuelrod % set_r8_1("cladding outer surface temperature, c", a)

    call fuelrod % init()

    do i = 1, 10

    call fuelrod % next(1.d0)

    call fuelrod % get_r8_1('bulk coolant temperature, c', a)
    call fuelrod % get_r8_1('pellet surface temperature, c', b)
    call fuelrod % get_r8_1('pellet centerline temperature, c', c)
    call fuelrod % get_r8_1('coolant density, kg/m^3', d)
    call fuelrod % get_r8_1('gap total heat transfer coefficient, w/(k*m^2)', thtc)
    call fuelrod % get_r8_1('gap solid heat transfer coefficient, w/(k*m^2)', shtc)
    call fuelrod % get_r8_1('gap gas heat transfer coefficient, w/(k*m^2)', ghtc)
    call fuelrod % get_r8_1('gap radiation heat transfer coefficient, w/(k*m^2)', rhtc)

!    call fuelrod % get_r8_1('coolant quality', q)
    write(*,'(I10,11F12.3)') i, time, sum(power*dz), maxval(a), maxval(b), maxval(c), maxval(d), maxval(thtc), maxval(shtc), maxval(ghtc), maxval(rhtc)

    enddo

    call fuelrod % makerf()

    call fuelrod % destroy ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPTRAN Calculation                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    w = power

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, &
                        ifixedcoolp = 0, &
                        ifixedtsurf = 1, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='fraptran')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')
!    call fuelrod % set_ch_0('restart file', './rest-0879.txt')

    call fuelrod % set_ch_0('bheat', 'on')
    call fuelrod % set_ch_0('coolant', 'off')

    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_1("linear power, W/cm", w)
!    call fuelrod % set_r8_0("inlet coolant temperature, C", tclad)
!    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
!    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    a = tclad
    call fuelrod % set_r8_1("cladding outer surface temperature, c", a)

    call fuelrod % init()

    time = 0.d0
    i = 0

    call fuelrod % get_r8_1('bulk coolant temperature, c', a)
    call fuelrod % get_r8_1('pellet surface temperature, c', b)
    call fuelrod % get_r8_1('pellet centerline temperature, c', c)
    call fuelrod % get_r8_1('coolant density, kg/m^3', d)
    call fuelrod % get_i4_1('heat transfer mode', q)
    call fuelrod % get_r8_1('gap total heat transfer coefficient, w/(k*m^2)', thtc)
    call fuelrod % get_r8_1('gap solid heat transfer coefficient, w/(k*m^2)', shtc)
    call fuelrod % get_r8_1('gap gas heat transfer coefficient, w/(k*m^2)', ghtc)
    call fuelrod % get_r8_1('gap radiation heat transfer coefficient, w/(k*m^2)', rhtc)
    write(*,'(I10,11F12.3)') i, time, sum(w*dz), maxval(a), maxval(b), maxval(c), maxval(d), maxval(thtc), maxval(shtc), maxval(ghtc), maxval(rhtc)

    do i = 1, nt
        time = time + dt
!        w = power * ( 1.d0 + (i-1) * 2.d0 / nt)
        w = 2 * power
        call fuelrod % set_r8_1("linear power, W/cm", w )
        call fuelrod % next (dt)
        call fuelrod % get_r8_1('bulk coolant temperature, C', a)
        call fuelrod % get_r8_1('pellet surface temperature, c', b)
        call fuelrod % get_r8_1('pellet centerline temperature, c', c)
        call fuelrod % get_r8_1('coolant density, kg/m^3', d)
        call fuelrod % get_i4_1('heat transfer mode', q)
        call fuelrod % get_r8_1('gap total heat transfer coefficient, w/(k*m^2)', thtc)
        call fuelrod % get_r8_1('gap solid heat transfer coefficient, w/(k*m^2)', shtc)
        call fuelrod % get_r8_1('gap gas heat transfer coefficient, w/(k*m^2)', ghtc)
        call fuelrod % get_r8_1('gap radiation heat transfer coefficient, w/(k*m^2)', rhtc)
        write(*,'(I10,11F12.3)') i, time, sum(w*dz), maxval(a), maxval(b), maxval(c), maxval(d), maxval(thtc), maxval(shtc), maxval(ghtc), maxval(rhtc)
    enddo

    call fuelrod % destroy ()

    write(*,*) 'Program complete!'

end program test2
