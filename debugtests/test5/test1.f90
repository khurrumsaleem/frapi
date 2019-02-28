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

    integer, parameter :: na = 5           ! number of axial mesh nodes
    integer, parameter :: nr = 5           ! number of radial mesh nodes in pellet
    integer, parameter :: nc = 3           ! number of radial mesh nodes in cladding
    integer, parameter :: ng = 45          ! number of radial mesh nodes in pellet for gas release model
    integer, parameter :: nt = 10          ! number of time steps

    real(8), parameter :: dt = 1.D-3       ! time step, sec
    real(8), parameter :: tcool = 306.D0   ! inlet coolant temperature, C
    real(8), parameter :: pcool = 16.11D0  ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 5.D+4    ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: power = 2.D+2    ! linear power, W/cm
    real(8), parameter :: dz = 365.D0 / na ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod             ! fuel rod object

    real(8) :: time = 0, w = 0
    real(8) :: a(na), b(na), c(na), d(na)
    integer :: q(na)  ! temporary variable
    integer :: i_rod, i = 0

    write(*,'(A10,8A12)') '','Time (s)', 'P (W/cm)', 'Tcb (C)', 'Tfs (C)', 'Tfc (C)', 'Dc (kg/cm3)', 'Mode'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPCON Calculation                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 1, &
                        ifixedcoolp = 1, &
                        ifixedtsurf = 0, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='frapcon')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')
    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W/cm", power)
    call fuelrod % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    a = pcool
    call fuelrod % set_r8_1("coolant pressure, mpa", a)
    a = tcool
    call fuelrod % set_r8_1("coolant temperature, c", a)

    call fuelrod % init()

    call fuelrod % next(1.D0)

    call fuelrod % makerf()

    call fuelrod % get_r8_1('bulk coolant temperature, c', a)
    call fuelrod % get_r8_1('pellet surface temperature, c', b)
    call fuelrod % get_r8_1('pellet centerline temperature, c', c)
    call fuelrod % get_r8_1('coolant density, kg/m^3', d)
!    call fuelrod % get_r8_1('coolant quality', q)
    write(*,'(I10,7F12.3)') i, time, power, maxval(a), maxval(b), maxval(c), maxval(d)

    call fuelrod % destroy ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPTRAN Calculation                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 1, &
                        ifixedcoolp = 1, &
                        ifixedtsurf = 0, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='fraptran')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')

    call fuelrod % set_ch_0('bheat', 'on')
    call fuelrod % set_ch_0('coolant', 'off')

    w = power

    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W/cm", w)
    call fuelrod % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    a = pcool
    call fuelrod % set_r8_1("coolant pressure, mpa", a)
    a = tcool
    call fuelrod % set_r8_1("coolant temperature, c", a)

    call fuelrod % init()

    time = 0.d0

    call fuelrod % get_r8_1('bulk coolant temperature, c', a)
    call fuelrod % get_r8_1('pellet surface temperature, c', b)
    call fuelrod % get_r8_1('pellet centerline temperature, c', c)
    call fuelrod % get_r8_1('coolant density, kg/m^3', d)
    call fuelrod % get_i4_1('heat transfer mode', q)
    write(*,'(I10,6F12.3,5I3)') i, time, power, maxval(a), maxval(b), maxval(c), maxval(d), q(:)

    do i = 1, nt
        time = time + dt
!        w = power * ( 1.d0 + (i-1) * 2.d0 / nt)
        w = 2 * power
        call fuelrod % set_r8_0("linear power, W/cm", w )
        call fuelrod % next (dt)
        call fuelrod % get_r8_1('bulk coolant temperature, C', a)
        call fuelrod % get_r8_1('pellet surface temperature, c', b)
        call fuelrod % get_r8_1('pellet centerline temperature, c', c)
        call fuelrod % get_r8_1('coolant density, kg/m^3', d)
        call fuelrod % get_i4_1('heat transfer mode', q)
        write(*,'(I10,6F12.3,5I3)') i, time, w, maxval(a), maxval(b), maxval(c), maxval(d), q(:)
    enddo

    call fuelrod % destroy ()

    write(*,*) 'Program complete!'

end program test2
