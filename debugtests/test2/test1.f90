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

    real(8), parameter :: dt = 1.D0        ! time step, days
    real(8), parameter :: tcool = 306.D0   ! inlet coolant temperature, C
    real(8), parameter :: pcool = 15.11D0  ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 5.D+3   ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: power = 2.D+2    ! linear power, W/cm
    real(8), parameter :: dz = 365.D0 / na ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod             ! fuel rod object

    real(8) :: a(na), b(na), c(na)         ! temporary variable
    integer :: i_rod, i = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPCON Calculation                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, &
                        ifixedcoolp = 0, &
                        ifixedtsurf = 0, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='frapcon')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')
    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W|cm", power)
    call fuelrod % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod % set_r8_0("coolant mass flux, kg|(s*m^2)", fcool)

    call fuelrod % init()

    call fuelrod % next(1.D0)

    call fuelrod % makerf()

    call fuelrod % get_r8_1('bulk coolant temperature, C', a)
    call fuelrod % get_r8_1('pellet surface temperature, c', b)
    call fuelrod % get_r8_1('pellet centerline temperature, c', c)
    write(*,*) i, maxval(a), maxval(b), maxval(c)

    call fuelrod % destroy ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPTRAN Calculation                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, &
                        ifixedcoolp = 0, &
                        ifixedtsurf = 0, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='fraptran')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')

    call fuelrod % set_ch_0('coolant', 'on')

    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W|cm", power)
    call fuelrod % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod % set_r8_0("coolant mass flux, kg|(s*m^2)", fcool)

    call fuelrod % init()

    do i = 1, 10
        call fuelrod % set_r8_0("linear power, W|cm", power * (1 + 0*i) )
        call fuelrod % next (1.D-3)
        call fuelrod % get_r8_1('bulk coolant temperature, C', a)
        call fuelrod % get_r8_1('pellet surface temperature, c', b)
        call fuelrod % get_r8_1('pellet centerline temperature, c', c)
        write(*,*) i, maxval(a), maxval(b), maxval(c)
    enddo

    call fuelrod % destroy ()

    write(*,*) 'Program complete!'

end program test2
