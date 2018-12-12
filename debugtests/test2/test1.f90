program test1
!   Fuel rod length 1 m with the constant linear power 200 W/cm
!
!   15.4MPa, 290C, 1000 kg/m2s => | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
!
    use frapi, only : t_fuelrod

    implicit none

    integer, parameter :: na = 10   ! number of axial mesh nodes
    integer, parameter :: nr = 20   ! number of radial mesh nodes in pellet
    integer, parameter :: nc = 5    ! number of radial mesh nodes in cladding
    integer, parameter :: ng = 45   ! number of radial mesh nodes in pellet for gas release model

    real(8), parameter :: dt = 1.D0      ! time step, days
    real(8), parameter :: tcool = 290.D0 ! inlet coolant temperature, C
    real(8), parameter :: pcool = 15.4D0 ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 1.D+3   ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: power = 2.D+2  ! linear power, W/cm
    real(8), parameter :: dz = 10.D0     ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod

    real(8) :: a(na) ! temporary variable
    integer :: i_rod, i

!!!!!!!!!!!!!!!!!! Fuel rod for 'frapcon' calculation !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, ifixedcoolp = 0, ifixedtsurf = 0, &
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
    write(*,*) a

    call fuelrod % destroy ()

!!!!!!!!!!!!!!!!!! Fuel rod for 'fraptran' calculation !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, ifixedcoolp = 0, ifixedtsurf = 0, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='fraptran')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')

    call fuelrod % set_ch_0('mheat',      'off')            ! central void in the fuel pellets
    call fuelrod % set_ch_0('bheat',      'off')            ! option to use cladding surface temperature as input
    call fuelrod % set_ch_0('reflood',    'off')            ! core reflood after loss of coolant
    call fuelrod % set_ch_0('radiation',  'off')            ! model a rod within a flow shroud
    call fuelrod % set_ch_0('coolant',    'on')            ! option to model the coolant
!    call fuelrod % set_ch_0('internal',   'off' )
!    call fuelrod % set_ch_0('deformation','off' )
!    call fuelrod % set_ch_0('metal',   'off')

    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W|cm", power)
!    call fuelrod % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod % set_r8_0("coolant mass flux, kg|(s*m^2)", fcool*100)
    call fuelrod % set_r8_0("inlet coolant enthalpy, j|kg", 560.d0)

    call fuelrod % init()

    call fuelrod % next (1.D-1)

    call fuelrod % get_r8_1('bulk coolant temperature, C', a)

    write(*,*) a

    write(*,*) 'Test done!!'

end program test1