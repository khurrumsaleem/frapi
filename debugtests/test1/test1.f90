program test1

    use frapi, only : t_fuelrod

    implicit none

    integer, parameter :: n_rod = 2 ! number of fuel rods
    integer, parameter :: na = 10   ! number of axial mesh nodes
    integer, parameter :: nr = 20   ! number of radial mesh nodes in pellet
    integer, parameter :: nc = 5    ! number of radial mesh nodes in cladding

    real(8), parameter :: dt = 1.D-10      ! time step, days
    real(8), parameter :: tcool = 290.D0 ! inlet coolant temperature, C
    real(8), parameter :: pcool = 15.4D0 ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 0.D+3   ! coolant mass flux
    real(8), parameter :: power = 0.D+2  ! linear power, W/cm
    real(8), parameter :: dz = 10.D0     ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod(n_rod)

    real(8) :: a(na) ! temporary variable
    integer :: i_rod, i

    i_rod = 1

    call fuelrod(i_rod) % make(nr=nr, na=na, ngasr=45, nce=nc, &
                          ifixedcoolt = 0, ifixedcoolp = 0, ifixedtsurf = 0, &
                          iq = 0, ivardm = 1, verbose = .true., frapmode='frapcon')

    call fuelrod(i_rod) % set_r8_0('axial mesh thickness, cm', dz)
    !call fuelrod(i_rod) % set_r8_1('axial mesh thickness, cm', (/(dz, i = 1, na)/) )
    call fuelrod(i_rod) % set_r8_0("linear power, W|cm", power)
    !call fuelrod(i_rod) % set_r8_1("linear power, W|cm", (/( power, i = 1, na )/) )
    call fuelrod(i_rod) % set_r8_0("inlet coolant temperature, C", tcool)
    call fuelrod(i_rod) % set_r8_0("inlet coolant pressure, MPa", pcool)
    call fuelrod(i_rod) % set_r8_0("coolant mass flux, kg|(s*m^2)", fcool)

    call fuelrod(i_rod) % init()

    !call fuelrod(i_rod) % save ()
    !call fuelrod(i_rod) % load ()

    call fuelrod(i_rod) % next(dt)

    call fuelrod(i_rod) % get_array('bulk coolant temperature, C', a)
    write(*,*) a

    write(*,*) 'Test done!'

end program test1