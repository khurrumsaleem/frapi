module m_od

    use frapi, only : t_fuelrod
    use m_utils, only : int2str

    implicit none

    integer, parameter :: na = 8           ! number of axial mesh nodes
    integer, parameter :: nr = 5           ! number of radial mesh nodes in pellet
    integer, parameter :: nc = 3           ! number of radial mesh nodes in cladding
    integer, parameter :: ng = 45          ! number of radial mesh nodes in pellet for gas release model
    integer, parameter :: nt = 1           ! number of time steps

    real(8), parameter :: dt = 1.D-3       ! time step, sec
    real(8), parameter :: tcool = 296.D0   ! inlet coolant temperature, C
    real(8), parameter :: tclad = 300.D0   ! cladding surface temperature, C
    real(8), parameter :: pcool = 15.5D0   ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 3101.d0    ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: dz = 365.D0 / na ! thickness of axial mesh, cm
    real(8), parameter :: lpower = 150.d0

    real(8) :: tmp_r8_1(na), tmp_r8_2(nr+nc,na)

    contains

    subroutine odprint (i, time, fuelrod)

        implicit none

        type(t_fuelrod), intent(inout) :: fuelrod
        integer :: i
        real(8) :: time
        real(8) :: output(na, 14)

        output = 0.d0

        call fuelrod % get_r8_1('cladding outer temperature, c', output(:,1))
        call fuelrod % get_r8_1('cladding inner temperature, c', output(:,2))
        call fuelrod % get_r8_1('pellet surface temperature, c', output(:,3))
        call fuelrod % get_r8_1('pellet centerline temperature, c', output(:,4))
        call fuelrod % get_r8_1('gap total heat transfer coefficient, w/(k*m^2)', output(:,5))
        call fuelrod % get_r8_1('gap solid heat transfer coefficient, w/(k*m^2)', output(:,6))
        call fuelrod % get_r8_1('gap gas heat transfer coefficient, w/(k*m^2)', output(:,7))
        call fuelrod % get_r8_1('gap radiation heat transfer coefficient, w/(k*m^2)', output(:,8))
        call fuelrod % get_r8_1('mechanical gap thickness, um', output(:,9))
        call fuelrod % get_r8_1('gap pressure, mpa', output(:,10))
        call fuelrod % get_r8_1('cladding hoop stress, mpa', output(:,11))
        call fuelrod % get_r8_1('deformed pellet radius, mm', output(:,12))
        call fuelrod % get_r8_1('deformed cladding inner radius, mm', output(:,13))
        call fuelrod % get_r8_1('deformed cladding outer radius, mm', output(:,14))

        write(*,'(I10,16F12.3)') i, time, (/( maxval(output(:,i)), i = 1, 14 )/)

    end subroutine odprint

end module m_od



program test2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                           !
!   Fuel rod length 3.65 m with the constant linear power 200 W/cm          !
!                                                                           !
!   15.4MPa, 290C, 5000 kg/m2s => | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | !
!                                                                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use frapi, only : t_fuelrod            ! 'fuelrod' class from 'frapi'
    use m_od

    implicit none

    type(t_fuelrod) :: fuelrod             ! fuel rod object

    real(8) :: time = 0
    integer :: i_rod, i = 0

    write(*,'(A10,16A12)') 'N','Time (s)', 'Tco (C)', 'Tci (C)', 'Tfs (C)', 'Tfc (C)', 'Tot HTC', 'Solid HTC', 'Gas HTC', 'Rad HTC', 'Gap (um)', 'Gap P (MPa)', 'HStress (MPa)', 'Rp (mm)', 'Rci (mm)', 'Rco (mm)'

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
    call fuelrod % set_r8_0("linear power, W/cm", lpower)
!    call fuelrod % set_r8_0("inlet coolant temperature, C", tclad)
    call fuelrod % set_r8_0("coolant pressure, MPa", pcool)
!    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    call fuelrod % set_r8_0("cladding outer surface temperature, c", tclad)

    call fuelrod % init()

    do i = 1, 10
        call fuelrod % next(1.d0)
        call odprint(i, time, fuelrod)
    enddo

    call fuelrod % makerf()

    call fuelrod % destroy ()

    write(*,'(A)') ''

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPTRAN Calculation                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call fuelrod % make(nr=nr, na=na, ngasr=ng, nce=nc, &
                        ifixedcoolt = 0, &
                        ifixedcoolp = 0, &
                        ifixedtsurf = 1, &
                        iq = 0, ivardm = 1, verbose = .true., frapmode='fraptran')

    call fuelrod % set_ch_0('restart file', './fuelrod-restart.txt')

    call fuelrod % set_ch_0('bheat', 'on')
    call fuelrod % set_ch_0('coolant', 'off')

    call fuelrod % set_r8_0('axial mesh thickness, cm', dz)
    call fuelrod % set_r8_0("linear power, W/cm", lpower)
!    call fuelrod % set_r8_0("inlet coolant temperature, C", tclad)
    call fuelrod % set_r8_0("coolant pressure, MPa", pcool)
!    call fuelrod % set_r8_0("inlet coolant mass flow, kg/(s*m^2)", fcool)
    call fuelrod % set_r8_0("cladding outer surface temperature, c", tclad)

!    call fuelrod % dftran % copy_f2r( './frod-0055.txt' )

!    call fuelrod % set_ch_0('restart file', './rest-0055.txt')

    call fuelrod % init()


    time = 0.d0
    i = 0

    call odprint(i, time, fuelrod)

    do i = 1, nt
        time = time + dt
        call odprint(i, time, fuelrod)
    enddo

    call fuelrod % destroy ()

    write(*,*) 'Program complete!'

end program test2
