program test3
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
    integer, parameter :: nt = 10          ! number of time steps

    real(8), parameter :: dt = 1.D-1       ! time step, s
    real(8), parameter :: tcool = 306.D0   ! inlet coolant temperature, C
    real(8), parameter :: pcool = 15.11D0  ! inlet coolant pressure, MPa
    real(8), parameter :: fcool = 5.D+3   ! coolant mass flux, kg/(m*2*s)
    real(8), parameter :: power = 2.D+2    ! linear power, W/cm
    real(8), parameter :: dz = 365.D0 / na ! thickness of axial mesh, cm

    type(t_fuelrod) :: fuelrod             ! fuel rod object

    real(8) :: tbc1(nt,na), tbc2(nt,na)    ! bulk coolant temperature
    real(8) :: tfc1(nt,na), tfc2(nt,na)    ! centerline pellet temperature
    real(8) :: tfs1(nt,na), tfs2(nt,na)    ! surface pellet temperature
    real(8) :: a, b, c, w, time
    integer :: i_rod, i = 0, j = 0

    write(*,*) "TEST DESCRIPTION: "
    write(*,*) "The test shows the restart capability of FRAPI/FRAPTRAN. Two modes are compared:"
    write(*,*) "a) 'REGULAR' mode is the step-by-step iteration w/o saving of data into an output file"
    write(*,*) "b) 'RESTART' mode stops and renews the transient on the every time step."
    write(*,*) "The fuel rod state is printed into the binary file before the stop."
    write(*,*) "  "
    write(*,*) "Tc : bulk coolant temperature  "
    write(*,*) "Tfc: centerline pellet temperature  "
    write(*,*) "Tfs: surface pellet temperature  "
    write(*,*) "W  : linear power  "
    write(*,*) "  "

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            FRAPCON Calculation                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(*,*) "                     FRAPCON Initial State Calculation"

    time = 0.D0

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

    call fuelrod % get_r8_1('bulk coolant temperature, C', tbc1(1,:) )
    call fuelrod % get_r8_1('pellet surface temperature, c', tfs1(1,:) )
    call fuelrod % get_r8_1('pellet centerline temperature, c', tfc1(1,:) )

    write(*,'(I10,5F20.3)') i, time, power, maxval(tbc1(1,:)), maxval(tfs1(1,:)), maxval(tfc1(1,:))
    write(*,*) "  "

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

    call fuelrod % save_bin("./fuelrod.bin")

    write(*,*) "REGULAR Mode"
    write(*,*) "                      Time (s)            W (W/cm)            Tc (C)             Tfs (C)            Tfc (C)"

    time = 0.d0

    do i = 1, nt
        w = power * (1.D0 + 1.D0 / nt * (i-1))
        call fuelrod % set_r8_0("linear power, W|cm", w )
        call fuelrod % next (dt)

        call fuelrod % get_r8_1('bulk coolant temperature, C', tbc1(i,:) )
        call fuelrod % get_r8_1('pellet surface temperature, c', tfs1(i,:) )
        call fuelrod % get_r8_1('pellet centerline temperature, c', tfc1(i,:) )

        time = time + dt

        write(*,'(I10,5F20.3)') i, time, w, maxval(tbc1(i,:)), maxval(tfs1(i,:)), maxval(tfc1(i,:))

    enddo

    call fuelrod % destroy ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!               FRAPTRAN Calculation with Restarts                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(*,*) "  "
    write(*,*) "RESTART Mode"
    write(*,*) "                      Time (s)            W (W/cm)            Tc (C)             Tfs (C)            Tfc (C)"

    time = 0.d0

    do i = 1, nt

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

        call fuelrod % load_bin("./fuelrod.bin")

        w = power * (1.D0 + 1.D0 / nt * (i-1))
        call fuelrod % set_r8_0("linear power, W|cm", w)

        call fuelrod % next (dt)

        call fuelrod % get_r8_1('bulk coolant temperature, C', tbc2(i,:) )
        call fuelrod % get_r8_1('pellet surface temperature, c', tfs2(i,:) )
        call fuelrod % get_r8_1('pellet centerline temperature, c', tfc2(i,:) )

        time = time + dt

        write(*,'(I10,5F20.3)') i, time, w, maxval(tbc2(i,:)), maxval(tfs2(i,:)), maxval(tfc2(i,:))

        call fuelrod % save_bin("./fuelrod.bin")

        call fuelrod % destroy ()

    enddo

    write(*,*) "  "
    write(*,*) "REGULAR vs RESTART Modes Comparison"
    write(*,*) "                      Time (s)            W (W/cm)            Tc (C)             Tfs (C)            Tfc (C)"

    time = 0.d0
    do i = 1, nt
        time = time + dt
        a = maxval( (/( abs(tbc2(i,j)/tbc1(i,j)-1), j = 1, na )/) * 100 )
        b = maxval( (/( abs(tfs2(i,j)/tfs1(i,j)-1), j = 1, na )/) * 100 )
        c = maxval( (/( abs(tfc2(i,j)/tfc1(i,j)-1), j = 1, na )/) * 100 )
        write(*,'(I10,F20.3,4E20.1)') i, time, 0.d0, a, b, c
    enddo

    write(*,*) 'Program complete!'

end program test3
