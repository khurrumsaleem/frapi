program test_frapi

    use m_problem,  only : t_problem
    use odfile, only : t_odfile

    implicit none

    type (t_odfile)  :: ofile
    type (t_problem) :: problem

    character(len=256) :: frapmode  ! 'frapcon' or 'fraptran'
    character(len=256) :: ifilename ! input file name
    character(len=256) :: rfilename ! restart file name
    character(len=256) :: ofilename ! output file name

    integer :: i, step

    real(8) :: time, dt

    call get_command_argument(1, frapmode)
    call get_command_argument(2, ifilename)
    call get_command_argument(3, rfilename)
    call get_command_argument(4, ofilename)

    time = 0.D0
    step = 0

    select case (frapmode)

        case ('frapcon')

        case ('fraptran')

            call problem % make_fraptran (ifilename, rfilename, ofilename, frapmode)
            call problem % update_fraptran (time)
            call problem % frod % init ()
            call problem % frod % accept ()

            do while (time < problem % finishtime)

                dt = problem % timestep_fraptran (time)

                call problem % update_fraptran (time + dt)
                call problem % frod % next (dt)
                call problem % frod % accept ()

                call problem % ofile % write_r8_0('frapi time, s', 'time, s', time + dt)
                call problem % ofile % write_i4_0('time step', 'time step', step)
                call problem % ofile % write_r8_0('time step size, s', 'time step size, s', dt)
                call problem % save_in_file_fraptran ()

                time = time + dt
                step = step + 1

                !write(*,*) step, dt, time

            enddo

    end select

    call problem % finalize ()

    write(*,*) 'Successfully done!'

end program test_frapi