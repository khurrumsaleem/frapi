program test_frapi

    use m_problem,  only : t_problem
    use odfile, only : t_odfile

    implicit none

    type (t_odfile)  :: ofile
    type (t_problem) :: problem

    character(len=256) :: frapmode  ! 'frapcon' or 'fraptran'
    character(len=256) :: ifilename ! input file name
    character(len=256) :: rfilename ! restart file name

    integer :: i

    real(8) :: time, dt

    call get_command_argument(1, frapmode)
    call get_command_argument(2, ifilename)
    call get_command_argument(3, rfilename)

    time = 0.D0

    select case (frapmode)

        case ('frapcon')

        case ('fraptran')

            call problem % make_fraptran (ifilename, rfilename, frapmode)
            call problem % update_fraptran (time)
            call problem % frod % init ()
            call problem % frod % accept ()

            do while (time < problem % finishtime)

                dt = problem % timestep_fraptran (time)

                call problem % update_fraptran (time + dt)
                call problem % frod % next (dt)
                call problem % frod % accept ()

                time = time + dt

            enddo

    end select

    call problem % frod % destroy ()

    write(*,*) 'Successfully done!'

end program test_frapi