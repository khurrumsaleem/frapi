MODULE StaticFEA_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to run the Cladding Static FEA calculation_frapcon.
    !> Subroutines include comput_static, forced_displ, gascav_pressure,
    !> init_displ, load_step, number_dofs, rupture, time_step, update,
    !> update_database, update_displ
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/21/2015
    !
    CONTAINS
    !
    SUBROUTINE comput_static()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE math_frapcon
    USE materials_frapcon
    USE sparse_matrix_frapcon
    USE spring_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE pressure1d_frapcon
    USE pressure2d_frapcon
    USE pressure3d_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE gascav3d_frapcon
    USE cont1d_frapcon
    USE cont2d_frapcon
    USE cont3d_frapcon
    USE variables_frapcon, ONLY : ounit
    USE FEA_IO_frapcon, ONLY : line_search, write_output
    IMPLICIT NONE
    !>@brief
    !> Static finite element analysis with Newton-Raphson iteration method
    LOGICAL :: &
        lsubstp, & ! CONTINUE substep loop
        conv_NR, & ! Convergence of Newton-Raphson iteration
        conv_ln ! Convergence of line search
    INTEGER(ipk) :: &
        ln_iter, & ! Line search number
        id,ip,in
    REAL(r8k) :: &
        frnorm, & ! Vector norm of residual forces
        finorm, & ! Vector norm of internal nodal force vector
        fenorm, & ! Vector norm of external nodal force vector
        maxrnorm, & ! Maximum vector norm of the force residual for the current load step
        r,r_a,r_b,r_c, & ! Parameters for line search
        dl,dl0,dl_a,dl_b,dl_c,dl_max ! Parameters for line search
    TYPE(node_type), POINTER :: current_node

    ! Initialize the FE model if needed
    IF (init_fe) CALL update_database()

    ! Initialize current node coordinate values
    CALL init_displ()

    ! Begin substep loop
    lsubstp = .TRUE.
    DO WHILE (lsubstp)
10      CONTINUE
        ! Initialize loads for the current substep
        CALL time_step(lsubstp)

        ! Forced displacements
        CALL forced_displ()

        !-- Subroutines that modify meshing -- 

        ! Initialize and delete contact elements
        CALL cont2d_activate()
        CALL cont3d_activate()

        ! Mesh refinement

        !-------------------------------------

        ! Number DOFs If needed
        IF (dof_numbering) CALL number_dofs()

        ! Initialize Newton-Raphson solution
        conv_NR = .FALSE.
        outwrt = .FALSE.
        du(1:ndof) = 0.0_r8k
        maxrnorm  = 0.0_r8k
        nr_iter = 0
        nwarn = 0

        ! Initialize temperature dependent properties
        CALL spring_temp()
        CALL quad4_temp()
        CALL hex8_temp()
        CALL solid1d_temp()
        CALL gascav1d_temp()
        CALL gascav2d_temp()
        CALL gascav3d_temp()

        ! Begin Newton-Raphson iteration loop
        DO WHILE (.NOT. conv_NR)

        ! Maximum Newton-Raphson iteration count exceeded
        IF (nr_iter > max_nnr) THEN
            nwarn = nwarn + 1
            maxgamma = HUGE(maxgamma)
            dtime = 0.5_r8k * dtime
            CALL init_displ()
            IF (.NOT. quiet) WRITE (ounit,FMT='(/A,A,E15.8/)') &
                'WARNING N-R iteration limit ', &
                'exeeded, reducing time step to',dtime
            GOTO 10
        END IF

        ! Begin line search loop
        ln_iter = 0
        dl_max = 1.0_r8k
        dl = dl_max
20      CONTINUE
        ln_iter = ln_iter + 1
        dl0 = dl

        ! Initialize loads for the current substep
        CALL load_step()

        ! Stress calculation
        elastic_strain_energy = 0.0_r8k
        delta_plastic_strain_energy = 0.0_r8k
        maxgamma = 0.0_r8k
        CALL quad4_stress()
        CALL hex8_stress()
        CALL solid1d_stress()

        ! Error in stress calculation. First try to reduce displacement
        ! increment and if this does not work stop calculation
        IF (lerror) THEN
            IF (ABS(dl - dl_min) < 1.0e-10_r8k) THEN
                nwarn = nwarn + 1
                lerror = .FALSE.
                maxgamma = HUGE(maxgamma)
                dtime = 0.5_r8k * dtime
                CALL init_displ()
                IF (.NOT. quiet) WRITE (ounit,FMT='(/A,E15.8,A/)') 'WARNING reducing time step to ', &
                &              dtime,' due to problems in stress calculation'
                GOTO 10
            ELSE
                lerror = .FALSE.
                dl = MAX(dl_min,0.5_r8k*dl)
                dl_max = dl
                CALL update_displ(dl0,dl)
                GOTO 20
            END IF
        END IF

        ! Pressure in the gas cavities
        CALL gascav_pressure()

        ! Pressure loads
        CALL pressure1d_fext()
        CALL pressure2d_fext()
        CALL pressure3d_fext()
        CALL gascav1d_fext()
        CALL gascav2d_fext()
        CALL gascav3d_fext()

        ! Internal force vector
        Fint(1:nd,1:nnodes) = 0.0_r8k
        CALL spring_fint()
        CALL quad4_fint()
        CALL hex8_fint()
        CALL solid1d_fint()

        ! Contact forces
        CALL cont1d_fint()
        CALL cont2d_fint()
        CALL cont3d_fint()

        ! Residual force
        fres(1:ndof) = 0.0_r8k
        current_node => first_node
        DO WHILE (ASSOCIATED(current_node))
            in = current_node%number
            DO id = 1, nd
                ip = dof_number(id,in)
                IF (ip > 0) fres(ip) = fres(ip) + Fext(id,in) - Fint(id,in)
            END DO
            current_node => current_node%next
        END DO

        ! Vector norm of force vectors
        finorm = 0.0_r8k
        fenorm = 0.0_r8k
        DO in = 1, nnodes
            DO id = 1, nd
                finorm = finorm + Fint(id,in) ** 2
                fenorm = fenorm + Fext(id,in) ** 2
            END DO
        END DO
        finorm = SQRT(finorm)
        fenorm = SQRT(fenorm)
        frnorm = vnorm(ndof,fres)

        ! Energy increment PARAMETER for the line search
        r = 0.0_r8k
        DO ip = 1, ndof
            r = r + fres(ip) * du(ip)
        END DO

        ! Line search
        IF ((nr_iter > 0) .AND. (ln_iter <= max_nln)) THEN
            CALL line_search(dl_max,ln_iter,dl,dl_a,dl_b,dl_c,r,r_a,r_b,r_c,conv_ln)

            ! Update displacements and restart line search If necessary
            IF (.NOT. conv_ln) THEN
                CALL update_displ(dl0,dl)
                GOTO 20
            END IF
        END IF

        ! Check for convergence
        IF (frnorm < MAX(max_rn * finorm, max_rn * fenorm, max_rn)) conv_NR =.TRUE.

        ! Solve new displacement increment If needed
        IF (.NOT. conv_NR) THEN
            ! Use symmetric elastic_frapcon-plastic tangent matrix by default
            symmetric_matrix = .TRUE.
            elastic_matrix = .FALSE.

30          CONTINUE
            ! Initialize stiffness matrix to zero
            CALL sparse_matrix_zero_values()

            ! Tangent stiffness matrix
            CALL spring_stiff()
            CALL quad4_stiff()
            CALL hex8_stiff()
            CALL solid1d_stiff()
            CALL pressure1d_stiff()
            CALL pressure2d_stiff()
            CALL pressure3d_stiff()
            CALL gascav1d_stiff()
            CALL gascav2d_stiff()
            CALL gascav3d_stiff()
            CALL cont1d_stiff()
            CALL cont2d_stiff()
            CALL cont3d_stiff()

            ! Solve displacement increment
40          CONTINUE
            CALL sparse_matrix_solve(symmetric_matrix,ndof,fres,du)

            ! If there is no solution reduce the previous displacement
            ! increment and try again
            IF (lerror) THEN
                IF ((ABS(dl - dl_min) < 1.0e-10_r8k) .OR. (nr_iter <= 1)) THEN
                    nwarn = nwarn + 1
                    lerror = .FALSE.
                    maxgamma = HUGE(maxgamma)
                    dtime = 0.5_r8k * dtime
                    CALL init_displ()
                    IF (.NOT. quiet) WRITE (ounit,FMT='(/A,E15.8,A/)') 'WARNING reducing time step to ', &
                      &              dtime,' due to errors in the solution of the linear system'
                    GOTO 10
                ELSE
                    lerror = .FALSE.
                    dl = MAX(dl_min, 0.5_r8k * dl)
                    dl_max = dl
                    CALL update_displ(dl0,dl)
                    GOTO 20
                END IF
            END IF

            ! Increment iteration
            nr_iter = nr_iter + 1

            ! Store line search parameters
            dl_a = 0.0_r8k
            r_a = 0.0_r8k
            DO id = 1, ndof
                r_a = r_a + fres(id) * du(id)
            END DO

            ! Update displacements
            current_node => first_node
            DO in = 1, nnodes
                DO id = 1, nd
                    ip = dof_number(id,in)
                    IF (ip > 0) THEN
                    u(id,in) = u(id,in) + du(ip)
                    x(id,in) = x0(id,in) + u(id,in)
                    END IF
                END DO
            END DO
        END IF
        END DO

        ! Reduce load step If there is large plastic strain increment
        IF (maxgamma > max_ple) THEN
            nwarn = nwarn + 1
            dtime = 0.9_r8k * max_ple * dtime / maxgamma
            CALL init_displ()
            IF (.NOT. quiet) WRITE (ounit,FMT='(/,A,E15.8,A,/)') &
                    'WARNING reducing time step to ',dtime, &
                    ' due to large plastic strain increment '
            GOTO 10
        END IF

        ! Update velocities and accelerations
        current_node => first_node
        DO WHILE (ASSOCIATED(current_node))
            in = current_node%number
            v(1:nd,in) = (u(1:nd,in) - current_node%u0(1:nd))/dtime
            a(1:nd,in) = (v(1:nd,in) - current_node%v0(1:nd))/dtime
            current_node => current_node%next
        END DO

        ! update explicit values
        CALL update()

        ! Check for rupture
        IF (lrupture) CALL rupture()

        ! Printout progress data
        IF (.NOT. quiet) THEN
            WRITE (ounit,FMT='(/,A)') '-----------------------------------------------'
            WRITE (ounit,FMT='(A,E13.6)') ' Newton-Raphson solution for time ',time
            WRITE (ounit,FMT='(A)')   '-----------------------------------------------'
            WRITE (ounit,FMT='(A,E36.7)') ' Time step',dtime
            WRITE (ounit,FMT='(A,I25)') ' Number of iterations',nr_iter
            WRITE (ounit,FMT='(A,E14.7)') ' Euclidean norm of force residual',frnorm
            WRITE (ounit,FMT='(A,F14.7,/)') ' Maximum plastic strain increment',maxgamma
        END IF

        ! Write output
        IF ((time - time_output - dtime_output) > -1.0e-10_r8k) CALL write_output()
    END DO
    !
    END SUBROUTINE comput_static
    !
    !
    !
    SUBROUTINE forced_displ()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE sparse_matrix_frapcon
    IMPLICIT NONE
    !>@brief
    !> Modify force residual vector and stiffness matrix to take into account forced displacements
    TYPE(node_type), POINTER :: current_node
    TYPE(coupled_set), POINTER :: current_set
    INTEGER(ipk) :: id,in
    !
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        DO id = 1, nd
            IF (current_node%dof_status(id) > 0) THEN
                ! Find coupled set
                current_set => first_coupled_set
                DO WHILE (ASSOCIATED(current_set))
                    IF (current_set%label == current_node%dof_status(id)) EXIT
                    current_set => current_set%next
                END DO
                IF (current_set%forced) THEN
                    u(id,in) = current_node%u0(id) + dload * (current_set%u_forced - current_node%u0(id))
                    x(id,in) = x0(id,in) + u(id,in)
                END IF
            END IF
        END DO
        current_node => current_node%next
    END DO
    !
    END SUBROUTINE forced_displ
    !
    !
    !
    SUBROUTINE gascav_pressure()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE gascav3d_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate pressures at gas cavities
    !
    TYPE(gascav_type), POINTER :: current

    ! Initialize VpT parameters
    current => first_gascav
    DO WHILE (ASSOCIATED(current))
        current%VpT = current%VpT0
        current%V_tot = current%V0
        current => current%next
    END DO

    ! Update integrated volume per temperature
    CALL gascav1d_vpt()
    CALL gascav2d_vpt()
    CALL gascav3d_vpt()

    ! Calculate pressures
    current => first_gascav
    DO WHILE (ASSOCIATED(current))
        IF (current%failed) THEN
        current%mgas = current%p * current%VpT
        ELSE
        current%p = current%mgas / current%VpT
        END IF
        current => current%next
    END DO
    !
    END SUBROUTINE gascav_pressure
    !
    !
    SUBROUTINE init_displ()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize displacements from the explicit values
    !
    INTEGER(ipk) ::in
    TYPE(node_type), POINTER :: current
    TYPE(gascav_type), POINTER :: currgc

    ! Set displacements to explicit values
    current => first_node
    DO WHILE (ASSOCIATED(current))
        in = current%number
        x0(1:nd,in) = current%x0(1:nd)
        u(1:nd,in) = current%u0(1:nd)
        x(1:nd,in) = x0(1:nd,in) + u(1:nd,in)
        v(1:nd,in) = current%v0(1:nd)
        a(1:nd,in) = current%a0(1:nd)
        temp(in) = current%temp0
        Fint(1:nd,in) = current%Fint0(1:nd)
        current => current%next
    END DO

    ! Initialize gas cavities
    currgc => first_gascav
    DO WHILE (ASSOCIATED(currgc))
        currgc%failed = currgc%failed0
        currgc%p0 = currgc%p0
        currgc%mgas = currgc%mgas0
        currgc => currgc%next
    END DO

    END SUBROUTINE init_displ
    !
    !
    !
    SUBROUTINE load_step()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate load and temperature increments
    !
    INTEGER(ipk) :: in
    TYPE(node_type), POINTER :: current_node

    ! Nodal loads
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        Fext(1:nd,in) = Fgrav(1:nd,in)
        IF (current_node%force_flag) Fext(1:nd,in) = Fext(1:nd,in) + &
            current_node%force(1:nd)
        current_node => current_node%next
    END DO

    END SUBROUTINE load_step
    !
    !
    !
    Subroutine number_dofs()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE sparse_matrix_frapcon
    USE spring_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE gascav3d_frapcon
    USE cont1d_frapcon
    USE cont2d_frapcon
    USE cont3d_frapcon
    IMPLICIT NONE
    !>@brief
    !> Set status and number of all DOFs
    !
    TYPE(coupled_set), POINTER :: current_set
    TYPE(node_type), POINTER :: current_node
    INTEGER(ipk) :: id,in

    ndof = 0
    dof_number = 0

    ! Number coupled DOFs
    current_set => first_coupled_set
    DO WHILE (ASSOCIATED(current_set))
        IF (.NOT. current_set%forced) THEN
            ndof = ndof + 1
            current_set%dof_number = ndof
        ELSE
            current_set%dof_number = 0
        END IF
        current_set => current_set%next
    END DO

    ! Mark those node DOFs fixed that are not connected to structural elements
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        DO id = 1, nd
            IF (current_node%dof_status(id) == -2) current_node%dof_status(id) = 0
            IF ((enumber(in) == 0) .AND. (current_node%dof_status(id) == 0)) THEN
                current_node%dof_status(id) = -2
                WRITE (ounit,FMT='(/,A,I0,A,I0,A,/)') &
                    'WARNING marking DOF ',id,' of node ',current_node%label, &
                    ' fixed because it is not connected to any structural element_frapcon'
            END IF
        END DO
        current_node => current_node%next
    END DO

    ! Number all free noncoupled DOFs
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        dof_number(1:nd,in) = 0
        DO id = 1, nd
            IF (current_node%dof_status(id) == 0) THEN
                ! Free DOF
                ndof = ndof + 1
                dof_number(id,in) = ndof
            ELSE IF (current_node%dof_status(id) > 0) THEN
                ! Coupled DOF
                current_set => first_coupled_set
                DO WHILE (ASSOCIATED(current_set))
                    IF (current_set%label == current_node%dof_status(id)) THEN
                        dof_number(id,in) = current_set%dof_number
                        EXIT
                    END IF
                    current_set => current_set%next
                END DO
            END IF
        END DO
        current_node => current_node%next
    END DO

    dof_numbering = .FALSE.

    ! Initialize sparse matrix solver
    CALL sparse_matrix_init(ndof)

    ! Add elements to the sparse matrix structure
    CALL spring_sparse_matrix()
    CALL quad4_sparse_matrix()
    CALL hex8_sparse_matrix()
    CALL solid1d_sparse_matrix()
    CALL gascav1d_sparse_matrix()
    CALL gascav2d_sparse_matrix()
    CALL gascav3d_sparse_matrix()
    CALL cont1d_sparse_matrix()
    CALL cont2d_sparse_matrix()
    CALL cont3d_sparse_matrix()

    ! Form the sparse matrix storage
    CALL sparse_matrix_storage_form()

    END SUBROUTINE number_dofs
    !
    !
    SUBROUTINE rupture()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE solid1d_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    IMPLICIT NONE
    !>@brief
    !> Check for rupture of gas cavity
    !
    TYPE(solid1d_type), POINTER :: currs1d
    TYPE(quad4_type), POINTER :: currq4
    TYPE(hex8_type), POINTER :: currh8
    TYPE(gascav_type), POINTER :: currgc
    LOGICAL :: lfailed
    INTEGER(ipk) :: ig

    lfailed = .FALSE.

    currs1d => first_solid1d
    DO WHILE (ASSOCIATED(currs1d))
        IF (currs1d%mat == rupture_mat) THEN
            IF (currs1d%epseff > rupture_eps) lfailed = .TRUE.
        END IF
        currs1d => currs1d%next
    END DO

    currq4 => first_quad4
    DO WHILE (ASSOCIATED(currq4))
        IF (currq4%mat == rupture_mat) THEN
            DO ig = 1, 4
                IF (currq4%epseff(ig) > rupture_eps) lfailed = .TRUE.
            END DO
        END IF
        currq4 => currq4%next
    END DO

    currh8 => first_hex8
    DO WHILE (ASSOCIATED(currh8))
        IF (currh8%mat == rupture_mat) THEN
            DO ig = 1,8
                IF (currh8%epseff(ig) > rupture_eps) lfailed = .TRUE.
            END DO
        END IF
        currh8 => currh8%next
    END DO

    IF (lfailed) THEN
        currgc => first_gascav
        DO WHILE (ASSOCIATED(currgc))
            IF (currgc%label == rupture_gc) EXIT
            currgc => currgc%next
        END DO

        IF (.NOT. ASSOCIATED(currgc)) THEN
            WRITE (ounit,FMT='(/,A,I0/)') 'ERROR can not find gascavity ',rupture_gc
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF

        currgc%failed = .TRUE.
        currgc%p = rupture_p

    END IF
    !
    END SUBROUTINE rupture
    !
    !
    !
    SUBROUTINE time_step (lsubstp)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE variables_frapcon, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Calculate load and temperature increments
    !
    INTEGER(ipk) :: in
    LOGICAL, INTENT(OUT) :: lsubstp
    TYPE(node_type), POINTER :: current_node
    TYPE(gascav_type), POINTER :: current_gascav
    REAL(r8k) :: dtime_p

    lsubstp = .TRUE.

    ! Check time step validity
    IF (time_end <= time0) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR invalid time step data'
        lerror = .TRUE.
        CALL nlfemp_stop(1)
    END IF

    IF (dtime < dtime_min) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR no convergence'
        lerror = .TRUE.
        CALL nlfemp_stop(1)
    END IF

    ! Increase time step If the deformations were small at the previous step
    ! and there was less that 10 N-R iterations at last load step
    !IF ((maxgamma < 0.75_r8k*max_ple) .AND. (nr_iter < 10).AND. &
    !     (dtime < dtime0) .AND. (nwarn == 0)) THEN
    IF ((maxgamma < 0.75_r8k * max_ple) .AND. (dtime < dtime0) .AND. (nwarn == 0)) THEN
        dtime_p = dtime
        dtime = 0.75_r8k * max_ple * dtime / MAX(maxgamma, 1.0e-10_r8k)
        dtime = MIN(dtime0, dtime)
        dtime = MIN(dtime, 1.5_r8k * dtime_p)
    END IF

    ! Reduce time step If there was large number of N-R iterations at the
    ! previous load step
    !IF (nr_iter > 20) THEN
    !   dtime = 0.8_r8k*dtime
    !END IF

    ! Current analysis time
    time = time0 + dtime

    ! adjustment to the last time step
    IF (time > (time_end - 0.01_r8k * dtime)) THEN
        time = time_end
        dtime = time_end - time0
        lsubstp = .FALSE.
    END IF

    ! Load step
    IF (linear_load) THEN
        dload = dtime / (time_end - time0)
    ELSE
        dload = 1.0_r8k
    END IF

    ! Gravity
    Fgrav(1:nd,1:nnodes) = 0.0_r8k
    grav = grav0 + dload * (grav_end - grav0)
    CALL quad4_grav()
    CALL hex8_grav()

    ! Nodal loads
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        temp(in) = current_node%temp0 + dload * (current_node%temp_end - current_node%temp0)
        IF (current_node%force_flag) current_node%force = current_node%force0 &
            + dload*(current_node%force_end - current_node%force0)     
        current_node => current_node%next
    END DO

    ! Gas cavities
    current_gascav => first_gascav
    DO WHILE (ASSOCIATED(current_gascav))
        current_gascav%mgas = current_gascav%mgas0 + dload * (current_gascav%mgas_end - current_gascav%mgas0)
        current_gascav => current_gascav%next
    END DO

    END SUBROUTINE time_step
    !
    !
    !
    SUBROUTINE update()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE materials_frapcon
    USE sparse_matrix_frapcon
    USE spring_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE pressure1d_frapcon
    USE pressure2d_frapcon
    USE pressure3d_frapcon
    USE cont1d_frapcon
    USE cont2d_frapcon
    USE cont3d_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update explicit values
    !
    TYPE(node_type), POINTER :: current_node
    TYPE(coupled_set), POINTER :: current_set
    TYPE(mat_type), POINTER :: current_mat
    TYPE(gascav_type), POINTER :: current_gascav
    INTEGER(ipk) :: id, in, ip, i, j, k
    REAL(r8k), ALLOCATABLE :: v_tmp(:)

    ! Update element strains
    CALL quad4_strains()
    CALL hex8_strains()
    CALL solid1d_strains()

    ! Update general data
    time0 = time
    grav0 = grav

    ! Update FE model energy data
    plastic_strain_energy = plastic_strain_energy + delta_plastic_strain_energy
    IF (analys(1:7) == 'DYNAMIC') THEN
        ALLOCATE(v_tmp(ndof))
        DO in = 1, nnodes
            DO id = 1, nd
                ip = dof_number(id,in)
                IF (ip > 0) v_tmp(ip) = v(id,in)
            END DO
        END DO
        real_tmp(1:ndof) = 0.0_r8k
        DO i = 1, ndof
            DO k = rowindex(i),rowindex(i+1)-1
                j = columns(k)
                real_tmp(i) = real_tmp(i) + values_mm(k) * v_tmp(j)
            END DO
        END DO
        kinetic_energy = 0.0_r8k
        DO i = 1, ndof
            kinetic_energy = kinetic_energy + 0.5_r8k * v_tmp(i) * real_tmp(i)
        END DO
    END IF

    ! Set coupled set displacement and force data to zero
    current_set => first_coupled_set
    DO WHILE (ASSOCIATED(current_set))
        current_set%u = 0.0_r8k
        current_set%force = 0.0_r8k
        current_set => current_set%next
    END DO

    ! Update nodal data
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        current_node%u0(1:nd) = u(1:nd,in)
        current_node%v0(1:nd) = v(1:nd,in)
        current_node%a0(1:nd) = a(1:nd,in)
        current_node%temp0 = temp(in)
        current_node%fint0(1:nd) = Fint(1:nd,in)
        IF (current_node%force_flag) THEN
            current_node%force0 = current_node%force
            IF ((MAXVAL(ABS(current_node%force0)) < 1.0e-15_r8k) .AND. &
                (MAXVAL(ABS(current_node%force_end)) < 1.0e-15_r8k)) THEN
                current_node%force_flag = .FALSE.
                DEALLOCATE(current_node%force,current_node%force0,current_node%force_end)
                current_node%force => NULL()
                current_node%force0 => NULL()
                current_node%force_end => NULL()
            END IF
        END IF
        ! Coupled set displacement and force data
        DO id = 1, nd
            IF (current_node%dof_status(id) > 0) THEN
                current_set => first_coupled_set
                DO WHILE (ASSOCIATED(current_set))
                    IF (current_set%label == current_node%dof_status(id)) EXIT
                    current_set => current_set%next
                END DO
                current_set%u = current_node%u0(id)
                current_set%v = current_node%v0(id)
                current_set%a = current_node%a0(id)
                current_set%force = current_set%force + current_node%fint0(id)
            END IF
        END DO
        current_node => current_node%next
    END DO

    ! Set material maximum effective stresses to zero
    current_mat => first_mat
    DO WHILE (ASSOCIATED(current_mat))
        current_mat%maxsigeff = 0.0_r8k
        current_mat => current_mat%next
    END DO

    ! Update gas cavities
    current_gascav => first_gascav
    DO WHILE (ASSOCIATED(current_gascav))
        current_gascav%failed0 = current_gascav%failed
        current_gascav%p0 = current_gascav%p
        current_gascav%mgas0 = current_gascav%mgas
        current_gascav => current_gascav%next
    END DO

    ! Update element data
    CALL quad4_update()
    CALL hex8_update()
    CALL solid1d_update()
    CALL pressure1d_update()
    CALL pressure2d_update()
    CALL pressure3d_update()
    CALL cont1d_update()
    CALL cont2d_update()
    CALL cont3d_update()

    END SUBROUTINE update
    !
    !
    !
    SUBROUTINE update_database()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE pressure1d_frapcon
    USE pressure2d_frapcon
    USE pressure3d_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE gascav3d_frapcon
    USE cont1d_frapcon
    USE cont2d_frapcon
    USE cont3d_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create element database
    TYPE(node_type), POINTER :: current_node
    TYPE(quad4_type), POINTER ::current_q4
    TYPE(hex8_type), POINTER :: current_h8
    TYPE(solid1d_type), POINTER :: current_s1d
    TYPE(pressure1d_type), POINTER :: current_p1d
    TYPE(pressure2d_type), POINTER :: current_p2d
    TYPE(pressure3d_type), POINTER :: current_p3d
    TYPE(gascav1d_type), POINTER :: current_gc1d
    TYPE(gascav2d_type), POINTER :: current_gc2d
    TYPE(gascav3d_type), POINTER :: current_gc3d
    TYPE(cont1d_type), POINTER :: current_c1
    TYPE(cont2d_type), POINTER :: current_c2
    TYPE(cont3d_type), POINTER :: current_c3
    INTEGER(ipk) :: i, j, in, nfound

    ! Put node labels in an array
    node_labels = 0
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        node_labels(in) = current_node%label
        x0(1:nd,in) = current_node%x0(1:nd)
        u(1:nd,in) = current_node%u0(1:nd)
        x(1:nd,in) = x0(1:nd,in) + u(1:nd,in)
        v(1:nd,in) = current_node%v0(1:nd)
        a(1:nd,in) = current_node%a0(1:nd)
        temp(in) = current_node%temp0
        Fint(1:nd,in) = current_node%Fint0(1:nd)
        current_node => current_node%next
    END DO

    enumber(1:nnodes) = 0

    ! Associate node labels to internal node numbers in QUAD4 elements
    current_q4 => first_quad4
    DO WHILE (ASSOCIATED(current_q4))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_q4%node_labels(j) == node_labels(i)) THEN
                    current_q4%node_numbers(j) = i
                    enumber(i) = enumber(i) + 1
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        CALL quad4_deriv(x0(1,current_q4%node_numbers(1)), &
            x0(1,current_q4%node_numbers(2)),x0(1,current_q4%node_numbers(3)), &
            x0(1,current_q4%node_numbers(4)),current_q4%V0, &
            current_q4%dV0,current_q4%dNdx0,current_q4%dNdxm,current_q4%x0)     
        current_q4 => current_q4%next
    END DO

    ! Associate node labels to internal node numbers in HEX8 elements
    current_h8 => first_hex8
    DO WHILE (ASSOCIATED(current_h8))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 8
                IF (current_h8%node_labels(j) == node_labels(i)) THEN
                    current_h8%node_numbers(j) = i
                    enumber(i) = enumber(i) + 1
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 8) EXIT
        END DO
        CALL hex8_deriv(x0(1,current_h8%node_numbers(1)), &
            x0(1,current_h8%node_numbers(2)),x0(1,current_h8%node_numbers(3)), &
            x0(1,current_h8%node_numbers(4)),x0(1,current_h8%node_numbers(5)), &
            x0(1,current_h8%node_numbers(6)),x0(1,current_h8%node_numbers(7)), &
            x0(1,current_h8%node_numbers(8)),current_h8%V0,current_h8%dV0, &
            current_h8%dNdx0,current_h8%dNdxm,current_h8%x0)
        current_h8 => current_h8%next
    END DO

    ! Associate node labels to internal node numbers in SOLID1D elements
    current_s1d => first_solid1d
    DO WHILE (ASSOCIATED(current_s1d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_s1d%node_labels(j) == node_labels(i)) THEN
                    current_s1d%node_numbers(j) = i
                    enumber(i) = enumber(i) + 1
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        CALL solid1d_deriv(x0(1,current_s1d%node_numbers(1)), &
            x0(1,current_s1d%node_numbers(2)), &
            x0(1,current_s1d%node_numbers(3)), &
            x0(1,current_s1d%node_numbers(4)),current_s1d%dV0, &
            current_s1d%dNdx,current_s1d%x0)
        current_s1d => current_s1d%next
    END DO

    ! Associate node labels to internal node numbers in PRESSURE1D elements
    current_p1d => first_pressure1d
    DO WHILE (ASSOCIATED(current_p1d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 3
                IF (current_p1d%node_labels(j) == node_labels(i)) THEN
                    current_p1d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 3) EXIT
        END DO
        current_p1d => current_p1d%next
    END DO

    ! Associate node labels to internal node numbers in PRESSURE2D elements
    current_p2d => first_pressure2d
    DO WHILE (ASSOCIATED(current_p2d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 2
                IF (current_p2d%node_labels(j) == node_labels(i)) THEN
                    current_p2d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 2) EXIT
        END DO
        current_p2d => current_p2d%next
    END DO

    ! Associate node labels to internal node numbers in PRESSURE3D elements
    current_p3d => first_pressure3d
    DO WHILE (ASSOCIATED(current_p3d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_p3d%node_labels(j) == node_labels(i)) THEN
                    current_p3d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        current_p3d => current_p3d%next
    END DO

    ! Associate node labels to internal node numbers in GASCAV1D elements
    current_gc1d => first_gascav1d
    DO WHILE (ASSOCIATED(current_gc1d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_gc1d%node_labels(j) == node_labels(i)) THEN
                    current_gc1d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        current_gc1d => current_gc1d%next
    END DO

    ! Associate node labels to internal node numbers in GASCAV2D elements
    current_gc2d => first_gascav2d
    DO WHILE (ASSOCIATED(current_gc2d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_gc2d%node_labels(j) == node_labels(i)) THEN
                    current_gc2d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        current_gc2d => current_gc2d%next
    END DO

    ! Associate node labels to internal node numbers in GASCAV3D elements
    current_gc3d => first_gascav3d
    DO WHILE (ASSOCIATED(current_gc3d))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 8
                IF (current_gc3d%node_labels(j) == node_labels(i)) THEN
                    current_gc3d%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 8) EXIT
        END DO
        current_gc3d => current_gc3d%next
    END DO

    ! Associate node labels to internal node numbers in CONT1D elements
    current_c1 => first_cont1d
    DO WHILE (ASSOCIATED(current_c1))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 4
                IF (current_c1%node_labels(j) == node_labels(i)) THEN
                    current_c1%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 4) EXIT
        END DO
        current_c1 => current_c1%next
    END DO

    ! Associate node labels to internal node numbers in CONT2D elements
    current_c2 => first_cont2d
    DO WHILE (ASSOCIATED(current_c2))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 3
                IF (current_c2%node_labels(j) == node_labels(i)) THEN
                    current_c2%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 3) EXIT
        END DO
        current_c2 => current_c2%next
    END DO

    ! Associate node labels to internal node numbers in CONT3D elements
    current_c3 => first_cont3d
    DO WHILE (ASSOCIATED(current_c3))
        nfound = 0
        DO i = 1, nnodes
            DO j = 1, 5
                IF (current_c3%node_labels(j) == node_labels(i)) THEN
                    current_c3%node_numbers(j) = i
                    nfound = nfound + 1
                END IF
            END DO
            IF (nfound == 5) EXIT
        END DO
        current_c3 => current_c3%next
    END DO

    init_fe = .FALSE.
    CALL init_displ()

    END SUBROUTINE update_database
    !
    !
    !
    SUBROUTINE update_displ (dl0, dl)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update displacements after line search
    !
    REAL(r8k), INTENT(IN) :: dl,dl0
    INTEGER(ipk) :: id,ip,in
    TYPE(node_type), POINTER :: current_node
    !
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        in = current_node%number
        DO id = 1, nd
            ip = dof_number(id,in)
            IF (ip > 0) THEN
                u(id,in) = u(id,in) - (dl0 - dl)*du(ip)
                x(id,in) = x0(id,in) + u(id,in)
            END IF
        END DO
        current_node => current_node%next
    END DO
    !
    END SUBROUTINE update_displ
    !
END MODULE StaticFEA_frapcon



