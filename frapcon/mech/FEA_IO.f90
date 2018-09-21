MODULE FEA_IO_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines to create the input and output for the cladding FEA model_frapcon.
    !> Subroutines include fileo, input_read_error, line_search, read_input_line, write_output
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE fileo()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Opening the input and output files
    !
    LOGICAL :: fex

    restart = .FALSE.

    outfile = 'frapcon-fem.edb'

    ! New output files are created
    OPEN(UNIT=out_unit,FILE=outfile,STATUS='UNKNOWN',FORM='UNFORMATTED')

    END SUBROUTINE fileo
    !
    !
    !
    SUBROUTINE input_read_error()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Report line number and stop program

    WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR while reading input at line ',nline
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE input_read_error
    !
    !
    !
    SUBROUTINE line_search (dl_max, ln_iter, dl, dl_a, dl_b, dl_c, r, r_a, r_b, r_c, conv_ln)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Line search
    !
    INTEGER(ipk), INTENT(IN) :: ln_iter
    REAL(r8k), INTENT(IN) :: dl_max
    REAL(r8k), INTENT(INOUT) :: dl, dl_a, dl_b, dl_c, r, r_a, r_b, r_c
    LOGICAL, INTENT(OUT) :: conv_ln
    REAL(r8k) :: t1, t2, t3, t4, dl0

    dl0 = dl
    conv_ln = .TRUE.

    IF (r_a < 0.0_r8k) RETURN
    IF (ln_iter > max_nln) RETURN

    IF ((ln_iter == 1) .OR. (r < 0.0_r8k)) THEN
        r_c = r
        dl_c = dl
    END IF

    ! Line search If there is a zero point
    IF (r_c < 0.0_r8k) THEN
        ! Use bisection to find the zero point_frapcon
        IF (r < 0.0_r8k) THEN
            dl_c = dl
            r_c = r
        ELSE
            dl_a = dl
            r_a = r
        END IF
        dl = 0.5_r8k * (dl_a + dl_c)
    ELSE
        ! Quadratic line search to minimize the value of r
        IF (ln_iter == 1) THEN
            ! If vector norm is less than limit value, Do not initiate line search
            IF (r > (rn_ratio * r_a)) THEN
                dl = (dl_a + dl_c) / 2.0_r8k
                conv_ln = .FALSE.
            ELSE
                RETURN
            END IF
        ELSE IF (ln_iter == 2) THEN
            dl_b = dl
            r_b = r
        ELSE
            IF (dl < dl_a) THEN
                dl_c = dl_b
                r_c = r_b
                dl_b = dl_a
                r_b = r_a
                dl_a = dl
                r_a = r
            ELSE IF (dl < dl_b) THEN
                IF (r < r_b) THEN 
                    dl_c = dl_b
                    r_c = r_b
                    dl_b = dl
                    r_b = r
                ELSE
                    dl_a = dl
                    r_a = r
                END IF
            ELSE IF (dl < dl_c) THEN
                IF (r < r_b) THEN
                    dl_a = dl_b
                    r_a = r_b
                    dl_b = dl
                    r_b = r
                ELSE
                    dl_c = dl
                    r_c = r
                END IF
            ELSE
                dl_a = dl_b
                r_a = r_b
                dl_b = dl_c
                r_b = r_c
                dl_c = dl
                r_c = r
            END IF
        END IF

        IF (r_a <= r_b) THEN
            dl = dl_a - 0.1_r8k
        ELSE IF (r_c <= r_b) THEN
            dl = dl_c + 0.1_r8k
        ELSE
            ! Parabolic approximation
            t1 = dl_b - dl_a
            t2 = dl_b - dl_c
            t3 = r_b - r_a
            t4 = r_b - r_c
            dl = dl_b - 0.5_r8k * (t1 ** 2 * t4 - t2 ** 2 * t3) / (t1 * t4 - t2 * t3)
        END IF
    END IF

    ! Limit the line search step If needed
    dl = MAX(dl, dl_min)
    dl = MIN(dl, dl_max)

    ! Check If new line search is needed 
    IF (ABS(dl - dl0) > dl_min) conv_ln = .FALSE.

    END SUBROUTINE line_search
    !
    !
    !
    SUBROUTINE read_input_line()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read next uncommented nonempty line from input line

    DO
        nline = nline + 1
        READ(UNIT=in_unit,FMT='(A1000)',ERR=10,End=10) line
        line = ADJUSTL(line)
        ! Ignore empty and commented lines
        IF ((LEN_TRIM(line) > 0) .AND. (line(1:1) /= '#')) RETURN
    END DO

10  CONTINUE
    CALL input_read_error()

    END SUBROUTINE read_input_line
    !
    !
    !
    SUBROUTINE write_output()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE materials_frapcon
    USE geometry_fpn_frapcon
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
    IMPLICIT NONE
    !>@brief
    !> Write restart/output file for the FE model to unit "out_unit"
    !
    TYPE(node_type), POINTER :: current_node
    TYPE(coupled_set), POINTER :: current_set
    TYPE(gascav_type), POINTER :: current_gascav

    ! Output number
    noutput = noutput + 1
    WRITE (UNIT=out_unit) noutput

    ! Write analysis type
    WRITE (UNIT=out_unit) analys
    WRITE (UNIT=out_unit) dimens
    WRITE (UNIT=out_unit) nd
    WRITE (UNIT=out_unit) units

    ! Write general data
    WRITE (UNIT=out_unit) time0
    WRITE (UNIT=out_unit) tref
    WRITE (UNIT=out_unit) grav0

    ! Write FE model energies
    WRITE (UNIT=out_unit) elastic_strain_energy
    WRITE (UNIT=out_unit) plastic_strain_energy
    WRITE (UNIT=out_unit) kinetic_energy

    ! Write nodal data
    WRITE (UNIT=out_unit) nnodes
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        WRITE (UNIT=out_unit) current_node%label
        WRITE (UNIT=out_unit) current_node%x0(1:nd)
        WRITE (UNIT=out_unit) current_node%ngroup
        WRITE (UNIT=out_unit) current_node%dof_status(1:nd)
        WRITE (UNIT=out_unit) current_node%u0(1:nd)
        IF (analys(1:7) == 'DYNAMIC') THEN
            WRITE (UNIT=out_unit) current_node%v0(1:nd)
            WRITE (UNIT=out_unit) current_node%a0(1:nd)
        END IF
        WRITE (UNIT=out_unit) current_node%temp0
        WRITE (UNIT=out_unit) current_node%fint0(1:nd)
        WRITE (UNIT=out_unit) current_node%force_flag
        IF (current_node%force_flag) WRITE (UNIT=out_unit) current_node%force0(1:nd)
        current_node => current_node%next
    END DO

    ! Write coupled sets
    WRITE (UNIT=out_unit) ncoupled
    current_set => first_coupled_set
    DO WHILE (ASSOCIATED(current_set))
        WRITE (UNIT=out_unit) current_set%label
        WRITE (UNIT=out_unit) current_set%forced
        WRITE (UNIT=out_unit) current_set%u_forced
        WRITE (UNIT=out_unit) current_set%u
        IF (analys(1:7) == 'DYNAMIC') THEN
            WRITE (UNIT=out_unit) current_set%v
            WRITE (UNIT=out_unit) current_set%a
        END IF
        WRITE (UNIT=out_unit) current_set%force
        current_set => current_set%next
    END DO

    ! Write material data
    CALL mat_write_output(out_unit)

    ! Write gas cavity data
    WRITE (UNIT=out_unit) ngascav
    current_gascav => first_gascav
    DO WHILE (ASSOCIATED(current_gascav))
        WRITE (UNIT=out_unit) current_gascav%label
        WRITE (UNIT=out_unit) current_gascav%failed0
        WRITE (UNIT=out_unit) current_gascav%p0
        WRITE (UNIT=out_unit) current_gascav%mgas0
        WRITE (UNIT=out_unit) current_gascav%V0
        WRITE (UNIT=out_unit) current_gascav%VpT0
        WRITE (UNIT=out_unit) current_gascav%V_tot
        current_gascav => current_gascav%next
    END DO

    ! Output geometry
    CALL write_geometry(out_unit)

    ! Write element data
    CALL spring_write_output(out_unit)
    CALL quad4_write_output(out_unit)
    CALL hex8_write_output(out_unit)
    CALL solid1d_write_output(out_unit)
    CALL pressure1d_write_output(out_unit)
    CALL pressure2d_write_output(out_unit)
    CALL pressure3d_write_output(out_unit)
    CALL gascav1d_write_output(out_unit)
    CALL gascav2d_write_output(out_unit)
    CALL gascav3d_write_output(out_unit)
    CALL cont1d_write_output(out_unit)
    CALL cont2d_write_output(out_unit)
    CALL cont3d_write_output(out_unit)

    ! Save output time
    time_output = time0
    outwrt = .TRUE.

    END SUBROUTINE write_output
    !
END MODULE FEA_IO_frapcon



