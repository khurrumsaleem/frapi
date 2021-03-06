MODULE pressure2d_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE sparse_matrix_frapcon
    IMPLICIT NONE
    !>@brief
    !> Axisymmetric pressure boundary element
    ! Nodes 1 and 2 must be connected to the same solid element
    !
    ! 1
    ! o<-- p(1)
    ! |<---
    ! |<----
    ! |<-----
    ! |<------
    ! o<------- p(2)
    ! 2
    !
    !
    INTEGER(ipk) :: npressure2d
    !
    TYPE pressure2d_type
        INTEGER(ipk) :: label
        INTEGER(ipk) :: node_labels(2)
        INTEGER(ipk) :: node_numbers(2)
        REAL(r8k) :: p0(2)
        REAL(r8k) :: p(2)
        REAL(r8k) :: p_end(2)
        TYPE(pressure2d_type), POINTER :: next
    END TYPE pressure2d_type
    !
    TYPE(pressure2d_type), POINTER :: first_pressure2d,last_pressure2d
    !
    REAL(r8k), PRIVATE :: N(2,2),wgt(2),xi(2)
    !
    CONTAINS
    !
    SUBROUTINE pressure2d_init()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pressure2d database
    !
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate, If databese already exists
    CALL pressure2d_deallocate()

    ! Initialize pointers and counters
    npressure2d = 0
    first_pressure2d => NULL()
    last_pressure2d => NULL()

    ! Integration points and weight factors
    wgt = 0.5_r8k
    tmp = 1.0_r8k / SQRT(3.0_r8k)
    xi(1) = 0.5_r8k * (1.0_r8k - tmp)
    xi(2) = 0.5_r8k * (1.0_r8k + tmp)

    DO ig = 1,2
        N(1,ig) = 1.0_r8k - xi(ig)
        N(2,ig) = xi(ig)
    END DO

    END SUBROUTINE pressure2d_init
    !
    !
    !
    SUBROUTINE pressure2d_setpress (label, p)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Set pressures for the set label
    !
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k) :: p
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) current%p_end(1:2) = p
        current => current%next
    END DO

    END SUBROUTINE pressure2d_setpress
    !
    !
    !
    SUBROUTINE pressure2d_updval (label, nodes, p)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update pressure values
    !
    INTEGER(ipk), INTENT(IN) :: label,nodes(2)
    REAL(r8k), INTENT(IN) :: p(2)
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) THEN
            IF (current%node_labels(1) == nodes(1)) THEN
                IF (current%node_labels(2) == nodes(2)) THEN
                current%p_end = p
                EXIT
                END IF
            END IF
        END IF
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) CALL pressure2d_create(label,nodes,p)

    END SUBROUTINE pressure2d_updval
    !
    !
    !
    SUBROUTINE pressure2d_create (label, nodes, p)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new PRESSURE2D element
    !
    INTEGER(ipk), INTENT(IN) :: label,nodes(2)
    REAL(r8k), INTENT(IN) :: p(2)
    INTEGER(ipk) :: nfound,in,jn
    TYPE(pressure2d_type), POINTER :: new_pressure2d

    ! ALLOCATE new element
    npressure2d = npressure2d + 1
    ALLOCATE(new_pressure2d)

    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 2
            IF (nodes(jn) == node_labels(in)) THEN
                new_pressure2d%node_labels(jn) = nodes(jn)
                new_pressure2d%node_numbers(jn) = in
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 2) EXIT
    END DO

    ! Check whether all element nodes were found in node database
    IF (nfound /= 2) THEN
        WRITE (ounit,FMT='(/,A,/)') &
            'ERROR incompatible NODES and PRESSURE2D data'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Initialize PRESSURE2D variables
    new_pressure2d%label = label
    new_pressure2d%p0(1:2) = 0.0_r8k
    new_pressure2d%p_end(1:2) = p(1:2)
    new_pressure2d%next => NULL()

    ! Add new element to the database
    IF (.NOT. ASSOCIATED(first_pressure2d)) THEN
        first_pressure2d => new_pressure2d
        last_pressure2d => new_pressure2d
    ELSE
        last_pressure2d%next => new_pressure2d
        last_pressure2d => new_pressure2d
    END IF

    END SUBROUTINE pressure2d_create
    !
    !
    !
    SUBROUTINE pressure2d_delete(label)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Delete element from the database
    !
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(pressure2d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the database is empty
    IF (.NOT. ASSOCIATED(first_pressure2d)) RETURN

    ! Mark first memeber to be deleted in database
    IF (first_pressure2d%label == label) THEN
        lfound = .TRUE.
        tobedeleted => first_pressure2d
        first_pressure2d => first_pressure2d%next
    END IF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_pressure2d
    DO WHILE ((ASSOCIATED(current)) .AND. (.NOT. lfound))
        IF (current%label == label) THEN
            lfound = .TRUE.
            previous%next => current%next
            tobedeleted => current
        END IF
        previous => current
        current => current%next
    END DO

    ! Set the pointer to the last PRESSURE2D element
    last_pressure2d => NULL()
    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        last_pressure2d => current
        current => current%next
    END DO
    
    ! Deallocate PRESSURE2D element
    IF (lfound) THEN
        npressure2d = npressure2d - 1
        dof_numbering = .TRUE.
        DEALLOCATE(tobedeleted)
    END IF

    END SUBROUTINE pressure2d_delete
    !
    !
    !
    SUBROUTINE pressure2d_fext()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate 2D pressure load contribution to the external force vector
    !
    TYPE(pressure2d_type), POINTER :: current
    INTEGER(ipk) :: ig
    REAL(r8k) :: r(2),z(2),rg,pg
    REAL(r8k), POINTER :: p(:)

    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        p => current%p(1:2)
        p = current%p0 + dload*(current%p_end - current%p0)
        r(1) = x(1,current%node_numbers(1))
        r(2) = x(1,current%node_numbers(2))
        z(1) = x(2,current%node_numbers(1))
        z(2) = x(2,current%node_numbers(2))
        DO ig = 1,2
            pg = N(1,ig)*p(1) + N(2,ig)*p(2)
            rg = N(1,ig)*r(1) + N(2,ig)*r(2)
            Fext(1,current%node_numbers(1)) = Fext(1,current%node_numbers(1)) + &
                wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
            Fext(2,current%node_numbers(1)) = Fext(2,current%node_numbers(1)) + &
                wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
            Fext(1,current%node_numbers(2)) = Fext(1,current%node_numbers(2)) + &
                wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
            Fext(2,current%node_numbers(2)) = Fext(2,current%node_numbers(2)) + &
                wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
        END DO
        current => current%next
    END DO

    END SUBROUTINE pressure2d_fext
    !
    !
    !
    SUBROUTINE pressure2d_stiff()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the pressure loads
    !
    TYPE(pressure2d_type), POINTER :: current
    INTEGER(ipk) :: ig,dofnum(4)
    REAL(r8k) :: r(2),z(2),pg,rg,Ke(4,4)
    REAL(r8k), POINTER :: p(:)

    IF (pressure_matrix) THEN

        IF (npressure2d > 0) symmetric_matrix = .FALSE.

        current => first_pressure2d
        DO WHILE (ASSOCIATED(current))
            p => current%p(1:2)
            p = current%p0 + dload*(current%p_end - current%p0)
            r(1) = x(1,current%node_numbers(1))
            r(2) = x(1,current%node_numbers(2))
            z(1) = x(2,current%node_numbers(1))
            z(2) = x(2,current%node_numbers(2))
            Ke = 0.0_r8k
            DO ig = 1,2
                pg = N(1,ig)*p(1) + N(2,ig)*p(2)
                rg = N(1,ig)*r(1) + N(2,ig)*r(2)
                Ke(1,1) = Ke(1,1) - wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*N(1,ig)
                Ke(1,2) = Ke(1,2) + wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg
                Ke(1,3) = Ke(1,3) - wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*N(2,ig)
                Ke(1,4) = Ke(1,4) - wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg
                Ke(2,1) = Ke(2,1) - wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg - &
                    wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*N(1,ig)
                Ke(2,3) = Ke(2,3) + wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg - &
                    wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*N(2,ig)
                Ke(3,1) = Ke(3,1) - wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*N(1,ig)
                Ke(3,2) = Ke(3,2) + wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg
                Ke(3,3) = Ke(3,3) - wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*N(2,ig)
                Ke(3,4) = Ke(3,4) - wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg
                Ke(4,1) = Ke(4,1) - wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg - &
                    wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*N(1,ig)
                Ke(4,3) = Ke(4,3) + wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg + &
                    wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*N(2,ig)
            END DO

            ! Place element matrix in the global matrix
            dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
            dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
            CALL sparse_matrix_place_element(4,dofnum,Ke)
            current => current%next
        END DO
    END IF

    END SUBROUTINE pressure2d_stiff
    !
    !
    !
    SUBROUTINE pressure2d_update()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update explicit values
    !
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        current%p0 = current%p
        current => current%next
    END DO

    END SUBROUTINE pressure2d_update
    !
    !
    !
    SUBROUTINE pressure2d_write_output(nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write pressure data
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(pressure2d_type), POINTER :: current

    WRITE (UNIT=nunit) npressure2d
    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%node_labels
        WRITE (UNIT=nunit) current%node_numbers
        WRITE (UNIT=nunit) current%p0
        current => current%next
    END DO

    END SUBROUTINE pressure2d_write_output
    !
    !
    !
    SUBROUTINE pressure2d_read_output(nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(pressure2d_type), POINTER :: current,previous

    READ(UNIT=nunit,ERR=20,End=20) npressure2d

    current => first_pressure2d
    DO i = 1, npressure2d
        IF (.NOT. ASSOCIATED(current)) THEN
            ALLOCATE(current)
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_pressure2d)) THEN
                first_pressure2d => current
                last_pressure2d => current
            ELSE
                last_pressure2d%next => current
                last_pressure2d => current
            END IF
        END IF
        READ(UNIT=nunit,ERR=20,End=20) current%label
        READ(UNIT=nunit,ERR=20,End=20) current%node_labels
        READ(UNIT=nunit,ERR=20,End=20) current%node_numbers
        READ(UNIT=nunit,ERR=20,End=20) current%p0
        current%p_end = current%p0
        last_pressure2d => current
        current => current%next
    END DO

    ! Remove excess element entries from the database
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_pressure2d)) last_pressure2d%next => NULL()
    IF (npressure2d == 0) THEN
        first_pressure2d => NULL()
        last_pressure2d => NULL()
    END IF

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading PRESS2D database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE pressure2d_read_output
    !
    !
    !
    SUBROUTINE pressure2d_deallocate()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate pressure2d database
    !
    TYPE(pressure2d_type), POINTER :: current,previous

    current => first_pressure2d
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    !
    END SUBROUTINE pressure2d_deallocate
    !
END MODULE pressure2d_frapcon



