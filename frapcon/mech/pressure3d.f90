MODULE pressure3d
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> 3D pressure boundary element
    !
    !              xi(2)
    !             ^
    !     N4      |      N3            xi(1),xi(2) ...... element coordinates
    !       o-----+-----o              N1,N2,N3,N4 ...... element nodes
    !       | G4  |  G3 |              G1,G2,G3,G4 ...... Gauss points or GPs
    !       |  x  |  x  |              S1,S2,S3,S4 ...... Element boundaries
    !       |     |     |
    !       |     +-----+----> xi(1)
    !       |           |
    !       |  x     x  |
    !       | G1     G2 |
    !       o-----------o
    !     N1             N2
    !
    INTEGER(ipk) :: npressure3d

    TYPE pressure3d_type
        INTEGER(ipk) :: label
        INTEGER(ipk) :: node_labels(4)
        INTEGER(ipk) :: node_numbers(4)
        REAL(r8k) :: p0(4)
        REAL(r8k) :: p(4)
        REAL(r8k) :: p_end(4)
        TYPE(pressure3d_type), POINTER :: next
    END TYPE pressure3d_type

    TYPE(pressure3d_type), POINTER :: first_pressure3d, last_pressure3d

    REAL(r8k), PRIVATE :: N(4,4), wgt(4), xi(2,4), dNdxi(2,4,4)
    !
    CONTAINS
    !
    SUBROUTINE pressure3d_init()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pressure3d database
    !
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate existing database
    CALL pressure3d_deallocate()

    ! GP element coordinates and weight factors
    wgt(1:4) = 1.0_r8k
    tmp = 1.0_r8k / SQRT(3.0_r8k)
    xi(1:2,1) = (/ -tmp, -tmp /)
    xi(1:2,2) = (/  tmp, -tmp /)
    xi(1:2,3) = (/  tmp,  tmp /)
    xi(1:2,4) = (/ -tmp,  tmp /)

    DO ig = 1, 4
        ! Shape functions at the gauss point
        N(1,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 4.0_r8k
        N(2,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 4.0_r8k
        N(3,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 4.0_r8k
        N(4,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 4.0_r8k

        ! Derivatives of the shape functions
        dNdxi(1,1,ig) = -(1.0_r8k - xi(2,ig)) / 4.0_r8k
        dNdxi(1,2,ig) =  (1.0_r8k - xi(2,ig)) / 4.0_r8k
        dNdxi(1,3,ig) =  (1.0_r8k + xi(2,ig)) / 4.0_r8k
        dNdxi(1,4,ig) = -(1.0_r8k + xi(2,ig)) / 4.0_r8k
        dNdxi(2,1,ig) = -(1.0_r8k - xi(1,ig)) / 4.0_r8k
        dNdxi(2,2,ig) = -(1.0_r8k + xi(1,ig)) / 4.0_r8k
        dNdxi(2,3,ig) =  (1.0_r8k + xi(1,ig)) / 4.0_r8k
        dNdxi(2,4,ig) =  (1.0_r8k - xi(1,ig)) / 4.0_r8k
    END DO

    npressure3d = 0
    first_pressure3d => NULL()
    last_pressure3d => NULL()

    END SUBROUTINE pressure3d_init
    !
    !
    !
    SUBROUTINE pressure3d_setpress (label, p)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Set pressures for the set label
    !
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k) :: p
    TYPE(pressure3d_type), POINTER :: current

    current => first_pressure3d
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) current%p_end(1:4) = p
        current => current%next
    END DO

    END SUBROUTINE pressure3d_setpress
    !
    !
    !
    SUBROUTINE pressure3d_updval (label, nodes, p)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update pressure values
    !
    INTEGER(ipk), INTENT(IN) :: label, nodes(4)
    REAL(r8k), INTENT(IN) :: p(4)
    TYPE(pressure3d_type), POINTER :: current

    current => first_pressure3d
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

    IF (.NOT. ASSOCIATED(current)) CALL pressure3d_create(label,nodes,p)

    END SUBROUTINE pressure3d_updval
    !
    !
    !
    SUBROUTINE pressure3d_create (label, nodes, p)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new PRESSURE3D element
    !
    INTEGER(ipk), INTENT(IN) :: label,nodes(4)
    REAL(r8k), INTENT(IN) :: p(4)
    INTEGER(ipk) :: nfound,in,jn
    TYPE(pressure3d_type), POINTER :: new_pressure3d

    ! ALLOCATE new element
    npressure3d = npressure3d + 1
    ALLOCATE(new_pressure3d)

    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 4
            IF (nodes(jn) == node_labels(in)) THEN
                new_pressure3d%node_labels(jn) = nodes(jn)
                new_pressure3d%node_numbers(jn) = in
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 4) EXIT
    END DO

    ! Check whether all element nodes were found in node database
    IF (nfound /= 4) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR incompatible NODES and PRESSURE3D data'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Initialize PRESSURE3D variables
    new_pressure3d%label = label
    new_pressure3d%p0(1:4) = 0.0_r8k
    new_pressure3d%p_end(1:4) = p(1:4)
    new_pressure3d%next => NULL()

    ! Add new element to the database
    IF (.NOT. ASSOCIATED(first_pressure3d)) THEN
        first_pressure3d => new_pressure3d
        last_pressure3d => new_pressure3d
    ELSE
        last_pressure3d%next => new_pressure3d
        last_pressure3d => new_pressure3d
    END IF

    END SUBROUTINE pressure3d_create
    !
    !
    !
    SUBROUTINE pressure3d_fext()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate 2D pressure load contribution to the external force vector
    !
    TYPE(pressure3d_type), POINTER :: current
    INTEGER(ipk) :: ig, id, jd, in, jn
    REAL(r8k) :: dxdxi(3,2), cprd(3), pg
    REAL(r8k), POINTER :: p(:)

    current => first_pressure3d
    DO WHILE (ASSOCIATED(current))
        p => current%p(1:4)
        p = current%p0 + dload*(current%p_end - current%p0)

        DO ig = 1, 4
            ! Shape Function derivatives in respect to element coordinates
            DO id = 1, 3
                DO jd = 1,2
                    dxdxi(id,jd) = 0.0_r8k
                    DO in = 1, 4
                        jn = current%node_numbers(in)
                        dxdxi(id,jd) = dxdxi(id,jd) + dNdxi(jd,in,ig)*x(id,jn)
                    END DO
                END DO
            END DO

            ! Evaluate cross produc dx/dxi_1 x dx/dxi_2
            cprd(1) = dxdxi(2,1)*dxdxi(3,2) - dxdxi(2,2)*dxdxi(3,1)
            cprd(2) = dxdxi(1,2)*dxdxi(3,1) - dxdxi(1,1)*dxdxi(3,2)
            cprd(3) = dxdxi(1,1)*dxdxi(2,2) - dxdxi(1,2)*dxdxi(2,1)

            ! Pressure at integration point
            pg = 0.0_r8k
            DO in = 1, 4
                pg = pg + N(in,ig)*p(in)
            END DO

            ! Nodal forces
            DO in = 1, 4
                jn = current%node_numbers(in)
                Fext(1:3,jn) = Fext(1:3,jn) - wgt(ig)*pg*N(in,ig)*cprd(1:3)
            END DO

        END DO
        current => current%next
    END DO

    END SUBROUTINE pressure3d_fext
    !
    !
    !
    SUBROUTINE pressure3d_stiff()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the pressure loads
    !
    TYPE(pressure3d_type), POINTER :: current
    INTEGER(ipk), DIMENSION(12) :: dofnum
    REAL(r8k), DIMENSION(12,12) :: Ke
    REAL(r8k), DIMENSION(:), POINTER :: p

    IF (pressure_matrix) THEN

        IF (npressure3d > 0) symmetric_matrix = .FALSE.

        current => first_pressure3d
        DO WHILE (ASSOCIATED(current))
            p => current%p(1:4)
            Ke = 0.0_r8k

            ! Place element matrix in the global matrix
            dofnum(1:3) = dof_number(1:3,current%node_numbers(1))
            dofnum(4:6) = dof_number(1:3,current%node_numbers(2))
            dofnum(7:9) = dof_number(1:3,current%node_numbers(3))
            dofnum(10:12) = dof_number(1:3,current%node_numbers(4))
            !Call sparse_matrix_place_element(12,dofnum,Ke)
            current => current%next
        END DO
    END IF

    END SUBROUTINE pressure3d_stiff
    !
    !
    !
    SUBROUTINE pressure3d_update()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update explicit values
    !
    TYPE(pressure3d_type), POINTER :: current

    current => first_pressure3d
    DO WHILE (ASSOCIATED(current))
        current%p0 = current%p
        current => current%next
    END DO

    END SUBROUTINE pressure3d_update
    !
    !
    !
    SUBROUTINE pressure3d_write_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write pressure data
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(pressure3d_type), POINTER :: current

    WRITE (UNIT=nunit) npressure3d
    current => first_pressure3d
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%node_labels
        WRITE (UNIT=nunit) current%node_numbers
        WRITE (UNIT=nunit) current%p0
        current => current%next
    END DO

    END SUBROUTINE pressure3d_write_output
    !
    !
    !
    SUBROUTINE pressure3d_read_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(pressure3d_type), POINTER :: current,previous

    READ(UNIT=nunit,ERR=20,END=20) npressure3d

    current => first_pressure3d
    DO i = 1, npressure3d
        IF (.NOT. ASSOCIATED(current)) THEN
            ALLOCATE(current)
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_pressure3d)) THEN
                first_pressure3d => current
                last_pressure3d => current
            ELSE
                last_pressure3d%next => current
                last_pressure3d => current
            END IF
        END IF
        READ(UNIT=nunit,ERR=20,END=20) current%label
        READ(UNIT=nunit,ERR=20,END=20) current%node_labels
        READ(UNIT=nunit,ERR=20,END=20) current%node_numbers
        READ(UNIT=nunit,ERR=20,END=20) current%p0
        current%p_end = current%p0
        last_pressure3d => current
        current => current%next
    END DO

    ! Remove excess element entries from the database
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_pressure3d)) last_pressure3d%next => NULL()
    IF (npressure3d == 0) THEN
        first_pressure3d => NULL()
        last_pressure3d => NULL()
    END IF

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading PRESS3D database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE pressure3d_read_output
    !
    !
    !
    SUBROUTINE pressure3d_deallocate()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate pressure3d database
    !
    TYPE(pressure3d_type), POINTER :: current,previous

    current => first_pressure3d
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE pressure3d_deallocate
    !
END MODULE pressure3d

