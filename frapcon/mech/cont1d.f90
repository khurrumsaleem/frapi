MODULE cont1d
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE materials
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> 1D contact element
    !
    ! 3        4
    ! o--+  +--o--
    !    |  |
    !    |  |
    !  1 o  o 2
    !    |  |
    !    |  |
    ! o--+  +--o--
    !
    INTEGER(ipk) :: ncont1d               ! Number of CONT1D elements

    TYPE cont1d_type
        INTEGER(ipk) :: label             ! Element label
        INTEGER(ipk) :: number            ! Internal element number
        INTEGER(ipk) :: mat               ! Material label
        LOGICAL :: closed                 ! Flag for gap status. True = Contact, False = Open Gap
        LOGICAL :: closed0                ! Explicit value for closed
        LOGICAL :: stick                  ! Flag for tangential sticking status. True = Fixed Contact, False = Slipping
        LOGICAL :: stick0                 ! Explicit value for stick
        INTEGER(ipk) :: node_labels(4)    ! Element node labels
        INTEGER(ipk) :: node_numbers(4)   ! Internal node numbering
        REAL(r8k) :: gap                  ! Gap Function or shortest distance from contactor node to the target surface
        REAL(r8k) :: gap0                 ! Explicit value for gap
        REAL(r8k) :: dz                   ! Difference in elevations at first contact
        REAL(r8k) :: dz0                  ! Explicit value for dz 
        REAL(r8k) :: pfact(2)             ! Penalty factor
        TYPE(cont1d_type), POINTER :: next
    END TYPE cont1d_type

    TYPE(cont1d_type), POINTER :: first_cont1d, last_cont1d
    !
    CONTAINS
    !
    SUBROUTINE cont1d_init()
    !>@brief
    !> Initialize cont1d database
    IMPLICIT NONE

    ! Deallocate, iff databese already exists
    CALL cont1d_deallocate()

    ! Initialize pointers and counters
    ncont1d = 0
    first_cont1d => NULL()
    last_cont1d => NULL()

    END SUBROUTINE cont1d_init
    !
    !
    !
    SUBROUTINE cont1d_create (label, mat, nodes)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new element
    !
    INTEGER(ipk), INTENT(IN) :: label, mat, nodes(4)
    INTEGER(ipk) :: nfound, in, jn
    TYPE(cont1d_type), POINTER :: new_cont1d, current

    ncont1d = ncont1d + 1
    ALLOCATE(new_cont1d)
    new_cont1d%label = label
    new_cont1d%number = ncont1d
    new_cont1d%mat = mat
    CALL mat_create(mat)

    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 4
            IF (nodes(jn) == node_labels(in)) THEN
                new_cont1d%node_labels(jn) = nodes(jn)
                new_cont1d%node_numbers(jn) = in
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 4) EXIT
    END DO

    ! Check whether all element nodes were found in node database
    IF (nfound /= 4) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR incompatible NODES and CONT1D data. Code stopped in subroutine cont1d_create.'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Check that the element label doesn't already exist
    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) THEN
            WRITE (ounit,FMT='(/,A,I0,A,/)') 'ERROR CONT1D ',label,' has been defined twice'
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF
        current => current%next
    END DO

    ! Initialize CONT1D variables
    new_cont1d%closed0 = .FALSE.
    new_cont1d%stick0 = .FALSE.
    new_cont1d%dz0 = x(2,new_cont1d%node_numbers(3)) - x(2,new_cont1d%node_numbers(4))
    new_cont1d%next => NULL()

    ! Check for contact conditions
    IF (x(1,new_cont1d%node_numbers(1)) >= x(1,new_cont1d%node_numbers(2))) new_cont1d%closed0 = .TRUE.

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add element to the CONT1D database
    IF (.NOT. ASSOCIATED(first_cont1d)) THEN
        first_cont1d => new_cont1d
        last_cont1d => new_cont1d
    ELSE
        last_cont1d%next => new_cont1d
        last_cont1d => new_cont1d
    END IF

    END SUBROUTINE cont1d_create
    !
    !
    !
    SUBROUTINE cont1d_sparse_matrix()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for cont1d elements
    !
    TYPE(cont1d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(4)

    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        dofnum(1) = dof_number(1,current%node_numbers(1))
        dofnum(2) = dof_number(1,current%node_numbers(2))
        dofnum(3) = dof_number(2,current%node_numbers(3))
        dofnum(4) = dof_number(2,current%node_numbers(4))
        CALL sparse_matrix_add_element(4,dofnum)
        current => current%next
    END DO

    END SUBROUTINE cont1d_sparse_matrix
    !
    !
    !
    SUBROUTINE cont1d_fint()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Contact forces
    !
    TYPE(cont1d_type), POINTER :: current
    REAL(r8k) :: Fn, Ft, frcoef
    REAL(r8k), POINTER :: pfact(:)

    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        ! Assign pointers
        pfact => current%pfact(1:2)

        ! Gap
        current%gap = x(1,current%node_numbers(2))-x(1,current%node_numbers(1))

        ! Difference in elevations
        current%dz = x(2,current%node_numbers(3)) - x(2,current%node_numbers(4))

        ! Penalty factors
        pfact(1) = mat_par(current%mat,tref,'PENALTY ')
        pfact(2) = mat_par(current%mat,tref,'PENALTY2')
        IF (pfact(2) < 1.0_r8k) pfact(2) = pfact(1)
       

        ! Check for opening of the gap
        current%closed = .FALSE.
        current%stick = .FALSE.
        IF (current%gap <= 0.0_r8k) THEN
            ! Contact flag
            current%closed = .TRUE.

            ! Normal penalty
            Fn = pfact(1) * current%gap
            !Fint(1,current%node_numbers(1)) = Fint(1,current%node_numbers(1)) - Fn
            !Fint(1,current%node_numbers(2)) = Fint(1,current%node_numbers(2)) + Fn
            Fext(1,current%node_numbers(1)) = Fext(1,current%node_numbers(1)) + Fn
            Fext(1,current%node_numbers(2)) = Fext(1,current%node_numbers(2)) - Fn

            IF (current%closed0) THEN

                ! Sticking flag
                current%stick = .TRUE.

                ! Friction coefficient
                frcoef = mat_par(current%mat,tref,'FRCOEF  ')

                ! Tangential force at axial node
                Ft = pfact(2)*(current%dz - current%dz0)

                ! Check for frictional sliding
                IF (ABS(Ft) > frcoef * ABS(Fn)) THEN
                    current%stick = .FALSE.
                    Ft = SIGN(frcoef * Fn,Ft)
                END IF

                ! Axial forces
                !Fint(2,current%node_numbers(3)) = Fint(2,current%node_numbers(3)) + Ft
                !Fint(2,current%node_numbers(4)) = Fint(2,current%node_numbers(4)) - Ft
                Fext(2,current%node_numbers(3)) = Fext(2,current%node_numbers(3)) - Ft
                Fext(2,current%node_numbers(4)) = Fext(2,current%node_numbers(4)) + Ft
            END IF
        END IF
        current => current%next
    END DO

    END SUBROUTINE cont1d_fint
    !
    !
    !
    SUBROUTINE cont1d_stiff()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the CONT1D elements
    !
    TYPE(cont1d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(2), i, j
    REAL(r8k) :: Ke(2,2), value_ij, frcoef, dir

    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        IF (current%closed) THEN
            Ke(1,1) =  current%pfact(1)
            Ke(1,2) = -current%pfact(1)
            Ke(2,1) = -current%pfact(1)
            Ke(2,2) =  current%pfact(1)
            dofnum(1:2) = dof_number(1,current%node_numbers(1:2))
            CALL sparse_matrix_place_element (2, dofnum, Ke)

            ! Friction coefficient
            frcoef = mat_par(current%mat,tref,'FRCOEF  ')

            Ke(1,1) =  current%pfact(2)
            Ke(1,2) = -current%pfact(2)
            Ke(2,1) = -current%pfact(2)
            Ke(2,2) =  current%pfact(2)

            IF (current%closed0) THEN
                IF (.NOT. current%stick) THEN
                    dir = current%dz - current%dz0
                    value_ij = SIGN(frcoef * current%pfact(1), dir)
                    symmetric_matrix = .FALSE.
                    i = dof_number(2,current%node_numbers(3))
                    j = dof_number(1,current%node_numbers(1))
                    CALL sparse_matrix_place_value (i, j, value_ij)
                    i = dof_number(2,current%node_numbers(3))
                    j = dof_number(1,current%node_numbers(2))
                    CALL sparse_matrix_place_value (i, j, -value_ij)
                    i = dof_number(2,current%node_numbers(4))
                    j = dof_number(1,current%node_numbers(1))
                    CALL sparse_matrix_place_value (i, j, -value_ij)
                    i = dof_number(2,current%node_numbers(4))
                    j = dof_number(1,current%node_numbers(2))
                    CALL sparse_matrix_place_value (i, j, value_ij)
                ELSE
                    dofnum(1:2) = dof_number(2,current%node_numbers(3:4))
                    CALL sparse_matrix_place_element (2, dofnum, Ke)
                END IF
            END IF
        END IF
        current => current%next
    END DO

    END SUBROUTINE cont1d_stiff
    !
    !
    !
    SUBROUTINE cont1d_update()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update contact condition
    !
    TYPE(cont1d_type), POINTER :: current
    REAL(r8k) :: dir, frcoef

    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        IF (.NOT. current%closed0) THEN
            current%dz0 = current%dz
        ELSE IF ((current%closed) .AND. (.NOT. current%stick)) THEN
            frcoef = mat_par(current%mat,tref,'FRCOEF  ')
            dir =  current%dz -  current%dz0
            current%dz0 = current%dz + SIGN(1.0_r8k, dir) * frcoef * current%gap
        END IF
        current%closed0 = current%closed
        current%stick0 = current%stick
        current%gap0 = current%gap
        current => current%next
    END DO

    END SUBROUTINE cont1d_update
    !
    !
    !
    SUBROUTINE cont1d_write_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write output to a file in unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(cont1d_type), POINTER :: current

    ! Output contact elements
    WRITE (UNIT=nunit) ncont1d
    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%mat
        WRITE (UNIT=nunit) current%node_labels
        WRITE (UNIT=nunit) current%node_numbers
        WRITE (UNIT=nunit) current%closed0
        WRITE (UNIT=nunit) current%stick0
        WRITE (UNIT=nunit) current%gap0
        WRITE (UNIT=nunit) current%dz0
        current => current%next
    END DO

    END SUBROUTINE cont1d_write_output
    !
    !
    !
    SUBROUTINE cont1d_read_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(cont1d_type), POINTER :: current, previous

    READ(UNIT=nunit,ERR=20,END=20) ncont1d

    current => first_cont1d
    DO i = 1, ncont1d
        IF (.NOT. ASSOCIATED(current)) THEN
            ALLOCATE(current)
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_cont1d)) THEN
                first_cont1d => current
                last_cont1d => current
            ELSE
                last_cont1d%next => current
                last_cont1d => current
            END IF
        END IF
        READ(UNIT=nunit,ERR=20,END=20) current%label
        READ(UNIT=nunit,ERR=20,END=20) current%mat
        READ(UNIT=nunit,ERR=20,END=20) current%node_labels
        READ(UNIT=nunit,ERR=20,END=20) current%node_numbers
        READ(UNIT=nunit,ERR=20,END=20) current%closed0
        READ(UNIT=nunit,ERR=20,END=20) current%stick0
        READ(UNIT=nunit,ERR=20,END=20) current%gap0
        READ(UNIT=nunit,ERR=20,END=20) current%dz0
        last_cont1d => current
        current => current%next
    END DO

    ! Remove excess element entries from the database
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_cont1d)) last_cont1d%next => NULL()
    IF (ncont1d == 0) THEN
        first_cont1d => NULL()
        last_cont1d => NULL()
    END IF

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading CONT1D database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE cont1d_read_output
    !
    !
    !
    SUBROUTINE cont1d_deallocate()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate cont1d database
    !
    TYPE(cont1d_type), POINTER :: current,previous

    current => first_cont1d
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE cont1d_deallocate
    !
END MODULE cont1d

