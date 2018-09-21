MODULE sparse_matrix_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE sparse_direct_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Matrix calculations for symmetric or non symmetric sparse matrices with a symmetric non zero structure
    !
    ! Sparse matrix is stored using row major storage scheme
    !
    ! [ a11   0 a13   0   0 ]
    ! [   0 a22   0   0 a25 ]
    ! [ a31   0 a33 a34   0 ]
    ! [   0   0 a43 a44   0 ]
    ! [   0 a52   0   0 a55 ]
    !
    ! nvalues   = 11
    ! values    = [a11, a13, a22, a25, a31, a33, a34, a43, a44, a52, a55]
    ! columns   = [ 1, 3, 2, 5, 1, 3, 4, 3, 4, 2, 5]
    ! rowindex  = [ 1, 3, 5, 8, 10, 12]
    ! diagindex = [ 1, 3, 6, 9, 11, 12]
    !
    INTEGER(ipk) :: neq ! Number of equations in linear system
    INTEGER(ipk) :: nvalues ! Size of the storage array
    INTEGER(ipk), ALLOCATABLE :: columns(:),rowindex(:),diagindex(:) ! Pointer vectors
    REAL(r8k), ALLOCATABLE :: values(:) ! Storage array for the sparse
                                            ! matrix
    LOGICAL :: linit ! True If the sparse direct solver has been initialized

    TYPE row_number
        INTEGER(ipk) :: number
        TYPE(row_number), POINTER :: next
    END TYPE row_number

    TYPE(row_number), POINTER :: row_first(:) ! First nonzero at each row

    !PUBLIC :: sparse_matrix_init,sparse_matrix_add_member, &
    !     sparse_matrix_add_element,sparse_matrix_storage_form, &
    !     sparse_matrix_zero_values,sparse_matrix_place_value, &
    !     sparse_matrix_place_element, &
    !PRIVATE :: neq,nvalues,columns,rowindex,values,row_number,row_first
    !
    CONTAINS
    !
    SUBROUTINE sparse_matrix_init(n)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialization of sparse matrix solver
    INTEGER(ipk), INTENT(IN) :: n ! Size of the linear system
    INTEGER(ipk) :: i

    ! Deallocate storage If it already exists
    CALL sparse_matrix_deallocate()

    ! Set size of the linear system
    neq = n

    ! Initialize sparse matrix storage
    ALLOCATE(row_first(neq))
    DO i = 1, neq
        row_first(i) % number = 0
        row_first(i) % next => NULL()
    END DO
    !
    END SUBROUTINE sparse_matrix_init
    !
    !
    !
    SUBROUTINE sparse_matrix_add_member (icol, irow)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    ! Add a nonzero member for the row
    INTEGER(ipk), INTENT(IN) :: &
            icol, & ! column number to be added to the linked list
            irow ! Row number
    TYPE(row_number), POINTER :: &
            first  ! Pointer to the first nonzero of the row
    TYPE(row_number), POINTER :: new_member,current,next

    first => row_first(irow)

    ! new member is the first member for the current row
    IF (first % number == 0) THEN
        first % number = icol
    ELSE IF (first % number > icol) THEN
        ALLOCATE(new_member)
        new_member % number = first % number
        new_member % next => first % next
        first % number = icol
        first % next => new_member

    ! Add new member to the current row so that ascending order remains
    ELSE
        current => first
        DO WHILE (current % number < icol)
            next => current % next
            IF (.NOT. ASSOCIATED(next)) THEN
                ALLOCATE(new_member)
                new_member % number = icol
                new_member % next => NULL()
                current % next => new_member
            ELSE IF (next % number > icol) THEN
                ALLOCATE(new_member)
                new_member % number = icol
                new_member % next => next
                current % next => new_member
            END IF
            current => current % next
        END DO
    END IF
    !
    END SUBROUTINE sparse_matrix_add_member
    !
    !
    !
    SUBROUTINE sparse_matrix_add_element (en, numbers)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Add element to the sparse matrix storage
    INTEGER(ipk), INTENT(IN) :: &
            en, & ! Number of DOFs in the element
            numbers(:) ! DOF numbering in element (nonpositive values are considered to be fixed DOFs)
    INTEGER(ipk) :: i, j, inumber, jnumber

    DO i = 1, en
        inumber = numbers(i)
        IF (inumber > 0) THEN
            DO j = 1, en
                jnumber = numbers(j)
                IF (jnumber > 0) THEN
                CALL sparse_matrix_add_member(jnumber,inumber)
                END IF
            END DO
        END IF
    END DO

    END SUBROUTINE sparse_matrix_add_element
    !
    !
    !
    SUBROUTINE sparse_matrix_storage_form()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Print out the pointer vectors from linked list to the INTEGER arrays
    INTEGER(ipk) :: i, j, ig
    TYPE(row_number), POINTER :: current,next

    ! Initialization is needed for the solver
    linit = .FALSE.

    ! Size of the storage array
    nvalues = 0
    DO i = 1, neq
        current => row_first(i)
        DO WHILE (ASSOCIATED(current))
            nvalues = nvalues + 1
            current => current % next
        END DO
    END DO

    ! ALLOCATE pointer vectors
    IF (ALLOCATED(columns)) DEALLOCATE(columns)
    IF (ALLOCATED(rowindex)) DEALLOCATE(rowindex)
    IF (ALLOCATED(diagindex)) DEALLOCATE(diagindex)
    ALLOCATE(columns(nvalues),rowindex(neq+1),diagindex(neq+1))

    ! ALLOCATE array for the stiffness matrix storage
    IF (ALLOCATED(values)) DEALLOCATE(values)
    ALLOCATE(values(nvalues))

    ! Pointer vectors
    ig = 1
    DO i = 1, neq
        rowindex(i) = ig
        current => row_first(i)
        DO WHILE (ASSOCIATED(current))
            columns(ig) = current % number
            ig = ig + 1
            current => current % next
        END DO
    END DO
    rowindex(neq+1) = ig

    ! Diagonal index
    diagindex = 0
    DO i = 1, neq
        DO j = rowindex(i),rowindex(i+1)-1
            IF (columns(j) == i) diagindex(i) = j
        END DO
    END DO
    diagindex(neq+1) = rowindex(neq+1)

    ! Deallocate linked list
    DO i = 1, neq
        current => row_first(i) % next
        DO WHILE (ASSOCIATED(current))
            next => current % next
            DEALLOCATE(current)
            current => next
        END DO
    END DO

    DEALLOCATE(row_first)

    IF (.NOT. quiet) WRITE (ounit,FMT='(/,A,F0.2,A,/)') &
         'Sparse matrix storage created with total size of ',nvalues*r8k*1.0/1024**2,' MB'
    !
    END SUBROUTINE sparse_matrix_storage_form
    !
    !
    !
    SUBROUTINE sparse_matrix_zero_values()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    ! Zero the values in sparse matrix storage
    !
    values = 0.0_r8k
    !
    END SUBROUTINE sparse_matrix_zero_values
    !
    !
    !
    FUNCTION sparse_matrix_pointer(i,j) RESULT (ip)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Return pointer to the sparse matrix storage
    !
    INTEGER(ipk) :: i, j, ip
    !
    IF ((i == 0) .OR. (j == 0)) THEN
        ip = 0
        RETURN
    END IF
    !
    DO ip = rowindex(i), rowindex(i+1)-1
        IF (columns(ip) == j) RETURN
    END DO
    !
    ip = 0
    !
    END FUNCTION sparse_matrix_pointer
    !
    !
    !
    SUBROUTINE sparse_matrix_place_value (i, j, value_ij)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Add value to the sparse matrix storage
    INTEGER(ipk), INTENT(IN) :: &
            i, & ! Row index
            j ! Column index
    REAL(r8k), INTENT(IN) :: &
            value_ij ! Value to be added to j:th term of the i:th row
    INTEGER(ipk) :: k, row_start, row_end

    ! Fixed DOF
    IF ((i <= 0) .OR. (j <= 0))  RETURN 

    ! Lower triangle
    IF (i > j) THEN
        row_start = rowindex(i)
        row_end = diagindex(i) - 1
    ! Diagonal and upper triangle
    ELSE
        row_start = diagindex(i)
        row_end = rowindex(i + 1) - 1
    END IF
    !
    DO k = row_start, row_end
        IF (columns(k) == j) THEN
            values(k) = values(k) + value_ij
            RETURN
        END IF
    END DO
    !
    lerror = .TRUE.
    WRITE (ounit,FMT='(A)') 'ERROR tried to access nondefined element in sparse matrix'
    CALL nlfemp_stop(0)
    !
    END SUBROUTINE sparse_matrix_place_value
    !
    !
    !
    SUBROUTINE sparse_matrix_place_element (ne, dofnum, K_e)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Place element matrix to the global matrix storage
    !
    ! Input
    ! 
    ! ne     - Number of DOFs in element
    ! dofnum - DOF numbering in element
    !
    ! Output
    !
    ! K_e    - Element stiffness matrix
    !
    INTEGER(ipk), INTENT(IN) :: ne, dofnum(ne)
    REAL(r8k) :: K_e(ne,ne)
    INTEGER(ipk) :: i, j
    !
    DO i = 1, ne
        DO j = 1, ne
            CALL sparse_matrix_place_value(dofnum(i), dofnum(j), K_e(i,j))
        END DO
    END DO
    !
    END SUBROUTINE sparse_matrix_place_element
    !
    !
    !
    SUBROUTINE sparse_matrix_prnmat()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Print matrix to a text file using Matrix Market FORMAT
    INTEGER(ipk) :: i,j
    !
    OPEN(UNIT=100,FILE="matrix.mtx",STATUS='UNKNOWN',FORM='FORMATTED')
    WRITE (UNIT=100,FMT='(A)') '%%MatrixMarket matrix coordinate REAL general'
    WRITE (UNIT=100,FMT='(3I8)') neq,neq,nvalues
    !
    IF (linit) THEN
        OPEN(UNIT=101,FILE="matrix-ordered.mtx", STATUS='UNKNOWN', FORM='FORMATTED')
        WRITE (UNIT=101,FMT='(A)') '%%MatrixMarket matrix coordinate REAL general'
        WRITE (UNIT=101,FMT='(3I8)') neq,neq,nvalues
    END IF
    !
    DO i = 1, neq
        DO j = rowindex(i), rowindex(i+1)-1
            WRITE (UNIT=100,FMT='(2I8,E15.5)') i,columns(j),values(j)
            IF (linit) WRITE (UNIT=101,FMT='(2I8,E15.5)') permu(i),permu(columns(j)),values(j)
        END DO
    END DO
    !
    CLOSE(UNIT=100)
    !
    CALL nlfemp_stop(0)
    !
    END SUBROUTINE sparse_matrix_prnmat
    !
    !
    !
    SUBROUTINE sparse_matrix_solve (symmetric_matrix, n, b, x)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Solution of the linear system
    !
    LOGICAL, INTENT(IN) :: symmetric_matrix   ! Flag whether matrix is symmetric
    INTEGER(ipk), INTENT(IN) :: n             ! Number of equations
    REAL(r8k), DIMENSION(n), INTENT(IN) :: b  ! Right hand side of the linear system
    REAL(r8k), DIMENSION(n), INTENT(OUT) :: x ! Solution
    LOGICAL :: lidef ! Flag for a semidefinite matrix
    ! Reordering of the matrix
    IF (.NOT. linit) THEN
        CALL sparse_direct_reorder(neq,nvalues,rowindex,diagindex,columns)
        linit = .TRUE.
    END IF

    ! Direct solution for the linear system
    IF (symmetric_matrix) THEN
        ! Matrix factoring
        CALL sparse_direct_ldlt(neq,nvalues,rowindex,diagindex,columns,values,lidef)
        ! Matrix is not positive definite
        !IF (lidef) THEN
        !   lerror = .TRUE.
        !   RETURN
        !END IF
        ! Solution
        CALL sparse_direct_ldltsol(neq,b,x)
    ELSE  !  Solution for nonsymmetric matrix
        ! Matrix factoring
        CALL sparse_direct_lu(neq,nvalues,rowindex,diagindex,columns,values,lidef)
        ! Matrix is not positive definite
        !IF (lidef) THEN
        !   lerror = .TRUE.
        !   RETURN
        !END IF
        ! Solution
        CALL sparse_direct_lusol(neq,b,x)
    END IF
    ! Print out stiffness matrix
    IF (lprnmat) CALL sparse_matrix_prnmat()
    !
    END SUBROUTINE sparse_matrix_solve
    !
    !
    !
    SUBROUTINE sparse_matrix_deallocate()
    IMPLICIT NONE
    !>@brief
    !> Deallocate all variables
    !
    CALL sparse_direct_deallocate()
    !
    IF (ALLOCATED(values)) DEALLOCATE(values)
    IF (ALLOCATED(columns)) DEALLOCATE(columns)
    IF (ALLOCATED(rowindex)) DEALLOCATE(rowindex)
    IF (ALLOCATED(diagindex)) DEALLOCATE(diagindex)
    !
    END SUBROUTINE sparse_matrix_deallocate
    !
END MODULE sparse_matrix_frapcon



