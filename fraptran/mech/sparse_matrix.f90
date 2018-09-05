MODULE sparse_matrix
    USE Kinds
    USE common_parameters
    USE sparse_direct
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
  INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: columns, rowindex, diagindex ! POINTER vectors
  REAL(r8k), DIMENSION(:), ALLOCATABLE :: values ! Storage array for the sparse matrix
  LOGICAL :: linit ! True If the sparse direct solver has been initialized

  TYPE row_number
     INTEGER(ipk) :: number
     TYPE(row_number), POINTER :: next
  END TYPE row_number

  TYPE(row_number), DIMENSION(:), POINTER :: row_first ! First nonzero at each row

  !PUBLIC :: sparse_matrix_init,sparse_matrix_add_member, &
  !     sparse_matrix_add_element,sparse_matrix_storage_form, &
  !     sparse_matrix_zero_values,sparse_matrix_place_value, &
  !     sparse_matrix_place_element, &
  !PRIVATE :: neq,nvalues,columns,rowindex,values,row_number,row_first

CONTAINS
    SUBROUTINE sparse_matrix_init(n)
    ! Initialization of sparse matrix solver
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: n ! Size of the linear system
    INTEGER(ipk) :: i

    ! Deallocate storage If it already exists
    CALL sparse_matrix_deallocate()

    ! Set size of the linear system
    neq = n

    ! Initialize sparse matrix storage
    ALLOCATE(row_first(neq))
    DO i=1,neq
       row_first(i) % number = 0
       row_first(i) % next => NULL()
    ENDDO

    RETURN

  END SUBROUTINE sparse_matrix_init


    SUBROUTINE sparse_matrix_add_member(icol,irow)
    ! Add a nonzero member for the row
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         icol, & ! column number to be added to the linked list
         irow ! Row number
    TYPE(row_number), POINTER :: &
         first  ! POINTER to the first nonzero of the row
    TYPE(row_number), POINTER :: new_member,current,next

    first => row_first(irow)

    ! new member is the first member for the current row
    IF ( first % number == 0 ) THEN
       first % number = icol
    ELSE If ( first % number > icol ) THEN
       ALLOCATE(new_member)
       new_member % number = first % number
       new_member % next => first % next
       first % number = icol
       first % next => new_member

    ! Add new member to the current row so that ascEnding order remains
    ELSE
       current => first
       DO WHILE ( current % number < icol )
          next => current % next
          IF ( .NOT.ASSOCIATED(next) ) THEN
             ALLOCATE(new_member)
             new_member % number = icol
             new_member % next => NULL()
             current % next => new_member
          ELSE If ( next % number > icol ) THEN
             ALLOCATE(new_member)
             new_member % number = icol
             new_member % next => next
             current % next => new_member
          ENDIF
          current => current % next
       ENDDO
    ENDIF

    RETURN

  END SUBROUTINE sparse_matrix_add_member


    SUBROUTINE sparse_matrix_add_element(en,numbers)
    ! Add element to the sparse matrix storage
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: en ! Number of DOFs in the element
    INTEGER(ipk), DIMENSION(:), INTENT(IN) :: &
         numbers ! DOF numbering in element (nonpositive values are considered to be fixed DOFs)
    INTEGER(ipk) :: i,j,inumber,jnumber

    DO i=1,en
       inumber = numbers(i)
       IF ( inumber > 0 ) THEN
          DO j=1,en
             jnumber = numbers(j)
             IF ( jnumber > 0 ) THEN
                CALL sparse_matrix_add_member(jnumber,inumber)
             ENDIF
          ENDDO
       ENDIF
    ENDDO

    RETURN

  END SUBROUTINE sparse_matrix_add_element


    SUBROUTINE sparse_matrix_storage_form()
    ! Print out the POINTER vectors from linked list to the INTEGER(ipk) arrays
    IMPLICIT NONE
    INTEGER(ipk) :: i,j,ig
    TYPE(row_number), POINTER :: current,next

    ! Initialization is needed for the solver
    linit = .FALSE.

    ! Size of the storage array
    nvalues = 0
    DO i=1,neq
       current => row_first(i)
       DO WHILE ( ASSOCIATED(current) )
          nvalues = nvalues + 1
          current => current % next
       ENDDO
    ENDDO

    ! Allocate POINTER vectors
    IF (ALLOCATED(columns)) DEALLOCATE(columns)
    IF (ALLOCATED(rowindex)) DEALLOCATE(rowindex)
    IF (ALLOCATED(diagindex)) DEALLOCATE(diagindex)
    ALLOCATE(columns(nvalues),rowindex(neq+1),diagindex(neq+1))

    ! Allocate array for the stIffness matrix storage
    IF (ALLOCATED(values)) DEALLOCATE(values)
    ALLOCATE(values(nvalues))

    ! POINTER vectors
    ig = 1
    DO i=1,neq
       rowindex(i) = ig
       current => row_first(i)
       DO WHILE ( ASSOCIATED(current) )
          columns(ig) = current % number
          ig = ig + 1
          current => current % next
       ENDDO
    ENDDO
    rowindex(neq+1) = ig

    ! Diagonal index
    diagindex = 0
    DO i=1,neq
       DO j=rowindex(i),rowindex(i+1)-1
          IF ( columns(j) == i ) diagindex(i) = j
       ENDDO
    ENDDO
    diagindex(neq+1) = rowindex(neq+1)

    ! Deallocate linked list
    DO i=1,neq
       current => row_first(i) % next
       DO WHILE ( ASSOCIATED(current) )
          next => current % next
          DEALLOCATE(current)
          current => next
       ENDDO
    ENDDO

    DEALLOCATE(row_first)

    IF ( .NOT.quiet ) WRITE(UNIT=6,FMT='(/,A,F0.2,A,/)') &
         'Sparse matrix storage created with total size of ', &
         nvalues*r8k*1.0/1024**2,' MB'

    RETURN

  END SUBROUTINE sparse_matrix_storage_form


    SUBROUTINE sparse_matrix_zero_values()
    ! Zero the values in sparse matrix storage
    IMPLICIT NONE

    values = 0.0_r8k

    RETURN

  END SUBROUTINE sparse_matrix_zero_values


  FUNCTION sparse_matrix_POINTER(i,j) RESULT (ip)
    ! RETURN POINTER to the sparse matrix storage
    IMPLICIT NONE
    INTEGER(ipk) :: i,j,ip

    IF ( ( i == 0 ).OR.( j == 0 ) ) THEN
       ip = 0
       RETURN
    ENDIF

    DO ip=rowindex(i),rowindex(i+1)-1
       IF ( columns(ip) == j ) RETURN
    ENDDO

    ip = 0

    RETURN

  END FUNCTION sparse_matrix_POINTER


    SUBROUTINE sparse_matrix_place_value(i,j,value_ij)
    ! Add value to the sparse matrix storage
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         i, & ! Row index
         j ! Column index
    REAL(r8k), INTENT(IN) :: &
         value_ij ! Value to be added to j:th term of the i:th row
    INTEGER(ipk) :: k,row_start,row_End

    ! Fixed DOF
    IF (( i <= 0 ).OR.( j <= 0 ))  RETURN 

    ! Lower triangle
    IF ( i > j ) THEN
       row_start = rowindex(i)
       row_End = diagindex(i) - 1
    ! Diagonal and upper triangle
    ELSE
       row_start = diagindex(i)
       row_End = rowindex(i + 1) - 1
    ENDIF

    DO k=row_start,row_End
       IF ( columns(k) == j ) THEN
          values(k) = values(k) + value_ij
          RETURN
       ENDIF
    ENDDO

    lerror = .TRUE.
    WRITE(UNIT=6,FMT='(A)') &
         'ERROR tried to access nondefined element in sparse matrix'
    CALL nlfemp_stop(0)

  END SUBROUTINE sparse_matrix_place_value


    SUBROUTINE sparse_matrix_place_element(ne,dofnum,K_e)
    ! Place element matrix to the global matrix storage
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         ne, & ! Number of DOFs in element
         dofnum(ne) ! DOF numbering in element
    REAL(r8k) :: &
         K_e(ne,ne) ! Element stIffness matrix
    INTEGER(ipk) :: i,j

    DO i=1,ne
       DO j=1,ne
          CALL sparse_matrix_place_value(dofnum(i),dofnum(j),K_e(i,j))
       ENDDO
    ENDDO

    RETURN

  END SUBROUTINE sparse_matrix_place_element


    SUBROUTINE sparse_matrix_prnmat()
    ! Print matrix to a text file using Matrix Market FORMAT
    IMPLICIT NONE
    INTEGER(ipk) :: i,j

    Open (UNIT=100,FILE="matrix.mtx",STATUS='UNKNOWN',FORM='FORMATTED')
    WRITE(UNIT=100,FMT='(A)') '%%MatrixMarket matrix coordinate Real general'
    WRITE(UNIT=100,FMT='(3I8)') neq,neq,nvalues

    IF ( linit ) THEN
       Open (UNIT=101,FILE="matrix-ordered.mtx",STATUS='UNKNOWN', &
            FORM='FORMATTED')
       WRITE(UNIT=101,FMT='(A)') &
            '%%MatrixMarket matrix coordinate Real general'
       WRITE(UNIT=101,FMT='(3I8)') neq,neq,nvalues
    ENDIF

    DO i=1,neq
       DO j=rowindex(i),rowindex(i+1)-1
          WRITE(UNIT=100,FMT='(2I8,E15.5)') i,columns(j),values(j)
          IF ( linit ) WRITE(UNIT=101,FMT='(2I8,E15.5)') &
               permu(i),permu(columns(j)),values(j)
       ENDDO
    ENDDO

    CLOSE(UNIT=100)

    CALL nlfemp_stop(0)

  END SUBROUTINE sparse_matrix_prnmat


    SUBROUTINE sparse_matrix_solve(symmetric_matrix,n,b,x)
    ! Solution of the linear system
    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: &
         symmetric_matrix ! Flag whether matrix is symmetric
    INTEGER(ipk), INTENT(IN) :: &
         n ! Number of equations
    REAL(r8k), INTENT(IN) :: &
         b(n) ! Right hand side of the linear system
    REAL(r8k), INTENT(OUT) :: x(n) ! Solution
    LOGICAL :: lidef ! Flag for a semidefinite matrix

    ! Reordering of the matrix
    IF ( .NOT.linit ) THEN
       CALL sparse_direct_reorder(neq,nvalues,rowindex,diagindex,columns)
       linit = .TRUE.
    ENDIF

    ! Direct solution for the linear system
    IF ( symmetric_matrix ) THEN
       ! Matrix factoring
       CALL sparse_direct_ldlt(neq,nvalues,rowindex,diagindex,columns, &
            values,lidef)

       ! Matrix is not positive definite
       !If ( lidef ) THEN
       !   lerror = .TRUE.
       !   RETURN
       !ENDIF

       ! Solution
       CALL sparse_direct_ldltsol(neq,b,x)

    !  Solution for nonsymmetric matrix
    ELSE
       ! Matrix factoring
       CALL sparse_direct_lu(neq,nvalues,rowindex,diagindex,columns, &
            values,lidef)

       ! Matrix is not positive definite
       !If ( lidef ) THEN
       !   lerror = .TRUE.
       !   RETURN
       !ENDIF

       ! Solution
       CALL sparse_direct_lusol(neq,b,x)

    ENDIF

    ! Print out stIffness matrix
    IF ( lprnmat ) Call sparse_matrix_prnmat()

    RETURN

  END SUBROUTINE sparse_matrix_solve


    SUBROUTINE sparse_matrix_deallocate()
    ! Deallocate all variables
    IMPLICIT NONE

    CALL sparse_direct_deallocate()

    IF ( ALLOCATED(values) ) DEALLOCATE(values)
    IF ( ALLOCATED(columns) ) DEALLOCATE(columns)
    IF ( ALLOCATED(rowindex) ) DEALLOCATE(rowindex)
    IF ( ALLOCATED(diagindex) ) DEALLOCATE(diagindex)

    RETURN

  END SUBROUTINE sparse_matrix_deallocate
END MODULE sparse_matrix


