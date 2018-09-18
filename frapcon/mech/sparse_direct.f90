MODULE sparse_direct_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Sparse matrix calculations for symmetric positive definite sparse matrix
    !> that has been stored in the row major storage FORMAT
    !
    ! Row major storage scheme
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
    INTEGER(ipk) :: ngraph                        ! The size of the undirected graph of the sparse matrix
    INTEGER(ipk), ALLOCATABLE :: graph(:)         ! The undirected graph of the sparse matrix
    INTEGER(ipk), ALLOCATABLE :: igraph(:)        ! The pointer vector for the undirected graph
    INTEGER(ipk), ALLOCATABLE :: permu(:)         ! Permutation of the unknowns numbers
    INTEGER(ipk), ALLOCATABLE :: degree(:)        ! Degree of the unknowns
    INTEGER(ipk) :: nvalues_dss                   ! Size of the sparse matrix storage for the DSS
    INTEGER(ipk), ALLOCATABLE :: hbwidth_dss(:)   ! Pointer vector of DSS storage
    INTEGER(ipk), ALLOCATABLE :: diagindex_dss(:) ! Pointer vector of DSS storage
    REAL(r8k), ALLOCATABLE :: values_dss(:)       ! sparse matrix storage for the upper triangle
    REAL(r8k), ALLOCATABLE :: values_dssl(:)      ! sparse matrix storage for the lower triangle

    TYPE vertex
        INTEGER(ipk) :: number
        TYPE(vertex), POINTER :: next
    END TYPE vertex

    PRIVATE :: ngraph, graph, igraph, degree, nvalues_dss, hbwidth_dss, diagindex_dss, values_dss, vertex, &
               sparse_direct_graph, sparse_direct_level, sparse_direct_root_sloan, sparse_direct_sloan, &
               sparse_direct_profile
    PUBLIC :: permu, sparse_direct_reorder, sparse_direct_deallocate, sparse_direct_ldlt,  sparse_direct_ldltsol

CONTAINS
    SUBROUTINE sparse_direct_graph (n, nvalues, rowindex, diagindex, columns)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Form an undirected graph from a sparse matrix
    !
    INTEGER(ipk), INTENT(IN) :: &
            n, &              ! Number of equations
            nvalues, &        ! Size of the storage array for the sparse matrix
            rowindex(n+1), &  ! Pointer vector for the sparse matrix storage
            diagindex(n+1), & ! Pointer vector for the sparse matrix storage
            columns(nvalues)  ! Pointer vector for the sparse matrix storage
    INTEGER(ipk) :: i, j, k, ig

    ! Size of the graph
    ngraph = nvalues - n

    ! ALLOCATE the INTEGER array for the graph
    IF (ALLOCATED(igraph)) DEALLOCATE(igraph)
    IF (ALLOCATED(graph)) DEALLOCATE(graph)
    ALLOCATE(igraph(n+1),graph(ngraph))

    ! form the undirected graph
    ig = 1
    DO i = 1, n
        igraph(i) = ig
        DO j = rowindex(i), rowindex(i+1)-1
            k = columns(j)
            IF (k /= i) THEN
                graph(ig) = k
                ig = ig + 1
            END IF
        END DO
    END DO
    igraph(n+1) = ig

    END SUBROUTINE sparse_direct_graph
    !
    !
    !
    SUBROUTINE sparse_direct_level (droot, n, level, maxlevel, visited, maxwidth)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Form the rooted level structure for the sparse matrix starting from droot
    !
    INTEGER(ipk), INTENT(IN) :: &
            droot, & ! The root vertex for the level structure
            n        ! Number of equations
    INTEGER(ipk), INTENT(OUT) :: &
            level(n), &   ! Level structure
            maxlevel, &   ! Number of levels
            visited(n), & ! flag array for visited vertices
            maxwidth      ! Maximum level width
    INTEGER(ipk) :: i, j, nvisited, nconnected, jdof

    maxwidth = 1
    maxlevel = 1
    level = 0
    ! First level
    level(droot) = maxlevel
    visited(droot) = 1
    nvisited = 1
    ! Consequent levels
    DO WHILE (nvisited < n)
        maxlevel = maxlevel + 1
        nconnected = 0
        DO i = 1, n
            IF (level(i) == maxlevel - 1) THEN
                DO j = igraph(i), igraph(i + 1) - 1
                jdof = graph(j)
                IF (level(jdof) == 0) THEN
                    level(jdof) = maxlevel
                    visited(jdof) = 1
                    nvisited = nvisited + 1
                    nconnected = nconnected + 1
                END IF
                END DO
            END IF
        END DO
        IF (nconnected > maxwidth) maxwidth = nconnected
        IF (nconnected == 0) THEN
            maxlevel = maxlevel - 1
            RETURN
        END IF
    END DO

    END SUBROUTINE sparse_direct_level
    !
    !
    !
    SUBROUTINE sparse_direct_root_sloan (n, nroot, roots, roote)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Sloan algorithm to find a pseudo pheripheral vertices
    !
    INTEGER(ipk), INTENT(IN) :: n ! Number of equations
    INTEGER(ipk), INTENT(OUT) :: &
            nroot, &    ! Number of unconnected graphs
            roots(n), & ! Starting vertices
            roote(n)    ! End vertices
    INTEGER(ipk) :: level(n), ndeepest, deepest(n), maxlevel, i, j, slevel(n),  &
            smaxlevel, mindegree, visited(n), maxwidth, smaxwidth, emaxwidth, lmindegree
    TYPE(vertex), POINTER :: start, last, new_vertex

    visited = 0
    nroot   = 0
10  CONTINUE
    nroot = nroot + 1
    ! Find a vertex with the minimum degree as a starting point 
    mindegree = n + 1
    DO i = 1, n
        IF ((visited(i) == 0) .AND. (degree(i) < mindegree)) THEN
            mindegree = degree(i)
            roots(nroot) = i
        END IF
    END DO

    ! Form the level structure for the minimum degree vertex
    CALL sparse_direct_level (roots(nroot), n, level, maxlevel, visited, maxwidth)

20  CONTINUE

    ! Sort the deepest level
    ALLOCATE(start)
    start % next => NULL()
    lmindegree = n + 1
    ndeepest = 0
    DO i = 1, n
        IF (level(i) == maxlevel) THEN
            ndeepest = ndeepest + 1
            deepest(ndeepest) = i
            IF (degree(i) < lmindegree) THEN
                lmindegree = degree(i)
                start % number = i
            END IF
        END IF
    END DO

    last => start
    DO i = 1, ndeepest
        IF (degree(deepest(i)) > degree(last % number)) THEN
            ALLOCATE(new_vertex)
            new_vertex % number = deepest(i)
            new_vertex % next => NULL()
            last % next => new_vertex
            last => new_vertex
        END IF
    END DO

    emaxwidth = n + 1
    last => start
    DO WHILE(ASSOCIATED(last))
        j = last % number
        CALL sparse_direct_level (j, n, slevel, smaxlevel, visited, smaxwidth)
        IF ((smaxlevel > maxlevel) .AND. (smaxwidth < maxwidth)) THEN
            roots(nroot) = j
            level = slevel
            maxlevel = smaxlevel
            maxwidth = smaxwidth
            ! deallocate list
            DO WHILE(ASSOCIATED(start))
                last => start
                start => last % next
                DEALLOCATE(last)
            END DO
            GOTO 20
        ELSE IF (smaxwidth < emaxwidth) THEN
            roote(nroot) = j
            emaxwidth = smaxwidth
        END IF
        last => last % next
    END DO

    ! deallocate list
    DO WHILE(ASSOCIATED(start))
        last => start
        start => last % next
        DEALLOCATE(last)
    END DO

    ! Unconnected graph
    IF (MINVAL(visited) == 0) GOTO 10

    END SUBROUTINE sparse_direct_root_sloan
    !
    !
    !
    SUBROUTINE sparse_direct_sloan (n, droots, droote, numbered)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Use Sloan_frapcon's algorithm to reorder the numbering of unknowns
    !
    INTEGER(ipk), INTENT(IN) :: &
            n, &      ! Number of equations
            droots, & ! Starting vertex
            droote    ! End vertex
    INTEGER(ipk), INTENT(INOUT) :: &
            numbered  ! Number of last numbered unknown
    INTEGER(ipk) :: &
            W1, W2, &    ! Parameters for the sloan algorithm
            status(n), & ! If vertex i is postactive or active status(i) = 0
                         ! If vertex i is preactive or inactive status(i) = 1
            priority(n), maxpriority, maxlevel, maxwidth, id, jd, kd, i, j, nq
    INTEGER(ipk) :: qstart, qend, qnext(n), qprev(n)

    ! Set the weight factors for the sloan algorithm
    W1 = 1
    W2 = 2
    ! Form the level structure for the End vertex and initialize status
    status = 1
    CALL sparse_direct_level (droote, n, priority, maxlevel, status, maxwidth)
    ! Initialize priorities
    priority = W1 * priority - W2 * (degree + 1)
    ! Initialize the list for group Q
    qnext = -1
    qprev = -1
    qstart = droots
    qend = droots
    qnext(droots) = 0
    qprev(droots) = 0
    ! Start the numbering
    DO
        ! Find the vertex id with the maximum priority from the list Q
        nq = 0
        jd = qstart
        maxpriority = -HUGE(maxpriority)
        DO WHILE (jd > 0)
            nq = nq + 1
            IF (priority(jd) > maxpriority) THEN
                maxpriority = priority(jd)
                id = jd
            END IF
            jd = qnext(jd)
        END DO
        IF (nq == 0) RETURN
        ! Number the vertex with the maximum priority
        numbered  = numbered + 1
        permu(id) = numbered
        ! id is preactive
        IF (status(id) == 1) THEN
            DO i = igraph(id), igraph(id + 1) - 1
                jd = graph(i)
                priority(jd) = priority(jd) + W2
                ! Update the list Q
                IF (qnext(jd) < 0) THEN
                qnext(qend) = jd
                qprev(jd) = qend
                qnext(jd) = 0
                qend = jd
                END IF
            END DO
        END IF
        status(id) = 0
        DO i = igraph(id), igraph(id + 1) - 1
            jd = graph(i)
            IF (status(jd) == 1) THEN
                status(jd) = 0
                DO j = igraph(jd), igraph(jd + 1) - 1
                kd = graph(j)
                IF (permu(kd) == 0) THEN
                    priority(kd) = priority(kd) + W2
                    IF (qnext(kd) < 0) THEN
                        qnext(qend) = kd
                        qprev(kd) = qend
                        qnext(kd) = 0
                        qend = kd
                    END IF
                END IF
                END DO
            END IF
        END DO
        IF ((qnext(id) == 0) .AND. (qprev(id) == 0)) RETURN
        ! Remove vertex id from list Q
        IF (qnext(id) == 0) THEN
            qend = qprev(id)
            qnext(qend) = 0
        ELSE IF (qprev(id) == 0) THEN
            qstart = qnext(id)
            qprev(qstart) = 0
        ELSE
            qnext(qprev(id)) = qnext(id)
            qprev(qnext(id)) = qprev(id)
        END IF
        qnext(id) = -1
        qprev(id) = -1
    END DO

    END SUBROUTINE sparse_direct_sloan
    !
    !
    !
    FUNCTION sparse_direct_profile(n) RESULT(prof)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate matrix profile
    !
    INTEGER(ipk) :: n, prof, i, j, jmin

    prof = 0
    DO i = 1, n
        jmin = permu(i)
        DO j = igraph(i),igraph(i + 1) - 1
            IF (permu(graph(j)) < jmin) jmin  = permu(graph(j))
        END DO
        prof = prof + permu(i) - jmin
    END DO

    END FUNCTION sparse_direct_profile
    !
    !
    !
    SUBROUTINE sparse_direct_reorder (n, nvalues, rowindex, diagindex, columns)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Reorder the numbering of unknownss to reduce the profile of the sparse matrix
    !
    INTEGER(ipk), INTENT(IN) :: &
            n, &              ! Number of equations
            nvalues, &        ! Size of the sparse matrix storage
            rowindex(n+1), &  ! Pointer vector for the sparse storage
            diagindex(n+1), & ! Pointer vector for the sparse storage
            columns(nvalues)  ! Pointer vector for the sparse storage
    INTEGER(ipk) :: i, j, tmp(n), nroot, roots(n), roote(n), prof0, prof, jmin, jmax, numbered

    IF (.NOT. quiet) WRITE (ounit,FMT='(A)') 'Using direct solver for a sparse matrix'

    ! ALLOCATE permutation and degree array
    IF (ALLOCATED(permu)) DEALLOCATE(permu)
    IF (ALLOCATED(degree)) DEALLOCATE(degree)
    ALLOCATE(permu(n),degree(n))

    ! Form the undirected graph of the sparse matrix
    CALL  sparse_direct_graph (n, nvalues, rowindex, diagindex, columns)

    ! Degree of the unknown and initialization for permu
    DO i = 1, n
        permu(i) = i
        degree(i) = igraph(i + 1) - igraph(i)
    END DO

    ! Initial profile
    prof0 = sparse_direct_profile(n)

    ! Print out matrix data
    IF (.NOT. quiet) THEN
        WRITE (ounit,FMT='(A,I0,A)') 'Sparse direct solver initialized for ',n,' degrees of freedom'
        WRITE (ounit,FMT='(A,I0)') ' - Initial matrix profile is ',prof0
    END IF

    ! Find pseude pheripheral vertex using Gibbs algorithm
    CALL sparse_direct_root_sloan (n, nroot, roots, roote)
    permu = 0
    numbered = 0
    ! Reorder the numbering using Sloan's algorithm
    DO i = 1, nroot
        CALL sparse_direct_sloan (n, roots(i), roote(i), numbered)
    END DO

    ! Profile after reordering
    prof = sparse_direct_profile(n)

    ! Fall back to initial ordering If the reordered prifile is larger than
    ! the original
    IF (prof >= prof0) THEN
        prof = prof0
        DO i = 1, n
            permu(i) = i
        END DO
    END IF

    ! Print out matrix data
    IF (.NOT. quiet) THEN
        WRITE (ounit,FMT='(A,I0)') ' - Matrix profile after the reduction is ',prof
        WRITE (ounit,FMT='(A,F0.2,A,/)') ' - Total size of the matrix storage for direct solver' &
          &                              // ' is ', (prof+n)*r8k / 1024.0_r8k ** 2,' MB'
    END IF

    ! Create sparse matrix storage for direct solution
    nvalues_dss = n + prof
    IF (ALLOCATED(values_dss)) DEALLOCATE(values_dss)
    ALLOCATE(values_dss(nvalues_dss))
    values_dss = 0.0_r8k
    IF (ALLOCATED(hbwidth_dss)) DEALLOCATE(hbwidth_dss)
    IF (ALLOCATED(diagindex_dss)) DEALLOCATE(diagindex_dss)
    ALLOCATE(hbwidth_dss(n),diagindex_dss(n+1))
    DO i = 1, n
        jmin = permu(i)
        jmax = permu(i)
        DO j = igraph(i),igraph(i + 1) - 1
            IF (permu(graph(j)) < jmin) jmin  = permu(graph(j))
            IF (permu(graph(j)) > jmax) jmax  = permu(graph(j))
        END DO
        tmp(permu(i)) = permu(i) - jmin + 1
        hbwidth_dss(permu(i)) = jmax - permu(i) + 1
    END DO
    diagindex_dss(1) = 1
    DO i = 2,n
        diagindex_dss(i) = diagindex_dss(i - 1) + tmp(i - 1)
        hbwidth_dss(i) = MAX(hbwidth_dss(i - 1) - 1,hbwidth_dss(i))
    END DO
    diagindex_dss(n+1) = diagindex_dss(n) + tmp(n)

    END SUBROUTINE sparse_direct_reorder
    !
    !
    !
    SUBROUTINE sparse_direct_ldlt (n, nvalues, rowindex, diagindex, columns, values, lidef)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> LDL^T factorization of the sparse matrix
    !
    INTEGER(ipk), INTENT(IN) :: &
            n, &                  ! Number of equations
            nvalues, &            ! Size of the sparse matrix storage
            rowindex(n+1), &      ! Pointer vector for the sparse storage
            diagindex(n+1), &     ! Pointer vector for the sparse storage
            columns(nvalues)      ! Pointer vector for the sparse storage
    LOGICAL, INTENT(OUT) :: lidef ! Flag for semidefinite matrix
    REAL(r8k), INTENT(IN) :: &
            values(nvalues)       ! Sparse matrix storage array
    INTEGER(ipk) :: i, j, k, l, i_dss, j_dss, imin, ip, ipmax, kp, kpmax, kmax, jp

    ! Initialize to zero
    values_dss = 0.0_r8k
    lidef = .FALSE.

    ! Copy stiffness matrix to the dss storage
    DO i = 1, n
        DO k = diagindex(i),rowindex(i + 1) - 1
            j = columns(k)
            i_dss = MIN(permu(i), permu(j))
            j_dss = MAX(permu(i), permu(j))
            l = diagindex_dss(j_dss) + j_dss - i_dss
            values_dss(l) = values(k)
        END DO
    END DO

    ! Loop over columns
    DO j = 1, n
        ! terms g_ij and d_jj for the column j
        imin = j-diagindex_dss(j+1) + diagindex_dss(j) + 1
        ipmax = diagindex_dss(j+1) - 1
        DO i = imin, j-1
            ip = diagindex_dss(j) + j - i
            kp = diagindex_dss(i)
            kpmax = diagindex_dss(i+1) - 1
            kmax = MIN(kpmax-kp, ipmax-ip)
            DO k = 1, kmax
                values_dss(ip)=values_dss(ip)-values_dss(ip+k)*values_dss(kp+k)
            END DO
        END DO
        ! diagonal term d_jj
        ip = diagindex_dss(j)
        DO i = imin, (j-1)
            jp = diagindex_dss(j) + j - i
            kp = diagindex_dss(i)
            values_dss(ip) = values_dss(ip) - values_dss(jp) ** 2 / values_dss(kp)
        END DO
        ! check for positive definites
        IF (values_dss(ip) <= 0.0_r8k) lidef = .TRUE.
        ! terms l_ij for the column j
        DO i = imin, (j - 1)
            ip = diagindex_dss(j) + j - i
            kp = diagindex_dss(i)
            values_dss(ip) = values_dss(ip) / values_dss(kp)
        END DO
    END DO

    END SUBROUTINE sparse_direct_ldlt
    !
    !
    !
    SUBROUTINE sparse_direct_lu (n, nvalues, rowindex, diagindex, columns, values, lidef)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> LU decomposition of nonsymmetric sparse matrix
    !
    INTEGER(ipk), INTENT(IN) :: &
            n, &                  ! Number of equations
            nvalues, &            ! Size of the sparse matrix storage
            rowindex(n+1), &      ! Pointer vector for the sparse storage
            diagindex(n+1), &     ! Pointer vector for the sparse storage
            columns(nvalues)      ! Pointer vector for the sparse storage
    LOGICAL, INTENT(OUT) :: lidef ! Flag for semidefinite matrix
    REAL(r8k), INTENT(IN) :: &
            values(nvalues)       ! Sparse matrix storage array
    INTEGER(ipk) :: i, j, k, l, i_dss, j_dss, j_min, j_max, k_max, ip, jp

    ! ALLOCATE array for the lower triangle If needed
    IF (.NOT. ALLOCATED(values_dssl)) ALLOCATE(values_dssl(nvalues_dss))

    ! Initialize to zero
    values_dss = 0.0_r8k
    values_dssl = 0.0_r8k
    lidef = .FALSE.

    ! Copy stiffness matrix to sparse direct solver arrays
    DO i = 1, n
        DO k = rowindex(i), rowindex(i + 1) - 1
            j = columns(k)
            i_dss = permu(i)
            j_dss = permu(j)
            IF (i_dss > j_dss) THEN
                ! Lower triangle
                l = diagindex_dss(i_dss) + i_dss - j_dss
                values_dssl(l) = values(k)
            ELSE IF (i_dss == j_dss) THEN
                ! Diagonal
                l = diagindex_dss(i_dss)
                values_dssl(l) = values(k)
                values_dss(l) = values(k)
            ELSE
                ! Upper triangle
                l = diagindex_dss(j_dss) + j_dss - i_dss
                values_dss(l) = values(k)
            END IF
        END DO
    END DO

    ! LU decomposition
    DO i = 1, n
        ! Mark l_ii = 1
        values_dssl(diagindex_dss(i)) = 1.0_r8k

        ! calculate terms u_ji
        j_min = i - diagindex_dss(i + 1) + diagindex_dss(i) + 1
        DO j = j_min,i
            ip = diagindex_dss(i) + i - j
            jp = diagindex_dss(j)
            k_max = diagindex_dss(i + 1) - ip - 1
            k_max = MIN(k_max, diagindex_dss(j + 1) - jp - 1)
            DO k = 1, k_max
                values_dss(ip) = values_dss(ip) - values_dssl(jp+k) * values_dss(ip+k)
            END DO
            values_dss(ip) = values_dss(ip)
        END DO

        IF (values_dss(ip) < 0.0_r8k) lidef = .TRUE.

        ! calculate terms l_ji
        j_max = i + hbwidth_dss(i) - 1
        DO j = i+1,j_max
            IF (j - i < diagindex_dss(j + 1) - diagindex_dss(j)) THEN
                ip = diagindex_dss(i)
                jp = diagindex_dss(j) + j - i
                k_max = diagindex_dss(j + 1) - jp - 1
                k_max = MIN(k_max, diagindex_dss(i + 1) - ip - 1)
                DO k = 1, k_max
                    values_dssl(jp) = values_dssl(jp) - values_dssl(jp+k) * values_dss(ip+k)
                END DO
                values_dssl(jp) = values_dssl(jp) / values_dss(ip)
            END IF
        END DO
    END DO

    END SUBROUTINE sparse_direct_lu
    !
    !
    !
    SUBROUTINE sparse_direct_ldltsol (n, r, x)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Solve the linear equation group using LDL^T factored matrix
    !
    INTEGER(ipk), INTENT(IN) :: n    ! Number of equations
    REAL(r8k), INTENT(IN) :: r(n)    ! Load vector
    REAL(r8k), INTENT(INOUT) :: x(n) ! Solution
    INTEGER(ipk) :: i, j, ip, jp, jpmax, jmax
    REAL(r8k) :: sum, y(n)

    DO i = 1, n
        x(i) = 0.0_r8k
        y(permu(i)) = r(i)
    END DO

    ! Back substitution
    DO i = 1, n
        ip = diagindex_dss(i)
        jpmax = diagindex_dss(i + 1) - 1
        sum = y(i)
        DO jp = ip+1, jpmax
            j = i + ip - jp
            sum = sum - values_dss(jp) * y(j)
        END DO
        y(i) = sum
    END DO

    ! Diagonal factor
    DO i = 1, n
        ip = diagindex_dss(i)
        y(i) = y(i)/values_dss(ip)
    END DO

    ! Forward substitution
    DO i = n,1,-1
        ip = diagindex_dss(i)
        jmax = i + hbwidth_dss(i) - 1
        sum = y(i)
        DO j = i+1,jmax
            jp = diagindex_dss(j) + j - i
            jpmax =  diagindex_dss(j + 1) - 1
            IF (jp <= jpmax) sum = sum - values_dss(jp) * y(j)
        END DO
        y(i) = sum
    END DO

    DO i = 1, n
        x(i) = y(permu(i))
    END DO

    END SUBROUTINE sparse_direct_ldltsol
    !
    !
    !
    SUBROUTINE sparse_direct_lusol (n, r, x)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Solve the linear equation group using LDL^T factored matrix
    !
    INTEGER(ipk), INTENT(IN) :: n    ! Number of equations
    REAL(r8k), INTENT(IN) :: r(n)    ! Load vector
    REAL(r8k), INTENT(INOUT) :: x(n) ! Solution

    INTEGER(ipk) :: i, j, ip, jp, jpmax, jmax
    REAL(r8k) :: sum, y(n)

    DO i = 1, n
        x(i) = 0.0_r8k
        y(permu(i)) = r(i)
    END DO

    ! Back substitution
    DO i = 1, n
        ip = diagindex_dss(i)
        jpmax = diagindex_dss(i + 1) - 1
        sum = y(i)
        DO jp = ip+1, jpmax
            j = i + ip - jp
            sum = sum - values_dssl(jp) * y(j)
        END DO
        y(i) = sum
    END DO

    ! Forward substitution
    DO i = n, 1, -1
        ip = diagindex_dss(i)
        jmax = i + hbwidth_dss(i) - 1
        sum = y(i)
        DO j = i+1, jmax
            jp = diagindex_dss(j) + j - i
            jpmax =  diagindex_dss(j + 1) - 1
            IF (jp <= jpmax) sum = sum - values_dss(jp) * y(j)
        END DO
        y(i) = sum / values_dss(ip)
    END DO

    DO i = 1, n
        x(i) = y(permu(i))
    END DO

    END SUBROUTINE sparse_direct_lusol
    !
    !
    !
    SUBROUTINE sparse_direct_deallocate()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate all the arrays

    IF (ALLOCATED(igraph)) DEALLOCATE(igraph)
    IF (ALLOCATED(graph)) DEALLOCATE(graph)
    IF (ALLOCATED(permu)) DEALLOCATE(permu)
    IF (ALLOCATED(degree)) DEALLOCATE(degree)
    IF (ALLOCATED(values_dss)) DEALLOCATE(values_dss)
    IF (ALLOCATED(values_dssl)) DEALLOCATE(values_dssl)
    IF (ALLOCATED(hbwidth_dss)) DEALLOCATE(hbwidth_dss)
    IF (ALLOCATED(diagindex_dss)) DEALLOCATE(diagindex_dss)

    END SUBROUTINE sparse_direct_deallocate
    !
END MODULE sparse_direct_frapcon



