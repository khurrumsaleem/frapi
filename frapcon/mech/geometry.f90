MODULE geometry_fpn_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE math_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE pressure2d_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE cont2d_frapcon
    USE variables_frapcon, ONLY :ounit
    USE FEA_Node_frapcon
    IMPLICIT NONE
    !>@brief
    !> Geometric and meshing entities
    !
    TYPE line_type
        INTEGER(ipk) :: number            ! Line label
        INTEGER(ipk) :: nelems            ! Number of element boundaries on line
        INTEGER(ipk), POINTER :: nodes(:) ! Node numbers at line
        TYPE(line_type), POINTER :: next  ! Next line in linked list
    END TYPE line_type

    TYPE surf_type
        INTEGER(ipk) :: number              ! Surface label
        INTEGER(ipk) :: nelems(2)           ! Number of element boundaries on line
        INTEGER(ipk) :: lines(4)            ! Line labels bounding the surface
        INTEGER(ipk), POINTER :: nodes(:,:) ! Nodes at the surface
        TYPE(surf_type), POINTER :: next
    END TYPE surf_type


    TYPE(line_type), POINTER :: &
        first_line, last_line     ! Pointers to the linked lists
    TYPE(surf_type), POINTER :: &
        first_surf, last_surf     ! Pointers to the linked lists
    INTEGER(ipk) :: nlines, nsurf ! Number of existing geometric entities
    REAL(r8k) :: n_wrk(3)         ! Normal for the current work plane
    !
    CONTAINS
    !
    SUBROUTINE init_geometry()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pointers for geometric objects

    ! Deallocate all existing gemetric objects
    CALL deallocate_geometry()

    ! Initialize pointers
    nlines = 0
    nsurf = 0
    first_line => NULL()
    last_line => NULL()
    first_surf => NULL()
    last_surf => NULL()

    ! Initialize work plane to (0,0,1)
    n_wrk(1:3) = (/ 0.0_r8k, 0.0_r8k, 1.0_r8k /)

    END SUBROUTINE init_geometry
    !
    !
    !
    SUBROUTINE create_line (lnumber, nodes, ne)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new line
    !
    INTEGER(ipk), INTENT(IN) :: lnumber, nodes(2), ne
    INTEGER(ipk) :: label, i, nfound
    TYPE(line_type), POINTER :: nextl, newl
    TYPE(node_type), POINTER :: nextn
    REAL(r8k) :: x1(3), x2(3), dx(3), x_node(3)

    ! Check that the line label does not already exist
    nextl => first_line
    DO WHILE (ASSOCIATED(nextl))
        IF (nextl%number == lnumber) THEN
            WRITE (ounit,FMT='(A,I0)') 'ERROR: double definition for line ',lnumber
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF
        nextl => nextl%next
    END DO

    nextn => first_node
    nfound = 0
    DO WHILE (ASSOCIATED(nextn))
        IF (nextn%label == nodes(1)) nfound = nfound + 1
        IF (nextn%label == nodes(2)) nfound = nfound + 1
        nextn => nextn%next
    END DO

    IF (nfound /= 2) THEN
        WRITE (ounit,'(A,I0,A)') 'ERROR: Line number ',lnumber,' uses undefined node(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    IF (ne < 1) THEN
        WRITE (ounit,'(A,I0,A)') 'ERROR: There must be at least one element at line ',lnumber,'.'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Increment lines counter
    nlines = nlines + 1

    ! Create line
    ALLOCATE(newl)
    newl%number = lnumber
    ALLOCATE(newl%nodes(ne+1))
    newl%nodes(1) = nodes(1)
    newl%nodes(ne+1) = nodes(2)
    newl%nelems = ne
    newl%next => NULL()

    ! Add line to the linked list
    IF (.NOT. ASSOCIATED(last_line)) THEN
        first_line => newl
        last_line => newl
    ELSE
        last_line%next => newl
        last_line => newl
    END IF

    ! Create nodes at line
    x1 = node_coords(nodes(1),0)
    x2 = node_coords(nodes(2),0)
    dx = (x2 - x1) / ne
    x_node = x1
    DO i = 2, ne
        label = max_node + 1
        x_node = x_node + dx
        CALL create_node (label, x_node)
        newl%nodes(i) = label
    END DO

    END SUBROUTINE create_line
    !
    !
    !
    SUBROUTINE create_arc (lnumber, nodes, r, ne)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new arc
    !
    INTEGER(ipk), INTENT(IN) :: lnumber, nodes(2), ne
    REAL(r8k), INTENT(IN) :: r
    INTEGER(ipk) :: label, i, nfound
    TYPE(line_type), POINTER :: nextl, newl
    TYPE(node_type), POINTER :: nextn
    REAL(r8k) :: r0(3), r1(3), r2(3), r12(3), theta, dl, rn(3), dalpha, xn(3)

    ! Check that the line label does not already exist
    nextl => first_line
    DO WHILE (ASSOCIATED(nextl))
        IF (nextl%number == lnumber) THEN
            WRITE (ounit,FMT='(A,I0)') 'ERROR: double definition for arc/line ',lnumber
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF
        nextl => nextl%next
    END DO

    nextn => first_node
    nfound = 0
    DO WHILE (ASSOCIATED(nextn))
        IF (nextn%label == nodes(1)) nfound = nfound + 1
        IF (nextn%label == nodes(2)) nfound = nfound + 1
        nextn => nextn%next
    END DO

    IF (nfound /= 2) THEN
        WRITE (ounit,'(A,I0,A)') 'ERROR: Line number ',lnumber,' uses undefined nodes(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    IF (ne < 1) THEN
        WRITE (ounit,'(A,I0,A)') 'ERROR: There must be at least one element at arc ',lnumber,'.'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Increment lines counter
    nlines = nlines + 1

    ! Create line
    ALLOCATE(newl)
    newl%number = lnumber
    newl%nelems = ne
    ALLOCATE(newl%nodes(ne+1))
    newl%nodes(1) = nodes(1)
    newl%nodes(ne+1) = nodes(2)
    newl%next => NULL()

    ! Add arc to the linked list
    IF (.NOT. ASSOCIATED(last_line)) THEN
        first_line => newl
        last_line => newl
    ELSE
        last_line%next => newl
        last_line => newl
    END IF

    ! Vectors to the nodes 1 and 2
    r1 = node_coords(nodes(1),0)
    r2 = node_coords(nodes(2),0)

    ! Vector from node 1 to node 2
    r12 = r2 - r1
    dl = vnorm(3,r12)

    ! Normal vector to r12 at the current work plane
    rn(1) = n_wrk(2) * r12(3) - r12(2) * n_wrk(3)
    rn(2) = n_wrk(3) * r12(1) - r12(3) * n_wrk(1)
    rn(3) = n_wrk(1) * r12(2) - r12(1) * n_wrk(2)
    rn = rn / vnorm(3,rn)

    ! Centre point of the circle
    r0 = r1 + 0.5_r8k * r12 + SQRT(r ** 2 - 0.25_r8k * dl ** 2) * rn

    ! Angle between two nodes
    theta = ASIN(0.5_r8k * dl / r)
    dalpha = 2.0_r8k * theta / ne

    ! Create nodes at arc
    xn = r1
    DO i = 2, ne
        r1 = xn - r0
        r2(1) = n_wrk(2) * r1(3) - r1(2) * n_wrk(3)
        r2(2) = n_wrk(3) * r1(1) - r1(3) * n_wrk(1)
        r2(3) = n_wrk(1) * r1(2) - r1(1) * n_wrk(2)
        xn = r0 + COS(dalpha) * r1 + SIN(dalpha) * r * r2 / vnorm(3,r2)
        label = max_node + 1
        CALL create_node(label,xn)
        newl%nodes(i) = label
    END DO

    END SUBROUTINE create_arc
    !
    !
    !
    SUBROUTINE deallocate_lines()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !
    TYPE(line_type), POINTER :: nextl, currl

    nextl => first_line
    DO WHILE (ASSOCIATED(nextl))
        currl => nextl
        nextl => currl%next
        DEALLOCATE(currl%nodes)
        DEALLOCATE(currl)
    END DO

    END SUBROUTINE deallocate_lines
    !
    !
    !
    SUBROUTINE create_lpress (label, nl)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create pressure boundary condition on a line
    !
    INTEGER(ipk), INTENT(IN) :: label, nl
    TYPE(line_type), POINTER :: currl
    TYPE(quad4_type), POINTER :: currq4
    INTEGER(ipk) :: nodes(2), i, found1, found2, nnl
    INTEGER(ipk), ALLOCATABLE :: lnodes(:)
    REAL(r8k) :: p(2)

    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        IF (currl%number == nl) EXIT
        currl => currl%next
    END DO

    IF (.NOT. ASSOCIATED(currl)) THEN
        WRITE (ounit,FMT='(A,I0,A)') 'ERROR Can not find line number ',nl,' for the pressure BC'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Find QUAD4 element associated to the line
    found1 = 0
    found2 = 0
    currq4 => first_quad4
    DO WHILE (ASSOCIATED(currq4))
        IF (currq4%node_labels(1) == currl%nodes(1)) found1 = 1
        IF (currq4%node_labels(2) == currl%nodes(1)) found1 = 2
        IF (currq4%node_labels(3) == currl%nodes(1)) found1 = 3
        IF (currq4%node_labels(4) == currl%nodes(1)) found1 = 4
        IF (found1 > 0) THEN
            IF (currq4%node_labels(1) == currl%nodes(2)) found2 = 1
            IF (currq4%node_labels(2) == currl%nodes(2)) found2 = 2
            IF (currq4%node_labels(3) == currl%nodes(2)) found2 = 3
            IF (currq4%node_labels(4) == currl%nodes(2)) found2 = 4
        END IF
        IF (found2 > 0) EXIT
        currq4 => currq4%next
    END DO

    IF (.NOT. ASSOCIATED(currq4)) THEN
        found1 = 1
        found2 = 2
    END IF

    nnl = currl%nelems + 1
    ALLOCATE(lnodes(nnl))
    IF ((found2 > found1) .OR. ((found1 == 4) .AND. (found2 == 1))) THEN
        lnodes(nnl:1:-1) = currl%nodes(1:nnl)
    ELSE
        lnodes(1:nnl) = currl%nodes(1:nnl)
    END IF

    p = 0.0_r8k
    DO i = 2,currl%nelems+1
        nodes(1) = lnodes(i-1)
        nodes(2) = lnodes(i)
        CALL pressure2d_updval (label, nodes, p)
    END DO

    DEALLOCATE(lnodes)

    END SUBROUTINE create_lpress
    !
    !
    !
    SUBROUTINE create_lcont (label, mat, nl)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create contact boundary condition on a line
    !
    INTEGER(ipk), INTENT(IN) :: label, mat, nl
    TYPE(line_type), POINTER :: currl
    TYPE(quad4_type), POINTER :: currq4
    INTEGER(ipk) :: ntarget, ncont, found1, found2, nnl
    INTEGER(ipk), ALLOCATABLE :: tnodes(:), cnodes(:)

    ! Find line
    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        IF (currl%number == nl) EXIT
        currl => currl%next
    END DO

    ! Stop If the line is not found
    IF (.NOT. ASSOCIATED(currl)) THEN
        WRITE (ounit,FMT='(A,I0,A)') 'ERROR Can not find line number ',nl,' for the contact BC'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ALLOCATE(tnodes(nnodes),cnodes(nnodes))

    ! Find QUAD4 element associated to the line
    found1 = 0
    found2 = 0
    currq4 => first_quad4
    DO WHILE (ASSOCIATED(currq4))
        IF (currq4%node_labels(1) == currl%nodes(1)) found1 = 1
        IF (currq4%node_labels(2) == currl%nodes(1)) found1 = 2
        IF (currq4%node_labels(3) == currl%nodes(1)) found1 = 3
        IF (currq4%node_labels(4) == currl%nodes(1)) found1 = 4
        IF (found1 > 0) THEN
            IF (currq4%node_labels(1) == currl%nodes(2)) found2 = 1
            IF (currq4%node_labels(2) == currl%nodes(2)) found2 = 2
            IF (currq4%node_labels(3) == currl%nodes(2)) found2 = 3
            IF (currq4%node_labels(4) == currl%nodes(2)) found2 = 4
        END IF
        IF (found2 > 0) EXIT
        currq4 => currq4%next
    END DO

    IF (.NOT. ASSOCIATED(currq4)) THEN
        found1 = 1
        found2 = 2
    END IF

    nnl = currl%nelems + 1
    IF ((found2 > found1) .OR. ((found1 == 4) .AND. (found2 == 1))) THEN
        tnodes(1:nnl) = currl%nodes(1:nnl)
    ELSE
        tnodes(nnl:1:-1) = currl%nodes(1:nnl)
    END IF

    ntarget = nnl
    ncont = nnl
    cnodes(1:ncont) = tnodes(1:ncont)

    CALL cont2d_create_surf (label, mat, ntarget, ncont, tnodes, cnodes)

    END SUBROUTINE create_lcont
    !
    !
    !
    SUBROUTINE create_lcont2 (label, mat, nl1, nl2)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create contact boundary condition on a line
    !
    INTEGER(ipk), INTENT(IN) :: label, mat, nl1, nl2
    TYPE(line_type), POINTER :: L1, L2, currl
    TYPE(quad4_type), POINTER :: currq4
    INTEGER(ipk) :: ntarget, ncont, nfound, found1, found2, nnl1, nnl2
    INTEGER(ipk), ALLOCATABLE :: tnodes(:), cnodes(:)

    ! Find line
    nfound = 0
    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        IF (currl%number == nl1) THEN
            nfound = nfound + 1
            L1 => currl
        END IF
        IF (currl%number == nl2) THEN
            nfound = nfound + 1
            L2 => currl
        END IF
        IF (nfound == 2) EXIT
        currl => currl%next
    END DO

    ! Stop If the line is not found
    IF (nfound /= 2) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR Can not find lines  for the contact BC'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    nnl1 = L1%nelems + 1
    nnl2 = L2%nelems + 1
    ALLOCATE(tnodes(nnl1),cnodes(nnl2))

    ! Find QUAD4 element associated to the line
    found1 = 0
    found2 = 0
    currq4 => first_quad4
    DO WHILE (ASSOCIATED(currq4))
        IF (currq4%node_labels(1) == L1%nodes(1)) found1 = 1
        IF (currq4%node_labels(2) == L1%nodes(1)) found1 = 2
        IF (currq4%node_labels(3) == L1%nodes(1)) found1 = 3
        IF (currq4%node_labels(4) == L1%nodes(1)) found1 = 4
        IF (found1 > 0) THEN
            IF (currq4%node_labels(1) == L1%nodes(2)) found2 = 1
            IF (currq4%node_labels(2) == L1%nodes(2)) found2 = 2
            IF (currq4%node_labels(3) == L1%nodes(2)) found2 = 3
            IF (currq4%node_labels(4) == L1%nodes(2)) found2 = 4
        END IF
        IF (found2 > 0) EXIT
        currq4 => currq4%next
    END DO

    IF (.NOT. ASSOCIATED(currq4)) THEN
        found1 = 1
        found2 = 2
    END IF

    IF ((found2 > found1) .OR. ((found1 == 4) .AND. (found2 == 1))) THEN
        tnodes(1:nnl1) = L1%nodes(1:nnl1)
    ELSE
        tnodes(nnl1:1:-1) = L1%nodes(1:nnl1)
    END IF

    ntarget = nnl1
    ncont = nnl2
    cnodes(1:nnl2) = L2%nodes(1:nnl2)

    CALL cont2d_create_surf (label, mat, ntarget, ncont, tnodes, cnodes)

    DEALLOCATE(tnodes,cnodes)

    END SUBROUTINE create_lcont2
    !
    !
    !
    SUBROUTINE create_lfixed (dim, nl, iend)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create pressure boundary condition on a line
    !
    INTEGER(ipk), INTENT(IN) :: dim, nl, iend
    INTEGER(ipk) :: i
    TYPE(line_type), POINTER :: currl

    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        IF (currl%number == nl) EXIT
        currl => currl%next
    END DO

    IF (.NOT. ASSOCIATED(currl)) THEN
        WRITE (ounit,FMT='(A,I0,A)') 'ERROR Can not find line number ',nl,' for the pressure BC'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    IF (iend == 1) THEN
        CALL set_node_status(currl%nodes(1),dim,-1)
        CALL set_node_status(currl%nodes(currl%nelems+1),dim,-1)
    END IF

    DO i = 2, currl%nelems
        CALL set_node_status(currl%nodes(i),dim,-1)
    END DO

    END SUBROUTINE create_lfixed
    !
    !
    !
    SUBROUTINE create_lcoupled (dim, nl, label)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create coupled DOFs on a line
    !
    INTEGER(ipk), INTENT(IN) :: dim, nl, label
    INTEGER(ipk) :: i
    TYPE(line_type), POINTER :: currl

    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        IF (currl%number == nl) EXIT
        currl => currl%next
    END DO

    IF (.NOT. ASSOCIATED(currl)) THEN
        WRITE (ounit,FMT='(A,I0,A)') 'ERROR Can not find line number ',nl,' for the coupled BC'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    DO i = 1, currl%nelems+1
        CALL create_coupled(label,currl%nodes(i),dim)
    END DO

    END SUBROUTINE create_lcoupled
    !
    !
    !
    SUBROUTINE create_surf4(label, l1, l2, l3, l4)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create an element mesh by six bounding lines
    !
    INTEGER(ipk), INTENT(IN) :: label, l1, l2, l3, l4
    INTEGER(ipk) :: i, j, dir(4), nlabel, nnl1, nnl2, nnl3, nnl4, nfound
    TYPE(line_type), POINTER :: pl1, pl2, pl3, pl4, nextl
    REAL(r8k) :: x1(3), x2(3), dx(3)
    TYPE(surf_type), POINTER :: new_surf

    nfound = 0
    nextl => first_line
    DO WHILE (ASSOCIATED(nextl))
        IF (nextl%number == l1) THEN
            nfound = nfound + 1
            nnl1 = nextl%nelems + 1
            pl1 => nextl
        END IF
        IF (nextl%number == l2) THEN
            nfound = nfound + 1
            nnl2 = nextl%nelems + 1
            pl2 => nextl
        END IF
        IF (nextl%number == l3) THEN
            nfound = nfound + 1
            nnl3 = nextl%nelems + 1
            pl3 => nextl
        END IF
        IF (nextl%number == l4) THEN
            nfound = nfound + 1
            nnl4 = nextl%nelems + 1
            pl4 => nextl
        END IF
        nextl => nextl%next
    END DO

    ! Check wether all the lines were found
    IF (nfound /= 4) THEN
        WRITE (ounit,'(A)') 'ERROR: Mesh uses undefined line(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Second line
    IF ((pl1%nodes(nnl1) == pl3%nodes(1)) .OR. (pl1%nodes(nnl1) == pl3%nodes(nnl3))) THEN
        nextl => pl2
        pl2 => pl3
        pl3 => nextl
        nnl2 = pl2%nelems + 1
        nnl3 = pl3%nelems + 1
    ELSE IF ((pl1%nodes(nnl1) == pl4%nodes(1)) .OR. (pl1%nodes(nnl1) == pl4%nodes(nnl4))) THEN
        nextl => pl2
        pl2 => pl4
        pl4 => nextl
        nnl2 = pl2%nelems + 1
        nnl4 = pl4%nelems + 1
    END IF

    ! Third line
    IF ((pl2%nodes(1) == pl4%nodes(1)) .OR. (pl2%nodes(1) == pl4%nodes(nnl4)) .OR. &
      & (pl2%nodes(nnl2) == pl4%nodes(1)) .OR. (pl2%nodes(nnl2) == pl4%nodes(nnl4))) THEN
        nextl => pl3
        pl3 => pl4
        pl4 => nextl
        nnl3 = pl3%nelems + 1
        nnl4 = pl4%nelems + 1
    END IF

    ! Direction in lines
    dir(1) = 1
    dir(2) = -1
    IF (pl1%nodes(nnl1) == pl2%nodes(1)) dir(2) = 1
    dir(3) = -1
    IF ((pl2%nodes(1) == pl3%nodes(1)) .OR. (pl2%nodes(nnl2) == pl3%nodes(1))) dir(3) = 1
    dir(4) = -1
    IF ((pl3%nodes(1) == pl4%nodes(1)) .OR. (pl3%nodes(nnl3) == pl4%nodes(1))) dir(4) = 1

    ! Check for inconsistency in element divisions
    IF ((pl1%nelems /= pl3%nelems) .OR. (pl2%nelems /= pl4%nelems)) THEN
        WRITE (ounit,'(A)') 'ERROR: Inconsistencies at element divisions.'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Create surface
    nsurf = nsurf + 1
    ALLOCATE(new_surf)
    new_surf%number = label
    new_surf%lines(1) = pl1%number
    new_surf%lines(2) = pl2%number
    new_surf%lines(3) = pl3%number
    new_surf%lines(4) = pl4%number
    new_surf%nelems(1) = nnl1 - 1
    new_surf%nelems(2) = nnl2 - 1
    ALLOCATE(new_surf%nodes(nnl1,nnl2))
    new_surf%next => NULL()

    ! Surface boundary nodes
    new_surf%nodes(1:nnl1,1) = pl1%nodes(1:nnl1)
    IF (dir(2) > 0) THEN
        new_surf%nodes(nnl1,1:nnl2) = pl2%nodes(1:nnl2)
    ELSE
        new_surf%nodes(nnl1,nnl2:1:-1) = pl2%nodes(1:nnl2)
    END IF
    IF (dir(3) > 0) THEN
        new_surf%nodes(nnl1:1:-1,nnl2) = pl3%nodes(1:nnl1)
    ELSE
        new_surf%nodes(1:nnl1,nnl2) = pl3%nodes(1:nnl1)
    END IF
    IF (dir(4) > 0) THEN
        new_surf%nodes(1,nnl2:1:-1) = pl4%nodes(1:nnl2)
    ELSE
        new_surf%nodes(1,1:nnl2) = pl4%nodes(1:nnl2)
    END IF

    ! Create nodes at at the surface
    DO j = 2, (nnl2 - 1)
        x1 =  node_coords(new_surf%nodes(1,j),0)
        x2 =  node_coords(new_surf%nodes(nnl1,j),0)
        dx = (x2 - x1) / (nnl1 - 1)
        DO i = 2, (nnl1 - 1)
            nlabel = max_node + 1
            new_surf%nodes(i,j) = nlabel
            x1 = x1 + dx
            CALL create_node(nlabel,x1)
        END DO
    END DO

    ! Add surface to the linked list
    IF (.NOT. ASSOCIATED(last_surf)) THEN
        first_surf => new_surf
        last_surf => new_surf
    ELSE
        last_surf%next => new_surf
        last_surf => new_surf
    END IF

    END SUBROUTINE create_surf4
    !
    !
    !
    SUBROUTINE deallocate_surf()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !
    TYPE(surf_type), POINTER :: nexts,currs

    nexts => first_surf
    DO WHILE (ASSOCIATED(nexts))
        currs => nexts
        nexts => currs%next
        DEALLOCATE(currs%nodes)
        DEALLOCATE(currs)
    END DO

    END SUBROUTINE deallocate_surf
    !
    !
    !
    SUBROUTINE meshh8(s1, s2, s3, s4, s5, s6, mat)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read hexahedral mesh bounded by six surfaces
    !
    INTEGER(ipk), INTENT(IN) :: s1, s2, s3, s4, s5, s6, mat
    INTEGER(ipk), ALLOCATABLE :: nodes(:,:,:), nodess(:,:)
    TYPE(surf_type), POINTER :: ps1, ps2, ps3, ps4, ps5, ps6, currs
    INTEGER(ipk) :: i, j, k, l, m, n, nn1, nn2, nn3, nfound, nnl, node1, node2, label, enodes(8), line2
    INTEGER(ipk), POINTER :: nodesl(:)
    REAL(r8k) :: x1(3), x2(3), dx(3), x_node(3)

    nfound = 0
    currs => first_surf
    DO WHILE (ASSOCIATED(currs))
        IF (currs%number == s1) THEN
            nfound = nfound + 1
            ps1 => currs
        END IF
        IF (currs%number == s2) THEN
            nfound = nfound + 1
            ps2 => currs
        END IF
        IF (currs%number == s3) THEN
            nfound = nfound + 1
            ps3 => currs
        END IF
        IF (currs%number == s4) THEN
            nfound = nfound + 1
            ps4 => currs
        END IF
        IF (currs%number == s5) THEN
            nfound = nfound + 1
            ps5 => currs
        END IF
        IF (currs%number == s6) THEN
            nfound = nfound + 1
            ps6 => currs
        END IF
        currs => currs%next
    END DO

    ! Check wether all the surfaces were found
    IF (nfound /= 6) THEN
        WRITE (ounit,'(A)') 'ERROR: mesh uses undefined surface(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Number of nodes
    nn1 = SIZE(ps1%nodes,1)
    nn2 = SIZE(ps1%nodes,2)
    IF (ps1%lines(1) == ps2%lines(1)) THEN
        nn3 = SIZE(ps2%nodes,2)
    ELSE IF (ps1%lines(1) == ps2%lines(2)) THEN
        nn3 = SIZE(ps2%nodes,1)
    ELSE IF (ps1%lines(1) == ps2%lines(3)) THEN
        nn3 = SIZE(ps2%nodes,2)
    ELSE IF (ps1%lines(1) == ps2%lines(4)) THEN
        nn3 = SIZE(ps2%nodes,1)
    END IF

    ! Boundary nodes
    nnl = MAX(nn1,nn2,nn3)
    ALLOCATE(nodes(nn1,nn2,nn3),nodess(nnl,nnl))

    ! Nodes at bottom
    nodes(1:nn1,1:nn2,1) = ps1%nodes(1:nn1,1:nn2) 

    ! Nodes at sides
    DO i = 1, 4
        SELECT CASE (i)
        CASE (1)
            currs => ps2
            nnl = nn1
            nodesl => ps1%nodes(1:nn1,1)
        CASE (2)
            currs => ps3
            nnl = nn2
            nodesl => ps1%nodes(nn1,1:nn2)
        CASE (3)
            currs => ps4
            nnl = nn1
            nodesl => ps1%nodes(nn1:1:-1,nn2)
        CASE (4)
            currs => ps5
            nnl = nn2
            nodesl => ps1%nodes(1,nn2:1:-1)
        END SELECT
        DO j = 1, 4
            IF (ps1%lines(i) == currs%lines(j)) EXIT
        END DO
        IF (i == 1) THEN
            k = j + 2
            IF (k > 4) k = k - 4
            line2 = currs%lines(k)
        END IF
        SELECT CASE (j)
        CASE (1)
            nodess(1:nnl,1:nn3) = currs%nodes(1:nnl,1:nn3)
        CASE (2)
            DO k = 1, nnl
                DO l = 1, nn3
                    m = nn3 + 1 - l
                   nodess(k,l) = currs%nodes(m,k)
                END DO
            END DO
        CASE (3)
            DO k = 1, nnl
                m = nnl + 1 - k
                DO l = 1, nn3
                    n = nn3 + 1 - l
                    nodess(k,l) = currs%nodes(m,n)
                END DO
            END DO
        CASE (4)
            DO k = 1, nnl
                m = nnl + 1 - k
                DO l = 1, nn3
                    nodess(k,l) = currs%nodes(l,m)
                END DO
            END DO
        END SELECT
        IF (nodesl(1) /= nodess(1,1)) THEN
            nodess(1:nnl,1:nn3) = nodess(nnl:1:-1,1:nn3)
        END IF

        SELECT CASE (i)
        CASE (1)
            nodes(1:nn1,1,1:nn3) = nodess(1:nn1,1:nn3)
        CASE (2)
            nodes(nn1,1:nn2,1:nn3) = nodess(1:nn2,1:nn3)
        CASE (3)
            nodes(nn1:1:-1,nn2,1:nn3) = nodess(1:nn1,1:nn3)
        CASE (4)
            nodes(1,nn2:1:-1,1:nn3) = nodess(1:nn2,1:nn3)
        END SELECT
    END DO

    ! Top nodes
    DO j = 1, 4
        IF (ps6%lines(j) == line2) EXIT
    END DO
    SELECT CASE (j)
    CASE (1)
        nodess(1:nn1,1:nn2) = ps6%nodes(1:nn1,1:nn2)
    CASE (2)
        DO k = 1, nn1
            DO l = 1, nn2
                m = nn2 + 1 - l
                nodess(k,l) = ps6%nodes(m,k)
            END DO
        END DO
    CASE (3)
        DO k = 1, nn1
            m = nn1 + 1 - k
            DO l = 1, nn2
                n = nn2 + 1 - l
                nodess(k,l) = ps6%nodes(m,n)
            END DO
        END DO
    CASE (4)
        DO k = 1, nn1
            m = nn1 + 1 - k
            DO l = 1, nn2
                nodess(k,l) = ps6%nodes(l,m)
            END DO
        END DO
    END SELECT
    IF (nodes(1,1,nn3) /= nodess(1,1)) nodess(1:nn1,1:nn2) = nodess(nn1:1:-1,1:nn2)

    DO j = 2, (nn2 - 1)
        DO i = 2, (nn1 - 1)
            nodes(i,j,nn3) = nodess(i,j)
        END DO
    END DO

    ! Create nodes inside the volume
    DO k = 2, (nn3 - 1)
        DO j = 2, (nn2 - 1)
            node1 = nodes(1,j,k)
            node2 = nodes(nn1,j,k)
            x1 = node_coords(node1,0)
            x2 = node_coords(node2,0)
            dx = (x2 - x1) / (nn1 - 1)
            x_node = x1
            DO i = 2, (nn1 - 1)
                label = max_node + 1
                x_node = x_node + dx
                CALL create_node (label, x_node)
                nodes(i,j,k) = label
            END DO
        END DO
    END DO

    ! Mesh volume with HEX8 elements
    DO k = 1, (nn3 - 1)
        DO j = 1, (nn2 - 1)
            DO i = 1, (nn1 - 1)
                label = nhex8 + 1
                enodes(1) = nodes(i,j+1,k)
                enodes(2) = nodes(i,j,k)
                enodes(3) = nodes(i+1,j,k)
                enodes(4) = nodes(i+1,j+1,k)
                enodes(5) = nodes(i,j+1,k+1)
                enodes(6) = nodes(i,j,k+1)
                enodes(7) = nodes(i+1,j,k+1)
                enodes(8) = nodes(i+1,j+1,k+1)
                CALL hex8_create(label,mat,enodes)
            END DO
        END DO
    END DO

    END SUBROUTINE meshh8
    !
    !
    !
    SUBROUTINE meshq4 (l1, l2, l3, l4, mat, etype)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create an element mesh by four bounding lines
    !
    INTEGER(ipk), INTENT(IN) :: l1, l2, l3, l4, mat, etype
    INTEGER(ipk) :: i, j, dir(4), n1, n2, enodes(4), label, nnl1, nnl2, nnl3, nnl4
    TYPE(line_type), POINTER :: pl1, pl2, pl3, pl4, nextl
    INTEGER(ipk), ALLOCATABLE :: nodes1(:), nodes2(:)
    REAL(r8k) :: x1(3), x2(3), dx(3)

    pl1 => NULL()
    pl2 => NULL()
    pl3 => NULL()
    pl4 => NULL()
    nextl => first_line
    DO WHILE (ASSOCIATED(nextl))
        IF (nextl%number == l1) pl1 => nextl
        IF (nextl%number == l2) pl2 => nextl
        IF (nextl%number == l3) pl3 => nextl
        IF (nextl%number == l4) pl4 => nextl
        nextl => nextl%next
    END DO

    ! Check wether all the lines were found
    IF ((.NOT. ASSOCIATED(pl1)) .OR. (.NOT. ASSOCIATED(pl2)) .OR. (.NOT. ASSOCIATED(pl3)) .OR. (.NOT. ASSOCIATED(pl4))) THEN
        WRITE (ounit,'(A)') 'ERROR: Mesh uses undefined line(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Node number at lines
    nnl1 = pl1%nelems + 1
    nnl2 = pl2%nelems + 1
    nnl3 = pl3%nelems + 1
    nnl4 = pl4%nelems + 1

    ! Second line
    IF ((pl1%nodes(nnl1) == pl3%nodes(1)) .OR. (pl1%nodes(nnl1) == pl3%nodes(nnl3))) THEN
        nextl => pl2
        pl2 => pl3
        pl3 => nextl
        nnl2 = pl2%nelems + 1
        nnl3 = pl3%nelems + 1
    ELSE IF ((pl1%nodes(nnl1) == pl4%nodes(1)) .OR. (pl1%nodes(nnl1) == pl4%nodes(nnl4))) THEN
        nextl => pl2
        pl2 => pl4
        pl4 => nextl
        nnl2 = pl2%nelems + 1
        nnl4 = pl4%nelems + 1
    END IF

    ! Third line
    IF ((pl2%nodes(1) == pl4%nodes(1)) .OR. (pl2%nodes(1) == pl4%nodes(nnl4)).OR. &
      & (pl2%nodes(nnl2) == pl4%nodes(1)) .OR. (pl2%nodes(nnl2) == pl4%nodes(nnl4))) THEN
        nextl => pl3
        pl3 => pl4
        pl4 => nextl
        nnl3 = pl3%nelems + 1
        nnl4 = pl4%nelems + 1
    END IF

    ! Direction in lines
    dir(1) = 1
    dir(2) = -1
    IF (pl1%nodes(nnl1) == pl2%nodes(1)) dir(2) = 1
    dir(3) = -1
    IF ((pl2%nodes(1) == pl3%nodes(1)).OR. (pl2%nodes(nnl2) == pl3%nodes(1))) dir(3) = 1
    dir(4) = -1
    IF ((pl3%nodes(1) == pl4%nodes(1)).OR. (pl3%nodes(nnl3) == pl4%nodes(1))) dir(4) = 1

    ! Check for inconsistency in element divisions
    IF ((pl1%nelems /= pl3%nelems).OR. (pl2%nelems /= pl4%nelems)) THEN
        WRITE (ounit,'(A)') 'ERROR: Inconsistencies at element divisions.'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Create nodes and elements
    ALLOCATE(nodes1(nnl1),nodes2(nnl1))
    nodes1(1:nnl1) = pl1%nodes(1:nnl1)
    DO i = 1, (nnl2 - 2)
        IF (dir(2) > 0) THEN
            n2 = pl2%nodes(i+1)
            x2 = node_coords(n2,0)
        ELSE
            n2 = pl2%nodes(nnl2-i)
            x2 = node_coords(n2,0)
        END IF
        IF (dir(4) < 0) THEN
            n1 = pl4%nodes(i+1)
            x1 = node_coords(n1,0)
        ELSE
            n1 = pl4%nodes(nnl2-i)
            x1 = node_coords(n1,0)
        END IF
        nodes2(1) = n1
        nodes2(nnl1) = n2
        dx = (x2 - x1) / (nnl1 - 1)

        DO j = 2, (nnl1 - 1)
            label = max_node + 1
            x1 = x1 + dx
            CALL create_node(label,x1)
            nodes2(j) = label
        END DO

        ! make elements using nodes1 and nodes2
        DO j = 1, (nnl1 - 1)
            label = nquad4 + 1
            enodes = (/nodes1(j),nodes1(j+1),nodes2(j+1),nodes2(j)/)
            SELECT CASE (etype)
            CASE (0)
                CALL quad4_create (label, mat, enodes)
            CASE (1)
                CALL gascav2d_create (label, mat, enodes, 5)
            CASE (2)
                IF (i == 1) THEN
                    CALL gascav2d_create (label, mat, enodes, 1)
                ELSE
                    CALL gascav2d_create (label, mat, enodes, 0)
                END IF
            END SELECT
        END DO
        nodes1 = nodes2
    END DO
    IF (dir(3) < 0) THEN
        nodes2(1:nnl1) = pl3%nodes(1:nnl1)
    ELSE
        nodes2(1:nnl1) = pl3%nodes(nnl1:1:-1)
    END IF
    ! make elements using nodes1 and nodes2
    DO j = 1, (nnl1 - 1)
        label = nquad4 + 1
        enodes = (/nodes1(j),nodes1(j+1),nodes2(j+1),nodes2(j)/)
        SELECT CASE (etype)
        CASE (0)
            CALL quad4_create (label, mat, enodes)
        CASE (1)
            CALL gascav2d_create (label, mat, enodes, 5)
        CASE (2)
            IF (nnl2 > 2) THEN
                CALL gascav2d_create (label, mat, enodes, 0)
            ELSE
                CALL gascav2d_create (label, mat, enodes, 1)
            END IF
        END SELECT
    END DO

    DEALLOCATE(nodes1,nodes2)

    END SUBROUTINE meshq4
    !
    !
    !
    SUBROUTINE meshs1d(n1, n2, n3, n4, ne, mat, etype, flag)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create SOLID1D mesh by using four nodes
    !
    INTEGER(ipk), INTENT(IN) :: n1, n2, n3, n4, ne, mat, etype, flag
    TYPE(node_type), POINTER :: pn1, pn2, pn3, pn4, nextn
    INTEGER(ipk) :: ie, number, nodes(4)
    REAL(r8k) :: dr, x_node(3)

    pn1 => NULL()
    pn2 => NULL()
    pn3 => NULL()
    pn4 => NULL()
    nextn => first_node
    DO WHILE (ASSOCIATED(nextn))
        IF (nextn%label == n1) pn1 => nextn
        IF (nextn%label == n2) pn2 => nextn
        IF (nextn%label == n3) pn3 => nextn
        IF (nextn%label == n4) pn4 => nextn
        nextn => nextn%next
    END DO

    ! Check wether all the lines were found
    IF ((.NOT. ASSOCIATED(pn1)) .OR. (.NOT. ASSOCIATED(pn2)) .OR. (.NOT. ASSOCIATED(pn3)) .OR. (.NOT. ASSOCIATED(pn4))) THEN
        WRITE (ounit,'(A)') 'ERROR: Mesh uses undefined nodes(s).'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! width of element
    dr = (pn2%x0(1) - pn1%x0(1)) / ne

    DO ie = 1, ne
        IF (ie < ne) THEN
            number = max_node + 1
            x_node(1) = pn1%x0(1) + dr
            x_node(2) = pn1%x0(2)
            x_node(3) = 0.0_r8k
            CALL create_node (number, x_node)
            nextn => last_node
        ELSE
            nextn => pn2
        END IF
        number = nsolid1d + 1
        nodes = (/ pn1%label, nextn%label, pn3%label, pn4%label /)
        SELECT CASE (etype)
        CASE (0)
            CALL solid1d_create (number, mat, nodes, flag)
        CASE (1)
            CALL gascav1d_create (number, mat, nodes, .FALSE.)
        END SELECT
        pn1 => nextn
    END DO

    END SUBROUTINE meshs1d
    !
    !
    !
    SUBROUTINE write_geometry (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Output geometry
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(line_type), POINTER :: currl
    TYPE(surf_type), POINTER :: currs

    ! Output lines data
    WRITE (UNIT=nunit) nlines
    currl => first_line
    DO WHILE (ASSOCIATED(currl))
        WRITE (UNIT=nunit) currl%number
        WRITE (UNIT=nunit) currl%nelems
        WRITE (UNIT=nunit) currl%nodes(1:currl%nelems+1)
        currl => currl%next
    END DO

    ! Output surface data
    WRITE (UNIT=nunit) nsurf
    currs => first_surf
    DO WHILE (ASSOCIATED(currs))
        WRITE (UNIT=nunit) currs%number
        WRITE (UNIT=nunit) currs%nelems(1:2)
        WRITE (UNIT=nunit) currs%lines(1:4)
        WRITE (UNIT=nunit) currs%nodes(1:currs%nelems(1)+1,1:currs%nelems(2)+1)
        currs => currs%next
    END DO

    END SUBROUTINE write_geometry
    !
    !
    !
    SUBROUTINE read_geometry (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Output geometry
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(line_type), POINTER :: currl, prevl
    TYPE(surf_type), POINTER :: currs, prevs

    ! Read lines
    READ(UNIT=nunit,ERR=10,END=10) nlines
    currl => first_line
    DO i = 1, nlines
        IF (.NOT. ASSOCIATED(currl)) THEN
            ALLOCATE(currl)
            currl%nodes => NULL()
            currl%next => NULL()
            IF (.NOT. ASSOCIATED(first_line)) THEN
                first_line => currl
                last_line => currl
            ELSE
                last_line%next => currl
                last_line => currl
            END IF
        END IF
        IF (ASSOCIATED(currl%nodes)) DEALLOCATE(currl%nodes)
        READ(UNIT=nunit,ERR=10,END=10) currl%number
        READ(UNIT=nunit,ERR=10,END=10) currl%nelems
        ALLOCATE(currl%nodes(currl%nelems+1))
        READ(UNIT=nunit,ERR=10,END=10) currl%nodes(1:currl%nelems+1)
        last_line => currl
        currl => currl%next
    END DO

    ! Remove excess line entries from the database
    DO WHILE (ASSOCIATED(currl))
        prevl => currl
        currl => currl%next
        DEALLOCATE(prevl)
    END DO
    IF (ASSOCIATED(last_line)) last_line%next => NULL()
    IF (nlines == 0) THEN
        first_line => NULL()
        last_line => NULL()
    END IF

    ! Read surfaces
    READ(UNIT=nunit,ERR=10,END=10) nsurf
    currs => first_surf
    DO i = 1, nsurf
        IF (.NOT. ASSOCIATED(currs)) THEN
            ALLOCATE(currs)
            currs%next => NULL()
            IF (.NOT. ASSOCIATED(first_surf)) THEN
                first_surf => currs
                last_surf => currs
            ELSE
                last_surf%next => currs
                last_surf => currs
            END IF
        END IF
        IF (ASSOCIATED(currs%nodes)) DEALLOCATE(currs%nodes)
        READ(UNIT=nunit,ERR=10,END=10) currs%number
        READ(UNIT=nunit,ERR=10,END=10) currs%nelems(1:2)
        READ(UNIT=nunit,ERR=10,END=10) currs%lines(1:4)
        ALLOCATE(currs%nodes(currs%nelems(1)+1,currs%nelems(2)+1))
        READ(UNIT=nunit,ERR=10,END=10) currs%nodes(1:currs%nelems(1)+1,1:currs%nelems(2)+1)
        last_surf => currs
        currs => currs%next
    END DO

    ! Remove excess surf entries from the database
    DO WHILE (ASSOCIATED(currs))
        prevs => currs
        currs => currs%next
        DEALLOCATE(prevs)
    END DO
    IF (ASSOCIATED(last_surf)) last_surf%next => NULL()
    IF (nsurf == 0) THEN
        first_surf => NULL()
        last_surf => NULL()
    END IF

    RETURN

10  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading geometry database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE read_geometry
    !
    !
    !
    SUBROUTINE deallocate_geometry()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate all geometric entities

    CALL deallocate_lines()
    CALL deallocate_surf()

    END SUBROUTINE deallocate_geometry
    !
    !
    !
    SUBROUTINE create_coupled (label, node, idim)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> Add DOF to the coupled set or create a new coupled set if it doesn't exist yet
    !
    INTEGER(ipk), INTENT(IN) :: label, node, idim
    TYPE(coupled_set), POINTER :: current_cs, new_cs
    TYPE(node_type), POINTER :: current_node

    ! Try to find the coupled set
    current_cs => first_coupled_set
    DO WHILE (ASSOCIATED(current_cs))
        IF (current_cs%label == label) EXIT
        current_cs => current_cs%next
    END DO

    ! Create a new coupled set If necessary
    IF (.NOT. ASSOCIATED(current_cs)) THEN
        ! Create coupled
        ncoupled = ncoupled + 1
        ALLOCATE(new_cs)
        new_cs%label = label
        new_cs%number = ncoupled
        new_cs%forced = .FALSE.
        new_cs%u_forced = 0.0_r8k
        new_cs%u = 0.0_r8k
        new_cs%force = 0.0_r8k
        new_cs%next => NULL()
        current_cs => new_cs

        ! Add coupled set to the database
        IF (.NOT. ASSOCIATED(first_coupled_set)) THEN
            first_coupled_set => new_cs
            last_coupled_set => new_cs
        ELSE
            last_coupled_set%next => new_cs
            last_coupled_set => new_cs
        END IF
    END IF

    ! Find the node and mark the correct DOF to the coupled set
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        IF (current_node%label == node) THEN
            current_node%dof_status(idim) = current_cs%label
            EXIT
        END IF
        current_node => current_node%next
    END DO

    ! DOF renumbering is needed
    dof_numbering = .TRUE.

    END SUBROUTINE create_coupled
    !
END MODULE geometry_fpn_frapcon



