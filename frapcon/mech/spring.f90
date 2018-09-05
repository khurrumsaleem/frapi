MODULE spring
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE math
    USE materials
    USE sparse_matrix
    USE variables_frapcon, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Spring: 2-noded two or three dimensional linear spring element 
    !> (takes into account geometric nonlinearities)
    TYPE spring_type
        ! Element database
        INTEGER(ipk) :: label  ! Label of the element
        INTEGER(ipk) :: number ! Internal number of the element
        INTEGER(ipk) :: mat    ! Material label
        INTEGER(ipk) :: egroup ! Element group label
        INTEGER(ipk) :: node_labels(2)  ! Element node labels
        INTEGER(ipk) :: node_numbers(2) ! Internal node numbering
        REAL(r8k) :: gtemp   ! Spring average temperature
        REAL(r8k) :: kt      ! Temperature dependent spring stiffness
        REAL(r8k) :: length0 ! original length
        REAL(r8k) :: length  ! current length
        REAL(r8k) :: force   ! spring force
        REAL(r8k) :: energy  ! Elastic energy stored in spring
        TYPE(spring_type), POINTER :: next
    END TYPE spring_type
    TYPE(spring_type), POINTER :: &
        first_spring, & ! Pointer to the first spring element
        last_spring     ! Pointer to the last spring elemen
    INTEGER(ipk) :: nspring
    !
    CONTAINS
    !
    SUBROUTINE spring_init()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pointers
    !
    CALL spring_deallocate()
    !
    nspring = 0
    first_spring => NULL()
    last_spring => NULL()
    !
    END SUBROUTINE spring_init
    !
    !
    !
    SUBROUTINE spring_create(label,mat,nodes)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new SPRING element
    !
    INTEGER(ipk), INTENT(IN) :: label, mat
    INTEGER(ipk) :: nfound, in, jn
    TYPE(spring_type), POINTER :: new_spring
    INTEGER(ipk), INTENT(IN), DIMENSION(2) :: nodes
    REAL(r8k), DIMENSION(3) :: dx
    !
    nspring = nspring + 1
    ALLOCATE(new_spring)
    new_spring%label = label
    new_spring%number = nspring
    new_spring%mat = mat
    new_spring%egroup = egroup
    CALL mat_create(mat)
    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 2
            IF (nodes(jn) == node_labels(in)) THEN
                new_spring%node_labels(jn) = nodes(jn)
                new_spring%node_numbers(jn) = in
                enumber(in) = enumber(in) + 1
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 2) EXIT
    END DO
    ! Check whether all element nodes were found in node database
    IF (nfound /= 2) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR incompatible NODES and SPRING data'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Initialize spring length and force
    dx(1:nd) = x0(1:nd,new_spring%node_numbers(2)) - x0(1:nd,new_spring%node_numbers(1))
    new_spring%length0 = vnorm(nd,dx)
    new_spring%next => NULL()

    ! Add new SPRING element to the database
    IF (.NOT. ASSOCIATED(first_spring)) THEN
        first_spring => new_spring
        last_spring => new_spring
    ELSE
        last_spring%next => new_spring
        last_spring => new_spring
    END IF

    END SUBROUTINE spring_create
    !
    !
    !
    SUBROUTINE spring_sparse_matrix()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for spring elements
    !
    TYPE(spring_type), POINTER :: current
    INTEGER(ipk), DIMENSION(6) :: dofnum
    !
    current => first_spring
    DO WHILE (ASSOCIATED(current))
        dofnum(1:nd) = dof_number(1:nd,current%node_numbers(1))
        dofnum(nd+1:2*nd) = dof_number(1:nd,current%node_numbers(2))
        CALL sparse_matrix_add_element(2*nd,dofnum)
        current => current%next
    END DO
    !
    END SUBROUTINE spring_sparse_matrix
    !
    !
    !
    SUBROUTINE spring_temp()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate temperature dependent properties of the element
    !
    INTEGER(ipk) :: im
    REAL(r8k) :: tg
    TYPE(spring_type), POINTER :: current
    !
    current => first_spring
    DO WHILE (ASSOCIATED(current))
       im = current%mat
       tg = 0.5_r8k * (temp(current%node_numbers(1)) + temp(current%node_numbers(2)))
       current%gtemp = tg
       current%kt = mat_par(im,tg,'EMOD    ')
       current => current%next
    END DO
    !
    END SUBROUTINE spring_temp
    !
    !
    !
    SUBROUTINE spring_fint()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Internal force vector
    !
    INTEGER(ipk) :: im,id
    REAL(r8k) :: dx(3),k,tmp
    REAL(r8k), POINTER :: length,length0,force,energy
    TYPE(spring_type), POINTER :: current
    !
    current => first_spring
    DO WHILE (ASSOCIATED(current))
        im = current%mat
        length => current%length
        length0 => current%length0
        force => current%force
        energy => current%energy
        k = current%kt
        dx(1:nd) = x(1:nd,current%node_numbers(2)) - x(1:nd,current%node_numbers(1))

        ! Current length of spring
        length = vnorm(nd,dx)

        ! Spring force
        force = k * (length - length0)

        ! Stored elastic energy
        energy = 0.5_r8k * force * (length - length0)

        ! Internal forces
        tmp = k * (1.0_r8k - length0 / length)
        DO id = 1, nd
            Fint(id,current%node_numbers(1)) = Fint(id,current%node_numbers(1)) - tmp * dx(id)
        END DO
        DO id = 1, nd
            Fint(id,current%node_numbers(2)) = Fint(id,current%node_numbers(2)) + tmp * dx(id)
        END DO
        current => current%next
    END DO
    !
    END SUBROUTINE spring_fint
    !
    !
    !
    SUBROUTINE spring_stiff()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the spring element
    !
    REAL(r8k) :: length,length0,k,dx(3),tmp1,tmp2 
    REAL(r8k), ALLOCATABLE :: Ke(:,:)
    INTEGER(ipk) :: i,j,dofnum(6)
    TYPE(spring_type), POINTER :: current
    !
    ALLOCATE(Ke(2*nd,2*nd))
    !
    current => first_spring
    DO WHILE (ASSOCIATED(current))
        length = current%length
        length0 = current%length0
        k = current%kt
        dx(1:nd) = x(1:nd,current%node_numbers(2)) - x(1:nd,current%node_numbers(1))
        tmp1 = k * length0 / length ** 3
        tmp2 = k * (1.0_r8k - length0 / length)
        ke = 0.0_r8k
        DO i = 1, nd
            DO j = i, nd
                ke(i,j) = tmp1 * dx(i) * dx(j)
                ke(j,i) = ke(i,j)
                ke(3 + i,3 + j) = ke(i,j)
                ke(3 + j,3 + i) = ke(i,j)
                ke(i,3 + j) = -ke(i,j)
                ke(3 + j,i) = -ke(i,j)
                ke(j,3 + i) = -ke(i,j)
                ke(3 + i,j) = -ke(i,j)
            END DO
            ke(i,i) = ke(i,i) + tmp2
            ke(i,3 + i) = ke(i,3 + i) - tmp2
            ke(3 + i,i) = ke(i,3 + i)
            ke(3 + i,3 + i) = ke(i,i)
        END DO
        dofnum(1:nd) = dof_number(1:nd,current%node_numbers(1))
        dofnum(nd+1:2*nd) = dof_number(1:nd,current%node_numbers(2))
        ! Place element matrix in the global matrix
        CALL sparse_matrix_place_element(2*nd,dofnum,ke)
        current => current%next
    END DO

    DEALLOCATE(Ke)

    END SUBROUTINE spring_stiff
    !
    !
    !
    SUBROUTINE spring_write_output(nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write output to a file in unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(spring_type), POINTER :: current

    WRITE (UNIT=nunit) nspring
    current => first_spring
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%mat
        WRITE (UNIT=nunit) current%node_labels
        WRITE (UNIT=nunit) current%node_numbers
        WRITE (UNIT=nunit) current%force
        current => current%next
    END DO

    END SUBROUTINE spring_write_output
    !
    !
    !
    SUBROUTINE spring_read_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(spring_type), POINTER :: current,previous

    READ(UNIT=nunit,ERR=20,End=20) nspring

    current => first_spring
    DO i = 1, nspring
        IF (.NOT. ASSOCIATED(current)) THEN
            ALLOCATE(current)
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_spring)) THEN
                first_spring => current
                last_spring => current
            ELSE
                last_spring%next => current
                last_spring => current
            END IF
        END IF
        READ(UNIT=nunit,ERR=20,End=20) current%label
        READ(UNIT=nunit,ERR=20,End=20) current%mat
        READ(UNIT=nunit,ERR=20,End=20) current%node_labels
        READ(UNIT=nunit,ERR=20,End=20) current%node_numbers
        READ(UNIT=nunit,ERR=20,End=20) current%force
        last_spring => current
        current => current%next
    END DO

    ! Remove excess element entries from the database
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_spring)) last_spring%next => NULL()
    IF (nspring == 0) THEN
        first_spring => NULL()
        last_spring => NULL()
    END IF

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading SPRING database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE spring_read_output
    !
    !
    !
    SUBROUTINE spring_deallocate()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate element database
    !
    TYPE(spring_type), POINTER :: current,previous

    current => first_spring
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE spring_deallocate
    !
END MODULE spring

