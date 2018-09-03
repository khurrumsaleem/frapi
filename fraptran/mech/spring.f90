MODULE spring
    USE Kinds
    USE common_parameters
    USE math
    USE materials_frap
    USE sparse_matrix
    USE Variables, ONLY : ounit
    !>@brief
    !> Spring: 2-noded two or three Dimensional linear spring element
    !> (takes into account geometric nonlinearities)
    IMPLICIT NONE
    TYPE spring_type
        ! Element Database
        INTEGER(ipk) :: label ! Label of the element
        INTEGER(ipk) :: number ! Internal number of the element
        INTEGER(ipk) :: mat ! Material label
        INTEGER(ipk) :: egroup ! Element group label
        INTEGER(ipk) :: node_labels(2) ! Element node labels
        INTEGER(ipk) :: node_numbers(2) ! Internal node numbering
        REAL(r8k) :: gtemp ! Spring average temperature
        REAL(r8k) :: kt ! Temperature dependent spring stIffness
        REAL(r8k) :: length0 ! original length
        REAL(r8k) :: length ! current length
        REAL(r8k) :: force ! spring force
        REAL(r8k) :: energy ! Elastic energy stored in spring
        TYPE(spring_type), POINTER :: next
    END TYPE spring_type
    TYPE(spring_type), POINTER :: &
        first_spring, & ! POINTER to the first spring element
        last_spring ! POINTER to the last spring elemen
    INTEGER(ipk) :: nspring
    !
    CONTAINS
    !
    SUBROUTINE spring_init()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Initialize POINTERs

    CALL spring_deallocate()

    nspring = 0
    first_spring => NULL()
    last_spring => NULL()

    END SUBROUTINE spring_init
    !
    !
    !
    SUBROUTINE spring_create (label, mat, nodes)
    USE Kinds
    IMPLICIT NONE
    ! Create new SPRING element
    INTEGER(ipk), INTENT(IN) :: label,mat,nodes(2)
    INTEGER(ipk) :: nfound,in,jn
    TYPE(spring_type), POINTER :: new_spring
    REAL(r8k) :: dx(3)
    !
    nspring = nspring + 1
    ALLOCATE(new_spring)
    new_spring%label = label
    new_spring%number = nspring
    new_spring%mat = mat
    new_spring%egroup = egroup
    CALL mat_create(mat)

    ! Find element nodes in node Database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 2
            IF ( nodes(jn) == node_labels(in) ) THEN
                 new_spring%node_labels(jn) = nodes(jn)
                 new_spring%node_numbers(jn) = in
                 enumber(in) = enumber(in) + 1
                 nfound = nfound + 1
            ENDIF
        ENDDO
        IF ( nfound == 2 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 2 ) THEN
        WRITE(ounit,FMT='(/,A,/)') 'ERROR incompatible NODES and SPRING Data'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    ENDIF

    ! Initialize spring length and force
    dx(1:nd) = x0(1:nd,new_spring%node_numbers(2)) - x0(1:nd,new_spring%node_numbers(1))
    new_spring%length0 = vnorm(nd,dx)
    new_spring%next => NULL()

    ! Add new SPRING element to the Database
    IF (.NOT. ASSOCIATED(first_spring)) THEN
        first_spring => new_spring
        last_spring => new_spring
    ELSE
        last_spring%next => new_spring
        last_spring => new_spring
    ENDIF

    END SUBROUTINE spring_create
    !
    !
    !
    SUBROUTINE spring_sparse_matrix()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for spring elements
    TYPE(spring_type), POINTER :: current
    INTEGER(ipk) :: dofnum(6)

    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1:nd) = dof_number(1:nd,current%node_numbers(1))
       dofnum(nd+1:2*nd) = dof_number(1:nd,current%node_numbers(2))
       CALL sparse_matrix_add_element(2*nd,dofnum)
       current => current%next
    ENDDO

  END SUBROUTINE spring_sparse_matrix


    SUBROUTINE spring_temp()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Calculate temperature dependent properties of the element
    TYPE(spring_type), POINTER :: current
    REAL(r8k) :: tg
    INTEGER(ipk) :: im

    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       im = current%mat
       tg = 0.5_r8k* &
            (temp(current%node_numbers(1)) + temp(current%node_numbers(2)))
       current%gtemp = tg
       current%kt = mat_par(im,tg,'EMOD    ')
       current => current%next
    ENDDO

  END SUBROUTINE spring_temp


    SUBROUTINE spring_fint()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Internal force vector
    INTEGER(ipk) :: im,id
    REAL(r8k) :: dx(3),k,tmp
    REAL(r8k), POINTER :: length,length0,force,energy
    TYPE(spring_type), POINTER :: current

    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       im = current%mat
       length => current%length
       length0 => current%length0
       force => current%force
       energy => current%energy
       k = current%kt
       dx(1:nd) = x(1:nd,current%node_numbers(2)) - &
            x(1:nd,current%node_numbers(1))

       ! Current length of spring
       length = vnorm(nd,dx)

       ! Spring force
       force = k*( length - length0 )

       ! Stored elastic energy
       energy = 0.5_r8k*force*( length - length0 )

       ! Internal forces
       tmp = k*( 1.0_r8k - length0/length )
       DO id=1,nd
          Fint(id,current%node_numbers(1)) = &
               Fint(id,current%node_numbers(1)) - tmp*dx(id)
       ENDDO
       DO id=1,nd
          Fint(id,current%node_numbers(2)) = &
               Fint(id,current%node_numbers(2)) + tmp*dx(id)
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE spring_fint


    SUBROUTINE spring_stIff()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the spring element
    REAL(r8k) :: length,length0,k,tmp1,tmp2
    REAL(r8k), DIMENSION(3) :: dx
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: Ke
    INTEGER(ipk) :: i,j
    INTEGER(ipk), DIMENSION(6) :: dofnum
    TYPE(spring_type), POINTER :: current

    ALLOCATE(Ke(2*nd,2*nd))

    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       length = current%length
       length0 = current%length0
       k = current%kt
       dx(1:nd) = x(1:nd,current%node_numbers(2)) - &
            x(1:nd,current%node_numbers(1))
       tmp1 = k*length0/length**3
       tmp2 = k*(1.0_r8k - length0/length)
       ke = 0.0_r8k
       DO i=1,nd
          DO j=i,nd
             ke(i,j) = tmp1*dx(i)*dx(j)
             ke(j,i) = ke(i,j)
             ke(3 + i,3 + j) = ke(i,j)
             ke(3 + j,3 + i) = ke(i,j)
             ke(i,3 + j) = -ke(i,j)
             ke(3 + j,i) = -ke(i,j)
             ke(j,3 + i) = -ke(i,j)
             ke(3 + i,j) = -ke(i,j)
          ENDDO
          ke(i,i) = ke(i,i) + tmp2
          ke(i,3 + i) = ke(i,3 + i) - tmp2
          ke(3 + i,i) = ke(i,3 + i)
          ke(3 + i,3 + i) = ke(i,i)
       ENDDO
       dofnum(1:nd) = dof_number(1:nd,current%node_numbers(1))
       dofnum(nd+1:2*nd) = dof_number(1:nd,current%node_numbers(2))
       ! Place element matrix in the global matrix
       CALL sparse_matrix_place_element(2*nd,dofnum,ke)
       current => current%next
    ENDDO

    DEALLOCATE(Ke)

    RETURN

  END SUBROUTINE spring_stIff


    SUBROUTINE spring_Write_output(nunit)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Write output to a file in unit 'nunit'
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(spring_type), POINTER :: current

    WRITE(UNIT=nunit) nspring
    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%mat
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       WRITE(UNIT=nunit) current%force
       current => current%next
    ENDDO

  END SUBROUTINE spring_Write_output


    SUBROUTINE spring_read_output(nunit)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(spring_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) nspring

    current => first_spring
    DO i=1,nspring
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_spring) ) THEN
             first_spring => current
             last_spring => current
          ELSE
             last_spring%next => current
             last_spring => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%mat
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       Read(UNIT=nunit,ERR=20,End=20) current%force
       last_spring => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_spring) ) last_spring%next => NULL()
    IF ( nspring == 0 ) THEN
       first_spring => NULL()
       last_spring => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading SPRING Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE spring_read_output


    SUBROUTINE spring_deallocate()
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Deallocate element Database
    TYPE(spring_type), POINTER :: current,previous

    current => first_spring
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

  END SUBROUTINE spring_deallocate
END MODULE spring
