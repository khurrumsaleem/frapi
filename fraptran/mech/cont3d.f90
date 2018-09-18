MODULE cont3d
    USE Kinds
    USE common_parameters
    USE math
    USE geometry
    USE materials_frap
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> 3D contact element

  TYPE cont3dsurf_type
     INTEGER(ipk) :: label ! Contact surface label
     INTEGER(ipk) :: mat ! Material label
     INTEGER(ipk) :: ntnodes(2) ! Target surface node numbers
     INTEGER(ipk), DIMENSION(:,:), POINTER :: tnodes ! Target surface nodes
     INTEGER(ipk) :: ncnodes ! Number of contactor nodes
     INTEGER(ipk), DIMENSION(:), POINTER :: cnodes ! Contactor nodes
     TYPE(cont3dsurf_type), POINTER :: next
  END TYPE cont3dsurf_type


  TYPE cont3d_type
     INTEGER(ipk) :: label
     INTEGER(ipk) :: surf ! Surface label that the target surface is associated
     INTEGER(ipk) :: mat ! Element material label
     INTEGER(ipk) :: node_labels(5) ! Element node labels
     INTEGER(ipk) :: node_numbers(5) ! Internal node numbering
     LOGICAL :: closed
     LOGICAL :: closed0
     LOGICAL :: stick
     LOGICAL :: stick0
     REAL(r8k) :: gap
     REAL(r8k) :: xi(2)
     REAL(r8k) :: gap0
     REAL(r8k) :: nv(3)
     REAL(r8k) :: tv(3,2)
     REAL(r8k) :: xi0(2)
     REAL(r8k) :: pfact ! Penalty factor
     TYPE(cont3d_type), POINTER :: next
  END TYPE cont3d_type


  REAL(r8k), PARAMETER :: cont3d_alpha_tol = 0.01_r8k
  REAL(r8k) :: cont3d_search
  TYPE(cont3dsurf_type), POINTER :: first_cont3dsurf,last_cont3dsurf
  TYPE(cont3d_type), POINTER :: first_cont3d, last_cont3d
  INTEGER(ipk) :: ncont3dsurf ! Number of 3D contact surface definitions
  INTEGER(ipk) :: ncont3d ! Number of CONT3D elements

CONTAINS
    SUBROUTINE cont3d_init()
    ! Initialize CONT3D Database
    IMPLICIT NONE

    ! Deallocate CONT3D Database
    CALL cont3d_deallocate()

    ! Initialize variables
    ncont3d = 0
    ncont3dsurf = 0
    first_cont3dsurf => NULL()
    last_cont3dsurf => NULL()
    first_cont3d => NULL()
    last_cont3d => NULL()
    cont3d_search = 2.0_r8k

    RETURN

  END SUBROUTINE cont3d_init


    SUBROUTINE cont3d_create_surf(label,mat,surf)
    ! Create 3D contact surface pair
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,mat,surf(2)
    TYPE(cont3dsurf_type), POINTER :: currs,news
    TYPE(surf_type), POINTER :: surf1,surf2,currgs
    INTEGER(ipk) :: i,j,k

    ! Check that the surface label does not exist already
    currs => first_cont3dsurf
    DO WHILE ( ASSOCIATED(currs) )
       IF ( currs%label == label ) THEN
          WRITE(UNIT=6,FMT='(A,I0)') &
               'ERROR two definitions for contact surface ',label
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ENDIF
       currs => currs%next
    ENDDO

    ! Find target surface
    surf1 => NULL()
    surf2 => NULL()
    currgs => first_surf
    DO WHILE ( ASSOCIATED(currgs) )
       IF ( currgs%number == surf(1) ) surf1 => currgs
       IF ( currgs%number == surf(2) ) surf2 => currgs
       currgs => currgs%next
    ENDDO

    ! Check that both surfaces were found
    IF ( ( .NOT.ASSOCIATED(surf1) ).OR.( .NOT.ASSOCIATED(surf2) ) ) THEN
       WRITE(UNIT=6,FMT='(A,I0)') &
            'ERROR cannot find surfaces for 3D contact surface ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Create new contact surface
    ALLOCATE(news)
    news%label = label
    news%mat = mat
    news%ntnodes = surf1%nelems + 1
    ALLOCATE(news%tnodes(news%ntnodes(1),news%ntnodes(2)))
    news%tnodes = surf1%nodes
    news%ncnodes = (surf2%nelems(1) + 1)*(surf2%nelems(2) + 1)
    ALLOCATE(news%cnodes(news%ncnodes))
    DO i=1,surf2%nelems(1) + 1
       DO j=1,surf2%nelems(2) + 1
          k = (i - 1)*(surf2%nelems(2) + 1) + j
          news%cnodes(k) = surf2%nodes(i,j)
       ENDDO
    ENDDO
    news%next => NULL()
    CALL mat_create(mat)

    ! Attach contact surface definition to the Database
    IF ( .NOT.ASSOCIATED(first_cont3dsurf) ) THEN
       first_cont3dsurf => news
       last_cont3dsurf => news
    ELSE
       last_cont3dsurf%next => news
       last_cont3dsurf => news
    ENDIF

    RETURN

  END SUBROUTINE cont3d_create_surf


    SUBROUTINE cont3d_delete_surf(label)
    ! Delete contact surface
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(cont3dsurf_type), POINTER :: current_surf,previous_surf,tobedeleted
    TYPE(cont3d_type), POINTER :: current,previous,tbd
    LOGICAL :: lfound

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_cont3dsurf) ) RETURN

    ! Mark first memeber to be deleted in Database
    lfound = .FALSE.
    IF ( first_cont3dsurf%label == label ) THEN
       tobedeleted => first_cont3dsurf
       first_cont3dsurf => first_cont3dsurf%next
       lfound = .TRUE.
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current_surf => first_cont3dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       IF ( current_surf%label == label ) THEN
          previous_surf%next => current_surf%next
          tobedeleted => current_surf
          lfound = .TRUE.
       ENDIF
       previous_surf => current_surf
       current_surf => current_surf%next
    ENDDO

    ! Set the POINTER to the last CONT3D element
    last_cont3dsurf => NULL()
    current_surf => first_cont3dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       last_cont3dsurf => current_surf
       current_surf => current_surf%next
    ENDDO
    
    ! Deallocate 3D contact surface
    IF ( lfound ) THEN
       ncont3dsurf = ncont3dsurf - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted%tnodes,tobedeleted%cnodes)
       DEALLOCATE(tobedeleted)
    ENDIF

    ! Delete contact elements associated to the surface
    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%surf == label ) THEN
          IF ( current%label == first_cont3d%label ) THEN
             tbd => current
             current => current%next
             first_cont3d => current
             IF ( ASSOCIATED(first_cont3d) ) first_cont3d%label = 1
             ncont3d = ncont3d - 1
             DEALLOCATE(tbd)
          ELSE
             previous%next => current%next
             tbd => current
             current => current%next
             IF ( ASSOCIATED(current) ) current%label = tbd%label
             ncont3d = ncont3d - 1
             DEALLOCATE(tbd)
          ENDIF
       ELSE
          previous => current
          current => current%next
       ENDIF
    ENDDO

    ! Set POINTER to the last contact element
    last_cont3d => NULL()
    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       last_cont3d => current
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont3d_delete_surf


    SUBROUTINE cont3d_activate()
    ! Activate elements in 3D contact surface
    IMPLICIT NONE
    TYPE(cont3dsurf_type), POINTER :: currs
    INTEGER(ipk) :: i,j,k,nodes(5),n1,n2,label
    REAL(r8k) :: x1(3),x2(3),x3(3),x4(3),x5(3),xm(3),x_tmp(3),length,dist
    TYPE(cont3d_type), POINTER :: curre

    currs => first_cont3dsurf
    DO WHILE ( ASSOCIATED(currs) )
       n1 = currs%ntnodes(1)
       n2 = currs%ntnodes(2)
       DO i=1,n1-1
          DO j=1,n2-1
             nodes(1:2) = currs%tnodes(i:i+1,j)
             nodes(3:4) = currs%tnodes(i+1:i:-1,j+1)
             x1 = node_coords(nodes(1),1)
             x2 = node_coords(nodes(2),1)
             x3 = node_coords(nodes(3),1)
             x4 = node_coords(nodes(4),1)
             xm = x3 - x1
             length = vnorm(3,xm)
             xm = x4 - x2
             length = vnorm(3,xm)
             length = 0.5_r8k*length
             xm = (x1 + x2 + x3 + x4)/4.0_r8k
             DO k=1,currs%ncnodes
                nodes(5) = currs%cnodes(k)
                x5 = node_coords(currs%cnodes(k),1)
                x_tmp = x5 - xm
                dist = vnorm(3,x_tmp)
                IF ( dist < cont3d_search*length ) THEN
                   ! Create contact element If the contactor node is close
                   label = ncont3d + 1
                   CALL cont3d_create(label,currs%mat,currs%label,nodes)
                ELSE
                   ! Remove element If the contactor node is far away
                   curre => first_cont3d
                   DO WHILE ( ASSOCIATED(curre) )
                      IF ( curre%surf == currs%label ) THEN
                         IF ( ( curre%node_labels(1) == nodes(1) ).AND. &
                              ( curre%node_labels(5) == nodes(5) ) ) THEN
                            label = curre%label
                            EXIT
                         ENDIF
                      ENDIF
                      curre => curre%next
                   ENDDO
                   CALL cont3d_delete(label)
                ENDIF
             ENDDO
          ENDDO
       ENDDO
       currs => currs%next
    ENDDO

    RETURN

  END SUBROUTINE cont3d_activate


    SUBROUTINE cont3d_create(label,mat,surf,nodes)
    ! Create 3D contact element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,mat,surf,nodes(5)
    INTEGER(ipk) :: nfound,i,j
    REAL(r8k) :: x_node(3,5),xi_b,shapef(4,4)
    TYPE(cont3d_type), POINTER :: new_cont3d,current

    ! If element with given nodes already exists RETURN and Do nothing
    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       IF ( ( current%node_labels(5) == nodes(5) ).AND. &
            ( current%node_labels(4) == nodes(4) ).AND. &
            ( current%node_labels(3) == nodes(3) ).AND. &
            ( current%node_labels(2) == nodes(2) ).AND. &
            ( current%node_labels(1) == nodes(1) ) ) RETURN
       current => current%next
    ENDDO

    ! Allocate new element
    ncont3d = ncont3d + 1
    ALLOCATE(new_cont3d)

    ! Find element nodes in node Database
    nfound = 0
    DO i=1,nnodes
       DO j=1,5
          IF ( node_labels(i) == nodes(j) ) THEN
             new_cont3d%node_labels(j) = nodes(j)
             new_cont3d%node_numbers(j) = i
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 5 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 5 ) THEN
       WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR incompatible NODES and CONT3D Data'
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Initialize CONT3D variables
    new_cont3d%label = label
    new_cont3d%surf = surf
    new_cont3d%mat = mat
    new_cont3d%closed0 = .FALSE.
    new_cont3d%stick0 = .FALSE.
    new_cont3d%next => NULL()

    ! Initial values for contact parameters

    ! gap function
    DO i=1,5
       x_node(1:3,i) = x(1:3,new_cont3d%node_numbers(i))
    ENDDO
    CALL cont3d_gap(x_node,new_cont3d%gap0,new_cont3d%nv,new_cont3d%tv, &
         new_cont3d%xi0,shapef)

    ! closing of the contact
    xi_b = 1.0_r8k + cont3d_alpha_tol
    IF ( ( new_cont3d%gap0 < 0.0_r8k ).AND. &
         ( new_cont3d%xi0(1) > -xi_b ).AND.( new_cont3d%xi0(1) < xi_b ).AND. &
         ( new_cont3d%xi0(2) > -xi_b ).AND.( new_cont3d%xi0(2) < xi_b ) ) THEN
       current%closed0 = .TRUE.
    ENDIF

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_cont3d) ) THEN
       first_cont3d => new_cont3d
       last_cont3d => new_cont3d
    ELSE
       last_cont3d%next => new_cont3d
       last_cont3d => new_cont3d
    ENDIF

    RETURN

  END SUBROUTINE cont3d_create


    SUBROUTINE cont3d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(cont3d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_cont3d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_cont3d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_cont3d
       first_cont3d => first_cont3d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       IF ( lfound ) THEN
          current%label = current%label - 1
       ELSE If ( current%label == label ) THEN
          lfound = .TRUE.
          previous%next => current%next
          tobedeleted => current
       ENDIF
       previous => current
       current => current%next
    ENDDO

    ! Set the POINTER to the last CONT3D element
    last_cont3d => NULL()
    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       last_cont3d => current
       current => current%next
    ENDDO
    
    ! Deallocate CONT3D element
    IF ( lfound ) THEN
       ncont3d = ncont3d - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE cont3d_delete


    SUBROUTINE cont3d_sparse_matrix()
    ! Initialize sparse matrix storage for cont3d elements
    IMPLICIT NONE
    TYPE(cont3d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(15)

    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1:3) = dof_number(1:3,current%node_numbers(1))
       dofnum(4:6) = dof_number(1:3,current%node_numbers(2))
       dofnum(7:9) = dof_number(1:3,current%node_numbers(3))
       dofnum(10:12) = dof_number(1:3,current%node_numbers(4))
       dofnum(13:15) = dof_number(1:3,current%node_numbers(5))
       CALL sparse_matrix_add_element(15,dofnum)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont3d_sparse_matrix


    SUBROUTINE cont3d_gap(x_node,gap,nv,tv,xi,shapef)
    ! Calculation of gap and normal vector for CONT3D.
    ! The target surface formed by four nodes is not plane. So, the solution
    ! of the distance and normal vector to the contactor node is iterative
    ! process. The gap and normal vector are solved finding the minimum for the
    ! square of distance from the target surface to the contactor node using
    ! Newtons method.
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: x_node(3,5)
    REAL(r8k), INTENT(OUT) :: gap,nv(3),tv(3,2),xi(2),shapef(4,4)
    INTEGER(ipk) :: iter,id,jd,in
    REAL(r8k) :: f,df(2),ax(4,3),jac(2,2),dfnorm,detj,dxi(2),length

    ! Closest point projection at target surface for the contactor node
    dfnorm = 1.0_r8k
    xi = 0.0_r8k
    dxi = 1.0_r8k
    iter = 0
    DO WHILE ( dfnorm > 1e-10_r8k )
       iter = iter + 1

       ! Shape functions for the target surface
       shapef(1,1) = (1.0_r8k - xi(1))*(1.0_r8k - xi(2))*0.25_r8k
       shapef(1,2) = (1.0_r8k + xi(1))*(1.0_r8k - xi(2))*0.25_r8k
       shapef(1,3) = (1.0_r8k + xi(1))*(1.0_r8k + xi(2))*0.25_r8k
       shapef(1,4) = (1.0_r8k - xi(1))*(1.0_r8k + xi(2))*0.25_r8k

       ! Derivatives of the shape functions
       shapef(2,1) = -(1.0_r8k - xi(2))*0.25_r8k
       shapef(2,2) =  (1.0_r8k - xi(2))*0.25_r8k
       shapef(2,3) =  (1.0_r8k + xi(2))*0.25_r8k
       shapef(2,4) = -(1.0_r8k + xi(2))*0.25_r8k
       shapef(3,1) = -(1.0_r8k - xi(1))*0.25_r8k
       shapef(3,2) = -(1.0_r8k + xi(1))*0.25_r8k
       shapef(3,3) =  (1.0_r8k + xi(1))*0.25_r8k
       shapef(3,4) =  (1.0_r8k - xi(1))*0.25_r8k

       ! Second derivatives of the shape functions
       shapef(4,1) =  0.25_r8k
       shapef(4,2) = -0.25_r8k
       shapef(4,3) =  0.25_r8k
       shapef(4,4) = -0.25_r8k

       ! Point at target surface
       DO id=1,3
          DO jd=1,4
             ax(jd,id) = 0.0
          ENDDO
          DO in=1,4
             DO jd=1,4
                ax(jd,id) = ax(jd,id) + shapef(jd,in)*x_node(id,in)
             ENDDO
          ENDDO
       ENDDO

       ! square of the length of the vector AP
       f = 0.0
       DO id=1,3
          f = f + (x_node(id,5) - ax(1,id))**2
       ENDDO

       ! derivatives of f
       df = 0.0
       DO id=1,3
          df(1) = df(1) + 2.0*(x_node(id,5) - ax(1,id))*ax(2,id)
          df(2) = df(2) + 2.0*(x_node(id,5) - ax(1,id))*ax(3,id)
       ENDDO

       ! Norm of df
       dfnorm = df(1)**2 + df(2)**2

       ! Jacobian of f
       jac = 0.0
       DO id=1,3
          jac(1,1) = jac(1,1) - 2.0_r8k*ax(2,id)**2
          jac(2,2) = jac(2,2) - 2.0_r8k*ax(3,id)**2
          jac(2,1) = jac(2,1) + 2.0_r8k*(x_node(id,5) - ax(1,id))*ax(4,id) - &
               2.0_r8k*ax(2,id)*ax(3,id)
       ENDDO
       jac(1,2) = jac(2,1)

       ! Determinant of jacobian
       detj = jac(1,1)*jac(2,2) - jac(2,1)**2
       dxi(1) = -(jac(2,2)*df(1) - jac(1,2)*df(2))/detj
       dxi(2) = -(-jac(2,1)*df(1) + jac(1,1)*df(2))/detj
       xi = xi + dxi
       WRITE(*,'(A)',ADVANCE='NO') ''
    ENDDO

    ! First tangent vector
    length = SQRT(ax(2,1)**2 + ax(2,2)**2 + ax(2,3)**2)
    DO id=1,3
       tv(id,1) = ax(2,id)/length
    ENDDO

    ! Second tangent vector
    length = SQRT(ax(3,1)**2 + ax(3,2)**2 + ax(3,3)**2)
    DO id=1,3
       tv(id,2) = ax(3,id)/length
    ENDDO

    ! Normal vector
    nv(1) = ax(2,2)*ax(3,3) - ax(2,3)*ax(3,2)
    nv(2) = ax(2,3)*ax(3,1) - ax(2,1)*ax(3,3)
    nv(3) = ax(2,1)*ax(3,2) - ax(2,2)*ax(3,1)
    length = SQRT(nv(1)**2 + nv(2)**2 + nv(3)**2)
    DO id=1,3
       nv(id) = nv(id)/length
    ENDDO

    ! Gap function
    gap = 0.0_r8k
    DO id=1,3
       gap = gap + (x_node(id,5) - ax(1,id))*nv(id)
    ENDDO

    RETURN

  END SUBROUTINE cont3d_gap


    SUBROUTINE cont3d_fint()
    ! Nodal contact forces
    IMPLICIT NONE
    TYPE(cont3d_type), POINTER :: current
    INTEGER(ipk) :: ip,in
    REAL(r8k) :: x_node(3,5),xi_b,shapef(4,4)
    REAL(r8k), POINTER :: pfact,gap
    REAL(r8k), DIMENSION(:), POINTER :: nv, xi
    REAL(r8k), DIMENSION(:,:), POINTER :: tv

    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       ! Assign POINTERs
       pfact => current%pfact
       gap => current%gap
       nv => current%nv(1:3)
       tv => current%tv(1:3,1:2)
       xi => current%xi(1:2)

       ! Penalty factor
       pfact = mat_par(current%mat,tref,'PENALTY ')

       ! Node coordinates
       DO in=1,5
          x_node(1:3,in) = x(1:3,current%node_numbers(in))
       ENDDO

       ! Gap function
       CALL cont3d_gap(x_node,gap,nv,tv,xi,shapef)

       ! Check for closure
       current%closed = .FALSE.
       xi_b = 1.0_r8k + cont3d_alpha_tol
       IF ( ( gap < 0.0_r8k ).AND. &
            ( xi(1) > -xi_b ).AND.( xi(1) < xi_b ).AND. &
            ( xi(2) > -xi_b ).AND.( xi(2) < xi_b ) ) THEN
          current%closed = .TRUE.
       ENDIF

       ! Normal penalty forces
       IF ( current%closed ) THEN
          DO in=1,4
             ip = current%node_numbers(in)
             !Fint(1:3,ip) = Fint(1:3,ip) + shapef(1,in)*pfact*gap*nv(1:3)
             Fext(1:3,ip) = Fext(1:3,ip) - shapef(1,in)*pfact*gap*nv(1:3)
          ENDDO
          ip = current%node_numbers(5)
          !Fint(1:3,ip) = Fint(1:3,ip) - pfact*gap*nv(1:3)
          Fext(1:3,ip) = Fext(1:3,ip) + pfact*gap*nv(1:3)
       ENDIF

       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont3d_fint


    SUBROUTINE cont3d_stIff()
    ! 3D contact stIffness matrix
    IMPLICIT NONE
    TYPE(cont3d_type), POINTER :: current
    REAL(r8k) :: x_node(3,5),Ke(15,15)
    INTEGER(ipk) :: i,dofnum(15)

    current => first_cont3d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%closed ) THEN
          ! Element node coordinates
          DO i=1,5
             x_node(1:3,i) = x(1:3,current%node_numbers(i))
          ENDDO

          ! Normal penalty part for the stIffness matrix
          CALL cont3d_stIff_normal(x_node,current%xi,current%pfact,Ke)

          ! DOF numbering
          dofnum(1:3) = dof_number(1:3,current%node_numbers(1))
          dofnum(4:6) = dof_number(1:3,current%node_numbers(2))
          dofnum(7:9) = dof_number(1:3,current%node_numbers(3))
          dofnum(10:12) = dof_number(1:3,current%node_numbers(4))
          dofnum(13:15) = dof_number(1:3,current%node_numbers(5))

          ! Place element matrix in the global matrix
          CALL sparse_matrix_place_element(15,dofnum,Ke)

       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont3d_stIff


    SUBROUTINE cont3d_stIff_normal(x_node,xi,pfact,Ke)
    ! 3D contact stiffness matrix
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: x_node(3,5),xi(2),pfact
    REAL(r8k), INTENT(OUT) :: Ke(15,15)
    REAL(r8k) :: t1, t2, t3, t4, t5, t8, t11, t12, t13, t15, t19, t25, t30, t32, t33, t38, t44, t46, t47, t50, t51, &
      &          t52, t53, t54, t55, t60, t61, t64, t65, t68, t71, t72, t73, t75, t78, t82, t84, t85, t96, t99, t110, &
      &          t113, t117, t118, t119, t120, t127, t128, t129, t133, t134, t138, t141, t144, t145, t146, &
      &          t150, t153, t156, t157, t160, t163, t170, t177, t178, t179, t182, t185, t186, t187, t191, &
      &          t194, t197, t198, t199, t203, t206, t209, t214, t221, t228, t230, t234, t235, t239, t243, &
      &          t244, t248, t252, t258, t260, t261, t273, t274, t277, t284, t287, t290, t291, t301, t306, &
      &          t313, t317, t320, t323, t326, t329, t333, t336, t339, t346, t351, t358, t365, t370, t377, &
      &          t382, t383, t395, t396, t399, t403, t413, t423, t428, t432, t435, t438, t442, t445, t448, &
      &          t451, t454, t461, t468, t473, t480, t487, t492, t497, t501, t503, t504, t516, t517, t533, &
      &          t543, t546, t549, t553, t556, t559, t563, t566, t569, t570, t575, t582, t589, t594, t601, &
      &          t608, t614, t616, t617, t629, t630, t646, t650, t653, t656, t659, t662, t666, t669, t672, &
      &          t673, t680, t685, t692, t699, t704, t711, t716, t717, t729, t730, t740, t743, t746, t750, &
      &          t753, t756, t759, t762, t763, t770, t777, t782, t789, t796, t801, t806, t810, t812, t813, &
      &          t825, t826, t829, t836, t839, t842, t846, t849, t852, t853, t858, t865, t872, t877, t884, &
      &          t891, t897, t899, t900, t912, t913, t916, t923, t926, t929, t930, t940, t945, t952, t959, &
      &          t964, t971, t976, t977, t989, t993, t997, t1007, t1017, t1022, t1029, t1036, t1041, t1046, t1050, &
      &          t1052, t1053, t1065, t1066, t1082, t1092, t1097, t1104, t1111, t1117, t1119, t1120, t1132, t1133, t1149, &
      &          t1156, t1161, t1168, t1173, t1174, t1186, t1200, t1207, t1212, t1213, t1217, t1218, t1219, t1220, t1224

      t1 = 0.1e1_r8k-xi(1)
      t2 = 0.1e1_r8k-xi(2)
      t3 = t1*t2
      t4 = 0.25_r8k*xi(2)
      t5 = -0.25_r8k+t4
      t8 = 0.25_r8k+t4
      t11 = t5*x_node(2,1)-t5*x_node(2,2)+t8*x_node(2,3)-t8*x_node(2,4)
      t12 = 0.25_r8k*xi(1)
      t13 = -0.25_r8k+t12
      t15 = -0.25_r8k-t12
      t19 = t13*x_node(3,1)+t15*x_node(3,2)-t15*x_node(3,3)-t13*x_node(3,4)
      t25 = t5*x_node(3,1)-t5*x_node(3,2)+t8*x_node(3,3)-t8*x_node(3,4)
      t30 = t13*x_node(2,1)+t15*x_node(2,2)-t15*x_node(2,3)-t13*x_node(2,4)
      t32 = t11*t19-t25*t30
      t33 = t32**2
      t38 = t13*x_node(1,1)+t15*x_node(1,2)-t15*x_node(1,3)-t13*x_node(1,4)
      t44 = t5*x_node(1,1)-t5*x_node(1,2)+t8*x_node(1,3)-t8*x_node(1,4)
      t46 = t25*t38-t44*t19
      t47 = t46**2
      t50 = t44*t30-t11*t38
      t51 = t50**2
      t52 = t33+t47+t51
      t53 = SQRT(t52)
      t54 = 1/t53
      t55 = t32*t54
      t60 = 0.1e1_r8k+xi(1)
      t61 = t60*t2
      t64 = 0.1e1_r8k+xi(2)
      t65 = t60*t64
      t68 = t1*t64
      t71 = x_node(1,5)-0.25_r8k*t3*x_node(1,1)-0.25_r8k*t61*x_node(1,2)- &
           0.25_r8k*t65*x_node(1,3)-0.25_r8k*t68*x_node(1,4)
      t72 = t71*t32
      t73 = t53**2
      t75 = 1/t73/t53
      t78 = t25*t13-t5*t19
      t82 = t5*t30-t11*t13
      t84 = t46*t78+t50*t82
      t85 = 2*t75*t84
      t96 = x_node(2,5)-0.25_r8k*t3*x_node(2,1)-0.25_r8k*t61*x_node(2,2)- &
           0.25_r8k*t65*x_node(2,3)-0.25_r8k*t68*x_node(2,4)
      t99 = t96*t46
      t110 = x_node(3,5)-0.25_r8k*t3*x_node(3,1)-0.25_r8k*t61*x_node(3,2)- &
           0.25_r8k*t65*x_node(3,3)-0.25_r8k*t68*x_node(3,4)
      t113 = t110*t50
      t117 = (-0.25_r8k*t3*t55-t72*t85/2+t96*t78*t54-t99*t85/2+t110*t82*t54- &
           t113*t85/2)*pfact
      t118 = t117*t1
      t119 = t2*t32
      t120 = t119*t54
      t127 = (t72*t54+t99*t54+t113*t54)*pfact
      t128 = t127*t1
      t129 = t119*t85
      t133 = t2*t46
      t134 = t133*t54
      t138 = t2*t78*t54
      t141 = t133*t85
      t144 = 0.25_r8k*t118*t134+0.25_r8k*t128*t138-0.125_r8k*t128*t141
      t145 = t2*t50
      t146 = t145*t54
      t150 = t2*t82*t54
      t153 = t145*t85
      t156 = 0.25_r8k*t118*t146+0.25_r8k*t128*t150-0.125_r8k*t128*t153
      t157 = t117*t60
      t160 = t127*t60
      t163 = 0.25_r8k*t157*t120-0.125_r8k*t160*t129
      t170 = 0.25_r8k*t157*t134+0.25_r8k*t160*t138-0.125_r8k*t160*t141
      t177 = 0.25_r8k*t157*t146+0.25_r8k*t160*t150-0.125_r8k*t160*t153
      t178 = t64*t32
      t179 = t178*t54
      t182 = t178*t85
      t185 = 0.25_r8k*t157*t179-0.125_r8k*t160*t182
      t186 = t64*t46
      t187 = t186*t54
      t191 = t64*t78*t54
      t194 = t186*t85
      t197 = 0.25_r8k*t157*t187+0.25_r8k*t160*t191-0.125_r8k*t160*t194
      t198 = t64*t50
      t199 = t198*t54
      t203 = t64*t82*t54
      t206 = t198*t85
      t209 = 0.25_r8k*t157*t199+0.25_r8k*t160*t203-0.125_r8k*t160*t206
      t214 = 0.25_r8k*t118*t179-0.125_r8k*t128*t182
      t221 = 0.25_r8k*t118*t187+0.25_r8k*t128*t191-0.125_r8k*t128*t194
      t228 = 0.25_r8k*t118*t199+0.25_r8k*t128*t203-0.125_r8k*t128*t206
      t230 = t32*t75
      t234 = -t117*t55+t127*t230*t84
      t235 = t46*t54
      t239 = t46*t75
      t243 = -t117*t235-t127*t78*t54+t127*t239*t84
      t244 = t50*t54
      t248 = t50*t75
      t252 = -t117*t244-t127*t82*t54+t127*t248*t84
      t258 = t44*t13-t5*t38
      t260 = -t32*t78+t50*t258
      t261 = 2*t75*t260
      t273 = (-t71*t78*t54-t72*t261/2-0.25_r8k*t3*t235-t99*t261/2+t110*t258*t54-t113*t261/2)*pfact
      t274 = t273*t1
      t277 = t133*t261
      t284 = t2*t258*t54
      t287 = t145*t261
      t290 = 0.25_r8k*t274*t146+0.25_r8k*t128*t284-0.125_r8k*t128*t287
      t291 = t273*t60
      t301 = 0.25_r8k*t291*t120-0.25_r8k*t160*t2*t78*t54-0.125_r8k*t160*t119*t261
      t306 = 0.25_r8k*t291*t134-0.125_r8k*t160*t277
      t313 = 0.25_r8k*t291*t146+0.25_r8k*t160*t284-0.125_r8k*t160*t287
      t317 = -t64*t78*t54
      t320 = t178*t261
      t323 = 0.25_r8k*t291*t179+0.25_r8k*t160*t317-0.125_r8k*t160*t320
      t326 = t186*t261
      t329 = 0.25_r8k*t291*t187-0.125_r8k*t160*t326
      t333 = t64*t258*t54
      t336 = t198*t261
      t339 = 0.25_r8k*t291*t199+0.25_r8k*t160*t333-0.125_r8k*t160*t336
      t346 = 0.25_r8k*t274*t179+0.25_r8k*t128*t317-0.125_r8k*t128*t320
      t351 = 0.25_r8k*t274*t187-0.125_r8k*t128*t326
      t358 = 0.25_r8k*t274*t199+0.25_r8k*t128*t333-0.125_r8k*t128*t336
      t365 = -t273*t55+t127*t78*t54+t127*t230*t260
      t370 = -t273*t235+t127*t239*t260
      t377 = -t273*t244-t127*t258*t54+t127*t248*t260
      t382 = -t32*t82-t46*t258
      t383 = 2*t75*t382
      t395 = (-t71*t82*t54-t72*t383/2-t96*t258*t54-t99*t383/2-0.25_r8k*t3*t244-t113*t383/2)*pfact
      t396 = t395*t1
      t399 = t145*t383
      t403 = t395*t60
      t413 = 0.25_r8k*t403*t120-0.25_r8k*t160*t2*t82*t54-0.125_r8k*t160*t119*t383
      t423 = 0.25_r8k*t403*t134-0.25_r8k*t160*t2*t258*t54-0.125_r8k*t160*t133*t383
      t428 = 0.25_r8k*t403*t146-0.125_r8k*t160*t399
      t432 = -t64*t82*t54
      t435 = t178*t383
      t438 = 0.25_r8k*t403*t179+0.25_r8k*t160*t432-0.125_r8k*t160*t435
      t442 = -t64*t258*t54
      t445 = t186*t383
      t448 = 0.25_r8k*t403*t187+0.25_r8k*t160*t442-0.125_r8k*t160*t445
      t451 = t198*t383
      t454 = 0.25_r8k*t403*t199-0.125_r8k*t160*t451
      t461 = 0.25_r8k*t396*t179+0.25_r8k*t128*t432-0.125_r8k*t128*t435
      t468 = 0.25_r8k*t396*t187+0.25_r8k*t128*t442-0.125_r8k*t128*t445
      t473 = 0.25_r8k*t396*t199-0.125_r8k*t128*t451
      t480 = -t395*t55+t127*t82*t54+t127*t230*t382
      t487 = -t395*t235+t127*t258*t54+t127*t239*t382
      t492 = -t395*t244+t127*t248*t382
      t497 = t25*t15+t5*t19
      t501 = -t5*t30-t11*t15
      t503 = t46*t497+t50*t501
      t504 = 2*t75*t503
      t516 = (-0.25_r8k*t61*t55-t72*t504/2+t96*t497*t54-t99*t504/2+t110*t501*t54-t113*t504/2)*pfact
      t517 = t516*t60
      t533 = 0.25_r8k*t517*t134+0.25_r8k*t160*t2*t497*t54-0.125_r8k*t160*t133*t504
      t543 = 0.25_r8k*t517*t146+0.25_r8k*t160*t2*t501*t54-0.125_r8k*t160*t145*t504
      t546 = t178*t504
      t549 = 0.25_r8k*t517*t179-0.125_r8k*t160*t546
      t553 = t64*t497*t54
      t556 = t186*t504
      t559 = 0.25_r8k*t517*t187+0.25_r8k*t160*t553-0.125_r8k*t160*t556
      t563 = t64*t501*t54
      t566 = t198*t504
      t569 = 0.25_r8k*t517*t199+0.25_r8k*t160*t563-0.125_r8k*t160*t566
      t570 = t516*t1
      t575 = 0.25_r8k*t570*t179-0.125_r8k*t128*t546
      t582 = 0.25_r8k*t570*t187+0.25_r8k*t128*t553-0.125_r8k*t128*t556
      t589 = 0.25_r8k*t570*t199+0.25_r8k*t128*t563-0.125_r8k*t128*t566
      t594 = -t516*t55+t127*t230*t503
      t601 = -t516*t235-t127*t497*t54+t127*t239*t503
      t608 = -t516*t244-t127*t501*t54+t127*t248*t503
      t614 = t44*t15+t5*t38
      t616 = -t32*t497+t50*t614
      t617 = 2*t75*t616
      t629 = (-t71*t497*t54-t72*t617/2-0.25_r8k*t61*t235-t99*t617/2+t110*t614*t54-t113*t617/2)*pfact
      t630 = t629*t60
      t646 = 0.25_r8k*t630*t146+0.25_r8k*t160*t2*t614*t54-0.125_r8k*t160*t145*t617
      t650 = -t64*t497*t54
      t653 = t178*t617
      t656 = 0.25_r8k*t630*t179+0.25_r8k*t160*t650-0.125_r8k*t160*t653
      t659 = t186*t617
      t662 = 0.25_r8k*t630*t187-0.125_r8k*t160*t659
      t666 = t64*t614*t54
      t669 = t198*t617
      t672 = 0.25_r8k*t630*t199+0.25_r8k*t160*t666-0.125_r8k*t160*t669
      t673 = t629*t1
      t680 = 0.25_r8k*t673*t179+0.25_r8k*t128*t650-0.125_r8k*t128*t653
      t685 = 0.25_r8k*t673*t187-0.125_r8k*t128*t659
      t692 = 0.25_r8k*t673*t199+0.25_r8k*t128*t666-0.125_r8k*t128*t669
      t699 = -t629*t55+t127*t497*t54+t127*t230*t616
      t704 = -t629*t235+t127*t239*t616
      t711 = -t629*t244-t127*t614*t54+t127*t248*t616
      t716 = -t32*t501-t46*t614
      t717 = 2*t75*t716
      t729 = (-t71*t501*t54-t72*t717/2-t96*t614*t54-t99*t717/2-0.25_r8k*t61*t244-t113*t717/2)*pfact
      t730 = t729*t60
      t740 = -t64*t501*t54
      t743 = t178*t717
      t746 = 0.25_r8k*t730*t179+0.25_r8k*t160*t740-0.125_r8k*t160*t743
      t750 = -t64*t614*t54
      t753 = t186*t717
      t756 = 0.25_r8k*t730*t187+0.25_r8k*t160*t750-0.125_r8k*t160*t753
      t759 = t198*t717
      t762 = 0.25_r8k*t730*t199-0.125_r8k*t160*t759
      t763 = t729*t1
      t770 = 0.25_r8k*t763*t179+0.25_r8k*t128*t740-0.125_r8k*t128*t743
      t777 = 0.25_r8k*t763*t187+0.25_r8k*t128*t750-0.125_r8k*t128*t753
      t782 = 0.25_r8k*t763*t199-0.125_r8k*t128*t759
      t789 = -t729*t55+t127*t501*t54+t127*t230*t716
      t796 = -t729*t235+t127*t614*t54+t127*t239*t716
      t801 = -t729*t244+t127*t248*t716
      t806 = -t25*t15-t8*t19
      t810 = t8*t30+t11*t15
      t812 = t46*t806+t50*t810
      t813 = 2*t75*t812
      t825 = (-0.25_r8k*t65*t55-t72*t813/2+t96*t806*t54-t99*t813/2+t110*t810*t54-t113*t813/2)*pfact
      t826 = t825*t60
      t829 = t178*t813
      t836 = t64*t806*t54
      t839 = t186*t813
      t842 = 0.25_r8k*t826*t187+0.25_r8k*t160*t836-0.125_r8k*t160*t839
      t846 = t64*t810*t54
      t849 = t198*t813
      t852 = 0.25_r8k*t826*t199+0.25_r8k*t160*t846-0.125_r8k*t160*t849
      t853 = t825*t1
      t858 = 0.25_r8k*t853*t179-0.125_r8k*t128*t829
      t865 = 0.25_r8k*t853*t187+0.25_r8k*t128*t836-0.125_r8k*t128*t839
      t872 = 0.25_r8k*t853*t199+0.25_r8k*t128*t846-0.125_r8k*t128*t849
      t877 = -t825*t55+t127*t230*t812
      t884 = -t825*t235-t127*t806*t54+t127*t239*t812
      t891 = -t825*t244-t127*t810*t54+t127*t248*t812
      t897 = -t44*t15-t8*t38
      t899 = -t32*t806+t50*t897
      t900 = 2*t75*t899
      t912 = (-t71*t806*t54-t72*t900/2-0.25_r8k*t65*t235-t99*t900/2+t110*t897*t54-t113*t900/2)*pfact
      t913 = t912*t60
      t916 = t186*t900
      t923 = t64*t897*t54
      t926 = t198*t900
      t929 = 0.25_r8k*t913*t199+0.25_r8k*t160*t923-0.125_r8k*t160*t926
      t930 = t912*t1
      t940 = 0.25_r8k*t930*t179-0.25_r8k*t128*t64*t806*t54-0.125_r8k*t128*t178*t900
      t945 = 0.25_r8k*t930*t187-0.125_r8k*t128*t916
      t952 = 0.25_r8k*t930*t199+0.25_r8k*t128*t923-0.125_r8k*t128*t926
      t959 = -t912*t55+t127*t806*t54+t127*t230*t899
      t964 = -t912*t235+t127*t239*t899
      t971 = -t912*t244-t127*t897*t54+t127*t248*t899
      t976 = -t32*t810-t46*t897
      t977 = 2*t75*t976
      t989 = (-t71*t810*t54-t72*t977/2-t96*t897*t54-t99*t977/2-0.25_r8k*t65*t244-t113*t977/2)*pfact
      t993 = t198*t977
      t997 = t989*t1
      t1007 = 0.25_r8k*t997*t179-0.25_r8k*t128*t64*t810*t54-0.125_r8k*t128*t178*t977
      t1017 = 0.25_r8k*t997*t187-0.25_r8k*t128*t64*t897*t54-0.125_r8k*t128*t186*t977
      t1022 = 0.25_r8k*t997*t199-0.125_r8k*t128*t993
      t1029 = -t989*t55+t127*t810*t54+t127*t230*t976
      t1036 = -t989*t235+t127*t897*t54+t127*t239*t976
      t1041 = -t989*t244+t127*t248*t976
      t1046 = -t25*t13+t8*t19
      t1050 = -t8*t30+t11*t13
      t1052 = t46*t1046+t50*t1050
      t1053 = 2*t75*t1052
      t1065 = (-0.25_r8k*t68*t55-t72*t1053/2+t96*t1046*t54-t99*t1053/2+t110*t1050*t54-t113*t1053/2)*pfact
      t1066 = t1065*t1
      t1082 = 0.25_r8k*t1066*t187+0.25_r8k*t128*t64*t1046*t54-0.125_r8k*t128*t186*t1053
      t1092 = 0.25_r8k*t1066*t199+0.25_r8k*t128*t64*t1050*t54-0.125_r8k*t128*t198*t1053
      t1097 = -t1065*t55+t127*t230*t1052
      t1104 = -t1065*t235-t127*t1046*t54+t127*t239*t1052
      t1111 = -t1065*t244-t127*t1050*t54+t127*t248*t1052
      t1117 = -t44*t13+t8*t38
      t1119 = -t32*t1046+t50*t1117
      t1120 = 2*t75*t1119
      t1132 = (-t71*t1046*t54-t72*t1120/2-0.25_r8k*t68*t235-t99*t1120/2+t110*t1117*t54-t113*t1120/2)*pfact
      t1133 = t1132*t1
      t1149 = 0.25_r8k*t1133*t199+0.25_r8k*t128*t64*t1117*t54-0.125_r8k*t128*t198*t1120
      t1156 = -t1132*t55+t127*t1046*t54+t127*t230*t1119
      t1161 = -t1132*t235+t127*t239*t1119
      t1168 = -t1132*t244-t127*t1117*t54+t127*t248*t1119
      t1173 = -t32*t1050-t46*t1117
      t1174 = 2*t75*t1173
      t1186 = (-t71*t1050*t54-t72*t1174/2-t96*t1117*t54-t99*t1174/2-0.25_r8k*t68*t244-t113*t1174/2)*pfact
      t1200 = -t1186*t55+t127*t1050*t54+t127*t230*t1173
      t1207 = -t1186*t235+t127*t1117*t54+t127*t239*t1173
      t1212 = -t1186*t244+t127*t248*t1173
      t1213 = 1/t52
      t1217 = pfact*t32
      t1218 = t46*t1213*t1217
      t1219 = t50*t1213
      t1220 = t1219*t1217
      t1224 = t1219*pfact*t46
      Ke(1,1) = 0.25_r8k*t118*t120-0.125_r8k*t128*t129
      Ke(1,2) = t144
      Ke(1,3) = t156
      Ke(1,4) = t163
      Ke(1,5) = t170
      Ke(1,6) = t177
      Ke(1,7) = t185
      Ke(1,8) = t197
      Ke(1,9) = t209
      Ke(1,10) = t214
      Ke(1,11) = t221
      Ke(1,12) = t228
      Ke(1,13) = t234
      Ke(1,14) = t243
      Ke(1,15) = t252
      Ke(2,1) = t144
      Ke(2,2) = 0.25_r8k*t274*t134-0.125_r8k*t128*t277
      Ke(2,3) = t290
      Ke(2,4) = t301
      Ke(2,5) = t306
      Ke(2,6) = t313
      Ke(2,7) = t323
      Ke(2,8) = t329
      Ke(2,9) = t339
      Ke(2,10) = t346
      Ke(2,11) = t351
      Ke(2,12) = t358
      Ke(2,13) = t365
      Ke(2,14) = t370
      Ke(2,15) = t377
      Ke(3,1) = t156
      Ke(3,2) = t290
      Ke(3,3) = 0.25_r8k*t396*t146-0.125_r8k*t128*t399
      Ke(3,4) = t413
      Ke(3,5) = t423
      Ke(3,6) = t428
      Ke(3,7) = t438
      Ke(3,8) = t448
      Ke(3,9) = t454
      Ke(3,10) = t461
      Ke(3,11) = t468
      Ke(3,12) = t473
      Ke(3,13) = t480
      Ke(3,14) = t487
      Ke(3,15) = t492
      Ke(4,1) = t163
      Ke(4,2) = t301
      Ke(4,3) = t413
      Ke(4,4) = 0.25_r8k*t517*t120-0.125_r8k*t160*t119*t504
      Ke(4,5) = t533
      Ke(4,6) = t543
      Ke(4,7) = t549
      Ke(4,8) = t559
      Ke(4,9) = t569
      Ke(4,10) = t575
      Ke(4,11) = t582
      Ke(4,12) = t589
      Ke(4,13) = t594
      Ke(4,14) = t601
      Ke(4,15) = t608
      Ke(5,1) = t170
      Ke(5,2) = t306
      Ke(5,3) = t423
      Ke(5,4) = t533
      Ke(5,5) = 0.25_r8k*t630*t134-0.125_r8k*t160*t133*t617
      Ke(5,6) = t646
      Ke(5,7) = t656
      Ke(5,8) = t662
      Ke(5,9) = t672
      Ke(5,10) = t680
      Ke(5,11) = t685
      Ke(5,12) = t692
      Ke(5,13) = t699
      Ke(5,14) = t704
      Ke(5,15) = t711
      Ke(6,1) = t177
      Ke(6,2) = t313
      Ke(6,3) = t428
      Ke(6,4) = t543
      Ke(6,5) = t646
      Ke(6,6) = 0.25_r8k*t730*t146-0.125_r8k*t160*t145*t717
      Ke(6,7) = t746
      Ke(6,8) = t756
      Ke(6,9) = t762
      Ke(6,10) = t770
      Ke(6,11) = t777
      Ke(6,12) = t782
      Ke(6,13) = t789
      Ke(6,14) = t796
      Ke(6,15) = t801
      Ke(7,1) = t185
      Ke(7,2) = t323
      Ke(7,3) = t438
      Ke(7,4) = t549
      Ke(7,5) = t656
      Ke(7,6) = t746
      Ke(7,7) = 0.25_r8k*t826*t179-0.125_r8k*t160*t829
      Ke(7,8) = t842
      Ke(7,9) = t852
      Ke(7,10) = t858
      Ke(7,11) = t865
      Ke(7,12) = t872
      Ke(7,13) = t877
      Ke(7,14) = t884
      Ke(7,15) = t891
      Ke(8,1) = t197
      Ke(8,2) = t329
      Ke(8,3) = t448
      Ke(8,4) = t559
      Ke(8,5) = t662
      Ke(8,6) = t756
      Ke(8,7) = t842
      Ke(8,8) = 0.25_r8k*t913*t187-0.125_r8k*t160*t916
      Ke(8,9) = t929
      Ke(8,10) = t940
      Ke(8,11) = t945
      Ke(8,12) = t952
      Ke(8,13) = t959
      Ke(8,14) = t964
      Ke(8,15) = t971
      Ke(9,1) = t209
      Ke(9,2) = t339
      Ke(9,3) = t454
      Ke(9,4) = t569
      Ke(9,5) = t672
      Ke(9,6) = t762
      Ke(9,7) = t852
      Ke(9,8) = t929
      Ke(9,9) = 0.25_r8k*t989*t60*t199-0.125_r8k*t160*t993
      Ke(9,10) = t1007
      Ke(9,11) = t1017
      Ke(9,12) = t1022
      Ke(9,13) = t1029
      Ke(9,14) = t1036
      Ke(9,15) = t1041
      Ke(10,1) = t214
      Ke(10,2) = t346
      Ke(10,3) = t461
      Ke(10,4) = t575
      Ke(10,5) = t680
      Ke(10,6) = t770
      Ke(10,7) = t858
      Ke(10,8) = t940
      Ke(10,9) = t1007
      Ke(10,10) = 0.25_r8k*t1066*t179-0.125_r8k*t128*t178*t1053
      Ke(10,11) = t1082
      Ke(10,12) = t1092
      Ke(10,13) = t1097
      Ke(10,14) = t1104
      Ke(10,15) = t1111
      Ke(11,1) = t221
      Ke(11,2) = t351
      Ke(11,3) = t468
      Ke(11,4) = t582
      Ke(11,5) = t685
      Ke(11,6) = t777
      Ke(11,7) = t865
      Ke(11,8) = t945
      Ke(11,9) = t1017
      Ke(11,10) = t1082
      Ke(11,11) = 0.25_r8k*t1133*t187-0.125_r8k*t128*t186*t1120
      Ke(11,12) = t1149
      Ke(11,13) = t1156
      Ke(11,14) = t1161
      Ke(11,15) = t1168
      Ke(12,1) = t228
      Ke(12,2) = t358
      Ke(12,3) = t473
      Ke(12,4) = t589
      Ke(12,5) = t692
      Ke(12,6) = t782
      Ke(12,7) = t872
      Ke(12,8) = t952
      Ke(12,9) = t1022
      Ke(12,10) = t1092
      Ke(12,11) = t1149
      Ke(12,12) = 0.25_r8k*t1186*t1*t199-0.125_r8k*t128*t198*t1174
      Ke(12,13) = t1200
      Ke(12,14) = t1207
      Ke(12,15) = t1212
      Ke(13,1) = t234
      Ke(13,2) = t365
      Ke(13,3) = t480
      Ke(13,4) = t594
      Ke(13,5) = t699
      Ke(13,6) = t789
      Ke(13,7) = t877
      Ke(13,8) = t959
      Ke(13,9) = t1029
      Ke(13,10) = t1097
      Ke(13,11) = t1156
      Ke(13,12) = t1200
      Ke(13,13) = -t33*t1213*pfact
      Ke(13,14) = -t1218
      Ke(13,15) = -t1220
      Ke(14,1) = t243
      Ke(14,2) = t370
      Ke(14,3) = t487
      Ke(14,4) = t601
      Ke(14,5) = t704
      Ke(14,6) = t796
      Ke(14,7) = t884
      Ke(14,8) = t964
      Ke(14,9) = t1036
      Ke(14,10) = t1104
      Ke(14,11) = t1161
      Ke(14,12) = t1207
      Ke(14,13) = -t1218
      Ke(14,14) = -t47*t1213*pfact
      Ke(14,15) = -t1224
      Ke(15,1) = t252
      Ke(15,2) = t377
      Ke(15,3) = t492
      Ke(15,4) = t608
      Ke(15,5) = t711
      Ke(15,6) = t801
      Ke(15,7) = t891
      Ke(15,8) = t971
      Ke(15,9) = t1041
      Ke(15,10) = t1111
      Ke(15,11) = t1168
      Ke(15,12) = t1212
      Ke(15,13) = -t1220
      Ke(15,14) = -t1224
      Ke(15,15) = -t51*t1213*pfact

    RETURN

  END SUBROUTINE cont3d_stIff_normal


    SUBROUTINE cont3d_update()
    ! 3D contact stIffness matrix
    IMPLICIT NONE

    RETURN

  END SUBROUTINE cont3d_update


    SUBROUTINE cont3d_Write_output(nunit)
    ! WRITE output to a file in unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
!!$    TYPE(cont3dsurf_type), POINTER :: currs

    ! Output contact surfaces
    WRITE(UNIT=nunit) ncont3dsurf
!!$    currs => first_cont3dsurf
!!$    DO WHILE ( ASSOCIATED(currs) )
!!$       WRITE(UNIT=nunit) currs%label
!!$       WRITE(UNIT=nunit) currs%mat
!!$       WRITE(UNIT=nunit) currs%ntnodes(1:2)
!!$       WRITE(UNIT=nunit) currs%tnodes(1:currs%ntnodes(1),1:currs%ntnodes(2))
!!$       WRITE(UNIT=nunit) currs%ncnodes
!!$       WRITE(UNIT=nunit) currs%cnodes(1:currs%ncnodes)
!!$       currs => currs%next
!!$    ENDDO

    ! Output contact elements
    WRITE(UNIT=nunit) ncont3d

    RETURN

  END SUBROUTINE cont3d_Write_output


    SUBROUTINE cont3d_read_output(nunit)
    ! Read output from unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
!!$    TYPE(cont3dsurf_type), POINTER :: currs,prevs
!!$    INTEGER(ipk) :: i

    ! Read 3D contact surfaces
    Read(UNIT=nunit,ERR=10,End=10) ncont3dsurf

!!$    currs => first_cont3dsurf
!!$    DO i=1,ncont3dsurf
!!$       IF ( .NOT.ASSOCIATED(currs) ) THEN
!!$          ALLOCATE(currs)
!!$          currs%next => NULL()
!!$          currs%tnodes => NULL()
!!$          currs%cnodes => NULL()
!!$          IF ( .NOT.ASSOCIATED(first_cont3dsurf) ) THEN
!!$             first_cont3dsurf => currs
!!$             last_cont3dsurf => currs
!!$          ELSE
!!$             last_cont3dsurf%next => currs
!!$             last_cont3dsurf => currs
!!$          ENDIF
!!$       ENDIF
!!$       IF ( ASSOCIATED(currs%tnodes) ) DEALLOCATE(currs%tnodes)
!!$       IF ( ASSOCIATED(currs%cnodes) ) DEALLOCATE(currs%cnodes)
!!$       Read(UNIT=nunit,ERR=10,End=10) currs%label
!!$       Read(UNIT=nunit,ERR=10,End=10) currs%mat
!!$       Read(UNIT=nunit,ERR=10,End=10) currs%ntnodes(1:2)
!!$       ALLOCATE(currs%tnodes(currs%ntnodes(1),currs%ntnodes(2)))
!!$       Read(UNIT=nunit,ERR=10,End=10) &
!!$            currs%tnodes(1:currs%ntnodes(1),1:currs%ntnodes(2))
!!$       Read(UNIT=nunit,ERR=10,End=10) currs%ncnodes
!!$       ALLOCATE(currs%cnodes(currs%ncnodes))
!!$       Read(UNIT=nunit,ERR=10,End=10) currs%cnodes(1:currs%ncnodes)
!!$       last_cont3dsurf => currs
!!$       currs => currs%next
!!$    ENDDO
!!$
!!$    ! Remove excess surface entries from the Database
!!$    DO WHILE ( ASSOCIATED(currs) )
!!$       prevs => currs
!!$       currs => currs%next
!!$       DEALLOCATE(prevs)
!!$    ENDDO
!!$    IF ( ASSOCIATED(last_cont3dsurf) ) last_cont3dsurf%next => NULL()
!!$    IF ( ncont3dsurf == 0 ) THEN
!!$       first_cont3dsurf => NULL()
!!$       last_cont3dsurf => NULL()
!!$    ENDIF

    ! Read 3D contact elements
    Read(UNIT=nunit,ERR=10,End=10) ncont3d

    RETURN

10  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading CONT3D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE cont3d_read_output


    SUBROUTINE cont3d_deallocate()
    ! Deallocate CONT3D Database
    IMPLICIT NONE
    TYPE(cont3d_type), POINTER :: curre,preve
    TYPE(cont3dsurf_type), POINTER :: currs,prevs

    ! Deallocate 3D contact elements
    curre => first_cont3d
    DO WHILE ( ASSOCIATED(curre))
       preve => curre
       curre => curre%next
       DEALLOCATE(preve)
    ENDDO

    ! Deallocate 3D contact surfaces
    currs => first_cont3dsurf
    DO WHILE ( ASSOCIATED(currs) )
       prevs => currs
       currs => currs%next
       DEALLOCATE(prevs%cnodes)
       DEALLOCATE(prevs)
    ENDDO

    ncont3d = 0
    ncont3dsurf = 0

    RETURN

  END SUBROUTINE cont3d_deallocate
END MODULE cont3d













