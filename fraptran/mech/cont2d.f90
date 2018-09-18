MODULE cont2d_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    USE materials_frap_fraptran
    USE sparse_matrix_fraptran
    IMPLICIT NONE
    !>@brief
    !> 2D contact element
    !> First two nodes define the target surface and the third node is the contactor node
    !
    !       o 2
    !       |
    !    xi ^ gap
    !       |<--->o 3
    !       +
    !       |
    !       |
    !       |
    !       o 1
    !

    INTEGER(ipk) :: ncont2d,ncont2dsurf
    REAL(r8k), PARAMETER :: cont2d_alpha_tol = 0.01_r8k

    TYPE cont2dsurf_type
        INTEGER(ipk) :: label ! Surface label
        INTEGER(ipk) :: mat ! Surface material label
        INTEGER(ipk) :: ntarget ! Number of target nodes
        INTEGER(ipk), DIMENSION(:), POINTER :: target_nodes ! Target nodes
        INTEGER(ipk) :: ncontactor ! Number of contactor nodes
        INTEGER(ipk), DIMENSION(:), POINTER :: contactor_nodes ! Contactor nodes
        TYPE(cont2dsurf_type), POINTER :: next
    END TYPE cont2dsurf_type


    TYPE cont2d_type
        INTEGER(ipk) :: label ! Element label
        INTEGER(ipk) :: surf ! Contact surface label
        INTEGER(ipk) :: mat ! Element material label
        LOGICAL :: closed ! Flag for gap status
                        !   True = contact
                        !   False = Open gap
        LOGICAL :: closed0 ! Explicit value for closed
        LOGICAL :: stick ! Flag for tangential sticking status
                        !   True = fixed contact
                        !   False = slipping
        LOGICAL :: stick0 ! Explicit value for stick
        INTEGER(ipk) :: node_labels(3) ! Element node labels
        INTEGER(ipk) :: node_numbers(3) ! Internal node numbering
        REAL(r8k) :: gap ! Gap function or shortest distance from
                            ! contactor node to the target surface
        REAL(r8k) :: gap0 ! Gap function or shortest distance from
                            ! contactor node to the target surface
        REAL(r8k) :: alpha ! Element coordinate value at target surface at
                                ! contact/shortest distance to contactor node
        REAL(r8k) :: alpha0 ! Explicit value for alpha
        REAL(r8k) :: dalpha ! Delta alpha
        REAL(r8k) :: dalpha0 ! Explicit value for alpha
        REAL(r8k) :: sinb
        REAL(r8k) :: cosb
        REAL(r8k) :: l0 ! Initial length of target surface
        REAL(r8k) :: pfact ! Penalty factor
        TYPE(cont2d_type), POINTER :: next
    END TYPE cont2d_type


    REAL(r8k) :: cont2d_search
    TYPE(cont2d_type), POINTER :: &
        first_cont2d,last_cont2d ! POINTERs to the contact element Database
    TYPE(cont2dsurf_type), POINTER :: &
        first_cont2dsurf,last_cont2dsurf ! POINTERs to 2D contact surfaces

    CONTAINS
    SUBROUTINE cont2d_init()
    ! Initialize cont2d Database
    IMPLICIT NONE

    ! Deallocate, Iff Databese already exists
    CALL cont2d_deallocate()

    ! Initialize POINTERs and counters
    ncont2d = 0
    ncont2dsurf = 0
    first_cont2d => NULL()
    last_cont2d => NULL()
    first_cont2dsurf => NULL()
    last_cont2dsurf => NULL()
    cont2d_search = 1.0_r8k

    RETURN

  END SUBROUTINE cont2d_init


    SUBROUTINE cont2d_create_surf(label,mat,numt,numc,tnodes,cnodes)
    ! Create 2D contact surface
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         label, & ! Surface label
         mat, & ! Material label for the surface
         numt, & ! Number of nodes at target surface
         numc, & ! Number of contactor nodes
         tnodes(numt), & ! Target node numbers
         cnodes(numc) ! Contactor node numbers
    TYPE(cont2dsurf_type), POINTER :: current

    ! Check that the surface label does not exist already
    current => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) THEN
          WRITE(UNIT=6,FMT='(A,I0)') &
               'ERROR two definitions for contact surface ',label
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ENDIF
       current => current%next
    ENDDO

    ! Check that there are enough target nodes
    IF ( numt < 2 ) THEN
       WRITE(UNIT=6,FMT='(A,I0)') &
            'ERROR at least two target nodes needed for contact surface ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Create new surface definition
    ncont2dsurf = ncont2dsurf + 1
    ALLOCATE(current)
    current%label = label
    current%mat = mat
    current%ntarget = numt
    ALLOCATE(current%target_nodes(numt))
    current%target_nodes = tnodes
    current%ncontactor = numc
    ALLOCATE(current%contactor_nodes(numc))
    current%contactor_nodes = cnodes
    current%next => NULL()
    CALL mat_create(mat)

    ! Attach contact surface definition to the Database
    IF ( .NOT.ASSOCIATED(first_cont2dsurf) ) THEN
       first_cont2dsurf => current
       last_cont2dsurf => current
    ELSE
       last_cont2dsurf%next => current
       last_cont2dsurf => current
    ENDIF

    RETURN

  END SUBROUTINE cont2d_create_surf


    SUBROUTINE cont2d_delete_surf(label)
    ! Delete contact surface
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(cont2dsurf_type), POINTER :: current_surf,previous_surf,tobedeleted
    TYPE(cont2d_type), POINTER :: current,previous,tbd
    LOGICAL :: lfound

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_cont2dsurf) ) RETURN

    ! Mark first memeber to be deleted in Database
    lfound = .FALSE.
    IF ( first_cont2dsurf%label == label ) THEN
       tobedeleted => first_cont2dsurf
       first_cont2dsurf => first_cont2dsurf%next
       lfound = .TRUE.
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current_surf => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       IF ( current_surf%label == label ) THEN
          previous_surf%next => current_surf%next
          tobedeleted => current_surf
          lfound = .TRUE.
       ENDIF
       previous_surf => current_surf
       current_surf => current_surf%next
    ENDDO

    ! Set the POINTER to the last CONT2D element
    last_cont2dsurf => NULL()
    current_surf => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       last_cont2dsurf => current_surf
       current_surf => current_surf%next
    ENDDO
    
    ! Deallocate 2D contact surface
    IF ( lfound ) THEN
       ncont2dsurf = ncont2dsurf - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted%target_nodes,tobedeleted%contactor_nodes)
       DEALLOCATE(tobedeleted)
    ENDIF

    ! Delete contact elements associated to the surface
    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%surf == label ) THEN
          IF ( current%label == first_cont2d%label ) THEN
             tbd => current
             current => current%next
             first_cont2d => current
             IF ( ASSOCIATED(first_cont2d) ) first_cont2d%label = 1
             ncont2d = ncont2d - 1
             DEALLOCATE(tbd)
          ELSE
             previous%next => current%next
             tbd => current
             current => current%next
             IF ( ASSOCIATED(current) ) current%label = tbd%label
             ncont2d = ncont2d - 1
             DEALLOCATE(tbd)
          ENDIF
       ELSE
          previous => current
          current => current%next
       ENDIF
    ENDDO

    ! Set POINTER to the last contact element
    last_cont2d => NULL()
    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       last_cont2d => current
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_delete_surf


    SUBROUTINE cont2d_activate()
    ! Activate elements on 2D contact surfaces
    IMPLICIT NONE
    TYPE(cont2dsurf_type), POINTER :: current_surf
    TYPE(cont2d_type), POINTER :: current
    INTEGER(ipk) :: i,j,label,nodes(3)
    REAL(r8k) :: x1(3),x2(3),x3(3),xm(2),length,dist
    LOGICAL :: lactive

    current_surf => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       DO i=1,current_surf%ntarget - 1
          nodes(1) = current_surf%target_nodes(i)
          nodes(2) = current_surf%target_nodes(i + 1)
          x1 = node_coords(nodes(1),1)
          x2 = node_coords(nodes(2),1)
          xm(1:2) = 0.5_r8k*(x1(1:2) + x2(1:2))
          length = SQRT((x2(1) - x1(1))**2 + (x2(2) - x1(2))**2)
          DO j=1,current_surf%ncontactor
             nodes(3) = current_surf%contactor_nodes(j)
             x3 = node_coords(nodes(3),1)
             dist = SQRT((x3(1) - xm(1))**2 + (x3(2) - xm(2))**2)
             lactive = ( dist < length*cont2d_search )
             IF (( nodes(3) == nodes(1) ).OR.( nodes(3) == nodes(2) )) &
                  lactive = .FALSE.
             IF ( lactive ) THEN
                ! Create new contact element If contactor node is close enough
                ! to the target surface
                label = ncont2d + 1
                CALL cont2d_create(label,current_surf%mat,current_surf%label, &
                     nodes)
             ELSE
                ! Delete contact element If contactor node is far from the
                ! target surface
                current => first_cont2d
                DO WHILE ( ASSOCIATED(current) )
                   IF ( ( current%node_labels(1) == nodes(1) ).AND. &
                        ( current%node_labels(2) == nodes(2) ).AND. &
                        ( current%node_labels(3) == nodes(3) ) ) THEN
                      label = current%label
                      CALL cont2d_delete(label)
                      EXIT
                   ENDIF
                   current => current%next
                ENDDO
             ENDIF
          ENDDO
       ENDDO
       current_surf => current_surf%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_activate


    SUBROUTINE cont2d_create(label,mat,surf,nodes)
    ! Create new CONT2D element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         label, & ! Element label
         surf, & ! Contact surface label
         mat, & ! Element material label
         nodes(3) ! Element nodes
    INTEGER(ipk) :: nfound,i,j
    REAL(r8k) :: x12(2),len,x13(2)
    TYPE(cont2d_type), POINTER :: new_cont2d,current

    ! If element with given nodes already exists RETURN and Do nothing
    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( ( current%node_labels(1) == nodes(1) ).AND. &
            ( current%node_labels(2) == nodes(2) ).AND. &
            ( current%node_labels(3) == nodes(3) ) ) RETURN
       current => current%next
    ENDDO

    ! Allocate new element
    ncont2d = ncont2d + 1
    ALLOCATE(new_cont2d)

    ! Find element nodes in node Database
    nfound = 0
    DO i=1,nnodes
       DO j=1,3
          IF ( node_labels(i) == nodes(j) ) THEN
             new_cont2d%node_labels(j) = nodes(j)
             new_cont2d%node_numbers(j) = i
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 3 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 3 ) THEN
       WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR incompatible NODES and CONT2D Data'
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Initialize CONT2D variables
    new_cont2d%label = label
    new_cont2d%surf = surf
    new_cont2d%mat = mat
    new_cont2d%closed0 = .FALSE.
    new_cont2d%stick0 = .FALSE.
    new_cont2d%next => NULL()

    ! Initial values for contact parameters
    x12(1:2) = & 
         x(1:2,new_cont2d%node_numbers(2)) - x(1:2,new_cont2d%node_numbers(1))
    x13(1:2) = &
         x(1:2,new_cont2d%node_numbers(3)) - x(1:2,new_cont2d%node_numbers(1))
    len = SQRT( x12(1)**2 + x12(2)**2 )
    new_cont2d%l0 = len
    new_cont2d%cosb = x12(1)/len
    new_cont2d%sinb = x12(2)/len

    ! gap function
    new_cont2d%gap0 = x13(1)*new_cont2d%sinb - x13(2)*new_cont2d%cosb

    ! element coordinate value
    new_cont2d%alpha0 = (x13(1)*new_cont2d%cosb + x13(2)*new_cont2d%sinb)/len
    new_cont2d%dalpha0 = 0.0_r8k

    ! closing of the contact
    IF ( ( new_cont2d%alpha0 > -cont2d_alpha_tol ).AND. &
         ( new_cont2d%alpha0 <  1.0_r8k + cont2d_alpha_tol ).AND. &
         ( new_cont2d%gap0 <= 0.0_r8k ) ) THEN
       new_cont2d%closed0 = .TRUE.
    ENDIF

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_cont2d) ) THEN
       first_cont2d => new_cont2d
       last_cont2d => new_cont2d
    ELSE
       last_cont2d%next => new_cont2d
       last_cont2d => new_cont2d
    ENDIF

    RETURN

  END SUBROUTINE cont2d_create


    SUBROUTINE cont2d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(cont2d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_cont2d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_cont2d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_cont2d
       first_cont2d => first_cont2d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_cont2d
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

    ! Set the POINTER to the last CONT2D element
    last_cont2d => NULL()
    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       last_cont2d => current
       current => current%next
    ENDDO
    
    ! Deallocate CONT2D element
    IF ( lfound ) THEN
       ncont2d = ncont2d - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE cont2d_delete


    SUBROUTINE cont2d_sparse_matrix()
    ! Initialize sparse matrix storage for cont2d elements
    IMPLICIT NONE
    TYPE(cont2d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(6)

    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
       dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
       dofnum(5:6) = dof_number(1:2,current%node_numbers(3))
       CALL sparse_matrix_add_element(6,dofnum)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_sparse_matrix


    SUBROUTINE cont2d_fint()
    ! Calculate contact penalty forces
    IMPLICIT NONE
    TYPE(cont2d_type), POINTER :: current
    REAL(r8k) :: x12(2),x13(2),len,frcoef,nv(2),tv(2),Fn,Ft
    REAL(r8k), POINTER :: sinb,cosb,gap,alpha,dalpha,alpha0,dalpha0,pfact

    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       ! Assign POINTERs
       sinb => current%sinb
       cosb => current%cosb
       gap => current%gap
       alpha => current%alpha
       alpha0 => current%alpha0
       dalpha => current%dalpha
       dalpha0 => current%dalpha0
       current%closed = current%closed0
       current%stick = current%stick0
       pfact => current%pfact

       x12(1:2) = x(1:2,current%node_numbers(2))-x(1:2,current%node_numbers(1))
       x13(1:2) = x(1:2,current%node_numbers(3))-x(1:2,current%node_numbers(1))

       len = SQRT( x12(1)**2 + x12(2)**2 )
       cosb = x12(1)/len
       sinb = x12(2)/len

       ! gap function
       gap = x13(1)*sinb - x13(2)*cosb

       ! element coordinate value
       alpha = (x13(1)*cosb + x13(2)*sinb)/len
       dalpha = alpha - alpha0

       ! Penalty factor
       pfact = mat_par(current%mat,tref,'PENALTY ')

       ! closing of the contact
       current%closed = .FALSE.
       current%stick = .FALSE.
       IF ( ( alpha > -cont2d_alpha_tol ).AND. &
            ( alpha <  1.0_r8k + cont2d_alpha_tol ).AND. &
            ( gap <= 0.0_r8k ) ) THEN

          ! Contact flag
          current%closed = .TRUE.

          ! Normal forces
          Fn = pfact*gap
          nv = (/ sinb, -cosb /)

          ! Normal penalty forces
          !Fint(1:2,current%node_numbers(1)) = &
          !     Fint(1:2,current%node_numbers(1)) - Fn*(1.0_r8k-alpha0)*nv(1:2)
          !Fint(1:2,current%node_numbers(2)) = &
          !     Fint(1:2,current%node_numbers(2)) - Fn*alpha0*nv(1:2)
          !Fint(1:2,current%node_numbers(3)) = &
          !     Fint(1:2,current%node_numbers(3)) + Fn*nv(1:2)

          Fext(1:2,current%node_numbers(1)) = &
               Fext(1:2,current%node_numbers(1)) + Fn*(1.0_r8k-alpha0)*nv(1:2)
          Fext(1:2,current%node_numbers(2)) = &
               Fext(1:2,current%node_numbers(2)) + Fn*alpha0*nv(1:2)
          Fext(1:2,current%node_numbers(3)) = &
               Fext(1:2,current%node_numbers(3)) - Fn*nv(1:2)

          IF ( current%closed0 ) THEN

             ! Sticking flag
             current%stick = .TRUE.

             ! Friction coefficient
             frcoef = mat_par(current%mat,tref,'FRCOEF  ')

             ! tangential penalty
             Ft = dalpha*pfact*current%l0

             ! Friction force
             IF ( ABS(Ft) > frcoef*ABS(Fn) ) THEN
                current%stick = .FALSE.
                Ft = SIGN(frcoef*Fn,dalpha)
             ENDIF

             ! Tangent unit vector
             tv = (/ cosb, sinb /)

             ! Tangential penalty forces
             !Fint(1:2,current%node_numbers(1)) = &
             !     Fint(1:2,current%node_numbers(1))-Ft*(1.0_r8k-alpha0)*tv(1:2)
             !Fint(1:2,current%node_numbers(2)) = &
             !     Fint(1:2,current%node_numbers(2)) - Ft*alpha0*tv(1:2)
             !Fint(1:2,current%node_numbers(3)) = &
             !     Fint(1:2,current%node_numbers(3)) + Ft*tv(1:2)

             Fext(1:2,current%node_numbers(1)) = &
                  Fext(1:2,current%node_numbers(1))+Ft*(1.0_r8k-alpha0)*tv(1:2)
             Fext(1:2,current%node_numbers(2)) = &
                  Fext(1:2,current%node_numbers(2)) + Ft*alpha0*tv(1:2)
             Fext(1:2,current%node_numbers(3)) = &
                  Fext(1:2,current%node_numbers(3)) - Ft*tv(1:2)
          ENDIF
       ENDIF

       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_fint


    SUBROUTINE cont2d_stIff()
    ! Stiffness matrix due to 2D contact forces
    IMPLICIT NONE
    TYPE(cont2d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(6)
    REAL(r8k) :: x1(2),x2(2),x3(2),Ke(6,6),frcoef,dir

    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%closed ) THEN
          x1(1:2) = x(1:2,current%node_numbers(1))
          x2(1:2) = x(1:2,current%node_numbers(2))
          x3(1:2) = x(1:2,current%node_numbers(3))

          ! DOF numbering
          dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
          dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
          dofnum(5:6) = dof_number(1:2,current%node_numbers(3))

          ! Normal stIffness part
          CALL cont2d_stIff_n(x1,x2,x3,current%pfact,current%alpha0,Ke)

          ! Place element matrix in the global matrix
          CALL sparse_matrix_place_element(6,dofnum,Ke)

          ! Tangential stIffness part
          IF ( current%closed0 ) THEN
             IF ( current%stick ) THEN
                CALL cont2d_stIff_t(x1,x2,x3,current%pfact,current%alpha0, &
                     current%l0,Ke)
             ELSE
                symmetric_matrix = .FALSE.
                frcoef = mat_par(current%mat,tref,'FRCOEF  ')
                dir = SIGN(1.0_r8k,current%dalpha)
                CALL cont2d_stIff_fr(x1,x2,x3,current%pfact,frcoef,dir, &
                     current%alpha0,Ke)
             ENDIF

             ! Place element matrix in the global matrix
             CALL sparse_matrix_place_element(6,dofnum,Ke)

          ENDIF

       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_stIff


    SUBROUTINE cont2d_stIff_n(x1,x2,x3,pfact,alpha0,Ke)
    ! CONT2D element matrix for normal penalty part
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: pfact, alpha0
    REAL(r8k) ::  t1,  t2,  t5,  t6,  t9, t10, t11, t12, t13, t14, t15, t16, t18, t19, t20, t23, t24, t25, t29, t30, &
      &          t36, t37, t38, t39, t43, t44, t48, t51, t52, t54, t55, t58, t59, t62, t65, t67, t71, t73, t74, t78, &
      &          t79, t80, t86, t93, t96, t100, t105, t110, t111, t117, t119, t126, t131, t136, t137, t143, t153, &
      &          t158, t159, t161, t165, t166

    REAL(r8k), DIMENSION(2), INTENT(IN) :: x1, x2, x3
    REAL(r8k), DIMENSION(6,6), INTENT(OUT) :: Ke

    t1 = x2(2)-x1(2)
    t2 = x2(1)**2
    t5 = x1(1)**2
    t6 = x2(2)**2
    t9 = x1(2)**2
    t10 = t2-2*x2(1)*x1(1)+t5+t6-2*x2(2)*x1(2)+t9
    t11 = SQRT(t10)
    t12 = 1/t11
    t13 = t1*t12
    t14 = x3(1)-x1(1)
    t15 = t14*t1
    t16 = t11**2
    t18 = 1/t16/t11
    t19 = -x2(1)+x1(1)
    t20 = 2*t18*t19
    t23 = x3(2)-x1(2)
    t24 = t23*t12
    t25 = -t23*t19
    t29 = pfact*(-t13-t15*t20/2+t24+t25*t20/2)
    t30 = 0.1e1_r8k-alpha0
    t36 = pfact*(t15*t12-t25*t12)
    t37 = t36*t1
    t38 = t18*t30
    t39 = 2*t38*t19
    t43 = -t19*t12
    t44 = t43*t30
    t48 = -t36*t19
    t51 = t29*t44-t36*t12*t30-t48*t39/2
    t52 = t13*alpha0
    t54 = t18*alpha0
    t55 = 2*t54*t19
    t58 = -t29*t52+t37*t55/2
    t59 = t43*alpha0
    t62 = t36*t12*alpha0
    t65 = t29*t59-t62-t48*t55/2
    t67 = t1*t18
    t71 = t29*t13-t36*t67*t19
    t73 = t36*t12
    t74 = -t18*t19
    t78 = -t29*t43+t73+t36*t74*t19
    t79 = t14*t12
    t80 = -2*t1*t18
    t86 = pfact*(-t79-t15*t80/2+t43+t25*t80/2)
    t93 = -2*t54*t1
    t96 = -t86*t52+t62+t37*t93/2
    t100 = t86*t59-t48*t93/2
    t105 = t86*t13-t73+t36*t67*t1
    t110 = -t86*t43-t36*t74*t1
    t111 = -2*t18*t19
    t117 = pfact*(-t15*t111/2-t24+t25*t111/2)
    t119 = -2*t54*t19
    t126 = t117*t59+t62-t48*t119/2
    t131 = t117*t13+t36*t67*t19
    t136 = -t117*t43-t73-t36*t74*t19
    t137 = 2*t1*t18
    t143 = pfact*(t79-t15*t137/2+t25*t137/2)
    t153 = t143*t13+t73-t36*t67*t1
    t158 = -t143*t43+t36*t74*t1
    t159 = t1**2
    t161 = 1/t10
    t165 = -pfact*t19*t161*t1
    t166 = t19**2
    Ke(1,1) = -t29*t13*t30+t37*t39/2
    Ke(1,2) = t51
    Ke(1,3) = t58
    Ke(1,4) = t65
    Ke(1,5) = t71
    Ke(1,6) = t78
    Ke(2,1) = t51
    Ke(2,2) = t86*t44+t48*t38*t1
    Ke(2,3) = t96
    Ke(2,4) = t100
    Ke(2,5) = t105
    Ke(2,6) = t110
    Ke(3,1) = t58
    Ke(3,2) = t96
    Ke(3,3) = -t117*t52+t37*t119/2
    Ke(3,4) = t126
    Ke(3,5) = t131
    Ke(3,6) = t136
    Ke(4,1) = t65
    Ke(4,2) = t100
    Ke(4,3) = t126
    Ke(4,4) = t143*t59-t48*t54*t1
    Ke(4,5) = t153
    Ke(4,6) = t158
    Ke(5,1) = t71
    Ke(5,2) = t105
    Ke(5,3) = t131
    Ke(5,4) = t153
    Ke(5,5) = pfact*t159*t161
    Ke(5,6) = -t165
    Ke(6,1) = t78
    Ke(6,2) = t110
    Ke(6,3) = t136
    Ke(6,4) = t158
    Ke(6,5) = -t165
    Ke(6,6) = pfact*t166*t161

    END SUBROUTINE cont2d_stIff_n


    SUBROUTINE cont2d_stIff_t(x1,x2,x3,pfact,alpha0,l0,Ke)
    !>@brief
    !> CONT2D element matrix when there is tangential slipping
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: pfact, alpha0, l0
    REAL(r8k) ::  t1,  t2,  t5,  t6,  t9, t11, t12, t13, t14, t15, t16, t17, t19, t20, t23, t24, t25, t32, t33, t37, &
      &          t38, t39, t44, t45, t47, t48, t49, t50, t55, t56, t58, t62, t63, t66, t67, t71, t72, t77, t79, t81, &
      &          t85, t87, t92, t93, t96, t104, t105, t113, t117, t122, t127, t132, t133, t143, t144, t146, t155, &
      &          t160, t165, t166, t176, t188, t193, t194, t196, t200, t201
    REAL(r8k), DIMENSION(2), INTENT(IN) :: x1, x2, x3
    REAL(r8k), DIMENSION(6,6), INTENT(OUT) :: Ke

    t1 = x2(1)-x1(1)
    t2 = x2(1)**2
    t5 = x1(1)**2
    t6 = x2(2)**2
    t9 = x1(2)**2
    t11 = SQRT(t2-2*x2(1)*x1(1)+t5+t6-2*x2(2)*x1(2)+t9)
    t12 = 1/t11
    t13 = t1*t12
    t14 = x3(1)-x1(1)
    t15 = t14*t12
    t16 = t14*t1
    t17 = t11**2
    t19 = 1/t17/t11
    t20 = -2*t19*t1
    t23 = x3(2)-x1(2)
    t24 = x2(2)-x1(2)
    t25 = t23*t24
    t32 = t16*t12+t25*t12
    t33 = t32*t19
    t37 = pfact*((-t13-t15-t16*t20/2-t25*t20/2)*t12+t33*t1)
    t38 = t37*l0
    t39 = 0.1e1_r8k-alpha0
    t44 = pfact*(t32*t12-alpha0)
    t45 = l0*t12
    t47 = t44*t45*t39
    t48 = t44*l0
    t49 = t19*t1
    t50 = -2*t39*t1
    t55 = t24*t12
    t56 = t55*t39
    t58 = t24*t19
    t62 = -t38*t56+t48*t58*t50/2
    t63 = t13*alpha0
    t66 = t44*t45*alpha0
    t67 = -2*alpha0*t1
    t71 = -t38*t63+t66+t48*t49*t67/2
    t72 = t55*alpha0
    t77 = -t38*t72+t48*t58*t67/2
    t79 = l0*t1*t12
    t81 = t44*t45
    t85 = t37*t79-t81+t48*t49*t1
    t87 = l0*t24*t12
    t92 = t37*t87+t48*t58*t1
    t93 = -2*t24*t19
    t96 = t23*t12
    t104 = pfact*((-t16*t93/2-t55-t96-t25*t93/2)*t12+t33*t24)
    t105 = t104*l0
    t113 = -2*alpha0*t24
    t117 = -t105*t63+t48*t49*t113/2
    t122 = -t105*t72+t66+t48*t58*t113/2
    t127 = t104*t79+t48*t49*t24
    t132 = t104*t87-t81+t48*t58*t24
    t133 = 2*t19*t1
    t143 = pfact*((t15-t16*t133/2-t25*t133/2)*t12-t33*t1)
    t144 = t143*l0
    t146 = 2*alpha0*t1
    t155 = -t144*t72+t48*t58*t146/2
    t160 = t143*t79+t81-t48*t49*t1
    t165 = t143*t87-t48*t58*t1
    t166 = 2*t24*t19
    t176 = pfact*((-t16*t166/2+t96-t25*t166/2)*t12-t33*t24)
    t188 = t176*t79-t48*t49*t24
    t193 = t176*t87+t81-t48*t58*t24
    t194 = t1**2
    t196 = t19*l0
    t200 = pfact*t24*t196*t1
    t201 = t24**2
    Ke(1,1) = -t38*t13*t39+t47+t48*t49*t50/2
    Ke(1,2) = t62
    Ke(1,3) = t71
    Ke(1,4) = t77
    Ke(1,5) = t85
    Ke(1,6) = t92
    Ke(2,1) = t62
    Ke(2,2) = -t105*t56+t47-t48*t58*t39*t24
    Ke(2,3) = t117
    Ke(2,4) = t122
    Ke(2,5) = t127
    Ke(2,6) = t132
    Ke(3,1) = t71
    Ke(3,2) = t117
    Ke(3,3) = -t144*t63-t66+t48*t49*t146/2
    Ke(3,4) = t155
    Ke(3,5) = t160
    Ke(3,6) = t165
    Ke(4,1) = t77
    Ke(4,2) = t122
    Ke(4,3) = t155
    Ke(4,4) = -t176*l0*t72-t66+t48*t58*alpha0*t24
    Ke(4,5) = t188
    Ke(4,6) = t193
    Ke(5,1) = t85
    Ke(5,2) = t127
    Ke(5,3) = t160
    Ke(5,4) = t188
    Ke(5,5) = pfact*t194*t196
    Ke(5,6) = t200
    Ke(6,1) = t92
    Ke(6,2) = t132
    Ke(6,3) = t165
    Ke(6,4) = t193
    Ke(6,5) = t200
    Ke(6,6) = pfact*t201*t196

    END SUBROUTINE cont2d_stIff_t


    SUBROUTINE cont2d_stIff_fr(x1,x2,x3,pfact,frcoef,dir,alpha0,Ke)
    IMPLICIT NONE
    !>@brief
    !> CONT2D element matrix when there is tangential slipping
    
    REAL(r8k), INTENT(IN) ::pfact, frcoef, dir, alpha0
    REAL(r8k) ::  t1,  t2,  t3,  t4,  t7,  t8, t11, t12, t13, t14, t16, t17, t18, t20, t21, t22, t25, t26, t27, t30, &
      &          t31, t32, t33, t38, t39, t41, t42, t43, t44, t45, t50, t51, t57, t58, t61, t66, t71, t72, t75, t80, &
      &          t85, t86, t89, t94, t95, t98, t99, t100, t103, t106, t111, t118, t125, t132, t133, t136, t140, t141, &
      &          t148, t155, t162, t169, t201, t202, t226, t232
    REAL(r8k), DIMENSION(2), INTENT(IN) :: x1, x2, x3
    REAL(r8k), DIMENSION(6,6), INTENT(OUT) :: Ke

      t1 = dir*frcoef
      t2 = t1*pfact
      t3 = x2(2)-x1(2)
      t4 = x2(1)**2
      t7 = x1(1)**2
      t8 = x2(2)**2
      t11 = x1(2)**2
      t12 = t4-2*x2(1)*x1(1)+t7+t8-2*x2(2)*x1(2)+t11
      t13 = SQRT(t12)
      t14 = 1/t13
      t16 = x3(1)-x1(1)
      t17 = t16*t3
      t18 = t13**2
      t20 = 1/t18/t13
      t21 = -x2(1)+x1(1)
      t22 = 2*t20*t21
      t25 = x3(2)-x1(2)
      t26 = t25*t14
      t27 = -t25*t21
      t30 = -t3*t14-t17*t22/2+t26+t27*t22/2
      t31 = -t30*t21
      t32 = 0.1e1_r8k-alpha0
      t33 = t14*t32
      t38 = t17*t14-t27*t14
      t39 = t38*t14
      t41 = t2*t39*t32
      t42 = pfact*t38
      t43 = t1*t42
      t44 = -t20*t21
      t45 = 2*t32*t21
      t50 = t16*t14
      t51 = -2*t20*t3
      t57 = -t50-t17*t51/2-t21*t14+t27*t51/2
      t58 = -t57*t21
      t61 = -2*t32*t3
      t66 = -2*t20*t21
      t71 = -t17*t66/2-t26+t27*t66/2
      t72 = -t71*t21
      t75 = -2*t32*t21
      t80 = 2*t20*t3
      t85 = t50-t17*t80/2+t27*t80/2
      t86 = -t85*t21
      t89 = 2*t32*t3
      t94 = 1/t12
      t95 = t3*t94
      t98 = -t2*t95*t32*t21
      t99 = t21**2
      t100 = t99*t94
      t103 = t30*t3
      t106 = t20*t3
      t111 = t57*t3
      t118 = t71*t3
      t125 = t85*t3
      t132 = t3**2
      t133 = t132*t94
      t136 = t14*alpha0
      t140 = t2*t39*alpha0
      t141 = 2*alpha0*t21
      t148 = -2*alpha0*t3
      t155 = -2*alpha0*t21
      t162 = 2*alpha0*t3
      t169 = -t2*t95*alpha0*t21
      t201 = t1*t42*t14
      t202 = -t38*t21
      t226 = -t2*t95*t21
      t232 = t38*t3
      Ke(1,1) = t2*t31*t33-t41-t43*t44*t45/2
      Ke(1,2) = t2*t58*t33-t43*t44*t61/2
      Ke(1,3) = t2*t72*t33+t41-t43*t44*t75/2
      Ke(1,4) = t2*t86*t33-t43*t44*t89/2
      Ke(1,5) = t98
      Ke(1,6) = -t2*t100*t32
      Ke(2,1) = t2*t103*t33-t43*t106*t45/2
      Ke(2,2) = t2*t111*t33-t41-t43*t106*t61/2
      Ke(2,3) = t2*t118*t33-t43*t106*t75/2
      Ke(2,4) = t2*t125*t33+t41-t43*t106*t89/2
      Ke(2,5) = t2*t133*t32
      Ke(2,6) = -t98
      Ke(3,1) = t2*t31*t136-t140-t43*t44*t141/2
      Ke(3,2) = t2*t58*t136-t43*t44*t148/2
      Ke(3,3) = t2*t72*t136+t140-t43*t44*t155/2
      Ke(3,4) = t2*t86*t136-t43*t44*t162/2
      Ke(3,5) = t169
      Ke(3,6) = -t2*t100*alpha0
      Ke(4,1) = t2*t103*t136-t43*t106*t141/2
      Ke(4,2) = t2*t111*t136-t140-t43*t106*t148/2
      Ke(4,3) = t2*t118*t136-t43*t106*t155/2
      Ke(4,4) = t2*t125*t136+t140-t43*t106*t162/2
      Ke(4,5) = t2*t133*alpha0
      Ke(4,6) = -t169
      Ke(5,1) = -t2*t31*t14+t201+t2*t202*t22/2
      Ke(5,2) = -t2*t58*t14+t2*t202*t51/2
      Ke(5,3) = -t2*t72*t14-t201+t2*t202*t66/2
      Ke(5,4) = -t2*t86*t14+t2*t202*t80/2
      Ke(5,5) = -t226
      Ke(5,6) = t1*pfact*t99*t94
      Ke(6,1) = -t2*t103*t14+t2*t232*t22/2
      Ke(6,2) = -t2*t111*t14+t201+t2*t232*t51/2
      Ke(6,3) = -t2*t118*t14+t2*t232*t66/2
      Ke(6,4) = -t2*t125*t14-t201+t2*t232*t80/2
      Ke(6,5) = -t1*pfact*t132*t94
      Ke(6,6) = t226

    RETURN

  END SUBROUTINE cont2d_stIff_fr


    SUBROUTINE cont2d_update()
    ! Update contact condition
    IMPLICIT NONE
    TYPE(cont2d_type), POINTER :: current
    REAL(r8k) :: frcoef

    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%closed0 ) THEN
          current%alpha0 = current%alpha
       ELSE If ( ( current%closed ).AND.( .NOT.current%stick) ) THEN
          frcoef = mat_par(current%mat,tref,'FRCOEF  ')
          current%alpha0 = current%alpha &
               + SIGN(1.0_r8k,current%dalpha)*frcoef*current%gap
       ENDIF
       current%closed0 = current%closed
       current%stick0 = current%stick
       current%gap0 = current%gap
       current%dalpha0 = current%dalpha
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE cont2d_update


    SUBROUTINE cont2d_Write_output(nunit)
    ! WRITE output to a file in unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(cont2dsurf_type), POINTER :: current_surf
    TYPE(cont2d_type), POINTER :: current

    ! Output surface definitions
    WRITE(UNIT=nunit) ncont2dsurf
    current_surf => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       WRITE(UNIT=nunit) current_surf%label
       WRITE(UNIT=nunit) current_surf%mat
       WRITE(UNIT=nunit) current_surf%ntarget
       WRITE(UNIT=nunit) current_surf%target_nodes(1:current_surf%ntarget)
       WRITE(UNIT=nunit) current_surf%ncontactor
       WRITE(UNIT=nunit) &
            current_surf%contactor_nodes(1:current_surf%ncontactor)
       current_surf => current_surf%next
    ENDDO

    ! Output contact elements
    WRITE(UNIT=nunit) ncont2d
    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%mat
       WRITE(UNIT=nunit) current%surf
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       WRITE(UNIT=nunit) current%closed0
       WRITE(UNIT=nunit) current%stick0
       WRITE(UNIT=nunit) current%gap0
       WRITE(UNIT=nunit) current%alpha0
       WRITE(UNIT=nunit) current%dalpha0
       current => current%next
    ENDDO    

    RETURN

  END SUBROUTINE cont2d_Write_output


    SUBROUTINE cont2d_read_output(nunit)
    ! Read output from unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(cont2dsurf_type), POINTER :: current_surf,previous_surf
    TYPE(cont2d_type), POINTER :: current,previous

    ! Read contact surface definitions
    Read(UNIT=nunit,ERR=20,End=20) ncont2dsurf

    current_surf => first_cont2dsurf
    DO i=1,ncont2dsurf
       IF ( .NOT.ASSOCIATED(current_surf) ) THEN
          ALLOCATE(current_surf)
          current_surf%next => NULL()
          current_surf%target_nodes => NULL()
          current_surf%contactor_nodes => NULL()
          IF ( .NOT.ASSOCIATED(first_cont2dsurf) ) THEN
             first_cont2dsurf => current_surf
             last_cont2dsurf => current_surf
          ELSE
             last_cont2dsurf%next => current_surf
             last_cont2dsurf => current_surf
          ENDIF
       ENDIF
       IF ( ASSOCIATED(current_surf%target_nodes) ) &
            DEALLOCATE(current_surf%target_nodes)
       IF ( ASSOCIATED(current_surf%contactor_nodes) ) &
            DEALLOCATE(current_surf%contactor_nodes)
       Read(UNIT=nunit,ERR=20,End=20) current_surf%label
       Read(UNIT=nunit,ERR=20,End=20) current_surf%mat
       Read(UNIT=nunit,ERR=20,End=20) current_surf%ntarget
       ALLOCATE(current_surf%target_nodes(current_surf%ntarget))
       Read(UNIT=nunit,ERR=20,End=20) &
            current_surf%target_nodes(1:current_surf%ntarget)
       Read(UNIT=nunit,ERR=20,End=20) current_surf%ncontactor
       ALLOCATE(current_surf%contactor_nodes(current_surf%ncontactor))
       Read(UNIT=nunit,ERR=20,End=20) &
            current_surf%contactor_nodes(1:current_surf%ncontactor)
       last_cont2dsurf => current_surf
       current_surf => current_surf%next
    ENDDO

    ! Remove excess surface entries from the Database
    DO WHILE ( ASSOCIATED(current_surf) )
       previous_surf => current_surf
       current_surf => current_surf%next
       DEALLOCATE(previous_surf)
    ENDDO
    IF ( ASSOCIATED(last_cont2dsurf) ) last_cont2dsurf%next => NULL()
    IF ( ncont2dsurf == 0 ) THEN
       first_cont2dsurf => NULL()
       last_cont2dsurf => NULL()
    ENDIF

    ! Read 2D contact elements
    Read(UNIT=nunit,ERR=20,End=20) ncont2d

    current => first_cont2d
    DO i=1,ncont2d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_cont2d) ) THEN
             first_cont2d => current
             last_cont2d => current
          ELSE
             last_cont2d%next => current
             last_cont2d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%mat
       Read(UNIT=nunit,ERR=20,End=20) current%surf
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels(1:3)
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers(1:3)
       Read(UNIT=nunit,ERR=20,End=20) current%closed0
       Read(UNIT=nunit,ERR=20,End=20) current%stick0
       Read(UNIT=nunit,ERR=20,End=20) current%gap0
       Read(UNIT=nunit,ERR=20,End=20) current%alpha0
       Read(UNIT=nunit,ERR=20,End=20) current%dalpha0
       last_cont2d => current
       current => current%next
    ENDDO

    ! Remove excess elements from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_cont2d) ) last_cont2d%next => NULL()
    IF ( ncont2d == 0 ) THEN
       first_cont2d => NULL()
       last_cont2d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading CONT2D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE cont2d_read_output


    SUBROUTINE cont2d_deallocate()
    ! Deallocate cont2d Database
    IMPLICIT NONE
    TYPE(cont2d_type), POINTER :: current,previous
    TYPE(cont2dsurf_type), POINTER :: current_surf,previous_surf

    current => first_cont2d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    current_surf => first_cont2dsurf
    DO WHILE ( ASSOCIATED(current_surf) )
       previous_surf => current_surf
       current_surf => current_surf%next
       DEALLOCATE(previous_surf%target_nodes,previous_surf%contactor_nodes)
       DEALLOCATE(previous_surf)
    ENDDO

    RETURN

  END SUBROUTINE cont2d_deallocate
END MODULE cont2d_fraptran














