MODULE gascav2d
    USE Kinds
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> Axisymmetric 2D gas cavity
    !
    !              xi(2)
    !             ^
    !     N4    S3|      N3            xi,eta ........... element coordinates
    !       o-----+-----o              N1,N2,N3,N4 ...... element nodes
    !       | G4  |  G3 |              G1,G2,G3,G4 ...... Gauss points or GPs
    !       |  x  |  x  |              S1,S2,S3,S4 ...... Element boundaries
    !       |     |     |
    !     S4|     +-----+----> xi(1)
    !       |           |S2
    !       |  x     x  |
    !       | G1     G2 |
    !       o-----------o
    !     N1     S1      N2
    !
    !

  INTEGER(ipk) :: ngascav2d
  REAL(r8k), PRIVATE :: xi(2,4) ! GP coordinate values in element CS
  REAL(r8k), PRIVATE :: N(4,4) ! Shape function values at GPs
  REAL(r8k), PRIVATE :: dNdxi(2,4,4) ! Shape function derivatives in
                                          ! element CS
  REAL(r8k), PRIVATE :: wgt(4) ! GP integration weight factors

  REAL(r8k), PRIVATE :: Ns(2,2),wgts(2),xis(2)

  TYPE gascav2d_type
     INTEGER(ipk) :: label ! Element label
     INTEGER(ipk) :: number ! Internal element number
     INTEGER(ipk) :: gascav ! Gas cavity label
     INTEGER(ipk) :: node_labels(4) ! Element node labels
     INTEGER(ipk) :: node_numbers(4) ! Internal element numbering
     LOGICAL :: active_side(4) ! Flag weather aply the pressure load
     REAL(r8k) :: gtemp(4) ! Integration point temperatures
     REAL(r8k) :: dNdx(2,4,4) ! Derivatives of the shape functions
     REAL(r8k) :: x(2,4) ! Integration point coordinates
     REAL(r8k) :: fepp(2,4) ! coefficient that give node forces
     REAL(r8k) :: dV(4) ! Integration point volumes
     TYPE(gascav2d_type), POINTER :: next
  END TYPE gascav2d_type

  TYPE(gascav2d_type), POINTER :: first_gascav2d,last_gascav2d

CONTAINS
    SUBROUTINE gascav2d_init()
    ! Initialize POINTERs and tables for Q4 element calculation
    IMPLICIT NONE
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate existing Database
    CALL gascav2d_deallocate()

    ! Initialize element number and POINTERs
    first_gascav2d => NULL()
    last_gascav2d => NULL()
    ngascav2d = 0

    ! Integration points and weight factors at the boundary
    wgts = 0.5_r8k
    tmp = 1.0_r8k/SQRT(3.0_r8k)
    xis(1) = 0.5_r8k*(1.0_r8k - tmp)
    xis(2) = 0.5_r8k*(1.0_r8k + tmp)

    DO ig=1,2
       Ns(1,ig) = 1.0_r8k - xis(ig)
       Ns(2,ig) = xis(ig)
    ENDDO

    ! GP element coordinates and weight factors
    wgt(1:4) = 1.0_r8k
    xi(1:2,1) = (/ -tmp, -tmp /)
    xi(1:2,2) = (/  tmp, -tmp /)
    xi(1:2,3) = (/  tmp,  tmp /)
    xi(1:2,4) = (/ -tmp,  tmp /)

    DO ig=1,4
       ! Shape functions at the gauss point
       N(1,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k - xi(2,ig))/4.0_r8k
       N(2,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k - xi(2,ig))/4.0_r8k
       N(3,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k + xi(2,ig))/4.0_r8k
       N(4,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k + xi(2,ig))/4.0_r8k

       ! Derivatives of the shape functions
       dNdxi(1,1,ig) = -(1.0_r8k - xi(2,ig))/4.0_r8k
       dNdxi(1,2,ig) =  (1.0_r8k - xi(2,ig))/4.0_r8k
       dNdxi(1,3,ig) =  (1.0_r8k + xi(2,ig))/4.0_r8k
       dNdxi(1,4,ig) = -(1.0_r8k + xi(2,ig))/4.0_r8k
       dNdxi(2,1,ig) = -(1.0_r8k - xi(1,ig))/4.0_r8k
       dNdxi(2,2,ig) = -(1.0_r8k + xi(1,ig))/4.0_r8k
       dNdxi(2,3,ig) =  (1.0_r8k + xi(1,ig))/4.0_r8k
       dNdxi(2,4,ig) =  (1.0_r8k - xi(1,ig))/4.0_r8k
    ENDDO    

    RETURN

  END SUBROUTINE gascav2d_init


    SUBROUTINE gascav2d_create(label,gascav,nodes,flag)
    ! Create new GASCAV2D element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: &
         label, & ! Element label
         gascav, & ! Gas cavity label for the element
         nodes(4), & ! Element nodel labels
         flag ! Flag for active side
              ! 0       = no forces
              ! 1 ... 4 = side number
              ! 5       = all sides
    INTEGER(ipk) :: id,nfound,in,jn,is
    REAL(r8k) :: tmp(2,2),tmp2
    TYPE(gascav2d_type), POINTER :: new_gascav2d

    ! Allocate new element
    ngascav2d = ngascav2d + 1
    ALLOCATE(new_gascav2d)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,4
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_gascav2d%node_labels(jn) = nodes(jn)
             new_gascav2d%node_numbers(jn) = in
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 4 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 4 ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR incompatible NODES and GASCAV2D Data in element ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Order element nodes properly
    is = flag
    DO id=1,2
       tmp(id,1) = x0(id,new_gascav2d%node_numbers(3)) - &
            x0(id,new_gascav2d%node_numbers(1))
       tmp(id,2) = x0(id,new_gascav2d%node_numbers(2)) - &
            x0(id,new_gascav2d%node_numbers(1))
    ENDDO
    tmp2 = tmp(1,1)*tmp(2,2) - tmp(1,2)*tmp(2,1)
    IF ( tmp2 > 0.0) THEN
       new_gascav2d%node_labels(1:4) = new_gascav2d%node_labels(4:1:-1)
       new_gascav2d%node_numbers(1:4) = new_gascav2d%node_numbers(4:1:-1)
       IF ( flag == 1 ) THEN
          is = 3
       ELSE If ( flag == 3 ) THEN
          is = 1
       ENDIF
    ENDIF

    ! Initialize GASCAV2D variables
    new_gascav2d%label = label
    new_gascav2d%number = ngascav2d
    new_gascav2d%gascav = gascav
    new_gascav2d%next => NULL()
    IF ( ( is > 4 ).OR.( is < 0 ) ) THEN
       new_gascav2d%active_side(1:4) = .TRUE.
    ELSE If ( is == 0 ) THEN
       new_gascav2d%active_side = .FALSE.
    ELSE
       new_gascav2d%active_side = .FALSE.
       new_gascav2d%active_side(is) = .TRUE.
    ENDIF

    ! New DOF numbering is needed
    IF ( pressure_matrix ) dof_numbering = .TRUE.

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_gascav2d) ) THEN
       first_gascav2d => new_gascav2d
       last_gascav2d => new_gascav2d
    ELSE
       last_gascav2d%next => new_gascav2d
       last_gascav2d => new_gascav2d
    ENDIF

    RETURN

  END SUBROUTINE gascav2d_create


    SUBROUTINE gascav2d_sparse_matrix()
    ! Initialize sparse matrix storage for gascav2d elements
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current
    TYPE(gascav_type), POINTER :: current_cav
    INTEGER(ipk) :: ndofnum
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: dofnum

    IF ( pressure_matrix ) THEN
       ALLOCATE(dofnum(4*nd*ngascav2d))
       current_cav => first_gascav
       DO WHILE ( ASSOCIATED(current_cav) )
          ! Find all elements that belong to this gas cavity and create a
          ! superelement from all of these elements
          ndofnum = 0
          current => first_gascav2d
          DO WHILE ( ASSOCIATED(current) )
             IF ( current%gascav == current_cav%label ) THEN
                dofnum(ndofnum+1:ndofnum+2) = &
                     dof_number(1:2,current%node_numbers(1))
                dofnum(ndofnum+3:ndofnum+4) = &
                     dof_number(1:2,current%node_numbers(2))
                dofnum(ndofnum+5:ndofnum+6) = &
                     dof_number(1:2,current%node_numbers(3))
                dofnum(ndofnum+7:ndofnum+8) = &
                     dof_number(1:2,current%node_numbers(4))
                ndofnum = ndofnum + 8
             ENDIF
             current => current%next
          ENDDO
          CALL sparse_matrix_add_element(ndofnum,dofnum)
          current_cav => current_cav%next
       ENDDO
       DEALLOCATE(dofnum)
    ENDIF

    RETURN

  END SUBROUTINE gascav2d_sparse_matrix


    SUBROUTINE gascav2d_temp()
    ! Initialize temperatures
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current
    INTEGER(ipk) :: ig,in
    REAL(r8k) :: gtemp

    current => first_gascav2d
    DO WHILE ( ASSOCIATED(current) )
       DO ig=1,4
          ! GP temperature
          gtemp = 0.0_r8k
          DO in=1,4
             gtemp = gtemp + N(in,ig)*temp(current%node_numbers(in))
          ENDDO
          current%gtemp(ig) = gtemp
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_temp


    SUBROUTINE gascav2d_deriv()
    ! Evaluate cartesian derivatives of the shape functions
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current
    INTEGER(ipk) :: ig,id,jd,in
    REAL(r8k) :: xn(2,4),jaci(2,2),jac(2,2),detj,tmp2

    current => first_gascav2d
    DO WHILE ( ASSOCIATED(current) )
       xn(1:2,1) = x(1:2,current%node_numbers(1))
       xn(1:2,2) = x(1:2,current%node_numbers(2))
       xn(1:2,3) = x(1:2,current%node_numbers(3))
       xn(1:2,4) = x(1:2,current%node_numbers(4))

       DO ig=1,4
          ! Jacobian
          DO id=1,2
             DO jd=1,2
                jac(id,jd) = 0.0_r8k
                DO in=1,4
                   jac(id,jd) = jac(id,jd) + dNdxi(id,in,ig)*xn(jd,in)
                ENDDO
             ENDDO
          ENDDO

          ! Inverse of jacobian
          detj = jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1)
          tmp2 = 1.0_r8k/detj
          jaci(1,1) =  jac(2,2)*tmp2
          jaci(2,2) =  jac(1,1)*tmp2
          jaci(1,2) = -jac(1,2)*tmp2
          jaci(2,1) = -jac(2,1)*tmp2

          ! Gauss point coordinates
          current%x(1:2,ig) = 0.0_r8k
          DO in=1,4
             current%x(1:2,ig) = current%x(1:2,ig) + N(in,ig)*xn(1:2,in)
          ENDDO

          ! Derivatives of the shape functions in cartesian coordinate system
          DO in=1,4
             DO id=1,2
                current%dNdx(id,in,ig) = 0.0_r8k
                DO jd=1,2
                   current%dNdx(id,in,ig) = current%dNdx(id,in,ig) &
                        + jaci(id,jd)*dNdxi(jd,in,ig)
                ENDDO
             ENDDO
          ENDDO

          ! Integration point weight factor
          current%dV(ig) = 2.0_r8k*pi*current%x(1,ig)*wgt(ig)*detj

       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_deriv


    SUBROUTINE gascav2d_vpt()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    INTEGER(ipk) :: ig
    TYPE(gascav_type), POINTER :: current_cav
    TYPE(gascav2d_type), POINTER :: current

    CALL gascav2d_deriv()

    current_cav => first_gascav
    DO WHILE ( ASSOCIATED(current_cav) )
       current => first_gascav2d
       DO WHILE ( ASSOCIATED(current) )
          IF ( current_cav%label == current%gascav ) THEN
             DO ig=1,4
                current_cav%VpT = current_cav%VpT + &
                     current%dV(ig)/current%gtemp(ig)
                current_cav%V_tot = current_cav%V_tot + current%dV(ig)
             ENDDO
          ENDIF
          current => current%next
       ENDDO
       current_cav => current_cav%next
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_vpt


    SUBROUTINE gascav2d_fext()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current
    TYPE(gascav_type), POINTER :: current_cav
    REAL(r8k) :: p,r(2),z(2),tmp,rg
    INTEGER(ipk) :: is,i,j,ig,N1,N2

    current => first_gascav2d
    DO WHILE ( ASSOCIATED(current) )
       current_cav => first_gascav
       DO WHILE ( ASSOCIATED(current_cav) )
          IF ( current%gascav == current_cav%label ) EXIT
          current_cav => current_cav%next
       ENDDO
       IF ( .NOT.ASSOCIATED(current_cav) ) THEN
          WRITE(UNIT=6,FMT='(A,I0)') &
               'ERROR cannot find gas cavity ',current%gascav
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ENDIF
       p = current_cav%p
       current%fepp = 0.0_r8k
       DO is=1,4
          IF ( current%active_side(is) ) THEN
             SELECT CASE (is)
             CASE ( 1 )
                N1 = current%node_numbers(1)
                N2 = current%node_numbers(2)
             CASE ( 2 )
                N1 = current%node_numbers(2)
                N2 = current%node_numbers(3)
             CASE ( 3 )
                N1 = current%node_numbers(3)
                N2 = current%node_numbers(4)
             CASE ( 4 )
                N1 = current%node_numbers(4)
                N2 = current%node_numbers(1)
             END SELECT
             i = is
             j = is + 1
             IF ( j == 5 ) j = 1
             r(1) = x(1,N1)
             r(2) = x(1,N2)
             z(1) = x(2,N1)
             z(2) = x(2,N2)
             DO ig=1,2
                rg = Ns(1,ig)*r(1) + Ns(2,ig)*r(2)
                tmp = wgts(ig)*Ns(1,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
                current%fepp(1,i) = current%fepp(1,i) + tmp
                Fext(1,N1) = Fext(1,N1) + p*tmp
                tmp = wgts(ig)*Ns(1,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
                current%fepp(2,i) = current%fepp(2,i) + tmp
                Fext(2,N1) = Fext(2,N1) + p*tmp
                tmp = wgts(ig)*Ns(2,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
                current%fepp(1,j) = current%fepp(1,j) + tmp
                Fext(1,N2) = Fext(1,N2) + p*tmp
                tmp = wgts(ig)*Ns(2,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
                current%fepp(2,j) = current%fepp(2,j) + tmp
                Fext(2,N2) = Fext(2,N2) + p*tmp
             ENDDO
          ENDIF
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_fext


    SUBROUTINE gascav2d_stIff()
    ! Stiffness matrix due to GASCAV2D elements
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current,current2
    TYPE(gascav_type), POINTER :: current_cav
    REAL(r8k) :: p,r(2),z(2),rg,Ke1(4,4),Ke2(8,8),Bi(2,4),mgas,VpT,tmp
    INTEGER(ipk) :: is,dofnum1(4),ig,in,jn,jd,ie,je,N1,N2

    IF ( pressure_matrix ) THEN

       IF ( ngascav2d > 0 ) symmetric_matrix = .FALSE.

       current => first_gascav2d
       DO WHILE ( ASSOCIATED(current) )
          current_cav => first_gascav
          DO WHILE ( ASSOCIATED(current_cav) )
             IF ( current%gascav == current_cav%label ) EXIT
             current_cav => current_cav%next
          ENDDO
          IF ( .NOT.ASSOCIATED(current_cav) ) THEN
             WRITE(UNIT=6,FMT='(A,I0)') &
                  'ERROR cannot find gas cavity ',current%gascav
             lerror = .TRUE.
             CALL nlfemp_stop(0)
          ENDIF
          p = current_cav%p
          mgas = current_cav%mgas
          VpT = current_cav%VpT
          DO is=1,4
             IF ( current%active_side(is) ) THEN
                SELECT CASE ( is )
                CASE ( 1 )
                   N1 = current%node_numbers(1)
                   N2 = current%node_numbers(2)
                CASE ( 2 )
                   N1 = current%node_numbers(2)
                   N2 = current%node_numbers(3)
                CASE ( 3 )
                   N1 = current%node_numbers(3)
                   N2 = current%node_numbers(4)
                CASE ( 4 )
                   N1 = current%node_numbers(4)
                   N2 = current%node_numbers(1)
                END SELECT
                r(1) = x(1,N1)
                r(2) = x(1,N2)
                z(1) = x(2,N1)
                z(2) = x(2,N2)
                Ke1 = 0.0_r8k
                DO ig=1,2
                   rg = Ns(1,ig)*r(1) + Ns(2,ig)*r(2)
                   Ke1(1,1) = Ke1(1,1) - &
                        wgts(ig)*p*Ns(1,ig)*(z(2) - z(1))*2.0_r8k*pi*Ns(1,ig)
                   Ke1(1,2) = Ke1(1,2) + wgts(ig)*p*Ns(1,ig)*2.0_r8k*pi*rg
                   Ke1(1,3) = Ke1(1,3) - &
                        wgts(ig)*p*Ns(1,ig)*(z(2) - z(1))*2.0_r8k*pi*Ns(2,ig)
                   Ke1(1,4) = Ke1(1,4) - wgts(ig)*p*Ns(1,ig)*2.0_r8k*pi*rg
                   Ke1(2,1) = Ke1(2,1) - wgts(ig)*p*Ns(1,ig)*2.0_r8k*pi*rg - &
                        wgts(ig)*p*Ns(1,ig)*(r(1) - r(2))*2.0_r8k*pi*Ns(1,ig)
                   Ke1(2,3) = Ke1(2,3) + wgts(ig)*p*Ns(1,ig)*2.0_r8k*pi*rg - &
                        wgts(ig)*p*Ns(1,ig)*(r(1) - r(2))*2.0_r8k*pi*Ns(2,ig)
                   Ke1(3,1) = Ke1(3,1) - &
                        wgts(ig)*p*Ns(2,ig)*(z(2) - z(1))*2.0_r8k*pi*Ns(1,ig)
                   Ke1(3,2) = Ke1(3,2) + wgts(ig)*p*Ns(2,ig)*2.0_r8k*pi*rg
                   Ke1(3,3) = Ke1(3,3) - &
                        wgts(ig)*p*Ns(2,ig)*(z(2) - z(1))*2.0_r8k*pi*Ns(2,ig)
                   Ke1(3,4) = Ke1(3,4) - wgts(ig)*p*Ns(2,ig)*2.0_r8k*pi*rg
                   Ke1(4,1) = Ke1(4,1) - wgts(ig)*p*Ns(2,ig)*2.0_r8k*pi*rg - &
                        wgts(ig)*p*Ns(2,ig)*(r(1) - r(2))*2.0_r8k*pi*Ns(1,ig)
                   Ke1(4,3) = Ke1(4,3) + wgts(ig)*p*Ns(2,ig)*2.0_r8k*pi*rg + &
                        wgts(ig)*p*Ns(2,ig)*(r(1) - r(2))*2.0_r8k*pi*Ns(2,ig)
                ENDDO
                Ke1 = Ke1
                dofnum1(1:2) = dof_number(1:2,N1)
                dofnum1(3:4) = dof_number(1:2,N2)
                CALL sparse_matrix_place_element(4,dofnum1,Ke1)
             ENDIF
          ENDDO
          Ke2 = 0.0_r8k
          Bi = 0.0_r8k
          DO ig=1,4
             tmp = mgas*current%dV(ig)/(current%gtemp(ig)*VpT**2)
             DO in=1,4
                Bi(1,in) = Bi(1,in) + &
                     tmp*(current%dNdx(1,in,ig) + N(in,ig)/current%x(1,ig))
                Bi(2,in) = Bi(2,in) + tmp*current%dNdx(2,in,ig)
             ENDDO
          ENDDO

          IF ( .NOT.current_cav%failed ) THEN
             current2 => first_gascav2d
             DO WHILE ( ASSOCIATED(current2) )
                IF ( current2%gascav == current%gascav ) THEN
                   DO in=1,4
                      N1 = current%node_numbers(in)
                      DO jn=1,4
                         N2 = current%node_numbers(jn)
                         DO jd=1,2
                            je = dof_number(jd,N2)
                            ie = dof_number(1,N1)
                            tmp = current2%fepp(jd,jn)*Bi(1,in)
                            CALL sparse_matrix_place_value(je,ie,tmp)
                            ie = dof_number(2,N1)
                            tmp = current2%fepp(jd,jn)*Bi(2,in)
                            CALL sparse_matrix_place_value(je,ie,tmp)
                         ENDDO
                      ENDDO
                   ENDDO
                ENDIF
                current2 => current2%next
             ENDDO
          ENDIF
          current => current%next
       ENDDO
    ENDIF

    RETURN

  END SUBROUTINE gascav2d_stIff


    SUBROUTINE gascav2d_Write_output(nunit)
    ! WRITE pressure Data
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(gascav2d_type), POINTER :: current

    WRITE(UNIT=nunit) ngascav2d
    current => first_gascav2d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%gascav
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_Write_output


    SUBROUTINE gascav2d_read_output(nunit)
    ! Read output from unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(gascav2d_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) ngascav2d

    current => first_gascav2d
    DO i=1,ngascav2d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_gascav2d) ) THEN
             first_gascav2d => current
             last_gascav2d => current
          ELSE
             last_gascav2d%next => current
             last_gascav2d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%gascav
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       last_gascav2d => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_gascav2d) ) last_gascav2d%next => NULL()
    IF ( ngascav2d == 0 ) THEN
       first_gascav2d => NULL()
       last_gascav2d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading GASCAV2D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE gascav2d_read_output


    SUBROUTINE gascav2d_deallocate()
    ! Deallocate gascav2d Database
    IMPLICIT NONE
    TYPE(gascav2d_type), POINTER :: current,previous

    current => first_gascav2d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE gascav2d_deallocate
END MODULE gascav2d
