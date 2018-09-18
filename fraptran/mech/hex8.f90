MODULE hex8
    USE Kinds
    USE common_parameters
    USE math
    USE sparse_matrix
    USE materials_frap
    IMPLICIT NONE
    !>@brief
    !> Hexahedral 8-node brick element with mean dilation procedure
    !> 2x2x2 quadrature for the deviatoric part and mean dilation procedure for the dilational part
    !
    !        5 o-------o 8
    !         /|  ^xi(3)
    !        / |  |  / |
    !     6 o-------o 7|
    !       |  |  | |  |
    !       |  |  | |  |
    !       |  |  +-|-----> xi(2)
    !       |  | /  |  |
    !       |1 o/- -|--o 4
    !       | /L xi(1)/
    !       |/      |/
    !       o-------o
    !     2           3
    !
    ! Vector presentation for stresses
    !   Index Stress component
    !     1     11
    !     2     22
    !     3     33
    !     4     12
    !     5     23
    !     6     13
    !

  INTEGER(ipk) :: nhex8 ! Number of HEX8 elements
  REAL(r8k), PRIVATE :: xi(3,8) ! GP coordinate values in element CS
  REAL(r8k), PRIVATE :: N(8,8) ! Shape function values at GPs
  REAL(r8k), PRIVATE :: dNdxi(3,8,8) ! Shape function derivatives in
                                          ! element CS
  REAL(r8k), PRIVATE :: wgt(8) ! GP integration weight factors


  TYPE hex8_type
     INTEGER(ipk) :: label ! Element label
     INTEGER(ipk) :: number ! Internal element number
     INTEGER(ipk) :: mat ! Material flag
     INTEGER(ipk) :: egroup ! Element group label
     INTEGER(ipk) :: node_labels(8) ! Element node labels
     INTEGER(ipk) :: node_numbers(8) ! Internal node numbering
     REAL(r8k) :: gtemp(8) ! Integration point temperatures
     REAL(r8k) :: mu(8) ! Temperature dependent material parameter
     REAL(r8k) :: lambda(8) ! Temperature dependent material parameter
     REAL(r8k) :: kappa ! Temperature dependent material parameter
     REAL(r8k) :: epsth(8) ! Thermal dilation
     REAL(r8k) :: sigma(6,8) ! Deviatoric Cauchy stresses
     REAL(r8k) :: phyd ! Hydrostatic pressure
     REAL(r8k) :: dNdx(3,8,8) ! Derivatives of the shape functions
     REAL(r8k) :: dNdx0(3,8,8) ! Initial derivatives of the shape
                                    ! functions
     REAL(r8k) :: dNdxm(3,8) ! Mean derivatives of the shape functions
     REAL(r8k) :: dV(8) ! Weight factor of GP
     REAL(r8k) :: dV0(8) ! Initial weight factor of GP
     REAL(r8k) :: V ! Element volume
     REAL(r8k) :: V0 ! Initial element volume
     REAL(r8k) :: dl2(3,8) ! Squares of principal stretches
     REAL(r8k) :: nv(3,3,8) ! Principal directions
     REAL(r8k) :: cpi(6,8) ! Inverse of plastic Green-Lagrange tensor
     REAL(r8k) :: cpi0(6,8) ! Explicit inverse of plastic Green-Lagrange
                                 ! strain tensor
     REAL(r8k) :: J(8) ! Dilation
     REAL(r8k) :: Jm ! Mean dilation
     REAL(r8k) :: gamma(8) ! Plastic multiplier
     REAL(r8k) :: av(3,8) ! Plastic flow direction
     REAL(r8k) :: epseff(8) ! Effective plastic strains
     REAL(r8k) :: epseff0(8) ! Explicit effective plastic strains
     REAL(r8k) :: sigdev(3,8) ! Principal deviatoric Cauchy stresses
     REAL(r8k) :: sigeff(8) ! Effective stress
     REAL(r8k) :: sigeff0(8) ! Explicit effective stress
     REAL(r8k) :: plSED(8) ! Plastic SED
     REAL(r8k) :: taueff(8) ! Effective trial Kirchoff stress
     REAL(r8k) :: dplmod(8) ! Plastic modulus
     REAL(r8k) :: deds(8) ! Creep modulus
     REAL(r8k) :: epstot(6,8) ! Total strains
     REAL(r8k) :: x(3,8) ! Coordinates of integration points
     REAL(r8k) :: x0(3,8) ! Initial coordinates of integration points
     TYPE(hex8_type), POINTER :: next ! Next element
  END TYPE hex8_type

  TYPE(hex8_type), POINTER :: &
       first_hex8,last_hex8 ! POINTERs to the element Database

CONTAINS
    SUBROUTINE hex8_init()
    ! Initialize POINTERs and tables for HEX8 element calculation
    IMPLICIT NONE
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate possible existing element Database
    CALL hex8_deallocate()

    ! Initialize element number and POINTERs
    first_hex8 => NULL()
    last_hex8 => NULL()
    nhex8 = 0

    ! GP element coordinates and weight factors
    wgt(1:8) = 1.0_r8k
    tmp = 1.0_r8k/SQRT(3.0_r8k)
    xi(1:3,1) = (/ -tmp, -tmp, -tmp/)
    xi(1:3,2) = (/  tmp, -tmp, -tmp/)
    xi(1:3,3) = (/  tmp,  tmp, -tmp/)
    xi(1:3,4) = (/ -tmp,  tmp, -tmp/)
    xi(1:3,5) = (/ -tmp, -tmp,  tmp/)
    xi(1:3,6) = (/  tmp, -tmp,  tmp/)
    xi(1:3,7) = (/  tmp,  tmp,  tmp/)
    xi(1:3,8) = (/ -tmp,  tmp,  tmp/)

    DO ig=1,8
       ! Shape functions at the gauss point
       N(1,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k - xi(2,ig))* &
            (1.0_r8k - xi(3,ig))/8.0_r8k
       N(2,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k - xi(2,ig))* &
            (1.0_r8k - xi(3,ig))/8.0_r8k
       N(3,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k + xi(2,ig))* &
            (1.0_r8k - xi(3,ig))/8.0_r8k
       N(4,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k + xi(2,ig))* &
            (1.0_r8k - xi(3,ig))/8.0_r8k
       N(5,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k - xi(2,ig))* &
            (1.0_r8k + xi(3,ig))/8.0_r8k
       N(6,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k - xi(2,ig))* &
            (1.0_r8k + xi(3,ig))/8.0_r8k
       N(7,ig) = (1.0_r8k + xi(1,ig))*(1.0_r8k + xi(2,ig))* &
            (1.0_r8k + xi(3,ig))/8.0_r8k
       N(8,ig) = (1.0_r8k - xi(1,ig))*(1.0_r8k + xi(2,ig))* &
            (1.0_r8k + xi(3,ig))/8.0_r8k

       ! Derivatives of the shape functions
       dNdxi(1,1,ig) = -(1.0_r8k - xi(2,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(1,2,ig) =  (1.0_r8k - xi(2,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(1,3,ig) =  (1.0_r8k + xi(2,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(1,4,ig) = -(1.0_r8k + xi(2,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(1,5,ig) = -(1.0_r8k - xi(2,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(1,6,ig) =  (1.0_r8k - xi(2,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(1,7,ig) =  (1.0_r8k + xi(2,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(1,8,ig) = -(1.0_r8k + xi(2,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(2,1,ig) = -(1.0_r8k - xi(1,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(2,2,ig) = -(1.0_r8k + xi(1,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(2,3,ig) =  (1.0_r8k + xi(1,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(2,4,ig) =  (1.0_r8k - xi(1,ig))*(1.0_r8k - xi(3,ig))/8.0_r8k
       dNdxi(2,5,ig) = -(1.0_r8k - xi(1,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(2,6,ig) = -(1.0_r8k + xi(1,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(2,7,ig) =  (1.0_r8k + xi(1,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(2,8,ig) =  (1.0_r8k - xi(1,ig))*(1.0_r8k + xi(3,ig))/8.0_r8k
       dNdxi(3,1,ig) = -(1.0_r8k - xi(1,ig))*(1.0_r8k - xi(2,ig))/8.0_r8k
       dNdxi(3,2,ig) = -(1.0_r8k + xi(1,ig))*(1.0_r8k - xi(2,ig))/8.0_r8k
       dNdxi(3,3,ig) = -(1.0_r8k + xi(1,ig))*(1.0_r8k + xi(2,ig))/8.0_r8k
       dNdxi(3,4,ig) = -(1.0_r8k - xi(1,ig))*(1.0_r8k + xi(2,ig))/8.0_r8k
       dNdxi(3,5,ig) =  (1.0_r8k - xi(1,ig))*(1.0_r8k - xi(2,ig))/8.0_r8k
       dNdxi(3,6,ig) =  (1.0_r8k + xi(1,ig))*(1.0_r8k - xi(2,ig))/8.0_r8k
       dNdxi(3,7,ig) =  (1.0_r8k + xi(1,ig))*(1.0_r8k + xi(2,ig))/8.0_r8k
       dNdxi(3,8,ig) =  (1.0_r8k - xi(1,ig))*(1.0_r8k + xi(2,ig))/8.0_r8k
    ENDDO    

    RETURN

  END SUBROUTINE hex8_init


    SUBROUTINE hex8_create(label,mat,nodes)
    ! Create new HEX8 element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,mat,nodes(8)
    INTEGER(ipk) :: ig,nfound,in,jn
    TYPE(hex8_type), POINTER :: new_hex8,current
    REAL(r8k) :: x12(3),x14(3),x15(3),cprd(3)

    ! Set analysis type to 3D If necessary
    IF ( nd == 2 ) THEN
       dimens = '3D'
       nd = 3
    ENDIF

    ! Allocate new element
    nhex8 = nhex8 + 1
    ALLOCATE(new_hex8)
    CALL mat_create(mat)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,8
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_hex8%node_labels(jn) = nodes(jn)
             new_hex8%node_numbers(jn) = in
             enumber(in) = enumber(in) + 1
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 8 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 8 ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR incompatible NODES and HEX8 Data in element ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Check that the element label doesn't already exist
    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) THEN
          WRITE(UNIT=6,FMT='(/,A,I0,A,/)') &
               'ERROR HEX8 ',label,' has been defined twice'
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ENDIF
       current => current%next
    ENDDO

    ! Order element nodes properly
    x12(1:3)=x0(1:3,new_hex8%node_numbers(2))-x0(1:3,new_hex8%node_numbers(1))
    x14(1:3)=x0(1:3,new_hex8%node_numbers(4))-x0(1:3,new_hex8%node_numbers(1))
    x15(1:3)=x0(1:3,new_hex8%node_numbers(5))-x0(1:3,new_hex8%node_numbers(1))
    cprd(1) = x12(2)*x14(3) - x14(2)*x12(3)
    cprd(2) = x12(3)*x14(1) - x14(3)*x12(1)
    cprd(3) = x12(1)*x14(2) - x14(1)*x12(2)
    IF ( DOT_PRODUCT(cprd,x15) < 0.0_r8k ) THEN
       new_hex8%node_labels(1:4) = new_hex8%node_labels(4:1:-1)
       new_hex8%node_labels(5:8) = new_hex8%node_labels(8:5:-1)
       new_hex8%node_numbers(1:4) = new_hex8%node_numbers(4:1:-1)
       new_hex8%node_numbers(5:8) = new_hex8%node_numbers(8:5:-1)
    ENDIF

    ! Initialize HEX8 variables
    new_hex8%label = label
    new_hex8%number = nhex8
    new_hex8%mat = mat
    new_hex8%egroup = egroup
    DO ig=1,8
       new_hex8%cpi0(1:6,ig) = &
            (/1.0_r8k,1.0_r8k,0.0_r8k,1.0_r8k,0.0_r8k,0.0_r8k/)
    ENDDO
    new_hex8%epseff0 = 0.0_r8k
    new_hex8%sigeff0 = 0.0_r8k
    new_hex8%sigma = 0.0_r8k
    new_hex8%phyd = 0.0_r8k
    new_hex8%epstot = 0.0_r8k
    new_hex8%epsth = 0.0_r8k
    new_hex8%plSED = 0.0_r8k
    new_hex8%next => NULL()

    ! Initial derivatives
    CALL hex8_deriv(x0(1,new_hex8%node_numbers(1)), &
         x0(1,new_hex8%node_numbers(2)),x0(1,new_hex8%node_numbers(3)), &
         x0(1,new_hex8%node_numbers(4)),x0(1,new_hex8%node_numbers(5)), &
         x0(1,new_hex8%node_numbers(6)),x0(1,new_hex8%node_numbers(7)), &
         x0(1,new_hex8%node_numbers(8)),new_hex8%V0,new_hex8%dV0, &
         new_hex8%dNdx0,new_hex8%dNdxm,new_hex8%x0)

    IF ( lerror ) Call nlfemp_stop(0)

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_hex8) ) THEN
       first_hex8 => new_hex8
       last_hex8 => new_hex8
    ELSE
       last_hex8%next => new_hex8
       last_hex8 => new_hex8
    ENDIF

    RETURN

  END SUBROUTINE hex8_create


    SUBROUTINE hex8_sparse_matrix()
    ! Initialize sparse matrix storage for hex8 elements
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: dofnum(24)

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1:3) = dof_number(1:3,current%node_numbers(1))
       dofnum(4:6) = dof_number(1:3,current%node_numbers(2))
       dofnum(7:9) = dof_number(1:3,current%node_numbers(3))
       dofnum(10:12) = dof_number(1:3,current%node_numbers(4))
       dofnum(13:15) = dof_number(1:3,current%node_numbers(5))
       dofnum(16:18) = dof_number(1:3,current%node_numbers(6))
       dofnum(19:21) = dof_number(1:3,current%node_numbers(7))
       dofnum(22:24) = dof_number(1:3,current%node_numbers(8))
       CALL sparse_matrix_add_element(24,dofnum)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_sparse_matrix


    SUBROUTINE hex8_temp()
    ! Initialize temperature dependent properties
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: im,ig,in
    REAL(r8k) :: emod,nu,lambda,mu,gtemp,epsth,epsth0

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       im = current%mat
       epsth0 = mat_par(im,tref,'THDIL   ')
       current%kappa = 0.0_r8k
       DO ig=1,8
          ! Integration point coordinates
          cp = current%x0(1:3,ig)

          ! GP temperature
          gtemp = 0.0_r8k
          DO in=1,8
             gtemp = gtemp + N(in,ig)*temp(current%node_numbers(in))
          ENDDO
          current%gtemp(ig) = gtemp

          ! Temperature dependent elastic material properties
          emod = mat_par(im,gtemp,'EMOD    ')
          nu = mat_par(im,gtemp,'PRATIO  ')
          lambda = emod*nu/(1.0_r8k-2.0_r8k*nu)/(1.0_r8k + nu)
          mu = 0.5_r8k*emod/(1.0_r8k+nu)
          current%lambda(ig) = lambda
          current%mu(ig) = mu
          current%kappa = current%kappa + (lambda + 2.0_r8k*mu/3.0_r8k)/8.0_r8k

          ! Thermal dilation
          epsth = mat_par(im,gtemp,'THDIL   ')
          current%epsth(ig) = 1.0_r8k/EXP(epsth - epsth0)
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_temp


    SUBROUTINE hex8_grav()
    ! Gravity load
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: im,in,ig,id
    REAL(r8k) :: rho
    INTEGER(ipk), DIMENSION(:), POINTER :: nodes

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       im = current%mat
       rho = mat_par(im,tref,'DENSITY ')
       nodes => current%node_numbers(1:8)
       DO in=1,8
          DO ig=1,8
             DO id=1,3
                Fgrav(id,nodes(in)) = Fgrav(id,nodes(in)) + &
                     N(in,ig)*rho*current%dV0(ig)*grav(id)
             ENDDO
          ENDDO
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_grav


    SUBROUTINE hex8_deriv(x1,x2,x3,x4,x5,x6,x7,x8,V,dV,dNdx,dNdxm,x)
    ! Evaluate cartesian derivatives of the shape functions
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: x1(3),x2(3),x3(3),x4(3),x5(3),x6(3),x7(3), &
         x8(3)
    REAL(r8k), INTENT(OUT) :: V,dV(8),dNdx(3,8,8),dNdxm(3,8),x(3,8)
    INTEGER(ipk) :: ig,id,jd,in
    REAL(r8k) :: xn(3,8),jaci(3,3),jac(3,3),detj

    xn(1:3,1) = x1(1:3)
    xn(1:3,2) = x2(1:3)
    xn(1:3,3) = x3(1:3)
    xn(1:3,4) = x4(1:3)
    xn(1:3,5) = x5(1:3)
    xn(1:3,6) = x6(1:3)
    xn(1:3,7) = x7(1:3)
    xn(1:3,8) = x8(1:3)

    V = 0.0_r8k
    dNdxm = 0.0_r8k

    DO ig=1,8
       ! Jacobian
       DO id=1,3
          DO jd=1,3
             jac(id,jd) = 0.0_r8k
             DO in=1,8
                jac(id,jd) = jac(id,jd) + dNdxi(id,in,ig)*xn(jd,in)
             ENDDO
          ENDDO
       ENDDO

       ! Inverse of jacobian
       CALL inverse3(jac,jaci,detj)

       ! Derivatives of the shape functions in cartesian coordinate system
       DO in=1,8
          DO id=1,3
             dNdx(id,in,ig) = 0.0_r8k
             DO jd=1,3
                dNdx(id,in,ig) = dNdx(id,in,ig) + jaci(id,jd)*dNdxi(jd,in,ig)
             ENDDO
          ENDDO
       ENDDO

       ! Integration point weight factor
       dV(ig) = wgt(ig)*detj

       ! Element volume
       V = V + dV(ig)

       ! Integration point coordinate
       x(1:3,ig) = 0.0_r8k
       DO in=1,8
          x(1:3,ig) = x(1:3,ig) + N(in,ig)*xn(1:3,in)
       ENDDO

       ! Mean derivatives
       DO in=1,8
          DO id=1,3
             dNdxm(id,in) = dNdxm(id,in) + dNdx(id,in,ig)*dV(ig)
          ENDDO
       ENDDO

       ! Check for poorly shaped elements
       IF ( dV(ig) <= 0.0_r8k ) THEN
          lerror = .TRUE.
          RETURN
       ENDIF
    ENDDO

    ! Scale mean derivatives with element volume
    dNdxm(1:3,1:8) = dNdxm(1:3,1:8)/V

    RETURN

  END SUBROUTINE hex8_deriv


    SUBROUTINE hex8_stress()
    ! Element stress calculation
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: ig,id,jd,kd,mat,in,jn
    REAL(r8k) :: Vm,lnJm,f(3,3),tmp,be(3,3),dl(3),en(3),taudev(3)
    REAL(r8k), POINTER :: gamma, epseff,sigeff,epsth,lambda,mu,gtemp, &
         epseff0,J,phyd,kappa,Jm,taueff,dplmod,deds
    REAL(r8k), DIMENSION(:), POINTER :: dl2, cpi, cpi0, sigdev, sigma, av
    REAL(r8k), DIMENSION(:,:), POINTER :: nv

    ! Stress calculation loop
    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       mat = current%mat

       ! Derivatives
       CALL hex8_deriv(x(1,current%node_numbers(1)), &
            x(1,current%node_numbers(2)),x(1,current%node_numbers(3)), &
            x(1,current%node_numbers(4)),x(1,current%node_numbers(5)), &
            x(1,current%node_numbers(6)),x(1,current%node_numbers(7)), &
            x(1,current%node_numbers(8)),current%V,current%dV, &
            current%dNdx,current%dNdxm,current%x)

       ! Exit If there are badly shaped elements
       IF ( lerror ) RETURN

       ! Assign POINTERs
       phyd => current%phyd
       kappa => current%kappa
       Jm => current%Jm

       ! Mechanical volume of the element
       Vm = 0.0_r8k
       DO ig=1,8
          Vm = Vm + current%dV(ig)*current%epsth(ig)**3
       ENDDO

       ! Mean dilation
       Jm = Vm/current%V0

       ! Evaluate mean hydrostatic pressure
       lnJm = LOG(Jm)
       phyd = kappa*lnJm/Jm

       ! Pressure part of the element strain energy contribution
       elastic_strain_energy = elastic_strain_energy + kappa*lnJm**2*current%V0

       ! Deviatoric part
       DO ig=1,8
          ! Integration point coordinates
          cp = current%x0(1:3,ig)

          ! Assign POINTERs
          dl2 => current%dl2(1:3,ig)
          nv => current%nv(1:3,1:3,ig)
          cpi => current%cpi(1:6,ig)
          cpi0 => current%cpi0(1:6,ig)
          gamma => current%gamma(ig)
          sigdev => current%sigdev(1:3,ig)
          epseff => current%epseff(ig)
          epseff0 => current%epseff0(ig)
          sigma => current%sigma(1:6,ig)
          sigeff => current%sigeff(ig)
          epsth => current%epsth(ig)
          lambda => current%lambda(ig)
          mu => current%mu(ig)
          gtemp => current%gtemp(ig)
          av => current%av(1:3,ig)
          J => current%J(ig)
          taueff => current%taueff(ig)
          dplmod => current%dplmod(ig)
          deds => current%deds(ig)

          ! total deformation gradient
          DO id=1,3
             DO jd=1,3
                f(id,jd) = 0.0_r8k
                DO in=1,8
                   jn = current%node_numbers(in)
                   f(id,jd) = f(id,jd) + current%dNdx0(jd,in,ig)*u(id,jn)
                ENDDO
             ENDDO
             f(id,id) = f(id,id) + 1.0_r8k
          ENDDO

          ! Remove thermal dilation part from the deformation gradient
          DO id=1,3
             DO jd=1,3
                f(id,jd) = f(id,jd)*epsth
             ENDDO
          ENDDO

          ! trial elastic left Cauchy-Green tensor be
          IF ( epseff0 > 1.0e-10_r8k ) THEN
             be = matmul_ABAT(f,cpi0)
          ELSE
             be = matmul_AAT(f)
          ENDIF

          ! spectral decomposition
          CALL jacobi3(be,dl2,nv)

          ! stretches and logarithmic strains
          dl(1:3) = SQRT(dl2(1:3))
          en(1:3) = LOG(dl(1:3))

          ! deviatoric stresses
          J = dl(1)*dl(2)*dl(3)
          tmp = (en(1) + en(2) + en(3))/3.0_r8k
          DO id=1,3
             taudev(id) = 2.0_r8k*mu*(en(id) - tmp)
             sigdev(id) = taudev(id)/J
          ENDDO

          ! Effective Kirchhoff stress
          taueff = SQRT(1.5_r8k*(taudev(1)**2 + taudev(2)**2 + taudev(3)**2))
          sigeff = taueff/J

          ! Initialize plastic strains
          gamma = 0.0_r8k
          epseff = epseff0

          ! Creep calculation
          CALL mat_creep_calc(mat,gtemp,mu,dtime,epseff0,taueff,gamma,deds)

          IF ( lerror ) RETURN

          ! plastic calculation
          CALL mat_radial_Return(mat,gtemp,mu,dtime,epseff0,taueff,gamma, &
               dplmod,deds)

          IF ( lerror ) RETURN

          IF ( gamma > 0.0_r8k ) THEN
             ! Update effective stresses
             sigeff = (taueff - 3.0_r8k*mu*gamma)/J

             ! Update effective plastic strain
             epseff = epseff0 + gamma

             ! flow rule (Maximum plastic dissipation)
             DO id=1,3
                av(id) = 1.5_r8k*taudev(id)/taueff
             ENDDO

             ! Update deviatoric stress
             DO id=1,3
                en(id) = en(id) - gamma*av(id)
                taudev(id) = taudev(id) - 2.0_r8k*mu*gamma*av(id)
                sigdev(id) = taudev(id)/J
             ENDDO

             ! elastic left Cauchy Green strain tensor
             DO id=1,3
                dl(id) = dl(id)/EXP(gamma*av(id))
                dl(id)=dl(id)**2
             ENDDO
             DO id=1,3
                DO jd=1,3
                   be(id,jd) = 0.0_r8k
                   DO kd=1,3
                      be(id,jd) = be(id,jd) + dl(kd)*nv(id,kd)*nv(jd,kd)
                   ENDDO
                ENDDO
             ENDDO

             ! update nonelastic deformations
             cpi = matmul_AiBAit(f,be)

             ! Plastic strain energy
             delta_plastic_strain_energy = delta_plastic_strain_energy + &
                  gamma*sigeff*current%dV(ig)

             ! Update maximum plastic increment
             maxgamma = MAX(maxgamma,gamma)
          ELSE
             cpi(1:6) = cpi0(1:6)
          ENDIF

          ! Deviatoric part of elastic strain energy
          elastic_strain_energy = elastic_strain_energy + mu*current%dV0(ig)* &
               ((en(1) - tmp)**2 + (en(2) - tmp)**2 + (en(3) - tmp)**2)

          ! Cauchy stresses in cartesian CS
          CALL prin2cart(sigdev,nv,sigma)
          sigma(1:3) = sigma(1:3) + phyd
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_stress


    SUBROUTINE hex8_fint()
    ! Element stress calculation
    IMPLICIT NONE
    INTEGER(ipk) :: in,jn,ig,id,jd
    REAL(r8k) :: dxm0,dym0,dzm0,dxi,dyi,dzi,dxmi,dymi,dzmi,Bi(6,3)
    REAL(r8k), POINTER :: dV
    REAL(r8k), DIMENSION(:), POINTER :: sigma
    TYPE(hex8_type), POINTER :: current

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       DO in=1,8
          jn = current%node_numbers(in)

          ! Internal forces
          dxm0 = current%dNdxm(1,in)/3.0_r8k
          dym0 = current%dNdxm(2,in)/3.0_r8k
          dzm0 = current%dNdxm(3,in)/3.0_r8k
          DO ig=1,8
             dxi = current%dNdx(1,in,ig)
             dyi = current%dNdx(2,in,ig)
             dzi = current%dNdx(3,in,ig)
             dxmi = dxi/3.0_r8k
             dymi = dyi/3.0_r8k
             dzmi = dzi/3.0_r8k
             DO id=1,3
                Bi(id,1) = dxm0 - dxmi
                Bi(id,2) = dym0 - dymi
                Bi(id,3) = dzm0 - dzmi
             ENDDO
             Bi(1,1) = Bi(1,1) + dxi
             Bi(2,2) = Bi(2,2) + dyi
             Bi(3,3) = Bi(3,3) + dzi
             Bi(4,1) = dyi
             Bi(4,2) = dxi
             Bi(4,3) = 0.0_r8k
             Bi(5,1) = 0.0_r8k
             Bi(5,2) = dzi
             Bi(5,3) = dyi
             Bi(6,1) = dzi
             Bi(6,2) = 0.0_r8k
             Bi(6,3) = dxi
             sigma => current%sigma(1:6,ig)
             dV => current%dV(ig)
             DO id=1,3
                DO jd=1,6
                   Fint(id,jn) = Fint(id,jn) + Bi(jd,id)*sigma(jd)*dV
                ENDDO
             ENDDO
          ENDDO
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_fint


    SUBROUTINE hex8_stIff()
    ! Element stIffness matrices
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: ig,in,jn,id,jd,kd,ie,je,dofnum(24)
    REAL(r8k), POINTER :: J,mu,lambda,dV, &
         phyd,kappa,Jm,gamma,taueff,dplmod,deds
    REAL(r8k), DIMENSION(:), POINTER :: sigdev, dl2, sigma, av
    REAL(r8k), DIMENSION(:,:), POINTER :: nv, dNdx
    REAL(r8k) :: Ke(24,24),tmod(21),Bi(6,3,8),BiC(6,3),dxm0,dym0,dzm0, &
         dxi,dyi,dzi,dxmi,dymi,dzmi,Keij(3,3),tmp

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )

       Ke = 0.0_r8k
       phyd => current%phyd
       kappa => current%kappa
       Jm => current%Jm
       ! Deviatoric part of the tangent stIffness matrix
       DO ig=1,8
          ! Assign POINTERs
          J => current%J(ig)
          mu => current%mu(ig)
          lambda => current%lambda(ig)
          sigdev => current%sigdev(1:3,ig)
          nv => current%nv(1:3,1:3,ig)
          dl2 => current%dl2(1:3,ig)
          dV => current%dV(ig)
          sigma => current%sigma(1:6,ig)
          dNdx => current%dNdx(1:3,1:8,ig)
          av => current%av(1:3,ig)
          gamma => current%gamma(ig)
          taueff => current%taueff(ig)
          dplmod => current%dplmod(ig)
          deds => current%deds(ig)

          ! Kinematic matrix
          DO in=1,8
             dxm0 = current%dNdxm(1,in)/3.0_r8k
             dym0 = current%dNdxm(2,in)/3.0_r8k
             dzm0 = current%dNdxm(3,in)/3.0_r8k
             dxi = dNdx(1,in)
             dyi = dNdx(2,in)
             dzi = dNdx(3,in)
             dxmi = dxi/3.0_r8k
             dymi = dyi/3.0_r8k
             dzmi = dzi/3.0_r8k
             DO id=1,3
                Bi(id,1,in) = dxm0 - dxmi
                Bi(id,2,in) = dym0 - dymi
                Bi(id,3,in) = dzm0 - dzmi
             ENDDO
             Bi(1,1,in) = Bi(1,1,in) + dxi
             Bi(2,2,in) = Bi(2,2,in) + dyi
             Bi(3,3,in) = Bi(3,3,in) + dzi
             Bi(4,1,in) = dyi
             Bi(4,2,in) = dxi
             Bi(4,3,in) = 0.0_r8k
             Bi(5,1,in) = 0.0_r8k
             Bi(5,2,in) = dzi
             Bi(5,3,in) = dyi
             Bi(6,1,in) = dzi
             Bi(6,2,in) = 0.0_r8k
             Bi(6,3,in) = dxi
          ENDDO
          ! Tangent operator
          tmod = hex8_tmod(J,Jm,mu,lambda,kappa,sigdev,phyd,nv,dl2,gamma, &
               av,taueff,dplmod,deds)
          DO in=1,8
             BiC = hex8_tmod_matmul(Bi(1,1,in),tmod)
             DO jn=in,8
                ! Material part of the stIffness matrix
                DO id=1,3
                   DO jd=1,3
                      Keij(id,jd) = 0.0_r8k
                      DO kd=1,6
                         Keij(id,jd) = Keij(id,jd) + BiC(kd,id)*Bi(kd,jd,jn)
                      ENDDO
                   ENDDO
                ENDDO
                ! Geometric part of the stIffness matrix
                tmp = dNdx(1,in)*dNdx(1,jn)*sigma(1) + &
                     dNdx(2,in)*dNdx(2,jn)*sigma(2) + &
                     dNdx(3,in)*dNdx(3,jn)*sigma(3) + &
                     (dNdx(1,in)*dNdx(2,jn)+dNdx(1,jn)*dNdx(2,in))*sigma(4) + &
                     (dNdx(2,in)*dNdx(3,jn)+dNdx(2,jn)*dNdx(3,in))*sigma(5) + &
                     (dNdx(1,in)*dNdx(3,jn)+dNdx(1,jn)*dNdx(3,in))*sigma(6)
                DO id=1,3
                   Keij(id,id) = Keij(id,id) + tmp
                ENDDO
                ! Place submatrix Keij to element matrix se
                DO id=1,3
                   DO jd=1,3
                      ie = (in - 1)*3 + id
                      je = (jn - 1)*3 + jd
                      Ke(ie,je) = Ke(ie,je) + Keij(id,jd)*dV
                   ENDDO
                ENDDO
             ENDDO
          ENDDO
       ENDDO

       ! Mirror the symmetric matrix
       DO ie=1,24
          DO je=ie+1,24
             Ke(je,ie) = Ke(ie,je)
          ENDDO
       ENDDO

       ! DOF numbering
       dofnum(1:3) = dof_number(1:3,current%node_numbers(1))
       dofnum(4:6) = dof_number(1:3,current%node_numbers(2))
       dofnum(7:9) = dof_number(1:3,current%node_numbers(3))
       dofnum(10:12) = dof_number(1:3,current%node_numbers(4))
       dofnum(13:15) = dof_number(1:3,current%node_numbers(5))
       dofnum(16:18) = dof_number(1:3,current%node_numbers(6))
       dofnum(19:21) = dof_number(1:3,current%node_numbers(7))
       dofnum(22:24) = dof_number(1:3,current%node_numbers(8))

       ! Place element matrix in the global matrix
       CALL sparse_matrix_place_element(24,dofnum,Ke)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_stIff


  FUNCTION hex8_tmod_matmul(Bi,tmod) RESULT ( BiC )
    ! Multiply kinematic matrix with tangent modulus
    IMPLICIT NONE
    REAL(r8k) :: Bi(6,3),tmod(21),BiC(6,3)
    INTEGER(ipk) tmodi(6,6),id,jd,kd

    tmodi(1:6,1) = (/  1,  3,  6, 10, 15, 21 /)
    tmodi(1:6,2) = (/  3,  2,  5,  9, 14, 20 /)
    tmodi(1:6,3) = (/  6,  5,  4,  8, 13, 19 /)
    tmodi(1:6,4) = (/ 10,  9,  8,  7, 12, 18 /)
    tmodi(1:6,5) = (/ 15, 14, 13, 12, 11, 17 /) 
    tmodi(1:6,6) = (/ 21, 20, 19, 18, 17, 16 /)

    DO id=1,3
       DO jd=1,6
          BiC(jd,id) = 0.0_r8k
          DO kd=1,6
             BiC(jd,id) = BiC(jd,id) + Bi(kd,id)*tmod(tmodi(kd,jd))
          ENDDO
       ENDDO
    ENDDO

    RETURN

  END FUNCTION hex8_tmod_matmul


  FUNCTION hex8_tmod(J,Jm,mu,lambda,kappa,sigdev,phyd,nv,dl2,gamma,av, &
       taueff,dplmod,deds) RESULT(tmod)
    ! Deviatoric material tangent modulus
    REAL(r8k) :: J,Jm,mu,lambda,kappa,sigdev(3),phyd,nv(3,3),dl2(3), &
         gamma,av(3),taueff,dplmod,deds,tmod(21),tmp(6),tmp2(3,3), &
         t2,t4,t7,t9,t12,t15,t16,t21,t22,t26,t27,t40,t44,t48,t57
    REAL(r8k) :: aik(3,3),bik
    INTEGER(ipk) :: i,k

    tmod = 0.0_r8k

    ! Deviatoric part
    aik = -2.0_r8k*mu/3.0_r8k/J
    DO i=1,3
       aik(i,i) = aik(i,i) + 2.0_r8k*mu/J
    ENDDO

    ! Creep part of the tangent modulus
    IF ( gamma > 1.0e-10_r8k ) THEN
       IF ( deds > 0.0_r8k ) THEN
          DO i=1,3
             DO k=1,3
                tmp2(i,k) = aik(i,k) - aik(i,k)*(3.0_r8k*mu*gamma/taueff) &
                     + 4.0_r8k*mu**2*av(i)*av(k)*gamma/taueff/J
             ENDDO
          ENDDO
          tmp(1) = 1.0_r8k + 2.0_r8k*mu*deds*av(1)*av(1)
          tmp(2) = 1.0_r8k + 2.0_r8k*mu*deds*av(2)*av(2)
          tmp(3) = 2.0_r8k*mu*deds*av(1)*av(2)
          tmp(4) = 1.0_r8k + 2.0_r8k*mu*deds*av(3)*av(3)
          tmp(5) = 2.0_r8k*mu*deds*av(2)*av(3)
          tmp(6) = 2.0_r8k*mu*deds*av(1)*av(3)
          t2 = tmp(5)**2
          t4 = tmp(1)*tmp(2)
          t7 = tmp(3)**2
          t9 = tmp(3)*tmp(6)
          t12 = tmp(6)**2
          t15 = 1/(t4*tmp(4)-tmp(1)*t2-t7*tmp(4)+2*t9*tmp(5)-t12*tmp(2))
          t16 = (tmp(2)*tmp(4)-t2)*t15
          t21 = (tmp(3)*tmp(4)-tmp(6)*tmp(5))*t15
          t22 = t21*tmp2(1,2)
          t26 = (-tmp(3)*tmp(5)+tmp(6)*tmp(2))*t15
          t27 = t26*tmp2(1,3)
          t40 = (tmp(1)*tmp(4)-t12)*t15
          t44 = (tmp(1)*tmp(5)-t9)*t15
          t48 = t44*tmp2(2,3)
          t57 = (t4-t7)*t15
          aik(1,1) = t16*tmp2(1,1)-t22-t27
          aik(1,2) = t16*tmp2(1,2)-t21*tmp2(2,2)-t26*tmp2(2,3)
          aik(1,3) = t16*tmp2(1,3)-t21*tmp2(2,3)-t26*tmp2(3,3)
          aik(2,1) = -t21*tmp2(1,1)+t40*tmp2(1,2)-t44*tmp2(1,3)
          aik(2,2) = -t22+t40*tmp2(2,2)-t48
          aik(2,3) = -t21*tmp2(1,3)+t40*tmp2(2,3)-t44*tmp2(3,3)
          aik(3,1) = -t26*tmp2(1,1)-t44*tmp2(1,2)+t57*tmp2(1,3)
          aik(3,2) = -t26*tmp2(1,2)-t44*tmp2(2,2)+t57*tmp2(2,3)
          aik(3,3) = -t27-t48+t57*tmp2(3,3)
       ELSE
          ! Plastic part of the tangent modulus
          DO i=1,3
             DO k=1,3
                aik(i,k) = aik(i,k) - aik(i,k)*(3.0_r8k*mu*gamma/taueff) &
                     - 2.0_r8k*mu*av(i)*av(k)*(2.0_r8k*mu/dplmod &
                     - 2.0_r8k*mu*gamma/taueff)/J
             ENDDO
          ENDDO
       ENDIF
    ENDIF

    ! 
    DO i=1,3
       DO k=1,3
          bik = aik(i,k)
          IF ( i == k ) bik = aik(i,k) - 2.0_r8k*sigdev(i)
          tmod(1) = tmod(1) + bik*nv(1,i)*nv(1,i)*nv(1,k)*nv(1,k)
          tmod(2) = tmod(2) + bik*nv(2,i)*nv(2,i)*nv(2,k)*nv(2,k)
          tmod(3) = tmod(3) + bik*nv(1,i)*nv(1,i)*nv(2,k)*nv(2,k)
          tmod(4) = tmod(4) + bik*nv(3,i)*nv(3,i)*nv(3,k)*nv(3,k)
          tmod(5) = tmod(5) + bik*nv(2,i)*nv(2,i)*nv(3,k)*nv(3,k)
          tmod(6) = tmod(6) + bik*nv(1,i)*nv(1,i)*nv(3,k)*nv(3,k)
          tmod(7) = tmod(7) + 2.0*bik*nv(1,i)*nv(2,i)*nv(1,k)*nv(2,k)
          tmod(8) = tmod(8) + 2.0*bik*nv(3,i)*nv(3,i)*nv(1,k)*nv(2,k)
          tmod(9) = tmod(9) + 2.0*bik*nv(2,i)*nv(2,i)*nv(1,k)*nv(2,k)
          tmod(10) = tmod(10) + 2.0_r8k*bik*nv(1,i)*nv(1,i)*nv(1,k)*nv(2,k)
          tmod(11) = tmod(11) + 2.0_r8k*bik*nv(2,i)*nv(3,i)*nv(2,k)*nv(3,k)
          tmod(12) = tmod(12) + 2.0_r8k*bik*nv(1,i)*nv(2,i)*nv(2,k)*nv(3,k)
          tmod(13) = tmod(13) + 2.0_r8k*bik*nv(3,i)*nv(3,i)*nv(2,k)*nv(3,k)
          tmod(14) = tmod(14) + 2.0_r8k*bik*nv(2,i)*nv(2,i)*nv(2,k)*nv(3,k)
          tmod(15) = tmod(15) + 2.0_r8k*bik*nv(1,i)*nv(1,i)*nv(2,k)*nv(3,k)
          tmod(16) = tmod(16) + 2.0_r8k*bik*nv(1,i)*nv(3,i)*nv(1,k)*nv(3,k)
          tmod(17) = tmod(17) + 2.0_r8k*bik*nv(1,i)*nv(3,i)*nv(2,k)*nv(3,k)
          tmod(18) = tmod(18) + 2.0_r8k*bik*nv(1,i)*nv(2,i)*nv(1,k)*nv(3,k)
          tmod(19) = tmod(19) + 2.0_r8k*bik*nv(3,i)*nv(3,i)*nv(1,k)*nv(3,k)
          tmod(20) = tmod(20) + 2.0_r8k*bik*nv(2,i)*nv(2,i)*nv(1,k)*nv(3,k)
          tmod(21) = tmod(21) + 2.0_r8k*bik*nv(1,i)*nv(1,i)*nv(1,k)*nv(3,k)
          IF ( i /= k ) THEN
             IF ( ABS(dl2(i) - dl2(k)) < 1.0e-10_r8k ) THEN
                bik = 2.0_r8k*(mu/J - sigdev(i))
             ELSE
                bik = 2.0_r8k*(sigdev(i)*dl2(k) - sigdev(k)*dl2(i)) / &
                     (dl2(i) - dl2(k))
             ENDIF
             tmod(1) = tmod(1) + bik*nv(1,i)*nv(1,k)*nv(1,i)*nv(1,k)
             tmod(2) = tmod(2) + bik*nv(2,i)*nv(2,k)*nv(2,i)*nv(2,k)
             tmod(3) = tmod(3) + bik*nv(1,i)*nv(1,k)*nv(2,i)*nv(2,k)
             tmod(4) = tmod(4) + bik*nv(3,i)*nv(3,k)*nv(3,i)*nv(3,k)
             tmod(5) = tmod(5) + bik*nv(2,i)*nv(2,k)*nv(3,i)*nv(3,k)
             tmod(6) = tmod(6) + bik*nv(1,i)*nv(1,k)*nv(3,i)*nv(3,k)
             tmod(7) = tmod(7) + bik*nv(1,i)*nv(2,k)*nv(1,i)*nv(2,k)
             tmod(7) = tmod(7) + bik*nv(1,i)*nv(2,k)*nv(2,i)*nv(1,k)
             tmod(8) = tmod(8) + bik*nv(3,i)*nv(3,k)*nv(1,i)*nv(2,k)
             tmod(8) = tmod(8) + bik*nv(3,i)*nv(3,k)*nv(2,i)*nv(1,k)
             tmod(9) = tmod(9) + bik*nv(2,i)*nv(2,k)*nv(1,i)*nv(2,k)
             tmod(9) = tmod(9) + bik*nv(2,i)*nv(2,k)*nv(2,i)*nv(1,k)
             tmod(10) = tmod(10) + bik*nv(1,i)*nv(1,k)*nv(1,i)*nv(2,k)
             tmod(10) = tmod(10) + bik*nv(1,i)*nv(1,k)*nv(2,i)*nv(1,k)
             tmod(11) = tmod(11) + bik*nv(2,i)*nv(3,k)*nv(2,i)*nv(3,k)
             tmod(11) = tmod(11) + bik*nv(2,i)*nv(3,k)*nv(3,i)*nv(2,k)
             tmod(12) = tmod(12) + bik*nv(1,i)*nv(2,k)*nv(2,i)*nv(3,k)
             tmod(12) = tmod(12) + bik*nv(1,i)*nv(2,k)*nv(3,i)*nv(2,k)
             tmod(13) = tmod(13) + bik*nv(3,i)*nv(3,k)*nv(2,i)*nv(3,k)
             tmod(13) = tmod(13) + bik*nv(3,i)*nv(3,k)*nv(3,i)*nv(2,k)
             tmod(14) = tmod(14) + bik*nv(2,i)*nv(2,k)*nv(2,i)*nv(3,k)
             tmod(14) = tmod(14) + bik*nv(2,i)*nv(2,k)*nv(3,i)*nv(2,k)
             tmod(15) = tmod(15) + bik*nv(1,i)*nv(1,k)*nv(2,i)*nv(3,k)
             tmod(15) = tmod(15) + bik*nv(1,i)*nv(1,k)*nv(3,i)*nv(2,k)
             tmod(16) = tmod(16) + bik*nv(1,i)*nv(3,k)*nv(1,i)*nv(3,k)
             tmod(16) = tmod(16) + bik*nv(1,i)*nv(3,k)*nv(3,i)*nv(1,k)
             tmod(17) = tmod(17) + bik*nv(1,i)*nv(3,k)*nv(2,i)*nv(3,k)
             tmod(17) = tmod(17) + bik*nv(1,i)*nv(3,k)*nv(3,i)*nv(2,k)
             tmod(18) = tmod(18) + bik*nv(1,i)*nv(2,k)*nv(1,i)*nv(3,k)
             tmod(18) = tmod(18) + bik*nv(1,i)*nv(2,k)*nv(3,i)*nv(1,k)
             tmod(19) = tmod(19) + bik*nv(3,i)*nv(3,k)*nv(1,i)*nv(3,k)
             tmod(19) = tmod(19) + bik*nv(3,i)*nv(3,k)*nv(3,i)*nv(1,k)
             tmod(20) = tmod(20) + bik*nv(2,i)*nv(2,k)*nv(1,i)*nv(3,k)
             tmod(20) = tmod(20) + bik*nv(2,i)*nv(2,k)*nv(3,i)*nv(1,k)
             tmod(21) = tmod(21) + bik*nv(1,i)*nv(1,k)*nv(1,i)*nv(3,k)
             tmod(21) = tmod(21) + bik*nv(1,i)*nv(1,k)*nv(3,i)*nv(1,k)
          ENDIF
       ENDDO
    ENDDO
    DO i=7,21
       tmod(i)=0.5_r8k*tmod(i)
    ENDDO


    ! Hydrostatic part
    bik = kappa*(1.0_r8k - LOG(Jm))/Jm + phyd
    tmod(1) = tmod(1) + bik - 2.0_r8k*phyd
    tmod(2) = tmod(2) + bik - 2.0_r8k*phyd
    tmod(4) = tmod(4) + bik - 2.0_r8k*phyd
    tmod(3) = tmod(3) + bik
    tmod(5) = tmod(5) + bik
    tmod(6) = tmod(6) + bik
    tmod(7) = tmod(7) - phyd
    tmod(11) = tmod(11) - phyd
    tmod(16) = tmod(16) - phyd

    RETURN

  END FUNCTION hex8_tmod


    SUBROUTINE hex8_update()
    ! Update explicit values
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: ig

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       DO ig=1,8
          current%plSED(ig) = current%plSED(ig) + 0.5_r8k*current%gamma(ig)* &
               (current%sigeff0(ig) + current%sigeff(ig))
       ENDDO
       current%epseff0 = current%epseff
       current%sigeff0 = current%sigeff
       current%cpi0 = current%cpi
       CALL mat_maximums(current%mat,current%epseff0,current%sigeff0, &
            current%plSED,current%sigma)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_update


    SUBROUTINE hex8_strains()
    ! Calculate logarithmic strain values
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current
    INTEGER(ipk) :: ig,id,jd,in,jn
    REAL(r8k) :: f(3,3),b(3,3),dl2(3),nv(3,3),dl(3),en(3)
    REAL(r8k), DIMENSION(:), POINTER :: epstot

    ! Stress calculation loop
    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       DO ig=1,8
          ! Assign POINTERs
          epstot => current%epstot(1:6,ig)

          ! total deformation gradient
          DO id=1,3
             DO jd=1,3
                f(id,jd) = 0.0_r8k
                DO in=1,8
                   jn = current%node_numbers(in)
                   f(id,jd) = f(id,jd) + current%dNdx0(jd,in,ig)*u(id,jn)
                ENDDO
             ENDDO
             f(id,id) = f(id,id) + 1.0_r8k
          ENDDO

          ! left Cauchy Green strain tensor
          b = matmul_AAT(f)

          ! spectral decomposition
          CALL jacobi3(b,dl2,nv)

          ! stretches and logarithmic strains
          dl(1:3) = SQRT(dl2(1:3))
          en(1:3) = LOG(dl(1:3))

          ! Total logarithmic strain tensor
          CALL prin2cart(en,nv,epstot)
          epstot(4:6) = 2.0_r8k*epstot(4:6)

          ! Thermal dilation
          current%epsth(ig) = LOG(1.0_r8k/current%epsth(ig))
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_strains


    SUBROUTINE hex8_Write_output(nunit)
    ! WRITE output to a file in unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(hex8_type), POINTER :: current

    WRITE(UNIT=nunit) nhex8

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%egroup
       WRITE(UNIT=nunit) current%mat
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       WRITE(UNIT=nunit) current%cpi0
       WRITE(UNIT=nunit) current%epseff0
       WRITE(UNIT=nunit) current%sigeff
       WRITE(UNIT=nunit) current%sigma
       WRITE(UNIT=nunit) current%phyd
       WRITE(UNIT=nunit) current%epstot
       WRITE(UNIT=nunit) current%epsth
       WRITE(UNIT=nunit) current%plSED
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE hex8_Write_output


    SUBROUTINE hex8_read_output(nunit)
    ! Read output from unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(hex8_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) nhex8

    current => first_hex8
    DO i=1,nhex8
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_hex8) ) THEN
             first_hex8 => current
             last_hex8 => current
          ELSE
             last_hex8%next => current
             last_hex8 => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%egroup
       Read(UNIT=nunit,ERR=20,End=20) current%mat
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels(1:8)
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers(1:8)
       Read(UNIT=nunit,ERR=20,End=20) current%cpi0
       Read(UNIT=nunit,ERR=20,End=20) current%epseff0
       Read(UNIT=nunit,ERR=20,End=20) current%sigeff
       Read(UNIT=nunit,ERR=20,End=20) current%sigma
       Read(UNIT=nunit,ERR=20,End=20) current%phyd
       Read(UNIT=nunit,ERR=20,End=20) current%epstot
       Read(UNIT=nunit,ERR=20,End=20) current%epsth
       Read(UNIT=nunit,ERR=20,End=20) current%plSED
       last_hex8 => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_hex8) ) last_hex8%next => NULL()
    IF ( nhex8 == 0 ) THEN
       first_hex8 => NULL()
       last_hex8 => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading HEX8 Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE hex8_read_output


    SUBROUTINE hex8_deallocate()
    ! Deallocate HEX8 Database
    IMPLICIT NONE
    TYPE(hex8_type), POINTER :: current,previous

    current => first_hex8
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE hex8_deallocate
END MODULE hex8













