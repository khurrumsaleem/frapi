MODULE quad4_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE math_frapcon
    USE sparse_matrix_frapcon
    USE materials_frapcon
    USE variables_frapcon, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Axisymmetric mean dilation Q4 element
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
    ! Arrays for principal values
    !   Index Coordinate direction
    !     1     in rz-plane
    !     2     in rz-plane
    !     3     hoop direction
    !
    ! Vector presentation for stresses
    !   Index Stress component
    !     1     radial
    !     2     axial
    !     3     hoop
    !     4     shear rz
    !
    INTEGER(ipk) :: nquad4 ! Number of Q4 elements
    REAL(r8k), PRIVATE :: xi(2,4) ! GP coordinate values in element CS
    REAL(r8k), PRIVATE :: N(4,4) ! Shape Function values at GPs
    REAL(r8k), PRIVATE :: dNdxi(2,4,4) ! Shape Function derivatives in
                                            ! element CS
    REAL(r8k), PRIVATE :: wgt(4) ! GP integration weight factors


    TYPE quad4_type
        INTEGER(ipk) :: label ! Element label
        INTEGER(ipk) :: number ! Internal element number
        INTEGER(ipk) :: mat ! Material flag
        INTEGER(ipk) :: egroup ! Element group label
        INTEGER(ipk) :: node_labels(4) ! Node labels
        INTEGER(ipk) :: node_numbers(4) ! Internal node numbers
        REAL(r8k) :: gtemp(4) ! Integration point temperatures
        REAL(r8k) :: mu(4) ! Temperature dependent material PARAMETER
        REAL(r8k) :: lambda(4) ! Temperature dependent material PARAMETER
        REAL(r8k) :: kappa ! Temperature dependent material PARAMETER
        REAL(r8k) :: epsth(4) ! Thermal dilation
        REAL(r8k) :: epsth0(4) ! Explicit value for thermal dilation
        REAL(r8k) :: epsth_end(4) ! Thermal dilation at the End of load step
        REAL(r8k) :: sigma(4,4) ! Deviatoric Cauchy stresses
        REAL(r8k) :: phyd ! Hydrostatic pressure
        REAL(r8k) :: x(2,4) ! Current coordinate values of GP
        REAL(r8k) :: x0(2,4) ! Initial coordinate values of GP
        REAL(r8k) :: dNdx(2,4,4) ! Derivatives of the shape functions
        REAL(r8k) :: dNdx0(2,4,4) ! Initial derivatives of the shape
                                    ! functions
        REAL(r8k) :: dNdxm(2,4) ! Mean derivatives of the shape functions
        REAL(r8k) :: dV(4) ! Weight factor of GP
        REAL(r8k) :: dV0(4) ! Initial weight factor of GP
        REAL(r8k) :: V ! Element volume
        REAL(r8k) :: V0 ! Initial element volume
        REAL(r8k) :: dl2(3,4) ! Squares of trial principal stretches
        REAL(r8k) :: nv(2,2,4) ! Principal directions
        REAL(r8k) :: nv0(2,4) ! Explicit principal direction
        REAL(r8k) :: cpi(4,4) ! Inverse of plastic Green-Lagrange tensor
        REAL(r8k) :: cpi0(4,4) ! Explicit inverse of plastic Green-Lagrange
                                    ! strain tensor
        REAL(r8k) :: J(4) ! Dilation
        REAL(r8k) :: Jm ! Mean dilation
        REAL(r8k) :: gamma(4) ! Plastic multiplier
        REAL(r8k) :: av(3,4) ! Plastic flow direction
        REAL(r8k) :: epseff(4) ! Effective plastic strains
        REAL(r8k) :: epseff0(4) ! Explicit effective plastic strains
        REAL(r8k) :: sigdev(3,4) ! Principal deviatoric Cauchy stresses
        REAL(r8k) :: sigeff(4) ! Effective stress
        REAL(r8k) :: sigeff0(4) ! Explicit effective stress
        REAL(r8k) :: plSED(4) ! Plastic SED
        REAL(r8k) :: taueff(4) ! Effective trial Kirchoff stress
        REAL(r8k) :: dplmod(4) ! Plastic modulus
        REAL(r8k) :: deds(4) ! Creep modulus
        REAL(r8k) :: epstot(4,4) ! Total strains
        TYPE(quad4_type), POINTER :: next ! Next element
    END TYPE quad4_type
  
    TYPE(quad4_type), POINTER :: &
        first_quad4,last_quad4 ! Pointers to the element database

    CONTAINS
    !
    SUBROUTINE quad4_init()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pointers and tables for Q4 element calculation
    !
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate possible existing database
    CALL quad4_deallocate()

    ! Initialize element number and pointers
    first_quad4 => NULL()
    last_quad4 => NULL()
    nquad4 = 0

    ! GP element coordinates and weight factors
    wgt(1:4) = 1.0_r8k
    tmp = 1.0_r8k / SQRT(3.0_r8k)
    xi(1:2,1) = (/ -tmp, -tmp /)
    xi(1:2,2) = (/  tmp, -tmp /)
    xi(1:2,3) = (/  tmp,  tmp /)
    xi(1:2,4) = (/ -tmp,  tmp /)

    DO ig = 1, 4
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
    END DO

    END SUBROUTINE quad4_init
    !
    !
    !
    SUBROUTINE quad4_create (label, mat, nodes)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new QUAD4 element and attach it to the element database
    !
    INTEGER(ipk), INTENT(IN) :: label,mat,nodes(4)
    INTEGER(ipk) :: id,ig,in,jn,nfound
    REAL(r8k) :: tmp(2,2),tmp2
    TYPE(quad4_type), POINTER :: new_quad4,current

    ! ALLOCATE new element
    nquad4 = nquad4 + 1
    ALLOCATE(new_quad4)
    CALL mat_create(mat)

    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 4
            IF (nodes(jn) == node_labels(in)) THEN
                new_quad4%node_labels(jn) = nodes(jn)
                new_quad4%node_numbers(jn) = in
                enumber(in) = enumber(in) + 1
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 4) EXIT
    END DO

    ! Check whether all element nodes were found in node database
    IF (nfound /= 4) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR incompatible NODES and QUAD4 data in element ',label
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Check that the element label doesn't already exist
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) THEN
            WRITE (ounit,FMT='(/,A,I0,A,/)') 'ERROR QUAD4 ',label,' has been defined twice'
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF
        current => current%next
    END DO

    ! Order element nodes properly
    DO id = 1, 2
        tmp(id,1) = x0(id,new_quad4%node_numbers(3)) - x0(id,new_quad4%node_numbers(1))
        tmp(id,2) = x0(id,new_quad4%node_numbers(2)) - x0(id,new_quad4%node_numbers(1))
    END DO
    tmp2 = tmp(1,1) * tmp(2,2) - tmp(1,2) * tmp(2,1)
    IF (tmp2 > 0.0) THEN
        new_quad4%node_labels(1:4) = new_quad4%node_labels(4:1:-1)
        new_quad4%node_numbers(1:4) = new_quad4%node_numbers(4:1:-1)
    END IF

    ! Initialize QUAD4 variables
    new_quad4%label = label
    new_quad4%number = nquad4
    new_quad4%mat = mat
    new_quad4%egroup = egroup
    DO ig = 1, 4
        new_quad4%cpi0(1:4,ig) = (/1.0_r8k,1.0_r8k,0.0_r8k,1.0_r8k/)       
        new_quad4%nv0(1:2,ig) = (/1.0_r8k, 0.0_r8k/)
        new_quad4%epsth0(ig) = 0.0_r8k
        new_quad4%epsth_end(ig) = 0.0_r8k
    END DO
    new_quad4%epseff0 = 0.0_r8k
    new_quad4%sigeff0 = 0.0_r8k
    new_quad4%sigma = 0.0_r8k
    new_quad4%phyd = 0.0_r8k
    new_quad4%epstot = 0.0_r8k
    new_quad4%epsth = 0.0_r8k
    new_quad4%plSED = 0.0_r8k
    new_quad4%next => NULL()

    ! Initial derivatives
    CALL quad4_deriv(x0(1,new_quad4%node_numbers(1)), &
            x0(1,new_quad4%node_numbers(2)),x0(1,new_quad4%node_numbers(3)), &
            x0(1,new_quad4%node_numbers(4)),new_quad4%V0, &
            new_quad4%dV0,new_quad4%dNdx0,new_quad4%dNdxm,new_quad4%x0)

    IF (lerror) THEN
        WRITE (ounit,FMT='(A,I0)') 'ERROR poorly shaped QUAD4 element ',label
        CALL nlfemp_stop(0)
    END IF

    ! Set fixed status for the third DOF of nodes
    IF (nd > 2) THEN
        DO in = 1, 4
            jn = new_quad4%node_labels(in)
            CALL set_node_status(jn,3,-1)
        END DO
    END IF

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add new element to the database
    IF (.NOT. ASSOCIATED(first_quad4)) THEN
        first_quad4 => new_quad4
        last_quad4 => new_quad4
    ELSE
        last_quad4%next => new_quad4
        last_quad4 => new_quad4
    END IF

    END SUBROUTINE quad4_create
    !
    !
    !
    SUBROUTINE quad4_delete (label)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Delete element from the database
    !
    INTEGER(ipk), INTENT(IN) :: label
    INTEGER(ipk) :: in, jn
    TYPE(quad4_type), POINTER :: current, previous, tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the database is empty
    IF (.NOT. ASSOCIATED(first_quad4)) RETURN

    ! Mark first memeber to be deleted in database
    IF (first_quad4%label == label) THEN
        lfound = .TRUE.
        tobedeleted => first_quad4
        first_quad4 => first_quad4%next
    END IF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        IF (lfound) THEN
            current%number = current%number - 1
        ELSE IF (current%label == label) THEN
            lfound = .TRUE.
            previous%next => current%next
            tobedeleted => current
        END IF
        previous => current
        current => current%next
    END DO

    ! Set the pointer to the last QUAD4 element
    last_quad4 => NULL()
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        last_quad4 => current
        current => current%next
    END DO
    
    ! Deallocate QUAD4 element
    IF (lfound) THEN
        nquad4 = nquad4 - 1
        dof_numbering = .TRUE.
        DO in = 1, 4
            jn = tobedeleted%node_numbers(in)
            enumber(jn) = enumber(jn) - 1
        END DO
        DEALLOCATE(tobedeleted)
    END IF

    END SUBROUTINE quad4_delete
    !
    !
    !
    SUBROUTINE quad4_sparse_matrix()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for quad4 elements
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: dofnum(8)

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
        dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
        dofnum(5:6) = dof_number(1:2,current%node_numbers(3))
        dofnum(7:8) = dof_number(1:2,current%node_numbers(4))
        CALL sparse_matrix_add_element(8,dofnum)
        current => current%next
    END DO

    END SUBROUTINE quad4_sparse_matrix
    !
    !
    !
    SUBROUTINE quad4_temp()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize temperature dependent properties
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: im, ig, in
    REAL(r8k) :: emod, nu, lambda, mu, gtemp, epsth, epsth_ref

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        im = current%mat
        epsth_ref = mat_par(im,tref,'THDIL   ')
        current%kappa = 0.0_r8k
        DO ig = 1, 4
            ! Integration point coordinates
            cp = (/ current%x0(1:2,ig), 0.0_r8k /)

            ! GP temperature
            gtemp = 0.0_r8k
            DO in = 1, 4
                gtemp = gtemp + N(in,ig) * temp(current%node_numbers(in))
            END DO
            current%gtemp(ig) = gtemp

            ! Temperature dependent elastic material properties
            emod = mat_par(im, gtemp, 'EMOD    ')
            nu = mat_par(im, gtemp, 'PRATIO  ')
            lambda = emod * nu / (1.0_r8k - 2.0_r8k * nu) / (1.0_r8k + nu)
            mu = 0.5_r8k * emod / (1.0_r8k + nu)
            current%lambda(ig) = lambda
            current%mu(ig) = mu
            current%kappa = current%kappa + (lambda + 2.0_r8k * mu / 3.0_r8k) / 4.0_r8k

            ! Thermal dilation
            IF (mat_volstr(im)) THEN
                current%epsth(ig) = current%epsth0(ig) + dload * (current%epsth_end(ig) - current%epsth0(ig))
            ELSE
                epsth = mat_par(im, gtemp, 'THDIL   ')
                current%epsth(ig) = epsth - epsth_ref
            END IF
        END DO

        current => current%next
    END DO

    END SUBROUTINE quad4_temp
    !
    !
    !
    SUBROUTINE quad4_grav()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Gravity load
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: im, ig
    REAL(r8k) :: rho
    INTEGER(ipk), POINTER :: nodes(:)

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        im = current%mat
        rho = mat_par(im,tref,'DENSITY ')
        nodes => current%node_numbers(1:4)
        DO ig = 1, 4
            Fgrav(2,nodes(1)) = Fgrav(2,nodes(1)) + N(1,ig) * rho * current%dV0(ig) * grav(2)
            Fgrav(2,nodes(2)) = Fgrav(2,nodes(2)) + N(2,ig) * rho * current%dV0(ig) * grav(2)
            Fgrav(2,nodes(3)) = Fgrav(2,nodes(3)) + N(3,ig) * rho * current%dV0(ig) * grav(2)
            Fgrav(2,nodes(4)) = Fgrav(2,nodes(4)) + N(4,ig) * rho * current%dV0(ig) * grav(2)
        END DO
        current => current%next
    END DO

    END SUBROUTINE quad4_grav
    !
    !
    !
    SUBROUTINE quad4_deriv(x1, x2, x3, x4, V, dV, dNdx, dNdxm, x)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Evaluate cartesian derivatives of the shape functions
    !
    REAL(r8k), INTENT(IN) :: x1(2),x2(2),x3(2),x4(2)
    REAL(r8k), INTENT(OUT) :: V,dV(4),dNdx(2,4,4),dNdxm(2,4),x(2,4)
    INTEGER(ipk) :: ig,id,jd,in
    REAL(r8k) :: xn(2,4),jaci(2,2),jac(2,2),detj,tmp2

    xn(1:2,1) = x1(1:2)
    xn(1:2,2) = x2(1:2)
    xn(1:2,3) = x3(1:2)
    xn(1:2,4) = x4(1:2)

    V = 0.0_r8k
    dNdxm = 0.0_r8k

    DO ig = 1, 4
        ! Jacobian
        DO id = 1, 2
            DO jd = 1, 2
                jac(id,jd) = 0.0_r8k
                DO in = 1, 4
                    jac(id,jd) = jac(id,jd) + dNdxi(id,in,ig) * xn(jd,in)
                END DO
            END DO
        END DO

        ! Inverse of jacobian
        detj = jac(1,1) * jac(2,2) - jac(1,2) * jac(2,1)
        tmp2 = 1.0_r8k / detj
        jaci(1,1) =  jac(2,2) * tmp2
        jaci(2,2) =  jac(1,1) * tmp2
        jaci(1,2) = -jac(1,2) * tmp2
        jaci(2,1) = -jac(2,1) * tmp2

        ! Gauss point coordinates
        x(1:2,ig) = 0.0_r8k
        DO in = 1, 4
            x(1:2,ig) = x(1:2,ig) + N(in,ig)*xn(1:2,in)
        END DO

        ! Derivatives of the shape functions in cartesian coordinate system
        DO in = 1, 4
            DO id = 1, 2
                dNdx(id,in,ig) = 0.0_r8k
                DO jd = 1,2
                    dNdx(id,in,ig) = dNdx(id,in,ig) + jaci(id,jd) * dNdxi(jd,in,ig)
                END DO
            END DO
        END DO

        ! Integration point weight factor
        dV(ig) = 2.0_r8k * pi * x(1,ig) * wgt(ig) * detj

        ! Element volume
        V = V + dV(ig)

        ! Mean derivatives
        DO in = 1, 4
            dNdxm(1,in) = dNdxm(1,in) + (N(in,ig) / x(1,ig) + dNdx(1,in,ig)) * dV(ig)
            dNdxm(2,in) = dNdxm(2,in) + dNdx(2,in,ig) * dV(ig)
        END DO

        ! Check for poorly shaped elements
        IF (dV(ig) <= 1.0e-15_r8k) THEN
            lerror = .TRUE.
            RETURN
        END IF
    END DO

    ! Scale mean derivatives with element volume
    dNdxm(1:2,1:4) = dNdxm(1:2,1:4)/V

    END SUBROUTINE quad4_deriv
    !
    !
    !
    SUBROUTINE quad4_stress()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Element stress calculation
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: ig,id,jd,kd,in,mat
    REAL(r8k) :: Vm, lnJm, f(2, 2), fh, tmp1, tmp2, be(2, 2), beh, dl(3), en(3), taudev(3), epsth2(4)
    REAL(r8k),  POINTER :: dl2(:), nv(:, :), cpi(:), cpi0(:), gamma,  sigdev(:), epseff, sigma(:), sigeff, lambda, &
                           mu, gtemp, av(:), epseff0, J, phyd, kappa, Jm, taueff, dplmod, epstot(:), deds

    ! Stress calculation loop
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        mat = current%mat

        ! Derivatives
        CALL quad4_deriv(x(1,current%node_numbers(1)), &
            x(1,current%node_numbers(2)),x(1,current%node_numbers(3)), &
            x(1,current%node_numbers(4)),current%V,current%dV,current%dNdx, &
            current%dNdxm,current%x)

        ! Exit If there are badly shaped elements
        IF (lerror) RETURN

        ! Assign pointers
        phyd => current%phyd
        kappa => current%kappa
        Jm => current%Jm

        ! Mechanical volume of the element
        Vm = 0.0_r8k
        DO ig = 1, 4
            epsth2(ig) = 1.0_r8k / EXP(current%epsth(ig))
            Vm = Vm + current%dV(ig) * (epsth2(ig)) ** 3
        END DO

        ! Mean dilation
        Jm = Vm / current%V0

        ! Evaluate mean hydrostatic pressure
        lnJm = LOG(Jm)
        phyd = kappa * lnJm / Jm

        ! Pressure part of the element strain energy contribution
        elastic_strain_energy = elastic_strain_energy + kappa * lnJm ** 2 * current%V0

        ! Deviatoric part
        DO ig = 1, 4
            ! Integration point coordinates
            cp = (/ current%x0(1:2,ig), 0.0_r8k /)

            ! Assign pointers
            dl2 => current%dl2(1:3,ig)
            nv => current%nv(1:2,1:2,ig)
            cpi => current%cpi(1:4,ig)
            cpi0 => current%cpi0(1:4,ig)
            gamma => current%gamma(ig)
            sigdev => current%sigdev(1:3,ig)
            epseff => current%epseff(ig)
            epseff0 => current%epseff0(ig)
            sigma => current%sigma(1:4,ig)
            sigeff => current%sigeff(ig)
            lambda => current%lambda(ig)
            mu => current%mu(ig)
            gtemp => current%gtemp(ig)
            av => current%av(1:3,ig)
            J => current%J(ig)
            taueff => current%taueff(ig)
            dplmod => current%dplmod(ig)
            deds => current%deds(ig)
            epstot => current%epstot(1:4,ig)

            ! total deformation gradient
            DO id = 1, 2
                DO jd = 1, 2
                    f(id,jd) = 0.0_r8k
                    DO in = 1, 4
                        f(id,jd) = f(id,jd) + current%dNdx0(jd,in,ig) * u(id,current%node_numbers(in))
                    END DO
                END DO
                f(id,id) = f(id,id) + 1.0_r8k
            END DO
            fh = current%x(1,ig) / current%x0(1,ig)

            ! Store deformation gradient for later Use
            epstot(1:2) = f(1:2,1)
            epstot(3:4) = f(1:2,2)

            ! Remove thermal dilation part from the deformation gradient
            DO id = 1, 2
                DO jd = 1, 2
                    f(id,jd) = f(id,jd) * epsth2(ig)
                END DO
            END DO
            fh = fh * epsth2(ig)

            ! trial elastic left Cauchy-Green tensor be
            IF (epseff0 > 1.0e-10_r8k) THEN
                be(1,1) = (f(1,1)*cpi0(1) + f(1,2)*cpi0(3))*f(1,1) + &
                    (f(1,1)*cpi0(3) + f(1,2)*cpi0(2))*f(1,2)
                be(1,2) = (f(1,1)*cpi0(1) + f(1,2)*cpi0(3))*f(2,1) + &
                    (f(1,1)*cpi0(3) + f(1,2)*cpi0(2))*f(2,2)
                be(2,1) = be(1,2)
                be(2,2) = (f(2,1)*cpi0(1) + f(2,2)*cpi0(3))*f(2,1) + &
                    (f(2,1)*cpi0(3) + f(2,2)*cpi0(2))*f(2,2)
                beh = fh**2*cpi0(4)
            ELSE
                be(1,1) = f(1,1)**2 + f(1,2)**2
                be(1,2) = f(1,1)*f(2,1) + f(1,2)*f(2,2)
                be(2,1) = be(1,2)
                be(2,2) = f(2,1)**2 + f(2,2)**2
                beh = fh**2
            END IF

            ! spectral decomposition
            CALL jacobi2(be,dl2,nv)
            dl2(3) = beh

            ! stretches and logarithmic strains
            dl(1:3) = SQRT(dl2(1:3))
            en(1:3) = LOG(dl(1:3))

            ! deviatoric stresses
            J = dl(1) * dl(2) * dl(3)
            tmp1 = (en(1) + en(2) + en(3)) / 3.0_r8k
            DO id = 1, 3
                taudev(id) = 2.0_r8k * mu * (en(id) - tmp1)
                sigdev(id) = taudev(id) / J
            END DO

            ! Trial effective Kirchhoff stress
            taueff = SQRT(1.5_r8k * (taudev(1) ** 2 + taudev(2) ** 2 + taudev(3) ** 2))
            sigeff = taueff / J

            ! Initialize plastic strains
            gamma = 0.0_r8k
            epseff = epseff0

            ! Creep calculation
            CALL mat_creep_calc(mat, gtemp, mu, dtime, epseff0, taueff, gamma, deds)

            IF (lerror) RETURN

            ! plastic calculation
            CALL mat_radial_return(mat, gtemp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)

            IF (lerror) RETURN

            IF (gamma > 0.0_r8k) THEN
                ! Update effective stresses
                sigeff = (taueff - 3.0_r8k * mu * gamma) / J

                ! Update effective plastic strain
                epseff = epseff0 + gamma

                ! flow rule (Maximum plastic dissipation)
                DO id = 1, 3
                    av(id) = 1.5_r8k * taudev(id) / taueff
                END DO

                ! Update deviatoric stress
                DO id = 1, 3
                    en(id) = en(id) - gamma * av(id)
                    taudev(id) = taudev(id) - 2.0_r8k * mu * gamma * av(id)
                    sigdev(id) = taudev(id) / J
                END DO

                ! elastic left Cauchy Green strain tensor
                DO id = 1, 3
                    dl(id) = dl(id) / EXP(gamma * av(id))
                    dl(id) = dl(id) ** 2
                END DO
                DO id = 1, 2
                    DO jd = 1, 2
                        be(id,jd) = 0.0_r8k
                        DO kd = 1, 2
                            be(id,jd) = be(id,jd) + dl(kd) * nv(id,kd) * nv(jd,kd)
                        END DO
                    END DO
                END DO
                beh = dl(3)

                ! update nonelastic deformations
                tmp2 = 1.0_r8k/(f(1,1)*f(2,2)-f(1,2)*f(2,1))
                cpi(1) = ((f(2,2)*be(1,1) - f(1,2)*be(2,1))*f(2,2) - &
                    (f(2,2)*be(1,2) - f(1,2)*be(2,2))*f(1,2))*tmp2**2
                cpi(3) = (-(f(2,2)*be(1,1) - f(1,2)*be(2,1))*f(2,1) + &
                    (f(2,2)*be(1,2) - f(1,2)*be(2,2))*f(1,1))*tmp2**2
                cpi(2) = (-(-f(2,1)*be(1,1) + f(1,1)*be(2,1))*f(2,1) + &
                    (-f(2,1)*be(1,2) + f(1,1)*be(2,2))*f(1,1))*tmp2**2
                cpi(4) = beh/fh**2

                ! Plastic strain energy
                delta_plastic_strain_energy = delta_plastic_strain_energy + &
                    gamma*sigeff*current%dV(ig)

                ! Find maximum plastic increment
                maxgamma = MAX(maxgamma,gamma)
            ELSE
                cpi(1:4) = cpi0(1:4)
            END IF

            ! Deviatoric part of elastic strain energy
            elastic_strain_energy = elastic_strain_energy + mu*current%dV0(ig)* &
                ((en(1) - tmp1)**2 + (en(2) - tmp1)**2 + (en(3) - tmp1)**2)

            ! Cauchy stresses in cartesian CS
            sigma(1) = sigdev(1)*nv(1,1)**2 + sigdev(2)*nv(1,2)**2 + phyd
            sigma(2) = sigdev(1)*nv(2,1)**2 + sigdev(2)*nv(2,2)**2 + phyd
            sigma(3) = sigdev(3) + phyd
            sigma(4) = nv(1,1)*sigdev(1)*nv(2,1) + nv(1,2)*sigdev(2)*nv(2,2)
        END DO
        current => current%next
    END DO

    END SUBROUTINE quad4_stress
    !
    !
    !
    SUBROUTINE quad4_fint()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Element stress calculation
    !
    INTEGER(ipk) :: in,ig,id,jd,node
    REAL(r8k) :: drm0, dzm0, dri, dzi, ri, drmi, dzmi, Bi(2, 4)
    REAL(r8k), POINTER :: sigma(:),dV
    TYPE(quad4_type), POINTER :: current

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        DO in = 1, 4
            ! Node number
            node = current%node_numbers(in)

            ! Internal forces
            drm0 = current%dNdxm(1,in) / 3.0_r8k
            dzm0 = current%dNdxm(2,in) / 3.0_r8k
            DO ig = 1, 4
                dri = current%dNdx(1,in,ig)
                dzi = current%dNdx(2,in,ig)
                ri = N(in,ig)/current%x(1,ig)
                drmi = (dri + ri) / 3.0_r8k
                dzmi = dzi / 3.0_r8k
                Bi(1,1) = dri + drm0 - drmi
                Bi(1,2) = drm0 - drmi
                Bi(1,3) = ri + drm0 - drmi
                Bi(1,4) = dzi
                Bi(2,1) = dzm0 - dzmi
                Bi(2,2) = dzi + dzm0 - dzmi
                Bi(2,3) = dzm0 - dzmi
                Bi(2,4) = dri
                sigma => current%sigma(1:4,ig)
                dV => current%dV(ig)
                DO id = 1, 2
                    DO jd = 1, 4
                        Fint(id,node) = Fint(id,node) + Bi(id,jd) * sigma(jd) * dV
                    END DO
                END DO
            END DO
        END DO
        current => current%next
    END DO

    END SUBROUTINE quad4_fint
    !
    !
    !
    SUBROUTINE quad4_stiff()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Element stiffness matrices
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: ig, in, jn, id, jd, kd, ie, je, dofnum(8)
    REAL(r8k),  POINTER :: J, mu, lambda, sigdev(:), nv(:, :), dl2(:), dV, phyd, deds, &
                           sigma(:), kappa, Jm, dNdx(:, :), av(:), gamma, taueff, dplmod
    REAL(r8k) :: Ke(8, 8), tmod(10), Bi(4, 2, 4), BiC(4, 2), drm0, dzm0, dri,  &
                 dzi, ri, drmi, dzmi, Keij(2, 2), tmp2

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        Ke = 0.0_r8k
        phyd => current%phyd
        kappa => current%kappa
        Jm => current%Jm
        ! Deviatoric part of the tangent stiffness matrix
        DO ig = 1, 4
            ! Assign pointers
            J => current%J(ig)
            mu => current%mu(ig)
            lambda => current%lambda(ig)
            sigdev => current%sigdev(1:3,ig)
            nv => current%nv(1:2,1:2,ig)
            dl2 => current%dl2(1:3,ig)
            dV => current%dV(ig)
            sigma => current%sigma(1:4,ig)
            dNdx => current%dNdx(1:2,1:4,ig)
            av => current%av(1:3,ig)
            gamma => current%gamma(ig)
            taueff => current%taueff(ig)
            dplmod => current%dplmod(ig)
            deds => current%deds(ig)

            ! Kinematic matrix
            DO in = 1, 4
                drm0 = current%dNdxm(1,in) / 3.0_r8k
                dzm0 = current%dNdxm(2,in) / 3.0_r8k
                dri = current%dNdx(1,in,ig)
                dzi = current%dNdx(2,in,ig)
                ri = N(in,ig) / current%x(1,ig)
                drmi = (dri + ri) / 3.0_r8k
                dzmi = dzi / 3.0_r8k
                Bi(1,1,in) = dri + drm0 - drmi
                Bi(2,1,in) = drm0 - drmi
                Bi(3,1,in) = ri + drm0 - drmi
                Bi(4,1,in) = dzi
                Bi(1,2,in) = dzm0 - dzmi
                Bi(2,2,in) = dzi + dzm0 - dzmi
                Bi(3,2,in) = dzm0 - dzmi
                Bi(4,2,in) = dri             
            END DO
            ! Material tangent modulus
            tmod = quad4_tmod(J, Jm, mu, lambda, kappa, sigdev, phyd, nv, dl2, gamma, av, taueff, dplmod, deds)
            DO in = 1, 4
                BiC = quad4_tmod_matmul(Bi(1,1,in),tmod)
                DO jn = in,4
                    ! Material part of the stiffness matrix
                    DO id = 1, 2
                        DO jd = 1, 2
                            Keij(id,jd) = 0.0_r8k
                            Do kd = 1, 4
                                Keij(id,jd) = Keij(id,jd) + BiC(kd,id) * Bi(kd,jd,jn)
                            END DO
                        END DO
                    END DO
                    ! Geometric part of the stiffness matrix
                    IF (.NOT. elastic_matrix) THEN
                        tmp2 = (dNdx(1,in) * sigma(1) + dNdx(2,in) * sigma(4)) * dNdx(1,jn) &
                             + (dNdx(1,in) * sigma(4) + dNdx(2,in) * sigma(2)) * dNdx(2,jn)
                        Keij(1,1) = Keij(1,1) + tmp2 + N(in,ig) * N(jn,ig) * sigma(3) / current%x(1,ig) ** 2
                        Keij(2,2) = Keij(2,2) + tmp2
                    END IF
                    ! Place submatrix Keij to element matrix se
                    DO id = 1, 2
                        DO jd = 1,2
                            ie = (in - 1) * 2 + id
                            je = (jn - 1) * 2 + jd
                            Ke(ie,je) = Ke(ie,je) + Keij(id,jd) * dV
                        END DO
                    END DO
                END DO
            END DO
        END DO

        ! Mirror the symmetric matrix
        DO ie = 1,  8
            DO je = (ie + 1), 8
                Ke(je,ie) = Ke(ie,je)
            END DO
        END DO

        ! DOF numbering
        dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
        dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
        dofnum(5:6) = dof_number(1:2,current%node_numbers(3))
        dofnum(7:8) = dof_number(1:2,current%node_numbers(4))

        ! Place element matrix in the global matrix
        CALL sparse_matrix_place_element(8,dofnum,Ke)
        current => current%next
    END DO

    END SUBROUTINE quad4_stiff
    !
    !
    !
    FUNCTION quad4_tmod_matmul (Bi, tmod) RESULT (BiC)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Multiply kinematic matrix with tangent modulus
    !
    REAL(r8k) :: Bi(4,2),tmod(10),BiC(4,2)
    INTEGER(ipk) :: tmodi(4,4),id,jd,kd

    tmodi(1:4,1) = (/  1,  3,  6, 10 /)
    tmodi(1:4,2) = (/  3,  2,  5,  9 /)
    tmodi(1:4,3) = (/  6,  5,  4,  8 /)
    tmodi(1:4,4) = (/ 10,  9,  8,  7 /)

    DO id = 1, 2
        DO jd = 1, 4
            BiC(jd,id) = 0.0_r8k
            DO kd = 1, 4
                BiC(jd,id) = BiC(jd,id) + Bi(kd,id)*tmod(tmodi(kd,jd))
            END DO
        END DO
    END DO

    END FUNCTION quad4_tmod_matmul
    !
    !
    !
    FUNCTION quad4_tmod(J, Jm, mu, lambda, kappa, sigdev, phyd, nv, dl2, gamma, av, taueff, dplmod, deds) RESULT(tmod)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deviatoric material tangent modulus
    REAL(r8k) :: J, Jm, mu, lambda, kappa, sigdev(3), phyd, nv(2, 2), dl2(3),  &
                 gamma, av(3), taueff, dplmod, deds, tmod(10), tmp(6), tmp2(3, 3),  &
                 t2, t4, t7, t9, t12, t15, t16, t21, t22, t26, t27, t40, t44, t48, t57
    REAL(r8k) :: aik(3, 3), bik
    INTEGER(ipk) :: i, k

    tmod = 0.0_r8k

    ! Use initial elastic matrix_frapcon
    IF (elastic_matrix) THEN
        tmod(1) = 2.0_r8k * mu + lambda
        tmod(2) = 2.0_r8k * mu + lambda
        tmod(4) = 2.0_r8k * mu + lambda
        tmod(7) = mu
        tmod(3) = lambda
        tmod(5) = lambda
        tmod(6) = lambda
        RETURN
    END IF

    ! Deviatoric part
    aik = -2.0_r8k * mu / 3.0_r8k / J
    DO i = 1, 3
        aik(i,i) = aik(i,i) + 2.0_r8k * mu / J
    END DO

    ! Creep part of the tangent modulus
    IF (gamma > 1.0e-10_r8k) THEN
        IF (deds > 0.0_r8k) THEN
            DO i = 1, 3
                DO k = 1, 3
                tmp2(i,k) = aik(i,k) - aik(i,k)*(3.0_r8k*mu*gamma/taueff) &
                        + 4.0_r8k*mu**2*av(i)*av(k)*gamma/taueff/J
                END DO
            END DO
            tmp(1) = 1.0_r8k + 2.0_r8k * mu * deds * av(1) * av(1)
            tmp(2) = 1.0_r8k + 2.0_r8k * mu * deds * av(2) * av(2)
            tmp(3) = 2.0_r8k * mu * deds * av(1) * av(2)
            tmp(4) = 1.0_r8k + 2.0_r8k * mu * deds * av(3) * av(3)
            tmp(5) = 2.0_r8k * mu * deds * av(2) * av(3)
            tmp(6) = 2.0_r8k * mu * deds * av(1) * av(3)
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
            DO i = 1, 3
                DO k = 1, 3
                    aik(i,k) = aik(i,k) - aik(i,k)*(3.0_r8k*mu*gamma/taueff) &
                            - 2.0_r8k*mu*av(i)*av(k)*(2.0_r8k*mu/dplmod &
                            - 2.0_r8k*mu*gamma/taueff)/J
                END DO
            END DO
        END IF
    END IF

    ! (3,3)
    tmod(4) = aik(3,3) - 2.0_r8k*sigdev(3)

    ! (1,1),(2,2),(4,4),(3,4),(1,4)
    DO i = 1, 2
        DO k = 1, 2
            bik = aik(i,k)
            IF (i == k) bik = aik(i,k) - 2.0_r8k*sigdev(i)
            tmod(1) = tmod(1) + bik*nv(1,i)**2*nv(1,k)**2
            tmod(2) = tmod(2) + bik*nv(2,i)**2*nv(2,k)**2
            tmod(3) = tmod(3) + bik*nv(1,i)**2*nv(2,k)**2
            tmod(7) = tmod(7) + 2.0_r8k*bik*nv(1,i)*nv(2,i)*nv(1,k)*nv(2,k)
            tmod(9) = tmod(9) + 2.0_r8k*bik*nv(2,i)*nv(2,i)*nv(1,k)*nv(2,k)
            tmod(10)= tmod(10)+ 2.0_r8k*bik*nv(1,i)*nv(1,i)*nv(1,k)*nv(2,k)
            IF (i /= k) THEN
                IF (ABS(dl2(i) - dl2(k)) < 1.0e-10_r8k) THEN
                    bik = 2.0_r8k * (mu / J - sigdev(i))
                ELSE
                    bik = 2.0_r8k * (sigdev(i) * dl2(k) - sigdev(k) * dl2(i)) / (dl2(i) - dl2(k))
                END IF
                tmod(1) = tmod(1) + bik*nv(1,i)*nv(1,k)*nv(1,i)*nv(1,k)
                tmod(2) = tmod(2) + bik*nv(2,i)*nv(2,k)*nv(2,i)*nv(2,k)
                tmod(3) = tmod(3) + bik*nv(1,i)*nv(1,k)*nv(2,i)*nv(2,k)
                tmod(7) = tmod(7) + bik*nv(1,i)*nv(2,k)*nv(1,i)*nv(2,k)
                tmod(7) = tmod(7) + bik*nv(1,i)*nv(2,k)*nv(2,i)*nv(1,k)
                tmod(9) = tmod(9) + bik*nv(2,i)*nv(2,k)*nv(1,i)*nv(2,k)
                tmod(9) = tmod(9) + bik*nv(2,i)*nv(2,k)*nv(2,i)*nv(1,k)
                tmod(10) = tmod(10) + bik*nv(1,i)*nv(1,k)*nv(1,i)*nv(2,k)
                tmod(10) = tmod(10) + bik*nv(1,i)*nv(1,k)*nv(2,i)*nv(1,k)
            END IF
        END DO
    END DO
    tmod(7) = 0.5_r8k * tmod(7)
    tmod(9) = 0.5_r8k * tmod(9)
    tmod(10) = 0.5_r8k * tmod(10)

    ! (1,3),(2,3),(2,4)
    DO i = 1, 2
        tmod(6) = tmod(6) + aik(i,3) * nv(1,i) * nv(1,i)
        tmod(5) = tmod(5) + aik(i,3) * nv(2,i) * nv(2,i)
        tmod(8) = tmod(8) + aik(i,3) * nv(1,i) * nv(2,i)
    END DO

    ! Hydrostatic part
    bik = kappa * (1.0_r8k - LOG(Jm)) / Jm + phyd
    tmod(1) = tmod(1) + bik - 2.0_r8k * phyd
    tmod(2) = tmod(2) + bik - 2.0_r8k * phyd
    tmod(4) = tmod(4) + bik - 2.0_r8k * phyd
    tmod(3) = tmod(3) + bik
    tmod(5) = tmod(5) + bik
    tmod(6) = tmod(6) + bik
    tmod(7) = tmod(7) - phyd

    END FUNCTION quad4_tmod
    !
    !
    !
    SUBROUTINE quad4_update()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Update explicit values
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: ig

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        DO ig = 1, 4
            current%plSED(ig) = current%plSED(ig) + 0.5_r8k*current%gamma(ig) * (current%sigeff0(ig) + current%sigeff(ig))
            current%nv0(1:2,ig) = current%nv(1:2,1,ig)
        END DO
        current%epseff0 = current%epseff
        current%sigeff0 = current%sigeff
        current%cpi0 = current%cpi
        current%epsth0 = current%epsth
        CALL mat_maximums(current%mat, current%epseff0, current%sigeff0, current%plSED, current%sigma)
        current => current%next
    END DO

    END SUBROUTINE quad4_update
    !
    !
    !
    SUBROUTINE quad4_strains()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate logarithmic strain values
    !
    TYPE(quad4_type), POINTER :: current
    INTEGER(ipk) :: ig
    REAL(r8k) :: f(2,2), fh, b(2,2), bh, dl2(3), nv(2,2), dl(3), en(3)
    REAL(r8k), POINTER :: epstot(:)

    ! Stress calculation loop
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        DO ig = 1, 4
            ! Assign pointers
            epstot => current%epstot(1:4,ig)

            ! total deformation gradient
            f(1:2,1) = epstot(1:2)
            f(1:2,2) = epstot(3:4)
            fh = current%x(1,ig)/current%x0(1,ig)

            ! left Cauchy Green strain tensor
            b(1,1) = f(1,1)**2 + f(1,2)**2
            b(1,2) = f(1,1)*f(2,1) + f(1,2)*f(2,2)
            b(2,1) = b(1,2)
            b(2,2) = f(2,1)**2 + f(2,2)**2
            bh = fh**2

            ! spectral decomposition
            CALL jacobi2(b,dl2,nv)
            dl2(3) = bh

            ! stretches and logarithmic strains
            dl(1:3) = SQRT(dl2(1:3))
            en(1:3) = LOG(dl(1:3))

            ! Total logarithmic strain tensor
            epstot(1) = en(1)*nv(1,1)**2 + en(2)*nv(1,2)**2
            epstot(2) = en(1)*nv(2,1)**2 + en(2)*nv(2,2)**2
            epstot(3) = en(3)
            epstot(4) = 2.0_r8k*(nv(1,1)*en(1)*nv(2,1) + nv(1,2)*en(2)*nv(2,2))
        END DO
        current => current%next
    END DO

    END SUBROUTINE quad4_strains
    !
    !
    !
    SUBROUTINE quad4_write_output (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write output to a file in unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(quad4_type), POINTER :: current

    WRITE (UNIT=nunit) nquad4
    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        WRITE (UNIT=nunit) current%label
        WRITE (UNIT=nunit) current%egroup
        WRITE (UNIT=nunit) current%mat
        WRITE (UNIT=nunit) current%node_labels
        WRITE (UNIT=nunit) current%node_numbers
        WRITE (UNIT=nunit) current%sigeff
        WRITE (UNIT=nunit) current%sigma
        WRITE (UNIT=nunit) current%phyd
        WRITE (UNIT=nunit) current%epstot
        WRITE (UNIT=nunit) current%epsth0
        WRITE (UNIT=nunit) current%epseff0
        WRITE (UNIT=nunit) current%cpi0
        WRITE (UNIT=nunit) current%plSED
        current => current%next
    END DO

    END SUBROUTINE quad4_write_output
    !
    !
    !
    SUBROUTINE quad4_read_output (nunit)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    !
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(quad4_type), POINTER :: current,previous

    READ(UNIT=nunit,ERR=20,END=20) nquad4

    current => first_quad4
    DO i = 1, nquad4
        IF (.NOT. ASSOCIATED(current)) THEN
            ALLOCATE(current)
            current%next => NULL()
            IF (.NOT. ASSOCIATED(first_quad4)) THEN
                first_quad4 => current
                last_quad4 => current
            ELSE
                last_quad4%next => current
                last_quad4 => current
            END IF
        END IF
        READ(UNIT=nunit,ERR=20,END=20) current%label
        READ(UNIT=nunit,ERR=20,END=20) current%egroup
        READ(UNIT=nunit,ERR=20,END=20) current%mat
        READ(UNIT=nunit,ERR=20,END=20) current%node_labels(1:4)
        READ(UNIT=nunit,ERR=20,END=20) current%node_numbers(1:4)
        READ(UNIT=nunit,ERR=20,END=20) current%sigeff
        READ(UNIT=nunit,ERR=20,END=20) current%sigma
        READ(UNIT=nunit,ERR=20,END=20) current%phyd
        READ(UNIT=nunit,ERR=20,END=20) current%epstot
        READ(UNIT=nunit,ERR=20,END=20) current%epsth0
        READ(UNIT=nunit,ERR=20,END=20) current%epseff0
        READ(UNIT=nunit,ERR=20,END=20) current%cpi0
        READ(UNIT=nunit,ERR=20,END=20) current%plSED
        last_quad4 => current
        current => current%next
    END DO

    ! Remove excess element entries from the database
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO
    IF (ASSOCIATED(last_quad4)) last_quad4%next => NULL()
    IF (nquad4 == 0) THEN
        first_quad4 => NULL()
        last_quad4 => NULL()
    END IF

    RETURN

20  CONTINUE
    WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading QUAD4 database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE quad4_read_output
    !
    !
    !
    SUBROUTINE quad4_deallocate()
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate QUAD4 database
    !
    TYPE(quad4_type), POINTER :: current,previous

    current => first_quad4
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE quad4_deallocate
    !
END MODULE quad4_frapcon



