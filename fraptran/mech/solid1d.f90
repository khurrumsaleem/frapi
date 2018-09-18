MODULE solid1D
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    USE materials_frap
    IMPLICIT NONE
    !>@brief
    !> 1 1/2-Dimensional axisymmetric SOLID1D element
    !
    !        ^ eta               xi,eta ........ element coordinates
    !        | N4                N1,N2,N3,N4 ... nodes
    !    +---o---+               G1 ............ Integration point
    !    |   |   |
    !    |   |   | N2
    ! N1 o   x---o-->
    !    |  G1   |   xi
    !    |       |
    !    +---o---+
    !        N3
    !
    ! Vector presentation for stresses
    !   Index Stress component
    !     1     radial
    !     2     axial
    !     3     hoop
    !

  INTEGER(ipk) :: nsolid1d

  TYPE solid1d_type
     INTEGER(ipk) :: label ! Element label
     INTEGER(ipk) :: number ! Internal element number
     INTEGER(ipk) :: mat ! Material label
     INTEGER(ipk) :: egroup ! Element group label
     LOGICAL :: rigid ! If true there will be no stress strain analysis
                      ! in this element type
     INTEGER(ipk) :: node_labels(4) ! Element node labels
     INTEGER(ipk) :: node_numbers(4) ! Internal node numbering
     REAL(r8k) :: mu ! Temperature dependent material parameter
     REAL(r8k) :: lambda ! Temperature dependent material parameter
     REAL(r8k) :: kappa ! Temperature dependent material parameter
     REAL(r8k) :: gtemp ! Integration point temperature
     REAL(r8k) :: epsth ! Thermal dilation
     REAL(r8k) :: sigma(3) ! Cauchy stresses
     REAL(r8k) :: sigdev(3) ! Deviatoric Cauchy stresses
     REAL(r8k) :: phyd ! Hydrostatic pressure
     REAL(r8k) :: epstot(3) ! True strains
     REAL(r8k) :: epsel(3) ! True strains
     REAL(r8k) :: epseff ! Effective plastic strain
     REAL(r8k) :: epspl(3) ! True plastic strains
     REAL(r8k) :: epseff0 ! Explicit effective plastic strain
     REAL(r8k) :: epspl0(3) ! Explicit true plastic strains
     REAL(r8k) :: sigeff ! Effective or von Mises stress
     REAL(r8k) :: sigeff0 ! Explicit effective or von Mises stress
     REAL(r8k) :: taueff ! Trial effective Kirchhoff stress
     REAL(r8k) :: gamma ! Plastic multiplier
     REAL(r8k) :: dplmod ! Plastic modulus
     REAL(r8k) :: deds ! 
     REAL(r8k) :: av(3) ! Plastic flow direction
     REAL(r8k) :: Je ! Jacobian determinant
     REAL(r8k) :: dV ! Weight factor for numerical integration
     REAL(r8k) :: dV0
     REAL(r8k) :: dNdx(3) ! Derivatives of the shape functions
     REAL(r8k) :: x(2) ! Integration point coordinate
     REAL(r8k) :: x0(2) ! Initial integration point coordinate
     REAL(r8k) :: plSED ! Plastic SED
     TYPE(solid1d_type), POINTER :: next
  END TYPE solid1d_type

  TYPE(solid1d_type), POINTER :: first_solid1d,last_solid1d

CONTAINS
    SUBROUTINE solid1d_init()
    ! Initialize element Database
    IMPLICIT NONE

    nsolid1d = 0
    first_solid1d => NULL()
    last_solid1d => NULL()

    RETURN

  END SUBROUTINE solid1d_init


    SUBROUTINE solid1d_create(label,mat,nodes,flag)
    ! Create new element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,mat,nodes(4),flag
    INTEGER(ipk) :: nfound,in,jn
    TYPE(solid1d_type), POINTER :: new_solid1d,current

    nsolid1d = nsolid1d + 1
    ALLOCATE(new_solid1d)
    new_solid1d%label = label
    new_solid1d%number = nsolid1d
    new_solid1d%mat = mat
    new_solid1d%egroup = egroup
    CALL mat_create(mat)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,4
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_solid1d%node_labels(jn) = nodes(jn)
             new_solid1d%node_numbers(jn) = in
             enumber(in) = enumber(in) + 1
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 4 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 4 ) THEN
       WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR incompatible NODES and SOLID1D Data'
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Check that the element label doesn't already exist
    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) THEN
          WRITE(UNIT=6,FMT='(/,A,I0,A,/)') &
               'ERROR SOLID1D ',label,' has been defined twice'
          lerror = .TRUE.
          CALL nlfemp_stop(0)
       ENDIF
       current => current%next
    ENDDO

    ! Initialize SOLID1D variables
    new_solid1d%rigid = .FALSE.
    IF ( flag == 0 ) new_solid1d%rigid = .TRUE.
    new_solid1d%epspl0 = 0.0_r8k
    new_solid1d%epseff0 = 0.0_r8k
    new_solid1d%sigeff0 = 0.0_r8k
    new_solid1d%sigma = 0.0_r8k
    new_solid1d%phyd = 0.0_r8k
    new_solid1d%epstot = 0.0_r8k
    new_solid1d%epsth = 0.0_r8k
    new_solid1d%plSED = 0.0_r8k
    new_solid1d%sigma = 0.0_r8k
    new_solid1d%next => NULL()

    ! Initial derivatives
    CALL solid1d_deriv(x0(1,new_solid1d%node_numbers(1)), &
         x0(1,new_solid1d%node_numbers(2)), &
         x0(1,new_solid1d%node_numbers(3)), &
         x0(1,new_solid1d%node_numbers(4)),new_solid1d%dV0, &
         new_solid1d%dNdx,new_solid1d%x0)

    IF ( lerror ) Call nlfemp_stop(0)

    ! Set fixed status for the right DOFs
    IF ( nd > 2 ) THEN
       DO in=1,4
          jn = new_solid1d%node_labels(in)
          CALL set_node_status(jn,3,-1)
       ENDDO
    ENDIF
    jn = new_solid1d%node_labels(1)
    CALL set_node_status(jn,2,-1)
    jn = new_solid1d%node_labels(2)
    CALL set_node_status(jn,2,-1)
    jn = new_solid1d%node_labels(3)
    CALL set_node_status(jn,1,-1)
    jn = new_solid1d%node_labels(4)
    CALL set_node_status(jn,1,-1)

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add element to the SOLID1D Database
    IF ( .NOT.ASSOCIATED(first_solid1d) ) THEN
       first_solid1d => new_solid1d
       last_solid1d => new_solid1d
    ELSE
       last_solid1d%next => new_solid1d
       last_solid1d => new_solid1d
    ENDIF

    RETURN

  END SUBROUTINE solid1d_create


    SUBROUTINE solid1d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(solid1d_type), POINTER :: current,previous,tobedeleted
    INTEGER(ipk) :: in,jn
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_solid1d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_solid1d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_solid1d
       first_solid1d => first_solid1d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( lfound ) THEN
          current%number = current%number - 1
       ELSE If ( current%label == label ) THEN
          lfound = .TRUE.
          previous%next => current%next
          tobedeleted => current
       ENDIF
       previous => current
       current => current%next
    ENDDO

    ! Set the POINTER to the last SOLID1D element
    last_solid1d => NULL()
    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       last_solid1d => current
       current => current%next
    ENDDO
    
    ! Deallocate SOLID1D element
    IF ( lfound ) THEN
       nsolid1d = nsolid1d - 1
       dof_numbering = .TRUE.
       DO in=1,4
          jn = tobedeleted%node_numbers(in)
          enumber(jn) = enumber(jn) - 1
       ENDDO
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE solid1d_delete


    SUBROUTINE solid1d_sparse_matrix()
    ! Initialize sparse matrix storage for solid1d elements
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(4),i

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1) = dof_number(1,current%node_numbers(1))
       dofnum(2) = dof_number(1,current%node_numbers(2))
       dofnum(3) = dof_number(2,current%node_numbers(3))
       dofnum(4) = dof_number(2,current%node_numbers(4))
       IF ( current%rigid ) THEN
          DO i=1,4
             IF ( dofnum(i) > 0 ) &
                  CALL sparse_matrix_add_member(dofnum(i),dofnum(i))
          ENDDO
       ELSE
          CALL sparse_matrix_add_element(4,dofnum)
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_sparse_matrix


    SUBROUTINE solid1d_temp()
    ! Calculate temperature dependent properties of the element
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    REAL(r8k) :: gtemp,emod,nu,epsth,epsth0
    INTEGER(ipk) :: mat

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%rigid ) THEN
          ! Integration point coordinates
          cp = (/ current%x0(1:2), 0.0_r8k /)

          ! Material label
          mat = current%mat

          ! Integration point temperature
          gtemp = 0.5_r8k*(temp(current%node_numbers(1)) + &
               temp(current%node_numbers(2)))
          current%gtemp = gtemp

          ! Temperature dependent elastic material properties
          emod = mat_par(mat,gtemp,'EMOD    ')
          nu = mat_par(mat,gtemp,'PRATIO  ')
          current%lambda = emod*nu/(1.0_r8k-2.0_r8k*nu)/(1.0_r8k + nu)
          current%mu = 0.5_r8k*emod/(1.0_r8k+nu)
          current%kappa = (current%lambda+2.0_r8k*current%mu/3.0_r8k)

          ! Thermal dilation
          epsth0 = mat_par(mat,tref,'THDIL   ')
          epsth = mat_par(mat,gtemp,'THDIL   ')
          current%epsth = 1.0_r8k/EXP(epsth - epsth0)
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_temp


    SUBROUTINE solid1d_deriv(x1,x2,x3,x4,dV,dNdx,x_gp)
    ! Internal forces calculation
    IMPLICIT NONE
    REAL(r8k), INTENT(IN) :: x1(2),x2(2),x3(2),x4(2)
    REAL(r8k), INTENT(OUT) :: dV,dNdx(3),x_gp(2)
    REAL(r8k) :: dr,dz,rm

    dr = x2(1) - x1(1)
    dz =  x4(2) - x3(2)
    rm = 0.5_r8k*(x1(1) + x2(1))
    dV = 2.0_r8k*pi*rm*dr*dz
    IF ( ( dr <= 0.0_r8k ).OR.( dz <= 0.0_r8k ) ) THEN
       lerror = .TRUE.
       RETURN
    ENDIF
    dNdx(1) = 1.0_r8k/dr
    dNdx(2) = 1.0_r8k/dz
    dNdx(3) = 0.5_r8k/rm
    x_gp(1) = rm
    x_gp(2) = 0.5_r8k*(x3(2) + x4(2))

    RETURN

  END SUBROUTINE solid1d_deriv


    SUBROUTINE solid1d_stress()
    ! Element stress calculation
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    INTEGER(ipk) :: mat,id
    REAL(r8k) :: lnJe,taudev(3),dl(3)
    REAL(r8k), POINTER :: gtemp,mu,lambda,epsth,epseff,epseff0, &
         gamma,Je,phyd,sigeff,dplmod,kappa,taueff,deds
    REAL(r8k), DIMENSION(:), POINTER :: epsel, epspl, epspl0, sigma, epstot, av, sigdev
    INTEGER(ipk), DIMENSION(:), POINTER :: nodes

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%rigid ) THEN
          ! Node numbers
          nodes => current%node_numbers(1:4)

          ! Integration point coordinates
          cp = (/ current%x0(1:2), 0.0_r8k /)

          ! Material label
          mat = current%mat

          ! Derivatives
          CALL solid1d_deriv(x(1,nodes(1)),x(1,nodes(2)),x(1,nodes(3)), &
               x(1,nodes(4)),current%dV,current%dNdx,current%x)

          ! RETURN If there are poorly shaped elements
          IF ( lerror ) RETURN

          ! Assign POINTERs
          gtemp => current%gtemp
          mu => current%mu
          lambda => current%lambda
          kappa => current%kappa
          epsel => current%epsel(1:3)
          epsth => current%epsth
          epseff => current%epseff
          epseff0 => current%epseff0
          gamma => current%gamma
          Je => current%Je
          epspl => current%epspl(1:3)
          epspl0 => current%epspl0(1:3)
          phyd => current%phyd
          sigma => current%sigma(1:3)
          sigeff => current%sigeff
          epstot => current%epstot(1:3)
          taueff => current%taueff
          av => current%av(1:3)
          dplmod => current%dplmod
          deds => current%deds
          sigdev => current%sigdev(1:3)

          ! Principal stretches
          dl(1) = 1.0_r8k + (u(1,nodes(2)) - u(1,nodes(1))) &
               /(x0(1,nodes(2)) - x0(1,nodes(1)))
          dl(2) = 1.0_r8k + (u(2,nodes(4)) - u(2,nodes(3))) &
               /(x0(2,nodes(4)) - x0(2,nodes(3)))
          dl(3) = 1.0_r8k + (u(1,nodes(1)) + u(1,nodes(2))) &
               /(x0(1,nodes(1)) + x0(1,nodes(2)))

          ! Total true strains
          epstot(1:3) = LOG(dl(1:3))

          ! Remove the plastic and thermal part from the stretches
          DO id=1,3
             dl(id) = dl(id)/EXP(epspl0(id))*epsth
          ENDDO

          ! Jacobian determinant
          Je = dl(1)*dl(2)*dl(3)
          lnJe = LOG(current%Je)

          ! Trial elastic strains
          epsel(1:3) = LOG(dl(1:3))

          ! Deviatoric trial kirchoff stress
          DO id=1,3
             taudev(id) = 2.0_r8k*mu*(epsel(id) - lnJe/3.0_r8k)
             sigdev(id) = taudev(id)/Je
          ENDDO

          ! Hydrostatic pressure
          phyd = kappa*lnJe/Je

          ! Trial effective Kirchhoff stress
          taueff = SQRT(1.5_r8k*(taudev(1)**2 + taudev(2)**2 + taudev(3)**2))
          sigeff = taueff/Je

          ! Initialize plastic multiplier
          gamma = 0.0_r8k
          epseff = epseff0
          epspl = epspl0

          ! Creep calculation
          CALL mat_creep_calc(mat,gtemp,mu,dtime,epseff0,taueff,gamma,deds)

          IF ( lerror ) RETURN

          ! plastic calculation
          CALL mat_radial_Return(mat,gtemp,mu,dtime,epseff0,taueff,gamma, &
               dplmod,deds)

          IF ( lerror ) RETURN

          IF ( gamma > 0.0_r8k ) THEN
             ! Update effective stress
             sigeff = (taueff - 3.0_r8k*mu*gamma)/Je

             ! flow rule (Maximum plastic dissipation)
             DO id=1,3
                av(id) = 1.5_r8k*taudev(id)/taueff
             ENDDO

             ! Update variables
             DO id=1,3
                taudev(id) = taudev(id) - 2.0_r8k*mu*gamma*av(id)
                sigdev(id) = taudev(id)/Je
                epspl(id) = epspl0(id) + gamma*av(id)
                epsel(id) = epsel(id) - gamma*av(id)
             ENDDO
             epseff = epseff0 + gamma

             ! Plastic strain energy
             delta_plastic_strain_energy = delta_plastic_strain_energy + &
                  gamma*sigeff*current%dV

             ! Find maximum plastic increment
             maxgamma = MAX(maxgamma,gamma)
          ENDIF

          ! Update energies
          elastic_strain_energy = elastic_strain_energy + &
               (mu*(epsel(1)**2 + epsel(2)**2 + epsel(3)**2) + &
               0.5_r8k*lambda*lnJe**2)*current%dV0

          ! Update stresses
          DO id=1,3
             sigma(id) = taudev(id)/Je + phyd
          ENDDO
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_stress


    SUBROUTINE solid1d_fint()
    ! Internal forces calculation
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    REAL(r8k), POINTER :: dV
    REAL(r8k), DIMENSION(:), POINTER :: sigma, dNdx

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%rigid ) THEN
          ! Assign POINTERs
          dV => current%dV
          sigma => current%sigma(1:3)
          dNdx => current%dNdx(1:3)
          Fint(1,current%node_numbers(1)) = Fint(1,current%node_numbers(1)) + &
               (-dNdx(1)*sigma(1) + sigma(3)*dNdx(3))*dV
          Fint(1,current%node_numbers(2)) = Fint(1,current%node_numbers(2)) + &
               (dNdx(1)*sigma(1) + sigma(3)*dNdx(3))*dV
          Fint(2,current%node_numbers(3)) = Fint(2,current%node_numbers(3)) - &
               dNdx(2)*sigma(2)*dV
          Fint(2,current%node_numbers(4)) = Fint(2,current%node_numbers(4)) + &
               dNdx(2)*sigma(2)*dV
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_fint


    SUBROUTINE solid1d_stIff()
    ! SOLID1D stIffness matrix
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    REAL(r8k) :: Ke(4,4),tmod(6),pfact
    REAL(r8k), POINTER :: dV
    REAL(r8k), DIMENSION(:), POINTER :: sigma, dNdx
    INTEGER(ipk) :: dofnum(4),i

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       dofnum(1) = dof_number(1,current%node_numbers(1))
       dofnum(2) = dof_number(2,current%node_numbers(3))
       dofnum(3) = dof_number(1,current%node_numbers(2))
       dofnum(4) = dof_number(2,current%node_numbers(4))
       pfact = mat_par(current%mat,tref,'EMOD    ')
       IF ( current%rigid ) THEN
          DO i=1,4
             CALL sparse_matrix_place_value(dofnum(i),dofnum(i),pfact)
          ENDDO
       ELSE
          ! Assign POINTERs
          dV => current%dV
          sigma => current%sigma(1:3)
          dNdx => current%dNdx(1:3)

          ! Material tangent modulus
          tmod = tmodulus(current%Je,current%mu,current%lambda,current%kappa, &
               current%sigdev,current%phyd,current%gamma,current%av, &
               current%taueff,current%dplmod,current%deds)

          ! material part of the tangent stIffness matrix
          Ke(1,1) = dV*dNdx(1)**2*tmod(1)-2.0_r8k*dV*dNdx(1)*dNdx(3)*tmod(6)+&
               dV*dNdx(3)**2*tmod(4)
          Ke(1,2) = dV*dNdx(2)*dNdx(1)*tmod(3)-dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(1,3) = -dV*dNdx(1)**2*tmod(1)+dV*dNdx(3)**2*tmod(4)
          Ke(1,4) = -dV*dNdx(2)*dNdx(1)*tmod(3)+dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(2,1) = dV*dNdx(2)*dNdx(1)*tmod(3)-dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(2,2) = dV*dNdx(2)**2*tmod(2)
          Ke(2,3) = -dV*dNdx(2)*dNdx(1)*tmod(3)-dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(2,4) = -dV*dNdx(2)**2*tmod(2)
          Ke(3,1) = -dV*dNdx(1)**2*tmod(1)+dV*dNdx(3)**2*tmod(4)
          Ke(3,2) = -dV*dNdx(2)*dNdx(1)*tmod(3)-dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(3,3) = dV*dNdx(1)**2*tmod(1)+2.0_r8k*dV*dNdx(1)*dNdx(3)*tmod(6)+&
               dV*dNdx(3)**2*tmod(4)
          Ke(3,4) = dV*dNdx(2)*dNdx(1)*tmod(3)+dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(4,1) = -dV*dNdx(2)*dNdx(1)*tmod(3)+dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(4,2) = -dV*dNdx(2)**2*tmod(2)
          Ke(4,3) = dV*dNdx(2)*dNdx(1)*tmod(3)+dV*dNdx(2)*dNdx(3)*tmod(5)
          Ke(4,4) = dV*dNdx(2)**2*tmod(2)

          ! geometric part of the tangent stIffness matrix
          Ke(1,1) = Ke(1,1) + &
               (dNdx(1)*sigma(1)*dNdx(1) + dNdx(3)*sigma(3)*dNdx(3))*dV
          Ke(2,2) = Ke(2,2) + dNdx(2)*sigma(2)*dNdx(2)*dV
          Ke(1,3) = Ke(1,3) + &
               (-dNdx(1)*sigma(1)*dNdx(1) + dNdx(3)*sigma(3)*dNdx(3))*dV
          Ke(3,1) = Ke(1,3)
          Ke(3,3) = Ke(3,3) + &
               (dNdx(1)*sigma(1)*dNdx(1) + dNdx(3)*sigma(3)*dNdx(3))*dV
          Ke(2,4) = Ke(2,4) - dNdx(2)*sigma(2)*dNdx(2)*dV
          Ke(4,2) = Ke(2,4)
          Ke(4,4) = Ke(4,4) + dNdx(2)*sigma(2)*dNdx(2)*dV

          CALL sparse_matrix_place_element(4,dofnum,Ke)
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_stIff


  FUNCTION tmodulus(Je,mu,lambda,kappa,sigdev,phyd,gamma,av, &
       taueff,dplmod,deds) RESULT(tmod)
    ! Calculate elastic-plastic tangent modulus
    IMPLICIT NONE
    INTEGER(ipk) :: i,k
    REAL(r8k) :: Je,mu,lambda,kappa,sigdev(3),phyd,gamma,taueff, &
         av(3),dplmod,deds,tmod(6),aik(3,3),bik,tmp(6),tmp2(3,3), &
         t2,t4,t7,t9,t12,t15,t16,t21,t22,t26,t27,t40,t44,t48,t57

    tmod = 0.0_r8k

    ! Use initial elastic matrix
    IF ( elastic_matrix ) THEN
       tmod(1) = 2.0_r8k*mu + lambda
       tmod(2) = 2.0_r8k*mu + lambda
       tmod(4) = 2.0_r8k*mu + lambda
       tmod(3) = lambda
       tmod(5) = lambda
       tmod(6) = lambda
       RETURN
    ENDIF

    ! Deviatoric part
    aik = -2.0_r8k*mu/3.0_r8k/Je
    DO i=1,3
       aik(i,i) = aik(i,i) + 2.0_r8k*mu/Je - 2.0_r8k*sigdev(i)
    ENDDO

    ! Creep part of the tangent modulus
    IF ( gamma > 1.0e-10_r8k ) THEN
       IF ( deds > 0.0_r8k ) THEN
          DO i=1,3
             DO k=1,3
                tmp2(i,k) = aik(i,k) - aik(i,k)*(3.0_r8k*mu*gamma/taueff) &
                     + 4.0_r8k*mu**2*av(i)*av(k)*gamma/taueff/Je
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
                     - 2.0_r8k*mu*gamma/taueff)/Je
             ENDDO
          ENDDO
       ENDIF
    ENDIF

    tmod(1) = aik(1,1)
    tmod(2) = aik(2,2)
    tmod(4) = aik(3,3)
    tmod(3) = aik(1,2)
    tmod(5) = aik(2,3)
    tmod(6) = aik(1,3)

    ! Hydrostatic part
    bik = kappa*(1.0_r8k - LOG(Je))/Je + phyd
    tmod(1) = tmod(1) + bik - 2.0_r8k*phyd
    tmod(2) = tmod(2) + bik - 2.0_r8k*phyd
    tmod(4) = tmod(4) + bik - 2.0_r8k*phyd
    tmod(3) = tmod(3) + bik
    tmod(5) = tmod(5) + bik
    tmod(6) = tmod(6) + bik

    RETURN

  END FUNCTION tmodulus


    SUBROUTINE solid1d_update()
    ! Update explicit values
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current
    REAL(r8k) :: epseff(1),sigeff(1),plSED(1),sigma(3,1)

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%rigid ) THEN
          current%plSED = current%plSED + 0.5_r8k*current%gamma* &
               (current%sigeff0 + current%sigeff)
          current%epspl0 = current%epspl
          current%epseff0 = current%epseff
          current%sigeff0 = current%sigeff
          epseff(1) = current%epseff0
          sigeff(1) = current%sigeff0
          plSED(1) = current%plSED
          sigma(1:3,1) = current%sigma(1:3)
          CALL mat_maximums(current%mat,epseff,sigeff,plSED,sigma)
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_update


    SUBROUTINE solid1d_strains()
    ! Calculate converged strain values
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( .NOT.current%rigid ) THEN
          ! Thermal dilation
          current%epsth = LOG(1.0_r8k/current%epsth)
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_strains


    SUBROUTINE solid1d_Write_output(nunit)
    ! WRITE element output
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(solid1d_type), POINTER :: current

    WRITE(nunit) nsolid1d
    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(nunit) current%label
       WRITE(nunit) current%egroup
       WRITE(nunit) current%mat
       WRITE(nunit) current%node_labels
       WRITE(nunit) current%node_numbers
       WRITE(nunit) current%rigid
       IF ( .NOT.current%rigid ) THEN
          WRITE(nunit) current%epspl0
          WRITE(nunit) current%epseff0
          WRITE(nunit) current%sigeff
          WRITE(nunit) current%sigma
          WRITE(nunit) current%phyd
          WRITE(nunit) current%epstot
          WRITE(nunit) current%epsth
          WRITE(nunit) current%plSED
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE solid1d_Write_output


    SUBROUTINE solid1d_read_output(nunit)
    ! Read element output
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(solid1d_type), POINTER :: current,previous

    Read(nunit,ERR=20,End=20) nsolid1d
    current => first_solid1d
    DO i=1,nsolid1d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_solid1d) ) THEN
             first_solid1d => current
             last_solid1d => current
          ELSE
             last_solid1d%next => current
             last_solid1d => current
          ENDIF
       ENDIF
       Read(nunit,ERR=20,End=20) current%label
       Read(nunit,ERR=20,End=20) current%egroup
       Read(nunit,ERR=20,End=20) current%mat
       Read(nunit,ERR=20,End=20) current%node_labels
       Read(nunit,ERR=20,End=20) current%node_numbers
       Read(nunit,ERR=20,End=20) current%rigid
       IF ( .NOT.current%rigid ) THEN
          Read(nunit,ERR=20,End=20) current%epspl0
          Read(nunit,ERR=20,End=20) current%epseff0
          Read(nunit,ERR=20,End=20) current%sigeff
          Read(nunit,ERR=20,End=20) current%sigma
          Read(nunit,ERR=20,End=20) current%phyd
          Read(nunit,ERR=20,End=20) current%epstot
          Read(nunit,ERR=20,End=20) current%epsth
          Read(nunit,ERR=20,End=20) current%plSED
       ELSE
          current%epspl0 = 0.0_r8k
          current%epseff0 = 0.0_r8k
          current%sigeff = 0.0_r8k
          current%sigma = 0.0_r8k
          current%phyd = 0.0_r8k
          current%epstot = 0.0_r8k
          current%epsth = 0.0_r8k
          current%plSED = 0.0_r8k
          current%sigma = 0.0_r8k
       ENDIF
       last_solid1d => current
       current => current%next
       nsolid1d = i
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_solid1d) ) last_solid1d%next => NULL()
    IF ( nsolid1d == 0 ) THEN
       first_solid1d => NULL()
       last_solid1d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading SOLID1D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE solid1d_read_output


    SUBROUTINE solid1d_deallocate()
    ! Deallocate element Database
    IMPLICIT NONE
    TYPE(solid1d_type), POINTER :: current,previous

    current => first_solid1d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE solid1d_deallocate
END MODULE solid1D













