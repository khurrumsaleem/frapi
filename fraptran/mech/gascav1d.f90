MODULE gascav1d
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> 1 1/2-Dimensional axisymmetric gas cavity
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
    !

  INTEGER(ipk) :: ngascav1d

  TYPE gascav1d_type
     INTEGER(ipk) :: label ! Element label
     INTEGER(ipk) :: number ! Internal element number
     INTEGER(ipk) :: gascav ! Gas cavity label
     INTEGER(ipk) :: node_labels(4) ! Element node labels
     INTEGER(ipk) :: node_numbers(4) ! Internal node numbering
     LOGICAL :: axload ! Flag wether axial load is calculated
     REAL(r8k) :: gtemp ! Integration point temperature
     REAL(r8k) :: dV ! Integration point volume
     REAL(r8k) :: fepp(2)
     REAL(r8k) :: dpdx(4)
     INTEGER(ipk) :: dofnum(4)
     TYPE(gascav1d_type), POINTER :: next
  END TYPE gascav1d_type

  TYPE(gascav1d_type), POINTER :: first_gascav1d,last_gascav1d

CONTAINS
    SUBROUTINE gascav1d_init()
    ! Initialize POINTERs and tables for element calculation
    IMPLICIT NONE

    ! Deallocate existing Database
    CALL gascav1d_deallocate()

    ! Initialize element number and POINTERs
    first_gascav1d => NULL()
    last_gascav1d => NULL()
    ngascav1d = 0

    RETURN

  END SUBROUTINE gascav1d_init


    SUBROUTINE gascav1d_create(label,gascav,nodes,axload)
    ! Create new GASCAV1D element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,gascav,nodes(4)
    LOGICAL, INTENT(IN) :: axload
    INTEGER(ipk) :: nfound,in,jn
    TYPE(gascav1d_type), POINTER :: new_gascav1d

    ! Allocate new element
    ngascav1d = ngascav1d + 1
    ALLOCATE(new_gascav1d)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,4
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_gascav1d%node_labels(jn) = nodes(jn)
             new_gascav1d%node_numbers(jn) = in
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 4 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 4 ) THEN
       WRITE(UNIT=6,FMT='(/,A,I0,/)') &
            'ERROR incompatible NODES and GASCAV1D Data in element ',label
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Initialize GASCAV1D variables
    new_gascav1d%label = label
    new_gascav1d%number = ngascav1d
    new_gascav1d%gascav = gascav
    new_gascav1d%axload = axload
    new_gascav1d%next => NULL()

    ! New DOF numbering is needed
    IF ( pressure_matrix ) dof_numbering = .TRUE.

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_gascav1d) ) THEN
       first_gascav1d => new_gascav1d
       last_gascav1d => new_gascav1d
    ELSE
       last_gascav1d%next => new_gascav1d
       last_gascav1d => new_gascav1d
    ENDIF

    RETURN

  END SUBROUTINE gascav1d_create


    SUBROUTINE gascav1d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(gascav1d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound


    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_gascav1d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_gascav1d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_gascav1d
       first_gascav1d => first_gascav1d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_gascav1d
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

    ! Set the POINTER to the last GASCAV1D element
    last_gascav1d => NULL()
    current => first_gascav1d
    DO WHILE ( ASSOCIATED(current) )
       last_gascav1d => current
       current => current%next
    ENDDO
    
    ! Deallocate GASCAV1D element
    IF ( lfound ) THEN
       ngascav1d = ngascav1d - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE gascav1d_delete


    SUBROUTINE gascav1d_sparse_matrix()
    ! Initialize sparse matrix storage for gascav1d elements
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current
    TYPE(gascav_type), POINTER :: current_cav
    INTEGER(ipk) :: ndofnum
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: dofnum

    IF ( pressure_matrix ) THEN
       ALLOCATE(dofnum(4*ngascav1d))
       current_cav => first_gascav
       DO WHILE ( ASSOCIATED(current_cav) )
          ! Find all elements that belong to this gas cavity and create a
          ! superelement from all of these elements
          ndofnum = 0
          current => first_gascav1d
          DO WHILE ( ASSOCIATED(current) )
             IF ( current%gascav == current_cav%label ) THEN
                dofnum(ndofnum+1) = dof_number(1,current%node_numbers(1))
                dofnum(ndofnum+2) = dof_number(1,current%node_numbers(2))
                dofnum(ndofnum+3) = dof_number(2,current%node_numbers(3))
                dofnum(ndofnum+4) = dof_number(2,current%node_numbers(4))
                ndofnum = ndofnum + 4
             ENDIF
             current => current%next
          ENDDO
          CALL sparse_matrix_add_element(ndofnum,dofnum)
          current_cav => current_cav%next
       ENDDO
       DEALLOCATE(dofnum)
    ENDIF

    RETURN

  END SUBROUTINE gascav1d_sparse_matrix


    SUBROUTINE gascav1d_temp()
    ! Initialize temperatures
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current

    current => first_gascav1d
    DO WHILE ( ASSOCIATED(current) )
       ! Integration point coordinate
       cp(1) = 0.5_r8k*(x0(1,current%node_numbers(1)) + &
            x0(1,current%node_numbers(2)))
       cp(2) = 0.5_r8k*(x0(2,current%node_numbers(3)) + &
            x0(2,current%node_numbers(4)))
       cp(3) = 0.0_r8k

       ! GP temperature
       current%gtemp = 0.5_r8k*(temp(current%node_numbers(1)) + &
            temp(current%node_numbers(2)))
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_temp


    SUBROUTINE gascav1d_deriv()
    ! Evaluate cartesian derivatives of the shape functions
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current

    current => first_gascav1d
    DO WHILE ( ASSOCIATED(current) )
       ! Integration point weight factor
       current%dV = pi*(x(1,current%node_numbers(2))**2 - &
            x(1,current%node_numbers(1))**2)* &
            (x(2,current%node_numbers(4)) - x(2,current%node_numbers(3)))
       current%dV = MAX(current%dV,0.0_r8k)
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_deriv


    SUBROUTINE gascav1d_vpt()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    TYPE(gascav_type), POINTER :: current_cav
    TYPE(gascav1d_type), POINTER :: current

    CALL gascav1d_deriv()

    current_cav => first_gascav
    DO WHILE ( ASSOCIATED(current_cav) )
       current => first_gascav1d
       DO WHILE ( ASSOCIATED(current) )
          IF ( current_cav%label == current%gascav ) THEN
             current_cav%VpT = current_cav%VpT + current%dV/current%gtemp
             current_cav%V_tot = current_cav%V_tot + current%dV
          ENDIF
          current => current%next
       ENDDO
       current_cav => current_cav%next
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_vpt


    SUBROUTINE gascav1d_fext()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current
    TYPE(gascav_type), POINTER :: current_cav
    REAL(r8k) :: dz,dA

    current => first_gascav1d
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
       dz = x(2,current%node_numbers(4)) - x(2,current%node_numbers(3))
       current%fepp(1) = -2.0_r8k*pi*x(1,current%node_numbers(1))*dz
       current%fepp(2) = 2.0_r8k*pi*x(1,current%node_numbers(2))*dz
       Fext(1,current%node_numbers(1)) = Fext(1,current%node_numbers(1)) + &
            current%fepp(1)*current_cav%p
       Fext(1,current%node_numbers(2)) = Fext(1,current%node_numbers(2)) + &
            current%fepp(2)*current_cav%p
       IF ( current%axload ) THEN
          dA = x(1,current%node_numbers(2))**2-x(1,current%node_numbers(1))**2
          dA = pi*MAX(dA,0.0_r8k)
          Fext(2,current%node_numbers(3)) = Fext(2,current%node_numbers(3)) - &
               dA*current_cav%p
          Fext(2,current%node_numbers(4)) = Fext(2,current%node_numbers(4)) + &
               dA*current_cav%p
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_fext


    SUBROUTINE gascav1d_stIff()
    ! Stiffness matrix due to GASCAV1D elements
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current,current2
    TYPE(gascav_type), POINTER :: current_cav
    INTEGER(ipk) :: i,j,k
    REAL(r8k) :: p,mgas,VpT,value_ij,tmp,dz

    IF ( pressure_matrix ) THEN

       IF ( ngascav1d > 0 ) symmetric_matrix = .FALSE.

       current => first_gascav1d
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
          tmp = -current_cav%mgas*pi/(current_cav%VpT**2*current%gtemp)
          dz = x(2,current%node_numbers(4)) - x(2,current%node_numbers(3))
          current%dpdx(1) = -tmp*2.0_r8k*x(1,current%node_numbers(1))*dz
          current%dpdx(2) = tmp*2.0_r8k*x(1,current%node_numbers(2))*dz
          current%dpdx(3) = -tmp*pi*(x(1,current%node_numbers(2))**2 - &
               x(1,current%node_numbers(1))**2)
          current%dpdx(4) = -current%dpdx(3)
          current%dofnum(1) = dof_number(1,current%node_numbers(1))
          current%dofnum(2) = dof_number(1,current%node_numbers(2))
          current%dofnum(3) = dof_number(2,current%node_numbers(3))
          current%dofnum(4) = dof_number(2,current%node_numbers(4))
          current => current%next
       ENDDO


       current => first_gascav1d
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
          tmp = 2.0_r8k*pi*p
          dz = x(2,current%node_numbers(4)) - x(2,current%node_numbers(3))
          i = dof_number(1,current%node_numbers(1))
          value_ij = tmp*dz
          CALL sparse_matrix_place_value(i,i,value_ij)
          j = dof_number(2,current%node_numbers(3))
          value_ij = -tmp*x(1,current%node_numbers(1))
          CALL sparse_matrix_place_value(i,j,value_ij)
          j = dof_number(2,current%node_numbers(4))
          value_ij = tmp*x(1,current%node_numbers(1))
          CALL sparse_matrix_place_value(i,j,value_ij)

          i = dof_number(1,current%node_numbers(2))
          value_ij = -tmp*dz
          CALL sparse_matrix_place_value(i,i,value_ij)
          j = dof_number(2,current%node_numbers(3))
          value_ij = tmp*x(1,current%node_numbers(2))
          CALL sparse_matrix_place_value(i,j,value_ij)
          j = dof_number(2,current%node_numbers(4))
          value_ij = -tmp*x(1,current%node_numbers(2))
          CALL sparse_matrix_place_value(i,j,value_ij)

          IF ( .NOT.current_cav%failed ) THEN
             current2 => first_gascav1d
             DO WHILE ( ASSOCIATED(current2) )
                IF ( current2%gascav == current%gascav ) THEN
                   DO k=1,4
                      j = current2%dofnum(k)
                      i = dof_number(1,current%node_numbers(1))
                      value_ij = -current%fepp(1)*current2%dpdx(k)
                      CALL sparse_matrix_place_value(i,j,value_ij)
                      i = dof_number(1,current%node_numbers(2))
                      value_ij = -current%fepp(2)*current2%dpdx(k)
                      CALL sparse_matrix_place_value(i,j,value_ij)
                   ENDDO
                ENDIF
                current2 => current2%next
             ENDDO
          ENDIF

          current => current%next
       ENDDO
    ENDIF

    RETURN

  END SUBROUTINE gascav1d_stIff


    SUBROUTINE gascav1d_Write_output(nunit)
    ! WRITE pressure Data
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(gascav1d_type), POINTER :: current

    WRITE(UNIT=nunit) ngascav1d
    current => first_gascav1d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%gascav
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_Write_output


    SUBROUTINE gascav1d_read_output(nunit)
    ! Read output from unit 'nunit'
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(gascav1d_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) ngascav1d

    current => first_gascav1d
    DO i=1,ngascav1d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_gascav1d) ) THEN
             first_gascav1d => current
             last_gascav1d => current
          ELSE
             last_gascav1d%next => current
             last_gascav1d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%gascav
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       last_gascav1d => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_gascav1d) ) last_gascav1d%next => NULL()
    IF ( ngascav1d == 0 ) THEN
       first_gascav1d => NULL()
       last_gascav1d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading GASCAV1D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE gascav1d_read_output


    SUBROUTINE gascav1d_deallocate()
    ! Deallocate gascav1d Database
    IMPLICIT NONE
    TYPE(gascav1d_type), POINTER :: current,previous

    current => first_gascav1d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE gascav1d_deallocate
END MODULE gascav1d
