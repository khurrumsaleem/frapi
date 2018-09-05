MODULE pressure1d
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> Axisymmetric pressure boundary element for SOLID1D element
    !> Nodes 1, 2, and 3 must be connected to the same solid element
    !
    ! 2
    ! o--+
    !    |
    !    |
    !  3 o<------ p
    !    |
    !    |
    ! o--+
    ! 1
    !

  INTEGER(ipk) :: npressure1d

  TYPE pressure1d_type
     INTEGER(ipk) :: label
     INTEGER(ipk) :: idir ! Direction of the pressure, 1 = radial, 2 = axial
     INTEGER(ipk) :: node_labels(3) ! Element node labels
     INTEGER(ipk) :: node_numbers(3) ! Internal node numbering
     REAL(r8k) :: p0 ! Explicit value for the pressure load
     REAL(r8k) :: p ! Pressure at the End of current load step
     REAL(r8k) :: p_End ! Pressure at the End of load step
     TYPE(pressure1d_type), POINTER :: next
  END TYPE pressure1d_type

  TYPE(pressure1d_type), POINTER :: first_pressure1d,last_pressure1d

CONTAINS
    SUBROUTINE pressure1d_init()
    ! Initialize pressure1d Database
    IMPLICIT NONE

    ! Deallocate, If Databese already exists
    CALL pressure1d_deallocate()

    ! Initialize POINTERs and counters
    npressure1d = 0
    first_pressure1d => NULL()
    last_pressure1d => NULL()

    RETURN

  END SUBROUTINE pressure1d_init


    SUBROUTINE pressure1d_setpress(label,p)
    ! Set pressures for the set label
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k) :: p
    TYPE(pressure1d_type), POINTER :: current

    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) current%p_End = p
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure1d_setpress


    SUBROUTINE pressure1d_updval(label,nodes,p)
    ! Update pressure values
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,nodes(3)
    REAL(r8k), INTENT(IN) :: p
    TYPE(pressure1d_type), POINTER :: current

    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) THEN
          IF ( current%node_labels(3) == nodes(3) ) THEN
             current%p_End = p
             EXIT
          ENDIF
       ENDIF
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) Call pressure1d_create(label,nodes,p)

    RETURN

  END SUBROUTINE pressure1d_updval


    SUBROUTINE pressure1d_create(label,nodes,p)
    ! Create new PRESSURE1D element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,nodes(3)
    REAL(r8k), INTENT(IN) :: p
    INTEGER(ipk) :: nfound,in,jn
    TYPE(pressure1d_type), POINTER :: new_pressure1d

    ! Allocate new element
    npressure1d = npressure1d + 1
    ALLOCATE(new_pressure1d)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,3
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_pressure1d%node_labels(jn) = nodes(jn)
             new_pressure1d%node_numbers(jn) = in
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 3 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 3 ) THEN
       WRITE(UNIT=6,FMT='(/,A,/)') &
            'ERROR incompatible NODES and PRESS1D Data'
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Initialize PRESSURE1D variables
    IF ( get_node_status(nodes(3),1) == -1 ) THEN
       new_pressure1d%idir = 2
    ELSE
       new_pressure1d%idir = 1
    ENDIF
    new_pressure1d%label = label
    new_pressure1d%p0 = 0.0_r8k
    new_pressure1d%p_End = p
    new_pressure1d%next => NULL()

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_pressure1d) ) THEN
       first_pressure1d => new_pressure1d
       last_pressure1d => new_pressure1d
    ELSE
       last_pressure1d%next => new_pressure1d
       last_pressure1d => new_pressure1d
    ENDIF

    RETURN

  END SUBROUTINE pressure1d_create


    SUBROUTINE pressure1d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(pressure1d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_pressure1d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_pressure1d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_pressure1d
       first_pressure1d => first_pressure1d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_pressure1d
    DO WHILE ( ( ASSOCIATED(current) ).AND.( .NOT.lfound ) )
       IF ( current%label == label ) THEN
          lfound = .TRUE.
          previous%next => current%next
          tobedeleted => current
       ENDIF
       previous => current
       current => current%next
    ENDDO

    ! Set the POINTER to the last PRESSURE1D element
    last_pressure1d => NULL()
    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       last_pressure1d => current
       current => current%next
    ENDDO
    
    ! Deallocate PRESSURE1D element
    IF ( lfound ) THEN
       npressure1d = npressure1d - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE pressure1d_delete


    SUBROUTINE pressure1d_fext()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    TYPE(pressure1d_type), POINTER :: current
    REAL(r8k) :: r(2),z(2)
    REAL(r8k), POINTER :: p

    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       p => current%p
       p = current%p0 + dload*(current%p_End - current%p0)
       IF ( current%idir == 1 ) THEN
          r(1) = x(1,current%node_numbers(3))
          z(1) = x(2,current%node_numbers(1))
          z(2) = x(2,current%node_numbers(2))
          Fext(1,current%node_numbers(3)) = Fext(1,current%node_numbers(3)) - &
               2.0_r8k*pi*r(1)*(z(2)-z(1))*p
       ELSE
          r(2) = x(1,current%node_numbers(1))
          r(1) = x(1,current%node_numbers(2))
          Fext(2,current%node_numbers(3)) = Fext(2,current%node_numbers(3)) - &
               pi*(r(2)**2 - r(1)**2)*p
       ENDIF
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure1d_fext


    SUBROUTINE pressure1d_stIff()
    ! Stiffness matrix for the pressure loads
    IMPLICIT NONE
    TYPE(pressure1d_type), POINTER :: current
    INTEGER(ipk) dofnum(3),i
    REAL(r8k) :: r(2),z(2),p,Ke(3)

    IF ( pressure_matrix ) THEN

       IF ( npressure1d > 0 ) symmetric_matrix = .FALSE.

       current => first_pressure1d
       DO WHILE ( ASSOCIATED(current) )
          p = current%p
          IF ( current%idir == 1 ) THEN
             r(1) = x(1,current%node_numbers(3))
             z(1) = x(2,current%node_numbers(1))
             z(2) = x(2,current%node_numbers(2))
             Ke = 0.0_r8k
             Ke(1) = -2.0_r8k*pi*r(1)*p
             Ke(2) = 2.0_r8k*pi*r(1)*p
             Ke(3) = 2.0_r8k*pi*(z(2)-z(1))*p
             dofnum(1) = dof_number(2,current%node_numbers(1))
             dofnum(2) = dof_number(2,current%node_numbers(2))
             dofnum(3) = dof_number(1,current%node_numbers(3))
          ELSE
             r(2) = x(1,current%node_numbers(1))
             r(1) = x(1,current%node_numbers(2))
             Ke(1) = -2.0_r8k*pi*r(1)*p
             Ke(2) = 2.0_r8k*pi*r(1)*p
             Ke(3) = 0.0_r8k
             dofnum(1) = dof_number(1,current%node_numbers(2))
             dofnum(2) = dof_number(1,current%node_numbers(1))
             dofnum(3) = 0
          ENDIF

          ! Place element matrix in the global matrix
          DO i=1,3
             CALL sparse_matrix_place_value(dofnum(3),dofnum(i),Ke(i))
          ENDDO

          current => current%next
       ENDDO
    ENDIF

    RETURN

  END SUBROUTINE pressure1d_stIff


    SUBROUTINE pressure1d_update()
    ! Update explicit values
    IMPLICIT NONE
    TYPE(pressure1d_type), POINTER :: current

    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       current%p0 = current%p
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure1d_update


    SUBROUTINE pressure1d_Write_output(nunit)
    ! WRITE pressure Data
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(pressure1d_type), POINTER :: current

    WRITE(UNIT=nunit) npressure1d
    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%idir
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       WRITE(UNIT=nunit) current%p0
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure1d_Write_output


    SUBROUTINE pressure1d_read_output(nunit)
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(pressure1d_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) npressure1d
    current => first_pressure1d
    DO i=1,npressure1d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_pressure1d) ) THEN
             first_pressure1d => current
             last_pressure1d => current
          ELSE
             last_pressure1d%next => current
             last_pressure1d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%idir
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       Read(UNIT=nunit,ERR=20,End=20) current%p0
       current%p_End = current%p0
       last_pressure1d => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_pressure1d) ) last_pressure1d%next => NULL()
    IF ( npressure1d == 0 ) THEN
       first_pressure1d => NULL()
       last_pressure1d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading PRESS1D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE pressure1d_read_output


    SUBROUTINE pressure1d_deallocate()
    ! Deallocate pressure1d Database
    IMPLICIT NONE
    TYPE(pressure1d_type), POINTER :: current,previous

    current => first_pressure1d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE pressure1d_deallocate
END MODULE pressure1d


