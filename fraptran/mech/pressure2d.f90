MODULE pressure2d
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> Axisymmetric pressure boundary element
    !> Nodes 1 and 2 must be connected to the same solid element
    !
    ! 1
    ! o<-- p(1)
    ! |<---
    ! |<----
    ! |<-----
    ! |<------
    ! o<------- p(2)
    ! 2
    !

  INTEGER(ipk) :: npressure2d

  TYPE pressure2d_type
     INTEGER(ipk) :: label
     INTEGER(ipk) :: node_labels(2)
     INTEGER(ipk) :: node_numbers(2)
     REAL(r8k) :: p0(2)
     REAL(r8k) :: p(2)
     REAL(r8k) :: p_End(2)
     TYPE(pressure2d_type), POINTER :: next
  END TYPE pressure2d_type

  TYPE(pressure2d_type), POINTER :: first_pressure2d,last_pressure2d

  REAL(r8k), PRIVATE :: N(2,2),wgt(2),xi(2)

CONTAINS
    SUBROUTINE pressure2d_init()
    ! Initialize pressure2d Database
    IMPLICIT NONE
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate, If Databese already exists
    CALL pressure2d_deallocate()

    ! Initialize POINTERs and counters
    npressure2d = 0
    first_pressure2d => NULL()
    last_pressure2d => NULL()

    ! Integration points and weight factors
    wgt = 0.5_r8k
    tmp = 1.0_r8k/SQRT(3.0_r8k)
    xi(1) = 0.5_r8k*(1.0_r8k - tmp)
    xi(2) = 0.5_r8k*(1.0_r8k + tmp)

    DO ig=1,2
       N(1,ig) = 1.0_r8k - xi(ig)
       N(2,ig) = xi(ig)
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_init


    SUBROUTINE pressure2d_setpress(label,p)
    ! Set pressures for the set label
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k) :: p
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) current%p_End(1:2) = p
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_setpress


    SUBROUTINE pressure2d_updval(label,nodes,p)
    ! Update pressure values
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,nodes(2)
    REAL(r8k), INTENT(IN) :: p(2)
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       IF ( current%label == label ) THEN
          IF ( current%node_labels(1) == nodes(1) ) THEN
             IF ( current%node_labels(2) == nodes(2) ) THEN
                current%p_End = p
                EXIT
             ENDIF
          ENDIF
       ENDIF
       current => current%next
    ENDDO

    IF ( .NOT.ASSOCIATED(current) ) Call pressure2d_create(label,nodes,p)

    RETURN

  END SUBROUTINE pressure2d_updval


    SUBROUTINE pressure2d_create(label,nodes,p)
    ! Create new PRESSURE2D element
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label,nodes(2)
    REAL(r8k), INTENT(IN) :: p(2)
    INTEGER(ipk) :: nfound,in,jn
    TYPE(pressure2d_type), POINTER :: new_pressure2d

    ! Allocate new element
    npressure2d = npressure2d + 1
    ALLOCATE(new_pressure2d)

    ! Find element nodes in node Database
    nfound = 0
    DO in=1,nnodes
       DO jn=1,2
          IF ( nodes(jn) == node_labels(in) ) THEN
             new_pressure2d%node_labels(jn) = nodes(jn)
             new_pressure2d%node_numbers(jn) = in
             nfound = nfound + 1
          ENDIF
       ENDDO
       IF ( nfound == 2 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 2 ) THEN
       WRITE(UNIT=6,FMT='(/,A,/)') &
            'ERROR incompatible NODES and PRESSURE2D Data'
       lerror = .TRUE.
       CALL nlfemp_stop(0)
    ENDIF

    ! Initialize PRESSURE2D variables
    new_pressure2d%label = label
    new_pressure2d%p0(1:2) = 0.0_r8k
    new_pressure2d%p_End(1:2) = p(1:2)
    new_pressure2d%next => NULL()

    ! Add new element to the Database
    IF ( .NOT.ASSOCIATED(first_pressure2d) ) THEN
       first_pressure2d => new_pressure2d
       last_pressure2d => new_pressure2d
    ELSE
       last_pressure2d%next => new_pressure2d
       last_pressure2d => new_pressure2d
    ENDIF

    RETURN

  END SUBROUTINE pressure2d_create


    SUBROUTINE pressure2d_delete(label)
    ! Delete element from the Database
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(pressure2d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_pressure2d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_pressure2d%label == label ) THEN
       lfound = .TRUE.
       tobedeleted => first_pressure2d
       first_pressure2d => first_pressure2d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_pressure2d
    DO WHILE ( ( ASSOCIATED(current) ).AND.( .NOT.lfound ) )
       IF ( current%label == label ) THEN
          lfound = .TRUE.
          previous%next => current%next
          tobedeleted => current
       ENDIF
       previous => current
       current => current%next
    ENDDO

    ! Set the POINTER to the last PRESSURE2D element
    last_pressure2d => NULL()
    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       last_pressure2d => current
       current => current%next
    ENDDO
    
    ! Deallocate PRESSURE2D element
    IF ( lfound ) THEN
       npressure2d = npressure2d - 1
       dof_numbering = .TRUE.
       DEALLOCATE(tobedeleted)
    ENDIF

    RETURN

  END SUBROUTINE pressure2d_delete


    SUBROUTINE pressure2d_fext()
    ! Calculate 2D pressure load contribution to the external force vector
    IMPLICIT NONE
    TYPE(pressure2d_type), POINTER :: current
    INTEGER(ipk) :: ig
    REAL(r8k) :: r(2),z(2),rg,pg
    REAL(r8k), DIMENSION(:), POINTER :: p

    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       p => current%p(1:2)
       p = current%p0 + dload*(current%p_End - current%p0)
       r(1) = x(1,current%node_numbers(1))
       r(2) = x(1,current%node_numbers(2))
       z(1) = x(2,current%node_numbers(1))
       z(2) = x(2,current%node_numbers(2))
       DO ig=1,2
          pg = N(1,ig)*p(1) + N(2,ig)*p(2)
          rg = N(1,ig)*r(1) + N(2,ig)*r(2)
          Fext(1,current%node_numbers(1)) = Fext(1,current%node_numbers(1)) + &
               wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
          Fext(2,current%node_numbers(1)) = Fext(2,current%node_numbers(1)) + &
               wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
          Fext(1,current%node_numbers(2)) = Fext(1,current%node_numbers(2)) + &
               wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*rg
          Fext(2,current%node_numbers(2)) = Fext(2,current%node_numbers(2)) + &
               wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*rg
       ENDDO
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_fext


    SUBROUTINE pressure2d_stIff()
    ! Stiffness matrix for the pressure loads
    IMPLICIT NONE
    TYPE(pressure2d_type), POINTER :: current
    INTEGER(ipk) ig,dofnum(4)
    REAL(r8k) :: r(2),z(2),pg,rg,Ke(4,4)
    REAL(r8k), DIMENSION(:), POINTER :: p

    IF ( pressure_matrix ) THEN

       IF ( npressure2d > 0 ) symmetric_matrix = .FALSE.

       current => first_pressure2d
       DO WHILE ( ASSOCIATED(current) )
          p => current%p(1:2)
          p = current%p0 + dload*(current%p_End - current%p0)
          r(1) = x(1,current%node_numbers(1))
          r(2) = x(1,current%node_numbers(2))
          z(1) = x(2,current%node_numbers(1))
          z(2) = x(2,current%node_numbers(2))
          Ke = 0.0_r8k
          DO ig=1,2
             pg = N(1,ig)*p(1) + N(2,ig)*p(2)
             rg = N(1,ig)*r(1) + N(2,ig)*r(2)
             Ke(1,1) = Ke(1,1) - &
                  wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*N(1,ig)
             Ke(1,2) = Ke(1,2) + wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg
             Ke(1,3) = Ke(1,3) - &
                  wgt(ig)*pg*N(1,ig)*(z(2) - z(1))*2.0_r8k*pi*N(2,ig)
             Ke(1,4) = Ke(1,4) - wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg
             Ke(2,1) = Ke(2,1) - wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg - &
                  wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*N(1,ig)
             Ke(2,3) = Ke(2,3) + wgt(ig)*pg*N(1,ig)*2.0_r8k*pi*rg - &
                  wgt(ig)*pg*N(1,ig)*(r(1) - r(2))*2.0_r8k*pi*N(2,ig)
             Ke(3,1) = Ke(3,1) - &
                  wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*N(1,ig)
             Ke(3,2) = Ke(3,2) + wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg
             Ke(3,3) = Ke(3,3) - &
                  wgt(ig)*pg*N(2,ig)*(z(2) - z(1))*2.0_r8k*pi*N(2,ig)
             Ke(3,4) = Ke(3,4) - wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg
             Ke(4,1) = Ke(4,1) - wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg - &
                  wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*N(1,ig)
             Ke(4,3) = Ke(4,3) + wgt(ig)*pg*N(2,ig)*2.0_r8k*pi*rg + &
                  wgt(ig)*pg*N(2,ig)*(r(1) - r(2))*2.0_r8k*pi*N(2,ig)
          ENDDO

          ! Place element matrix in the global matrix
          dofnum(1:2) = dof_number(1:2,current%node_numbers(1))
          dofnum(3:4) = dof_number(1:2,current%node_numbers(2))
          CALL sparse_matrix_place_element(4,dofnum,Ke)
          current => current%next
       ENDDO
    ENDIF

    RETURN

  END SUBROUTINE pressure2d_stIff


    SUBROUTINE pressure2d_update()
    ! Update explicit values
    IMPLICIT NONE
    TYPE(pressure2d_type), POINTER :: current

    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       current%p0 = current%p
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_update


    SUBROUTINE pressure2d_Write_output(nunit)
    ! WRITE pressure Data
    IMPLICIT NONE
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(pressure2d_type), POINTER :: current

    WRITE(UNIT=nunit) npressure2d
    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       WRITE(UNIT=nunit) current%label
       WRITE(UNIT=nunit) current%node_labels
       WRITE(UNIT=nunit) current%node_numbers
       WRITE(UNIT=nunit) current%p0
       current => current%next
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_Write_output


    SUBROUTINE pressure2d_read_output(nunit)
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(pressure2d_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) npressure2d

    current => first_pressure2d
    DO i=1,npressure2d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_pressure2d) ) THEN
             first_pressure2d => current
             last_pressure2d => current
          ELSE
             last_pressure2d%next => current
             last_pressure2d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       Read(UNIT=nunit,ERR=20,End=20) current%p0
       current%p_End = current%p0
       last_pressure2d => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_pressure2d) ) last_pressure2d%next => NULL()
    IF ( npressure2d == 0 ) THEN
       first_pressure2d => NULL()
       last_pressure2d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading PRESS2D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

  END SUBROUTINE pressure2d_read_output


    SUBROUTINE pressure2d_deallocate()
    ! Deallocate pressure2d Database
    IMPLICIT NONE
    TYPE(pressure2d_type), POINTER :: current,previous

    current => first_pressure2d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    RETURN

  END SUBROUTINE pressure2d_deallocate
END MODULE pressure2d


