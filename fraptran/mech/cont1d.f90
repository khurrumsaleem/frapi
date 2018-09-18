MODULE cont1d_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    USE materials_frap_fraptran
    USE sparse_matrix_fraptran
    IMPLICIT NONE
    !>@brief
    !> 1D contact element
    !
    ! 3        4
    ! o--+  +--o--
    !    |  |
    !    |  |
    !  1 o  o 2
    !    |  |
    !    |  |
    ! o--+  +--o--
    !

    INTEGER(ipk) :: ncont1d ! Number of CONT1D elements

    TYPE cont1d_type
        INTEGER(ipk) :: label ! Element label
        INTEGER(ipk) :: number ! Internal element number
        INTEGER(ipk) :: mat ! Material label
        LOGICAL :: closed ! Flag for gap status
                          !   True = contact
                          !   False = Open gap
        LOGICAL :: closed0 ! Explicit value for closed
        LOGICAL :: stick ! Flag for tangential sticking status
                         !   True = fixed contact
                         !   False = slipping
        LOGICAL :: stick0 ! Explicit value for stick
        INTEGER(ipk) :: node_labels(4) ! Element node labels
        INTEGER(ipk) :: node_numbers(4) ! Internal node numbering
        REAL(r8k) :: gap ! Gap function or shortest distance from contactor node to the target surface
        REAL(r8k) :: gap0 ! Explicit value for gap
        REAL(r8k) :: dz ! difference in elevations at first contact
        REAL(r8k) :: dz0 ! Explicit value for dz 
        REAL(r8k) :: pfact(2) ! Penalty factor
        TYPE(cont1d_type), POINTER :: next
    END TYPE cont1d_type

    TYPE(cont1d_type), POINTER :: first_cont1d,last_cont1d

    CONTAINS
    !
    SUBROUTINE cont1d_init()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Initialize cont1d Database

    ! Deallocate, Iff Databese already exists
    CALL cont1d_deallocate()

    ! Initialize POINTERs and counters
    ncont1d = 0
    first_cont1d => NULL()
    last_cont1d => NULL()

    END SUBROUTINE cont1d_init
    !
    !
    !
    SUBROUTINE cont1d_create (label, mat, nodes)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Create new element
    INTEGER(ipk), INTENT(IN) :: label, mat, nodes(4)
    INTEGER(ipk) :: nfound, in, jn
    TYPE(cont1d_type), POINTER :: new_cont1d, current

    ncont1d = ncont1d + 1
    ALLOCATE(new_cont1d)
    new_cont1d%label = label
    new_cont1d%number = ncont1d
    new_cont1d%mat = mat
    CALL mat_create(mat)

    ! Find element nodes in node Database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 4
            IF ( nodes(jn) == node_labels(in) ) THEN
                new_cont1d%node_labels(jn) = nodes(jn)
                new_cont1d%node_numbers(jn) = in
                nfound = nfound + 1
            ENDIF
        ENDDO
        IF ( nfound == 4 ) EXIT
    ENDDO

    ! Check whether all element nodes were found in node Database
    IF ( nfound /= 4 ) THEN
        WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR incompatible NODES and CONT1D Data'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    ENDIF

    ! Check that the element label doesn't already exist
    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        IF ( current%label == label ) THEN
            WRITE(UNIT=6,FMT='(/,A,I0,A,/)') 'ERROR CONT1D ',label,' has been defined twice'
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        ENDIF
        current => current%next
    ENDDO

    ! Initialize CONT1D variables
    new_cont1d%closed0 = .FALSE.
    new_cont1d%stick0 = .FALSE.
    new_cont1d%dz0 = x(2,new_cont1d%node_numbers(3)) - x(2,new_cont1d%node_numbers(4)) 
    new_cont1d%next => NULL()

    ! Check for contact conditions
    IF ( x(1,new_cont1d%node_numbers(1)) >= x(1,new_cont1d%node_numbers(2)) ) new_cont1d%closed0 = .TRUE.

    ! New DOF numbering is needed
    dof_numbering = .TRUE.

    ! Add element to the CONT1D Database
    IF ( .NOT.ASSOCIATED(first_cont1d) ) THEN
        first_cont1d => new_cont1d
        last_cont1d => new_cont1d
    ELSE
        last_cont1d%next => new_cont1d
        last_cont1d => new_cont1d
    ENDIF

    END SUBROUTINE cont1d_create
    !
    !
    !
    SUBROUTINE cont1d_delete (label)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Delete element from the Database
    INTEGER(ipk), INTENT(IN) :: label
    TYPE(cont1d_type), POINTER :: current,previous,tobedeleted
    LOGICAL :: lfound

    lfound = .FALSE.

    ! Do nothing If the Database is empty
    IF ( .NOT.ASSOCIATED(first_cont1d) ) RETURN

    ! Mark first memeber to be deleted in Database
    IF ( first_cont1d%label == label ) THEN
        lfound = .TRUE.
        tobedeleted => first_cont1d
        first_cont1d => first_cont1d%next
    ENDIF

    ! Search for the to be deleted member and renumber rest of the members
    current => first_cont1d
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

    ! Set the POINTER to the last CONT1D element
    last_cont1d => NULL()
    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        last_cont1d => current
        current => current%next
    ENDDO
    
    ! Deallocate CONT1D element
    IF ( lfound ) THEN
        ncont1d = ncont1d - 1
        dof_numbering = .TRUE.
        DEALLOCATE(tobedeleted)
    ENDIF

    END SUBROUTINE cont1d_delete
    !
    !
    !
    SUBROUTINE cont1d_sparse_matrix()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for cont1d elements
    TYPE(cont1d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(4)

    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        dofnum(1) = dof_number(1,current%node_numbers(1))
        dofnum(2) = dof_number(1,current%node_numbers(2))
        dofnum(3) = dof_number(2,current%node_numbers(3))
        dofnum(4) = dof_number(2,current%node_numbers(4))
        CALL sparse_matrix_add_element(4,dofnum)
        current => current%next
    ENDDO

    END SUBROUTINE cont1d_sparse_matrix
    !
    !
    !
    SUBROUTINE cont1d_fint()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Contact forces
    TYPE(cont1d_type), POINTER :: current
    REAL(r8k) :: Fn, Ft, frcoef
    REAL(r8k), DIMENSION(:), POINTER :: pfact

    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        ! Assign POINTERs
        pfact => current%pfact(1:2)

        ! Gap
        current%gap = x(1,current%node_numbers(2)) - x(1,current%node_numbers(1))

        ! difference in elevations
        current%dz = x(2,current%node_numbers(3)) - x(2,current%node_numbers(4))

        ! Penalty factors
        pfact(1) = mat_par(current%mat,tref,'PENALTY ')
        pfact(2) = mat_par(current%mat,tref,'PENALTY2')
        IF ( pfact(2) < 1.0_r8k ) pfact(2) = pfact(1)
       

        ! Check for opening of the gap
        current%closed = .FALSE.
        current%stick = .FALSE.
        IF ( current%gap <= 0.0_r8k ) THEN
            ! Contact flag
            current%closed = .TRUE.

            ! Normal penalty
            Fn = pfact(1)*current%gap
            !Fint(1,current%node_numbers(1)) = Fint(1,current%node_numbers(1))-Fn
            !Fint(1,current%node_numbers(2)) = Fint(1,current%node_numbers(2))+Fn
            Fext(1,current%node_numbers(1)) = Fext(1,current%node_numbers(1))+Fn
            Fext(1,current%node_numbers(2)) = Fext(1,current%node_numbers(2))-Fn

            IF ( current%closed0 ) THEN

                ! Sticking flag
                current%stick = .TRUE.

                ! Friction coefficient
                frcoef = mat_par(current%mat,tref,'FRCOEF  ')

                ! Tangential force at axial node
                Ft = pfact(2) * (current%dz - current%dz0)

                ! Check for frictional sliding
                IF ( ABS(Ft) > frcoef*ABS(Fn) ) THEN
                    current%stick = .FALSE.
                    Ft = SIGN(frcoef*Fn,Ft)
                ENDIF

                ! Axial forces
                !Fint(2,current%node_numbers(3)) = Fint(2,current%node_numbers(3)) + Ft
                !Fint(2,current%node_numbers(4)) = Fint(2,current%node_numbers(4)) - Ft
                Fext(2,current%node_numbers(3)) = Fext(2,current%node_numbers(3)) - Ft
                Fext(2,current%node_numbers(4)) = Fext(2,current%node_numbers(4)) + Ft
            ENDIF
        ENDIF
        current => current%next
    ENDDO

    END SUBROUTINE cont1d_fint
    !
    !
    !
    SUBROUTINE cont1d_stIff()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix for the CONT1D elements
    TYPE(cont1d_type), POINTER :: current
    INTEGER(ipk) :: dofnum(2),i,j
    REAL(r8k) :: Ke(2,2),value_ij,frcoef,dir

    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        IF ( current%closed ) THEN
            Ke(1,1) =  current%pfact(1)
            Ke(1,2) = -current%pfact(1)
            Ke(2,1) = -current%pfact(1)
            Ke(2,2) =  current%pfact(1)
            dofnum(1:2) = dof_number(1,current%node_numbers(1:2))
            CALL sparse_matrix_place_element(2,dofnum,Ke)

            ! Friction coefficient
            frcoef = mat_par(current%mat,tref,'FRCOEF  ')

            Ke(1,1) =  current%pfact(2)
            Ke(1,2) = -current%pfact(2)
            Ke(2,1) = -current%pfact(2)
            Ke(2,2) =  current%pfact(2)

            IF ( current%closed0 ) THEN
                IF ( .NOT.current%stick ) THEN
                    dir = current%dz - current%dz0
                    value_ij = SIGN(frcoef*current%pfact(1),dir)
                    symmetric_matrix = .FALSE.
                    i = dof_number(2,current%node_numbers(3))
                    j = dof_number(1,current%node_numbers(1))
                    CALL sparse_matrix_place_value(i,j,value_ij)
                    i = dof_number(2,current%node_numbers(3))
                    j = dof_number(1,current%node_numbers(2))
                    CALL sparse_matrix_place_value(i,j,-value_ij)
                    i = dof_number(2,current%node_numbers(4))
                    j = dof_number(1,current%node_numbers(1))
                    CALL sparse_matrix_place_value(i,j,-value_ij)
                    i = dof_number(2,current%node_numbers(4))
                    j = dof_number(1,current%node_numbers(2))
                    CALL sparse_matrix_place_value(i,j,value_ij)
                ELSE
                    dofnum(1:2) = dof_number(2,current%node_numbers(3:4))
                    CALL sparse_matrix_place_element(2,dofnum,Ke)
                ENDIF
            ENDIF
        ENDIF
        current => current%next
    ENDDO

    END SUBROUTINE cont1d_stiff
    !
    !
    !
    SUBROUTINE cont1d_update()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Update contact condition
    TYPE(cont1d_type), POINTER :: current
    REAL(r8k) :: dir, frcoef

    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        IF ( .NOT.current%closed0 ) THEN
            current%dz0 = current%dz
        ELSE IF ( ( current%closed ).AND.( .NOT.current%stick ) ) THEN
            frcoef = mat_par(current%mat,tref,'FRCOEF  ')
            dir = current%dz - current%dz0
            current%dz0 = current%dz + SIGN(1.0_r8k,dir) * frcoef * current%gap
        ENDIF
        current%closed0 = current%closed
        current%stick0 = current%stick
        current%gap0 = current%gap
        current => current%next
    ENDDO

    END SUBROUTINE cont1d_update
    !
    !
    !
    SUBROUTINE cont1d_Write_output(nunit)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Write output to a file in unit 'nunit'
    INTEGER(ipk), INTENT(IN) :: nunit
    TYPE(cont1d_type), POINTER :: current

    ! Output contact elements
    WRITE(UNIT=nunit) ncont1d
    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
        WRITE(UNIT=nunit) current%label
        WRITE(UNIT=nunit) current%mat
        WRITE(UNIT=nunit) current%node_labels
        WRITE(UNIT=nunit) current%node_numbers
        WRITE(UNIT=nunit) current%closed0
        WRITE(UNIT=nunit) current%stick0
        WRITE(UNIT=nunit) current%gap0
        WRITE(UNIT=nunit) current%dz0
        current => current%next
    ENDDO    

    END SUBROUTINE cont1d_Write_output
    !
    !
    !
    SUBROUTINE cont1d_read_output(nunit)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    INTEGER(ipk), INTENT(IN) :: nunit
    INTEGER(ipk) :: i
    TYPE(cont1d_type), POINTER :: current,previous

    Read(UNIT=nunit,ERR=20,End=20) ncont1d

    current => first_cont1d
    DO i=1,ncont1d
       IF ( .NOT.ASSOCIATED(current) ) THEN
          ALLOCATE(current)
          current%next => NULL()
          IF ( .NOT.ASSOCIATED(first_cont1d) ) THEN
             first_cont1d => current
             last_cont1d => current
          ELSE
             last_cont1d%next => current
             last_cont1d => current
          ENDIF
       ENDIF
       Read(UNIT=nunit,ERR=20,End=20) current%label
       Read(UNIT=nunit,ERR=20,End=20) current%mat
       Read(UNIT=nunit,ERR=20,End=20) current%node_labels
       Read(UNIT=nunit,ERR=20,End=20) current%node_numbers
       Read(UNIT=nunit,ERR=20,End=20) current%closed0
       Read(UNIT=nunit,ERR=20,End=20) current%stick0
       Read(UNIT=nunit,ERR=20,End=20) current%gap0
       Read(UNIT=nunit,ERR=20,End=20) current%dz0
       last_cont1d => current
       current => current%next
    ENDDO

    ! Remove excess element entries from the Database
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO
    IF ( ASSOCIATED(last_cont1d) ) last_cont1d%next => NULL()
    IF ( ncont1d == 0 ) THEN
       first_cont1d => NULL()
       last_cont1d => NULL()
    ENDIF

    RETURN

20  CONTINUE
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR while reading CONT1D Database'
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END SUBROUTINE cont1d_read_output
    !
    !
    !
    SUBROUTINE cont1d_deallocate()
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Deallocate cont1d Database
    TYPE(cont1d_type), POINTER :: current,previous

    current => first_cont1d
    DO WHILE ( ASSOCIATED(current) )
       previous => current
       current => current%next
       DEALLOCATE(previous)
    ENDDO

    END SUBROUTINE cont1d_deallocate
    !
END MODULE cont1d_fraptran














