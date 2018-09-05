MODULE gascav3d
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE sparse_matrix
    IMPLICIT NONE
    !>@brief
    !> 3D gas cavity
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
    !
    INTEGER(ipk) :: ngascav3d
    REAL(r8k), PRIVATE :: xi(3,8)      ! GP coordinate values in element CS
    REAL(r8k), PRIVATE :: N(8,8)       ! Shape Function values at GPs
    REAL(r8k), PRIVATE :: dNdxi(3,8,8) ! Shape Function derivatives in element CS
    REAL(r8k), PRIVATE :: wgt(8)       ! GP integration weight factors

    REAL(r8k), PRIVATE :: Ns(4,4), wgts(4), dNdxis(2,4,4)

    TYPE gascav3d_type
        INTEGER(ipk) :: label    ! Element label
        INTEGER(ipk) :: number   ! Internal element number
        INTEGER(ipk) :: gascav   ! Gas cavity label
        INTEGER(ipk) :: node_labels(8)  ! Element node labels
        INTEGER(ipk) :: node_numbers(8) ! Internal node numbering
        REAL(r8k) :: gtemp(8)    ! Integration point temperatures
        REAL(r8k) :: dNdx(3,8,8) ! Derivatives of the shape functions
        REAL(r8k) :: x(3,8)      ! Integration point coordinates
        REAL(r8k) :: fepp(4,8)   ! coefficient that give node forces
        REAL(r8k) :: dV(8)       ! Integration point volumes
        TYPE(gascav3d_type), POINTER :: next
    END TYPE gascav3d_type

    TYPE(gascav3d_type), POINTER :: first_gascav3d, last_gascav3d
    !
    CONTAINS
    !
    SUBROUTINE gascav3d_init()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize pointers and tables for Q4 element calculation
    !
    INTEGER(ipk) :: ig
    REAL(r8k) :: tmp

    ! Deallocate existing database
    CALL gascav3d_deallocate()

    ! Initialize element number and pointers
    first_gascav3d => NULL()
    last_gascav3d => NULL()
    ngascav3d = 0

    ! GP element coordinates and weight factors
    wgts(1:4) = 1.0_r8k

    DO ig = 1, 4
        ! Shape functions at the gauss point
        Ns(1,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 4.0_r8k
        Ns(2,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 4.0_r8k
        Ns(3,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 4.0_r8k
        Ns(4,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 4.0_r8k

        ! Derivatives of the shape functions
        dNdxis(1,1,ig) = -(1.0_r8k - xi(2,ig)) / 4.0_r8k
        dNdxis(1,2,ig) =  (1.0_r8k - xi(2,ig)) / 4.0_r8k
        dNdxis(1,3,ig) =  (1.0_r8k + xi(2,ig)) / 4.0_r8k
        dNdxis(1,4,ig) = -(1.0_r8k + xi(2,ig)) / 4.0_r8k
        dNdxis(2,1,ig) = -(1.0_r8k - xi(1,ig)) / 4.0_r8k
        dNdxis(2,2,ig) = -(1.0_r8k + xi(1,ig)) / 4.0_r8k
        dNdxis(2,3,ig) =  (1.0_r8k + xi(1,ig)) / 4.0_r8k
        dNdxis(2,4,ig) =  (1.0_r8k - xi(1,ig)) / 4.0_r8k
    END DO

    ! GP element coordinates and weight factors
    wgt(1:8) = 1.0_r8k
    tmp = 1.0_r8k / SQRT(3.0_r8k)
    xi(1:3,1) = (/ -tmp, -tmp, -tmp/)
    xi(1:3,2) = (/  tmp, -tmp, -tmp/)
    xi(1:3,3) = (/  tmp,  tmp, -tmp/)
    xi(1:3,4) = (/ -tmp,  tmp, -tmp/)
    xi(1:3,5) = (/ -tmp, -tmp,  tmp/)
    xi(1:3,6) = (/  tmp, -tmp,  tmp/)
    xi(1:3,7) = (/  tmp,  tmp,  tmp/)
    xi(1:3,8) = (/ -tmp,  tmp,  tmp/)

    DO ig = 1,8
        ! Shape functions at the gauss point
        N(1,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        N(2,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        N(3,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        N(4,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        N(5,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        N(6,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        N(7,ig) = (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        N(8,ig) = (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k

        ! Derivatives of the shape functions
        dNdxi(1,1,ig) = -(1.0_r8k - xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(1,2,ig) =  (1.0_r8k - xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(1,3,ig) =  (1.0_r8k + xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(1,4,ig) = -(1.0_r8k + xi(2,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(1,5,ig) = -(1.0_r8k - xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(1,6,ig) =  (1.0_r8k - xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(1,7,ig) =  (1.0_r8k + xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(1,8,ig) = -(1.0_r8k + xi(2,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(2,1,ig) = -(1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(2,2,ig) = -(1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(2,3,ig) =  (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(2,4,ig) =  (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(3,ig)) / 8.0_r8k
        dNdxi(2,5,ig) = -(1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(2,6,ig) = -(1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(2,7,ig) =  (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(2,8,ig) =  (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(3,ig)) / 8.0_r8k
        dNdxi(3,1,ig) = -(1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 8.0_r8k
        dNdxi(3,2,ig) = -(1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 8.0_r8k
        dNdxi(3,3,ig) = -(1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 8.0_r8k
        dNdxi(3,4,ig) = -(1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 8.0_r8k
        dNdxi(3,5,ig) =  (1.0_r8k - xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 8.0_r8k
        dNdxi(3,6,ig) =  (1.0_r8k + xi(1,ig)) * (1.0_r8k - xi(2,ig)) / 8.0_r8k
        dNdxi(3,7,ig) =  (1.0_r8k + xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 8.0_r8k
        dNdxi(3,8,ig) =  (1.0_r8k - xi(1,ig)) * (1.0_r8k + xi(2,ig)) / 8.0_r8k
    END DO

    END SUBROUTINE gascav3d_init
    !
    !
    !
    SUBROUTINE gascav3d_create (label, gascav, nodes)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Create new GASCAV3D element
    !
    INTEGER(ipk), INTENT(IN) :: label, gascav, nodes(4)
    INTEGER(ipk) :: in, jn, nfound
    REAL(r8k) :: x12(3), x14(3), x15(3), cprd(3)
    TYPE(gascav3d_type), POINTER :: new_gascav3d, current

    ! ALLOCATE new element
    ngascav3d = ngascav3d + 1
    ALLOCATE(new_gascav3d)

    ! Find element nodes in node database
    nfound = 0
    DO in = 1, nnodes
        DO jn = 1, 8
            IF (nodes(jn) == node_labels(in)) THEN
                new_gascav3d%node_labels(jn) = nodes(jn)
                new_gascav3d%node_numbers(jn) = in
                nfound = nfound + 1
            END IF
        END DO
        IF (nfound == 8) EXIT
    END DO

    ! Check whether all element nodes were found in node database
    IF (nfound /= 8) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR incompatible NODES and GASCAV3D data in element ',label
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF

    ! Check that the element label doesn't already exist
    current => first_gascav3d
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) THEN
            WRITE (ounit,FMT='(/,A,I0,A,/)') 'ERROR GASCAV3D ',label,' has been defined twice'
            lerror = .TRUE.
            CALL nlfemp_stop(0)
        END IF
        current => current%next
    END DO

    ! Order element nodes properly
    x12(1:3) = x0(1:3,new_gascav3d%node_numbers(2)) - x0(1:3,new_gascav3d%node_numbers(1))
    x14(1:3) = x0(1:3,new_gascav3d%node_numbers(4)) - x0(1:3,new_gascav3d%node_numbers(1))
    x15(1:3) = x0(1:3,new_gascav3d%node_numbers(5)) - x0(1:3,new_gascav3d%node_numbers(1))
    cprd(1) = x12(2) * x14(3) - x14(2) * x12(3)
    cprd(2) = x12(3) * x14(1) - x14(3) * x12(1)
    cprd(3) = x12(1) * x14(2) - x14(1) * x12(2)
    IF (DOT_PRODUCT(cprd,x15) < 0.0_r8k) THEN
        new_gascav3d%node_labels(1:4) = new_gascav3d%node_labels(4:1:-1)
        new_gascav3d%node_labels(5:8) = new_gascav3d%node_labels(8:5:-1)
        new_gascav3d%node_numbers(1:4) = new_gascav3d%node_numbers(4:1:-1)
        new_gascav3d%node_numbers(5:8) = new_gascav3d%node_numbers(8:5:-1)
    END IF

    ! Initialize GASCAV3D variables
    new_gascav3d%label = label
    new_gascav3d%number = ngascav3d
    new_gascav3d%gascav = gascav
    new_gascav3d%next => NULL()

    ! New DOF numbering is needed
    IF (pressure_matrix) dof_numbering = .TRUE.

    ! Add new element to the database
    IF (.NOT. ASSOCIATED(first_gascav3d)) THEN
        first_gascav3d => new_gascav3d
        last_gascav3d => new_gascav3d
    ELSE
        last_gascav3d%next => new_gascav3d
        last_gascav3d => new_gascav3d
    END IF

    END SUBROUTINE gascav3d_create
    !
    !
    !
    SUBROUTINE gascav3d_sparse_matrix()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize sparse matrix storage for gascav3d elements
    !
    TYPE(gascav3d_type), POINTER :: current
    TYPE(gascav_type), POINTER :: current_cav
    INTEGER(ipk) :: ndofnum
    INTEGER(ipk), ALLOCATABLE :: dofnum(:)

    IF (pressure_matrix) THEN
        ALLOCATE(dofnum(8*nd*ngascav3d))
        current_cav => first_gascav
        DO WHILE (ASSOCIATED(current_cav))
            ! Find all elements that belong to this gas cavity and create a
            ! superelement from all of these elements
            ndofnum = 0
            current => first_gascav3d
            DO WHILE (ASSOCIATED(current))
                IF (current%gascav == current_cav%label) THEN
                    dofnum(ndofnum+1:ndofnum+3) = dof_number(1:3,current%node_numbers(1))
                    dofnum(ndofnum+4:ndofnum+6) = dof_number(1:3,current%node_numbers(2))
                    dofnum(ndofnum+7:ndofnum+9) = dof_number(1:3,current%node_numbers(3))
                    dofnum(ndofnum+10:ndofnum+12) = dof_number(1:3,current%node_numbers(4))
                    dofnum(ndofnum+13:ndofnum+15) = dof_number(1:3,current%node_numbers(5))
                    dofnum(ndofnum+16:ndofnum+18) = dof_number(1:3,current%node_numbers(6))
                    dofnum(ndofnum+19:ndofnum+21) = dof_number(1:3,current%node_numbers(7))
                    dofnum(ndofnum+22:ndofnum+24) = dof_number(1:3,current%node_numbers(8))
                    ndofnum = ndofnum + 24
                END IF
                current => current%next
            END DO
            CALL sparse_matrix_add_element(ndofnum,dofnum)
            current_cav => current_cav%next
        END DO
        DEALLOCATE(dofnum)
    END IF

    END SUBROUTINE gascav3d_sparse_matrix
    !
    !
    !
    SUBROUTINE gascav3d_temp()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Initialize temperatures
    !
    TYPE(gascav3d_type), POINTER :: current
    INTEGER(ipk) :: ig, in
    REAL(r8k) :: gtemp

    current => first_gascav3d
    DO WHILE (ASSOCIATED(current))
        DO ig = 1, 8
            ! Integration point coordinates
            cp = current%x(1:3,ig)

            ! GP temperature
            gtemp = 0.0_r8k
            DO in = 1, 8
                gtemp = gtemp + N(in,ig) * temp(current%node_numbers(in))
            END DO
            current%gtemp(ig) = gtemp

        END DO
        current => current%next
    END DO

    END SUBROUTINE gascav3d_temp
    !
    !
    !
    SUBROUTINE gascav3d_vpt()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate 3D pressure load contribution to the external force vector
    !
    END SUBROUTINE gascav3d_vpt
    !
    !
    !
    SUBROUTINE gascav3d_fext()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Calculate 3D pressure load contribution to the external force vector
    !
    END SUBROUTINE gascav3d_fext
    !
    !
    !
    SUBROUTINE gascav3d_stiff()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stiffness matrix due to GASCAV3D elements

    IF (pressure_matrix .AND. ngascav3d > 0) symmetric_matrix = .FALSE.

    END SUBROUTINE gascav3d_stiff
    !
    !
    !
    SUBROUTINE gascav3d_write_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Write pressure data
    !
    INTEGER(ipk), INTENT(IN) :: nunit

    WRITE (UNIT=nunit) ngascav3d

    END SUBROUTINE gascav3d_write_output
    !
    !
    !
    SUBROUTINE gascav3d_read_output (nunit)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Read output from unit 'nunit'
    INTEGER(ipk) :: InputStat = 0
    INTEGER(ipk), INTENT(IN) :: nunit

    READ (UNIT=nunit, IOSTAT=InputStat) ngascav3d

    IF (InputStat /= 0) THEN
        WRITE (ounit,FMT='(/,A,/)') 'ERROR while reading GASCAV3D database'
        lerror = .TRUE.
        CALL nlfemp_stop(0)
    END IF
    
    END SUBROUTINE gascav3d_read_output
    !
    !
    !
    SUBROUTINE gascav3d_deallocate()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate gascav3d database
    !
    TYPE(gascav3d_type), POINTER :: current, previous

    current => first_gascav3d
    DO WHILE (ASSOCIATED(current))
        previous => current
        current => current%next
        DEALLOCATE(previous)
    END DO

    END SUBROUTINE gascav3d_deallocate
    !
END MODULE gascav3d

