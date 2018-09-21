MODULE FEA_Mesh_fraptran
    !>@brief
    !> This module contains the subroutines used to create mesh for fuel rod_fraptran
    !> and to create forced DOF.
    !> Subroutines include mesh_fuel_rod, create_displ, create_gascav
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    USE Kinds_fraptran
    USE FEA_Node_fraptran
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    !
    !
SUBROUTINE create_displ (label, u_forced)
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> Create forced DOF values for a coupled set

    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: u_forced
    TYPE(coupled_set), POINTER :: current

    current => first_coupled_set
    DO WHILE ( ASSOCIATED(current) )
        IF ( current%label == label ) THEN
            IF ( .NOT.current%forced ) dof_numbering = .TRUE.
            current%forced = .TRUE.
            current%u_forced = u_forced
            EXIT
        ENDIF
        current => current%next
    ENDDO

    IF (.NOT. ASSOCIATED(current) ) THEN
        WRITE(UNIT=6,FMT='(/,A,I0,/)') 'ERROR can not find coupled set ',label
        CALL nlfemp_stop(1)
    ENDIF

END SUBROUTINE create_displ
!
!
    SUBROUTINE mesh_fuel_rod (na, nce, ncladi, nmesh, rc, rpp, RadialBoundO, AxialNodLen, cpl, pitch)
    USE Kinds_fraptran
    USE common_parameters_fraptran
    USE geometry_fraptran
    USE pressure1d_fraptran
    USE cont1d_fraptran
    USE gascav1d_fraptran
    USE gascav2d_fraptran
    USE fraptran_variables_fraptran
    IMPLICIT NONE
    !>@brief
    !> Create element mesh for a fuel rod and mark nodes and elements to correct groups
    ! Node group  Referred nodes
    ! 1001 ...... Fuel nodes at axial slice 1
    ! 1002 ...... Cladding nodes at axial slice 1
    !
    !
    ! Material flags
    !  1001 ....... UO2 fuel
    !  1002 ....... Zircaloy cladding
    !
    ! Pressure boundary conditions
    ! 1000 ....... Coolant pressure at axial node 1
    ! 1001 ....... Rod inner pressure at axial node 1
    !
    INTEGER(ipk), INTENT(IN) :: &
        na, & ! Number of axial meshes
        nce, & ! Number of radial nodes in cladding
        ncladi,nmesh ! Index numbers used to extract Data from RadialBoundO
    REAL(r8k), INTENT(IN) :: &
        rpp, & ! Pellet outer radius (in)
        rc, & ! Pellet inner radius (in)
        RadialBoundO(nmesh,na), & ! Radial node coordinates (ft)
        AxialNodLen(na), & ! Lengths of the axial slices (in)
        cpl, & ! Upper plenum length
        pitch ! Rod pitch in the assembly
    INTEGER(ipk) :: ia,number,nodes(6)
    REAL(r8k) :: node_x(3)
    TYPE(node_type), POINTER :: &
        ftnode, & ! Top fuel node for the current axial slice
        fbnode, & ! Bottom fuel node
        finode, & ! ID fuel node
        fonode, & ! OD fuel node
        ctnode, & ! Top cladding node
        cbnode, & ! Bottom cladding node
        cinode, & ! ID cladding node
        conode, & ! OD cladding node
        node0 ! node at the z-axis
    INTEGER(ipk) :: clad_mat_code

    ! Set cladding material label
    clad_mat_code = 1002
    !If ( clad_CladType == 7 ) clad_mat_code = 1003

    ! Set DIMENSION
    nd = 2

    ! Create gas cavity for the gap and plenum
    CALL create_gascav(1,0.0_r8k)

    ! Create first nodess for fuel and cladding at the bottom of the rod
    ngroup = 1
    node_x(1:3) = (/ (rc + rpp) / 2.0_r8k, 0.0_r8k, 0.0_r8k /)
    CALL create_node(5,node_x)
    ftnode => last_node

    ngroup = 2
    node_x(1) = (RadialBoundO(ncladi,1) + RadialBoundO(nmesh,1)) * 6.0_r8k
    CALL create_node(6,node_x)
    ctnode => last_node

    ! Create nodes for a contact element that prevents exessive deformations
    ngroup = 10
    IF (0.5_r8k * pitch > RadialBoundO(nmesh,1) ) THEN
        node_x(1) = pitch * 6.0_r8k
        node_x(2) = 0.0_r8k
        CALL create_node(1,node_x)
        last_node%dof_status = -1
        DO ia = 1, (na - 1)
            node_x(2) = node_x(2) + AxialNodLen(ia)
        ENDDO
        node_x(2) = node_x(2) + cpl
        CALL create_node(2,node_x)
        last_node%dof_status = -1
        nodes(1:2) = (/ 2, 1 /)
        CALL create_line(1,nodes,1)
    ENDIF

    ! Set fixed boundary condition for the bottom nodes
    ftnode%dof_status = -1
    ctnode%dof_status = -1

    ! Create meshing for the rod starting at the bottom
    DO ia = 1, (na - 1)
        ! last top nodes are now mesh bottom nodes
        fbnode => ftnode
        cbnode => ctnode

        ! Create top nodes for the radial meshing of the fuel and cladding
        ngroup = ia*1000 + 1
        number = ia*1000 + 5
        node_x(1:2) = (/ (rc + rpp) / 2.0_r8k, fbnode%x0(2) + AxialNodLen(ia) /)
        CALL create_node(number,node_x)
        ftnode => last_node
        CALL create_coupled(number,number,2)

        ngroup = ia*1000 + 2
        number = ia*1000 + 6
        node_x(1:2) = (/ (RadialBoundO(ncladi,ia) + RadialBoundO(nmesh,ia)) * 6.0_r8k, cbnode%x0(2) + AxialNodLen(ia) /)
        CALL create_node(number,node_x)
        ctnode => last_node

        ! Create ID and OD nodes for the radial meshing of the fuel and cladding
        ngroup = ia*1000 + 1
        number = ia*1000 + 1
        node_x(1:2) = (/ rc, (fbnode%x0(2) + ftnode%x0(2)) / 2.0_r8k /)
        CALL create_node(number,node_x)
        finode => last_node
        CALL create_coupled(number,number,1)

        number = ia*1000 + 2
        node_x(1:2) = (/ rpp, finode%x0(2) /)
        CALL create_node(number,node_x)
        fonode => last_node
        CALL create_coupled(number,number,1)

        ngroup = ia*1000 + 2
        number = ia*1000 + 3
        node_x(1:2) = (/ RadialBoundO(ncladi,ia) * 12.0_r8k, (cbnode%x0(2) + ctnode%x0(2))/2.0_r8k /)
        CALL create_node(number,node_x)
        cinode => last_node

        number = ia*1000 + 4
        node_x(1:2) = (/ RadialBoundO(nmesh,ia)*12.0_r8k, cinode%x0(2) /)
        CALL create_node(number,node_x)
        conode => last_node

        ! Mesh fuel axial slice
        ngroup = ia*1000 + 1
        egroup = ia*1000 + 1
        CALL meshs1d(finode%label,fonode%label,fbnode%label,ftnode%label,10,1001,0,0)

        ! Mesh cladding axial slice
        ngroup = ia*1000 + 2
        egroup = ia*1000 + 2
        CALL meshs1d(cinode%label,conode%label,cbnode%label,ctnode%label,nce,clad_mat_code,0,1)

        ! Define contact element that prevents exessive plastic deformation
        IF ( pitch > RadialBoundO(nmesh,1) ) THEN
            nodes(1:3) = (/ 2, 1, conode%label /)
            number = ia*1000 + 1
            CALL cont2d_create(number,1001,1,nodes)
        ENDIF

        ! Gas cavity in pellet cladding gap
        egroup = ia*1000
        number = ia*1000
        nodes(1:4) = (/ fonode%label, cinode%label, fbnode%label, ftnode%label /)
        CALL gascav1d_create(number,1,nodes,.FALSE.)

        ! Define pressure boundary conditions
        number = ia*1000
        nodes(1:3) = (/ cbnode%label, ctnode%label, conode%label /) 
        CALL pressure1d_create(number,nodes,0.0_r8k)

        ! Define contact boundary conditions
        number = ia*1000
        nodes(1:4) = (/ fonode%label, cinode%label, ftnode%label, ctnode%label /)
        CALL cont1d_create(number,clad_mat_code,nodes)
    ENDDO

    ! Mesh upper plenum
    cbnode => ctnode

    ngroup = na*1000 + 2
    number = na*1000 + 6
    node_x(1:2) = (/ (RadialBoundO(ncladi,na-1) + RadialBoundO(nmesh,na-1))*6.0_r8k, cbnode%x0(2) + cpl /)
    CALL create_node(number,node_x)
    ctnode => last_node

    number = na*1000 + 3
    node_x(1:2) = (/ RadialBoundO(ncladi,na-1) * 12.0_r8k, (cbnode%x0(2) + ctnode%x0(2))/2.0_r8k /)
    CALL create_node(number,node_x)
    cinode => last_node

    number = na*1000 + 4
    node_x(1:2) = (/ RadialBoundO(nmesh,na-1) * 12.0_r8k, cinode%x0(2) /)
    CALL create_node(number,node_x)
    conode => last_node

    number = na*1000 + 7
    node_x(1:2) = (/ 0.0_r8k, ctnode%x0(2) /)
    CALL create_node(number,node_x)
    last_node%dof_status = -1
    node0 => last_node

    ! Mesh cladding axial slice
    egroup = na*1000 + 1
    CALL meshs1d(cinode%label,conode%label,cbnode%label,ctnode%label,nce, &
        clad_mat_code,0,1)

    ! Gas cavity in upper plenum
    egroup = na*1000
    number = na*1000
    nodes(1:4) = (/ node0%label, cinode%label, ftnode%label, ctnode%label /)
    CALL gascav1d_create(number,1,nodes,.TRUE.)

    ! Pressure boundary conditions at plenum
    number = na*1000
    nodes(1:3) = (/ cbnode%label, ctnode%label, conode%label /) 
    CALL pressure1d_create(number,nodes,0.0_r8k)

    number = (na+1)*1000
    nodes(1:3) = (/conode%label, node0%label, ctnode%label/) 
    CALL pressure1d_create(number,nodes,0.0_r8k)

    END SUBROUTINE mesh_fuel_rod
    !
    !
    SUBROUTINE create_gascav (label, mgas)
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> Create or update gas cavity Data

    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: mgas
    TYPE(gascav_type), POINTER :: current

    ! Try to find the gas cavity label from the existing Database
    current => first_gascav
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) EXIT
        current => current%next
    ENDDO

    IF (.NOT. ASSOCIATED(current)) THEN
        ! Create new gas cavity entry If necessary
        ngascav = ngascav + 1
        ALLOCATE(current)
        current%label = label
        current%failed = .FALSE.
        current%failed0 = .FALSE.
        current%mgas0 = mgas
        current%mgas_End = mgas
        current%p0 = 0.0_r8k
        current%V0 = 0.0_r8k
        current%VpT0 = 0.0_r8k
        current%next => NULL()
        IF (.NOT. ASSOCIATED(first_gascav)) THEN
            first_gascav => current
            last_gascav => current
        ELSE
            last_gascav%next => current
            last_gascav => current
        ENDIF
    ELSE
        ! Update existing gas cavity entry
        current%mgas_End = mgas
    ENDIF

    END SUBROUTINE create_gascav
    !
END MODULE FEA_Mesh_fraptran













