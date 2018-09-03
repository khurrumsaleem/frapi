MODULE FEA_Mesh
    USE Kinds
    USE Conversions
    USE FEA_Node
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to create mesh for fuel rod
    !> and to create forced DOF.
    !> Subroutines include mesh_fuel_rod, create_displ
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/24/2015
    !
    CONTAINS
    !
    SUBROUTINE mesh_fuel_rod
    USE Kinds
    USE Conversions
    USE common_parameters
    USE geometry_fpn
    USE pressure1d
    USE cont1d
    USE Variables, ONLY : na, nce, rc, dci, dco, ounit, cpl, icm, dp, deltaz
    IMPLICIT NONE
    !>@brief
    !> Create element mesh for a fuel rod and mark nodes and elements to correct groups
    ! Node group  Referred nodes
    ! 1001 ...... Fuel nodes at axial slice 1
    ! 1002 ...... Cladding nodes at axial slice 1
    ! 1005 ...... Cladding internal nodes at axial slice 1
    !
    ! Material flags
    !  1001 ....... UO2 fuel
    !  1002 ....... Zircaloy cladding
    !
    ! Pressure boundary conditions
    ! 1000 ....... Coolant pressure at axial node 1
    ! 1001 ....... Rod inner pressure at axial node 1
    !
    ! Input
    !
    ! na     - Number of axial meshes
    ! nce    - Number of radial nodes in cladding
    ! dp(j)  - Pellet outer diameter (in) for axial node j
    ! rc(j)  - Pellet inner radius (in) for axial node j
    ! dci    - Radial node coordinate values at cladding ID (in) Diameter
    ! dco    - Radial node coordinate values at cladding OD (in) Diameter
    ! deltaz - Lengths of the axial slices (ft)
    ! cpl    - Upper plenum length
    ! Output
    !
    ! Internal
    !
    ! ftnode - Top fuel node for the current axial slice
    ! fbnode - Bottom fuel node
    ! finode - ID fuel node
    ! fonode - OD fuel node
    ! ctnode - Top cladding node
    ! cbnode - Bottom cladding node
    ! cinode - ID cladding node
    ! conode - OD cladding node
    ! node0  - node at the z-axis
    ! rp(j)  - pellet outer radius (in) for axial node j
    !
    INTEGER(ipk) :: ia, number,nodes(6), clad_mat_code
    REAL(r8k) :: node_x(3), rpp(na)
    TYPE(node_type), POINTER :: ftnode, fbnode, finode, fonode, ctnode, cbnode, cinode, conode, node0
    ! Set pellet radius
    rpp(1:na) = dp(1:na) / 2.0_r8k
    ! Set cladding material label
    clad_mat_code = 1002
    !IF (icm == 5) clad_mat_code = 1003

    ! Set Dimension
    nd = 2

    ! Create first nodess for fuel and cladding at the bottom of the rod
    ngroup = 1
    node_x(1:3) = (/ (rc(1) + rpp(1)) / 2.0_r8k, 0.0_r8k, 0.0_r8k /)
    CALL create_node (5, node_x)
    ftnode => last_node

    ngroup = 2
    node_x(1) = ((dci(1) + dco(1)) / 2.0_r8k) / 2.0_r8k
    CALL create_node (6, node_x)
    ctnode => last_node

    ! Set fixed boundary condition for the bottom nodes
    ftnode%dof_status = -1
    ctnode%dof_status = -1

    ! Create meshing for the rod starting at the bottom
    DO ia = 1, (na - 1)
        ! last top nodes are now mesh bottom nodes
        fbnode => ftnode
        cbnode => ctnode

        ! Create top nodes for the radial meshing of the fuel and cladding
        ngroup = ia * 1000 + 1
        number = ia * 1000 + 5
        node_x(1:2) = (/ (rc(ia) + rpp(ia)) / 2.0_r8k, fbnode%x0(2) + fttoin * deltaz(ia) /)
        CALL create_node (number, node_x)
        ftnode => last_node
        CALL create_coupled (number, number, 2)

        ngroup = ia * 1000 + 2
        number = ia * 1000 + 6
        node_x(1:2) = (/ (dci(ia) + dco(ia)) / 4.0_r8k, cbnode%x0(2) + fttoin * deltaz(ia) /)
        CALL create_node (number, node_x)
        ctnode => last_node

        ! Create ID and OD nodes for the radial meshing of the fuel and cladding
        ngroup = ia * 1000 + 1
        number = ia * 1000 + 1
        node_x(1:2) = (/ rc(ia), (fbnode%x0(2) + ftnode%x0(2)) / 2.0_r8k /)
        CALL create_node (number, node_x)
        finode => last_node
        CALL create_coupled (number, number, 1)

        number = ia * 1000 + 2
        node_x(1:2) = (/ rpp(ia), finode%x0(2) /)
        CALL create_node (number, node_x)
        fonode => last_node
        CALL create_coupled (number, number, 1)

        ngroup = ia * 1000 + 2
        number = ia * 1000 + 3
        node_x(1:2) = (/ dci(ia) / 2.0_r8k, (cbnode%x0(2) + ctnode%x0(2)) / 2.0_r8k /)
        CALL create_node (number, node_x)
        cinode => last_node

        number = ia * 1000 + 4
        node_x(1:2) = (/ dco(ia), cinode%x0(2) /)
        CALL create_node (number, node_x)
        conode => last_node

        ! Mesh fuel axial slice
        ngroup = ia * 1000 + 1
        egroup = ia * 1000 + 1
        CALL meshs1d (finode%label, fonode%label, fbnode%label, ftnode%label, 10, 1001, 0, 0)

        ! Mesh cladding axial slice
        ngroup = ia * 1000 + 2
        egroup = ia * 1000 + 2
        CALL meshs1d (cinode%label, conode%label, cbnode%label, ctnode%label, nce, clad_mat_code, 0, 1)

        ! Define pressure boundary conditions
        number = ia * 1000
        nodes(1:3) = (/ cbnode%label, ctnode%label, conode%label /) 
        CALL pressure1d_create (number, nodes, 0.0_r8k)
        number = ia * 1000 + 1
        nodes(1:3) = (/ ctnode%label, cbnode%label, cinode%label /) 
        CALL pressure1d_create (number, nodes, 0.0_r8k)
        !nodes(1:3) = (/ fbnode%label, ftnode%label, fonode%label /) 
        !Call pressure1d_create(number, nodes, 0.0_r8k)

        ! Define contact boundary conditions
        number = ia * 1000
        nodes(1:4) = (/ fonode%label, cinode%label, ftnode%label, ctnode%label /)
        CALL cont1d_create (number, clad_mat_code, nodes)

    END DO

    ! Mesh upper plenum
    cbnode => ctnode

    ngroup = na * 1000 + 2
    number = na * 1000 + 6
    node_x(1:2) = (/ (dci(na) + dco(na)) / 4.0_r8k, cbnode%x0(2) + cpl /)
    CALL create_node (number, node_x)
    ctnode => last_node

    number = na * 1000 + 3
    node_x(1:2) = (/ dci(na) / 2.0_r8k, (cbnode%x0(2) + ctnode%x0(2)) / 2.0_r8k /)
    CALL create_node (number, node_x)
    cinode => last_node

    number = na * 1000 + 4
    node_x(1:2) = (/ dco(na) / 2.0_r8k, cinode%x0(2) /)
    CALL create_node (number, node_x)
    conode => last_node

    number = na * 1000 + 7
    node_x(1:2) = (/ 0.0_r8k, ctnode%x0(2) /)
    CALL create_node (number, node_x)
    last_node%dof_status = -1
    node0 => last_node

    ! Mesh cladding axial slice
    egroup = na * 1000 + 1
    CALL meshs1d (cinode%label, conode%label, cbnode%label, ctnode%label, nce, clad_mat_code, 0, 1)

    ! Pressure boundary conditions at plenum
    number = na * 1000
    nodes(1:3) = (/ cbnode%label, ctnode%label, conode%label /) 
    CALL pressure1d_create (number, nodes, 0.0_r8k)
    number = na * 1000 + 1
    nodes(1:3) = (/ ctnode%label, cbnode%label, cinode%label /) 
    CALL pressure1d_create (number, nodes, 0.0_r8k)
    !nodes(1:3) = (/fonode%label, finode%label, ftnode%label/) 
    !Call pressure1d_create(number,nodes,0.0_r8k)

    number = (na+1) * 1000 + 1
    nodes(1:3) = (/node0%label, cinode%label, ctnode%label/) 
    CALL pressure1d_create (number, nodes, 0.0_r8k)

    number = (na+1) * 1000
    nodes(1:3) = (/conode%label, node0%label, ctnode%label/) 
    CALL pressure1d_create (number, nodes, 0.0_r8k)

    END SUBROUTINE mesh_fuel_rod
    !
    !
    !
    SUBROUTINE create_displ (label, u_forced)
    USE Kinds
    USE Conversions
    USE common_parameters
    IMPLICIT NONE
    !>@brief
    !> Create forced DOF values for a coupled set
    !
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: u_forced
    TYPE(coupled_set), POINTER :: current

    current => first_coupled_set
    DO WHILE (ASSOCIATED(current))
        IF (current%label == label) THEN
            IF (.NOT. current%forced) dof_numbering = .TRUE.
            current%forced = .TRUE.
            current%u_forced = u_forced
            EXIT
        END IF
        current => current%next
    END DO

    IF (.NOT. ASSOCIATED(current)) THEN
        WRITE (ounit,FMT='(/,A,I0,/)') 'ERROR can not find coupled set ',label
        CALL nlfemp_stop(1)
    END IF

    END SUBROUTINE create_displ
    !
END MODULE FEA_Mesh