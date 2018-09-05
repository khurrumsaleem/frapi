MODULE common_parameters
    USE Kinds
    USE conversions_fraptran, ONLY : pi
    IMPLICIT NONE
    !>@brief
    !> All common parameters and variables

    ! Parameters
    ! ==========

    ! Derived types
    ! =============

    TYPE node_type
        INTEGER(ipk) :: label ! Node label
        INTEGER(ipk) :: number ! Node internal number
        !INTEGER(ipk) :: enumber ! Number of elements associated to the node
        INTEGER(ipk) :: ngroup ! Node group number (0 If none)
        INTEGER(ipk) :: icoord ! Coordinate system
                    !  1 = cartesian
        REAL(r8k) :: x0(3) ! Initial node coordinates
        !REAL(r8k) :: x(3) ! Current node coordinates
        INTEGER(ipk) :: dof_status(3) ! Status of node DOFs
                    !   0 ....... Free
                    !   n > 0 ... Coupled to coupled set n
                    !  -1 ....... Fixed
                    !  -2 ....... Temporarily marked as fixed
        REAL(r8k) :: temp0 ! Explicit value of temperature at node
        REAL(r8k) :: temp_End ! Temperature at the End of the load step
        REAL(r8k) :: u0(3) ! Explicit value of node displacement
        REAL(r8k) :: v0(3) ! Explicit value of node velocity
        REAL(r8k) :: a0(3) ! Explicit value of node acceleration
        REAL(r8k) :: fint0(3) ! Explicit value of total internal force
        LOGICAL :: force_flag ! Flag for external nodal force
        REAL(r8k), DIMENSION(:), POINTER :: force ! External force load on node
        REAL(r8k), DIMENSION(:), POINTER :: force0 ! Explicit value of external force load
        REAL(r8k), DIMENSION(:), POINTER :: force_End ! External force load at the end of the load step
        TYPE(node_type), POINTER :: next
    END TYPE node_type

    TYPE coupled_set
        INTEGER(ipk) :: label ! External label
        INTEGER(ipk) :: number ! Internal number
        INTEGER(ipk) :: dof_number ! DOF number for the coupled set of DOFs
        LOGICAL :: forced ! Flag for forced displacement
        REAL(r8k) :: u_forced ! Forced displacement at coupled DOF
        REAL(r8k) :: u ! Displacement at coupled DOF
        REAL(r8k) :: v ! Velocity at coupled DOF
        REAL(r8k) :: a ! Acceleration at coupled DOF
        REAL(r8k) :: force ! Force at the coupled set
        TYPE(coupled_set), POINTER :: next
    END TYPE coupled_set

    TYPE gascav_type
        INTEGER(ipk) :: label
        LOGICAL :: failed ! Flag for failure
        LOGICAL :: failed0 ! Flag for failure
        REAL(r8k) :: p ! Pressure in the gas cavity
        REAL(r8k) :: p0 ! Explicit value for the pressure in the gas cavity
        REAL(r8k) :: V_tot ! Total volume of the gas cavity
        REAL(r8k) :: V0
        REAL(r8k) :: VpT ! Integrated volume per temperature at gas cavity
        REAL(r8k) :: VpT0
        REAL(r8k) :: mgas0 ! Explicit amount of gas in the cavity
        REAL(r8k) :: mgas ! Amount of gas in the cavity
        REAL(r8k) :: mgas_End ! Amount of gas at the End of time step
        TYPE(gascav_type), POINTER :: next
    END TYPE gascav_type

    ! =========
    ! Variables
    ! =========

    INTEGER(ipk) :: &
        nline, & ! Line number at the input file
        nd, & ! Number of Dimensions
        nnodes, & ! Number of nodes
        ncoupled, & ! Number of coupled sets
        nfixed, & ! Number of fixed DOFs
        ndispl, & ! Number of forced displacements
        ndof, & ! Number of free DOFs
        ngascav, & ! Number of gas cavities
        noutput, & ! Number of output
        max_node, & ! Maximum node number
        max_nnr, & ! Maximum number of Newton-Raphson iterations
        max_nln, & ! Maximum number of line searches
        units, & ! Units (does not have any effect on FEA)
            !   0 = British units (in,Psi)
            !   1 = SI units (m,Pa)
            !   2 = unknown
        nr_iter, & ! Newton-Raphson iteration count
        ngroup, & ! Current node group
        egroup, & ! Current element group
        nwarn, & ! Number of warning events for last iteration
        n_array, & ! Size for nodal arrays
        balloon, & ! Ballooning model
        rupture_mat, & ! Material label for rupture model
        rupture_gc ! Gas cavity for rupture model
    INTEGER(ipk), PARAMETER :: in_unit = 110
    INTEGER(ipk), PARAMETER :: out_unit = 120
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: &
        node_labels, & ! Array for node labels
        enumber, & ! Number of elements associated to node
        int_tmp ! Temporary integer array
    INTEGER(ipk), DIMENSION(:,:), ALLOCATABLE :: &
        dof_number  ! DOF numbering (0 is a fixed DOF)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: &
        du, & ! Displacement icrement
        fres, & ! Force residual
        temp, & ! Node temperatures
        Real_tmp, & ! Temporary real array for subroutines
        values_mm ! Mass matrix storage
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: &
        u, & ! Nodal displacements
        v, & ! Nodal velocities
        a, & ! Nodal accelerations
        x0, & ! Initial node coordinates
        x, & ! Node coordinates
        Fint, & ! Node coordinates
        Fext, & ! Node coordinates
        Fgrav ! Gravity load
    CHARACTER(LEN=8) :: &
        dimens, & ! Flag for Dimensions of the calculation
                !   AXI = axisymmetric analysis
                !   2D = two Dimensional analysis
                !   3D  = three Dimensional analysis
        analys ! Analysis type
            !   STATIC = static analysis (neglect inertial forces)
            !   DYNAMIC = dynamic analysis
    CHARACTER(LEN=100) :: &
        infile, & ! Input file name
        outfile, & ! Output file name
        title ! Analysis title
    CHARACTER(LEN=1000) :: &
        line ! Last read line at input file
    REAL(r8k) :: &
        time, & ! Analysis time
        time0, & ! Explicit analysis time
        time_End, & ! Time at the End of load step
        tref, & ! Reference temperature
        dtime, & ! Time step length
        dtime0, & ! Initial timestep (given as input)
        dtime_min, & ! Minimum time step length
        dload, & ! Load step length
        grav(3), & ! Gravity load
        grav0(3), & ! Explicit gravity load
        grav_End(3), & ! Gravity load at the End of load step
        max_rn, & ! Maximum relative value for the vector norm of the force residual
        max_ple, & ! Maximum allowed plastic strain increment
        maxgamma, & ! Maximum effective plastic strain increment
        dl_min, & ! Parameters for line search
        rn_ratio, & ! Parameters for line search
        time_output, & ! Last output time
        dtime_output, & ! Output time interval
        cp(3), & ! Current calculation point
        rupture_eps, & ! Effective plastic strain value for rupture modelling
        rupture_p, & ! Pressure of the gas cavity after the rupture
        newmark_beta, & ! Parameter for Implicit Newmark time integration
        newmark_gamma, & ! Parameter for Implicit Newmark time integration
        elastic_strain_energy, &
        delta_plastic_strain_energy, &
        plastic_strain_energy, &
        kinetic_energy, &
        potential_energy
    LOGICAL :: &
        restart, & ! Flag for restart calculation
        last_step, & ! Flag for last load step
        linear_load, & ! Make a linear interpolation for the loads between load steps
        dof_numbering, & ! true If there is a need for DOF numbering
        outwrt, & ! true If the out put was written for the current load step
        init_fe, & ! true If FE modelling paramers nedd initialization
        symmetric_matrix, & ! True If stIffness matrix is symmetric
        elastic_matrix, & ! True forces to use elastic tangent stIffness matrix
        pressure_matrix, & ! Use stIffnes matrix due to pressure loads
        lprnmat, & ! If true print out tangent stIffness matrix and STOP
        lerror, & ! Error state
        quiet, & ! If true suppress output to stdout excluding error messages
        lrupture ! Rupture modelling for a gas cavity
    TYPE(node_type), POINTER :: &
        first_node,last_node ! POINTERs to the nodes
    TYPE(coupled_set), POINTER :: &
        first_coupled_set,last_coupled_set ! POINTERs for coupled DOF sets
    TYPE(gascav_type), POINTER :: &
        first_gascav,last_gascav ! POINTERs to the gas cavity Data

    CONTAINS
    !
    FUNCTION node_coords (i, csys) RESULT(x_node)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Find node coordinates x for node i
    INTEGER(ipk) :: i,csys,in
    REAL(r8k) :: x_node(3)
    TYPE(node_type), POINTER :: nextn

    x_node = 0.0_r8k

    nextn => first_node
    DO WHILE ( ASSOCIATED(nextn) )
        in = nextn%number
        IF ( nextn % label == i ) THEN
            SELECT CASE (csys)
            CASE ( 0 )
                x_node(1:nd) = x0(1:nd,in)
            CASE ( 1 )
                x_node(1:nd) = x(1:nd,in)
            CASE ( 2 )
                x_node(1:nd) = u(1:nd,in)
            END SELECT
            RETURN
        ENDIF
        nextn => nextn%next
    ENDDO

    END FUNCTION node_coords
    !
    !
    !
    SUBROUTINE set_node_status(label,idim,status)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Set node DOF status
    INTEGER(ipk), INTENT(IN) :: label, idim, status
    TYPE(node_type), POINTER :: nextn

    nextn => first_node
    DO WHILE ( ASSOCIATED(nextn) )
        IF ( nextn % label == label ) THEN
            nextn % dof_status(idim) = status
            RETURN
        ENDIF
        nextn => nextn%next
    ENDDO

    END SUBROUTINE set_node_status
    !
    !
    !
    FUNCTION get_node_status (label, idim)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Get node DOF status
    INTEGER(ipk) :: label,idim,get_node_status
    TYPE(node_type), POINTER :: nextn

    get_node_status = 0

    nextn => first_node
    DO WHILE ( ASSOCIATED(nextn) )
        IF ( nextn % label == label ) THEN
            get_node_status = nextn % dof_status(idim)
            RETURN
        ENDIF
        nextn => nextn%next
    ENDDO

    END FUNCTION get_node_status
    !
    !
    !  
    SUBROUTINE set_vpt (label, vpt0, V0)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Set initial VpT value for gas cavity
    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: vpt0,V0
    TYPE(gascav_type), POINTER :: current_cav

    current_cav => first_gascav
    DO WHILE ( ASSOCIATED(current_cav) )
        IF ( current_cav%label == label ) EXIT
        current_cav => current_cav%next
    ENDDO

    IF ( ASSOCIATED(current_cav) ) THEN
        current_cav%VpT0 = vpt0
        current_cav%V0 = V0
    ENDIF

    END SUBROUTINE set_vpt
    !
    !
    !
    SUBROUTINE gascav_failure(label,p)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Switch for prescribed pressure in gas cavity

    INTEGER(ipk), INTENT(IN) :: label
    REAL(r8k), INTENT(IN) :: p
    TYPE(gascav_type), POINTER :: current_cav

    current_cav => first_gascav
    DO WHILE ( ASSOCIATED(current_cav) )
        IF ( current_cav%label == label ) EXIT
        current_cav => current_cav%next
    ENDDO

    IF ( ASSOCIATED(current_cav) ) THEN
        current_cav%failed = .TRUE.
        current_cav%failed0 = .TRUE.
        current_cav%p = p
    ENDIF

    END SUBROUTINE gascav_failure
    !
END MODULE common_parameters


