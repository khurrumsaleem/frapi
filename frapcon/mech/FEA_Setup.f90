MODULE FEA_Setup
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to initialized the Cladding FEA calculation.
    !> Subroutines include default_values, init
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/21/2015
    !
    CONTAINS
    !
    SUBROUTINE default_values()
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    IMPLICIT NONE
    !>@brief
    !> Set FEA variables to their default values

    ! Default starting time
    time0 = 0.0_r8k

    ! Minimum time step length
    dtime_min = 1.0e-25_r8k

    ! Do output only at the End of the load step
    dtime_output = HUGE(dtime_output)

    ! No gravity load
    grav0 = 0.0_r8k

    ! Static 3D analysis by default
    nd = 3
    dimens = '3D'
    analys = 'STATIC'

    ! Use trapezoidal rule by default (no artificial damping)
    newmark_beta = 0.25_r8k
    newmark_gamma = 0.5_r8k

    ! Newton_Raphson solver parameters
    max_nnr = 25
    max_rn = 1.0e-6_r8k
    max_ple = 0.1_r8k

    ! Line search parameters
    max_nln = 10
    dl_min = 1.0e-3_r8k
    rn_ratio = 0.1_r8k
    nr_iter = 0

    ! No title
    title = 'untitled'

    ! Default reference temperature
    tref = 0.0_r8k

    ! Initialize analysis flags
    last_step = .FALSE.
    linear_load = .TRUE.
    outwrt = .FALSE.
    quiet = .FALSE.
    lrupture = .FALSE.

    ! Default groups
    ngroup = 0
    egroup = 0

    ! Ballooning model inactive
    balloon = 0

    ! units
    units = 2

    END SUBROUTINE default_values
    !
    !
    !
    SUBROUTINE init()
    USE Kinds
    USE conversions_frapcon
    USE common_parameters
    USE materials
    USE spring
    USE quad4
    USE hex8
    USE solid1d
    USE geometry_fpn
    USE pressure1d
    USE pressure2d
    USE pressure3d
    USE gascav1d
    USE gascav2d
    USE gascav3d
    USE cont1d
    USE cont2d
    USE cont3d
    USE FEA_deallocate
    IMPLICIT NONE
    !>@brief
    !> Initialize parameters and variables with default values

    ! Deallocate all existing variables
    CALL nlfemp_deallocate()

    ! Set counters to zero
    nnodes = 0
    ncoupled = 0
    nfixed = 0
    ndispl = 0
    noutput = 0
    ngascav = 0
    n_array = 0

    ! Initialize pointers
    first_node => NULL()
    last_node => NULL()
    first_coupled_set => NULL()
    last_coupled_set => NULL()
    first_gascav => NULL()
    last_gascav => NULL()

    ! Initialize analysis flags and parameters
    dof_numbering = .TRUE.
    init_fe = .TRUE.
    pressure_matrix = .FALSE.
    lprnmat = .FALSE.
    lerror = .FALSE.
    maxgamma = 0.0_r8k
    cp = 0.0_r8k

    ! Initialize elements
    CALL init_geometry()
    CALL mat_init()
    CALL spring_init()
    CALL quad4_init()
    CALL hex8_init()
    CALL solid1d_init()
    CALL pressure1d_init()
    CALL pressure2d_init()
    CALL pressure3d_init()
    CALL gascav1d_init()
    CALL gascav2d_init()
    CALL gascav3d_init()
    CALL cont1d_init()
    CALL cont2d_init()
    CALL cont3d_init()

    END SUBROUTINE init
    !
END MODULE FEA_Setup

