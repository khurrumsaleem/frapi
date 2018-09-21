MODULE FEA_deallocate_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to deallocate all FEA variables_frapcon.
    !> Subroutines include nlfemp_deallocate
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/24/2015
    !
    CONTAINS
    !
    SUBROUTINE nlfemp_deallocate()
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE sparse_matrix_frapcon
    USE materials_frapcon
    USE spring_frapcon
    USE quad4_frapcon
    USE hex8_frapcon
    USE solid1d_frapcon
    USE geometry_fpn_frapcon
    USE pressure1d_frapcon
    USE pressure2d_frapcon
    USE pressure3d_frapcon
    USE data_grid_frapcon
    USE gascav1d_frapcon
    USE gascav2d_frapcon
    USE gascav3d_frapcon
    USE cont1d_frapcon
    USE cont2d_frapcon
    USE cont3d_frapcon
    IMPLICIT NONE
    !>@brief
    !> Deallocate all finite element variables
    !
    TYPE(node_type), POINTER :: current_node, previous_node
    TYPE(coupled_set), POINTER :: current_set, previous_set
    TYPE(gascav_type), POINTER :: current_gascav, previous_gascav

    ! Deallocate all ALLOCATABLE array variables
    IF (ALLOCATED(x0)) DEALLOCATE(x0, x, u, v, a, temp, Fgrav, Fext, Fint, enumber, &
                                  dof_number, node_labels, du, fres, real_tmp, int_tmp)

    ! Deallocate mass matrix
    IF (ALLOCATED(values_mm)) DEALLOCATE(values_mm)

    ! Deallocate nodes
    current_node => first_node
    DO WHILE (ASSOCIATED(current_node))
        previous_node => current_node
        current_node => current_node%next
        DEALLOCATE(previous_node)
    END DO

    ! Deallocate coupled sets
    current_set => first_coupled_set
    DO WHILE (ASSOCIATED(current_set))
        previous_set => current_set
        current_set => current_set%next
        DEALLOCATE(previous_set)
    END DO

    ! Deallocate gas cavity data
    current_gascav => first_gascav
    DO WHILE (ASSOCIATED(current_gascav))
        previous_gascav => current_gascav
        current_gascav => current_gascav%next
        DEALLOCATE(previous_gascav)
    END DO

    ! Deallocate geometries
    CALL deallocate_geometry()

    ! Deallocate all variables in all modules
    CALL mat_deallocate()
    CALL sparse_matrix_deallocate()
    CALL spring_deallocate()
    CALL quad4_deallocate()
    CALL hex8_deallocate()
    CALL solid1d_deallocate()
    CALL pressure1d_deallocate()
    CALL pressure2d_deallocate()
    CALL pressure3d_deallocate()
    CALL grid_deallocate()
    CALL gascav1d_deallocate()
    CALL gascav2d_deallocate()
    CALL gascav3d_deallocate()
    CALL cont1d_deallocate()
    CALL cont2d_deallocate()
    CALL cont3d_deallocate()
    !
    END SUBROUTINE nlfemp_deallocate
    !
END MODULE FEA_deallocate_frapcon



