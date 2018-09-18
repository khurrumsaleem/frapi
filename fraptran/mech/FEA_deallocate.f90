MODULE FEA_deallocate_fraptran
    !>@brief
    !> This module contains the subroutines used to deallocate all FEA variables_fraptran.
    !> Subroutines include nlfemp_deallocate
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    USE Kinds_fraptran
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE nlfemp_deallocate()
    USE Kinds_fraptran
    USE common_parameters_fraptran
    USE sparse_matrix_fraptran
    USE materials_frap_fraptran
    USE spring_fraptran
    USE quad4_fraptran
    USE hex8_fraptran
    USE solid1d_fraptran
    USE geometry_fraptran
    USE pressure1d_fraptran
    USE pressure2d_fraptran
    USE pressure3d_fraptran
    USE Data_grid_fraptran
    USE gascav1d_fraptran
    USE gascav2d_fraptran
    USE gascav3d_fraptran
    USE cont1d_fraptran
    USE cont2d_fraptran
    USE cont3d_fraptran
    IMPLICIT NONE
    !>@brief
    !> Deallocate all finite element variables
    TYPE(node_type), POINTER :: current_node, previous_node
    TYPE(coupled_set), POINTER :: current_set, previous_set
    TYPE(gascav_type), POINTER :: current_gascav, previous_gascav

    ! Deallocate all allocatable array variables
    IF ( ALLOCATED(x0) ) DEALLOCATE (x0,x,u,v,a,temp,Fgrav,Fext,Fint,enumber, &
      &                              dof_number,node_labels,du,fres,Real_tmp,int_tmp)

    ! Deallocate mass matrix
    IF ( ALLOCATED(values_mm) ) DEALLOCATE(values_mm)

    ! Deallocate nodes
    current_node => first_node
    DO WHILE ( ASSOCIATED(current_node) )
        previous_node => current_node
        current_node => current_node%next
        DEALLOCATE(previous_node)
    ENDDO

    ! Deallocate coupled sets
    current_set => first_coupled_set
    DO WHILE ( ASSOCIATED(current_set) )
        previous_set => current_set
        current_set => current_set%next
        DEALLOCATE(previous_set)
    ENDDO

    ! Deallocate gas cavity Data
    current_gascav => first_gascav
    DO WHILE ( ASSOCIATED(current_gascav) )
        previous_gascav => current_gascav
        current_gascav => current_gascav%next
        DEALLOCATE(previous_gascav)
    ENDDO

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

    END SUBROUTINE nlfemp_deallocate    
    
       !
END MODULE FEA_deallocate_fraptran













