MODULE FEA_deallocate
    !>@brief
    !> This module contains the subroutines used to deallocate all FEA variables.
    !> Subroutines include nlfemp_deallocate
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    USE Kinds
    !
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE nlfemp_deallocate()
    USE Kinds
    USE common_parameters
    USE sparse_matrix
    USE materials_frap
    USE spring
    USE quad4
    USE hex8
    USE solid1d
    USE geometry
    USE pressure1d
    USE pressure2d
    USE pressure3d
    USE Data_grid
    USE gascav1d
    USE gascav2d
    USE gascav3d
    USE cont1d
    USE cont2d
    USE cont3d
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
END MODULE FEA_deallocate

