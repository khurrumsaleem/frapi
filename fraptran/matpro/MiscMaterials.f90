! This file contains the following miscellaneous material properties:
! Crud, Plenum spring
    
MODULE Crud_Data
    USE Kinds
    USE conversions_fraptran
    USE Functions
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains crud properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/26/2015
    !
    PRIVATE
    PUBLIC :: CrudProperties
    !
    ! Crud thermal conductivity is assumed as a constant value of 0.8648 W/m*K
    REAL(r8k), PARAMETER :: CrudCond = 0.8648_r8k
    !
    TYPE CrudProperties
        REAL(r8k) :: Temp
    CONTAINS
        PROCEDURE :: MatProp
    END TYPE CrudProperties
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (Crud, property) RESULT (mat_prop)
    USE Kinds
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calls of the crud material properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    ! Inputs
    !
    ! property    - material property to be calculated
    ! temp        - material temperature
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    CLASS (CrudProperties) :: Crud
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THERMCOND')  ! Thermal Conductivity
        mat_prop = CrudCond
    CASE DEFAULT ! Wrong property called
        WRITE (0, 100) property
        WRITE (ounit, 100) property
100     FORMAT ('Error in Module Crud. Invalid material property ID. Material Property ID = ',a20)
        ERROR STOP 'Error in Module Crud. Invalid material property called'
    END SELECT
    !
    END FUNCTION MatProp
    !
END MODULE Crud_Data
!
!
!
MODULE PlenumSpring
    USE Kinds
    USE conversions_fraptran
    USE Functions
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains all plenum spring properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/26/2015
    !
    PRIVATE
    PUBLIC :: SpringProperties
    !
    ! Spring Properties
    ! Linear expansion coefficient, 1/K
    REAL(r8k), PARAMETER, PRIVATE :: SpringAlpha = 1.39e-5_r8k
    ! Reference Temperature, K
    REAL(r8k), PARAMETER, PRIVATE :: SpringTref = 294.26_r8k
    !
    TYPE SpringProperties
        REAL(r8k) :: Temp
    CONTAINS
        PROCEDURE :: MatProp
        PROCEDURE, PRIVATE :: thexp
    END TYPE SpringProperties
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatProp (Spring, property) RESULT (mat_prop)
    USE Kinds
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This function calls plenum spring material properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    ! Inputs
    !
    ! property    - material property to be calculated
    ! temp        - material temperature
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    CLASS (SpringProperties) :: Spring
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THEXP')  ! Linear thermal expansion coefficient
        mat_prop = Spring%thexp ()
    CASE DEFAULT ! Wrong property called
        WRITE (0, 100) property
        WRITE (ounit, 100) property
100     FORMAT ('Error in Module PlenumSpring. Invalid material property ID. Material Property ID = ',a20)
        ERROR STOP 'Error in Module PlenumSpring. Invalid material property called'
    END SELECT
    !
    END FUNCTION MatProp
    !
    !
    !
    REAL(r8k) FUNCTION thexp (Spring)
    USE Kinds
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> Calculates radial thermal expansion of the spring (assumed behaves isotropically)
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/20/2015
    !
    ! Input
    !
    ! Spring%Temp  - Spring temperature, K
    !
    ! Output
    !
    ! Springthexp  - spring thermal strain (unitless)
    !
    CLASS (SpringProperties), INTENT(IN) :: Spring
    !
    thexp = SpringAlpha * (Spring%Temp - SpringTref)
    !
    END FUNCTION thexp
    !
END MODULE PlenumSpring

