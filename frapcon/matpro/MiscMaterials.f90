! This file contains the following miscellaneous material properties:
! Crud, Plenum spring
    
MODULE Crud
    USE Kinds
    USE Conversions
    USE Functions
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains crud properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/26/2015
    !
    ! Crud thermal conductivity is assumed as a constant value of 0.8648 W/m*K
    REAL(r8k), PARAMETER :: CrudCond = 0.8648_r8k
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatPropCrud (property, temp) RESULT (mat_prop)
    USE Kinds
    USE Conversions
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
    REAL(r8k), INTENT(IN) :: temp
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THERMCOND')  ! Thermal Conductivity
        mat_prop = CrudCond
    CASE DEFAULT ! Wrong property called
        WRITE (0, 100) property
        WRITE (ounit, 100) property
100     FORMAT ('Error in Module Crud. Invalid material property ID. Material Property ID = ',a20)
        STOP
    END SELECT
    !
    END FUNCTION MatPropCrud
    !
END MODULE Crud
!
!
!
MODULE PlenumSpring
    USE Kinds
    USE Conversions
    USE Functions
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains all plenum spring properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 5/26/2015
    !
    ! Spring Properties
    ! Linear expansion coefficient, 1/K
    REAL(r8k), PARAMETER, PRIVATE :: SpringAlpha = 1.39e-5_r8k
    ! Reference Temperature, K
    REAL(r8k), PARAMETER, PRIVATE :: SpringTref = 294.26_r8k
    !
    PRIVATE :: Springthexp
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatPropSpring (property, temp) RESULT (mat_prop)
    USE Kinds
    USE Conversions
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
    REAL(r8k), INTENT(IN) :: temp
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THEXP')  ! Linear thermal expansion coefficient
        mat_prop = Springthexp (temp)
    CASE DEFAULT ! Wrong property called
        WRITE (0, 100) property
        WRITE (ounit, 100) property
100     FORMAT ('Error in Module PlenumSpring. Invalid material property ID. Material Property ID = ',a20)
        STOP
    END SELECT
    !
    END FUNCTION MatPropSpring
    !
    !
    !
    REAL(r8k) FUNCTION Springthexp (TempK)
    USE Kinds
    USE Conversions
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
    ! TempK  - Spring temperature, K
    !
    ! Output
    !
    ! Springthexp  - spring thermal strain (unitless)
    !
    REAL(r8k), INTENT(IN) :: TempK
    !
    Springthexp = SpringAlpha * (TempK - SpringTref)
    !
    END FUNCTION Springthexp
    !
END MODULE PlenumSpring