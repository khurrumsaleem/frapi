MODULE zirconium_fraptran_fraptranDioxide_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    USE functions_fraptran
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This Module contains all Zirconium Properties_fraptran
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    PRIVATE
    PUBLIC :: ZircaloyDioxide
    !
    TYPE ZircaloyDioxide
        REAL(r8k) :: Temp
    CONTAINS
        PROCEDURE :: MatPropZrO2
        PROCEDURE, PRIVATE :: Zotcon
    END TYPE ZircaloyDioxide
    ! Theoretical Density (g/cm^3)
    REAL(r8k), PARAMETER :: ZrO2TD = 5.68_r8k
    ! Melting temperature
    REAL(r8k), PARAMETER :: ZrO2TMelt = 2098.15_r8k
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION MatPropZrO2 (ZrO2, Property) RESULT (mat_prop)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calls all of the Zirconium dioxide Material Properties
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 2/21/2015
    !
    ! Inputs
    !
    ! property    - material property to be calculated
    ! ZrO2        - Class containing spring properties
    !
    ! Outputs
    !
    ! mat_prop    - calculated material property value
    !
    CLASS (ZircaloyDioxide), INTENT(IN) :: ZrO2
    CHARACTER(LEN=*), INTENT(IN) :: property
    !
    SELECT CASE (property)
    CASE ('THERMCOND')       ! Thermal Conductivity
        mat_prop = Zro2%zotcon ()
    CASE ('TDENSITY') ! Theoretical Density, (g/cm^3)
        mat_prop = ZrO2TD
    CASE ('TMELT')           ! Melting Temperature
        mat_prop = ZrO2TMelt
    CASE ('SPECHEAT')        ! Specific Heat (J/kg*K)
        mat_prop = 0.0_r8k
    CASE ('SWELLING')        ! Swelling
        mat_prop = 0.0_r8k
    CASE ('THEXP')           ! Thermal Expansion
        mat_prop = 0.0_r8k
    CASE ('EMISS')           ! Emissivity
        mat_prop = 0.0_r8k
    CASE ('DENSIFICATION')   ! Densification
        mat_prop = 0.0_r8k
    CASE ('ENTHALPY')        ! Enthalpy
        mat_prop = 0.0_r8k
    CASE ('RELOCATION')      ! Relocation
        mat_prop = 0.0_r8k
    CASE ('POIS_RATIO')      ! Poisson's Ratio
        mat_prop = 0.0_r8k
    CASE ('YOUNG_MOD')       ! Young's Modulus
        mat_prop = 0.0_r8k
    CASE ('YIELD_STRESS')    ! Yield stress
        mat_prop = 0.0_r8k
    CASE ('THEXP_COEF')      ! Thermal expansion coefficient
        mat_prop = 0.0_r8k
    CASE ('CREEP')           ! Creep
        mat_prop = 0.0_r8k
    CASE ('AXIAL_GROW')      ! Axial Growth
        mat_prop = 0.0_r8k
    CASE ('SHEAR_MOD')       ! Shear Modulus
        mat_prop = 0.0_r8k
    CASE ('MEYER_HARD')      ! Meyer's Hardness
        mat_prop = 0.0_r8k
    CASE DEFAULT ! Wrong property called
        WRITE (0,100) property
        WRITE (ounit,100) property
100     FORMAT ('Error in Module zirconiumDioxide. Invalid material property ID. Material Property ID = ',a20)
        ERROR STOP 'Error in Module zirconiumDioxide. Invalid material property called'
    END SELECT
    
    END FUNCTION MatPropZrO2
    !
    !
    !
    REAL(r8k) FUNCTION zotcon (ZrO2)
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> zotcon calculates zirconium dioxide thermal conductivity as a function of temperature.
    !>@author
    !> zotcon was coded by r.l.miller in march 1974.
    !> modified by r.l. miller  may 1974
    !
    ! Input
    !
    ! ZrO2%temp - Cladding meshpoint temperature (k)
    !
    ! Output
    !
    ! zotcon    - Thermal conductivity of zro2 (w/m-k)
    !
    ! Reference:
    !
    ! (1) w.d. kingery, j. francl, r.l. coble and t. vasilos, j. amer. ceram. soc., 37 no.i (1954) pp 107-110
    !
    ! Note:
    !
    ! Use caution above 1700k_fraptran
    !
    CLASS (ZircaloyDioxide), INTENT(IN) :: ZrO2
    
    zotcon = 1.9599_r8k - ZrO2%Temp * (2.41e-4_r8k - ZrO2%Temp * (6.43e-7_r8k - ZrO2%Temp * 1.946e-10_r8k))
    
    END FUNCTION zotcon
    !
END MODULE zirconium_fraptran_fraptranDioxide_fraptran













