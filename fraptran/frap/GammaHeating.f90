MODULE GammaHeating
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module is used to model the gamma-ray heating distribution of the fuel, cladding and coolant.
    !>@author
    !> Ian Porter, University of South Carolina
    !>@date
    !> 2014
    !
    CONTAINS
    !
    REAL(r8k) PURE FUNCTION GammaClad (WaterDensity)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine GammaClad calculates the gamma-ray heating of the cladding as a function of coolant density
    !>@author
    !> Ian Porter, University of South Carolina
    !>@date
    !> 2014
    !
    ! Input
    !
    ! WaterDensity - Density of the water (g/cm^3)
    !
    ! Output
    !
    ! GammaClad    - Fraction of energy deposited in the cladding
    !
    ! Reference:
    !
    ! (1) Equation developed by Ian Porter for a 17x17 PWR fuel assembly assuming 31% of decay energy comes from gamma rays
    !     "FUEL PERFORMANCE ASSESSMENT WHEN MODELING GAMMA HEATING DURING STEADY-STATE AND TRANSIENT SCENARIOS",
    !     Proceedings of ICAPP 2014, Charlotte, NC USA April 6-9, 2014
    !
    REAL(r8k), INTENT(IN) :: WaterDensity
    REAL(r8k), PARAMETER :: GammaClad_Min = 1.0e-10_r8k
    
    ! Cladding gamma-ray heating fraction
    GammaClad = 2.4682E-2_r8k - 5.91E-4_r8k * WaterDensity - 8.98E-4_r8k * WaterDensity ** 2
    
    ! Ensure does not go negative
    GammaClad = MAX(GammaClad, GammaClad_Min)
    
    END FUNCTION GammaClad
    !
    !
    !
    REAL(r8k) PURE FUNCTION GammaCoolant (WaterDensity)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This subroutine calculates the gamma-ray heating of the coolant
    !>@author
    !> Ian Porter, University of South Carolina
    !>@date
    !> 2014
    !
    ! Input
    !
    ! WaterDensity - Density of the water (g/cm^3)
    !
    ! Output
    !
    ! GammaCoolant - Fraction of total energy that goes directly to the coolant, %
    !
    ! Reference:
    !
    ! (1) Equation developed by Ian Porter for a 17x17 PWR fuel assembly assuming 31% of decay energy comes from gamma rays
    !     "FUEL PERFORMANCE ASSESSMENT WHEN MODELING GAMMA HEATING DURING STEADY-STATE AND TRANSIENT SCENARIOS",
    !     Proceedings of ICAPP 2014, Charlotte, NC USA April 6-9, 2014
    !
    REAL(r8k), INTENT(IN) :: WaterDensity
    REAL(r8k), PARAMETER :: GammaCoolant_Min = 1.0e-10_r8k
    
    ! Coolant gamma-ray heating fraction
    GammaCoolant = 3.453E-3_r8k + 2.613E-2_r8k * WaterDensity - 3.228E-3_r8k * WaterDensity ** 2
    
    ! Ensure does not go negative
    GammaCoolant = MAX(GammaCoolant, GammaCoolant_Min)
        
    END FUNCTION GammaCoolant
    !
END MODULE GammaHeating

