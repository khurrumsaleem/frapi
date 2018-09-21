MODULE nuclear_fuel_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> UO2 fuel material properties
    !
    LOGICAL :: fuel_used
    LOGICAL :: fuel_plast
    LOGICAL :: fuel_creep
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION fuel_mat_par (temp, keyword)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Returns UO2 fuel material property
    !
    REAL(r8k) :: temp
    CHARACTER(LEN=8) :: keyword

    fuel_mat_par = 0.0_r8k

    SELECT CASE (keyword)
    CASE ('EMOD')
        fuel_mat_par = 1.0e11_r8k
    CASE ('PENALTY')
        fuel_mat_par = 1.0e11_r8k
    CASE ('FROCEF')
        fuel_mat_par = 0.0_r8k
    END SELECT

    END FUNCTION fuel_mat_par
    !
    !
    !
    SUBROUTINE fuel_radial_return (temp, mu, dtime, epseff0, taueff, gamma, dplmod)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Radial return for power law yield function
    !
    REAL(r8k), INTENT(IN) :: temp,mu,epseff0,taueff,dtime
    REAL(r8k), INTENT(INOUT) :: gamma,dplmod
    !
    END SUBROUTINE fuel_radial_return
    !
END MODULE nuclear_fuel_frapcon




