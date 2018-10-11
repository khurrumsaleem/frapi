MODULE nuclear_fuel_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> UO2 fuel material properties

    LOGICAL, target :: fuel_used
    LOGICAL, target :: fuel_plast
    LOGICAL, target :: fuel_creep

    CONTAINS
    !
    FUNCTION fuel_mat_par (temp, keyword)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Return UO2 fuel material property
    REAL(r8k) :: temp, fuel_mat_par
    CHARACTER(LEN=8) :: keyword

    fuel_mat_par = 0.0_r8k

    SELECT CASE ( keyword )
    CASE ( 'EMOD' )
       fuel_mat_par = 1.0e11_r8k
    CASE ( 'PENALTY' )
       fuel_mat_par = 1.0e11_r8k
    CASE ( 'FROCEF' )
       fuel_mat_par = 0.0_r8k
    END SELECT

    END FUNCTION fuel_mat_par
    !
    !
    !
    SUBROUTINE fuel_radial_Return (temp, mu, dtime, epseff0, taueff, gamma, dplmod)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Radial return for power law yield function
    REAL(r8k), INTENT(IN) :: temp, mu, epseff0, taueff, dtime
    REAL(r8k), INTENT(INOUT) :: gamma,dplmod

    END SUBROUTINE fuel_radial_Return
    !
END MODULE nuclear_fuel_fraptran














