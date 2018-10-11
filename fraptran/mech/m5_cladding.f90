MODULE m5_cladding_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> M5 cladding material properties

    LOGICAL, target :: m5_clad_used
    LOGICAL, target :: m5_clad_plast
    LOGICAL, target :: m5_clad_creep

    CONTAINS
    !
    FUNCTION m5_clad_mat_par (temp, keyword)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> RETURN cladding material property
    REAL(r8k) :: temp, m5_clad_mat_par
    CHARACTER(LEN=8) :: keyword

    m5_clad_mat_par = 0.0_r8k

    ! Stop the program
    WRITE(UNIT=6,FMT='(/,A,/)') 'ERROR: M5 cladding material library not included in this version '
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END FUNCTION m5_clad_mat_par
    !
    !
    !
    SUBROUTINE m5_clad_radial_Return(temp, mu, dtime, epseff0, taueff, gamma, dplmod, deds)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Radial RETURN for power law yield function
    REAL(r8k) :: temp, mu, epseff0, taueff, dtime
    REAL(r8k) :: gamma, dplmod, deds

    END SUBROUTINE m5_clad_radial_Return
    !   
    !
    !
    SUBROUTINE m5_clad_creep_calc (temp, mu, dtime, epseff0, taueff, gamma, deds)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Cladding creep calculation
    REAL(r8k) :: temp, mu, dtime, epseff0, taueff, gamma, deds

    END SUBROUTINE m5_clad_creep_calc
    !
END MODULE m5_cladding_fraptran














