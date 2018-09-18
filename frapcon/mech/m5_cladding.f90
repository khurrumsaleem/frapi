MODULE m5_cladding_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    IMPLICIT NONE
    !>@brief
    !> M5 cladding material properties
    LOGICAL :: m5_clad_used
    LOGICAL :: m5_clad_plast
    LOGICAL :: m5_clad_creep
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION m5_clad_mat_par (temp, keyword)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Returns cladding material property
    REAL(r8k) :: temp
    CHARACTER(LEN=8) :: keyword

    m5_clad_mat_par = 0.0_r8k

    ! Stop the program
    WRITE (ounit,FMT='(/,A,/)') 'ERROR: M5 cladding material library not included in this version '
    lerror = .TRUE.
    CALL nlfemp_stop(0)

    END FUNCTION m5_clad_mat_par
    !
    !
    !
    SUBROUTINE m5_clad_radial_return (temp, mu, dtime, epseff0, taueff,gamma, dplmod, deds)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Radial return for power law yield function
    !
    REAL(r8k) :: temp, mu, epseff0, taueff, dtime
    REAL(r8k) :: gamma, dplmod, deds

    END SUBROUTINE m5_clad_radial_return
    !
    !
    !
    SUBROUTINE m5_clad_creep_calc (temp, mu, dtime, epseff0, taueff, gamma, deds)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Cladding creep calculation
    !
    REAL(r8k) :: temp, mu, dtime, epseff0, taueff, gamma, deds
    !
    END SUBROUTINE m5_clad_creep_calc
    !
END MODULE m5_cladding_frapcon



