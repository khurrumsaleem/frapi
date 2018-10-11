MODULE fraptran_variables_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> FRAPTRAN temporary storage

    INTEGER(ipk), target :: nax
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: elev ! Elevations
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: axsliceheight
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_width
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, target :: ut
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: tempt
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, target :: epsplt
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: epsefft
    LOGICAL, DIMENSION(:), ALLOCATABLE, target :: closedt, stickt
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: dz0t
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: prt
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, target :: flag2d
    LOGICAL, target :: failed_cladding
    INTEGER(ipk), target :: rupture_model ! 0 = use only effective plastic strain criterion_fraptran
                                ! 1 = NUREG 0630 fast ramp correlation
                                ! 2 = NUREG 0630 slow ramp correlation
    REAL(r8k), target :: Prupt,epsrupt,h_rupt
    INTEGER(ipk), target :: clad_Cladtype
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_OxygenConcenAve
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_EffFastFluStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_EffFastFluStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_EffColdWkStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE, target :: clad_EffColdWkStrnHardExp
    REAL(r8k), target :: clad_frcoef

END MODULE fraptran_variables_fraptran













