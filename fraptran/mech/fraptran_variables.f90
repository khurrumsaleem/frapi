MODULE fraptran_variables
    USE Kinds
    USE common_parameters
    IMPLICIT NONE
    !>@brief
    !> FRAPTRAN temporary storage

    INTEGER(ipk) :: nax
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: elev ! Elevations
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: axsliceheight
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_width
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: ut
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tempt
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: epsplt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: epsefft
    LOGICAL, DIMENSION(:), ALLOCATABLE :: closedt, stickt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dz0t
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: prt
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: flag2d
    LOGICAL :: failed_cladding
    INTEGER(ipk) :: rupture_model ! 0 = use only effective plastic strain criterion
                                ! 1 = NUREG 0630 fast ramp correlation
                                ! 2 = NUREG 0630 slow ramp correlation
    REAL(r8k) :: Prupt,epsrupt,h_rupt
    INTEGER(ipk) :: clad_Cladtype
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_OxygenConcenAve
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_EffFastFluStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_EffFastFluStrnHardExp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_EffColdWkStrenCoef
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: clad_EffColdWkStrnHardExp
    REAL(r8k) :: clad_frcoef

END MODULE fraptran_variables












