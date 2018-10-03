MODULE Uncertainty_Vals_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module contains the user_fraptran-input uncertainty values
    TYPE Uncertainty_Var
        ! INPUT: fuel thermal conductivity uncertainty multiplier
        REAL(r8k) :: sigfuelthermcond
        ! INPUT: cladding thermal conductivity uncertainty multiplier
        REAL(r8k) :: sigcladthermcond
        ! INPUT: fuel thermal expansion uncertainty multiplier
        REAL(r8k) :: sigfuelthermexp
        ! INPUT: cladding thermal expansion uncertainty multiplier
        REAL(r8k) :: sigcladthermexp
        ! INPUT: fuel specific heat capacity uncertainty multiplier
        REAL(r8k) :: sigfuelheatcapa
        ! INPUT: cladding yield strength uncertainty multiplier
        REAL(r8k) :: sigcladyieldstr
        ! INPUT: surface heat transfer coeefficient uncertainty multiplier
        REAL(r8k) :: sigsurfhtc
        ! Printout time for the DAKOTA output file
        REAL(r8k) :: tdkt
        ! INPUT:  2D array to specify DAKOTA output time intervals (similar to drmaxa, dtpoa, and dtplta)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtdkta
        ! Reference fuel enthalpy for dakota output file
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Uncert_fuelref
        ! Array to store all DAKOTA output parameters at every plot time step
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dktouts
        ! INPUT: logical to specify DAKOTA output is desired
        LOGICAL :: dakota
        ! # of input pairs in dtdkta (similar to  npltn)
        INTEGER(ipk) :: ndktn
        ! INPUT: # of parameters to be output for DAKOTA
        INTEGER(ipk) :: ndktparams
        ! Time step counter for dktouts array
        INTEGER(ipk) :: dktoutcounter
    END TYPE Uncertainty_Var
    !
    REAL(r8k) , target :: sigfuelthermcond
    REAL(r8k) , target :: sigfuelthermexp
    REAL(r8k) , target :: sigfuelheatcapa
    REAL(r8k) , target :: sigcladthermcond
    REAL(r8k) , target :: sigcladthermexp
    REAL(r8k) , target :: sigcladyieldstr
    REAL(r8k) , target :: sigsurfhtc
    REAL(r8k) , target :: tdkt
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dtdkta
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: dktouts
    LOGICAL , target :: dakota
    INTEGER(ipk) , target :: ndktn
    INTEGER(ipk) , target :: ndktparams
    INTEGER(ipk) , target :: dktoutcounter
    !
END MODULE Uncertainty_Vals_fraptran













