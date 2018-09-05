MODULE frapc
    USE Kinds
    IMPLICIT NONE
    !> @brief
    !> This module replaces the frapc.h common block and allocate/deallocates the arrays needed for link with T/H Code.
    !> It also fixes several parameters (unit conversion, ensuring proper ordering) that can occur with T/H Code.
    !> @author
    !> Ian Porter, NRC
    !> @date
    !> March 2014
    !
    TYPE frapc_var
        ! T/H Code corresponding heat structure #
        INTEGER(ipk) :: idx2
        ! T/H Code # of radial nodes
        INTEGER(ipk) :: ncorlp
        ! T/H Code radial node corresponding to clad inner surface
        INTEGER(ipk) :: ncirlp
        ! T/H Code radial node corresponding to fuel outer surface
        INTEGER(ipk) :: nforlp
        ! T/H Code # of axial nodes
        INTEGER(ipk) :: kmxrlp
        ! FRAPTRAN # of axial nodes
        INTEGER(ipk) :: kmxfrp
        ! Link with T/H code
        INTEGER(ipk) :: ncool2
        ! Switch to advance in time or perform calculations only at time t12
        INTEGER(ipk) :: ndtadv
        ! FRAPTRAN control switch to read input cards this Call (0-No, 1-Yes)
        INTEGER(ipk) :: ncard2
        ! Control switch on storage place of FrapTran restart Data (0-lcm storage)
        INTEGER(ipk) :: nrest2
        ! Plenum pressure
        REAL(r8k) :: tprlp
        ! Flag to specify whether the FRAPTRAN arrays have been allocated
        LOGICAL :: allocate_arrays_ft
        ! Flag to specify whether or not to convert units between the T/H Code & FRAPTRAN
        LOGICAL :: convert_units
        !
        LOGICAL :: setunits
        ! Indicates whether it is the first pass to FRAPTRAN for the given fuel rod
        LOGICAL :: first_pass
        ! FRAPTRAN File Name
        CHARACTER(LEN=80) :: FrapTranFN
        ! FRAPTRAN cladding failure indicator (0-No, 1-Yes)
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Iffrp
        ! T/H Code temperatures
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tmprlp
        ! T/H Code Radial dimensions. rmrlp(1) = fuel centerline, rmrlp(lmax) = radius of cladding outside surface
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: rmrlp
        ! FRAPCON Gap Conductance
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hgpfrp
        ! T/H Code pressure
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pclrlp
        ! T/H Code Axial dimensions
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ElevatThermHydr
        ! FRAPTRAN elevation of k-th axial node (m). Bottom of fuel pellet stack assumed to have elevation of zero
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ElevatFrap
        ! FRAPTRAN fuel rod outer diameter
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: drdfrp
        ! FRAPTRAN fuel void fraction due to relocation (unitless)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vrlfrp
        ! FRAPTRAN fuel rod internal gas pressure (n/m**2)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: pgpfrp
        ! FRAPTRAN fuel burnup (MW-sec/kg)
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: bufrp
        ! Heat transfer coefficient to the liquid
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HTC_L
        ! Heat transfer coefficient to the vapor
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: HTC_V
        ! Bulk coolant temperature of the liquid
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: TBulk_L
        ! Bulk coolant temperature of the vapor
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: TBulk_V
    END TYPE frapc_var
    ! Integers
    INTEGER(ipk) :: idx2
    INTEGER(ipk) :: ncorlp
    INTEGER(ipk) :: ncirlp
    INTEGER(ipk) :: nforlp
    INTEGER(ipk) :: kmxrlp
    INTEGER(ipk) :: kmxfrp
    INTEGER(ipk) :: ncool2
    INTEGER(ipk) :: ndtadv
    INTEGER(ipk) :: ncard2
    INTEGER(ipk) :: nrest2
    INTEGER(ipk) :: ntimes ! Number of output data timesteps provided by T/H Code
    ! Reals
    REAL(r8k) :: tprlp                                     ! T/H code plenum gas temperature
    REAL(r8k) :: t12 = 0                                   ! Start time for FRAPTRAN calculations (s)
    REAL(r8k) :: t22 = 0                                   ! End time for FRAPTRAN calculations (s)
    ! Logicals
    LOGICAL :: Allocate_Arrays_FT
    LOGICAL :: convert_units
    LOGICAL :: setunits                                    ! Flag to specify whether the Units have been set for the FRAPTRAN files
    LOGICAL :: first_pass                                  ! Flag to specify whether its the first pass through HtStrCrunchM
    LOGICAL :: coupled                                     ! Flag to specify whether the program is coupled.  Set by ncool2 flag.
    LOGICAL :: first_call = .TRUE.                         ! Flag to specify whether its the first pass through HtStrCrunchM
    ! Characters
    CHARACTER(LEN=80) :: FrapTranFN                        !The name of the fraptran input file
    !             *** Arrays ***
    ! Integers
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: Iffrp                ! FRAPTRAN cladding failure indicator (0-No, 1-Yes)
    ! Reals
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tmprlp                !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: rmrlp                   !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hgpfrp                  ! FRAPTRAN gas gap conductance (W/m**2-K)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pclrlp                  ! T/H Code coolant pressure (n/m**2)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ElevatThermHydr         !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ElevatFrap              !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: drdfrp                  !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vrlfrp                  ! FRAPTRAN fuel void fraction due to relocation (unitless)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: pgpfrp                  ! FRAPTRAN fuel rod internal gas pressure (n/m**2)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: bufrp                   ! FRAPTRAN fuel burnup (MW-sec/kg)
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tEnd                    ! FRAPTRAN End time as determined by T/H Code
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gfInt                   ! FRAPTRAN plot time as determined by T/H Code
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: edInt                   ! FRAPTRAN output time as determined by T/H Code
    REAL(r8k) :: gsmfrp                                             ! FRAPTRAN gram moles of gas
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HTC_L                   ! Heat transfer coefficient to the liquid
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: HTC_V                   ! Heat transfer coefficient to the vapor
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: TBulk_L                 ! Bulk coolant temperature of the liquid
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: TBulk_V                 ! Bulk coolant temperature of the vapor
    !
END MODULE frapc

