MODULE FissionGasRelease_h
    USE Kinds
    !>@brief
    !> This module replaces the comdeck FissionGasRelease and transfgr
    IMPLICIT NONE
    TYPE FissionGasRelease_var
        INTEGER(ipk) :: ngasr
        INTEGER(ipk) :: TranSwell         ! Flag to turn on user-supplied transient fuel swelling
        INTEGER(ipk) :: presfgr           ! Suboption for fission gas release history (0=off, 1=user-input pairs, 2=FRAPFGR)
        INTEGER(ipk) :: nFuelSwellPairs   ! # of pairs of fuel swelling values
        INTEGER(ipk) :: nFGRpairs         ! # of pairs of fission gas release values
        REAL(r8k) :: GasMoles0            ! Total gas moles
        REAL(r8k) :: prodxe               ! Produced fission gas of Xe, assuming 31cm3/MWd of burnup
        REAL(r8k) :: prodkr               ! Produced fission gas of Kr, assuming 31cm3/MWd of burnup
        REAL(r8k) :: relfract             ! Current FGR fraction
        REAL(r8k) :: TranFuelSwell        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gsmol0            !7. Moles of individual gases
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: relfraca          !2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelGasSwell      !2000
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ansr                                ! 51.
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: fmgp                                ! 151.
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasavail1                         ! 151,51.
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasavail2                         ! 151,51.
    END TYPE FissionGasRelease_var
    !
    INTEGER(ipk) :: ngasr
    !
    INTEGER(ipk) :: TranSwell
    ! Suboption for fission gas release history (0=off, 1=user-input pairs, 2=FRAPFGR)
    INTEGER(ipk) :: presfgr
    ! # of pairs of fuel swelling values
    INTEGER(ipk) :: nFuelSwellPairs
    ! # of pairs of fission gas release values
    INTEGER(ipk) :: nFGRpairs
    !
    REAL(r8k) :: GasMoles0
    !
    REAL(r8k) :: prodxe
    !
    REAL(r8k) :: prodkr
    !
    REAL(r8k) :: relfract
    !
    REAL(r8k) :: TranFuelSwell
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gsmol0
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: relfraca
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FuelGasSwell
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ansr
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: fmgp
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasavail1
    !
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: gasavail2
    !
    END MODULE FissionGasRelease_h












