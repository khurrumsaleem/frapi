MODULE HeatConduction_h
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comdeck HeatConduction
    TYPE HeatConduction_var
        INTEGER(ipk) :: Nchan
        ! Surface area at xo (ht1tdp.f)
        REAL(r8k) :: areao
        ! Surface area at xn time dependent surface area = pi * drod (ht1tdp.f)
        REAL(r8k) :: arean
        ! Thermal convergence criterion
        REAL(r8k) :: epsht1
        !
        REAL(r8k) :: GapConductivity
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: BoundaryCondition
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ThermalConductAdv
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: FinalTemp
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevTemp
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ArrayE
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ArrayF
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: VolumeWeightL
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: VolumeWeightR
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: AreaWeight
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ThermalConductivity
        ! Storage area with channel data and coolant conditions for coolant channel n
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: acond
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RhoCp
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RhoCp0
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: PrevIterateTemp
    END TYPE HeatConduction_var
    !
    INTEGER(ipk) :: Nchan
    REAL(r8k) :: areao
    REAL(r8k) :: arean
    REAL(r8k) :: epsht1
    REAL(r8k) :: GapConductivity
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: BoundaryCondition
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ThermalConductAdv
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: FinalTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: PrevTemp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ArrayE
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ArrayF
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: VolumeWeightL
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: VolumeWeightR
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: AreaWeight
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ThermalConductivity
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: acond
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RhoCp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: RhoCp0
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: PrevIterateTemp
    !
    END MODULE HeatConduction_h