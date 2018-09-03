MODULE Comde
    USE Kinds
    USE Conversions
    USE Variables, ONLY : na, nr, im
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comde.h file. This is the common file used from the transuranis code.
    !> It is used by subroutines gaspro, tubrnp, turbin, turbo, gaprs and fueltp.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/7/2014
    !> 9/22/2014 - Made arrays dynamic (IP)
    !
    ! --- control variables
    INTEGER(ipk), TARGET :: lschni
    ! --- porosity, grain size and structural data
    REAL(r8k), TARGET :: por000
    ! --- variables needed for lwr burnup eqs.
    REAL(r8k), TARGET :: ntot
    ! --- neutron flux, power and burn-up
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: brnup1
    ! REAL(r8k), DIMENSION(m31max,ifemax,2) :: brnup3 !Note: These variables are now in Variables Module.
    ! REAL(r8k), DIMENSION(m31max,ifemax) :: formf    !Note: These variables are now in Variables Module.
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conU235    ! Boundary-specific concentrations of U-235 (atoms per cm^3)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conU238    ! Boundary-specific concentrations of U-238 (atoms per cm^3)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conPu239   ! Boundary-specific concentrations of Pu-239 (atoms per cm^3)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conPu240   ! Boundary-specific concentrations of Pu-240 (atoms per cm^3)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conPu241   ! Boundary-specific concentrations of Pu-241 (atoms per cm^3)
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, TARGET :: conPu242   ! Boundary-specific concentrations of Pu-242 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coU235av       ! Volume average concentration of U-235 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coU238av       ! Volume average concentration of U-238 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coPu239av      ! Volume average concentration of Pu-239 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coPu240av      ! Volume average concentration of Pu-240 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coPu241av      ! Volume average concentration of Pu-241 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: coPu242av      ! Volume average concentration of Pu-242 (atoms per cm^3)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriU235       ! Enrichment of U-235 (atom% in HM)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriU238       ! Enrichment of U-238 (atom% in HM)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriPu239      ! Enrichment of Pu-239 (atom% in HM)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriPu240      ! Enrichment of Pu-240 (atom% in HM)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriPu241      ! Enrichment of Pu-241 (atom% in HM)
    REAL(r8k), DIMENSION(:),   ALLOCATABLE, TARGET :: enriPu242      ! Enrichment of Pu-242 (atom% in HM)
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfU235       ! Power fraction coming from U-235
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfU238       ! Power fraction coming from U-238
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfPu239      ! Power fraction coming from Pu-239
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfPu240      ! Power fraction coming from Pu-240
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfPu241      ! Power fraction coming from Pu-241
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, TARGET :: pfPu242      ! Power fraction coming from Pu-242
    !
    CONTAINS
    !
    SUBROUTINE Allocate_Comde
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutines allocates the comde variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/21/2014
    !
    ALLOCATE (brnup1(1:na-1,1:2))
    ALLOCATE (conU235(1:na-1,1:nr,1:2))
    ALLOCATE (conU238(1:na-1,1:nr,1:2))
    ALLOCATE (conPu239(1:na-1,1:nr,1:2))
    ALLOCATE (conPu240(1:na-1,1:nr,1:2))
    ALLOCATE (conPu241(1:na-1,1:nr,1:2))
    ALLOCATE (conPu242(1:na-1,1:nr,1:2))
    ALLOCATE (coU235av(1:na-1))
    ALLOCATE (coU238av(1:na-1))
    ALLOCATE (coPu239av(1:na-1))
    ALLOCATE (coPu240av(1:na-1))
    ALLOCATE (coPu241av(1:na-1))
    ALLOCATE (coPu242av(1:na-1))
    ALLOCATE (enriU235(1:na-1))
    ALLOCATE (enriU238(1:na-1))
    ALLOCATE (enriPu239(1:na-1))
    ALLOCATE (enriPu240(1:na-1))
    ALLOCATE (enriPu241(1:na-1))
    ALLOCATE (enriPu242(1:na-1))
    ALLOCATE (pfU235(1:na-1,0:im+1))
    ALLOCATE (pfU238(1:na-1,0:im+1))
    ALLOCATE (pfPu239(1:na-1,0:im+1))
    ALLOCATE (pfPu240(1:na-1,0:im+1))
    ALLOCATE (pfPu241(1:na-1,0:im+1))
    ALLOCATE (pfPu242(1:na-1,0:im+1))
    ! Set default values to 0.0
    brnup1 = 0.0_r8k
    conU235 = 0.0_r8k
    conU238 = 0.0_r8k
    conPu239 = 0.0_r8k
    conPu240 = 0.0_r8k
    conPu241 = 0.0_r8k
    conPu242 = 0.0_r8k
    coU235av = 0.0_r8k
    coU238av = 0.0_r8k
    coPu239av = 0.0_r8k
    coPu240av = 0.0_r8k
    coPu241av = 0.0_r8k
    coPu242av = 0.0_r8k
    enriU235 = 0.0_r8k
    enriU238 = 0.0_r8k
    enriPu239 = 0.0_r8k
    enriPu240 = 0.0_r8k
    enriPu241 = 0.0_r8k
    enriPu242 = 0.0_r8k
    pfU235 = 0.0_r8k
    pfU238 = 0.0_r8k
    pfPu239 = 0.0_r8k
    pfPu240 = 0.0_r8k
    pfPu241 = 0.0_r8k
    pfPu242 = 0.0_r8k
    !
    END SUBROUTINE Allocate_Comde
    !
END MODULE comde