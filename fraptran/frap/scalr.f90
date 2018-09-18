MODULE scalr_h
    USE Kinds
    !>@brief
    !> This module replaces comdeck scalr1 & comdeck scalr2
    IMPLICIT NONE
    TYPE scalr_var
        ! Not used
        INTEGER(ipk) :: Ifaila
        ! Not used
        INTEGER(ipk) :: iagap
        ! Sent to restfs but not used
        INTEGER(ipk) :: iagap0
        ! Not used
        INTEGER(ipk) :: m5fc2
        ! Not used
        INTEGER(ipk) :: m6fc2
        ! User specified whether or not to print detailed FRACAS 2 stress/strain calculations
        INTEGER(ipk) :: IndexFC2Print
        ! User specified mechanical model
        INTEGER(ipk) :: mechan
        ! User specified rupture model with FEA model
        INTEGER(ipk) :: irupt
        ! User specified whether to use refinement in case of ballooning with FEA model
        INTEGER(ipk) :: irefine
        !
        INTEGER(ipk) :: Ifailue
        !
        INTEGER(ipk) :: nradq
        !
        INTEGER(ipk) :: nbuq
        !
        REAL(r8k) :: atop
        !
        REAL(r8k) :: dcldh
        !
        REAL(r8k) :: delth
        !
        REAL(r8k) :: gpthk0
        !
        REAL(r8k) :: cnfsol
        !
        REAL(r8k) :: cnfliq
        !
        REAL(r8k) :: tmax
        !
        REAL(r8k) :: trecrd
        !
        REAL(r8k) :: t0
        !
        REAL(r8k) :: dtenfb
        !
        REAL(r8k) :: dtenfo
        ! User specified Coulomb friction coefficient b/t cladding & fuel
        REAL(r8k) :: frcoef
        ! User specified max effective plastic strain value with FEA model
        REAL(r8k) :: ruptstrain
        ! User specified parameter for mesh refinement in FEA model. 2D element axial length/radial length
        REAL(r8k) :: refine
        !
        REAL(r8k) :: cfluxa
        !
        REAL(r8k) :: coldw
        !
        REAL(r8k) :: dishd
        !
        REAL(r8k) :: dishv0
        !
        REAL(r8k) :: bup
        !
        REAL(r8k) :: beta1
        !
        REAL(r8k) :: fqcrit
        !
        REAL(r8k) :: frden
        !
        REAL(r8k) :: frpo2
        !
        REAL(r8k) :: drod
        !
        REAL(r8k) :: dtpo
        !
        REAL(r8k) :: dtss
        !
        REAL(r8k) :: fpdcay
        !
        REAL(r8k) :: TotalGasMoles
        !
        REAL(r8k) :: pelh
        !
        REAL(r8k) :: pfail
        !
        REAL(r8k) :: powop
        !
        REAL(r8k) :: PlenumGasMoles
        !
        REAL(r8k) :: prsacc
        !
        REAL(r8k) :: rhof
        !
        REAL(r8k) :: rl
        !
        REAL(r8k) :: rshd
        !
        REAL(r8k) :: rhoc
        !
        REAL(r8k) :: rvoid
        !
        REAL(r8k) :: srcv2
        !
        REAL(r8k) :: src1
        !
        REAL(r8k) :: src2
        !
        REAL(r8k) :: tdbugd
        !
        REAL(r8k) :: tempcs
        !
        REAL(r8k) :: tflux
        !
        REAL(r8k) :: tgas0
        !
        REAL(r8k) :: timop
        ! Maximum cladding temperature. 5140 F
        REAL(r8k) :: tmaxc
        ! Maximum fuel temperature. 3320 F
        REAL(r8k) :: tmaxf
        !
        REAL(r8k) :: tmpac1
        !
        REAL(r8k) :: tpowf
        !
        REAL(r8k) :: trest
        ! Minimum temperature in cladding thermal properties table
        REAL(r8k) :: t0c
        ! Minimum temperature in fuel thermal properties table
        REAL(r8k) :: t0f
        !
        REAL(r8k) :: zvoid1
        !
        REAL(r8k) :: zvoid2
        !
        REAL(r8k) :: OpenPorosityFraction
        !
        REAL(r8k) :: dcldh0
        !
        REAL(r8k) :: delth0
        !
        REAL(r8k) :: dtheta
        !
        REAL(r8k) :: dtold
        !
        REAL(r8k) :: fr
        !
        REAL(r8k) :: frchdt
        !
        REAL(r8k) :: frchmx
        !
        REAL(r8k) :: fuelrd
        !
        REAL(r8k) :: time0
        ! Output write time interval
        REAL(r8k) :: tpo
        !
        REAL(r8k) :: zbot
        !
        REAL(r8k) :: zmesh
        !
        REAL(r8k) :: zro
        ! Power step at first time step (kW/m)
        REAL(r8k) :: dppowi
        !
        REAL(r8k) :: powimx
        !
        REAL(r8k) :: powict
        !
        REAL(r8k) :: aidtot
        !
        REAL(r8k) :: aidlng
        !
        REAL(r8k) :: aidsht
        !
        REAL(r8k) :: aidsur
        !
        CHARACTER(LEN=12) :: relocmodel
        ! Arrays
        ! Not used
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: idumr1
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vplen
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: apln0
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: bpln0
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: vsn
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: bu
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gappr0
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: scd
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: spl
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: swd
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: qpln
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: qpln0
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: flwblk
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tp
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: powave
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dvdt
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: apln
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: bpln
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dvdt0
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hfusn
        ! Melting temperature. tmelt(1) = ftmelt, tmelt(2) = ctmelt, tmelt(3) = 1.0d20 to exlude gases from melting
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tmelt
        ! Stores restart information. lsclr3 = 25
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal3
        ! Stores restart information. lsclr2 = 57
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal2
        ! Stores restart information. lsclr1 = 29
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal1
        !
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: gadolin
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: pchn
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: burad
        ! Not used
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radpow
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrc
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrco
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: fuelrad
        !
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radtemp
    END TYPE scalr_var
    !
    INTEGER(ipk) :: Ifaila
    INTEGER(ipk) :: iagap
    INTEGER(ipk) :: iagap0
    INTEGER(ipk) :: m5fc2
    INTEGER(ipk) :: m6fc2
    INTEGER(ipk) :: IndexFC2Print
    INTEGER(ipk) :: mechan
    INTEGER(ipk) :: irupt
    INTEGER(ipk) :: irefine
    INTEGER(ipk) :: Ifailue
    INTEGER(ipk) :: nradq
    INTEGER(ipk) :: nbuq
    REAL(r8k) :: atop
    REAL(r8k) :: dcldh
    REAL(r8k) :: delth
    REAL(r8k) :: gpthk0
    REAL(r8k) :: cnfsol
    REAL(r8k) :: cnfliq
    REAL(r8k) :: tmax
    REAL(r8k) :: trecrd
    REAL(r8k) :: t0
    REAL(r8k) :: dtenfb
    REAL(r8k) :: dtenfo
    REAL(r8k) :: frcoef
    REAL(r8k) :: ruptstrain
    REAL(r8k) :: refine
    REAL(r8k) :: cfluxa
    REAL(r8k) :: coldw
    REAL(r8k) :: dishd
    REAL(r8k) :: dishv0
    REAL(r8k) :: bup
    REAL(r8k) :: beta1
    REAL(r8k) :: fqcrit
    REAL(r8k) :: frden
    REAL(r8k) :: frpo2
    REAL(r8k) :: drod
    REAL(r8k) :: dtpo
    REAL(r8k) :: dtss
    REAL(r8k) :: fpdcay
    REAL(r8k) :: TotalGasMoles
    REAL(r8k) :: pelh
    REAL(r8k) :: pfail
    REAL(r8k) :: powop
    REAL(r8k) :: PlenumGasMoles
    REAL(r8k) :: prsacc
    REAL(r8k) :: rhof
    REAL(r8k) :: rl
    REAL(r8k) :: rshd
    REAL(r8k) :: rhoc
    REAL(r8k) :: rvoid
    REAL(r8k) :: srcv2
    REAL(r8k) :: src1
    REAL(r8k) :: src2
    REAL(r8k) :: tdbugd
    REAL(r8k) :: tempcs
    REAL(r8k) :: tflux
    REAL(r8k) :: tgas0
    REAL(r8k) :: timop
    REAL(r8k) :: tmaxc
    REAL(r8k) :: tmaxf
    REAL(r8k) :: tmpac1
    REAL(r8k) :: tpowf
    REAL(r8k) :: trest
    REAL(r8k) :: t0c
    REAL(r8k) :: t0f
    REAL(r8k) :: zvoid1
    REAL(r8k) :: zvoid2
    REAL(r8k) :: OpenPorosityFraction
    REAL(r8k) :: dcldh0
    REAL(r8k) :: delth0
    REAL(r8k) :: dtheta
    REAL(r8k) :: dtold
    REAL(r8k) :: fr
    REAL(r8k) :: frchdt
    REAL(r8k) :: frchmx
    REAL(r8k) :: fuelrd
    REAL(r8k) :: time0
    REAL(r8k) :: tpo
    REAL(r8k) :: zbot
    REAL(r8k) :: zmesh
    REAL(r8k) :: zro
    REAL(r8k) :: dppowi
    REAL(r8k) :: powimx
    REAL(r8k) :: powict
    REAL(r8k) :: aidtot
    REAL(r8k) :: aidlng
    REAL(r8k) :: aidsht
    REAL(r8k) :: aidsur
    CHARACTER(LEN=12) :: relocmodel
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: idumr1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vplen
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: apln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: bpln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: vsn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: bu
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gappr0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: scd
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: spl
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: swd
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qpln
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: qpln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: flwblk
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tp
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: powave
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dvdt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: apln
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: bpln
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dvdt0
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: hfusn
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: tmelt
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal3
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal2
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: ascal1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: gadolin
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: pchn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: burad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radpow
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrc
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radsrco
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: fuelrad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: radtemp
    !
    END MODULE scalr_h












