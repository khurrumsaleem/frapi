MODULE scalr_h_fraptran
    USE Kinds_fraptran
    !>@brief
    !> This module replaces comdeck scalr1 _fraptran& comdeck scalr2
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
        ! User specified whether to use refinement in case of ballooning with FEA model_fraptran
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
    INTEGER(ipk) , target :: Ifaila
    INTEGER(ipk) , target :: iagap
    INTEGER(ipk) , target :: iagap0
    INTEGER(ipk) , target :: m5fc2
    INTEGER(ipk) , target :: m6fc2
    INTEGER(ipk) , target :: IndexFC2Print
    INTEGER(ipk) , target :: mechan
    INTEGER(ipk) , target :: irupt
    INTEGER(ipk) , target :: irefine
    INTEGER(ipk) , target :: Ifailue
    INTEGER(ipk) , target :: nradq
    INTEGER(ipk) , target :: nbuq
    REAL(r8k) , target :: atop
    REAL(r8k) , target :: dcldh
    REAL(r8k) , target :: delth
    REAL(r8k) , target :: gpthk0
    REAL(r8k) , target :: cnfsol
    REAL(r8k) , target :: cnfliq
    REAL(r8k) , target :: tmax
    REAL(r8k) , target :: trecrd
    REAL(r8k) , target :: t0
    REAL(r8k) , target :: dtenfb
    REAL(r8k) , target :: dtenfo
    REAL(r8k) , target :: frcoef
    REAL(r8k) , target :: ruptstrain
    REAL(r8k) , target :: refine
    REAL(r8k) , target :: cfluxa
    REAL(r8k) , target :: coldw
    REAL(r8k) , target :: dishd
    REAL(r8k) , target :: dishv0
    REAL(r8k) , target :: bup
    REAL(r8k) , target :: beta1
    REAL(r8k) , target :: fqcrit
    REAL(r8k) , target :: frden
    REAL(r8k) , target :: frpo2
    REAL(r8k) , target :: drod
    REAL(r8k) , target :: dtpo
    REAL(r8k) , target :: dtss
    REAL(r8k) , target :: fpdcay
    REAL(r8k) , target :: TotalGasMoles
    REAL(r8k) , target :: pelh
    REAL(r8k) , target :: pfail
    REAL(r8k) , target :: powop
    REAL(r8k) , target :: PlenumGasMoles
    REAL(r8k) , target :: prsacc
    REAL(r8k) , target :: rhof
    REAL(r8k) , target :: rl
    REAL(r8k) , target :: rshd
    REAL(r8k) , target :: rhoc
    REAL(r8k) , target :: rvoid
    REAL(r8k) , target :: srcv2
    REAL(r8k) , target :: src1
    REAL(r8k) , target :: src2
    REAL(r8k) , target :: tdbugd
    REAL(r8k) , target :: tempcs
    REAL(r8k) , target :: tflux
    REAL(r8k) , target :: tgas0
    REAL(r8k) , target :: timop
    REAL(r8k) , target :: tmaxc
    REAL(r8k) , target :: tmaxf
    REAL(r8k) , target :: tmpac1
    REAL(r8k) , target :: tpowf
    REAL(r8k) , target :: trest
    REAL(r8k) , target :: t0c
    REAL(r8k) , target :: t0f
    REAL(r8k) , target :: zvoid1
    REAL(r8k) , target :: zvoid2
    REAL(r8k) , target :: OpenPorosityFraction
    REAL(r8k) , target :: dcldh0
    REAL(r8k) , target :: delth0
    REAL(r8k) , target :: dtheta
    REAL(r8k) , target :: dtold
    REAL(r8k) , target :: fr
    REAL(r8k) , target :: frchdt
    REAL(r8k) , target :: frchmx
    REAL(r8k) , target :: fuelrd
    REAL(r8k) , target :: time0
    REAL(r8k) , target :: tpo
    REAL(r8k) , target :: zbot
    REAL(r8k) , target :: zmesh
    REAL(r8k) , target :: zro
    REAL(r8k) , target :: dppowi
    REAL(r8k) , target :: powimx
    REAL(r8k) , target :: powict
    REAL(r8k) , target :: aidtot
    REAL(r8k) , target :: aidlng
    REAL(r8k) , target :: aidsht
    REAL(r8k) , target :: aidsur
    CHARACTER(LEN=12) , target :: relocmodel
    ! Arrays
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: idumr1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vplen
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: apln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: bpln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: vsn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: bu
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gappr0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: scd
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: spl
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: swd
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qpln
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: qpln0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: flwblk
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tp
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: powave
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dvdt
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: apln
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: bpln
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dvdt0
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hfusn
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tmelt
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ascal3
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ascal2
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: ascal1
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: gadolin
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: butemp
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: pchn
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: burad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: radpow
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: radsrc
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: radsrco
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: fuelrad
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: radtemp
    !
    END MODULE scalr_h_fraptran













