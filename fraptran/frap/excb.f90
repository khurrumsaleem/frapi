MODULE excb_h_fraptran
    USE Kinds_fraptran
    !>@brief
    !> This module replaces the comdeck excb_fraptran
    IMPLICIT NONE
    TYPE excb_var
        INTEGER(ipk) :: ndim
        INTEGER(ipk) :: nzmesh
        INTEGER(ipk) :: n2        ! = 1
        INTEGER(ipk) :: n4
        INTEGER(ipk) :: nsymm
        INTEGER(ipk) :: nconsw
        INTEGER(ipk) :: naz
        INTEGER(ipk) :: nnaz
        INTEGER(ipk) :: npdtpo
        INTEGER(ipk) :: nbalsw
        INTEGER(ipk) :: nrefld
        INTEGER(ipk) :: nlac
        INTEGER(ipk) :: nprsw
        INTEGER(ipk) :: modkf
        INTEGER(ipk) :: nbotpl
        INTEGER(ipk) :: ncolbp
        REAL(r8k) :: dzmesh
        REAL(r8k) :: timmd
        REAL(r8k) :: dofset
        REAL(r8k) :: dofang
        REAL(r8k) :: pitch
        REAL(r8k) :: fgrns
        REAL(r8k) :: fotmtl
        REAL(r8k) :: rsntr
        REAL(r8k) :: tsntrk
        REAL(r8k) :: volbp
        REAL(r8k) :: splbp
        REAL(r8k) :: coldbp
        REAL(r8k) :: spdbp
        REAL(r8k) :: BottomPlenumGasMoles
        REAL(r8k) :: cldwdc
        ! Arrays
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: aexcb     !1. This is not used so unclear if real or integer
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtpoa     !40
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tplbot  !6,2
    END TYPE excb_var
    !
    INTEGER(ipk) , target :: ndim
    INTEGER(ipk) , target :: nzmesh
    INTEGER(ipk) , target :: n2        ! = 1
    INTEGER(ipk) , target :: n4
    INTEGER(ipk) , target :: nsymm
    INTEGER(ipk) , target :: nconsw
    INTEGER(ipk) , target :: naz
    INTEGER(ipk) , target :: nnaz
    INTEGER(ipk) , target :: npdtpo
    INTEGER(ipk) , target :: nbalsw
    INTEGER(ipk) , target :: nrefld
    INTEGER(ipk) , target :: nlac
    INTEGER(ipk) , target :: nprsw
    INTEGER(ipk) , target :: modkf
    INTEGER(ipk) , target :: nbotpl
    INTEGER(ipk) , target :: ncolbp
    REAL(r8k) , target :: dzmesh
    REAL(r8k) , target :: timmd
    REAL(r8k) , target :: dofset
    REAL(r8k) , target :: dofang
    REAL(r8k) , target :: pitch
    REAL(r8k) , target :: fgrns
    REAL(r8k) , target :: fotmtl
    REAL(r8k) , target :: rsntr
    REAL(r8k) , target :: tsntrk
    REAL(r8k) , target :: volbp
    REAL(r8k) , target :: splbp
    REAL(r8k) , target :: coldbp
    REAL(r8k) , target :: spdbp
    REAL(r8k) , target :: BottomPlenumGasMoles
    REAL(r8k) , target :: cldwdc
    ! Arrays
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: aexcb      !1. Restart Array
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: dtpoa      !40
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE , target :: tplbot   !6,2
    !

END MODULE excb_h_fraptran













