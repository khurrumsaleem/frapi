MODULE excb_h
    USE Kinds
    !>@brief
    !> This module replaces the comdeck excb
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
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: aexcb      !1. Restart Array
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtpoa      !40
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: tplbot   !6,2
    !

END MODULE excb_h

