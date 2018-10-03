MODULE htcb_h_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !>This modules replaces the common file htcb
    TYPE htcb_var
        INTEGER(ipk) :: kaxhtc
        INTEGER(ipk) :: nfhtc
        INTEGER(ipk) :: nradsh
        INTEGER(ipk) :: nqbow
        REAL(r8k) :: tshtc
        REAL(r8k) :: fhtc
        REAL(r8k) :: rshrd
        REAL(r8k) :: zroug1
        REAL(r8k) :: zroug2
        REAL(r8k) :: ffchf
        REAL(r8k) :: htflxa
        REAL(r8k) :: bowthr
    END TYPE htcb_var
    !
    INTEGER(ipk) , target :: kaxhtc
    INTEGER(ipk) , target :: nfhtc
    INTEGER(ipk) , target :: nradsh
    INTEGER(ipk) , target :: nqbow
    REAL(r8k) , target :: tshtc
    REAL(r8k) , target :: fhtc
    REAL(r8k) , target :: rshrd
    REAL(r8k) , target :: zroug1
    REAL(r8k) , target :: zroug2
    REAL(r8k) , target :: ffchf
    REAL(r8k) , target :: htflxa
    REAL(r8k) , target :: bowthr
    !
    END MODULE htcb_h_fraptran













