MODULE htcb_h
    USE Kinds
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
    !
    END MODULE htcb_h

