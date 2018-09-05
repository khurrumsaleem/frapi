MODULE modcom_h
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module replaces the modcom common block
    TYPE modcom_var
        INTEGER(ipk) :: mwork
        INTEGER(ipk) :: mtabl1
        INTEGER(ipk) :: mtabl2
        INTEGER(ipk) :: mtabl3
        INTEGER(ipk) :: mgaspr
        REAL(r8k) :: trise
    END TYPE modcom_var
    !
    INTEGER(ipk) :: mwork
    INTEGER(ipk) :: mtabl1
    INTEGER(ipk) :: mtabl2
    INTEGER(ipk) :: mtabl3
    INTEGER(ipk) :: mgaspr
    REAL(r8k) :: trise
    !
    END MODULE modcom_h

