MODULE bcdcom_h_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comdeck bcdcom_fraptran.h
    TYPE bcdcom_var
        INTEGER(ipk) :: jchf
        INTEGER(ipk) :: mbowr
        INTEGER(ipk) :: mgbh
        INTEGER(ipk) :: mhbh
        INTEGER(ipk) :: mhinta
        INTEGER(ipk) :: mhlqc1
        INTEGER(ipk) :: mhlqcp
        INTEGER(ipk) :: mhupta
        INTEGER(ipk) :: mndchf
        INTEGER(ipk) :: mpbh
        INTEGER(ipk) :: mprest
        INTEGER(ipk) :: mtempt
        INTEGER(ipk) :: nbowr
        INTEGER(ipk) :: ncooli
        INTEGER(ipk) :: nhtc
        INTEGER(ipk) :: nbhtc
        INTEGER(ipk) :: jtr
        REAL(r8k) :: achn
        REAL(r8k) :: dhe
        REAL(r8k) :: dhy
        REAL(r8k) :: jhtc
        CHARACTER(LEN=3) :: Radiation
        INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: ndchfi
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: bowrat
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: techfi
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: tschfi
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: hlqcl1
    END TYPE bcdcom_var
    !
    INTEGER(ipk) , target :: jchf
    INTEGER(ipk) , target :: mbowr
    INTEGER(ipk) , target :: mgbh
    INTEGER(ipk) , target :: mhbh
    INTEGER(ipk) , target :: mhinta
    INTEGER(ipk) , target :: mhlqc1
    INTEGER(ipk) , target :: mhlqcp
    INTEGER(ipk) , target :: mhupta
    INTEGER(ipk) , target :: mndchf
    INTEGER(ipk) , target :: mpbh
    INTEGER(ipk) , target :: mprest
    INTEGER(ipk) , target :: mtempt
    INTEGER(ipk) , target :: nbowr
    INTEGER(ipk) , target :: ncooli
    INTEGER(ipk) , target :: nhtc
    INTEGER(ipk) , target :: nbhtc
    INTEGER(ipk) , target :: jtr
    REAL(r8k) , target :: achn
    REAL(r8k) , target :: dhe
    REAL(r8k) , target :: dhy
    REAL(r8k) , target :: jhtc
    CHARACTER(LEN=3) , target :: Radiation
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE , target :: ndchfi
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: bowrat
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: techfi
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: tschfi
    REAL(r8k), DIMENSION(:), ALLOCATABLE , target :: hlqcl1
    !
    END MODULE bcdcom_h_fraptran













