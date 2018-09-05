MODULE bcdcom_h
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module replaces the comdeck bcdcom.h
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
    !
    END MODULE bcdcom_h

