MODULE Kinds
    USE ISO_FORTRAN_ENV, ONLY : INT32, INT64, REAL32, REAL64, REAL128
    IMPLICIT NONE
    !>@brief
    !> This module contains the kinds used for specifying the precision of variables
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/6/2014
    ! Set the default precision for real and integer variables
    ! Double precision for reals
    INTEGER(int64), PARAMETER :: r8k = REAL64
    ! Single precision for integers
    INTEGER(int64), PARAMETER :: ipk = INT32
    !
END MODULE Kinds

