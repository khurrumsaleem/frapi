MODULE Functions
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This module contains the main functions used by FRAPCON
    !> Functions include polate, terp, bes2, and ShiftArray
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 05/26/2015
    !
    CONTAINS
    !
    REAL(r8k) FUNCTION polate (xy, xx, nn, kk)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> Polate is a linear interpolation function
    !
    ! Input
    !
    ! xy - a table of y(1), x(1), y(2), x(2), ... y(nn), x(nn)
    ! xx - the given value for x
    ! nn - the number of pairs of entries in xy
    ! kk - both the position guess and the final value (OPTIONAL)
    !
    INTEGER(ipk) :: nerr=1, n, m, k, i
    INTEGER(ipk), INTENT(IN) :: nn
    INTEGER(ipk), INTENT(INOUT), OPTIONAL :: kk
    INTEGER(ipk), PARAMETER :: nprmax = 20
    REAL(r8k) :: x
    REAL(r8k), INTENT(IN) :: xx
    REAL(r8k), PARAMETER :: zero = 0.0_r8k
    REAL(r8k), PARAMETER :: dm10 = 1.0e-10_r8k
    REAL(r8k), DIMENSION(2*nn), INTENT(IN) :: xy
    !
    x = xx
    n = nn
    m = ABS(n)
    IF (PRESENT(kk)) THEN
        k = kk
    ELSE
        k = nn / 2
    END IF
    IF (k < 1) k = 1
    IF (k >= m) k = m - 1
    ! is constant wanted
    IF ((m - 1) < 0) GOTO 100
    IF ((m - 1) == 0) GOTO 110
    IF ((m - 1) > 0) GOTO 120
100 polate = zero
    RETURN
110 polate = xy(1)
    RETURN
    ! loop to decrease index
120 IF ((xy(2*k) - x) <= 0.0_r8k) GOTO 140
    k = k - 1
    IF (k <= 0) GOTO 160
    IF (k > 0) GOTO 120
    ! loop to increase index
140 IF ((x - xy(2*k + 2) - dm10) <= 0.0_r8k) GOTO 200
    k = k + 1
    IF ((k - m) < 0) GOTO 140
    IF ((k - m) >= 0) GOTO 180
    ! test for extrapolation
160 IF (n < 0) GOTO 170
    IF (n == 0) GOTO 100
    IF (n > 0) GOTO 210
170 k = 1
    GOTO 200
180 IF (n < 0) GOTO 190
    IF (n == 0) GOTO 100
    IF (n > 0) GOTO 220
190 k = m - 1
    ! everything ok, get answer
200 IF (PRESENT(kk)) kk = k
    polate = xy(2*k - 1) + (x - xy(2*k)) * (xy(2*k + 1) - xy(2*k - 1)) / (xy(2*k + 2) - xy(2*k))
    RETURN
    ! polate failure, search out of bounds
210 polate = xy(1)
    GOTO 230
220 polate = xy(2*m - 1)
230 IF (nerr > nprmax) GOTO 240
    IF (PRESENT(kk)) THEN
        WRITE (ounit,250) kk, k, n, x, (xy(2*i),i=1,n)
    ELSE
        WRITE (ounit,251) k, n, x, (xy(2*i),i=1,n)
    END IF
    ! CALL errtra
    IF (nerr == nprmax) WRITE (ounit,260)
    nerr = nerr + 1
240 RETURN
250 FORMAT (' error in polate'/'0initial index =',i6,10x,'final index =',i6,10x, &
      &     'array length =',i6,10x,'argument =',e14.6/'0table of x values ='/(8e15.6))
251 FORMAT (' error in polate'/'0initial index not provided',10x,'final index =',i6,10x, &
      &     'array length =',i6,10x,'argument =',e14.6/'0table of x values ='/(8e15.6))
260 FORMAT ('0***** further messages suppressed')
    !
    END FUNCTION polate
    !
    !
    !
    REAL(r8k) FUNCTION terp (tt, ti, td, n)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> Interpolates to find a value terp from the td (dependent) array to correspond to the
    !> value tt from the ti (independent) array.  n is the number of values present in the ti or td arrays.
    !> The routine is capable of handling an independent array arranged in either increasing or decreasing order.
    !> To speed solution on large arrays, the arrays are subdivided into two or four sub-arrays to enhance the sorting.
    INTEGER(ipk), INTENT(IN) :: n
    INTEGER(ipk) :: no1, no2, no3, j, j1, j2
    REAL(r8k), INTENT(IN) :: tt
    REAL(r8k), DIMENSION(n), INTENT(IN) :: ti, td
    !
    no2 = n / 2
    IF (ti(1) > ti(n)) GOTO 210
    IF (tt <= ti(1)) GOTO 170
    IF (tt >= ti(n)) GOTO 180
    IF (tt >= ti(no2)) GOTO 120
    IF (n <= 12) GOTO 110
    no1 = no2 / 2
    IF (tt >= ti(no1)) GOTO 100
    j1 = 1
    j2 = no1
    GOTO 150
100 CONTINUE
    j1 = no1
    j2 = no2
    GOTO 150
110 CONTINUE
    j1 = 1
    j2 = no2
    GOTO 150
120 CONTINUE
    IF (n <= 12) GOTO 140
    no3 = (no2 + n) / 2
    IF (tt >= ti(no3)) GOTO 130
    j1 = no2
    j2 = no3
    GOTO 150
130 CONTINUE
    j1 = no3
    j2 = n
    GOTO 150
140 CONTINUE
    j1 = no2
    j2 = n
150 CONTINUE
    DO j = j1, j2
        IF (tt - ti(j) < 0.0_r8k) GOTO 200
        IF (tt - ti(j) == 0.0_r8k) GOTO 190
    END DO
    GOTO 180
170 terp = td(1)
    RETURN
180 terp = td(n)
    RETURN
190 terp = td(j)
    RETURN
200 CONTINUE
    terp = td(j-1) + (td(j) - td(j-1)) * (tt - ti(j-1)) / (ti(j) - ti(j-1))
    RETURN
210 IF (tt >= ti(1)) GOTO 170
    IF (tt <= ti(n)) GOTO 180
    IF (tt <= ti(no2)) GOTO 240
    IF (n <= 12) GOTO 230
    no1 = no2 / 2
    IF (tt <= ti(no1)) GOTO 220
    j1 = 1
    j2 = no1
    GOTO 270
220 CONTINUE
    j1 = no1
    j2 = no2
    GOTO 270
230 CONTINUE
    j1 = 1
    j2 = no2
    GOTO 270
240 CONTINUE
    IF (n <= 12) GOTO 260
    no3 = (no2 + n) / 2
    IF (tt <= ti(no3)) GOTO 250
    j1 = no2
    j2 = no3
    GOTO 270
250 CONTINUE
    j1 = no3
    j2 = n
    GOTO 270
260 CONTINUE
    j1 = no2
    j2 = n
270 CONTINUE
    DO j = j1, j2
        IF ((tt - ti(j)) == 0.0_r8k) GOTO 190
        IF ((tt - ti(j)) > 0.0_r8k) GOTO 200
    END DO
    GOTO 180
    END FUNCTION terp
    !
    !
    !
    REAL(r8k) FUNCTION bes2 (arg, n, m, ier)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> To calculate i and k bessel functions of order 0 and 1
    !> @author
    !> origen: i d palmer
    !> modified and tested by r kunze
    !
    ! Input
    !
    ! arg  - argument
    ! x    - argument (equal to arg)
    ! n    - order of the bessel Function (0 or 1)
    ! m    - Determines i or k function.
    !    0 = i funktion
    !    1 = k funktion
    ! ier  - error indicator : =1  negative arg
    !
    INTEGER(ipk), INTENT(IN) :: n, m
    INTEGER(ipk), INTENT(OUT) :: ier
    REAL(r8k), INTENT(INOUT) :: arg
    REAL(r8k) :: x
    !
    ier = 0
    IF (arg <= 0.0_r8k) THEN
        IF (arg < 0.0_r8k) THEN
            ! --- argument is negative
            ier = 1
            arg = 0.0_r8k
        END IF
        IF (m == 0) THEN
            IF (n == 0) THEN
                bes2 = 1.0_r8k
                RETURN
            ELSE IF (n == 1) THEN
                bes2 = 0.0_r8k
                RETURN
            END IF
        ELSE IF (m == 1) THEN
            IF (n == 0) THEN
                bes2 = 1.0e10_r8k
                RETURN
            ELSE IF (n == 1) THEN
                bes2 = 1.0e10_r8k
                RETURN
            END IF
        END IF
    ELSE
        x = arg
        IF (m == 0) THEN
            IF (n == 0) THEN
                ! Calculation of i0
                IF (x <= 3.75_r8k) THEN
                    bes2 = bi0l((x ** 2) / (3.75_r8k ** 2))
                    RETURN
                ELSE
                    bes2 = bi0g(3.75_r8k / x) * EXP(x) / SQRT(x)
                    RETURN
                END IF
            ELSE IF (n == 1) THEN
                ! --- calculation of i1
                IF (x <= 3.75_r8k) THEN
                    bes2 = bi1l((x ** 2) / (3.75_r8k ** 2)) * x
                    RETURN
                END IF
            END IF
        ELSE IF (m == 1) THEN
            IF (n == 0) THEN
                ! --- calculation of k0
                IF (x <= 2.0_r8k) THEN
                    bes2 = bk0l((x ** 2) / 4.0_r8k) - LOG(x / 2.0_r8k) * bi0l((x ** 2) / (3.75_r8k ** 2))
                    RETURN
                ELSE
                    bes2 = bk0g(2.0_r8k / x) / EXP(x) / SQRT(x)
                    RETURN
                END IF
            ELSE IF (n == 1) THEN
                ! --- calculation of k1
                IF (x <= 2.0_r8k) THEN
                    bes2 = bk1l((x ** 2) / 4.0_r8k) / x + LOG(x / 2.0_r8k) * bi1l((x ** 2) / (3.75_r8k ** 2)) * x
                    RETURN
                ELSE
                    bes2 = bk1g(2.0_r8k / x) / EXP(x) / SQRT(x)
                    RETURN
                END IF
            END IF
        END IF
    END IF
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION bi0l (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bi0l = 1.0_r8k + x * (3.5156229_r8k + x * (3.0899424_r8k + x * (1.2067492_r8k + x * (0.2659732_r8k + &
          &    x * (0.0360768_r8k + x *  0.0045813_r8k)))))
        !
        END FUNCTION bi0l
        !
        !
        !
        REAL(r8k) FUNCTION bi0g (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bi0g = 0.39894228_r8k + x * (0.01328592_r8k + x * (0.0022532_r8k + x * (-0.00157565_r8k + x * (0.0091628_r8k + &
          &    x * (-0.02057706_r8k + x * (0.02635537_r8k + x * (-0.01647633_r8k + x * 0.00392377_r8k)))))))
        !
        END FUNCTION bi0g
        !
        !
        !
        REAL(r8k) FUNCTION bi1l (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bi1l = 0.5_r8k + x * (0.87890594_r8k + x * (0.51498869_r8k + x * (0.15084934_r8k + x * (0.02658733_r8k + &
          &    x * (0.00301532_r8k + x *  0.00032411_r8k)))))
        !
        END FUNCTION bi1l
        !
        !
        !
        REAL(r8k) FUNCTION bk0l (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bk0l = -0.57721566_r8k + x * (0.42278420_r8k + x * (0.23069756_r8k + x * (0.03488590_r8k + x * (0.00262698_r8k + &
          &    x * (0.00010750_r8k + x * 0.00000740_r8k)))))
        !
        END FUNCTION bk0l
        !
        !
        !
        REAL(r8k) FUNCTION bk0g (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bk0g = 1.25331414_r8k + x * (-0.07832358_r8k + x * (0.02189568_r8k + x * (-0.01062446_r8k + x * (0.00587872_r8k + &
          &       x * (-0.00251540_r8k + x * 0.00053208_r8k)))))
        !
        END FUNCTION bk0g
        !
        !
        !
        REAL(r8k) FUNCTION bk1l (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bk1l = 1.0_r8k + x * (0.15443144_r8k + x * (-0.67278579_r8k + x * (-0.18156897_r8k + x * (-0.01919402_r8k + &
          &    x * (-0.00110404_r8k - x * 0.00004686_r8k)))))
        !
        END FUNCTION bk1l
        !
        !
        !
        REAL(r8k) FUNCTION bk1g (x)
        USE Kinds
        USE Conversions
        !>@author
        !> Ian Porter, NRC
        REAL(r8k), INTENT(IN) :: x
        !
        bk1g = 1.25331414_r8k + x * (0.23498619_r8k + x * (-0.03655620_r8k + x * (0.01504268_r8k + x * (-0.00780353_r8k + &
          &    x * (0.00325614_r8k - x *  0.00068245_r8k)))))
        !
        END FUNCTION bk1g
    !
    END FUNCTION bes2
    !
    !
    !
    SUBROUTINE ShiftArray (size1, size2, ArrayVal, ShiftType)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine increases an array from (array1, size1) to (array2, size2) using either 
    !> (1) linear interpolation or (2) histogram.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    !
    INTEGER(ipk), INTENT(IN) :: size1, size2, ShiftType
    INTEGER(ipk) :: n, count, i, j
    REAL(r8k), DIMENSION(:), INTENT(INOUT) :: ArrayVal
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: DumArray
    !
    ALLOCATE (DumArray(1:size1))
    !
    n = size2 / size1
    count = 0
    !
    SELECT CASE (ShiftType)
    CASE(1) ! LinearInterpolation
        ! Store old values
        DumArray(1:size1) = ArrayVal(1:size1)
        !
        DO i = 1, size1
            DO j = 1, n
                IF (i < size1) THEN
                    ArrayVal((i - 1) * n + j) = DumArray(i) + (DumArray(i + 1) - DumArray(i)) / n * j
                ELSE
                    ArrayVal((i - 1) * n + j) = DumArray(size1)
                END IF
            END DO
        END DO
    CASE (2) ! Histogram
        ! Store old values
        DumArray(1:size1) = ArrayVal(1:size1)
        !
        DO i = 1, size1
            DO j = 1, n
                ArrayVal((i - 1) * n + j) = DumArray(i)
            END DO
        END DO
    CASE DEFAULT
        WRITE (0,110)
        WRITE (ounit,110)
110     FORMAT('ERROR in Subroutine ShiftArray. Wrong shift type called. Program Execution Stopped.')
        STOP
    END SELECT
    !
    DEALLOCATE (DumArray)
    !
    END SUBROUTINE ShiftArray
    !
    !
    !
    SUBROUTINE ReAllocateArray (size_old, size_new, startval, RealArray)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine re-allocates the array size but keeps the data in the same location
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    !
    INTEGER(ipk), INTENT(IN) :: size_old, size_new, startval
    INTEGER(ipk) :: i, MinSize, MaxSize
    REAL(r8k), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: RealArray
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: DumArray
    !
    MinSize = MIN(size_old, size_new)
    MaxSize = MAX(size_old, size_new)
    !
    ALLOCATE(DumArray(startval:MinSize))
    DumArray = 0.0_r8k
    !
    DO i = startval, MinSize
        DumArray(i) = RealArray(i)
    END DO
    !
    IF (ALLOCATED(RealArray)) DEALLOCATE (RealArray)
    ALLOCATE (RealArray(startval:size_new))
    RealArray = 0.0_r8k
    !
    DO i = startval, MinSize
        RealArray(i) = DumArray(i)
    END DO
    !
    IF (ALLOCATED(DumArray)) DEALLOCATE (DumArray)
    !
    END SUBROUTINE ReAllocateArray
    !
    !
    SUBROUTINE ReAllocateArray1DInteger (size_old, size_new, startval, IntArray)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine re-allocates the array size but keeps the data in the same location
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    !
    INTEGER(ipk), INTENT(IN) :: size_old, size_new, startval
    INTEGER(ipk) :: i, MinSize, MaxSize
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: IntArray
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: DumArray
    !
    MinSize = MIN(size_old, size_new)
    MaxSize = MAX(size_old, size_new)
    !
    ALLOCATE(DumArray(startval:MinSize))
    DumArray = 0
    !
    DO i = startval, MinSize
        DumArray(i) = IntArray(i)
    END DO
    !
    IF (ALLOCATED(IntArray)) DEALLOCATE (IntArray)
    ALLOCATE (IntArray(startval:size_new))
    IntArray = 0.0_r8k
    !
    DO i = startval, MinSize
        IntArray(i) = DumArray(i)
    END DO
    !
    IF (ALLOCATED(DumArray)) DEALLOCATE (DumArray)
    !
    END SUBROUTINE ReAllocateArray1DInteger
    !
    !
    !
    SUBROUTINE ReAllocateArray2D (size1_old, size1_new, startval1, size2_old, size2_new, startval2, RealArray)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine re-allocates the 2-D array size but keeps the data in the same location
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    !
    INTEGER(ipk), INTENT(IN) :: size1_old, size1_new, startval1, size2_old, size2_new, startval2
    INTEGER(ipk) :: i, l, MinSize1, MaxSize1, MinSize2, MaxSize2
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: RealArray
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: DumArray
    !
    MinSize1 = MIN(size1_old, size1_new)
    MaxSize1 = MAX(size1_old, size1_new)
    MinSize2 = MIN(size2_old, size2_new)
    MaxSize2 = MAX(size2_old, size2_new)
    !
    ALLOCATE(DumArray(startval1:MinSize1, startval2:MinSize2))
    DumArray = 0.0_r8k
    !
    DO i = startval1, MinSize1
        DO l = startval2, MinSize2
            DumArray(i,l) = RealArray(i,l)
        END DO
    END DO
    !
    IF (ALLOCATED(RealArray)) DEALLOCATE (RealArray)
    ALLOCATE (RealArray(startval1:size1_new, startval2:size2_new))
    RealArray = 0.0_r8k
    !
    DO i = startval1, MinSize1
        DO l = startval2, MinSize2
            RealArray(i,l) = DumArray(i,l)
        END DO
    END DO
    !
    IF (ALLOCATED(DumArray)) DEALLOCATE (DumArray)
    !
    END SUBROUTINE ReAllocateArray2D
    !
    !
    !
    SUBROUTINE ReAllocateArray3D(size1_old, size1_new, startval1, size2_old, size2_new, startval2, &
      &                          size3_old, size3_new, startval3, RealArray)
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine re-allocates the 3-D array size but keeps the data in the same location
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    !
    INTEGER(ipk), INTENT(IN) :: size1_old, size1_new, startval1, size2_old, size2_new, startval2, &
      &                         size3_old, size3_new, startval3
    INTEGER(ipk) :: i, l, k, MinSize1, MaxSize1, MinSize2, MaxSize2, MinSize3, MaxSize3
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: RealArray
    REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: DumArray
    !
    MinSize1 = MIN(size1_old, size1_new)
    MaxSize1 = MAX(size1_old, size1_new)
    MinSize2 = MIN(size2_old, size2_new)
    MaxSize2 = MAX(size2_old, size2_new)
    MinSize3 = MIN(size3_old, size3_new)
    MaxSize3 = MAX(size3_old, size3_new)
    !
    ALLOCATE(DumArray(startval1:MinSize1, startval2:MinSize2, startval3:MinSize3))
    DumArray = 0.0_r8k
    !
    DO i = startval1, MinSize1
        DO l = startval2, MinSize2
            DO k = startval3, MinSize3
                DumArray(i,l,k) = RealArray(i,l,k)
            END DO
        END DO
    END DO
    !
    IF (ALLOCATED(RealArray)) DEALLOCATE (RealArray)
    ALLOCATE (RealArray(startval1:size1_new, startval2:size2_new, startval3:size3_new))
    RealArray = 0.0_r8k
    !
    DO i = startval1, MinSize1
        DO l = startval2, MinSize2
            DO k = startval3, MinSize3
                RealArray(i,l,k) = DumArray(i,l,k)
            END DO
        END DO
    END DO
    !
    IF (ALLOCATED(DumArray)) DEALLOCATE (DumArray)
    !
    END SUBROUTINE ReAllocateArray3D
    !
    !
    !
    PURE FUNCTION TO_UPPERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes lowercase text in a string to uppercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(LOWL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = CAPL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_UPPERCASE
    !
    !
    !
    PURE FUNCTION TO_LOWERCASE (String) RESULT (NewString)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This function changes uppercase text in a string to lowercase text
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/30/2015
    INTEGER(ipk) :: i, j
    CHARACTER(LEN=*), INTENT(IN) :: String
    CHARACTER(LEN=LEN_TRIM(String)) :: NewString
    CHARACTER(LEN=26), PARAMETER :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(LEN=26), PARAMETER :: LOWL = 'abcdefghijklmnopqrstuvwxyz'
    !
    NewString = ''
    !
    DO i = 1, LEN_TRIM(String)
        j = INDEX(CAPL, String(i:i))
        IF (j > 0) THEN
            NewString(i:i) = LOWL(j:j)
        ELSE
            NewString(i:i) = string(i:i)
        ENDIF
    ENDDO
    !
    END FUNCTION TO_LOWERCASE
    !
    !
    !
END MODULE Functions