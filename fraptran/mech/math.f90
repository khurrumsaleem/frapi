MODULE math_fraptran
    USE Kinds_fraptran
    USE common_parameters_fraptran
    IMPLICIT NONE
    !>@brief
    !> General purpose mathematical functions and Subroutines

    CONTAINS
    
    REAL(r8k) PURE FUNCTION vnorm (n, vect)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Vector norm for vect(1:n)
    INTEGER(ipk), INTENT(IN) :: n
    INTEGER(ipk) :: i
    REAL(r8k), DIMENSION(n), INTENT(IN) :: vect

    vnorm = 0.0_r8k

    DO i = 1, n
        vnorm = vnorm + vect(i) ** 2
    ENDDO

    vnorm = SQRT(vnorm)

    END FUNCTION vnorm
    !
    !
    !
    SUBROUTINE inverse3 (A, Ainv, detA)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Calculate inverse and determinant of matrix A(3,3)
    REAL(r8k), INTENT(IN) :: A(3,3)
    REAL(r8k), INTENT(OUT) :: Ainv(3,3), detA
    REAL(r8k) :: t4, t6, t8, t10, t12, t14, t15, t17

    t4 = A(1,1)*A(2,2)
    t6 = A(1,1)*A(2,3)
    t8 = A(1,2)*A(2,1)
    t10 = A(1,3)*A(2,1)
    t12 = A(1,2)*A(3,1)
    t14 = A(1,3)*A(3,1)
    t15 = t4*A(3,3)-t6*A(3,2)-t8*A(3,3)+t10*A(3,2)+t12*A(2,3)-t14*A(2,2)
    t17 = 1.0_r8k/t15
    Ainv(1,1) = (A(2,2)*A(3,3)-A(2,3)*A(3,2))*t17
    Ainv(1,2) = -(A(1,2)*A(3,3)-A(1,3)*A(3,2))*t17
    Ainv(1,3) = -(-A(1,2)*A(2,3)+A(1,3)*A(2,2))*t17
    Ainv(2,1) = -(A(2,1)*A(3,3)-A(2,3)*A(3,1))*t17
    Ainv(2,2) = (A(1,1)*A(3,3)-t14)*t17
    Ainv(2,3) = -(t6-t10)*t17
    Ainv(3,1) = -(-A(2,1)*A(3,2)+A(2,2)*A(3,1))*t17
    Ainv(3,2) = -(A(1,1)*A(3,2)-t12)*t17
    Ainv(3,3) = (t4-t8)*t17

    detA = t15

    END SUBROUTINE inverse3
    !
    !
    !
    FUNCTION matmul_AAt(A) RESULT (C)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Matric multiplication C = A A^T for 3x3 matrices
    REAL(r8k) :: A(3,3), C(3,3)
    REAL(r8k) :: t1, t2, t3, t8, t12, t13, t14, t15, t20, t21, t22, t23

    t1 = A(1,1)**2
    t2 = A(1,2)**2
    t3 = A(1,3)**2
    t8 = A(1,1)*A(2,1)+A(1,2)*A(2,2)+A(1,3)*A(2,3)
    t12 = A(1,1)*A(3,1)+A(1,2)*A(3,2)+A(1,3)*A(3,3)
    t13 = A(2,1)**2
    t14 = A(2,2)**2
    t15 = A(2,3)**2
    t20 = A(2,1)*A(3,1)+A(2,2)*A(3,2)+A(2,3)*A(3,3)
    t21 = A(3,1)**2
    t22 = A(3,2)**2
    t23 = A(3,3)**2
    C(1,1) = t1+t2+t3
    C(1,2) = t8
    C(1,3) = t12
    C(2,1) = t8
    C(2,2) = t13+t14+t15
    C(2,3) = t20
    C(3,1) = t12
    C(3,2) = t20
    C(3,3) = t21+t22+t23

    END FUNCTION matmul_AAt
    !
    !
    !
    FUNCTION matmul_ABAt(A,B) RESULT(C)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Matrix multiplication C = A B A^T where A is general 3x3 matrix and
    !> B is symmetric 3x3 matrix stored in condences FORMAT
    REAL(r8k) :: A(3,3), B(6), C(3,3)
    REAL(r8k) :: t4, t9, t14, t28, t33, t38, t52, t57, t62

    t4 = A(1,1)*B(1)+A(1,2)*B(3)+A(1,3)*B(6)
    t9 = A(1,1)*B(3)+A(1,2)*B(2)+A(1,3)*B(5)
    t14 = A(1,1)*B(6)+A(1,2)*B(5)+A(1,3)*B(4)
    t28 = A(2,1)*B(1)+A(2,2)*B(3)+A(2,3)*B(6)
    t33 = A(2,1)*B(3)+A(2,2)*B(2)+A(2,3)*B(5)
    t38 = A(2,1)*B(6)+A(2,2)*B(5)+A(2,3)*B(4)
    t52 = A(3,1)*B(1)+A(3,2)*B(3)+A(3,3)*B(6)
    t57 = A(3,1)*B(3)+A(3,2)*B(2)+A(3,3)*B(5)
    t62 = A(3,1)*B(6)+A(3,2)*B(5)+A(3,3)*B(4)

    C(1,1) = t4*A(1,1)+t9*A(1,2)+t14*A(1,3)
    C(1,2) = t4*A(2,1)+t9*A(2,2)+t14*A(2,3)
    C(1,3) = t4*A(3,1)+t9*A(3,2)+t14*A(3,3)
    C(2,1) = C(1,2)
    C(2,2) = t28*A(2,1)+t33*A(2,2)+t38*A(2,3)
    C(2,3) = t28*A(3,1)+t33*A(3,2)+t38*A(3,3)
    C(3,1) = C(1,3)
    C(3,2) = C(2,3)
    C(3,3) = t52*A(3,1)+t57*A(3,2)+t62*A(3,3)

    END FUNCTION matmul_ABAt
    !
    !
    !
    FUNCTION matmul_AiBAit(A,B) RESULT(C)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Solve C = inv(A) B inv(A^T), where A is general 3x3 matrix B is symmetric
    !> 3x3 matrix and C is symmetic 3x3 matrix in condenced FORMAT
    REAL(r8k) :: A(3,3), B(3,3), C(6)
    REAL(r8k) :: t3, t4, t6, t8, t10, t12, t14, t17, t18, t22, t23, t27, t28, t30, t36, t42, &
      &          t48, t49, t52, t53, t55, t56, t58, t64, t70, t83, t84, t87, t88, t90, t91

    t3 = A(2,2)*A(3,3)-A(2,3)*A(3,2)
    t4 = A(1,1)*A(2,2)
    t6 = A(1,1)*A(2,3)
    t8 = A(1,2)*A(2,1)
    t10 = A(1,3)*A(2,1)
    t12 = A(1,2)*A(3,1)
    t14 = A(1,3)*A(3,1)
    t17 = 1/(t4*A(3,3)-t6*A(3,2)-t8*A(3,3)+t10*A(3,2)+t12*A(2,3)-t14*A(2,2))
    t18 = t3*t17
    t22 = A(1,2)*A(3,3)-A(1,3)*A(3,2)
    t23 = t22*t17
    t27 = -A(1,2)*A(2,3)+A(1,3)*A(2,2)
    t28 = t27*t17
    t30 = t18*B(1,1)-t23*B(1,2)-t28*B(1,3)
    t36 = t18*B(1,2)-t23*B(2,2)-t28*B(2,3)
    t42 = t18*B(1,3)-t23*B(2,3)-t28*B(3,3)
    t48 = A(2,1)*A(3,3)-A(2,3)*A(3,1)
    t49 = t48*t17
    t52 = A(1,1)*A(3,3)-t14
    t53 = t52*t17
    t55 = t6-t10
    t56 = t55*t17
    t58 = -t49*B(1,1)+t53*B(1,2)-t56*B(1,3)
    t64 = -t49*B(1,2)+t53*B(2,2)-t56*B(2,3)
    t70 = -t49*B(1,3)+t53*B(2,3)-t56*B(3,3)
    t83 = -A(2,1)*A(3,2)+A(2,2)*A(3,1)
    t84 = t83*t17
    t87 = A(1,1)*A(3,2)-t12
    t88 = t87*t17
    t90 = t4-t8
    t91 = t90*t17
    C(1) = t30*t3*t17-t36*t22*t17-t42*t27*t17
    C(2) = -t58*t48*t17+t64*t52*t17-t70*t55*t17
    C(3) = -t30*t48*t17+t36*t52*t17-t42*t55*t17
    C(4) = -(-t84*B(1,1)-t88*B(1,2)+t91*B(1,3))*t83*t17-&
         (-t84*B(1,2)-t88*B(2,2)+t91*B(2,3))*t87*t17+(-t84*B(1,3)-&
         t88*B(2,3)+t91*B(3,3))*t90*t17
    C(5) = -t58*t83*t17-t64*t87*t17+t70*t90*t17
    C(6) = -t30*t83*t17-t36*t87*t17+t42*t90*t17

    END FUNCTION matmul_AiBAit
    !
    !
    !
    SUBROUTINE  prin2cart(L,V,C)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Output condenced 3x3 symmetric tensor C from its eigenvalues L and eigen vectors V
    REAL(r8k), INTENT(IN) :: L(3), V(3,3)
    REAL(r8k), INTENT(OUT) :: C(6)

    C(1) = V(1,1)**2*L(1) + V(1,2)**2*L(2) + V(1,3)**2*L(3)
    C(2) = V(2,1)**2*L(1) + V(2,2)**2*L(2) + V(2,3)**2*L(3)
    C(3) = V(3,1)**2*L(1) + V(3,2)**2*L(2) + V(3,3)**2*L(3)
    C(4) = V(1,1)*L(1)*V(2,1) + V(1,2)*L(2)*V(2,2) + V(1,3)*L(3)*V(2,3)
    C(5) = V(2,1)*L(1)*V(3,1) + V(2,2)*L(2)*V(3,2) + V(2,3)*L(3)*V(3,3)
    C(6) = V(1,1)*L(1)*V(3,1) + V(1,2)*L(2)*V(3,2) + V(1,3)*L(3)*V(3,3)

    END SUBROUTINE prin2cart
    !
    !
    !
    SUBROUTINE jacobi2 (be, dl2, nvect)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Solve eigen values and vectors for two Dimensional case
    REAL(r8k), INTENT(IN) :: &
         be(2,2)    ! 2x2 matrix
    REAL(r8k), INTENT(OUT) :: &
         dl2(3), &  ! Eigen values
         nvect(2,2) ! Eigen vectors
    REAL(r8k) :: tau, t, c, c2, s, s2, tmp(3)

    nvect(1,1) = 1.0_r8k
    nvect(2,1) = 0.0_r8k
    nvect(1,2) = 0.0_r8k
    nvect(2,2) = 1.0_r8k

    tmp(1) = be(1,1)
    tmp(2) = be(2,2)
    tmp(3) = be(1,2)
    dl2(1) = tmp(1)
    dl2(2) = tmp(2)

    IF ( ABS(tmp(3)) > 0.0_r8k ) THEN 
        tau = 0.5_r8k * (tmp(2) - tmp(1)) / tmp(3)
        t   = SIGN(1.0_r8k, tau) / (ABS(tau) + SQRT(1.0_r8k + tau ** 2))
        c2  = 1.0_r8k / (1 + t ** 2)
        c   = SQRT(c2)
        s   = t * c
        s2  = s ** 2
        dl2(1) = c2 * tmp(1) - 2.0_r8k * c * s * tmp(3) + s2 * tmp(2)
        dl2(2) = s2 * tmp(1) + 2.0_r8k * c * s * tmp(3) + c2 * tmp(2)
        nvect(1,1) =  c
        nvect(2,1) = -s
        nvect(1,2) =  s
        nvect(2,2) =  c
    ENDIF

    END SUBROUTINE jacobi2
    !
    !
    !
    SUBROUTINE jacobi3 (be, dl2, nvect)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Solve eigen values and vectors for three Dimensional case
    REAL(r8k), INTENT(IN) :: be(3,3)    ! 3x3 matrix
    REAL(r8k), INTENT(OUT) :: dl2(3), &  ! Eigen values
         nvect(3,3) ! Eigen vectors
    REAL(r8k), PARAMETER :: eps_Real = 1.0e-15_r8k
    INTEGER(ipk) :: i, j, maxi
    REAL(r8k) :: tau, t, c, c2, s, s2, cs, tmp(6), tmp2(6), tmpvect(3,3), absnd(3), maxnd

    DO i = 1, 3
        DO j = 1, 3
            nvect(i,j) = 0.0_r8k
        ENDDO
        nvect(i,i) = 1.0_r8k
    ENDDO

    tmp(1) = be(1,1)
    tmp(2) = be(2,2)
    tmp(3) = be(3,3)
    tmp(4) = be(1,2)
    tmp(5) = be(2,3)
    tmp(6) = be(1,3)
    dl2(1) = tmp(1)
    dl2(2) = tmp(2)
    dl2(3) = tmp(3)

    DO
        ! Maximum nondiagonal term
        absnd(1:3) = (/ABS(tmp(4)),ABS(tmp(5)),ABS(tmp(6))/)
        maxnd = MAXVAL(absnd)
        maxi = 1
        maxnd = absnd(1)
        DO i = 2, 3
            IF ( absnd(i) > maxnd ) THEN
                maxnd = absnd(i)
                maxi = i
            ENDIF
        ENDDO
        ! Convergence
        IF ( maxnd < eps_Real ) EXIT
        SELECT CASE (maxi)
        CASE (1)
            tau = 0.5_r8k*(tmp(2) - tmp(1)) / tmp(4)
            t = SIGN(1.0_r8k,tau) / (ABS(tau) + SQRT(1.0_r8k + tau ** 2))
            c2 = 1.0_r8k / (1.0_r8k + t ** 2)
            c = SQRT(c2)
            s = t * c
            s2 = s ** 2
            cs = c * s
            tmp2(1) = c2 * tmp(1) - 2.0 * cs * tmp(4) + s2 * tmp(2)
            tmp2(2) = s2 * tmp(1) + 2.0 * cs * tmp(4) + c2 * tmp(2)
            tmp2(3) = tmp(3)
            tmp2(4) = cs * tmp(1) + c2 * tmp(4) - s2 * tmp(4) - cs * tmp(2)
            tmp2(5) = s * tmp(6) + c * tmp(5)
            tmp2(6) = c * tmp(6) - s * tmp(5)
            tmp(1:6) = tmp2(1:6)
            tmpvect(1,1) = nvect(1,1) * c - nvect(1,2) * s
            tmpvect(2,1) = nvect(2,1) * c - nvect(2,2) * s
            tmpvect(3,1) = nvect(3,1) * c - nvect(3,2) * s
            tmpvect(1,2) = nvect(1,1) * s + nvect(1,2) * c
            tmpvect(2,2) = nvect(2,1) * s + nvect(2,2) * c
            tmpvect(3,2) = nvect(3,1) * s + nvect(3,2) * c
            DO i = 1, 3
                nvect(i,1) = tmpvect(i,1)
                nvect(i,2) = tmpvect(i,2)
            ENDDO
       CASE (2)
            tau = 0.5_r8k * (tmp(3) - tmp(2)) / tmp(5)
            t = SIGN(1.0_r8k, tau) / (ABS(tau) + SQRT(1.0_r8k + tau ** 2))
            c2 = 1.0_r8k / (1.0_r8k + t ** 2)
            c = SQRT(c2)
            s = t * c
            s2 = s ** 2
            cs = c * s
            tmp2(1) = tmp(1)
            tmp2(2) = c2 * tmp(2) - 2.0_r8k * cs * tmp(5) + s2 * tmp(3)
            tmp2(3) = s2 * tmp(2) + 2.0_r8k * cs * tmp(5) + c2 * tmp(3)
            tmp2(4) = c * tmp(4) - s * tmp(6)
            tmp2(5) = cs * tmp(2) + c2 * tmp(5) - s2 * tmp(5) - cs * tmp(3)
            tmp2(6) = s * tmp(4) + c * tmp(6)
            tmp(1:6) = tmp2(1:6)
            tmpvect(1,2) = nvect(1,2) * c - nvect(1,3) * s
            tmpvect(2,2) = nvect(2,2) * c - nvect(2,3) * s
            tmpvect(3,2) = nvect(3,2) * c - nvect(3,3) * s
            tmpvect(1,3) = nvect(1,2) * s + nvect(1,3) * c
            tmpvect(2,3) = nvect(2,2) * s + nvect(2,3) * c
            tmpvect(3,3) = nvect(3,2) * s + nvect(3,3) * c
            DO i=1,3
                nvect(i,2) = tmpvect(i,2)
                nvect(i,3) = tmpvect(i,3)
            ENDDO
       CASE (3)
            tau = 0.5_r8k * (tmp(3) - tmp(1)) / tmp(6)
            t = SIGN(1.0_r8k, tau) / (ABS(tau)+SQRT(1.0_r8k + tau ** 2))
            c2 = 1.0_r8k / (1.0_r8k + t ** 2)
            c = SQRT(c2)
            s = t * c
            s2 = s ** 2
            cs = c * s
            tmp2(1) = c2 * tmp(1) - 2.0_r8k * cs * tmp(6) + s2 * tmp(3)
            tmp2(2) = tmp(2)
            tmp2(3) = s2 * tmp(1) + 2.0_r8k * cs * tmp(6) + c2 * tmp(3)
            tmp2(4) = c * tmp(4) - s * tmp(5)
            tmp2(5) = s * tmp(4) + c * tmp(5)
            tmp2(6) = cs * tmp(1) - s2 * tmp(6) + c2 * tmp(6) - cs * tmp(3)
            tmp(1:6) = tmp2(1:6)
            tmpvect(1,1) = nvect(1,1) * c - nvect(1,3) * s
            tmpvect(2,1) = nvect(2,1) * c - nvect(2,3) * s
            tmpvect(3,1) = nvect(3,1) * c - nvect(3,3) * s
            tmpvect(1,3) = nvect(1,1) * s + nvect(1,3) * c
            tmpvect(2,3) = nvect(2,1) * s + nvect(2,3) * c
            tmpvect(3,3) = nvect(3,1) * s + nvect(3,3) * c
            DO i = 1, 3
                nvect(i,1) = tmpvect(i,1)
                nvect(i,3) = tmpvect(i,3)
            ENDDO
        END SELECT
    ENDDO

    DO i = 1, 3
        dl2(i) = tmp(i)
    ENDDO

    END SUBROUTINE jacobi3
    !
    !
    !
    FUNCTION interpolate(n, Data_x, Data_y, xp) RESULT (yp)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !>
    INTEGER(ipk) :: n
    REAL(r8k) :: Data_x(n), Data_y(n), xp, yp
    INTEGER(ipk) :: i

    ! Value below the Data range
    yp = Data_y(1)
    IF ( xp < Data_x(1) ) RETURN

    ! Linear interpolation
    DO i = 2, n
        IF ( xp < Data_x(i) ) THEN
            yp = (Data_y(i) - Data_y(i-1)) * (xp - Data_x(i-1)) / (Data_x(i) - Data_x(i-1)) + Data_y(i-1)
            RETURN
        ENDIF
        yp = Data_y(i)
    ENDDO

    END FUNCTION interpolate
    !
END MODULE math_fraptran














