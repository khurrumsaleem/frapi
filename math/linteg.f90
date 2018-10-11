function linteg(ya,yb,ra,rb,h) result(total)
! Integral of the linear function
! over cylindrical segment
    real(8) :: total
    real(8) :: pi = 3.14159265358979323846264338327950288d0
    real(8) :: ya, yb, ra, rb, h

    total = 2 * pi * h * (-ra**2 * (ya/2 + yb)/3 + ra*rb*(-ya + yb)/6 + rb**2 * (ya + yb/2)/3)

    return

end function linteg
