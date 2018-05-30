subroutine linterp(d0, dx, d1, n)
! Linear interpolation and extrapolation of the data
! dx : (n,)-array of mesh thickness
! d0 : (n,)-array of the values at the center nodes
! d1 : (n+1,)-array of the values at the mesh nodes

    integer :: i, n
    real(8) :: d0(n), d1(n+1), dx(n)

    d1(1)   = ( d0(1) * ( 2*dx(1) + dx(2) ) - d0(2) * dx(1) ) / sum(dx(:2))
    d1(2:n) = (/( ( d0(i) * dx(i+1) + d0(i+1) * dx(i) ) / sum(dx(i:i+1)), i = 1, n-1 )/)
    d1(n+1) = ( d0(n) * ( 2*dx(n) + dx(n-1) ) - d0(n-1) * dx(n) ) / sum(dx(n-1:))

    ! in order to prevent a negative value at the ends
    d1(1)   = max(d1(1), 0)
    d1(n+1) = max(d1(n+1), 0)

end subroutine linterp