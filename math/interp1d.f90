module m_interp1d

    implicit none

    interface interp1d
        module procedure interp1d_1
        module procedure interp1d_2
    end interface interp1d

    contains

    real(8) function interp1d_1 (mesh, vals, x)
        implicit none
        real(8), dimension (:) :: mesh
        real(8), dimension (:) :: vals
        real(8) :: x
        logical :: error
        real(8) :: xmin, xmax, dx, dy
        integer :: nx, ny, i

        nx = size(mesh)
        ny = size(vals)

        xmin = minval(mesh)
        xmax = maxval(mesh)

        error = .not. ((xmin <= x).and.(x <= xmax).and.(nx == ny))
        if (error) then
            write (*,*) 'ERROR: interp1d got wrong input data: ', nx, ny, xmin, xmax, x
            call backtrace
            call exit (-1)
        endif

        i = 1
        do while (x > mesh(i))
            i = i + 1
        enddo

        if (i == 1) then
            interp1d_1 = vals(1)
        else
            dx = mesh(i) - mesh(i-1)
            dy = vals(i) - vals(i-1)
            interp1d_1 = vals(i-1) + dy / dx * (x - mesh(i-1))
        endif

    end function interp1d_1

    real(8) function interp1d_2 (xydata, x)
        implicit none
        real(8), dimension (:) :: xydata
        real(8) :: x
        integer :: n, i
        n = size(xydata)
        if (.not. mod(n,2) == 0 ) then
            write(*,*) 'ERROR: interp1d got wrong input data: ', n
            call backtrace
            call exit (-1)
        endif
        interp1d_2 = interp1d_1( (/( xydata(2*i),   i = 1, n/2 )/), &
                                 (/( xydata(2*i-1), i = 1, n/2 )/), x)
    end function interp1d_2

end module m_interp1d