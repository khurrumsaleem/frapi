module m_convergence

    type, public :: t_convergence
        integer :: rank, ndim
        real(8) :: atol, rtol
        real(8), allocatable :: vstate(:,:)
        real(8), allocatable :: errors(:)
        contains

        procedure :: setup  => p_setup
        procedure :: update => p_update

    end type t_convergence

    contains

    subroutine p_setup(this, rank, ndim)
        implicit none
        class (t_convergence), intent(inout) :: this
        integer :: rank, ndim
        this % atol = 1.D-9
        this % rtol = 1.D-7
        this % rank = rank
        this % ndim = ndim
        allocate( this % vstate(rank, ndim) )
        allocate( this % errors(rank) )
        this % vstate(:,:) = 0.D+0
        this % errors(:) = 0.D+0
    end subroutine p_setup

    subroutine p_update(this, i, x)
        implicit none
        class (t_convergence), intent(inout) :: this
        integer :: i
        real(8) :: x(:)
        real(8) :: locerr
        this % errors(i) = norm2( (this % vstate(i,:) - x(:)) / &
                          (this % atol + this % rtol * x(:) ) ) / this % ndim**0.5
        this % vstate(i,:) = x(:)
    end subroutine p_update

end module m_convergence