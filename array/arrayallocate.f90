module arrayallocate

    implicit none

    interface clone
        module procedure clone_ch_1
        module procedure clone_i4_1
        module procedure clone_i4_2
        module procedure clone_i4_3
        module procedure clone_r8_1
        module procedure clone_r8_2
        module procedure clone_r8_3
    end interface clone

    contains

    subroutine clone_ch_1(new, old)
        character(*), dimension(:) :: old
        character(*), dimension(:), allocatable :: new
        integer :: n(1)
        n = shape(old)
        call checkshape(n, 1)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1)) )
        endif
    end subroutine clone_ch_1

    subroutine clone_i4_1(new, old)
        integer(4), dimension(:) :: old
        integer(4), dimension(:), allocatable :: new
        integer :: n(1)
        n = shape(old)
        call checkshape(n, 1)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1)) )
        endif
    end subroutine clone_i4_1

    subroutine clone_i4_2(new, old)
        integer(4), dimension(:,:) :: old
        integer(4), dimension(:,:), allocatable :: new
        integer :: n(2)
        n = shape(old)
        call checkshape(n, 2)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1),1:n(2)) )
        endif
    end subroutine clone_i4_2

    subroutine clone_i4_3(new, old)
        integer(4), dimension(:,:,:) :: old
        integer(4), dimension(:,:,:), allocatable :: new
        integer :: n(3)
        n = shape(old)
        call checkshape(n, 3)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1), 1:n(2), 1:n(3)) )
        endif
    end subroutine clone_i4_3

    subroutine clone_r8_1(new, old)
        real(8), dimension(:) :: old
        real(8), dimension(:), allocatable :: new
        integer :: n(1)
        n = shape(old)
        call checkshape(n, 1)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1)) )
        endif
    end subroutine clone_r8_1

    subroutine clone_r8_2(new, old)
        real(8), dimension(:,:) :: old
        real(8), dimension(:,:), allocatable :: new
        integer :: n(2)
        n = shape(old)
        call checkshape(n, 2)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1),1:n(2)) )
        endif
    end subroutine clone_r8_2

    subroutine clone_r8_3(new, old)
        real(8), dimension(:,:,:) :: old
        real(8), dimension(:,:,:), allocatable :: new
        integer :: n(3)
        n = shape(old)
        call checkshape(n, 3)
        if( .not. allocated(new) ) then
            allocate( new(1:n(1), 1:n(2), 1:n(3)) )
        endif
    end subroutine clone_r8_3

    subroutine checkshape(shape, n)
        integer :: n
        integer :: shape(n)
        if ( shape(1) == 0 ) then
            write(*,*) 'ERROR: array 2 must be allocated'
            stop
        endif
    end subroutine checkshape

end module arrayallocate