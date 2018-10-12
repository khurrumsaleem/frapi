module m_array_allocate

    implicit none

    interface array_allocate
        module procedure allocate_r8_1
    end interface array_allocate

    contains

    subroutine allocate_r8_1(var, i1, i2)
        real(8), dimension(:), allocatable :: var
        integer :: i1, i2
        if( .not. allocated(var) ) then
            allocate( var(i1:i2) )
        else
            write(*,*) 'ERROR: variables is already allocated'
        endif
    end subroutine allocate_r8_1

end module m_array_allocate