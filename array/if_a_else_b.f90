module m_if_a_else_b

    implicit none

    interface if_a_else_b
        module procedure if_a_else_b_i4_0
        module procedure if_a_else_b_r8_0
    end interface if_a_else_b

    contains

    integer function if_a_else_b_i4_0(c, a, b)
        implicit none
        logical :: c
        integer :: a, b
        if (c) then
            if_a_else_b_i4_0 = a
        else
            if_a_else_b_i4_0 = b
        endif
    end function if_a_else_b_i4_0

    real(8) function if_a_else_b_r8_0(c, a, b)
        implicit none
        logical :: c
        real(8) :: a, b
        if (c) then
            if_a_else_b_r8_0 = a
        else
            if_a_else_b_r8_0 = b
        endif
    end function if_a_else_b_r8_0

end module m_if_a_else_b