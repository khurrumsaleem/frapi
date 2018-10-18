module m_round
    contains
    real(8) function round(x, level)
        implicit none
        integer :: level
        real(8) :: x, a
        a = (1.D+1)**level
        round = anint(x*a) / a
    end function round
end module m_round