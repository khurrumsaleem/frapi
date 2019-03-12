module m_utils

    implicit none

    contains

    function int2str (a) result (b)
        ! Convertation of integer to string
        implicit none
        integer :: a
        character(len=128) :: b
        write(b,'(I0.4)') a
    end function int2str

    function readword (line, n) result (word)
        ! Read n-th word in a line
        implicit none
        character(*), intent(in) :: line
        integer, intent(in) :: n
        integer, parameter :: clen = 512
        character(len=512) :: word
        character :: a
        integer :: i, j, k
        logical :: is_word

        k = 0
        j = 0
        is_word = .false.
        word = ''
        do i = 1, clen
            a = line(i:i)
            if (a == ' ') then
                if ((is_word).and.(k == n)) then
                    exit
                else
                    is_word = .false.
                    cycle
                endif
            else
                if (.not. is_word) k = k + 1
                is_word = .true.
                if (k == n) then
                    j = j + 1
                    word(j:j) = a
                endif
            endif
        enddo

    end function readword

end module m_utils