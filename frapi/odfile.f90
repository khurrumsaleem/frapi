module odfile

    implicit none

    type, public :: t_odfile

        integer :: findex

        contains

        procedure :: open  => open_
        procedure :: write_i4_0 => write_i4_0_
        procedure :: write_i4_1 => write_i4_1_
        procedure :: write_r8_0 => write_r8_0_
        procedure :: write_r8_1 => write_r8_1_
        procedure :: write_r8_2 => write_r8_2_
        procedure :: close => close_

    end type t_odfile

    contains

    subroutine open_(this, fname, findex)
        class (t_odfile), intent(inout) :: this
        character(*) :: fname
        integer, optional :: findex
        this % findex = 1057
        if (present(findex)) this % findex = findex
        open(this % findex, file=fname, status='unknown', form='formatted')
    end subroutine open_

    subroutine close_(this)
        class (t_odfile), intent(in) :: this
        close(this % findex)
    end subroutine close_

    subroutine write_i4_0_(this, name, comment, var)
        class (t_odfile), intent(in) :: this
        character(*) :: name, comment
        integer :: var
        write(this % findex, '(a,a)') '#', trim(name)
        write(this % findex, '(a,a)') '#', trim(comment)
        write(this % findex, '(a)') '#i4_0'
        write(this % findex, '(i10)') var
    end subroutine write_i4_0_

    subroutine write_i4_1_(this, name, comment, var)
        class (t_odfile), intent(in) :: this
        character(*) :: name, comment
        integer :: shape_var(1)
        integer :: var(:)
        shape_var = shape(var)
        write(this % findex, '(a,a)') '#',trim(name)
        write(this % findex, '(a,a)') '#',trim(comment)
        write(this % findex, '(a,i10)') '#i4_1 ', shape_var(1)
        write(this % findex, '(10000i10)') var
    end subroutine write_i4_1_

    subroutine write_r8_0_(this, name, comment, var)
        class (t_odfile), intent(in) :: this
        character(*) :: name, comment
        real(8) :: var
        write(this % findex, '(a,a)') '#',trim(name)
        write(this % findex, '(a,a)') '#',trim(comment)
        write(this % findex, '(a)') '#r8_0'
        write(this % findex, '(e20.9)') var
    end subroutine write_r8_0_

    subroutine write_r8_1_(this, name, comment, var)
        class (t_odfile), intent(in) :: this
        character(*) :: name, comment
        integer :: shape_var(1)
        real(8) :: var(:)
        shape_var = shape(var)
        write(this % findex, '(a,a)') '#',trim(name)
        write(this % findex, '(a,a)') '#',trim(comment)
        write(this % findex, '(a,i10)') '#r8_1 ', shape_var(1)
        write(this % findex, '(10000e20.9)') var
    end subroutine write_r8_1_

    subroutine write_r8_2_(this, name, comment, var)
        class (t_odfile), intent(in) :: this
        character(*) :: name, comment
        integer :: shape_var(2), i
        real(8) :: var(:,:)
        shape_var = shape(var)
        write(this % findex, '(a,a)') '#',trim(name)
        write(this % findex, '(a,a)') '#',trim(comment)
        write(this % findex, '(a,2i10)') '#r8_2 ', shape_var(1), shape_var(2)
        do i = 1, shape_var(1)
            write(this % findex, '(10000e20.9)') var(i,:)
        enddo
    end subroutine write_r8_2_

end module odfile