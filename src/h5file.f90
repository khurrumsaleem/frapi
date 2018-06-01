#if hdf5

module h5file

    use HDF5

    implicit none

    type, public :: tp_h5file
    contains
        procedure :: open  => open_
        procedure :: dump  => dump_
        procedure :: close => close_
    end type tp_h5file

contains

    subroutine open_(this, filename)
        class (tp_h5file), intent(in) :: this
        character(*) :: filename
    end subroutine open_

    subroutine dump_(this, key, value)
        class (tp_h5file), intent(in) :: this
        character(*) :: key
        real(8) :: value(:)
    end subroutine dump_

    subroutine close_(this)
        class (tp_h5file), intent(in) :: this
    end subroutine close_

end module h5file

#endif

!    subroutine frapcon_dump(this, filename)
!
!        class (frapcon_type), intent(in) :: this
!
!        character(len=*) :: filename
!
!        integer :: hdferr
!        integer(HID_T)   :: file, space, dset1, dset2, dcpl ! handles
!
!        INTEGER          , PARAMETER :: dim0     = 4
!        INTEGER          , PARAMETER :: dim1     = 7
!        !INTEGER(HSIZE_T) , DIMENSION(1:2)           :: dims = (/dim0, dim1/) ! size write buffer
!        INTEGER          , PARAMETER :: rank     = 1
!
!        ! Initialize FORTRAN interface.
!        call h5open_f(hdferr)
!        ! Create a new file using the default properties.
!        !
!        CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
!        !
!        ! Create dataspace with a null dataspace.
!        !
!        CALL H5Screate_f(H5S_NULL_F, space, hdferr)
!        CALL h5dcreate_f(file, dataset, H5T_STD_I32LE, space, dset, hdferr)
!        CALL h5sclose_f(space, hdferr)
!        !
!        ! Create dataspace.  Setting maximum size to be the current size.
!        !
!        CALL h5screate_simple_f(2, dims, space, hdferr)
!        !
!        ! Create the attribute and write the floating point data to it.
!        ! In this example we will save the data as 64 bit little endian
!        ! IEEE floating point numbers, regardless of the native type.  The
!        ! HDF5 library automatically converts between different floating
!        ! point types.
!        !
!        CALL H5Acreate_f(dset, attribute, H5T_IEEE_F64LE, space, attr, hdferr)
!        f_ptr = C_LOC(wdata(1,1))
!        CALL H5Awrite_f(attr, H5T_NATIVE_DOUBLE, f_ptr, hdferr)
!        !
!        ! Close and release resources.
!        !
!        CALL H5Aclose_f(attr, hdferr)
!        CALL H5Dclose_f(dset, hdferr)
!        CALL H5Fclose_f(file, hdferr)
!
!    end subroutine frapcon_dump
