module h5file

    use HDF5

    implicit none

    INTEGER :: error ! Error flag
    INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
    INTEGER(HID_T) :: file_id       ! File identifier    
    INTEGER(HID_T) :: group_id     ! Group identifier 

    type, public :: tp_h5file

    contains
        procedure :: open  => open_
        procedure :: dump  => dump_
        procedure :: read  => read_
        procedure :: close => close_
        procedure :: makegroup => make_group_
        procedure :: closegroup => close_group_
    end type tp_h5file

contains

    subroutine open_(this, filename)

        class (tp_h5file), intent(in) :: this

        character(*) :: filename
        !
        ! Initialize FORTRAN interface.
        !
        CALL h5open_f(error)
        !
        CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

    end subroutine open_

    subroutine make_group_(this, groupname)

        class (tp_h5file), intent(in) :: this

        character(*) :: groupname

        !
        ! Create a group named "/MyGroup" in the file.
        !
        CALL h5gcreate_f(file_id, groupname, group_id, error)
    
    end subroutine make_group_

    subroutine close_group_(this)

        class (tp_h5file), intent(in) :: this

        !
        ! Close the group.
        !
        CALL h5gclose_f(group_id, error)
    
    end subroutine close_group_

    subroutine dump_(this, dataname, value)

        class (tp_h5file), intent(in) :: this

        character(*)    :: dataname
        integer         :: i
        real(8), target :: value(:)

        INTEGER(HID_T)     :: dset_id      ! Dataset identifier 
        INTEGER(HSIZE_T)   :: data_dims(1)

        data_dims(1) = size(value)

        !
        ! Open an existing group in the specified file.
        !
        !CALL h5gopen_f(this % file_id, groupname, this % group_id, error)
        !
        !i = scan(dataname, '/')
        !dataname(1) = '|'
        !
        ! Create dataspaces for datasets
        !
        CALL h5screate_simple_f(1, data_dims , dspace_id, error)
        !
        ! Create the dataset.
        !
        CALL H5Dcreate_f(group_id, dataname, H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
        !
        ! Write the dataset.
        !
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, value, data_dims, error)
        !
        ! Close the dataspace for the second dataset.
        !
        CALL h5sclose_f(dspace_id, error)

    end subroutine dump_

    subroutine read_(this, key, value)

        class (tp_h5file), intent(in) :: this

        character(*) :: key
        real(8)      :: value(:)

        !f_ptr = C_LOC(value(1))
        !CALL h5dread_f(dset_idr8, h5kind_to_type(real_kind_15,H5_REAL_KIND), f_ptr,  error)

    end subroutine read_

    subroutine close_(this)

        class (tp_h5file), intent(in) :: this
        !
        ! Close the file.
        !
        CALL h5fclose_f(file_id, error)
        !
        ! Close FORTRAN interface.
        !
        CALL h5close_f(error)

    end subroutine close_

end module h5file

