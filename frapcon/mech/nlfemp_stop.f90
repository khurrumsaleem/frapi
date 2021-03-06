   SUBROUTINE nlfemp_stop (wrtout)
    USE Kinds_frapcon
    USE conversions_frapcon
    USE common_parameters_frapcon
    USE FEA_IO_frapcon, ONLY : write_output
    USE FEA_deallocate_frapcon
    IMPLICIT NONE
    !>@brief
    !> Stop the calculation
    !
    INTEGER(ipk), INTENT(IN) :: wrtout

    ! Write output for last converged step when stopped due to error
    IF ((wrtout == 1) .AND. (.NOT. outwrt)) CALL write_output()

    ! Close all input/output units
    CLOSE (in_unit)
    CLOSE (out_unit)

    ! Deallocate all FE variables
    CALL nlfemp_deallocate()

    IF ((.NOT. lerror) .AND. (.NOT. quiet)) WRITE (ounit,FMT='(/,A,/)') 'NLFEMP analysis completed!'
    !
    STOP

   END SUBROUTINE nlfemp_stop



