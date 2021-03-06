    SUBROUTINE nlfemp_stop(wrtout)
    USE Kinds_fraptran
    USE common_parameters_fraptran
    USE FEA_deallocate_fraptran
    USE FEA_IO_fraptran
    IMPLICIT NONE
    !>@brief
    !> Stops the calculation
    INTEGER(ipk), INTENT(IN) :: wrtout

    ! WRITE output for last converged step when stopped due to error
    IF ((wrtout == 1) .AND. (.NOT. outwrt)) CALL Write_output()

    ! Close all input/output units
    CLOSE(UNIT=in_unit)
    CLOSE(UNIT=out_unit)

    ! Deallocate all FE variables
    CALL nlfemp_deallocate()

    IF ((.NOT. lerror) .AND. (.NOT. quiet)) WRITE(UNIT=6,FMT='(/,A,/)') 'NLFEMP analysis completed!'
    
    STOP

    END SUBROUTINE nlfemp_stop














