PROGRAM FRAPCON_4_0_Patch_1
    USE Kinds
    USE Conversions
    USE Variables, ONLY : ProblemTime, ounit, it
    USE FileIO, ONLY : iofiles, IOEcho
    USE TimeStep
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    !>@brief
    !> This is the driver for the FRAPCON Code. Version 4.0 Patch 1.
    !
    REAL(r8k) :: LastTime
    !
    CALL iofiles
    CALL IOEcho
    !
    CALL frpcon
    !
    CLOSE (ounit)
    ! Print the time at which the code execution stopped (Convert from seconds to day)
    LastTime = ProblemTime(it) * sectoday
    WRITE (0,10) LastTime
10  FORMAT ('FRAPCON code execution finished with a problem time of ',f11.4,' days.')
    !
END PROGRAM FRAPCON_4_0_Patch_1