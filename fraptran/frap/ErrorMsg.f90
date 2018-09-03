MODULE ErrorMsg
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains subroutines that give error messages and abort the run if necessary.
    !> Subroutines include 
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 05/15/2016
    !
    CONTAINS
    !
    SUBROUTINE fabend
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine aborts execution.
    !>@author
    !> $Id: envrl.s,v 2.102 1999/05/19 23:08:13 randyt Exp randyt $
    !
    !EXTERNAL abort
    !CALL abort
    stop
    !
    END SUBROUTINE fabend
    !
    !
    !
    SUBROUTINE errori (nerr, nstop)
    USE Kinds
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine prints out specified input error messages
    !>@author
    !> Updated by Ian Porter, NRC
    !
    INTEGER(ipk) :: nerr, nstop
    CHARACTER(LEN=4) :: nextstep
    !
    nextstep = 'STOP'
    !
    WRITE(ounit,5)
5   FORMAT(' *** error detected *** ')
    !
    SELECT CASE (nerr)
    CASE (1)
        WRITE(ounit,12)
12      FORMAT(/,' Input error found in Subroutine ht1inp ')
        IF (nstop >= 1) nextstep = 'cont'
    CASE (2)
        WRITE(ounit,22)
22      FORMAT(' Not enough space in dynamic storage array',/,'region size must be increased')
        IF (nstop >= 1) nextstep = 'cont'
    CASE (3)
        WRITE(ounit,32)
32      FORMAT (' The number of radial nodes is more than the maximum')
        IF (nstop >= 1) nextstep = 'cont'
    CASE DEFAULT
        WRITE(ounit,202) nerr
202     FORMAT(' Undefined input error, no.', i5)
        nextstep = 'STOP'
    END SELECT
    !
    SELECT CASE (nextstep) !Determines whether the code stops or continues
    CASE ('STOP') ! Code stops
        WRITE(ounit,902)
902     FORMAT(/,' Execution stopped ')
        CALL fabEnd
        STOP
    CASE ('cont') ! Code continues running
        WRITE(ounit,922)
922     FORMAT(/,' Execution continuing ')
    END SELECT
    !
    END SUBROUTINE errori
    !
    !
    !
    SUBROUTINE Namelist_Read_Error (FileUnit, NameListBlock)
    USE Kinds
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This subroutine reads the input line that had an error and writes it to the screen and output file
    !> before terminating the code. The input line that had an error is assumed to be the line that was just read.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 12/21/2015
    !
    ! Input
    !
    ! FileUnit      - Unit # associated with the file being read
    ! NameListBlock - Name of namelist block that an error occured in
    !
    INTEGER(ipk), INTENT(IN) :: FileUnit
    CHARACTER(LEN=*), INTENT(IN) :: NameListBlock
    CHARACTER(LEN=200) :: errorline
    !
    WRITE (0,154) NameListBlock
    WRITE (ounit,154) NameListBlock
154 FORMAT ('Syntax error in namelist $',a,'. Part of line:')
    BACKSPACE FileUnit
    READ (FileUnit,'(A)') errorline
    WRITE (0,'(A)') TRIM(errorline)
    WRITE (ounit,'(A)') TRIM(errorline)
    ERROR STOP
    !
    END SUBROUTINE Namelist_Read_Error
    !
END MODULE ErrorMsg
