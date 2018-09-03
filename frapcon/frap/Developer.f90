MODULE Developer
    USE Kinds
    USE Conversions
    USE Variables
    IMPLICIT NONE
    !>@brief
    !> This module contains the information for reading and applying developer options
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 7/17/2015
    !
    ! ********************************************************************************
    ! **  NOTE: THE USE OF THESE OPTIONS ARE NOT RECOMMENDED AND MAY INVALIDATE     **
    ! **        THE RESULTS OF THE CALCULATION. THE FRAPCON VALIDATION TEST CASES   **
    ! **        DO NOT USE THESE OPTIONS. USE AT YOUR OWN DISCRETION.               **
    ! ********************************************************************************
    !
    CONTAINS
    !
    SUBROUTINE DeveloperOptions
    USE Kinds
    USE Conversions
    USE Variables, ONLY : nfrttr, modheat, cladelmod, relocmodel, fuelreloc, gaprecov, &
      &                   iunit, ounit, TimeIntegration, newtimestep, RestartTime, updated_restart, &
      &                   fuelreloc, nread, nrestr
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE FileIO, ONLY : Namelist_Read_Error
    IMPLICIT NONE
    !>@brief
    !> This subroutine performs the read for the developer input options
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> June 25, 2015
    !
    INTEGER(ipk) :: Value, InputStat
    LOGICAL :: IsDeveloper = .FALSE.
    CHARACTER(LEN=100) :: line, errorline
    !
    NAMELIST / developer / nfrttr, modheat, cladelmod, RelocModel, fuelreloc, gaprecov, &
      &                    TimeIntegration, newtimestep, updated_restart, nread, nrestr, &
      &                    RestartTime, gaphtcmult, calcoxide
    ! Set Developer Defaults
    nfrttr = 0
    modheat = 0.0_r8k
    cladelmod = -1.0_r8k
    RelocModel = 'FRAPCON-3.5'
    fuelreloc = -1.0_r8k
    gaprecov = 0.5_r8k
    updated_restart = .FALSE.
    TimeIntegration = 0 ! (Default = Off)
    newtimestep = 1.0_r8k ! (1 day)
    !
    ! Read $developer
    ! This first checks to see if the block $Developer exists. Extra logic was addded so that
    ! it does not matter where in the input file the block was placed. If the block exists
    ! (if the words $Developer are in the input file), Developer becomes true and it reads the
    ! block. If not, once it reaches the end of the file, it will skip the read.
    REWIND Iunit
    ReadLoop: DO
        READ (iunit, '(A)', IOSTAT = InputStat) line
        IF (InputStat == 0) THEN
            Value = INDEX(line,'$developer')
            IF (Value > 0) THEN
                BACKSPACE iunit
                ! Make sure the line is not commented out
                Value = INDEX(line(1:2),'!')
                IF (Value == 0) IsDeveloper = .TRUE.
                EXIT ReadLoop
            END IF
        ELSE IF (InputStat < 0) THEN
            ! End of File or Record
            IsDeveloper = .FALSE.
            EXIT ReadLoop
        ELSE
            ! Error
            IsDeveloper = .FALSE.
            WRITE (0,*) 'Syntax error in namelist $developer part of line'
            WRITE (ounit,*) 'Syntax error in namelist $developer part of line'
            STOP
        END IF
    END DO ReadLoop
    !
    IF (IsDeveloper) THEN ! User is using developer options
        READ (iunit, developer, IOSTAT = InputStat)
        IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'developer')
        !
        ! Check for errors and perform conversions
        CALL DeveloperErrorCheck
        !
    END IF
    !
    END SUBROUTINE DeveloperOptions
    !
    !
    !
    SUBROUTINE DeveloperErrorCheck
    USE Kinds
    USE Conversions
    USE Variables, ONLY : nfrttr, modheat, cladelmod, relocmodel, fuelreloc, gaprecov, &
      &                   ounit, TimeIntegration, newtimestep, RestartTime, updated_restart, &
      &                   fuelreloc, ProblemTime, im, iplant
    IMPLICIT NONE
    !>@brief
    !> This subroutine checks for errors in the developer block.
    !> Afterwards, it performs any necessary conversions and calculations.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> June 25, 2015
    !
    ! Internal
    !
    ! unit(i)   - File to print information to
    !         0 = Command Window
    !     ounit = Output File
    !
    INTEGER(ipk) :: i, isw = 0
    INTEGER(ipk), PARAMETER :: n_write_files = 2
    INTEGER(ipk), DIMENSION(n_write_files) :: unit
    !
    unit(1) = 0
    unit(2) = ounit
    !
    ! Write to the screen (unit = 0) and the output file (unit = ounit) the
    ! developer options that are being used.
    ! On the last iteration (i = unitend), perform any necessary conversions.
    DO i = 1, n_write_files
        WRITE (unit(i), 10)
        ! Relocation Model
        SELECT CASE (RelocModel)
        CASE ('FRAPCON-3.3', 'FRAPCON-3.4', 'FRAPCON-3.4A')
            WRITE (unit(i), 100) RelocModel
        CASE ('FRAPCON-3.5')
            ! Do nothing. This is the default model.
        CASE ('OFF')
            WRITE (unit(i), 104) RelocModel
        CASE ('USER')
            WRITE (unit(i), 100) RelocModel
            IF (fuelreloc > 0.0_r8k .AND. fuelreloc < 1.0_r8k) THEN
                WRITE (unit(i), 101) fuelreloc
            ELSE
                WRITE (unit(i), 102) fuelreloc
            END IF
        CASE DEFAULT
            WRITE (unit(i), 103) RelocModel
            isw = isw + 1
        END SELECT
        ! Moderating heating option
        IF (modheat < 0.0_r8k) THEN
            ! User opts to use default values for direct moderator heating, set by the plant type flag.
            WRITE (unit(i), 111)
            SELECT CASE (iplant)
            CASE (-2)
                ! PWR Plant
                WRITE (unit(i), 112)
                IF (i == n_write_files) modheat = 0.026_r8k
            CASE (-3)
                ! BWR Plant
                WRITE (unit(i), 113)
                IF (i == n_write_files) modheat = 0.035_r8k
            CASE (-4)
                ! CANDU Plant
                WRITE (unit(i), 114)
                IF (i == n_write_files) modheat = 0.084_r8k
            CASE (-5)
                ! HTGR Plant
                WRITE (unit(i), 115)
                IF (i == n_write_files) modheat = 0.0_r8k
            CASE DEFAULT
                WRITE (unit(i), 116)
                IF (i == n_write_files) modheat = 0.0_r8k ! Error detected in iplant (outside of bounds). Set to 0.0.
            END SELECT
        ELSE IF (modheat >= 0.0_r8k .AND. modheat < 1.0_r8k) THEN
            ! Default (0.0) or User supplied values
            IF (modheat /= 0.0_r8k) WRITE (unit(i), 110) modheat
        ELSE
            ! Error
            WRITE (unit(i), 118) modheat
            isw = isw + 1
        END IF
        ! Cladding elastic modulus
        IF (cladelmod > 0.0_r8k) WRITE (unit(i), 120)
        ! Gap Recovery
        IF (gaprecov /= 0.5_r8k) THEN
            IF (gaprecov < 0.0_r8k .OR. gaprecov > 1.0_r8k) THEN
                WRITE (unit(i), 130) gaprecov
            ELSE
                WRITE (unit(i), 131) gaprecov
            END IF
        END IF
        ! Gap HTC multiplier
        IF (gaphtcmult /= 1.0_r8k .AND. gaphtcmult <= 0.0_r8k) WRITE (unit(i), 140) gaphtcmult
        ! Oxidation Calculation (ON/OFF)
        IF (.NOT. calcoxide) WRITE (unit(i), 150)
        ! Updated FRAPTRAN Restart File
        IF (updated_restart) WRITE (unit(i), 200)
        ! .frttr file for TRACE output options
        IF (nfrttr /= 0) WRITE (unit(i), 210)
        ! FRAPCON-to-FRAPCON Restart file to be written
        IF (nrestr /= 0) WRITE (unit(i), 220)
        ! FRAPCON-to-FRAPCON Restart file to be read
        IF (nread /= 0) WRITE (unit(i), 230)
        ! End of developer options
        WRITE (unit(i), 500)
        ! If input errors detected, set flag to stop execution (iquit)
        IF (isw /= 0) THEN
            WRITE (unit(i),850) isw
            IF (i == n_write_files) THEN
                iquit = 1
            ELSE
                isw = 0
            ENDIF
        END IF
    END DO
    !
10  FORMAT (/,5x,'Developer Options used.', &
      &     /,5x,'********************************************************************************', &
      &     /,5x,'**  NOTE: THE USE OF THESE OPTIONS ARE NOT RECOMMENDED AND MAY INVALIDATE     **', &
      &     /,5x,'**        THE RESULTS OF THE CALCULATION. THE FRAPCON VALIDATION TEST CASES   **', &
      &     /,5x,'**        DO NOT USE THESE OPTIONS. USE AT YOUR OWN DISCRETION.               **', &
      &     /,5x,'********************************************************************************', &
      &     /,5x,'The following non-default options are set:',/)
100 FORMAT (5x,'Relocation Model = ',a12)
101 FORMAT (5x,'User supplied relocation model will be used. fuelreloc = ',f10.4)
102 FORMAT (5x,'User supplied relocation model selected but bad value for fuelreloc. fuelreloc = ',f10.4)
103 FORMAT (5x,'Error in Relocation Model. RelocModel = ',a12)
104 FORMAT (5x,'Fuel relocation turned OFF')
110 FORMAT (5x,'User supplied moderator heating vaue will be used. modheat = ',f10.4)
111 FORMAT (5x,'Default plant value will be used for moderating heating')
112 FORMAT (5x,'PWR plant default moderator heating fraction of 0.026 will be used.')
113 FORMAT (5x,'BWR plant default moderator heating fraction of 0.035 will be used.')
114 FORMAT (5x,'HWR plant default moderator heating fraction of 0.084 will be used.')
115 FORMAT (5x,'HGTR plant default moderator heating fraction of 0.0 will be used.')
116 FORMAT (5x,'Error: bad value for plant type (iplant). Moderator heating fraction set to 0.0.')
118 FORMAT (5x,'Bad input for modheat. Must be < 1.0. modheat = ',f10.4)
120 FORMAT (5x,'User supplied cladding elastic modulus',f10.4,'Pa')
130 FORMAT (5x,'Bad input for gaprecov. Must be 0.0 < gaprecov < 1.0. gaprecov = ',f10.4)
131 FORMAT (5x,'User supplied gap recovery fraction will be used. gaprecov = ',f10.4)
140 FORMAT (5x,'Gap heat transfer coefficient multiplier not within limits (> 0.0). gaphtcmult = ',e11.4)
150 FORMAT (5x,'Oxidation calculation will be turned off. calcoxide = .FALSE.')
200 FORMAT (5x,'Updated FRAPTRAN restart file will be created.')
210 FORMAT (5x,'FRTTR file will be written.')
220 FORMAT (5x,'FRAPCON-to-FRAPCON Restart file will be written.')
230 FORMAT (5x,'FRAPCON to start from FRAPCON-to-FRAPCON Restart File.')
500 FORMAT (/,5x,'End of Developer options.',/)
850 FORMAT (1x,'total number of errors=',i5)
    !
    END SUBROUTINE DeveloperErrorCheck
    !
END MODULE Developer