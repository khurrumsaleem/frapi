MODULE SpentFuel
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines needed for reading the $spentfuel block 
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 7/17/2015
    !
    CONTAINS
    !
    SUBROUTINE SpentFuelRead ()
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : iunit, ounit, addgmles, addswell, stopox, IsModelingSpentFuel
    USE DatingData, ONLY : ncreephist, creeptime, ncreepstep ,creeppooltime, datingtstep, &
      &                    creeptabtime, creeptabtemp, creeptabstress, idatingcreep, ncreeptab, &
      &                    Allocate_DatingData
    USE FileIO, ONLY : Namelist_Read_Error
    IMPLICIT NONE
    !>@brief
    !> Reads the input file for the block $spentfuel, and if it exists, reads the data and allocates
    !> any arrays. The $spentfuel block contains variable ncreeptab, which is used to allocate the
    !> size of several arrays also in the block, so a traditional block NAMELIST read is not
    !> performed. Instead, the code does a search within the block to find ncreeptab, get the value,
    !> allocate the arrays and then perform a traditional NAMELIST read.
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 7/16/2015
    !
    INTEGER(ipk) :: i, Value, Value2, InputStat, LineLength, Comment, linecnt = 0
    LOGICAL :: EndofBlock = .FALSE., EndofLine = .FALSE.
    CHARACTER(LEN=200) :: line, errorline
    !
    ! Variables in $spentfuel input block
    !
    NAMELIST / spentfuel / ncreephist, creeptime, ncreepstep ,creeppooltime, datingtstep, &
      &                    creeptabtime, creeptabtemp, creeptabstress, idatingcreep, ncreeptab, &
      &                    addgmles, addswell, stopox
    ! Set defaults for the DATING spent fuel creep model (OFF)
    idatingcreep = 0
    ncreeptab = 0
    ! Check to see if the $spentfuel block exists
    REWIND iunit
    ReadLoop: DO
        READ (iunit, '(A)', IOSTAT = InputStat) line
        IF (InputStat == 0) THEN
            ! Line read was successful
            linecnt = linecnt + 1
            ! Check to see if spent fuel block exists
            Value = INDEX(line,'$spentfuel')
            ! Make sure the line is not commented out
            Comment = INDEX(line,'!')
            IF ((Value > 0) .AND. ((Comment == 0) .OR. (Value < Comment))) THEN
                ! Spent fuel block exists
                BACKSPACE iunit
                ! Read $spentfuel block for value of ncreeptab
                SpentFuelReadLoop: DO
                    ! Check for input flag ncreeptab to allocate necessary arrays
                    READ (iunit, '(A)', IOSTAT = InputStat) line
                    IF (InputStat == 0) THEN
                        Value = INDEX(line,'ncreeptab')
                        Comment = INDEX(line,'!')
                        ! Check to see if line has a comment before the variable
                        IF ((Value > 0) .AND. ((Comment == 0) .OR. (Value < Comment))) THEN
                            ! Shift to the equals sign after the variable
                            Value = Value + INDEX(line(Value:),'=')
                            ! Remove any spaces after the = and before the value
                            DO WHILE (line(Value:Value) == ' ')
                                Value = Value + 1
                            END DO
                            ! Find the end of the value
                            Value2 = Value + INDEX(line(Value:),',')
                            IF (Value2 == Value) THEN
                                ! No comma was found after value. Search for space or end of text instead
                                DO WHILE (line(Value2:Value2) /= ' ' .AND. .NOT. EndofLine)
                                    Value2 = Value2 + 1
                                    IF (Value2 > LEN_TRIM(line)) EndofLine = .TRUE.
                                END DO
                            ELSE
                                ! Shift to before the comma
                                Value2 = Value2 - 1
                                ! Ensure no spaces were placed between the comma and the number
                                DO WHILE (line(Value2-1:Value2-1) == ' ')
                                    Value2 = Value2 - 1
                                END DO
                            END IF
                            ! Store the value
                            READ (line(Value:Value2-1),'(i15)') ncreeptab
                            ! Rewind to start of block
                            REWIND iunit
                            DO i = 1, (linecnt - 1)
                                READ (iunit)
                            END DO
                            IsModelingSpentFuel = .TRUE.
                            EXIT ReadLoop
                        ELSE
                            ! Check to see if end of block
                            Value = INDEX(line,'$end')
                            Value2 = INDEX(line, '!')
                            IF ((Value > 0) .AND. ((Value2 == 0) .OR. (Value < Value2))) THEN
                                ! End of block was reached before ncreeptab was read
                                EndofBlock = .TRUE.
                                WRITE (0, 110)
                                WRITE (ounit, 110)
        110                     FORMAT (' Error: variable ncreeptab was not found in $spentfuel block.',/, &
                                  &     ' Arrays can not be allocated. Code will not model model spent fuel.')
                                IsModelingSpentFuel = .FALSE.
                            END IF
                        END IF
                    ELSE IF (InputStat < 0) THEN
                        ! End of File or Record
                        IsModelingSpentFuel = .FALSE.
                    ELSE
                        ! Error
                        IsModelingSpentFuel = .FALSE.
                        WRITE (0,*) 'Syntax error in namelist $spentfuel part of line'
                        WRITE (ounit,*) 'Syntax error in namelist $spentfuel part of line'
                        STOP
                    END IF
                    EXIT ReadLoop
                END DO SpentFuelReadLoop
            END IF
        ELSE IF (InputStat < 0) THEN
            ! End of File or Record
            IsModelingSpentFuel = .FALSE.
            EXIT ReadLoop
        ELSE
            ! Error
            IsModelingSpentFuel = .FALSE.
            WRITE (0,*) 'Syntax error in namelist $spentfuel part of line'
            WRITE (ounit,*) 'Syntax error in namelist $spentfuel part of line'
            STOP
        END IF
    END DO ReadLoop
    !
    IF (IsModelingSpentFuel) THEN ! User is using spent fuel modeling options
        ! Allocate variables
        CALL Allocate_DatingData
        ! Read $spentfuel block
        READ (iunit, spentfuel, IOSTAT = InputStat)
        IF (InputStat == 0) THEN
            ! Read was successful. Check for obvious input value errors.
            CALL SpentFuelErrorCheck
        ELSE
            CALL Namelist_Read_Error (iunit, 'spentfuel')
        END IF
    END IF
    !
    END SUBROUTINE SpentFuelRead
    !
    !
    !
    SUBROUTINE SpentFuelErrorCheck
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : ounit, iquit
    USE DatingData, ONLY : ncreephist, creeptime, ncreepstep ,creeppooltime, datingtstep, &
      &                    creeptabtime, creeptabtemp, creeptabstress, idatingcreep, ncreeptab, &
      &                    Allocate_DatingData
    IMPLICIT NONE
    !>@brief
    !> This subroutine checks for errors in the $spentfuel block
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> August 3, 2015
    !
    INTEGER(ipk) :: i, isw = 0
    !
    ! DATING Flags
    IF (idatingcreep < 0 .OR. idatingcreep > 3) WRITE (ounit,960) idatingcreep
    IF (idatingcreep < 0 .OR. idatingcreep > 3) isw = isw + 1
    IF (ncreephist < 1 .OR. ncreephist > 4) WRITE (ounit,961) ncreephist
    IF (ncreephist < 1 .OR. ncreephist > 4) isw = isw + 1
    IF (creeptime < 0.0_r8k) WRITE (ounit,962) creeptime
    IF (creeptime < 0.0_r8k) isw = isw + 1
    IF (creeppooltime < 5.0_r8k) WRITE (ounit,963) creeppooltime
    IF (creeppooltime < 5.0_r8k) isw = isw + 1
    IF (ncreepstep < 1) WRITE (ounit,964) ncreepstep
    IF (ncreepstep < 1) isw = isw + 1
    IF (ncreephist == 3 .OR. ncreephist == 4) THEN
        IF (ncreeptab < 1) WRITE (ounit,965) ncreeptab
        IF (ncreeptab < 1) isw = isw + 1
        DO i = 1, ncreeptab
            IF (creeptabtime(i) < 0.0_r8k) WRITE (ounit,966) i, creeptabtime(i)
            IF (creeptabtime(i) < 0.0_r8k) isw = isw + 1
        END DO
    END IF
    ! If input errors detected, set flag to stop execution (iquit)
    IF (isw /= 0) THEN
        WRITE (ounit,850) isw
        iquit = 1
    END IF
    !
10  FORMAT (/,5x,'Spent Fuel Modeling will be performed.')
960 FORMAT (1x,'idatingcreep must be between 0 and 3. idatingcreep = ',i3)
961 FORMAT (1x,'ncreephist must be between 1 and 4. ncreephist = ',i3)
962 FORMAT (1x,'creeptime not within limits (>= 0.0). creeptime = ',e11.4)
963 FORMAT (1x,'creeppooltime not within limits (>= 5.0). creeppooltime = ',e11.4)
964 FORMAT (1x,'ncreepstep not within limits (>= 1). ncreepstep = ',i5)
965 FORMAT (1x,'ncreeptab not within limits (>= 1). ncreeptab = ',i5)
966 FORMAT (1x,'creeptabtime(i) not within limits (>= 0.0). creeptabtime(',i5,') = ',e11.4)
850 FORMAT (1x,'total number of errors=',i5)
500 FORMAT (/,5x,'End of SpentFuel options.',/)
    !
    END SUBROUTINE SpentFuelErrorCheck
    !
END MODULE SpentFuel

