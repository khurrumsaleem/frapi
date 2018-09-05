MODULE FileIO
    USE Kinds
    USE RunProperties, ONLY : edate, clockx
    IMPLICIT NONE
    !
    CONTAINS
    !
    SUBROUTINE iofiles
    USE Kinds
    USE conversions_frapcon
    USE Functions
    USE variables_frapcon, ONLY : iunit, ounit, scrunit
    IMPLICIT NONE
    !> @brief
    !> Subroutine iofiles reads the command line to get the input file name, opens the input file
    !> and opens any associated files (i.e. output file, plot file)
    !> @author
    !> iofiles developed by k. r. jones. Version 2.01 - dec
    !
    ! Input
    !
    ! iunit   - input file unit (filename obtained from command line or prompt)
    ! ounit   - output file unit
    ! scrunit - scratch file unit
    !
    ! Note:
    !
    ! Comments can be entered by placing a '*' in column 1 and will be echoed to the output file.
    ! Commens can be made on any line using a '!'
    ! An example file specification FORMAT is shown below.  Only the unit number and file name 
    ! are required input. Columns 73 to 80 are reserved for comments, a line ending in a comma 
    ! is continued on the following line.
    !
    ! FILEnn='\path\filename',  STATUS='status' , ACCESS='access',
    !         FORM='form', CARRIAGE CONTROL='carriage', ACTION='action'
    ! 
    ! where:
    !       'nn'  is the fortran unit number to be opened.
    !       '\path\filename' is the path and filename.
    !       'status' is the open status of the file.
    !       'access' is the access method for the file.
    !       'form' indicates the file FORMAT.
    !       'carriage' indicates whether fortran printer carriage control characters are used.
    !       'action' specifies the permission level for the file.
    !
    INTEGER(ipk) :: FileUnit, j, jlast, InputStat, EqualSign
    INTEGER(ipk), PARAMETER :: unit_min = 0_ipk
    INTEGER(ipk), PARAMETER :: unit_max = 99999_ipk
    LOGICAL :: File_exists, Scanning_for_OutputFile, File_is_open
    CHARACTER(LEN=8) :: ClockTime
    CHARACTER(LEN=14) :: Status, Form, Access, Carriage
    CHARACTER(LEN=16) :: Today
    CHARACTER(LEN=72) :: FileName, InputFileName, command_line
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=4*LEN(line)) :: File_Info
    CHARACTER(LEN=1), PARAMETER :: Apostrophe = "'"
    CHARACTER(LEN=1), PARAMETER :: Equals = "="
    CHARACTER(LEN=*), PARAMETER :: Default_input_file = 'frapcon.inp'
    
    ! Open file handle file using command line input or prompt
    IF (COMMAND_ARGUMENT_COUNT() > 0) THEN
        ! If command line argument is provided, this will be read as the input file name (only 1 argument allowed)
       CALL GET_COMMAND_ARGUMENT(1,command_line)
    ELSE
        ! No command line argument is present. Therefore, code will use default input file name
        command_line = Default_input_file
    END IF
    
    ! Check to see if input file exists. If not, have user re-enter file name or terminate program execution
    InputFile_Inquiry: DO
        InputFileName = command_line
        INQUIRE (file = InputFileName, exist = File_exists, opened = File_is_open)
        IF (File_exists) THEN
            EXIT InputFile_Inquiry
        ELSE
            WRITE (0,300) InputFileName
            READ (scrunit,900) command_line
            IF (TO_UPPERCASE(command_line(1:1)) == 'Q') ERROR STOP 'Program execution stopped by command line interface'
        END IF
    END DO InputFile_Inquiry
    
    ! Open input file
    IF (File_is_open) THEN
        ! Rewind the file if it is already open to ensure read starts from the beginning
        REWIND (iunit)
    ELSE
        ! Open the input file
        OPEN (iunit, file = InputFileName, iostat = InputStat, Status = 'old', Form = 'formatted')
        IF (InputStat > 0) CALL File_IO_Error (FileUnit=iunit, InputStat=InputStat, FileName=InputFileName, &
            &                                  Status='old', Form='formatted', Access='sequential')
    ENDIF
    
    ! Get current date and time
    CALL edate (Today)
    CALL clockx (ClockTime)
    
    ! Start by looking for output file information
    Scanning_for_OutputFile = .TRUE.
    
    ! Read the input file to get the I/O file information
    ReadLoop: DO
        
        ! Read each line
        READ (iunit, 100, iostat = InputStat) line
        ! Return if end of file
        IF (InputStat < 0) RETURN
        ! Remove any leading spaces
        line = ADJUSTL(line)
        
        ! Check to see if finish processing all I/O file information (/*) or if the line is to be skipped (*)
        IF (Scanning_for_OutputFile) THEN
            IF (line(1:1) == '*') CYCLE ReadLoop ! Keep looking
            IF (line(1:2) == '/*') THEN
                ! Reached end of file processing and output file information not found. Use defaults.
                WRITE (0,203) ounit
                WRITE (0,500) Today, ClockTime, TRIM(InputFileName), TRIM(FileName)
                REWIND (iunit)
                Scanning_for_OutputFile = .FALSE.
                CYCLE ReadLoop
            END IF
        ELSE
            WRITE (ounit,102) TRIM(line)
            IF (line(1:1) == '*') CYCLE ReadLoop
            IF (line(1:2) == '/*') THEN
                WRITE (ounit,103)
                EXIT ReadLoop
            END IF
        END IF
        
        ! Check for file unit #. This must be after FILE and before the = sign
        IF (TO_UPPERCASE(line(1:4)) == 'FILE') THEN
            
            ! Find the equals sign
            EqualSign = INDEX(line(5:), Equals)
            IF (EqualSign > 0) THEN
                READ (line(5:3+EqualSign),*) FileUnit
            ELSE
                ERROR STOP "Can't find file units. Execution terminated in Subroutine: iofiles"
            END IF
            
            ! Make sure unit is within bounds
            IF (FileUnit < unit_min .OR. FileUnit > unit_max) THEN
                WRITE (0,201) FileUnit
                ERROR STOP 'File units are outside of bounds. Execution terminated in Subroutine: iofiles'
            END IF
            IF (FileUnit /= ounit .AND. Scanning_for_OutputFile) CYCLE ReadLoop
        ELSE
            CYCLE ReadLoop
        END IF

        ! Store first line in File_Info and look for line continuations
        File_Info(1:) = TRIM(line)
        
        File_Data_Search: DO
            ! Check for line continuations (commas), starting after the "=" sign
            jlast = (3 + EqualSign) + INDEX(line(4+EqualSign:),',')
        
            Comma_Search: DO
                j = jlast + INDEX(line(jlast+1:),',')
                IF (j <= jlast) EXIT Comma_Search ! There are no more commas
                jlast = j
            END DO Comma_Search
        
            ! Check to see if all blank spaces after the final comma.
            ! If so, there will be another line to read. If not, all file information has been read
            IF (line(jlast+1:) == ' ') THEN
            
                ! Read the next line and remove any leading spaces
                READ (iunit, 100, iostat = InputStat) line
                ! Return if end of file
                IF (InputStat < 0) RETURN
                ! Rewmove any leading spaces
                line = ADJUSTL(line)
            
                IF (.NOT. Scanning_for_OutputFile) WRITE (ounit,102) TRIM(line)
            
                IF (line(1:2) == '/*') THEN
                    WRITE (0,101)
                    ERROR STOP 'Searching for line continuation but reached end of IO file block. Terminated in Sub: iofiles'
                ELSE
                    File_Info(LEN_TRIM(File_Info)+1:) = TRIM(line)
                END IF
            ELSE
                EXIT File_Data_Search
            END IF
        
        END DO File_Data_Search
        
        ! ----- File Name -----
        FileName = Search_String (File_Info, 'FILE')
        
        ! ---- File Status ----
        IF (INDEX(File_Info, 'STATUS') > 0) THEN
            Status = Search_String (File_Info, 'STATUS')
        ELSE
            Status = 'UNKNOWN'
        END IF
        
        ! If it's a scratch file (originally called nullfile), set status to scratch. Note that this file is no longer used needed.
        IF (TO_UPPERCASE(TRIM(FileName)) == 'NULLFILE') Status = 'SCRATCH'
        
        ! ----- File Form -----
        IF (INDEX(File_Info, 'FORM') > 0) THEN
            Form = Search_String (File_Info, 'FORM')
        ELSE
            Form = 'FORMATTED'
        END IF
        
        ! ---- File Access ----
        IF (INDEX(File_Info,'ACCESS') > 0) THEN
            Access = Search_String (File_Info, 'ACCESS')
        ELSE
            Access = 'SEQUENTIAL'
        END IF
        
        ! Open the file
        IF (Scanning_for_OutputFile .OR. (.NOT. Scanning_for_OutputFile .AND. FileUnit /= ounit)) THEN
            IF (FileName == 'NULLFILE' .OR. Status == 'SCRATCH') THEN
                ! Ensure file is not alredy open from command line read
                INQUIRE (unit = FileUnit, opened = File_is_open)
                IF (File_is_open) CLOSE (unit = FileUnit)
            ENDIF
            OPEN (unit = FileUnit, file = FileName, status = Status, form = Form, access = Access, iostat = InputStat)
            IF (InputStat > 0) CALL File_IO_Error (FileUnit=FileUnit, InputStat=InputStat, FileName=FileName, &
              &                                    Status=Status, Form=Form, Access=Access)
        END IF
        
        ! Write data when scanning for output file
        IF (Scanning_for_OutputFile) THEN
            WRITE (ounit,501)
            WRITE (0,500) Today, ClockTime, TRIM(InputFileName), TRIM(FileName)
            WRITE (ounit,500) Today, ClockTime, TRIM(InputFileName), TRIM(FileName)
            REWIND (iunit)
            Scanning_for_OutputFile = .FALSE.
        END IF
        
        ! Clear the stored data in File_Info and line
        File_Info = ' '
        line = ' '
        
    END DO ReadLoop
    
100 FORMAT (a)
101 FORMAT (' Expected continuation line not found')
102 FORMAT (' ',a)
103 FORMAT (' # End of file processing')
201 FORMAT(/' iofiles: Error on trying to open unit number: ',i10)
202 FORMAT(/' iofiles: Error trying to interpret file handles')
203 FORMAT(/' iofiles: Output file unit:',i2,' specification not', &
      &     ' found'/      ' std output will be routed to console')
300 FORMAT(/' The input file could not be located.', &
      &    /' file:',a &
      &    /' Re-enter file specification or "q" to quit.')
500 FORMAT(' Frapcon 4.0 steady-state fuel rod analysis code', &
      &   /'                ', &
      &   /'    Current date: ',a9,13x,'  Time:     ',a11, &
      &   /'    Input file:   ',a,/,'    Output file:  ',a/)
501 FORMAT(////, &
      & 5x,'ffffffffff  rrrrrrrrr      aaaa     ppppppppp    ccccccccc   oooooooo   nn      nn '/ &
      & 5x,'ffffffffff  rrrrrrrrrr    aaaaaa    pppppppppp  cccccccccc  oooooooooo  nn      nn '/ &
      & 5x,'ff          rr      rr   aa    aa   pp      pp  cc          oo      oo  nnn     nn '/ &
      & 5x,'ff          rr      rr  aa      aa  pp      pp  cc          oo      oo  nnnn    nn '/ &
      & 5x,'ffffffffff  rrrrrrrrrr  aaaaaaaaaa  pppppppppp  cc          oo      oo  nn nn   nn '/ &
      & 5x,'ffffffffff  rrrrrrrrr   aaaaaaaaaa  ppppppppp   cc          oo      oo  nn  nn  nn '/ &
      & 5x,'ff          rr   rr     aa      aa  pp          cc          oo      oo  nn   nn nn '/ &
      & 5x,'ff          rr    rr    aa      aa  pp          cc          oo      oo  nn    nnnn '/ &
      & 5x,'ff          rr     rr   aa      aa  pp          cccccccccc  oooooooooo  nn     nnn '/ &
      & 5x,'ff          rr      rr  aa      aa  pp           ccccccccc   oooooooo   nn      nn '///)
600 FORMAT(/' Enter file name or "q" to quit.', /' >')
900 FORMAT(a)
    
        CONTAINS
        
            FUNCTION Search_String (String, KeyWord) RESULT (Value)
            IMPLICIT NONE
            !>@brief
            !> This function looks in a string for the text associated with the KeyWord value passed in.
            !> It looks for the trailing values within the 1st and 2nd following Apostrophe
            !>@author
            !> Ian Porter, NRC
            !>@date
            !> 5/4/2016
            INTEGER(ipk) :: Starting_Loc, First_Apostrophe, Last_Apostrophe
            CHARACTER(LEN=*), INTENT(IN) :: String, KeyWord
            CHARACTER(LEN=LEN_TRIM(String)) :: String_UCase
            CHARACTER(LEN=LEN(KeyWord)) :: KeyWord_UCase
            CHARACTER(LEN=:), ALLOCATABLE :: Value
            
            ! Make sure all strings are converted to uppercase letters
            Keyword_UCase = TO_UPPERCASE(Keyword)
            String_UCase = TO_UPPERCASE(String)
            
            ! Find the starting location
            Starting_Loc = INDEX(File_Info, Keyword)
            
            ! Search for the bounding apostrophe's
            First_Apostrophe = Starting_Loc + INDEX(String_UCase(Starting_Loc+1:LEN_TRIM(String_UCase)), Apostrophe)
            Last_Apostrophe = First_Apostrophe + INDEX(String_UCase(First_Apostrophe+1:LEN_TRIM(String_UCase)), Apostrophe)
            
            ! Set value to the correct size
            ALLOCATE (Value, source = REPEAT(" ",ncopies = Last_Apostrophe - First_Apostrophe))
            
            ! Assign the value, removing any spaces that may appear before the value
            Value = ADJUSTL(String(First_Apostrophe+1:Last_Apostrophe-1))
            
            END FUNCTION Search_String
            !
            SUBROUTINE File_IO_Error (FileUnit, InputStat, FileName, Status, Form, Access)
            IMPLICIT NONE
            !>@brief
            !> This subroutine aborts the code execution if an error is encountered when trying to open a file
            !>@author
            !> Ian Porter, NRC
            !>@date
            !> 5/4/2016
            
            INTEGER(ipk), INTENT(IN) :: FileUnit, InputStat
            CHARACTER(LEN=*), INTENT(IN) :: FileName, Status, Form, Access
            
            WRITE (0,200) FileUnit, InputStat, TRIM(FileName), Status, Form, Access
            WRITE (ounit,200) FileUnit, InputStat, TRIM(FileName), Status, Form, Access
            ERROR STOP 'Error in opening file. Execution terminated in Subroutine: iofiles.'
            
200         FORMAT(/' iofiles: Error on trying to open unit ',i5,/, &
              &       2x,'Error message number = ',i10,' ***',//, &
              &       2x,'File name: ',a,/, &
              &       2x,'Status   :',a15,'  Form     :',a15,/, &
              &       2x,'Access   :',a15)
            
            END SUBROUTINE File_IO_Error
    !
    END SUBROUTINE iofiles
    !
    !
    !
    SUBROUTINE IOEcho
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : iunit, ounit
    IMPLICIT NONE
    !>@brief
    !> IOEcho reads the input file and echos it to the scratch and output files
    !>@author
    !> coded by Ian Porter, NRC, Nov 2015
    !
    ! Input
    !
    ! iunit   - unit for input file
    ! ounit   - unit for output file
    !
    ! Internal
    !
    ! Line    - line being read from input file
    ! Linecnt - # of input lines
    !
    INTEGER(ipk) :: i, InputStat, Linecnt = 0
    CHARACTER(LEN=200) :: Line
    
    ! Read until reach end of file
    ReadLoop: DO
        READ (iunit, '(A)', IOSTAT = InputStat) Line
        IF (InputStat == 0) THEN
            ! Write line to output file (removing trailing blanks)
            WRITE (ounit,130) TRIM(Line)
130         FORMAT (10x,A)
            Linecnt = Linecnt + 1
        ELSE IF (InputStat < 0) THEN
            ! End of File
            WRITE (ounit,150) Linecnt
150         FORMAT (i9,' input cards'///)
            EXIT ReadLoop
        ELSE
            ! Error
            WRITE (0,*) 'Error in IOEcho reading input file to write to output file'
            WRITE (ounit,*) 'Error in IOEcho reading input file to write to output file'
            ERROR STOP 'Error in IOEcho reading input file to write to output file'
        ENDIF
    END DO ReadLoop
    
    ! Rewind input file to start of necessary information (end of file information block)
    DO i = Linecnt, 0, -1
        BACKSPACE (iunit)
    ENDDO
    !
    END SUBROUTINE IOEcho
    !
    !
    !
    SUBROUTINE Namelist_Read_Error (FileUnit, NameListBlock)
    USE Kinds
    USE variables_frapcon, ONLY : ounit
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
    ERROR STOP 'Namelist read error. See output file for details. Execution Stopped in Subroutine: Namelist_Read_Error.'
    !
    END SUBROUTINE Namelist_Read_Error
    !
END MODULE FileIO

