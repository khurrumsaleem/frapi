MODULE setup_fraptran
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to start and run the problem_fraptran.
    !> Subroutines include main, frap, and iofiles
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    !
    PRIVATE
    PUBLIC :: Main, Input_Echo
    !
    CONTAINS
    !
    !
    SUBROUTINE Main
    USE Kinds_fraptran
    USE variables_fraptran, ONLY : coupled, iunit, ounit, plotunit, frtrunit, h2ounit, fcunit, dakotaunit, nrestart, ncards, &
      &                   title, codeid, defsize, pre_na, pre_nr, Allocate_Variables
    USE frapc_fraptran
    USE Dyna_h_fraptran
    USE collct_h_fraptran
    USE resti_h_fraptran
    USE excb_h_fraptran
    USE scalr_h_fraptran
    USE FissionGasRelease_h_fraptran
    USE NCGases_fraptran
    USE FEA_Setup_fraptran
    USE ErrorMsg_fraptran, ONLY : namelist_read_error
    USE Initial_Read_fraptran, ONLY : preread
    IMPLICIT NONE
    !> @brief
    !> Main subroutine in FrapTran computer code. This subroutine controls the reading and processing of the namelist $begin
    !> FrapTran calculates the transient performance of a LWR fuel rod during operational transients and hypothetical accidents
    !> @author
    !> Modified by Ian Porter, NRC, April 2014 to clean coding and convert to .f90
    !
    ! Input
    !
    ! NRestart         - 1 = FrapTran restart file written
    ! ncards           - 1 = read FrapTran input cards
    ! ProblemStartTime - Problem start time (s)
    ! ProblemEndTime   - Problem end time (s)
    ! IndexFC2Print    - 1 = print detailed Fracas-2 stress-strain calculations
    ! IndexGrainBndSep - 1 = grain boundary separation effects are considered and gbse input is required
    !
    INTEGER(ipk) :: IndexGrainBndSep, k, i, InputStat = 0
    INTEGER(ipk) :: radial, axial, ntimepairs
    INTEGER(ipk), PARAMETER :: unitmax = 99999_ipk
    REAL(r8k) :: ProblemStartTime = 0.0_r8k
    REAL(r8k) :: ProblemEndTime = 0.0_r8k
    LOGICAL :: lopen = .FALSE.
    CHARACTER(LEN=100) :: InputFileName
    
    ! Define $begin
    NAMELIST / begin / NRestart, ncards, IndexFC2Print, IndexGrainBndSep, ProblemEndTime, ProblemStartTime, gbse, defsize
    
    ! Initialize variables
    IF (.NOT. coupled) THEN
        ! Set arbitrary values to allocate the frapc arrays that are used for link with T/H Code
        CALL Preread (InputFileName)
        radial = pre_nr
        axial = pre_na
        ntimepairs = 1  !# of time history pairs from T/H Code
        IF (first_call) THEN
            CALL Allocate_Variables (ntimepairs, radial, axial)
            CALL Allocate_Gas
        END IF
    END IF
    
    ! Only set these variables if it is the first time calling this subroutine
    IF (first_pass) THEN
        n1 = 1
        n2 = 1
        n3 = 2
        n4 = 1
        ldir = 3
        ntapou = 2
        npbh   = 50
        nhbh   = 25
        ngbh   = 100
        nhinta = 50
        nhupta = 50
        lcoold = 1952
        lflect = 630
        lblona = 73
        lprntb = 37
        lexcb  = 110
        lresr1 = 136
        lhtcb  = 12
        ndap1  = 21
        naxn   = 1
        ntco   = 4
        ntplot = 1
        kdbug  = 0
        ndtmax = 40
        ncards = 1
        ngaspr = 0
        ngastmp = 0
        nodpln = 6
        ! Set by IP
        idx2 = 1
        tmax = 0.0_r8k
        ! Modified by IP.  There were duplicated variables causing problems in this Subroutine
        IF (ncool2 == 1) coupled = .TRUE.
        
        ! Set defaults
        ! These variables are arbitrarily set to 1 until DD is removed
        ldialb = 1
        lphypr = 1
        ! End of arbitrarily set variables
        lresi2 = 68
        lresi3 = 18
        lmatpc = 3
        lsclr1 = 29
        lsclr2 = 57
        lsclr3 = 25
        lcolct = 671
        TranSwell = 0
        lthyd = 1516
        MaximumIterations = 120
        IndexFC2Print = 0
        IndexGrainBndSep = 0
        NRestart = 0
        TranFuelSwell = 1.0_r8k
        coldw  = 0.0_r8k
        ! relfraca = 0.0_r8k
        FuelGasSwell = 0.0_r8k
        ! set default values for modeling fission gas release due to fuel grain disintegration.
        gbse(1) = 0.0_r8k
        gbse(2) = 5000.0_r8k
        gbse(3) = 1.0_r8k
        gbse(4) = 1.0_r8k
        gbse(5) = 0.0_r8k
        
        ! Initialize FE model (Only useable when not coupled)
        IF (.NOT. coupled) THEN
            CALL init()
            CALL default_values()
        ENDIF
        
        ! Open file handler
        CALL iofiles (InputFileName)
        
        ! Read control variables
        READ(iunit,'(A)') Title
        
        ! Read $begin
        READ (unit = iunit, nml = begin, IOSTAT = InputStat)
        IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'begin')
        
        ! Fuel disintegration modeling
        IF (IndexGrainBndSep == 1) THEN
            WRITE(ounit,103)
103         FORMAT(/,' Modeling of fuel grain disintegration specified')
            WRITE(ounit,105) gbse(1)
105         FORMAT(' Kg-moles/m of fission gas to be released when fuel grains disintegrate = ',e11.4)
            WRITE(ounit,107) gbse(2)
107         FORMAT(' Surface temperature of fuel that triggers grain boundary separation    = ',e11.4,' K')
            WRITE(ounit,109) gbse(3)
109         FORMAT(' Period of time over which gases released from disintegrated grains     = ',e11.4,' s')
            WRITE(ounit,111) gbse(4)
111         FORMAT(' Time after disintegration before pressure equilibrium with plenum      = ',e11.4,' s')
            WRITE(ounit,113) gbse(5)
113         FORMAT(' Initial fraction of fuel volume with fission gases that are released with grain disintegration =',e11.4) 
        END IF
        
        ! Write out page header
        WRITE(ounit,150) TRIM(codeid)
 150    FORMAT(1x,a)
        ! set control variables in frapc common block
        ndtadv = 0
        nrest2 = 0
        ncool2 = 1
        ! Modified to allow coupling of T/H code. IP
        ncool2 = 7
        ! Switched from ncool2 = 0 to ncoolw = 1. IP
        IF (ncards == 1) WRITE(ounit,61) ProblemStartTime, ProblemEndTime
61      FORMAT(//' *** Start time = ',es12.5,' s, End time = ',es12.5,' s  ***' )
        IF (ncards == 0) WRITE(ounit,62) ProblemEndTime
62      FORMAT(///' *** FrapTran execution started. This run is a continuation of a previous run. New end time = ',e12.5,' s ***')
        IF (NRestart == 1) WRITE(ounit,66)
66      FORMAT(///' *** FrapTran restart tape to be written  *** ' )
        IF (ncards == 1) WRITE(ounit,60)
60      FORMAT(///' *** FrapTran execution started. This run begins from scratch. Input cards to be read. ***' )
        WRITE(ounit,65)
65      FORMAT(//)        
    ELSE ! It is not the first time fraptran has been called.
        IF (coupled) THEN 
            ! Reset the problem start and end times based on the time supplied by T/H Code.
            ProblemStartTime = t12
            ProblemEndTime = t22
            ncards = ncard2
        ENDIF
    ENDIF
    !
    IF (.NOT. coupled) THEN
        ncard2 = ncards
        t12 = ProblemStartTime
        t22 = ProblemEndTime
        t0 = ProblemStartTime
        tmax = ProblemEndTime
        nrest2 = NRestart
    ELSE
        ! Only let ncard2 equal the T/H Code value after the first pass has been made.
        IF (t12 > 0.0_r8k) ncards = ncard2
        ! The value for t12 and t22 are set by the T/H Code through the frapc common block
        t0 = t12
        tmax = MAXVAL(tEnd)
        nrest2 = NRestart
    ENDIF
    
    ! Echo input file to output file
    CALL Input_Echo
    
    ! Start code execution
    CALL frap
    
    ! Set variable saying it's no longer the first call to FRAPTRAN
    first_pass = .FALSE.
    !
    END SUBROUTINE Main
    !
    !
    !
    SUBROUTINE frap
    USE Kinds_fraptran
    USE frapc_fraptran
    USE variables_fraptran
    USE sth2x_fraptran, ONLY : sth2xi
    USE Initialization_fraptran, ONLY : initia
    USE Read_Input_fraptran
    USE timestep_fraptran, ONLY : crank6
    USE ErrorMsg_fraptran, ONLY :errori
    IMPLICIT NONE
    !> @brief
    !> Subroutine repeatedly calls the main FrapTran program
    !
    ! Input
    !
    ! ncard2 - Control switch on read of input cards
    !      1 = Read input cards this Call (first Call for given rod)
    !      0 = Do not read cards
    ! nrest2 - Control switch on storage place of FrapTran restart Data set
    !      0 = lcm storage
    ! ncool2 - On/off switch for link with t/h code
    !      0 = Off
    !      1 = On
    ! ndtadv - Switch to advance in time or perform calculations only at time t12.
    !      0 = Advance in time
    !      1 = Only perform calculations at time t12
    ! t12    = Start time for FrapTran calculations (sec)
    ! t22    = End time for FrapTran calculations   (sec)
    !
    INTEGER(ipk) :: lmax, lrest, l1, i, InputStat
    !
    t1 = t12
    t2 = t22
    ndtad = ndtadv
    
    ! Check to see if restart tape to be read
    IF (ncool /= 7 .AND. ncards /= 1) THEN
        ! This works when nrepr is set to 2. It will read the last timestep value.
        READ(unit = frtrunit, iostat = InputStat) afrap(1), afrap(2), afrap(3)
    
        ! Abort on error
        IF (InputStat /= 0) THEN
            WRITE (ounit, 101)
101         FORMAT ('Error reading length of restart array. Execution terminated in Subroutine: frap')
            ERROR STOP 'Error reading length of restart array. Execution terminated in Subroutine: frap'
        END IF
    
        ! t12  = start time for this job  (s)
        IF (.NOT. coupled) THEN
            t12 = afrap(1)
            t1 = t12
        ENDIF
        ! lafrap = total length of afrap array
        lafrap = afrap(2)
        ! lafrap = 1
        lmax = lafrap
        
        ! Write restart array information to output file
        WRITE(ounit,905) afrap(1), afrap(2), lafrap
905     FORMAT(' in FrapTran, afrap1 =  ',e13.6,' afrp2 =  ',e13.6,' lafrap =  ',i8)
    
        ! Read the restart array
        READ(unit = frtrunit, iostat = InputStat) (afrap(lrest), lrest = 4,lmax)
    
        ! Abort on error
        IF (InputStat /= 0) THEN
            WRITE (ounit, 110)
110         FORMAT ('Error reading restart array. Execution terminated in Subroutine: frap')
            ERROR STOP 'Error reading restart array. Execution terminated in Subroutine: frap'
        END IF
    
        ! Initialize water property tables (This is done in cominp if no restart)
        l1 = 17000
        !Get water properties
        CALL sth2xi (aasth, l1)
        IF (l1 < 0) Call errori (2,0)
        
    END IF
    
    IF (Coupled) THEN
        DO i = 1, SIZE(tEnd)
            tmax = MAX(tmax,tEnd(i))
        ENDDO
    ELSE
        tmax = t2
    ENDIF
    t0 = t1
    tc1 = t1
    tc2 = t2
    
    ! If ncards = 0 , cold startup
    ! Read input data and initialize variables
    IF (ncards == 1 .AND. first_pass) THEN
        ! read input cards
        CALL cardin
        ! initialize variables
        CALL initia
    END IF
    
    ! Advance solution in time
    CALL crank6
    
    ! Write out regulatory summary at end of code execution
    WRITE(ounit,200) maxfueltemp, maxcladtemp, maxenthalpy, maxgaspressure
200 FORMAT(//,20x,'                 REGULATORY OUTPUT SUMMARY',/, &
      &       20x,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',/, &
      &       20x,'x                                                            x',/, &
      &       20x,'x   Maximum Fuel Temperature           = ',f10.2, ' deg.F    x',/, &
      &       20x,'x   Maximum Cladding Temperature       = ',f10.2, ' deg.F    x',/, &
      &       20x,'x   Maximum Rad. Ave. Fuel Enthalpy    = ',f10.2, ' BTU/lb   x',/, &
      &       20x,'x   Maximum Plenum Pressure            = ',f10.2, ' psi      x',/, &
      &       20x,'x                                                            x',/, &
      &       20x,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
    
    END SUBROUTINE frap
    !
    !
    !
    SUBROUTINE iofiles (InputFileName)
    USE Kinds_fraptran
    USE conversions_fraptran
    USE functions_fraptran
    USE variables_fraptran, ONLY : iunit, ounit, scrunit
    USE RunProperties_fraptran, ONLY : edate, clockx
    IMPLICIT NONE
    !> @brief
    !> Subroutine iofiles reads the command line to get the input file name, opens the input file
    !> and opens any associated files (i.e. output file, plot file)
    !> @author
    !> iofiles developed by k. r. jones. Version 2.01 - dec
    !> Re-written by Ian Porter, NRC, May 2016
    !
    ! Input
    !
    ! InputFileName - Input file read by subroutine pre-read
    ! iunit         - Input file unit (filename obtained from command line or prompt)
    ! ounit         - Output file unit
    ! scrunit       - Scratch file unit
    !
    ! Note:
    !
    ! Comments can be entered by placing a '*' in column 1 and will be echoed to the output file.
    ! Commens can be made on any line using a '!'
    ! An example file specification FORMAT is shown below.  Only the unit number and file name 
    ! are required input. A line ending in a comma  is continued on the following line.
    !
    ! FILEnn='\path\filename',  STATUS='status' , ACCESS='access',
    !         FORM='form', ACTION='action'
    ! 
    ! where:
    !       'nn'  is the fortran unit number to be opened.
    !       '\path\filename' is the path and filename.
    !       'status' is the open status of the file.
    !       'access' is the access method for the file.
    !       'form' indicates the file FORMAT.
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
    
    ! Check to see if input file exists. If not, have user re-enter file name or terminate program execution
    InputFile_Inquiry: DO
        
        INQUIRE (file = InputFileName, exist = File_exists, opened = File_is_open)
        IF (File_exists) THEN
            EXIT InputFile_Inquiry
        ELSE
            WRITE (0,300) InputFileName
            READ (scrunit,'(a)') command_line
            IF (TO_UPPERCASE(command_line(1:1)) == 'Q') THEN
                ERROR STOP 'Program execution stopped by command line interface'
            ELSE
                InputFileName = command_line
            END IF
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
                ! Reached end of file processing and output file information not found. Use defaults_fraptran.
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
!            OPEN (unit = FileUnit, file = FileName, status = Status, form = Form, access = Access, iostat = InputStat)
!
! ****      Modification by G. Longoni on 07/2016
! ****      Need to modify the previous OPEN statement to work with GNU Fortran
!
            OPEN (unit = FileUnit, file = FileName, iostat = InputStat)
!
!
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
201 FORMAT (/' iofiles: Error on trying to open unit number: ',i10)
202 FORMAT (/' iofiles: Error trying to interpret file handler')
203 FORMAT (/' iofiles: Output file unit:',i2,' specification not found,', &
      &     /' Standard output will be routed to console')
300 FORMAT (/' The input file could not be located.', &
      &     /' File: ',a &
      &     /' Re-enter file specification or "q" to quit.')
500 FORMAT (' FRAPTRAN-2.0 Transient Fuel Rod Analysis Code',//, &
      &     '    Current date: ',a9,13x,'  Time:     ',a11,/, &
      &     '    Input file:   ',a,/, &
      &     '    Output file:  ',a,/)
501 FORMAT(////, &
      & 10x,'FFFFFFFFFF  RRRRRRRRR      AAAA     PPPPPPPPP   TTTTTTTTTT  RRRRRRRRR      AAAA     NNN     NN',/, &
      & 10x,'FFFFFFFFFF  RRRRRRRRRR    AAAAAA    PPPPPPPPPP  TTTTTTTTTT  RRRRRRRRRR    AAAAAA    NNN     NN',/, &
      & 10x,'FF          RR      RR   AA    AA   PP      PP      TT      RR      RR   AA    AA   NNNN    NN',/, &
      & 10x,'FF          RR      RR  AA      AA  PP      PP      TT      RR      RR  AA      AA  NN NN   NN',/, &
      & 10x,'FFFFFFFFFF  RRRRRRRRRR  AAAAAAAAAA  PPPPPPPPPP      TT      RRRRRRRRRR  AAAAAAAAAA  NN  NN  NN',/, &
      & 10x,'FFFFFFFFFF  RRRRRRRRR   AAAAAAAAAA  PPPPPPPPP       TT      RRRRRRRRR   AAAAAAAAAA  NN   NN NN',/, &
      & 10x,'FF          RR   RR     AA      AA  PP              TT      RR   RR     AA      AA  NN    NNNN',/, &
      & 10x,'FF          RR    RR    AA      AA  PP              TT      RR    RR    AA      AA  NN     NNN',/, &
      & 10x,'FF          RR     RR   AA      AA  PP              TT      RR     RR   AA      AA  NN     NNN',/, &
      & 10x,'FF          RR      RR  AA      AA  PP              TT      RR      RR  AA      AA  NN     NNN'/)
    
        CONTAINS
        
            PURE FUNCTION Search_String (String, KeyWord) RESULT (Value)
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
            
200         FORMAT(/  ' iofiles: Error on trying to open unit ',i5,/, &
              &    2x,'Error message number = ',i10,' ***',//, &
              &    2x,'File name: ',a,/, &
              &    2x,'Status   :',a15,'  Form     :',a15,/, &
              &    2x,'Access   :',a15)
            
            END SUBROUTINE File_IO_Error
    !
    END SUBROUTINE iofiles
    !
    !
    !
    SUBROUTINE Input_Echo
    USE Kinds_fraptran
    USE conversions_fraptran
    USE variables_fraptran, ONLY : iunit, ounit
    IMPLICIT NONE
    !>@brief
    !> Input_Echo reads the input file and echos it to the scratch and output files
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
150         FORMAT (i9,' Input cards'//)
            EXIT ReadLoop
        ELSE
            ! Error
            WRITE (0,160)
            WRITE (ounit,160)
160         FORMAT ('Error in reading input file to write to output file. Execution terminated in Subroutine: Input_Echo')
            ERROR STOP 'Error in reading input file to write to output file. Execution terminated in Subroutine: Input_Echo'
        END IF
    END DO ReadLoop
    
    ! Rewind input file to start of necessary information (end of file information block)
    DO i = Linecnt, 0, -1
        BACKSPACE (iunit)
    END DO
    !
    END SUBROUTINE Input_Echo
    !
END MODULE setup_fraptran














