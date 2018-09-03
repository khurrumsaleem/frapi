MODULE Initial_Read
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Module Initial_Read performs an initial reading of the input file to determine array sizes
    !> for # of axial nodes, radial nodes and timesteps
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 5/5/2016
    
    PRIVATE
    PUBLIC :: preread
    
    CONTAINS
    
    SUBROUTINE preread (iofile)
    USE Kinds
    USE frapc, ONLY : coupled, FrapTranFN
    USE variables, ONLY : pre_na, pre_nr, pre_nt, iunit, ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine preread does an initial opening of the input file in order to determine array sizes
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 5/5/2016
    INTEGER(ipk) :: naxn, nfmesh, ncmesh, defsize, InputStat = 0
    LOGICAL :: lexist, lopen
    CHARACTER(LEN=100) :: cmdline
    CHARACTER(LEN=100), INTENT(OUT) :: iofile
    CHARACTER(LEN=12), PARAMETER :: Default_iofile = 'fraptran.inp'
    
    ! Open file handle file using command line input or prompt
    IF (coupled) THEN
        iofile = FrapTranFN
    ELSE
        IF (COMMAND_ARGUMENT_COUNT() > 0) THEN
            ! If command line argument is provided, this will be read as the input file name (only 1 argument allowed)
           CALL GET_COMMAND_ARGUMENT(1,cmdline)
        ELSE
            ! No command line argument is present. Therefore, code will use default input file name
            cmdline = Default_iofile
        END IF
    ENDIF
    
    ! Get input file name
    IO_Inquiry: DO
        iofile = cmdline
        INQUIRE (file = iofile, exist = lexist, opened = lopen)
        IF (lexist) THEN
            EXIT IO_Inquiry
        ELSE
            WRITE (0,300) TRIM(iofile)
300         FORMAT(/' the input file could not be located.',/' file:',a,/' reenter file specification or "q" to quit.')
            READ (5,'(A)') cmdline
            IF ((cmdline == 'q')  .OR. (cmdline == 'Q')) STOP
        END IF
    END DO IO_Inquiry
    
    ! Save the input file name so that the command line interface does not have to be executed again asking for the input file name
    iofile = cmdline
    
    ! Open FRAPTRAN input file
    IF (lopen) THEN
        ! File is already open (may happen if linked with T/H code and reading same input multiple times)
        REWIND(iunit)
    ELSE
        ! Open input file
        OPEN (iunit, file = iofile, IOSTAT=InputStat, status = 'old', form = 'formatted', action = 'read')
        IF (InputStat /= 0) THEN
            WRITE (ounit,305)
305         FORMAT ('Error opening input file for pre-read. Execution terminated in Subroutine: preread')
            ERROR STOP 'Error opening input file for pre-read. Execution terminated in Subroutine: preread'
        END IF
    END IF
    
    ! Set an arbitrary default size of arrays to 400
    defsize = 400
    
    ! Perform pre-reads to get array sizes
    CALL begin_pre (iunit, defsize)
    CALL numinp_pre (iunit, naxn, nfmesh, ncmesh, defsize)
    
    ! Set the # of axial, radial and timesteps to allocate the code's values on.
    pre_na = naxn + 25
    pre_nr = nfmesh + ncmesh + 1
    pre_nt = defsize
    
    ! Close the input file
    CLOSE (iunit)
    
    END SUBROUTINE preread
    !
    !
    !
    SUBROUTINE begin_pre (input_unit, defsize)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine begin_pre does an initial reading of the $begin block to get the user-supplied value for defsize
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 5/5/2016
    INTEGER(ipk) :: InputStat = 0
    INTEGER(ipk) :: input_unit
    INTEGER(ipk) :: NRestart, ncards, IndexFC2Print, IndexGrainBndSep, defsize
    REAL(r8k) :: ProblemEndTime, ProblemStartTime, gbse
    
    ! Define $begin
    NAMELIST / begin / NRestart, ncards, IndexFC2Print, IndexGrainBndSep, ProblemEndTime, ProblemStartTime, gbse, defsize
    
    ! Read $begin
    READ (unit = input_unit, nml = begin, IOSTAT = InputStat)
    
    END SUBROUTINE begin_pre
    !
    !
    !
    SUBROUTINE numinp_pre (iunit, naxn, nfmesh, ncmesh, defsize)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine numinp_pre does an initial reading of the $solution block to get the array sizes for zelev, fmesh and cmesh
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 5/5/2016
    INTEGER(ipk) ::soltyp, maxit, noiter, naxn, nfmesh, ncmesh, nce, iunit, defsize, InputStat = 0
    REAL(r8k) :: dtss, prsacc, tmpac1, epsht1
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dtmaxa, zelev, fmesh, cmesh
    
    ! Define $solution
    NAMELIST / solution / dtmaxa, dtss, prsacc, tmpac1, soltyp, maxit, noiter, epsht1, &
      &                   naxn, zelev, nfmesh, ncmesh, fmesh, cmesh, nce
    
    !
    ALLOCATE (dtmaxa(1:defsize))
    ALLOCATE (zelev(1:defsize))
    ALLOCATE (fmesh(1:defsize))
    ALLOCATE (cmesh(1:defsize))
    
    ! Set default values
    maxit = 200
    dtss = 1.0e5_r8k
    prsacc = 0.005_r8k
    tmpac1 = 0.005_r8k
    soltyp = 0
    noiter = 200
    epsht1 = 0.001_r8k
    naxn = 0
    nfmesh = 0
    ncmesh = 0
    nce = 5
    dtmaxa(:) = 0.0_r8k
    zelev(:) = 0.0_r8k
    fmesh(:) = 0.0_r8k
    cmesh(:) = 0.0_r8k
    
    ! Read $solution
    READ (iunit, solution, IOSTAT=InputStat)
    
    IF (naxn == 0) naxn = countarray(zelev,defsize)
    IF (nfmesh == 0) nfmesh = countarray(fmesh,defsize)
    IF (ncmesh == 0) ncmesh = countarray(cmesh,defsize)
    
    END SUBROUTINE numinp_pre
    !
    !
    !
    INTEGER(ipk) FUNCTION countarray(array, iarray)
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Function countarray counts the number of values in an array that are not equal to 0.0
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 5/5/2016
    INTEGER(ipk) :: isize, i, iarray
    REAL(r8k), DIMENSION(iarray), INTENT(IN) ::  array
    
    isize = 0
    i = 2
    
    DO WHILE (isize == 0)
        IF(array(i) /= 0.0_r8k) THEN
            i = i + 1
        ELSE
            isize = i - 1
        END IF
    END DO
    
    ! Set the # of found array values
    countarray = isize
    
    END FUNCTION countarray
    
END MODULE Initial_Read
