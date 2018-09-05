MODULE RunProperties
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to determine run time 
    !> and data and write out page headers.
    !> Subroutines include clockx, edate, pghead, propid
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 06/23/2015
    !
    CONTAINS
    !
    SUBROUTINE clockx(word)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Subroutine clockx returns local time in word in Character type in form hh:mm:ss.
    CHARACTER(LEN=8), INTENT(OUT) :: word
    CHARACTER(LEN=24) :: string
    !
    CALL fdate (string)
    word = string(12:19)
    !
    END SUBROUTINE clockx
    !
    !
    !
    SUBROUTINE edate (chdate)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> edate returns date in 9-Character string as dd-mmm-yy, where mmm is an abbreviation for the month.
    !>@author
    !> J. E. Tolli, EG&G Idaho, Inc.
    !> @date
    !> 10/91
    ! Output
    ! chdate - date (output)
    ! Note: This routine is a modification of the Environmental library date Subroutine.
    !
    CHARACTER(LEN=9), INTENT(OUT) :: chdate
    CHARACTER(LEN=24) :: string
    !
    CALL fdate (string)
    chdate = string(23:24) // '-' // string(5:7) // '-' // string(9:10)
    !
    END SUBROUTINE edate
    !
    !
    !
    FUNCTION TimeStamp (ReqInfo) RESULT (ReqData)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine returns the time or date
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/10/2015
    !
    ! Input
    !
    ! ReqInfo    - Requested information from CPU
    !       Time = Computer time in format HH:MM:SS (Hours:Minutes:Seconds)
    !       Date = Computer date in format MON DY, YEAR (Month, Day, year)
    !
    ! Output
    !
    ! ReqData    - Requested Computer data (time or date)
    !
    CHARACTER(LEN=8) :: Date
    CHARACTER(LEN=10) :: Time
    CHARACTER(LEN=5) :: TimeZone
    CHARACTER(LEN=*), INTENT(IN) :: ReqInfo
    CHARACTER(LEN=12) :: ReqData
    INTEGER(ipk), DIMENSION(8) :: Values
    CHARACTER(LEN=3), DIMENSION(12), PARAMETER :: Month = (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
                                                           & 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
    CALL DATE_AND_TIME(DATE=Date, Time=Time, Zone=TimeZone, Values=Values)
    SELECT CASE (ReqInfo)
    CASE ('Date')
        ReqData = Month(Values(2)) // " " // CHAR(Values(3)) // ", " // CHAR(Values(1))
    CASE ('Time')
        ReqData = CHAR(Values(5)) // ":" // CHAR(Values(6)) // ":" // CHAR(Values(7))
    END SELECT
    !
    END FUNCTION TimeStamp
    !
    !
    !
    SUBROUTINE pghead
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : codeid, buildid, title, ounit
    IMPLICIT NONE
    !>@brief
    !> This Subroutine prints out the program version identification at the top of each output page
    !
    INTEGER(ipk) :: npage=0, i
    CHARACTER(LEN=9) :: day
    !
    npage = npage + 1
    CALL edate(day)
    WRITE (ounit,120)
    WRITE (ounit,130)
    WRITE (ounit,140) codeid
    WRITE (ounit,150) buildid
    WRITE (ounit,190) title
    WRITE (ounit,180) day, npage
    WRITE (ounit,130)
120 FORMAT(/)
130 FORMAT(28x,77('x'))
140 FORMAT(28x,'x',5x,'** ',1a60,' **',4x,'x')
150 FORMAT(28x,'x',26x,1a24,25x,'x')
180 FORMAT(28x,'x',17x,' run date:  ',a9,11x,'page ',i4,17x,'x')
190 FORMAT(28x,'x',2x,a72,1x,'x')
    !
    END SUBROUTINE pghead
    !
END MODULE RunProperties


