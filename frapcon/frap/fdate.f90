    subroutine fdate(str)
    USE Kinds
    implicit none
    CHARACTER(LEN=8) :: Date
    CHARACTER(LEN=10) :: Time
    CHARACTER(LEN=5) :: TimeZone
    INTEGER(ipk), DIMENSION(8) :: Values
    CHARACTER(LEN=3), DIMENSION(12), PARAMETER :: Month = (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
                                                           & 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
    integer   :: d,m,y, weekday
    !
    CHARACTER(LEN=24), INTENT(OUT) :: str
    !
    CALL DATE_AND_TIME(DATE=Date, Time=Time, Zone=TimeZone, Values=Values)
    !
    str(1:24) = ""
    str(5:7)  = Month(Values(2))        ! mm
    write(str(9:10),"(i2)") Values(3)   ! dd
    write(str(12:19),"(i2,':',i2,':',i2)") Values(5), Values(6),Values(7) ! hh:mm:ss
    write(str(21:24),"(i4)") Values(1)  ! yyyy
    ! str(1:3)                          ! weekday
    ! Kim Larsson Formula to get weekday
    d = Values(3)
    m = Values(2)
    y = Values(1)
    if(m==1 .or. m==2) then 
       m = m + 12  
       y = y - 1  
    end if
    weekday = mod((d+2*m+3*(m+1)/5+y+y/4-y/100+y/400),7);
    select case(weekday)
       case(0) 
           str(1:3) = "Mon"
       case(1) 
           str(1:3) = "Tue"
       case(2) 
           str(1:3) = "Wed"
       case(3)
           str(1:3) = "Thu"
       case(4) 
           str(1:3) = "Fri"
       case(5) 
           str(1:3) = "Sat"
       case(6) 
           str(1:3) = "Sun"
       case default; str(1:3) = 'Err'
   end select   
   !
   return
   end subroutine fdate
