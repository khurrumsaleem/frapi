MODULE DatingData_frapcon
    !> Module Dating calculates cladding creep of spent fuel_frapcon
    !> @author
    !> Modified by I. Porter & P. Raynaud, NRC & K. Geelhood, PNNL
    !> @date
    !> March 2014
    USE variables_frapcon, ONLY : na, ounit, im, nunits, ddunit
    USE Kinds_frapcon
    USE conversions_frapcon
    !
    ! Input
    !
    !
    ! Output
    !
    !
    ! Internal
    !
    ! MF      - The method flag (used only on first call, unless index = -1). Allowed values are 10, 11, 12, 13,
    !           20, 21, 22, 23. MF is an integer with two decimal digits, METH and MITER (mf = 10*METH + MITER). (mf
    !           can be thought of as the ordered pair (METH,MITER).)
    ! METH    - The basic method indicator.
    !       1 = indicates variable-step size, variable order adams method, suitable for non stiff problems.
    !       2 = indicates variable-step size, variable order backward differentiation method, suitable for stiff problems.
    ! MITER   - The method of iterative correction (nonlinear system solution).
    !       0 = Functional iteration (no partial derivatives needed).
    !       1 = Chord or semi-stationary newton method with closed form (exact) jacobian,
    !           which is computed in the user supplied subroutine pederv(n,t,y,pd,n0) described below.
    !       2 = Chord or semi-stationary newton method with an internally computed finite difference approx. to the jacobian.
    !       3 = a chord or semi-stationary newton method with an internally computed diagonal matrix approx. to the jacobian,
    !           based on a directional derivative.
    !
    ! IMPORTANT NOTE: MF was equal to 22 for DATING, so MITER = 2 and METH = 2 were made into parameters,
    !                 and other parameters were set accordingly: MAXDER, LMAX
    !
    ! INTERP - Interpolates to give output values at t = tout by using data in the y array.
    ! STIFF  - The core integration subroutine, which integrates over a single step and does associated error control.
    ! COSET  - Sets coefficients for use in tstep_frapcon.
    ! PSET   - Computes and processes the jacobian matrix, j = df/dy.
    ! DEC    - Performs the lu decomposition of a matrix.
    ! SOL    - Solves a linear system a*x = b, after dec has been called for the matrix a.
    ! DIFFUN - Computes the function YDOT = f(Y,T).
    ! YDOT   - the right hand side of the ordinary differential equation system, where Y and YDOT are vectors of length NEQ.
    !
    ! Integers
    ! Creep model choice (BE vs. conservative creep and Monkman-Grant)
    INTEGER(ipk), TARGET :: idatingcreep
    ! Type of solution (1: helium cooling, 2: nitrogen cooling, 3: specified temperature, 4: specified temperature and stress
    INTEGER(ipk), TARGET :: ncreephist
    ! # of creep output steps
    INTEGER(ipk), TARGET :: ncreepstep
    ! # of input pairs for ncreephist = 3 or 4
    INTEGER(ipk), TARGET :: ncreeptab
    ! See description in DRIVE
    INTEGER(ipk), PRIVATE :: DATING_INDEX
    ! Only used in STIFF, but value must be retained from one call of STIFF to the next
    INTEGER(ipk), PRIVATE :: IDOUB
    ! Changes values based on the path taken by the STIFF solver to reach the solution.
    ! Value must be retained from one call of STIFF to the next
    INTEGER(ipk), PRIVATE :: IWEVAL
    ! An integer used on input and output.
    ! On input, it has the following values and meanings:
    ! 0 = Perform the first step, > 0 = take a new step continuing from the last, < 0 = take the next step with a new value of H
    ! On exit, JSTART is set to NQ, the current order of the method
    INTEGER(ipk), PRIVATE :: JSTART
    ! A completion code with the following meanings:
    ! 0  = Time step successful
    ! -1 = Error could not be achieved with abs(H) = HMIN
    ! -2 = Error is smaller than can be handled for this problem
    ! -3 = Corrector convergence could not be achieved for abs(H) = HMIN.
    ! On a return with KFLAG negative, the values of T and the Y array are as of the beginning of the last step
    ! and H is the last step size attempted
    INTEGER(ipk), PRIVATE :: KFLAG
    ! Order Counter
    INTEGER(ipk), PRIVATE :: L
    ! Creep rate & rupture mechanism flag
    INTEGER(ipk), PRIVATE :: MFLAG
    ! Step counter
    INTEGER(ipk), PRIVATE :: nstep_driver
    ! Athermal creep assumed during calculation flag
    INTEGER(ipk), PRIVATE :: NFLAG
    ! Yield stress exceeded flag
    INTEGER(ipk), PRIVATE :: NFLAGS
    ! Alpha/Beta transition temperature exceeded flag
    INTEGER(ipk), PRIVATE :: NFLAGT
    ! Current order of the method. Also declared in STIFF and COSET because is an argument for these procedures
    INTEGER(ipk), PRIVATE :: NQ
    ! STIFF local time step counter, but value must be retained from one call of STIFF to the next
    INTEGER(ipk), PRIVATE :: NSTEPJ
    ! GEAR method parameter, Also declared in PSET because is an argument for this procedure
    INTEGER(ipk), PARAMETER, PRIVATE :: METH = 2
    ! GEAR method parameter, Also declared in PSET because is an argument for this procedure
    INTEGER(ipk), PARAMETER, PRIVATE :: MITER = 2
    ! These store the value for the array dimensions.
    ! METH==2, so MAXDER = 5
    INTEGER(ipk), PARAMETER, PRIVATE :: MAXDER = 5
    ! LMAX = MAXDER + 1
    INTEGER(ipk), PARAMETER, PRIVATE :: LMAX = MAXDER + 1
    ! The number of differential equations. NEQ must never be increased during a given problem.
    INTEGER(ipk), PARAMETER, PRIVATE :: NEQ = 3
    ! Reals
    ! Time spent in pool after reactor discharge (years)
    REAL(r8k), TARGET :: creeppooltime
    ! Time duration of creep calculation (years)
    REAL(r8k), TARGET :: creeptime
    ! Timestep size to be used in creep calculation (s)
    REAL(r8k), TARGET :: datingtstep
    ! Constant for Nitrogen cooling temperature calculation (ncreephist = 2)
    REAL(r8k), PRIVATE :: A02
    ! Constant for Nitrogen cooling temperature calculation (ncreephist = 2)
    REAL(r8k), PRIVATE :: A06
    ! Constant for Nitrogen cooling temperature calculation (ncreephist = 2)
    REAL(r8k), PRIVATE :: A12
    ! Constant for Nitrogen cooling temperature calculation (ncreephist = 2)
    REAL(r8k), PRIVATE :: A16
    ! Used in convergence test in STIFF
    REAL(r8k), PRIVATE :: BND
    ! Convergence rate in STIFF
    REAL(r8k), PRIVATE :: CRATE
    ! Error calculation in STIFF
    REAL(r8k), PRIVATE :: E
    ! Error calculation in STIFF
    REAL(r8k), PRIVATE :: EDN
    ! Error calculation in STIFF
    REAL(r8k), PRIVATE :: EUP
    ! Relative error bound
    REAL(r8k), PRIVATE :: EPS
    ! EPSJ = SQRT(UROUND)
    REAL(r8k), PRIVATE :: EPSJ
    ! Old value of EPS
    REAL(r8k), PRIVATE :: EPSOLD
    ! Cladding fluence from FRAPCON
    REAL(r8k), PRIVATE :: FLUE
    ! Fast neutron fluence at hottest node taken from FRAPCON
    REAL(r8k), PRIVATE :: FLUE21
    ! Initial cladding stress at hottest node taken from FRAPCON
    REAL(r8k), PRIVATE :: FSTIN
    ! Maximum temperature taken from FRAPCON
    REAL(r8k), PRIVATE :: FTMAX
    ! Current timestep size used for calculation
    REAL(r8k), PRIVATE :: H
    ! Maximum value of the time step size to be used
    REAL(r8k), PRIVATE :: HMAX
    ! Minimum value of the time step size to be used
    REAL(r8k), PRIVATE :: HMIN
    ! Old value of H
    REAL(r8k), PRIVATE :: HOLD
    ! Timestep size actually used in creep calculations (s)
    REAL(r8k), PRIVATE :: HUSED
    ! Old value of L0
    REAL(r8k), PRIVATE :: OLDL0
    ! Used in time step size adjustment
    REAL(r8k), PRIVATE :: RC
    ! Recovery fraction
    REAL(r8k), PRIVATE :: RECOV
    ! Used in relative local error calculations
    REAL(r8k), PRIVATE :: RMAX
    ! Cladding stress
    REAL(r8k), PRIVATE :: ST
    ! Current time value in calculation
    REAL(r8k), PRIVATE :: T
    ! Cladding temperature (C)
    REAL(r8k), PRIVATE :: TC
    ! Initial temperature at hottest node taken from FRAPCON
    REAL(r8k), PRIVATE :: TMAX
    ! Old value of T
    REAL(r8k), PRIVATE :: TOLD
    ! Value of T at which output is next desired. Integration will norally go beyond TOUT and interpolate to T = TOUT
    REAL(r8k), PRIVATE :: TOUT
    ! Previous output time in DRIVE.
    REAL(r8k), PRIVATE :: TOUTP
    ! Time in years
    REAL(r8k), PRIVATE :: TY
    ! Cladding temperature in Kelvin used to calculate stress
    REAL(r8k), PRIVATE :: TZTBL
    ! Alpha-Beta phase transition temperature for zirconium (approximated)
    REAL(r8k), PARAMETER, PRIVATE :: TEMPAB = 800.0_r8k
    ! Unit of roundoff for the computer being used.
    REAL(r8k), PARAMETER, PRIVATE :: UROUND = 2.22044604e-16_r8k
    !
    !    *** Arrays ***
    !
    ! Integers
    ! Array of length NEQ, which is used for pivot information for the linear algebraic system in the correction process.
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE, PRIVATE :: IPIV
    ! Reals
    ! Time input for ncreephist = 3 or 4
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: creeptabtime
    ! Temperature input for ncreephist = 3 or 4
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: creeptabtemp
    ! Stress input for ncreephist = 4
    REAL(r8k), DIMENSION(:), ALLOCATABLE, TARGET :: creeptabstress
    ! Array of NEQ elements. ERROR(i)/TQ(2) is the estimated local error in Y(I) per SS units of T or per step (of size H).
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: ERROR
    ! Array for working storage, length = NEQ
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: SAVE1
    ! Array for working storage, length = NEQ
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: SAVE2
    ! Block of locations used for the partial derivatives of f with respect to Y
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: PW
    ! Array of NEQ elements with which the estimated local errors in Y are compared
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: YMAX
    ! YSA is the zircaloy cladding yield stress material property array
    REAL(r8k), DIMENSION(7), PARAMETER, PRIVATE :: YSA = &
      &  (/ 387.1_r8k, 313.6_r8k, 245.0_r8k, 194.0_r8k, 96.2_r8k, 58.7_r8k, 46.1_r8k /)
    !
    CONTAINS
    !
    SUBROUTINE Datingdriver
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : CladAveTemp, FastFluence, sig
    IMPLICIT NONE
    !> @brief
    !> This is the driver Subroutine for the dating module. It is called from frpcon after the in-reactor analysis is completed.
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! Internal
    !
    ! nodemax - Index of the maximum temperature node from the last step of the FRAPCON calculation
    !
    INTEGER(ipk) :: i, nodemax
    ! Ensure that datingtstep is below 1 second.
    IF (datingtstep >= 1) datingtstep = 0.1_r8k
    ! Tables for options 3 and 4
    ! Variables calculated in FRAPCON
    ! Find Maximum Temperature node
    tmax = 0.0_r8k
    DO i = 1, (na - 1)
        IF (CladAveTemp(i) > tmax) THEN
            tmax = CladAveTemp(i)
            nodemax = i
        END IF
    END DO
    ! Set initial temperature, fluence , and stress
    FTMAX = tfc(tmax)
    FLUE21 = FastFluence(nodemax) / 1.0e25_r8k
    FSTIN = sig(nodemax,1) * PSItoMPa
    ! Initialize time, temperature, and stress arrays based on user input (defaults are set to zero if not provided by user)
    DO i = 1, ncreeptab
        ! Ensure that creeptabtime(i) is > creeptabtime (i-1). If not, add 1 hour - Modified by IP
        IF (i > 2) THEN
            IF (creeptabtime(i) <= creeptabtime(i-1)) creeptabtime(i) = creeptabtime(i) + 1.0_r8k / 24.0_r8k * daytoyr
        END IF
    END DO
    ! Print that it is in the Dating Calculation and the time parameters used in the calculation
    WRITE (0,205) creeppooltime, creeptime, datingtstep
205 FORMAT (2x,'Performing Dating Calculation. ',/,10x,'Start time',7x,'= ',1pe11.4,' years',/, &
      &     10x,'End time',9x,'= ',1pe11.4,' years',/,10x,'Initial timestep = ',1pe11.4,' seconds')
    ! Call DATING
    CALL Dating_Main
    !
    END SUBROUTINE Datingdriver
    !
    !
    !
    SUBROUTINE Dating_Main
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> Dating_Main writes summaries to the output and calls DRIVE to perform the DATING calculation
    !> @author
    !> Coded by Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    !
    INTEGER(ipk) :: IP, I, LPTS ! DO loop counters
    INTEGER(ipk) :: LTMAX       ! Used to calculate the yield stress: LTMAX = TMAXL/100
    INTEGER(ipk) :: TMAXL       ! Used to calculate the yield stress: temperature rounded down to the nearest multiple of 100 K
    ! INTEGER(ipk) :: FLTEST should be added if user wants a code stop when NFLAGT or NFLAGS are triggered
    REAL(r8k) :: BN                                     ! parameter in nitrogen cooling temperature calculation
    REAL(r8k) :: DTIME                                  ! time interval between printout steps
    REAL(r8k) :: REC0, STRAIN, DAM                      ! solution output: cladding recovery, strain, and damage
    REAL(r8k) :: SLOPE, SLOPES                          ! temperature and stress input slopes
    REAL(r8k) :: T0                                     ! initial time for calculations
    REAL(r8k) :: TCC, TZ, TB, TCA, TCB, TK, TKA, TKB    ! used in temperature calculations
    REAL(r8k) :: YS                                     ! yield stress
    REAL(r8k), DIMENSION(NEQ) :: Y                      ! solution vector: creep strain, recovery fraction, and creep damage
    !
    ! *********************************************************************
    ! *** FLAGS COMMUNICATES FLAGGED CONDITIONS BETWEEN MAIN AND DIFFUN ***
    ! ***   MFLAG  - CREEP RATE AND RUPTURE MECHANISM                   ***
    ! ***   NFLAG  - ATHERMAL CREEP ASSUMED DURING CALCULATION          ***
    ! ***   NFLAGT - ALPHA/BETA TRANSITION TEMPERATURE EXCEEDED         ***
    ! ***   NFLAGS - YIELD STRESS EXCEEDED                              ***
    ! *********************************************************************
    !
    ! *************************
    ! *** INITIALIZE VALUES ***
    ! *************************
    !
    NFLAG = 0
    KFLAG = 0
    NFLAGT = 0
    NFLAGS = 0
    MFLAG = 0
    !
    ! **************
    ! *** HEADER ***
    ! **************
    !
    WRITE (ounit,246)
246 FORMAT (/,'*** Prediction of creep under spent fuel storage conditions was requested ***',/, &
      &       '*** Calculations performed using DATING  Rev1 Beta Version*',/)
    WRITE (ounit,137)
137 FORMAT (/,'*** DO NOT EXCEED THE YIELD STRESS*',/,'***400MPa unirradiated and 700MPa irradiated***')
    WRITE (ounit,110)
110 FORMAT(/,2x, 'Model Input')
    !
    ! ***************************
    ! *** CREEP MODEL OPTIONS ***
    ! ***************************
    !
    SELECT CASE (idatingcreep)
    CASE (1)
        WRITE (ounit,115)
115    FORMAT (4X,'Model Selection: CONSERVATIVE CREEP + CONSERVATIVE M-G')
    CASE (2)
        WRITE (ounit,116)
116    FORMAT (4X,'Model Selection: CONSERVATIVE CREEP + BEST ESTIMATE M-G ')
    CASE (3)
        WRITE (ounit,117)
117    FORMAT (4X,'Model Selection: BEST ESTIMATE CREEP + BEST ESTIMATE M-G ')
    END SELECT
    !
    ! **************************************
    ! **************************************
    ! ***   BEGIN CUMULATIVE FRACTION    ***
    ! ***         CALCULATIONS           ***
    ! **************************************
    ! **************************************
    !
    ! ************************************
    ! *** SELECTED TEMP/STRESS HISTORY ***
    ! ************************************
    !
    SELECT CASE (ncreephist)
    CASE (1)
        WRITE (ounit,87)
87      FORMAT (4X,'Temperature/stress history: Helium temperature decay curve with FRAPCON initialization')
    CASE (2)
        WRITE (ounit,88)
88      FORMAT (4X,'Temperature/stress history: Nitrogen temperature decay curve with FRAPCON initialization')
    CASE (3)
        WRITE (ounit,89)
89      FORMAT (4X,'Temperature/stress history: User specified temperature decay curve with FRAPCON stress initialization')
    CASE (4)
        WRITE (ounit,93)
93      FORMAT (4X,'Temperature/stress history: User specified temperature and stress history')
    END SELECT
    !
    ! If ncreephist = 3 or 4, read initial TEMP and/or STRESS FROM input file
    ! If ncreephist = 1, 2, or 3, write FRAPCON initialized temperature and/or stress for cumulative fraction calculation
    !
    WRITE (ounit,136) FLUE21 * 1.0E25_r8k
136 FORMAT(4X,'Fast Neutron Fluence: ',1pe11.4,' n/m^2')
    FLUE = FLUE21 * 1.0E21_r8k
    WRITE (ounit,84) creeppooltime
84  FORMAT(4X,'Time out of reactor prior to dry storage:',f10.4,' years')
    !
    SELECT CASE (ncreephist)
    CASE (1, 2)
        WRITE (ounit,19) FTMAX
        WRITE (ounit,21) FSTIN
    CASE (3)
        TZTBL = creeptabtemp(1) + 273.16_r8k
        FTMAX = creeptabtemp(1)
        WRITE (ounit,21) FSTIN
    CASE (4)
        TZTBL = creeptabtemp(1) + 273.16_r8k
        FTMAX = creeptabtemp(1)
        FSTIN = creeptabstress(1)
    END SELECT
    TMAX = FTMAX
19  FORMAT(4X,'Temperature at beginning of storage period: ',f10.4,' deg. C')
21  FORMAT(4X,'Hoop Stress at beginning of storage period: ',f10.4,' MPa')
    !
    ! **************************************************************************
    ! *** INITIAL CHECKS FOR YIELD STRESS AND FOR ALPHA-BETA TRANSITION TEMP ***
    ! **************************************************************************
    !
    LTMAX = INT(TMAX / 100.0_r8k)
    TMAXL = LTMAX * 100
    IF (TMAX < 100.0_r8k) THEN
        YS = -0.735_r8k * TMAX + 460.6_r8k
    ELSE IF (TMAX > 700.0_r8k) THEN
        YS = -0.126_r8k * TMAX + 134.3_r8k
    ELSE
        YS = YSA(LTMAX) + (TMAX - TMAXL) * (YSA(LTMAX+1) - YSA(LTMAX)) / 100.0_r8k
    END IF
    IF (FSTIN > YS) THEN
        WRITE (0,310)
310     FORMAT (2X,'** ERROR ** INITIAL STRESS EXCEEDS THE YIELD STRESS')
    END IF
    !
    IF (TMAX > TEMPAB) THEN
        WRITE (0,311)
311     FORMAT (2X,'** ERROR ** INITIAL TEMPERATURE EXCEEDS ALPHA/BETA TRANSITION')
    END IF
    
    IF ((FSTIN <= YS) .OR. (TMAX <= TEMPAB)) THEN
        ! **************************************
        ! *** INITIALIZE PARAMETERS FOR GEAR ***
        ! **************************************
        !
        ! EPS IS THE ERROR CONTROL PARAMETER FOR GEAR
        EPS = 1.0E-8_r8k
        T0 = 0.0_r8k
        DATING_INDEX = 1
        Y(1) = 0.0_r8k
        Y(2) = 0.0_r8k
        Y(3) = 0.0_r8k
        REC0 = 1.0_r8k
        DTIME = creeptime / REAL(ncreepstep)
        ! Write time from discharge (yrs), damage fraction, strain (in/in), recovery fraction, 
        ! cladding temp (C), and cladding Hoop stress (MPa)
        WRITE (ounit,202)
202     FORMAT (/,'  Model Results',&
            &   /,'   Time from                                               Cladding      Cladding', &
            &   /,'   Discharge     Damage        Strain        Recovery      Temperature   Hoop Stress', &
            &   /,'   Years         Fraction      in/in         Fraction      Deg. C        MPa')
        WRITE (ounit,100) creeppooltime,Y(3),Y(1),REC0,TMAX,FSTIN
100     FORMAT (2X,6E14.7)
        !
        ! ********************************************************
        ! *** LOOP FOR CALCULATING CUMULATIVE FRACTION VS TIME ***
        ! ********************************************************
        !
        DO I = 1, ncreepstep
            TOUT = I * yrtosec * DTIME !Output time after initiation of calculation, seconds
            TY = TOUT / yrtosec 
            !
            ! ***************************************************************
            ! *** CALL DRIVE INTERGRATES VALUES TO TIME TOUT USING GEAR   ***
            ! ***          GEAR USES RATES DEFINED IN DIFFUN              ***
            ! ***************************************************************
            ! *** GEAR SELECTS TIME STEPS FOR INTEGRATION BY CALCULATING  ***
            ! *** RATES OF CHANGE IN THE PARAMETERS STRAIN, RECOV         ***
            ! *** AND DAM DEFINED IN DIFFUN.  INTEGRATION PROCEEDS UNTIL  ***
            ! *** THE TIME EXCEEDS TOUT AND THE VALUES ARE THEN           ***
            ! *** INTERPOLATED TO TOUT FOR OUTPUT.                        ***
            ! ***************************************************************
            !
            ! Reinitialize the convergence rate at the beginnig of each time step to avoid infinite time step refinement
            CRATE = 0.0_r8k
            !
            ! Call the driver to solve the problem at this time step
            CALL DRIVE (T0, Y)
            !
            ! Calculate results from DRIVE output Y
            STRAIN = Y(1)
            RECOV = 1.0_r8k / (1.0_r8k + Y(2))
            DAM = Y(3)
            !
            !The following lines can be added to printout warnings to the active window at each time step
    !        IF (NFLAGS > 0) THEN ! Test for yield stress flag
    !            WRITE (0,312) creeppooltime + TY
    !312         FORMAT (2X,'** WARNING ** STRESS EXCEEDS THE YIELD STRESS AT TIME ',e14.7)
    !        END IF
    !        IF (NFLAGT > 0) THEN ! Test for alpha-beta transition temperature flag
    !            WRITE (0,313) creeppooltime + TY
    !313         FORMAT (2X,'** WARNING ** TEMPERATURE EXCEEDS ALPHA/BETA TRANSITION AT TIME ',e14.7)
    !        END IF
            !
            !FLTEST = NFLAGT + NFLAGS ! If any of the two flags above have been triggered, stop the code
            !IF (FLTEST /= 0) GOTO 77 ! Should be commented out to keep the code running even if limits are exceeded
            !
            ! ***************************************************
            ! ***   CALCULATE TEMP AND STRESS FOR PRINT OUT   ***
            ! ***************************************************
            ! *** THE TEMPERATURE AND STRESS HISTORY          ***
            ! *** EQUATIONS ARE IDENTICAL TO THOSE IN DIFFUN. ***
            ! *** THEY ARE REPRODUCED HERE TO CALCULATE THE   ***
            ! *** THE TEMPERATURE AND STRESS AT TOUT.         ***
            ! *** GEAR INTERPOLATES STRAIN, RECOV AND DAM AT  ***
            ! *** TOUT BUT DOES NOT INTERPOLATE TEMPERATURE   ***
            ! *** AND STRESS BECAUSE THEY ARE NOT CALCULATED  _frapcon***
            ! *** FROM RATE EQUATIONS IN DIFFUN.              ***
            ! ***************************************************
            !
            SELECT CASE (ncreephist)
            CASE (1)    ! HELIUM COOLING
                TZ = (TMAX + 273.16_r8k) * (creeppooltime * 12.0_r8k) ** (0.34_r8k)
                IF (creeppooltime > 7.0_r8k) TZ = (TMAX + 273.16_r8k) * 84.0_r8k ** 0.34_r8k / 84.0_r8k ** 0.084_r8k * &
                  &   (creeppooltime * 12.0_r8k) ** 0.084_r8k
                TB = TZ * 84.0_r8k ** (-0.34_r8k) / (84.0_r8k ** (-0.084_r8k))
                TCA = TZ * (creeppooltime * 12.0_r8k + TY * 12.0_r8k) ** (-0.34_r8k)
                TCB = TB * (creeppooltime * 12.0_r8k + TY * 12.0_r8k) ** (-0.084_r8k)
                ! Cladding temperature in Kelvin (TK) and Celsius (TC)
                TK = MAX(TCA, TCB)
                TC = tkc(TK)
                ! Cladding stress
                ST = FSTIN * TK / (TMAX + 273.16_r8k)
            CASE (2)    ! NITROGEN COOLING
                ! Initialize constants for nitrogen cooling temperature history
                BN = 30.0_r8k
                A02 = EXP(1.455_r8k + 0.204_r8k * LOG(BN) - 0.23291E-1_r8k * LOG(BN) ** 2)
                A06 = EXP(1.167_r8k + 0.169_r8k * LOG(BN))
                A12 = -1.0339_r8k + 0.0094_r8k * BN
                A16 = -0.51391E-1_r8k - 0.98789E-2_r8k * BN + 0.92362E-4_r8k * BN ** 2
                !
                TZ = EXP(A02 + A12 * LOG(creeppooltime)) + 273.16_r8k
                IF (creeppooltime > 5.629_r8k) TZ = EXP(A06 + A16 * LOG(creeppooltime)) + 273.16_r8k
                TKA = (EXP(A02 + A12 * LOG(creeppooltime + TY)) + 273.16_r8k) / TZ * (TMAX + 273.16_r8k)
                TKB = (EXP(A06 + A16 * LOG(creeppooltime + TY)) + 273.16_r8k) / TZ * (TMAX + 273.16_r8k)
                ! Cladding temperature in Kelvin (TK) and Celsius (TC)
                TK = MAX(TKA, TKB)
                TC = TKC(TK)
                ! Cladding stress
                ST = FSTIN * TK / (TMAX + 273.16_r8k)    ! Cladding Stress
            CASE (3, 4) ! TABLE TEMPERATURE DEPENDENCE
                DO IP = 1, ncreeptab - 1
                    IF (TY <= creeptabtime(IP+1)) THEN
                        ! Temperature slope
                        SLOPE = (creeptabtemp(IP+1) - creeptabtemp(IP)) / (creeptabtime(IP+1) - creeptabtime(IP))
                        ! Temperature value
                        TCC = creeptabtemp(IP) + SLOPE * (TY - creeptabtime(IP))
                        ! Stress slope
                        SLOPES = (creeptabstress(IP+1) - creeptabstress(IP)) / (creeptabtime(IP+1) - creeptabtime(IP))
                        ! Stress value
                        ST = creeptabstress(IP) + SLOPES * (TY - creeptabtime(IP))
                        EXIT
                    END IF
                END DO
                IF (IP == ncreeptab) THEN
                    TCC = creeptabtemp(ncreeptab)
                    ST = creeptabstress(ncreeptab)
                END IF
                IF (ncreephist == 4) TZTBL = TMAX + 273.16_r8k
                ! Calculate stress change from temperature cooling for ncreephist = 3
                ! ncreephist = 4 uses stress magnitudes provided by input
                ! Cladding temperature in Kelvin (TK) and Celsius (TC)
                TK = (TMAX + 273.16_r8k) / TZTBL * (TCC + 273.16_r8k)
                TC = tkc(TK)
                ! Cladding stress
                IF (ncreephist == 3) ST = FSTIN * TK / (TMAX + 273.16_r8k)
            END SELECT
            !
            ! ********************************************************* 
            ! *** OUTPUT FOR CUMULATIVE DAMAGE FRACTION CALCULATION *** 
            ! ********************************************************* 
            !
            WRITE (ounit,100) (creeppooltime + TY), DAM, STRAIN, RECOV, TC, ST
        END DO
        WRITE (0,203)
203     FORMAT (/,2X,'INTEGRATION WAS COMPLETED USING')
        WRITE (0,204) nstep_driver,datingtstep,HUSED
204     FORMAT (2X,'TIME STEPS = ',I10,5X,'FIRST STEP(SEC) = ',E11.3,5X,'LAST STEP(SEC) = ',E11.3)
    END IF
    !
    IF (NFLAG > 0) WRITE (ounit,78)
78  FORMAT (2X,'NOTE ATHERMAL CREEP WAS PREDICTED IN CALCULATIONS')
    IF (NFLAGS > 0) WRITE (ounit,79)
79  FORMAT (2X,'** WARNING ** STRESS EXCEEDING THE YIELD STRESS PREDICTED IN CALCULATIONS')
    IF (NFLAGT > 0) WRITE (ounit,80)
80  FORMAT (2X,'** WARNING ** TEMPERATURE EXCEEDING ALPHA/BETA TRANSITION PREDICTED IN CALCULATIONS')
    ! *********************************** 
    ! *** OUTPUT "HIST" TABLE IF USED *** 
    ! *********************************** 
    IF (ncreephist > 2) THEN
        WRITE (ounit,241) 
241     FORMAT (/2X,'User specified temperature and pressure were used')
        WRITE (ounit,242) ! The units are always converted to SI (Celcius & MPa) no matter what the input flag nunits
242     FORMAT (3x,'Time          Temperature   Stress',/,3x,'Years         deg. C        MPa')
        DO LPTS = 1, ncreeptab
            WRITE (ounit,244) (creeptabtime(LPTS) + creeppooltime), creeptabtemp(LPTS), creeptabstress(LPTS)
244         FORMAT(2X,3E14.7)
        END DO
    END IF
    !
    END SUBROUTINE Dating_main
    !
    !
    !
    SUBROUTINE DRIVE (T0, Y0)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> DRIVE writes summaries to the output and calls STIFF for the DATING calculation
    !> DRIVE is a driver subroutine for the integration package, it is to be called once for each output value of T.
    !> DRIVE then makes repeated calls to the core integrator subroutine, STIFF.
    !> DRIVE runs in an infinite loop until convergence criteria are met.
    !> @author
    !> Coded by Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! DRIVE input parameters (from EPISODE code description)
    ! NEQ =     the number of differential equations (used only on first call, unless index = -1). NEQ must never be
    !           increased during a given problem. Set as parameter equal to 3 for DATING
    ! T0 =      the initial value of T, the independent variable (used for input only on first call).
    ! H0 =      the step size in T (used for input only on the first call, unless DATING_INDEX = 3 on input). when
    !           DATING_INDEX = 3, H0 is the maximum absolute value of the step size to be used.
    ! Y0 =      a vector of length NEQ containing the initial values of Y (used for input only on first call).
    ! TOUT =    the value of T at which output is desired next. integration will normally go beyond tout and
    !           interpolate to T = TOUT. (used only for input.)
    ! EPS =     the relative error bound
    ! DATING_INDEX =   integer used on input to indicate type of call, with the following values and meanings..
    !       1 this is the first call for this problem.
    !       0 this is not the first call for this problem, and integration is to continue.
    !       -1 this is not the first call for the problem, and the user has reset NEQ, EPS, and/or MF.
    !       2 same as 0 except that tout is to be hit exactly (no interpolation is done).
    !           assumes tout .ge. the current t.
    !       3 same as 0 except control returns to calling program after one step. tout is ignored.
    !           since the normal output value of index is 0, it need not be reset for normal continuation.
    !
    ! after the initial call, if a normal return occurred and a normal continuation is desired, simply reset tout and call again.
    ! all other parameters will be ready for the next call.
    ! a change of parameters with DATING_INDEX = -1 can be made after either a successful or an unsuccessful return.
    !
    ! DRIVE output parameters (from EPISODE code description)
    ! T0 =      the output value of T. if integration was successful, T0 = TOUT. otherwise, T0 is the last value of T
    !           reached successfully.
    ! H0 =      the step size H used last, whether successfully or not.
    ! Y0 =      the computed values of Y at T = T0.
    ! DATING_INDEX =   integer used on output to indicate results with the following values and meanings..
    !       0 integration was completed to TOUT or beyond.
    !       -1 the integration was halted after failing to pass the error test even after reducing H by a factor of
    !           1.e10 from its initial value.
    !       -2 after some initial success, the integration was halted either by repeated error test failures or
    !           by a test on EPS. possibly too much accuracy has been requested, or a bad choice of MF was made.
    !       -3 the integration was halted after failing to achieve corrector convergence even after reducing H by a
    !           factor of 1.e10 from its initial value.
    !       -4 immediate halt because of illegal values of input parameters_frapcon. see printed message.
    !       -5 DATING_INDEX was -1 on input, but the desired changes of parameters were not implemented because TOUT_frapcon
    !           was not beyond T. interpolation to T = TOUT was performed as on a normal return. to continue,
    !           simply call again with DATING_INDEX = -1 and a new TOUT.
    !       -6 DATING_INDEX was 2 on input, but TOUT was not beyond T. no action was taken.
    !
    ! Procedure arguments
    REAL(r8k), INTENT(INOUT) :: T0                  ! see above
    REAL(r8k), DIMENSION(NEQ), INTENT(INOUT) :: Y0  ! see above
    !
    INTEGER(ipk) :: I       ! DO loop counter
    INTEGER(ipk) :: NHOUT   ! Iteration counter for integration loop
    REAL(r8k) :: AYI        !
    REAL(r8k) :: D          ! used for error and convergence calculations
    REAL(r8k), DIMENSION(NEQ,LMAX) :: Y ! NEQ by LMAX array containing the dependent variables and their scaled derivatives
    !
    SELECT CASE (DATING_INDEX)
    CASE (1)
        IF (EPS <= 0.0_r8k) THEN
            WRITE (ddunit,405)
            DATING_INDEX = -4
            RETURN
        END IF
        IF (NEQ <= 0) THEN
            WRITE (ddunit,415)
            DATING_INDEX = -4
            RETURN
        END IF
        IF ((T0 - TOUT) * datingtstep >= 0.0_r8k) THEN
            WRITE (ddunit,425)
            DATING_INDEX = -4
            RETURN
        END IF
        !
        DO I = 1, NEQ
            YMAX(I) = ABS(Y0(I))
            IF (YMAX(I) == 0.0_r8k) YMAX(I) = 1.0_r8k
            Y(I,1) = Y0(I)
        END DO
        T = T0
        H = datingtstep
        IF ((T + H) == T) WRITE (ddunit,15)
        HMIN = ABS(datingtstep)
        HMAX = ABS(T0 - TOUT) * 10.0_r8k
        JSTART = 0
        EPSJ = SQRT(UROUND)
        NHOUT = 0
    CASE (0)
        DO
            HMAX = ABS(TOUT - TOUTP) * 10.0_r8k
            IF ((T - TOUT) * H < 0.0_r8k) THEN
                IF ((T + H) == T) WRITE (ddunit,15)
                EXIT
            END IF
            CALL INTERP(TOUT,Y,Y0)
            DATING_INDEX = KFLAG
            TOUTP = TOUT
            datingtstep = HUSED
            IF (KFLAG /= 0) datingtstep = H
            RETURN
        END DO
    CASE (-1)
        IF ((T - TOUT) * H >= 0.0_r8k) THEN
            WRITE (ddunit,445) T,TOUT,H
            CALL INTERP(TOUT,Y,Y0)
            DATING_INDEX = -5
            RETURN
        END IF
        JSTART = -1
        IF ((T + H) == T) WRITE (ddunit,15)
    CASE (2)
        DO
            HMAX = ABS(TOUT - TOUTP) * 10.0_r8k
            IF ((T - TOUT) * H >= 0.0_r8k) THEN
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF (((T + H) - TOUT) * H <= 0.0_r8k) THEN
                IF ((T + H) == T) WRITE (ddunit,15)
                EXIT
            END IF
            IF (ABS(T - TOUT) <= 100.0_r8k * UROUND * HMAX) THEN
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF ((T - TOUT) * H >= 0.0_r8k) THEN
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            H = (TOUT - T) * (1 - 4.0_r8k * UROUND)
            JSTART = -1
            IF ((T + H) == T) WRITE (ddunit,15)
            EXIT
        END DO
    CASE (3)
        IF ((T + H) == T) WRITE (ddunit,15)
    CASE DEFAULT
        WRITE (ddunit,435) DATING_INDEX
        DATING_INDEX = -4
        RETURN
    END SELECT
    !
    DO
        CALL STIFF (Y, NQ)
        SELECT CASE (1 - KFLAG)
        CASE (1)
            !
            ! normal return from STIFF.
            !
            ! the weights ymax(i) are updated. if different values are desired,
            ! they should be set here. if ss is to be updated for control of
            ! error per ss units of t, it should also be done here. a test is
            ! made to determine if eps is too small for machine precision.
            !
            ! any other tests or calculations that are required after each step
            ! should be inserted here.
            ! if DATING_INDEX = 3, Y0 is set to the current Y values on return.
            ! if DATING_INDEX = 2, H is controlled to hit TOUT (within roundoff
            ! error), and then the current Y values are put in Y0 on
            ! return. for any other value of DATING_INDEX, control returns to
            ! the integrator unless tout has been reached. then
            ! interpolated values of Y are computed and stored in Y0 on
            ! return.
            ! if interpolation is not desired, the call to INTERP should
            ! be deleted
            !
            D = 0.0_r8k
            DO I = 1, NEQ
                AYI = ABS(Y(I,1))
                YMAX(I) = MAX(YMAX(I),AYI)
                D = D + (AYI / YMAX(I)) ** 2
            END DO
            D = D * (UROUND / EPS) ** 2
            IF (D > REAL(NEQ)) THEN
                WRITE (ddunit,255) T
                KFLAG = -2
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF (DATING_INDEX == 3) THEN
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF (DATING_INDEX == 2) THEN
                IF (((T + H) - TOUT) * H <= 0.0_r8k) THEN
                    IF ((T + H) == T) WRITE (ddunit,15)
                    CYCLE
                END IF
                IF (ABS(T - TOUT) <= 100.0_r8k * UROUND * HMAX) THEN
                    TOUT = T
                    Y0(1:NEQ) = Y(1:NEQ,1)
                    DATING_INDEX = KFLAG
                    TOUTP = TOUT
                    datingtstep = HUSED
                    IF (KFLAG /= 0) datingtstep = H
                    RETURN
                END IF
                IF ((T - TOUT) * H >= 0.0_r8k) THEN
                    TOUT = T
                    Y0(1:NEQ) = Y(1:NEQ,1)
                    DATING_INDEX = KFLAG
                    TOUTP = TOUT
                    datingtstep = HUSED
                    IF (KFLAG /= 0) datingtstep = H
                    RETURN
                END IF
                H = (TOUT - T) * (1 - 4.0_r8k * UROUND)
                JSTART = -1
                IF ((T + H) == T) WRITE (ddunit,15)
                CYCLE
            END IF
            IF ((T - TOUT) * H < 0.0_r8k) THEN
                IF ((T + H) == T) WRITE (ddunit,15)
                CYCLE
            END IF
            CALL INTERP(TOUT,Y,Y0)
            DATING_INDEX = KFLAG
            TOUTP = TOUT
            datingtstep = HUSED
            IF (KFLAG /= 0) datingtstep = H
            RETURN
        CASE (2)
            !
            ! on an error return from STIFF, an immediate return occurs if
            ! KFLAG = -2, and recovery attempts are made otherwise.
            ! to recover, H and HMIN are reduced by a factor of .1 up to 10
            ! times before giving up.
            !
            WRITE (ddunit,105) T
            IF (NHOUT == 10) THEN
                WRITE (ddunit,155)
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            NHOUT = NHOUT + 1
            HMIN = 0.1_r8k * HMIN
            H = 0.1_r8k * H
            WRITE (ddunit,115) H
            JSTART = -1
            IF ((T + H) == T) WRITE (ddunit,15)
            CYCLE
        CASE (3)
            WRITE (ddunit,206) T,H
            TOUT = T
            Y0(1:NEQ) = Y(1:NEQ,1)
            DATING_INDEX = KFLAG
            TOUTP = TOUT
            datingtstep = HUSED
            IF (KFLAG /= 0) datingtstep = H
            RETURN
        CASE (4)
            WRITE (ddunit,305) T
            IF (NHOUT == 10) THEN
                WRITE (ddunit,155)
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            NHOUT = NHOUT + 1
            HMIN = 0.1_r8k * HMIN
            H = 0.1_r8k * H
            WRITE (ddunit,115) H
            JSTART = -1
            IF ((T + H) == T) WRITE (ddunit,15)
            CYCLE
        CASE DEFAULT
            D = 0.0_r8k
            DO I = 1, NEQ
                AYI = ABS(Y(I,1))
                YMAX(I) = MAX(YMAX(I),AYI)
                D = D + (AYI / YMAX(I)) ** 2
            END DO
            D = D * (UROUND / EPS) ** 2
            IF (D > REAL(NEQ)) THEN
                WRITE (ddunit,255) T
                KFLAG = -2
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF (DATING_INDEX == 3) THEN
                TOUT = T
                Y0(1:NEQ) = Y(1:NEQ,1)
                DATING_INDEX = KFLAG
                TOUTP = TOUT
                datingtstep = HUSED
                IF (KFLAG /= 0) datingtstep = H
                RETURN
            END IF
            IF (DATING_INDEX == 2) THEN
                IF (((T + H) - TOUT) * H <= 0.0_r8k) THEN
                    IF ((T + H) == T) WRITE (ddunit,15)
                    CYCLE
                END IF
                IF (ABS(T - TOUT) <= 100.0_r8k * UROUND * HMAX) THEN
                    TOUT = T
                    Y0(1:NEQ) = Y(1:NEQ,1)
                    DATING_INDEX = KFLAG
                    TOUTP = TOUT
                    datingtstep = HUSED
                    IF (KFLAG /= 0) datingtstep = H
                    RETURN
                END IF
                IF ((T - TOUT) * H >= 0.0_r8k) THEN
                    TOUT = T
                    Y0(1:NEQ) = Y(1:NEQ,1)
                    DATING_INDEX = KFLAG
                    TOUTP = TOUT
                    datingtstep = HUSED
                    IF (KFLAG /= 0) datingtstep = H
                    RETURN
                END IF
                H = (TOUT - T) * (1 - 4.0_r8k * UROUND)
                JSTART = -1
                IF ((T + H) == T) WRITE (ddunit,15)
                CYCLE
            END IF
            IF ((T - TOUT) * H < 0.0_r8k) THEN
                IF ((T + H) == T) WRITE (ddunit,15)
                CYCLE
            END IF
            CALL INTERP(TOUT,Y,Y0)
            DATING_INDEX = KFLAG
            TOUTP = TOUT
            datingtstep = HUSED
            IF (KFLAG /= 0) datingtstep = H
            RETURN
        END SELECT
    END DO
    !
    !Format statements
15  FORMAT(' WARNING..  T+H=T(N NEXT STEP.)')
105 FORMAT(//' KFLAG = -1 FROM INTEGRATOR AT T =',D16.8/' ERROR TEST FAILED WITH ABS(H) = HMIN',/)
115 FORMAT(' H HAS BEEN REDUCED TO ',D16.8/' AND STEP WILL BE RETRIED',//)
155 FORMAT(//' PROBLEM APPEARS UNSOLVED WIHT GIVEN INPUT',//)
206 FORMAT(//' KFLAG = -2 FROM INTEGRATOR AT T= ',D16.8,'H = ',D16.8/' THE REQUESTED ERROR IS SMALLER THAN CAN BE HANDLED',//)
255 FORMAT(//' INTEGRATION HALTED BY DRIVER AT T = ',D16.8/' EPS TOO SMALL TO BE ATTAINED FOR THE MACHINE PRECISION',/)
305 FORMAT(//' KFLAG = -3 FROM INTEGRATOR AT T = ',D16.8/' CORRECTOR CONVERGENCE COULD NOT BE ACHIEVED',/)
405 FORMAT(//' ILLEGAL INPUT.. EPS == 0.',//)
415 FORMAT(//' ILLEGAL INPUT.. NEQ <= 0',//)
425 FORMAT(//' ILLEGAL INPUT.. (T0-TOUT)*H >= 0.',//)
435 FORMAT(//' ILLEGAL INPUT.. DATING_INDEX =',I5,//)
445 FORMAT(//' DATING_INDEX = -1 ON INPUT WITH (T-TOUT)*H >= 0.'//' T = ',D16.8,'TOUT = ',D16.8,'H = ',D16.8,/ &
            &' INTERPOLATION WAS DoNE AS ON NORMAL RETURN.',/' DESIRED PARAMETER CHANGES WERE NOT MADE.',/)
    !
    END SUBROUTINE DRIVE
    !
    !
    !
    SUBROUTINE INTERP (TOUT, Y, Y0)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> INTERP computes interpolated values of the dependent variable Y and stores them in Y0. the interpolation is to the
    !> point T = TOUT and uses the nordsieck history array Y as follows
    !>  NEQ
    !> Y0(I) = sum Y(I,J+1)*S**J
    !>  J=0
    !> where S = -(T-TOUT)/H
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! Input
    !
    ! TOUT  - Interpolation time
    ! Y     - Array (NEQ,LMAX) containing the dependent variables and their scaled derivatives
    !
    ! Output
    !
    ! Y0    - Interpolated values of Y at TOUT
    !
    ! Internal
    !
    ! S     - Fraction of time step to interpolate to
    ! S1    - S1 = S^J for interpolation via the nordsiek history array
    !
    INTEGER(ipk) :: I, J
    REAL(r8k) :: S, S1
    REAL(r8k), INTENT(IN) :: TOUT
    REAL(r8k), DIMENSION(NEQ), INTENT(OUT) :: Y0
    REAL(r8k), DIMENSION(NEQ,LMAX), INTENT(IN) :: Y
    !
    Y0(1:NEQ) = Y(1:NEQ,1)
    L = JSTART + 1
    S = (TOUT - T) / H
    S1 = 1.0_r8k
    DO J = 2, L
        S1 = S1 * S
        DO I = 1, NEQ
            Y0(I) = Y0(I) + S1 * Y(I,J)
        END DO
    END DO
    !
    END SUBROUTINE INTERP
    !
    !
    !
    SUBROUTINE STIFF (Y, NQ)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> STIFF performs one step of the integration of an initial value problem for a system of ordinary differential equations.
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! communication with STIFF is via the following variables..
    !
    ! Y         an NEQ by LMAX array containing the dependent variables and their scaled derivatives. LMAX is currently 6 for
    !           the variable step backward differentiation formulas (METH=2). (LMAX -1) = MAXDER, the maximum order used.
    !           see subroutine coset. Y(I,J+1) contains the J-th derivative of Y(i), scaled by H**J/factorial(J)
    !           for J = 0,1,...,NQ, where NQ is the current order.
    ! T         the independent variable, updated on each step taken.
    ! H         the step size to be attempted on the next step. H is altered by the error control algorithm during
    !           the solution of the problem. H can be either positive or negative, but its sign must remain constant
    !           throughout the problem run.
    ! HMIN,     the minimum and maximum absolute values of the step size to be used for the step. these may be changed at
    ! HMAX      any time, but the change will not take effect until the next change in H is made.
    ! EPS       the relative error bound
    ! SS        the size of the time interval to be used for error control. a default value of 0 is used to produce
    !           control of error per step. see subroutine epsode.
    ! YMAX      an array of NEQ elements with which the estimated local errors in Y are compared.
    ! ERROR     an array of NEQ elements. ERROR(i)/TQ(2) is the estimated local error in Y(I) per SS units of
    !           T or per step (of size H).
    ! SAVE1,    two arrays for working storage,
    ! SAVE2     each of length NEQ.
    ! PW        a block of locations used for the partial derivatives of f with respect to Y, if MITER is not 0. see
    !           description in subroutine epsode.
    ! IPIV      an integer array of length NEQ, which is used for pivot information for the linear algebraic system in the
    !           correction process.
    !
    ! Procedure arguments
    REAL(r8k), DIMENSION(NEQ,LMAX), INTENT(INOUT) :: Y  ! NEQ by LMAX array containing the dependent variables
                                                        !   and their scaled derivatives
    INTEGER(ipk), INTENT(INOUT) :: NQ   ! current order of the system of equations
    !
    INTEGER(ipk) :: EXIT_FLAG           ! on error or non-convergence, flag indicating which statements to perform upon exit
    INTEGER(ipk) :: FN                  ! = REAL (NEQ) = 3.0
    INTEGER(ipk) :: IREDO               ! flag to determine whether to continue iterating for a solution
    INTEGER(ipk) :: IER                 ! argument for the DEC subroutine: singularity indicator
    INTEGER(ipk) :: I, J, J1, J2        ! DO loop counters
    INTEGER(ipk) :: M                   ! solver iteration counter
    INTEGER(ipk) :: NEWQ                ! new order of the system of equations
    REAL(r8k) :: CON                        ! = -H * EL(1), input for PSET
    REAL(r8k) :: D, D1                      ! used for error and residual calculations
    REAL(r8k) :: DIL1, DIL2, HILF           ! used for convergence determinations
    REAL(r8k) :: ENQ3, ENQ2, ENQ1           ! used to calculate PR1, PR2, PR3
    REAL(r8k) :: PR1, PR2, PR3              ! used in tests to adjust the value of NQ and NEWQ
    REAL(r8k) :: R1, RH                 ! used for time step size adjustment calcualtions
    REAL(r8k), DIMENSION(4) :: TQ           ! vector of constants used for the convergence test,
                                            !   the error test, and selection of H at a new order
    REAL(r8k), DIMENSION(LMAX) :: EL        ! argument for COSET: array of coefficients for the corrector formula
    !
    DO
        EL(2) = 1.0_r8k
        OLDL0 = 1.0_r8k
        KFLAG = 0
        TOLD = T
        IF (JSTART < 0) THEN
            ! if the user has changed H, then Y must be rescaled.
            IF (EPS == EPSOLD) THEN
                IF (H == HOLD) EXIT
                RH = H / HOLD
                H = HOLD
                IREDO = 3
                RH = MIN(RH, HMAX / ABS(H), RMAX)
                R1 = 1.0_r8k
                DO J = 2, L
                    R1 = R1 * RH
                    DO I = 1, NEQ
                        Y(I,J) = Y(I,J) * R1
                    END DO
                END DO
                H = H * RH
                RC = RC * RH
                IDOUB = L + 1
                EXIT
            END IF
            EPSOLD = EPS
            IDOUB = L + 1
            ! COSET is called to obtain EL, the vector of coefficients of
            ! length NQ + 1. RC is the ratio of new to old values of the
            ! coefficient H/EL(2)
            CALL COSET (NQ,EL,TQ)
            RC = RC * EL(1) / OLDL0
            OLDL0 = EL(1)
        ELSE IF (JSTART == 0) THEN
            ! on the first call, the order is set to 1 and the initial
            ! derivatives are calculated. RAMX is the maximum ratio by
            ! which H can be increased in a single step. it is 1.e04 for the
            ! first step to compensate for the small initial H, then 10 for
            ! the next 10 steps, and then 1.5 thereafter. if a failure
            ! occurs (in corrector convergence or error test), RMAX is set at 1
            ! for the next increase. 0.1 is the minimum ratio by which
            ! H can be reduced on any retry of a step.
            CALL DIFFUN (Y, SAVE1)
            DO I = 1, NEQ
                Y(I,2) = H * SAVE1(I) !Convert from rate (1/s) to total displacement strain
            END DO
            NQ = 1
            L = 2
            IDOUB = 3
            RMAX = 1.0E4_r8k
            RC = 0.0_r8k
            CRATE = 1.0_r8k
            EPSOLD = EPS
            HOLD = H
            nstep_driver = 0
            NSTEPJ = 0
            ! COSET is called to obtain EL, the vector of coefficients of
            ! length NQ + 1. RC is the ratio of new to old values of the
            ! coefficient H/EL(2)
            CALL COSET (NQ,EL,TQ)
            RC = RC * EL(1) / OLDL0
            OLDL0 = EL(1)
        ELSE IF (JSTART > 0) THEN
            EXIT
        END IF
        !
        FN = REAL(NEQ)
        EDN = FN * (TQ(1) * EPS) ** 2
        E = FN * (TQ(2) * EPS) ** 2
        EUP = FN * (TQ(3) * EPS) ** 2
        BND = FN * (TQ(4) * EPS) ** 2
        IF (H == HOLD) EXIT
        RH = H / HOLD
        H = HOLD
        IREDO = 3
        RH = MIN(RH, HMAX / ABS(H), RMAX)
        R1 = 1.0_r8k
        DO J = 2, L
            R1 = R1 * RH
            DO I = 1, NEQ
                Y(I,J) = Y(I,J) * R1
            END DO
        END DO
        H = H * RH
        RC = RC * RH
        IDOUB = L + 1
        EXIT
        !
    END DO
    !
    IF (ABS(RC - 1.0_r8k) > 0.3_r8k) IWEVAL = MITER
    IF (nstep_driver >= NSTEPJ + 20) IWEVAL = MITER
    ! this section computes the predicted values by effectively
    ! multiplying the Y array by the pascal triangle matrix
    T = T + H
    DO J1 = 1, NQ
        DO J2 = J1, NQ
            J = (NQ + J1) - J2
            DO I = 1, NEQ
                Y(I,J) = Y(I,J)+Y(I,J+1)
            END DO
        END DO
    END DO
    !
    DO
        M = 0
        DO
            DO 
                EXIT_FLAG = 0
                ! up to 3 corrector iterations are taken. a convergence test is made
                ! on the root mean square norm of each correction, using BND, which
                ! is dependent on EPS. the sum of the corrections is accumulated in
                ! the vector ERROR. the Y array is not altered in the corrector
                ! loop. the updated Y vector is stored temporarily in SAVE1.
                ERROR(1:NEQ) = 0.0_r8k
                CALL DIFFUN (Y, SAVE2)
                IF (IWEVAL <= 0) EXIT
                ! if indicated, the matrix p = i - h*rl1*j is reevaluated before
                ! starting the corrector iteration. IWEVAL is set to 0 as an
                ! indicator that this has been done. p is computed and processed in pset
                IWEVAL = 0
                RC = 1.0_r8k
                NSTEPJ = nstep_driver
                CON = -H * EL(1)
                CALL PSET(Y,CON,IER)
                IF (IER /=  0) THEN
                    EXIT_FLAG = 420
                    EXIT
                END IF
                DO I = 1, NEQ
                    SAVE1(I) = H * SAVE2(I) - (Y(I,2) + ERROR(I))
                END DO
                CALL SOL(PW,SAVE1,IPIV)
                D = 0.0_r8k
                DO I = 1, NEQ
                    ERROR(I) = ERROR(I) + SAVE1(I)
                    D = D + (SAVE1(I) / YMAX(I)) ** 2
                    SAVE1(I) = Y(I,1) + EL(1) * ERROR(I)
                END DO
                ! test for convergence. if m .gt. 0, an estimate of the square of
                ! the convergence rate constant is stored in crate, and this is used
                ! in the test.
                IF (M /= 0) CRATE = MAX(0.9_r8k * CRATE, D / D1)
                DIL1 = 2.0_r8k * CRATE
                DIL2 = 1.0_r8k
                HILF = D * MIN(DIL2,DIL1)
                IF (HILF <= BND) THEN
                    EXIT_FLAG = 450
                    EXIT
                END IF
                D1 = D
                M = M + 1
                ! the corrector iteration failed to converge in 3 tries. if partial
                ! derivatives are involved but are not up to date, they are
                ! reevaluated for the next try. otherwise the Y array is restored
                ! to its values before prediction, and H is reduced,
                ! if possible. if not, a no-convergence exit is taken.
                IF (M == 3) THEN
                    IF (IWEVAL == -1) THEN
                        IWEVAL = MITER
                        CYCLE
                    END IF
                END IF
                CALL DIFFUN (SAVE1, SAVE2)
                EXIT
            END DO
            !
            IF (EXIT_FLAG > 0) EXIT
            DO
                EXIT_FLAG = 0
                DO I = 1, NEQ
                    SAVE1(I) = H * SAVE2(I) - (Y(I,2) + ERROR(I))
                END DO
                CALL SOL(PW,SAVE1,IPIV)
                D = 0.0_r8k
                DO I = 1, NEQ
                    ERROR(I) = ERROR(I) + SAVE1(I)
                    D = D + (SAVE1(I) / YMAX(I)) ** 2
                    SAVE1(I) = Y(I,1) + EL(1) * ERROR(I)
                END DO
                IF (ABS(D) == 0.0_r8k) EXIT
                ! test for convergence. if m .gt. 0, an estimate of the square of
                ! the convergence rate constant is stored in crate, and this is used
                ! in the test.
                IF (M /= 0) CRATE = MAX(0.9_r8k * CRATE, D / D1)
                DIL1 = 2.0_r8k * CRATE
                DIL2 = 1.0_r8k
                HILF = D * MIN(DIL2,DIL1)
                IF (HILF <= BND)  THEN
                    EXIT_FLAG = 450
                    EXIT
                END IF
                D1 = D
                M = M + 1
                ! the corrector iteration failed to converge in 3 tries. if partial
                ! derivatives are involved but are not up to date, they are
                ! reevaluated for the next try. otherwise the Y array is restored
                ! to its values before prediction, and H is reduced,
                ! if possible. if not, a no-convergence exit is taken.
                IF (M == 3) THEN
                    IF (IWEVAL == -1) THEN
                        IWEVAL = MITER
                        EXIT_FLAG = 220
                        EXIT
                    END IF
                    EXIT_FLAG = 420
                    EXIT
                END IF
                CALL DIFFUN (SAVE1, SAVE2)
                CYCLE
            END DO
            IF (EXIT_FLAG > 0) EXIT
        END DO
        !
        SELECT CASE (EXIT_FLAG)
        CASE (220)
            CYCLE
        CASE (420)
            T = TOLD
            RMAX = 2.0_r8k
            ! this section computes the predicted values by effectively
            ! multiplying the Y array by the pascal triangle matrix
            DO J1 = 1, NQ
                DO J2 = J1, NQ
                    J = (NQ + J1) - J2
                    DO I = 1, NEQ
                        Y(I,J) = Y(I,J) - Y(I,J+1)
                    END DO
                END DO
            END DO
            IF (ABS(H) <= HMIN * 1.00001_r8k) THEN
                KFLAG = -3
                HOLD = H
                JSTART = NQ
                RETURN
            END IF
            RH = 0.25_r8k
            IREDO = 1
            RH = MAX(RH, HMIN / ABS(H))
            RH = MIN(RH, HMAX / ABS(H), RMAX)
            R1 = 1.0_r8k
            DO j = 2, L
                R1 = R1 * RH
                DO i = 1, NEQ
                    Y(I,J) = Y(I,J) * R1
                END DO
            END DO
            H = H * RH
            RC = RC * RH
            IDOUB = L + 1
            IF (ABS(RC - 1.0_r8k) > 0.3_r8k) IWEVAL = MITER
            IF (nstep_driver >= NSTEPJ + 20) IWEVAL = MITER
            T = T + H
            ! this section computes the predicted values by effectively
            ! multiplying the Y array by the pascal triangle matrix
            DO J1 = 1, NQ
                DO J2 = J1, NQ
                    J = (NQ + J1) - J2
                    DO i = 1, NEQ
                        Y(I,J) = Y(I,J)+Y(I,J+1)
                    END DO
                END DO
            END DO
            CYCLE
        CASE (450)
            DO
                ! the corrector has converged. IWEVAL is set to -1 if partial
                ! derivatives were used, to signal that they may need updating on
                ! subsequent steps. the error test is made and the DO loop is exited
                ! if the error test passes
                IWEVAL = -1
                D = 0.0_r8k
                DO I = 1, NEQ
                    D = D + (ERROR(I) / YMAX(I)) ** 2
                END DO
                IF (D > E) THEN
                    ! the error test failed. KFLAG keeps track of multiple failures.
                    ! T and the Y array are restored to their previous values. a new
                    ! H for a retry of the step is computed. the order is kept fixed.
                    KFLAG = KFLAG - 1
                    T = TOLD
                    ! this section computes the predicted values by effectively
                    ! multiplying the Y array by the pascal triangle matrix
                    DO J1 = 1, NQ
                        DO J2 = J1, NQ
                            J = (NQ + J1) - J2
                            DO I = 1, NEQ
                                Y(I,J) = Y(I,J) - Y(I,J+1)
                            END DO
                        END DO
                    END DO
                    RMAX = 2.0_r8k
                    IF (ABS(H) <= HMIN * 1.00001_r8k) THEN
                        KFLAG = -1
                        HOLD = H
                        JSTART = NQ
                        RETURN
                    END IF
                    IF (KFLAG <= -3) EXIT
                    IREDO = 2
                    PR3 = 1.0E20_r8k
                    EXIT
                END IF
                !
                ! after a successful step, the Y array, nstep_driver, and IDOUB are
                ! updated, and a new value of HUSED at order NQ is computed
                KFLAG = 0
                IREDO = 0
                nstep_driver = nstep_driver + 1
                HUSED = H
                DO J = 1, L
                    DO I = 1, NEQ
                        Y(I,J) = Y(I,J) + EL(J) * ERROR(I)
                    END DO
                END DO
                IF (IDOUB == 1) THEN
                    PR3 = 1.0E20_r8k
                    IF (L == LMAX) EXIT
                    D1 = 0.0_r8k
                    DO I = 1, NEQ
                        D1 = D1 + ((ERROR(I) - Y(I,LMAX)) / YMAX(I)) ** 2
                    END DO
                    ENQ3 = 0.5_r8k / REAL(L+1)
                    PR3 = ((D1 / EUP) ** ENQ3) * 1.4_r8k + 1.4E-6_r8k
                    EXIT
                END IF
                IDOUB = IDOUB - 1
                IF (IDOUB>1) THEN
                    HOLD = H
                    JSTART = NQ
                    RETURN
                END IF
                IF (L == LMAX) THEN
                    HOLD = H
                    JSTART = NQ
                    RETURN
                END IF
                Y(1:NEQ,LMAX) = ERROR(1:NEQ)
                HOLD = H
                JSTART = NQ
                RETURN
            END DO
            !
            IF (KFLAG <= -3) THEN
                ! control reaches this section if 3 or more consecutive failures
                ! have occurred. it is assumed that the elements of the Y array
                ! have accumulated errors of the wrong order. the order is reduced
                ! by one, if possible. then H is reduced by a factor of 0.1 and
                ! the step is retried. after a total of 7 consecutive failures,
                ! an exit is taken with KFLAG = -2.
                IF (KFLAG == -7) THEN
                    KFLAG = -2
                    HOLD = H
                    JSTART = NQ
                    RETURN
                END IF
                RH = 0.1_r8k
                RH = MAX(HMIN / ABS(H), RH)
                H = H * RH
                CALL DIFFUN (Y, SAVE1)
                Y(1:NEQ,2) = H * SAVE1(1:NEQ)
                IWEVAL = MITER
                IDOUB = 10
                IF (NQ /= 1) THEN
                    NQ = 1
                    L = 2
                    ! COSET is called to obtain EL, the vector of coefficients of
                    ! length NQ + 1. RC is the ratio of new to old values of the
                    ! coefficient H/EL(2)
                    CALL COSET (NQ,EL,TQ)
                    RC = RC * EL(1) / OLDL0
                    OLDL0 = EL(1)
                    FN = REAL(NEQ)
                    EDN = FN * (TQ(1) * EPS) ** 2
                    E = FN * (TQ(2) * EPS) ** 2
                    EUP = FN * (TQ(3) * EPS) ** 2
                    BND = FN * (TQ(4) * EPS) ** 2
                END IF
                IF (ABS(RC - 1.0_r8k) > 0.3_r8k) IWEVAL = MITER
                IF (nstep_driver >= NSTEPJ + 20) IWEVAL = MITER
                T = T + H
                ! this section computes the predicted values by effectively
                ! multiplying the Y array by the pascal triangle matrix
                DO J1 = 1, NQ
                    DO J2 = J1, NQ
                        J = (NQ + J1) - J2
                        DO i = 1, NEQ
                            Y(I,J) = Y(I,J)+Y(I,J+1)
                        END DO
                    END DO
                END DO
            ELSE
                ! successful step statements
                ENQ2 = 0.5_r8k / REAL(L)
                PR2 = ((D / E) ** ENQ2) * 1.2_r8k + 1.2E-6_r8k
                PR1 = 1.0E20_r8k
                IF (NQ /= 1) THEN
                    D = 0.0_r8k
                    DO I = 1, NEQ
                        D = D + (Y(I,L) / YMAX(I)) ** 2
                    END DO
                    ENQ1 = 0.5_r8k / REAL(NQ)
                    PR1 = ((D / EDN) ** ENQ1) * 1.3_r8k + 1.3E-6_r8k
                END IF
                IF (PR3 < PR1) THEN
                    NEWQ = L
                    RH = 1.0_r8k / PR3
                    IF (RH < 1.1_r8k) THEN
                        IDOUB = 10
                        HOLD = H
                        JSTART = NQ
                        RETURN
                    END IF
                    DO I = 1, NEQ
                        Y(I,NEWQ+1) = ERROR(I) * EL(L) / REAL(L)
                    END DO
                    NQ = NEWQ
                    L = NQ + 1
                    ! COSET is called to obtain EL, the vector of coefficients of
                    ! length NQ + 1. RC is the ratio of new to old values of the
                    ! coefficient H/EL(2)
                    CALL COSET (NQ,EL,TQ)
                    RC = RC * EL(1) / OLDL0
                    OLDL0 = EL(1)
                    FN = REAL(NEQ)
                    EDN = FN * (TQ(1) * EPS) ** 2
                    E = FN * (TQ(2) * EPS) ** 2
                    EUP = FN * (TQ(3) * EPS) ** 2
                    BND = FN * (TQ(4) * EPS) ** 2
                    RH = MAX(RH, HMIN / ABS(H))
                    RH = MIN(RH, HMAX / ABS(H), RMAX)
                    R1 = 1.0_r8k
                    DO J = 2, L
                        R1 = R1 * RH
                        DO I = 1, NEQ
                            Y(I,J) = Y(I,J) * R1
                        END DO
                    END DO
                    H = H * RH
                    RC = RC * RH
                    IDOUB = L + 1
                    IF (IREDO == 0) THEN
                        RMAX = 10.0_r8k
                        HOLD = H
                        JSTART = NQ
                        RETURN
                    END IF
                    IF (ABS(RC - 1.0_r8k) > 0.3_r8k) IWEVAL = MITER
                    IF (nstep_driver >= NSTEPJ + 20) IWEVAL = MITER
                    T = T + H
                    ! this section computes the predicted values by effectively
                    ! multiplying the Y array by the pascal triangle matrix
                    DO J1 = 1, NQ
                        DO J2 = J1, NQ
                            J = (NQ + J1) - J2
                            DO i = 1, NEQ
                                Y(I,J) = Y(I,J)+Y(I,J+1)
                            END DO
                        END DO
                    END DO
                    CYCLE
                ELSE IF (PR2 <= PR1) THEN
                    NEWQ = NQ
                    RH = 1.0_r8k / PR2
                ELSE
                    NEWQ = NQ - 1
                    RH = 1.0_r8k / PR1
                END IF
                IF ((KFLAG == 0) .AND. (RH < 1.1_r8k)) THEN
                    IDOUB = 10
                    HOLD = H
                    JSTART = NQ
                    RETURN
                END IF
                ! a change in NQ up or down by 1 is considered if L = 0.
                ! if L = 1 and NQ < MAXDER, then ERROR is saved
                ! for use in a possible order increase on the next step_frapcon.
                ! a change in H or NQ is made only of the increase in H
                ! is by a factor of at least 1.3.
                ! if not, L is set to 2 to prevent testing for that many
                ! steps. if NQ is changed, L is set to NQ + 1 (new value).  
                IF (NEWQ == NQ) THEN
                    RH = MAX(RH, HMIN / ABS(H))
                ELSE
                    NQ = NEWQ
                    L = NQ + 1
                    ! COSET is called to obtain EL, the vector of coefficients of
                    ! length NQ + 1. RC is the ratio of new to old values of the
                    ! coefficient H/EL(2)
                    CALL COSET (NQ,EL,TQ)
                    RC = RC * EL(1) / OLDL0
                    OLDL0 = EL(1)
                    FN = REAL(NEQ)
                    EDN = FN * (TQ(1) * EPS) ** 2
                    E = FN * (TQ(2) * EPS) ** 2
                    EUP = FN * (TQ(3) * EPS) ** 2
                    BND = FN * (TQ(4) * EPS) ** 2
                    RH = MAX(RH, HMIN / ABS(H))
                END IF
                RH = MIN(RH, HMAX / ABS(H), RMAX)
                R1 = 1.0_r8k
                DO j = 2, L
                    R1 = R1 * RH
                    DO i = 1, NEQ
                        Y(I,J) = Y(I,J) * R1
                    END DO
                END DO
                H = H * RH
                RC = RC * RH
                IDOUB = L + 1
                IF (IREDO == 0) THEN
                    RMAX = 10.0_r8k
                    HOLD = H
                    JSTART = NQ
                    RETURN
                END IF
                IF (ABS(RC - 1.0_r8k) > 0.3_r8k) IWEVAL = MITER
                IF (nstep_driver >= NSTEPJ + 20) IWEVAL = MITER
                T = T + H
                ! this section computes the predicted values by effectively
                ! multiplying the Y array by the pascal triangle matrix
                DO J1 = 1, NQ
                    DO J2 = J1, NQ
                        J = (NQ + J1) - J2
                        DO i = 1, NEQ
                            Y(I,J) = Y(I,J)+Y(I,J+1)
                        END DO
                    END DO
                END DO
            END IF
            CYCLE
        END SELECT
    END DO
    !
    END SUBROUTINE STIFF
    !
    !
    !
    SUBROUTINE DIFFUN (Y, YDOT)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    ! CREEP ADDED FOR REVISED COBLE AND CLIMB CREEP EQUATIONS WITH DG WIDTH
    !> @brief
    !> DIFFUN calculated temperature and stress and then determine creep rates, dominant
    !> creep rate, and rupture times
    !> @author
    !> Coded by Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    !********************************************* 
    !*** DATE BLOCKS COMMUNICATE INPUT, OUTPUT *** 
    !*** AND CONSTANTS BETWEEN MAIN AND DIFFUN *** 
    !********************************************* 
    !************************************************************* 
    !*** FLAG BLOCK COMMUNICATES FLAGGED CONDITIONS BETWEEN    *** 
    !*** MAIN AND DIFFUN                                       *** 
    !***   MFLAG  - CREEP RATE AND RUPTURE MECHANISM           *** 
    !***   NFLAG  - ATHERMAL CREEP ASSUMED DURING CALCULATION  *** 
    !***   NFLAGT - ALPHA/BETA TRANSITION TEMPERATURE EXCEEDED *** 
    !***   NFLAGS - YIELD STRESS EXCEEDED                      *** 
    !*************************************************************
    !
    ! Input
    !
    ! Y         - Solution vector: creep strain, recovery fraction, and creep damage
    !
    ! Output
    !
    ! YDOT      - Rate vector: dominant creep, radiation damage annealing, and creep damage fraction accumulation
    !
    ! Internal
    !
    ! DG        - Grain boundary width (m)
    ! EDOTOT    - transient creep
    ! EM, EM1   - Used in determining low temperature climb and grain boundary sliding creep rates
    ! SLOPE     - Temperature input slope
    ! SLOPES    - Stress input slope
    ! TM        - Cladding melting temperature (K)
    ! TTX       - Input to the transient creep strain rate
    ! YS        - Yield stress
    !
    REAL(r8k), DIMENSION(NEQ), INTENT(IN) :: Y
    REAL(r8k), DIMENSION(NEQ), INTENT(OUT) :: YDOT
    !
    INTEGER(ipk) :: IP, JER, JJER
    INTEGER(ipk) :: LTMAX       ! Used to calculate the yield stress: LTMAX = TMAXL/100
    INTEGER(ipk) :: TMAXL       ! Used to calculate the yield stress: temperature rounded down to the nearest multiple of 100 K
    REAL(r8k) :: DG, EM, EM1, EDOTR, SLOPE, SLOPES, TCC, TK, TZ, TB, TCA, TCB, TKA, TKB, TMINR, &
      &          TTX, EDOTOT, YS, ZZ, TMTK, TMTK1, TMTK2, EMA, ZA
    REAL(r8k), DIMENSION(7) :: ER                       ! Creep Rates
    REAL(r8k), PARAMETER :: TM = 2125.28_r8k
    !
    ! T IS TIME IN SECONDS
    ! TY IS IN YEARS
    TY = T / yrtosec
    !****************************************
    !*** CALCULATE TEMPERATURE AND STRESS ***
    !****************************************
    !
    SELECT CASE (ncreephist)
    CASE (1)    ! HELIUM COOLING
        TZ = (TMAX + 273.16_r8k) * (creeppooltime * 12.0_r8k) ** (0.34_r8k)
        IF (creeppooltime > 7.0_r8k) TZ = (TMAX + 273.16_r8k) * 84.0_r8k ** 0.34_r8k / 84.0_r8k ** 0.084_r8k * &
          &   (creeppooltime * 12.0_r8k) ** 0.084_r8k
        TB = TZ * 84.0_r8k ** (-0.34_r8k) / (84.0_r8k ** (-0.084_r8k))
        TCA = TZ * (creeppooltime * 12.0_r8k + T * 12.0_r8k / yrtosec) ** (-0.34_r8k)
        TCB = TB * (creeppooltime * 12.0_r8k + T * 12.0_r8k / yrtosec) ** (-0.084_r8k)
        TK = MAX(TCA, TCB)                      ! Cladding Temperature in Kelvin
        TC = TK  -273.16_r8k                    ! Cladding Temperature in Celsius
        ST = FSTIN * TK / (TMAX + 273.16_r8k)    ! Cladding Stress
    CASE (2)    ! NITROGEN COOLING
        TZ = EXP(A02 + A12 * LOG(creeppooltime)) + 273.16_r8k
        IF (creeppooltime > 5.629_r8k) TZ = EXP(A06 + A16 * LOG(creeppooltime)) + 273.16_r8k
        TKA = (EXP(A02 + A12 * LOG(creeppooltime + TY)) + 273.16_r8k) / TZ * (TMAX + 273.16_r8k)
        TKB = (EXP(A06 + A16 * LOG(creeppooltime + TY)) + 273.16_r8k) / TZ * (TMAX + 273.16_r8k)
        TK = MAX(TKA, TKB)                      ! Cladding Temperature in Kelvin
        TC = TK - 273.16_r8k                    ! Cladding Temperature in Celsius
        ST = FSTIN * TK / (TMAX + 273.16_r8k)    ! Cladding Stress
    CASE (3, 4) ! TABLE TEMPERATURE DEPENDENCE
        DO IP = 1, ncreeptab - 1
            IF (TY <= creeptabtime(IP+1)) THEN
                ! Temperature slope
                SLOPE = (creeptabtemp(IP+1) - creeptabtemp(IP)) / (creeptabtime(IP+1) - creeptabtime(IP))
                ! Temperature Value
                TCC = creeptabtemp(IP) + SLOPE * (TY - creeptabtime(IP))
                ! Stress slope
                SLOPES = (creeptabstress(IP+1) - creeptabstress(IP)) / (creeptabtime(IP+1) - creeptabtime(IP))
                ! Stress value
                ST = creeptabstress(IP) + SLOPES * (TY - creeptabtime(IP))
                EXIT
            END IF
        END DO
        IF (IP == ncreeptab) THEN
            TCC = creeptabtemp(ncreeptab)
            ST = creeptabstress(ncreeptab)
        END IF
        IF (ncreephist == 4) TZTBL = TMAX + 273.16_r8k
        ! Calculate stress change from temperature cooling for ncreephist = 3
        ! ncreephist = 4 uses stress magnitudes provided by input
        ! Calculate cladding temperature in Kelvin (TK) and Celsius (TC)
        TK = (TMAX + 273.16_r8k) / TZTBL * (TCC + 273.16_r8k)
        TC = TK - 273.16_r8k
        ! Cladding stress
        IF (ncreephist == 3) ST = FSTIN * TK / (TMAX + 273.16_r8k)
    END SELECT
    !
    ! CHECK FOR TEMPERATURE GREATER THEN ALPHA-BETA TRANSITION TEMPERATURE
    IF (TC > TEMPAB) NFLAGT = 1
    LTMAX = INT(TC / 100.0_r8k)
    TMAXL = LTMAX * 100
    ! CHECK FOR STRESS GREATER THAN YIELD STRESS
    IF (TC < 100.0_r8k) THEN
        YS = -0.735_r8k * TC + 460.6_r8k
    ELSE IF(TC > 700.0_r8k) THEN
        YS = -0.126_r8k * TC + 134.3_r8k
    ELSE
        YS = YSA(LTMAX) + (TC - TMAXL) * (YSA(LTMAX+1) - YSA(LTMAX)) / 100.0_r8k
    END IF
    IF (ST > YS) NFLAGS = 1
    IF (TC < 250.0_r8k) THEN
        EM = (11.81_r8k - 14.59_r8k * TK / TM) * 1.0E4_r8k
    ELSE
        EM = (11.09_r8k - 11.61_r8k * TK / TM) * 1.0E4_r8k
    END IF
    EM1 = LOG(EM / 1.0E4_r8k)
    !*****************************
    !*** CALCULATE CREEP RATES ***
    !*****************************
    ! DG IS THE GRAIN BOUNDARY WIDTH IN METERS
    DG = 6.0E-6_r8k
    RECOV = 1.0_r8k / (1.0_r8k + Y(2))
    ! ER(1) = HIGH TEMPERATURE CLIMB (NOT CALCULATED)
    ER(1) = 0.0_r8k
    ! ER(2) = LOW TEMPERATURE CLIMB
    ER(2) = 2.4E5_r8k * EM / TK * EXP(-185.0E3_r8k / (R_JmolK * TK)) * DSINH(420.0_r8k * ST / EM) ** 5 &
        &     / (1.0_r8k + 200.0_r8k * (1.0_r8k - 1.0_r8k / DCOSH(RECOV * FLUE / 5.0E21_r8k)))
    ! ER(3) = GRAIN BOUNDARY SLIDING
    ER(3) = 30.0_r8k * EM * 3.23E-10_r8k / (1.38E-29_r8k * TK) * 4.2E-13_r8k / 2.0_r8k / 3.23E-10_r8k &
        &     * EXP(-167.0E3_r8k / (R_JmolK * TK)) * (3.23E-10_r8k / (1.776_r8k * DG)) ** 2 * (ST / EM) ** 2 &
        &     / (1.0_r8k + 10.0_r8k * (1.0_r8k - 1.0_r8k / DCOSH(RECOV * FLUE / 5.0E21_r8k)))
    ! ER(4) = GRAIN BOUNDARY LATTICE DIFFUSION CONTROL (NOT CALCULATED)
    ER(4) = 0.0_r8k
    ! ER(5) = NABORRO HERRING (NOT CALCULATED)
    ER(5) = 0.0_r8k
    ! ER(6) = COBLE
    ER(6) = 150.0_r8k / 3.1416_r8k * 3.23E-10_r8k / (1.38E-29_r8k * TK) * 4.2E-13_r8k / 2.0_r8k / 3.23E-10_r8k * &
        &     EXP(-167.0E3_r8k / (R_JmolK * TK)) * (3.23E-10_r8k / (1.776_r8k * DG)) ** 3 * ST
    ! ER(7) = ATHERMAL (CALCULATED LATER BASED ON STRESS AND TEMPERATURE)
    ER(7) = 0.0_r8k
    !*************************************
    !*** DETERMINE DoMINANT CREEP RATE ***
    !*************************************
    EDOTR = ER(1)
    JJER = 10
    DO JER = 2, 6
        IF (ER(JER) > EDOTR) THEN
            EDOTR = ER(JER)
            JJER = JER * 10
        END IF
    END DO
    ZZ = ST / EM
    TMTK = TM / TK
    TMTK1 = (1.62_r8k - LOG(ZZ)) / 1.827_r8k
    TMTK2 = (-4.171_r8k - LOG(ZZ)) / 0.547_r8k
    IF (LOG(ZZ) > -6.644_r8k) THEN
        IF (TMTK > TMTK1) THEN
            EMA = (11.09_r8k - 11.61_r8k / TMTK1) * 1.0E4_r8k
            IF (TMTK1 > 4.06_r8k) EMA = (11.81_r8k - 14.59_r8k / TMTK1) * 1.0E4_r8k
            ZA = ST / EMA
            EDOTR = EXP(7.0_r8k * LOG(ZA) + 55.18_r8k - 10.19_r8k * TMTK1 + LOG(TMTK1) + LOG(EMA / 1.0E4_r8k))
            NFLAG = 1
            ER(7) = EDOTR
            JJER = 70
        END IF
    ELSE
        IF (TMTK > TMTK2) THEN
            EMA = (11.09_r8k - 11.61_r8k / TMTK2) * 1.0E4_r8k
            IF (TMTK2 > 4.06_r8k) EMA = (11.81_r8k - 14.59_r8k / TMTK2) * 1.0E4_r8k
            ZA = ST / EMA
            EDOTR = EXP(7.0_r8k * LOG(ZA) + 55.18_r8k - 10.19_r8k * TMTK1 + LOG(TMTK1) + LOG(EMA / 1.0E4_r8k))
            NFLAG = 1
            ER(7) = EDOTR
            JJER = 70
        END IF
    END IF
    ! **********************************
    ! *** DETERMINE TOTAL CREEP RATE ***
    ! **********************************
    EDOTR = ER(2) + ER(3) + ER(6)
    IF (idatingcreep == 3) EDOTR = (ER(2) + ER(3) + ER(6)) / 5.0_r8k
    ! *********************************************
    ! *** CALCULATE MONKMAN-GRANT RUPTURE TIMES ***
    ! *********************************************
    TMINR = 0.02_r8k * EXP(6.205_r8k - 4300.9_r8k / TK) / EDOTR
    IF (idatingcreep > 1) TMINR = 0.0366_r8k * EXP(6.205_r8k - 4300.9_r8k / TK) / EDOTR
    ! MFLAG DEFINES THE CREEP MECHANISMS
    MFLAG = JJER
    TTX = T + 1.0E-2_r8k
    ! EDOTOT INCLUDES THE TRANSIENT STRAIN
    EDOTOT = EDOTR + (1.37108_r8k * EDOTR ** 1.109_r8k) / (EXP(52.0_r8k * SQRT(EDOTR * TTX)) &
        &      * SQRT(EDOTR * TTX) * (2.0_r8k - DTANH(1.278E8_r8k * EDOTR)) ** 2.05_r8k)
    ! *****************************************
    ! *** CALCULATE RATES REQUESTED BY GEAR ***
    ! *****************************************
    ! YDOT(1) IS THE DoMINANT CREEP RATE
    YDOT(1) = EDOTOT
    ! YDOT(2) IS THE RATE OF RADIATION DAMAGE ANNEALING
    YDOT(2) = 5.0E21_r8k * EXP(-4.6444E4_r8k / TK)
    ! YDOT(3) IS THE RATE OF ACCUMULATION OF CREEP DAMAGE FRACTION
    YDOT(3) = 1.0_r8k / TMINR
    !
    END SUBROUTINE DIFFUN
    !
    !
    !
    SUBROUTINE COSET (NQ, EL, TQ)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> COSET is called by STIFF and sets coefficients for use there_frapcon.
    !>
    !>for each order NQ, the coefficients in EL are contained in the PERTST array
    !>
    !> coset also used to set MAXDER, the maximum order of the formulas
    !> available. currently this is 5 for the backward differentiation
    !> formulas, and has been set as a parameter in this module
    !>
    !> in addition to variables described previously, communication
    !> with coset uses the following..
    !> EL =      a vector of length 6 in which COSET stores the
    !>           coefficients for the corrector formula.
    !> TQ =      a vector of length 4 in which COSET stores constants
    !>           used for the convergence test, the error test, and
    !>           selection of H at a new order.
    !> NQ =      the current order.
    !>
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! Procedure arguments
    INTEGER(ipk), INTENT(IN) :: NQ  ! current order of the method
    REAL(r8k), DIMENSION(LMAX), INTENT(OUT) :: EL   ! array of coefficients for the corrector formula
    REAL(r8k), DIMENSION(4), INTENT(OUT) :: TQ      ! vector of constants used for the convergence test,
                                                    !   the error test, and selection of H at a new order
    !
    INTEGER(ipk) :: K               ! DO loop counter
    REAL(r8k), DIMENSION(12,2,3), PARAMETER :: PERTST = reshape ( &   ! array of pre-calculated constants for TQ
        & (/ 1.0_r8k, 1.0_r8k, 2.0_r8k, 1.0_r8k, 0.3158_r8k, 0.07407_r8k, 0.01391_r8k, 0.002182_r8k, &
        &    0.0002945_r8k, 0.00003492_r8k, 0.000003692_r8k, 0.0000003524_r8k, 1.0_r8k, 1.0_r8k, &
        &    0.5_r8k, 0.1667_r8k, 0.04167_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, &
        &    1.0_r8k, 2.0_r8k, 12.0_r8k, 24.0_r8k, 37.89_r8k, 53.33_r8k, 70.08_r8k, 87.97_r8k, &
        &    106.9_r8k, 126.7_r8k, 147.4_r8k, 168.8_r8k, 191.0_r8k, 2.0_r8k, 4.5_r8k, 7.333_r8k, &
        &    10.42_r8k, 13.7_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, &
        &    12.0_r8k, 24.0_r8k, 37.89_r8k, 53.33_r8k, 70.08_r8k, 87.97_r8k, 106.9_r8k, 126.7_r8k, &
        &    147.4_r8k, 168.8_r8k, 191.0_r8k, 1.0_r8k, 3.0_r8k, 6.0_r8k, 9.167_r8k, 12.5_r8k, 1.0_r8k, &
        &    1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k /), &
        &    (/ 12,2,3 /))
    !
    SELECT CASE (NQ)
    CASE (1)
        EL(1) = 1.0_r8k
    CASE (2)
        EL(1) = 6.6666666666667E-1_r8k
        EL(3) = 3.3333333333333E-1_r8k
    CASE (3)
        EL(1) = 5.4545454545455E-1_r8k
        EL(3) = EL(1)
        EL(4) = 9.0909090909091E-2_r8k
    CASE (4)
        EL(1) = 0.48_r8k
        EL(3) = 0.7_r8k
        EL(4) = 0.2_r8k
        EL(5) = 0.02_r8k
    CASE (5)
        EL(1) = 4.3795620437956E-1_r8k
        EL(3) = 8.2116788321168E-1_r8k
        EL(4) = 3.1021897810219E-1_r8k
        EL(5) = 5.4744525547445E-2_r8k
        EL(6) = 3.6496350364964E-3_r8k
    END SELECT
    DO K = 1, 3
        TQ(K) = PERTST(NQ,METH,K)
    END DO
    TQ(4) = 0.5_r8k * TQ(2) / REAL(NQ + 2)
    !
    END SUBROUTINE COSET
    !
    !
    !
    SUBROUTINE PSET (Y, CON, IER)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> PSET is called by STIFF to compute and to process the matrix PW = I - (H/EL(2))*J, where J is an approximation to the
    !> jacobian. J is computed by finite differences, when MITER = 2. J is stored in PW, using CON = -H/EL(2).
    !> Then PW is subjected to an LU decomposition for later solution of linear algebraic systems with PW as the
    !> coefficient matrix.
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! Internal
    !
    ! Y     - NEQ by LMAX array containing the dependent variables and their scaled derivatives
    ! IER   - Argument for the DEC subroutine: singularity indicator
    ! D, R  - SRSS error and total relative error
    ! R0    - Used in relative local error calculations
    ! YJ    - Y(J,I)
    !
    INTEGER(ipk) :: I, J, J1, J3
    INTEGER(ipk), INTENT(INOUT) :: IER
    REAL(r8k) :: D, R, R0, YJ
    REAL(r8k), INTENT(IN) :: CON
    REAL(r8k), DIMENSION(NEQ,LMAX), INTENT(INOUT) :: Y
    !
    D = 0.0_r8k
    DO I = 1, NEQ
        D = D + SAVE2(I) ** 2
    END DO
    R0 = ABS(H) * SQRT(D) * 1.0_r8k + 3 * UROUND
    J1 = 0
    DO J = 1, NEQ
        YJ = Y(J,1)
        R = EPSJ * YMAX(J)
        R = MAX(R,R0)
        Y(J,1) = Y(J,1) + R
        D = CON / R
        CALL DIFFUN (Y, SAVE1)
        DO I = 1, NEQ
            J3 = I + J1
            PW(J3) = (SAVE1(I) - SAVE2(I)) * D
        END DO
        Y(J,1) = YJ
        J1 = J1 + NEQ
    END DO
    J = 1
    DO I = 1, NEQ
        PW(J) = PW(J) + 1.0_r8k
        J = J + (NEQ + 1)
    END DO
    CALL DEC (PW, IPIV, IER)
    !
    END SUBROUTINE PSET
    !
    !
    !
    SUBROUTINE DEC (A, IP, IER)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> DEC performs matrix triangularization by gaussian elimination.
    !>
    !> input..
    !> NEQ =    order of matrix.
    !> NEQ =    declared dimension of array a .
    !> A =      matrix to be triangularized.
    !>
    !> output..
    !> A(I,J), I<=J = upper triangular factor, U .
    !> A(I,J), I>J  = multipliers = lower triangular factor, I - L.
    !> IP(K),  K<N  = index of K-th pivot row.
    !> IP(N)        = (-1)**(number of interchanges) or o .
    !> IER          = 0 if a nonsingular, or K if A found to be
    !>              singular at stage k.
    !> use SOL to obtain solution of linear system_frapcon.
    !> DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
    !> if IP(N)=0, A is singular, SOL will divide by zero.
    !> interchanges finished in U , only partly in L .
    !> 
    !> reference..
    !> c. b. moler, algorithm 423, linear equation solver,
    !> comm. assoc. comput. mach., 15 (1972), p. 274.
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    ! Internal
    !
    ! T   - Intermediate real quantity used in triangularization and gaussian elimination steps
    ! Procedure arguments
    INTEGER(ipk) :: I, M, K, J
    INTEGER(ipk), INTENT(OUT) :: IER                    ! argument for the DEC subroutine: singularity indicator
    REAL(r8k) :: T
    INTEGER(ipk), DIMENSION(NEQ), INTENT(OUT) :: IP     ! vector of pivot rows and interchange counter
    REAL(r8k), DIMENSION(NEQ,NEQ), INTENT(INOUT) :: A   ! matrix to be triangularized
    !
    IER = 0
    IP(NEQ) = 1
    DO K = 1, NEQ - 1
        M = K
        DO I = K + 1, NEQ
            IF (ABS(A(I,K)) > ABS(A(M,K))) M = I
        END DO
        IP(K) = M
        T = A(M,K)
        IF (M /= K) THEN
            IP(NEQ) = -IP(NEQ)
            A(M,K) = A(K,K)
            A(K,K) = T
        END IF
        IF (T == 0.0_r8k) THEN
            IER = K
            IP(NEQ) = 0
            RETURN
        END IF
        T = 1.0_r8k / T
        DO I = K + 1, NEQ
            A(I,K) = -A(I,K) * T
        END DO
        DO J = K + 1, NEQ
            T = A(M,J)
            A(M,J) = A(K,J)
            A(K,J) = T
            IF (T /= 0.0_r8k) THEN
                DO I = K + 1, NEQ
                    A(I,J) = A(I,J) + A(I,K) * T
                END DO
            END IF
        END DO
    END DO
    K = NEQ
    IF (A(NEQ,NEQ) /= 0.0_r8k) RETURN
    IER = K
    IP(NEQ) = 0
    !
    END SUBROUTINE DEC
    !
    !
    !
    SUBROUTINE SOL (A, B, IP)
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> SOL performs solution of linear system, A*X = B .
    !>
    !> input..
    !> NEQ =    order of matrix.
    !> NEQ =    declared dimension of array A .
    !> A =      triangularized matrix obtained from DEC.
    !> B =      right hand side vector.
    !> IP =     pivot vector obtained from DEC.
    !>          do not use if DEC has set IER_frapcon/=0.
    !>
    !> output..
    !> B =      solution vector, X .
    !> @author
    !> Coded by Ian Porter and Patrick Raynaud, NRC
    !> @date
    !> 8/10/2015
    !
    !
    REAL(r8k), DIMENSION(NEQ,NEQ), INTENT(IN) :: A  ! triangularized matrix obtained from DEC in the A*X=B matrix equation
    REAL(r8k), DIMENSION(NEQ), INTENT(INOUT) :: B   ! right hand side vector and result in the A*X=B matrix equation
    INTEGER(ipk), DIMENSION(NEQ), INTENT(IN) :: IP  ! pivot vector obtained from DEC
    !
    INTEGER(ipk) :: K, KB, M, I     ! DO loop counters and indices
    REAL(r8k) :: T                  ! intermediate real quantity used solving process
    !
    IF (NEQ /= 1) THEN
        DO K = 1, NEQ - 1
            M = IP(K)
            T = B(M)
            B(M) = B(K)
            B(K) = T
            DO I = K + 1, NEQ
                B(I) = B(I) + A(I,K) * T
            END DO
        END DO
        DO KB = 1, NEQ - 1
            K = NEQ - KB + 1
            B(K) = B(K) / A(K,K)
            T = -B(K)
            DO I = 1, NEQ - KB
                B(I) = B(I) + A(I,K) * T
            END DO
        END DO
    END IF
    B(1) = B(1) / A(1,1)
    !
    END SUBROUTINE SOL
    !
    !
    !
    SUBROUTINE Allocate_DatingData
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> Allocate_DatingData allocates the arrays used by the Dating module based on the user input_frapcon
    !> value for ncreeptab
    !> @author
    !> Coded by Ian Porter, NRC
    !> @date
    !> 3/4/2014
    !
    INTEGER(ipk) :: maxsize
    ! Set a default minimum value for ncreeptab
    maxsize = ncreeptab
    !
    ALLOCATE (creeptabtime(1:maxsize))
    ALLOCATE (creeptabtemp(1:maxsize))
    ALLOCATE (creeptabstress(1:maxsize))
    ALLOCATE (YMAX(1:NEQ))
    ALLOCATE (ERROR(1:NEQ))
    ALLOCATE (SAVE1(1:NEQ))
    ALLOCATE (SAVE2(1:NEQ))
    ALLOCATE (PW(1:NEQ*NEQ))
    ALLOCATE (IPIV(1:NEQ))
    ! Supply Initial Values of 0
    creeptabtime = 0.0_r8k
    creeptabtemp = 0.0_r8k
    creeptabstress = 0.0_r8k
    YMAX = 0.0_r8k
    ERROR = 0.0_r8k
    SAVE1 = 0.0_r8k
    SAVE2 = 0.0_r8k
    PW = 0.0_r8k
    IPIV = 0
    !
    END SUBROUTINE Allocate_DatingData
    !
END MODULE DatingData_frapcon




