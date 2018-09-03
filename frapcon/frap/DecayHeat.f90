MODULE Decay
    USE Kinds
    USE Conversions
    USE Functions
    USE Variables, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> This module contains decay heat data and calculations
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/26/2015
    !
    !
    INTEGER(ipk), TARGET :: DecayModel = 1                               ! Decay model used. 1 = ANS-5.1-2004 (Default)
    INTEGER(ipk), PRIVATE :: NDecays = 0                                 ! Total # of timesteps associated with decay calculation
    REAL(r8k), TARGET :: fpdcay = 1.0_r8k                                ! User specified decay heat multiplier (Default = 1.0)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: OperationTime       ! Time reactor was in operation for each timestep (s)
    REAL(r8k), DIMENSION(:), ALLOCATABLE, PRIVATE :: ReactorPower        ! Power reactor operated at over time interval (it)
    !
    CONTAINS
    !
    !
    !
    SUBROUTINE DecayHeatSetup
    USE Kinds
    USE Conversions
    USE Variables, ONLY : im, qmpy, ProblemTime, it, na, jst
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called during the initial read and allocates the arrays used for decay heat calculations,
    !> as well as determines the time and power at which decay heating is required to be calculated
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 8/18/2015
    !
    ! Input
    !
    ! powop - operating power at time of LOCA initiation (kW/ft) (averaged over fuel rod length)
    ! pkw   - fuel rod linear power at axial elevation y (kW/ft)
    ! pkw0  - linear power at start of time step (kW/ft)
    !
    ! Output
    !
    ! qmpy    - average rod power during decay timestep
    !
    ! Internal
    !
    ! t0    - time rods were operating at power (s)
    ! ts    - time after shutdown (s)
    ! frac  - power fraction
    ! dt    - time-step size in iteration
    !
    INTEGER(ipk) :: i, k
    REAL(r8k) :: t0, ts, frac, dt
    REAL(r8k), DIMENSION(na-1) :: pkw
    ! Determine the # of decay timesteps (Note: Skip over ProblemTime(0) because must have a power step to have decay)
    NDecays = COUNT(qmpy(1:im) < 0.0_r8k)
    IF (NDecays > 0) THEN ! Decay calculation has been requested by user
        ! Allocate the variables associated with DecayHeat and set the problemtimes for the start/stop of decay heating
        WRITE (0,100)
        WRITE (ounit,100)
100     FORMAT ('Decay heating model will be used.')
        CALL AllocateDecayHeat
        ! Assign start and stop times for decay calculation(s)
        DO i = 1, im
            IF (qmpy(i) < 0.0_r8k) THEN ! Reactor is shutdown
                ReactorPower(i) = 0.0_r8k
            ELSE ! Reactor is operating
                ReactorPower(i) = qmpy(i)
            END IF
            OperationTime(i) = ProblemTime(i) - ProblemTime(i-1)
        END DO
        !
    END IF
    !
    END SUBROUTINE DecayHeatSetup
    !
    !
    !
    SUBROUTINE DecayHeat
    USE Kinds
    USE Conversions
    USE Variables, ONLY : im, qmpy, ProblemTime, it, na, qaxnorm, dco, jst, jpeak
    IMPLICIT NONE
    !>@brief
    !> Subroutine DecayHeat calculates the average decay heat fraction after shutdown over a timestep (it)
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/26/2015
    !
    ! Input
    !
    ! powop - operating power at time of LOCA initiation (kW/ft) (averaged over fuel rod length)
    ! pkw   - fuel rod linear power at axial elevation y (kW/ft)
    ! pkw0  - linear power at start of time step (kW/ft)
    !
    ! Output
    !
    ! qmpy    - average rod power during decay timestep
    ! qaxnorm - normalized axial power profile during decay timestep
    !
    ! Internal
    !
    ! t0    - time rods were operating at power (s)
    ! ts    - time after shutdown (s)
    ! frac  - power fraction
    ! dt    - time-step size in iteration
    !
    INTEGER(ipk) :: k, i, ii
    REAL(r8k) :: t0, ts, frac, dt
    REAL(r8k), DIMENSION(na-1) :: pkw
    !
    ! Find time when operation started 
    ! (Note: This may not be the last problem time if modeling multiple steps during a single decay)
    i = 0
    DO WHILE (ReactorPower(it-i) <= 0.0_r8k)
        i = i + 1
    END DO
    t0 = ProblemTime(it-i)
    !
    DO k = 1, (na - 1)
        frac = 0.0_r8k
        ! Find time after shutdown/start of decay (Same note as above for t0)
        ii = 0
        ts = 0.0_r8k
        DO WHILE (ReactorPower(it-ii) <= 0.0_r8k .AND. ReactorPower(it-(ii+1)) <= 0.0_r8k)
            ts = ts + (ProblemTime(it-(ii+1)) - ProblemTime(it-(ii+2)))
            ii = ii + 1
        END DO
        DO WHILE (ProblemTime(it-i) + ts < ProblemTime(it))
            ! Change the timestep size based on the known changes in the decay power curve (to speed up calculation)
            IF (ts < 10.0_r8k) THEN
                dt = 0.1_r8k
            ELSE IF (ts < 1000.0_r8k) THEN
                dt = 10.0_r8k
            ELSE IF (ts < 1.0E5_r8k) THEN
                dt = 1000.0_r8k
            ELSE
                dt = 1.0E5_r8k
            END IF
            ! Ensure dt will not exceed the end of the problem timestep
            IF (ProblemTime(it-i) + ts + dt > ProblemTime(it)) dt = ProblemTime(it) - ProblemTime(it-i) - ts
            ! Time after shutdown, s
            ts = ts + dt
            ! Calculate decay heat fraction, frac, based on user selected model
            SELECT CASE (DecayModel)
            CASE (1)
                frac = frac + fans51(t0, ts, k) * dt / (ProblemTime(it) - ProblemTime(it-1))
            CASE DEFAULT
                WRITE (0, 100) DecayModel
                WRITE (ounit, 100) DecayModel
100             FORMAT ('Error in Subroutine DecayHeat. Improper Value for Decay Heat Model.', &
                  &     'DecayModel = ',i2,' which is outside of bounds (DecayModel=1 for ANS-5.1-2004')
                STOP
            END SELECT
        END DO
        ! Set the axial power * decay fraction
        pkw(k) = frac * fpdcay
    END DO
    ! Update the rod average power
    qmpy(it) = SUM(pkw(1:na-1)) / REAL(na-1)
    ! Update the axial power profile
    ! Check to see if there are enough spaces in the axial power profile array (qaxnorm).
    ! If not, add more spaces to hold new profile. IF so, overwrite any existing power profile for the decay step.
    IF (MAXVAL(jst) < im) THEN
        CALL ReAllocateArray2D(na, na, 1, MAXVAL(jst), MAXVAL(jst)+1, 1, qaxnorm)
        jst(it) = MAXVAL(jst) + 1
    END IF
    DO k = 1, (na - 1)
        qaxnorm(k, jst(it)) = pkw(k) / qmpy(it)
    END DO
    ! Set new peak node
    jpeak(jst(it)) = MAXLOC(pkw,DIM=1) + 1
    !
    END SUBROUTINE DecayHeat
    !
    !
    !
    REAL(r8k) FUNCTION fans51 (t0, ts, AxNode) RESULT (Pd)
    USE Kinds
    USE Conversions
    USE Variables, ONLY : jst, it, qmpy, qaxnorm, ProblemTime
    USE Comde
    IMPLICIT NONE
    !>@brief
    !> This is the function for the ANS 5.1 decay heat correlation.
    !>@author
    !> Developed by Ken Geelhood, PNNL
    !> Modified by Ian Porter, NRC
    !>@date
    !> May 2015
    !
    ! Input
    !
    ! t0        - Time of operation (s)
    ! ts        - Time since reactor shutdown (s)
    ! AxNode    - Axial node being analyzed
    !
    ! Output
    !
    ! Pd        - Fraction of decay heat power
    !
    ! Internal
    !
    ! FIFA      - Fissions per initial fisile atom (dimensionless)
    ! OpTime    - Time reactor was operating (s)
    ! Pd_uncor  - Fraction of decay heat power uncorrected for neutron capture in fission proucts (MeV/s)
    ! Q         - Total recoverable energy associated with one fission of nuclide(i), (MeV/fission)
    ! R         - atoms of U239 produced per second per fission per second for the reactor composition
    !             at the time of shutdown
    !
    ! Reference:
    !
    ! American National Standard for Decay Heat Power in Light Water Reactors (ANSI/ANS-5.1-2004)
    ! Data taken for thermal fissions
    !
    INTEGER(ipk) :: i, ii, a, l, OperationOffset, DecayOffset
    INTEGER(ipk), INTENT(IN) :: AxNode
    INTEGER(ipk), PARAMETER :: NGroups = 23
    INTEGER(ipk), PARAMETER :: NNuclides = 4
    REAL(r8k) :: t00, Pmax, ShutDownTime
    REAL(r8k), INTENT(IN) :: t0, ts
    REAL(r8k), PARAMETER :: FIFA = 1.0_r8k
    REAL(r8k), PARAMETER :: MeVFission = 200.0_r8k
    REAL(r8k), PARAMETER :: R = 0.6_r8k
    REAL(r8k), PARAMETER :: Tinfinity = 1.0E13_r8k
    REAL(r8k), DIMENSION(NNuclides) :: FractionalPower, AtomFraction, Q, Pd_uncor
    REAL(r8k), DIMENSION(NNuclides,im) :: Power
    REAL(r8k), DIMENSION(NNuclides,na-1,0:im) :: NuclidePowerFraction
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: alphaU235 = (/ &
      &         5.2800E-04_r8k, 6.8588E-01_r8k, 4.0752E-01_r8k, 2.1937E-01_r8k, 5.7701E-02_r8k, &
      &         2.2530E-02_r8k, 3.3392E-03_r8k, 9.3667E-04_r8k, 8.0899E-04_r8k, 1.9572E-04_r8k, &
      &         3.2609E-05_r8k, 7.5827E-06_r8k, 2.5189E-06_r8k, 4.9836E-07_r8k, 1.8523E-07_r8k, &
      &         2.6592E-08_r8k, 2.2356E-09_r8k, 8.9582E-12_r8k, 8.5968E-11_r8k, 2.1072E-14_r8k, &
      &         7.1219E-16_r8k, 8.1126E-17_r8k, 9.4678E-17_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: lambdaU235 = (/ &
      &         2.7216E+00_r8k, 1.0256E+00_r8k, 3.1419E-01_r8k, 1.1788E-01_r8k, 3.4365E-02_r8k, &
      &         1.1762E-02_r8k, 3.6065E-03_r8k, 1.3963E-03_r8k, 6.2608E-04_r8k, 1.8924E-04_r8k, &
      &         5.5074E-05_r8k, 2.0971E-05_r8k, 9.9940E-06_r8k, 2.5401E-06_r8k, 6.6332E-07_r8k, &
      &         1.2281E-07_r8k, 2.7163E-08_r8k, 3.2955E-09_r8k, 7.4225E-10_r8k, 2.4681E-10_r8k, &
      &         1.5596E-13_r8k, 2.2573E-14_r8k, 2.0503E-14_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: alphaU238 = (/ &
      &         3.9368E-01_r8k, 7.4588E-01_r8k, 1.2169E+00_r8k, 5.2820E-01_r8k, 1.4805E-01_r8k, &
      &         4.5980E-02_r8k, 1.0406E-02_r8k, 1.6991E-03_r8k, 6.9102E-04_r8k, 1.4736E-04_r8k, &
      &         2.4049E-05_r8k, 6.9288E-06_r8k, 6.4927E-07_r8k, 4.3556E-07_r8k, 1.6020E-07_r8k, &
      &         2.3089E-08_r8k, 2.5481E-09_r8k, 3.5071E-11_r8k, 6.3399E-11_r8k, 4.1599E-14_r8k, &
      &         5.3295E-16_r8k, 1.6695E-18_r8k, 4.1058E-16_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: lambdaU238 = (/ &
      &         4.3427E+00_r8k, 1.7114E+00_r8k, 6.0572E-01_r8k, 1.9429E-01_r8k, 6.9788E-02_r8k, &
      &         1.8809E-02_r8k, 6.1265E-03_r8k, 1.3799E-03_r8k, 5.2799E-04_r8k, 1.6145E-04_r8k, &
      &         4.8419E-05_r8k, 1.5644E-05_r8k, 5.3610E-06_r8k, 2.1689E-06_r8k, 6.3343E-07_r8k, &
      &         1.2879E-07_r8k, 2.5604E-08_r8k, 9.1544E-09_r8k, 7.3940E-10_r8k, 2.4731E-10_r8k, &
      &         1.9594E-13_r8k, 6.4303E-14_r8k, 6.4229E-14_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: alphaPu239 = (/ &
      &         1.6540E-01_r8k, 3.6928E-01_r8k, 2.4006E-01_r8k, 1.0269E-01_r8k, 3.4916E-02_r8k, &
      &         2.2961E-02_r8k, 3.9070E-03_r8k, 1.3080E-03_r8k, 7.0265E-04_r8k, 1.4297E-04_r8k, &
      &         1.7642E-05_r8k, 7.3646E-06_r8k, 1.7720E-06_r8k, 5.4945E-07_r8k, 1.6736E-07_r8k, &
      &         2.1160E-08_r8k, 2.9388E-09_r8k, 1.3659E-10_r8k, 5.7450E-11_r8k, 3.8422E-14_r8k, &
      &         1.8030E-16_r8k, 1.8342E-15_r8k, 1.9884E-16_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: lambdaPu239 = (/ &
      &         8.9246E+00_r8k, 6.9005E-01_r8k, 2.3618E-01_r8k, 1.0118E-01_r8k, 3.7193E-02_r8k, &
      &         1.4319E-02_r8k, 4.5094E-03_r8k, 1.3211E-03_r8k, 5.3481E-04_r8k, 1.7297E-04_r8k, &
      &         4.8918E-05_r8k, 2.0155E-05_r8k, 8.3687E-06_r8k, 2.3620E-06_r8k, 6.4594E-07_r8k, &
      &         1.2822E-07_r8k, 2.5166E-08_r8k, 1.3176E-08_r8k, 7.3568E-10_r8k, 2.4663E-10_r8k, &
      &         3.3490E-13_r8k, 1.8761E-13_r8k, 3.1544E-14_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: alphaPu241 = (/ &
      &         3.0934E-01_r8k, 5.4434E-01_r8k, 4.0782E-01_r8k, 1.5828E-01_r8k, 4.1577E-02_r8k, &
      &         1.4818E-02_r8k, 5.8176E-03_r8k, 1.9482E-03_r8k, 9.5196E-04_r8k, 1.8208E-04_r8k, &
      &         1.5310E-05_r8k, 4.5039E-06_r8k, 9.8277E-07_r8k, 5.1832E-07_r8k, 2.3018E-08_r8k, &
      &         1.5817E-07_r8k, 1.8074E-08_r8k, 3.6922E-09_r8k, 5.3843E-11_r8k, 5.3003E-11_r8k, &
      &         4.8358E-14_r8k, 9.8516E-16_r8k, 1.3076E-16_r8k /)
    REAL(r8k), DIMENSION(NGroups), PARAMETER :: lambdaPu241 = (/ &
      &         2.9049E+00_r8k, 6.4911E-01_r8k, 2.5569E-01_r8k, 8.7123E-02_r8k, 2.5068E-02_r8k, &
      &         1.3323E-02_r8k, 6.3772E-03_r8k, 2.0221E-03_r8k, 6.2933E-04_r8k, 1.7462E-04_r8k, &
      &         4.0172E-05_r8k, 1.5289E-05_r8k, 7.6113E-06_r8k, 2.5083E-06_r8k, 1.1312E-06_r8k, &
      &         6.2987E-07_r8k, 1.3149E-07_r8k, 2.4237E-08_r8k, 9.6433E-09_r8k, 7.3467E-10_r8k, &
      &         2.4827E-10_r8k, 1.6873E-13_r8k, 8.3639E-15_r8k /)
    REAL(r8k), DIMENSION(NGroups, NNuclides), PARAMETER :: Alpha = reshape(                     & 
      &         (/ alphaU235, alphaU238, alphaPu239, alphaPu241 /), (/ NGroups, NNuclides /) )
    REAL(r8k), DIMENSION(NGroups, NNuclides), PARAMETER :: Lambda = reshape(                    &
      &         (/ lambdaU235, lambdaU238, lambdaPu239, lambdaPu241 /), (/ NGroups, NNuclides /) )
    CHARACTER(LEN=6), DIMENSION(NNuclides), PARAMETER :: NuclideID = (/ 'U-235 ' , 'U-238 ', 'Pu-239', 'Pu-241' /)
    !
    ! note (a) in table 9 of ANS-5.1-2004 says to use 1e13 for any time over 1e13
    t00 = MIN(t0, Tinfinity)
    !
    ! Calculate relative power fractions coming from each Nuclide based on fuel composition (atom basis)
    NuclidePowerFraction(1,:,:) = pfU235(:,:)
    NuclidePowerFraction(2,:,:) = pfU238(:,:)
    NuclidePowerFraction(3,:,:) = pfPu239(:,:)
    ! Note: Since there is no correlation provided for Pu-240 or Pu-242, they are added to Pu-241
    !       because it initially (first several hundred seconds) has higher power than Pu-239, thus
    !       making the prediction more conservative. However, these values are typically very small 
    !       ( < ~0.3%) and will not have a major impact on results
    NuclidePowerFraction(4,:,:) = pfPu240(:,:) + pfPu241(:,:) + pfPu242(:,:)
    Pd_uncor = 0.0_r8k
    DO i = 1, NNuclides
        ! Assume all Nuclides produce 200 MeV/Fision
        Q(i) = MeVFission
        ! Use equation for F(t,T) in note (a) of table 9 in ANS-5.1-2004
        ShutDownTime = ts
        Pmax = 0.0_r8k
        DO a = (it), 1, -1
            IF (ReactorPower(a) > 0.0_r8k) THEN
                ! Calculate max power during operating history, used to calculate PdHE
                Pmax = MAX(Pmax, qmpy(a) * qaxnorm(AxNode, jst(a)))
                ! Calculate nuclide contribution to power for timestep a
                Power(i,a) = qmpy(a) * qaxnorm(AxNode, jst(a)) * NuclidePowerFraction(i, AxNode, a)
                ! Calculate uncorrected decay heat power from isotope (i) during timestep (a)
                Pd_uncor(i) = Pd_uncor(i) + (Power(i,a) * (F(ShutDownTime,Tinfinity,i) - &
                  &           F(ShutDownTime + OperationTime(a),Tinfinity,i))) / (Q(i))
                ! Set the next ShutDownTime
                IF (a /= 1) THEN
                    ! Add the power step during this calculation
                    ShutDownTime = ShutDownTime + OperationTime(a)
                    ii = 1
                    ! Add any/all decay steps
                    DO WHILE (ReactorPower(a-ii) <= 0.0_r8k)
                        ShutDownTime = ShutDownTime + OperationTime(a-ii)
                        ii = ii + 1
                    END DO
                END IF
            END IF
        END DO
        !
    END DO
    ! Total decay heat power (Pd)
    Pd = SUM(Pd_uncor(1:NNuclides)) * G(ts,t00,FIFA) + PdHE(Pmax,ts,t00,R)
    !
    CONTAINS
        !
        REAL(r8k) FUNCTION F (ShutDownTime, OperationTime, Nuclide)
        USE Kinds
        USE Conversions
        IMPLICIT NONE
        !>@brief
        !> This function calculates the decay heat power of nuclide i in the absense of neutron capture in fission products
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> August 17, 2015
        !
        ! Input
        !
        ! OperationTime  - Total operating period, including intermediate periods at zero power (s)
        ! ShutDownTime   - Time after shutdown (cooling time) (s)
        ! Nuclide        - Nuclide being analyzed
        !
        ! Output
        !
        ! F      - (MeV/s) / (fissions/s)
        !
        INTEGER(ipk) :: k
        INTEGER(ipk), INTENT(IN) :: Nuclide
        REAL(r8k), INTENT(IN) :: ShutDownTime, OperationTime
        !
        F = 0.0_r8k
        !
        DO k = 1, NGroups
            F = F + alpha(k,Nuclide) / lambda(k,Nuclide) * EXP(-lambda(k,Nuclide) * ShutDownTime) * &
              & (1.0_r8k - EXP(-lambda(k,Nuclide) * OperationTime))
        END DO
        !
        END FUNCTION F
        !
        !
        !
        REAL(r8k) FUNCTION G (ShutDownTime, OperationTime, FIFA)
        USE Kinds
        USE Conversions
        USE Functions
        IMPLICIT NONE
        !>@brief
        !> This function calculates the factor which accounts for neutron capture in fission products
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> August 17, 2015
        !
        ! Input
        !
        ! OperationTime  - Total operating period, including intermediate periods at zero power (s)
        ! ShutDownTime   - Time after shutdown (cooling time) (s)
        ! FIFA           - Fissions per initial fisile atom (dimensionless)
        !
        ! Output
        !
        ! G              - Factor accounting for neutron capture in fission products (dimensionless)
        !
        INTEGER(ipk), PARAMETER :: NValues = 62
        REAL(r8k), INTENT(IN) :: ShutDownTime, OperationTime, FIFA
        REAL(r8k), DIMENSION(2*NValues), PARAMETER :: AbsorptionDecayHeatRatio = (/ &
          &     1.019_r8k, 1.0E+00_r8k, 1.020_r8k, 1.5E+00_r8k, 1.020_r8k, 2.0E+00_r8k, &
          &     1.021_r8k, 4.0E+00_r8k, 1.022_r8k, 6.0E+00_r8k, 1.022_r8k, 8.0E+00_r8k, &
          &     1.023_r8k, 1.0E+01_r8k, 1.024_r8k, 1.5E+01_r8k, 1.024_r8k, 2.0E+01_r8k, &
          &     1.025_r8k, 4.0E+01_r8k, 1.025_r8k, 6.0E+01_r8k, 1.026_r8k, 8.0E+01_r8k, &
          &     1.026_r8k, 1.0E+02_r8k, 1.027_r8k, 1.5E+02_r8k, 1.028_r8k, 2.0E+02_r8k, &
          &     1.031_r8k, 4.0E+02_r8k, 1.034_r8k, 6.0E+02_r8k, 1.035_r8k, 8.0E+02_r8k, &
          &     1.037_r8k, 1.0E+03_r8k, 1.041_r8k, 1.5E+03_r8k, 1.044_r8k, 2.0E+03_r8k, &
          &     1.053_r8k, 4.0E+03_r8k, 1.060_r8k, 6.0E+03_r8k, 1.066_r8k, 8.0E+03_r8k, &
          &     1.071_r8k, 1.0E+04_r8k, 1.081_r8k, 1.5E+04_r8k, 1.089_r8k, 2.0E+04_r8k, &
          &     1.111_r8k, 4.0E+04_r8k, 1.126_r8k, 6.0E+04_r8k, 1.137_r8k, 8.0E+04_r8k, &
          &     1.144_r8k, 1.0E+05_r8k, 1.155_r8k, 1.5E+05_r8k, 1.161_r8k, 2.0E+05_r8k, &
          &     1.166_r8k, 4.0E+05_r8k, 1.167_r8k, 6.0E+05_r8k, 1.168_r8k, 8.0E+05_r8k, &
          &     1.169_r8k, 1.0E+06_r8k, 1.170_r8k, 1.5E+06_r8k, 1.171_r8k, 2.0E+06_r8k, &
          &     1.175_r8k, 4.0E+06_r8k, 1.183_r8k, 6.0E+06_r8k, 1.194_r8k, 8.0E+06_r8k, &
          &     1.206_r8k, 1.0E+07_r8k, 1.241_r8k, 1.5E+07_r8k, 1.275_r8k, 2.0E+07_r8k, &
          &     1.384_r8k, 4.0E+07_r8k, 1.452_r8k, 6.0E+07_r8k, 1.491_r8k, 8.0E+07_r8k, &
          &     1.497_r8k, 1.0E+08_r8k, 1.397_r8k, 1.5E+08_r8k, 1.256_r8k, 2.0E+08_r8k, &
          &     1.080_r8k, 3.0E+08_r8k, 1.010_r8k, 4.0E+08_r8k, 1.000_r8k, 6.0E+08_r8k, &
          &     1.000_r8k, 8.0E+08_r8k, 1.000_r8k, 1.0E+09_r8k, 1.000_r8k, 1.5E+09_r8k, &
          &     1.000_r8k, 2.0E+09_r8k, 1.000_r8k, 4.0E+09_r8k, 1.000_r8k, 6.0E+09_r8k, &
          &     1.000_r8k, 8.0E+09_r8k, 1.000_r8k, 1.0E+10_r8k /) 
        !
        IF (ShutDownTime <= 1.0E4_r8k) THEN
            G = 1.0_r8k + (3.24E-06_r8k + 5.23E-10_r8k * ShutDownTime) * (OperationTime ** 0.4_r8k) * FIFA
        ELSE IF (ShutDownTime < 1.0E+10_r8k) THEN
            G = polate (AbsorptionDecayHeatRatio, ShutDownTime, NValues)
        ELSE
            G = 1.0_r8k
        END IF
        !
        END FUNCTION G
        !
        !
        !
        REAL(r8k) FUNCTION PdHE (MaxPower, ShutDownTime, OperationTime, R)
        USE Kinds
        USE Conversions
        IMPLICIT NONE
        !>@brief
        !> This function calculates the fission product power from the decay of heavy elements U-239 and Np-239
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> August 17, 2015
        !
        ! Input
        !
        ! OperationTime  - Total operating period, including intermediate periods at zero power (s)
        ! MaxPower       - Max reactor power during operating history
        ! ShutDownTime   - Time after shutdown (cooling time) (s)
        ! R              - Atoms of U239 produced per second per fission per second evaluated for the reactor
        !                  composition at the time of shutdown
        !
        ! Output
        !
        ! PdHE           - Decay heat power contributed by heavy elements U-239 and Np-239
        !
        ! Internal
        !
        ! Qeff           - Effective energy release per fission evaluated for the reactor compoisition
        !                  at time of reactor shutdown (MeV/fission)
        !
        REAL(r8k), INTENT(IN) :: MaxPower, ShutDownTime, OperationTime, R
        REAL(r8k), PARAMETER :: Qeff = 200.0_r8k   ! MeV/fission
        !
        PdHE = MaxPower / Qeff * (Fu239(ShutDownTime, OperationTime, R) + FNp239(ShutDownTime, OperationTime, R))
        !
        END FUNCTION PdHE
        !
        !
        !
        REAL(r8k) FUNCTION FU239 (ShutDownTime, OperationTime, R)
        USE Kinds
        USE Conversions
        IMPLICIT NONE
        !>@brief
        !> This function calculates the contribution to decay power for U-239
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> August 17, 2015
        !
        ! Input
        !
        ! OperationTime  - Total operating period, including intermediate periods at zero power (s)
        ! ShutDownTime   - Time after shutdown (cooling time) (s)
        ! R              - Atoms of U239 produced per second per fission per second evaluatd for the reactor
        !                  composition at the time of shutdown
        !
        ! Output
        !
        ! FU239          - Factor accounting for contribution to decay heat from U-239
        !
        ! Internal
        !
        ! EnergyU239     - Average energy from decay of one U-239 atom (= 0.474 MeV)
        ! LambdaU239     - Decay constant for U-239 (= 4.91E-4 per second)
        !
        REAL(r8k), INTENT(IN) :: ShutDownTime, OperationTime, R
        REAL(r8k), PARAMETER :: EnergyU239 = 0.474_r8k     ! MeV
        REAL(r8k), PARAMETER :: LambdaU239 = 4.91E-4_r8k   ! 1/s
        !
        FU239 = EnergyU239 * R * (1.0_r8k - EXP(-lambdaU239 * OperationTime)) * EXP(-lambdaU239 * ShutDownTime)
        !
        END FUNCTION FU239
        !
        !
        !
        REAL(r8k) FUNCTION FNp239 (ShutDownTime, OperationTime, R)
        USE Kinds
        USE Conversions
        IMPLICIT NONE
        !>@brief
        !> This function calculates the contribution to decay power for Np-239
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> August 17, 2015
        !
        ! Input
        !
        ! OperationTime  - Total operating period, including intermediate periods at zero power (s)
        ! ShutDownTime   - Time after shutdown (cooling time) (s)
        ! R              - Atoms of U239 produced per second per fission per second evaluatd for the reactor
        !                  composition at the time of shutdown
        !
        ! Output
        !
        ! FNp239         - Factor accounting for contribution to decay heat from Np-239
        !
        ! Internal
        !
        ! EnergyNp239    - Average energy from decay of one Np-239 atom (= 0.419 MeV)
        ! LambdaNp239    - Decay constant for Np-239 (= 3.41E-6 per second)
        ! LambdaU239     - Decay constant for U-239 (= 4.91E-4 per second)
        !
        REAL(r8k), INTENT(IN) :: ShutDownTime, OperationTime, R
        REAL(r8k), PARAMETER :: EnergyNp239 = 0.419_r8k    ! MeV
        REAL(r8k), PARAMETER :: LambdaU239 = 4.91E-4_r8k   ! 1/s
        REAL(r8k), PARAMETER :: LambdaNp239 = 3.41E-6_r8k  ! 1/s
        !
        FNp239 = EnergyNp239 * R * (LambdaU239 / (LambdaU239 - LambdaNp239) * (1.0_r8k - EXP(-lambdaNp239 * OperationTime)) * &
          &      EXP(-lambdaNp239 * ShutDownTime) - LambdaNp239 / (LambdaU239 - LambdaNp239) * &
          &      (1.0_r8k - EXP(-lambdaU239 * OperationTime)) * EXP(-lambdaU239 * ShutDownTime))
        !
        END FUNCTION FNp239
    !
    END FUNCTION fans51
    !
    !
    !
    SUBROUTINE AllocateDecayHeat
    USE Kinds
    USE Conversions
    USE Variables, ONLY : im
    IMPLICIT NONE
    !>@brief
    !> This subroutine allocates the arrays used for decay heat calculations and sets a default value
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/26/2015
    !
    ! Allocate arrays
    ALLOCATE (OperationTime(1:im))
    ALLOCATE (ReactorPower(1:im))
    ! Zero out the arrays
    OperationTime = 0.0_r8k
    ReactorPower = 0.0_r8k
    !
    END SUBROUTINE AllocateDecayHeat
    !
    !
    !
    SUBROUTINE DeAllocateDecayHeat
    USE Kinds
    USE Conversions
    IMPLICIT NONE
    !>@brief
    !> This subroutine deallocates the arrays used for the decay heating calculations
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 1/27/2015
    !
    IF (ALLOCATED(OperationTime)) DEALLOCATE(OperationTime)
    IF (ALLOCATED(ReactorPower)) DEALLOCATE(ReactorPower)
    !
    END SUBROUTINE DeAllocateDecayHeat
    !
END MODULE Decay
