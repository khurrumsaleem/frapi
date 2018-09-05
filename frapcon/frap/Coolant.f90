MODULE CoolantData
    USE Kinds
    USE conversions_frapcon
    USE variables_frapcon, ONLY : BulkCoolantTemp, rhof, modheat, p2, tw, deltaz, qc, ounit, coolanttemp, ifixedcoolt, &
      &                   cladtarray, ifixedtsurf, jstsurftemp, cooltype, de, go, na, dco, crdtt, ProblemTime, &
      &                   FilmCoefficient, im, crudmult, it, j, pitch, tsat, wt, crdtr, icor, &
      &                   deltcr, deltdb, deltfc, deltjl, totl, jmin, SurfTempOxide
    USE Material_Properties, ONLY : MatProp
    IMPLICIT NONE
    !> @brief
    !> Module CoolantData calculates bulk coolant temperatures for water & helium
    !> @author
    !> Module CoolantData was coded by Ian Porter, NRC
    !> @date
    !> June, 2014
    !
    CONTAINS
    !
    ! Water Coolant Properties
    !
    SUBROUTINE Coolant
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from frpcon and computes the temperature rise in the coolant (bulk coolant temperature) and 
    !> the temperature rise across the film and crud layers
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 6/16/2014
    !
    ! Input
    !
    ! CoolType - coolant type indicator (0 - Water (Default), 1 - Helium)
    !
    ! Internal
    !
    ! deh      - channel equivalent diameter (ft)
    ! g        - average mass flux (lbm/hr-ft**2)
    !
    REAL(r8k) :: deh, g
    !
    deh = de(j-1) * intoft
    g = go(it)
    !
    SELECT CASE (CoolType)
    CASE (0) ! Water Coolant
        ! The following call to coolt returns bulk coolant temperatures as well as the saturation temperature for axial node j.
        CALL coolt (deh, g)
        ! The following call to flmdrp computes the temperature drop across the film and crud for water coolant
        CALL flmdrp (deh, g)
    END SELECT
    !
    END SUBROUTINE Coolant
    !
    ! *********************************************************************************
    ! ****                     Water Coolant Properties                             ***
    ! *********************************************************************************
    !
    SUBROUTINE coolt (deh, g)
	USE variables_frapcon, only: flag_iapws
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> Subroutine coolt computes the bulk coolant temperature and saturation temperature for water.
    !> @author
    !> coolt was coded by g a berna november 1977.
    !> Coolt was modified by Ian Porter, 2014.
    !
    ! Input
    !
    ! dco     - Outer cladding diameter (m)
    ! deh     - Channel equivalent diameter (ft)
    ! deltaz  - Axial node length (ft)
    ! g       - Average mass flux (lbm/hr-ft**2)
    ! it      - Power-time step index
    ! j       - Axial node index
    ! modheat - Moderator heating fraction
    ! na      - Maximum number of axial nodes
    ! p2      - System pressure (psia)
    ! pitch   - Fuel rod pitch (m)
    ! qc      - Node heat flux (btu/hr-ft**2)
    ! rhof    - Bulk coolant density; unsaturated (lbm/ft**3)
    ! tw      - Inlet system coolant temperature (F)
    !
    ! Output
    !
    ! BulkCoolantTemp - Bulk coolant temperature at top of node j (F)
    ! tsat            - Coolant saturation temperature (F)
    ! wt              - Average bulk coolant temperature at node j (F)
    !
    ! Internal
    !
    ! bchc     - Bulk coolant heat capacity (btu/lbm-F)
    ! densat   - Bulk coolant density; saturated (lbm/ft**3)
    ! TempCalc - Specifies which calculation to use for determining coolant temperatures.
    !
    REAL(r8k), INTENT(IN) :: deh, g
    REAL(r8k) :: densat, bchc
    CHARACTER(LEN=6) :: TempCalc
    ! Default. Coolant Enthalpy Rise Model.
    TempCalc = 'coolen'
    ! User prescribed cladding surface temperatures to be used as coolant temperature
    IF (ifixedtsurf == 1 .AND. g <= 0.0_r8k) TempCalc = 'cladt'
    ! User prescribed cladding surface temperatures to be used as coolant temperature
    IF (ifixedtsurf == 1 .AND. jstsurftemp(it) /= 0) TempCalc = 'cladt'
    ! User has prescribed no coolant flow. Will use inlet temperature as temperature at all nodes.
    IF (ifixedcoolt == 0 .AND. g <= 0.0_r8k) TempCalc = 'noflow'
    ! User prescribed coolant temperatures
    IF (ifixedcoolt == 1) TempCalc = 'coolt'
    !
    SELECT CASE (TempCalc)
    CASE ('cladt') !Bulk coolant temperature equals the user specified cladding temperature
        BulkCoolantTemp(jmin-1) = tw(it)
        BulkCoolantTemp(1) = cladtarray(it,1)
        BulkCoolantTemp(j) = cladtarray(it,j)
    CASE ('noflow')
        BulkCoolantTemp(jmin-1) = tw(it)
        BulkCoolantTemp(j) = tw(it)
        tsat = (((0.47616716e-8_r8k * p2(it) - 0.4424529e-4_r8k) * p2(it)) + 0.19042968_r8k) * p2(it) + 394.03519_r8k
    CASE ('coolt') !User specifies coolant temperatures
        BulkCoolantTemp(jmin-1) = coolanttemp(it,jmin-1)
        BulkCoolantTemp(j) = coolanttemp(it,j)
        tsat = (((0.47616716e-8_r8k * p2(it) - 0.4424529e-4_r8k) * p2(it)) + 0.19042968_r8k) * p2(it) + 394.03519_r8k
        IF (BulkCoolantTemp(j-1) <= 450.0_r8k) THEN
            rhof(j-1) = 65.43_r8k - BulkCoolantTemp(j-1) * 0.03486_r8k
        ELSE
            ! rhof(j-1) = -810.063_r8k + (((-1.06824e-8_r8k * BulkCoolantTemp(j-1) + 2.29413e-5_r8k) * BulkCoolantTemp(j-1) &
            !   &         - 0.0184872_r8k) * BulkCoolantTemp(j-1) + 6.56174_r8k) * BulkCoolantTemp(j-1)
			! YU JIANKAI used new steam table from iapws-if97
            if (flag_iapws) then 
               rhof(j-1) = calc_cool_density_iapws( tfc( BulkCoolantTemp(j-1)) ) * kgm3tolbft3  ! conversion the unit  
            else 
               rhof(j-1) = calc_cool_density_asme68(BulkCoolantTemp(j-1))
            end if
            ! 
        END IF
    CASE DEFAULT !Default single channel coolant enthalpy rise model. Equivalent to using CASE ('coolen')
        BulkCoolantTemp(jmin-1) = tw(it)
        tsat = (((0.47616716e-8_r8k * p2(it) - 0.4424529e-4_r8k) * p2(it)) + 0.19042968_r8k) * p2(it) + 394.03519_r8k
        IF (BulkCoolantTemp(j-1) < tsat) THEN
            IF (BulkCoolantTemp(j-1) <= 450.0_r8k) THEN
                rhof(j-1) = 65.43_r8k - BulkCoolantTemp(j-1) * 0.03486_r8k
            ELSE
                ! rhof(j-1) = -810.063_r8k + (((-1.06824e-8_r8k * BulkCoolantTemp(j-1) + 2.29413e-5_r8k) * BulkCoolantTemp(j-1) &
                !   &         - 0.0184872_r8k) * BulkCoolantTemp(j-1) + 6.56174_r8k) * BulkCoolantTemp(j-1)
				! YU JIANKAI used new steam table from iapws-if97
                if (flag_iapws) then 
                   rhof(j-1) = calc_cool_density_iapws( tfc( BulkCoolantTemp(j-1)) ) * kgm3tolbft3  ! conversion the unit  
                else 
                   rhof(j-1) = calc_cool_density_asme68(BulkCoolantTemp(j-1)) 
                end if
                ! 
            END IF
            densat = rhof(j-1) * (1.0_r8k + 0.648e-6_r8k * ((tsat - BulkCoolantTemp(j-1)) ** 0.1_r8k) * (p2(it) - 800.0_r8k))
            IF (BulkCoolantTemp(j-1) < 520.0_r8k) THEN
                bchc = 57.3_r8k
            ELSE IF (BulkCoolantTemp(j-1) < 590.0_r8k) THEN
                bchc = 57.3_r8k * (1.0_r8k + 7.73e-4_r8k * (BulkCoolantTemp(j-1) - 520.0_r8k))
            ELSE
                bchc = 57.3_r8k * (1.0_r8k + 2.95e-3_r8k * (BulkCoolantTemp(j-1) - 571.6_r8k))
            END IF
            BulkCoolantTemp(j) = BulkCoolantTemp(j-1) + (qc(j-1) / (1.0_r8k - modheat) * densat) / &
              &                  (deh * g * bchc / (4.0_r8k * deltaz(j-1)))
            IF (BulkCoolantTemp(j) >= tsat) BulkCoolantTemp(j) = tsat
        ELSE
            BulkCoolantTemp(j) = tsat
        END IF
    END SELECT
    ! Water temperature is the average of the node above & below
    wt = (BulkCoolantTemp(j) + BulkCoolantTemp(j-1)) / 2.0_r8k
    !
    END SUBROUTINE Coolt
    !
    !
    !
    SUBROUTINE flmdrp (deh, g)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine is called from frpcon and calculates temperature drop across the film and crud
    !>@author
    !> flmdrp was coded by g a berna november 1977.
    !> v1m5 updates february 1985 by dd lanning,pnl
    !
    ! Input
    !
    ! crdtr  - Crud deposition rate (mils/hr)
    ! deh    - Equivalent hydraulic diameter (ft)
    ! g      - Mass flux of coolant (lbm/hr-ft**2)
    ! icor   - Crud deposition index (0 or 1 - crdt is read as constant thickness. 2 - crdt is read as crud growth rate)
    ! it     - Power-time step index
    ! im     - Number of problem power-time steps
    ! j      - Axial node index
    ! na     - Maximum number of axial nodes
    ! p2     - Coolant pressure during power-time step (psia)
    ! ProblemTime - End of step time (sec)
    ! qc     - Heat flux at node j (btu/hr-ft**2)
    ! tsat   - Saturation temperature (F)
    ! wt     - Average bulk water temperature (F)
    !
    ! Output
    !
    ! crdtt  - Crud thickness (mils)
    ! deltcr - Temperature drop across the crud (F)
    ! deltdb - Temperature drop across the film using dittus-boelter (F)
    ! deltfc - Temperature drop across the film total forced convec. (F)
    ! deltjl - Temperature drop across the film using jens-lottes (F)
    ! FilmCoefficient - Surface heat transfer coefficient (btu/hr-ft**2-F)
    ! SurfTempOxide   - Rod oxide surface temperature (F)
    !
    ! Internal
    !
    ! JLcoef   - Jens-Lottes Coefficient
    !          = 60.0_r8k * (1.0_r8k / 1.0E6_r8k) ** (0.25_r8k)
    !
    ! Reference:
    !
    ! (1) Journal of heat transfer vol.89(1967) p.242
    !
    REAL(r8k), INTENT(IN) :: deh, g
    REAL(r8k) :: hflmdb, Crudthcon
    REAL(r8k), PARAMETER :: JLcoef = 1.897366596101027536_r8k
    CHARACTER(LEN=5) :: TempCalc 
    !
    TempCalc = 'Norm '
    IF (g == 0.0_r8k) TempCalc = 'Fixed'
    IF (ifixedtsurf == 1 .and. jstsurftemp(it) /= 0) TempCalc = 'Fixed'
    !
    SELECT CASE (TempCalc)
    CASE ('Fixed')
        SurfTempOxide(j-1) = wt
        deltcr = 0.0_r8k
        deltdb = 0.0_r8k
        deltfc = 0.0_r8k
        deltjl = 0.0_r8k
        FilmCoefficient(j-1) = 0.0_r8k
    CASE DEFAULT
        ! Calculate crud thickness
        ! Crud thickness (mils) = crudmultiplier * crud deposition rate (mils/hr) * time (hr)
        IF (icor == 2) crdtt(j-1) = crudmult(j-1) * crdtr * (ProblemTime(it) * sectohr)
        ! Dittus-boelter film heat transfer coefficient (Btu/(hr*ft^2*F))
        hflmdb = DittusBoelterHTC (wt, g, p2(it), deh)
        ! Dittus-Boelter film temperature drop, F
        deltdb = qc(j-1) / hflmdb
        ! Crud temperature drop, F
        Crudthcon = MatProp ('CRUD', 'THERMCOND', wt + deltdb)
        deltcr = qc(j-1) * crdtt(j-1) * miltoft / (Crudthcon * WmKtoBhftF)
        ! Limit crud temperature drop to 20F when constant thickness is supplied
        IF (icor == 0 .OR. icor == 1) deltcr = MIN(deltcr, 20.0_r8k)
        ! Film + Crud temperature drop
        deltfc = deltdb + deltcr
        ! Jens-Lottes temperature drop for nucleate boiling, wcap 2059,pp 21-22
        deltjl = JLcoef * (qc(j-1) ** 0.25_r8k) / EXP(p2(it) * PSItoPa / 6.2E6_r8k)
        !
        ! Calculate rod surface temperature, F
        SurfTempOxide(j-1) = MIN(wt + deltdb, tsat + deltjl) + deltcr
        IF (icor == 0) SurfTempOxide(j-1) = MIN(wt + deltfc, tsat + deltjl)
        !
        ! Calculate surface heat transfer coefficient
        FilmCoefficient(j-1) = qc(j-1) / (SurfTempOxide(j-1) - wt)
    END SELECT
    !
    END SUBROUTINE flmdrp
    !
    !
    !
    REAL(r8k) FUNCTION DittusBoelterHTC (Twater, Flux, Pressure, HydroDiam) RESULT (HTC)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> Function calculates the dittus boelter film drop heat transfer coefficient
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/29/2015
    !
    ! Input
    !
    ! Twater    - Water temperature, F
    ! Flux      - Water mass flux, lbm/hr-ft**2
    ! Pressure  - Coolant pressure, psi
    ! HydroDiam - Hydraulic diameter, ft
    !
    ! Output
    !
    ! HTC   - Dittus Boelter Heat Transfer Coefficient, Btu/(hr*ft^2*F)
    !
    ! Internal
    !
    ! MassFlux - Coolant mass flux. Taken as max of 1.0E-4 and Flux.
    !
    REAL(r8k), INTENT(IN) :: Twater, Flux, Pressure, HydroDiam
    REAL(r8k) :: MassFlux
    !
    ! Check for input errors
    IF (Twater <= 0.0_r8k .OR. Flux < 0.0_r8k .OR. Pressure <= 0.0_r8k .OR. HydroDiam <= 0.0_r8k ) THEN
        WRITE (0, 100) Twater, Flux, Pressure, HydroDiam
        WRITE (ounit, 100) Twater, Flux, Pressure, HydroDiam
100     FORMAT ('Bad input into Funtion DittusBoelterHTC.',/,'Twater must be > 0.0. Twater = ',e12.5, &
          &     /,'Flux must be >= 0.0. Flux = ',e12.5,/,'Pressure must be > 0.0. Pressure = ',e12.5, &
          &     /,'HydroDiam must be > 0.0. HydroDiam = ',e12.5,/,'Code execution stopped.')
        STOP
    END IF
    !
    ! Ensure code stability when no flow is entered (can happen when using fixed cladding or coolant temps)
    MassFlux = MAX(Flux, 1.0E-5_r8k)
    !
    HTC = ((-5.1889e-5_r8k + 6.5044e-8_r8k * Twater) * Twater + (3.5796e-7_r8k - 1.0337e-9_r8k * Twater) &
      &   * Pressure + 3.2377e-2_r8k) * MassFlux * (MassFlux * HydroDiam) ** (-0.2_r8k)
    !
    END FUNCTION DittusBoelterHTC
	! 
	! YU JIANKAI
    ! the following borrowed from IAPWS-IF97 from TH1D solver
    ! cubic polynomial for density as a function of temperature, max err=0.0692%
    !  15.5 Mpa,  280 < T < 340, rho in Kg/M^3, T in C
    function calc_cool_density_iapws(t) result(den)
       implicit none
       real(8)     :: den
       real(8)     :: t
       
       den = 5.9901166e+03_8 + &
          &  t*(-5.1618182e+01_8 + t*(1.7541848e-01_8 + t*(-2.0613054e-04_8)))
       return
    end function calc_cool_density_iapws
    !
    !
    function calc_cool_density_asme68(t) result(den)
       implicit none
       real(8)     :: den
       real(8)     :: t
       !   the orignial steam table from ASME1968 formulation
       !   input : t   (F)
       !   output: den (lb/ft3)
       den = -810.063_r8k + (((-1.06824e-8_r8k * t + 2.29413e-5_r8k) * t &
             - 0.0184872_r8k) * t + 6.56174_r8k) * t
       return
    end function calc_cool_density_asme68
    !
END MODULE CoolantData

