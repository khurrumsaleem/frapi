MODULE TimeStep
    USE Kinds
    USE cnvt
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to advance the problem in time.
    !> Subroutines include crank6, store6, setup6, init6, restrw
    !> comput, honr, timstp
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/14/2016
    !
    CONTAINS
    !
    SUBROUTINE crank6
    USE Kinds
    USE variables_fraptran, ONLY : ounit, afrap, Time
    USE CoolantProperties, ONLY : tc1, tc2
    USE Dyna_h
    USE collct_h
    USE resti_h
    USE htcb_h
    USE excb_h
    USE scalr_h
    USE frapc
    USE cnvt
    IMPLICIT NONE
    !> @brief
    !> This subroutine calls the subroutines which compute fuel rod solution for an advanced time
    !
    CALL setup6
    !
    n2 = 1
    ! Top of time step loop
    SELECT CASE (coupled)
    CASE (.TRUE.) ! Solution scheme uses fuel temperatures calculated by T/H Code
        IF (convert_units) CALL cnvt12 ! Convert from SI to British units
        CoupledTimeStep: DO
            tc1 = t12
            tc2 = t22
            IF (time <= (t12 - 1.0e-10_r8k)) THEN
                WRITE(ounit,*) time
20              FORMAT(/,'FRAPTRAN Execution Finished with time ',e12.5,' seconds.',/)
                RETURN
            ENDIF
            ! Calculate results for current time step
            CALL comput
            ! Store results for current time step
            CALL store6
            ! If ntstep=0, continue power ramp at first time step
            IF (ntstep > 0 .AND. (time >= (t22 - 1.0e-8_r8k))) EXIT CoupledTimeStep
        END DO CoupledTimeStep
        IF (convert_units) CALL cnvt21 ! Convert from British to SI units before sending data back to TH code
    CASE (.FALSE.)
        ! Default solution scheme. FRAPTRAN calculates all fuel rod & coolant conditions.
        TimeStep: DO
            ! Calculate results for current time step
            CALL comput
            ! Store results for current time step
            CALL store6
            ! If ntstep=0, continue power ramp at first time step
            IF (ntstep > 0 .AND. (Time >= (tmax - 1.0e-6_r8k))) EXIT TimeStep
        END DO TimeStep
    END SELECT
    !
    END SUBROUTINE crank6
    !
    !
    !
    SUBROUTINE store6
    USE Kinds
    USE conversions_fraptran, ONLY : ftin, tfk
    USE variables_fraptran
    USE rlpst, ONLY : rlpst3
    USE PlotFile
    USE OutputFile
    USE Uncertainties, ONLY : dktout
    USE Functions, ONLY : polate
    USE FEA_IO
    IMPLICIT NONE
    !> @brief
    !> This Subroutine stores the value of the fuel rod variables computed by subroutine comput.
    !> It also controls plot & output printing
    !
    INTEGER(ipk) :: i, j, k, l, m, Indexcladmelt, icount, idtp, node
    INTEGER(ipk), PARAMETER :: nrestp = 20 ! nrestp must have same value as that assigned to it in crank6
    REAL(r8k) :: tfail, hex, UE0, UEhex, Afail, nfail, UE, deleplas, cladnodes, cladthk, nodethk, &
      &     hsolinf, hydthk, fold, told, tnew, fraccomp, hp, hsol, tflxc, epstrain1, epstrain2, &
      &     epstrain3, delhoopstrain, delaxialstrain, delradialstrain, stresshoop, stressaxial, &
      &     stressradial, delsedepri, dtplt1, dtdkt1, bulocal, dtpo1, eppinit
    REAL(r8k), PARAMETER :: em20 = 1.0e-20_r8k
    LOGICAL :: Write_to_Output = .FALSE.
    !
    tflxc = tflux + Time - t0
    !
    DO k = 1,naxn
        IF (NSteadyTrans /= 1) THEN
            DO m = 1, nmesh
                EnrgyMelt(m,k) = EnrgyMeltZ(m,k)
                EnrgyMeltp1(m,k) = EnrgyMeltZp1(m,k)
            ENDDO
        ENDIF
        !
        IF (ndebug) THEN
            WRITE(ounit,*) ' STORE6: RhoCp, for k = ',k
            WRITE(ounit,906) (RhoCp(i,k), i = 1,nmesh)
            WRITE(ounit,*) '      FhoCp0'
            WRITE(ounit,906) (RhoCp0(i,k), i = 1,nmesh)
906         FORMAT(8(2x,e11.4))
        ENDIF
        !
        cladnodes = REAL(nmesh - ncladi)
        cladthk = RadialBound(nmesh) - RadialBound(ncladi)
        ! reduce cladding thickness for oxidation
        cladthk = cladthk - EOSOxideThick(k) / 12.0_r8k / 1.56_r8k
        nodethk = cladthk / cladnodes
        ! Modify geometry with regard to initial pellet and cladding deformations (needed for FE model)
        DO m = 1, nmesh
            ! Get initial cladding hoop strain for the current axial position:
            !IF (nepp0 > 0) THEN
            !    eppinit = polate (eppinp, zelev(k), nepp0)
            !ELSE
            !    eppinit = 0.0_r8k
            !ENDIF
            ! Pellet nodes and cladding nodes:
            IF (m <= igpnod) THEN
                RadialBoundO(m,k) = RadialBound(m) * (1.0_r8k + PelRadDeviat(k) / RadialBound(igpnod))
            ELSE
                !RadialBoundO(m,k) = RadialBound(ncladi) * (1.0_r8k + eppinit) + REAL(m - ncladi) * nodethk
                RadialBoundO(m,k) = RadialBound(ncladi) * (1.0_r8k + CldPlasStrn(k,1)) + REAL(m - ncladi) * nodethk
            END IF
            
            !IF (CldPlasStrn(k,1) /= eppinit) PRINT *, 'CldPlasStrn(k,1)=',CldPlasStrn(k,1),'eppinit=',eppinit
            
            RhoCp0(m,k) = RhoCp(m,k)
            BOSRad(m,k) = EOSRad(m,k)
            BOSTemp(m,k) = EOSTemp(m,k)
        ENDDO
    ENDDO
    !
    IF (ndebug) THEN
        WRITE(ounit,*)' STORE6: BOSTemp(m,k) = '
        DO k = 1, naxn
            WRITE(ounit,*) 'For axial node: ',k
            WRITE(ounit,906) (BOSTemp(m,k),m = 1,nmesh)
        ENDDO
    ENDIF
    !
    IndexCladMelt = 0
    delth0 = delth
    dcldh0 = dcldh
    !
    apln0(1) = apln(1)
    bpln0(1) = bpln(1)
    qpln0(1) = qpln(1)
    !
    DO l = 1, nodpln
        tplbot(l,1) = tplbot(l,2)
        tplna(l,1,1) = tplna(l,2,1)
    ENDDO
    !
    IF ((swllfr >= em20 .OR. ies(1) == 1) .AND. Ifchk == 0) THEN
        dvdt0(1) = dvdt(1)
        IF (ies(1) == 1) THEN
            vs0(1) = vsn(1)
            pswll0(1) = GasPress(Kswell)
        ELSE
            pswll0(1) = GasPress(1)
        END IF
        GasMolesAx0(naxn+1) = GasMolesAx(naxn+1)
    END IF
    !
    DO k = 1, naxn
        HeatFlux0(k) = HeatFlux(k)
        CoolEnthalpy0(k) = CoolEnthalpy(k)
        CoolDensity0(k) = CoolDensity(k)
        CldPermStrn0(k) = CldPermStrn(k)
        GasPress0(k) = GasPress(k)
        RodOD0(k) = RodOD(k)
        OldCoolPrs(k) = CoolPress(k)
        OldCladT(k) = CladAveTemp(k)
        OldGasPrs(k) = GasPress(k)
        OldPelDis(k) = PelSrfDispl(k) * ftin
        BOSGapIndex(k) = GapIndex(k)
        OldCoolPrs0(k) = OldCoolPrs(k)
        OldCladT0(k) = OldCladT(k)
        OldGasPrs0(k) = OldGasPrs(k)
        OldPelDis0(k) = PelSrfDispl(k) * ftin
        BOSGapIndex0(k) = BOSGapIndex(k)
        RInterfacPrs0(k) = RInterfacPrs(k)
        PelletRad0(k) = PelletRad(k)
        CladCollIndx0(k) = CladCollapseIndex(k)
        IF (EOSTemp(nmesh,k) >= tmelt(2)) IndexCladMelt = 1
        IF (NSteadyTrans /= 1) THEN
            BOSOxideThick(k) = EOSOxideThick(k)
            AlphaThk1(k) = AlphaThk2(k)
            OxiThk1(k) = OxiThk2(k)
            OxUptakeID1(k) = OxUptakeID2(k)
            AlphaThk11(k) = AlphaThk22(k)
        ENDIF
        IF (swllfr >= em20 .AND. Ifchk /= 1) GasMolesAx0(k) = GasMolesAx(k)
    ENDDO
    !
    DO k = 1, naxn
        FuelSrfStrRat0(k) = FuelSrfStrRat(k)
        EDotFZ0(k)        = EDotFZ(k)
        PelSrfStrn0(k)    = PelSrfStrn(k,2)
        EffStrain0(k)     = EffStrain(k)
        OldCldAxStrn0(k)  = OldCldAxStrn(k)
        OldFuelAxStrn0(k) = OldFuelAxStrn(k)
        DO l = 1, ldir
            CldStrnRat0(k,l)    = CldStrnRat(k,l)
            CldResidStrn0(k,l)  = CldResidStrn(k,l)
            CldPlasStrn0(k,l)   = CldPlasStrn(k,l)
            FuelResidStrn0(k,l) = FuelResidStrn(k,l)
        ENDDO
    ENDDO
    !
    DO k = 1, naxn
        OldCldAxStrn(k) = CldStrn(k,2)
        OldFuelAxStrn(k) = PelSrfStrn(k,2)
        OldGapIndex(k) = GapIndex(k)
        OldCladT(k) = CladAveTemp(k)
        OldGasPrs(k) = GasPress(k)
        OldCoolPrs(k) = CoolPress(k)
        OldPelDis(k) = PelSrfDispl(k)
    ENDDO
    ! calculate strain energy density (SED)
    DO k = 1, naxn
        ! calculate PNNL SED using effective elastic and plastic strain
        ! assume radial thermal strain = hoop thermal strain
        CldThermStrn(k,3) = CldThermStrn(k,1)
        ! subtract thermal strain from total strain
        EPStrain1 = CldStrn(k,1) - CldThermStrn(k,1)
        EPStrain2 = CldStrn(k,2) - CldThermStrn(k,2)
        EPStrain3 = CldStrn(k,3) - CldThermStrn(k,3)
        ! calculate current effective strain
        EffStrainPNNL(k) = 0.47140_r8k * SQRT((EPStrain1 - EPStrain2) ** 2 + (EPStrain2 - EPStrain3) ** 2 + &
          &                (EPStrain3 - EPStrain1) ** 2)
        ! calculate SED based on incremental strain and average effectove stress, SED = MJ/m3
        SEDPNNL(k) = SEDPNNLold(k) + ((CladEffStress(k) + CladEffStressOld(k)) / 2.0_r8k * &
          &         (EffStrainPNNL(k) - oldEffStrainPNNL(k)) * 6909.0e-6_r8k)
        ! save values for next time step
        oldEffStrainPNNL(k) = EffStrainPNNL(k)
        SEDPNNLold(k) = SEDPNNL(k)
        CladEffStressOld(k) = CladEffStress(k)
        ! calculate SED using EPRI formulation, hoop and axial only
        ! 6/11/03: use absolute values for stress and strain increments
        ! calculate incremental strain
        delHoopStrain = ABS(EPStrain1 - EPStrain1old(k))
        delAxialStrain = ABS(EPStrain2 - EPstrain2old(k))
        delRadialStrain = ABS(EPStrain3 - EPStrain3old(k))
        ! calculate stress term for time step
        StressHoop = (CldStress(k,1) + StressHoopOld(k)) / 2.0_r8k
        StressAxial = (CldStress(k,2) + StressAxialOld(k)) / 2.0_r8k
        StressRadial = (CldStress(k,3) + StressRadialOld(k)) / 2.0_r8k
        ! calculate incremental SED and convert to MJ/m3
        dElseDEPRI = (StressHoop * delHoopStrain + StressAxial * delAxialStrain) * 6909.0e-6_r8k
        ! add incremental SED for total SED
        SEDEPRI(k) = SEDEPRIold(k) + dElseDEPRI
        ! WRITE(0,705)Time,k,SEDEPRI(k),dElseDEPRI,delHoopStrain,
        ! &   delAxialStrain,StressHoop,StressAxial
        ! 705 FORMAT(1x,f10.6,i2,f6.1,f6.3,4e13.5)
        ! Save values for next time step
        SEDEPRIold(k) = SEDEPRI(k)
        StressHoopOld(k) = CldStress(k,1)
        StressAxialOld(k) = CldStress(k,2)
        StressRadialOld(k) = CldStress(k,3)
        EPStrain1old(k) = EPStrain1
        EPStrain2old(k) = EPStrain2
        EPStrain3old(k) = EPStrain3
    ENDDO
    ! Calculate failure strain and compare it to plastic hoop strains
    IF (Ifailue == 0) THEN
        ! Cladding has not yet failed
        Check_Failure_Strain: DO k = 1, naxn
            tfail = tfk(CladAveTemp(k))
            ! find excess hydrogen
            hsolinf = 1.2e5_r8k * EXP(-8550.0_r8k / 1.99_r8k / tfail)
            IF (Time == 0.0_r8k) THEN
                OldCldPlasStrn(k) = CldPlasStrn(k,1)
                hsol = hsolinf
            ELSE
                hydthk = 2.5e-4_r8k
                fold = MIN(hsolold(k) / hsolinf, 1.0_r8k)
                told = (fold ** 2) * (hydthk ** 2) / (0.78_r8k ** 2) * EXP(24880.0_r8k / 1.99_r8k / tfail)
                tnew = told + TimeIncrement
                fraccomp = MIN(1.0_r8k, 0.78_r8k / hydthk * SQRT(tnew) * EXP(-12440.0_r8k / 1.99_r8k / tfail))
                hp = 1180.0_r8k * EXP(-35716.0_r8k / 8.314_r8k / tfail)
                hsol = MIN(cexh2a(k), MAX(fraccomp * hsolinf, hsolold(k) - hp * TimeIncrement))
            ENDIF
            hex = MAX(0.0_r8k, cexh2a(k) - hsol)
            hsolold(k) = hsol
            ! Find maximum failure strain based on uniform elongation
            UE0 = 2.2_r8k
            UEhex = UE0
            IF (tfail > 700.0_r8k) THEN
                Afail = 1.840803_r8k
                nfail = 0.107131_r8k
            ELSE
                Afail = 1211.0_r8k * EXP(-0.00927_r8k * tfail)
                nfail = -0.001783_r8k * tfail + 1.355231_r8k
            END IF
            IF (hex > 0.0_r8k) UEhex = Afail * hex ** (-nfail)
            UE = MIN(UE0, UEhex)
            UE = UE / 100.0_r8k
            IF (hex >= 650.0_r8k) UE = 0.0005_r8k
            deleplas = CldPlasStrn(k,1) - OldCldPlasStrn(k)
            IF (CldPlasStrn(k,1) >= UE .AND. tfail <= 700.0_r8k .AND. deleplas > 0.0_r8k) Ifailue = 1
            IF (Ifailue == 1) THEN
                RodFailIndex(k) = 1
                RuptFailIndex(k) = 1
                Ifaila = k
                WRITE(ounit,805) Time, k
                WRITE(0,805) Time, k
805             FORMAT('***** Cladding Failed at time ',e13.6,' sec when the hoop strain exceeded the failure strain ', &
                  &    'based on uniform elongation at node ',i3,' *****')
                EXIT Check_Failure_Strain
            ENDIF
            OldCldPlasStrn(k) = CldPlasStrn(k,1)
        END DO Check_Failure_Strain
    END IF
    ! Calculate Oxygen uptake for BJ oxidation model
    ! Calculate ECR for BJ and CP oxidation model
    DO k = 1, naxn
        IF (modmw == 2) THEN
            OxygenUptake(k) = EOSOxideThick(k) * 5680.0_r8k * 0.2597_r8k * 0.0254_r8k
            OxUptakeID2(k) = OxiThk2(k) * 5680.0_r8k * 0.2597_r8k * 0.0254_r8k
        ENDIF
        ! ECR calc includes ID and OD oxygen uptake
        ! Regulations specify zirconium reacted, and density of zirconium is 6.49 g/cm3
        ! density of zircaloy is 6.56 g/cm3, and was previously used here instead of density of zirconium
        ! correction made on 10/05/2012
        ECR(k) = 0.00328_r8k * (OxygenUptake(k) + OxUptakeID2(k)) * 2.85_r8k / 6.49_r8k / &
          &     ((RadialBound(nmesh) - RadialBound(ncladi)) * (1.0_r8k + CldPlasStrn(k,3)))
    ENDDO
    
    ! Plot file
    IF ((ntplot /= 0) .AND. (ntstep /= 0) .AND. (Time >= (tplot - 1.0e-8_r8k))) THEN
        ! Call routine to put out a frame of graphics Data
        tp(1) = tplna(1,2,1)
        CALL grafout
        !
        ! increment the counter for the DAKOTA output storage variable dktouts
        IF (Time < tmax) dktoutcounter = dktoutcounter + 1
        !
        ! At the end of the run, redimension dktouts to get rid of zeros if the array was over-dimensioned:
        !   1- create a dummy array and store the dktouts values
        !   2- deallocate and reallocate dktouts to achieve the proper dimensions
        !   3- copy the DAKOTA outputs back into dktouts from the dummy array
        !
    
        ! Finite Element Output
        IF (mechan == 1) Call Write_output()
    
        ! Determine the plot time
        SELECT CASE (coupled)
        CASE (.FALSE.) ! If not coupled, use the default way of calculating the plot time
            dtplta(npltn*2+2) = tmax
            DO icount = 1, npltn
                idtp = 2 * icount - 1
                IF (Time >= (dtplta(idtp+1) - 1.0e-6_r8k) .AND. Time < (dtplta(idtp+3) - 1.0e-6_r8k)) EXIT
            ENDDO
            dtplt1 = dtplta(idtp) ! New plot time increment
        CASE (.TRUE.) ! If coupled, use the print interval specified in the T/H Code
            DO icount = 1, SIZE(gfInt)
                IF (Time >= tEnd(icount) - 1.0e-6_r8k .AND. Time < tEnd(icount+1)-1.0e-6_r8k) EXIT
            ENDDO
            dtplt1 = gfInt(icount+1) !Plot time increment to be used is contained in higher tEnd array
        END SELECT
        tplot = tplot + dtplt1
        IF (tplot > tmax) tplot = tmax
    END IF
    
    ! If DAKOTA output is specified, printout DAKOTA output and determine the next DAKOTA output time
    IF (dakota .AND. Time >= (tdkt - 1.0e-8_r8k)) THEN
        ! Call routine to put out a frame of graphics Data
        node = 1 ! hard wired to print out the values for the DAKOTA output at axial node #1
        CALL dktout (node)
        SELECT CASE (coupled)
        CASE (.FALSE.) !If not coupled, use the default way of calculating the plot time
            dtdkta(ndktn*2+2) = tmax
            DO icount = 1,ndktn
                idtp = 2 * icount - 1
                IF (Time >= dtdkta(idtp+1) - 1.0e-6_r8k .AND. Time < dtdkta(idtp+3) - 1.0e-6_r8k) Exit
            ENDDO
            dtdkt1 = dtdkta(idtp) !New plot time increment
        CASE (.TRUE.) !If coupled, use the print interval specified in the T/H Code
            ! P. Raynaud NOTE: gfInt in the plot time for the TH code. 
            ! It may be required to create a dakota output time for the TH code...?
            DO icount = 1, SIZE(gfInt)
                IF (Time >= tEnd(icount) - 1.0e-6_r8k .AND. Time < tEnd(icount+1) - 1.0e-6_r8k) Exit
            ENDDO
            dtdkt1 = gfInt(icount+1) ! DAKOTA output time increment to be used is contained in higher tEnd array
        END SELECT
        tdkt = tdkt + dtdkt1
        IF (tdkt > tmax) tdkt = tmax
        ! Write the maximum value of the DAKOTA parameteres of interest at end of run
        IF (Time >= tmax - 1.0e-6_r8k) THEN
            WRITE(dakotaunit,*) (MAXVAL(dktouts(1,:)) + 1.0_r8k)
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(2,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(3,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(4,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(5,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(6,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(7,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(8,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(9,1:dktoutcounter))
            WRITE(dakotaunit,'(e13.5)') MAXVAL(dktouts(10,1:dktoutcounter))
        END IF
    ENDIF
    !
    IF (IterationCount >= (MaximumIterations + 2)) RETURN
    ! Check for cladding melting
    IF (IndexCladMelt == 1) THEN
        WRITE(ounit,790) Tmelt(2), (EOSTemp(nmesh,k), k=1,naxn)
790     FORMAT(/'Run terminated due to exceeding clad melting temperature',1x,f7.1,' EOSTemp(nmesh,k) = '/5(5(2x,e11.4)/))
        RETURN
    END IF
    
    ! Check to see if restart should be written
    IF ((nswinw == 2) .AND. (MOD(ntstep,nrestp) == 0) .AND. (Time >= (tmax - 1.0e-6_r8k))) CALL restrw
    
    !
    IF (ncool >= 1) Call rlpst3 (AxNodElevat, GapThick, PelletRad, GasPress, RInterfacPrs, RodOD, naxn, gadolin, bulocal, &
      &                          gapmin, Ifchk, n2, RodFailIndex, Vreloc, modfd, GapTemp, FuelSurfT, EOSTemp, ncladi, &
      &                          GasFraction, OxiThk2, RadialBound, igpnod, roughc, roughf, frden, tflxc, FastFlux, &
      &                          coldw, AxBurnup, frpo2, CladMaxT, n1, TotalGasMoles, HGapAV, fotmtl)
    !
    Write_to_Output = .FALSE.
    IF (ntstep == 0) RETURN
    IF (ntstep == 1) Write_to_Output = .TRUE.
    IF ((Time >= (tpo - 1.0e-8_r8k)) .OR. (Time >= (tmax - 1.0e-8_r8k))) Write_to_Output = .TRUE.
    !
    IF (.NOT. Write_to_Output) RETURN
    
    ! Output file
    tp(1) = tplna(1,2,1)
    CALL prntot
    
    ! Update new print time for output file
    SELECT CASE (coupled)
    CASE (.FALSE.)
        dtpoa(npdtpo*2+2) = tmax
        DO icount = 1, npdtpo
            idtp = 2 * icount - 1
            IF (Time >= dtpoa(idtp+1) - 1.0e-6_r8k .AND. Time < dtpoa(idtp+3) - 1.0e-6_r8k) EXIT
        ENDDO
        dtpo1 = dtpoa(idtp)
    CASE (.TRUE.) !If coupled, use the print interval specified in the T/H Code
        DO icount = 1, SIZE(edInt)
            IF (Time >= tEnd(icount) - 1.0e-6_r8k .AND. Time < tEnd(icount+1) - 1.0e-6_r8k) EXIT
        ENDDO
        dtpo1 = edInt(icount+1)
    END SELECT
    !
    tpo = Time + dtpo1
    IF (ntstep == 0) tpo = t0
    !
    END SUBROUTINE store6
    !
    !
    !
    SUBROUTINE setup6
    USE Kinds
    USE variables_fraptran, ONLY : ncards, ncool, ndtad, t1, t2, &
    & IndexTempConverg, time, ntstep, ncall, t0, tmax, vsn, vs0, tEnd, coupled
    IMPLICIT NONE
    !>@brief
    !> This Subroutine sets up variables for time step advancement by subroutine comput
    ! Note: If cold start, initialization done in start0.
    !
    INTEGER(ipk) :: i
    !
    IF (ncards /= 1 .AND. ncool /= 7) CALL init6
    !
    SELECT CASE (coupled)
    CASE (.TRUE.) ! Since coupled, timesteps are set by T/H code
        t0 = t1
        Time = t1
        tmax = MAXVAL(tEnd)
    CASE (.FALSE.) ! Not coupled. Default.
        t0 = Time
        ncall = 0
        tmax = t2
    END SELECT
    !
    vsn(1) = vs0(1)
    IndexTempConverg = 1
    IF (ndtad == 1) THEN
        ntstep = 0
        tmax = t0
    ENDIF
    !
    END SUBROUTINE setup6
    !
    !
    !
    SUBROUTINE init6
    USE Kinds
    USE variables_fraptran, ONLY : ounit, afrap, maxidx, amatpc, unit, apowrd, aprntb, a1, time, DebugTime, fdial, acoold, arest1, &
      &                   irest2, irest3, aexcb, tz1, ithymx, ablona, trecrd, ascal1, ascal2, ascal3, ftmelt, aflcht
    USE collct_h
    IMPLICIT NONE
    !> @brief
    !> This Subroutine performs transient initialization
    !
    ! array afrap must have Dimensions of (la1tot+lcombk)*(& of rods)
    ! afrap(1) = time (s)
    ! afrap(2) = index for afrap of last written word
    !
    ! Debug is for use in controling debug output by setting DebugTime
    !
    INTEGER(ipk) :: la1tot, lstart, lpostn, lcolc2, l
    REAL(r8k), DIMENSION(1) :: aphypr
    !
    aphypr(1) = ftmelt
    la1tot = 0
    WRITE(ounit,911)
911 FORMAT(' init6 entered ')
    ! Initialization to values at end of last cycle of calculations. 
    ! Find starting position in afrap array of data for fuel rod numbered nr
    trecrd = afrap(1)
    lstart = 3
    ! afrap(2)+1 = starting location of restart data for rod & nr.  
    ! It is defined at end of restrw to be last word + 1 written for rod nr - 1
    lstart = INT(afrap(2))
    lpostn = lstart + 1
    WRITE(ounit,913) lstart
913 FORMAT(' lstart = ',i8)
    ! this is restart block fix
    maxidx = 21
    lcolc2 = 665
    ! lcolc2 must be changed each time collct data block changed
    DO l = 1, lcolc2
        acolct(l+1) = afrap(lpostn+l-1)
    ENDDO
    WRITE(ounit,915) la1max, lresr1, lresi3, lcolct, lcolc2
915 FORMAT(' la1max = ',i8,' lresr1 = ',i8,' lresi3 = ',i8,' lcolct = ',i8,' lcolc2 = ',i8)
    lpostn = lpostn + lcolc2
    !
    WRITE(ounit,917) la1tot
917 FORMAT(' la1tot = ',i8)
    !
    DO l = 1, la1tot
        a1(l) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + la1tot
    !
    DO l = 1, lsclr1
        ascal1(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lsclr1
    !
    DO l = 1, lsclr2
        ascal2(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lsclr2
    !
    DO l = 1, lsclr3
        ascal3(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lsclr3
    !
    apowrd = afrap(lpostn+l-1)
    lpostn = lpostn + 1
    !
    DO l = 1, lcoold
        acoold(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lcoold
    !
    DO l = 1, lexcb
        aexcb(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lexcb
    !
    DO l = 1, lprntb
        aprntb(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lprntb
    !
    ! Do 840 l = 1,maxidx
    !  840   emflag(l) = afrap(lpostn+l-1)
    lpostn = lpostn + maxidx
    !
    DO l = 1, lflect
        aflcht(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lflect
    !
    DO l = 1, ldialb
        fdial(l) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + ldialb
    !
    DO l = 1, lresr1
        arest1(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lresr1
    !
    DO l = 1, lresi2
        irest2(l+1) = INT(afrap(lpostn+l-1))
    ENDDO
    lpostn = lpostn + lresi2
    !
    DO l = 1, lresi3
        irest3(l+1) = INT(afrap(lpostn+l-1))
    ENDDO
    lpostn = lpostn + lresi3
    !
    DO l = 1, lmatpc
        amatpc(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lmatpc
    !
    DO l = 1, lblona
        ablona(l+1) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lblona
    !
    DO l = 1, lphypr
        aphypr(l) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lphypr
    !
    DO l = 1, lthyd
        tz1(l) = afrap(lpostn+l-1)
    ENDDO
    lpostn = lpostn + lthyd
    !
    Time = afrap(lpostn)
    DebugTime = afrap(lpostn+1)
    lpostn = lpostn + 2
    ithymx = INT(afrap(lpostn))
    npramp = 2
    WRITE(ounit,919) ithymx
919 FORMAT(' ithymx = ',i8)
    ftmelt = aphypr(1)
    !
    END SUBROUTINE init6
    !
    !
    !
    SUBROUTINE restrw
    USE Kinds
    USE variables_fraptran
    IMPLICIT NONE
    !> @brief
    !> Subroutine stores all information needed for restarting into array afrap, which is stored in lcm.
    !> Optionally, array afrap written onto tape
    !
    ! array afrap must have Dimensions of (la1tot + lcombk)*(# of rods)
    ! afrap(1)  =  time (s)
    ! afrap(2)  =  index for afrap of last written word
    !
    INTEGER(ipk) :: l, la1tot, lstart, lp, msgno, lmax
    LOGICAL :: lopen
    REAL(r8k), DIMENSION(1) :: aphypr
    !
    aphypr(1) = ftmelt
    la1tot = 0
    afrap(1) = Time
    afrap(2) = 3
    afrap(3) = la1tot
    lstart = INT(afrap(2)) + 1
    lp = lstart
    !
    DO l = 1,lcolct
        afrap(lp+l-1) = acolct(l+1)
    ENDDO
    WRITE(ounit,905) lcolct
905 FORMAT(' in restrw, lcolct = ',i8)
    lp = lp + lcolct
    !
    !     in restrw, store starting index in afrap array of azimuthal coolant factors
    !
    IF (ithymx > 0) ixazim = lp + ixazmn-1
    !
    DO l = 1,la1tot
        afrap(lp+l-1) = a1(l)
    ENDDO
    lp = lp + la1tot
    !
    DO l = 1,lsclr1
        afrap(lp+l-1) = ascal1(l+1)
    ENDDO
    lp = lp + lsclr1
    !
    DO l = 1,lsclr2
        afrap(lp+l-1) = ascal2(l+1)
    ENDDO
    lp = lp + lsclr2
    !
    DO l = 1,lsclr3
        afrap(lp+l-1) = ascal3(l+1)
    ENDDO
    lp = lp + lsclr3
    !
    afrap(lp+l-1) = apowrd
    lp = lp + 1
    !
    DO l = 1,lcoold
        afrap(lp+l-1) = acoold(l+1)
    ENDDO
    lp = lp + lcoold
    !
    DO l = 1,lexcb
        afrap(lp+l-1) = aexcb(l+1)
    ENDDO
    lp = lp + lexcb
    !
    DO l = 1,lprntb
        afrap(lp+l-1) = aprntb(l+1)
    ENDDO
    lp = lp + lprntb
    !
    !    DO 250 l = 1,maxidx
    !  250   afrap(lp+l-1) = emflag(l)
    !    lp  =  lp + maxidx
    !
    DO l = 1,lflect
        afrap(lp+l-1) = aflcht(l+1)
    ENDDO
    lp = lp + lflect
    !
    DO l = 1,ldialb
        afrap(lp+l-1) = fdial(l)
    ENDDO
    lp = lp + ldialb
    !
    DO l = 1,lresr1
        afrap(lp+l-1) = arest1(l+1)
    ENDDO
    lp = lp + lresr1
    !
    DO l = 1,lresi2
        afrap(lp+l-1) = irest2(l+1)
    ENDDO
    lp = lp + lresi2
    !
    DO l = 1,lresi3
        afrap(lp+l-1) = irest3(l+1)
    ENDDO
    lp = lp + lresi3
    !
    DO l = 1,lmatpc
        afrap(lp+l-1) = amatpc(l+1)
    ENDDO
    lp = lp + lmatpc
    !
    DO l = 1,lblona
        afrap(lp+l-1) = ablona(l+1)
    ENDDO
    lp = lp + lblona
    !
    DO l = 1,lphypr
        afrap(lp+l-1)  =  aphypr(l)
    ENDDO
    lp  =  lp + lphypr
    !
    DO l = 1,lthyd
        afrap(lp+l-1)  =  tz1(l)
    ENDDO
    lp  =  lp + lthyd
    !
    afrap(lp) = Time
    afrap(lp+1) = DebugTime
    lp = lp + 2
    afrap(lp) = ithymx
    afrap(2) = lp
    WRITE(ounit,901) afrap(1), afrap(2)
901 FORMAT(' at End of restrw, afrap1 = ',e13.6,' afrap2 = ',e13.6)
    ! If (ncool /= 0) GOTO 100
    Inquire (frtrunit,opened = lopen)
    IF (.NOT. lopen) Open (frtrunit,iostat = msgno, status = 'new',form = 'unFORMATTED')
    rewind frtrunit !This clears the Data written in the last timestep and starts writing to file (frtrunit) from the beginning
    WRITE(frtrunit) afrap(1), afrap(2), afrap(3) !This is the original way.  This Writes it as a binary File
    ! WRITE(frtrunit,*) afrap(1), afrap(2), afrap(3)  !This Writes it as an ascii file
    lmax = lp
    WRITE(frtrunit) (afrap(l),l = 4,lmax) !This is the original way.  This Writes it as a binary File
    ! WRITE(frtrunit,*) (afrap(l),l = 4,lmax) !This Writes it as an ascii file
    !  
    END SUBROUTINE restrw
    !
    !
    !
    SUBROUTINE comput
    USE Kinds
    USE conversions_fraptran, ONLY : pi, ftom, powcnv, tfk
    USE Functions, ONLY : polate, terp
    USE variables_fraptran
    USE rlpst, ONLY : rlpst2
    USE frapc, ONLY : coupled, t12, t22
    USE Deformation
    USE Volume, ONLY : vswell
    USE HeatSolution
    USE ZircSpecHeat
    USE Void, ONLY : gsflow
    USE NCGases, ONLY : SteamIndex
    USE Plenum, ONLY : plnt
    Use Ballooning
    USE ZrModels, ONLY : caneal
    USE Oxidation, ONLY : cobild
    USE Initialization, ONLY : phyprp
    USE AxialPower, ONLY : power
    USE Cs_I, ONLY : cesiod
    IMPLICIT NONE
    !> @brief
    !> This Subroutine computes the value of the fuel rod variables at an advanced time.
    !
    ! Input
    !
    ! tm1bal - Clad temperature at lower node (K)
    ! tp1bal - Clad temperature at upper node (K)
    ! ztmax  - Elevation of ballooning node (m)
    ! zm1bal - Elevation of lower node (m)
    ! zp1bal - Elevation of upper node (m)
    ! zndbal - Total length of fuel rod (m)
    ! nodpln - Number of nodes in plenum temperature model
    !
    INTEGER(ipk) :: k, icount, idtp, m, nswmd0, kk, i, ls, l1, npconv, itswp, &
      &             l, nfcall, j, nswpmn, id, npi, nprdbg = 0, Ifirst = 0, nbredt = 0
    INTEGER(ipk), PARAMETER :: itdmax = 100
    INTEGER(ipk), PARAMETER :: nodgas = 1
    REAL(r8k) :: dtx, tflxc, powmax, totpow, hlpln, hupln, qchsum, totpnd, powgnd, powlnd, ptnd, &
      &          hf1, dzh, hf2, tsplen, tcool, hcoef, dcldi, tinsul, qplnb0, aplnb, bplnb, fitcd, &
      &          pitave, plenl,tempf,gmolxe, gmolkr, pnew, rc, dpnew, tempc0, tempc1, drodm0, thkm0, &
      &          thkox0, dmxi1k, woxupt, pfcpa, tmpdot, cflux1, tcldmx, dvsum, tsum, dv, tface, &
      &          cestot, ainsur, timpro, dzpowi, amuran, amurak, delbu, rpmult, rparea, busi, ft2m2, &
      &          delbu1, ftmax, ftemp, amural, tcld0, tcldp, cpint, tfave, rdot, hflux = 0.0_r8k, &
      &          Gscale, dum1 = 0.0_r8k, dum2 = 0.0_r8k, dum3 = 0.0_r8k, dum4 = 0.0_r8k, powrz = 0.0_r8k
    CHARACTER(LEN=7) :: TopPTempCalc, BotPTempCalc
    INTEGER(ipk), ALLOCATABLE, SAVE :: Iflag(:,:)
    INTEGER(ipk) :: nfirst
    REAL(r8k), DIMENSION(3) :: dep
    REAL(r8k), DIMENSION(5) :: cescom
    REAL(r8k), DIMENSION(8) :: aidcom
    REAL(r8k), DIMENSION(nmesh) :: temptemp
    REAL(r8k), DIMENSION(naxn) :: molrel
    REAL(r8k), DIMENSION(naxn+1) :: rwa7
    if (.not. allocated(Iflag)) allocate(Iflag(ngasr,2))
    !
    nprofile = 1
    IF(ntstep==0) Iflag(1:ngasr,1:2)=0
    !
    !ConvgLoop: DO
60  CONTINUE
        !
        ! Update the radial power profile in the fuel and cladding
        !
        ! Call Update_Power_Distribution(idx)
        !
        ! Begin loop to compute fuel rod temperature and stress-strain distribution
        !
        IterationCount = 0
        ! 
        SELECT CASE (ntstep)
        CASE (0)
            IF (npramp > 0) THEN ! Power for first power step set in Subroutine POWRMP
                powict = powict + dppowi
                IF (powict >= powimx) ntstep = 1
                IF (powict > powimx) powict = powimx
            ENDIF
            npramp = npramp + 1
            IF (ntstep == 1) ndtred = 0
            ! If ndtred=1, explicit temperature solution forced
        CASE DEFAULT
            ! Check to see if smaller time step should be enforced because of ballooning.
            dtenfb = 1.0E10_r8k
            !
            DO k = 1, naxn
                IF (Ifaila < 1 .AND. CldStrnRat0(k,1) >= 1.0e-5_r8k) THEN
                    dtx = 0.01_r8k / CldStrnRat0(k,1)
                    IF (dtx < dtenfb) dtenfb = dtx
                ENDIF
            ENDDO
            ! Round off time step
            IF (dtenfb > 50.0_r8k .AND. dtenfb < 200.0_r8k) dtenfb = 100.0_r8k
            IF (dtenfb > 15.0_r8k .AND. dtenfb <= 50.0_r8k) dtenfb = 25.0_r8k
            IF (dtenfb >  8.0_r8k .AND. dtenfb <= 15.0_r8k) dtenfb = 10.0_r8k
            IF (dtenfb >  3.0_r8k .AND. dtenfb <=  8.0_r8k) dtenfb = 5.0_r8k
            IF (dtenfb >  1.5_r8k .AND. dtenfb <=  3.0_r8k) dtenfb = 2.0_r8k
            IF (dtenfb >  0.8_r8k .AND. dtenfb <=  1.5_r8k) dtenfb = 1.0_r8k
            IF (dtenfb >  0.3_r8k .AND. dtenfb <=  0.8_r8k) dtenfb = 0.5_r8k
            IF (dtenfb > 0.15_r8k .AND. dtenfb <=  0.3_r8k) dtenfb = 0.2_r8k
            IF (dtenfb > 0.08_r8k .AND. dtenfb <= 0.15_r8k) dtenfb = 0.1_r8k
            IF (dtenfb <= 0.08_r8k) dtenfb = 0.05_r8k
            !Enforce timestep by either (1) user supplied value or (2) the start/end time from T/H Code
            SELECT CASE (coupled)
            CASE (.FALSE.) !Default. Not coupled.
                dtmaxa(ndtmax*2+2) = tmax
                DO icount = 1, ndtmax
                    idtp = 2 * icount - 1
                    IF ((Time >= dtmaxa(idtp+1) - 1.0e-8_r8k) .AND. (Time < dtmaxa(idtp+3) - 1.0e-8_r8k)) EXIT
                ENDDO
                dtp = dtmaxa(idtp)
            CASE (.TRUE.) !Use values from T/H Code
                dtp = t22 - t12
            END SELECT
            !
            CALL timstp
            !
            IF (TimeIncrement > dtold .AND. numdta < 100) TimeIncrement = dtold
            IF ((Time + TimeIncrement) > tmax) TimeIncrement = tmax - Time
            time0 = Time
            Time = Time + TimeIncrement
            ! Check to see if time for debugging
            IF (Time >= DebugTime .AND. Time <= DebugTimeStop) THEN
                ndebug = .TRUE.
            ELSE
                ndebug = .FALSE.
            END IF
            DO m = 1, NumAxProfiles
                IF (ProfileStartTime(m) - Time <= 1.0e-10_r8k ) nprofile = m
                IF (time0 == 0.0_r8k) nprofile = 1
            ENDDO
            ntstep = ntstep + 1
        END SELECT
        !
        tflxc = tflux + Time - t0
        ! *** first time step assumed to be steady-state. force large TimeIncrement.
        IF (ntstep <= 1) THEN
            TimeIncrement = 0.1_r8k
        ELSE
            NSteadyTrans = 2
        END IF
        itdmx = 0
        ! Check for disintegration of fuel grains.
        IF (MAXVAL(TimeofGBSep(1:naxn)) > 1.0e-6_r8k) prsacc = 1.0E20_r8k
        ! If time step greater than dtss, perform steady-state calculation
        IF (TimeIncrement >= dtss) NSteadyTrans = 1
        nswmd0 = 0
        nswmd  = 0
        IF (ndim >= 1) THEN
            ! assume 2-d heat conduction starts from beginning
            nswmd0 = 1
            IF (ntstep <= 1) nswmd0 = 0
            ! assume 2-d heat conduction starts from beginning
            nswmd = 1
        ENDIF
        apln(1) = apln0(1)
        bpln(1) = bpln0(1)
        dvdt(1) = dvdt0(1)
        vsn(1) = vs0(1)
        dcldh = dcldh0
        delth = delth0
        GasMolesAx(naxn+1) = GasMolesAx0(naxn+1)
        !
        Ifaila = 0
        DO k = 1, naxn
            IF (RuptFailIndex(k) == 1 .OR. RuptFailIndex(k) == 4) Ifaila = k
            GasMolesAx(k) = GasMolesAx0(k)
            IndexTempConverg(k) = 1
            GasPress(k) = GasPress0(k)
            HeatFlux(k) = HeatFlux0(k)
            RodOD(k) = RodOD0(k)
            RInterfacPrs(k) = RInterfacPrs0(k)
            PelletRad(k) = PelletRad0(k)
            CladCollapseIndex(k) = CladCollIndx0(k)
        ENDDO
        ! Compute average rod power and axial power peaking factor for the
        ! FLECHT correlation in Subroutine REFLOOD
        powmax = 0.0_r8k
        totpow = 0.0_r8k
        totpnd = 0.0_r8k
        !
        DO k = 1, naxn
            CALL power (nptha, npaxp, AxNodElevat(k), Time, TimeIncrement, fuelrd, powrz, dum3, dum4, &
              &         dum1, powop, timop, fpdcay, mpdcay, NSteadyTrans, powict, powimx, ntstep, &
              &         ExtentOfBow(k), nqbow, dum2, 1.0_r8k, powgnd, powlnd, rvoid)

            powmax = MAX(powrz, powmax)
            totpow = totpow + powrz * AxialNodLen(k)
            ! True rod total power
            totpnd = totpnd + powlnd * AxialNodLen(k)
        ENDDO
        !
        pavg = totpow / rl
        ! Calculate true rod average LHGR (ptnd) and avoid dividing by zero for LHGR = 0
        ptnd = totpnd / rl
        IF (pavg > 0.0_r8k) THEN
            pfnuc = powmax / pavg
        ELSE
            pfnuc = 1.0_r8k
        ENDIF
        ! The true (ptnd) and prescribed (powgnd) rod average LHGR without 
        ! decay heat contribution may differ, due to axial discretization 
        ! and rod bowing. This is accounted for by scaling rod average power
        ! with Gscale in the next call to subroutine power:
        Gscale = (powgnd + 1.0e-20_r8k) / (ptnd + 1.0e-20_r8k)
        pavg   = pavg * Gscale
        ! Update fuel and cladding melt temperatures
        bumtp  = bu(1)
        compmt = frpo2 * 100.0_r8k
        deloxy = OxygenConcenAve(1)
        !
        CALL phyprp
        !
        fdelta = 1.0_r8k
        fhtc = 1.0_r8k
        !
45      CONTINUE
        ! Write out convergence info
        IF (Time >= DebugTime) THEN
            WRITE(ounit,*)
            WRITE(ounit,909) IterationCount
909         FORMAT('Summary of Iteration: IterationCount = ',i4/)
            WRITE(ounit,*) '  Axial Node Number:'
            WRITE(ounit,1906) (i,i = 1,naxn)
            WRITE(ounit,*) '  IndexTempConverg - Temperature Convergence Index:'
            WRITE(ounit,1906) (IndexTempConverg(i),i = 1,naxn)
            WRITE(ounit,*) '  Fuel Centerline Temperature:'
            WRITE(ounit,906) (FuelCenterT(i),i = 1,naxn)
            WRITE(ounit,*) '  Fuel Surface Temperature:'
            WRITE(ounit,906) (FuelSurfT(i),i=1,naxn)
            WRITE(ounit,*) '  Clad Surface Temperature:'
            WRITE(ounit,906) (CladSurfT(i),i = 1,naxn)
            WRITE(ounit,*) '  Bulk Coolant Temperature:'
            WRITE(ounit,906) (BulkCoolTemp(i),i = 1,naxn)
            WRITE(ounit,*) '  Axial Power:'
            WRITE(ounit,906) (AxialPowr(i),i = 1,naxn)
            WRITE(ounit,*) '  Thermal Gap Thickness:'
            WRITE(ounit,906) (GapThick(i),i = 1,naxn)
            WRITE(ounit,*) '  Structural Gap Thickness:'
            WRITE(ounit,906) (RInterfacGap(i),i = 1,naxn)
            WRITE(ounit,*) '  Workspace Gap Thickness:'
            WRITE(ounit,906) (WorkSpaceGAP(i),i = 1,naxn)
            WRITE(ounit,*) '  Gap Conductance:'
            WRITE(ounit,906) (HGapAV(i),i = 1,naxn)
            WRITE(ounit,*) '  Surface Heat Flux:'
            WRITE(ounit,906) (SurfHtFlux(i),i = 1,naxn)
            WRITE(ounit,*) '  Cladding Premanent Hoop Strain:'
            WRITE(ounit,906) (CldPlasStrn(i,1),i = 1,naxn)
            WRITE(ounit,*) '  Cladding Premanent Axial Strain:'
            WRITE(ounit,906) (CldPlasStrn(i,2),i = 1,naxn)
            WRITE(ounit,*)
906         FORMAT(8(2x,e11.4))
1906        FORMAT(8(i8,4x))
        ENDIF
        !
        IterationCount = IterationCount + 1
        itswt = 0
        SELECT CASE (ncool)
        CASE (7)
            ! FrapTran temperature calculations bypassed. Instead, fuel rod
            ! temperature distribution and plenum gas temperature obtained from RELAP
            CALL rlpst2 (EOSTemp, tplna, GapTemp, FuelSurfT, CladAveTemp, IndexTempConverg, CoolPress, FuelCenterT, CladSurfT, &
              &          CrackTemp, OpenPorTemp, tmelt, FilmCoeffAv, BulkCoolTemp, HeatFlux, CladMaxT, AxNodElevat, naxn, &
              &          nmesh, igpnod, ncladi, RadialBound, BOSTemp, WatrMetlEnrgy, EOSOxideThick, AlphaThk2, OxiThk2, &
              &          AlphaThk22, CritHtFlux, HGapAv, AxialPowr, powave)
            !
        CASE DEFAULT
            IF (nqchn == 0 .OR. nqchn == 7) THEN
                !
                ! compute heat added to each channel from inlet to elevation z
                !
                SELECT CASE (nqchn)
                CASE (0)
                    ! check to see which direction flow is going
                    ! inlet is from plenum with smaller enthalpy
                    hlpln = polate (hinta, Time, nhinta)
                    hupln = polate (hupta, Time, nhupta)
                CASE (7)
                    ! If nqchn = 7, always assume inlet flow to be consistent with assumption for nqchn=7 in Subroutine COOL
                    hlpln = 500.0_r8k
                    hupln = 600.0_r8k
                END SELECT
                !
                qchsum = 0.0_r8k
                !
                DO i = 1, naxn
                    !
                    ls = 0
                    Nchan = nrc(1,1)
                    IF (Nchan >= 1) THEN
                        IF (hupln < hlpln) THEN
                            k = naxn - i + 1 + ls
                        ELSE
                            k = i + ls
                        ENDIF
                        IF (i == 1) THEN
                            hf1 = 0.0_r8k
                            dzh = AxialNodLen(k) / 2.0_r8k
                        ELSE
                            IF (hupln < hlpln) THEN
                                hf1 = 0.5_r8k * (HeatFlux0(k+1) + HeatFlux(k+1))
                                dzh = 0.5_r8k * (AxialNodLen(k) + AxialNodLen(k+1))
                            ELSE
                                hf1 = 0.5_r8k * (HeatFlux0(k-1) + HeatFlux(k-1))
                                dzh = 0.5_r8k * (AxialNodLen(k) + AxialNodLen(k-1))
                            ENDIF
                        ENDIF
                        hf2 = 0.5_r8k * (HeatFlux0(k) + HeatFlux(k))
                        qchsum = qchsum + pchn(1,1) * RodOD(k) * pi * dzh * (hf1 + hf2) / 2.0_r8k
                    ENDIF
                ENDDO
            ENDIF
            !
            CALL heat (Gscale)
            !
            IF (nerr == 1 .OR. nerr == 2) GOTO 885
            ! Determine the plenum temperature calculation to be used for the top plenum.
            ! Default. Plenum temperature is (10F by default) higher than bulk coolant temperature.
            IF (nplnt /= 1) TopPTempCalc = 'Bulk+10'
            ! Detailed plenump temperature calculation
            IF (nplnt == 1) TopPTempCalc = 'Detaild'
            ! User supplied plenum temperature
            IF (ngastmp(1) > 0) TopPTempCalc = 'UserVal'
            ! Set plenum gas temperature to that of surface of top axial node. Core is empty of coolant and adiabatic heatup begins.
            IF (Time >= empytm) TopPTempCalc = 'TopNode'
            ! Compute temperature of gas in plenum
            SELECT CASE (TopPTempCalc)
            CASE ('UserVal') ! User supplied plenum temperature
                DO l1 = 1, nodpln
                    tplna(l1,2,1) = polate(gasths(1:2*ngastmp(1),1), Time, ngastmp(1))
                ENDDO
                qpln(1) = 0.0_r8k
            CASE ('TopNode') ! Coolant conditions surrounding plenum assumed same as those at top axial node
                DO l1 = 1, nodpln
                    tplna(l1,2,1) = EOSTemp(nmesh,naxn)
                ENDDO
                qpln(1) = 0.0_r8k
            CASE ('Bulk+10') ! Assume insulator pellet is trise (10F by default) higher in temperature than coolant surrounding it
                DO l1 = 1, nodpln
                    tplna(l1,2,1) = BulkCoolTemp(naxn) + trise
                ENDDO
                qpln(1) = 0.0_r8k
            CASE ('Detaild') ! Evaluate cladding surface temperature at plenum
                tsplen = BulkCoolTemp(naxn)
                hflux = 0.0_r8k
                tcool = tsplen
                hcoef = 0.0_r8k
                drod = 2.0_r8k * RadialBound(nmesh)
                dcldi = 2.0_r8k * RadialBound(ncladi)
                vplenc = vplen(1) - (delth - dcldh) * atop
                IF (vplenc < 0.005_r8k * vplen(1)) vplenc = 0.005_r8k * vplen(1)
                IF (ndebug) WRITE(ounit,905) Time, IterationCount, hcoef, hflux, tcool, GasMoles0, TotalGasMoles, &
                  &                          VoidVolumeRatio(1)
905             FORMAT('before PLNT Call at Time = ',e13.6, ' IterationCount = ',i5,' hcoef = ',e13.6,' hflux = ' &
                  &    ,e13.6,' tcool = ',e13.6,/' GasMoles0 = ',e13.6,' TotalGasMoles = ',e13.6,' VoidVolumeRatio(1) = ',e13.6)
                !
                tinsul = tcool + trise
                IF (ntstep == 0 .AND. IterationCount == 1) THEN
                    PlenumGasMoles = GasMoles0 * 0.6_r8k
                ELSE
                    PlenumGasMoles = TotalGasMoles * VoidVolumeRatio(1)
                ENDIF
                !
                tpln(1:nodpln,1) = tplna(1:nodpln,1,1)
                !
                CALL plnt (drod, dcldi, vplenc, powave(1), TimeIncrement, spl(1), scd(1), swd(1), tinsul, &
                  &        tsplen, PlenumGasMoles, GasFraction, rhoc, NSteadyTrans, ncs(1), nodpln, &
                  &        qpln0(1), hflux, apln(1), bpln(1), tpln, GasPress(naxn+1), npconv)
                !
                IF (npconv == 0) GOTO 883
                tplna(1:nodpln,2,1) = tpln(1:nodpln,2)
                qpln(1) = hflux
            END SELECT
            ! Determine the plenum temperature calculation to be used for the bottom plenum.
            ! If plenum exists, default model. Plenum temperature is (10F by default) higher than bulk coolant temperature.
            IF (nplnt /= 1) BotPTempCalc = 'Bulk+10'
            ! Detailed plenum temperature model.
            IF (nplnt == 1) BotPTempCalc = 'Detaild'
            ! User supplied values for lower plenum temperature
            IF (ngastmp(2) > 0) BotPTempCalc = 'UserVal'
            ! Default. No bottom plenum exists.
            IF (nbotpl /= 1) BotPTempCalc = 'No Plen'
            !
            SELECT CASE (BotPTempCalc)
            CASE ('No Plen')
                DO l1 = 1, nodpln
                    tplbot(l1,2) = 1.0e+10_r8k
                ENDDO
            CASE ('UserVal')
                DO l1 = 1, nodpln
                    tplbot(l1,2) = polate(gasths(1:2*ngastmp(2),2), Time, ngastmp(2))
                ENDDO
            CASE ('Bulk+10')
                DO l1 = 1, nodpln
                    tplbot(l1,2) = BulkCoolTemp(1) + trise
                ENDDO
            CASE ('Detaild') ! Compute plenum gas temperature in bottom plenum
                tsplen = BulkCoolTemp(1)
                hflux = 0.0_r8k
                qplnb0 = 0.0_r8k
                tcool = tsplen
                hcoef = 0.0_r8k
                drod = 2.0_r8k * RadialBound(nmesh)
                dcldi = 2.0_r8k * RadialBound(ncladi)
                ! Assume bottom plenum does not change volume with time
                vplenb = volbp
                tinsul = tcool + trise
                tplbt1(1:nodpln,1) = tplbot(1:nodpln,1)
                !
                CALL plnt (drod, dcldi, vplenb, powave(1), TimeIncrement, splbp, coldbp, spdbp, tinsul, &
                  &        tsplen, BottomPlenumGasMoles, GasFraction, rhoc, NSteadyTrans, ncolbp, nodpln, &
                  &        qplnb0, hflux, aplnb, bplnb, tplbt1, GasPress(1), npconv)
                !
                tplbot(1:nodpln,2) = tplbt1(1:nodpln,2)
            END SELECT
            ! Begin loop for deformation and rod internal pressure calculations
            !
            ! Deformation and pressure iterated upon until convergence. Then fuel rod temperature distribution recomputed.
        END SELECT
        fitcd = 0.0_r8k
        pitave = 0.0_r8k
        ! If IterationCount=1, extrapolate gap pressure from previous time step
        itcntd = 0
110     CONTINUE
        itcntd = itcntd + 1
        fitcd = fitcd + 1.0_r8k
        itswp = 0
        ! Initialize Fracas subcode arrays to value at start of time step
        DO k = 1, naxn
            EffStrain(k) = EffStrain0(k)
            DO l = 1, ldir
                CldResidStrn(k,l) = CldResidStrn0(k,l)
                CldPlasStrn(k,l) = CldPlasStrn0(k,l)
                CldStrnRat(k,l) = CldStrnRat0(k,l)
                FuelResidStrn(k,l) = FuelResidStrn0(k,l)
                OldCldAxStrn(k) = OldCldAxStrn0(k)
                OldFuelAxStrn(k) = OldFuelAxStrn0(k)
                IF (ntstep > 1) THEN
                    OldCoolPrs(k) = OldCoolPrs0(k)
                    OldCladT(k) = OldCladT0(k)
                    OldGasPrs(k) = OldGasPrs0(k)
                ELSE
                    OldCladT(k) = CladAveTemp(k)
                    OldGasPrs(k) = GasPress(k)
                    OldCoolPrs(k) = CoolPress(k)
                ENDIF
                OldPelDis(k) = OldPelDis0(k)
                BOSGapIndex(k) = BOSGapIndex0(k)
            ENDDO
        ENDDO
        ! Compute deformations and stresses in each fuel rod
        ies(1) = 0
        ! Check to see If cladding failed or instability strain exceeded
        IF (ANY(RuptFailIndex(1:naxn) == 1)) THEN
            GasPress(1:naxn) = CoolPress(1:naxn)
            nedtsw = 1
        END IF
        !
        pelprm(1)  = frden
        pelprm(2)  = rshd
        pelprm(3)  = dishv0
        pelprm(4)  = dishd
        pelprm(5)  = pelh
        pelprm(6)  = tempcs
        pelprm(7)  = tmelt(1)
        pelprm(11) = coldw
        pelprm(12) = tflxc
        pelprm(13) = vs0(1)
        pelprm(14) = dvdt(1)
        Ifstor(9)  = ies(1)
        pelprm(8)  = TimeIncrement
        pelprm(9)  = bu(1)
        pelprm(27) = pfail
        pelprm(33) = pitch
        pelprm(29) = fgrns
        pelprm(30) = fotmtl
        pelprm(31) = rsntr
        pelprm(32) = tsntrk
        pelprm(28) = Time
        nfcall     = 0
        IF (itcntd > 1) nfcall = 1
        Ifstor(14) = nfcall
        Ifstor(15) = nedtsw
        Ifstor(16) = nbalsw
        ! Set boundary node numbers to values for use with dynamically dimensioned arrays
        Ifstor(1)  = igpnod
        Ifstor(2)  = ncladi
        Ifstor(3)  = nmesh
        Ifstor(4)  = naxn
        Ifstor(5)  = NSteadyTrans
        Ifstor(7)  = 1
        Ifstor(13) = modfal(1)
        !
        ! Determine current value of transient fission product fuel swelling (assumed as constant over all axial nodes)
        IF (TranSwell == 1) TranFuelSwell = polate (FuelGasSwell, Time, nFuelSwellPairs)
        !
        ! Call FRACAS-1 (deform)
        CALL deform
        !
        modfal(1) = Ifstor(13)
        flowbk = flwblk(1)
        ! If j Returned as -1, deform could not obtain solution. Time step needs to be reduced.
        ! Force cladding to be failed when no convergence in sub. fcmi
        IF (Ifstor(7) == (-1)) THEN
            RuptFailIndex(1) = 1
            GOTO 883
        ENDIF
        !
        atop = pi * (RadialBound(igpnod) ** 2)
        !
        ! Estimate plenum length
        plenl = vplen(1) / (pi * RadialBound(ncladi) ** 2) - delth
        !
        ! Cladding extension has exceeded fuel stack
        IF (-dcldh >= plenl) THEN
            dcldh = -plenl
            WRITE(ounit,901) delth, plenl
901         FORMAT('cladding axial extension limited by fuel stack, delth = ',e11.4,' plenl = ',e11.4)
            ! Assume cladding fails when this occurs
            RodFailIndex(naxn) = 1
        ENDIF
        !
        DO k = 1, naxn
            CldPermAxStrn(k) = CldPlasStrn(k,2)
            CldPermHoopStrn(k) = CldPlasStrn(k,1)
            PelletRad(k) = EOSRad(igpnod,k)
            RodOD(k) = 2.0_r8k * EOSRad(nmesh,k)
        ENDDO
        ! Compute internal pressure in fuel rods
        Ifail = 0
        tplen = tplna(nodgas,2,1)
        !
        DO k = 1, naxn
            ! Pass thermal gas gap thickness to gas pressure subcode
            CrackVolume(k) = CrackVolume(k) + Vreloc(k) * pi * (RadialBound(nmesh) + PelRadDeviat(k)) ** 2
            IF (RuptFailIndex(k) == 1 .OR. RuptFailIndex(k) == 4) Ifail = 1
            rwa7(k) = GasMolesAx0(k)
            ! If central void at node k, replace dish volume per unit length with central void volume per unit length. 
            ! EOSRad(2,k) = radius of void.
            IF (nvoid == 1 .AND. AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) THEN
                CentVoidVol(k) = pi * EOSRad(2,k) ** 2
            ELSE
                CentVoidVol(k) = 0.0_r8k
            ENDIF
        ENDDO
        TotalGasMoles = GasMoles0
        
        ! Fission gas release modeling
        IF (presfgr > 0) THEN
            ! Coding for user input of fission gas release
            IF (presfgr == 1) THEN
                ! Determine current release fraction by interpolation
                relfract = polate (relfraca, Time, nFGRpairs)
            ELSE IF (presfgr == 2) THEN
                ! Data read from FRAPFGR model using FRAPCON restart file
                DO k = 1, naxn
                    TempTemp(1:igpnod) = EOSTemp(1:igpnod,k)
                    rdot = 0.0_r8k
                    DO j = 1, ngasr
                        tempf = terp(ansr(j) / 12.0_r8k, RadialBound, TempTemp, igpnod)
                        IF (tempf >= 1950.0_r8k .OR. Iflag(j,1) == 1) THEN
                            rdot = rdot + gasavail1(k,j)
                            Iflag(j,1) = 1
                        ENDIF
                        IF (tempf >= 3300.0_r8k .OR. Iflag(j,2) == 1) THEN
                            rdot = rdot + gasavail2(k,j)
                            Iflag(j,2) = 1
                        ENDIF
                    ENDDO
                    molrel(k) = rdot * fmgp(k)
                ENDDO
                relfract = SUM(molrel) / SUM(fmgp)
            ENDIF
            
            ! Calculate released gas in cm3 and convert to moles (22.412 liter/g-mole)
            gmolxe = relfract * prodXe / 1000.0_r8k / 22.412_r8k
            gmolkr = relfract * prodKr / 1000.0_r8k / 22.412_r8k
            ! Add fission gas release to TotalGasMoles
            TotalGasMoles = GasMoles0 + gmolxe + gmolkr
            ! Update moles of each species of gas
            GasFraction(1) = gsmol0(1) / TotalGasMoles
            GasFraction(2) = gsmol0(2) / TotalGasMoles
            GasFraction(3) = (gsmol0(3) + gmolkr) / TotalGasMoles
            GasFraction(4) = (gsmol0(4) + gmolxe) / TotalGasMoles
            GasFraction(5) = gsmol0(5) / TotalGasMoles
            GasFraction(6) = gsmol0(8) / TotalGasMoles ! Switched to model Nitrogen
            GasFraction(7) = gsmol0(6) / TotalGasMoles
            GasFraction(8) = gsmol0(7) / TotalGasMoles
        END IF
        
        ! Set the gas composition to steam for a failed rod
        IF (Ifail == 1) THEN
            GasFraction = 0.0_r8k
            GasFraction(SteamIndex) = 1.0
        ENDIF
        !
        rwa7(naxn+1) = GasMolesAx0(naxn+1)
        pctop = CoolPress(naxn)
        !
        vplenc = vplen(1) - (delth - dcldh) * atop
        IF (vplenc < 0.005_r8k * vplen(1)) vplenc = 0.005_r8k * vplen(1)
        ! Set lower plenum volume & temperature
        vplenb = volbp
        tplenb = tplbot(nodgas,2)
        nswpmn = nswpm(1)
        IF (Ifail /= 1) CALL vswell (PelletRad, GapThick, AxialNodLen, naxn, Kswell, GapTemp, VolAveGasTemp, rl)
        !
        CALL gsflow
        ! Check for convergence of deform.-pres iteration loop by comparing pressure input to Fracas
        ! and that output from gsflow at node 1.
        nswpm(1) = nswpmn
        !
        DO k = 1, naxn
            IF (ntstep > 1) THEN
                ! If large cladding bulging, preclude p < pcool for next deform cal.
                IF (GasPress(1) > (CoolPress(1) + 10.0_r8k) .AND. GasPress(k) < CoolPress(k)) GasPress(k) = CoolPress(k)
            ENDIF
            IF (ndebug) WRITE(ounit,981) Time, itcntd, k, GasPress(k)
981         FORMAT(' COMPUT: Time = ',e13.6,' itcntd = ',i3,' k = ',i3,' GasPress = ',e13.6)
        ENDDO
        !
        IF (swllfr >= 1.0E-20 .AND. Ifail /= 1) THEN
            dvdt(1) = dvdtp
            GasMolesAx(naxn+1) = rwa7(naxn+1)
            GasMolesAx(Kswell) = rwa7(Kswell)
        ENDIF
        pinput(itcntd) = GasPress(1)
        potput(itcntd) = GasPress(naxn+1)
        pitave = ((fitcd - 1.0_r8k) * pitave + GasPress(naxn+1)) / fitcd
        pnew = pitave
        IF (ies(1) /= 1) THEN
            rc = ABS((potput(itcntd) - pinput(itcntd)) / pinput(itcntd))
            IF (rc > prsacc) itswp = 1
            ! If two successive time step reductions without time step increase, force explicit solution
            IF (ndtred >= 1) itswp = 0
            IF (itswp /= 0) THEN
                IF (itcntd >= 3) THEN
                    ! Perform estimate of gap pressure by higher order newton-raphson
                    CALL honr (pinput, potput, pnew, itcntd)
                    !
                    dpnew = pnew - pinput(itcntd)
                    IF (ABS(dpnew / pitave) > 0.05_r8k) dpnew = 0.05_r8k * pitave * dpnew / ABS(dpnew)
                    pnew = pinput(itcntd) + dpnew
                ENDIF
                id = itdmax - 2
                IF (nprdbg /= 0 .AND. itcntd > id) THEN
                    WRITE(ounit,941)
941                 FORMAT(' pinput vector ')
                    WRITE(ounit,943) (pinput(npi), npi = 1,itcntd)
943                 FORMAT(8(2x,e11.4))
                    WRITE(ounit,945)
945                 FORMAT(' potput vector' )
                    WRITE(ounit,943) (potput(npi), npi = 1,itcntd)
                ENDIF
                GasPress(1:naxn) = pnew
            ENDIF
        ENDIF
        !
        IF (itdmx < itcntd) itdmx = itcntd
        ! Update temperatures, radial coordinates, and gap pressures
        DO k = 1, naxn
            DO m = 1, nmesh
                IF (itcntd <= 1) THEN
                    IF (ntstep <= 1) BOSTemp(m,k) = EOSTemp(m,k)
                ENDIF
            ENDDO
        ENDDO
        ! If 2-D or 3-D calculations, set tempz values to EOSTemp values for theta and/or refined z mesh points.
        ! Check to see if convergence has occurred. If not, go through another iteration cycle.
        ! Checks of both pressure and deformation convergence made.
        IF (itcntd >= itdmax) THEN
879         WRITE(ounit,881)
881         FORMAT(///,' *** Iteration on pressure and deformation did not converge', &
              &        ' - time step reduced and execution continuing *** ')
            WRITE(ounit,882) Time, TimeIncrement
            WRITE(ounit,941)
            WRITE(ounit,943) (pinput(npi), npi = 1,itcntd)
            WRITE(ounit,945)
            WRITE(ounit,943) (potput(npi), npi = 1,itcntd)
            WRITE(ounit,947)
947         FORMAT(' Execution continuing ')
            !EXIT ConvgLoop
            GO TO 892
        ELSE
            IF ((itcntd <= (itdmax-2) .OR. ntstep /= 0) .AND. itswp == 1) GOTO 110
            !EXIT ConvgLoop
            GO TO 892
        ENDIF
        !
885     CONTINUE
        WRITE(ounit,886)
886     FORMAT(///,' *** Iteration on temperature did not converge - timestep reduced and execution continuing *** ')
883     CONTINUE
        WRITE(ounit,882) Time, TimeIncrement
882     FORMAT(/,' Nonconvergence occurred at time = ',e13.6,' with time step = ',e11.4)
        IF (ntstep > 1) THEN
            Time = Time - TimeIncrement
            ! Shift back time used as switch in sub. reflood
            oldtim = Time - TimeIncrement
            nitdt = nitdt + 1
            IF (numdta < 100) ndtred = ndtred + 1
            numdta = 0
            dtold = TimeIncrement
            ntstep = ntstep - 1
        ELSE
            powict = powict - dppowi
            WRITE(ounit,914) dppowi, powict
914         FORMAT(' After stat 86, dppowi = ',e11.4,' powict = ',e11.4)
            dppowi = dppowi * 0.1_r8k
            ! dppowi = power step at first time step  (kW/m)
            ! If (ndtred >= 1) explicit temperature solution
            ndtred = 1
            IF (dppowi < 0.2_r8k) dppowi = 0.2_r8k
        END IF
    !END DO ConvgLoop
        GO TO 60
892 CONTINUE
    !
    itswt = 0
    ! If temperature calc. from t/h code, bypass conversion check
    IF (ncool /= 7) THEN
        DO k = 1, naxn
            IF (NSteadyTrans == 1 .AND. IterationCount >= (MaximumIterations - 2) .AND. IndexTempConverg(k) == 1) THEN
                WRITE(ounit,871) Time, IterationCount, k
871             FORMAT(/,' At time(sec) = ',e13.6,' IterationCount = ',i3,' k = ',i3, &
                  &      ' Temperature-deformation loop did not converge')
                WRITE(ounit,873) k
873             FORMAT(' Temperature distribution at axial node',i4)
                WRITE(ounit,874) (EOSTemp(l,k),l = 1,nmesh)
874             FORMAT(8(2x,e11.4))
                WRITE(ounit,875) HGapAv(k),GapThick(k),GasPress(k), RInterfacPrs(k)
875             FORMAT(' hgap(btu/hr-ft2-F) = ',e11.4, ' GapThick(ft) = ',e11.4,' GasPress(psi) = ',e11.4,' interface pressure ', &
                  &    '(psi) = ',e11.4)
            ENDIF
            IF (IndexTempConverg(k) == 1) itswt = 1
        ENDDO
        ! ntstep=1 only on last power step of first ramp
        IF (IterationCount == MaximumIterations .AND. ntstep <= 1) itswt = 0
        IF (IterationCount == MaximumIterations .AND. itswt == 1) GOTO 885
        IF (itswt == 1) GOTO 45
        IF (nitmin == 1 .AND. IterationCount < 2) GOTO 45
    ENDIF
    dtold = TimeIncrement
    ! Temperature-pressure-deformation iteration loop converged.
    ! Now perform calculations that are done explicitly.
    ! If option specified, calculate oxygen concentration in cladding.
    IF (modmw == 0) THEN
        DO k = 1, naxn
            tempc0 = 0.5_r8k * (BOSTemp(ncladi,k) + BOSTemp(nmesh,k))
            tempc1 = 0.5_r8k * (EOSTemp(ncladi,k) + EOSTemp(nmesh,k))
            ! convert from F to K
            tempc0 = tfk(tempc0)
            tempc1 = tfk(tempc1)
            ! check for high cladding temperature
            IF (tempc1 > 1073.0_r8k) THEN
! LOJ qt15: REMARK
!          Only deformation by plastic strain considered in calculation
!          of metal-water reactions with Cathcart-Pawel model.
!          Also other deformation mechanisms should be included.
                drodm0 = 2.0_r8k * RadialBound(nmesh) / 3.28084_r8k * (1.0_r8k + CldPlasStrn(k,1))
                thkm0 = (RadialBound(nmesh) - RadialBound(ncladi)) / 3.28084_r8k
                ! convert start of time step oxide thickness from inches to meters
                thkox0 = BOSOxideThick(k) / (12.0_r8k * 3.28084_r8k)
                IF (ProtectiveOxide == 1) thkox0 = BOSOxideThick(k) / (12.0_r8k * 3.28084_r8k) - oxideod(k)
                dmxi1k = AlphaThk1(k)
                woxupt = OxygenUptake(k)
                IF (ProtectiveOxide == 1) woxupt = OxygenUptake(k) - oxideod(k) * 5680.0_r8k * 0.2597_r8k
                pfcpa = RInterfacPrs(k) * 6894.76_r8k
                !
                CALL cobild (tempc0, tempc1, TimeIncrement, thkm0, drodm0, pfcpa, IndexPCMI(k), &
                  &          IndexPCMIOnce(k),thkox0, dmxi1k, OxStAlphaThkNearFuel(k), &
                  &          OxStAlphaThkRemain(k), OxConcenAAO(k), OxConcenABO(k), OxConcenACO(k), &
                  &          OxConcenADO(k), OxConcenAEO(k), OxConcenAFO(k), OxConcenAGO(k), &
                  &          OxConcenAHO(k), OxConcenAIO(k), OxConcenAAI(k), OxConcenABI(k), &
                  &          OxConcenACI(k), OxConcenADI(k), OxConcenAEI(k), OxConcenAFI(k), &
                  &          OxConcenAGI(k), OxConcenAHI(k), OxConcenAII(k), woxupt, &
                  &          OxiPowerGen(k)  ,PrcntSatBeta(k) ,OxygenConcenAve(k), BetaThickness(k), &
                  &          iStoicGrad)
                !
                EOSOxideThick(k) = 12.0_r8k * 3.28084_r8k * thkox0
                IF (ProtectiveOxide == 1) EOSOxideThick(k) = 12.0_r8k * 3.28084_r8k * (thkox0 + oxideod(k))
                AlphaThk2(k) = dmxi1k
                OxygenUptake(k) = woxupt
                IF (ProtectiveOxide == 1) OxygenUptake(k) = woxupt + oxideod(k) * 5680.0_r8k * 0.2597_r8k
            ELSE ! No high temperature oxidation occurring
                OxiPowerGen(k) = 0.0_r8k
                ! oxide thickness (inches)
                EOSOxideThick(k) = BOSOxideThick(k)
                ! oxygen stabilized alpha thickness
                AlphaThk2(k) = AlphaThk1(k)
            ENDIF
        ENDDO
    END IF
    ! Calculate cladding annealing
    ! Calculate rate of change of cladding temperature (K/s)
    ! calculate change in effective fluence and cold work during time step
    DO k = 1, naxn
        tempc0 = 0.5_r8k * (BOSTemp(ncladi,k) + BOSTemp(nmesh,k))
        tempc1 = 0.5_r8k * (EOSTemp(ncladi,k) + EOSTemp(nmesh,k))
        ! convert from F to K
        tempc0 = tfk(tempc0)
        tempc1 = tfk(tempc1)
        tmpdot = (tempc1 - tempc0) / TimeIncrement
        cflux1 = FastFlux(k)
        ! Force no incrementing of fluence until time advanced
        IF (ntstep /= 0) CALL caneal (cflux1, TimeIncrement, tempc0, tmpdot, EffFastFluStrenCoef(k), &
          &                           EffFastFluStrnHardExp(k), EffColdWkStrenCoef(k), &
          &                           EffColdWkStrnHardExp(k))
    ENDDO
    !
    IF (nbalsw /= 1 .AND. ifbaln /= 1 .AND. mechan /= 1) THEN
        ! If cladding failure has occurred, bypass balon2 call coding associated with balon2 follows:
        ! Determine if balon2 should be called and which FrapTran axial node to be used
        kbaln = knonue
        IF (kbaln /= 0) THEN
            IF (nbncal /= 1) THEN
                ! First balon2 call. force ballooning to occur at hot node
                tcldmx = 0.0_r8k
                DO k = 1, naxn
                    IF (CladAveTemp(k) > tcldmx) THEN
                        knonue = k
                        tcldmx = CladAveTemp(k)
                    ENDIF
                ENDDO
                kbaln = knonue
                zbaln = 0.08_r8k
            ENDIF
            ! Set up for balon2 call
            dtbal  = TimeIncrement
            dtobal = dtold
            emwbal = 1000.0_r8k * WatrMetlEnrgy(kbaln) / ftom
            fabal  = 0.0_r8k
            flxbal = FastFlux(kbaln)
            htcgba = HGapAv(kbaln) * 5.6785_r8k
            htcbal = FilmCoeffAv(kbaln) * 5.6785_r8k
            IF (htcbal < 1.0_r8k) htcbal = 1.0_r8k
            h0bal  = (RadialBound(nmesh) - RadialBound(ncladi)) * ftom
            pcbal  = CoolPress(kbaln) * 6894.76_r8k
            psbal  = GasPress0(kbaln) * 6894.76_r8k
            qbal   = HeatFlux(kbaln) * 11360.0_r8k * EOSRad(nmesh,kbaln) / EOSRad(igpnod,kbaln)
            qlbal  = HeatFlux(kbaln) * 11360.0_r8k * EOSRad(nmesh,kbaln) / EOSRad(igpnod,kbaln)
            rfbal  = EOSRad(igpnod,kbaln) * ftom
            rmpbal = roi(1) * ftom
            r0bal  = RadialBound(nmesh) * ftom
            tbkbal = tfk(BulkCoolTemp(kbaln))
            tc0bal = tfk(CladAveTemp(kbaln))
            tf0bal = tfk(EOSTemp(igpnod,kbaln))
            tgbal  = tfk(GapTemp(kbaln))
            ! calculate average fuel temperature at ballooning node
            ! calculate like in Subroutine prntot
            ! find average fuel temperature
            dvsum = 0.0_r8k
            tsum  = 0.0_r8k
            DO l = 1, igpnod
                IF (l == 1) THEN
                    IF (AxNodElevat(kbaln) > zvoid2 .OR. AxNodElevat(kbaln) < zvoid1) THEN
                        dv = pi * ((RadialBound(l+1) + RadialBound(l)) / 2.0_r8k) ** 2 * AxialNodLen(kbaln)
                    ELSE
                        dv = 0.0_r8k
                    ENDIF
                ELSE IF (l == igpnod) THEN
                    dv = pi * (RadialBound(l) ** 2 - ((RadialBound(l-1) + RadialBound(l)) / 2.0_r8k) ** 2) * AxialNodLen(kbaln)
                ELSE IF (l == 2) THEN
                    IF (AxNodElevat(kbaln) > zvoid2 .OR. AxNodElevat(kbaln) < zvoid1) THEN
                        dv = pi * (((RadialBound(l) + RadialBound(l+1)) / 2.0_r8k) ** 2 - &
                          &       ((RadialBound(l-1) + RadialBound(l)) / 2.0_r8k) ** 2) * AxialNodLen(kbaln)
                    ELSE
                        dv = pi * ((RadialBound(l+1) + RadialBound(l)) / 2.0_r8k) ** 2 * AxialNodLen(kbaln)
                    ENDIF
                ELSE
                    dv = pi * (((RadialBound(l) + RadialBound(l+1)) / 2.0_r8k) ** 2 - &
                      &       ((RadialBound(l-1) + RadialBound(l)) / 2.0_r8k) ** 2) * AxialNodLen(kbaln)
                END IF
                dvsum = dvsum + dv
                tsum  = tsum + dv * EOSTemp(l,kbaln)
            ENDDO
            !
            tfave  = tsum / dvsum
            tfavba = tfk(tfave)
            timbal = Time
            modbal = 1
            nprntb = 0
            zndbal = rl * ftom
            IF (naxn < 3) THEN
                zm1bal = 0.46_r8k
                ztmax =  0.5_r8k
                zp1bal = 0.54_r8k
                tm1bal = tc0bal * 0.99_r8k
                tp1bal = tc0bal * 0.99_r8k
            ELSE
                IF (kbaln == naxn) THEN
                    zm1bal = AxNodElevat(kbaln-2) * ftom
                    ztmax = AxNodElevat(kbaln) * ftom
                    zp1bal = AxNodElevat(kbaln-1) * ftom
                    tm1bal = tfk(CladAveTemp(kbaln-2))
                    tp1bal = tfk(CladAveTemp(kbaln-1))
                ELSE IF (kbaln == 1) THEN
                    zm1bal = AxNodElevat(kbaln+1) * ftom
                    ztmax  = AxNodElevat(kbaln) * ftom
                    zp1bal = AxNodElevat(kbaln+2) * ftom
                    tm1bal = tfk(CladAveTemp(kbaln+1))
                    tp1bal = tfk(CladAveTemp(kbaln+2))
                ELSE
                    zm1bal = AxNodElevat(kbaln-1) * ftom
                    ztmax  = AxNodElevat(kbaln) * ftom
                    zp1bal = AxNodElevat(kbaln+1) * ftom
                    tm1bal = tfk(CladAveTemp(kbaln-1))
                    tp1bal = tfk(CladAveTemp(kbaln+1))
                ENDIF
            ENDIF
            !
            cexh2l = cexh2a(kbaln)
            !
            CALL far1 (GasFraction, Baln2Twall, gadolin(kbaln))
            !
            nbncal = 1
            chstrs = chstrs / 6894.76_r8k
            !
            IF (ifbaln == 1) RodFailIndex(kbaln) = 1
        END IF
    END IF
    numdta = numdta + 1
    ! Implicit solution If ndtred=0, explicit If 1
    IF ((ntstep >= 2 .AND. numdta >= 100) .OR. ntstep == 1) ndtred = 0
    nitdt = 0
    ! Solution converged for this time step.
    ! Calculate amount of iodine released from fuel to fuel-cladding gap.
    aidtot = 0.0_r8k
    aidlng = 0.0_r8k
    aidsht = 0.0_r8k
    cestot = 0.0_r8k
    ainsur = 0.0_r8k
    timpro = tflux
    IF (trest > 1000.0_r8k) timpro = trest
    !
    DO k = 1, naxn
        ! calculate radially averaged fuel burnup during time step
        dzpowi = 0.5_r8k * (AxialPowr(k) + AxialPowr0(k)) * 0.001_r8k * TimeIncrement
        !Consider the possibility of a central void, and use the value for fuel density from input (in lb/ft3)
        nfirst = 1
        IF (nvoid == 1) THEN
            IF(AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nfirst = 2
        ENDIF
        ! Pellet cross-sectional area (ft^2)
        rparea = pi * (RadialBound(igpnod) ** 2 - RadialBound(nfirst) ** 2) 
        ! Calculate mass of uranium / length (kg/m) in axial zone k.
        amuran = 0.880177_r8k * 16.018463_r8k * rhof * rparea * ftom ** 2
        ! amurak = amount of fuel in axial zone k (kg)
        amurak = amuran * AxialNodLen(k) * ftom
        IF (amuran /= 0.0_r8k) delbu = dzpowi / amuran
        busi = AxBurnup(k)
        ! ft2m2 = factor for converting from ft**2 to m**2
        ft2m2 = 0.09290343_r8k
        ainsur = ainsur + 2.0_r8k * pi * RadialBound(ncladi) * AxialNodLen(k) * ft2m2
        ! Central void may still exist
        DO m = nfirst, (igpnod - 1)
            delbu1 = delbu
            ftmax = tfk(BOSTemp(m,k))
            ftemp = 0.5_r8k * (tfk(EOSTemp(m,k)) + tfk(EOSTemp(m+1,k)))
            !
            CALL cesiod (timpro, ftmax, delbu1, TimeIncrement, ftemp, frden, cescom, aidcom, busi)
            !
            aidcom(1) = MAX(0.0_r8k, aidcom(1))
            ! amural = amount of fuel in radial mesh m  (kg)
            amural = ((RadialBound(m+1) ** 2 - RadialBound(m) ** 2) * pi / rparea) * amurak
            aidtot = aidtot + aidcom(1) * amural
            aidsht = aidsht + amural * (aidcom(4) + aidcom(5) + aidcom(6) + aidcom(7) + aidcom(8))
            aidlng = aidlng + amural * (aidcom(2) + aidcom(3))
            cestot = cestot + amural * cescom(1)
            aidtot = MAX(0.0_r8k, aidtot)
            aidsht = MAX(0.0_r8k, aidsht)
            aidlng = MAX(0.0_r8k, aidlng)
        ENDDO
    ENDDO
    ! Calculate iodine surface concentration.
    aidsur = aidtot / ainsur
    ! Assume axially uniform iodine concentration.
    IodineContent(1:naxn) = aidsur
    !
    RuptFailIndex(1:naxn) = RodFailIndex(1:naxn)
    Ihtreg(1:naxn) = Ih(1:naxn)
    ! Check for any failure nodes
    IF (ANY(RodFailIndex(1:naxn) == 1) .OR. ANY(RodFailIndex(1:naxn) == 4)) THEN
        Ifchk = 1
    ELSE
        Ifchk = 0
    END IF
    ! Store information needed for energy balance printout
    ! EnergyPerL(k) has units of kW-s/ft
    DO k = 1, naxn
        ! If (modmw == 2) EOSOxideThick(k) = EOSOxideThick(k) * (1.0_r8k + (CldPlasStrn(k,3) - CldPlasStrn0(k,3)))
        IF (NSteadyTrans == 1) THEN
            HeatFlux0(k) = HeatFlux(k)
            EnergyPerL(k) = 0.0_r8k
            HFXSum(k) = 0.0_r8k
            Enthl(k) = 0.0_r8k
        ELSE
            ! Add oxidation created energy to fuel input energy
            EnergyPerL(k) = EnergyPerL(k) + WatrMetlEnrgy(k) * TimeIncrement
            EnergyPerL(k) = 0.5_r8k * (AxialPowr0(k) + AxialPowr(k)) * TimeIncrement + EnergyPerL(k)
            ! HFXSum(k) has units of kW-s/ft
            HFXSum(k) = 0.5_r8k * (HeatFlux(k) + HeatFlux0(k)) * TimeIncrement * pi * RodOD(k) / powcnv + HFXSum(k)
            IF (nvoid == 1 .AND. (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2)) THEN
                Enthl(k) = EnergyPerL(k) / (pi * rhof * (RadialBound(igpnod) ** 2 - RadialBound(2) ** 2))
            ELSE
                Enthl(k) = EnergyPerL(k) / (pi * rhof * RadialBound(igpnod) ** 2)
            END IF
        END IF
        ! Calculate energy absorbed in cladding
        CladEnrgPerL(k) = 0.0_r8k
        ! Calculate stored energy in each mesh in cladding
        DO l = (igpnod + 1), nmesh
            tcld0 = tempcs
            tcldp = EOSTemp(l,k)
            tcld0 = tfk(tcld0)
            tcldp = tfk(tcldp)
            !
            cpint = ccpint(tcld0,tcldp) * 0.42987e-3_r8k
            !
            IF (l == (igpnod + 1)) THEN
                dv = (0.5_r8k * (RadialBound(l+1) + RadialBound(l))) ** 2 - RadialBound(l) ** 2
            ELSE IF (l == nmesh) THEN
                dv = RadialBound(l) ** 2 - (0.5_r8k * (RadialBound(l) + RadialBound(l-1))) ** 2
            ELSE
                dv = (0.5_r8k * (RadialBound(l+1) + RadialBound(l))) ** 2 - &
                  &  (0.5_r8k * (RadialBound(l) + RadialBound(l-1))) ** 2
            ENDIF
            ! units of CladEnrgPerL are kW-s/ft , cpint has units of btu/lb
            CladEnrgPerL(k) = CladEnrgPerL(k) + rhoc * pi * dv * cpint / powcnv
        ENDDO
    ENDDO
    ! In case rapid cladding temperature rise due to oxidation, enforce reduction in time step.
    dtenfo = 1.0E10_r8k
    !
    DO k = 1, naxn
        IF (WatrMetlEnrgy(k) >= 0.01_r8k) THEN
            tmpdot = (EOSTemp(nmesh,k) - BOSTemp(nmesh,k)) / TimeIncrement
            IF (tmpdot >= 1.0_r8k) THEN
                dtx = 50.0_r8k / tmpdot
                IF (dtx < dtenfo) dtenfo = dtx
            ENDIF
        ENDIF
    ENDDO
    ! Round off enforced time step
    IF (dtenfo > 50.0_r8k .AND. dtenfo < 200.0_r8k) dtenfo = 100.0_r8k
    IF (dtenfo > 15.0_r8k .AND. dtenfo <= 50.0_r8k) dtenfo = 25.0_r8k
    IF (dtenfo >  8.0_r8k .AND. dtenfo <= 15.0_r8k) dtenfo = 10.0_r8k
    IF (dtenfo >  3.0_r8k .AND. dtenfo <=  8.0_r8k) dtenfo = 5.0_r8k
    IF (dtenfo >  1.5_r8k .AND. dtenfo <=  3.0_r8k) dtenfo = 2.0_r8k
    IF (dtenfo >  0.8_r8k .AND. dtenfo <=  1.5_r8k) dtenfo = 1.0_r8k
    IF (dtenfo >  0.3_r8k .AND. dtenfo <=  0.8_r8k) dtenfo = 0.5_r8k
    IF (dtenfo > 0.15_r8k .AND. dtenfo <=  0.3_r8k) dtenfo = 0.2_r8k
    IF (dtenfo > 0.08_r8k .AND. dtenfo <= 0.15_r8k) dtenfo = 0.1_r8k
    IF (dtenfo <= 0.08_r8k) dtenfo = 0.05_r8k
    !
    END SUBROUTINE comput
    !
    !
    !
    SUBROUTINE honr (xi, xo, xnew, np)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine honr performs higher order newton-raphson estimate of quantity being iterated upon
    !
    ! Input
    !
    ! xi(n)  - input guess of iteration x for iteration number n
    ! xo(n)  - output guess of iteration x for iteration number n
    ! np     - number of iterations performed
    !
    ! Output
    !
    ! xnew   - new estimate of converged value of iterat x
    !
    INTEGER(ipk) :: j, k, l, m, jmin
    INTEGER(ipk), INTENT(IN) :: np
    REAL(r8k) :: ximin, xi1, xo1, xi2, xo2, s
    REAL(r8k), INTENT(OUT) :: xnew
    INTEGER(ipk), DIMENSION(np) :: nord
    REAL(r8k), DIMENSION(:), INTENT(IN) :: xi, xo
    !
    nord(1:np) = 0
    ! Order xi values according to increasing values
    DO m = 1, np
        ximin = 1.0e30_r8k
        DO j = 1, np
            IF (xi(j) >= ximin) GOTO 300
            DO l = 1, np
                IF (nord(l) == j) GOTO 300
            ENDDO
            jmin = j
            ximin = xi(j)
300     ENDDO
        nord(m) = jmin
    ENDDO
    ! Find line connecting points (xi(j),xo(j)) and (xi(j+1),xo(j+1)) that intersects line 45 degrees to abcissa
    DO k = 1, (np - 1)
        j = nord(k)
        m = nord(k+1)
        IF (xo(j) >= xi(j) .AND. xo(m) < xi(m)) GOTO 700
    ENDDO
    WRITE(ounit,902)
902 FORMAT(///,'  *** No lines formed connecting xi, xo pairs intersects 45 degree line - Subroutine honr *** ')
    WRITE(ounit,605) np
605 FORMAT(' Non-intersection at iteration #',i4)
    IF (xo(np-1) <= xi(np-1)) THEN
        ! Case where all output points lie below input points force lower guess of final answer for next try
        xnew = 0.95_r8k * xi(np)
    ELSE
        ! Case where all output points lie above input points
        xnew = 1.05_r8k * xi(np)
    ENDIF
    !
    RETURN
    ! Find intersection point of line connecting xi, xo points and 45 degree line
700 xi1 = xi(j)
    xo1 = xo(j)
    xi2 = xi(m)
    xo2 = xo(m)
    IF (ABS(xi2 - xi1) < 1.0e-20_r8k) THEN
        xnew = xi2
    ELSE
        s = (xo2 - xo1) / (xi2 - xi1)
        xnew = (xo1 - s * xi1) / (1.0_r8k - s)
    ENDIF
    !
    END SUBROUTINE honr
    !
    !
    !
    SUBROUTINE timstp
    USE Kinds
    USE variables_fraptran, ONLY : ounit, timeincrement, dtp, nitdt, dtenfb, dtenfo, dtold
    IMPLICIT NONE
    !> @brief
    !> Subroutine computes new time step. extimate is made of largest
    !> time step which will produce convergent solution
    !
    ! Input
    !
    ! dtold  - Past time step (sec)
    ! dtp    - Maximum permissible time step prescribed in input (sec)
    ! nitdt  - Number of times that time step has been reduced after nonconvergence encountered
    ! dtenfb - Enforced reduced time step (s) to keep calculations accurate during cladding ballooning
    ! dtenfo - Enforced time step to keep calculations accurate during cladding oxidation
    !
    ! Output
    !
    ! TimeIncrement - To be computed: new time step (sec)
    !
    IF (nitdt < 1) THEN
        TimeIncrement = dtp
        IF (dtenfb < TimeIncrement) TimeIncrement = dtenfb
        IF (dtenfo < TimeIncrement) TimeIncrement = dtenfo
    ELSE IF (nitdt >= 5) THEN
        WRITE(ounit,21)
21      FORMAT(//' *** Execution stopped in Subroutine: timstp; Pressure and deformation iterations could not converge', &
          &      ' after time step reduction ***')
        ERROR STOP 'Execution stopped in Subroutine: timstp; Pressure and deformation iterations did not converge.'
    ELSE
        TimeIncrement = 0.5_r8k * dtold
    ENDIF
    !
    END SUBROUTINE timstp
!
END MODULE TimeStep

