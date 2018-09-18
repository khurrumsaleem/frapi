MODULE Uncertainties_fraptran
    USE Kinds_fraptran
    USE variables_fraptran
    USE Dyna_h_fraptran
    USE resti_h_fraptran
    USE conversions_fraptran
    USE Uncertainty_Vals_fraptran
    IMPLICIT NONE
    !>@brief
    !> Module Uncertainty is used to_fraptran:
    !> 1) Bias the following parameters by a fraction of their value:
    !       a) Fuel thermal conductivity
    !       b) Cladding thermal conductivity
    !       c) Fuel thermal expansion
    !       d) Cladding thermal expansion
    !       e) Fuel specific heat capacity
    !       f) Cladding yield stress
    !       g) Cladding to coolant heat transfer coefficient
    !>@author
    !> Patrick Raynaud, NRC
    !>@date
    !> 5/22/2015
    !
    PRIVATE
    PUBLIC :: dktout, ReadUncertainty, Uncert_fuelref
    ! Reference fuel enthalpy from steady-state operation
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: Uncert_fuelref
    !
    CONTAINS
    !
    SUBROUTINE AllocateUncertaintyvars
    !>@brief
    !> Subroutine AllocateUncertaintyvars allocates the Uncertainty variables for the rod being modeled
    !>@author
    !> Patrick Raynaud, NRC
    !>@date
    !> 5/22/2015
    ! Input
    ! idx    - fuel rod #
    ! Output
    ! None
    IMPLICIT NONE
    
    ! Allocate arrays
    ALLOCATE (dtdkta(1:pre_nt+2))
    ALLOCATE (dktouts(1,1))
    
    ! Assign Default Values
    sigfuelthermcond = 1.0_r8k
    sigcladthermcond = 1.0_r8k
    sigfuelthermexp = 1.0_r8k
    sigcladthermexp = 1.0_r8k
    sigfuelheatcapa = 1.0_r8k
    sigcladyieldstr = 1.0_r8k
    sigsurfhtc = 1.0_r8k
    tdkt = 0.0_r8k
    dtdkta = 0.0_r8k
    dktouts = 0.0_r8k
    dakota = .FALSE.
    ndktn = 0_ipk
    ndktparams = 0_ipk
    dktoutcounter = 1_ipk
    !
    END SUBROUTINE AllocateUncertaintyvars
    !
    !
    !
    SUBROUTINE ReadUncertainty
    !>@brief
    !> Subroutine ReadDispersal reads the dispersal input block
    !>@author
    !> Patrick Raynaud, NRC
    !>@date
    !> 5/22/2015
    !
    ! Input
    !
    ! idx       - fuel rod # being modeled
    !
    ! Internal
    !
    ! InputStat - identifier for input error (< 0 = EOF, > 0 = input error)
    USE ErrorMsg_fraptran, ONLY : namelist_read_error
    IMPLICIT NONE
    !
    REAL(r8k) :: plotsteps = 0.0_r8k        ! Calculated number of plot output steps
    INTEGER(ipk) :: Value, InputStat
    INTEGER(ipk) :: nplotsteps = 0_ipk      ! Integer conversion of calculated number of plot output steps
    LOGICAL :: EndofFile = .FALSE., UncertExists = .FALSE.
    CHARACTER(LEN=100) :: line
    INTEGER(ipk) :: Iflag
    INTEGER(ipk) :: i, icount, idktn
    
    ! Define $uncertainties
    NAMELIST / uncertainties / sigfuelthermcond, sigfuelthermexp, sigfuelheatcapa, &
      &                        sigcladthermcond, sigcladthermexp, sigcladyieldstr, &
      &                        sigsurfhtc, dakota, dtdkta, ndktparams
    
    ! Initializize arrays
    CALL AllocateUncertaintyvars
    
    ! Read $uncertainties
    ! This first checks to see if the block $uncertainties exists. Extra logic was addded so that
    ! it does not matter where in the input file the block was placed. If the block exists
    ! (if the words $uncertainties are in the input file), UncertExists becomes true and it reads the
    ! block. If not, once it reaches the end of the file, it will skip the read.
    REWIND iunit
    ReadLoop: DO
        READ (iunit, '(A)', IOSTAT = InputStat) line
        IF (InputStat == 0) THEN
            Value = INDEX(line,'$uncertainties')
            IF (Value > 0) THEN
                BACKSPACE iunit
                ! Make sure the line is not commented out
                Value = INDEX(line(1:2),'!')
                IF (Value == 0) UncertExists = .TRUE.
                EXIT ReadLoop
            END IF
        ELSE IF (InputStat < 0) THEN
            ! End of File or Record
            UncertExists = .FALSE.
            EXIT ReadLoop
        ELSE
            ! Error
            UncertExists = .FALSE.
            WRITE (0,*) 'Syntax error in namelist $uncertainties part of line'
            WRITE (ounit,*) 'Syntax error in namelist $uncertainties part of line'
            STOP
        END IF
    END DO ReadLoop
    !
    IF (UncertExists) THEN
        !
        READ (iunit, uncertainties, IOSTAT = InputStat)
        IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'uncertainties')
        !
        ! Deallocate, reallocate, and reassign dktouts once inputs have been read
        !
        DEALLOCATE (dktouts)
        ! Calculate second dimension for dktouts based on number of plot output time steps
        DO i = 1, npltn - 1
            plotsteps = plotsteps + (dtplta(2*i+2) - dtplta(2*i)) / dtplta(2*i-1)
        END DO
        ! Note: nplotsteps may be larger than what is needed by up to npltn elements
        nplotsteps = NINT(plotsteps) + npltn
        !Reallocate and reassign dktouts
        ALLOCATE (dktouts(ndktparams,nplotsteps))
        dktouts = 0.0_r8k
        !
        ! Print summary of uncertainty multiplier inputs
        !
        WRITE(ounit,200)
200     FORMAT(/,40x,'model uncertainty multiplier input block')
        WRITE(ounit,201) sigfuelthermcond, sigcladthermcond, sigfuelthermexp, sigcladthermexp, &
          &              sigfuelheatcapa, sigcladyieldstr, sigsurfhtc
        !
201     FORMAT(/15x,'Model uncertainty multipliers:',/, &
          &     20x,'Fuel thermal conductivity multiplier:         ',1pe11.4,/, &
          &     20x,'Cladding thermal conductivity multiplier:     ',1pe11.4,/, &
          &     20x,'Fuel thermal expansion multiplier:            ',1pe11.4,/, &
          &     20x,'Cladding thermal expansion multiplier:        ',1pe11.4,/, &
          &     20x,'Fuel heat capacity multiplier:                ',1pe11.4,/, &
          &     20x,'Cladding yield stress multiplier:             ',1pe11.4,/, &
          &     20x,'Surface heat transfer coefficient multiplier: ',1pe11.4)
        !
        ! Count and process/error check the input pairs for dakota ouput frequency
        !
        ndktn = 0_ipk
        Iflag = 0_ipk
        icount = 1_ipk
        DO WHILE (Iflag == 0_ipk)
            IF (dtdkta(icount) == 0.0_r8k .AND. dtdkta(icount+1) == 0.0_r8k) Iflag = 1_ipk
            icount = icount + 2_ipk
        ENDDO
        ndktn = (icount - 3_ipk) / 2_ipk
        ! If DAKOTA output frequency pairs are specified, set dakota to .TRUE. to write DAKOTA output file
        IF (ndktn >= 1_ipk) dakota = .TRUE.
        !
        ! Write DAKOTA output specification summary if selected
        !
        IF (dakota) WRITE(ounit,202)
202     FORMAT(/15x,'DAKOTA output file will be generated')
        ! 
        ! If user specifes DAKOTA output is required, but no DAKOTA output frequency is specified,
        ! set default DAKOTA output frequency to code output frequency, or if code output frequency
        ! is not specified, set DAKOTA output frequency to 10 times the time step size
        !
        IF ((dakota) .AND. (ndktn <= 0_ipk)) THEN
            IF (npdtpo >= 1_ipk) THEN
                WRITE(ounit,203)
203             FORMAT(/10x, 'DAKOTA output frequency unspecified by user: frequency defaults to code output frequency')
                idktn = npdtpo * 2_ipk
                DO i = 1, idktn, 2
                    dtdkta(i+1) = dtpoa(i+1)
                    dtdkta(i) = dtpoa(i)
                ENDDO
                ndktn = idktn / 2_ipk
            ELSE
                WRITE(ounit,204)
204             FORMAT(/10x, 'DAKOTA output frequency unspecified by user: frequency defaults to 5 times time step')
                idktn = ndtmax * 2_ipk
                DO i = 1, idktn, 2
                    dtdkta(i+1) = dtmaxa(i+1)
                    dtdkta(i) = dtmaxa(i) * 5.0_r8k
                ENDDO
                ndktn = idktn / 2_ipk
                END IF
        END IF
        !
        ! If ndktn >= 1, printout values to be used by the code and perform checks for validity
        !
        IF (ndktn >= 1_ipk) THEN
            WRITE(ounit,205) ndktn,(dtdkta(i),i = 1,2*ndktn)
205         FORMAT(/10x,'DAKOTA output interval specification'/&
                &   15x,'no. of pairs',3x,3(5x,'DAKOTA output',2x,'problem',3x)/&
                &   30x,3(5x,'interval',7x,'time',6x)/&
                &   30x,3(5x,'(s)',12x,'(s)',7x)/&
                &   15x,i10,5x,6(2x,1pe13.4)/&
                &  (30x,6(2x,1pe13.4)))
            ! Check that the DKOTA ouput frequencies are positive
            DO i = 1, ndktn
                IF (dtdkta(2*i-1) <= 0.0_r8k) THEN
                    WRITE(ounit,206) i, dtdkta(2*i-1)
206                 FORMAT(/,' DAKOTA print interval, dtdkta(',i3,')=',f10.4,' must be positive')
                    STOP
                ENDIF
            ENDDO
            ! Check to make sure dtdkta(even) is increasing
            DO i = 2, ndktn
                IF (dtdkta(2*i) < dtdkta(2*(i-1))) THEN
                    WRITE(ounit,207) 2*i, dtdkta(2*i), 2*(i-1), dtdkta(2*(i-1))
207                 FORMAT(/,' DAKOTA output time values are not in increasing: dtdkta(',i3,')=',f10.4,' < dtdkta(',i3,')=',f10.4)
                    STOP
                ENDIF
            ENDDO
        ENDIF
        !
        ! write end of summary
        !
        WRITE(ounit,300)
300     FORMAT(/' Uncertainty multiplier block input completed'//)
    ENDIF
    !
    END SUBROUTINE ReadUncertainty
    !
    !
    SUBROUTINE dktout (node)
    USE Kinds_fraptran
    USE conversions_fraptran
    USE variables_fraptran
    USE Temperature_fraptran
    IMPLICIT NONE
    !>@brief
    !> Subroutine dktout prints out the code results of interest to the dkt file for DAKOTA post-processing
    !> This subroutine is based on subroutine grafout, and will need to further be modified for increased flaxibility
    !> Coded on 09/02/2015 by P. Raynaud for OECD/NEA/CSNI/WGFS RIA BEnchmark Phase II, Task 2
    !> all statements from the grafout subroutine that were not required for the benchmark were commented out.
    !
    INTEGER(ipk) :: k, l, j, nvoids
    INTEGER(ipk), INTENT(IN) :: node
    REAL(r8k) :: aupk, ttlmwe, dvsum, tsum, sum, sumv, dv, dv1, dv2, r1, r2, r3, tbar
    REAL(r8k), DIMENSION(nmesh, naxn) :: edat
    !
    IF (.NOT. ALLOCATED(Uncert_fuelref)) THEN
        ALLOCATE(Uncert_fuelref(1:naxn))
        Uncert_fuelref = 0.0_r8k
    ENDIF
    edat = 0.0_r8k
    !
    !        the coneu conversion are as follows;
    !        TYPE     from                to
    !
    !         1       K                   F
    !        11       ft                  mils
    !        12       btu/(s.ft2)         W/m2
    !        13       btu/((hr)(ft2)(F))  W/((m2)(K))
    !        14       ft                  m
    !        15       in                  mm
    !        16       mils                mm
    !        17       in3                 mm3
    !        18       psia                mpa
    !        19       kW/ft               kW/m
    !        20       lb/((hr)(ft2))      kg/((s)(m2))
    !        21       btu/lb              j/kg
    !        22       ft                  mm
    !        23       ft3                 in3
    !        24       ft                  in
    !        25       btu                 j
    !        26       btu/((s)(ft2))      W/m2
    !        27       lbm/ft3             kg/m3
    !        28       lbm                 kg
    !
    !***** put out the non-nodally dependant variables.
    !
    !***** put out the time.
    !
    WRITE(dakotaunit,*) Time
        !
    !***** put out the Radially Average Fuel Enthalpy
    !
    DO k = 1, naxn
        nvoids = 0
        IF (nvoid >= 1) THEN
            IF (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
        ENDIF
        !
        CALL energy (EOSTemp, RadialBound, fotmtl, tempcs, EnrgyMeltZ, EnrgyMeltZp1, qmaxmelt, &
          &          qmaxmeltp1,  edat(1,k), tbar, k, nvoids, igpnod)
        !
        IF (Time == 0.0_r8k) Uncert_fuelref(k) = edat(1,k)
        edat(2,k) = edat(1,k) - Uncert_fuelref(k)
        edat(2,k) = edat(2,k) / 4186.8_r8k
        IF (unit) edat(1,k) = edat(1,k) * 4.2987e-04_r8k
    ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    WRITE(dakotaunit,'(e13.5)') (edat(2,node))
    !
    !***** put out the Centerline Temperature in C
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(1,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(dakotaunit,'(e13.5)') edat(1,node) - 273.15_r8k
    !
    !***** put out the Fuel Pellet Surface Temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(igpnod,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(dakotaunit,'(e13.5)') edat(1,node) - 273.15_r8k
    !
    !***** put out the cladding outer temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(nmesh,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(dakotaunit,'(e13.5)') edat(1,node) - 273.15_r8k
    !
    !***** put out the cladding hoop strain in % for WGFS benchmark
    !
    WRITE(dakotaunit,'(e13.5)') (CldStrn(node,1)*100.0_r8k)
    !
    !***** put out the cladding axial ext.
    !
    CALL CONEU (dcldh, aupk, 24)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 15)
    WRITE(dakotaunit,'(e13.5)') aupk
    !
    !***** put out the fuel stack axial ext.
    !
    CALL CONEU (delth, aupk, 24)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 15)
    WRITE(dakotaunit,'(e13.5)') aupk
    !
    !***** put out the cladding hoop stress.
    !
    DO k = 1, naxn
        edat(1,k) = CldStress(k,1)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(dakotaunit,'(e13.5)') (edat(1,node))
    !
    !***** put out the Fuel Outer Radius
    !
    DO k = 1, naxn
        edat(1,k) = DeformedRadiusOfMesh(igpnod,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    ENDDO
    WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the average fuel rod power.
    !!
    !aupk = powave(1)
    !IF (.NOT. unit) CALL CONEU (aupk, aupk, 19)
    !WRITE(dakotaunit,*) aupk
    !!
    !!***** put out the plenum gas temp.
    !!
    !aupk = tp(1)
    !IF (.NOT. unit) CALL CONEU (aupk, aupk, 1)
    !WRITE(dakotaunit,*) aupk
    !!
    !!***** put out the plenum gas pressure.
    !!
    !aupk = GasPress(naxn + 1)
    !IF (.NOT. unit) CALL CONEU (aupk, aupk, 18)
    !WRITE(dakotaunit,*) aupk
    !!
    !!***** put out the total free gas volume.
    !!
    !CALL CONEU (TotalVoidVol, aupk, 23)
    !IF (.NOT. unit) CALL CONEU (aupk, aupk, 17)
    !WRITE(dakotaunit,*) aupk
    !!
    !!***** determine the calculated variables.
    !!
    !ttlmwe = 0.0_r8k
    !dvsum = 0.0_r8k
    !tsum = 0.0_r8k
    !sum = 0.0_r8k
    !sumv = 0.0_r8k
    !DO k = 1, naxn
    !    ttlmwe = ttlmwe + WatrMetlEnrgy(k) * AxialNodLen(k)
    !    sumv = sumv + AxialNodLen(k) * (RadialBound(igpnod) ** 2) * pi
    !    DO j = 1, igpnod
    !        IF (j == 1) THEN
    !            dv = pi * ((RadialBound(j+1) + RadialBound(j)) / 2.0_r8k) ** 2 * AxialNodLen(k)
    !        ELSE IF (j == igpnod) THEN
    !            dv = pi * (RadialBound(j) ** 2 - ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) &
    !              &  * AxialNodLen(k)
    !        ELSE
    !            dv = pi * (((RadialBound(j) + RadialBound(j+1)) / 2.0_r8k) ** 2 &
    !              &  - ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
    !        END IF
    !        dvsum = dvsum + dv
    !        tsum = tsum + dv * EOSTemp(j,k)
    !        IF (j > 1 .AND. qmaxmelt(1) >= 1.0e-10_r8k) THEN
    !            r1 = RadialBound(j-1)
    !            r2 = r1 + 0.5_r8k * (RadialBound(j) - RadialBound(j-1))
    !            dv1 = AxialNodLen(k) * pi * (r2 ** 2 - r1 ** 2)
    !            r3 = RadialBound(j)
    !            dv2 = AxialNodLen(k) * pi * (r3 ** 2 - r2 ** 2)
    !            sum = sum + (EnrgyMeltZ(j,k) / qmaxmelt(j)) * dv1 + (EnrgyMeltZp1(j,k) / qmaxmeltp1(j)) * dv2
    !        ENDIF
    !    ENDDO
    !ENDDO
    !!***** put out the average fuel temperature.
    !aupk = tsum / dvsum
    !IF (.NOT. unit) CALL CONEU (aupk, aupk, 1)
    !WRITE(dakotaunit,*) aupk
    !!***** put out the total energy generated by metal water reaction.
    !!    IF (.NOT. unit) CALL CONEU (ttlmwe, ttlmwe, 25)
    !WRITE(dakotaunit,*) ttlmwe
    !!
    !!***** put out the cladding axial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldStrn(node,2))
    !!
    !!***** put out the cladding radial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldStrn(node,3))
    !!
    !!***** put out the cladding perm axial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldPlasStrn(node,2))
    !!
    !!***** put out the cladding perm hoop strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldPlasStrn(node,1))
    !!
    !!***** put out the cladding perm radial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldPlasStrn(node,3))
    !!
    !!***** put out the cladding axial stress.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CldStress(k,2)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') (edat(1,node))
    !!
    !!***** put out the Effective Clad Stress.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CladEffStress(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') (edat(1,node))
    !!
    !!***** put out the Cladding Effective Elastic-Plastic Strain
    !!
    !WRITE(dakotaunit,'(e13.5)') (EffStrainPNNL(node))
    !!
    !!***** put out the Structural Radial Gap.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = RInterfacGap(k)*1.2e4_r8k
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 16)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Thermal Radial Gap.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = GapThick(k)*1.2e4_r8k
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 16)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Clad Yield Stress.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CladYieldStress(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!  put out the K coefficient
    !!
    !WRITE(dakotaunit,'(e13.5)') (coefk(node))
    !!
    !!  put out the n coefficient
    !!
    !WRITE(dakotaunit,'(e13.5)') (coefn(node))
    !!
    !!  put out the m coefficient
    !!
    !WRITE(dakotaunit,'(e13.5)') (coefm(node))
    !!
    !!  put out the elastic modulus
    !!
    !WRITE(dakotaunit,'(e13.5)') (Emodulus(node))
    !!
    !!  put out the strain rate coefficient
    !!
    !WRITE(dakotaunit,'(e13.5)') (strainrateterm(node))
    !!
    !!***** put out the cladding instability strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (EinstabilStrain(node))
    !!
    !!***** put out the SED based on elastic-plastic effective strain
    !!
    !WRITE(dakotaunit,'(e13.5)') (SEDPNNL(node))
    !!
    !!***** put out the SED based on EPRI's formulation
    !!
    !WRITE(dakotaunit,'(e13.5)') (SEDEPRI(node))
    !
    !!***** put out the Coolant Quality.
    !!
    !WRITE(dakotaunit,'(e13.5)') (Coolqual(node))
    !!
    !!***** put out the Axial Node Elevation.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = AxNodElevat(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 14)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Gap Heat Transfer Coefficient.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = HGapAv(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 13)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Water Metal Reaction Energy.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = WatrMetlEnrgy(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 19)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !! Skip oxidation material if oxidation not calculated
    !IF (modmw /= 1) THEN
    !    !
    !    !***** put out the Outer Oxide Thickness (C-P) or (B-J)
    !    !
    !    DO k = 1, naxn
    !        edat(1,k) = EOSOxideThick(k)
    !        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 15)
    !    ENDDO
    !    WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !    !
    !    !***** put out the Inner Oxide Thickness (C-P) or (B-J)
    !    !
    !    DO k = 1, naxn
    !        edat(1,k) = OxiThk2(k)
    !        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 15)
    !    ENDDO
    !    WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !    !
    !    !***** put out the C-P or B-J OD oxygen uptake.
    !    !
    !    WRITE(dakotaunit,'(e13.5)') (OxygenUptake(node))
    !    !
    !    !***** put out the C-P or B-J ID oxygen uptake.
    !    !
    !    WRITE(dakotaunit,'(e13.5)') (OxUptakeID2(node))
    !    !
    !    !***** put out the C-P or B-J cladding ECR
    !    !
    !    WRITE(dakotaunit,'(e13.5)') (ECR(node))
    !    !
    !ENDIF
    !!
    !!***** put out the Surface Heat Transfer Coefficient.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = FilmCoeffAv(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 13)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Surface Heat Flux.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = HeatFlux(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 12)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Coolant Mass Flux.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = rmassflux(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 20)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Coolant Pressure.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CoolPress(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Critical Heat Flux.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CritHtFlux(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 12)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the cladding inner temperature.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = EOSTemp(ncladi,k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Average Cladding Temperature.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CladAveTemp(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Bulk Coolant Temperature.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = BulkCoolTemp(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Fuel Surface Displacement.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = PelSrfDispl(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Gap Pressure.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = GasPress(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Axial Power.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = AxialPowr(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 19)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Structural Gap Interface Pressure.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = TerfacePres(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Coolant Density.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = CoolDensity(k)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 27)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Pellet Surface Axial Strain
    !!
    !DO k = 1, naxn
    !    edat(1,k) = PelSrfStrn(k,2)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Pellet Surface Hoop Strain.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = PelSrfStrn(k,1)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Cladding Inside Surface Radial Displacement
    !!
    !DO k = 1, naxn
    !    edat(1,k) = EOSRad(ncladi,k)-RadialBound(ncladi)
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the Fuel Surface Displacement.
    !!
    !DO k = 1, naxn
    !    edat(1,k) = (DeformedRadiusOfMesh(igpnod,k) - RadialBound(igpnod))
    !    IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !!***** put out the cladding axial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldStrn(node,2))
    !!
    !!***** put out the cladding elastic axial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldElStrn(node,2))
    !!
    !!***** put out the cladding thermal axial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldThermStrn(node,2))
    !!
    !!***** put out the cladding elastic hoop strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldElStrn(node,1))
    !!
    !!***** put out the cladding thermal hoop strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldThermStrn(node,1))
    !!
    !!***** put out the cladding elastic radial strain.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldElStrn(node,3))
    !!
    !!***** put out the cladding hoop strain rate.
    !!
    !WRITE(dakotaunit,'(e13.5)') (CldStrnRat(node,1))
    !!***** Put out Energy in Fuel
    !DO k = 1, naxn
    !    IF (.NOT. unit) edat(1,k) = edat(1,k) * 4.2987e-4
    !    nvoids = 0
    !    IF (nvoid >= 1) THEN
    !        IF (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
    !    ENDIF
    !    IF (nvoids == 0) THEN
    !        edat(1,k) = rhof * pi * (RadialBound(igpnod)**2) * edat(1,k)
    !    ELSE
    !        edat(1,k) = rhof * pi * (RadialBound(igpnod)**2 - RadialBound(2)**2) * edat(1,k)
    !    ENDIF
    !    ! convert from btu to kW-s
    !    edat(1,k) = 1.054886_r8k * edat(1,k)
    !    IF (.NOT. unit) edat(1,k) = 3.28084_r8k * edat(1,k)
    !ENDDO
    !!
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!***** Put out Energy in Cladding
    !DO k = 1, naxn
    !    edat(1,k) = CladEnrgPerL(k)
    !    IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!***** Put out Energy Input After Steady State
    !DO k = 1, naxn
    !    edat(1,k) = EnergyPerL(k)
    !    IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!***** Put out Energy Output After Steady State
    !DO k = 1, naxn
    !    edat(1,k) = HFXSum(k)
    !    IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    !ENDDO
    !WRITE(dakotaunit,'(e13.5)') edat(1,node)
    !!
    !! -- add radii Data
    !!
    !DO l = 1, nmesh
    !    DO k = 1, naxn
    !        ! -------radii
    !        edat(l,k) = EOSRad(l,k)
    !        IF (.NOT. unit) CALL CONEU (edat(l,k), edat(l,k), 22)
    !    ENDDO
    !ENDDO
    !DO l = 1, nmesh
    !    WRITE(dakotaunit,'(e13.5)') (edat(l,node))
    !ENDDO
    !
    !DO l = 1, nmesh
    !    DO k = 1, naxn
    !        ! -------temperatures
    !        edat(l,k) = EOSTemp(l,k)
    !        IF (.NOT. unit) CALL CONEU (edat(l,k), edat(l,k), 1)
    !    ENDDO
    !ENDDO
    !DO l = 1, nmesh
    !    WRITE(dakotaunit,'(e13.5)') (edat(l,node))
    !ENDDO
    !
    END SUBROUTINE dktout
    !
    !
    !
END MODULE Uncertainties_fraptran













