MODULE PlotFile
    USE Kinds
    USE variables_fraptran
    USE Dyna_h
    USE resti_h
    USE conversions_fraptran, ONLY : pi, coneu
    USE Uncertainty_Vals
    !>@brief
    !> This module contains the subroutines needed to make the plot file
    !>@author
    !> Updated by Ian Porter, NRC
    !>@date
    !> 3/26/2015
    IMPLICIT NONE
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: efuelref       ! Reference fuel enthalpy from steady-state operation
    !
    CONTAINS
    !
    SUBROUTINE grafini
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> Subroutine grafini prints the descriptions of the variables written to the plot file.
    !
    INTEGER(ipk) :: i
    REAL(r8k) :: dummy
    !
    WRITE(plotunit,10) TRIM(codeid), TRIM(Title)
10  FORMAT(a,/,a,/,' strip file generated by FrapTran')
    WRITE(plotunit,*)'data'
    WRITE(plotunit,*)'1 sec sec Time'
    WRITE(plotunit,*)'2 kW/ft kW/m Average Rod Power'
    WRITE(plotunit,*)'3 in mm Fuel Stack Elongation'
    WRITE(plotunit,*)'4 in mm Clad Axial Elongation'
    WRITE(plotunit,*)'5 F K Plenum Gas Temperature'
    WRITE(plotunit,*)'6 psia MPa Plenum Pressure'
    WRITE(plotunit,*)'7 in^3 mm^3 Free Gas Volume'
    WRITE(plotunit,*)'8 F K Average Fuel Temperature'
    WRITE(plotunit,*)'9 kW kW Metal Water Reaction Energy'
    WRITE(plotunit,*)'1D data'
    WRITE(plotunit,*)'100 na na Cladding Axial Strain'
    WRITE(plotunit,*)'101 na na Cladding Hoop Strain'
    WRITE(plotunit,*)'102 na na Cladding Radial Strain'
    WRITE(plotunit,*)'103 na na Cladding Perm Axial Strain'
    WRITE(plotunit,*)'104 na na Cladding Perm Hoop Strain'
    WRITE(plotunit,*)'105 na na Cladding Perm Radial Strain'
    WRITE(plotunit,*)'106 psia MPa Cladding Axial Stress'
    WRITE(plotunit,*)'107 psia MPa Cladding Hoop Stress'
    WRITE(plotunit,*)'108 psia MPa Effective Cladding Stress'
    WRITE(plotunit,*)'161 na na Cladding Eff. Elastic-Plastic Strain'
    WRITE(plotunit,*)'111 mils mm Structural Radial Gap'
    WRITE(plotunit,*)'112 mils mm Thermal Radial Gap'
    WRITE(plotunit,*)'113 psia MPa Clad Yield Stress'
    WRITE(plotunit,*)'114 Pa Pa K coefficient'
    WRITE(plotunit,*)'115 na na n coefficient'
    WRITE(plotunit,*)'116 na na m coefficient'
    WRITE(plotunit,*)'117 Pa Pa elastic modulus'
    WRITE(plotunit,*)'118 1/s 1/s strain rate for yield strength'
    WRITE(plotunit,*)'119 na na Clad Instability Strain'
    WRITE(plotunit,*)'122 MJ/m3 MJ/m3 SED: PNNL elastic-plastic eff. strain'
    WRITE(plotunit,*)'121 MJ/m3 MJ/m3 SED: EPRI formulation'
    WRITE(plotunit,*)'123 na na Coolant Quality'
    WRITE(plotunit,*)'124 ft m Axial Node Elevation'
    WRITE(plotunit,*)'125 Btu/(hr-ft^2-F) W/(m^2-K) Gap Heat Transfer Coefficient'
    WRITE(plotunit,*)'126 kW/ft kW/m Water Metal Reaction Energy'
    SELECT CASE (modmw)
    CASE (0) ! C-P oxidation selected
        WRITE(plotunit,*)'127 in mm C-P OD Oxide Thickness'
        WRITE(plotunit,*)'128 in mm C-P ID Oxide Thickness'
        WRITE(plotunit,*)'131 kg/m**2 kg/m**2 C-P OD O2 Uptake'
        WRITE(plotunit,*)'132 kg/m**2 kg/m**2 C-P ID O2 Uptake'
        WRITE(plotunit,*)'133 frac frac C-P Total Cladding ECR'
    CASE (2) ! B-J oxidation selected
        WRITE(plotunit,*)'127 in mm B-J OD Oxide Thickness'
        WRITE(plotunit,*)'128 in mm B-J ID Oxide Thickness'
        WRITE(plotunit,*)'131 kg/m**2 kg/m**2 B-J OD O2 Uptake'
        WRITE(plotunit,*)'132 kg/m**2 kg/m**2 B-J ID O2 Uptake'
        WRITE(plotunit,*)'133 frac frac B-J Total Cladding ECR'
    END SELECT
    WRITE(plotunit,*)'134 Btu/(hr-ft^2-F) W/(m^2-K) Surface Heat Transfer Coefficient'
    WRITE(plotunit,*)'135 Btu/(hr-ft^2) W/(m^2) Surface Heat Flux'
    WRITE(plotunit,*)'136 lb/(hr-ft^2) kg/(s-m^2) Coolant Mass Flux'
    WRITE(plotunit,*)'137 psia MPa Coolant Pressure'
    WRITE(plotunit,*)'138 BTU/(s-ft^2) W/(m^2) Critical Heat Flux'
    WRITE(plotunit,*)'170 na na Heat transfer mode'
    WRITE(plotunit,*)'139 F K Fuel Centerline Temperature'
    WRITE(plotunit,*)'140 F K Fuel Pellet Surface Temperature'
    WRITE(plotunit,*)'141 F K Cladding Inside Temperature'
    WRITE(plotunit,*)'142 F K Cladding Average Temperature'
    WRITE(plotunit,*)'143 F K Cladding Outside Temperature'
    WRITE(plotunit,*)'144 F K Bulk Coolant Temperature'
    WRITE(plotunit,*)'145 ft mm Fuel Surface Displacement'
    WRITE(plotunit,*)'146 psia MPa Gap Gas Pressure'
    WRITE(plotunit,*)'147 kW/ft kW/m Axial Power'
    WRITE(plotunit,*)'148 psia MPa Gap Interface Pressure'
    WRITE(plotunit,*)'149 lbm/ft^3 kg/m^3 Coolant Density'
    WRITE(plotunit,*)'150 na na Fuel Surface Axial Strain'
    WRITE(plotunit,*)'151 na na Fuel Surface Hoop Strain'
    WRITE(plotunit,*)'152 ft mm Cladding Inside Surface Displacement'
    WRITE(plotunit,*)'153 ft mm Fuel Surface Displacement <DfRdOMsh'
    WRITE(plotunit,*)'154 na na Cladding Total Axial Strain'
    WRITE(plotunit,*)'155 na na Cladding Elastic Axial Strain'
    WRITE(plotunit,*)'156 na na Cladding Thermal Axial Strain'
    WRITE(plotunit,*)'157 na na Cladding Elastic Hoop Strain'
    WRITE(plotunit,*)'158 na na Cladding Thermal Hoop Strain'
    WRITE(plotunit,*)'159 na na Cladding Elastic Radial Strain'
    WRITE(plotunit,*)'160 1/sec 1/sec Cladding Hoop Strain Rate'
    SELECT CASE (unit)
    CASE (.FALSE.)
        dummy = (tref - 32.0_r8k) / 1.8_r8k + 273.15_r8k
        WRITE(plotunit,30) dummy
30      FORMAT('161 BTU/lb J/kg Rad Ave Fuel Enthalpy ',f6.2,'K Ref')
    CASE (.TRUE.)
        WRITE(plotunit,40) tref
40      FORMAT('161 BTU/lb J/kg Rad Ave Fuel Enthalpy ',f6.2,'F Ref')
    END SELECT
    WRITE(plotunit,*) '162 cal/g cal/g Fuel Enthalpy Increase After  Steady State'
    WRITE(plotunit,*) '163 kW-s/ft kW-s/m Energy in Fuel'
    WRITE(plotunit,*) '164 kW-s/ft kW-s/m Energy in Cladding'
    WRITE(plotunit,*) '165 kW-s/ft kW-s/m Energy Input After Steady State'
    WRITE(plotunit,*) '166 kW-s/ft kW-s/m Energy Output After Steady State'
    WRITE(plotunit,*)'2D data'
    WRITE(plotunit,*)'200 ft mm Rod Mesh Radii'
    WRITE(plotunit,*)'201 F K Fuel Rod Mesh Temperatures'
    WRITE(plotunit,*)'end data'
    ! WRITE the number of axial and radial nodes to plot file
    WRITE(plotunit,*) 'naxn= ',naxn
    WRITE(plotunit,*) 'nmesh= ',nmesh
    WRITE(plotunit,*) 'igpnod= ',igpnod
    WRITE(plotunit,*) 'ncladi= ',ncladi
    WRITE(plotunit,*) 'unit= ',unit
    !
    END SUBROUTINE grafini
    !
    !
    !
    SUBROUTINE grafout ()
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran
    USE Temperature
    IMPLICIT NONE
    !>@brief
    !> Subroutine grafout prints out the code results to the plot file as user-specified time intervals
    !
    INTEGER(ipk) :: k, l, j, nvoids
    INTEGER(ipk) :: node = 1_ipk        ! Printout node for DAKOTA output, hardwired to 1
    REAL(r8k) :: aupk, ttlmwe, dvsum, tsum, sum, sumv, dv, dv1, dv2, r1, r2, r3, tbar
    REAL(r8k), DIMENSION(nmesh, naxn) :: edat
    !
    IF (.NOT. ALLOCATED(efuelref)) THEN
        ALLOCATE(efuelref(1:naxn))
        efuelref = 0.0_r8k
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
    WRITE(plotunit,*) '===next time step'
    WRITE(plotunit,*) "1 ", Time
    IF (dakota) dktouts(1,dktoutcounter) = Time
    !
    !***** put out the average fuel rod power.
    !
    aupk = powave(1)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 19)
    WRITE(plotunit,*) "2 ", aupk
    !
    !***** put out the fuel stack axial ext.
    !
    CALL CONEU (delth, aupk, 24)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 15)
    WRITE(plotunit,*) "3 ", aupk
    IF (dakota) dktouts(8,dktoutcounter) = aupk
    !
    !***** put out the cladding axial ext.
    !
    CALL CONEU (dcldh, aupk, 24)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 15)
    WRITE(plotunit,*) "4 ", aupk
    IF (dakota) dktouts(7,dktoutcounter) = aupk
    !
    !***** put out the plenum gas temp.
    !
    aupk = tp(1)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 1)
    WRITE(plotunit,*) "5 ", aupk
    !
    !***** put out the plenum gas pressure.
    !
    aupk = GasPress(naxn + 1)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 18)
    WRITE(plotunit,*) "6 ", aupk
    !
    !***** put out the total free gas volume.
    !
    CALL CONEU (TotalVoidVol, aupk, 23)
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 17)
    WRITE(plotunit,*) "7 ", aupk
    !
    !***** determine the calculated variables.
    !
    ttlmwe = 0.0_r8k
    dvsum = 0.0_r8k
    tsum = 0.0_r8k
    sum = 0.0_r8k
    sumv = 0.0_r8k
    DO k = 1, naxn
        ttlmwe = ttlmwe + WatrMetlEnrgy(k) * AxialNodLen(k)
        sumv = sumv + AxialNodLen(k) * (RadialBound(igpnod) ** 2) * pi
        DO j = 1, igpnod
            IF (j == 1) THEN
                dv = pi * ((RadialBound(j+1) + RadialBound(j)) / 2.0_r8k) ** 2 * AxialNodLen(k)
            ELSE IF (j == igpnod) THEN
                dv = pi * (RadialBound(j) ** 2 - ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
            ELSE
                dv = pi * (((RadialBound(j) + RadialBound(j+1)) / 2.0_r8k) ** 2 - &
                  &  ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
            END IF
            dvsum = dvsum + dv
            tsum = tsum + dv * EOSTemp(j,k)
            IF (j > 1 .AND. qmaxmelt(1) >= 1.0e-10_r8k) THEN
                r1 = RadialBound(j-1)
                r2 = r1 + 0.5_r8k * (RadialBound(j) - RadialBound(j-1))
                dv1 = AxialNodLen(k) * pi * (r2 ** 2 - r1 ** 2)
                r3 = RadialBound(j)
                dv2 = AxialNodLen(k) * pi * (r3 ** 2 - r2 ** 2)
                sum = sum + (EnrgyMeltZ(j,k) / qmaxmelt(j)) * dv1 + (EnrgyMeltZp1(j,k) / qmaxmeltp1(j)) * dv2
            ENDIF
        ENDDO
    ENDDO
    !***** put out the average fuel temperature.
    aupk = tsum / dvsum
    IF (.NOT. unit) CALL CONEU (aupk, aupk, 1)
    WRITE(plotunit,*) "8 ", aupk
    !***** put out the total energy generated by metal water reaction.
    !    IF (.NOT. unit) CALL CONEU (ttlmwe, ttlmwe, 25)
    WRITE(plotunit,*) "9 ", ttlmwe
    !
    !***** put out the cladding axial strain.
    !
    WRITE(plotunit,'("100 ",100(e13.5))') (CldStrn(k,2),k=1,naxn)
    !
    !***** put out the cladding hoop strain.
    !
    WRITE(plotunit,'("101 ",100(e13.5))') (CldStrn(k,1), k=1,naxn)
    IF (dakota) dktouts(6,dktoutcounter) = CldStrn(node,1) * 100.0_r8k    ! Conversion to % for DAKOTA
    !
    !***** put out the cladding radial strain.
    !
    WRITE(plotunit,'("102 ",100(e13.5))') (CldStrn(k,3),k=1,naxn)
    !
    !***** put out the cladding perm axial strain.
    !
    WRITE(plotunit,'("103 ",100(e13.5))') (CldPlasStrn(k,2), k=1,naxn)
    !
    !***** put out the cladding perm hoop strain.
    !
    WRITE(plotunit,'("104 ",100(e13.5))') (CldPlasStrn(k,1), k=1,naxn)
    !
    !***** put out the cladding perm radial strain.
    !
    WRITE(plotunit,'("105 ",100(e13.5))') (CldPlasStrn(k,3), k=1,naxn)
    !
    !***** put out the cladding axial stress.
    !
    DO k = 1, naxn
        edat(1,k) = CldStress(k,2)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("106 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the cladding hoop stress.
    !
    DO k = 1, naxn
        edat(1,k) = CldStress(k,1)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("107 ",100(e13.5))') (edat(1,k),k=1,naxn)
    IF (dakota) dktouts(9,dktoutcounter) = edat(1,node)
    !
    !***** put out the Effective Clad Stress.
    !
    DO k = 1, naxn
        edat(1,k) = CladEffStress(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("108 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Cladding Effective Elastic-Plastic Strain
    !
    WRITE(plotunit,'("161 ",100(e13.5))') (EffStrainPNNL(k),k=1,naxn)
    !
    !***** put out the Structural Radial Gap.
    !
    DO k = 1, naxn
        edat(1,k) = RInterfacGap(k)*1.2e4_r8k
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 16)
    ENDDO
    WRITE(plotunit,'("111 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Thermal Radial Gap.
    !
    DO k = 1, naxn
        edat(1,k) = GapThick(k)*1.2e4_r8k
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 16)
    ENDDO
    WRITE(plotunit,'("112 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Clad Yield Stress.
    !
    DO k = 1, naxn
        edat(1,k) = CladYieldStress(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("113 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !  put out the K coefficient
    !
    WRITE(plotunit,'("114 ",100(e13.5))') (coefk(k),k=1,naxn)
    !
    !  put out the n coefficient
    !
    WRITE(plotunit,'("115 ",100(e13.5))') (coefn(k),k=1,naxn)
    !
    !  put out the m coefficient
    !
    WRITE(plotunit,'("116 ",100(e13.5))') (coefm(k),k=1,naxn)
    !
    !  put out the elastic modulus
    !
    WRITE(plotunit,'("117 ",100(e13.5))') (Emodulus(k),k=1,naxn)
    !
    !  put out the strain rate coefficient
    !
    WRITE(plotunit,'("118 ",100(e13.5))') (strainrateterm(k),k=1,naxn)
    !
    !***** put out the cladding instability strain.
    !
    WRITE(plotunit,'("119 ",100(e13.5))') (EinstabilStrain(k),k=1,naxn)
    !
    !***** put out the SED based on elastic-plastic effective strain
    !
    WRITE(plotunit,'("122 ",100(e13.5))') (SEDPNNL(k),k=1,naxn)
    !
    !***** put out the SED based on EPRI's formulation
    !
    WRITE(plotunit,'("121 ",100(e13.5))') (SEDEPRI(k),k=1,naxn)

    !***** put out the Coolant Quality.
    !
    WRITE(plotunit,'("123 ",100(e13.5))') (Coolqual(k),k=1,naxn)
    !
    !***** put out the Axial Node Elevation.
    !
    DO k = 1, naxn
        edat(1,k) = AxNodElevat(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 14)
    ENDDO
    WRITE(plotunit,'("124 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Gap Heat Transfer Coefficient.
    !
    DO k = 1, naxn
        edat(1,k) = HGapAv(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 13)
    ENDDO
    WRITE(plotunit,'("125 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Water Metal Reaction Energy.
    !
    DO k = 1, naxn
        edat(1,k) = WatrMetlEnrgy(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 19)
    ENDDO
    WRITE(plotunit,'("126 ",100(e13.5))') (edat(1,k),k=1,naxn)
    ! Skip oxidation material if oxidation not calculated
    IF (modmw /= 1) THEN
        !
        !***** put out the Outer Oxide Thickness (C-P) or (B-J)
        !
        DO k = 1, naxn
            edat(1,k) = EOSOxideThick(k)
            IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 15)
        ENDDO
        WRITE(plotunit,'("127 ",100(e13.5))') (edat(1,k),k=1,naxn)
        !
        !***** put out the Inner Oxide Thickness (C-P) or (B-J)
        !
        DO k = 1, naxn
            edat(1,k) = OxiThk2(k)
            IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 15)
        ENDDO
        WRITE(plotunit,'("128 ",100(e13.5))') (edat(1,k),k=1,naxn)
        !
        !***** put out the C-P or B-J OD oxygen uptake.
        !
        WRITE(plotunit,'("131 ",100(e13.5))') (OxygenUptake(k), k=1,naxn)
        !
        !***** put out the C-P or B-J ID oxygen uptake.
        !
        WRITE(plotunit,'("132 ",100(e13.5))') (OxUptakeID2(k), k=1,naxn)
        !
        !***** put out the C-P or B-J cladding ECR
        !
        WRITE(plotunit,'("133 ",100(e13.5))') (ECR(k),k=1,naxn)
        !
    ENDIF
    !
    !***** put out the Surface Heat Transfer Coefficient.
    !
    DO k = 1, naxn
        edat(1,k) = FilmCoeffAv(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 13)
    ENDDO
    WRITE(plotunit,'("134 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Surface Heat Flux.
    !
    DO k = 1, naxn
        edat(1,k) = HeatFlux(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 12)
    ENDDO
    WRITE(plotunit,'("135 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Coolant Mass Flux.
    !
    DO k = 1, naxn
        edat(1,k) = rmassflux(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 20)
    ENDDO
    WRITE(plotunit,'("136 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Coolant Pressure.
    !
    DO k = 1, naxn
        edat(1,k) = CoolPress(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("137 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Critical Heat Flux.
    !
    DO k = 1, naxn
        edat(1,k) = CritHtFlux(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 12)
    ENDDO
    WRITE(plotunit,'("138 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Heat Transfer Mode
    !
    WRITE(plotunit,'("178 ",100(i4))') (ih(k),k=1,naxn)
    !
    !***** put out the Centerline Temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(1,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("139 ",100(e13.5))') (edat(1,k),k=1,naxn)
    IF (dakota) dktouts(3,dktoutcounter) = edat(1,node) - 273.15_r8k    ! Conversion to degrees C for DAKOTA
    !
    !***** put out the Fuel Pellet Surface Temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(igpnod,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("140 ",100(e13.5))') (edat(1,k),k=1,naxn)
    IF (dakota) dktouts(4,dktoutcounter) = edat(1,node) - 273.15_r8k    ! Conversion to degrees C for DAKOTA
    !
    !***** put out the cladding inner temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(ncladi,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("141 ",100(e13.5))') (edat(1,k), k=1,naxn)
    !
    !***** put out the Average Cladding Temperature.
    !
    DO k = 1, naxn
        edat(1,k) = CladAveTemp(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("142 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the cladding outer temperature.
    !
    DO k = 1, naxn
        edat(1,k) = EOSTemp(nmesh,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("143 ",100(e13.5))') (edat(1,k),k=1,naxn)
    IF (dakota) dktouts(5,dktoutcounter) = edat(1,node) - 273.15_r8k    ! Conversion to degrees C for DAKOTA
    !
    !***** put out the Bulk Coolant Temperature.
    !
    DO k = 1, naxn
        edat(1,k) = BulkCoolTemp(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 1)
    ENDDO
    WRITE(plotunit,'("144 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Fuel Surface Displacement.
    !
    DO k = 1, naxn
        edat(1,k) = PelSrfDispl(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    ENDDO
    WRITE(plotunit,'("145 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Gap Pressure.
    !
    DO k = 1, naxn
        edat(1,k) = GasPress(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("146 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Axial Power.
    !
    DO k = 1, naxn
        edat(1,k) = AxialPowr(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 19)
    ENDDO
    WRITE(plotunit,'("147 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Structural Gap Interface Pressure.
    !
    DO k = 1, naxn
        edat(1,k) = TerfacePres(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 18)
    ENDDO
    WRITE(plotunit,'("148 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Coolant Density.
    !
    DO k = 1, naxn
        edat(1,k) = CoolDensity(k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 27)
    ENDDO
    WRITE(plotunit,'("149 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Pellet Surface Axial Strain
    !
    DO k = 1, naxn
        edat(1,k) = PelSrfStrn(k,2)
    ENDDO
    WRITE(plotunit,'("150 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Pellet Surface Hoop Strain.
    !
    DO k = 1, naxn
        edat(1,k) = PelSrfStrn(k,1)
    ENDDO
    WRITE(plotunit,'("151 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Cladding Inside Surface Radial Displacement
    !
    DO k = 1, naxn
        edat(1,k) = EOSRad(ncladi,k)-RadialBound(ncladi)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    ENDDO
    WRITE(plotunit,'("152 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** put out the Fuel Surface Displacement.
    !
    DO k = 1, naxn
        ! DeformedRadiusOfMesh does not include contributions from initial permanent pellet surface displacement
        !edat(1,k) = (DeformedRadiusOfMesh(igpnod,k) - RadialBound(igpnod))
        edat(1,k) = (EOSRad(igpnod,k) - RadialBound(igpnod))
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    ENDDO
    WRITE(plotunit,'("153 ",100(e13.5))') (edat(1,k),k=1,naxn)
    !
    !***** Save the Fuel Outer Radius for DAKOTA
    !
    DO k = 1, naxn
        edat(1,k) = DeformedRadiusOfMesh(igpnod,k)
        IF (.NOT. unit) CALL CONEU (edat(1,k), edat(1,k), 22)
    ENDDO
    IF (dakota) dktouts(10,dktoutcounter) = edat(1,node)
    !
    !***** put out the cladding axial strain.
    !
    WRITE(plotunit,'("154 ",100(e13.5))') (CldStrn(k,2),k=1,naxn)
    !
    !***** put out the cladding elastic axial strain.
    !
    WRITE(plotunit,'("155 ",100(e13.5))') (CldElStrn(k,2),k=1,naxn)
    !
    !***** put out the cladding thermal axial strain.
    !
    WRITE(plotunit,'("156 ",100(e13.5))') (CldThermStrn(k,2), k=1,naxn)
    !
    !***** put out the cladding elastic hoop strain.
    !
    WRITE(plotunit,'("157 ",100(e13.5))') (CldElStrn(k,1),k=1,naxn)
    !
    !***** put out the cladding thermal hoop strain.
    !
    WRITE(plotunit,'("158 ",100(e13.5))') (CldThermStrn(k,1), k=1,naxn)
    !
    !***** put out the cladding elastic radial strain.
    !
    WRITE(plotunit,'("159 ",100(e13.5))') (CldElStrn(k,3),k=1,naxn)
    !
    !***** put out the cladding hoop strain rate.
    !
    WRITE(plotunit,'("160 ",100(e13.5))') (CldStrnRat(k,1), k=1,naxn)
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
        IF (Time == 0.0_r8k) efuelref(k) = edat(1,k)
        edat(2,k) = edat(1,k) - efuelref(k)
        edat(2,k) = edat(2,k) / 4186.8_r8k
        IF (unit) edat(1,k) = edat(1,k) * 4.2987e-04_r8k
    ENDDO
    WRITE(plotunit,'("161 ",100(e13.5))') (edat(1,k), k=1,naxn)
    WRITE(plotunit,'("162 ",100(e13.5))') (edat(2,k), k=1,naxn)
    IF (dakota) dktouts(2,dktoutcounter) = edat(2,node)
    !***** Put out Energy in Fuel
    DO k = 1, naxn
        IF (.NOT. unit) edat(1,k) = edat(1,k) * 4.2987e-4
        nvoids = 0
        IF (nvoid >= 1) THEN
            IF (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
        ENDIF
        IF (nvoids == 0) THEN
            edat(1,k) = rhof * pi * (RadialBound(igpnod)**2) * edat(1,k)
        ELSE
            edat(1,k) = rhof * pi * (RadialBound(igpnod)**2 - RadialBound(2)**2) * edat(1,k)
        ENDIF
        ! convert from btu to kW-s
        edat(1,k) = 1.054886_r8k * edat(1,k)
        IF (.NOT. unit) edat(1,k) = 3.28084_r8k * edat(1,k)
    ENDDO
    !
    WRITE(plotunit,'("163 ",100(e13.5))') (edat(1,k), k=1,naxn)
    !***** Put out Energy in Cladding
    DO k = 1, naxn
        edat(1,k) = CladEnrgPerL(k)
        IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    ENDDO
    WRITE(plotunit,'("164 ",100(e13.5))') (edat(1,k), k=1,naxn)
    !***** Put out Energy Input After Steady State
    DO k = 1, naxn
        edat(1,k) = EnergyPerL(k)
        IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    ENDDO
    WRITE(plotunit,'("165 ",100(e13.5))') (edat(1,k), k=1,naxn)
    !***** Put out Energy Output After Steady State
    DO k = 1, naxn
        edat(1,k) = HFXSum(k)
        IF (.NOT. unit) CALL CONEU(edat(1,k), edat(1,k),19)
    ENDDO
    WRITE(plotunit,'("166 ",100(e13.5))') (edat(1,k), k=1,naxn)
    !
    ! -- add radii Data
    !
    DO l = 1, nmesh
        DO k = 1, naxn
            ! -------radii
            edat(l,k) = EOSRad(l,k)
            IF (.NOT. unit) CALL CONEU (edat(l,k), edat(l,k), 22)
        ENDDO
    ENDDO
    DO l = 1, nmesh
        WRITE(plotunit,'("200 ",100(e13.5))') (edat(l,k),k=1,naxn)
    ENDDO
 
    DO l = 1, nmesh
        DO k = 1, naxn
            ! -------temperatures
            edat(l,k) = EOSTemp(l,k)
            IF (.NOT. unit) CALL CONEU (edat(l,k), edat(l,k), 1)
        ENDDO
    ENDDO
    DO l = 1, nmesh
        WRITE(plotunit,'("201 ",100(e13.5))') (edat(l,k),k=1,naxn)
    ENDDO
    !
    END SUBROUTINE grafout
    !
    END MODULE PlotFile












