MODULE OutputFile_fraptran
    USE Kinds_fraptran
    USE conversions_fraptran
    IMPLICIT NONE
    !>@brief
    !> Subroutines for printing to the output file
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/17/2015
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: efuelref
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: eout0
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: ein0
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: efuel0
    REAL(r8k), DIMENSION(:), ALLOCATABLE, SAVE :: eclad0
    !
    CONTAINS
    !
    SUBROUTINE Allocate_Output_vars (naxial)
    USE Kinds_fraptran
    IMPLICIT NONE
    !>@brief
    !> Allocate the variables needed for printing to the output file
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 4/17/2015
    !
    ! Input
    !
    ! naxial - # of axial nodes
    !
    
    INTEGER(ipk), INTENT(IN) :: naxial
    
    ! Allocate the arrays
    ALLOCATE(efuelref(1:naxial))
    ALLOCATE(eout0(1:naxial))
    ALLOCATE(ein0(1:naxial))
    ALLOCATE(efuel0(1:naxial))
    ALLOCATE(eclad0(1:naxial))
    
    ! Set the default values
    efuelref = 0.0_r8k
    eout0 = 0.0_r8k
    ein0 = 0.0_r8k
    efuel0 = 0.0_r8k
    eclad0 = 0.0_r8k
    
    END SUBROUTINE Allocate_Output_vars
    !
    !
    !
    SUBROUTINE prntot
    USE Kinds_fraptran
    USE conversions_fraptran, ONLY : pi, sechr, ftom, ftin, tfk
    USE variables_fraptran
    USE Temperature_fraptran
    USE NCGases_fraptran, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Subroutine prints out results of calculations
    !
    ! EOSTemp(j)            - EOS Temperature at calculation node j
    ! EOSRad(i,j)           - radius to node j at time t
    ! RadialBound           - cold state radius to node j (reference for strains)
    ! sigmah                - cladding hoop stress
    ! sigmaz                - cladding axial stress
    ! CldPermAxStrn         - permanent hoop strain in cladding
    ! CldPermHoopStrn       - permanent axial strain in clad
    ! Ih                    - rod-to-coolant heat transfer mode
    ! n1                    - Dimension of radial node vectors
    ! n2                    - Dimension of axial node vectors. 1 = implied dimension for no. of fuel rods
    ! nmesh                 - number of nodes in mesh - to outer surface of clad
    ! ncladi                - number of nodes to gas gap / clad boundary
    ! igpnod                - node number at surface of fuel pellet stack
    ! modfd                 - deformation model indicator
    ! naxn                  - number of axial nodes
    ! IterationCount        - iteration count
    ! RuptFailIndex         - rupture indicator ( 1 --# yes )
    ! CladCollapseIndex     - buckled clad indicator ( 1 --# yes )
    ! unit                  - .TRUE. --# i/o in british units, .FALSE. --# s.i.
    ! t                     - current time
    ! GasPress              - gap pressure (psia)
    ! AxialPowr             - local fuel rod power   (kW/ftom)
    ! delth                 - fuel pellet stack axial displacement
    ! dcldh                 - cladding axial displacement
    ! HeatFlux              - surface heat flux (btu/sec-ftom**2)
    ! AxNodElevat           - axial position
    ! CritHtFlux            - critical heat flux (btu/sec-ftom**2)
    ! powave                - average fuel rod power (kW/ftom)
    ! CoolPress             - pressure in coolant channel (psia)
    ! flwblk                - percent flow blockage, If ruptured
    ! ruptem                - temperature (F) at rupture
    ! tp                    - temperature of plenum gas (F)
    ! flowg                 - rate of flow of gas from plenum (gm-moles/sec)
    ! FilmCoeffAv           - heat transfer coeficient at rod surface (btu/hr-ftom**2-F)
    ! HGapAv                - heat transfer coeficient across gas gap (btu/hr-ftom**2-F)
    ! EOSOxideThick         - depth of metal-water reaction on cladding (inches)
    ! WatrMetlEnrgy         - energy generated by metal-water reaction on cladding surface (kW/ftom)
    ! AxialNodLen(n)        - length associated with axial node n (ftom)
    ! CldPermStrn(k)        - cladding permanent radial strain
    ! EnrgyMeltZ(ll,k)      - energy absorbed in melting by l-th half mesh (two half meshes per radial node) (btu/ftom**3)
    ! qmaxmelt(ll)          - maximum energy half mesh ll can absorb in melting (btu/ftom**3)
    ! n3                    - total number of half meshes = 2*n1
    ! bup                   - fuel burnup (mwd/mtu)
    ! frpo2                 - fraction of fuel by weight that is plutonium
    ! tmelt(1)              - melt temperature of fuel (F)
    ! RInterfacGap(k)       - gap thickness at pellet interfaces (ftom)
    ! RInterfacPrs(k)       - interface pressure between fuel and cladding, psi
    ! rbmax                 - cladding radial displacement at peak ballooning point,in.
    ! CladEffStress(k)      - cladding effective stress (psi)
    ! modfal(jrod)          - mode of cladding failure. see sub. fala for detai
    ! TotalVoidVol          - total fuel rod void volume (ftom**3)
    ! EffStrain(k)          - effective plastic strain at axial node k,cladding
    ! EinstabilStrain(k)    - instability strain of cladding, axial node k
    ! StressAtInstStrain(k) - stress at instability strain, psi
    ! CladYieldStress(k)    - work-hardened yield stress, psi
    !
    ! common block  frcb  is in Subroutines fraptran, deform, initia,frap, cardin, heat, and restrw
    ! it stores in lcm arrays used by fracas-2 subcode. (sub. fcmi2)
    ! common block thcntl is link whereby thermal-hydraulic code controls the FrapTran calculations
    !
    ! common block bloona contains balon2 and far1 variables that are used outside of comput, or are required for restart
    !
    !    balon2 variables
    ! chstrs = maximum hoop stress in balloon region (psi)
    ! frbal  = balon2 circular/radial model switch
    ! pmxbal = balon2 maximum gap pressure (Pa)
    ! r8bal  = balon2 average radius at balon2 axial node 8 (m)
    ! tcebal = maximum circumferential strain in balloon region
    ! ifbaln = 1 If balon2 predicts failure
    ! jmnbal = balon2 radial contact node
    ! kntbal = 1 If balon2 predicts pellet-clad contact
    ! nbncal = 1 If balon2 has been called once
    !
    !    far1 variables
    ! pdrato = bundle pitch to rod diameter ratio
    ! rnbnt  = ratio of balloonable cells to total cells
    ! totnb  = total number of cells in a bundle
    ! nodprm = number flow area reduction subnodes
    ! farbal = flow area reduction   (m**2/m**2)
    ! sdfar  = standard deviation of flow area reduction
    ! zfarbl = axial location flow area reduction subnodes  (m)
    !
    INTEGER(ipk) :: jrod, kax, kmax, j, i, k, n, kb, nvoids, ick, kk, imxbl, iu = 2
    REAL(r8k) :: ftmm, thour, dum, dvsum, tsum, dv, tfave, bwrelng, sum, sumv, r1, r2, r3, &
      &          dv1, dv2, frmlt1, frmlt2, temp, dum1, dum2, bwrlhgr, tbar, r, bwrenth, trefout, &
      &          bwrhgapa, bwrgap, bwrhstrn, bwrstress, bwrtcl, bwrtfs, bwrtci, bwrtco
    LOGICAL :: u, PrintOxidation
    CHARACTER(LEN=1) :: tempunits
    CHARACTER(LEN=10), DIMENSION(2,12), PARAMETER :: Units = reshape([ &
      &         'ft)       ', 'm)        ', &
      &         '(inch)    ', '(mm)      ', &
      &         '(mils)    ', '(mm)      ', &
      &         '(psia)    ', '(n/m**2)  ', &
      &         '(Btu/sec  ', '(watt/m*  ', &
      &         '-ft**2)   ', '*2)       ', &
      &         '(Btu/hr.  ', '(watt/m*  ', &
      &         'ft**2-F)  ', '*2-K)     ', &
      &         '(F)       ', '(K)       ', &
      &         '(btu/lb)  ', '(J/kg)    ', &
      &         '(kW/ft)   ', '(kW/m)    ', &
      &         '(lb/hr-ft2', '(kg/sec-m2'  ], [2, 12])
    !
    REAL(r8k), PARAMETER :: htcoef = 5.6785_r8k
    REAL(r8k), PARAMETER :: psi = 6.89476e3_r8k
    REAL(r8k), PARAMETER :: flux = 1.135652667e4_r8k
    REAL(r8k), PARAMETER :: thou = 1.0e3_r8k
    REAL(r8k), PARAMETER :: cnvh = 0.42987e-03_r8k
    INTEGER(ipk), DIMENSION(naxn) :: il
    REAL(r8k), DIMENSION(naxn) :: adum2, adum3, duma, dumb, esum, ein
    REAL(r8k), DIMENSION(naxn + 1) :: adum
    !
    il = 0
    adum = 0.0_r8k
    adum2 = 0.0_r8k
    adum3 = 0.0_r8k
    duma = 0.0_r8k
    dumb = 0.0_r8k
    esum = 0.0_r8k
    ein = 0.0_r8k    
    ! Allocate arrays on the first call (determined  by efuelref not being allocated)
    IF (.NOT. ALLOCATED(efuelref)) CALL Allocate_Output_vars (naxn)
    !
    ftmm = thou * ftom
    !
    IF (unit) iu = 1
    u = .NOT. unit
    !
    jrod = 1
    Kax = 1
1   CONTINUE
    Kmax = Kax + 5
    IF (Kmax > naxn) Kmax = naxn
    j = 1
    IF (Kax <= 1) THEN
        j = 0
        thour = Time / sechr
        ! print out page header
        WRITE(ounit,9999) TRIM(codeid), TRIM(Title)
9999    FORMAT(//2x,a,/,2x,a)
        !
        WRITE(ounit,75) Time, thour
75      FORMAT(/' Conditions at time = ',e13.6,' sec',5x,' (',e13.6,' hours)' )
        IF (nbncal == 1) THEN
            IF (printballoon) THEN
                WRITE (0,116)
                WRITE (0,216) Time
216             FORMAT (' At a problem time of ',es12.5,' seconds')
                printballoon = .FALSE.
            ENDIF
        ENDIF
        IF (Ifaila /=  0) THEN
            IF (printrodburst) THEN
                WRITE(ounit,76) Ifaila, AxNodElevat(Ifaila), Time
                WRITE (0,76) Ifaila, AxNodElevat(Ifaila), Time
76              FORMAT(/' Rod burst at node',i3,', elevation  = ',f6.3,' ft',/,' At a problem time of ',es12.5,' seconds')
                printrodburst = .FALSE.
            ENDIF
        ENDIF
        WRITE(ounit,100) TimeIncrement, IterationCount, itcntd
100     FORMAT(/' time step = ',e13.6,' sec',/, &
            &   ' number of temperature-deformation-pressure loop iterations  = ',i4,/, &
            &   ' number of deformation-pressure loop iterations              = ',i4)
        dum = powave(jrod)
        IF (dum <= 1.0e-19_r8k) dum = 0.0_r8k
        IF (u) dum = dum / ftom
        WRITE(ounit,110) Units(iu,1), dum
110     FORMAT(' average fuel rod power (kW/',a3,33x,1pe14.6)
        ! Find average fuel temperature
        dvsum = 0.0_r8k
        tsum = 0.0_r8k
        !
        DO k = 1, naxn
            DO j = 1, igpnod
                IF (j == 1) THEN
                    IF (AxNodElevat(k) > zvoid2 .OR. AxNodElevat(k) < zvoid1) THEN
                        dv = pi * ((RadialBound(j+1) + RadialBound(j)) / 2.0_r8k) ** 2 * AxialNodLen(k)
                    ELSE
                        dv = 0.0_r8k
                    ENDIF
                ELSE IF (j == 2) THEN
                    IF (AxNodElevat(k) > zvoid2 .OR. AxNodElevat(k) < zvoid1) THEN
                        dv = pi * (((RadialBound(j) + RadialBound(j+1)) / 2.0_r8k) ** 2 - &
                          &  ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
                    ELSE
                        dv = pi * (((RadialBound(j+1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
                    ENDIF
                ELSE IF (j == igpnod) THEN
                    dv = pi * (RadialBound(j) ** 2 - ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
                ELSE
                    dv = pi * (((RadialBound(j) + RadialBound(j+1)) / 2.0_r8k) ** 2 - &
                      &  ((RadialBound(j-1) + RadialBound(j)) / 2.0_r8k) ** 2) * AxialNodLen(k)
                ENDIF
                dvsum = dvsum + dv
                tsum = tsum + dv * EOSTemp(j,k)
            ENDDO
        ENDDO
        !
        tfave = tsum/dvsum
        dum = tfave
        IF (u) dum = tfk(dum)
        WRITE(ounit,128) Units(iu,9), dum
128     FORMAT(' volume averaged fuel temperature ',a6,24x,1pe14.6)
        dum = 0.0_r8k
        !
        DO k = 1, naxn
            dum = dum + WatrMetlEnrgy(k) * AxialNodLen(k)
        ENDDO
        !
        WRITE(ounit,115) dum
115     FORMAT(' energy generated by metal-water reaction(kW) ',18x,1pe14.6)
        dum = delth * ftin
        IF (u) dum = delth * ftmm
        WRITE(ounit,120) Units(iu,2), dum
120     FORMAT(/' fuel stack axial extension ',a6,30x,1pe14.6)
        dum = dcldh * ftin
        IF (u) dum = dcldh * ftmm
        WRITE(ounit,130) Units(iu,2), dum
        bwrelng = dum
130     FORMAT(' cladding axial extension ',a6,32x,1pe14.6)
        dum = tp(jrod)
        IF (u) dum = tfk(dum)
        WRITE(ounit,132) Units(iu,9), dum
132     FORMAT(/' plenum gas temperature ',a6,34x,1pe14.6)
        IF (VoidVolumeRatio(6) >= 1.0e-9_r8k) THEN
            dum = tplenb
            IF (u) dum = tfk(dum)
            WRITE(ounit,152) Units(iu,9), dum
152         FORMAT(' bottom plenum gas temperature ',a6, 27x ,1pe14.6)
        ENDIF
        dum = GasPress(naxn + 1)
        maxgaspressure = MAX(maxgaspressure, GasPress(naxn+1))
        IF (u) dum = dum * psi
        WRITE(ounit,134) Units(iu,4), dum
134     FORMAT(' plenum gas pressure ',a8,35x,1pe14.6)
        WRITE(ounit,136) flowg(jrod)
136     FORMAT(' gas flow rate from plenum (gm-moles/sec) ',22x,1pe14.6)
        ! If qmaxmelt(1) = 0, steady-state. bypass molten fuel accounting
        IF (qmaxmelt(1) >= 1.0e-10_r8k) THEN ! Compute fraction of molten fuel in rod = jrod
            sum = 0.0_r8k
            sumv = 0.0_r8k
            DO k = 1, naxn
                sumv = sumv + AxialNodLen(k) * RadialBound(igpnod) * RadialBound(igpnod) * pi
                DO n = 2, igpnod
                    r1 = RadialBound(n-1)
                    r2 = r1 + 0.5_r8k * (RadialBound(n) - RadialBound(n-1))
                    dv1 = AxialNodLen(k) * pi * (r2 ** 2 - r1 ** 2)
                    r3 = RadialBound(n)
                    dv2 = AxialNodLen(k) * pi * (r3 ** 2 - r2 ** 2)
                    IF (NSteadyTrans == 1) THEN
                        ! steady state heat conduction was used
                        frmlt1 = 0.0_r8k
                        temp = EOSTemp(n-1,k)
                        temp = tfk(temp)
                        IF (temp > ftmelt) frmlt1 = 1.0_r8k
                        frmlt2 = 0.0_r8k
                        temp = EOSTemp(n,k)
                        temp = tfk(temp)
                        IF (temp > ftmelt) frmlt2 = 1.0_r8k
                        sum = sum + frmlt1 * dv1 + frmlt2 * dv2
                    ELSE
                        sum = sum + (EnrgyMeltZp1(n-1,k) / qmaxmeltp1(n-1)) * dv1 + (EnrgyMeltZ(n,k) / qmaxmelt(n)) * dv2
                    ENDIF
                ENDDO
            ENDDO
            !
            sum = sum / sumv
            WRITE(ounit,138) sum
138         FORMAT(/' fraction of molten fuel in fuel rod',28x,1pe14.6)
        ENDIF
        !
        DO n = 1, naxn
            IF (RuptFailIndex(n) == 3) Ifail = 3
            IF (RuptFailIndex(n) == 3 .OR. RuptFailIndex(n) == 1) kb = n
            IF (RuptFailIndex(n) == 1) Ifail = 1
        ENDDO
        !
        dum = TotalVoidVol * 1728.0_r8k
        IF (u) dum = dum * 25.4000508_r8k ** 3
        WRITE(ounit,135) Units(iu,2), dum
135     FORMAT(/' total free gas volume ',a4,'**3',34x,1pe14.6)
        IF (dum >= 1.0E-10_r8k) THEN
            WRITE(ounit,501) VoidVolumeRatio(1)
501     FORMAT('    plenum volume fraction',38x,1pe14.6)
            WRITE(ounit,502) VoidVolumeRatio(2)
502     FORMAT('    crack volume fraction ',38x,1pe14.6)
            WRITE(ounit,503) VoidVolumeRatio(3)
503     FORMAT('    gap volume fraction   ',38x,1pe14.6)
            WRITE(ounit,504) VoidVolumeRatio(4)
504     FORMAT('    open porosity volume fraction',31x,1pe14.6)
            WRITE(ounit,505) VoidVolumeRatio(5)
505     FORMAT('    dish volume fraction  ',38x,1pe14.6)
            !
        ENDIF
        IF (VoidVolumeRatio(6) > 1.0E-9_r8k) WRITE(ounit,506) VoidVolumeRatio(6)
506     FORMAT('    bottom plenum fraction', 38x,1pe14.6)
        WRITE(ounit,507) VoidVolumeRatio(7)
507     FORMAT('    central void volume fraction',32x,1pe14.6)
        WRITE(ounit,508) VoidVolumeRatio(8)
508     FORMAT('    fuel surface roughness volume fraction',22x,1pe14.6)
        WRITE(ounit,509) VoidVolumeRatio(9)
509     FORMAT('    cladding surface roughness volume fraction',18x,1pe14.6)
        IF (VoidVolumeRatio(10) > 1.0e-9_r8k) WRITE(ounit,511) VoidVolumeRatio(10)
511     FORMAT('    external plenum volume fraction',29x,1pe14.6)
        WRITE(ounit,*) ' '
        WRITE(ounit,910) relfract
910     FORMAT(1x,'current fission gas release fraction',27x,1pe14.6)
        WRITE(ounit,912) (GasFraction(i),i=1,ngases), TotalGasMoles
912     FORMAT(1x,'current fill gas composition',17x,f6.3,' He, ',f6.3,' Ar, ',f6.3,' Kr, ',f6.3,' Xe, ',f6.3,' H2, ', &
          &    f6.3,' N2, ',f6.3,' Air, ',f6.3,' H2O',/,1x,'current moles of gas in rod ',35x,1pe14.6/)
        WRITE(ounit,913) TranFuelSwell
913     FORMAT( 1x,'current transient fuel swelling factor   ', 22x,1pe14.6)
        IF (nbncal /=  0) THEN
            WRITE(ounit,116)
116         FORMAT(/' *** cladding ballooning has occurred. ***')
            dum1 = 0.0_r8k
            DO k = 1, naxn
                dum1 = MAX(dum1, CldPlasStrn(k,1))
            ENDDO
            dum1 = 100.0_r8k * dum1
            WRITE(ounit,117) dum1
117         FORMAT('     maximum circumferential strain (%)',25x,1pe14.6)
            dum1 = AxNodElevat(knonue)
            IF (u) dum1 = dum1 * ftom
            WRITE(ounit,121) Units(iu,1), dum1
121         FORMAT('     elevation of ballooning (',a3,31x,1pe14.6)
            IF (ifbaln == 0) THEN
                dum1 = 1.0E20_r8k
                dum2 = 0.0_r8k
                DO k = 1, 16
                    DO j = 1, 16
                        IF (Baln2Twall(k,j) < dum1) dum1 = Baln2Twall(k,j)
                        IF (Baln2Twall(k,j) > dum2) dum2 = Baln2Twall(k,j)
                    ENDDO
                ENDDO
                dum1 = dum1 / ftin
                dum2 = dum2 / ftin
                IF (.NOT. u) THEN
                    dum1 = dum1 * ftin
                    dum2 = dum2 * ftin
                ELSE
                    dum1 = dum1 * ftmm
                    dum2 = dum2 * ftmm
                ENDIF
                WRITE(ounit,86) Units(iu,2), dum1
86              FORMAT('     minimum wall thickness ',a6,30x,1pe14.6)
                WRITE(ounit,87) Units(iu,2), dum2
87              FORMAT('     maximum wall thickness ',a6,30x,1pe14.6)
                dum = chstrs
                IF (u) dum = dum * psi
                WRITE(ounit,118) Units(iu,4), dum
118             FORMAT('     maximum hoop stress ',a8,31x,1pe14.6)
            ENDIF
119     ENDIF
        !
        DO n = 1, naxn
            IF (RuptFailIndex(k) == 1 .OR. RuptFailIndex(k) == 4) THEN
                dum = AxNodElevat(k)
                IF (u) dum = dum * ftom
                WRITE(ounit,140) dum, Units(iu,1)
140             FORMAT(/' **** failure predicted',1pe14.6,2x,'(',a2,' from bottom of rod **** ')
            ENDIF
        ENDDO
        !
        IF (Time >= (reflpr + TimeIncrement)) THEN
            WRITE(ounit,510)
510         FORMAT(//, ' reflooding of core occurring ' )
            dum = fldrpr
            IF (u) dum = dum * ftmm / ftin
            WRITE(ounit,514) dum, Units(iu,2)
514         FORMAT(/,' flooding rate      = ',1pe14.6,a6,'/sec' )
            WRITE(ounit,516) crfpr
516         FORMAT(/,' carry out fraction = ',1pe14.6)
        ENDIF
        DO n = 1, naxn
            IF (CladCollapseIndex(n) == 1) THEN
                WRITE(ounit,145) n
145             FORMAT(/' *** Cladding collapse predicted at node', i5)
                j = 1
            ENDIF
        ENDDO
        !
    ENDIF
    !
    WRITE(ounit,142) Time
142 FORMAT(/'Time(sec) = ',1pe14.6/)
    ! Data for each axial node
    WRITE(ounit,150) (n,n = Kax,Kmax)
150 FORMAT(' Axial node number',15x,6(2x,i14))
    !
    DO k = Kax, Kmax
        dum = AxNodElevat(k)
        IF (u) dum = dum * ftom
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,170) Units(iu,1), (adum(k),k = Kax,Kmax)
170 FORMAT(' Elevation (',a3, 27x, 6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = AxialPowr(k)
        IF (dum <= 1.0E-19_r8k) dum = 0.0_r8k
        IF (u) dum = dum / ftom
        adum(k) = dum
    ENDDO
    bwrlhgr = adum(2)
    WRITE(ounit,180) Units(iu,1), (adum(k),k = Kax,Kmax)
180 FORMAT(' Local fuel rod power (kW/',a3, 13x, 6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        nvoids = 0
        IF (nvoid >= 1 .AND. AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
        CALL energy (EOSTemp, RadialBound, fotmtl, tempcs, EnrgyMeltZ, EnrgyMeltZp1, qmaxmelt, &
          &          qmaxmeltp1,  adum(k), tbar, k, nvoids, igpnod)
        IF (Time == 0.0_r8k) efuelref(k) = adum(k)
        adum2(k) = adum(k) - efuelref(k)
        adum2(k) = adum2(k) / 4186.8_r8k
        maxenthalpy=max(maxenthalpy,adum(k)* cnvh)
    ENDDO
    !
    IF (.NOT. u) adum(Kax:Kmax) = cnvh * adum(Kax:Kmax)
    bwrenth = adum(2) * 2.3887e-4_r8k
    trefout = tref
    tempunits  = 'F'
    IF (u) trefout = (trefout - 32.0_r8k) / 1.8_r8k + 273.15_r8k
    IF (u) tempunits = 'K'
    WRITE(ounit,172) trefout,tempunits,Units(iu,10), (adum(k),k = Kax,Kmax)
172 FORMAT(' rad.ave.fuel enthalpy ',f6.1,a1,' ref',a8,6(2x,1pe14.4))
    WRITE(ounit,572) (adum2(k),k = Kax,Kmax)
572 FORMAT(' fuel enthalpy increase after S.S.(cal/g)',1x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        IF (u) adum(k) = adum(k) * cnvh
        nvoids = 0
        IF (nvoid >= 1) THEN
            IF (AxNodElevat(k) >= zvoid1 .AND. AxNodElevat(k) <= zvoid2) nvoids = 1
        ENDIF
        IF (nvoids == 0) THEN
            adum(k) = rhof * pi * (RadialBound(igpnod) ** 2) * adum(k)
        ELSE
            adum(k) = rhof * pi * (RadialBound(igpnod) ** 2 - RadialBound(2) ** 2) * adum(k)
        ENDIF
        ! convert from btu to kW-s
        adum(k) = 1.054886_r8k * adum(k)
        IF (u) adum(k) = 3.28084_r8k * adum(k)
    ENDDO
    !
    WRITE(ounit,195) Units(iu,1),(adum(k),k = Kax,Kmax)
195 FORMAT(' energy in fuel (kW-s/',a3,17x,6(2x,1pe14.4))
    DO k = kax, kmax
        esum(k) = 0.0_r8k
        IF (Time == 0.0_r8k) efuel0(k) = adum(k)
        esum(k) = esum(k) + adum(k) - efuel0(k)
    ENDDO
    ! calculate energy absorbed in fuel
    ! printout -
    ! 1.adiabatic energy input per unit mass of fuel
    ! 2.adiabatic energy input per unit length of fuel rod
    ! 3.energy output per unit length
    DO k = Kax, Kmax
        adum(k) = CladEnrgPerL(k)
        IF (u) adum(k) = adum(k) / ftom
    ENDDO
    !
    WRITE(ounit,197) Units(iu,1),(adum(k),k = Kax,Kmax)
197 FORMAT(' energy in cladding (kW-s/',a3,13x,6(2x,1pe14.4))
    DO k = kax, kmax
        IF (Time == 0.0_r8k) eclad0(k) = adum(k)
        esum(k) = esum(k) + adum(k) - eclad0(k)
    ENDDO
    !
    DO k = Kax, Kmax
        dum = EnergyPerL(k)
        IF (u) dum = dum / ftom
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,189) Units(iu,1), (adum(k),k = Kax,Kmax)
189 FORMAT(' energy input after steady state (kW-s/',a3,6(2x,1pe14.4))
    DO k = kax, kmax
        IF (Time == 0.0_r8k) ein0(k) = adum(k)
        ein(k) = adum(k)
    ENDDO
    !
    DO k = Kax, Kmax
        dum = HFXSum(k)
        IF (u) dum = dum / ftom
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,192) Units(iu,1), (adum(k),k = Kax,Kmax)
192 FORMAT(' energy output after steady state(kW-s/',a3,6(2x,1pe14.4))
    DO k = kax, kmax
        IF (Time == 0.0_r8k) eout0(k) = adum(k)
        esum(k) = esum(k) + adum(k) - eout0(k)
    ENDDO
    DO k = kax, kmax
        adum(k) = esum(k) - ein(k) + ein0(k)
    ENDDO
    WRITE(ounit,193) Units(iu,1),(adum(k),k = Kax,Kmax)
193 FORMAT(' energy balance difference(kW-s/',a3,7x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = BulkCoolTemp(k)
        IF (u) dum = tfk(dum)
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,213) Units(iu,9), (adum(k),k = Kax,Kmax)
213 FORMAT(/' coolant bulk temperature',a3,14x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        adum(k) = coolqual(k)
    ENDDO
    !
    WRITE(ounit,215) (adum(k),k = Kax,Kmax)
215 FORMAT(' coolant quality',26x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = CoolPress(k)
        IF (u) dum = psi * dum
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,273) Units(iu,4), (adum(k),k = Kax,Kmax)
273 FORMAT(' coolant pressure ',a8,16x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = rmassflux(k)
        IF (u) dum = 0.001356_r8k * dum
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,217) Units(iu,12), (adum(k),k = Kax,Kmax)
217 FORMAT(' coolant mass flux ',a10,13x,6(2x,1pe14.4))
    DO k = Kax, Kmax
        dum = CladAveTemp(k)
        IF (u) dum = tfk(dum)
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,1073) Units(iu,9), (adum(k),k = Kax,Kmax)
1073 FORMAT(' cladding ave temperature',a3,14x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = HeatFlux(k)
        duma(k) = dum
        IF (ABS(duma(k)) < 1.0e-10_r8k) duma(k) = 1.0e-10_r8k
        IF (u) dum = dum * flux
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,220) Units(iu,5), Units(iu,6), (adum(k),k = Kax,Kmax)
220 FORMAT(' surface heat flux ',2a8, 7x, 6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = CritHtFlux(k)
        dumb(k) = dum
        IF (u) dum = dum * flux
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,230) Units(iu,5), Units(iu,6), (adum(k),k = Kax,Kmax)
230 FORMAT(' critical heat flux ',2a8, 6x, 6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        adum(k) = dumb(k) / duma(k)
    ENDDO
    !
    WRITE(ounit,234) (adum(k),k = Kax,Kmax)
234 FORMAT(' critical heat flux / surface heat flux',3x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = FilmCoeffAv(k)
        IF (u) dum = dum * htcoef
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,240) Units(iu,7), Units(iu,8), (adum(k),k = Kax,Kmax)
240 FORMAT(' surf. heat transf. coef. ',2a8,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        il(k) = Ih(k)
    ENDDO
    !
    WRITE(ounit,246) (il(k),k = Kax,Kmax)
246 FORMAT(' heat transfer mode',14x,6(13x,i3))
    !
    DO k = Kax, Kmax
        dum = HGapAv(k)
        IF (u) dum = dum * htcoef
        adum(k) = dum
    ENDDO
    bwrhgapa = adum(2)
    !
    WRITE(ounit,250) Units(iu,7), Units(iu,8), (adum(k),k = Kax,Kmax)
250 FORMAT(/' gap heat transfer coef. ',2a8,1x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = GapThick(k) * thou
        IF (.NOT. u) dum = dum * ftin
        IF (u) dum = dum * ftom
        adum(k) = dum
    ENDDO
    bwrgap = adum(2)
    !
    WRITE(ounit,260) Units(iu,3), (adum(k),k = Kax,Kmax)
260 FORMAT(' thermal radial gas gap',5x,a6,8x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = RInterfacGap(k) * thou
        IF (.NOT. u) dum = dum * ftin
        IF (u) dum = dum * ftom
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,267) Units(iu,3), (adum(k),k = Kax,Kmax)
267 FORMAT(' structural radial gas gap',4x,a6,6x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = GasPress(k)
        IF (u) dum = dum * psi
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,270) Units(iu,4), (adum(k),k = Kax,Kmax)
270 FORMAT(' gap gas pressure ',a8,16x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = TerfacePres(k)
        IF (u) dum = dum * psi
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,376) Units(iu,4), (adum(k),k = Kax,Kmax)
376 FORMAT(' interface pressure, struct. gap',a8,2x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = PelSrfDispl(k)
        dum = dum * ftin
        IF (u) dum = dum * ftmm / ftin
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,290) Units(iu,2), (adum(k),k = Kax,Kmax)
290 FORMAT(/' displacement of fuel outer surface ',a6,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = EOSRad(ncladi,k) - RadialBound(ncladi)
        dum = dum * ftin
        IF (u) dum = dum * ftmm / ftin
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,301) Units(iu,2), (adum(k),k = Kax,Kmax)
301 FORMAT(' displacement of clad inner surface ',a6,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = EOSRad(nmesh,k) - RadialBound(nmesh)
        dum = dum * ftin
        IF (u) dum = dum * ftmm / ftin
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,300) Units(iu,2), (adum(k),k = Kax,Kmax)
300 FORMAT(' displacement of clad outer surface ',a6,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        ! hoop strain
        adum(k) = CldStrn(k,1)
        ! axial strain
        adum2(k) = CldStrn(k,2)
        ! radial strain
        adum3(k) = CldStrn(k,3)
    ENDDO
    bwrhstrn = adum(2)
    !
    WRITE(ounit,1500) (CldStrn(k,1),k = Kax,Kmax)
1500 FORMAT(/' cladding total hoop strain    ',11x,6(2x,1pe14.4))
    WRITE(ounit,1501) (CldThermStrn(k,1),k = Kax,Kmax)
1501 FORMAT( ' cladding thermal hoop strain  ',11x,6(2x,1pe14.4))
    WRITE(ounit,1502) (CldElStrn(k,1),k = Kax,Kmax)
1502 FORMAT( ' cladding elastic hoop strain  ',11x,6(2x,1pe14.4))
    WRITE(ounit,1503) (CldPlasStrn(k,1),k = Kax,Kmax)
1503 FORMAT( ' cladding plastic hoop strain  ',11x,6(2x,1pe14.4))
    WRITE(ounit,1504) (CldStrn(k,2),k = Kax,Kmax)
1504 FORMAT(/' cladding total axial strain   ',11x,6(2x,1pe14.4))
    WRITE(ounit,1505) (CldThermStrn(k,2),k = Kax,Kmax)
1505 FORMAT( ' cladding thermal axial strain ',11x,6(2x,1pe14.4))
    WRITE(ounit,1506) (CldElStrn(k,2),k = Kax,Kmax)
1506 FORMAT( ' cladding elastic axial strain ',11x,6(2x,1pe14.4))
    WRITE(ounit,1507) (CldPlasStrn(k,2),k = Kax,Kmax)
1507 FORMAT( ' cladding plastic axial strain ',11x,6(2x,1pe14.4))
    WRITE(ounit,1508) (CldStrn(k,3),k = Kax,Kmax)
1508 FORMAT(/' cladding total radial strain  ',11x,6(2x,1pe14.4))
    WRITE(ounit,1509) (CldThermStrn(k,3),k = Kax,Kmax)
1509 FORMAT( ' cladding thermal radial strain',11x,6(2x,1pe14.4))
    WRITE(ounit,1510) (CldElStrn(k,3),k = Kax,Kmax)
1510 FORMAT( ' cladding elastic radial strain',11x,6(2x,1pe14.4))
    WRITE(ounit,1511) (CldPlasStrn(k,3),k = Kax,Kmax)
1511 FORMAT( ' cladding plastic radial strain',11x,6(2x,1pe14.4))
    DO k = Kax, Kmax
        dum = CldStress(k,1)
        IF (u) dum = dum * psi
        adum(k) = dum
    ENDDO
    !
    bwrstress = adum(2)
    WRITE(ounit,320) Units(iu,4), (adum(k),k = Kax,Kmax)
320 FORMAT(/' cladding hoop stress     ',a8,8x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = CldStress(k,2)
        IF (u) dum = dum * psi
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,330) Units(iu,4), (adum(k),k = Kax,Kmax)
330 FORMAT( ' cladding axial stress ',a8,11x,6(2x,1pe14.4))
    ! compute and print cladding effective stress
    DO k = Kax, Kmax
        dum = CladEffStress(k)
        IF (u) dum = psi * dum
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,382) Units(iu,4), (adum(k),k = Kax,Kmax)
382 FORMAT( ' effective cladding stress ',a8,7x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = CladYieldStress(k)
        IF (u) dum = dum * psi
        adum(k) = dum
    ENDDO
    !
    WRITE(ounit,422) Units(iu,4), (adum(k),k = Kax,Kmax)
422 FORMAT( ' cladding yield stress ',a8,11x,6(2x,1pe14.4))
    !
    DO k = Kax, Kmax
        dum = EffStrain(k)
        adum(k) = dum
    ENDDO
    !
    ! WRITE(ounit,432) (adum(k),k = Kax,Kmax)
    ! 432 FORMAT(' cladding effective plastic strain',7x,1p6e14.3)
    WRITE(ounit,434) (EinstabilStrain(k), k = Kax,Kmax)
434 FORMAT( ' cladding instability strain',14x,6(2x,1pe14.4))
    ! check effective stress against yield stress to warn user
    DO k = Kax, Kmax
        ! If small plastic strain, skip checks
        IF (EffStrain(k) >= 8.0e-04_r8k) THEN
            IF (Time > 0.0_r8k) THEN
                iok(1:5,k) = 0
                ! check to see If hit a nonconvergence criteria, set flags
                ! check If effective stress is within 5% of yield stress
                IF (CladEffStress(k) < CladYieldStress(k) * 0.95_r8k .OR. CladEffStress(k) > CladYieldStress(k) * 1.05_r8k) &
                  &     iok(1,k) = 1
                ! check If hoop stress is within 15% of effective stress
                IF (CldStress(k,1) < CladEffStress(k) * 0.85_r8k .OR. CldStress(k,1) > CladEffStress(k) * 1.15_r8k) &
                  &    iok(2,k) = 1
                ! check If effective plastic strain decreased from last time step
                IF ((EffStrain(k) - oldepeff(k)) < 0.0_r8k) iok(3,k) = 1
                ! check If effective plastic strain increased more than 0.5e-3
                IF ((EffStrain(k) - oldepeff(k)) > 5.0E-3_r8k) iok(4,k) = 1
                ! check If cladding hoop strain has increased
                IF (CldStrn(k,1) > oldeps(k)) iok(5,k) = 1
                ! WRITE out warning statement for specific conditions
                IF (iok(1,k) == 1 .AND. (iok(3,k) == 1 .OR. iok(4,k) == 1)) &
                  &     WRITE(ounit,385) n, iok(1,k), iok(3,k), iok(4,k), oldepeff(k)
                IF (iok(2,k) == 1 .AND. (iok(3,k) == 1 .OR. iok(4,k) == 1)) &
                  &     WRITE(ounit,487) n, iok(2,k), iok(3,k), iok(4,k), oldepeff(k)
                ! WRITE out the following warning only If had hit noncovergence on prior step
                DO ick = 1, 5
                    IF (iokold(ick,k) /=  0) THEN
                        IF (iok(5,k) == 1 .AND. (iok(1,k) == 1 .OR. iok(2,k) == 1)) &
                          &     WRITE(ounit,489) k,iok(5,k), iok(1,k), iok(2,k), oldeps(k)
                    ENDIF
                ENDDO
                !
385             FORMAT(1x,'WARNING, for axial node',i3,', mechanical convergence has not been achieved in COUPLE ', &
                  &       'for large cladding plastic strains.',/, &
                  &    1x,'plastic strains are too large, reduce time step size'/1x,'iok1,iok3,iok4,oldepeff = ',3i4,e13.4)
487             FORMAT(1x,'WARNING, for axial node',i3,', mechanical convergence has not been achieved in COUPLE ', &
                  &       'for large cladding plastic strains.',/, &
                  &    1x,'plastic strains are too large, reduce time step size'/1x,'iok2,iok3,iok4,oldepeff = ',3i4,e13.4)
489             FORMAT(1x,'WARNING, for axial node',i3,', mechanical convergence has not been achieved in COUPLE ', &
                  &       'for large cladding plastic strains',/, &
                  &    1x,'plastic strains are too large, reduce time step size'/1x,'iok5,iok1,iok2,oldeps = ',3i4,e13.4)
            ENDIF
            oldepeff(k) = EffStrain(k)
            oldeps(k) = CldStrn(k,1)
            iokold(1:5,k) = iok(1:5,k)
        ENDIF
    ENDDO
    ! output SED inFormation
    WRITE(ounit,708) (EffStrainPNNL(k),k = Kax,Kmax)
708 FORMAT (' PNNL elastic-plastic effective strain',4x,6(2x,1pe14.4))
    WRITE(ounit,706) (SEDPNNL(k),k = Kax,Kmax)
706 FORMAT (' SED using PNNL E-P eff. strain, MJ/m3',4x,6(2x,1pe14.4))
    WRITE(ounit,707) (SEDEPRI(k),k = Kax,Kmax)
707 FORMAT (' SED using EPRI formulation, MJ/m3',8x,6(2x,1pe14.4))
    ! WRITE oxide delta T
    DO k = Kax, Kmax
        ! convert from F to K
        adum(k) = deltox(k) / 1.8_r8k
    ENDDO
    WRITE(ounit,801) (adum(k),k = Kax,Kmax)
801  FORMAT(/' OD oxide temperature drop (K)',12x,6(2x,1pe14.4))
    ! Oxidation Printouts
    SELECT CASE (modmw)
    CASE (0) !Cathcart-Pawel
        IF(iStoicGrad == 0) WRITE(ounit,854)
        IF(iStoicGrad /=  0) WRITE(ounit,855)
854     FORMAT(1x,'Results for Cathcart-Pawel oxidation model, perfect stoichiometry assumption')
855     FORMAT(1x,'Results for Cathcart-Pawel oxidation model, stoichiometry gradient assumption')
        PrintOxidation = .TRUE.
    CASE (1) !No oxidation
        PrintOxidation = .FALSE.
    CASE (2) !Baker-Just
        WRITE(ounit,861)
861     FORMAT(1x,'Results for Baker-Just oxidation model')
        PrintOxidation = .TRUE.
    END SELECT
    IF (PrintOxidation) THEN
        DO k = Kax, Kmax
            dum = EOSOxideThick(k) * thou
            IF (u) dum = dum * ftom / ftin
            
            adum(k) = dum
        ENDDO
        !
        WRITE(ounit,346) Units(iu,3), (adum(k),k = Kax,Kmax)
346     FORMAT(' OD oxide thickness ',a6,16x,6(2x,1pe14.4))
        !
        DO k = Kax, Kmax
            dum = OxiThk2(k) * thou
            IF (u) dum = dum * ftom / ftin
            adum(k) = dum
        ENDDO
        !
        WRITE(ounit,347) Units(iu,3), (adum(k),k = Kax,Kmax)
347     FORMAT( ' ID oxide thickness ',a6,16x,6(2x,1pe14.4))
        !
        DO k = Kax, Kmax
            dum = OxygenUptake(k)
            adum(k) = dum
        ENDDO
        !
        WRITE(ounit,352) (adum(k),k = Kax,Kmax)
352     FORMAT(' OD oxygen uptake (kg/m**2)',15x,6(2x,1pe14.4))
        !
        WRITE(ounit,851) (OxUptakeID2(k),k = Kax,Kmax)
851     FORMAT(' ID oxygen uptake (kg/m**2)',15x,6(2x,1pe14.4))
        !
        WRITE(ounit,357) (ECR(k),k = Kax,Kmax)
357     FORMAT(' total ECR for oxidation, fraction',8x,6(2x,1pe14.4))
        !
        DO k = Kax, Kmax
            dum = WatrMetlEnrgy(k)
            IF (u) dum = dum / ftom
            adum(k) = dum
        ENDDO
        !
        WRITE(ounit,354) Units(iu,11), (adum(k),k = Kax,Kmax)
354     FORMAT(' metal-water reaction energy ',a8,5x,6(2x,1pe14.4))
    ENDIF
    !
    WRITE(ounit,355) (cexh2a(k),k = Kax,Kmax)
355 FORMAT(' total hydrogen (ppm) ',20x,6(2x,1pe14.4))
    !
    IF (nbncal == 1) THEN
        IF (Kax == 1) THEN
            dum = 0.0_r8k
            DO i = 1, nodprm
                IF (dum > farbal(i)) GOTO 362
                dum = farbal(i)
                imxbl = i
362         ENDDO
        ENDIF
        dum = AxNodElevat(knonue) * ftom - zfarbl(imxbl) + 0.5_r8k * (zfarbl(2) - zfarbl(1))
        !
        DO 366 k = Kax, Kmax
            dum1 = (2.0_r8k * zfarbl(1) - zfarbl(2) + dum) / ftom
            IF (AxNodElevat(k) > dum1) GOTO 364
            adum(k) = 0.0_r8k
            duma(k) = 0.0_r8k
            GOTO 366
364         CONTINUE
            !
            DO 365 kk = 1, nodprm
                dum1 = (zfarbl(kk) + dum) / ftom
                IF (AxNodElevat(k) > dum1) GOTO 365
                adum(k) = farbal(kk)
                duma(k) = sdfar(kk)
                GOTO 366
365         CONTINUE
            !
            adum(k) = 0.0_r8k
            duma(k) = 0.0_r8k
366     CONTINUE
        WRITE(ounit,372) (adum(k),k = Kax,Kmax)
372     FORMAT(' flow area reduction                  ',6(2x,1pe14.4))
        WRITE(ounit,373) (duma(k),k = Kax,Kmax)
373     FORMAT(' flow area reduction uncertainty      ',6(2x,1pe14.4))
    END IF
    WRITE(ounit,400) nprofile
400 FORMAT(/30x,'temperatures by radial mesh points using axial power profile number',i2)
    WRITE(ounit,401) Units(iu,2), Units(iu,9)
401 FORMAT(23x,' no.       mesh radius     temperatures'/ 34x,a6,10x,a3)
    !
    DO j = 1, nmesh
        r = RadialBound(j) * ftin
        IF (u) r = RadialBound(j) * ftmm
        !
        DO k = Kax, Kmax
            dum = EOSTemp(j,k)
            IF (u) dum = tfk(dum)
            adum(k) = dum
            if(j<=igpnod) maxfueltemp=MAX(maxfueltemp, EOSTemp(j,k))
            if(j>igpnod) maxcladtemp=MAX(maxcladtemp, EOSTemp(j,k))
        ENDDO
        ! WRITE out temperatures in C
        IF(j == 1) bwrtcl = adum(2) - 273.15_r8k
        IF(j == igpnod) bwrtfs = adum(2) - 273.15_r8k
        IF(j == igpnod+1) bwrtci = adum(2) - 273.15_r8k
        IF(j == nmesh) bwrtco = adum(2) - 273.15_r8k
        !
        IF (j == igpnod) THEN
            WRITE(ounit,411) j, r, (adum(k),k = Kax,Kmax)
411         FORMAT(i26,'**',7(2x,1pe14.4))
        ELSE
            WRITE(ounit,410) j, r, (adum(k),k = Kax,Kmax)
410         FORMAT(i26,2x,  7(2x,1pe14.4))
        ENDIF
    ENDDO
    !
    IF (Kmax >= naxn) GOTO 500
    Kax = Kmax + 1
    GOTO 1
    !
500 CONTINUE
    !
    END SUBROUTINE prntot
    !
    END MODULE OutputFile_fraptran













