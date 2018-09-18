MODULE Read_Input
    USE Kinds
    USE ErrorMsg, ONLY : namelist_read_error
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to read the input file.
    !> Subroutines include cardin, bcdinp, cinip, definp, intinp, ioinp
    !> modinp, numinp, powinp
    !>@author
    !> Ken Geelhood, PNNL
    !>@date
    !> 03/11/2016
    !
    PRIVATE
    PUBLIC :: cardin
    !
    ! Input units flag
    INTEGER(ipk) :: iu
    INTEGER(ipk), PARAMETER :: British = 1_ipk
    INTEGER(ipk), PARAMETER :: SI = 2_ipk
    !
    CONTAINS
    !
    SUBROUTINE cardin
    USE Kinds
    USE variables_fraptran
    USE frapc, ONLY : coupled
    USE UntinpData, ONLY : Convert_Input_Units
    USE Uncertainties, ONLY : ReadUncertainty
    USE Initialization
    IMPLICIT NONE
    !>@brief
    !> This Subroutine controls the input of all data blocks.  It processes the input
    !> parameters specified within the namelist options $iodata, $solution, $design,
    !> $power, $model & $boundary.
    !
    INTEGER(ipk) :: i
    
    ! ************ bcdinp arrays *****************
    mbowr      =    151
    mgbh       =    2000
    mhbh       =    2000
    mhinta     =    2000
    nhtca      =    151
    mhupta     =    2000
    nhtclv     =    151
    mhlqc1     =    2000
    mhlqcp     =    2000
    mndchf     =    151
    mpbh       =    2000
    mprest     =    40
    mtempt     =    40
    !
    ! ************ modinp arrays *****************
    !
    mwork      =    20
    maxidx     =    21
    mtabl1     =    50
    mtabl2     =    50
    mtabl3     =    50
    mgaspr     =    20
    !
    ! ************ powinp arrays *****************
    !
    npaxp1     =    600
    naazpd     =    10
    nrazpd     =    30
    nptha1     =    200
    !
    ! ************ ioinp arrays *****************
    !
    nopt(1:8) = 0
    !
    ! Initialize option switches for input blocks
    !
    nunopt = 0
    ibnopt = 0
    !
    ! Initialize error switch. IF input error occurs ierr will be set to one and execution will terminate at the end of cardin
    !
    ierr = 0
    !
    ! Initialize input common block variables
    !
    IF (ncards == 1) CALL cininp
    !
    ! $ioData block
    !
    nopt(1:6) = 1
    !
    CALL ioinp
    !
    WRITE(ounit,506)
506 FORMAT(/' Input/Output block input completed'//)
    
    !
    ! $solution block
    !
    CALL numinp
    !
    WRITE(ounit,507)
507 FORMAT(/' Solution control block input completed'//)
    
    !
    ! $design block
    !
    CALL intinp
    !
    WRITE(ounit,510)
510 FORMAT(/' Design of fuel rod block input completed'//)
    
    !
    ! $power input block
    !
    CALL powinp
    !
    WRITE(ounit,509)
509 FORMAT(/' Power definition block input completed'//)
    
    !
    ! $model selection block
    !
    CALL modinp
    !
    WRITE(ounit,511)
511 FORMAT(/' Model selection block input completed'//)
    
    !
    ! $boundary condition block
    !
    IF (.NOT. coupled) THEN
        ! Not linked with TH code. Read boundary condition block
        CALL bcdinp
        WRITE(ounit,512)
512     FORMAT(/' Boundary condition block input completed'//)
    ELSE
        ! It is linked with T/H Code. Bypass reading this block.
        WRITE(ounit,513)
513     FORMAT(/,15x,' Boundary condition block not read due to link with T/H Code'//)
    ENDIF
    
    !
    ! $uncertainties block
    !
    CALL ReadUncertainty
    
    ! Convert units if necessary
    IF (.NOT. unit) THEN
        CALL Convert_Input_Units
        WRITE(ounit,514)
514     FORMAT(' SI units converted to British internal units'//)
    ENDIF
    
    ! IF not restart, set default values based on multiple input blocks
    IF (ncards == 1) CALL definp
    
    ! Perturb input quantities for uncertainty analysis
    ! Compute necessary common block values which are derived from input
    CALL cominp
    !
    WRITE(ounit,515)
515 FORMAT(' Input variables have been placed in common'//)

    ! Input processing complete
    WRITE(ounit,200)
200 FORMAT(46x,' ***** Input processing completed *****'/)
    
    ! Terminate program based on error switch
    IF (ierr > 0) THEN
        WRITE(ounit,201)
201     FORMAT(40x,' ***** Execution terminated because of input errors ***** '////)
        ERROR STOP 'Execution terminated because of input errors in Subroutine: cardin'
    ENDIF
    !
    END SUBROUTINE cardin
    !
    !
    !
    SUBROUTINE bcdinp
    USE Kinds
    USE conversions_fraptran, ONLY : TO_UPPERCASE
    USE variables_fraptran, ONLY : ounit, htco, tem, pbh1, pbh2, hlqcl, ts, unit, iunit, reflpr
    USE collct_h
    USE resti_h
    USE htcb_h
    USE excb_h
    USE CoolantProperties
    USE bcdcom_h
    USE Dyna_h, ONLY : nodchf, Extentofbow, tschf, techf
    IMPLICIT NONE
    !> @brief
    !> Subroutine to read in Boundary Condition ($boundary) Data block
    !
    INTEGER(ipk) :: geomet, tape1, nvol1, lowpl, pressu, massfl, coreav, chf, filmbo, coldwa, axpow, &
      &             bowing, spefbz, geometry, nbundl, time, radiat, ruptur, liquid, inlet, reflo, pressure, &
      &             collaps, frapt4, geom, temp, tape2, nvol2, press, zone, upppl, jfb, nucbo, i, iiopt, &
      &             nchmfd, l, ndat, InputStat = 0
    REAL(r8k) :: ffch, emptm, fltgap2
    REAL(r8k), PARAMETER :: pfflc1 = 1.66_r8k       ! Flecht axial power peaking factor (peak power)/(average power)
    CHARACTER(LEN=3) :: coolant = 'OFF'
    CHARACTER(LEN=3) :: heat = 'OFF'
    CHARACTER(LEN=3) :: reflood = 'OFF'
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$boundary selection input block'
    CHARACTER(LEN=10), DIMENSION(2,11), PARAMETER :: Units = reshape([ &
      &          '(ft)      ', '(m)       ', &
      &          '(F)       ', '(K)       ', &
      &          '(ft**2)   ', '(m**2)    ', &
      &          '(btu/lbm) ', '(J/kg)    ', &
      &          '(psia)    ', '(Pa)      ', &
      &          '(lbm/hr-ft', '(kg/s-m**2', &
      &          '**2)      ', ')         ', &
      &          '(btu/hr-ft', '(W/m**2-K)', &
      &          '**2-F)    ', '          ', &
      &          '(ft/s)    ', '(m/s)     ', &
      &          '(inch/s)  ', '(m/s)     ' ], [2, 11])
    
    ! Define $boundary
    NAMELIST /boundary / coolant, reflood, radiation, heat, jchf, jfb, upppl, hupta, zad, zs, &
      &                  fltgap, fltgap2, geomet, tape1, nvol1, nchn, lowpl, pressu, massfl, &
      &                  coreav, chf, filmbo, coldwa, axpow, bowing, spefbz, geometry, nbundl, &
      &                  time, radiat, ruptur, liquid, inlet, reflo, pressure, collaps, frapt4, &
      &                  geom, temp, tape2,nvol2, press, zone, htco, nodchf, tem, dhe, dhy, achn, &
      &                  hinta, pbh1, gbh, hbh, ffch, bowthr, ExtentOfBow, tschf, techf, hydiam, &
      &                  flxsec, emptm, refdtm, hrad, temptm, fldrat, prestm, hlqcl, rshrd, ts, &
      &                  pbh2, htclev, htca, tblka, nucbo, nbhtc, jtr
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
    ! Set default values
    pfflec = pfflc1
    nbrliq = 0
    jchf = 0
    jfb = 0
    jtr = 0
    nbhtc = 0
    upppl = 0
    zad = 0.0_r8k
    zs = 0.0_r8k
    fltgap = 0.0_r8k
    fltgap2 = 0.0_r8k
    geomet = 0
    tape1 = 0
    nvol1 = 0
    lowpl = 0
    pressu = 0
    massfl = 0
    coreav = 0
    chf = 0
    filmbo = 0
    nucbo = 0
    coldwa = 0
    axpow = 0
    bowing = 0
    spefbz = 0
    geometry = 0
    nbundl = 0
    time = 0
    radiat = 0
    ruptur = 0
    liquid = 0
    inlet = 0
    reflo = 0
    pressure = 0
    collaps = 0
    frapt4 = 0
    geom = 0
    temp = 0
    tape2 = 0
    nvol2 = 0
    press = 0
    zone = 0
    htco = 0
    tem = 0
    dhe = 0.0_r8k
    dhy = 0.0_r8k
    achn = 0.0_r8k
    ffch = 0.0_r8k
    bowthr = 0
    hydiam = 0.0_r8k
    flxsec = 0.0_r8k
    emptm = 1.0E20_r8k
    refdtm = 1.0E20_r8k
    hrad = 0.0_r8k
    rshrd = 0.0_r8k
    
    ! Read $boundary
    READ (iunit, boundary, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'boundary')
    
    ! Ensure all input option characters are in uppercase
    coolant = TO_UPPERCASE(coolant)
    heat = TO_UPPERCASE(heat)
    radiation = TO_UPPERCASE(radiation)
    reflood = TO_UPPERCASE(reflood)
    
    ! Set parameters based on input
    nrefld = 0
    
    ! Radiation option
    IF (radiation == 'ON') THEN
        nradsh = 1
        IF (temp >= 1) THEN
            nradsh = temp
            DO i = 1, (2 * temp)
                tshrda(i) = ts(i)
            ENDDO
        ENDIF
    ENDIF
    
    ! Coolant option
    IF (coolant == 'ON') THEN
        ncooli = 1
        IF (tape1 == 1) THEN
            nvol = nvol1
            ithymx = nchn
            IF (ithymx > 0) ithymx = ithymx - 1
            iiopt = 0
            npbh = 2
            nqchn = 2
            nhbh = 2
            ngbh = 2
            nhinta = 2
            nhupta = 2
        END IF
        IF (lowpl >= 1) nhinta = lowpl
        IF (upppl >= 1) nhupta = upppl
        IF (lowpl == 0) GOTO 1330
        IF (upppl >= 1) GOTO 1340
        nhupta = nhinta
        hupta(1:2*nhinta) = hinta(1:2*nhinta)
        GOTO 1340
1330    IF (upppl == 0) GOTO 1340
        nhinta = nhupta
        hinta(1:2*nhupta) = hupta(1:2*nhupta)
1340    CONTINUE
        IF (pressu >= 1) THEN
            npbh = pressu
            pbh(1:2*pressu) = pbh1(1:2*pressu)
        ENDIF
        IF (massfl >= 1) ngbh = massfl
        IF (coreav >= 1) nhbh = coreav
        IF (filmbo == 1) jhtc = jfb
        IF (filmbo /= 1) jtr = 0
        IF (nucbo /= 1) nbhtc = 0
        IF (bowing >= 1) THEN
            nqbow = 1
            ffchf = ffch
            nbowr = bowing
            bowrat(1:2*bowing) = ExtentOfBow(1:2*bowing)
        ENDIF
        IF (coldwa == 1) lchf = 2
        IF (axpow == 1) lchf = 1
        IF (coldwa == 1 .AND. axpow == 1) lchf = 0
        IF (spefbz >= 1) THEN
            nchmfd = spefbz
            ndchfi(1:spefbz) = nodchf(1:spefbz)
            tschfi(1:spefbz) = tschf(1:spefbz)
            techfi(1:spefbz) = techf(1:spefbz)
        ENDIF
    ENDIF
    
    ! Heat option
    IF (heat == 'ON') THEN
        DO i = 2, zone
            IF (htco(i) == 0) htco(i) = htco(1)
            IF (tem(i) == 0) tem(i) = tem(1)
        ENDDO
        nhtc = 1
        nqchn = 3
        IF (tape2 == 1) THEN
            nqchn = 4
            nvol = nvol2
            fltgap = fltgap2
            IF (fltgap == 0.0_r8k) fltgap = 1.0_r8k
            npbh = 2
            nhbh = 2
            nhtcz = 2
        ENDIF
        IF (press >= 1) THEN
            npbh = press
            pbh(1:2*press) = pbh2(1:2*press)
        ENDIF
        IF (zone >= 1) nhtcz = zone
        DO l = 2, nhtcz
            IF (htco(l) == 0) htco(l) = htco(1)
            IF (tem(l) == 0) tem(l) = tem(1)
        ENDDO
        ! For axially supplied values, nhtcz = # of axial zones
        DO l = 1, nhtcz
            nhprs(l) = htco(l) ! HTC pairs
            ntprs(l) = tem(l)  ! Temperature pairs
        ENDDO
    ENDIF
    
    ! Reflood option
    IF (reflood == 'ON') THEN
        nrefld = 1
        IF (geometry == 1) THEN
            mbdl = nbundl
            IF (mbdl >= 0) mzq1 = 0
            IF (mbdl < 0) mzq1 = 1
            IF (mbdl < 0) mflt = 1
            IF (mbdl > 0) mflt = 1
            IF (mbdl == 0) mflt = 0
            IF (mbdl < 0) mbdl = ABS(mbdl)
        ENDIF
        IF (time == 1) THEN
            empytm = emptm
            reflpr = refdtm
        ENDIF
        IF (radiat == 1) THEN
            IF (fltgap == 0.0_r8k) fltgap = 1.0_r8k
            izadfg = 1
            IF (zad == 0) izadfg = 0
        ENDIF
        IF (ruptur == 1) lodmrk = 'ruptur'
        IF (liquid == 1) lodmrk = 'liqlev'
        IF (inlet >= 1) nbrtmp = inlet
        IF (reflo >= 1) nbrfdr = reflo
        IF (pressure >= 1) nbrpst = pressure
        IF (collaps >= 1) THEN
            nbrliq = collaps
            hlqcl1(1:2*collaps) = hlqcl(1:2*collaps)
        ENDIF
        IF (frapt4 == 1) nflec = 1
    ENDIF
    IF (jchf == 1 .OR. jchf == 2) lchf = 3
    lhtc = jchf * 10 + jhtc
    ! End of coolant condition input
    
    ! List out options
    ! Radiation input
    IF (nradsh /= 0) THEN
        WRITE(ounit,130)
130     FORMAT(/10x,'Radiation specification input')
        IF (geom == 1) WRITE(ounit,131) rshrd, Units(iu,1)
131     FORMAT(15x,'Flow shroud inner radius is ',e13.4,a4)
        ndat = nradsh * 2
        IF (temp >= 1) WRITE(ounit,132) (Units(iu,2),i = 1,3), nradsh, (tshrda(i),i = 1,ndat)
132     FORMAT(/10x,'Flow shroud temperature history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Temperature',4x,'Time',6x), &
          &    /30x,3(5x,a4,11x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
    ENDIF
    
    ! Coolant condition input
    IF (ncooli /= 0) THEN
        WRITE(ounit,140)
140     FORMAT(/10x,'Coolant condition specification input') 
        IF (geomet == 1) WRITE(ounit,141) dhe, Units(iu,1), dhy, Units(iu,1), achn, Units(iu,3)
141     FORMAT(15x,'Heated equivalent diameter of flow channel is ',e13.4,a4,/, &
          &    15x,'Hydraulic diameter of flow channel is         ',e13.4,a4,/, &
          &    15x,'Cross-sectional area of flow channel is       ',e13.4,a7)
        IF (tape1 == 1) WRITE(ounit,142) nvol ,ithymx + 1
142     FORMAT(/10x,'Coolant conditions are to be input on tape with ', &
          &    /15x,'Number of zones along the rod is ',i5, &
          &    /15x,'Number of bordering channels is  ',i5)
        IF (tape1 == 0) WRITE(ounit,143)
143     FORMAT(/10x,'Enthalpy calculation is specified')
        IF (lowpl >= 1) WRITE(ounit,150) (Units(iu,4),i = 1,3), nhinta, (hinta(i),i = 1,2*nhinta)
150     FORMAT(/10x,'Lower plenum coolant enthalpy history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Enthalpy',7x,'Time',6x), &
          &    /30x,3(5x,a9,6x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        IF (upppl >= 1) WRITE(ounit,155) (Units(iu,4),i = 1,3), nhupta, (hupta(i),i = 1,2*nhupta)
155     FORMAT(/10x,'Upper plenum coolant enthalpy history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Enthalpy',7x,'Time',6x), &
          &    /30x,3(5x,a9,6x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        IF (pressu >= 1) WRITE(ounit,160) (Units(iu,5),i = 1,3), npbh, (pbh(i),i = 1,2*npbh)
160     FORMAT(/10x,'Average core pressure history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Pressure',7x,'Time',6x), &
          &    /30x,3(5x,a6,9x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        IF (massfl >= 1) WRITE(ounit,165) (Units(iu,6), Units(iu,7),i = 1,3), ngbh, (gbh(i),i = 1,2*ngbh)
165     FORMAT(/10x,'Average mass flux', &
          &    /15x,'No. of pairs',3x,3(5x,'Mass flux',6x,'Time',6x), &
          &    /30x,3(5x,a10,a4,1x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        IF (coreav >= 1) WRITE(ounit,170) (Units(iu,4),i = 1,3), nhbh, (hbh(i),i = 1,2*nhbh)
170     FORMAT(/10x,'Core average enthalpy history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Enthalpy',7x,'Time',6x), &
          &    /30x,3(5x,a9,6x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        
        ! Nucleate boiling correlation
        SELECT CASE (nbhtc)
        CASE (0)
            WRITE(ounit,200)
200         FORMAT(/10x,'Nucleate boiling correlation selected is:   Thom plus Dittus-Boeleter')  
        CASE (1)
            WRITE(ounit,201)
201         FORMAT(/10x,'Nucleate boiling correlation selected is:   Chen')
        CASE DEFAULT
            WRITE(ounit,*) nbhtc
202         FORMAT ('Bad input value for nbhtc. Execution terminated in Subroutine: bcdinp. nbhtc = ',i3)
            ERROR STOP 'Bad input value for nbhtc. Execution terminated in Subroutine: bcdinp.'
        END SELECT
        
        ! Critical heat flux correlation
        SELECT CASE (jchf)
        CASE (0)
            WRITE(ounit,210)
210         FORMAT(/10x,'CHF correlation selected is:                EPRI-1')
        CASE (1)
            WRITE(ounit,211)
211         FORMAT(/10x,'CHF correlation selected is:                Bowring mixed flow cluster')
        CASE (2)
            WRITE(ounit,212)
212         FORMAT(/10x,'CHF correlation selected is:                MacBeth')
        CASE (3)
            WRITE(ounit,213)
213         FORMAT(/10x,'CHF correlation selected is:                Biasi')
        CASE (4)
            WRITE(ounit,214)
214         FORMAT(/10x,'CHF correlation selected is:                Modified Zuber')
        CASE DEFAULT
            WRITE(ounit,215) jchf
215         FORMAT ('Bad input for jchf. Execuction terminated in Subroutine: bcdinp. jchf = ',i3)
            ERROR STOP 'Bad input for jchf. Execuction terminated in Subroutine: bcdinp.'
        END SELECT
        
        ! Film boiling correlation
        SELECT CASE (jfb)
        CASE (0)
            WRITE(ounit,220)
220         FORMAT(/10x,'Film boiling correlation selected is:       Cluster Geometry Groeneveld 5.9')
        CASE (1)
            WRITE(ounit,221)
221         FORMAT(/10x,'Film boiling correlation selected is:       Annulus Geometry Groeneveld 5.7')
        CASE (2)
            WRITE(ounit,222)
222         FORMAT(/10x,'Film boiling correlation selected is:       Bishop-Sandberg-Tong')
        CASE (3)
            WRITE(ounit,223)
223         FORMAT(/10x,'Film boiling correlation selected is:       Groeneveld-Delorme')
        CASE DEFAULT
            WRITE(ounit,224) jfb
224         FORMAT ('Bad input for jfb. Execution terminated in Subroutine: bcdinp. jfb = ',i3)
            ERROR STOP 'Bad input for jfb. Execution terminated in Subroutine: bcdinp.'
        END SELECT
        !
        SELECT CASE (jtr)
        CASE (0)
            WRITE(ounit,230)
230         FORMAT(/10x,'Transition boiling correlation selected is: Modified Tong-Young')
        CASE (1)
            WRITE(ounit,231)
231         FORMAT(/10x,'Transition boiling correlation selected is: Modified Condie-Bengston')
        CASE (2)
            WRITE(ounit,232)
232         FORMAT(/10x,'Transition boiling correlation selected is: Bjornard-Griffith')
        CASE DEFAULT
            WRITE(ounit,233) jtr
233         FORMAT ('Bad input for jtr. Execution terminated in Subroutine: bcdinp. jtr = ',i3)
            ERROR STOP 'Bad input for jtr. Execution terminated in Subroutine: bcdinp.'
        END SELECT
        
        ! Rod bowing
        IF (bowing >= 1) WRITE(ounit,250) ffchf, bowthr, (Units(iu,1),i = 1,3), nbowr, (bowrat(i),i = 1,2*nbowr)
250     FORMAT(/10x,'Multiplier for rod bowing effect on chf is:              ',e13.4, &
          &    /10x,'Maximum bowing which can occur without effecting chf is: ',e13.4, &
          &    /10x,'Fraction of deflection in fuel rod due to bowing is      ', &
          &    /15x,'No. of pairs',3x,3(5x,'Enthalpy',7x,'Elevation',1x), &
          &    /30x,3(5x,15x,a4,6x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        ! Axial power factor
        IF (lchf == 0 .OR. lchf == 1) WRITE(ounit,255)
255     FORMAT(/10x,'Axial power factor is included in chf correlation')
        ! Cold wall factor
        IF (lchf == 0 .OR. lchf == 2) WRITE(ounit,256)
256     FORMAT(/10x,'Cold wall factor is included in chf correlation')
        ! Film boiling
        IF (spefbz >= 1) WRITE(ounit,257) (ndchfi(i), tschfi(i),techfi(i) ,i = 1,nchfmd)
257     FORMAT(/10x,'Specified film boiling zone', &
          &    /15x,'Node',4x,'Start time',5x,'End time', &
          &    /(15x,i4,4x,2(2x,1pe13.4)))
    ENDIF ! End of coolant condition input
    
    ! Heat transfer coeficient input
    IF (nhtc /= 0) THEN
        WRITE(ounit,300)
300     FORMAT(/10x,'Heat transfer coefficient specification input')
        
        ! Heat transfer coefficients
        IF (MAXVAL(htco) > 0) THEN
            DO l = 1, nhtcz
                ndat = nhprs(l) * 2
                WRITE(ounit,310) l, htclev(l), Units(iu,1)
                WRITE(ounit,311) (Units(iu,8), Units(iu,9),i = 1,3), nhprs(l), (htca(i,l),i = 1,ndat)
310             FORMAT(/10x,'Heat transfer coefficient history for zone ',i4,' With top boundary elevation of ',e13.6,a4)
311             FORMAT(15x,'No. of pairs',3x,3(5x,'HTC',12x,'Time',6x),/, &
                  &    30x,3(3x,a10,a6,1x,'(s)',7x),/, &
                  &    15x,i10,5x,6(2x,1pe13.4),/, &
                  &   (30x,6(2x,1pe13.4)))
            END DO
        END IF
        
        ! Temperature values
        IF (MAXVAL(tem) > 0) THEN
            DO l = 1, nhtcz
                ndat = ntprs(l) * 2
                WRITE(ounit,320) l, htclev(l), Units(iu,1)
                WRITE(ounit,321) (Units(iu,2),i = 1,3), ntprs(l), (tblka(i,l),i = 1,ndat)
320             FORMAT(/10x,'Bulk temperature history for zone ',i4,' With top boundary elevation of ',e13.6,a4)
321             FORMAT(15x,'No. of pairs',3x,3(5x,'Temperature',4x,'Time',6x),/, &
                  &    30x,3(5x,a4,11x,'(s)',7x),/, &
                  &    15x,i10,5x,6(2x,1pe13.4),/, &
                  &   (30x,6(2x,1pe13.4)))
            END DO
            
            ! Pressure values
            IF (press == 1) WRITE(ounit,160) (Units(iu,5),i = 1,3), npbh, (pbh(i),i = 1,2*npbh)
            
        ENDIF
    ENDIF ! End of heat transfer coefficient input
    
    ! Reflood input
    IF (nrefld == 1) THEN
        WRITE(ounit,400)
400     FORMAT(/10x,'Reflood specification input')
        !***********************************
        !* FLECHT-SEASET input information *
        !***********************************
        IF (mflt == 0 .AND. nflec == 0) WRITE (ounit,403)
        IF (mflt == 1 .AND. mzq1 == 0) WRITE (ounit,401)
        IF (mflt == 1 .AND. mzq1 == 1) WRITE (ounit,402)
401     FORMAT(15x,'Flecht-seaset correlation specified with the flooding correlation')
402     FORMAT(15x,'Flecht-seaset correlation specified with input of the quench elevation')
403     FORMAT(15x,'Line generalized flecht correlation specified')
        IF (nflec == 1) WRITE(ounit,405)
405     FORMAT(15x,'FRAP-T4 version of flecht correlation specified')
        IF (geometry == 1) WRITE(ounit,410) hydiam, Units(iu,1), flxsec, Units(iu,3)
410     FORMAT(15x,'Hydraulic diameter for reflood is:             ',1pe13.4,a4,/, &
          &    15x,'Flow channel cross sectional area for reflood: ',1pe13.4,a7)
        IF (time == 1) WRITE(ounit,412) refdtm, empytm
412     FORMAT(15x,'Problem time for initiation of reflood:        ',1pe13.4,'(s)',/, &
          &    15x,'Problem time for start of adiabatic heatup:    ',1pe13.4,'(s)')
        IF (radiat == 1) WRITE(ounit,413) hrad, Units(iu,8), Units(iu,9)
413     FORMAT(15x,'Radiation heat transfer coefficient is:        ',1pe13.4,a10,a6)
        IF (ruptur == 1) WRITE(ounit,414)
414     FORMAT(15x,'Line of demarcation is plane of cladding rupture')
        IF (liquid == 1) WRITE(ounit,415)
415     FORMAT(15x,'Line of demarcation is collapsed liquid level')
        
        ! Inlet temperature of flooding water as a function of time
        ndat = ABS(nbrtmp) * 2
        IF (inlet >= 1) WRITE(ounit,420) (Units(iu,2),i = 1,3), ABS(nbrtmp), (temptm(i),i = 1,ndat)
420     FORMAT(/10x,'Inlet temperature history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Temperature',4x,'Time',6x), &
          &    /30x,3(5x,a4,11x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        
        ! Reflood rate as a function of time
        ndat = ABS(nbrfdr) * 2
        IF (reflo >= 1) WRITE(ounit,425) (Units(iu,11),i = 1,3), ABS(nbrfdr), (fldrat(i),i = 1,ndat)
425     FORMAT(/10x,'Flooding rate history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Flooding rate',2x,'Time',6x), &
          &    /30x,3(5x,a6,9x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        
        ! Reactor vessel pressure as a function of time
        ndat = ABS(nbrpst) * 2
        IF (pressure >= 1) WRITE(ounit,430) (Units(iu,5),i = 1,3), ABS(nbrpst), (prestm(i),i = 1,ndat)
430     FORMAT(/10x,'Reactor vessel pressure history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Pressure',7x,'Time',6x), &
          &    /30x,3(5x,a4,11x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        
        ! Collapsed liquid level and time data pairs, specified from beginning of reflood
        ndat = ABS(nbrliq) * 2
        IF (collaps >= 1) WRITE(ounit,435) (Units(iu,1),i = 1,3), ABS(nbrliq), (hlqcl1(i),i = 1,ndat)
435     FORMAT(/10x,'Collapsed liquid level history is', &
          &    /15x,'No. of pairs',3x,3(5x,'Level',10x,'Time',6x), &
          &    /30x,3(5x,a6,9x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
    ENDIF ! End of reflood input
    !
    END SUBROUTINE bcdinp
    !
    !
    !
    SUBROUTINE cininp
    USE Kinds
    USE variables_fraptran
    USE NCGases, ONLY : ngases, HeIndex
    IMPLICIT NONE
    !> @brief
    !> This Subroutine initializes all input common block variables.
    !
    dtpo = 0.0_r8k
    luout = 0
    npdtpo = 0
    npltn = 0
    NFrapconInitialization = 0
    nswinw = 0
    ntplot = 0
    trest = 0.0_r8k
    unit = .TRUE.
    ! * * * * * problem numerics variables * * * * *
    dtss = 1.0e5_r8k
    epsht1 = 0.001_r8k
    idebug = 0
    nbalsw = 0
    nitmin = 0
    nzmesh = 0
    ndtmax = 0
    nprsw  = 0
    prsacc = 0.0005_r8k
    tdbugd = 0.001_r8k
    tmax   = 0.0_r8k
    tmpac1 = 0.0005_r8k
    zelev = 0.0_r8k
    fmesh = 0.0_r8k
    cmesh = 0.0_r8k
    dtmaxa = 0.0_r8k
    ! * * * * * problem tuning variables * * * * *
    fqcrit = 1.0_r8k
    nfhtc = 0
    tshtc = 1.0e20_r8k
    ! * * * * * power definition optional variables * * * * *
    CladdingPower = 0.0_r8k
    fpdcay = 0.0_r8k
    powop = 0.0_r8k
    timop = 0.0_r8k
    tpowf = 0.0_r8k
    !  * * * * * initial condition optional variables * * * * *
    cladid = 0.0_r8k
    coldbp = 0.0_r8k
    dishd = 0.0_r8k
    dishv0 = 0.0_r8k
    drod = 0.0_r8k
    gappr0(1) = 0.0_r8k
    nbotpl = 0
    nepp0 = 0
    nfastf = 0
    pelh = 0.0_r8k
    FuelPelDiam = 0.0_r8k
    pitch = 0.0_r8k
    pdrato = 1.32_r8k
    rnbnt = 1.0_r8k
    totnb = 289.0_r8k
    rhoc = 0.0_r8k
    rhof = 0.0_r8k
    rl = 0.0_r8k
    rshd = 0.0_r8k
    rsntr = 0.0_r8k
    scd(1) = 0.0_r8k
    spdbp = 0.0_r8k
    spl(1) = 0.0_r8k
    splbp = 0.0_r8k
    swd(1) = 0.0_r8k
    tempcs = 77.0
    tgas0 = 0.0_r8k
    tsntrk = 0.0_r8k
    volbp = 0.0_r8k
    vplen(1) = 0.0_r8k
    ! Default gas to 100% Helium
    GasFraction = 0.0_r8k
    GasFraction(HeIndex) = 1.0_r8k
    ! * * * * * model selection variables * * * * *
    dofang = 0.0_r8k
    dofset = 0.0_r8k
    dtmpcl = 0.0_r8k
    emflag = 'off'
    modfd = 0
    modkf = 2
    modmw = 1
    naz = 0
    nconsw = 0
    ndim = 0
    nedtsw = 0
    nlac = 0
    nnaz = 0
    ! Default to using plenum gas temperature model
    nplnt = 0
    nsymm = 0
    nvoid = 0
    pfail = 1.1_r8k
    rvoid = 0.0_r8k
    swllfr = 0.0_r8k
    zvoid1 = 0.0_r8k
    zvoid2 = 0.0_r8k
    ! * * * * * boundary condition variables * * * * *
    achn = 0.0_r8k
    crf = 0.0_r8k
    dhe = 0.0_r8k
    dhy = 0.0_r8k
    empytm = 1.0e20_r8k
    flowbk = 0.0_r8k
    flxsec = 0.0_r8k
    hliq = 0.0_r8k
    hrad = 0.0_r8k
    hydiam = 0.0_r8k
    irup = 0
    ithymx = 0
    nsrad3 = 0
    ! Default to using W-3 chf correlation
    jchf = 4
    ! Default to no axial or cold wall factor on chf
    lchf = 3
    ! Default to using condie-bengston film boiling corr.
    jhtc = 0
    kaxnlo = 0
    lodmrk = 'ruptur'
    nchn = 1
    ncooli = 0
    nflec = 0
    nhtc = 0
    nqbow = 0
    IF (.NOT. coupled) nqchn = 0 !Don't override command coming from T/H Code Link
    nradsh = 0
    nrc(1,1) = 1
    nrefld = 0
    IF (.NOT. coupled) nvol = 1
    oldtim = 0.0_r8k
    pchn(1,1) = 1.0_r8k
    qmax  =  0.0_r8k
    refdtm = 1.0e20_r8k
    rshrd = 0.0_r8k
    rupflg = 'off'
    ruplev = 0.0_r8k
    tempmx = 0.0_r8k
    tflood = 0.0_r8k
    tsub = 0.0_r8k
    zqch = 0.0_r8k
    zroug1 = 1.0e10_r8k
    zroug2 = 1.0e10_r8k
    !
    END SUBROUTINE cininp
    !
    !
    !
    SUBROUTINE definp
    USE Kinds
    USE variables_fraptran
    IMPLICIT NONE
    !> @brief
    !> This subroutine calculates all defaults when particular subroutines are not input
    INTEGER(ipk) :: i, j, nh2, ndt2, iindx, ir, ipo, index1, index2, ipltn
    REAL(r8k) :: sump, pmax, tof, toc
    INTEGER(ipk), DIMENSION(9) :: ndeflt = 0
    
    ! Write to output file
    WRITE(ounit,10)
10  FORMAT(40x,'Default summary'/)
    
    ! Define variables for solution control
    !
    ! * * * * * time limits * * * * *
    !
    ! * * * * * default time step * * * * *
    !
    IF (dtmaxa(1) == 0.0_r8k) THEN
        ndeflt(2) = 1
        ! estimate the type of scenerio based upon the power input and problem time
        ! check for slow ramp
        IF (tmax - t0 - 200.0_r8k < 0.0) GOTO 100
        IF (tmax - t0 - 200.0_r8k >= 0.0) GOTO 300
        ! compute average power for problem
100     nh2 = nptha * 2 - 2
        sump = RodAvePower(1) * (RodAvePower(4) - RodAvePower(2)) + RodAvePower(nh2-1) * (RodAvePower(nh2+2) - RodAvePower(nh2))
        IF (nh2 >= 4) THEN
            DO i = 4, nh2, 2
                sump = sump + RodAvePower(i-1) * (RodAvePower(i+2) - RodAvePower(i-2))
            ENDDO
        END IF
        sump = 0.5_r8k * sump / RodAvePower(nh2+2)
        ! compute maximum power
        pmax = 0.0_r8k
        DO i = 1,nh2,2
            IF (pmax < RodAvePower(i)) pmax = RodAvePower(i)
        ENDDO
        ! check for ria (pmax>30kW/ft)
        IF (pmax - 30.0_r8k <= 0.0_r8k) GOTO 140
        IF (pmax - 30.0_r8k > 0.0_r8k) GOTO 260
        ! check for loca( paverage < 6 kW/ft)
140     IF (sump - 6.0_r8k < 0.0_r8k) GOTO 250
        IF (sump - 6.0_r8k >= 0.0_r8k) GOTO 150
        ! check for  atws
150     IF (RodAvePower(nh2+1) - RodAvePower(1) < 0.0_r8k) GOTO 160
        IF (RodAvePower(nh2+1) - RodAvePower(1) >= 0.0_r8k) GOTO 180
        ! atws
        ! If tmax>20 set up geometrically varying time step
160     IF (tmax > 20.0_r8k) GOTO 400
        GOTO 300
        ! pcm event
        ! set up time step accordding to tmax
        !
        ! If tmax>50 set up geometrically varying time step
180     IF (tmax - t0 > 50.0_r8k) GOTO 400
        GOTO 300
        ! If tmax>50 set up geometrically varying time step
250     IF (tmax - t0 > 50.0_r8k) GOTO 400
        dtmaxa(1) = 0.02_r8k
        dtmaxa(2) = t0
        dtmaxa(3) = 0.02_r8k
        dtmaxa(4) = t0 + 0.05_r8k
        dtmaxa(5) = 0.05_r8k
        dtmaxa(6) = t0 + 0.0501_r8k
        dtmaxa(7) = 0.05_r8k
        dtmaxa(8) = t0 + 1.9_r8k
        dtmaxa(9) = 0.1_r8k
        dtmaxa(10) = t0 + 2.0_r8k
        ndtmax = 5
        GOTO 450
        ! ria
        ! If tmax>0.5 set up geometrically varying time step
260     IF (tmax - t0 > 0.5_r8k) GOTO 400
        ndtmax = 1
        dtmaxa(1) = 0.001_r8k
        dtmaxa(2) = t0
        GOTO 450
        ! slow ramp or transient suitable for a constant time step
300     ndtmax = 1
        dtmaxa(1) = (tmax - t0) / 500.0_r8k
        dtmaxa(2) = t0
        GOTO 450
400     ndt2 = 14
        ndtmax = 7
        ! set up geometrical increasing time step for 500 total points
        dtmaxa(2) = t0
        dtmaxa(6) = (tmax - t0) / 8.0_r8k
        dtmaxa(10) = dtmaxa(4) * 2.0_r8k
        dtmaxa(14) = dtmaxa(6) * 2.0_r8k
        dtmaxa(1) = dtmaxa(4) / 250.0_r8k
        dtmaxa(5) = dtmaxa(1) * 2.0_r8k
        dtmaxa(9) = dtmaxa(3) * 4.0_r8k
        dtmaxa(13) = dtmaxa(5) * 2.0_r8k
        DO i = 4, ndt2, 4
            dtmaxa(i-1) = dtmaxa(i-3)
            dtmaxa(i) = dtmaxa(i+2) - dtmaxa(1) / 2.0_r8k
        ENDDO
    END IF
450 CONTINUE
    ! * * * * * property table initialization * * * * *
    ! Skip If restart is being performed and property table input was not changed
!450 GO TO 500
    !ndeflt(3) = 1
    ! Set temperature spans to room temperature and melting
    !tof = 70.0_r8k
    !toc = 70.0_r8k
    !tmaxf = 5140.0_r8k
    !tmaxc = 3320.0_r8k
    ! Set number of points to span about 450 F for fuel
    !nkf = 20
    !nsf = 20
    ! Set number of points to span about 90 F for cladding
    !nkc = 36
    !nsc = 36
    ! * * * * * nodalization * * * * *
500 IF (nunopt(11) <= 0) THEN
        ! Set default axial nodes to correspond to axial power profile
        ndeflt(4) = 1
        iindx = npaxp * 2
        j = 1
        DO i = 2, iindx, 2
            IF (AxPowProfile(i,1) > rl) EXIT
            zelev(j) = AxPowProfile(i,1)
            j = j + 1
        ENDDO
        IF (i > iindx) j = j - 1
        naxn = j
    END IF
    
    ! Set defaults for fuel dimensions based upon radial power profile
    SELECT CASE (nunopt(12))
    CASE (0)
        ndeflt(5) = 1
        ir = nprad * 2
        j = 2
        fmesh(1) = 0.0_r8k
        ! Set second node to rvoid if central void is specified
        IF (nvoid == 1) THEN
            fmesh(2) = rvoid
            j = 3
        END IF
        !
        DO i = 2, ir, 2
            IF (RadPowProfile(i) > fmesh(j-1)) THEN
                fmesh(j) = RadPowProfile(i)
                ! Make sure default nodes do not exceed fuel radius
                IF (fmesh(j) > rf + 1.0e-10_r8k) EXIT
                j = j + 1
            ENDIF
        ENDDO
        ! Make sure outer node corresponds to fuel radius
        IF (i > ir) j = j - 1
        IF (fmesh(j) <= rf - 1.0e-10_r8k) fmesh(j) = rf
        nfmesh = j
    CASE (1)
        ! Do nothing
    CASE DEFAULT
        ! Check to make sure outer radial node radius is equal to rf
        IF (ABS(fmesh(nfmesh) - rf) >= 1.0e-6_r8k) THEN
            ierr = 1
            WRITE(ounit,310) fmesh(nfmesh), rf
310         FORMAT(/' *** Outer fuel radial node radius, ',1pe13.4,' does not equal the pellet radius, ',1pe13.4,/, &
              &     ' Check input')
        END IF
    END SELECT
    
    ! Set defaults for clad dimensions
    SELECT CASE (nunopt(13))
    CASE (0)
        ndeflt(5) = 1
        RadialBound(j) = cladid / 2.0_r8k
        RadialBound(j+1) = cladod / 2.0_r8k
        ncmesh = 2
    CASE (1)
        ! Do nothing
    CASE DEFAULT
        ! Check to make sure cladding radial nodes correspond to cladding radius
        IF (ABS(cmesh(1) - cladid / 2.0_r8k) >= 1.0e-10_r8k) THEN
            ierr = 1
            WRITE(ounit,320) cmesh(1), cladid / 2.0_r8k
320         FORMAT(/' *** Inner cladding radial node radius, ',1pe13.4,' does not equal the cladding inner radius, ',1pe13.4,/, &
              &     ' Check input')
        END IF
        IF (ABS(cmesh(ncmesh) - cladod / 2.0_r8k) >= 1.0e-10_r8k) THEN
            ierr = 1
            WRITE(ounit,321) cmesh(ncmesh),cladod/2.0_r8k
321         FORMAT(/' *** Outer cladding radial node radius, ',1pe13.4,' does not equal the cladding outer radius, ',1pe13.4/&
              &     ' Check input')
        END IF
    END SELECT
    
    ! Initial estimates
    
    ndeflt(7) = 1
    
    ! Printout time interval
    IF (dtpoa(1) == 0.0_r8k) THEN
        ipo = ndtmax * 2
        ndeflt(6) = 1
        npdtpo = ndtmax
        DO i = 1, ipo, 2
            dtpoa(i) = dtmaxa(i) * 5.0_r8k
            dtpoa(i+1) = dtmaxa(i+1)
        ENDDO
        ! check to make sure total number of printouts will be less than 200
        iindx = 0
        index2 = ipo - 2
        DO i = 1, index2, 2
            index1 = i + 1
            IF ((i+3) > ipo .OR. tmax < dtmaxa(i+3)) EXIT
            iindx = iindx + INT((dtmaxa(i+3) - dtmaxa(i+1)) / dtmaxa(i))
        ENDDO
        iindx = iindx + INT((tmax - dtmaxa(index1)) / dtmaxa(index1-1))
        IF (iindx > 200) THEN
            DO i = 1, index1, 2
                dtpoa(i) = dtpoa(i) * AINT(iindx / 20.0_r8k) * 0.1_r8k
            ENDDO
            ndtmax = index1 / 2
        END IF
    END IF
    
    ! Plot time interval
    IF (npltn == 0) THEN
        ndeflt(9) = 1
        IF (npdtpo <= 1) THEN
            npltn = 0
        ELSE
            ipltn = npdtpo * 2
            DO i = 1, ipltn, 2
                dtplta(i+1) = dtpoa(i+1)
                dtplta(i) = dtpoa(i) / 10.0_r8k
            ENDDO
            npltn = ipltn / 2
        END IF
    END IF
    
    ! Design of fuel rod variables
    
    ! Generate cladding fast neutron flux default
    IF (nfastf ==  0) ndeflt(8) = 1
    
    ! I/O control
    WRITE(ounit,501)
501 FORMAT(/10x,'Input/Output defaults')
    
    ! Output file interval
    IF (ndeflt(6) /= 0) WRITE(ounit,502) (dtpoa(i),i = 1,ipo)
502 FORMAT(/10x,'Printout interval specification', &
      &    /15x,4(3x,' Print  ',7x,'Problem',5x), &
      &    /15x,4(3x,'Interval',7x,' Time  ',5x), &
      &    /15x,4(3x,'  (s)   ',7x,'  (s)  ',5x), &
      &    /(15x,8(2x,1pe13.6)))
     
    ! Plot file interval
    IF (ndeflt(9) /= 0) THEN
        IF (npltn == 0) THEN
            WRITE(ounit,505)
505         FORMAT(/10x,'Plot interval is same as time step')
        ELSE IF (npltn > 0) THEN
            WRITE(ounit,506) (dtplta(i),i = 1,ipltn)
506         FORMAT(/10x,'Plot interval specification', &
              &    /15x,4(3x,'  Plot  ',7x,'Problem',5x), &
              &    /15x,4(3x,'Interval',7x,' Time  ',5x), &
              &    /15x,4(3x,'  (s)   ',7x,'  (s)  ',5x), &
              &    /(15x,8(2x,1pe13.6)))
        END IF
     END IF
    
    ! Time control
    WRITE(ounit,510)
510 FORMAT(/10x,'Time control defaults')
    
    ! Problem time step size
    IF (ndeflt(2) /= 0) THEN
        ndt2 = ndtmax * 2
        WRITE(ounit,512) ndtmax, (dtmaxa(i),i = 1,ndt2)
512     FORMAT(/10x,'Time dependent time step', &
          &    /15x,'No. of pairs',3x,3(5x,'Time',11x,'Time',7x), &
          &    /30x,3(5x,'increment',16x), &
          &    /15x,i10,5x,3(5x,'(s)',12x,'(s)',7x), &
          &    /(30x,6(2x,1pe13.4)))
    END IF
    
    ! Property table initialization
    IF (ndeflt(3) /= 0) WRITE(ounit,520) nkf, tof, tmaxf, nkc, toc, tmaxc
520 FORMAT(/10x,'Fuel property tables will have     ',i5,' values', &
      &    /15x,'Lower temperature limit is: ',1pe13.4,' (F)', &
      &    /15x,'Upper temperature limit is: ',1pe13.4,' (F)'/, &
      &    /10x,'Cladding property tables will have ',i5,' values', &
      &    /15x,'Lower temperature limit is: ',1pe13.4,' (F)', &
      &    /15x,'Upper temperature limit is: ',1pe13.4,' (F)')
    
    ! Axial nodalization
    IF (ndeflt(4) /= 0) WRITE(ounit,524) (i,zelev(i),i = 1,naxn)
524 FORMAT(/10x,'Specified axial nodes', &
      &    /15x,'Node',6x,'Elevation', &
      &    /(15x,(i4,3x,1pe13.4)))
    
    ! Radial nodalization
    IF (ndeflt(5) /= 0) THEN
        iindx = nfmesh + ncmesh
        WRITE(ounit,526) (i,RadialBound(i),i = 1,iindx)
526     FORMAT(/10x,'Specified fuel radial nodes', &
          &    /15x,'Node',6x,'Radius', &
          &    /15x,'    ',6x,' (ft) ', &
          &    /(15x,(i4,3x,1pe13.4)))
     END IF
    
    ! Deformation model
    IF (ndeflt(8) /= 0) THEN
        WRITE(ounit,530)
530     FORMAT(/10x,'Deformation model defaults')
        WRITE(ounit,531)
531     FORMAT(15x,'Cladding fast flux axial flux profile is the same as the axial power profile')
    END IF
    
    !
    WRITE(ounit,550)
550 FORMAT(/' End of default summary'/)
    !
    END SUBROUTINE definp
    !
    !
    !
    SUBROUTINE intinp
    USE Kinds
    USE conversions_fraptran, ONLY : tfk
    USE variables_fraptran, ONLY : ounit, gadoln, unit, naxn, iunit, &
      & fluxz, ncs, GasFraction, npelraddeviat, nfastf, pdrato, rnbnt, &
      & totnb, roughf, roughc, bumtp, compmt, deloxy, imox, CladType, GasMoles0, nepp0, eppinp, radpel, &
      & cladid, cladod, fuelpeldiam, rf, cladtk, pitch, fotmtl, tsntrk, fgrns, cldwdc, splbp, coldbp, &
      & spdbp, volbp, ncolbp, nbotpl, rsntr
    USE scalr_h
    USE NCGases, ONLY : ngases
    IMPLICIT NONE
    !>@brief
    !> Subroutine to read in the $design block
    !>@author
    !> Modified by I. Porter, NRC, April 2014 to clean coding and convert to .f90
    !
    INTEGER(ipk) :: i, icount, j, InputStat = 0
    REAL(r8k) :: RodDiameter, RodLength, gapthk, gsms, rodfabtemp, fdens, sumg
    REAL(r8k), DIMENSION(ngases) :: gfrac
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$design input block'
    CHARACTER(LEN=10), DIMENSION(2,5), PARAMETER :: Units = reshape([ &
      &                           '(ft)      ', '(m)       ', &
      &                           '(F)       ', '(K)       ', &
      &                           '(ft**3)   ', '(m**3)    ', &
      &                           '(lb/ft**3)', '(kg/m**3) ', &
      &                           '(psia)    ', '(n/m**2)  ' ], [2, 5])
    
    ! Define $design
    NAMELIST / design / pitch, pdrato, rnbnt, CladType, RodLength, RodDiameter, dishd, pelh, dishv0, &
      &                 FuelPelDiam, roughf, frden, bup, rshd, frpo2, fotmtl, tsntrk, fgrns, gadoln, &
      &                 gapthk, coldw, roughc, cfluxa, tflux, cldwdc, spl, scd, swd, vplen, splbp, &
      &                 coldbp, spdbp, volbp, gfrac, gsms, gappr0, tgas0, fluxz, radpel, eppinp, &
      &                 totnb, ncs, ncolbp, OpenPorosityFraction
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
    ! Set default values
    fluxz = 0.0_r8k
    pitch = 0.0_r8k
    pdrato = 1.32_r8k
    rnbnt = 1.0_r8k
    CladType = 4
    RodLength = 0.0_r8k
    RodDiameter = 0.0_r8k
    rshd = 0.0_r8k
    dishd = 0.0_r8k
    pelh = 0.0_r8k
    dishv0 = 0.0_r8k
    FuelPelDiam = 0.0_r8k
    roughf = 2.0_r8k
    frden = 0.0_r8k
    bup = 0.0_r8k
    frpo2 = 0.0_r8k
    fotmtl = 2.0_r8k
    tsntrk = 1883.0_r8k
    fgrns = 10.0_r8k
    gadoln = -1.0_r8k
    gapthk = 0.0_r8k
    coldw = 0.0_r8k
    roughc = 0.5_r8k
    cfluxa = 0.0_r8k
    tflux = 0.0_r8k
    cldwdc = 0.0_r8k
    spl = 0.0_r8k
    scd = 0.0_r8k
    swd = 0.0_r8k
    vplen = 0.0_r8k
    splbp = 0.0_r8k
    coldbp = 0.0_r8k
    spdbp = 0.0_r8k
    volbp = 0.0_r8k
    GasMoles0 = 0.0_r8k
    gfrac(1) = 1.0_r8k
    gfrac(2:ngases) = 0.0_r8k
    gappr0 = 0.0_r8k
    tgas0 = 0.0_r8k
    totnb = 289.0_r8k
    ncs = 1
    ncolbp = 1
    OpenPorosityFraction = 0.0_r8k
    gsms = 0.0_r8k
    
    ! Read $design
    READ (iunit, design, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'design')
    
    ! Set options based on input
    
    ! Assume that the user-supplied dish is on both sides of the pellet
    dishv0 = dishv0
    
    ! Gadolinia
    gadolin = gadoln
    IF (gadolin(2) < 0.0_r8k) gadolin(2:naxn) = gadolin(1)
    DO i = 1, naxn
        IF (gadolin(i) < 0.0_r8k) gadolin(i) = 0.0_r8k
    ENDDO
    
    rl = RodLength
    drod = RodDiameter
    RodFabTemp = tempcs
    IF (.NOT. unit) RodFabTemp = tfk(tempcs)
    cladod = drod
    rf = FuelPelDiam / 2.0_r8k
    cladid = FuelPelDiam + 2.0_r8k * gapthk
    roughf = MAX(1.0E-3_r8k, roughf)
    roughc = MAX(1.0E-3_r8k, roughc)
    nbotpl = 0
    IF (volbp > 0.0_r8k) nbotpl = 1
    ! Put gfrac into array GasFraction.
    ! Note: The arrays are not identical in terms of the same gases (gfrac doesn't include Nitrogen)
    ! Helium
    GasFraction(1) = gfrac(1)
    ! Argon
    GasFraction(2) = gfrac(2)
    ! Krypton
    GasFraction(3) = gfrac(3)
    ! Xenon
    GasFraction(4) = gfrac(4)
    ! Hydrogen
    GasFraction(5) = gfrac(5)
    ! Nitrogen - This is not an original input into FRAPTRAN but has been used in FRAPCON and was updated in FRAPCON-4.0.
    GasFraction(6) = gfrac(8)
    ! Air
    GasFraction(7) = gfrac(6)
    ! Steam
    GasFraction(8) = gfrac(7)
    
    ! Deviation from nominal fuel pellet radius (radpel(odd)) at elevation (radpel(even))
    icount = 1
    
    Count_radpel_Vals: DO
        IF ((icount + 1) > UBOUND(radpel,1)) EXIT Count_radpel_Vals
        IF (radpel(icount) == 0.0_r8k .AND. radpel(icount+1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(radpel,1)) EXIT Count_radpel_Vals
            IF (radpel(icount+2) == 0.0_r8k .AND. radpel(icount+3) == 0.0_r8k) EXIT Count_radpel_Vals
        END IF
        icount = icount + 2
    END DO Count_radpel_Vals
    !# of pairs
    nPelRadDeviat = (icount - 1) / 2
    
    ! Initial cladding permanent hoop strain (eppinp(odd)) relative to RodDiameter at elevation (eppinp(even))
    icount = 1
    
    Count_eppinp_Vals: DO
        IF ((icount + 1) > UBOUND(eppinp,1)) EXIT Count_eppinp_Vals
        IF (eppinp(icount) == 0.0_r8k .AND. eppinp(icount+1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(eppinp,1)) EXIT Count_eppinp_Vals
            IF (eppinp(icount+2) == 0.0_r8k .AND. eppinp(icount+3) == 0.0_r8k) EXIT Count_eppinp_Vals
        END IF
        icount = icount + 2
    END DO Count_eppinp_Vals
    !# of pairs
    nepp0 = (icount - 1) / 2
    
    ! Axial profile of cladding fast neutron flux (fluxz(odd)) at elevation (fluxz(even))
    icount = 1
    
    Count_fluxz_Vals: DO
        IF ((icount + 1) > UBOUND(fluxz,1)) EXIT Count_fluxz_Vals
        IF (fluxz(icount) == 0.0_r8k .AND. fluxz(icount+1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(fluxz,1)) EXIT Count_fluxz_Vals
            IF (fluxz(icount+2) == 0.0_r8k .AND. fluxz(icount+3) == 0.0_r8k) EXIT Count_fluxz_Vals
        END IF
        icount = icount + 2
    END DO Count_fluxz_Vals
    !# of pairs
    nfastf = (icount - 1) / 2
    
    ! Perform conversions
    rhof = frden * 684.214168_r8k
    fdens = frden * 10960.05959_r8k
    rsntr = 0.0_r8k
    bumtp = bup
    
    ! Weight percent PuO2
    compmt = frpo2! * 100.0_r8k
    
    ! Fuel type (either UO2 or MOX based on Pu content)
    IF (compmt > 0.0_r8k) THEN
        imox = 1
    ELSE
        imox = 0
    END IF
    
    ! Gas data
    GasMoles0 = gsms
    
    ! Check to see if gas fractions add to 1. If not, normalize.
    sumg = SUM(GasFraction(1:ngases))
    IF (ABS(sumg - 1.0_r8k) > 0.001_r8k) THEN
        WRITE(ounit,101)
101     FORMAT(/' WARNING: Gas fractions do not sum to zero. They will be normalized if possible.',/, &
          &     ' Otherwise, execution will be halted.')
        IF (sumg < 0.1_r8k) ERROR STOP 'Could not normalize gas fraction. Execution termianted in Subroutine: intinp'
        ! Normalize
        DO i = 1, ngases
            GasFraction(i) = GasFraction(i) / sumg
        ENDDO
    ENDIF
    
    ! List input
    
    ! Bundle Data
    WRITE(ounit,110) pitch ,Units(iu,1) ,pdrato ,rnbnt ,totnb
110 FORMAT(/10x,'Bundle data', &
      &    /15x,'Pitch                                                     = ',1pe13.4,a4, &
      &    /15x,'Pitch to diameter ratio                                   = ',1pe13.4, &
      &    /15x,'Ratio of balloonable to cells to total cells              = ',1pe13.4, &
      &    /15x,'Total number of cells in a bundle                         = ',1pe13.4/)
    
    ! Fuel rod data
    WRITE(ounit,115) rl, Units(iu,1), RodFabTemp, Units(iu,2), drod, Units(iu,1)
115 FORMAT(/10x,'Fuel rod data', &
      &    /15x,'Fuel stack cold length                                    = ',1pe13.4,a4, &
      &    /15x,'Rod fabrication temperature                               = ',1pe13.4,a3, &
      &    /15x,'Cold fuel rod outer diameter                              = ',1pe13.4,a4/)
    
    ! Pellet data
    WRITE(ounit,120) rshd, Units(iu,1), dishd, Units(iu,1), pelh, Units(iu,1), dishv0, Units(iu,3), FuelPelDiam, Units(iu,1), &
      &              roughf, frden, bup, frpo2, OpenPorosityFraction, tsntrk, fgrns, gadolin(1), fotmtl
120 FORMAT(/10x,'Pellet data', &
      &    /15x,'Cold state radius to pellet shoulder                      = ',1pe13.4,a4, &
      &    /15x,'Cold state pellet dish depth                              = ',1pe13.4,a4, &
      &    /15x,'Cold state pellet height                                  = ',1pe13.4,a4, &
      &    /15x,'Cold state pellet dish volume                             = ',1pe13.4,a7, &
      &    /15x,'Pellet outer diameter                                     = ',1pe13.4,a4, &
      &    /15x,'Pellet surface roughness                                  = ',1pe13.4,'(microns)', &
      &    /15x,'Pellet fraction of theoretical density                    = ',1pe13.4, &
      &    /15x,'Fuel burnup                                               = ',1pe13.4,'(MWs/kg)', &
      &    /15x,'PUO2 weight fraction                                      = ',1pe13.4, &
      &    /15x,'Fuel open porosity fraction                               = ',1pe13.4, &
      &    /15x,'Fuel sintering temperature                                = ',1pe13.4,'(K)', &
      &    /15x,'Fuel grain size                                           = ',1pe13.4,'(microns)', &
      &    /15x,'Weight fraction of Gd2O3(1)                               = ',1pe13.4, &
      &    /15x,'Ratio of fuel oxygen atoms to uranium and plutonium atoms = ',1pe13.4/)
      
    ! User-supplied pellet radius
    IF (nPelRadDeviat /= 0) WRITE(ounit,125) (Units(iu,1),j = 1,6), nPelRadDeviat, (radpel(i),i = 1,nPelRadDeviat * 2)
125 FORMAT(/10x,'Specified pellet radius deviations', &
      &    /15x,'No. of pairs',3x,3(5x,'Deviation',6x,'Elevation',1x), &
      &    /30x,3(5x,a4,11x,a4,6x), &
      &    /15x,i10,5x,6(2x,1pe13.4), &
      &    /(30x,6(2x,1pe13.4)))
    
    ! Convert fuel roughness from um to cm
    roughf = roughf / 1.0E4_r8k
    
    ! Cladding data
    WRITE(ounit,130)
130 FORMAT(/10x,'Cladding data')
    
    ! Write cladding type & set cladding density (lb/ft3)
    SELECT CASE (CladType)
    CASE (1)
        WRITE(ounit,401)
401     FORMAT(15x,'CladType = 1 entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (2)
        WRITE(ounit,402)
402     FORMAT(15x,'CladType = 2 (Zircaloy-2) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (3)
        WRITE(ounit,403)
403     FORMAT(15x,'CladType = 3 (Optimized ZIRLO) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (4)
        WRITE(ounit,404)
404     FORMAT(15x,'CladType = 4 (Zircaloy-4) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (5)
        WRITE(ounit,405)
405     FORMAT(15x,'CladType = 5 (ZIRLO) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (6)
        WRITE(ounit,406)
406     FORMAT(15x,'CladType = 6 (Zr-1%Nb) entered; will use RRC-KI Zr-1%Nb properties')
        rhoc = 408.05_r8k
        beta1 = 0.5_r8k
        deloxy = 0.0_r8k
    CASE (7)
        WRITE(ounit,407)
407     FORMAT(15x,'CladType = 7 (M5) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (8)
        WRITE(ounit,408)
408     FORMAT(15x,'CladType = 8 (E-110) entered; will use RRC-KI Zr-1%Nb properties')
        rhoc = 408.05_r8k
        beta1 = 0.5_r8k
        deloxy = 0.0_r8k
    CASE (9)
        WRITE(ounit,409)
409     FORMAT(15x,'CladType = 9 (low-tin Zry) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (10)
        WRITE(ounit,410)
410     FORMAT(15x,'CladType = 10 (cold-worked annealed SRA) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    CASE (11)
        WRITE(ounit,411)
411     FORMAT(15x,'CladType = 11 (fully annealed Zry) entered; will use generic Zircaloy properties')
        rhoc = 409.54_r8k
    END SELECT
    
    ! Cladding Data
    WRITE(ounit,160) cfluxa, tflux, gapthk, Units(iu,1), coldw, cldwdc, rhoc, roughc
160 FORMAT(15x,'Axially and time averaged fast flux for lifetime          = ',1pe13.4,'(nt/m**2-s)',/, &
      &    15x,'Time span of fast flux exposure                           = ',1pe13.4,'(s)',/, &
      &    15x,'Gas gap radial thickness                                  = ',1pe13.4,a4,/, &
      &    15x,'Strength cold work                                        = ',1pe13.4,/, &
      &    15x,'Ductility cold work                                       = ',1pe13.4,/, &
      &    15x,'Cladding density                                          = ',1pe13.4,'(lb/ft3)',/, &
      &    15x,'Inner surface roughness                                   = ',1pe13.4,'(microns)')
    ! Cladding thickness
    cladtk = (cladod - cladid) / 2.0_r8k
    WRITE(ounit,161) cladtk, Units(iu,1)
161 FORMAT(15x,'Cladding thickness                                        = ',1pe13.4,a4/)
    ! Convert cladding roughness from um to cm
    roughc = roughc / 1.0E4_r8k
    
    ! Cladding plastic hoop strain
    IF (nepp0 /= 0) WRITE(ounit,165) (Units(iu,1),j = 1,3), nepp0, (eppinp(i),i = 1,nepp0*2)
165 FORMAT(/10x,'Specified cladding plastic hoop strains', &
      &    /15x,'No. of pairs',3x,3(5x,'Hoop',11x,'Elevation',1x), &
      &    /30x,3(5x,'Strain',9x,a4,6x), &
      &    /15x,i10,5x,6(2x,1pe13.4), &
      &    /(30x,6(2x,1pe13.4)))
    
    ! Fast neutron axial flux
    IF (nfastf /= 0) WRITE(ounit,170) (Units(iu,1),j = 1,3), nfastf, (fluxz(i),i = 1,nfastf*2)
170 FORMAT(/10x,'Specified normalized fast neutron axial flux', &
      &    /15x,'No. of pairs',3x,3(5x,'Fast',11x,'Elevation',1x), &
      &    /30x,3(5x,'Flux',11x,a4,6x), &
      &    /15x,i10,5x,6(2x,1pe13.4), &
      &    /(30x,6(2x,1pe13.4)))
    
    ! Upper plenum Data
    WRITE(ounit,175) ncs,spl, Units(iu,1), scd, Units(iu,1), swd,Units(iu,1),vplen, Units(iu,3)
175 FORMAT(/10x,'Upper plenum data', &
      &    /15x,'Number of coils in spring                                 = ',i4, &
      &    /15x,'Uncompressed spring height                                = ',1pe13.4,a4, &
      &    /15x,'Uncompressed coil diameter                                = ',1pe13.4,a4, &
      &    /15x,'Spring wire diameter                                      = ',1pe13.4,a4, &
      &    /15x,'Total plenum volume                                       = ',1pe13.4,a7/)
    
    ! Lower plenum data (skip if nothing supplied in input file)
    IF (nbotpl /= 0) WRITE(ounit,176) ncolbp, splbp, Units(iu,1), coldbp, Units(iu,1), spdbp, Units(iu,1), volbp, Units(iu,3)
176 FORMAT(/10x,'Lower plenum data', &
      &    /15x,'Number of spring coils                                    = ',i3, &
      &    /15x,'Uncompressed spring height                                = ',1pe13.4,a4, &
      &    /15x,'Uncompressed coil outer diameter                          = ',1pe13.4,a4, &
      &    /15x,'Spring wire diameter                                      = ',1pe13.4,a4, &
      &    /15x,'Total plenum volume                                       = ',1pe13.4,a7/)
    
    ! Gas composition data
    WRITE(ounit,180) GasFraction
180 FORMAT(/10x,'Gas composition data',/, &
      &     15x,'Fraction of helium                                        = ',1pe13.4,/, &
      &     15x,'Fraction of argon                                         = ',1pe13.4,/, &
      &     15x,'Fraction of krypton                                       = ',1pe13.4,/, &
      &     15x,'Fraction of xenon                                         = ',1pe13.4,/, &
      &     15x,'Fraction of hydrogen                                      = ',1pe13.4,/, &
      &     15x,'Fraction of nitrogen                                      = ',1pe13.4,/, &
      &     15x,'Fraction of air                                           = ',1pe13.4,/, &
      &     15x,'Fraction of water vapor                                   = ',1pe13.4)
    IF (GasMoles0 <= 1.0E-10_r8k) WRITE(ounit,181) gappr0(1), Units(iu,5), tgas0, Units(iu,2)
181 FORMAT( 15x,'Cold state gas pressure                                   = ',1pe13.4,a8,/, &
      &     15x,'Cold state gas temperature                                = ',1pe13.4,a3)
    IF (GasMoles0 /= 0.0_r8k) WRITE(ounit,182) GasMoles0, gappr0(1), Units(iu,5)
182 FORMAT( 15x,'Amount of gas in rod                                      = ',1pe13.4,'(g-moles)',/, &
      &     15x,'Cold state gas pressure estimate                          = ',1pe13.4,a8)
    !
    END SUBROUTINE intinp
    !
    !
    !
    SUBROUTINE ioinp
    USE Kinds
    USE variables_fraptran, ONLY : ounit, iunit, unit, dtplta, npltn
    USE collct_h
    USE resti_h
    USE excb_h
    USE scalr_h
    IMPLICIT NONE
    !>@brief
    !> Subroutine to read in $ioData block
    !>@author
    !> Modified by I. Porter, NRC, April 2014 to clean coding and convert to .f90
    !
    INTEGER(ipk) :: icount, i, unitin, unitout, inp, res, pow, InputStat = 0
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$ioData input block'
    
    ! Define $ioData
    NAMELIST / ioData / unitin, unitout, trest, inp, res, pow, dtpoa, dtplta
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
    ! Set default values
    unitin = 0
    unitout = 0
    trest = 0.0_r8k
    inp = 0
    res = 0
    pow = 0
    
    ! Read $ioData
    READ (iunit, ioData, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'ioData')
    
    ! Set input file units
    IF (unitin == 0) THEN
        unit = .TRUE.
        iu = British
    ELSE
        unit = .FALSE.
        iu = SI
    ENDIF
    
    ! Set output file units
    IF (unitout == 1) THEN
        luout = 1
    ELSE
        luout = unitin
    END IF
    
    ! Output interval of output file
    icount = 1
    
    Count_Output_Intervals: DO
        IF ((icount + 1) > UBOUND(dtpoa,1)) EXIT Count_Output_Intervals
        IF (dtpoa(icount) == 0.0_r8k .AND. dtpoa(icount+1) == 0.0_r8k) EXIT Count_Output_Intervals
        icount = icount + 2
    END DO Count_Output_Intervals
    ! # of pairs
    npdtpo = (icount - 1) / 2
    IF (npdtpo == 1) dtpo = dtpoa(1)
    
    ! Output interval of plot file
    icount = 1
    
    Count_Plot_Intervals: DO
        IF ((icount + 1) > UBOUND(dtplta,1)) EXIT Count_Plot_Intervals
        IF (dtplta(icount) == 0.0_r8k .AND. dtplta(icount+1) == 0.0_r8k) EXIT Count_Plot_Intervals
        icount = icount + 2
    END DO Count_Plot_Intervals
    ! # of pairs
    npltn = 0
    npltn = (icount - 1) / 2
    IF (npltn >= 1) ntplot = 1
    
    ! Initialization from FRAPCON restart file
    NFrapconInitialization = inp
    
    ! Check to see if FRAPTRAN restart file is to be created
    IF (res == 1) THEN
        nswinw = 2
    ELSE
        nswinw = 0
    END IF
    
    ! Check input
    
    ! Check to make sure dtpoa(even) is increasing
    IF (npdtpo >= 2) THEN
        DO i = 2, npdtpo
            IF (dtpoa(2*i) < dtpoa(2*(i-1))) THEN
                WRITE(ounit,101)
101             FORMAT(/,'Print time values (dtpoa) are not increasing. Execution terminated in Subroutine: ioinp')
                ERROR STOP 'Print time values (dtpoa) are not increasing. Execution terminated in Subroutine: ioinp'
            ENDIF
        ENDDO
    ENDIF
    
    ! Check to make sure that dtpo is positive
    IF (npdtpo == 1 .AND. dtpo <= 0.0_r8k) THEN
        WRITE(ounit,102) dtpo
102     FORMAT(/,'Print interval (dtpoa) must be positive. dtpo = ',f10.4,' Execution terminated in Subroutine: ioinp')
        ERROR STOP 'Print interval (dtpoa) must be positive. Execution terminated in Subroutine: ioinp'
    ENDIF
    
    ! Check to make sure dtplta(even) is increasing
    IF (npltn >= 2) THEN
        DO i = 2, npltn
            IF (dtplta(2*i) < dtplta(2*(i-1))) THEN
                WRITE(ounit,103)
103             FORMAT(/,'Plot time values (dtplta) are not increasing. Execution terminated in Subroutine: ioinp')
                ERROR STOP 'Plot time values (dtplta) are not increasing. Execution terminated in Subroutine: ioinp'
            ENDIF
        ENDDO
    ENDIF
    
    ! Check that dtplta(1) is positive
    IF (npltn == 1 .AND. dtplta(1) <= 0.0_r8k) THEN
        WRITE(ounit,104) dtplta(1)
104     FORMAT(/,'Plot interval (dtplta) must be positive. dtplta(1) = ',f10.4,' Execution terminated in Subroutine: ioinp')
        ERROR STOP 'Plot interval (dtplta) must be positive. Execution terminated in Subroutine: ioinp'
    ENDIF
    
    ! Input options
    WRITE(ounit,205)
205 FORMAT(/10x,'Input options selected')
    IF (unit) THEN
        WRITE(ounit,206)
206     FORMAT(15x,'British units')
    ELSE
        WRITE(ounit,207)
207     FORMAT(15x,'SI units')
    END IF
    
    ! Check if reading FRAPCON restart file
    IF (NFrapconInitialization == 1) WRITE(ounit,208) trest
208 FORMAT(/10x,'FRAPCON tape will be used as input for fuel rod conditions at time = ',e12.4, ' seconds')
    
    ! Output options
    WRITE(ounit,220)
220 FORMAT(/10x,'Output options selected')
    IF (luout == 1) THEN
        WRITE(ounit,221)
221     FORMAT(15x,'Output in SI units'/)
    ELSE
        WRITE(ounit,222)
222     FORMAT(15x,'Output in British units'/)
    END IF
    
    ! Output file printing interval
    SELECT CASE (npdtpo)
    CASE (0)
        WRITE(ounit,223)
223     FORMAT(15x,'Print interval will default, see default summary page')
    CASE (1)
        WRITE(ounit,224) dtpo
224     FORMAT(15x,'Print interval is constant at ',f10.4,' seconds')
    CASE DEFAULT
        WRITE(ounit,225) npdtpo,(dtpoa(i),i = 1,2*npdtpo)
225     FORMAT(/10x,'Printout interval specification', &
          &    /15x,'No. of pairs',3x,3(5x,'Print',10x,'Problem',3x), &
          &    /30x,3(5x,'Interval',7x,'Time',6x), &
          &    /30x,3(5x,'(s)',12x,'(s)',7x), &
          &    /15x,i10,5x,6(2x,1pe13.4), &
          &    /(30x,6(2x,1pe13.4)))
        WRITE(ounit,'(/)')
    END SELECT
    
    ! Plot file printing interval
    SELECT CASE (ntplot)
    CASE (0)
        WRITE(ounit,230)
230     FORMAT(/10x,'No plot file request.')
    CASE (1)
        WRITE(ounit,231)
231     FORMAT(/10x,'Plot output file requested')
        IF (npltn == 0) THEN
            WRITE(ounit,232)
232         FORMAT(/10x,'Plot interval will default, see default summary page')
        ELSE IF (npltn >= 2) THEN
            WRITE(ounit,233) npltn,(dtplta(i),i = 1,2*npltn)
233         FORMAT(/10x,'Plot interval specification', &
              &     /15x,'No. of pairs',3x,3(5x,'Plot',11x,'Problem',3x), &
              &     /30x,3(5x,'Interval',7x,'Time',6x), &
              &     /30x,3(5x,'(s)',12x,'(s)',7x), &
              &     /15x,i10,5x,6(2x,1pe13.4), &
              &     /(30x,6(2x,1pe13.4)))
            WRITE(ounit,'(/)')
        END IF
    END SELECT
    
    ! FRAPTRAN restart ouput file
    IF (nswinw == 2) WRITE(ounit,240)
240 FORMAT(/10x,'Restart output file requested')
    
    !
    END SUBROUTINE ioinp
    !
    !
    !
    SUBROUTINE modinp
    USE Kinds
    USE conversions_fraptran
    USE variables_fraptran, ONLY : ounit, iunit, tref, nthermex, unit, swllfr, gasphs, gasths, prestmp, cexh2a, protectiveoxide, &
      &                   nidoxide, explenumv, explenumt, npair, CladType
    USE resti_h
    USE excb_h
    USE scalr_h
    USE FissionGasRelease_h
    USE modcom_h
    USE Dyna_h, ONLY : oxideid, oxideod
    IMPLICIT NONE
    !>@brief
    !> Subroutine to read $model block
    !>@author
    !> Modified by I. Porter, NRC, April 2014 to clean coding and convert to .f90
    !
    INTEGER(ipk) :: nsym, gasflo, grass, prescri, idoxid, odoxid, cathca, baker, noball, cenvoi, i, rtheta, &
      &             PlenumTemp, unitin, icount, InputStat = 0
    REAL(r8k) :: buoxide, fastfluence
    CHARACTER(LEN=3) :: internal = 'OFF'
    CHARACTER(LEN=3) :: metal = 'OFF'
    CHARACTER(LEN=3) :: deformation = 'OFF'
    CHARACTER(LEN=3) :: heat = 'OFF'
    CHARACTER(LEN=10) :: inst = 'OFF'
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$model selection input block'
    CHARACTER(LEN=10), DIMENSION(2,3), PARAMETER :: Units = reshape([ &
      &          '(ft)      ', '(m)       ', &
      &          '(F)       ', '(K)       ', &
      &          '(psi)     ', '(Pa)      ' ], [2, 3])
    
    ! Define $model
    NAMELIST / model / internal, metal, deformation, heat, inst, nsym, naz, gasphs, oxideid, &
      &                oxideod, zvoid1, zvoid2, rvoid, dofset, dofang, cexh2a, gasflo, grass, prescri, &
      &                idoxid, odoxid, cathca, baker, noball, cenvoi, rtheta, presfgr, relfraca, tref, &
      &                TranSwell, FuelGasSwell, PlenumTemp, nthermex, ProtectiveOxide, frcoef, mechan, &
      &                irupt, ruptstrain, irefine, refine, nIDoxide, BuOxide, explenumv, explenumt, &
      &                iStoicGrad, prestmp, gasths, ngastmp, trise, relocmodel
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
    ! Set default values
    gasphs = 0.0_r8k
    gasths = 0.0_r8k
    cexh2a = 0.0_r8k
    relfraca = 0.0_r8k
    oxideod = 3.0e-6_r8k
    oxideid = 3.0e-6_r8k
    nsym = 0
    naz = 0
    cathca = 0
    iStoicGrad = 0
    baker = 0 
    ProtectiveOxide = 0
    zvoid1 = 0.0_r8k
    zvoid2 = 0.0_r8k
    rvoid = 0.0_r8k
    dofset = 0.0_r8k
    dofang = 0.0_r8k
    gasflo = 0
    grass = 0
    prescri = 0
    prestmp = 0
    idoxid = 0
    odoxid = 0
    noball = 0
    cenvoi = 0
    rtheta = 0
    TranSwell = 0
    presfgr = 0
    PlenumTemp = 0
    nthermex = 0
    nIDoxide = 0
    BuOxide = 0.0_r8k
    explenumv = 0.0_r8k
    frcoef = 0.015_r8k
    mechan = 2
    irupt = 1
    ruptstrain = 1.0_r8k
    irefine = 1
    refine = 3.0_r8k
    
    ! Set default temperatures based on input units
    IF (.NOT. unit) THEN
        trise = 10.0_r8k / 1.8_r8k
        tref = 298.15_r8k
        explenumt(1) = 298.15_r8k
        explenumt(2) = 0.0_r8k
    ELSE
        trise = 10.0_r8k
        tref = 77.0_r8k
        explenumt(1) = 77.0_r8k
        explenumt(2) = 0.0_r8k
    ENDIF
    
    ! Read $model
    READ (iunit, model, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'model')
    
    ! Ensure all input option characters are in uppercase
    deformation = TO_UPPERCASE(deformation)
    heat = TO_UPPERCASE(heat)
    inst = TO_UPPERCASE(inst)
    internal = TO_UPPERCASE(internal)
    metal = TO_UPPERCASE(metal)
    relocmodel = TO_UPPERCASE(relocmodel)
    
    ! Perform unit conversions if necessary
    IF (.NOT. unit) THEN
        tref = (tref - 273.15_r8k) * 1.8_r8k + 32.0_r8k
        explenumv = explenumv * (3.2808_r8k ** 3)
        trise = trise * 1.8_r8k
        i = 1
        DO WHILE (explenumt(i+1) >= 0.0_r8k)
            explenumt(i) = (explenumt(i) - 273.15_r8k) * 1.8_r8k + 32.0_r8k
            i = i + 2
        ENDDO
    ENDIF
    
    ! Determine the # of pairs for user supplied plenum temperature
    i = 2
    DO WHILE (explenumt(i) >= 0.0_r8k)
        i = i + 2
        npair = npair + 1
    ENDDO
    tempcs = tref
    
    ! Deformation models
    IF (heat == 'ON') THEN
        IF (cenvoi == 1) THEN
            IF (inst == 'INSTRUMENT') THEN
                nvoid = 2
            ELSE
                nvoid = 1
            ENDIF
        ELSE
             rvoid  = 0.0_r8k
             zvoid1 = 0.0_r8k
             zvoid2 = 0.0_r8k
        ENDIF
    ELSE
         rvoid  = 0.0_r8k
         zvoid1 = 0.0_r8k
         zvoid2 = 0.0_r8k  
    ENDIF
    
    ! Option to specify 1 or more of the following suboptions
    IF (deformation == 'ON') THEN
        
        ! Turn on ballooning
        IF (noball == 1) nbalsw = 1
        
        ! Turn on user-supplied fuel swelling vs. time
        IF (TranSwell == 1) THEN
            
            ! Count the number of user-supplied fuel swelling history pairs
            icount = 1
            
            Count_FuelGasSwell_Vals: DO
                IF ((icount + 1) > UBOUND(FuelGasSwell,1)) EXIT Count_FuelGasSwell_Vals
                IF (FuelGasSwell(icount) == 0.0_r8k .AND. FuelGasSwell(icount+1) == 0.0_r8k) THEN
                    ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
                    IF ((icount + 3) > UBOUND(FuelGasSwell,1)) EXIT Count_FuelGasSwell_Vals
                    IF (FuelGasSwell(icount+2) == 0.0_r8k .AND. FuelGasSwell(icount+3) == 0.0_r8k) EXIT Count_FuelGasSwell_Vals
                END IF
                icount = icount + 2
            END DO Count_FuelGasSwell_Vals
            !# of pairs
            nFuelSwellPairs = (icount - 1) / 2
        ENDIF
    ENDIF
    
    ! Rod internal gas pressure models
    IF (internal == 'ON') THEN
        ! Plenum temperature model
        IF (PlenumTemp == 1) nplnt = 1
        ! Transient gas flow between plenum and gas-gap
        IF (gasflo == 1) swllfr = 1.0_r8k
        ! User defined internal rod pressure history
        IF (prescri == 1) THEN
            ! Count the number of user-supplied internal rod gas pressure history pairs
            icount = 1
            
            Count_gasphs_Vals: DO
                IF ((icount + 1) > UBOUND(gasphs,1)) EXIT Count_gasphs_Vals
                IF (gasphs(icount) == 0.0_r8k .AND. gasphs(icount+1) == 0.0_r8k) THEN
                    ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
                    IF ((icount + 3) > UBOUND(gasphs,1)) EXIT Count_gasphs_Vals
                    IF (gasphs(icount+2) == 0.0_r8k .AND. gasphs(icount+3) == 0.0_r8k) EXIT Count_gasphs_Vals
                END IF
                icount = icount + 2
            END DO Count_gasphs_Vals
            !# of pairs
            ngaspr = (icount - 1) / 2
        ENDIF
        
        SELECT CASE (prestmp)
        CASE (1, 2, 3)
            ! User supplies plenum temperatures vs time
            IF (prestmp == 1 .OR. prestmp == 3) THEN
                ! User supplies the plenum temperature vs time for the upper plenum
                icount = 1
            
                Count_gasths_UpperPlenum_Vals: DO
                    IF ((icount + 1) > UBOUND(gasths(:,1),1)) EXIT Count_gasths_UpperPlenum_Vals
                    IF (gasths(icount,1) == 0.0_r8k .AND. gasths(icount+1,1) == 0.0_r8k) THEN
                        ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
                        IF ((icount + 3) > UBOUND(gasths(:,1),1)) EXIT Count_gasths_UpperPlenum_Vals
                        IF (gasths(icount+2,1) == 0.0_r8k .AND. gasths(icount+3,1) == 0.0_r8k) EXIT Count_gasths_UpperPlenum_Vals
                    END IF
                    icount = icount + 2
                END DO Count_gasths_UpperPlenum_Vals
                !# of pairs
                ngastmp(1) = (icount - 1) / 2
            
            END IF
        
            IF (prestmp == 2 .OR. prestmp == 3) THEN
                ! User supplies the plenum temperature vs time for the lower plenum
                icount = 1
                
                Count_gasths_LowerPlenum_Vals: DO
                    IF ((icount + 1) > UBOUND(gasths(:,2),1)) EXIT Count_gasths_LowerPlenum_Vals
                    IF (gasths(icount,2) == 0.0_r8k .AND. gasths(icount+1,2) == 0.0_r8k) THEN
                        ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
                        IF ((icount + 3) > UBOUND(gasths(:,2),1)) EXIT Count_gasths_LowerPlenum_Vals
                        IF (gasths(icount+2,2) == 0.0_r8k .AND. gasths(icount+3,2) == 0.0_r8k) EXIT Count_gasths_LowerPlenum_Vals
                    END IF
                    icount = icount + 2
                END DO Count_gasths_LowerPlenum_Vals
                !# of pairs
                ngastmp(2) = (icount - 1) / 2
            
            END IF
            
        CASE DEFAULT
            ! User does not supply the plenum temperature vs time
            ngastmp = 0
        END SELECT
        
        ! Fission gas release
        SELECT CASE (presfgr)
        CASE (0)
            ! No FGR model will be used
        CASE (1)
            ! User supplied FGR as a function of time
            icount = 1
            
            Count_relfraca_Vals: DO
                IF ((icount + 1) > UBOUND(relfraca,1)) EXIT Count_relfraca_Vals
                IF (relfraca(icount) == 0.0_r8k .AND. relfraca(icount+1) == 0.0_r8k) THEN
                    ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
                    IF ((icount + 3) > UBOUND(relfraca,1)) EXIT Count_relfraca_Vals
                    IF (relfraca(icount+2) == 0.0_r8k .AND. relfraca(icount+3) == 0.0_r8k) EXIT Count_relfraca_Vals
                END IF
                icount = icount + 2
            END DO Count_relfraca_Vals
            !# of pairs
            nFGRpairs = (icount - 1) / 2

        CASE (2)
            ! Use FRAPFGR model, reading from FRAPCON restart file
        END SELECT
        
    ENDIF
    
    ! Metal water reaction modeling
    IF (metal == 'ON') THEN
        IF (cathca == 1) THEN
            modmw = 0
        ELSE IF (baker == 1) THEN
            modmw = 2
        ELSE IF (cathca == 1 .AND. baker == 1) THEN
            WRITE (ounit,104)
104         FORMAT ('Both catcha and baker models requested. Only 1 may be chosen. Execution terminated in Subroutine: modinp')
            ERROR STOP 'Both catcha and baker models requested. Only 1 may be chosen. Execution terminated in Subroutine: modinp'
        ELSE
            WRITE (ounit,105)
            WRITE (0,105)
105         FORMAT ("Warning: input flag metal='on' but both catcha and baker = 0. Will use values for oxide thickness and ", &
              &     "hydrogen concentration, but will not perform high temperature oxidation calculations.")
            modmw = 1
        END IF
    ELSE
        ! No metal-water reaction to be modeled
        modmw = 1
    ENDIF
    
    ! Fuel relocation model
    SELECT CASE (relocmodel)
    CASE ('FRAPCON-3.3')
        WRITE (ounit, 110)
110     FORMAT(/10x,'FRAPCON-3.3 fuel relocation model will be used')
    CASE ('FRAPCON-3.4', 'FRAPCON-3.4A')
        WRITE (ounit, 111)
111     FORMAT(/10x,'FRAPCON-3.4 fuel relocation model will be used')
        IF (relocmodel == 'FRAPCON-3.4A') relocmodel = 'FRAPCON-3.4'
    CASE ('FRAPCON-3.5')
        WRITE (ounit, 112)
112     FORMAT(/10x,'FRAPCON-3.5 fuel relocation model will be used')
    CASE ('OFF')
        WRITE (*, 113)
        WRITE (ounit, 113)
113     FORMAT(/10x,'Fuel relocation is turned off')
    CASE DEFAULT
        relocmodel = 'FRAPCON-3.5'
        WRITE (ounit, 115) relocmodel
115     FORMAT(/10x,'Bad input for relocmodel. relocmodel = ')
        WRITE (ounit, 112)
    END SELECT
    
    ! Selected yield stress/strength model
    IF (CladType == 6 .OR. CladType == 8) THEN
        ! Set fast fluence correlation used
        IF ((cfluxa * tflux) < 1.0e25_r8k) THEN
            WRITE(ounit,130)
130         FORMAT(/10x,'Zr-1%Nb, non-irradiated, yield strength calculated using n, m, K coefficients as specified by RRC-KI')
        ELSE
            WRITE(ounit,131)
131         FORMAT(/10x,'Zr-1%Nb, irradiated, yield strength calculated using n, m, K coefficients as specified by RRC-KI')
        END IF
    END IF
    
    ! Thermal expansion model being used
    SELECT CASE (nthermex)
    CASE (0)
        WRITE(ounit,140)
140     FORMAT(/10x,'Thermal Expansion Calculated With Original Radial Free Thermal Expansion Model')
    CASE (1)
        WRITE(ounit,141)
141     FORMAT(/10x,'Thermal Expansion Calculated With Maximum of Circumferential and Radial Thermal Expansion Models')
    CASE DEFAULT
        WRITE(ounit,*) nthermex
145     FORMAT(/10x,'Bad input for fuel thermal expansion model. Will use default radial free thermal expansion model. ',&
          &         'nthermex =',i3)
        nthermex = 0
    END SELECT
    
    ! User specified fission gas release history
    WRITE(ounit,150)
150 FORMAT(1x)
    IF (presfgr == 1) THEN
        WRITE(ounit,151)
151     FORMAT(/10x,'User specified fission gas release history', &
          &    /15x,'Release fraction',5x,'Time(s)')
        DO i = 1, nFGRpairs
            WRITE(ounit,152) relfraca(2*i-1), relfraca(2*i)
152         FORMAT(10x,2x,1pe13.4,6x,2x,1pe13.4)
        END DO
    END IF
    
    ! Central void models
    SELECT CASE (nvoid)
    CASE (0)
        WRITE(ounit,203)
203     FORMAT(/10x,'No central void modeled.')
    CASE (1)
        WRITE(ounit,204)
        WRITE(ounit,205) zvoid1, Units(iu,1), zvoid2, Units(iu,1), rvoid, Units(iu,1)
205     FORMAT(/10x,'Empty central void', &
          &    /15x,'Elevation of void bottom  = ',e13.6,a4, &
          &    /15x,'Elevation of void top     = ',e13.6,a4, &
          &    /15x,'Radius of void            = ',e13.6,a4)
    CASE (2)
        WRITE(ounit,204)
        WRITE(ounit,206) zvoid1, Units(iu,1), zvoid2, Units(iu,1), rvoid, Units(iu,1)
206     FORMAT(/10x,'Central void with instrumentation', &
          &    /15x,'Elevation of void bottom  = ',e13.6,a4, &
          &    /15x,'Elevation of void top     = ',e13.6,a4, &
          &    /15x,'Radius of void            = ',e13.6,a4)
    CASE DEFAULT
        WRITE(ounit,208) nvoid
208     FORMAT(/10x,'Bad input for central void in Subroutine: modinp. Will assume no void exists. nvoid = ',i3)
    END SELECT
204 FORMAT(/10x,'Heat conduction options selected ')
    
    ! Deformation models
    WRITE(ounit,215)
215 FORMAT(/10x,'Deformation options selected')
     
    ! Fracas-1 option (only option)
    WRITE(ounit,220)
220 FORMAT(15x,'Fracas-1 rigid pellet deformation model')
    
    ! No balloon option
    IF (nbalsw == 1) WRITE(ounit,230)
230 FORMAT(15x,'No balloon option selected')
    
    ! User specified transient fuel swelling history
    WRITE(ounit,240)
240 FORMAT(1x)
    IF (TranSwell == 1) THEN
        WRITE(ounit,241)
241     FORMAT(/10x,'User specified transient gaseous fission product fuel swelling history', &
          &    /15x,'Fractional fuel radius change',4x,'Time(s)')
        DO i = 1, nFuelSwellPairs
            WRITE(ounit,245) FuelGasSwell(2*i-1), FuelGasSwell(2*i)
245         FORMAT(10x,2x,1pe13.4,20x,1pe13.4)
        ENDDO
    ENDIF
    
    ! Internal gas models
    WRITE(ounit,510)
510 FORMAT(/10x,'Internal gas pressure model with:')
    SELECT CASE (nplnt)
    CASE (0)
        WRITE(ounit,511)
511     FORMAT(15x,'Plenum gas temperature set at coolant temperature + 10 deg F.')
    CASE (1)
        WRITE(ounit,512)
512     FORMAT(15x,'Plenum gas temperatures calculated from plenum temperature model')
    END SELECT
    IF (gasflo == 1) WRITE(ounit,515)
515 FORMAT(15x,'Gas flow between plenum and gap included')
    
    ! Metal water reaction modeling
    WRITE(ounit,300)
300 FORMAT(/10x,'Metal Water Reaction Modelling')
    ! Cladding ID Oxidation
    WRITE(ounit,301) (i, oxideid(i),i = 1,naxn)
301 FORMAT(/15x,'Axial Node     Initial values of cladding inside surface oxide thickness (m)',/, &
      &    (15x,6x,i4,2x,1pe13.4))
    ! Cladding OD Oxidation
    WRITE(ounit,302) (i, oxideod(i),i = 1,naxn)
302 FORMAT(/15x,'Axial Node     Initial values of cladding outside surface oxide thickness (m)',/, &
      &    (15x,6x,i4,2x,1pe13.4))
    ! Cladding hydrogen content
    WRITE(ounit,303) (i, cexh2a(i),i = 1,naxn)
303 FORMAT(/15x,'Axial Node     Initial values of cladding total H2 (ppm)',/, &
      &    (15x,6x,i4,2x,1pe13.4))
    
    ! Oxidation Model
    SELECT CASE (modmw)
    CASE (0) ! Cathcart model
        IF (iStoicGrad == 0) THEN
            WRITE(ounit,310)
310         FORMAT(/15x,' Metal water reaction modeled using Cathcart-Pawel model with: Perfect stoichiometry assumption')
        ELSE
            WRITE(ounit,311)
311         FORMAT(/15x,' Metal water reaction modeled using Cathcart-Pawel model with: Stoichiometry gradient assumption')
        ENDIF
    CASE (1) ! Not modeled
        WRITE(ounit,315)
315     FORMAT(/15x,' Metal water reaction NOT modeled')
    CASE (2) ! Baker-Just model
        WRITE(ounit,312)
312     FORMAT(/15x,' Metal water reaction modeled using Baker-Just model')
    END SELECT
    
    ! Internal rod pressure prescribed by input file
    IF (prescri == 1) THEN
        WRITE(ounit,330)
330     FORMAT(/10x,'Internal gas pressure history prescribed')
        WRITE(ounit,331)
331     FORMAT(15x,'Time(s)',8x,'Gas pressure')
        DO i = 1, ngaspr
            WRITE(ounit,332) gasphs(2*i),gasphs(2*i-1)
332         FORMAT(12x,1pe13.4,2x,1pe13.4)
        ENDDO
    ENDIF
    
    ! Upper plenum temperature history prescribed by input file
    IF (prestmp == 1 .OR. prestmp == 3) THEN
        WRITE(ounit,340)
340     FORMAT(/10x,'Upper plenum temperature history prescribed')
        WRITE(ounit,341)
341     FORMAT(15x,'Time(s)',8x,'Plenum temp')
        DO i = 1, ngastmp(1)
            WRITE(ounit,342) gasths(2*i,1),gasths(2*i-1,1)
342         FORMAT(12x,1pe13.4,2x,1pe13.4)
        ENDDO
    ENDIF
    
    ! Lower plenum temperature history prescribed by input file
    IF (prestmp == 2 .OR. prestmp == 3) THEN
        WRITE(ounit,345)
345     FORMAT(/10x,'Lower plenum temperature history prescribed')
        WRITE(ounit,346)
346     FORMAT(15x,'Time(s)',8x,'Plenum Temp')
        DO i = 1, ngastmp(2)
            WRITE(ounit,347) gasths(2*i+1,2),gasths(2*i-1,2)
347         FORMAT(12x,1pe13.4,2x,1pe13.4)
        ENDDO
    ENDIF
    !
    WRITE(ounit,'(/)')
    !
    END SUBROUTINE modinp
    !
    !
    !
    SUBROUTINE numinp
    USE Kinds
    USE variables_fraptran, ONLY : ounit, iunit, unit, zelev, nfmesh, ncmesh, fmesh, cmesh, idebug, maxit, noiter, nunopt
    USE Dyna_h
    USE collct_h
    USE resti_h
    USE excb_h
    USE scalr_h
    USE heatconduction_h
    IMPLICIT NONE
    !>@brief
    !> Subroutine to read in $solution block
    !>@author
    !> Modified by I. Porter, NRC, March 2014 to clean coding and convert to .f90
    !
    INTEGER(ipk) :: icount, i, j, soltyp, InputStat = 0
    CHARACTER(LEN=4), DIMENSION(2,2), PARAMETER :: Units = &
    &                                              reshape([ '(F) ', '(K) ', '(ft)', '(m) ' ], [2, 2])
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$solution control definition input block'
    
    ! Define $solution
    NAMELIST / solution / dtmaxa, dtss, prsacc, tmpac1, soltyp, maxit, noiter, epsht1, &
      &                   naxn, zelev, nfmesh, ncmesh, fmesh, cmesh, nce
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
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
    idebug = 0
    
    ! Read $solution
    READ (iunit, solution, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'solution')
    
    ! Set options based on input
    nkf = 100
    nkc = 50
    IF (unit) THEN
        t0f = 62.33_r8k
        tmaxf = 5840.33_r8k
        t0c = 62.33_r8k
        tmaxc = 4040.33_r8k
    ELSE
        t0f = 290.0_r8k
        tmaxf = 3500.0_r8k
        t0c = 290.0_r8k
        tmaxc = 2500.0_r8k
    ENDIF
        
    ! Problem time-step history
    icount = 1
    
    Count_ProblemTime_Intervals: DO
        IF ((icount + 1) > UBOUND(dtmaxa,1)) EXIT Count_ProblemTime_Intervals
        IF (dtmaxa(icount) == 0.0_r8k .AND. dtmaxa(icount+1) == 0.0_r8k) EXIT Count_ProblemTime_Intervals
        icount = icount + 2
    END DO Count_ProblemTime_Intervals
    ! # of pairs
    ndtmax = (icount - 1) / 2
    
    IF (dtmaxa(3) <= 0.0_r8k) THEN
        dtmaxa(3) = dtmaxa(1)
        dtmaxa(4) = 1.0e10_r8k
    ENDIF
    
    ! Maximum fractional change in internal fuel rod pressure between two successive iterations for convergence
    IF (prsacc < 1.0e-20_r8k) prsacc = 1.0e-03_r8k
    ! Maximum fractional change in temperature at any radial node between two successive iterations for convergence
    IF (tmpac1 < 1.0e-20_r8k) tmpac1 = 1.0e-03_r8k
    
    ! Force explicit solution
    IF (prsacc >= 1.0_r8k) THEN
        prsacc = 1.0e20_r8k
        tmpac1 = 1.0e20_r8k
    ENDIF
    
    ! Option to specify an explicit solution
    IF (soltyp == 1) THEN
        prsacc = 1.0e20_r8k
        tmpac1 = 1.0e20_r8k
    ENDIF
    
    ! User-input elevation of axial nodes (above the bottom of the rod).
    ! Input location is the axial midpoint of each axial node. (Can't start with a value of 0.0!)
    icount = 1
    
    IF (naxn == 0) THEN
        nunopt(11) = 2
        IF (zelev(1) == 0.0_r8k) THEN
            WRITE(0,90)
90          FORMAT ("Error: Can't input a value of 0.0 for first axial elevation in array zelev. Must input axial node MIDPOINT.", &
              &     "Execution terminated in Soubroutine: numinp")
            ERROR STOP "Error: Can't input a value of 0.0 for first axial elevation in array zelev. Must input axial node MIDPOINT"
        END IF
        
        Count_zelev_Vals: DO
            IF ((icount) > UBOUND(zelev,1)) EXIT Count_zelev_Vals
            IF (zelev(icount) == 0.0_r8k) EXIT Count_zelev_Vals
            icount = icount + 1
        END DO Count_zelev_Vals
        !# of pairs
        naxn = icount - 1
        
    ELSE
        nunopt(11) = 1
    END IF
    
    ! User-input radial fuel nodes. Values are the radius of each radial node. Start at 0.0.
    ! The last input radius must equal the fuel pellet radius and account for any dimensional changes (e.g. swelling)
    icount = 2
    
    IF (nfmesh == 0) THEN
        nunopt(12) = 2
        Count_fmesh_Vals: DO
            IF ((icount) > UBOUND(fmesh,1)) EXIT Count_fmesh_Vals
            IF (fmesh(icount) == 0.0_r8k) EXIT Count_fmesh_Vals
            icount = icount + 1
        END DO Count_fmesh_Vals
        !# of pairs
        nfmesh = icount - 1
    ELSE
        nunopt(12) = 1
    END IF
    
    ! User-input radial cladding nodes. Values are the radius of each radial node.
    ! The last input radius must equal the cladding outer radius
    icount = 1
    
    IF (ncmesh == 0) THEN
        nunopt(13) = 2
        Count_cmesh_Vals: DO
            IF ((icount) > UBOUND(cmesh,1)) EXIT Count_cmesh_Vals
            IF (cmesh(icount) == 0.0_r8k) EXIT Count_cmesh_Vals
            icount = icount + 1
        END DO Count_cmesh_Vals
        !# of pairs
        ncmesh = icount - 1
    ELSE
        nunopt(13) = 1
    END IF
    
    ! Check input for errors
    
    ! Make sure problem time (dtmaxa(even)) is increasing
    IF (ndtmax >= 2) THEN
        DO i = 2, ndtmax
            IF (dtmaxa(2*i) < dtmaxa(2*(i-1))) THEN
                WRITE(ounit,101)
101             FORMAT(/,' Time step time values (dtmaxa) are not in increasing order. Execution terminated in Subroutine: numinp')
                ERROR STOP 'Time step time values (dtmaxa) are not in increasing order. Execution terminated in Subroutine: numinp'
            END IF
        ENDDO
    ENDIF
    
    ! Make sure axial nodes (zelev) are increasing
    IF (zelev(1) > 0.0_r8k .AND. naxn > 1) THEN
        DO i = 2, naxn
            IF (zelev(i) < zelev(i-1)) THEN
                WRITE(ounit,102)
102             FORMAT(/,'User specified axial node values (zelev) are not increasing. Execution terminated in Subroutine: numinp')
                ERROR STOP 'User specified axial node values (zelev) are not increasing. Execution terminated in Sub: numinp'
            ENDIF
        ENDDO
    ENDIF
    
    ! Make sure fuel radial nodes (fmesh) are increasing
    IF (fmesh(2) > 0.0_r8k) THEN
        DO i = 2, nfmesh
            IF (fmesh(i) < fmesh(i-1)) THEN
                WRITE(ounit,103)
103             FORMAT(/,' User specified fuel radial node values (fmesh) are not in increasing')
                ERROR STOP
            ENDIF
        ENDDO
    ENDIF
    
    ! Make sure cladding radial nodes (cmesh) are increasing
    IF (cmesh(1) > 0.0_r8k .AND. ncmesh > 1) THEN
        DO i = 2, ncmesh
            IF (cmesh(i) < cmesh(i-1)) THEN
                WRITE(ounit,104)
104             FORMAT(/,' User specified cladding radial node values (cmesh) are not in increasing.', &
                  &      ' Execution terminated in Subroutine: numinp')
                ERROR STOP 'Cladding radial nodes (cmesh) not increasing. Execution terminated in Subroutine: numinp'
            ENDIF
        ENDDO
    ENDIF
    
    ! List out options selected
    
    ! Time control option
    WRITE(ounit,250)
250 FORMAT(/10x,'Time control options selected')
    
    ! Time dependent time step sizes
    IF (ndtmax == 1) THEN
        WRITE(ounit,252) dtmaxa(1)
252     FORMAT(/15x,'Time step is: ',1pe13.4)
    ELSE IF (ndtmax >= 2) THEN
        WRITE(ounit,251) ndtmax, (dtmaxa(i),i = 1,2*ndtmax)
251     FORMAT(/10x,'Time dependent time step', &
          &    /15x,'No. of pairs',3x,3(5x,'Time',11x,'Time',7x), &
          &    /30x,3(5x,'Increment',16x), &
          &    /15x,i10,5x,3(5x,'(s)',12x,'(s)',7x), &
          &    /(30x,6(2x,1pe13.4)))
    END IF
    
    ! Steady-state time threshold
    WRITE(ounit,254) dtss
254 FORMAT(/15x,' Steady state calculation time threshhold is: ',1pe13.4,'(s)')
    
    ! Convergence control options
    WRITE(ounit,260)
260 FORMAT(/10x,' Convergence control options selected')
    
    ! Solution type
    IF (soltyp == 0) THEN
        WRITE(ounit,261) prsacc, tmpac1
261     FORMAT(/15x,'Implicit calculations selected with', &
          &    /15x,'  A minimum fractional difference in rod pressure of     ',1pe13.4, &
          &    /15x,'  A minimum fractional difference in temperature of      ',1pe13.4)
    ELSE IF (soltyp == 1) THEN
        WRITE(ounit,262)
262     FORMAT(/15x,'Explicit calculations selected')
    END IF
    
    ! Convergence criteria
    WRITE(ounit,263) maxit, noiter, epsht1, Units(iu,1)
263 FORMAT(/15x,'Maximum number of iterations for steady state solution is ',i5, &
      &    /15x,'Maximum number of iterations on material properties is    ',i5, &
      &    /15x,'Convergence criteria for temperature subcode is           ',1pe13.4,a4)
    
    ! Nodalization options
    WRITE(ounit,300)
300 FORMAT(/10x,'Nodalization options selected')
    
    ! Axial nodes
    IF (nunopt(11) == 2) THEN
        WRITE(ounit,301) Units(iu,2), (j,zelev(j),j = 1,naxn)
301     FORMAT(/15x,'Specified axial nodes', &
          &    /20x,3x,'Node',6x,'Elevation', &
          &    /20x,13x,a4, &
          &    /(20x,(3x,i4,3x,1pe13.4)))
    ELSE IF (nunopt(11) == 1) THEN
        WRITE(ounit,302) naxn
302     FORMAT(/15x,i4,' Evenly spaced axial nodes selected')
    END IF
    
    ! Fuel radial nodes
    IF (nunopt(12) == 2) THEN
        WRITE(ounit,305) Units(iu,2),(j,fmesh(j),j = 1,nfmesh)
305     FORMAT(/15x,'Specified fuel radial nodes', &
          &    /20x,3x,'Node',6x,'Radius', &
          &    /20x,13x,a4, &
          &    /(20x,(3x,i4,3x,1pe13.4)))
    ELSE IF (nunopt(12) == 1) THEN
        WRITE(ounit,306) nfmesh
306     FORMAT(/15x,i4,' Equal area radial nodes in fuel selected')
    END IF
    
    ! Cladding radial nodes
    IF (nunopt(13) == 2)  THEN
        WRITE(ounit,310) Units(iu,2),(j,cmesh(j),j = 1,ncmesh)
310     FORMAT(/15x,'Specified cladding radial nodes', &
          &    /20x,3x,'Node',6x,'Radius', &
          &    /20x,13x,a4, &
          &    /(20x,(3x,i4,3x,1pe13.4)))
    ELSE IF (nunopt(13) == 1) THEN
        WRITE(ounit,311) ncmesh
311     FORMAT(/15x,i4,' Evenly spaced radial nodes in cladding selected')
    END IF
    
    !
    END SUBROUTINE numinp
    !
    !
    !
    SUBROUTINE powinp
    USE Kinds
    USE variables_fraptran
    USE collct_h
    USE resti_h
    USE scalr_h
    USE Dyna_h, ONLY : buradv, bufrad
    IMPLICIT NONE
    !> @brief
    !> Subroutine to read $power block
    !
    INTEGER(ipk) :: azang, profile, icount, icnt, k, j, ibu, i, InputStat = 0
    REAL(r8k) :: ph, pl, doffst, fpowr, CladPower
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: butemp
    CHARACTER(LEN=3), DIMENSION(2), PARAMETER :: Units = [ 'ft)' ,  'm) ' ]
    CHARACTER(LEN=*), PARAMETER :: BlockDescription = '$power input block'
    
    ! Define $power
    NAMELIST / power / RodAvePower, AxPowProfile, RadPowProfile, butemp, azpang, pazp, ph, pl, &
      &                doffst, fpowr, powop, tpowf, timop, fpdcay, CladPower, azang, profile, &
      &                NumAxProfiles, ProfileStartTime, modheat
    
    ! Write block being read to output file
    WRITE(ounit,'(A)') BlockDescription
    
    ! Set default values
    azpang = 0.0_r8k
    ph = 0.0_r8k
    pl = 0.0_r8k
    doffst = 0.0_r8k
    fpowr = 1.0_r8k
    powop = 0.0_r8k
    tpowf = 0.0_r8k
    timop = 0.0_r8k
    fpdcay = 1.0_r8k
    CladPower = 0.0_r8k
    NumAxProfiles = 1
    azang = 0
    profile = 0
    
    ! Allocate variables
    !ALLOCATE (butemp(1:(naxn*(ntimesteps))))
    ALLOCATE (butemp(1:(naxn*(nfmesh+1))))
    butemp = 0.0_r8k
    
    ! Read $power block
    READ (iunit, power, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'power')
    
    ! Moderator heating
    IF (modheat > 1.0_r8k) THEN
        ! Error
        WRITE(0,110) modheat
        WRITE(ounit,110) modheat
110     FORMAT ('Syntax error in namelist POWER part of line. modheat = ',e12.5,/,'Execution terminated in Subroutine: powinp')
        ERROR STOP 'Syntax error in namelist POWER part of line. Variable modheat. Execution terminated in Subroutine: powinp'
    ELSE IF (modheat < 0.0_r8k) THEN
        ! Use the Gamma Coolant Density Model
        ModHeatModel = 'CoolantDensity'
        WRITE(0,111)
        WRITE(ounit,111)
111     FORMAT ('Coolant density model will be used for calculating coolant gamma-ray heating')        
    ELSE IF (modheat == 0.02_r8k) THEN
        ! Default FRAPTRAN value
        ModHeatModel = 'Default'
    ELSE
        ! User-supplied value
        ModHeatModel = 'UserValue'
    ENDIF
    
    ! Count the # of rod average power values (Assume that there is a value in the first Power, time block)
    icount = 3
    
    Count_RodAvePower_Intervals: DO
        IF ((icount + 1) > UBOUND(RodAvePower,1)) EXIT Count_RodAvePower_Intervals
        IF (RodAvePower(icount) == 0.0_r8k .AND. RodAvePower(icount+1) == 0.0_r8k) EXIT Count_RodAvePower_Intervals
        icount = icount + 2
    END DO Count_RodAvePower_Intervals
    ! # of pairs
    nptha = (icount - 1) / 2
    
    ! Count the number of axial power profile values
    icount = 1
    
    Count_AxPowProfile_Vals: DO
        IF ((icount + 1) > UBOUND(AxPowProfile(:,1),1)) EXIT Count_AxPowProfile_Vals
        IF (AxPowProfile(icount,1) == 0.0_r8k .AND. AxPowProfile(icount+1,1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(AxPowProfile(:,1),1)) EXIT Count_AxPowProfile_Vals
            IF (AxPowProfile(icount+2,1) == 0.0_r8k .AND. AxPowProfile(icount+3,1) == 0.0_r8k) EXIT Count_AxPowProfile_Vals
        END IF
        icount = icount + 2
    END DO Count_AxPowProfile_Vals
    !# of pairs
    npaxp = (icount - 1) / 2
    
    ! Decay power
    IF (powop > 0.0_r8k) THEN
        WRITE(ounit,201) powop, timop, fpdcay, tpowf
201     FORMAT(' Decay heat option set with:',/, &
          &    '      powop = ',g11.4,/, &
          &    '      timop = ',g11.4,/, &
          &    '      fpdcay= ',g11.4,/, &
          &    '      tpowf = ',g11.4)
        mpdcay = 1
        IF (timop <= 0.0_r8k) THEN
            WRITE(ounit,202) timop
202         FORMAT ('timop must be greater than 0.0. timop = ',e12.5,/,'Execution terminated in Subroutine: numinp')
            ERROR STOP 'timop must be greater than 0.0. Execution terminated in Subroutine: numinp'
        ENDIF
    ENDIF
    
    ! Count the # of radial power profile values
    icount = 1
    
    Count_RadPowProfile_Vals: DO
        IF ((icount + 1) > UBOUND(RadPowProfile,1)) EXIT Count_RadPowProfile_Vals
        IF (RadPowProfile(icount) == 0.0_r8k .AND. RadPowProfile(icount+1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(RadPowProfile,1)) EXIT Count_RadPowProfile_Vals
            IF (RadPowProfile(icount+2) == 0.0_r8k .AND. RadPowProfile(icount+3) == 0.0_r8k) EXIT Count_RadPowProfile_Vals
        END IF
        icount = icount + 2
    END DO Count_RadPowProfile_Vals
    !# of pairs
    nprad = (icount - 1) / 2
    
    ! Load 2-D radial power profile-related arrays for later use in cominp
    nradq = nprad / naxn
    IF (nradq > 0) THEN
        ALLOCATE (radtemp(1:naxialnodes,1:nradq))
        ALLOCATE (fuelrad(1:naxialnodes,1:nradq))
        radtemp = 0.0_r8k
        fuelrad = 0.0_r8k
    END IF
    
    icnt = 1
    DO k = 1, naxn
        DO j = 1, nradq
            ! Relative radial power profile value
            radtemp(k,j) = RadPowProfile(icnt)
            ! Radius for relative radial power profile value
            fuelrad(k,j) = RadPowProfile(icnt+1)
            ! Increment counter by 2
            icnt = icnt + 2
        ENDDO
    ENDDO
    
    ! Count the # of radial burnup profile values
    icount = 1
    Count_butemp_Vals: DO
        IF ((icount + 1) > UBOUND(butemp,1)) EXIT Count_butemp_Vals
        IF (butemp(icount) == 0.0_r8k .AND. butemp(icount+1) == 0.0_r8k) THEN
            ! Check to see if the first provided values were (0.0, 0.0) or if no values were provided
            IF ((icount + 3) > UBOUND(butemp,1)) EXIT Count_butemp_Vals
            IF (butemp(icount+2) == 0.0_r8k .AND. butemp(icount+3) == 0.0_r8k) EXIT Count_butemp_Vals
        END IF
        icount = icount + 2
    END DO Count_butemp_Vals
    !# of pairs
    ibu = (icount - 1) / 2
    
    ! Load 2-D burnup-related arrays for later use in cominp
    nbuq = ibu / naxn
    IF (nbuq > 0) THEN
        ALLOCATE (buradv(1:naxialnodes,1:nradialnodes))
        ALLOCATE (bufrad(1:nradialnodes,1:naxialnodes))
        buradv = 0.0_r8k
        bufrad = 0.0_r8k
    END IF
    
    icnt = 1
    DO k = 1, naxn
        DO j = 1, nbuq
            ! Radial burnup values
            buradv(k,j) = butemp(icnt)
            ! Fuel radius for radial burnup values
            bufrad(j,k) = butemp(icnt+1)
            ! Increment counter by 2
            icnt = icnt + 2
        ENDDO
        CladdingPower(k) = CladPower
    END DO
    
    ! Adjust the powers by a factor of fpowr
    DO i = 1, (2*nptha-1), 2
        RodAvePower(i) = RodAvePower(i) * fpowr
    END DO
    
    ! Error checking
    ! Make sure RodAvePower(even) is increasing
    IF (nptha > 1) THEN
        DO i = 2, nptha
            IF (RodAvePower(2*i) < RodAvePower(2*(i-1))) THEN
                WRITE(ounit,501)
501             FORMAT ('Time values in RodAvePower are not increasing. Execution stopped in Subroutine: numinp')
                ERROR STOP 'Time values in RodAvePower are not increasing. Execution stopped in Subroutine: numinp'
            ENDIF
        ENDDO
    ENDIF
    ! Make sure RodAvePower(odd) is positive
    DO i = 1, nptha
        IF (RodAvePower(2*i-1) < 0.0_r8k) THEN
            WRITE(ounit,504)
504         FORMAT(' Power values in RodAvePower are negative')
            ERROR STOP 'Power valeus in RodAvePower are negative. Terminated in Subroutine: numinp'
        ENDIF
    ENDDO
    ! Make sure AxPowProfile(even,n) is increasing
    DO j = 1, NumAxProfiles
        IF (npaxp > 1) THEN
            DO i = 2, npaxp
                IF (AxPowProfile(2*i,j) < AxPowProfile(2*(i-1),j)) THEN
                    WRITE(ounit,510) j
510                 FORMAT(' Axial elevation values in AxPowProfile are not increasing for shape number: ',i4,/, &
                           ' Execution terminated in Subroutine: numinp')
                    ERROR STOP 'Axial elevation values in AxPowProfile are not increasing. Terminated in Subroutine: numinp'
                ENDIF
            ENDDO
        ENDIF
    ENDDO
    ! Make sure AxPowProfile(odd,n) is positive
    DO j = 1, NumAxProfiles
        DO i = 1, npaxp
            IF (AxPowProfile(2*i-1,j) < 0.0_r8k) THEN
                WRITE(ounit,511) j
511             FORMAT(' Negative values are not allowed in axial power profile.  Shape number: ',i4,/, &
                       ' Execution terminated in Subroutine: numinp')
                ERROR STOP ' Negative values are not allowed in axial power profile. Terminated in Subroutine: numinp'
            ENDIF
        ENDDO
    ENDDO
    ! Make sure AxPowProfile(2*npaxp)=RodLength
    DO j = 1, NumAxProfiles
        IF (AxPowProfile(2*npaxp,j) /= rl) THEN
            WRITE(ounit,512) j
512         FORMAT(' Final axial elevation value in AxPowProfile is not equal to RodLength for shape number: ',i4,/, &
                   ' Execution terminated in Subroutine: numinp')
            ERROR STOP 'Final axial elevation value in AxPowProfile /= RodLength. Terminated in Subroutine: numinp'
        ENDIF
    ENDDO
    
    ! Write out input values
    
    ! Power vs. time history
    WRITE(ounit,205) (Units(iu),i = 1,4), nptha,(RodAvePower(i),i = 1,nptha*2)
205 FORMAT(50x,'Power time history',//,' No. of pairs',2x,4(5x,'Linear',9x,'Time',6x),/, &
      &    15x,4(5x,'Power',10x,'(sec)',5x),/, &
      &    15x,4(5x,'(kW/',a4,17x),/, &
      &    i10,5x,8(2x,1pe13.4),/, &
      &    (15x,8(2x,1pe13.4)))
    
    ! Axial power profiles
    DO j = 1, NumAxProfiles
        WRITE(ounit,206) j,ProfileStartTime(j), (Units(iu),i=1,4), npaxp,(AxPowProfile(i,j),i=1,npaxp*2)
206     FORMAT(/30x,'Axial power profile number ',i4,', Start time is: ',e13.4, &
          &    //,  ' No. of pairs',2x,4(5x,'Axial',10x,'Axial',5x), &
          &    /15x,4(5x,'Power',10x,'Distance',2x), &
          &    /15x,4(5x,'Ratio',10x,'(',a4,5x), &
          &    /i10,5x,8(2x,1pe13.4), &
          &    /(15x,8(2x,1pe13.4)))
    ENDDO
    
    ! Radial profiles (if not reading from FRAPCON restart file)
    IF (NFrapconInitialization == 0) THEN
        
        ! Radial power profile
        WRITE(ounit,207)
207     FORMAT(/' Input relative radial power profiles in fuel for each axial node')
        DO k = 1, naxn
            WRITE(ounit,208) k
208         FORMAT(/,' Axial node ',i4,/, &
              &    /6x,' Radial ',8x,'Radial', &
              &    /6x,'Distance',8x,'Power ', &
              &    /6x,'(meters)',8x,'Ratio ')
            DO j = 1, nradq
                WRITE(ounit,209) fuelrad(k,j),radtemp(k,j)
209             FORMAT(3x,1pe13.4,3x,1pe13.4)
            ENDDO
        ENDDO
        
        ! Print out radial burnup profile
        IF (ibu >  0) THEN
            WRITE(ounit,401)
401         FORMAT(/' Radial burnup profiles in fuel for each axial node')
            DO k = 1, naxn
                WRITE(ounit,402) k
402             FORMAT(/,' Axial node ',i4,/, &
                  &    /6x,' Radial ',8x,' burnup  ', &
                  &    /6x,'Distance',8x,'(MWd/MTM)', &
                  &    /6x,'(meters)')
                DO j = 1, nbuq
                    WRITE(ounit,403) bufrad(j,k),buradv(k,j)
403                 FORMAT(3x,1pe13.4,3x,1pe13.4)
                ENDDO
            ENDDO
        ENDIF
        !
    ENDIF
    !
    END SUBROUTINE powinp
    !
END MODULE Read_Input












