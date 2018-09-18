MODULE rlpst
    USE Kinds
    USE frapc
    USE scalr_h, ONLY : burad
    USE conversions_fraptran, ONLY : sechr, ftin
    USE HeatTransferCoefficient, ONLY : gaphtc
    IMPLICIT NONE
    !>@brief
    !> This module replaces the three files rlpst1.f, rlpst2.f & rlpst3.f.
    !> Those subroutines are all stored in this Moudle
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 3/23/2014
    CONTAINS
    !
    SUBROUTINE rlpst1 (igpnod, ncladi, nmesh, RadialBound)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine rlpst1 defines parameters normally defined in input for temperature calculation.
    !> Subroutine only used If temperature distribution calculated in relap (ncools = 7 option)
    !
    ! ncard2  - Control switch on read of input cards
    !       0 = Do not read cards
    !       1 = Read input cards this Call (first Call for given rod)
    ! nrest2  - Control switch on storage place of frap restart Data set.  0 = lcm storage
    ! ncool2  - On/off switch for link with t/h code (0 = Off, 1 = On)
    ! ndtadv  - Switch to advance in time or perform calculations only at time t12.
    !       0 = Advance in time
    !       1 = Only perform calculations at time t12
    ! t12     - Start time for frap calculations (sec)
    ! t22     - End time for frap calculations (sec)
    !
    ! for all arrays, maximum value of l  =  20 , n  =  12
    !
    ! for arrays with name Ending in rlp, maximum value of  k  =  24
    ! for arrays with name Ending in frp, maximum value of  k  =  20
    !
    ! l  - radial node
    ! k  - axial node(heat slab)
    ! n  - fuel rod number
    ! tmprlp(l,k)        - T/H Code computed fuel rod temperature  (K)
    ! rmrlp(l)           - Radial coordinate (cold state) (m) (from t/h code) radius to radial node l of fuel rod n
    !                  0 = fuel centerline
    ! rmrlp(lmax)        - Radius of cladding outside surface
    ! tprlp              - Plenum gas temperature  (K)        (from t/h code)
    ! nforlp             - Radial node at fuel pellet surface (from t/h code)
    ! ncirlp             - Radial node at cladding inside surface  (t/h code)
    ! ncorlp             - Radial node at cladding outside surface (t/h code)
    ! pclrlp(k)          - Coolant pressure (n/m**2)               (t/h code)
    ! ElevatThermHydr(k) - Elevation of t/h code axial node (heat slab) (m) 
    !                      Bottom of fuel pellet stack assumed to have elevation of zero
    ! kmxrlp             - number t/h code axial nodes
    ! kmxfrp             - number of frap axial nodes            (from frap)
    ! ElevatFrap(k)      - elevation of frap k-th axial node (m) (bottom of fuel pellet stack has elevation of 0.0)
    ! hgpfrp(k)          - gas gap conductance (watt/m**2-K)          (frap)
    ! drdfrp(k)          - fuel rod outer diameter (m)                (frap)
    ! Iffrp(k)           - cladding failure indicator, 0 = no , 1 = yes (frap)
    ! vrlfrp(k)          - fuel void fraction due  to relocation  (unitless)
    ! pgpfrp(k)          - fuel rod internal gas pressure (n/m**2)    (frap)
    ! GasFraction(j)     - fraction of j-th gas                        (frap)
    !                  1 = helium
    !                  2 = argon
    !                  3 = krypton
    !                  4 = xenon
    !                  5 = hydrogen
    !                  6 = air
    !                  7 = water vapor
    ! bufrp(k)           - fuel burnup (MW-sec/kg) (frap)
    ! gsmfrp             - gram-moles of gas
    !
    INTEGER(ipk) :: l, k, nbugr = 0
    INTEGER(ipk) :: igpnod, ncladi, nmesh
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: RadialBound
    !
    igpnod = nforlp
    ncladi = ncirlp
    nmesh = ncorlp
    RadialBound(1:nmesh) = rmrlp(1:nmesh)
    !Determine whether to WRITE out data passed b/t codes
    IF (nbugr == 1) THEN
        ! Write out data
        WRITE(ounit,972)
972     FORMAT(//' info passed to frap from relap  ')
        WRITE(ounit,973) kmxrlp, nforlp,ncirlp, ncorlp, pclrlp(1)
973     FORMAT(' kmxrlp = ',i4,' nforlp = ',i3,' ncirlp = ',i3,' ncorlp = ',i3,' pcrlp = ',e13.6)
        WRITE(ounit,974)
974     FORMAT(' radial mesh, ft  ' )
        WRITE(ounit,976) (rmrlp(l), l = 1,ncorlp)
976     FORMAT(8(2x,e11.4))
        DO k = 1, kmxrlp
            WRITE(ounit,982) k, ElevatThermHydr(k)
982         FORMAT(' at heat slab',i4,' elevation  = ',e13.6)
            WRITE(ounit,983)
983         FORMAT(' radial temperature distribution ' )
            WRITE(ounit,976) (tmprlp(l,k),l = 1,ncorlp)
        ENDDO
    END IF
    !  
    END SUBROUTINE rlpst1
    !
    !
    !
    SUBROUTINE rlpst2 (EOSTemp, tplna, GapTemp, FuelSurfT, CladAveTemp, IndexTempConverg, CoolPress, &
      &                FuelCenterT, CladSurfT, CrackTemp, OpenPorTemp, tmelt, FilmCoeffAv, BulkCoolTemp, &
      &                HeatFlux, CladMaxT, AxNodElevat, naxn, nmesh, igpnod, ncladi, RadialBound, &
      &                BOSTemp, WatrMetlEnrgy, EOSOxideThick, AlphaThk2, OxiThk2, AlphaThk22, CritHtFlux, &
      &                HGapAv, AxialPowr, powave)
    USE Kinds
    USE variables_fraptran, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Subroutine defines quantities that are normally defined in subroutine heat. 
    !> Info from relap passed thru common block /frapc/ used to define these quantities.
    !> This subroutine is only used if temperature distribution calculated in relap (ncools = 7).
    !> In this case, the subroutine heat is bypassed.
    !
    !
    ! ncard2  - Control switch on read of input cards
    !       0 = Do not read cards
    !       1 = Read input cards this Call (first Call for given rod)
    ! nrest2  - Control switch on storage place of frap restart Data set.  0 = lcm storage
    ! ncool2  - On/off switch for link with t/h code (0 = Off, 1 = On)
    ! ndtadv  - Switch to advance in time or perform calculations only at time t12.
    !       0 = Advance in time
    !       1 = Only perform calculations at time t12
    ! t12     - Start time for frap calculations (sec)
    ! t22     - End time for frap calculations (sec)
    !
    ! for all arrays, maximum value of l  =  20 , n  =  12
    !
    ! for arrays with name Ending in rlp, maximum value of  k  =  24
    ! for arrays with name Ending in frp, maximum value of  k  =  20
    !
    ! l  - radial node
    ! k  - axial node(heat slab)
    ! n  - fuel rod number
    ! tmprlp(l,k)        - T/H Code computed fuel rod temperature  (K)
    ! rmrlp(l)           - Radial coordinate (cold state) (m) (from t/h code) radius to radial node l of fuel rod n
    !                  0 = fuel centerline
    ! rmrlp(lmax)        - Radius of cladding outside surface
    ! tprlp              - Plenum gas temperature  (K)        (from t/h code)
    ! nforlp             - Radial node at fuel pellet surface (from t/h code)
    ! ncirlp             - Radial node at cladding inside surface  (t/h code)
    ! ncorlp             - Radial node at cladding outside surface (t/h code)
    ! pclrlp(k)          - Coolant pressure (n/m**2)               (t/h code)
    ! ElevatThermHydr(k) - Elevation of t/h code axial node (heat slab) (m) 
    !                      Bottom of fuel pellet stack assumed to have elevation of zero
    ! kmxrlp             - number t/h code axial nodes
    ! kmxfrp             - number of frap axial nodes            (from frap)
    ! ElevatFrap(k)      - elevation of frap k-th axial node (m) (bottom of fuel pellet stack has elevation of 0.0)
    ! hgpfrp(k)          - gas gap conductance (watt/m**2-K)          (frap)
    ! drdfrp(k)          - fuel rod outer diameter (m)                (frap)
    ! Iffrp(k)           - cladding failure indicator, 0 = no , 1 = yes (frap)
    ! vrlfrp(k)          - fuel void fraction due  to relocation  (unitless)
    ! pgpfrp(k)          - fuel rod internal gas pressure (n/m**2)    (frap)
    ! GasFraction(j)     - fraction of j-th gas                        (frap)
    !                  1 = helium
    !                  2 = argon
    !                  3 = krypton
    !                  4 = xenon
    !                  5 = hydrogen
    !                  6 = air
    !                  7 = water vapor
    ! bufrp(k)           - fuel burnup (MW-sec/kg) (frap)
    ! gsmfrp             - gram-moles of gas
    !
    INTEGER(ipk) :: ndbug=0, kr1, k, naxn, kr2, l, nmesh, ncards, nodgas, n, igpnod, ncladi
    REAL(r8k) :: zf, zrb, zrt, tr1, tr2, pr1, pr2, tplen, sum, asum, ps, dps
    REAL(r8k), PARAMETER :: tmeltf = 5143.4_r8k
    REAL(R8k), PARAMETER :: tmeltc = 3316.4_r8k
    INTEGER(ipk), DIMENSION(:) :: IndexTempConverg
    REAL(r8k), DIMENSION(3) :: tmelt
    REAL(r8k), DIMENSION(:) :: RadialBound, GapTemp, HeatFlux, FuelSurfT, CladAveTemp, FuelCenterT, CladSurfT, &
      &                        CrackTemp, OpenPorTemp, FilmCoeffAv, BulkCoolTemp, CoolPress, &
      &                        CladMaxT, AxNodElevat, WatrMetlEnrgy, EOSOxideThick, AlphaThk2, &
      &                        OxiThk2, AlphaThk22, CritHtFlux, HGapAv, AxialPowr, powave
    REAL(r8k), DIMENSION(:,:) :: EOSTemp, BOSTemp
    REAL(r8k), DIMENSION(:,:,:) :: tplna
    ! If frap-s restart read this frap call, get temp from restart tape
    !
    ! Find current radial temperature distribution at frap axial nodes.
    ! Do so by interpolating in tmprlp array of temperatures recieved from relap.
    !
    ! frap and relap forced to have same radial nodalization in subroutine rlpst1.
    !
    powave(1) = 0.0_r8k
    kr1 = 1
    DO k = 1, naxn
        zf = AxNodElevat(k)
5       zrb = ElevatThermHydr(kr1)
        IF (zrb < zf) GOTO 10
        kr2 = kr1
        zrb = zf
        zrt = zrb + 1.0_r8k
        GOTO 30
10      kr2 = kr1 + 1
        IF (kr2 <= kmxrlp) GOTO 15
        kr2 = kmxrlp
        zrt = ElevatThermHydr(kr2)
        IF (zrt >= zf) GOTO 30
        kr1 = kr2
        zrb = zf
        GOTO 30
15      zrt = ElevatThermHydr(kr2)
        IF (zrt >= zf) GOTO 30
        kr1 = kr2
        GOTO 5
30      DO l = 1, nmesh
            tr1 = tmprlp(l,kr1)
            tr2 = tmprlp(l,kr2)
            EOSTemp(l,k) = tr1 + ((zf - zrb) / (zrt - zrb)) * (tr2 - tr1)
        ENDDO
        pr1 = pclrlp(kr1)
        pr2 = pclrlp(kr2)
        CoolPress(k) = pr1 + ((zf - zrb) / (zrt - zrb)) * (pr2 - pr1)
        IF (ndbug == 1) THEN
            WRITE(ounit,901) k,zf,zrb,zrt,kr1,kr2
901         FORMAT(' k = ',i3,' zf = ',e13.6,' zrb = ',e13.6,' zrt = ',e13.6,' kr1 = ',i4,' kr2 = ',i4)
            WRITE(ounit,903)
903         FORMAT(' frap temperatures ' )
            WRITE(ounit,905) (EOSTemp(l,k),l = 1,nmesh)
905         FORMAT(8(2x,e11.4))
            WRITE(ounit,907)
907         FORMAT(' relap temp, bottom ')
            WRITE(ounit,905) (tmprlp(l,kr1),l = 1,nmesh)
            WRITE(ounit,909)
909         FORMAT(' relap temp, top ')
            WRITE(ounit,905) (tmprlp(l,kr2),l = 1,nmesh)
        ENDIF
    ENDDO
    IF (ncards == 1) THEN
        DO l = 1,nmesh
            BOSTemp(l,1:naxn) = EOSTemp(l,1:naxn)
        ENDDO
    ENDIF
    ! define temperatures in fuel rod plenum
    nodgas = 6
    tplen = tprlp
    tplna(1:nodgas,2,1) = tplen
    ! define fuel, cladding, gas gap freezing temperatures
    tmelt(1) = tmeltf
    tmelt(2) = tmeltc
    tmelt(3) = 1.0E20_r8k
    DO k = 1, naxn
        WatrMetlEnrgy(k) = 0.0_r8k
        EOSOxideThick(k) = 0.0_r8k
        AlphaThk2(k) = 0.0_r8k
        OxiThk2(k) = 0.0_r8k
        AlphaThk22(k) = 0.0_r8k
        ! compute average fuel temperature
        sum = 0.0_r8k
        asum = 0.0_r8k
        DO n = 1, igpnod
            IF (n > 1) GOTO 75
            ps = (0.5_r8k * (RadialBound(1) + RadialBound(2))) ** 2
            GOTO 78
75          IF (n == igpnod) GOTO 77
            ps = (0.5_r8k * (RadialBound(n) + RadialBound(n+1))) ** 2 - (0.5_r8k * (RadialBound(n) + RadialBound(n-1))) ** 2
            GOTO 78
77          ps = RadialBound(igpnod) ** 2 - (0.5_r8k * (RadialBound(igpnod) + RadialBound(igpnod-1))) ** 2
78          CONTINUE
            sum = sum + ps * EOSTemp(n,k)
            asum = asum + ps
        ENDDO
        OpenPorTemp(k) = sum / asum
        CrackTemp(k) = OpenPorTemp(k)
        GapTemp(k) = 0.5_r8k * (EOSTemp(nmesh,k) + EOSTemp(ncladi,k))
        FuelSurfT(k) = EOSTemp(igpnod,k)
        ! find average cladding temperature
        sum = 0.0_r8k
        ps = 0.0_r8k
        DO l = ncladi, nmesh
            dps = 1.0_r8k
            IF (l == ncladi .OR. l == nmesh) dps = 0.5_r8k
            sum = sum + dps * EOSTemp(l,k)
            ps = ps + dps
        ENDDO
        CladAveTemp(k) = sum / ps
        IF (CladMaxT(k) < CladAveTemp(k)) CladMaxT(k) = CladAveTemp(k)
        IndexTempConverg(k) = 0
        FuelCenterT(k) = EOSTemp(1,k)
        CladSurfT(k) = EOSTemp(nmesh,k)
        !
        !**** FilmCoeffAv, BulkCoolTemp, and HeatFlux quantities are needed by the balloon model
        !**** at ballooning axial node. since balloon model not sensitive to these quantities, 
        !****give them a nominal value to avoid bringing them thru link
        !
        FilmCoeffAv(k) =  10.0_r8k / sechr
        BulkCoolTemp(k) = 300.0_r8k
        HeatFlux(k) = 10000.0_r8k / sechr
        CritHtFlux(k) = 0.0_r8k
        HGapAv(k) = 0.0_r8k
        AxialPowr(k) = 0.0_r8k
    ENDDO
    END SUBROUTINE rlpst2
    !
    !
    !
    SUBROUTINE rlpst3 (AxNodElevat, GapThick, PelletRad, GasPress, RInterfacPrs, RodOD, naxn, gadolin, &
      &                bulocal, gapmin, Ifchk, n2, RodFailIndex, Vreloc, modfd, GapTemp, FuelSurfT, &
      &                EOSTemp, ncladi, GasFraction, OxiThk2, RadialBound, igpnod, roughc, roughf, &
      &                frden, tflxc, FastFlux, coldw, AxBurnup, frpo2, CladMaxT, n1, TotalGasMoles, &
      &                HGapAv, fotmtl)
    USE Kinds
    USE variables_fraptran, ONLY : nmesh
    IMPLICIT NONE
    !>@brief
    !> Subroutine defines frap computed quantities in relap/frap Data link (/frapc / common block )
    !> This subroutine is only called if temperature distribution is calculated in relap (ncool=7)
    !
    !
    ! ncard2  - Control switch on read of input cards
    !       0 = Do not read cards
    !       1 = Read input cards this Call (first Call for given rod)
    ! nrest2  - Control switch on storage place of frap restart Data set.  0 = lcm storage
    ! ncool2  - On/off switch for link with t/h code (0 = Off, 1 = On)
    ! ndtadv  - Switch to advance in time or perform calculations only at time t12.
    !       0 = Advance in time
    !       1 = Only perform calculations at time t12
    ! t12     - Start time for frap calculations (sec)
    ! t22     - End time for frap calculations (sec)
    !
    ! for all arrays, maximum value of l  =  20 , n  =  12
    !
    ! for arrays with name Ending in rlp, maximum value of  k  =  24
    ! for arrays with name Ending in frp, maximum value of  k  =  20
    !
    ! l  - radial node
    ! k  - axial node(heat slab)
    ! n  - fuel rod number
    ! tmprlp(l,k)        - T/H Code computed fuel rod temperature  (K)
    ! rmrlp(l)           - Radial coordinate (cold state) (m) (from t/h code) radius to radial node l of fuel rod n
    !                  0 = fuel centerline
    ! rmrlp(lmax)        - Radius of cladding outside surface
    ! tprlp              - Plenum gas temperature  (K)        (from t/h code)
    ! nforlp             - Radial node at fuel pellet surface (from t/h code)
    ! ncirlp             - Radial node at cladding inside surface  (t/h code)
    ! ncorlp             - Radial node at cladding outside surface (t/h code)
    ! pclrlp(k)          - Coolant pressure (n/m**2)               (t/h code)
    ! ElevatThermHydr(k) - Elevation of t/h code axial node (heat slab) (m) 
    !                      Bottom of fuel pellet stack assumed to have elevation of zero
    ! kmxrlp             - number t/h code axial nodes
    ! kmxfrp             - number of frap axial nodes            (from frap)
    ! ElevatFrap(k)      - elevation of frap k-th axial node (m) (bottom of fuel pellet stack has elevation of 0.0)
    ! hgpfrp(k)          - gas gap conductance (watt/m**2-K)          (frap)
    ! drdfrp(k)          - fuel rod outer diameter (m)                (frap)
    ! Iffrp(k)           - cladding failure indicator, 0 = no , 1 = yes (frap)
    ! vrlfrp(k)          - fuel void fraction due  to relocation  (unitless)
    ! pgpfrp(k)          - fuel rod internal gas pressure (n/m**2)    (frap)
    ! GasFraction(j)     - fraction of j-th gas                        (frap)
    !                  1 = helium
    !                  2 = argon
    !                  3 = krypton
    !                  4 = xenon
    !                  5 = hydrogen
    !                  6 = Nitrogen
    !                  7 = air
    !                  8 = water vapor
    ! bufrp(k)           - fuel burnup (MW-sec/kg) (frap)
    ! gsmfrp             - gram-moles of gas
    !
    INTEGER(ipk) :: naxn, k, ifail, ncladi, igpnod, ngaps, l, ngapi, modfd, ifchk, n2, n1
    REAL(r8k) :: totalgasmoles, rfinch, roughf, rcinch, roughc, cruf, rcinchh, gapmin, gpthk0, &
      &          zroi, ftemp, ctemp, gptemp, tshftm, pshftm, fngaps, frgseg, fnx, tgshft, pofset, &
      &          frcsum, hgpsum, gasgpi, pfci, fulori, bulocal, tflxc, frden, coldw, fotmtl, hgap, frpo2
    INTEGER(ipk), DIMENSION(:), INTENT(IN) :: RodFailIndex
    REAL(r8k), DIMENSION(10) :: delgap, frcgap, delpfc
    REAL(r8k), DIMENSION(:) :: RadialBound, AxNodElevat, GapThick, PelletRad, GasPress, RInterfacPrs, Vreloc, GasFraction, &
      &                         GapTemp, HGapAv, RodOD, FuelSurfT, OxiThk2, FastFlux, AxBurnup, CladMaxT, gadolin
    REAL(r8k), DIMENSION(:,:) :: EOSTemp
    !
    kmxfrp = naxn
    gsmfrp = TotalGasMoles
    DO k = 1, naxn
        ElevatFrap(k) = AxNodElevat(k) !ft
        pgpfrp(k) = GasPress(k)        !psi
        bufrp(k) = AxBurnup(k)
        drdfrp(k) = RodOD(k)
        vrlfrp(k) = Vreloc(k)
        Ifail = RodFailIndex(k)
        IF (Ifail >= 1) Ifail = 1
        Iffrp(k) = Ifail
    ENDDO
    ! compute gap conductance
    rfinch = roughf / 2.54000508_r8k
    rcinch = roughc / 2.54000508_r8k
    cruf   = rfinch + rcinchh
    gapmin = MAX(cruf, 0.5e-05_r8k) / ftin
    !
    DO k = 1, naxn
        gpthk0 = RadialBound(ncladi) - RadialBound(igpnod)
        zroi = OxiThk2(k) * 0.0254_r8k
        ftemp = FuelSurfT(k)
        ctemp = EOSTemp(ncladi,k)
        gptemp = GapTemp(k)
        ngaps = 8
        tshftm = GapThick(k)
        IF (tshftm > (gpthk0 / 2.0_r8k)) tshftm = gpthk0 / 2.0_r8k
        pshftm = 500.0_r8k
        fngaps = ngaps
        frgseg = 1.0_r8k / fngaps
        DO l = 1, ngaps
            frcgap(l) = frgseg
            fnx = l
            tgshft = -1.0_r8k + 2.0_r8k * (fnx - 1.0_r8k) / (fngaps - 1.0_r8k)
            delgap(l) = tshftm * (-1.0_r8k + (2.0_r8k * fnx - 1.0_r8k) / fngaps)
            IF (RInterfacPrs(k) > 0.0_r8k) THEN
                pofset = 0.0_r8k
                delpfc(l) = (tgshft - pofset) * pshftm
            ELSE
                delpfc(l) = 0.0_r8k
            ENDIF
        ENDDO
        frcsum = 0.0_r8k
        hgpsum = 0.0_r8k
        DO ngapi = 1, ngaps
            frcsum = frcsum + frcgap(ngapi)
            gasgpi = GapThick(k) + delgap(ngapi)
            IF (gasgpi < 0.0_r8k) gasgpi = 0.0_r8k
            pfci = RInterfacPrs(k) + delpfc(ngapi)
            IF (pfci < 0.0_r8k) pfci = 0.0_r8k
            fulori = PelletRad(k) - delgap(ngapi)
            !
            bulocal = burad(k,1) !Set the burnup to the burnup of the fuel center
            CALL gaphtc (gasgpi, fulori, pfci, gptemp, ftemp, ctemp, GasPress(k), GasFraction, &
              &          FastFlux(k), tflxc, rcinch, rfinch, frden, coldw, zroi, fotmtl, &
              &          CladMaxT(k), modfd, hgap, gadolin(k), bulocal, gapmin, k)
            !
            hgpsum = hgpsum + frcgap(ngapi) * hgap
        ENDDO
        hgap = hgpsum / frcsum
        HGapAv(k) = hgap
        hgpfrp(k) = hgap
    ENDDO
    !
    END SUBROUTINE rlpst3
    !
END MODULE rlpst












