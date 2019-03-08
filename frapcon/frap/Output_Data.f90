MODULE Output_Data_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon
    USE Gas_frapcon, ONLY : ngases
    USE RunProperties_frapcon
    USE Material_Properties_frapcon, ONLY : MatProp
    IMPLICIT NONE
    !> This module contains the output data for the following files_frapcon:
    !> print1 (FILE=06) This is the input information for the output file
    !> print2 (FILE=06) This is the run information for the output file
    !> frttr file (FILE=50) This file is for the FRAPCON to TRACE BU dependent data for NRC internal use only_frapcon
    !> @author
    !> Ian Porter, NRC
    !> @date
    !> December 2013
    !
    ! Fuel melting indicator
    INTEGER(ipk), PRIVATE :: imelt = 0
    !
    CONTAINS
    ! ******************
    !   Output File
    ! ******************
    SUBROUTINE print1
    USE Kinds_frapcon
    USE conversions_frapcon
    IMPLICIT NONE
    !> @brief
    !> print1 is called for the initial prints to the output file describing the input options for the case that is being analyzed.
    !
    INTEGER(ipk) :: oligp2, i, ii, ii_Next
    REAL(r8k) :: rate, ci3tm3, ruffml, rufcml, sirufc, siruff, shdsi, buinsi, dcosi, dcisi, dpsi, &
      &          desi, pitchs, fgpavs, hpltsi, totlsi, rdishs, hdishs, cfvsi, vsssi, cplsi, dspgsi, &
      &          cvvsi, dspgws, tccsi, cdgsi, crdtsi, tsintk, gp, gpsi, gpmil, gosi, twsi, powsi, &
      &          p2si, timehr, cpvsi, qmpychan, powsichan, kgUm, kgUft, UDensity
    LOGICAL :: zontbl = .FALSE.
    !
    REAL(r8k), DIMENSION(na-1) :: rcsi
    !
    CALL pghead
    rate = 0.0_r8k
    IF (icor == 2) rate = crdtr * 8760.0_r8k
    WRITE (ounit,190)
    SELECT CASE (iplant)
    CASE (1)
        WRITE (ounit,200)
    CASE (2)
        WRITE (ounit,210)
    CASE (3)
        WRITE (ounit,220) comp(1)
    CASE (4)
        WRITE (ounit,230) comp(1)
    END SELECT
    ci3tm3 = intom ** 3
    ruffml = roughf * 1000.0_r8k
    rufcml = roughc * 1000.0_r8k
    sirufc = roughc * intomm
    siruff = roughf * intomm
    shdsi = dishsd * intomm
    buinsi = buin(1) / 1000.0_r8k
    dcosi = dco(1) * intocm
    dcisi = dci(1) * intocm
    dpsi = dp(1) * intocm
    desi = de(1) * intocm
    pitchs = pitch * intocm
    IF (nunits == 0) THEN
        pitchs = pitch
        pitch = pitch * cmtoin
    END IF
    fgpavs = fgpav * PSItoMPa
    hpltsi = hplt * intocm
    totlsi = totl * ftom
    rdishs = rdish * intocm
    hdishs = hdish * intomm
    rcsi(1:na-1) = rc(1:na-1) * intomm
    IF (MAXVAL(rc) /= MINVAL(rc)) zontbl = .TRUE.
    cfvsi = cfv * ci3tm3
    vsssi = Spring%Vcold * ci3tm3
    cplsi = cpl * intocm
    dspgsi = Spring%dspg * intocm
    cpvsi = cpv * ci3tm3
    cvvsi = cvv * ci3tm3
    dspgws = Spring%dspgw * intomm
    tcc = (dco(1) - dci(1)) / 2.0_r8k
    tccsi = tcc * intomm
    cdgsi = cdg(1) * intomm / 1.0e3_r8k
    crdtsi = crdt * intomm / 1.0e3_r8k
    tsintk = tfk(tsint)
    SELECT CASE (icm)
    CASE (2)
        WRITE (ounit,240)
    CASE (4)
        WRITE (ounit,250)
    CASE (5)
        WRITE (ounit,251)
    CASE (6)
        WRITE (ounit,252)
    CASE (7)
        WRITE (ounit,253)
    END SELECT
    IF (ivardm == 0) THEN
        WRITE (ounit,260) dcosi, dco(1)
        WRITE (ounit,270) dcisi, dci(1)
        WRITE (ounit,280) tccsi, tcc
        WRITE (ounit,400) sirufc, rufcml
        WRITE (ounit,290) cdgsi, cdg(1)
        WRITE (ounit,300) dpsi, dp(1)
    ELSE
        WRITE (ounit,400) sirufc, rufcml
    END IF
    WRITE (ounit,310) hpltsi, hplt
    WRITE (ounit,350) hdishs, hdish
    WRITE (ounit,360) shdsi, dishsd
    WRITE (ounit,370) rdishs, rdish
    IF (.NOT. zontbl) WRITE (ounit,380) rcsi(1), rc(1)
    WRITE (ounit,410) tsintk, tsint
    WRITE (ounit,340) den
    IF (rsntr == 0.0_r8k) THEN
         WRITE (ounit,342)
      ELSE
         WRITE (ounit,345) rsntr
    END IF
    WRITE (ounit,420) cfvsi, cfv
    WRITE (ounit,390) siruff, ruffml
    WRITE (ounit,320) totlsi, totl
    UDensity = MatProp ('FUEL', 'ASFABDENSITY', tref) * MatProp ('FUEL', 'MWT_U_FRACTION', tref) ! (grams U/in^3)
    kgUm = cfv * (UDensity * gtokg) / totlsi
    kgUft = cfv * (UDensity * gtokg) / totl
    WRITE (ounit,321) kgUm, kgUft
    IF (.NOT. zontbl) THEN
        IF (hdish > 0.0_r8k .AND. rc(1) <= 0.0_r8k .AND. chmfrh <= 0.0_r8k) WRITE (ounit,430) pecdh
        IF (hdish <= 0.0_r8k .AND. rc(1) > 0.0_r8k .AND. chmfrh <= 0.0_r8k) WRITE (ounit,431) pecdh
        IF (hdish <= 0.0_r8k .AND. rc(1) <= 0.0_r8k .AND. chmfrh > 0.0_r8k) WRITE (ounit,432) pecdh
        IF (hdish > 0.0_r8k .AND. rc(1) > 0.0_r8k .AND. chmfrh <= 0.0_r8k) WRITE (ounit,433) pecdh
        IF (hdish > 0.0_r8k .AND. rc(1) <= 0.0_r8k .AND. chmfrh > 0.0_r8k) WRITE (ounit,434) pecdh
        IF (hdish <= 0.0_r8k .AND. rc(1) > 0.0_r8k .AND. chmfrh > 0.0_r8k) WRITE (ounit,435) pecdh
        IF (hdish > 0.0_r8k .AND. rc(1) > 0.0_r8k .AND. chmfrh > 0.0_r8k) WRITE (ounit,436) pecdh
    END IF
    IF (MAXVAL(comp) /= MINVAL(comp)) zontbl = .TRUE.
    IF (MAXVAL(enrch) /= MINVAL(enrch)) zontbl = .TRUE.
    IF (MAXVAL(gadoln) /= MINVAL(gadoln)) zontbl = .TRUE.
    IF (.NOT. zontbl) THEN
        SELECT CASE (imox)
        CASE (0)
            WRITE (ounit,335)
        CASE (1, 2)
            WRITE (ounit,336)
        END SELECT
        WRITE (ounit,330) enrch(1) 
        WRITE (ounit,337) gadoln(1) * 100.0_r8k
    END IF
    IF (imox == 1 .OR. imox == 2) THEN
        WRITE (ounit,331) enrpu39
        WRITE (ounit,332) enrpu40
        WRITE (ounit,333) enrpu41
        WRITE (ounit,334) enrpu42
    END IF
    WRITE (ounit,500) sgapf
    WRITE (ounit,510) ppmh2o
    WRITE (ounit,520) ppmn2
    WRITE (ounit,440) cplsi, cpl
    WRITE (ounit,450) dspgsi, Spring%dspg
    WRITE (ounit,460) dspgws, Spring%dspgw
    WRITE (ounit,470) vsssi, Spring%Vcold
    WRITE (ounit,480) cpvsi, cpv
    WRITE (ounit,490) Spring%vs
    WRITE (ounit,491) Spring%Vf
    WRITE (ounit,550) cvvsi, cvv
    SELECT CASE (idxgas)
    CASE (1)
        WRITE (ounit,570) fgpavs, fgpav
    CASE (2)
        WRITE (ounit,580) fgpavs, fgpav
    CASE (3)
        WRITE (ounit,590) fgpavs, fgpav
    CASE (4)
        WRITE (ounit,600) fgpavs, fgpav
    CASE (5)
        WRITE (ounit,610) fgpavs, fgpav
    CASE (6)
        WRITE (ounit,620) fgpavs, fgpav
    END SELECT
    IF (crdt > 0.0_r8k) WRITE (ounit,630) crdtsi, crdt
    IF (ivardm == 0) THEN
        IF (buin(1) > 0.0_r8k) WRITE (ounit,540) buinsi, buin(1)
        WRITE (ounit,525) pitchs, pitch
        WRITE (ounit,530) desi, de(1)
    END IF
    SELECT CASE (geom)
    CASE (0)
        WRITE (ounit,534)
    CASE (1)
        WRITE (ounit,535)
    END SELECT
    WRITE (ounit,810) modheat
    IF (ivardm /= 0 .OR. zontbl) THEN
        WRITE (ounit,639)
        WRITE (ounit,640)
        DO i = 2, na
            ii = i - 1
            buinsi = buin(i) / 1000.0_r8k
            desi = de(i) * intomm
            dcosi = dco(i) * intomm
            dcisi = dci(i) * intomm
            dpsi = dp(i) * intomm
            gp = dci(i) - dp(i)
            gpsi = gp * intomm
            gpmil = gp * 1.0e3_r8k
            WRITE (ounit,650) ii, desi, de(i), dcosi, dco(i), dcisi, dci(i), gpsi, gpmil, dpsi, dp(i), buinsi, buin(i)
        END DO
    END IF
    IF (zontbl) THEN
        SELECT CASE (imox)
        CASE (0)
            WRITE (ounit,780)
            WRITE (ounit,790)
            WRITE (ounit,795)
            DO i = 1, (na - 1)
                WRITE (ounit,800) i, gadoln(i) * 100.0_r8k, enrch(i), rcsi(i), rc(i)
            END DO
        CASE (1, 2)
            WRITE (ounit,780)
            WRITE (ounit,791)
            WRITE (ounit,796)
            DO i = 1, (na - 1)
                WRITE (ounit,801) i, gadoln(i) * 100.0_r8k, enrch(i), comp(i), rcsi(i), rc(i)
            END DO
        END SELECT
    END IF
    ii_Next = 1
    DO ii = 1, im
        IF (ii == ii_Next) THEN
            ii_Next = ii + 50
            CALL pghead
            SELECT CASE (iq)
            CASE (0)
                IF (modheat == 0) THEN
                    WRITE (ounit,660) 
                ELSE
                    WRITE (ounit,662)
                END IF
            CASE DEFAULT
                IF (modheat == 0) THEN
                    WRITE (ounit,661)
                ELSE
                    WRITE (ounit,663)
                END IF
            END SELECT
        END IF
        gosi = go(ii) * lbhrft2toksm2
        twsi = tfk(tw(ii))
        powsi = qmpy(ii) * mtoft
        qmpychan = qmpy(ii) / (1.0_r8k - modheat)
        powsichan = qmpychan * mtoft
        p2si = p2(ii) * PSItoMPa
        timehr = ProblemTime(ii) * daytohr
        IF (modheat == 0) THEN
            WRITE (ounit,670) ii, timehr, ProblemTime(ii), powsi, qmpy(ii), p2si, p2(ii), twsi, tw(ii), gosi, go(ii)
        ELSE
            WRITE (ounit,671) ii, timehr, ProblemTime(ii), powsi, qmpy(ii), powsichan, qmpychan, p2si, p2(ii),  &
              &               twsi, tw(ii), gosi, go(ii)
        END IF
    END DO
    WRITE (ounit,770) tref
    !
190 FORMAT (/)
200 FORMAT (4x,'******','pwr system  u-235 rods')
210 FORMAT (4x,'******','bwr system  u-235 rods')
220 FORMAT (4x,'******','pwr system  pu rods',2x,'comp = ',f7.4)
230 FORMAT (4x,'******','bwr system  pu rods',2x,'comp = ',f7.4)
240 FORMAT (4x,'cladding material is zircaloy-2        ')
250 FORMAT (4x,'cladding material is zircaloy-4        ')
251 FORMAT (4x,'cladding material is M5                ')
252 FORMAT (4x,'cladding material is ZIRLO(TM)         ')
253 FORMAT (4x,'cladding material is Optimized ZIRLO TM')
260 FORMAT (3x,' cladding outside diameter             cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
270 FORMAT (3x,' cladding inside diameter              cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
280 FORMAT (3x,' cladding thickness                    mm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
290 FORMAT (3x,' diametral gap thickness               mm(mils)            ',2x,1pe11.4,' (',1pe11.4,')')
300 FORMAT (3x,' fuel pellet diameter                  cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
310 FORMAT (3x,' fuel pellet length                    cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
320 FORMAT (3x,' fuel stack height                     m(ft.)              ',2x,1pe11.4,' (',1pe11.4,')')
321 FORMAT (3x,' Uranium mass per unit length          kgU/m(kgU/ft.)      ',2x,1pe11.4,' (',1pe11.4,')')
330 FORMAT (3x,' U-235 enrichment                      at% in U            ',2x,1pe11.4)
331 FORMAT (3x,' Pu-239 enrichment                     at% in Pu           ',2x,1pe11.4)
332 FORMAT (3x,' Pu-240 enrichment                     at% in Pu           ',2x,1pe11.4)
333 FORMAT (3x,' Pu-241 enrichment                     at% in Pu           ',2x,1pe11.4)
334 FORMAT (3x,' Pu-242 enrichment                     at% in Pu           ',2x,1pe11.4)
335 FORMAT (3x,' Fuel is UO2')
336 FORMAT (3x,' Fuel is mixed oxide with              wt% plutonia        ',2x,1pe11.4)
337 FORMAT (3x,' Fuel is doped with                    wt% Gd              ',2x,1pe11.4)
340 FORMAT (3x,' fuel pellet true density              percent             ',2x,1pe11.4)
342 FORMAT ('****caution, densification calculated without input of densification information')
345 FORMAT (3x,' fuel pellet resinter density chng     kg/cu.m             ',2x,1pe11.4)
350 FORMAT (3x,' fuel pellet dish depth                mm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
360 FORMAT (3x,' fuel pellet dish shoulder width       mm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
370 FORMAT (3x,' fuel pellet dish sperical radius      cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
380 FORMAT (3x,' fuel pellet core radius               mm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
390 FORMAT (3x,' fuel arithmetic mean roughness        mm(mils)            ',2x,1pe11.4,' (',1pe11.4,')')
400 FORMAT (3x,' clad arithmetic mean roughness        mm(mils)            ',2x,1pe11.4,' (',1pe11.4,')')
410 FORMAT (3x,' fuel pellet sintering temperature     K(F)                ',2x,1pe11.4,' (',1pe11.4,')')
420 FORMAT (3x,' fuel volume                           cu.m(cu.in.)        ',2x,1pe11.4,' (',1pe11.4,')')
430 FORMAT (3x,' fuel dish volume fraction                                 ',2x,1pe11.4)
431 FORMAT (3x,' fuel annulus volume fraction                              ',2x,1pe11.4)
432 FORMAT (3x,' fuel chamfer volume fraction                              ',2x,1pe11.4)
433 FORMAT (3x,' fuel dish & annulus volume fraction                       ',2x,1pe11.4)
434 FORMAT (3x,' fuel dish & chamfer volume fraction                       ',2x,1pe11.4)
435 FORMAT (3x,' fuel annulus & chamfer volume fraction                    ',2x,1pe11.4)
436 FORMAT (3x,' fuel dish, chamfer & annulus volume fraction              ',2x,1pe11.4)
440 FORMAT (3x,' plenum length                         cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
450 FORMAT (3x,' plenum spring diameter                cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
460 FORMAT (3x,' plenum spring wire diameter           mm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
470 FORMAT (3x,' plenum spring volume                  cu.m(cu.in.)        ',2x,1pe11.4,' (',1pe11.4,')')
480 FORMAT (3x,' plenum volume                         cu.m(cu.in.)        ',2x,1pe11.4,' (',1pe11.4,')')
490 FORMAT (3x,' plenum spring turns                                       ',2x,1pe11.4)
491 FORMAT (3x,' volume fraction of plenum occupied by spring              ',2x,1pe11.4)
500 FORMAT (3x,' fuel fission atoms(Xe + Kr)/100 fissions                  ',2x,1pe11.4)
510 FORMAT (3x,' fuel water concentration              ppm                 ',2x,1pe11.4)
520 FORMAT (3x,' fuel nitrogen concentration           ppm                 ',2x,1pe11.4)
525 FORMAT (3x,' fuel rod pitch                        cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
530 FORMAT (3x,' channel equivalent diameter           cm(in.)             ',2x,1pe11.4,' (',1pe11.4,')')
534 FORMAT (3x,' Rectangular array geometry is used')
535 FORMAT (3x,' Triangular array geometry is used')
540 FORMAT (3x,' initial burnup                        MWd/kgU(MWd/mtU)    ',2x,1pe11.4,' (',1pe11.4,')')
550 FORMAT (3x,' rod total void volume                 cu.m(cu.in.)        ',2x,1pe11.4,' (',1pe11.4,')')
570 FORMAT (3x,' rod internal   helium    pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
580 FORMAT (3x,' rod internal    air      pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
590 FORMAT (3x,' rod internal  nitrogen   pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
600 FORMAT (3x,' rod internal fission gas pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
610 FORMAT (3x,' rod internal    argon    pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
620 FORMAT (3x,' rod internal  mixed gas  pressure     mpa(psia)           ',2x,1pe11.4,' (',1pe11.4,')')
630 FORMAT (3x,' rod crud thickness                    mm(mils)            ',2x,1pe11.4,' (',1pe11.4,')')
639 FORMAT (3x,' Summary of Axial Dimensions')
640 FORMAT (/,8x,' axial',3x,' equiv. diam.',7x,' clad o.d.',10x, &
     & 'clad i.d.',8x,' gap(diam)',9x,' fuel o.d.',6x,' initial burnup', &
     & /,8x,' node',4x,' (mm)',3x,' (in)',7x,' (mm)',3x,' (in)',6x, &
     & ' (mm)',3x,' (in)',5x,' (mm)',4x,'(mils)',4x,' (mm)',2x,' (in)',4x, &
     & ' MWd/kgU',4x,' MWd/mtU',/)
650 FORMAT (10x,i2,5x,3(f6.2,2x,f7.4,4x),f6.3,2x,f6.2,4x,f6.2,2x,f7.4,2x,1pe10.3,2x,1pe10.3)
660 FORMAT (/,4x,' Power-time',7x,' Time',12x,'Rod Ave. Power',6x, &
     & ' Coolant Pressure',3x,' Coolant inlet temp.',6x,' Coolant', &
     & ' Mass Flux',/,7x,' step',4x,' (hrs)',4x,' (days)',5x,' (kW/m)', &
     & 2x,' (kW/ft)',5x,' (mpa)',2x,' (psia)',7x,' (K)',5x,' (F)',5x, &
     & ' (kg/s-m**2)',2x,' (lbm/hr-ft2)',/)
661 FORMAT (/,4x,' Power-time',7x,' Time',14x,'Peak Power',8x, &
     & ' Coolant Pressure',3x,' Coolant inlet temp.',6x,' Coolant', &
     & ' Mass Flux',/,7x,' step',4x,' (hrs)',4x,' (days)',5x,' (kW/m)', &
     & 2x,' (kW/ft)',5x,' (mpa)',2x,' (psia)',7x,' (K)',5x,' (F)',5x, &
     & ' (kg/s-m**2)',2x,' (lbm/hr-ft2)',/)
662 FORMAT (/,4x,' Power-time',7x,' Time',12x,'Rod Ave. Power',7x, &
     & ' Channel Power',6x,' Coolant Pressure',3x,' Coolant inlet temp.',5x,' Coolant', &
     & ' Mass Flux',/,7x,' step',4x,' (hrs)',4x,' (days)',5x,' (kW/m)', &
     & 2x,' (kW/ft)',4x,' (kW/m)',2x,' (kW/ft)',5x,' (mpa)',2x, &
     & ' (psia)',7x,' (K)',5x,' (F)',5x,' (kg/s-m**2)',2x,' (lbm/hr-ft2)',/)
663 FORMAT (/,4x,' Power-time',7x,' Time',14x,'Peak Power',9x, &
     & ' Channel Power',6x,' Coolant Pressure',3x,' Coolant inlet temp.',5x,' Coolant', &
     & ' Mass Flux',/,7x,' step',4x,' (hrs)',4x,' (days)',5x,' (kW/m)', &
     & 2x,' (kW/ft)',4x,' (kW/m)',2x,' (kW/ft)',5x,' (mpa)',2x, &
     & ' (psia)',7x,' (K)',5x,' (F)',5x,' (kg/s-m**2)',2x,' (lbm/hr-ft2)',/)
670 FORMAT (6x,i3,5x,f8.1,3x,f9.2,5x,f9.2,3x,f9.2,7x,f9.2,2x,f9.1,5x,f9.1,2x,f9.1,6x,f8.1,4x,f10.1)
671 FORMAT (6x,i3,5x,f8.1,3x,f9.2,5x,f9.2,3x,f9.2,6x,f9.2,3x,f9.2,7x,f9.2,2x,f9.1,5x,f9.1,2x,f9.1,6x,f8.1,4x,f10.1)
680 FORMAT (//,30x,' the user has specified that the following set',/,30x, &
     & ' of evaluation models will be used in the',/,30x,' analysis', &
     & ' of this fuel rod , = 0 is off , = 1, is on. ',//)
770 FORMAT(/4x,' the stored energy is normalized to',f7.2,' degrees F',/)
780 FORMAT (3x,'Summary of axial zoning')
790 FORMAT (15x,'Gd',7x,'U-235',7x,'hole radius')
791 FORMAT (15x,'Gd',7x,'U-235',7x,'Pu',7x,'hole radius')
795 FORMAT (3x,'Axial Node',2x,'(wt%)',4x,'(at%)',7x,'mm',5x,'(in.)') 
796 FORMAT (3x,'Axial Node',2x,'(wt%)',4x,'(at%)',7x,'(wt%)',4x,'mm',7x,'(in.)') 
800 FORMAT (3x,i5,6x,f6.2,3x,1pe9.2,3x,1pe9.2,' (',1pe9.2,')') 
801 FORMAT (3x,i5,6x,f6.2,3x,1pe9.2,3x,f6.2,3x,1pe9.2,' (',1pe9.2,')')
810 FORMAT (3x,' Moderator heating fraction              ',21x,1pe9.2)
    !
    END SUBROUTINE print1
    !
    !
    !
    SUBROUTINE print2
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> Subroutine print2 produces the output file data
    !
    INTEGER(ipk) :: z, i, ii, nrp1, nrp2, itm1, l, im1na, im1na2, im1, ij, iwarn, im2, im2na, im2na2, &
      &             iit, jpeak1, ii_Next, pknode
    REAL(r8k) :: wtk, tsecon, tpsec, toxo, toxok, testr, tcom, tcim, tbark, tagk, siflmp, qcm, presg, &
      &          powerm, pinsi, pinbr, oxmils, uo2llx, tsr, timday, tdays, tcl, resis, resi, radcnd, qpeakm, &
      &          qavm, oxmicr, hgapsi, hfllsi, gthcon, tsrk, totcr, tclk, sigm3b, sigm3, sigm2b, sigm2, sigm1b, &
      &          sigm1, gascnd, fulden, expnr, eps3, eps2, eps11, cpwx2, dpwx, dlrdsi, densix, deltp, &
      &          delday, delbpm, creapl, crack, congap, concnd, cgapsi, bppm, bpm, tttk, totdsi, totcrm, swlrmc, &
      &          rpm, roxm, rox, rfnci, relocs, relocm, rcom, rcohm, vva, vfrcrf, vfrcpr, vfrcgp, vfrcds, vfrccr, &
      &          tsfk, tpcak, tnterk, thvvsi, tcrkk, tcrkf, tagak, tafak, rcoh, rcihm, rcih, radm, txefr, tkrfr, &
      &          th2fr, tfgpct, tarfr, sigrab, sigra, sighpo, sighpi, sigaxb, sigax, sghpob, sghpib, exprmc, expnrm, &
      &          expnrl, epsra, epshpo, epshpi, dpwx2, denrmc, delrng, crprml, crprmc, creapm, busi, tfrk, tcak, tblkk, &
      &          sigrpo, sigrpi, sighab, sigha, sgrpob, sgrpib, fmgrpct, fdi, tblkf, srm, epsunp, epsrpo, epsrpi, &
      &          epsha, epsax, epprpo, epprpi, eppra, epphpo, epphpi, eppha, eppax, epgro, cpdltx, creap, exprml, &
      &          denrml, swlrml, pressMPa, bp, bpp, oxide
    INTEGER(ipk), DIMENSION(2) :: icltemp
    REAL(r8k), DIMENSION(20) :: aaa
    !
    iwarn = 0
    !
    ! Determine which array value to used based on whether the print is for the axial loop or summary
    SELECT CASE (PrintType)
    CASE ('Axial Loop')
        z = j - 1
        !
        nrp1 = nr + 1
        nrp2 = nr + 2
        itm1 = it - 1
        IF (it == 1) THEN
            IF (j == jpeak(1)) THEN
                dlrdsi = intomm * dlrod
                WRITE (ounit,400) dlrdsi, dlrod
                dhfll = hfll - (totl * fttoin)
                hfllsi = dhfll * intomm
                WRITE (ounit,401) hfllsi, dhfll
            END IF
            RETURN
        END IF
        IF (j == jmax + 1) THEN
            ! End of timestep loop
            buavearray(it-1) = bu
            CALL pghead
            busi = bu / 1000.0_r8k
            rfnci = rfnvff * cfv
            tagak = tfk(taga(it))
            tafak = tfk(tafa(it))
            tpcak = tfk (tpca)
            tsfk = tfk(tsfa(it))
            tnterk = tfk(tntera)
            thvv = hpv + hgv + hdshv + hcrv + rfnci + hporv + hva
            voidvolarray(it-1) = thvv
            thvvsi = thvv * in3tocm3
            tplens = tfk(tplen)
            tcrkf = 0.0_r8k
            DO i = (jmin - 1), (jmax - 1)
                tcrkf = tcrkf + (FuelTempRestruRad(i) + PelSurfTemp(i)) / 2.0_r8k * deltaz(i)
            END DO
            tcrkf = tcrkf / totl
            tcrkk = tfk(tcrkf)
            vfrcpl = hpv / thvv
            vfrccr = hcrv / thvv
            vfrcds = hdshv / thvv
            vva = hva / thvv
            vfrcpr = hporv / thvv
            vfrcrf = rfnci / thvv
            vfrcgp = hgv / thvv
            SELECT CASE (imox)
            CASE (0)
                tkrfr = 0.15_r8k * tfgfr
                txefr = 0.85_r8k * tfgfr
            CASE (1, 2)
                tkrfr = 0.0588_r8k * tfgfr
                txefr = 0.9412_r8k * tfgfr
            END SELECT
            tarfr = 0.0_r8k
            th2fr = 0.0_r8k
            tfgpct = tfgfr * 100.0_r8k
            pressMPa = press * PSItoMPa
            WRITE (ounit,780) itm1
            WRITE (ounit, 790) busi, bu, thefr, amfhe, th2fr, amfh2
            WRITE (ounit,810) tplens, tplen, vfrcpl, tn2fr, amfn2
            WRITE (ounit,820) tagak, taga(it), vfrcgp, tarfr, amfarg

            SELECT CASE (ngasmod)
            CASE (1, 2, 3)
                WRITE (ounit,830) tsfk, tsfa(it), vfrcrf, tkrfr, amfkry
                IF (hdish > 0.0_r8k) WRITE (ounit,840) tafak, tafa(it), vfrcds, txefr, amfxe
                IF (rc(1) > 0.0_r8k .AND. hdish == 0.0_r8k) WRITE (ounit,842) tpcak, tpca, vva, txefr, amfxe
            CASE (4)
                WRITE (ounit,831) tsfk, tsfa(it), vfrcrf, th2ofr, amfh2o
                IF (hdish > 0.0_r8k) WRITE (ounit,841) tafak, tafa(it), vfrcds
                IF (rc(1) > 0.0_r8k .AND. hdish == 0.0_r8k) WRITE (ounit,843) tpcak, tpca, vva
            END SELECT
            IF (hdish == 0.0_r8k .AND. rc(1) == 0.0_r8k) WRITE (ounit,853) tnterk, tntera, vfrcds, txefr, amfxe

            SELECT CASE (ngasmod)
            CASE (1, 2, 3)
                WRITE (ounit,850) tafak, tafa(it), vfrcpr, th2ofr, amfh2o
            CASE (4)
                WRITE (ounit,851) tafak, tafa(it), vfrcpr
            END SELECT
            
            WRITE (ounit,852) tcrkk, tcrkf, vfrccr

            IF (hdish == 0.0_r8k .AND. rc(1) > 0.0_r8k) WRITE (ounit,854) tnterk, tntera, vfrcds
            IF (rc(1) > 0.0_r8k .AND. hdish > 0.0_r8k) WRITE (ounit,855) tpcak, tpca, vva

            SELECT CASE (ngasmod)
            CASE (1, 2, 3)
                WRITE (ounit,800) thvvsi, thvv, gasmo(itm1), pressMPa, press, tfgpct
            CASE (4)
                WRITE (ounit,800) thvvsi, thvv, gasmo(itm1), pressMPa, press, SUM(RB_rod(:,it))*100.0_r8k
                ! Output gas release fractions based on ANS 5.4 2011 (by G. Longoni, April 2015)
                WRITE (ounit,'(/,32x,a,2x,a)') 'ANS 5.4 2011 - Fuel Rod Cumulative Fission Gas Release Fractions (Release/Birth)'
                WRITE (ounit,'(/,32x,a,2x,a)') 'Short Lived Nuclides (Half Life <6 h)', &
                  &                            '| Long Lived Nuclides (6 h < Half Life < 60 days)'
                WRITE (ounit,'(/,40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-135m',RB_rod(1,it),'Xe-133',RB_rod(12,it)
                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-137',RB_rod(2,it),'Xe-135',RB_rod(13,it)
                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-138',RB_rod(3,it),'I-131',RB_rod(14,it)
                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-139',RB_rod(4,it),'I-133',RB_rod(15,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-85m',RB_rod(5,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-87',RB_rod(6,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-88',RB_rod(7,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-89',RB_rod(8,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-90',RB_rod(9,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-132',RB_rod(10,it)
                WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-134',RB_rod(11,it)
                WRITE (ounit,'(/,41x,a8,1x,es12.2,18x,a8,es12.2)') 'Total ::',SUM(RB_rod(1:11,it)),'Total ::',SUM(RB_rod(12:15,it))
                WRITE (ounit,*)
            END SELECT          
            WRITE (ounit,940)
            SELECT CASE (mechan)
            CASE (1)
                WRITE (ounit,920)
                WRITE (ounit,860)
            CASE (2)
                WRITE (ounit,910)
                WRITE (ounit,860)
            END SELECT
            DO i = (jmin - 1), (jmax - 1)
                im1 = i - 1
                im1na = im1 + na
                im1na2 = im1na + na
                sigra = sig(i,3) * PSItoMPa
                sigrab = sig(i,3)
                sigax = sig(i,2) * PSItoMPa
                sigaxb = sig(i,2)
                sighpo = sig(i,1) * PSItoMPa
                sighpi = sighpo
                sghpob = sig(i,1)
                sghpib = sghpob
                epshpo = eps(i,1) * 1.0e2_r8k
                epshpi = epshpo
                epsra = eps(i,3) * 1.0e2_r8k
                epsax = eps(i,2) * 1.0e2_r8k
                WRITE (ounit,870) i,sigra,sigrab,sighpi,sghpib,sighpo,sghpob,sigax, &
                  &               sigaxb,epsra,epshpi,epshpo,epsax
            END DO
            WRITE (ounit,903)
903         FORMAT (/,' cladding H2 concentration and its effect on ductility')
            WRITE (ounit,905)
905         FORMAT (/,1x,'axial',9x,'H2',10x,'excess H2',6x,'uniform',8x,'Fuel',11x,'Oxide layer',15x,'Fast', &
              &    7x,'Cumulative Fission Gas')
            WRITE (ounit,907)
907         FORMAT (' region   concentration  concentration    strain',9x,'Duty',12x,'thickness',14x,'Fluence',9x,'Release,')
            WRITE (ounit,908)
908         FORMAT (15x,'ppm',13x,'ppm',10x,'%',12x,'Index',9x,'mils   (microns)',9x, 'n/m^2',11x,'percent')
            DO i = jmin, jmax
                oxmils = EOSZrO2Thk(i-1) * 12000.0_r8k
                oxmicr = oxmils * intomm
                im1 = i - 1
                fmgrpct = rdotwrt(im1) * 100.0_r8k
                epsunp = 100.0_r8k * UniformAxNodStrn(im1)
                IF (epsunp < 0.0_r8k) THEN
                    epsunp = 0.0_r8k
                    iwarn = 1
                END IF
                !
                IF (it == 1) THEN
                    FDItave(im1) = MAX(580.0_r8k, SurfTempOxide(im1))
                ELSE
                    FDItave(im1) = (FDItave(im1) * ProblemTime(it-1) + MAX(580.0_r8k, &
                      &             SurfTempOxide(im1)) * (ProblemTime(it) - ProblemTime(it-1))) / ProblemTime(it)
                END IF
                fdi = (MAX(0.0_r8k, (FDItave(im1) - 580.0_r8k) / 100.0_r8k * ProblemTime(it) * sectohr / 1000.0_r8k)) ** 2
                SELECT CASE (ngasmod)
                CASE (1, 2, 3)
                    WRITE (ounit,909) im1, CladH2Concen(im1), ExcessH2Concen(im1), epsunp, fdi, &
                      &               oxmils, oxmicr, FastFluence(i-1), fmgrpct
                CASE (4)
                    WRITE (ounit,909) im1, CladH2Concen(im1), ExcessH2Concen(im1), epsunp, fdi, &
                      &               oxmils, oxmicr, FastFluence(i-1), SUM(RB_axial(:,im1))*100.0_r8k
                END SELECT
909             FORMAT (2x,i3,6x,f8.2,8x,f8.2,6x,f6.3,6x,f8.2,9x,f7.4,1x,'(',f7.3,')', 7x, e11.4, 7x, f6.2)
            END DO
            IF (iwarn == 1) WRITE (ounit,906)
906         FORMAT ('***Warning:  Calculated values of uniform strain were negative.  They have been set equal to 0.0.  ***')
            iwarn = 0
            WRITE (ounit,880)
            DO i = jmin, jmax
                im1 = i - 1
                im2 = i - 2
                im2na = im2 + na
                im2na2 = im2na + na
                eppra = epp(i-1,3) * 100.0_r8k
                eppax = epp(i-1,2) * 100.0_r8k
                epphpi = epp(i-1,1) * 100.0_r8k
                epphpo = epphpi
                ij = nr * i - 1
                cpdltx = StoredEnergy(im1) * BTUlbtoJkg
                tcak = tfk(CladAveTemp(im1))
                tbark = tfk(PelAveTemp(im1))
                tfrk = tfk(tmpfuel(nr,im1))
                tblkf = (BulkCoolantTemp(im1) + BulkCoolantTemp(i)) / 2.0_r8k
                tblkk = tfk(tblkf)
                epgro = CladIrradGrowStrn(i-1) * 100.0_r8k
                WRITE (ounit,890) im1, tfrk, tmpfuel(nr,im1), tbark, PelAveTemp(im1), tcak, CladAveTemp(im1),  &
                     &              tblkk, tblkf, cpdltx, StoredEnergy(im1), eppra, epphpi, epphpo, eppax, epgro
                IF (tfrk > ftmelt) THEN
                    imelt = 1
                    WRITE (ounit,331) im1, tfrk, ftmelt
                    WRITE (0,331) im1, tfrk, ftmelt
331                 FORMAT ('Fuel has melted.  FRAPCON-4 is not validated beyond melting point.',/, &
                      &     'Centerline temperature at node ',i2,' is', f10.2,'K.',/, &
                      &     'Fuel melting temperature is ',f10.2,'K.',/,'Code execution continuing.')
                END IF
                IF (tcak > ctmelt) THEN
                    WRITE (ounit,332) im1, tcak, ctmelt
                    WRITE (0,332) im1, tcak, ctmelt
332                 FORMAT ('Cladding has melted.  FRAPCON-4 is not validated beyond melting point.',/, &
                      &     'Cladding average temperature at node',i2,' is', f10.2,'K.',/, &
                      &     'Cladding melting temperature is ',f10.2,'K.',/,'Code execution stopped.')
                    STOP
                END IF
            END DO
            dlrdsi = dlrod * intomm
            WRITE (ounit,750) dlrdsi, dlrod
            dhfll = hfll - (totl * fttoin)
            hfllsi = dhfll * intomm
            WRITE (ounit,751) hfllsi, dhfll
        ELSE
            ! Print data for the axial node
            IF (j == jpeak(1) .OR. jdlpr /= 1) THEN
                IF (jdlpr >= 0) CALL pghead
            END IF
            fulden = den * 0.01_r8k
            tpsec = ProblemTime(it-1)
            IF (tpsec <= 0.001_r8k) tpsec = 0.0_r8k
            tsecon = ProblemTime(it)
            qcm = qc(j-1) * Bhft2toWm2
            !
            ! These adjustments by 10 should be removed when the cause is found_frapcon.
            !
            bpp = BOSNodeburnup(j-1) / 10.0_r8k
            bp = EOSNodeburnup(j-1) / 10.0_r8k
            buarray(it-1,j-1) = bp * 10.0_r8k
            fuelburntot(j-1) = buarray(it-1,j-1)
            delbp = StepNodeburnup(j-1) / 10.0_r8k
            !
            bpm = bp / 1000.0_r8k
            bppm = bpp / 1000.0_r8k
            delbpm = delbp / 1000.0_r8k
            wt = (BulkCoolantTemp(j) + BulkCoolantTemp(j-1)) / 2.0_r8k
            wtk = tfk(wt)
            oxide = EOSZrO2Thk(j-1) * fttoin
            oxmils = oxide * 1000.0_r8k
            oxidelayer(j-1) = oxmils
            oxmicr = oxmils * intomm
            hflmp = FilmCoefficient(j-1)
            siflmp = hflmp * Bhft2FtoWm2K
            toxo = SurfTempOxide(j-1)
            toxok = tfk(toxo)
            tci = CladInSurfTemp(j-1)
            tco = CladOutSurfTemp(j-1)
            tcom = tfk(tco)
            tcim = tfk(tci)
            hgapt = TotalHgap(j-1)
            hsolid = SolidHgap(j-1)
            hgap = GasHgap(j-1)
            hgapr = RadHgap(j-1)
            hgapsi = hgapt * Bhft2FtoWm2K
            gapHTC(j-1) = hgapsi
            testr = 0.0_r8k
            pinbr = MAX(RinterfacPress(j-1), testr)
            pinsi = pinbr * PSItoMPa
            crack = FuelCondFactor(j-1)
            tagk = tfk(GapAveTemp(j-1))
            presg = press * PSItoPa
            cgapsi = MatProp ('GAS', 'THERMCOND', tagk)
            congap = cgapsi * WmKtoBhftF
            tbark = tfk(PelAveTemp(j-1))
            Powerm = Power(j-1) * mtoft
            tdays = ProblemTime(itm1) * sectoday
            qpeakm = qpeak * mtoft
            qavm = qav * mtoft
            uo2llx = 0.0_r8k
            dpwx = 0.0_r8k
            dpwx2 = 0.0_r8k
            densix = 0.0_r8k
            gascnd = hgap / hgapt
            concnd = hsolid / hgapt
            radcnd = hgapr / hgapt
            resi = 1.0_r8k / hgapsi
            resis = 1.0_r8k / hgapt
            timday = tsecon * sectoday
            tdays = tpsec * sectoday
            deltp = tsecon - tpsec
            delday = deltp * sectoday
            tsr = tmpfuel(1,j-1)
            tcl = tmpfuel(nr,j-1)
            cltemparray(it-1,j-1) = tcl
            storedearray(it-1,j-1) = StoredEnergy(j-1)
            tclk = tfk(tcl)
            tsrk = tfk(tsr)
            eps11 = eps(j-1,1) * 100.0_r8k
            eps2 = eps(j-1,2) * 100.0_r8k
            eps3 = eps(j-1,3) * 100.0_r8k
            totcr = eps11
            creap = eppsv(j-1,1) * 100.0_r8k
            expnr = totcr - creap
            sigm1b = sig(j-1,1) / 1.0e3_r8k
            sigm2b = sig(j-1,2) / 1.0e3_r8k
            sigm3b = sig(j-1,3) / 1.0e3_r8k
            sigm1 = sigm1b * PSItoKPa
            sigm2 = sigm2b * PSItoKPa
            sigm3 = sigm3b * PSItoKPa
            crack = 1.0_r8k
            strainarray(it-1,j-1) = eps11
            creaparray(it-1,j-1) = epp(j-1,1)
            IF (it == 2) THEN
                straindiffarray(it-1,j-1) = 0.0_r8k
                creapratearray(j-1) = creaparray(it-1,j-1) / ProblemTime(it)
            ELSE
                straindiffarray(it-1,j-1) = strainarray(it-1,j-1) - strainarray(it-2,j-1)
                creapratearray(j-1) = (creaparray(it-1,j-1) - creaparray(it-2,j-1)) / (ProblemTime(it) - ProblemTime(it-1))
            END IF
            creapl = creap * (rco - 0.5_r8k * tcc) * 10.0_r8k
            cladcrptot(j-1) = creapl
            creapm = creapl * intomm
            expnrl = expnr * (rco - 0.5_r8k * tcc) * 10.0_r8k + eps3 * 0.5_r8k * tcc * 10.0_r8k
            expnrm = expnrl * intomm
            totcrl(j-1) = creapl + expnrl
            totcrm = totcrl(j-1) * intomm
            rcih = CladDiamHot(j-1) / 2.0_r8k
            rcoh = rcih + tcc * (1.0_r8k + eps3 / 100.0_r8k)
            rox = rcoh + oxide
            rcihm = rcih * intocm
            rcohm = rcoh * intocm
            roxm = rox * intocm
            !
            DO i = 2, nr
                l = nr + 1 - i
                ij = nr * (j-1) + l - 1
                delrng = (crad(l,j-1) - crad(l+1,j-1))
                uo2llx = uo2llx + delrng * uo2exp(l,j-1)
                IF (i == 1) uo2llx = uo2llx + delrng * uo2exp(l,j-1)
                dpwx = dpwx + delrng * dpw(l,j-1)
                densix = densix + delrng * densf(l,j-1)
            END DO
            dpwxarray(it-1,j-1) = dpwx / crad(1,j-1)
            IF (it == 2) THEN
                dpwxrate(j-1) = dpwxarray(it-1,j-1) / ProblemTime(it)
            ELSE
                dpwxrate(j-1) = (dpwxarray(it-1,j-1) - dpwxarray(it-2,j-1)) / (ProblemTime(it) - ProblemTime(it-1))
            END IF
            rpm = rp * intocm
            rcom = rco * intocm
            denrml = densix * 1000.0_r8k
            fueldentot(j-1) = denrml   ! Total densification for axial node
            denrmc = denrml * intomm
            swlrml = dpwx * 1000.0_r8k
            fuelswltot(j-1) = swlrml   ! Total swelling for axial node
            swlrmc = swlrml * intomm
            exprml = uo2llx * 1000.0_r8k
            fuelexptot(j-1) = exprml   ! Total thermal expansion for axial node
            exprmc = exprml * intomm
            relocm = Relocation(j-1) * 1000.0_r8k
            relocm_true = relocm
            totdef(j-1) = denrml + swlrml + exprml + relocm
            totinner(j-1) = eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k
            gapmech(j-1) = thkgap(j-1) * 1000.0_r8k - totdef(j-1) + totinner(j-1)
            gapthrm(j-1) = cgapsi / (hgapsi * gascnd) * mtoin * 1000.0_r8k
            relocm = relocm + gapmech(j-1) - gapthrm(j-1)
            relocm_mod = relocm
            fuelrelmod(j-1) = relocm
            relocs = relocm * intomm
            totdef(j-1) = denrml + swlrml + exprml + relocm
            gapmech(j-1) = thkgap(j-1) * 1000.0_r8k - (denrml + swlrml + exprml + 0.5_r8k * relocm_true) + totinner(j-1)
            gapplot(j-1) = thkgap(j-1) * 1000.0_r8k - totdef(j-1) + totinner(j-1)
            ! colddef = cold displacement of fuel radius (inches) (passed to rstfs)
            colddef(z) = (totdef(j-1) - relocm - exprml) / 1000.0_r8k
            totdsi = totdef(j-1) * intomm
            IF (j /= jpeak(1) .AND. jdlpr == 1) RETURN
            IF (jdlpr < 0) RETURN
            WRITE (ounit,410) j-1, itm1
            WRITE (ounit,420) qavm, qav
            WRITE (ounit,430) Powerm, Power(j-1), qcm, qc(j-1)
            WRITE (ounit,440) qpeakm, qpeak
            WRITE (ounit,450) tdays, tpsec, bppm, bpp
            WRITE (ounit,460) delday, deltp, delbpm, delbp
            WRITE (ounit,470) timday, tsecon, bpm, bp
            WRITE (ounit,480)
            radm = hrad(nr,j-1) * intocm
            WRITE (ounit,490) radm, hrad(nr,j-1), tclk, tcl, rapow(nr,j-1)
            DO ii = 1, (nr - 2)
                i = nr - ii
                tttk = tfk(tmpfuel(i,j-1))
                radm = hrad(i,j-1) * intocm
                WRITE (ounit,510) radm, hrad(i,j-1), tttk, tmpfuel(i,j-1), rapow(i,j-1)
            END DO
            radm = hrad(1,j-1) * intocm
            PelletRad(j-1) = hrad(1,j-1)
            WRITE (ounit,540) radm, hrad(1,j-1), tsrk, tsr, rapow(1,j-1)
            WRITE (ounit,550) rcihm, rcih, tcim, tci
            WRITE (ounit,560) rcohm, rcoh, tcom, tco
            WRITE (ounit,570) roxm, rox, toxok, toxo
            WRITE (ounit,580) wtk, wt
            WRITE (ounit,590) rp, rpm
            WRITE (ounit,600) cgapsi, sigm3, sigm3b, eps3, congap, sigm2, sigm2b, eps2
            WRITE (ounit,610) sigm1, sigm1b, eps11
            WRITE (ounit,620) denrml, denrmc, hgapsi
            WRITE (ounit,630) swlrml, swlrmc, hgapt
            WRITE (ounit,640) exprml, exprmc
            WRITE (ounit,650) relocm, relocs, resi, resis, totdef(j-1), totdsi, siflmp, hflmp, crdtt(j-1)
            WRITE (ounit,660) rco, rcom
            WRITE (ounit,680) gascnd
            WRITE (ounit,700) creapl, creapm, concnd
            ! define colddec= cladding permanent displacement (radial inches) for FRAPTRAN restart file
            colddec(z) = creapl / 1000.0_r8k
            WRITE (ounit,720) expnrl, expnrm, radcnd
            WRITE (ounit,741) totcrl(j-1), totcrm, pinsi, pinbr
            totinner(j-1) = eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k
            cladinpermdef(j-1) = (eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k) * 0.001_r8k
            WRITE (ounit,742) rci, rci * intocm, totinner(j-1), totinner(j-1) * intomm
        END IF
    CASE ('Summary')
        !
        ! Summary report
        !
        jpeak(1) = jfix
        z = jfix
        ii_Next = 2
        DO ii = 2, im
            IF (ii == ii_Next) THEN
                ii_Next = ii + 50
                CALL pghead
                WRITE (ounit,960)
                IF (nunits == 1) WRITE (ounit,970)
                IF (nunits == 0) WRITE (ounit,980)
            END IF
            iit = ii - 1
            jpeak1 = jpeak(jst(ii)) - 1
            aaa = 0.0_r8k
            SELECT CASE (nunits)
            CASE (0)
                aaa(1) = ProblemTime(ii) * sectoday
                aaa(2) = pkBurnup(iit) * 1.0e-3_r8k
                aaa(3) = pkPower(iit) * mtoft
                aaa(4) = tfk(pkODCladTemp(iit))
                aaa(5) = tfk(pkAveCladTemp(iit))
                aaa(6) = tfk(pkIDCladTemp(iit))
                aaa(7) = pkGap(iit) * intomm
                aaa(8) = pkFisGasRelFrac(iit) * 100.0_r8k
                aaa(9) = tfk(pkPelSurfTemp(iit))
                aaa(10) = tfk(pkPelAveTemp(iit))
                aaa(11) = tfk(pkPelCentTemp(iit))
                aaa(12) = pkIntefacePres(iit) * PSItoMPa
                aaa(13) = pkHoopStres(iit) * PSItoMPa
                aaa(14) = pkAxlStres(iit) * PSItoMPa
                aaa(15) = pkHoopStrain(iit) * 100.0_r8k
                aaa(16) = pkFuelPelOD(iit) * intomm
                aaa(17) = pkGapCond(iit) * Bhft2FtoWm2K
                aaa(18) = pit(iit) * PSItoMPa
                aaa(19) = pkZrO2(iit) * intomm
                aaa(20) = pkH2up(iit)
                SELECT CASE (ngasmod)
                CASE (1, 2, 3)
                    WRITE (ounit,1000) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), aaa(8), aaa(19), aaa(20)
                CASE (4)
                    WRITE (ounit,1000) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), &
                      &                SUM(RB_rod(:,ii))*100.0_r8k, aaa(19), aaa(20)
                END SELECT
            CASE (1)
                aaa(1) = ProblemTime(ii) * sectoday
                aaa(2) = pkBurnup(iit) * 1.0e-3_r8k
                aaa(3) = pkPower(iit)
                aaa(4) = pkODCladTemp(iit)
                aaa(5) = pkAveCladTemp(iit)
                aaa(6) = pkIDCladTemp(iit)
                aaa(7) = pkGap(iit)
                aaa(8) = pkFisGasRelFrac(iit) * 100.0_r8k
                aaa(9) = pkPelSurfTemp(iit)
                aaa(10) = pkPelAveTemp(iit)
                aaa(11) = pkPelCentTemp(iit)
                aaa(12) = pkIntefacePres(iit)
                aaa(13) = pkHoopStres(iit)
                aaa(14) = pkAxlStres(iit)
                aaa(15) = pkHoopStrain(iit) * 100.0_r8k
                aaa(16) = pkFuelPelOD(iit)
                aaa(17) = pkGapCond(iit)
                aaa(18) = pit(iit)
                aaa(19) = pkZrO2(iit)
                aaa(20) = pkH2up(iit)
                SELECT CASE (ngasmod)
                CASE (1, 2, 3)
                    WRITE (ounit,990) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), aaa(8), aaa(19), aaa(20)
                CASE (4)
                    WRITE (ounit,990) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), &
                      &               SUM(RB_rod(:,ii))*100.0_r8k, aaa(19), aaa(20)
                END SELECT
            END SELECT
        END DO
        srm = 100.0_r8k
        DO i = 1, im
            srm = MIN(pkHoopStrain(i-1), srm)
        END DO
        CALL pghead
        WRITE (ounit,1010) ABS(pkHoopStrain(im-1) - srm)
        SELECT CASE (ngasmod)
        CASE (1, 2, 3)
            WRITE (ounit,1020) tfgfr
        CASE (4)
            WRITE (ounit,1020) SUM(RB_rod(:,im))
            ! Output gas release fractions based on ANS 5.4 2011 (by G. Longoni, April 2015)
            WRITE (ounit,'(/,32x,a,2x,a)') 'ANS 5.4 2011 - Fuel Rod Cumulative Fission Gas Release Fractions (Release/Birth)'
            WRITE (ounit,'(/,32x,a,2x,a)') 'Short Lived Nuclides (Half Life <6 h)', &
              &                            '| Long Lived Nuclides (6 h < Half Life < 60 days)'
            WRITE (ounit,'(/,40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-135m',RB_rod(1,im),'Xe-133',RB_rod(12,im)
            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-137',RB_rod(2,im),'Xe-135',RB_rod(13,im)
            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-138',RB_rod(3,im),'I-131',RB_rod(14,im)
            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-139',RB_rod(4,im),'I-133',RB_rod(15,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-85m',RB_rod(5,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-87',RB_rod(6,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-88',RB_rod(7,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-89',RB_rod(8,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-90',RB_rod(9,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-132',RB_rod(10,im)
            WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-134',RB_rod(11,im)
            WRITE (ounit,'(/,41x,a8,1x,es12.2,18x,a8,es12.2)') 'Total ::',SUM(RB_rod(1:11,im)),'Total ::',SUM(RB_rod(12:15,im))
        END SELECT
        IF (icm <= 7) WRITE (ounit,1030) pkZrO2WtGain
        ! Ouputs of regulatory interest
        ! Rod internal pressure
        ! Set peak node location. The - 1 is because the array starts at node 0_frapcon.
        pknode = MAXLOC(pit, DIM=1) - 1
        SELECT CASE (ngasmod)
        CASE (1, 2, 3)
            WRITE (ounit,2001) cvv, MAXVAL(pit), pkBurnup(pknode) / 1000.0_r8k, &
              &                 buavearray(pknode) / 1000.0_r8k, voidvolarray(pknode), &
              &                 pkFisGasRelFrac(pknode) * 100.0_r8k, ProblemTime(pknode+1) * sectoday
        CASE (4)
            WRITE (ounit,2001) cvv, MAXVAL(pit), pkBurnup(pknode) / 1000.0_r8k, &
              &                 buavearray(pknode) / 1000.0_r8k, voidvolarray(pknode), &
              &                 SUM(RB_rod(:,im)) * 100.0_r8k, ProblemTime(pknode+1) * sectoday
        END SELECT
        ! Centerline temp
        icltemp = MAXLOC(cltemparray)
        WRITE (ounit,2002) MAXVAL(cltemparray), icltemp(2), storedearray(icltemp(1), icltemp(2)), &
          &               buarray(icltemp(1), icltemp(2)) / 1000.0_r8k, buavearray(icltemp(1)) / 1000.0_r8k, &
          &               ProblemTime(icltemp(1) + 1) * sectoday
        ! Strain increment
        icltemp = MAXLOC(straindiffarray)
        WRITE (ounit,2003) MAXVAL(straindiffarray), icltemp(2), buarray(icltemp(1), icltemp(2)) / 1000.0_r8k, &
          &               buavearray(icltemp(1)) / 1000.0_r8k, ProblemTime(icltemp(1) + 1) * sectoday
        ! Fuel melting
        IF (imelt == 1) THEN
            WRITE (ounit,2004)
        ELSE
            WRITE (ounit,2005)
        END IF
    END SELECT
    !
400 FORMAT (10x,'zero power cladding axial expansion',' (room temperature reference), mm(in)',f10.2,3x,'(',f7.4,')'/)
401 FORMAT (10x,'zero power fuel axial expansion',35x,'mm(in)',f10.2,3x,'(',f7.4,')'/)
410 FORMAT (/,40x,' axial region number ',i4,10x,' power-time',' step' ,i5,/)
420 FORMAT (3x,'avg.  linear heat rating, kW/m(kW/ft)',3x,f6.2,'(',f6.2,')')
430 FORMAT (3x,'local linear heat rating, kW/m(kW/ft)',3x,f6.2,'(',f6.2, &
      &     ')',10x,' rod surface heat flux, W/m**2(btu/hr-ft**2)',2x,1pe9.2,'(',1pe9.2,')')
440 FORMAT (3x,'peak  linear heat rating, kW/m(kW/ft)',3x,f6.2,'(',f6.2,')'/)
450 FORMAT (3x,'step starts at time, days(sec)  ',f7.2,'(',1pe9.2,')', &
      &     15x,'starting burnup,  MWd/kgU(MWd/mtU)   ',f5.2,'(',f7.1,')')
460 FORMAT (3x,'time increment,      days(sec)  ',f7.2,'(',1pe9.2,')', &
      &     15x,'burnup increment, MWd/kgU(MWd/mtU)   ',f5.2,'(',f7.1,')')
470 FORMAT (3x,'end step,            days(sec)  ',f7.2,'(',1pe9.2,')', &
      &     15x,'end step burnup,  MWd/kgU(MWd/mtU)   ',f5.2,'(',f7.1,')')
480 FORMAT (/52x,'Radial temperature and power distribution' &
      &     //29x,27x,'Radii , cm(in)',17x,'Temperature, K(F)',3x,'Power profile'/)
490 FORMAT (27x,'fuel--    center      ',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')',6x,f7.4)
510 FORMAT (27x,'fuel--                ',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')',6x,f7.4)
540 FORMAT (27x,'fuel-- outer surface  ',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')',6x,f7.4/)
550 FORMAT (27x,'cladding inner surface',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')')
560 FORMAT (27x,'cladding outer surface',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')'/)
570 FORMAT (27x,'oxide surface         ',5x,f8.5,1x,'(',f8.5,')',16x,f6.1,1x,'(',f6.1,')'/)
580 FORMAT (27x,'coolant temperature   ',40x,f6.1,1x,'(',f6.1,')'//)
590 FORMAT (3x,'radial geometry changes    ',8x,'             interface', &
      &     ' conditions          ',14x,' stresses, mpa(ksi)  ',9x,'strains, pct '/ &
      &     3x,'initial fuel radius ',f7.4,' in. (',f7.4,' cm)')
600 FORMAT (3x,'change in fuel radius',27x,'gas conductivity, W/m-K',8x,1pe9.2, &
      &     5x,'radial  ',0pf7.2,'(',f6.2,')',5x,f6.3/3x,'for thermal model due to:', &
      &     34x,'(btu/hr-ft-F)',6x,'(',1pe9.2,')',4x,'axial',3x,0pf7.2,'(',f6.2,')',5x,f6.3)
610 FORMAT (23x,'mils         microns',53x,'hoop',4x,f7.2,'(',f6.2,')',5x,f6.3)
620 FORMAT (5x,' densification',3x,e11.4,2x,e11.4,5x,'gap conductance, W/m2-K',8x,1pe9.2)
630 FORMAT (5x,' swelling',8x,e11.4,2x,e11.4,15x,'(btu/hr-ft2-F)',6x,'(',1pe9.2,')')
640 FORMAT (5x,' therm expansion',1x,e11.4,2x,e11.4)
650 FORMAT (5x,' relocation',6x,e11.4,2x,e11.4,5x &
      &     ,'interface resistance, m2-K/W',3x,e9.2 &
      &     /22x,'-----------  -----------',20x,'(hr-ft2-F/btu)',1x,'(',1pe9.2,')' &
      &     /5x,'Total     ',7x,e11.4,2x,e11.4 &
      &     /51x,'clad-coolant film   W/m2-K',4x,e10.2 &
      &     /51x,'coefficient  (btu/hr-ft2-F)   (',e9.2,')' &
      &     /50x,'(Includes effect of ',f6.3,' mil crud layer)')
660 FORMAT (/3x,'initial cladding outer radius'/23x,f7.4,' in. (',f7.4,' cm)' &
      &     /'   change in cladding outside radius due to:',7x,'fraction of conductance due to:')
680 FORMAT (23x,'mils         microns',8x,'gas conductance',6x,f6.3)
700 FORMAT (5x,' permanent  ',5x,e11.4,2x,e11.4,5x,'contact    ',10x,f6.3)
720 FORMAT (5x,' recoverable',5x,e11.4,2x,e11.4,5x,'radiation  ',10x,f6.3/22x,'-----------  -----------')
740 FORMAT (5x,'Total  ',10x,e11.4,2x,e11.4,5x,'interface pressure:' &
      &     ,'       mpa',6x,f6.1,10x,'cracked fuel thermal conductivity' &
      &     /74x,'(psi)',3x,'(',f7.1,')',16x,'degradation factor:',f7.3/)
741 FORMAT (5x,'Total  ',10x,e11.4,2x,e11.4,5x,'interface pressure,' &
      &     ,' mpa',6x,f6.1,/70x,'(psi)',3x,'(',f7.1,')')
742 FORMAT (3x,'initial cladding inner radius'/23x,f7.4,' in. (',f7.4,' cm)',/, &
      &     3x,'change in cladding inside radius is ',f7.3,' mils (',f6.2,' microns)',/)
750 FORMAT (/10x,'cladding axial expansion (room temperature reference)', &
      &     ', mm(in)',f10.2,3x,'(',f7.4,')',3x,'(INCLUDING IRRAD GROWTH)')
751 FORMAT (10x,'fuel axial expansion',35x,'mm(in)',f10.2,3x,'(',f7.4,')')
760 FORMAT (3x,'plenum',79x,f5.2,6x,f5.2,3x,f5.2,5x,f5.2)
770 FORMAT (3x,'plenum',3x,f6.1,'(',f7.1,')',4x,f6.1,'(',f7.1,')',1x,f6.1, &
      &     '(',f7.1,')',4x,f6.1,'(',f7.1,')',7x,f5.2,6x,f5.2,3x,f5.2,5x,f5.2)
780 FORMAT (/56x,' power-time step',i4)
790 FORMAT (/10x,'burnup to end of time step, MWd/kgU (MWd/mtU)',2x,f6.2,1x,'(',f7.1,')',24x,'gas composition',//, &
      &      16x,'components of internal void and temperatures',20x,'gas',8x,'cumulative fraction',5x,'current mole',/, &
      &      79x,'component',10x,'released,',12x,'fraction',/, &
      &      36x,'associated',12x,'fraction',/, &
      &      13x,'item',16x,'temperature, K(F)',6x,'total volume',13x,'he',16x,f6.3,15x,f6.3,/, &
      &      81x,'h',17x,f6.3,15x,f6.3)
800 FORMAT (/12x,' total void volume, cu.cm(cu.in.)',7x,f7.2,'(',f6.3 &
      &     ,')',7x,'cumulative quantity of free gas in the rod, moles',2x,1pe9.2, &
      &     /,12x,' rod internal gas pressure, mpa(psia)',4x,0pf6.3,'(',f6.1, &
      &     ')',7x,'cumulative fission gas release, percent',11x,f6.2//)
810 FORMAT (13x,'plenum           ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'n  ',15x,f6.3,15x,f6.3)
820 FORMAT (13x,'gap              ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'ar ',15x,f6.3,15x,f6.3)

830 FORMAT (13x,'roughness        ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'kr ',15x,f6.3,15x,f6.3)
831 FORMAT (13x,'roughness        ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'h2o',15x,f6.3,15x,f6.3)

840 FORMAT (13x,'dishes           ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'xe ',15x,f6.3,15x,f6.3)
841 FORMAT (13x,'dishes           ',5x,f6.1,'(',f6.1,')',10x,f6.3)

842 FORMAT (13x,'annulus          ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'xe ',15x,f6.3,15x,f6.3)
843 FORMAT (13x,'annulus          ',5x,f6.1,'(',f6.1,')',10x,f6.3)

850 FORMAT (13x,'porosity         ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'h2o',15x,f6.3,15x,f6.3)
851 FORMAT (13x,'porosity         ',5x,f6.1,'(',f6.1,')',10x,f6.3)

852 FORMAT (13x,'crack            ',5x,f6.1,'(',f6.1,')',10x,f6.3)
853 FORMAT (13x,'interface        ',5x,f6.1,'(',f6.1,')',10x,f6.3,16x,'xe ',15x,f6.3,15x,f6.3)
854 FORMAT (13x,'pellet interface ',5x,f6.1,'(',f6.1,')',10x,f6.3)
855 FORMAT (13x,'annulus volume   ',5x,f6.1,'(',f6.1,')',10x,f6.3)

860 FORMAT (13x,'---------------------cladding stresses, mpa(psi)------', &
      &     '----------------',9x,'-----cladding strains,percent-----',/,3x,'axial' &
      &     ,9x,'radial',23x,'hoop',23x,'axial',14x,'radial',10x,'hoop' &
      &     ,9x,'axial',/,3x,'region',10x,'id',18x,'id',15x,'od',35x,'id' &
      &     ,9x,'id',6x,'od'/)
870 FORMAT (4x,i3,5x,f6.1,'(',f7.1,')',4x,f6.1,'(',f8.1,')',1x,f6.1,'(', &
      &     f8.1,')',3x,f6.1,'(',f8.1,')',6x,f9.4,1x,f9.4,   f9.4,1x,f9.4)
880 FORMAT (/3x,'axial',3x,'centerline',6x,'fuel average',3x,'cladding average', &
      &     6x,'coolant',11x,'stored',10x,6('-'),'cladding permanent', &
      &     ' strains,percent',6('-'),/,3x,'region',2x,'temperature',5x, &
      &     'temperature',7x,'temperature',6x,'temperature',9x,'energy',10x, &
      &     'radial',5x,'hoop',14x,'axial',/,14x,'K(F)',13x,'K(F)',13x,'K(F)', &
      &     13x,'K(F)',9x,'j/kg',6x,'Btu/lbm',3x,'id',9x,'id',6x,'od', &
      &     7x, 'growth',3x,'irradtion'/)
890 FORMAT (3x,i3,3x,2(f6.1,'(',f6.1,')',3x),f6.1,'(',f6.1,')',3x, &
      &     f6.1,'(',f6.1,')',3x,1pe8.1,2x,1pe8.1,2x,0pf5.2,6x,f5.2,3x,f5.2,5x,f5.2,4x,f5.2)
910 FORMAT (79x,' Fracas-1 subcode',/)
920 FORMAT (79x,' FEA subcode',/)
940 FORMAT (35x,' Mechanical parameters and results using the')
960 FORMAT (/42x,'**********',2x,' Peak power axial node output **********',/)
970 FORMAT (3x,'   time peak  burnup  power      clad temp (F)     gap     fuel', &
      &     ' temp (F)    contact   clad stress      strain  fuel od  gap con  press' &
      &     ,'  fgas   ZrO2     H2'/'      days node MWd/kgU  kW/ft   od.   avg.    id.' &
      &     ,'   mils   od.   avg.  cent.     psia       hoop  axial     pct       in', &
      &     '  B/hft2F   psia  fgr%   mils    ppm' &
      &     ,/'xxx xxxxxx xxxx xxxxxxx xxxxxx', &
      &     ' xxxxxxxxxxxxxxxxxxxx xxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxx xxxxxxxxxxxxxxxxx xxxxxxx', &
      &     ' xxxxxxxx xxxxxxxx xxxxxx xxxxx xxxxxx xxxxxx')
980 FORMAT (3x,'   time peak  burnup  power      clad temp (K)     gap       fuel', &
      &     ' temp (K)    contact   clad stress    strain  fuel od  gap con  press' &
      &     ,'  fgas    ZrO2     H2'/'      days node MWd/kgU   kW/m   od.   avg.    id.' &
      &     ,'  microns   od.   avg.  cent.      MPa    hoop   axial     pct       mm ', &
      &     '  W/m2-K    MPa  rel% microns    ppm' &
      &     /'xxx xxxxxx xxxx xxxxxxx xxxxxx', &
      &     ' xxxxxxxxxxxxxxxxxxxx xxxxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxx xxxxxxxxxxxxxxx xxxxxxx', &
      &     ' xxxxxxxx xxxxxxxx xxxxxx xxxxx xxxxxxx xxxxxx')
990 FORMAT (i3,1x,f6.1,2x,i3,1x,f7.2,1x,f6.2,1x,3(f6.1,1x),f5.2,1x, &
      &     3(f6.1,1x),f7.1,1x,f8.1,1x,f8.1,1x,f7.4,1x,f8.5,1x,f8.1,1x,f6.1,1x, &
      &     f5.1,1x,f6.2,1x,f6.1)
1000 FORMAT (i3,1x,f6.1,2x,i3,1x,f7.2,1x,f6.2,1x,3(f6.1,1x),2x,f5.1,1x, &
      &     3(f6.1,1x),1x,f6.2,1x,f7.1,1x,f7.1,1x,f7.4,1x,f8.4,1x,f8.1,1x,f6.3,1x, &
      &     f5.1,3x,f5.1,1x,f6.1)
1010 FORMAT (//,40x,' End of life strain range (percent) = ',f9.4)
1020 FORMAT (//,40x,' Fission gas cumulative fraction release = ',f9.6,/)
1030 FORMAT (//,40x,' ZrO2 weight gain ,(gm/m**2) =',f8.2,/)
2001 FORMAT (/,'                 REGULATORY OUTPUT SUMMARY',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',/ &
      &     'x                Rod Internal Pressure                       x',/ &
      &     'x                                                            x',/ &
      &     'x   Initial Cold Fuel Rod Plenum Volume = ',F10.5,' in^3    x',/ &
      &     'x                                                            x',/ &
      &     'x   Maximum Fuel Rod Internal Pressure  = ',F10.2,' psi     x',/ &
      &     'x            Peak nodal burnup          = ',F10.2,' GWd/MTU x',/ &
      &     'x            Rod average burnup         = ',F10.2,' GWd/MTU x',/ &
      &     'x            Fuel rod void volume       = ',F10.5,' in^3    x',/ &
      &     'x            Fission gas release        = ',F10.2,' %       x',/ &
      &     'x            Time                       = ',F10.3,' days    x',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
2002 FORMAT ( &
      &     'x                 Centerline Temperature                     x',/ &
      &     'x                                                            x',/ &
      &     'x   Maximum Fuel Centerline Temperature = ',F10.2,' deg.F   x',/ &
      &     'x            Axial node                 = ',7x,I3,'         x',/ &
      &     'x            Max Rad. Ave Fuel Enthalpy = ',F10.2,' BTU/lbm x',/ &
      &     'x            Nodal burnup               = ',F10.2,' GWd/MTU x',/ &
      &     'x            Rod average burnup         = ',F10.2,' GWd/MTU x',/ &
      &     'x            Time                       = ',F10.3,' days    x',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
2003  FORMAT ( &
      &     'x                 Strain Increment                           x',/ &
      &     'x                                                            x',/ &
      &     'x   Maximum Strain Increment(elas+plas) = ',F10.6,' %       x',/ &
      &     'x            Axial node                 = ',7x,I3,'         x',/ &
      &     'x            Nodal burnup               = ',F10.2,' GWd/MTU x',/ &
      &     'x            Rod average burnup         = ',F10.2,' GWd/MTU x',/ &
      &     'x            Time                       = ',F10.3,' days    x',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
2004 FORMAT ( &
      &     'x                 Fuel Melting                               x',/ &
      &     'x                                                            x',/ &
      &     'x   Fuel has Melted                                          x',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
2005 FORMAT ( &
      &     'x                 Fuel Melting                               x',/ &
      &     'x                                                            x',/ &
      &     'x   Fuel has not Melted                                      x',/ &
      &     'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
    !
    END SUBROUTINE print2
    !
    ! ******************
    !    FRTTR File
    ! ******************
    !
    SUBROUTINE fraptotrace
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon, ONLY : j, it, rp, press
    USE Material_Properties_frapcon
    IMPLICIT NONE
    !> @brief
    !> fraptotrace was written to store burnup dependent data that can be used to develop an input file for the T/H Code TRACE
    !> during the initialization of a run. Burnup is in units of MWd/MTU. All dimensional changes are in units of mils
    !> This is ONLY designed to work with the NRC's Internal Auto Input Generator.
    !> @author
    !> Ian Porter, NRC
    !> @date
    !> May 2013
    INTEGER(ipk) :: zz, ii
    REAL(r8k) :: cvvsi, Total_Power, EnergySum, APR, RinterfacPress_max, fueldensity, temp, pressMPa
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: TRACEPowerProfile
    !
    IF (it == 1) THEN !First timestep, only print out header information for variable description
        WRITE (funit,'("1 Fuel Thermal Expansion (mils)",150(e13.5))')
        WRITE (funit,'("2 Fuel Swelling (mils)",150(e13.5))')
        WRITE (funit,'("3 Fuel Densification (mils)",150(e13.5))')
        WRITE (funit,'("4 Fuel Relocation (mils)",150(e13.5))')
        WRITE (funit,'("5 Burnup (MWd/MTU)",150(e13.5))')
        WRITE (funit,'("6 Cladding O.D. creep (mils)",150(e13.5))')
        WRITE (funit,'("7 Gas Pressure (MPa), Mole Fraction: He, H, N, Ar, Kr, Xe, H2O, Total # of Moles, ", &
          &            "As-Fab rod free volume (m3), As-Fab # of moles, plenum volume fraction occupied by spring, ", &
          &            "Max Interfacial Pressure (MPa)",150(e13.5))')
        WRITE (funit,'("8 Gap HTC (W/m2K)",150(e13.5))')
        WRITE (funit,'("9 Oxide Layer Thickness (mils)",150(e13.5))')
        WRITE (funit,'("10 Axial Power shape factor",150(e13.5))')
        WRITE (funit,'("11 Total Stored Energy (J), Axial Stored Energy (J/kg)",150(e13.5))')
        DO zz = 1, nr
            WRITE (funit,'("12 Radial Power Distribution at each axial node for radial node ",150(i4))') zz
        END DO
        DO zz = 1, nr
            WRITE (funit,'("13 Radial location corresponding to power distribution for radial node ",150(i4))') zz
        END DO
    ELSE
        IF (j == (na + 1)) THEN !Finished axial node iteration loop. End of timestep. Print out values
            pressMPa = press * psitoMPa
            ALLOCATE (TRACEPowerProfile(1:nr, 1:(na-1)))
            TRACEPowerProfile = 0.0_r8k
            APR = 0.0_r8k
            cvvsi = cvv * in3tom3
            Total_Power = 0.0_r8k
            EnergySum = 0.0_r8k
            fueldensity = MatProp ('FUEL', 'ASFABDENSITY', temp)
            DO zz = 2, na
                Total_Power = Total_Power + Power(zz-1) * AxialNodLength(zz-1)
                ! Calculate total stored energy in Joules (J/kg)*(in)*(unitless)*(in)^2*(g/in^3)
                ! Note: doesn't assume dishes or chamfers
                EnergySum = EnergySum + 2.32444e3_r8k * (StoredEnergy(zz-1) * AxialNodLength(zz-1) * Pi * rp ** 2 * fueldensity)
            END DO
            DO zz = 2, na
                DO ii = 1, nr
                    APR = Power(zz-1) * AxialNodLength(zz-1) / (Total_Power / na)
                    TRACEPowerProfile(ii,zz-1) = APR * rapow(ii,zz-1)
                END DO
            END DO
            RinterfacPress_max = 0.0_r8k
            RinterfacPress_max = MAXVAL(RinterfacPress)
            WRITE (funit,'("1 ",150(e13.5))') (fuelexptot(j-1),j=2,na)
            WRITE (funit,'("2 ",150(e13.5))') (fuelswltot(j-1),j=2,na)
            WRITE (funit,'("3 ",150(e13.5))') (fueldentot(j-1),j=2,na)
            WRITE (funit,'("4 ",150(e13.5))') (Relocation(j-1) * 1000.0_r8k / 2.0_r8k,j=2,na)
            WRITE (funit,'("5 ",150(e13.5))') (fuelburntot(j-1),j=2,na)
            WRITE (funit,'("6 ",150(e13.5))') (cladcrptot(j-1),j=2,na)
            WRITE (funit,'("7 ",150(e13.5))') pressMPa, amfhe, amfh2, amfn2, amfarg, amfkry, amfxe, amfh2o, &
              &                               gasmo(it-1), cvvsi, gmlesAsFab, Spring%Vf, RinterfacPress_max
            WRITE (funit,'("8 ",150(e13.5))') (gapHTC(j-1),j=2,na)
            WRITE (funit,'("9 ",150(e13.5))') (oxidelayer(j-1),j=2,na)
            WRITE (funit,'("10 ",150(e13.5))') (Power(j-1) * AxialNodLength(j-1) / (Total_Power / na),j=2,na)
            WRITE (funit,'("11 ",150(e13.5))') EnergySum,(StoredEnergy(j-1) * 2.32444e3_r8k,j=2,na) ! BTUlbtoJkg?
            DO zz = 1, nr
                ! This intentionally loops over all axial nodes for the same radial dimension to match 
                ! TRACE's 2D array power input requirements
                WRITE (funit,'("12 ",150(e13.5))') (TRACEPowerProfile(nr-(zz-1),j-1),j=2,na)
            END DO
            DO zz = 1, nr
                ! This intentionally loops over all axial nodes for the same radial dimension to match
                ! TRACE's 2D array power input requirements
                WRITE (funit,'("13 ",150(e13.5))') (hrad(nr-(zz-1),j-1) * intom,j=2,na)
            END DO
        END IF
    END IF
    !
    IF(ALLOCATED(TRACEPowerProfile)) DEALLOCATE(TRACEPowerProfile)
    !
    END SUBROUTINE fraptotrace
    !

    SUBROUTINE print2_
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon, ONLY : MatProp
    IMPLICIT NONE
    !>@brief
    !> Subroutine print2 produces the output file data
    !
    INTEGER(ipk) :: z, i, ii, nrp1, nrp2, itm1, l, im1na, im1na2, im1, ij, iwarn, im2, im2na, im2na2, &
      &             iit, jpeak1, ii_Next, pknode
    REAL(r8k) :: wtk, tsecon, tpsec, toxo, toxok, testr, tcom, tcim, tbark, tagk, siflmp, qcm, presg, &
      &          powerm, pinsi, pinbr, oxmils, uo2llx, tsr, timday, tdays, tcl, resis, resi, radcnd, qpeakm, &
      &          qavm, oxmicr, hgapsi, hfllsi, gthcon, tsrk, totcr, tclk, sigm3b, sigm3, sigm2b, sigm2, sigm1b, &
      &          sigm1, gascnd, fulden, expnr, eps3, eps2, eps11, cpwx2, dpwx, dlrdsi, densix, deltp, &
      &          delday, delbpm, creapl, crack, congap, concnd, cgapsi, bppm, bpm, tttk, totdsi, totcrm, swlrmc, &
      &          rpm, roxm, rox, rfnci, relocs, relocm, rcom, rcohm, vva, vfrcrf, vfrcpr, vfrcgp, vfrcds, vfrccr, &
      &          tsfk, tpcak, tnterk, thvvsi, tcrkk, tcrkf, tagak, tafak, rcoh, rcihm, rcih, radm, txefr, tkrfr, &
      &          th2fr, tfgpct, tarfr, sigrab, sigra, sighpo, sighpi, sigaxb, sigax, sghpob, sghpib, exprmc, expnrm, &
      &          expnrl, epsra, epshpo, epshpi, dpwx2, denrmc, delrng, crprml, crprmc, creapm, busi, tfrk, tcak, tblkk, &
      &          sigrpo, sigrpi, sighab, sigha, sgrpob, sgrpib, fmgrpct, fdi, tblkf, srm, epsunp, epsrpo, epsrpi, &
      &          epsha, epsax, epprpo, epprpi, eppra, epphpo, epphpi, eppha, eppax, epgro, cpdltx, creap, exprml, &
      &          denrml, swlrml, pressMPa, bp, bpp, oxide
    INTEGER(ipk), DIMENSION(2) :: icltemp
    REAL(r8k), DIMENSION(20) :: aaa
    !
    iwarn = 0
    !
    ! Determine which array value to used based on whether the print is for the axial loop or summary
    SELECT CASE (PrintType)
    CASE ('Axial Loop')
        z = j - 1
        !
        nrp1 = nr + 1
        nrp2 = nr + 2
        itm1 = it - 1
        IF (it == 1) THEN
            IF (j == jpeak(1)) THEN
                dlrdsi = intomm * dlrod
!                WRITE (ounit,400) dlrdsi, dlrod
                dhfll = hfll - (totl * fttoin)
                hfllsi = dhfll * intomm
!                WRITE (ounit,401) hfllsi, dhfll
            END IF
            RETURN
        END IF
        IF (j == jmax + 1) THEN
            ! End of timestep loop
            buavearray(it-1) = bu
            CALL pghead
            busi = bu / 1000.0_r8k
            rfnci = rfnvff * cfv
            tagak = tfk(taga(it))
            tafak = tfk(tafa(it))
            tpcak = tfk (tpca)
            tsfk = tfk(tsfa(it))
            tnterk = tfk(tntera)
            thvv = hpv + hgv + hdshv + hcrv + rfnci + hporv + hva
            voidvolarray(it-1) = thvv
            thvvsi = thvv * in3tocm3
            tplens = tfk(tplen)
            tcrkf = 0.0_r8k
            DO i = (jmin - 1), (jmax - 1)
                tcrkf = tcrkf + (FuelTempRestruRad(i) + PelSurfTemp(i)) / 2.0_r8k * deltaz(i)
            END DO
            tcrkf = tcrkf / totl
            tcrkk = tfk(tcrkf)
            vfrcpl = hpv / thvv
            vfrccr = hcrv / thvv
            vfrcds = hdshv / thvv
            vva = hva / thvv
            vfrcpr = hporv / thvv
            vfrcrf = rfnci / thvv
            vfrcgp = hgv / thvv
            SELECT CASE (imox)
            CASE (0)
                tkrfr = 0.15_r8k * tfgfr
                txefr = 0.85_r8k * tfgfr
            CASE (1, 2)
                tkrfr = 0.0588_r8k * tfgfr
                txefr = 0.9412_r8k * tfgfr
            END SELECT
            tarfr = 0.0_r8k
            th2fr = 0.0_r8k
            tfgpct = tfgfr * 100.0_r8k
            pressMPa = press * PSItoMPa
!            WRITE (ounit,780) itm1
!            WRITE (ounit, 790) busi, bu, thefr, amfhe, th2fr, amfh2
!            WRITE (ounit,810) tplens, tplen, vfrcpl, tn2fr, amfn2
!            WRITE (ounit,820) tagak, taga(it), vfrcgp, tarfr, amfarg

!            SELECT CASE (ngasmod)
!            CASE (1, 2, 3)
!                WRITE (ounit,830) tsfk, tsfa(it), vfrcrf, tkrfr, amfkry
!                IF (hdish > 0.0_r8k) WRITE (ounit,840) tafak, tafa(it), vfrcds, txefr, amfxe
!                IF (rc(1) > 0.0_r8k .AND. hdish == 0.0_r8k) WRITE (ounit,842) tpcak, tpca, vva, txefr, amfxe
!            CASE (4)
!                WRITE (ounit,831) tsfk, tsfa(it), vfrcrf, th2ofr, amfh2o
!                IF (hdish > 0.0_r8k) WRITE (ounit,841) tafak, tafa(it), vfrcds
!                IF (rc(1) > 0.0_r8k .AND. hdish == 0.0_r8k) WRITE (ounit,843) tpcak, tpca, vva
!            END SELECT
!            IF (hdish == 0.0_r8k .AND. rc(1) == 0.0_r8k) WRITE (ounit,853) tnterk, tntera, vfrcds, txefr, amfxe

!            SELECT CASE (ngasmod)
!            CASE (1, 2, 3)
!                WRITE (ounit,850) tafak, tafa(it), vfrcpr, th2ofr, amfh2o
!            CASE (4)
!                WRITE (ounit,851) tafak, tafa(it), vfrcpr
!            END SELECT

!            WRITE (ounit,852) tcrkk, tcrkf, vfrccr

!            IF (hdish == 0.0_r8k .AND. rc(1) > 0.0_r8k) WRITE (ounit,854) tnterk, tntera, vfrcds
!            IF (rc(1) > 0.0_r8k .AND. hdish > 0.0_r8k) WRITE (ounit,855) tpcak, tpca, vva

!            SELECT CASE (ngasmod)
!            CASE (1, 2, 3)
!                WRITE (ounit,800) thvvsi, thvv, gasmo(itm1), pressMPa, press, tfgpct
!            CASE (4)
!                WRITE (ounit,800) thvvsi, thvv, gasmo(itm1), pressMPa, press, SUM(RB_rod(:,it))*100.0_r8k
!                ! Output gas release fractions based on ANS 5.4 2011 (by G. Longoni, April 2015)
!                WRITE (ounit,'(/,32x,a,2x,a)') 'ANS 5.4 2011 - Fuel Rod Cumulative Fission Gas Release Fractions (Release/Birth)'
!                WRITE (ounit,'(/,32x,a,2x,a)') 'Short Lived Nuclides (Half Life <6 h)', &
!                  &                            '| Long Lived Nuclides (6 h < Half Life < 60 days)'
!                WRITE (ounit,'(/,40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-135m',RB_rod(1,it),'Xe-133',RB_rod(12,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-137',RB_rod(2,it),'Xe-135',RB_rod(13,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-138',RB_rod(3,it),'I-131',RB_rod(14,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-139',RB_rod(4,it),'I-133',RB_rod(15,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-85m',RB_rod(5,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-87',RB_rod(6,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-88',RB_rod(7,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-89',RB_rod(8,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-90',RB_rod(9,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-132',RB_rod(10,it)
!                WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-134',RB_rod(11,it)
!                WRITE (ounit,'(/,41x,a8,1x,es12.2,18x,a8,es12.2)') 'Total ::',SUM(RB_rod(1:11,it)),'Total ::',SUM(RB_rod(12:15,it))
!                WRITE (ounit,*)
!            END SELECT          
!            WRITE (ounit,940)
!            SELECT CASE (mechan)
!            CASE (1)
!                WRITE (ounit,920)
!                WRITE (ounit,860)
!            CASE (2)
!                WRITE (ounit,910)
!                WRITE (ounit,860)
!            END SELECT
            DO i = (jmin - 1), (jmax - 1)
                im1 = i - 1
                im1na = im1 + na
                im1na2 = im1na + na
                sigra = sig(i,3) * PSItoMPa
                sigrab = sig(i,3)
                sigax = sig(i,2) * PSItoMPa
                sigaxb = sig(i,2)
                sighpo = sig(i,1) * PSItoMPa
                sighpi = sighpo
                sghpob = sig(i,1)
                sghpib = sghpob
                epshpo = eps(i,1) * 1.0e2_r8k
                epshpi = epshpo
                epsra = eps(i,3) * 1.0e2_r8k
                epsax = eps(i,2) * 1.0e2_r8k
!                WRITE (ounit,870) i,sigra,sigrab,sighpi,sghpib,sighpo,sghpob,sigax, &
!                  &               sigaxb,epsra,epshpi,epshpo,epsax
            END DO
!            WRITE (ounit,903)
!903         FORMAT (/,' cladding H2 concentration and its effect on ductility')
!            WRITE (ounit,905)
!905         FORMAT (/,1x,'axial',9x,'H2',10x,'excess H2',6x,'uniform',8x,'Fuel',11x,'Oxide layer',15x,'Fast', &
!              &    7x,'Cumulative Fission Gas')
!            WRITE (ounit,907)
!907         FORMAT (' region   concentration  concentration    strain',9x,'Duty',12x,'thickness',14x,'Fluence',9x,'Release,')
!            WRITE (ounit,908)
!908         FORMAT (15x,'ppm',13x,'ppm',10x,'%',12x,'Index',9x,'mils   (microns)',9x, 'n/m^2',11x,'percent')
            DO i = jmin, jmax
                oxmils = EOSZrO2Thk(i-1) * 12000.0_r8k
                oxmicr = oxmils * intomm
                im1 = i - 1
                fmgrpct = rdotwrt(im1) * 100.0_r8k
                epsunp = 100.0_r8k * UniformAxNodStrn(im1)
                IF (epsunp < 0.0_r8k) THEN
                    epsunp = 0.0_r8k
                    iwarn = 1
                END IF
                !
                IF (it == 1) THEN
                    FDItave(im1) = MAX(580.0_r8k, SurfTempOxide(im1))
                ELSE
                    FDItave(im1) = (FDItave(im1) * ProblemTime(it-1) + MAX(580.0_r8k, &
                      &             SurfTempOxide(im1)) * (ProblemTime(it) - ProblemTime(it-1))) / ProblemTime(it)
                END IF
                fdi = (MAX(0.0_r8k, (FDItave(im1) - 580.0_r8k) / 100.0_r8k * ProblemTime(it) * sectohr / 1000.0_r8k)) ** 2
!                SELECT CASE (ngasmod)
!                CASE (1, 2, 3)
!                    WRITE (ounit,909) im1, CladH2Concen(im1), ExcessH2Concen(im1), epsunp, fdi, &
!                      &               oxmils, oxmicr, FastFluence(i-1), fmgrpct
!                CASE (4)
!                    WRITE (ounit,909) im1, CladH2Concen(im1), ExcessH2Concen(im1), epsunp, fdi, &
!                      &               oxmils, oxmicr, FastFluence(i-1), SUM(RB_axial(:,im1))*100.0_r8k
!                END SELECT
!909             FORMAT (2x,i3,6x,f8.2,8x,f8.2,6x,f6.3,6x,f8.2,9x,f7.4,1x,'(',f7.3,')', 7x, e11.4, 7x, f6.2)
            END DO
!            IF (iwarn == 1) WRITE (ounit,906)
!906         FORMAT ('***Warning:  Calculated values of uniform strain were negative.  They have been set equal to 0.0.  ***')
            iwarn = 0
!            WRITE (ounit,880)
            DO i = jmin, jmax
                im1 = i - 1
                im2 = i - 2
                im2na = im2 + na
                im2na2 = im2na + na
                eppra = epp(i-1,3) * 100.0_r8k
                eppax = epp(i-1,2) * 100.0_r8k
                epphpi = epp(i-1,1) * 100.0_r8k
                epphpo = epphpi
                ij = nr * i - 1
                cpdltx = StoredEnergy(im1) * BTUlbtoJkg
                tcak = tfk(CladAveTemp(im1))
                tbark = tfk(PelAveTemp(im1))
                tfrk = tfk(tmpfuel(nr,im1))
                tblkf = (BulkCoolantTemp(im1) + BulkCoolantTemp(i)) / 2.0_r8k
                tblkk = tfk(tblkf)
                epgro = CladIrradGrowStrn(i-1) * 100.0_r8k
!                WRITE (ounit,890) im1, tfrk, tmpfuel(nr,im1), tbark, PelAveTemp(im1), tcak, CladAveTemp(im1),  &
!                     &              tblkk, tblkf, cpdltx, StoredEnergy(im1), eppra, epphpi, epphpo, eppax, epgro
                IF (tfrk > ftmelt) THEN
                    imelt = 1
!                    WRITE (ounit,331) im1, tfrk, ftmelt
                    WRITE (*,331) im1, tfrk, ftmelt
331                 FORMAT ('Fuel has melted.  FRAPCON-4 is not validated beyond melting point.',/, &
                      &     'Centerline temperature at node ',i2,' is', f10.2,'K.',/, &
                      &     'Fuel melting temperature is ',f10.2,'K.',/,'Code execution continuing.')
                END IF
                IF (tcak > ctmelt) THEN
!                    WRITE (ounit,332) im1, tcak, ctmelt
                    WRITE (*,332) im1, tcak, ctmelt
332                 FORMAT ('Cladding has melted.  FRAPCON-4 is not validated beyond melting point.',/, &
                      &     'Cladding average temperature at node',i2,' is', f10.2,'K.',/, &
                      &     'Cladding melting temperature is ',f10.2,'K.',/,'Code execution stopped.')
                    STOP
                END IF
            END DO
            dlrdsi = dlrod * intomm
!            WRITE (ounit,750) dlrdsi, dlrod
            dhfll = hfll - (totl * fttoin)
            hfllsi = dhfll * intomm
!            WRITE (ounit,751) hfllsi, dhfll
        ELSE
            ! Print data for the axial node
            IF (j == jpeak(1) .OR. jdlpr /= 1) THEN
                IF (jdlpr >= 0) CALL pghead
            END IF
            fulden = den * 0.01_r8k
            tpsec = ProblemTime(it-1)
            IF (tpsec <= 0.001_r8k) tpsec = 0.0_r8k
            tsecon = ProblemTime(it)
            qcm = qc(j-1) * Bhft2toWm2
            !
            ! These adjustments by 10 should be removed when the cause is found_frapcon.
            !
            bpp = BOSNodeburnup(j-1) / 10.0_r8k
            bp = EOSNodeburnup(j-1) / 10.0_r8k
            buarray(it-1,j-1) = bp * 10.0_r8k
            fuelburntot(j-1) = buarray(it-1,j-1)
            delbp = StepNodeburnup(j-1) / 10.0_r8k
            !
            bpm = bp / 1000.0_r8k
            bppm = bpp / 1000.0_r8k
            delbpm = delbp / 1000.0_r8k
            wt = (BulkCoolantTemp(j) + BulkCoolantTemp(j-1)) / 2.0_r8k
            wtk = tfk(wt)
            oxide = EOSZrO2Thk(j-1) * fttoin
            oxmils = oxide * 1000.0_r8k
            oxidelayer(j-1) = oxmils
            oxmicr = oxmils * intomm
            hflmp = FilmCoefficient(j-1)
            siflmp = hflmp * Bhft2FtoWm2K
            toxo = SurfTempOxide(j-1)
            toxok = tfk(toxo)
            tci = CladInSurfTemp(j-1)
            tco = CladOutSurfTemp(j-1)
            tcom = tfk(tco)
            tcim = tfk(tci)
            hgapt = TotalHgap(j-1)
            hsolid = SolidHgap(j-1)
            hgap = GasHgap(j-1)
            hgapr = RadHgap(j-1)
            hgapsi = hgapt * Bhft2FtoWm2K
            gapHTC(j-1) = hgapsi
            testr = 0.0_r8k
            pinbr = MAX(RinterfacPress(j-1), testr)
            pinsi = pinbr * PSItoMPa
            crack = FuelCondFactor(j-1)
            tagk = tfk(GapAveTemp(j-1))
            presg = press * PSItoPa
            cgapsi = MatProp ('GAS', 'THERMCOND', tagk)
            congap = cgapsi * WmKtoBhftF
            tbark = tfk(PelAveTemp(j-1))
            Powerm = Power(j-1) * mtoft
            tdays = ProblemTime(itm1) * sectoday
            qpeakm = qpeak * mtoft
            qavm = qav * mtoft
            uo2llx = 0.0_r8k
            dpwx = 0.0_r8k
            dpwx2 = 0.0_r8k
            densix = 0.0_r8k
            gascnd = hgap / hgapt
            concnd = hsolid / hgapt
            radcnd = hgapr / hgapt
            resi = 1.0_r8k / hgapsi
            resis = 1.0_r8k / hgapt
            timday = tsecon * sectoday
            tdays = tpsec * sectoday
            deltp = tsecon - tpsec
            delday = deltp * sectoday
            tsr = tmpfuel(1,j-1)
            tcl = tmpfuel(nr,j-1)
            cltemparray(it-1,j-1) = tcl
            storedearray(it-1,j-1) = StoredEnergy(j-1)
            tclk = tfk(tcl)
            tsrk = tfk(tsr)
            eps11 = eps(j-1,1) * 100.0_r8k
            eps2 = eps(j-1,2) * 100.0_r8k
            eps3 = eps(j-1,3) * 100.0_r8k
            totcr = eps11
            creap = eppsv(j-1,1) * 100.0_r8k
            expnr = totcr - creap
            sigm1b = sig(j-1,1) / 1.0e3_r8k
            sigm2b = sig(j-1,2) / 1.0e3_r8k
            sigm3b = sig(j-1,3) / 1.0e3_r8k
            sigm1 = sigm1b * PSItoKPa
            sigm2 = sigm2b * PSItoKPa
            sigm3 = sigm3b * PSItoKPa
            crack = 1.0_r8k
            strainarray(it-1,j-1) = eps11
            creaparray(it-1,j-1) = epp(j-1,1)
            IF (it == 2) THEN
                straindiffarray(it-1,j-1) = 0.0_r8k
                creapratearray(j-1) = creaparray(it-1,j-1) / ProblemTime(it)
            ELSE
                straindiffarray(it-1,j-1) = strainarray(it-1,j-1) - strainarray(it-2,j-1)
                creapratearray(j-1) = (creaparray(it-1,j-1) - creaparray(it-2,j-1)) / (ProblemTime(it) - ProblemTime(it-1))
            END IF
            creapl = creap * (rco - 0.5_r8k * tcc) * 10.0_r8k
            cladcrptot(j-1) = creapl
            creapm = creapl * intomm
            expnrl = expnr * (rco - 0.5_r8k * tcc) * 10.0_r8k + eps3 * 0.5_r8k * tcc * 10.0_r8k
            expnrm = expnrl * intomm
            totcrl(j-1) = creapl + expnrl
            totcrm = totcrl(j-1) * intomm
            rcih = CladDiamHot(j-1) / 2.0_r8k
            rcoh = rcih + tcc * (1.0_r8k + eps3 / 100.0_r8k)
            rox = rcoh + oxide
            rcihm = rcih * intocm
            rcohm = rcoh * intocm
            roxm = rox * intocm
            !
            DO i = 2, nr
                l = nr + 1 - i
                ij = nr * (j-1) + l - 1
                delrng = (crad(l,j-1) - crad(l+1,j-1))
                uo2llx = uo2llx + delrng * uo2exp(l,j-1)
                IF (i == 1) uo2llx = uo2llx + delrng * uo2exp(l,j-1)
                dpwx = dpwx + delrng * dpw(l,j-1)
                densix = densix + delrng * densf(l,j-1)
            END DO
            dpwxarray(it-1,j-1) = dpwx / crad(1,j-1)
            IF (it == 2) THEN
                dpwxrate(j-1) = dpwxarray(it-1,j-1) / ProblemTime(it)
            ELSE
                dpwxrate(j-1) = (dpwxarray(it-1,j-1) - dpwxarray(it-2,j-1)) / (ProblemTime(it) - ProblemTime(it-1))
            END IF
            rpm = rp * intocm
            rcom = rco * intocm
            denrml = densix * 1000.0_r8k
            fueldentot(j-1) = denrml   ! Total densification for axial node
            denrmc = denrml * intomm
            swlrml = dpwx * 1000.0_r8k
            fuelswltot(j-1) = swlrml   ! Total swelling for axial node
            swlrmc = swlrml * intomm
            exprml = uo2llx * 1000.0_r8k
            fuelexptot(j-1) = exprml   ! Total thermal expansion for axial node
            exprmc = exprml * intomm
            relocm = Relocation(j-1) * 1000.0_r8k
            relocm_true = relocm
            totdef(j-1) = denrml + swlrml + exprml + relocm
            totinner(j-1) = eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k
            gapmech(j-1) = thkgap(j-1) * 1000.0_r8k - totdef(j-1) + totinner(j-1)
            gapthrm(j-1) = cgapsi / (hgapsi * gascnd) * mtoin * 1000.0_r8k
            relocm = relocm + gapmech(j-1) - gapthrm(j-1)
            relocm_mod = relocm
            fuelrelmod(j-1) = relocm
            relocs = relocm * intomm
            totdef(j-1) = denrml + swlrml + exprml + relocm
            gapmech(j-1) = thkgap(j-1) * 1000.0_r8k - (denrml + swlrml + exprml + 0.5_r8k * relocm_true) + totinner(j-1)
            gapplot(j-1) = thkgap(j-1) * 1000.0_r8k - totdef(j-1) + totinner(j-1)
            ! colddef = cold displacement of fuel radius (inches) (passed to rstfs)
            colddef(z) = (totdef(j-1) - relocm - exprml) / 1000.0_r8k
            totdsi = totdef(j-1) * intomm
            IF (j /= jpeak(1) .AND. jdlpr == 1) RETURN
            IF (jdlpr < 0) RETURN
!            WRITE (ounit,410) j-1, itm1
!            WRITE (ounit,420) qavm, qav
!            WRITE (ounit,430) Powerm, Power(j-1), qcm, qc(j-1)
!            WRITE (ounit,440) qpeakm, qpeak
!            WRITE (ounit,450) tdays, tpsec, bppm, bpp
!            WRITE (ounit,460) delday, deltp, delbpm, delbp
!            WRITE (ounit,470) timday, tsecon, bpm, bp
!            WRITE (ounit,480)
            radm = hrad(nr,j-1) * intocm
!            WRITE (ounit,490) radm, hrad(nr,j-1), tclk, tcl, rapow(nr,j-1)
            DO ii = 1, (nr - 2)
                i = nr - ii
                tttk = tfk(tmpfuel(i,j-1))
                radm = hrad(i,j-1) * intocm
!                WRITE (ounit,510) radm, hrad(i,j-1), tttk, tmpfuel(i,j-1), rapow(i,j-1)
            END DO
            radm = hrad(1,j-1) * intocm
            PelletRad(j-1) = hrad(1,j-1)
!            WRITE (ounit,540) radm, hrad(1,j-1), tsrk, tsr, rapow(1,j-1)
!            WRITE (ounit,550) rcihm, rcih, tcim, tci
!            WRITE (ounit,560) rcohm, rcoh, tcom, tco
!            WRITE (ounit,570) roxm, rox, toxok, toxo
!            WRITE (ounit,580) wtk, wt
!            WRITE (ounit,590) rp, rpm
!            WRITE (ounit,600) cgapsi, sigm3, sigm3b, eps3, congap, sigm2, sigm2b, eps2
!            WRITE (ounit,610) sigm1, sigm1b, eps11
!            WRITE (ounit,620) denrml, denrmc, hgapsi
!            WRITE (ounit,630) swlrml, swlrmc, hgapt
!            WRITE (ounit,640) exprml, exprmc
!            WRITE (ounit,650) relocm, relocs, resi, resis, totdef(j-1), totdsi, siflmp, hflmp, crdtt(j-1)
!            WRITE (ounit,660) rco, rcom
!            WRITE (ounit,680) gascnd
!            WRITE (ounit,700) creapl, creapm, concnd
            ! define colddec= cladding permanent displacement (radial inches) for FRAPTRAN restart file
            colddec(z) = creapl / 1000.0_r8k
!            WRITE (ounit,720) expnrl, expnrm, radcnd
!            WRITE (ounit,741) totcrl(j-1), totcrm, pinsi, pinbr
            totinner(j-1) = eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k
            cladinpermdef(j-1) = (eps11 * (rco + rci) * 0.5_r8k * 10.0_r8k - eps3 * (rco - rci) * 0.5_r8k * 10.0_r8k) * 0.001_r8k
!            WRITE (ounit,742) rci, rci * intocm, totinner(j-1), totinner(j-1) * intomm
        END IF
    CASE ('Summary')
        !
        ! Summary report
        !
        jpeak(1) = jfix
        z = jfix
        ii_Next = 2
        DO ii = 2, im
            IF (ii == ii_Next) THEN
                ii_Next = ii + 50
                CALL pghead
!                WRITE (ounit,960)
!                IF (nunits == 1) WRITE (ounit,970)
!                IF (nunits == 0) WRITE (ounit,980)
            END IF
            iit = ii - 1
            jpeak1 = jpeak(jst(ii)) - 1
            aaa = 0.0_r8k
            SELECT CASE (nunits)
            CASE (0)
                aaa(1) = ProblemTime(ii) * sectoday
                aaa(2) = pkBurnup(iit) * 1.0e-3_r8k
                aaa(3) = pkPower(iit) * mtoft
                aaa(4) = tfk(pkODCladTemp(iit))
                aaa(5) = tfk(pkAveCladTemp(iit))
                aaa(6) = tfk(pkIDCladTemp(iit))
                aaa(7) = pkGap(iit) * intomm
                aaa(8) = pkFisGasRelFrac(iit) * 100.0_r8k
                aaa(9) = tfk(pkPelSurfTemp(iit))
                aaa(10) = tfk(pkPelAveTemp(iit))
                aaa(11) = tfk(pkPelCentTemp(iit))
                aaa(12) = pkIntefacePres(iit) * PSItoMPa
                aaa(13) = pkHoopStres(iit) * PSItoMPa
                aaa(14) = pkAxlStres(iit) * PSItoMPa
                aaa(15) = pkHoopStrain(iit) * 100.0_r8k
                aaa(16) = pkFuelPelOD(iit) * intomm
                aaa(17) = pkGapCond(iit) * Bhft2FtoWm2K
                aaa(18) = pit(iit) * PSItoMPa
                aaa(19) = pkZrO2(iit) * intomm
                aaa(20) = pkH2up(iit)
!                SELECT CASE (ngasmod)
!                CASE (1, 2, 3)
!                    WRITE (ounit,1000) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), aaa(8), aaa(19), aaa(20)
!                CASE (4)
!                    WRITE (ounit,1000) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), &
!                      &                SUM(RB_rod(:,ii))*100.0_r8k, aaa(19), aaa(20)
!                END SELECT
            CASE (1)
                aaa(1) = ProblemTime(ii) * sectoday
                aaa(2) = pkBurnup(iit) * 1.0e-3_r8k
                aaa(3) = pkPower(iit)
                aaa(4) = pkODCladTemp(iit)
                aaa(5) = pkAveCladTemp(iit)
                aaa(6) = pkIDCladTemp(iit)
                aaa(7) = pkGap(iit)
                aaa(8) = pkFisGasRelFrac(iit) * 100.0_r8k
                aaa(9) = pkPelSurfTemp(iit)
                aaa(10) = pkPelAveTemp(iit)
                aaa(11) = pkPelCentTemp(iit)
                aaa(12) = pkIntefacePres(iit)
                aaa(13) = pkHoopStres(iit)
                aaa(14) = pkAxlStres(iit)
                aaa(15) = pkHoopStrain(iit) * 100.0_r8k
                aaa(16) = pkFuelPelOD(iit)
                aaa(17) = pkGapCond(iit)
                aaa(18) = pit(iit)
                aaa(19) = pkZrO2(iit)
                aaa(20) = pkH2up(iit)
!                SELECT CASE (ngasmod)
!                CASE (1, 2, 3)
!                    WRITE (ounit,990) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), aaa(8), aaa(19), aaa(20)
!                CASE (4)
!                    WRITE (ounit,990) iit, aaa(1), jpeak1, (aaa(i), i=2, 7), (aaa(i), i=9, 18), &
!                      &               SUM(RB_rod(:,ii))*100.0_r8k, aaa(19), aaa(20)
!                END SELECT
            END SELECT
        END DO
        srm = 100.0_r8k
        DO i = 1, im
            srm = MIN(pkHoopStrain(i-1), srm)
        END DO
        CALL pghead
!        WRITE (ounit,1010) ABS(pkHoopStrain(im-1) - srm)
!        SELECT CASE (ngasmod)
!        CASE (1, 2, 3)
!            WRITE (ounit,1020) tfgfr
!        CASE (4)
!            WRITE (ounit,1020) SUM(RB_rod(:,im))
!            ! Output gas release fractions based on ANS 5.4 2011 (by G. Longoni, April 2015)
!            WRITE (ounit,'(/,32x,a,2x,a)') 'ANS 5.4 2011 - Fuel Rod Cumulative Fission Gas Release Fractions (Release/Birth)'
!            WRITE (ounit,'(/,32x,a,2x,a)') 'Short Lived Nuclides (Half Life <6 h)', &
!              &                            '| Long Lived Nuclides (6 h < Half Life < 60 days)'
!            WRITE (ounit,'(/,40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-135m',RB_rod(1,im),'Xe-133',RB_rod(12,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-137',RB_rod(2,im),'Xe-135',RB_rod(13,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-138',RB_rod(3,im),'I-131',RB_rod(14,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2,16x,a8,2x,es12.2)') 'Xe-139',RB_rod(4,im),'I-133',RB_rod(15,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-85m',RB_rod(5,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-87',RB_rod(6,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-88',RB_rod(7,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-89',RB_rod(8,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'Kr-90',RB_rod(9,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-132',RB_rod(10,im)
!            WRITE (ounit,'(40x,a8,2x,es12.2)') 'I-134',RB_rod(11,im)
!            WRITE (ounit,'(/,41x,a8,1x,es12.2,18x,a8,es12.2)') 'Total ::',SUM(RB_rod(1:11,im)),'Total ::',SUM(RB_rod(12:15,im))
!        END SELECT
!        IF (icm <= 7) WRITE (ounit,1030) pkZrO2WtGain
        ! Ouputs of regulatory interest
        ! Rod internal pressure
        ! Set peak node location. The - 1 is because the array starts at node 0_frapcon.
        pknode = MAXLOC(pit, DIM=1) - 1
!        SELECT CASE (ngasmod)
!        CASE (1, 2, 3)
!            WRITE (ounit,2001) cvv, MAXVAL(pit), pkBurnup(pknode) / 1000.0_r8k, &
!              &                 buavearray(pknode) / 1000.0_r8k, voidvolarray(pknode), &
!              &                 pkFisGasRelFrac(pknode) * 100.0_r8k, ProblemTime(pknode+1) * sectoday
!        CASE (4)
!            WRITE (ounit,2001) cvv, MAXVAL(pit), pkBurnup(pknode) / 1000.0_r8k, &
!              &                 buavearray(pknode) / 1000.0_r8k, voidvolarray(pknode), &
!              &                 SUM(RB_rod(:,im)) * 100.0_r8k, ProblemTime(pknode+1) * sectoday
!        END SELECT
        ! Centerline temp
        icltemp = MAXLOC(cltemparray)
!        WRITE (ounit,2002) MAXVAL(cltemparray), icltemp(2), storedearray(icltemp(1), icltemp(2)), &
!          &               buarray(icltemp(1), icltemp(2)) / 1000.0_r8k, buavearray(icltemp(1)) / 1000.0_r8k, &
!          &               ProblemTime(icltemp(1) + 1) * sectoday
!        ! Strain increment
        icltemp = MAXLOC(straindiffarray)
!        WRITE (ounit,2003) MAXVAL(straindiffarray), icltemp(2), buarray(icltemp(1), icltemp(2)) / 1000.0_r8k, &
!          &               buavearray(icltemp(1)) / 1000.0_r8k, ProblemTime(icltemp(1) + 1) * sectoday
        ! Fuel melting
!        IF (imelt == 1) THEN
!            WRITE (ounit,2004)
!        ELSE
!            WRITE (ounit,2005)
!        END IF
    END SELECT
    END SUBROUTINE print2_


END MODULE Output_Data_frapcon




