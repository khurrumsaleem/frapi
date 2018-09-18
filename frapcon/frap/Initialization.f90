MODULE Initialization_frapcon
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Functions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This module contains the subroutines used to initialize the FRAPCON problem by reading_frapcon
    !> in the main input blocks and performing necesary unit conversions and error checking.
    !> Subroutines include inital, check, and ResetTimesteps
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 05/26/2015
    !
    CONTAINS
    !
    SUBROUTINE inital
    USE Kinds_frapcon
    USE conversions_frapcon
    USE Material_Properties_frapcon
    USE variables_frapcon
    USE DatingData_frapcon
    USE Refabrication_frapcon
    USE Output_Data_frapcon, ONLY : print1
    USE Burn_frapcon, ONLY : turbin
    USE Functions_frapcon, ONLY : terp
    USE Decay_frapcon, ONLY : fpdcay, DecayModel, DecayHeat
    USE Gas_frapcon, ONLY : ngases
    USE Developer_frapcon
    USE SpentFuel_frapcon
    USE FileIO_frapcon, ONLY : Namelist_Read_Error
    IMPLICIT NONE
    !> @brief
    !> Subroutine inital declares the default values before the input
    !> file is read, then reads the input blocks $frpcon & $frpmox.
    !> It then increases the user-defined time-dependent values by 1 timestep
    !> to allow for FRAPCON's 'initial' timestep calculation of 0.001 days.
    !
    INTEGER(ipk) :: oligp2, ifixtedtsurf, ncoolanthist, npairs, ncoolz, i, ii, mt, kz, kt, &
      &             nap1, icreep, jnsurftemp_max, numshapes, jj, nstart, ij, l, InputStat = 0
    REAL(r8k) :: rchmfr, rcrack, axheight, beta, dishsg, u, gden, hi, qmpy_tmp, dummy, cmol, &
      &          GasDensity, MWtMix, vs, dspg, dspgw
    CHARACTER(LEN=100) :: errorline
    !
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dumarray2, dumarray1
    !
    ! Variables in $frpcon input block
    !
    NAMELIST / frpcon / afdn, amfair, amfarg, amffg, amfhe, amfh2, amfh2o, amfkry, amfn2, amfxe, &
      &                 beta, buin, catexf, chorg, cldwks, comp, cpl, crdt, crdtr, crephr, ctmax, &
      &                 dco, deltaz, den, deng, dishsd, dspg, dspgw, enrch, fa, fgpav, flux, fotmtl, &
      &                 gadoln, go, hdish, hplt, icm, icor, idxgas, iplant, iq, ivardm, imox, &
      &                 jdlpr, jn, jst, igas, nopt, nplot, nread, nrestr, nsp, ntape, nunits, p2, &
      &                 pitch, ppmh2o, ppmn2, ProblemTime, roughc, qend, qf, qmpy, rc, roughf, rsntr, &
      &                 sgapf, slim, thkcld, thkgap, totl, tsint, tw, vs, x, xt, cladt, ifixedtsurf, &
      &                 jstsurftemp, jnsurftemp, grnsize, frcoef, p1, igascal, ifba, b10, zrb2thick, &
      &                 zrb2den, ngasmod, zr2vintage, sigftc, sigftex, sigfgr, sigswell, sigcreep, &
      &                 siggro, sigcor, sigh2, crudmult, chmfrh, chmfrw, irefab, nrefab1, nrefab2, &
      &                 cplrefab, vsrefab, dspgrefab, dspgwrefab, fgpavrefab, airrefab, n2refab, &
      &                 arrefab, fgrefab, herefab, krrefab, xerefab, &
      &                 ifixedcoolt, tcoolant, zcool, ifixedcoolp, pcoolant, fpdcay, DecayModel
    !
    ! Variables in $frpmox input block
    !
    NAMELIST / frpmox / enrpu39, enrpu40, enrpu41, enrpu42, moxtype
    !
    IF (newprb /= 1) THEN
        vs = 0.0_r8k
        dspg = 0.0_r8k
        dspgw = 0.0_r8k
        jnsurftemp_max = 0
        stold = 0.0_r8k
        deltimeold = 0.0_r8k
        sagold = 0.0_r8k
        delsagold = 0.0_r8k
        stnew = 0.0_r8k
        deltimenew = 0.0_r8k
        sagnew = 0.0_r8k
        delsagnew = 0.0_r8k
        delst = 0.0_r8k
        afdn = 1.0_r8k
        afgr = 1.0_r8k
        afal = 1.0_r8k
        amfair = 0.0_r8k
        amfarg = 0.0_r8k
        amffg = 0.0_r8k
        amfhe = 0.0_r8k
        amfh2 = 0.0_r8k
        amfh2o = 0.0_r8k
        amfkry = 0.0_r8k
        amfn2 = 0.0_r8k
        amfxe = 0.0_r8k
        beta = 1.0_r8k
        catexf = 0.05_r8k
        chmfrh = 0.0_r8k
        chmfrw = 0.0_r8k
        coldwk = 0.1_r8k
        cldwks = 0.2_r8k
        crdt = 0.0_r8k
        crdtr = 0.0_r8k
        crephr = 10.0_r8k
        deng = 0.0_r8k
        dishsd = 0.0_r8k
        hdish = 0.0_r8k
        moxtype = 1
        enrpu39 = 0.0_r8k
        enrpu40 = 0.0_r8k
        enrpu41 = 0.0_r8k
        enrpu42 = 0.0_r8k
        fotmtl = 2.0_r8k
        roughc = 1.97e-5_r8k
        roughf = 7.87e-5_r8k
        sigftc = 0.0_r8k
        sigftex = 0.0_r8k
        sigfgr = 0.0_r8k
        sigswell = 0.0_r8k
        sigcreep = 0.0_r8k
        siggro = 0.0_r8k
        sigcor = 0.0_r8k
        sigh2 = 0.0_r8k
        imox = 0
        igas = 0
        ngasmod = 2
        zr2vintage = 1
        idxgas = 1
        ifixedtsurf = 0
        ivardm = 0
        ncreep = 0
        nheal = 1
        nopt = 0
        nplot = 0
        nread = 0
        nrestr = 0
        ntape = 0
        jdlpr = 1
        nunits = 1
        calcoxide = .TRUE.
        cooltype = 0
        geom = 0
        gaphtcmult = 1.0_r8k
        ! Dating Default Values
        creeptime = 0.0_r8k
        creeppooltime = 5.0_r8k
        ncreepstep = 1
        ncreephist = 1
        datingtstep = 1.0e-2_r8k
        ! Extra values added in lieu of dating
        addgmles = 0.0_r8k
        addswell = 0.0_r8k
        stopox = 1.0e10_r8k
        ! Values for user supplied axially varying coolant conditions
        ifixedcoolt = 0
        ifixedcoolp = 0
        iplant = -2
        ppmn2 = 0.0_r8k
        ppmh2o = 0.0_r8k
        sgapf = 31.0_r8k
        chorg = 10.0_r8k
        irefab = 10000
        nrefab1 = 0
        nrefab2 = 0
        cplrefab = 0.0_r8k
        vsrefab = 0.0_r8k
        dspgrefab = 0.0_r8k
        dspgwrefab = 0.0_r8k
        fgpavrefab = 0.0_r8k
        airrefab = 0.0_r8k
        n2refab = 0.0_r8k
        arrefab = 0.0_r8k
        fgrefab = 0.0_r8k
        herefab = 1.0_r8k
        krrefab = 0.0_r8k
        xerefab = 0.0_r8k
        rc = -1.0_r8k
        gadoln = -1.0_r8k
        enrch = -1.0_r8k
        tcc = 0.0_r8k
        ! Fuel reference temperature for enthalpy calcuations, F
        tref = 77.0_r8k
        ! Fuel sintering temperature, F
        tsint = 2911.0_r8k
        ! Temperature at which fill gas pressure is specified, F
        TgasFab = 77.0_r8k
        ! Swelling limit, volume %
        slim = 0.05_r8k
        ifba = 0.0_r8k
        b10 = 0.0_r8k
        zrb2thick = 0.0_r8k
        zrb2den = 90.0_r8k
        icor = 0
        rprm1 = 3.45_r8k
        frcoef = 0.015_r8k
        igascal = 1
        xt = 0.0_r8k
        cladt = 0.0_r8k
        jnsurftemp = 0
        crudmult = -1.0_r8k
        flux = 0.221e17_r8k
        nap1 = na + 1
        ctmax = 386.33_r8k
        qend = 0.3_r8k
    END IF
    !
    ! Read the $frpcon block
    READ (iunit, frpcon, IOSTAT=InputStat)
    IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'frpcon')
    ! Reset nread and nrestr so that they can only be used by $developer input block
    nread = 0
    nrestr = 0
    !
    ! Read the $frpmox block if MOX fuel is being modeled
    IF (imox == 1 .OR. imox == 2) THEN
        READ (iunit, frpmox, IOSTAT=InputStat)
        IF (InputStat /= 0) CALL Namelist_Read_Error (iunit, 'frpmox')
    END IF
    !
    ! Check to see if $SpentFuel exists
    CALL SpentFuelRead
    !
    ! Check to see if user is a developer using developer options
    CALL DeveloperOptions
    !
    ! Set j = 2 to correspond to bottom axial node
    j = 2
    ! Fix ProblemTime
    DO i = im + 1, 1, -1
        ProblemTime(i) = ProblemTime(i-1)
    END DO
    ProblemTime(0) = 0.0_r8k
    ! Check to see if user wants to re-define # of timesteps
    IF (TimeIntegration > 0) CALL ResetTimesteps
    ! Redefine the dimensions for jnsurftemp
    jnsurftemp_max = MAXVAL(jnsurftemp)
    ! Allocate temporary arrays
    ALLOCATE (dumarray1(1:naxim))
    ALLOCATE (dumarray2(1:naxim))
    dumarray1 = 0.0_r8k
    dumarray2 = 0.0_r8k
    ! If fraptran start tape made, change jdlpr to 0
    IF (ntape > 0) jdlpr = 0
    ! For HWR Use rprm1_frapcon=2.21 based on JNM 255,222-233
    IF (iplant == -4) rprm1 = 2.21_r8k
    ! Make grainsize be 10 micrometers, regardless of user input value
    grnsize = 10.0_r8k
    !
    ! The following arrays may be input as either a single value or na
    ! Ensure that if they were put in as a single value that the value is applied to all axial nodes
    ! Note that they are initialized to -1.0 to allow for a user-input value of 0.0
    !
    ! Cladding Outer Diameter
    IF (dco(2) < 0.0_r8k) THEN ! Single value input
        dco = dco(1)
    ELSE ! User-defined input values
        IF (MINVAL(dco) <= 0.0_r8k) THEN
            i = MINLOC(dco,DIM=1)
            IF (i /= na) THEN
                WRITE (0,101) dco(i), i
                WRITE (ounit,101) dco(i), i
101             FORMAT ('Warning: Cladding diameter set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            IF (dco(na) <= 0.0_r8k) dco(na) = dco(na - 1)
        END IF
    END IF
    ! Cladding thickness
    IF (thkcld(2) < 0.0_r8k) THEN ! Single value input
        thkcld = thkcld(1)
    ELSE ! User-defined input values
        IF (MINVAL(thkcld) <= 0.0_r8k) THEN
            i = MINLOC(thkcld,DIM=1)
            IF (i /= na) THEN
                WRITE (0,102) thkcld(i), i
                WRITE (ounit,102) thkcld(i), i
102             FORMAT ('Warning: Cladding thickness set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            IF (thkcld(na) <= 0.0_r8k) thkcld(na) = thkcld(na - 1)
        END IF
    END IF
    ! Gap Thickness
    IF (thkgap(2) < 0.0_r8k) THEN ! Single value input
        thkgap = thkgap(1)
    ELSE ! User-defined input values
        IF (MINVAL(thkgap) <= 0.0_r8k) THEN
            i = MINLOC(thkgap,DIM=1)
            IF (i /= na) THEN
                WRITE (0,103) thkgap(i), i
                WRITE (ounit,103) thkgap(i), i
103             FORMAT ('Warning: Gap thickness set to ',e10.3,' at axial node ',i4, ' which is outside', &
                  &     ' of the bounds (>=0.0).',/,9x,'Program execution stopped.')
                STOP
            END IF
            ! Allow for a gap of 0.0 but not negative
            IF (thkgap(na) < 0.0_r8k) thkgap(na) = thkgap(na - 1)
        END IF
    END IF
    ! Central hole
    IF (rc(2) < 0.0_r8k) rc = rc(1)
    DO i = 1, na
        IF (rc(i) < 0.0_r8k) rc(i) = 0.0_r8k
    END DO
    ! Gadolinia content
    IF (gadoln(2) < 0.0_r8k) gadoln = gadoln(1)
    DO i = 1, nt
        IF (gadoln(i) < 0.0_r8k) gadoln(i) = 0.0_r8k
    END DO
    ! U-235 enrichment
    IF (enrch(2) < 0.0_r8k) enrch = enrch(1)
    DO i = 1, nt
        IF (enrch(i) <= 0.0_r8k) THEN
            WRITE (0,104) enrch(i), i
            WRITE (ounit,104) enrch(i), i
104         FORMAT ('Warning: Enrichment set to ',e10.3,' at axial node ',i4, ' which is outside', &
              &     ' of the bounds (>0.0).',/,9x,'Will reset to 1.0E-10 to ensure code stability')
            enrch(i) = 1.0E-10_r8k
        END IF
    END DO
    ! Pu content
    IF (comp(2) < 0.0_r8k) comp = comp(1)
    DO i = 1, nt
        IF (comp(i) < 0.0_r8k) THEN
            WRITE (0,105) comp(i), i
            WRITE (ounit,105) comp(i), i
105         FORMAT ('Warning: Pu content set to ',e10.3,' at axial node ',i4, ' which is outside', &
              &     ' of the bounds (>=0.0).',/,9x,'Will reset to 0.0')
            comp(i) = 0.0_r8k
        END IF
    END DO
    ! Account for gamma heating (if supplied)
    IF (modheat > 0.0_r8k) qmpy = qmpy * (1.0_r8k - modheat)
    ! Set axial values for hydraulic diameter (de), clad outer diameter (dci), pellet diameter (dp) and cold gap (cdg)
    DO i = 1, na
        de(i) = 4.0_r8k * ((pitch ** 2) - (pi / 4.0_r8k) * dco(i) ** 2) / (pi * dco(i))
        dci(i) = dco(i) - 2.0_r8k * thkcld(i)
        dp(i) = dci(i) - 2.0_r8k * thkgap(i)
        cdg(i) = (dci(i) - dp(i)) * 1.0e3_r8k
    END DO
    ! Start the crud film at 0.0 for user specified crud growth
    IF (icor == 2) crdt = 0.0_r8k
    !
    buin = buin(1)
    ! Check to see that ivardm was properly set in the input file
    IF (ivardm == 0) deltaz(1:nt) = totl / nt
    ! Plenum height
    deltaz(na) = cpl
    ! Set the conversion factors based on the input units
    IF (nunits == 0) pitch = 100.0_r8k * pitch ! (cm)
    ! If fixed coolant conditions (nsp=0), apply 1st timestep value to all timesteps (im)
    IF (nsp == 0) THEN
        go = go(1)
        p2 = p2(1)
        tw = tw(1)
    END IF
    ! Set spring variables
    Spring%vs = vs
    Spring%dspgw = dspgw
    Spring%dspg = dspg
    ! The following call to check edits the input information for obvious errors
    CALL check
    !
    IF (iquit == 1) THEN
        DEALLOCATE(dumarray1)
        DEALLOCATE(dumarray2)
        RETURN
    END IF
    DO ii = 1, nt
        ij = nt - ii + 1
        ctmax(ij+1) = ctmax(ij)
    END DO
    IF (nunits == 0) THEN
        IF (crdt == 1.0_r8k) crdt = 2.54e-5_r8k
        IF (crdtr == 1.1415525e-4_r8k) crdtr = 8.05e-13_r8k
        IF (roughc == 1.97e-5_r8k) roughc = 5.0e-7_r8k
        IF (roughf == 7.87e-5_r8k) roughf = 2.0e-6_r8k
        IF (tref == 77.0_r8k) tref = 298.15_r8k
        IF (tsint == 2911.0_r8k) tsint = 1872.59_r8k
        IF (TGasFab == 77.0_r8k) TGasFab = 298.15_r8k
        cpl = cpl * mtoin
        cplrefab = cplrefab * mtoin
        cdg = cdg * mtoin
        crdt = crdt * mtomil
        crdtr = crdtr * mtomil * hrtosec
        dishsd = dishsd * mtoin
        dspg = dspg * mtoin
        Spring%dspg = Spring%dspg * mtoin
        dspgrefab = dspgrefab * mtoin
        dspgw = dspgw * mtoin
        Spring%dspgw = Spring%dspgw * mtoin
        dspgwrefab = dspgwrefab * mtoin
        fgpav = fgpav * PatoPSI
        fgpavrefab = fgpavrefab * PatoPSI
        hdish = hdish * mtoin
        chmfrh = chmfrh * mtoin
        chmfrw = chmfrw * mtoin
        hplt = hplt * mtoin
        rc = rc * mtoin
        thkcld = mtoin * thkcld
        thkgap = mtoin * thkgap
        zrb2thick = zrb2thick * mtoin
        tcc = tcc * mtoin
        totl = totl * mtoft
        deltaz = deltaz * mtoft
        roughf = roughf * mtoin
        roughc = roughc * mtoin
        tref = tkf(tref)
        tsint = tkf(tsint)
        TGasFab = tkf(TGasFab)
        DO ii = 1, (nt + 1)
            buin(ii) = buin(ii) * MWskgUtoMWdMTU
            dco(ii) = dco(ii) * mtoin
            dci(ii) = dci(ii) * mtoin
            dp(ii) = dp(ii) * mtoin
            de(ii) = de(ii) * mtoin
            ctmax(ii) = tkf(ctmax(ii))
        END DO
        DO ii = 1, im
            go(ii) = go(ii) * ksm2tolbhrft2
            IF (igascal == 0) p1(ii) = p1(ii) * PatoPSI
            p2(ii) = p2(ii) * PatoPSI
            qmpy(ii) = qmpy(ii) / mtoft
            tw(ii) = tkf(tw(ii))
        END DO
        x = x * mtoft
    END IF
    ! Convert creep storage temperature & pressure used in Dating Model
    IF (idatingcreep > 0 .AND. nunits == 1) THEN !Convert from British to SI
        DO icreep = 1, ncreeptab
            creeptabtemp(icreep) = tfc(creeptabtemp(icreep))
            creeptabstress(icreep) = creeptabstress(icreep) * PSItoMPa
        END DO
    END IF
    ! Set up array for fixed cladding temperatures
    IF (ifixedtsurf == 1) THEN
        axheight = totl / nt
        DO kt = 1, nt + 1
            IF (kt == 1) THEN
                axialnode(kt) = 0.0_r8k
            ELSE
                axialnode(kt) = axialnode(kt-1) + axheight
            END IF
        END DO
        numshapes = MAXVAL(jstsurftemp)
        IF (nunits == 0) THEN
            DO kt = 1, naxim
                xt(kt) = xt(kt) * mtoft
                cladt(kt) = tkf(cladt(kt))
            END DO
        END IF
        nstart = 0
        DO mt = 1, numshapes
            DO kt = 1, jnsurftemp(mt)
                dumarray1(kt) = xt(kt + nstart)
                dumarray2(kt) = cladt(kt + nstart)
            END DO
            nstart = nstart + jnsurftemp(mt)
            DO kt = 1, nt + 1
                dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, jnsurftemp(mt))
            END DO
        END DO
        DO mt = 2, im + 1
            IF (jstsurftemp(mt-1) > 0) THEN
                DO kt = 1, nt + 1
                    cladtarray(mt,kt) = dumarray3(jstsurftemp(mt-1),kt)
                END DO
                cladtarray(mt,na+1) = dumarray3(jstsurftemp(mt-1),na)
            END IF
        END DO
    END IF
    ! Check to see if fixed coolant temperatures or pressures
    IF (ifixedcoolt == 1 .OR. ifixedcoolp == 1) THEN
        ! Determine the axial node increments used by the code
        axheight = totl / nt
        axialnode(1) = 0.0_r8k
        DO kz = 2, na
            axialnode(kz) = axialnode(kz-1) + deltaz(kz-1)
        END DO
        ! Determine the # of axial nodes supplied by the user
        ncoolz = COUNT(zcool(2:na) > 0.0_r8k) + 1
        IF (ncoolz < 3) THEN
            WRITE (ounit,*) 'Error: # of zcool pairs is < 3. Code Stopped'
            STOP
        END IF
        ! Reduce the array size to save memory
        CALL ReAllocateArray(na, ncoolz, 1, zcool)
        ! Convert axial heights (m to ft)
        IF (nunits == 0) zcool(1:ncoolz) = zcool(1:ncoolz) * mtoft ! Convert m to feet
        DO kz = 2, ncoolz
            ! Determine the # of axial nodes supplied by the user
            IF (zcool(kz) <= 0.0_r8k) THEN
                WRITE (ounit,*) 'Error: Axial heights for zcool pairs is not increasing. Code Stopped'
                STOP
            ELSE IF (zcool(ncoolz) /= totl) THEN
                WRITE (ounit,*) 'User supplied axial heights zcool not equal to total length of fuel rod, totl'
                WRITE (ounit,*) 'zcool(top) = ',zcool(kz-1),'totl = ',totl
                WRITE (ounit,*) 'This will be reset to equal totl.'
                zcool(ncoolz) = totl  ! Force the last node to be equal to the total fuel length
            END IF
        END DO
        ! Fixed coolant temperatures
        IF (ifixedcoolt == 1) THEN
            ! Determine the # of temperature/history pairs
            ncoolanthist = COUNT(tcoolant(1:(im*ncoolz)) > 0.0_r8k)
            npairs = ncoolanthist / ncoolz
            ! Reduce the array size to save memory
            CALL ReAllocateArray(im*na, ncoolanthist, 1, tcoolant)
            ! Convert coolant temperatures from K to F if necessary
            IF (nunits == 0) THEN
                DO kt = 1, (ncoolanthist)
                    tcoolant(kt) = tkf(tcoolant(kt))
                END DO
            END IF
            DO mt = 1, im
                DO kt = 1, ncoolz
                    dumarray1(kt) = zcool(kt)
                    IF (mt <= npairs) THEN
                        dumarray2(kt) = tcoolant(mt + (kt-1) * npairs)
                    ELSE ! Last state is assumed to happen for all remaining timesteps
                        dumarray2(kt) = tcoolant(kt * npairs)
                    END IF
                END DO
                DO kt = 1, na
                    dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, ncoolz)
                END DO
            END DO
            DO mt = 2, (im + 1)
                DO kt = 1, na
                    coolanttemp(1,kt) = dumarray3(1,kt)
                    coolanttemp(mt,kt) = dumarray3(mt-1,kt)
                END DO
                coolanttemp(1,na+1) = dumarray3(1,na)
                coolanttemp(mt,na+1) = dumarray3(mt-1,na)
            END DO
        END IF
        ! Check to see if fixed coolant pressures
        IF (ifixedcoolp == 1) THEN
            ! Determine the # of temperature/history pairs
            ncoolanthist = COUNT(Pcoolant(1:(im*ncoolz)) > 0)
            npairs = ncoolanthist / ncoolz
            ! Reduce the array size to save memory
            CALL ReAllocateArray(im*na, ncoolanthist, 1, pcoolant)
            ! Convert coolant pressures from Pa to Psi
            IF (nunits == 0) pcoolant(1:ncoolanthist) = pcoolant(1:ncoolanthist) * PatoPSI
            DO mt = 1, im
                DO kt = 1, ncoolz
                    dumarray1(kt) = zcool(kt)
                    IF (mt <= npairs) THEN
                        dumarray2(kt) = Pcoolant(mt + (kt-1) * npairs)
                    ELSE ! Last state is assumed to happen for all remaining timesteps
                        dumarray2(kt) = Pcoolant(kt * npairs)
                    END IF
                END DO
                DO kt = 1, na
                    dumarray3(mt,kt) = terp(axialnode(kt), dumarray1, dumarray2, ncoolz)
                END DO
            END DO
            DO mt = 2, im + 1
                DO kt = 1, na
                    coolantpressure(1,kt) = dumarray3(1,kt)
                    coolantpressure(mt,kt) = dumarray3(mt-1,kt)
                END DO
            END DO
        END IF
    END IF
    ! Crud multiplier. If not supplied, set equal to 1.0
    IF (crudmult(1) == -1.0_r8k) crudmult(1:na) = 1.0_r8k
    ! If not supplied for plenum (by default, not supplied) set equal to value for cladding node directly below plenum.
    IF (crudmult(na) == -1.0_r8k) crudmult(na) = crudmult(na-1)
    ! Crud thickness = crud thickness * axial crud rate multiplier
    crdtt(1:na) = crdt * crudmult(1:na)
    ! # of axial nodes + 1
    ir1 = nt + 1
    ! Check to ensure LHGR > 0 and an axial power shape is provided
    DO ii = 1, im
        IF (jst(ii) == 0) jst(ii) = 1
        IF (qmpy(ii) == 0.0_r8k) qmpy(ii) = 1.0e-6_r8k
    END DO
    ! # of axial power shapes
    jjc = MAXVAL(jst)
    ! Resize qaxnorm if jjc < im
    IF (jjc < im) CALL ReAllocateArray2D(na, na, 1, im+1, jjc, 1, qaxnorm)
    ! As-fabricated fuel density (g/in^3)
    rhofuel = MatProp ('FUEL', 'ASFABDENSITY', dummy)
    ! Calculate the empty volume of the plenum
    cpv = pi * dci(na) ** 2 * cpl / 4.0_r8k
    ! Calculate the volume of the spring
    Spring%Vcold = vs * (pi * (dspgw / 2.0_r8k) ** 2) * pi * (dspg - dspgw)
    ! Calculate the volume fraction of the plenum occupied by the spring
    Spring%Vf = Spring%Vcold / cpv
    ! Calculate the empty volume of the plenum
    cpv = cpv - Spring%Vcold
    ! Calculation of initial fuel porosity
    prty = (100.0_r8k - den) / 100.0_r8k
    nrm1 = nr - 1
    ! Fuel porosity
    FORALL (jj=1:na-1,i=1:nr-1) porosold(i,jj) = prty / 3.0_r8k
    ! Calculation of the radial node placement in the fuel region. This function will place more nodes
    ! near the surface of the pellet to account for rim effects. node 1 is on the fuel surface.
    DO jj = 1, nt
        DO i = 1, nr
            crad(i,jj) = (1.0_r8k - (REAL(i-1) / REAL(nr-1)) ** 3) * (dp(jj) / 2.0_r8k - rc(jj)) + rc(jj)
            ! Subroutines fueltp and tubrnp use a reversed nodalization_frapcon
            rrev(nr-(i-1),jj) = crad(i,jj)
        END DO
    END DO
    ! cfva - total cold fuel volume assuming cylindrical pellets
    cfva = 0.0_r8k
    DO jj = 1, nt
        cfva = cfva + pi * dp(jj) ** 2 * deltaz(jj) * 3.0_r8k
    END DO
    cfv = 0.0_r8k
    ! Determine the cold free volume (cfv)
    DO jj = 1, nt
        IF (hdish <= 0.0_r8k .AND. chmfrh <= 0.0_r8k) THEN !No dish or chamfers are modeled
            DO l = 1, nrm1
                ringvol(l,jj) = (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * pi * deltaz(jj) * fttoin
                coldringl(l,jj) = deltaz(jj) * fttoin
            END DO
            cfv = cfva - pi * rc(jj) * rc(jj) * totl * fttoin
        ELSE
            ! dish radius calculation
            dishsg = dp(jj) / 2.0_r8k - dishsd
            ! dish radius of curvature
            IF (hdish <= 0.0_r8k) THEN
                rdish = 0.0_r8k
            ELSE
                rdish = (hdish * hdish + dishsg * dishsg) / (2.0_r8k * hdish)
            END IF
            ! chamfer inner radius
            rchmfr = dp(jj) / 2.0_r8k - chmfrw
            ! volume of pellet annulus
            vplt = pi * rc(jj) * rc(jj) * deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
            ! radial pellet ring height calculation
            DO l = nrm1, 1, -1
                IF (rdish < crad(l,jj)) THEN
                    hi = hdish
                ELSE
                    hi = rdish - MAX(SQRT(rdish ** 2 - crad(l,jj) ** 2), (rdish - hdish))
                END IF
                ringvol(l,jj) = (deltaz(jj) / hplt) * pi * (crad(l,jj) ** 2 * (hplt + 2.0_r8k * (hi - hdish)) - &
                  &              2.0_r8k * hi ** 2 * (rdish - hi / 3.0_r8k)) * fttoin - vplt
                IF (chmfrh > 0.0_r8k .AND. chmfrw > 0.0_r8k) THEN
                    IF (crad(l,jj) > rchmfr) ringvol(l,jj)   = &
                      & (deltaz(jj) / hplt) * (pi * (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * hplt - &
                      &  2.0_r8k * pi * crad(l,jj) ** 2 * chmfrh * (crad(l,jj) - rchmfr) / chmfrw + &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l,jj) - rchmfr) / chmfrw * &
                      &  (crad(l,jj) ** 2 + (rchmfr) ** 2 + crad(l,jj) * (rchmfr))) * fttoin
                    IF (crad(l+1,jj) > rchmfr) ringvol(l,jj) = &
                      & (deltaz(jj) / hplt) * (pi * (crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * hplt - &
                      &  2.0_r8k * pi * crad(l,jj) ** 2 * chmfrh * (crad(l,jj) - rchmfr) / chmfrw + &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l,jj) - rchmfr) / chmfrw * &
                      &  (crad(l,jj) ** 2 + (rchmfr) ** 2 + crad(l,jj) * (rchmfr)) + &
                      &  2.0_r8k * pi * crad(l+1,jj) ** 2 * chmfrh * (crad(l+1,jj) - rchmfr) / chmfrw - &
                      &  2.0_r8k * pi / 3.0_r8k * chmfrh * (crad(l+1,jj) - rchmfr) / chmfrw * &
                      &  (crad(l+1,jj) ** 2 + (rchmfr) ** 2 + crad(l+1,jj) * (rchmfr))) * fttoin
                END IF
                coldringl(l,jj) = ringvol(l,jj) / ((crad(l,jj) ** 2 - crad(l+1,jj) ** 2) * pi)
                vplt = vplt + ringvol(l,jj)
            END DO
            cfv = cfv + (vplt - pi * rc(jj) * rc(jj) * deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish))
            IF (rc(jj) > 0.0_r8k) coldringl(nr-1,jj) = deltaz(jj) * fttoin / hplt * (hplt - 2.0_r8k * hdish)
        END IF
    END DO
    pecdh = (cfva - cfv) / cfva
    ! Calculation of pellet open porosity
    gden = den - 1.25_r8k
    IF (gden >= 94.0_r8k) THEN
        fpor1 = 0.0_r8k
    ELSE IF (gden > 91.25_r8k) THEN
        fpor1 = 0.0012019636_r8k * (94.0_r8k - gden)
    ELSE
        fpor1 = 16.929682_r8k - 0.23285470_r8k * gden - 8.7183686e-4_r8k * gden ** 2 + 1.5242216e-5_r8k * gden ** 3
    END IF
    ! Calculation of as-fabricated roughness volume (rfnvff) and free volume (cvv) in in^3
    rfnvff = 0.0_r8k
    cvv = 0.0_r8k
    DO jj = 1, nt
        rfnvff = rfnvff + 14.4e-6_r8k * pi * dp(jj) * deltaz(jj) * ft2toin2 / cfv
        cvv = cvv + (dci(jj) ** 2 - (1.0_r8k - pecdh) * dp(jj) ** 2) * pi * deltaz(jj) * 3.0_r8k
    END DO
    cvv = cvv + cpv + rfnvff * cfv + fpor1 * cfv
    ! Initial rod pressure = fill gas pressure
    press = fgpav
    ! Set default gas value based on user input
    SELECT CASE (idxgas)
    CASE (1)
        amfhe = 1.0_r8k
    CASE (2)
        amfair = 1.0_r8k
    CASE (3)
        amfn2 = 1.0_r8k
    CASE (4)
        amffg = 1.0_r8k
    CASE (5)
        amfarg = 1.0_r8k
    END SELECT
    ! Set values for Xe & Kr
    IF (amffg > 0.0_r8k) THEN
        SELECT CASE (imox)
        CASE (0) !UO2 Xe/Kr Ratio
            amfkry = 0.15_r8k * amffg
            amfxe = 0.85_r8k * amffg
        CASE (1, 2) !MOX Xe/Kr Ratio
            amfkry = 0.0588_r8k * amffg
            amfxe = 0.9412_r8k * amffg
        CASE DEFAULT ! STOP
            WRITE (0,601) imox
            WRITE (ounit,601) imox
601         FORMAT ('Wrong value for imox called in subroutine inital for determing Xe/Kr Ratio. imox = ',i3)
            STOP
        END SELECT
    END IF
    ! Set gas molecular weight ratios
    gases(1) = amfhe
    gases(2) = amfarg
    gases(3) = amfkry
    gases(4) = amfxe
    gases(5) = amfh2
    gases(6) = amfn2
    gases(7) = amfair
    gases(8) = amfh2o
    ! Calculate initial gas moles (cmol)
    ! From PV=nRT, n = PV / R_igc*T = V/MWt_mix * (P/R_mix*T) where P/R_mix*T = Density
    GasDensity = MatProp ('GAS', 'DENSITY', tfk(TgasFab)) ! kg/m3
    MWtMix = MatProp ('GAS', 'MOLWT', tfk(TgasFab)) ! g/mol
    cmol = cvv * in3tom3 * GasDensity * kgtog / MWtMix
    ! Set input # of moles of each gas species
    hein = amfhe * cmol
    argin = amfarg * cmol
    IF (amffg > 0.0_r8k) THEN ! Specified fission gas fraction in input
        fgin = cmol * amffg
    ELSE ! Fission gas fraction is sum of Xe and Kr
        fgin = (amfxe + amfkry) * cmol
    END IF
    kryin = amfkry * cmol
    xein = amfxe * cmol
    h2in = amfh2 * cmol
    an2in = amfn2 * cmol
    airin = amfair * cmol
    h2oin = amfh2o * cmol
    ! Calculation of initial concentration of nitrogen in fuel
    angi = ppmn2 * cfv * rhofuel / (N2MWt * 1.0e6_r8k)
    DO i = 1, nt
        ang(i,1) = angi * (deltaz(i) / totl)
    END DO
    ! Calculation of initial concentration of water in fuel
    h2omi = ppmh2o * cfv * rhofuel / (H2OMWt * 1.0e6_r8k)
    DO i = 1, nt
        ah2og(i,1) = h2omi * (deltaz(i) / totl)
    END DO
    ! Fixed coolant conditions.
    IF (nsp == 0) THEN
        IF (ifixedcoolt == 1) THEN ! User supplied axial coolant temperatures
            tw(1:im) = coolanttemp(2:im+1,1)
        ELSE ! User supplied only one value to be used for all timesteps
            tw = tw(1)
        END IF
        IF (ifixedcoolp == 1) THEN ! User supplied axial coolant pressures
            p2(1:im) = coolantpressure(2:im+1,1)
        ELSE ! User supplied only one value to be used for all timesteps
            p2 = p2(1)
        END IF
        go = go(1)
    END IF
    ! Calculate initial concentrations of U-235, U-238, Pu-239, Pu-240, Pu-241 and Pu-242.
    CALL turbin
    ! Write some input file data to output file
    CALL print1
    ! Set sintering temperature, convert from F to K
    tsint = tfk(tsint)
    DO ii = 1, im
        qmpy_tmp = 0.0_r8k
        ! Normalize the heat flux over all axial cladding dimensions
        DO i = 1, nt
            qmpy_tmp = qmpy_tmp + (qmpy(ii) * kWtoBTUh / ((dco(i) * intoft) * pi)) * (deltaz(i) / totl)
        END DO
        qmpy(ii) = qmpy_tmp
    END DO
    ! Convert problem time from days to seconds
    ProblemTime = ProblemTime * daytosec
    ! Convert stopox from days to seconds
    stopox = stopox * daytosec
    ! The following coding creates an inital time step with small values of time and power to avoid instabilities
    DO ij = 1, im
        ii = im - ij + 1
        IF (igascal == 0) p1(ii+1) = p1(ii)
        p2(ii+1) = p2(ii)
        go(ii+1) = go(ii)
        tw(ii+1) = tw(ii)
        qmpy(ii+1) = qmpy(ii)
        ProblemTime(ii+1) = ProblemTime(ii)
        jst(ii+1) = jst(ii)
        jstsurftemp(ii+1) = jstsurftemp(ii)
        ! If the user option for addgmles(it) or addswell(it) > 0, it won't be applied until the first true timestep value (it = 2)
        addgmles(ii+1) = addgmles(ii)
        addswell(ii+1) = addswell(ii)
    END DO
    ! Set values for initial power step
    qmpy(1) = 3000.0_r8k
    ProblemTime(1) = 0.001_r8k
    jstsurftemp(1) = 0
    addgmles(1) = 0.0_r8k
    addswell(1) = 0.0_r8k
    ! Incriment im by 1 to account for initial power step
    im = im + 1
    !
    DEALLOCATE(dumarray1)
    DEALLOCATE(dumarray2)
    !
    END SUBROUTINE inital
    !
    !
    !
    SUBROUTINE check
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon
    USE DatingData_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutines checks the input file for errors
    !>@author
    !> Updated by P. Raynaud & Ian Porter, NRC
    !>@date
    !> May 2015
    !
    ! Internal
    !
    ! isw  - Flag used in the check subroutine for calculating the # of errors in the input file
    !
    INTEGER(ipk) :: i, ii, shape, ShapeStart, ShapeEnd, isw
    INTEGER(ipk), PARAMETER :: nfiles = 2
    REAL(r8k) :: dtot, aadd, totpu
    INTEGER(ipk), DIMENSION(nfiles) :: unit
    !
    isw = 0
    unit(1) = 0
    unit(2) = ounit
    ! Write the errors produced to the screen (unit = 0) and the output file (unit = ounit)
    ! If error is detected, on the last iteration (i = unitend), force quit of program
    DO i = 1, nfiles
        ! # of timesteps
        IF (im < 1) WRITE (unit(i),250) im
        IF (im < 1) isw = isw + 1
        IF (im <= 0 .AND. i == nfiles) im = 1
        ! # of axial nodes
        IF (na < 1) WRITE (unit(i),260) na - 1
        IF (na < 1) isw = isw + 1
        ! Fuel density
        IF (den <= 50.0_r8k .OR. den >= 100.0_r8k) WRITE (unit(i),290) den
        IF (den <= 50.0_r8k .OR. den >= 100.0_r8k) isw = isw + 1
        ! Peak to average power ratio
        IF (fa < 0.0_r8k .OR. fa > 5.0_r8k) WRITE (unit(i),310) fa
        IF (fa < 0.0_r8k .OR. fa > 5.0_r8k) isw = isw + 1
        ! Cladding type
        IF (icm /= 2 .AND. icm /= 4 .AND. icm /= 5 .AND. icm /= 6 .AND. icm /= 7) WRITE (unit(i),320) icm
        IF (icm /= 2 .AND. icm /= 4 .AND. icm /= 5 .AND. icm /= 6 .AND. icm /= 7) isw = isw + 1
        ! Cold plenum length
        IF (cpl < 0.0_r8k) WRITE (unit(i),370) cpl
        IF (cpl < 0.0_r8k) isw = isw + 1
        ! Crud rate
        IF (crdt < 0.0_r8k) WRITE (unit(i),380) crdt
        IF (crdt < 0.0_r8k) isw = isw + 1
        ! Indicator for axial power shape
        IF (iq < 0) WRITE (unit(i),390) iq
        IF (iq < 0) isw = isw + 1
        ! Dish shoulder width
        IF (dishsd < 0.0_r8k) WRITE (unit(i),400) dishsd
        IF (dishsd < 0.0_r8k) isw = isw + 1
        ! Spring diameter
        IF (Spring%dspg <= 0.0_r8k .OR. Spring%dspg > dci(na-1)) WRITE (unit(i),410) Spring%dspg
        IF (Spring%dspg <= 0.0_r8k .OR. Spring%dspg > dci(na-1)) isw = isw + 1
        ! Spring wire diameter
        IF (Spring%dspgw <= 0.0_r8k) WRITE (unit(i),420) Spring%dspgw
        IF (Spring%dspgw <= 0.0_r8k) isw = isw + 1
        ! Number of spring turns
        IF (Spring%vs <= 0.0_r8k) WRITE (unit(i),550) Spring%vs
        IF (Spring%vs <= 0.0_r8k) isw = isw + 1
        ! Fill gas pressure
        IF (fgpav < 0.0_r8k) WRITE (unit(i),430) fgpav
        IF (fgpav < 0.0_r8k) isw = isw + 1
        ! Dish height
        IF (hdish < 0.0_r8k) WRITE (unit(i),440) hdish
        IF (hdish < 0.0_r8k) isw = isw + 1
        ! Fuel pellet height
        IF (hplt < 0.0_r8k) WRITE (unit(i),450) hplt
        IF (hplt < 0.0_r8k) isw = isw + 1
        ! Crud model
        IF (icor < 0) WRITE (unit(i),460) icor
        IF (icor < 0) isw = isw + 1
        ! Fill gas type
        IF (idxgas < 0) WRITE (unit(i),470) idxgas
        IF (idxgas < 0) isw = isw + 1
        ! Output print control
        IF (nopt < 0) WRITE (unit(i),490) nopt
        IF (nopt < 0) isw = isw + 1
        ! Coolant conditions option
        IF (nsp < 0) WRITE (unit(i),500) nsp
        IF (nsp < 0) isw = isw + 1
        ! Chamfer width
        IF (chmfrw < 0.0_r8k .OR. chmfrw > dishsd) WRITE (unit(i),511) chmfrw
        IF (chmfrw < 0.0_r8k .OR. chmfrw > dishsd) isw = isw + 1
        ! Chamfer height
        IF (chmfrh < 0.0_r8k .OR. chmfrh > (hplt / 2.0_r8k)) WRITE (unit(i),512) chmfrh
        IF (chmfrh < 0.0_r8k .OR. chmfrh > (hplt / 2.0_r8k)) isw = isw + 1
        ! Dish radius of curvature
        IF (rdish < 0.0_r8k) WRITE (unit(i),520) rdish
        IF (rdish < 0.0_r8k) isw = isw + 1
        ! Active fuel height
        IF (totl <= 0.0_r8k) WRITE (unit(i),530) totl
        IF (totl <= 0.0_r8k) isw = isw + 1
        ! Units indicator
        IF (nunits < 0) WRITE (unit(i),540) nunits
        IF (nunits < 0) isw = isw + 1
        ! Elevation defining each power shape. Must start at height 0.0.
        IF (x(1) /= 0.0_r8k) WRITE (unit(i),560) x(1)
        IF (x(1) /= 0.0_r8k) isw = isw + 1
        ! Fuel roughness
        IF (roughf < 0.0_r8k) WRITE (unit(i),570) roughf
        IF (roughf < 0.0_r8k) isw = isw + 1
        ! Cladding roughness
        IF (roughc < 0.0_r8k) WRITE (unit(i),580) roughc
        IF (roughc < 0.0_r8k) isw = isw + 1
        ! Fission gas release nodes. Must be at least 6. (45 recommended by default)
        IF (ngasr < 6) WRITE (unit(i),590) ngasr
        IF (ngasr < 6) isw = isw + 1
        ! Fission gas release models
        IF (ngasmod /= 1 .AND. ngasmod /= 2 .AND. ngasmod /= 3 .AND. ngasmod /= 4) WRITE (unit(i),595) ngasmod
        IF (ngasmod /= 1 .AND. ngasmod /= 2 .AND. ngasmod /= 3 .AND. ngasmod /= 4) isw = isw + 1
        ! # of qf, x pairs for each axial power shape must be > 0
        IF (jn(1) < 0) WRITE (unit(i),600) jn(1)
        IF (jn(1) < 0) isw = isw + 1
        ! Cladding deformation models
        IF (mechan /= 1 .AND. mechan /= 2) WRITE (unit(i),605) mechan
        IF (mechan /= 1 .AND. mechan /= 2) isw = isw + 1
        dtot = 0.0_r8k
        ! Check (potentially) axially varying parameters
        DO ii = 1, (na - 1)
            ! Enrichment
            IF (enrch(ii) < 0.0_r8k .OR. enrch(ii) >= 100.0_r8k) WRITE (unit(i),300) enrch(ii)
            IF (enrch(ii) < 0.0_r8k .OR. enrch(ii) >= 100.0_r8k) isw = isw + 1
            ! Fuel pellet central annulus
            IF (rc(ii) < 0.0_r8k) WRITE (unit(i),510) ii, rc(ii)
            IF (rc(ii) < 0.0_r8k) isw = isw + 1
            ! Diameter of fuel pellet
            IF (dp(ii) <= 0.0_r8k) WRITE (unit(i),610) ii, dp(ii)
            IF (dp(ii) <= 0.0_r8k) isw = isw + 1
            ! Cladding inner diameter
            IF (dci(ii) <= dp(ii)) WRITE (unit(i),620) ii, dci(ii)
            IF (dci(ii) <= dp(ii)) isw = isw + 1
            ! Cladding outer diameter
            IF (dco(ii) <= dci(ii)) WRITE (unit(i),630) ii, dco(ii)
            IF (dco(ii) <= dci(ii)) isw = isw + 1
            ! Hydraulic diameter
            IF (de(ii) < 0.0_r8k) WRITE (unit(i),640) ii, de(ii)
            IF (de(ii) < 0.0_r8k) isw = isw + 1
            ! Axial node height
            IF (deltaz(ii) < 0.0_r8k) WRITE (unit(i),650) ii, deltaz(ii)
            IF (deltaz(ii) < 0.0_r8k) isw = isw + 1
            dtot = dtot + deltaz(ii)
            ! Input fuel burnup
            IF (buin(ii) < 0.0_r8k) WRITE (unit(i),660) ii, buin(ii)
            IF (buin(ii) < 0.0_r8k) isw = isw + 1
        END DO
        IF (ivardm /= 0) THEN
            ! Specified variable axial node length
            IF (dtot > (totl + 0.01_r8k) .OR. dtot < (totl - 0.01_r8k)) WRITE (unit(i),670) dtot
            IF (dtot > (totl + 0.01_r8k) .OR. dtot < (totl - 0.01_r8k)) isw = isw + 1
        END IF
        DO ii = 1, im
            ! Coolant flow rate
            IF (go(ii) < 0.0_r8k) WRITE (unit(i),680) ii, go(ii)
            IF (go(ii) < 0.0_r8k) isw = isw + 1
            ! If negative LHGR, using decay heat approximation
            IF (qmpy(ii) < 0.0_r8k) WRITE (unit(i),689) ProblemTime(ii)
            IF (ii > 1) THEN
                ! Don't do this check if using decay heating approximation for current or last timestep
                IF (qmpy(ii) > 0.0_r8k .AND. qmpy(ii-1) > 0.0_r8k) THEN
                    ! Test for max change in LHGR (5 kW/ft, 16.4 kW/m)
                    IF ((ABS(qmpy(ii) - qmpy(ii-1)) > 5.0_r8k .AND. nunits == 1) .OR. &
                      & (ABS(qmpy(ii) - qmpy(ii-1)) > 16.4_r8k .AND. nunits == 0)) WRITE (unit(i),691) ii, qmpy(ii)
                    IF (nread == 0 .OR. ii > 2) THEN
                        ! Test for max problem timestep (50 days)
                        IF (ProblemTime(ii) - ProblemTime(ii-1) > 50.0_r8k) WRITE (unit(i),692) ii, ProblemTime(ii)
                    END IF
                END IF
            END IF
            ! Problem time
            IF (ProblemTime(ii) <= 0.0_r8k) WRITE (unit(i),700) ii, ProblemTime(ii)
            IF (ProblemTime(ii) <= 0.0_r8k) isw = isw + 1
            ! Coolant Inlet temperature
            IF (tw(ii) < 0.0_r8k) WRITE (unit(i),710) ii, tw(ii)
            IF (tw(ii) < 0.0_r8k) isw = isw + 1
            ! Coolant pressure
            IF (p2(ii) <= 0.0_r8k .AND. (ifixedcoolp == 0)) WRITE (unit(i),720) ii, p2(ii)
            IF (p2(ii) <= 0.0_r8k .AND. (ifixedcoolp == 0)) isw = isw + 1
            ! Power shape number
            IF (jst(ii) < 0 .OR. jst(ii) > im) WRITE (unit(i),730) ii, jst(ii)
            IF (jst(ii) < 0 .OR. jst(ii) > im) isw = isw + 1
        END DO
        ! Increasing problem time
        DO ii = 1, (im - 1)
            IF (ProblemTime(ii + 1) <= ProblemTime(ii)) WRITE (unit(i),760) ii, ProblemTime(ii),ProblemTime(ii+1)
            IF (ProblemTime(ii + 1) <= ProblemTime(ii)) isw = isw + 1
        END DO
        ! User input gas composition must be normalized (equal to 1.0.)
        IF (idxgas == 6) THEN
            aadd = amffg + amfarg + amfh2o + amfhe + amfn2 + amfair + amfh2 + amfxe + amfkry
            IF (aadd /= 1.0_r8k) WRITE (unit(i),770) aadd
            IF (aadd < 0.99_r8k .OR. aadd > 1.01_r8k) isw = isw + 1
        END IF
        IF (iq == 1) THEN
            ! Peak to average power ratio must be greater than 1.0.
            IF (fa <= 1.0_r8k) WRITE (unit(i),840) fa
            IF (fa <= 1.0_r8k) isw = isw + 1
        ELSE
            ! Check all axial power shapes
            shape = 1
            ShapeStart = 1
            ShapeEnd = jn(shape)
            DO WHILE (jn(shape) > 0)
                DO ii = ShapeStart, ShapeEnd
                    ! Axial power shape must be >= 0.0.
                    IF (qf(ii) < 0.0_r8k) WRITE (unit(i),780) ii, qf(ii)
                    IF (qf(ii) < 0.0_r8k) isw = isw + 1
                    ! Max axial power shape factor must be 1.0
                    IF (qf(ii) > 1.0_r8k .AND. fa /= 1.0_r8k) WRITE (unit(i),820) ii, qf(ii)
                    IF (qf(ii) > 1.0_r8k .AND. fa /= 1.0_r8k) isw = isw + 1
                END DO
                ! Axial elevations corresponding to power shape must be increasing.
                DO ii = ShapeStart, (ShapeEnd - 1)
                    IF (x(ii+1) < x(ii)) WRITE (unit(i),790) ii+1, x(ii+1)
                    IF (x(ii+1) < x(ii)) isw = isw + 1
                END DO
                ! Max location for axial elevation must correspond to top of fuel pellet stack (totl)
                IF (x(ShapeEnd) /= totl) WRITE (unit(i),800) ShapeEnd, x(ShapeEnd)
                IF (x(ShapeEnd) /= totl) isw = isw + 1
                ! Exit if nshapes = im
                IF (shape == im) THEN
                    EXIT
                ELSE
                    ShapeStart = ShapeStart + jn(shape)
                    ShapeEnd = ShapeEnd + jn(shape + 1)
                    shape = shape + 1
                END IF
            END DO
        END IF
        ! Fuel type
        IF (imox < 0 .OR. imox > 2) WRITE (unit(i),900) imox
        IF (imox < 0 .OR. imox > 2) isw = isw + 1
        ! Plutonium only allowed if modeling MOX fuel (imox = 1 or 2)
        IF (imox == 0 .AND. MAXVAL(comp) /= 0.0_r8k) WRITE (unit(i),910) imox, MAXVAL(comp)
        IF (imox == 0 .AND. MAXVAL(comp) /= 0.0_r8k) isw = isw + 1
        IF ((imox == 1 .OR. imox == 2) .AND. MAXVAL(comp) <= 0.0_r8k) WRITE (unit(i),920) imox, MAXVAL(comp)
        IF ((imox == 1 .OR. imox == 2) .AND. MAXVAL(comp) <= 0.0_r8k) isw = isw + 1
        totpu = enrpu39 + enrpu40 + enrpu41 + enrpu42
        IF ((imox == 1 .OR. imox == 2) .AND. totpu > 100.001_r8k) WRITE (unit(i),930) totpu
        IF ((imox == 1 .OR. imox == 2) .AND. totpu > 100.001_r8k) isw = isw + 1
        IF ((imox == 1 .OR. imox == 2) .AND. totpu < 99.999_r8k) WRITE (unit(i),930) totpu
        IF ((imox == 1 .OR. imox == 2) .AND. totpu < 99.999_r8k) isw = isw + 1
        IF (nunits /= 0 .AND. nunits /= 1) WRITE (unit(i),950) nunits
        IF (nunits /= 0 .AND. nunits /= 1) isw = isw + 1
        !
        ! If input errors detected, set flag to stop execution (iquit)
        IF (isw > 0) THEN
            WRITE (unit(i),850) isw
            iquit = 1
            IF (i == nfiles) THEN
                iquit = 1
            ELSE
                isw = 0
            ENDIF
        END IF
    END DO
    !
250 FORMAT (1x,'im (number of time steps) not within limits (> 1) im =',i5)
260 FORMAT (1x,'na (number of axial nodes) not within limits (> 1) na= ',i5)
290 FORMAT (1x,'den (pellet density, %TD) not within limits (50 to 100) den=',e11.4)
300 FORMAT (1x,'enrch (fuel U-235 enrichment, %) not within limits (0 to 100) enrch=',e11.4)
310 FORMAT (1x,'fa (peak to average power ratio) not within limits (0 to 5) fa=',e11.4)
320 FORMAT (1x,'icm (cladding material index) not within limits (2, 4, 5, 6, or 7) icm=',i5)
370 FORMAT (1x,'cpl (plenum length) not within limits (>= 0) cpl=',e11.4)
380 FORMAT (1x,'crdt (rate of crud accumulation) not within limits (>= 0) crdt=',e11.4)
390 FORMAT (1x,'iq (indicator for axial power shape) not within limits (>= 0) iq=',i5)
400 FORMAT (1x,'dishsd (dish shoulder width) not within limits (>= 0) dishsd=',e11.4)
410 FORMAT (1x,'dspg (spring diameter) not within limits (0 <= dspg <= dci(cladding inner diameter) dspg=',e11.4)
420 FORMAT (1x,'dspgw (spring wire diameter) not within limits (>= 0) dspgw=',e11.4)
430 FORMAT (1x,'fgpav (fill gas pressure) not within limits (>= 0) fgpav=',e11.4)
440 FORMAT (1x,'hdish (dish depth) not within limits (>= 0) hdish=',e11.4)
450 FORMAT (1x,'hplt (pellet length) not within limits (>= 0) hplt=',e11.4)
460 FORMAT (1x,'icor (crud model index) not within limits (>= 0) icor=',e11.4)
470 FORMAT (1x,'idxgas (fill gas indicator) not within limits (>= 0) idxgas=',e11.4)
490 FORMAT (1x,'nopt (output indicator) not within limits (>= 0) nopt=',i5)
500 FORMAT (1x,'nsp (signal for time dependent coolant conditions) not within limits (>= 0) nsp=',i5)
510 FORMAT (1x,'rc (pellet inner radius) not within limits (>= 0) rc(',i5,')=',e11.4)
511 FORMAT (1x,'chmfrw (chamfer width) not within limits (0.0 <= chmfrw <= dishsd) chmfrw=',e11.4)
512 FORMAT (1x,'chmfrh (chamfer height) not within limits (0.0 <= chmfrh <= hplt/2.0) chmfrh=',e11.4)
520 FORMAT (1x,'rdish (dish radius) not within limits (>= 0) rdish=',e11.4)
530 FORMAT (1x,'totl (pellet stack length) not within limits (gt 0) totl=',e11.4)
540 FORMAT (1x,'nunits (units indicator) not within limits (>= 0) nunits=',e11.4)
550 FORMAT (1x,'vs (number of spring turns) not within limits (>= 0) vs=',e11.4)
560 FORMAT (1x,'x(1) (first axial elevation for axial profiles) not within limits (= 0.0) x(1)=',e11.4)
570 FORMAT (1x,'roughf (fuel surface roughness) not within limits (>= 0) roughf=',e11.4)
580 FORMAT (1x,'roughc (cladding surface roughness) not within limits (>= 0) roughc=',e11.4)
590 FORMAT (1x,'ngasr (radial nodes in gas release model) not within limits (>= 6) ngasr=',i5)
595 FORMAT (1x,'ngasmod (Fission gas release model) not within limits (1, 2, 3 or 4) ngasmod=',i5)
600 FORMAT (1x,'jn(1) (number of pairs in axial shape) not within limits (>= 0) jn(1)=',i5)
605 FORMAT (1x,'mechan (Mechanical deformation model) not within limits (1, or 2) mechan=',i5)
610 FORMAT (1x,'dp (pellet diameter) not within limits (> 0) dp(',i5,')=',e11.4)
620 FORMAT (1x,'dci (cladding inner diameter) not within limits (> dp) dci(',i5,')=',e11.4)
630 FORMAT (1x,'dco (cladding outer diameter) not within limits (> dci) dco(',i5,')=',e11.4)
640 FORMAT (1x,'de (hydraulic diameter) not within limits (>= 0) de(',i5,')=',e11.4)
650 FORMAT (1x,'deltaz not within limits (>= 0) deltaz(',i5,')=',e11.4)
660 FORMAT (1x,'buin not within limits (>= 0) buin=(',i5,') ',e11.4)
670 FORMAT (1x,'deltaz total not equal to totl, deltaz total=',e11.4)
680 FORMAT (1x,'go (coolant mass flux) not within limits (>= 0) go(',i5,')=',e11.4)
689 FORMAT (1x,'Decay heating will be used for time =',e11.4)
690 FORMAT (1x,'qmpy (rod average LHGR) not within limits (>= 0) qmpy=',e11.4)
691 FORMAT (1x, 'power step number',i5,' is greater than 5 kW/ft --consider smaller change; power=', e11.4)
692 FORMAT (1x, 'time step number',i5,' is greater than 50 days --consider smaller time steps; ProblemTime =',e11.4)
700 FORMAT (1x,'ProblemTime not within limits (> 0.0) ProblemTime(',i5,')=',e11.4)
710 FORMAT (1x,'tw (coolant inlet temperature) not within limits (> 0.0) tw(',i5,')=',e11.4)
720 FORMAT (1x,'p2 (coolant pressure) not within limits (> 0.0) p2(',i5,')=',e11.4)
730 FORMAT (1x,'jst (index for power shape) not within limits (1 to im) jst(',i5,')=',i5)
760 FORMAT (1x,'ProblemTime less than or equal to previous ProblemTime.',/,' previous step =' &
      &    ,i5,3x,'previous ProblemTime=',e11.4,3x,'ProblemTime=',e11.4)
770 FORMAT (1x,'fill gas fraction total not equal to 1.0, total=',e11.4)
780 FORMAT (1x,'qf (axial profile) not within limits (>= 0) qf(',i5,')=',e11.4)
790 FORMAT (1x,'x (elevation for axial profile) value less than previous x value, x(',i5,')=',e11.4)
800 FORMAT (1x,'x (top value for axial profile) not equal to totl (pellet stack length),, x(',i5,')=',e11.4)
820 FORMAT (1x,'fa greater than 1.0, maximum qf must equal 1.0  qf(',i5,') = ',e11.4)
840 FORMAT (1x,'when iq = 1, fa must be greater than 1.0   fa =',e11.4)
850 FORMAT (1x,'total number of errors=',i5)
860 FORMAT (////1x,50('*')//5x,'The ANS-5.4 fission gas release model may not calculate the correct fission gas ', &
      &         'release fraction if the',/5x,'time step size is less than 4 hours. This restriction applies only ', &
      &         'after the first day in-reactor however.',//20x,'time step',i5,' is only',e11.4,' days long'//1x,50('*')///)
900 FORMAT (1x,'imox (mox indicator) = ',i2,' should be between 0 and 2')
910 FORMAT (1x,'imox = ',i2,' comp (fraction of PuO2) should be 0.0.  comp = ',e11.4)
920 FORMAT (1x,'imox = ',i2,' comp (fraction of PuO2) should be greater than zero. comp = ',e11.4)
930 FORMAT (1x,'sum of enrpu39, enrpu40, enrpu41, and enrpu42 (Pu isotope fractions) should be 100.  Sum = ',e11.4)
950 FORMAT (1x,'nunits (Input/Output units flag) must be 0 or 1. nunits = ',i3)
    !
    END SUBROUTINE check
    !
    !
    !
    SUBROUTINE ResetTimesteps
    USE Kinds_frapcon
    USE conversions_frapcon
    USE variables_frapcon
    USE Functions_frapcon, ONLY : ShiftArray
    !>@brief
    !> This subroutine resets the # of timesteps to a user specified timestep interval
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 9/22/2014
    IMPLICIT NONE
    !
    INTEGER(ipk) :: i, jj, PrevVal, NextVal, count, npairs, ncoolanthist, ncoolz, kz
    INTEGER(ipk), DIMENSION(:), ALLOCATABLE :: jst_old, jstsurftemp_old
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dumarray1, qmpy_old, ProblemTime_old, go_old, p2_old, &
      &                                     tw_old, p1_old, tcoolant_old, pcoolant_old, addgmles_old
    REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dumarray2
    !
    ALLOCATE(jst_old(1:im+1))
    ALLOCATE(jstsurftemp_old(1:im+1))
    ALLOCATE(qmpy_old(1:im+1))
    ALLOCATE(go_old(1:im+1))
    ALLOCATE(p2_old(1:im+1))
    ALLOCATE(tw_old(1:im+1))
    ALLOCATE(p1_old(1:im+1))
    ALLOCATE(tcoolant_old(1:(im*na)))
    ALLOCATE(pcoolant_old(1:(im*na)))
    ALLOCATE(addgmles_old(1:im+1))
    !
    im_old = im
    ! Set new value for im
    im = ProblemTime(im_old) / newtimestep
    DO i = 0, im_old
        ProblemTime_Prev(i) = ProblemTime(i)
    END DO
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, ProblemTime)
    DO i = 1, im
        ProblemTime(i) = ProblemTime(i-1) + newtimestep
    END DO
    IF (ProblemTime(im) /= ProblemTime_Prev(im_old)) ProblemTime(im) = ProblemTime_Prev(im_old)
    !
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, go)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, p2)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, qmpy)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, tw)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, p1)
    CALL ReAllocateArray1DInteger((im_old + 1), (im + 1), 1, jst)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, acmfg)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, acmhe)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, acmh2)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, acmn2)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, amgpt)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, gasmo)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, hmgpt)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, tafa)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, taga)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, tsfa)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, taca)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkODCladTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkPelAveTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkPower)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkAveCladTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkIDCladTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkGap)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkFisGasRelFrac)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkPelSurfTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkH2up)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkPelCentTemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkIntefacePres)    
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pit)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkHoopStres)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkAxlStres)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkBurnup)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkHoopStrain)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkFuelPelOD)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkGapCond)
    CALL ReAllocateArray((im_old + 1), (im + 1), 0, pkZrO2)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, HeatFlux)
    CALL ReAllocateArray1DInteger((im_old + 1), (im + 1), 1, jstsurftemp)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, buavearray)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, voidvolarray)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, he_ifba)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, addgmles)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, addswell)
    CALL ReAllocateArray((im_old + 1), (im + 1), 1, addswell)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na+1, na+1, 1, coolanttemp)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, coolantpressure)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, storedearray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, cltemparray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, buarray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, strainarray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, straindiffarray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, dpwxarray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, creaparray)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, dumarray3)
    CALL ReAllocateArray2D((im_old + 1), (im + 1), 1, na, na, 1, cladtarray)
    CALL ReAllocateArray3D(ngasr, ngasr, 1, na, na, 1, (im_old + 1), (im + 1), 1, prdct)
    CALL ReAllocateArray3D((im_old + 1), (im + 1), 1, ngasr, ngasr, 1, na, na, 1, ansd)
    !
    DO i = 1, (im_old + 1)
        qmpy_old(i) = qmpy(i)
        go_old(i) = go(i)
        p2_old(i) = p2(i)
        tw_old(i) = tw(i)
        jst_old(i) = jst(i)
        jstsurftemp_old(i) = jstsurftemp(i)
        p1_old(i) = p1(i)
        addgmles_old(i) = addgmles(i)
    END DO
    !
    DO kz = 2, na
        ! Determine the # of axial nodes supplied by the user
        IF (zcool(kz) > 0.0_r8k) ncoolz = kz
    END DO
    CALL ReAllocateArray((na * (im_old)), (ncoolz * (im + 1)), 1, tcoolant)
    CALL ReAllocateArray((na * (im_old)), (ncoolz * (im + 1)), 1, pcoolant)
    ! Set # of axial nodes TH Data provided for
    DO i = 1, (ncoolz*(im_old))
        tcoolant_old(i) = tcoolant(i)
        pcoolant_old(i) = pcoolant(i)
    END DO
    ncoolanthist = 0
    DO kz = 1, (im*ncoolz)
        IF (Tcoolant(kz) > 0) ncoolanthist = ncoolanthist + 1
    END DO
    npairs = ncoolanthist/ncoolz
    !
    SELECT CASE (TimeIntegration)
    CASE (1) ! Linear Interpolation

        !CALL ShiftArray(im_old, im, ProblemTime, 1)
    CASE (2) ! Histogram
        !CALL ShiftArray(im_old, im, ProblemTime, 2)
        Count = 0
        PrevVal = 1
        NextVal = 0
        DO i = 1, im_old
            DO jj = 1, im
                count = count + 1
                IF (ProblemTime(Count) >= ProblemTime_Prev(i)) EXIT
            END DO
            IF (PrevVal == 1) THEN
                NextVal = jj
            ELSE
                NextVal = (PrevVal - 1) + jj
            END IF
            CycleSteps(i) = NextVal
            DO jj = PrevVal, NextVal
                qmpy(jj) = qmpy_old(i)
                go(jj) = go_old(i)
                p2(jj) = p2_old(i)
                tw(jj) = tw_old(i)
                jst(jj) = jst_old(i)
                jstsurftemp(jj) = jstsurftemp_old(i)
                p1(jj) = p1_old(i)
                addgmles(jj) = addgmles_old(i)
                !
                DO kz = 1, ncoolz
                    tcoolant(jj + (kz - 1) * im) = tcoolant_old(i + (kz - 1) * im_old)
                    pcoolant(jj + (kz - 1) * im) = pcoolant_old(i + (kz - 1) * im_old)
                END DO
            END DO
            PrevVal = NextVal + 1
        END DO
            !
    CASE DEFAULT
        WRITE (0, 100)
        WRITE (ounit, 100)
100     FORMAT('Error in subroutine ResetTimesteps! Wrong value set for TimeIntegration.',/,'Code Stopped.')
        STOP
    END SELECT
    !
    IF (ALLOCATED(dumarray1)) DEALLOCATE (dumarray1)
    IF (ALLOCATED(dumarray2)) DEALLOCATE (dumarray2)
    IF (ALLOCATED(jst_old)) DEALLOCATE(jst_old)
    IF (ALLOCATED(jstsurftemp_old)) DEALLOCATE(jstsurftemp_old)
    IF (ALLOCATED(qmpy_old)) DEALLOCATE(qmpy_old)
    IF (ALLOCATED(go_old)) DEALLOCATE(go_old)
    IF (ALLOCATED(p2_old)) DEALLOCATE(p2_old)
    IF (ALLOCATED(tw_old)) DEALLOCATE(tw_old)
    IF (ALLOCATED(p1_old)) DEALLOCATE(p1_old)
    IF (ALLOCATED(tcoolant_old)) DEALLOCATE(tcoolant_old)
    IF (ALLOCATED(pcoolant_old)) DEALLOCATE(pcoolant_old)
    !
    END SUBROUTINE ResetTimesteps
    !
END MODULE Initialization_frapcon



