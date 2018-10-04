module fpn_reader

    use conversions_frapcon

    implicit none

    integer, parameter :: ifile = 1012
    integer :: ierror
    integer :: ntimesteps, naxialnodes, nradialnodes

    integer :: na, ngasr, nr, nce, im, naxim, mechan, graphna, graphnr, icm, icor, idxgas, iplant, & 
             & iquit, ivardm, imox, jdlpr, igas, nopt, nplot, nread, nrestr, nsp, ntape, nunits,   &
             & ifixedtsurf, igascal, ngasmod, zr2vintage, irefab, nrefab1, nrefab2, ifixedcoolt,   &
             & ifixedcoolp, moxtype, DecayModel, iq

    real(8) :: beta, afdn, amfair, amfarg, amffg, amfhe, amfh2, amfh2o, amfkry, amfn2,     & 
             & amfxe, catexf, chorg, cldwks, cpl, crdt, crdtr, crephr, dcobol, deng, dishsd,    &
             & dspgrefab, dspgwrefab, fa, fgpav, fotmtl, hdish, hplt, pitch, ppmh2o, ppmn2, roughc,   &
             & rci, roughf, rsntr, sgapf, slim, totl, tsint, vsrefab, xein, grnsize, frcoef, ifba,    & 
             & b10, zrb2thick, zrb2den, sigftc, sigftex, sigfgr, sigswell, sigcreep, siggro, sigcor,  &
             & sigh2, chmfrh, chmfrw, cplrefab, fgpavrefab, airrefab, den, dspg, dspgw, vs, &
             & n2refab, arrefab, fgrefab, herefab, krrefab, xerefab, fpdcay, enrpu39, enrpu40, enrpu41, enrpu42


    INTEGER, DIMENSION(:), ALLOCATABLE :: jstsurftemp, jnsurftemp, jn, jst

    REAL(8), DIMENSION(:), ALLOCATABLE :: buin, comp, ctmax, dco, deltaz, enrch, flux, gadoln, &
                                        & go, p2, ProblemTime, qend, qf, qmpy, rc, thkcld, thkgap,    &
                                        & tw, x, xt, cladt, p1, crudmult, tcoolant, zcool, pcoolant

    include "fp_ivars_h.f90"

    contains

    subroutine read_frapcon_file(filename)

        implicit none

        character(*) :: filename

        namelist / frpcn  / im, mechan, na, ngasr, nr, nce, graphna, graphnr

        namelist / frpcon / afdn, amfair, amfarg, amffg, amfhe, amfh2, amfh2o, amfkry, amfn2, amfxe, &
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

        namelist / frpmox / enrpu39, enrpu40, enrpu41, enrpu42, moxtype

        open(ifile, file=filename, status='unknown', form='formatted')

        read(ifile, frpcn,  iostat=ierror)

        allocate(buin(1:na+1))
        allocate(comp(1:na+1))
        allocate(ctmax(1:na+1))
        allocate(dco(1:na+1))
        allocate(deltaz(1:na+1))
        allocate(enrch(1:na+1))
        allocate(flux(1:na+1))
        allocate(gadoln(1:na+1))
        allocate(go(1:im+1))
        allocate(jn(1:im+1))
        allocate(jst(1:im+1))
        allocate(p2(1:im+1))
        allocate(ProblemTime(0:im+1))
        allocate(qend(1:im+1))
        allocate(qf(1:(na+1)*im))
        allocate(qmpy(1:im+1))
        allocate(rc(1:na+1))
        allocate(thkcld(1:na+1))
        allocate(thkgap(1:na+1))
        allocate(tw(1:im+1))
        allocate(x(1:(na+1)*im))
        allocate(xt(1:(na+1)*im))
        allocate(cladt(1:(na+1)*im))
        allocate(jstsurftemp(1:im+1))
        allocate(jnsurftemp(1:im+1))
        allocate(p1(1:im+1))
        allocate(crudmult(1:na+1))
        allocate(tcoolant(1:(na+1)*im))
        allocate(zcool(1:na+1))
        allocate(pcoolant(1:(na+1)*im))

        mechan              = 2
        ngasmod             = 2
        icm                 = 4
        icor                = 0
        zr2vintage          = 1
        ifixedcoolt         = 0
        ifixedcoolp         = 0
        ifixedtsurf         = 0
        jst(1)              = 1                           
        nplot               = 0                           
        iq                  = 1                           
        jdlpr               = 0                           
        nunits              = 1                           
        ivardm              = 0
        nsp                 = 1                           
        chmfrh              = 0.d0                        
        chmfrw              = 0.d0                        
        comp                = 0.d0                        
        moxtype             = 0                           
        ifba                = 0.0d0                       
        b10                 = 0.0d0                       
        zrb2thick           = 0.0d0                       
        zrb2den             = 90.0d0                      
        ppmh2o              = 0.0d0                       
        ppmn2               = 0.0d0                       
        deng                = 0.0d0                       
        tsint               = 2911.0d0                    
        cldwks              = 0.2d0                       
        catexf              = 0.05d0                      
        chorg               = 10.0d0                      
        amfair              = 0.0d0                       
        amfarg              = 0.0d0                       
        amffg               = 0.0d0                       
        amfhe               = 0.0d0                       
        amfh2               = 0.0d0                       
        amfkry              = 0.0d0                       
        amfn2               = 0.0d0                       
        amfxe               = 0.0d0                       
        crdtr               = 0.0d0                       
        crudmult            =-1.0d0                       
        flux                = 0.221d+17                   
        DecayModel          = 1                           
        fpdcay              = 1.0d0                       
        ifixedtsurf         = 0                           
        xt                  = 0.0d0                       
        cladt               = 0.0d0                       
        jnsurftemp          = 0                           
        zcool               = 0.0d0                       
        crephr              = 10.0d0                      
        sgapf               = 31.0d0                      
        qend                = 0.3d0                       
        igas                = 0                           
        frcoef              = 0.015d0                     
        igascal             = 1                           
        sigftc              = 0.0d0                       
        sigftex             = 0.0d0                       
        sigfgr              = 0.0d0                       
        sigswell            = 0.0d0                       
        sigcreep            = 0.0d0                       
        siggro              = 0.0d0                       
        sigcor              = 0.0d0                       
        sigh2               = 0.0d0                       
        irefab              = 10000                       
        nrefab1             = 0                           
        nrefab2             = 0                           
        cplrefab            = 0.0d0                       
        vsrefab             = 0.0d0                       
        dspgrefab           = 0.0d0                       
        dspgwrefab          = 0.0d0                       
        fgpavrefab          = 0.0d0                       
        airrefab            = 0.0d0                       
        n2refab             = 0.0d0                       
        arrefab             = 0.0d0                       
        fgrefab             = 0.0d0                       
        herefab             = 1.0d0                       
        krrefab             = 0.0d0                       
        xerefab             = 0.0d0                       
        nopt                = 0                           
        nplot               = 0                           
        ntape               = 0                           
        nread               = 0                           
        nrestr              = 0                           
        cpl                 = 6.3386d0                    
        crdt                = 0.d0                        
        thkcld              = 0.0224d0                    
        thkgap              = 0.0033d0                    
        dco                 = 0.3740d0                    
        pitch               = 0.5d0                     
        rc                  = 0.0d0                     
        fotmtl              = 2.d0                        
        dishsd              = 0.0d0                     
        den                 = 94.43d0                   
        dspg                = 0.3                         
        fa                  = 1.d0                        
        dspgw               = 0.0394                      
        enrch               = 1.0d0                     
        fgpav               = 340.84                      
        hdish               = 0.d0                    
        hplt                = 0.387d0                     
        idxgas              = 1                           
        iplant              =-2                           
        imox                = 0                           
        totl                = 1.3100d0                  
        roughc              = 1.97d-5                     
        roughf              = 7.87d-5                     
        vs                  = 30.d0                       
        rsntr               = 100.d0                      
        nsp                 = 0                           
        slim                = 0.05d0                      
        enrpu39             = 0.d0                        
        enrpu40             = 0.d0                        
        enrpu41             = 0.d0                        
        enrpu42             = 0.d0                        
        tw(:)               = 600.                        
        p2(:)               = 2000.                       
        go(:)               = 2.D+6                       
        qf(:)               = 1.                          
        qmpy                = 6.                          
        gadoln(:)           = 0.d0                        
        x(:)                = 0.d0                        
        deltaz(:)           = 0.d0                        
        afdn                = 1.d0                        
        amfh2o              = 0.d0                        
        ctmax               = 386.33d0                    

        buin(:) = 0.d0
        comp(:) = 0.d0
        ctmax(:) = 0.d0
        dco(:) = 0.d0
        deltaz(:) = 0.d0
        enrch(:) = 0.d0
        flux(:) = 0.d0
        gadoln(:) = 0.d0
        go(:) = 0.d0
        jn(:) = 0.d0
        jst(:) = 0.d0
        p2(:) = 0.d0
        ProblemTime(:) = 0.d0
        qend(:) = 0.d0
        qf(:) = 0.d0
        qmpy(:) = 0.d0
        rc(:) = 0.d0
        thkcld(:) = 0.d0
        thkgap(:) = 0.d0
        tw(:) = 0.d0
        x(:) = 0.d0
        xt(:) = 0.d0
        cladt(:) = 0.d0
        jstsurftemp(:) = 0.d0
        jnsurftemp(:) = 0.d0
        p1(:) = 0.d0
        crudmult(:) = 0.d0
        tcoolant(:) = 0.d0
        zcool(:) = 0.d0
        pcoolant(:) = 0.d0

        read(ifile, frpcon, iostat=ierror)
        !read(ifile, frpmox, iostat=ierror)

        if( .not. (ierror == 0) ) then
            write(*,*) 'Namelist reading error, ierror = ', ierror
            stop
        endif


        close(ifile)

    end subroutine read_frapcon_file


    subroutine read_fraptran_file(filename)

        implicit none

        character(*) :: filename
        character(len=3) :: heat

        namelist / begin / NRestart, ncards, IndexFC2Print, IndexGrainBndSep, ProblemEndTime, &
         &                 ProblemStartTime, gbse

        namelist /boundary / coolant, reflood, radiation, heat, jchf, jfb, upppl, hupta, zad, zs, &
          &                  fltgap, fltgap2, geomet, tape1, nvol1, nchn, lowpl, pressu, massfl, &
          &                  coreav, chf, filmbo, coldwa, axpow, bowing, spefbz, geometry, nbundl, &
          &                  time, radiat, ruptur, liquid, inlet, reflo, pressure, collaps, frapt4, &
          &                  geom, temp, tape2,nvol2, press, zone, htco, nodchf, tem, dhe, dhy, achn, &
          &                  hinta, pbh1, gbh, hbh, ffch, bowthr, ExtentOfBow, tschf, techf, hydiam, &
          &                  flxsec, emptm, refdtm, hrad, temptm, fldrat, prestm, hlqcl, rshrd, ts, &
          &                  pbh2, htclev, htca, tblka, nucbo, nbhtc, jtr

        namelist / design / pitch, pdrato, rnbnt, CladType, RodLength, RodDiameter, dishd, pelh, dishv0, &
          &                 FuelPelDiam, roughf, frden, bup, rshd, frpo2, fotmtl, tsntrk, fgrns, gadoln, &
          &                 gapthk, coldw, roughc, cfluxa, tflux, cldwdc, spl, scd, swd, vplen, splbp, &
          &                 coldbp, spdbp, volbp, gfrac, gsms, gappr0, tgas0, fluxz, radpel, eppinp, &
          &                 totnb, ncs, ncolbp, OpenPorosityFraction

        namelist / ioData / unitin, unitout, trest, inp, res, pow, dtpoa, dtplta

        namelist / model / internal, metal, deformation, heat, inst, nsym, naz, gasphs, oxideid, &
          &                oxideod, zvoid1, zvoid2, rvoid, dofset, dofang, cexh2a, gasflo, grass, prescri, &
          &                idoxid, odoxid, cathca, baker, noball, cenvoi, rtheta, presfgr, relfraca, tref, &
          &                TranSwell, FuelGasSwell, PlenumTemp, nthermex, ProtectiveOxide, frcoef, mechan, &
          &                irupt, ruptstrain, irefine, refine, nIDoxide, BuOxide, explenumv, explenumt, &
          &                iStoicGrad, prestmp, gasths, ngastmp, trise, relocmodel

        namelist / solution / dtmaxa, dtss, prsacc, tmpac1, soltyp, maxit, noiter, epsht1, &
          &                   naxn, zelev, nfmesh, ncmesh, fmesh, cmesh, nce

        namelist / power / RodAvePower, AxPowProfile, RadPowProfile, butemp, azpang, pazp, ph, pl, &
          &                doffst, fpowr, powop, tpowf, timop, fpdcay, CladPower, azang, profile, &
          &                NumAxProfiles, ProfileStartTime, modheat

        defsize = 400
        ngases = 8
        coolant = 'OFF'
        mheat = 'OFF'
        bheat = 'OFF'
        reflood = 'OFF'
        internal = 'OFF'
        metal = 'OFF'
        deformation = 'OFF'
        inst = 'OFF'
        radiation = 'OFF'
        relocmodel = 'FRAPCON-3.3'
        jchf = 0
        jfb = 0
        jtr = 0
        nbhtc = 0
        upppl = 0
        zad = 0.0d0
        zs = 0.0d0
        fltgap = 0.0d0
        fltgap2 = 0.0d0
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
        dhe = 0.0d0
        dhy = 0.0d0
        achn = 0.0d0
        ffch = 0.0d0
        bowthr = 0
        hydiam = 0.0d0
        flxsec = 0.0d0
        emptm = 1.0d20
        refdtm = 1.0d20
        hrad = 0.0d0
        rshrd = 0.0d0
        pitch = 0.0d0
        pdrato = 1.32d0
        rnbnt = 1.0d0
        CladType = 4
        RodLength = 0.0d0
        RodDiameter = 0.0d0
        rshd = 0.0d0
        dishd = 0.0d0
        pelh = 0.0d0
        dishv0 = 0.0d0
        FuelPelDiam = 0.0d0
        roughf = 2.0d0
        frden = 0.0d0
        bup = 0.0d0
        frpo2 = 0.0d0
        fotmtl = 2.0d0
        tsntrk = 1883.0d0
        fgrns = 10.0d0
        gapthk = 0.0d0
        coldw = 0.0d0
        roughc = 0.5d0
        cfluxa = 0.0d0
        tflux = 0.0d0
        cldwdc = 0.0d0
        splbp = 0.0d0
        coldbp = 0.0d0
        spdbp = 0.0d0
        volbp = 0.0d0
        GasMoles0 = 0.0d0
        tgas0 = 0.0d0
        totnb = 289.0d0
        ncolbp = 1
        OpenPorosityFraction = 0.0d0
        gsms = 0.0d0
        unitin = 0
        unitout = 0
        trest = 0.0d0
        inp = 0
        res = 0
        pow = 0
        nsym = 0
        naz = 0
        cathca = 0
        iStoicGrad = 0
        baker = 0 
        ProtectiveOxide = 0
        zvoid1 = 0.0d0
        zvoid2 = 0.0d0
        rvoid = 0.0d0
        dofset = 0.0d0
        dofang = 0.0d0
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
        BuOxide = 0.0d0
        explenumv = 0.0d0
        frcoef = 0.015d0
        mechan = 2
        irupt = 1
        ruptstrain = 1.0d0
        irefine = 1
        refine = 3.0d0
        trise = 10.0d0
        tref = 77.0d0
        maxit = 200
        dtss = 1.0d5
        prsacc = 0.005d0
        tmpac1 = 0.005d0
        soltyp = 0
        noiter = 200
        epsht1 = 0.001d0
        naxn = 0
        nfmesh = 0
        ncmesh = 0
        nce = 5
        ph = 0.0d0
        pl = 0.0d0
        doffst = 0.0d0
        fpowr = 1.0d0
        powop = 0.0d0
        tpowf = 0.0d0
        timop = 0.0d0
        fpdcay = 1.0d0
        CladPower = 0.0d0
        NumAxProfiles = 1
        azang = 0
        profile = 0

        open(ifile, file=filename, status='unknown', form='formatted')

        read(ifile, begin, iostat=ierror)
        call read_error(ierror, 'begin')

        ntimesteps = defsize

        allocate(gfrac(1:ngases))
        allocate(dtmaxa(1:ntimesteps))
        allocate(hbh(1:ntimesteps))
        allocate(hupta(1:ntimesteps))
        allocate(hinta(1:ntimesteps))
        allocate(gbh(1:ntimesteps))
        allocate(explenumt(1:2*ntimesteps))
        allocate(pbh2(1:ntimesteps))
        allocate(rodavepower(1:ntimesteps))
        allocate(dtpoa(1:ntimesteps+2))
        allocate(ngastmp(1:2))
        allocate(ncs(1))

        gfrac(1) = 1.0d0
        gfrac(2:ngases) = 0.0d0
        explenumt(1) = 77.0d0
        explenumt(2) = 0.0d0

        read(ifile, solution, iostat=ierror)
        call read_error(ierror, 'solution')

        naxialnodes = naxn + 25
        nradialnodes = nfmesh + ncmesh + 1

        allocate(zelev                (1:naxialnodes))
        allocate(butemp               (1:(naxialnodes*nradialnodes)))
        allocate(gadoln               (1:naxialnodes))
        allocate(htclev               (1:naxialnodes))
        allocate(htco                 (1:naxialnodes))
        allocate(gbse                 (1:5) )
        allocate(scd                  (1) )
        allocate(radpel               (1:2*naxialnodes) )
        allocate(azpang               (1:naxialnodes) )
        allocate(ExtentOfBow          (1:naxialnodes) )
        allocate(fluxz                (1:2*naxialnodes) )
        allocate(nodchf               (1:naxialnodes+1) )
        allocate(swd                  (1) )
        allocate(oxideod              (1:naxialnodes) )
        allocate(cexh2a               (1:naxialnodes) )
        allocate(pazp                 (1:2*nradialnodes,1:naxialnodes) )
        allocate(oxideid              (1:naxialnodes) )
        allocate(spl                  (1) )
        allocate(eppinp               (1:2*naxialnodes) )
        allocate(techf                (1:naxialnodes) )
        allocate(tschf                (1:naxialnodes) )
        allocate(gappr0               (1) )
        allocate(vplen                (1) )
        allocate(fmesh                (1:nradialnodes))
        allocate(cmesh                (1:nradialnodes))
        allocate(tem                  (1:naxialnodes))
        
        allocate(htca                 (1:ntimesteps, 1:naxialnodes))
        allocate(tblka                (1:ntimesteps, 1:naxialnodes))
        allocate(axpowprofile         (1:2*naxialnodes,1:ntimesteps))
        allocate(RadPowProfile        (1:2*naxialnodes*ntimesteps) )
        allocate(gasths               (1:2*ntimesteps,1:2) )
        allocate(fldrat               (1:ntimesteps) )
        allocate(gasphs               (1:2*ntimesteps) )
        allocate(dtplta               (1:ntimesteps+2) )
        allocate(ProfileStartTime     (1:ntimesteps) )
        allocate(FuelGasSwell         (1:2*ntimesteps) )
        allocate(temptm               (1:ntimesteps) )
        allocate(relfraca             (1:2*ntimesteps) )
        allocate(prestm               (1:ntimesteps) )
        allocate(pbh1                 (1:ntimesteps) )
        allocate(hlqcl                (1:ntimesteps) )

        gadoln(:) = -1.0d0
        htco(:) = 0
        tem(:) = 0

        rewind(ifile)
        read(ifile, design, iostat=ierror)
        call read_error(ierror, 'design')

        rewind(ifile)
        read(ifile, boundary, iostat=ierror)
        call read_error(ierror, 'boundary')

        bheat = heat

        rewind(ifile)
        read(ifile, iodata, iostat=ierror)
        call read_error(ierror, 'iodata')

        rewind(ifile)
        read(ifile, model, iostat=ierror)
        call read_error(ierror, 'model')

        mheat = heat

        rewind(ifile)
        read(ifile, power, iostat=ierror)
        call read_error(ierror, 'power')

        close(ifile)

    end subroutine read_fraptran_file

    subroutine read_error(ierror, listname)
        implicit none
        character(*) :: listname
        character(len=100) :: line
        integer :: ierror
        if( .not. (ierror == 0) ) then
            backspace(ifile)
            read(ifile,'(A)') line
            write(*,*) 'ERROR: Namelist ', listname, ' reading error #', ierror, ' in the line ', trim(line)
            stop
        endif
    end subroutine read_error

end module fpn_reader
