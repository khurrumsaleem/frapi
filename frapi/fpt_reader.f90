module fpt_reader

    use conversions_frapcon
    use frapi_input_vars

    implicit none

    integer, parameter :: ifile = 1012
    integer :: ierror

    contains

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
        ncards = 1
        IndexFC2Print = 0
        IndexGrainBndSep = 0
        NRestart = 0
        nchn = 1
        modheat = 2.D-2

        open(ifile, file=filename, status='unknown', form='formatted')

        read(ifile, begin, iostat=ierror)
        call read_error(ierror, 'begin')

        ntimesteps = defsize

        allocate(gfrac(1:ngases))
        allocate(dtmaxa(1:2*ntimesteps))
        allocate(hbh(1:2*ntimesteps))
        allocate(hupta(1:2*ntimesteps))
        allocate(hinta(1:2*ntimesteps))
        allocate(gbh(1:2*ntimesteps))
        allocate(explenumt(1:2*ntimesteps+2))
        allocate(pbh2(1:2*ntimesteps))
        allocate(ts(1:2*ntimesteps))
        allocate(rodavepower(1:2*ntimesteps))
        allocate(dtpoa(1:2*ntimesteps+2))
        allocate(ngastmp(1:2))
        allocate(ncs(1))

        gfrac(1) = 1.D0
        gfrac(2:ngases) = 0.D0
        dtmaxa(:) = 0.D0
        hbh(:) = 0.D0
        hupta(:) = 0.D0
        hinta(:) = 0.D0
        gbh(:) = 0.D0
        explenumt(:) = -1.D0
        explenumt(1) = 298.15D0
        explenumt(2) = 0.D0
        pbh2(:) = 0.D0
        rodavepower(:) = 0.D0
        dtpoa(:) = 0.D0
        ngastmp(:) = 0
        ncs(1) = 1
        ts(:) = 0.D0

        read(ifile, solution, iostat=ierror)
        call read_error(ierror, 'solution')

        naxialnodes = naxn + 25
        nradialnodes = nfmesh + ncmesh + 1

        !write(*,*) ntimesteps, naxialnodes, nradialnodes

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
        allocate(htca                 (1:2*ntimesteps, 1:naxialnodes))
        allocate(tblka                (1:2*ntimesteps, 1:naxialnodes))
        allocate(axpowprofile         (1:2*naxialnodes,1:ntimesteps))
        allocate(RadPowProfile        (1:2*naxialnodes*ntimesteps) )
        allocate(gasths               (1:2*ntimesteps,1:2) )
        allocate(fldrat               (1:2*ntimesteps) )
        allocate(gasphs               (1:2*ntimesteps) )
        allocate(dtplta               (1:ntimesteps+2) )
        allocate(ProfileStartTime     (1:ntimesteps) )
        allocate(FuelGasSwell         (1:2*ntimesteps) )
        allocate(temptm               (1:2*ntimesteps) )
        allocate(relfraca             (1:2*ntimesteps) )
        allocate(prestm               (1:2*ntimesteps) )
        allocate(pbh1                 (1:2*ntimesteps) )
        allocate(hlqcl                (1:2*ntimesteps) )

        gadoln               = -1.D0
        zelev                = 0
        butemp               = 0
        htclev               = 0
        htco                 = 0
        scd                  = 0
        radpel               = 0
        azpang               = 0
        ExtentOfBow          = 0
        fluxz                = 0
        nodchf               = 0
        swd                  = 0
        oxideod              = 3.D-6
        cexh2a               = 0
        pazp                 = 0
        oxideid              = 3.D-6
        spl                  = 0
        eppinp               = 0
        techf                = 0
        tschf                = 0
        gappr0               = 0
        vplen                = 0
        fmesh                = 0
        cmesh                = 0
        tem                  = 0
        htca                 = 0
        tblka                = 0
        axpowprofile         = 0
        RadPowProfile        = 0
        gasths               = 0
        fldrat               = 0
        gasphs               = 0
        dtplta               = 0
        ProfileStartTime     = 0
        FuelGasSwell         = 0
        temptm               = 0
        relfraca             = 0
        prestm               = 0
        pbh1                 = 0
        hlqcl                = 0
        gbse(1)              = 0.0d0
        gbse(2)              = 5000.0d0
        gbse(3)              = 1.0d0
        gbse(4)              = 1.0d0
        gbse(5)              = 0.0d0


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

end module fpt_reader
