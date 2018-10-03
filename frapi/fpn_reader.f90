module fpn_reader

    use conversions_frapcon

    implicit none

    integer, parameter :: ifile = 1012
    integer :: ierror

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

        !include "ft_default_h.f90"

        defsize = 400
        ngases = 8

        open(ifile, file=filename, status='unknown', form='formatted')

        read(ifile, begin, iostat=ierror)
        call read_error(ierror, 'begin')

        allocate(gfrac(1:ngases))
        allocate(dtmaxa(1:defsize))
        allocate(zelev(1:defsize))
        allocate(fmesh(1:defsize))
        allocate(cmesh(1:defsize))
        allocate(hbh(1:defsize))
        allocate(hupta(1:defsize))
        allocate(hinta(1:defsize))
        allocate(gbh(1:defsize))
        allocate(tem(1:defsize))
        allocate(explenumt(1:2*defsize))
        allocate(pbh2(1:defsize))
        allocate(rodavepower(1:defsize))
        allocate(dtpoa(1:defsize+2))

        gfrac(1) = 1.0d0
        gfrac(2:ngases) = 0.0d0
        tem(:) = 0
        explenumt(1) = 77.0d0
        explenumt(2) = 0.0d0

        read(ifile, solution, iostat=ierror)
        call read_error(ierror, 'solution')

        allocate(butemp(1:(naxn*(nfmesh+1))))
        allocate(htca(1:defsize, 1:naxn))
        allocate(tblka(1:defsize, 1:naxn))
        allocate(gadoln(1:naxn))
        allocate(htclev(1:naxn))
        allocate(htco(1:naxn))
        allocate(axpowprofile(1:2*naxn,1:defsize))

        gadoln(:) = -1.0d0
        htco(:) = 0

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
