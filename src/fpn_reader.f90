module fpn_reader

    use conversions

    implicit none

    integer :: ifile = 1012
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

end module fpn_reader