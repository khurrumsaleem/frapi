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

        namelist / frpcn  / im, mechan, na, ngasr, nr, nce, graphna, graphnr, naxim

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

        open(ifile, file=filename, status='unknown')

        read(ifile, frpcn,  iostat=ierror)

        allocate(buin(1:na))
        allocate(comp(1:na))
        allocate(ctmax(1:na))
        allocate(dco(1:na))
        allocate(deltaz(1:na))
        allocate(enrch(1:na))
        allocate(flux(1:na))
        allocate(gadoln(1:na))
        allocate(go(1:im+1))
        allocate(jn(1:im+1))
        allocate(jst(1:im+1))
        allocate(p2(1:im+1))
        allocate(ProblemTime(0:im+1))
        allocate(qend(1:im+1))
        allocate(qf(1:naxim))
        allocate(qmpy(1:im+1))
        allocate(rc(1:na))
        allocate(thkcld(1:na))
        allocate(thkgap(1:na))
        allocate(tw(1:im+1))
        allocate(x(1:naxim))
        allocate(xt(1:naxim))
        allocate(cladt(1:naxim))
        allocate(jstsurftemp(1:im+1))
        allocate(jnsurftemp(1:im+1))
        allocate(p1(1:im+1))
        allocate(crudmult(1:na))
        allocate(tcoolant(1:naxim))
        allocate(zcool(1:na))
        allocate(pcoolant(1:naxim))

        read(ifile, frpcon, iostat=ierror)
        read(ifile, frpmox, iostat=ierror)

        close(ifile)

    end subroutine read_frapcon_file

end module fpn_reader