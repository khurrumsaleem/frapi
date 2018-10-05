    character(len=3)  :: coolant, mheat, bheat, reflood, internal, metal, deformation, radiation
    character(len=10) :: inst
    character(len=12) :: relocmodel

    integer :: tape1, axpow, nvol2, azang, geom, spefbz, inp, &
             & inlet, tape2, res, iiopt, nchn, nidoxide, nsym, press, &
             & geometry, chf, noiter, temp, idebug, coreav, indexgrainbndsep, &
             & prescri, noball, prestmp, time, jchf, icnt, idoxid, pressu, zone, transwell, &
             & nvol1, frapt4, indexfc2print, rtheta, cladtype, lowpl, pressure, gasflo, reflo, &
             & odoxid, collaps, nbundl, coldwa, ruptur, jfb, cathca, soltyp, ncards, &
             & jtr, irefine, profile, ndat, nucbo, ncmesh, unitout, numaxprofiles, radiat, &
             & nchmfd, ncolbp, nrestart, irupt, nbrliq, cenvoi, nthermex, unitin, upppl, &
             & liquid, nfmesh, bake, baker, geomet, PlenumTemp, &
             & naz, pow, input_unit, bowing, massfl, grass, ibu, maxit, protectiveoxide, &
             & istoicgrad, iunit, filmbo, ngases, defsize, naxn, presfgr, nbhtc

    real(8) :: problemendtime, fdens, zvoid2, rvoid, &
             & coldbp, achn, bup, pdrato, sumg, &
             & ruptstrain, tgas0, gapthk, frpo2, openporosityfraction, dofset, &
             & ts, emptm, trise, coldw, dtss, dhy, pelh, &
             & rnbnt, fuelpeldiam, gsms, pl, dofang, &
             & hrad, problemstarttime, flxsec, ffch, &
             & fltgap2, rodfabtemp, roddiameter, refdtm, gasmoles0, &
             & modheat, bowthr, epsht1, buoxide, dhe, tpowf, tref, dishv0, &
             & fltgap, fastfluence, explenumv, volbp, &
             & zs, refine, rshrd, rshd, totnb, splbp, tsntrk, fpowr, cfluxa, hydiam, &
             & zad, rodlength, ph, tflux, spdbp, fgrns, pfflec, &
             & cladpower, prsacc, dishd, powop, zvoid1, doffst, tmpac1, &
             & frden, cldwdc, timop, &
             & trest

    real(8), dimension(:), allocatable :: gfrac, butemp, cmesh, explenumt, dtmaxa, zelev, fmesh, &
                                          hbh, hupta, hinta, gbh, pbh2, htclev, dtpoa, rodavepower, &
                                          dtplta, scd, azpang, radpowprofile, ExtentOfBow, &
                                          gbse, ProfileStartTime, radpel,  &
                                          FuelGasSwell, &
                                          temptm, pbh1, fluxz, hlqcl, nodchf, &
                                          swd, oxideod, cexh2a, fldrat, &
                                          oxideid, gasphs, spl, eppinp, techf, &
                                          relfraca, tschf, gappr0, prestm, vplen

    real(8), dimension(:,:), allocatable :: htca, tblka, axpowprofile, gasths, pazp

    integer, dimension(:), allocatable :: tem, htco, ngastmp, ncs