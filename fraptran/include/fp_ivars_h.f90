    character(len=3)  :: coolant, mheat, bheat, reflood, internal, metal, deformation, radiation
    character(len=10) :: inst
    character(len=12) :: relocmodel

    integer :: tape1, axpow, nvol2, azang, geom, spefbz, inp, &
             & inlet, tape2, res, iiopt, nchn, nidoxide, nsym, press, &
             & geometry, chf, noiter, temp, idebug, ngastmp, coreav, indexgrainbndsep, &
             & prescri, noball, prestmp, time, jchf, icnt, idoxid, pressu, zone, transwell, &
             & nvol1, frapt4, indexfc2print, rtheta, cladtype, lowpl, pressure, gasflo, reflo, &
             & odoxid, collaps, nbundl, coldwa, ruptur, jfb, cathca, soltyp, ncards, &
             & jtr, irefine, profile, ndat, nucbo, ncmesh, unitout, numaxprofiles, radiat, &
             & nchmfd, ncolbp, nrestart, irupt, nbrliq, cenvoi, nthermex, unitin, upppl, &
             & liquid, nfmesh, bake, baker, geomet, PlenumTemp, &
             & naz, pow, input_unit, bowing, massfl, grass, ibu, maxit, protectiveoxide, &
             & istoicgrad, ncs, iunit, filmbo, ngases, defsize, naxn, presfgr, nbhtc

    real(8) :: problemendtime, fdens, zvoid2, spl, rvoid, &
             & coldbp, gappr0, achn, bup, pdrato, sumg, &
             & ruptstrain, tgas0, gapthk, prestm, scd, frpo2, openporosityfraction, dofset, &
             & ts, emptm, trise, coldw, dtss, dtplta, dhy, pelh, &
             & rnbnt, pazp, hlqcl, fuelpeldiam, gsms, pl, dofang, &
             & hrad, oxideid, problemstarttime, flxsec, ffch, &
             & fltgap2, temptm, rodfabtemp, roddiameter, radpel, refdtm, gasmoles0, &
             & modheat, bowthr, epsht1, buoxide, pbh1, dhe, swd, tpowf, tref, dishv0, &
             & relfraca, fltgap, techf, radpowprofile, fuelgasswell, fastfluence, explenumv, volbp, &
             & zs, refine, rshrd, rshd, totnb, splbp, tsntrk, fpowr, tschf, cfluxa, gasths, hydiam, &
             & cexh2a, zad, gbse, rodlength, ph, tflux, spdbp, fgrns, vplen, pfflec, &
             & cladpower, prsacc, dishd, extentofbow, powop, fluxz, zvoid1, doffst, azpang, tmpac1, &
             & nodchf, profilestarttime, frden, fldrat, cldwdc, timop, eppinp, &
             & trest, gasphs, oxideod

    real(8), dimension(:), allocatable :: gfrac, butemp, cmesh, explenumt, dtmaxa, zelev, fmesh, &
                                        & hbh, hupta, hinta, gbh, pbh2, htclev, dtpoa, rodavepower

    real(8), dimension(:,:), allocatable :: htca, tblka, axpowprofile

    integer, dimension(:), allocatable :: tem, htco