a = set("""ffch, emptm, fltgap2, RodDiameter, RodLength, gapthk, gsms, rodfabtemp, fdens, sumg,
             ph, pl, doffst, fpowr, CladPower, buoxide, fastfluence, dtss, prsacc, tmpac1, epsht1,
             ProblemEndTime, ProblemStartTime, gbse, achn, splbp, tpowf, ruptstrain, naxn, scd, dtplta,
             gasmoles0, bowthr, hrad, dofang, coldbp, inlet, ts, frden, cexh2a, azpang, zad, refdtm, press, htclev,
             totnb, timop, powop, gapthk, gbh, ffch, gbse, tem, fuelgasswell, prsacc, fpowr, tref, temptm, tblka,
             rodlength, res, pelh, pdrato, tgas0, tsntrk, modheat, spdbp, pbh2, pbh1, pressure, achn, flxsec,
             rodavepower, tflux, fluxz, axpowprofile, hlqcl, geometry, nodchf, roddiameter, epsht1, pow,
             profilestarttime, cladpower, rshrd, doffst, chf, gappr0, emptm, swd, trise, htca, buoxide,
             fltgap2, hydiam, dishd, cldwdc, ph, oxideod, pl, dtss, bup, hupta, hinta, pazp, fldrat, radpel,
             hbh, temp, cfluxa, rvoid, dofset, problemendtime, extentofbow, fltgap, oxideid, gasphs, frpo2,
             trest, inp, spl, refine, eppinp, fuelpeldiam, tmpac1, coldw, openporosityfraction, dhe, explenumv,
             dtpoa, techf, dhy, zs, volbp, radpowprofile, relfraca, profile, tschf, gsms, dishv0, problemstarttime,
             gasths, rnbnt, zvoid2, prestm, zvoid1, vplen, fgrns, rshd, time, pfflec
""".split())

n = len(a)
print ("%s "*n)%tuple(a)
