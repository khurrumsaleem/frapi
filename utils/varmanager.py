data = """
NRestart, ncards, IndexFC2Print, IndexGrainBndSep, ProblemEndTime, 
ProblemStartTime, gbse,
coolant, reflood, radiation, heat, jchf, jfb, upppl, hupta, zad, zs, 
fltgap, fltgap2, geomet, tape1, nvol1, nchn, lowpl, pressu, massfl, 
  coreav, chf, filmbo, coldwa, axpow, bowing, spefbz, geometry, nbundl, 
  time, radiat, ruptur, liquid, inlet, reflo, pressure, collaps, frapt4, 
  geom, temp, tape2,nvol2, press, zone, htco, nodchf, tem, dhe, dhy, achn, 
  hinta, pbh1, gbh, hbh, ffch, bowthr, ExtentOfBow, tschf, techf, hydiam, 
  flxsec, emptm, refdtm, hrad, temptm, fldrat, prestm, hlqcl, rshrd, ts, 
  pbh2, htclev, htca, tblka, nucbo, nbhtc, jtr,
 pitch, pdrato, rnbnt, CladType, RodLength, RodDiameter, dishd, pelh, dishv0, 
 FuelPelDiam, roughf, frden, bup, rshd, frpo2, fotmtl, tsntrk, fgrns, gadoln, 
 gapthk, coldw, roughc, cfluxa, tflux, cldwdc, spl, scd, swd, vplen, splbp, 
 coldbp, spdbp, volbp, gfrac, gsms, gappr0, tgas0, fluxz, radpel, eppinp, 
 totnb, ncs, ncolbp, OpenPorosityFraction,
 unitin, unitout, trest, inp, res, pow, dtpoa, dtplta,
internal, metal, deformation, heat, inst, nsym, naz, gasphs, oxideid, 
oxideod, zvoid1, zvoid2, rvoid, dofset, dofang, cexh2a, gasflo, grass, prescri, 
idoxid, odoxid, cathca, baker, noball, cenvoi, rtheta, presfgr, relfraca, tref, 
TranSwell, FuelGasSwell, PlenumTemp, nthermex, ProtectiveOxide, frcoef, mechan, 
irupt, ruptstrain, irefine, refine, nIDoxide, BuOxide, explenumv, explenumt, 
iStoicGrad, prestmp, gasths, ngastmp, trise, relocmodel,
 dtmaxa, dtss, prsacc, tmpac1, soltyp, maxit, noiter, epsht1, 
   naxn, zelev, nfmesh, ncmesh, fmesh, cmesh, nce,
RodAvePower, AxPowProfile, RadPowProfile, butemp, azpang, pazp, ph, pl, 
doffst, fpowr, powop, tpowf, timop, fpdcay, CladPower, azang, profile, 
NumAxProfiles, ProfileStartTime, modheat
"""

data = [a.replace(',', '') for a in data.split()]
for a in data:
      n = data.count(a)
      if n > 1: print a, n

