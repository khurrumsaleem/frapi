#!/usr/bin/python
import re
import sys, os
import argparse
import fnmatch

srcdir = "../fraptran"

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

comments = """
geomet      ! suboption to specify geometry of coolant channel cooling fuel rod (default is 0)
nvol1       ! Number of coolant zones stacked on top of each other and surrounding fuel rod
lowpl       ! Suboption to specify the enthalpy history of coolant at bottom of fuel rod (inlet enthalpy)
pressu      ! Suboption to specify the coolant pressure history
massfl      ! Suboption to specify the coolant mass flux history
coreav      ! Suboption to specify the core average coolant enthalpy history
chf         ! Suboption to select the CHF correlation to be used
filmbo      ! Suboption to select the post-CHF heat transfer correlations to be used in transition and film boiling
coldwa      ! Suboption to modify the critical heat flux for cold wall effect
axpow       ! Suboption to modify the critical heat flux for effect of axially varying power
bowing      ! Suboption to modify the critical heat flux as calculated according to the chf correlation suboption for fuel rod bowing effect.
spefbz      ! Suboption to prescribe film boiling over part of fuel rod.
geometry    ! Suboption to specify geometry parameters
nbundl      ! About a FLECHT correlation
refloodtime ! Suboption to specify start time of reactor core reflooding.
radiat      ! Suboption to specify the radiation heat transfer at the cladding surface during reflood.
ruptur      ! Suboption to specify the rupture plane as the line of demarcation between the FLECHT and steam cooling models
liquid      ! Suboption to specify the collapsed liquid level as the line of demarcation instead of the rupture plane
inlet       ! Suboption to specify the fraction of flooding water carried out of the core
reflo       ! Suboption to specify reflood rate as a function of time
pressure    ! Suboption to specify reactor vessel pressure as a function of time.
collaps     ! Suboption to specify the fraction of flooding water carried out of the core.
frapt4      ! Suboption to specify the FRAP-T4 FLECHT correlation instead of the generalized FLECHT correlation
geom        ! Suboption to specify the inner radius of the flow shroud. 
temp        ! Suboption to specify temperature history of flow shroud
tape2       ! Suboption to specify that the heat transfer coefficients, coolant temperature, and pressure are input on tape
nvol2       ! Number of heat transfer coefficient zones stacked on top of each other.
press       ! Suboption to specify coolant pressure.
zone        ! Suboption to specify the elevation of heat transfer coefficient zone 1
upppl       ! Suboption to specify the enthalpy history of coolant at the top of the fuel rod(exit enthalpy)
jfb         ! Is the indicator of the film boiling correlation to be used.
nucbo       ! Suboption to select the nucleate boiling heat transfer correlation to be used
unitin      ! Option to specify that the input data are in SI units
unitout     ! Option to specify that the output is to be in SI units even though the input is in British units
res         ! Option to specify that a restart file is to be created
pow         ! Option to specify the printout of the fuel rod state at each step of the first power ramp
gasflo      ! Suboption to model transient flow of gas between fuel rod plenum and cladding ballooning region
idoxid      ! Suboption to specify the initial oxide thickness on the inner surface of the cladding
cathca      ! Suboption to specify the modeling of the metal-water reaction with the COBILD subroutine and the Cathcart correlation of MATPRO
baker       ! Suboption to specify the modeling of the metal-water reaction with the Baker-Just model.
noball      ! Suboption (modfd=0,nbalsw=1) to specify that the BALON subcode is to be bypassed and cladding failure occurs when the effective cladding plastic strain exceeds the instability strain.
cenvoi      ! Suboption to specify that a portion of the fuel pellets have a central void, such as that required to contain a thermocouple to measure the temperature of the center of the fuel.
soltyp      ! Option to specify an explicit solution
"""

clean = lambda x: x[:-1] if x[-1] == ',' else x
vars = map(clean, data.split())

split = lambda x: [z.strip() for z in x.split('!')]
comments = dict(map(split,comments.split('\n')[1:-1]))

tvars = {}

#patternt = r'(integer|real|character)\s*,?(dimension)?\s*?\(:,?\)(intent?(::)?%s\W'
#pattern = r'(integer|real|character)[\s,]*?(dimension)?\(?([:,]*)?\)?[\s,]*?(allocatable)?[\s,]*?(intent)?\(?(in|out)?\)?[:\s]*[^!]*%s'
#pattern = r'([\w\W]*)::[^!]%s'

for var in vars:
    for root, dir, files in os.walk(srcdir):
        for item in fnmatch.filter(files, '*.f90'):
            with open(os.path.join(root,item), 'r') as f:
                for line in f:
                    a = line.split('::')
                    if len(a) > 1:
                        if var in a[1]:
                            vtype = a[0].lower()
                            tvars[var] = vtype

for r in ['character','integer','real']:
    for var in tvars:
        comment = ""
        if var in comments: comment = comments[var]
        vtype = tvars[var]
        if r in vtype and 'dimension' in vtype:
            print "{:<60}, pointer :: {:<20} ! {:}".format(vtype, var, comment)
