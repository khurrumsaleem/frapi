import re
import os, sys

variables = """
mechan, graphna, graphnr, naxim,
afdn, amfair, amfarg, amffg, amfhe, amfh2, amfh2o, amfkry, amfn2, amfxe, 
buin, catexf, chorg, cldwks, comp, cpl, crdt, crdtr, crephr, ctmax, 
dco, deltaz, den, deng, dishsd, dspg, dspgw, enrch, fa, fgpav, flux, fotmtl, 
gadoln, go, hdish, hplt, icm, icor, idxgas, iplant, iq, ivardm, imox, 
jdlpr, jn, jst, igas, nopt, nplot, nread, nrestr, nsp, ntape, nunits, p2, 
pitch, ppmh2o, ppmn2, ProblemTime, roughc, qend, qf, qmpy, rc, roughf, rsntr, 
sgapf, slim, thkcld, thkgap, totl, tsint, tw, vs, x, xt, cladt, ifixedtsurf, 
jstsurftemp, jnsurftemp, grnsize, frcoef, p1, igascal, ifba, b10, zrb2thick, 
zrb2den, ngasmod, zr2vintage, sigftc, sigftex, sigfgr, sigswell, sigcreep, 
siggro, sigcor, sigh2, crudmult, chmfrh, chmfrw, irefab, nrefab1, nrefab2, 
cplrefab, vsrefab, dspgrefab, dspgwrefab, fgpavrefab, airrefab, n2refab, 
arrefab, fgrefab, herefab, krrefab, xerefab, 
ifixedcoolt, tcoolant, zcool, ifixedcoolp, pcoolant, fpdcay, 
enrpu39, enrpu40, enrpu41, enrpu42, moxtype
"""

variables = re.sub('[\n,]', ' ', variables).split()

with open('../../frapcon/Include/fp_pointers_h.f90') as f:
	lines = f.read().split('\n')
	for var in variables:
		for line in lines:
			if re.search(' %s '%var, line): 
				if re.search('DIMENSION', line): 
					#if re.search('ecay', line): 
						print line
						break
		