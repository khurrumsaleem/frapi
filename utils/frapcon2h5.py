#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
import argparse
from h5py import *

MWdMTUtoMWskgU = lambda x: x * 86.4 * 1.E-3
fttoin = lambda x: x * 12.0
intocm = lambda x: x * 2.54
fttocm = lambda x: fttoin(intocm(x))
miltoft = lambda x: x * 0.001
miltomm = lambda x: intocm(x) * 0.01
miltoum = lambda x: miltomm(x) * 1.E+3
fttoum = lambda x: intocm(fttoin(x)) * 1.E+5
BTUlbtoJkg = lambda x: x * 2326.000292
Bhft2FtoWm2K = lambda x: x * 5.678263
lbtog = lambda x: x * 453.5923699997481
lbft3tokgm3 = lambda x: x * 16.0184634
WtoBTUh = lambda x: x * 3.41214163513
PSItoMPa = lambda x: x * 6.89475728E-3
ktf = lambda x: x * 1.8 - 459.67
ftk = lambda x: (x + 459.67) / 1.8
ftoc = lambda x: (x - 32.0) / 1.8
kwfttowcm = lambda x: x * 1.E+3 / 12.0 / 2.54
pct = lambda x: x * 100
none = lambda x: x

keylist={
 1  : ('time, day', none),
 2  : ('average linear power, W|cm' , kwfttowcm),
# 20 : 'peak linear power, W|cm', 
 10 : ('average fuel burnup, MW*d|kg', none),
# 35 : 'peak power burnup, MWd/kgU',
# 34 : 'na na Peak Power Axial Element Indicator',
# 36 : 'F K Peak Power Pellet CenterLine Temperature',
# 37 : 'F K Peak Power Pellet Surface Temperature',
# 38 : 'F K Peak Power Clad Inside Temperature',
# 39 : 'F K Peak Power Clad Outside Temperature',
 8  : ('average fuel temperature, C', ftoc),
# 9  : 'Btu/lbm j/kg Fuelled Region Stored Energy',
 5  : ('plenum gas temperature, C', ftoc),
 6  : ('plenum gas pressure, MPa', PSItoMPa),
# 7  : 'in^3 cm^3 Total Void Volume',
# 3  : 'in mm Fuel Stack Axial Extension',
# 4  : 'in mm Cladding Axial Extension (Including Irrad Growth)',
# 33 : '% % Relative Cladding Axial Extension',
# 40 : '% % Relative Fuel Axial Extension',
 11 : ('fission gas release, %', none),
# 21 : '% % Molar Fraction of He',
# 22 : '% % Molar Fraction of Fission Products (Xe+Kr)',
# 23 : '% % Molar Fraction of Air',
# 24 : '% % Molar Fraction of Ar',
# 25 : 'na na Free Gas in the Rod cm3STP',
# 26 : 'na na Moles of Fission Gas Released',
# 28 : 'na na Moles of Gas in Rod',
 130: ('axial linear power, W|cm', kwfttowcm),
# 156: 'na na Normalized axial node power',
# 116: 'Btu/(hr-ft^2) W/(m^2) Surface Heat Flux',
 142: ('fuel burnup, MW*d|kg', none),
# 157: 'n/m^2 n/m^2 Axial Fast Fluence',
 150: ('axial mesh, cm', fttocm),
 122: ('centerline temperature, C', ftoc),
 123: ('fuel pellet surface temperature, C', ftoc),
 145: ('fuel volume average temperature, C', ftoc),
 158: ('gap average temperature, C', ftoc),
 124: ('cladding inside temperature, C', ftoc),
 125: ('cladding average temperature, C', ftoc),
 126: ('cladding outside temperature, C', ftoc),
 139: ('oxide surface temperature, C', ftoc),
 127: ('bulk coolant temperature, C', ftoc),
# 114: 'na na Fuel Duty Index',
 121: ('fuel stored energy, J|kg', BTUlbtoJkg),
 107: ('cladding axial stress, MPa', PSItoMPa),
 108: ('cladding hoop stress, MPa', PSItoMPa),
 109: ('effective cladding stress, MPa', PSItoMPa),
 101: ('cladding total axial strain, %', pct),
 102: ('cladding total hoop strain, %', pct),
 103: ('cladding total radial strain, %', pct),
 136: ('cladding elastic hoop strain, %', pct),
 137: ('cladding elastic axial strain, %', pct),
 138: ('cladding elastic radial strain, %', pct),
 104: ('cladding permanent axial strain, %', pct),
 105: ('cladding permanent hoop strain, %', pct),
 106: ('cladding permanent radial strain, %', pct),
 144: ('cladding creep rate', none),
# 152: '% % Axial Strain due to Irradiation growth',
 169: ('cladding inner radius total deformation, um', miltoum),
 170: ('cladding outer radius total deformation, um', miltoum),
 128: ('fuel surface displacement, um',  miltoum),
 161: ('fuel thermal Expansion, um',  miltoum),
 162: ('fuel swelling, um',  miltoum),
 164: ('fuel densification, um',  miltoum),
 165: ('fuel relocation, um',  miltoum),
 143: ('fuel swelling rate', none),
 133: ('fuel surface axial strain', none),
# 141: '% % Nodal FGR',
 131: ('gap interface pressure, MPa', PSItoMPa),
 129: ('gap pressure, MPa',PSItoMPa),
 155: ('mechanical gap, um', miltoum),
 112: ('thermal gap, um', miltoum),
 146: ('total gap conductance, W|(m^2*K)', Bhft2FtoWm2K),
 115: ('oxide thickness, um', miltoum),
 140: ('cladding hydrogen concentration, ppm', none),
# 117: 'lbm/ft^3 kg/m^3 Coolant Density',
# 118: 'lb/(hr-ft^2) kg/(s-m^2) Coolant Mass Flux',
 119: ('coolant pressure, MPa', PSItoMPa),
# 201: 'F K Fuel Rod Mesh Temperatures',
 }


def txt2array(d):
	x = {}
	for line in d.split('\n')[:-1]:
		line = line.split()
		key = int(line[0])
		value = map(float, line[1:])
		if key in x: 
			x[key].extend(value)
		else:
			x[key] = value
	return x

if __name__ == '__main__':
	parser = argparse.ArgumentParser(prog="Make *.h5 file from frapcon output")
	parser.add_argument("ifile", nargs='?', help="output frapcon file")
	parser.add_argument("ofile", nargs='?', help="output HDF5 file")
	args = parser.parse_args()

	if os.path.isfile(args.ofile): os.remove(args.ofile)

	with open(args.ifile) as f:
		data = f.read()
		data = data.split("===next time step")[1:]
		data = map(txt2array, data)

	with File(args.ofile) as f:
		for itime, d in enumerate(data):
			group = f.create_group("%06i"%itime)
			group.attrs['time, day'] = d[1]
			for i in keylist:
				key, conv = keylist[i]
				group[key] = map(conv, d[i])
