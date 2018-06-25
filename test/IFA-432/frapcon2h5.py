#!/bin/env/python
# -*- coding:utf-8 -*-
import argparse
from h5py import *

MWskgUtoMWdMTU = 1000.0
fttoin = 12.0
intocm = 2.54
miltoft = 0.001
miltomm = 0.01
BTUlbtoJkg = 2326.000292
Bhft2FtoWm2K = 5.678263
lbtog = 453.5923699997481
lbft3tokgm3 = 16.0184634
WtoBTUh = 3.41214163513
PSItoMPa = 1.0E-3
ktf = lambda x: x * 1.8 - 459.67
ftk = lambda x: (x + 459.67) / 1.8

keylist={
 1  : 'time, days',
 2  : 'linear power, W|cm',
 20 : 'peak linear power, W|cm',
 10 : 'average burnup,  MWd|kgU',
# 35 : 'peak power burnup, MWd/kgU',
# 34 : 'na na Peak Power Axial Element Indicator',
# 36 : 'F K Peak Power Pellet CenterLine Temperature',
# 37 : 'F K Peak Power Pellet Surface Temperature',
# 38 : 'F K Peak Power Clad Inside Temperature',
# 39 : 'F K Peak Power Clad Outside Temperature',
# 8  : 'F K Average Fuel Temperature',
# 9  : 'Btu/lbm j/kg Fuelled Region Stored Energy',
 5  : 'plenum gas temperature, C',
 6  : 'plenum gas pressure, MPa',
# 7  : 'in^3 cm^3 Total Void Volume',
# 3  : 'in mm Fuel Stack Axial Extension',
# 4  : 'in mm Cladding Axial Extension (Including Irrad Growth)',
# 33 : '% % Relative Cladding Axial Extension',
# 40 : '% % Relative Fuel Axial Extension',
 11 : 'fission gas release, %',
# 21 : '% % Molar Fraction of He',
# 22 : '% % Molar Fraction of Fission Products (Xe+Kr)',
# 23 : '% % Molar Fraction of Air',
# 24 : '% % Molar Fraction of Ar',
# 25 : 'na na Free Gas in the Rod cm3STP',
# 26 : 'na na Moles of Fission Gas Released',
# 28 : 'na na Moles of Gas in Rod',
 130: 'axial linear power, W|cm',
# 156: 'na na Normalized axial node power',
# 116: 'Btu/(hr-ft^2) W/(m^2) Surface Heat Flux',
# 142: 'GWd/MTU GWd/MTU Nodal Burnup',
# 157: 'n/m^2 n/m^2 Axial Fast Fluence',
 #150: 'ft m Axial Element Elevation',
 122: 'centerline temperature, C',
# 123: 'F K Fuel Pellet Surface Temperature',
# 145: 'F K Fuel Volume Average Temperature',
# 158: 'F K Gap Average Temperature',
# 124: 'F K Cladding Inside Temperature',
# 125: 'F K Cladding Average Temperature',
# 126: 'F K Cladding Outside Temperature',
# 139: 'F K Oxide Surface Temperature',
# 127: 'F K Bulk Coolant Temperature',
# 114: 'na na Fuel Duty Index',
 121: 'fuel stored energy, J|kg',
 107: 'cladding axial stress, MPa',
 108: 'cladding hoop stress, MPa',
 109: 'effective cladding stress, MPa',
 101: 'cladding axial strain, MPa',
 102: 'cladding hoop strain, MPa',
 103: 'cladding radial strain, MPa',
 136: 'cladding elastic hoop strain, MPa',
 137: 'cladding elastic axial strain, MPa',
 138: 'cladding elastic radial strain, MPa',
 104: 'cladding permanent axial strain, MPa',
 105: 'cladding permanent hoop strain, MPa',
 106: 'cladding permanent radial strain, MPa',
 144: 'cladding creep rate',
# 152: '% % Axial Strain due to Irradiation growth',
 169: 'cladding inner radius total deformation, um',
 170: 'cladding outer radius total deformation, um',
 128: 'fuel surface displacement, um',
 161: 'fuel thermal Expansion, um',
 162: 'fuel swelling, um',
 164: 'fuel densification, um',
 165: 'fuel relocation, um',
 143: 'fuel swelling rate',
 133: 'fuel surface axial strain',
# 141: '% % Nodal FGR',
 131: 'gap pressure, MPa',
# 129: 'psia mpa Gap Gas Pressure',
 155: 'mechanical gap, um',
 112: 'thermal gap, um',
 146: 'gap conductance, W|(m^2K)',
 115: 'oxide thickness, um',
# 140: 'ppm ppm Zircaloy-2 Hydrogen Concentration',
# 117: 'lbm/ft^3 kg/m^3 Coolant Density',
# 118: 'lb/(hr-ft^2) kg/(s-m^2) Coolant Mass Flux',
# 119: 'psia mpa Coolant Pressure',
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
	parser.add_argument("fname", nargs='?', help="output frapcon file")
	args = parser.parse_args()

	with open(args.fname) as f:
		data = f.read()
		data = data.split("===next time step")[1:]
		data = map(txt2array, data)

	with File(args.fname.split('.')[0]+'.h5') as f:
		for itime, d in enumerate(data):
			group = f.create_group("%06i"%itime)
			group.attrs['time, day'] = d[1]
			for i in keylist:
				group[keylist[i]] = d[i]
