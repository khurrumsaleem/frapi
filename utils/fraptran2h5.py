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

keylist = {
   1 : ( 'time, s', none ),
   2 : ( 'kW/ft kW/m Average Rod Power', none ),
   3 : ( 'delth', none ),
   4 : ( 'in mm Clad Axial Elongation', none ),
   5 : ( 'F K Plenum Gas Temperature', none ),
   6 : ( 'psia MPa Plenum Pressure', none ),
   7 : ( 'in^3 mm^3 Free Gas Volume', none ),
   8 : ( 'F K Average Fuel Temperature', none ),
   9 : ( 'kW kW Metal Water Reaction Energy', none ),
 100 : ( 'na na Cladding Axial Strain', none ),
 101 : ( 'na na Cladding Hoop Strain', none ),
 102 : ( 'na na Cladding Radial Strain', none ),
 103 : ( 'na na Cladding Perm Axial Strain', none ),
 104 : ( 'na na Cladding Perm Hoop Strain', none ),
 105 : ( 'na na Cladding Perm Radial Strain', none ),
 106 : ( 'psia MPa Cladding Axial Stress', none ),
 107 : ( 'psia MPa Cladding Hoop Stress', none ),
 108 : ( 'psia MPa Effective Cladding Stress', none ),
 161 : ( 'na na Cladding Eff. Elastic-Plastic Strain', none ),
 111 : ( 'mils mm Structural Radial Gap', none ),
 112 : ( 'rinterfacgap', none ),
 113 : ( 'psia MPa Clad Yield Stress', none ),
 114 : ( 'Pa Pa K coefficient', none ),
 115 : ( 'na na n coefficient', none ),
 116 : ( 'na na m coefficient', none ),
 117 : ( 'Pa Pa elastic modulus', none ),
 118 : ( '1/s 1/s strain rate for yield strength', none ),
 119 : ( 'na na Clad Instability Strain', none ),
 122 : ( 'MJ/m3 MJ/m3 SED: PNNL elastic-plastic eff. strain', none ),
 121 : ( 'MJ/m3 MJ/m3 SED: EPRI formulation', none ),
 123 : ( 'na na Coolant Quality', none ),
 124 : ( 'ft m Axial Node Elevation', none ),
 125 : ( 'Btu/(hr-ft^2-F) W/(m^2-K) Gap Heat Transfer Coefficient', none ),
 126 : ( 'kW/ft kW/m Water Metal Reaction Energy', none ),
 127 : ( 'in mm C-P OD Oxide Thickness', none ),
 128 : ( 'in mm C-P ID Oxide Thickness', none ),
 131 : ( 'kg/m**2 kg/m**2 C-P OD O2 Uptake', none ),
 132 : ( 'kg/m**2 kg/m**2 C-P ID O2 Uptake', none ),
 133 : ( 'frac frac C-P Total Cladding ECR', none ),
 134 : ( 'Btu/(hr-ft^2-F) W/(m^2-K) Surface Heat Transfer Coefficient', none ),
 135 : ( 'Btu/(hr-ft^2) W/(m^2) Surface Heat Flux', none ),
 136 : ( 'lb/(hr-ft^2) kg/(s-m^2) Coolant Mass Flux', none ),
 137 : ( 'psia MPa Coolant Pressure', none ),
 138 : ( 'BTU/(s-ft^2) W/(m^2) Critical Heat Flux', none ),
 #170 : ( 'na na Heat transfer mode', none ),
 139 : ( 'F K Fuel Centerline Temperature', none ),
 140 : ( 'F K Fuel Pellet Surface Temperature', none ),
 141 : ( 'F K Cladding Inside Temperature', none ),
 142 : ( 'F K Cladding Average Temperature', none ),
 143 : ( 'F K Cladding Outside Temperature', none ),
 144 : ( 'F K Bulk Coolant Temperature', none ),
 145 : ( 'ft mm Fuel Surface Displacement', none ),
 146 : ( 'psia MPa Gap Gas Pressure', none ),
 147 : ( 'kW/ft kW/m Axial Power', none ),
 148 : ( 'psia MPa Gap Interface Pressure', none ),
 149 : ( 'lbm/ft^3 kg/m^3 Coolant Density', none ),
 150 : ( 'na na Fuel Surface Axial Strain', none ),
 151 : ( 'na na Fuel Surface Hoop Strain', none ),
 152 : ( 'ft mm Cladding Inside Surface Displacement', none ),
 153 : ( 'ft mm Fuel Surface Displacement <DfRdOMsh', none ),
 154 : ( 'na na Cladding Total Axial Strain', none ),
 155 : ( 'na na Cladding Elastic Axial Strain', none ),
 156 : ( 'na na Cladding Thermal Axial Strain', none ),
 157 : ( 'na na Cladding Elastic Hoop Strain', none ),
 158 : ( 'na na Cladding Thermal Hoop Strain', none ),
 159 : ( 'na na Cladding Elastic Radial Strain', none ),
 160 : ( '1/sec 1/sec Cladding Hoop Strain Rate', none ),
 161 : ( 'BTU/lb J/kg Rad Ave Fuel Enthalpy  77.00F Ref', none ),
 162 : ( 'cal/g cal/g Fuel Enthalpy Increase After  Steady State', none ),
 163 : ( 'kW-s/ft kW-s/m Energy in Fuel', none ),
 164 : ( 'kW-s/ft kW-s/m Energy in Cladding', none ),
 165 : ( 'kW-s/ft kW-s/m Energy Input After Steady State', none ),
 166 : ( 'kW-s/ft kW-s/m Energy Output After Steady State', none ),
 }

def txt2array(d):
    x = {}
    for line in d.split('\n')[1:-1]:
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
