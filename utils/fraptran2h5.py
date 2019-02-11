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
   2 : ( 'average fuel rod power, kW|m', none ),
   3 : ( 'fuel stack elongation, mm', none ),
   4 : ( 'cladding axial elongation, mm', none ),
   5 : ( 'plenum gas temperature, K', none ),
   6 : ( 'plenum gas pressure, MPa', none ),
   7 : ( 'total void volume, mm^3', none ),
#   8 : ( 'F K Average Fuel Temperature', none ),
   9 : ( 'total water metal reaction energy, kW|m', none ),
 100 : ( 'cladding total axial strain, %', none ),
 101 : ( 'cladding total hoop strain, %', none ),
 102 : ( 'cladding total radial strain, %', none ),
 103 : ( 'cladding plastic axial strain, %', none ),
 104 : ( 'cladding plastic hoop strain, %', none ),
 105 : ( 'cladding plastic radial strain, %', none ),
 106 : ( 'cladding axial stress, MPa', none ),
 107 : ( 'cladding hoop stress, MPa', none ),
 108 : ( 'cladding effective stress, MPa', none ),
 161 : ( 'cladding effective elastic-plastic strain, %', none ),
 111 : ( 'structural radial gap, mm', none ),
 112 : ( 'thermal radial gap, mm', none ),
 113 : ( 'work-hardened yield stress, MPa', none ),
 114 : ( 'k coefficient', none ),
 115 : ( 'n coefficient', none ),
 116 : ( 'm coefficient', none ),
 117 : ( 'elastic modulus, MPa', none ),
 118 : ( 'strain rate coefficient', none ),
 119 : ( 'cladding instability strain, %', none ),
 122 : ( 'sed based on elastic-plastic effective strain, %', none ),
 121 : ( "sed based on epri's formulation", none ),
 123 : ( 'coolant quality', none ),
 124 : ( 'axial node elevation, mm', none ),
 125 : ( 'heat transfer coefficient, W|m^2K', none ),
 #126 : ( 'water metal reaction energy, kW|m', none ), !repetition
 127 : ( 'outer oxide thickness, mm', none ),
 128 : ( 'inner oxide thickness, mm', none ),
 131 : ( 'c-p or b-j od oxygen uptake', none ),
 132 : ( 'c-p or b-j id oxygen uptake', none ),
 133 : ( 'c-p or b-j cladding ecr', none ),
 134 : ( 'surface heat transfer coefficient, W|m^2K', none), 
 135 : ( 'surface heat flux, W|m^2', none), 
 136 : ( 'coolant mass flux, kg|(s*m^2)', none),
 137 : ( 'coolant pressure, MPa', none), 
 138 : ( 'critical heat flux, W|m2', none), 
 178 : ( 'heat transfer mode', none), 
 139 : ( 'centerline temperature, K', none), # esotemp
 140 : ( 'fuel pellet surface temperature, K', none), 
 141 : ( 'cladding inner temperature, K', none), 
 142 : ( 'average cladding temperature, K', none), 
 143 : ( 'cladding outer temperature, K', none), 
 144 : ( 'bulk coolant temperature, K', none), 
 145 : ( 'pellet surface displacement, mm', none), 
 146 : ( 'gap pressure, MPa', none), 
 147 : ( 'axial power, kW|m', none), 
 148 : ( 'structural gap interface pressure, MPa', none), 
 149 : ( 'coolant density, kg|m^3', none), 
 150 : ( 'pellet surface axial strain, %', none), 
 151 : ( 'pellet surface hoop strain, %', none), 
 152 : ( 'cladding inside surface radial displacement, mm', none), #EOSRad(ncladi,k)-RadialBound(ncladi) : 22
 153 : ( 'fuel pellet surface displacement, mm', none), #EOSRad(igpnod,k) - RadialBound(igpnod) : 22
 155 : ( 'cladding elastic axial strain, %', none),
 156 : ( 'cladding thermal axial strain, %', none),
 157 : ( 'cladding elastic hoop strain, %', none),
 158 : ( 'cladding thermal hoop strain, %', none),
 159 : ( 'cladding elastic radial strain, %', none),
 160 : ( 'cladding hoop strain rate', none),
 #161 : ( 'BTU/lb J/kg Rad Ave Fuel Enthalpy  77.00F Ref', none ),
 #162 : ( 'cal/g cal/g Fuel Enthalpy Increase After  Steady State', none ),
 #163 : ( 'kW-s/ft kW-s/m Energy in Fuel', none ),
 #164 : ( 'kW-s/ft kW-s/m Energy in Cladding', none ),
 #165 : ( 'kW-s/ft kW-s/m Energy Input After Steady State', none ),
 #166 : ( 'kW-s/ft kW-s/m Energy Output After Steady State', none ),
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
