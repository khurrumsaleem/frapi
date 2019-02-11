#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
from subprocess import call
from h5py import File
import matplotlib.pyplot as plt
from numpy import array

def draw(filename):
    f0 = File('%s-frapi.h5'%filename)
    f1 = File('%s-fraptran.h5'%filename)

    dirname = '../../doc/graphics'
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    dirname = '../../doc/graphics/%s'%filename
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    print "%50s %10s %10s %10s %10s"%('Parameter', 'Units', 'RMS, %', 'MAX, %', 'Result')

    for i, name in enumerate(names):
        fig, ax = plt.subplots()
        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['frapi time, s'][0])
            data0.append(f0[group][name][0])

        time1 , data1 = [], []    
        for group in f1.keys():
            time1.append(f1[group]['time, s'])
            data1.append(f1[group][name][0]) 

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.plot(time1, data1, '-o', ms=1, lw=2, alpha=0.7, mfc='blue')
        ax.set_xlabel('time, s')
        ax.set_ylabel(name)
        ax.legend(['FRAPI', 'FRAPTRAN'])
        ax.grid()

        plt.savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))
    f0.close()
    f1.close()

names = [
'average fuel rod power, kW|m',
'fuel stack elongation, mm',
'cladding axial elongation, mm',
'plenum gas temperature, K',
'plenum gas pressure, MPa',
'total void volume, mm^3',
'cladding total axial strain, %',
'cladding total hoop strain, %',
'cladding total radial strain, %',
'cladding plastic axial strain, %',
'cladding plastic hoop strain, %',
'cladding plastic radial strain, %',
'cladding axial stress, MPa',
'cladding hoop stress, MPa',
'cladding effective stress, MPa',
'structural radial gap, mm',
'thermal radial gap, mm',
'work-hardened yield stress, MPa',
'elastic modulus, MPa',
'strain rate coefficient',
'cladding instability strain, %',
'coolant quality',
'heat transfer coefficient, W|m^2K',
#'total water metal reaction energy, kW|m',
#'outer oxide thickness, mm',
#'inner oxide thickness, mm',
'surface heat transfer coefficient, W|m^2K',
'surface heat flux, W|m^2',
#'coolant mass flux, kg|sm2',
'coolant pressure, MPa',
'critical heat flux, W|m2',
'centerline temperature, K',
#'fuel pellet surface temperature, c',
#'cladding inner temperature, K',
#'average cladding temperature, K',
#'cladding outer temperature, K',
#'bulk coolant temperature, K',
#'pellet surface displacement, mm',
'gap pressure, MPa',
'axial power, kW|m',
'structural gap interface pressure, MPa',
'coolant density, kg|m^3',
'pellet surface axial strain, %',
'pellet surface hoop strain, %',
'cladding elastic axial strain, %',
'cladding thermal axial strain, %',
'cladding elastic hoop strain, %',
'cladding thermal hoop strain, %',
'cladding elastic radial strain, %',
'cladding hoop strain rate',
]


task = 'rep-na1'

if True:
    print "FRAPCON: "
#    call(['../../build/debug/main_frapcon', 'rep-na1-frapcon.inp'])
    print "FRAPTRAN: "
#    call(['../../build/debug/main_fraptran', 'rep-na1-fraptran.inp'])
#    call(["../../utils/fraptran2h5.py", "%s-fraptran.plot"%task, "%s-fraptran.h5"%task])
    print "FRAPI: "
    call(['../../build/debug/test_frapi', "fraptran", '%s-fraptran.inp'%task, './restart-na1.txt', './%s-out.txt'%task])
#    call(["../../utils/frapi2h5.py", "%s-out.txt"%task, "%s-frapi.h5"%task])
#    draw(task)