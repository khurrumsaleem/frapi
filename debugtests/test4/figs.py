#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
from subprocess import call
from h5py import File
import matplotlib.pyplot as plt
from numpy import array

def draw():
    f0 = File('test.h5')

    for i, name in enumerate(names):

        fig, ax = plt.subplots()

        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['time, s'][0])
            data0.append(f0[group][name][0])

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.set_xlabel('time, s')
        ax.set_ylabel(name)
#        ax.legend(['FRAPI', 'FRAPTRAN'])
        ax.grid()

#        plt.savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))

    plt.show()

    f0.close()
    f1.close()

names = [
#'time step size, s',
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
'coolant mass flux, kg|(s*m^2)',
'coolant pressure, MPa',
'critical heat flux, W|m2',
'centerline temperature, K',
#'fuel pellet surface temperature, c',
#'cladding inner temperature, K',
#'average cladding temperature, K',
#'cladding outer temperature, K',
'bulk coolant temperature, K',
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

draw()