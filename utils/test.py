#!/usr/bin/python 
# -*- coding:utf-8 -*-
import argparse
import os
from subprocess import call
from h5py import File
import matplotlib.pyplot as plt
from numpy import array

names = [
'average linear power, W|cm',
'fuel burnup, MW*d|kg',
'average fuel burnup, MW*d|kg',
'centerline temperature, C',
'fuel volume average temperature, C',
'gap average temperature, C',
'cladding average temperature, C',
'bulk coolant temperature, C',
'cladding axial strain, %',
'cladding radial strain, %',
'cladding hoop strain, %',
'cladding axial stress, MPa',
'cladding hoop stress, MPa',
'gap pressure, MPa',
'oxide thickness, um',
'cladding hydrogen concentration, ppm',
'fission gas release, %',
'total gap conductance, W|(m^2*K)',
#'cladding creep rate',
#'fuel swelling, um',
'fuel stored energy, J|kg',
]


def make(filename):
    call(["../../build/debug/test_frapi", "%s.inp"%filename])
    call(["../../build/debug/main_frapcon", "%s.inp"%filename])
    call(["../../utils/frapcon2h5.py", "%s.plot"%filename, "%s-frapcon.h5"%filename])
    call(["../../utils/frapi2h5.py", "%s-frapi.txt"%filename, "%s-frapi.h5"%filename])

def draw(filename):
    f0 = File('%s-frapi.h5'%filename)
    f1 = File('%s-frapcon.h5'%filename)

    dirname = '../../doc/graphics'
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    dirname = '../../doc/graphics/%s'%filename
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    print "%50s %10s %10s"%('Parameter', 'RMS, %', 'MAX, %')

    for i, name in enumerate(names):
        fig, ax = plt.subplots()

        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['average fuel burnup, MW*d|kg'][0])
            data0.append(f0[group][name][0])

        time1 , data1 = [], []    
        for group in f1.keys():        
            time1.append(f1[group]['average fuel burnup, MW*d|kg'])
            data1.append(f1[group][name][0]) 

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.plot(time1, data1, '-o', ms=1, lw=2, alpha=0.7, mfc='blue')
        ax.set_xlabel('Average fuel burnup, MW*d/kg')
        ax.set_ylabel(name)
        ax.legend(['FRAPI', 'FRAPCON'])
        ax.grid()

        plt.savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))

        data0 = array(data0)[:]
        data1 = array(data1)[:]
        eps = 1.e-6
        a = (data0 + eps)/(data1 + eps) - 1
        errmax = 100 * max(abs(a))
        errrms = 100 * pow(pow(a, 2).mean(), 0.5)
        if errmax < 5 and errrms < 1: 
            res = 'OK'
        else:
            res = 'FAIL'
        print "%50s %10.1f %10.1f %s"%(name, errrms, errmax, res)

    f0.close()
    f1.close()

if __name__ == '__main__':
    
    parser = argparse.ArgumentParser(prog="Make *.h5 file from frapcon output")
    parser.add_argument("ifile", nargs='?', help="input frapcon file")
    
    args = parser.parse_args()

    make(args.ifile)
    draw(args.ifile)