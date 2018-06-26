#!/usr/bin/python 
# -*- coding:utf-8 -*-
import argparse
import os
from subprocess import call
from h5py import File
import matplotlib.pyplot as plt

names = [
'centerline temperature, C',
'fission gas release, %',
'average linear power, W|cm',
'bulk coolant temperature, C',
'total gap conductance, W|(m^2*K)',
'oxide thickness, um',
#'mechanical gap thickness, um',
'gap pressure, MPa',
'cladding hoop strain, %',
'cladding hoop stress, MPa',
'fuel burnup, MW*s|kg',
]   

def make(filename):
    call(["../../build/debug/frapcon_input_file", "%s.inp"%filename])
    call(["../../build/debug/frapcon_original", "%s.inp"%filename])
    call(["../../utils/frap2h5.py", "%s.plot"%filename, "%s-plot.h5"%filename])

def draw(filename):
    f0 = File('%s.h5'%filename)
    f1 = File('%s-plot.h5'%filename)

    dirname = '../../doc/UserGuide/figs/%s'%filename
    if not os.path.isdir(dirname): 
        os.mkdir(dirname)

    for i, name in enumerate(names):
        fig, ax = plt.subplots()

        time0 , data0 = [], []
        for group in f0.keys():
            time0.append(f0[group]['time, day'][0])
            data0.append(f0[group][name][0])

        time1 , data1 = [], []    
        for group in f1.keys():        
            time1.append(f1[group]['time, day'])
            data1.append(f1[group][name][0]) 

        ax.plot(time0, data0, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
        ax.plot(time1, data1, '-o', ms=1, lw=2, alpha=0.7, mfc='blue')
        ax.set_xlabel('Time, day')
        ax.set_ylabel(name)
        ax.legend(['NEW FRAPCON', 'OLD FRAPCON'])
        ax.grid()

        plt.savefig('%s/%s.png'%(dirname, name.split(',')[0].replace(' ','_')))
    
    #plt.show()

    f0.close()
    f1.close()  

if __name__ == '__main__':
    
    parser = argparse.ArgumentParser(prog="Make *.h5 file from frapcon output")
    parser.add_argument("ifile", nargs='?', help="input frapcon file")
    
    args = parser.parse_args()

    make(args.ifile)
    draw(args.ifile)