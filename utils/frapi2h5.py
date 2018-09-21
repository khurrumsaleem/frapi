#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
import argparse
from h5py import *



def txt2array(d):
    x = []
    lines = d.split('\n')
    for i in range(0,len(lines),3)[:-1]:
        key  = lines[i]
        line = lines[i+2]
        line = line.split()
        value = map(float, line)
        if key == 'frapi burnup step':
            x.append({key: value})
        else:
            x[-1][key] = value
    return x

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog="Make *.h5 file from frapcon output")
    parser.add_argument("ifile", nargs='?', help="output frapcon file")
    parser.add_argument("ofile", nargs='?', help="output HDF5 file")
    args = parser.parse_args()

    if os.path.isfile(args.ofile): os.remove(args.ofile)

    with open(args.ifile) as f:
        data = f.read()
        data = txt2array(data)

    with File(args.ofile) as f:
        for itime, d in enumerate(data):
            group = f.create_group("%06i"%itime)
            group.attrs['time, day'] = d['time, day']
            for key in d:
                group[key] = d[key]
