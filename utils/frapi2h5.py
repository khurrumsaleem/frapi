#!/usr/bin/python 
# -*- coding:utf-8 -*-
import os
import argparse
from h5py import *


def txt2array(d):
    x = []
    lines = d.split('\n')
    for line in lines:
        mark = line[:2]
        if mark == '#0':
            varname = line[3:]
            values = []
        elif mark == '#1':
            comment = line[3:]
        elif mark == '#2':
            vartype = line[3:]
        elif mark == '#3':
            values += map(float, line[3:].split())
        elif mark == '#4':
            if varname == 'frapi time, s':
                x.append({varname: values})
            else:
                x[-1][varname] = values
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
            #group.attrs['time, day'] = d['time, day']
            for key in d:
                group[key] = d[key]
