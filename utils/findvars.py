#!/usr/bin/python
import re
import sys, os
import argparse
import fnmatch

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog="Find type of vatiables")
    parser.add_argument("-i", help="List of variables")
    parser.add_argument("-d", help="Source files")
    parser.add_argument("-o", help="Variable types")
    args = parser.parse_args()

    with open(args.i, 'r') as fi:
        txt = fi.read()
        vars = []
        for line in txt.split('\n'):
            a = re.search(r'Error: Symbol ([\w]*) at \(\d\) has no IMPLICIT type', line)
            if a:
                vars.append(a.group(1))

    tvars = {}

    for var in vars:
        for root, dir, files in os.walk(args.d):
            for item in fnmatch.filter(files, '*.f90'):
                with open(os.path.join(root,item), 'r') as f:
                    for line in f:
                        a = re.search(r'(integer|real)[^!]*%s\W'%var, line, re.IGNORECASE)
                        if a:
                            key = a.group(1).lower()
                            if not key in tvars: tvars[key] = set([])
                            tvars[key].add(var)

    for key in tvars:
        n = len(tvars[key])
        print "%s :: "%key + ("%s, "*n)%tuple(tvars[key])