#!/usr/bin/python

import re
import os, sys
from os.path import join
import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog="Rename module")
    parser.add_argument("-d", help="directory with source files")
    parser.add_argument("-p", help="postfix")
    args = parser.parse_args()

    for src_name in os.listdir(args.d):
        counter = 0
        txt_ = ""
        with open(join(args.d, src_name), 'r') as f:
            txt = f.read().split('\n')

        for line in txt:
            a = re.search(r'[\w\s]*?(use|module) [\s]*([\w\s]*)', line, re.IGNORECASE)
            if a and not re.search('procedure', line, re.IGNORECASE):
                if not a.group(2).split('_')[-1] == args.p:
                    line = line.replace(a.group(2), "%s_%s"%(a.group(2),args.p))
                counter += 1
            txt_ += line + '\n'

        with open(join(args.d, src_name), 'w') as f:
            f.write(txt_)

        print("File %30s: %4i lines"%(src_name,counter))