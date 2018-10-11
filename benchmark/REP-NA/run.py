#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

#call(['../../build/debug/main_frapcon', 'rep-na1-frapcon.inp'])
#call(['../../build/debug/main_fraptran', 'rep-na1-fraptran.inp'])
call(['../../build/debug/test_frapi', "fraptran", 'rep-na1-fraptran.inp', './restart-na1.txt'])