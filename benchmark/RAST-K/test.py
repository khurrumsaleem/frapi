#!/usr/bin/python 
# -*- coding:utf-8 -*-

from subprocess import call

call(['../../build/debug/test_rastk', 'data.inp', 'data.out'])

import err
#import plot
