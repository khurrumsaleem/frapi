from numpy import *
import matplotlib.pyplot as plt

table = loadtxt('data.inp', skiprows=14)

distortion = lambda t: 1. + 0.1 * sin(pi*t/20.)

data = table[1:] * distortion(table[0])

#f = open('data2.inp', 'w')

savetxt('data2.inp',data,fmt='%11.7f')

#for d in data:
#	f.write('{:.6}'.format(d))

