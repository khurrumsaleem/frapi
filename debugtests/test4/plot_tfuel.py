#!/usr/bin/python

from numpy import loadtxt
import matplotlib.pyplot as plt
from subprocess import call
from h5py import File
from numpy import array, block

def fetch(fname, key):
    with File(fname) as f:
        d = []
        for key1 in f:
            g = f[key1][key]
            if g.dtype == 'float64':
                d.append(g[()])
            else:
                d.append(g[:])
        d = array(d)
    return d

def plot0d(time, d, title, legend="", figsize=(8,2), dpi=100):
    fig = plt.figure(num=None, figsize=figsize, dpi=dpi, facecolor='w', edgecolor='k')
    ax = fig.add_subplot(111)
    ax.plot(time, d, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
    ax.set_xlabel('Time (s)')
    ax.set_ylabel(title)
    ax.legend(legend)
    ax.grid()
#    plt.savefig("graphics/0D/"+"".join(title.split()[:-1])+"0D.png")

def plot1d(zmesh, d, title, legend="", figsize=(5,8), dpi=100):
    fig = plt.figure(num=None, figsize=figsize, dpi=dpi, facecolor='w', edgecolor='k')
    ax = fig.add_subplot(111)
    ax.plot(d, zmesh, '-o', ms=1, lw=2, alpha=0.7, mfc='orange')
    ax.set_ylabel('Elevation (cm)')
    ax.set_xlabel(title)
    ax.legend(legend)
    ax.grid()
#    plt.savefig("graphics/1D/"+"".join(title.split()[:-1])+"1D.png")

def plot2d(d, title):
    d = block([ [d[i,j] for j in range(2)] for i in range(2)])
    ax = plt.matshow(d, cmap='coolwarm')#, vmin=0, vmax=1000)
    bar = plt.colorbar(ax)
    plt.title(title)
    plt.xticks([])
    plt.yticks([])
#    plt.savefig("graphics/2D/"+"".join(title.split()[:-1])+"2D.png")

itime = -1

a = array([30.]+[365./8]*8+[30.])
zmesh = array([sum(a[:i+1]) for i in range(len(a))])[1:-1]

fname = "test.h5"

time  = fetch(fname, "time, s")

# Centerline fuel temperature
a = fetch(fname, "centerline temperature").max(1).max(1)
b = fetch(fname, "centerline temperature").min(1).min(1)
c = fetch(fname, "centerline temperature")[:,0,3]
d = array([a,b,c]).T
plot0d(time, d, "Centerline temperature (C)", legend=["maximun","minimum","point"], figsize=(5,7), dpi=100)

# Surface fuel temperature
a = fetch(fname, "surface temperature").max(1).max(1)
b = fetch(fname, "surface temperature").min(1).min(1)
c = fetch(fname, "surface temperature")[:,0,3]
d = array([a,b,c]).T
plot0d(time, d, "Surface temperature (C)", legend=["maximun","minimum", "point"], figsize=(5,7), dpi=100)

a = fetch(fname, "surface temperature")[:,:,3]
b = a[itime,:].reshape(2,2,16,16)
plot2d(b, "Surface pellet temperature (C)")

a = fetch(fname, "centerline temperature")[:,:,3]
b = a[itime,:].reshape(2,2,16,16)
plot2d(b, "Centerline pellet temperature (C)")

plt.show()