from numpy import *
import matplotlib.pyplot as plt

ref = loadtxt('ref.out').T
table = loadtxt('data.out').T
data, mesh = table[:-1], table[-1]

names = [
'axial fuel temperature, C',
'bulk coolant temperature, C',
'total gap conductance, W/(m^2*K)',
'oxide thickness, um',
'mechanical gap thickness, um',
'gap pressure, MPa',
'cladding hoop strain, %',
'cladding axial stress, MPa'
]   

for i, name in enumerate(names):
    #if i in [0,2]:
        fig, ax = plt.subplots()
        ax.plot(mesh, data[i], '-o', ms=10, lw=2, alpha=0.7, mfc='orange')
        ax.plot(mesh, ref[i], '-o', ms=10, lw=2, alpha=0.7, mfc='red')
        ax.set_xlabel('Axial elevation, cm')
        ax.set_ylabel(name)
        ax.legend(['NEW FRAPCON', 'OLD FRAPCON'])
        ax.grid()
        plt.savefig('../doc/UserGuide/figs/%s.png'%name.split(',')[0].replace(' ','_'))

#plt.show()
