##### Program that calculates the initial model for the calculation of H / V
import os
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statistics as stats
import sys
from matplotlib.ticker import ScalarFormatter, NullFormatter
from matplotlib.legend_handler import HandlerLine2D
print("Se hace el modelo inicial con los espectros")

#path1=%pwd
path=os.getcwd()
#print(path)
#### Número de estaciones
est=path+"/dist.txt"

est=open(est,'r+')
ne=len(est.readlines())
print(ne)
est.close()


path_Z=os.path.join(path,"HHZ/ESP")
path_N=os.path.join(path,"HHN/ESP")
path_E=os.path.join(path,"HHE/ESP")




Archivos_Z=[os.path.join(path_Z,d) for d in sorted(os.listdir(path_Z)) if d.endswith(".esp")]
Archivos_N=[os.path.join(path_N,d) for d in sorted(os.listdir(path_N)) if d.endswith(".esp")]
Archivos_E=[os.path.join(path_E,d) for d in sorted(os.listdir(path_E)) if d.endswith(".esp")]

Archivos=[os.path.join(path_Z,d) for d in os.listdir(path_Z) if d.endswith(".esp")]


df=pd.read_csv(Archivos[0],sep="\s+", header=None)


############################################################
################ Parámetros de la fuentes ##################
############################################################
Mw=float(input("Por favor introduzca el valor de Mw del evento:"))
print (Mw)

########## Momento Escalar #############
#Mo=10**((2/3)*(MW+10.7))#kananmori aproximation
#Mo=10**((Mw*1.466)+14.1)
Mo=10**((Mw*1.5)+16.13)

MoNM=Mo*(10**-7)##Dinas to Nm
print(MoNM)
### Frecuencia de esquina
FC=(MoNM/(1.0000e+16))**(-1/3)
print('Fc=',FC)

##Low frequency spectral lebvel Omega
Omega=math.log((Mo/(1*10**16)),10)
print('Omega=',Omega)

##Atenuation funtion to cost
Q=(211*(df[0]**0.46))
## local attenuation
t=np.array(Q**(-1))
###Media of local attenuation
t_m=stats.mean(t)
print('t=',t_m)


plt.title('Local Attenuation')
plt.plot(df[0],t)
plt.yscale('log')
plt.xscale('log')



################################################################################
#######################EFECTO DE SITIO #########################################
################################################################################

nn=0
HH=np.zeros((1024))
AMP_AUX=np.zeros((1024))
HV=np.zeros((1024))
ALL=np.zeros((len(Archivos))*1024)
TT=np.zeros((len(Archivos))*1024)
AMP_REAL=np.zeros((len(Archivos))*1024)


for arch in range(len(Archivos)):

#       try:
        r=Archivos[arch]

        rr=Archivos_N[arch]
        #print(rr[109:113])
        rrr=Archivos_E[arch]
        df=pd.read_csv(Archivos_Z[arch],sep="\s+", header=None)
        dfN=pd.read_csv(Archivos_N[arch],sep="\s+", header=None)
        dfE=pd.read_csv(Archivos_E[arch],sep="\s+", header=None)
        dist_2=round(dfN.iloc[0,2],2)##redoondear a 2 decimales
        print(dist_2)
        str_dist=str(dist_2)
        titulo=rr[109:113]+" "+str_dist+"[km]"
        name_i=str(rr[109:113])
        print(titulo)

        E=np.array(dfE[1]**2)
        N=np.array(dfN[1]**2)
        Z=np.array(df[1]**2)

        for i in range(len(E)):
            HH[i]=E[i]+N[i]
            HV[i]=math.sqrt(HH[i]/(2*Z[i]))
            nn=(nn+1)
            rr=nn-1
##### Amplitud de los datos observados
            AMP_AUX[i]=math.sqrt(E[i]+N[i]+Z[i])


            ALL[rr]=HV[i]
            TT[rr]=t[i]
            AMP_REAL[rr]=AMP_AUX[i]
        fig, axs = plt.subplots(1,2)
        fig.suptitle(titulo)
        axs[0].set_xscale('log')
        axs[0].set_yscale('log')
        xticks=[0.1, 1, 10]
        yticks=[0.0000000001,0.000000001,0.00000001,0.0000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1,10]
        axs[0].set_xticks(xticks)
        axs[0].set_yticks(yticks)
        axs[0].grid(True,which="both",ls="-")
        linehhz,=axs[0].plot(df[0],df[1],'tab:blue', label="HHZ")
        linehhn,=axs[0].plot(df[0],dfN[1],'tab:orange', label="HHN")
        linehhe,=axs[0].plot(df[0],dfE[1],'tab:red', label="HHE")
        lineampl,=axs[0].plot(df[0],AMP_AUX,'tab:green', label="AMPLITUD")
        axs[0].set_title('Spectrums')
        axs[0].legend(handler_map={linehhz:HandlerLine2D(numpoints=4)},loc='lower right')
        #axs[0].set_ylim(0, 10)

        #print(rrr[77:81])

        axs[1].plot(df[0],HV)
        axs[1].set_xlim(1, 10)
        axs[1].set_xscale('log')
        axs[1].set_yscale('log')
        xticks=[0.1, 1, 10]
        yticks=[0.0001, 0.001, 0.01, 0.1, 1, 10]
        axs[1].set_xticks(xticks)
        axs[1].set_yticks(yticks)
        #axs[1].set_ylim(0, 10)
        axs[1].set_xlabel('Frecuencia [Hz]')
        axs[1].grid(True,which="both",ls="-")
        axs[1].set_title('Site Effect H/V Nakamura')
        axs[1].set_ylim(0, 10)
        plt.savefig(name_i)
        #plt.grid(True,which="majorminor",ls="-", color='0.65')


npar=(2*1)+(ne*(1024+1))
fsalida = open('modelinicial.dat', 'w')
fsalida.write('%10.4f\n' % Omega)
fsalida.write('%10.4f\n' % FC)
#print(npar)
for h in range(1,ne+1):
    fsalida.write('%10.4f\n' % t_m)


for j in range(len(ALL)):
    fsalida.write('%10.4f\n' % (ALL[j]))
fsalida.close()

############## ESCRIBO AMPLITUD ########################
fampl = open('ampl.txt', 'w')
for ni in range(0,len(AMP_REAL)):
    fampl.write('%10.10f\n' % (AMP_REAL[ni]))
fampl.close()






#plt.show()
