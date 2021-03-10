##### Programa que saca el H/V promedio para las estaciones
import os
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statistics as stats
import sys
import os.path as path
from os import remove

print("************************************")
print("*H/V PROMEDIO POR ESTACIÓN***")
print("************************************")
file = open('Estaciones_VEOX.txt')
line=file.read().replace("\n",",")
dr=50/1023
fr=[0:dr:50]



ID = open('ID_ENTRADA.txt')
ID2=ID.read().replace("\n",",")
#print(ID2)
#print(line)
n0=len(ID2)//15#Número de eventos en total utilizados
#print(n2)
ii=0
HV1=np.zeros((1024))

for i in range(50):#42
    j=ii+4
    #print(ii,j)
    ee=line[ii:j]
    ii=j+1
    #print(ee)
    name_p=ee
    name=ee+'.txt'#Guardar mi HV Promedio
    n1=0
    #Eventos por estación
    EPE=0
    #Inicializar Efecto de sitio por estación
    HV2=np.zeros(1024)
    HV3=np.zeros(1024)
    HVPROM_AUX=np.zeros(1024)
    HVDESV_AUX=np.zeros(1024)
    HVPROM=np.zeros(1024)
    HVDESV=np.zeros(1024)

    for nn in range(n0):#n0 número de eventos
        n2=n1+14
        ID3=ID2[n1:n2]
        c_name=ee+'_'+ID3+'.txt'#Estacion+ID
        print(c_name)
        n1=n2+1

        if (path.exists(c_name)):#Aseguar que este el archivo

            #Primer criterio
            HV1=np.loadtxt(c_name)
            maximo=max(HV1)
            minimo=min(HV1)
            #print(c_name)
            #print(maximo,minimo)

            #Remover archivos con valores menores
            if (maximo > 10 or minimo < 0.1):
               remove(c_name)

        if (path.exists(c_name)):
            EPE=EPE+1
            HV1=np.loadtxt(c_name)
            fig, axs = plt.subplots(1,2)
            fig.suptitle(titulo)
            axs[0].set_xscale('log')
            axs[0].set_yscale('log')
            axs[0].grid(True,which="both",ls="-")
            axs[0].plot(fr,HV1,'tab:blue')
            axs[0].set_title('H/V Por estación')
            axs[0].set_ylim(0, 10)

        #print(rrr[77:81])


            for kk in range(len(HV1)):
                HV2[kk]=HV2[kk]+HV1[kk]
                HV3[kk]=HV3[kk]+HV1[kk]**2
    #Primer promedio
    for g in range(len(HV2)):
        HVPROM_AUX[g]=HV2[g]/EPE
        HVDESV_AUX[g]=np.sqrt(abs((HV3[g]/(EPE-1))-((HV2[g]/(EPE-1))**2)))

    axs[1].plot(fr,HVPROM_AUX)
    axs[1].set_xlim(1, 10)
    axs[1].set_xscale('log')
    axs[1].set_yscale('log')
    axs[1].set_ylim(0, 10)
    axs[1].set_xlabel('Frecuencia [Hz]')
    axs[1].grid(True,which="both",ls="-")
    axs[1].set_title('H/V PROMEDIO')
    axs[1].set_ylim(0, 10)
    plt.savefig(name_p)


    #Comparar las curvas por evento con el HVPROM_AUX
    HV2=np.zeros(1024)
    HV3=np.zeros(1024)
    Limite_inf=np.zeros(1024)
    Limite_sup=np.zeros(1024)
    EPE=0
    n1=0
    #############################
'''
    for nn in range(n0):#n0 número de eventos
        n2=n1+14
        ID3=ID2[n1:n2]
        c_name=ee+'_'+ID3+'.txt'#Estacion+ID
        print(c_name)
        n1=n2+1
        if (path.exists(c_name)):#Aseguar que este el archivo

            #Primer criterio
            HV1=np.loadtxt(c_name)
            criterio=0.5
            for rr in range(len(HVPROM_AUX)):
                Limite_inf[rr]=HVPROM_AUX[rr]-(criterio*HVDESV_AUX[rr])
                Limite_sup[rr]=HVPROM_AUX[rr]+(criterio*HVDESV_AUX[rr])
            #Remover archivos con valores menores
                if (HV1[rr]>Limite_sup[rr] or HV1[rr]<Limite_inf[rr]):
                        if (path.exists(c_name)):
                             remove(c_name)

        if (path.exists(c_name)):
            EPE=EPE+1
            HV1=np.loadtxt(c_name)

            for kk in range(len(HV1)):
                HV2[kk]=HV2[kk]+HV1[kk]
                HV3[kk]=HV3[kk]+HV1[kk]**2

    for g in range(len(HV2)):
        HVPROM[g]=HV2[g]/EPE
        HVDESV[g]=np.sqrt(abs((HV3[g]/(EPE-1))-((HV2[g]/(EPE-1))**2)))'''
    fsalida = open(name, 'w')
    for q in range(len(HVPROM)):
        fsalida.write('%10.4f\t%10.4f\n' % (HVPROM_AUX[q],HVDESV_AUX[q]))

    fsalida.close()






    #########################
