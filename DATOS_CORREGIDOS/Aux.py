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
estaciones=np.array(line.split(","))
#print(estaciones)
#print(type(estaciones))
#print(estaciones[5])
dr=50/1024
fr=np.arange(0,50,dr)#Frecuencia
g1=0
file.close()
#print (dr , fr)

est=1
ID = open('ID_ENTRADA.txt')
ID2=ID.read().replace("\n",",")
ID.close()
n0=len(ID2)//15#Número de eventos en total utilizados
#print(n2)
ii=0
HV1=np.zeros((1024))
gg=0

print(ID2)## Imprime las fechas

#Recorre las estaciones del Arreglo
for i in range(3):#42
    j=ii+4
    print(ii,j)
    ee=line[ii:j]
    print(ee)


    ii=j+1
    name_p=ee #Nombre de la estación

    n1=0
    #Eventos por estación
    EPE=0
    #Inicializar Efecto de sitio por estación
    HV2=np.zeros(1024)
    HV3=np.zeros(1024)
    HV1_MATRIZ=np.zeros((1024,n0))
    #EXISTE_MATRIZ=np.zeros((50,n0))
    HVPROM=np.zeros(1024)
    HVDESV=np.zeros(1024)
    #plt.figure()
    for nn in range(n0):#n0 número de eventos
        n2=n1+14
        ID3=ID2[n1:n2]
        c_name=ee+'_'+ID3+'.txt'#Estacion+ID
        #print(c_name)
        n1=n2+1

        if (path.exists(c_name)):

            #Primer criterio
            HV1=np.loadtxt(c_name)
            maximo=max(HV1)
            minimo=min(HV1)
            #print(c_name)
            #print(maximo,minimo)

            #Remover archivos con valores menores
            if (maximo > 10 or minimo < 0.1):
               remove(c_name)
               print("Entro y removio", c_name)

        if (path.exists(c_name)):

            for j in  range (len(estaciones)):
                if (estaciones[j] == name_p):
                    indice=j
                    print(estaciones[indice])
