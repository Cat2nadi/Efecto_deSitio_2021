##### Programa que saca el H/V promedio para las estaciones
import os
import math
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statistics as stats
import sys
import fnmatch

import os.path as path
from os import remove
import shutil
file = open('Estaciones_VEOX.txt')
line=file.read().replace("\n",",")
estaciones=np.array(line.split(","))

dr=50/1024
fr=np.arange(0,50,dr)
g1=0
file.close()
cwd = os.getcwd()

est=1
#El archivo ID_ENTRADA se crea a partir del los eventos seleccionados AAAAMMDDHHmmmss
ID = open('ID_ENTRADA.txt')
ID2=ID.read().replace("\n",",")
#Nombre de los eventos
ID3=np.array(ID2.split(","))#lee los eventos del archivo

ID.close()

n0=len(ID2)//15#NO nos da el número de eventos
#El número de caracteres 15 entre la longitud

for i in range(len(ID3)-1):
    r=cwd+'/'+ID3[i]#El path de las carpetas por eventos AAAAMMDDHHmmmss

    cont=0
    for j in range(len(estaciones)-1):
        #nombre_busqueda ESTACION_ID3.txt= RIVA_20080212021305.txt
        nombre_busqueda=estaciones[j]+'_'+ID3[i]+'.txt'
        #nombre=ZAPO20080212021305
        nombre=estaciones[j]+ID3[i]

        if (path.exists(nombre_busqueda)):

            #path2=os.chdir(r)#cambiar de directorio

            #Nombre de directorios de los eventos en el formato AAAAMMDDHHmmmss
            nombre_directorio=ID3[i]

            nombre_directorio_2=r+'/'+estaciones[j]+'.txt'

            #Copiar el promedio H/V por estaciones de la carpeta general a la individual por eventos
            #Copiar el archivo AGUA.txt a /home/nadia/Escritorio/Tesis_2019/Procesamiento/EFECTO_DE_SITIO/DATOS_CORREGIDOS/20080208203118/ZAPO.txt
            shutil.copy(estaciones[j]+'.txt', nombre_directorio_2)


            cont=cont+1





    #Cambio de directorio por carpeta de evento AAAAMMDDHHmmmss
    os.chdir(r)
    #print(os.getcwd())

    #*_20080212021305*txt
    name_aux='*_'+ID3[i]+'*txt'
    #cantidad de estaciones reales por eventos
    cantidad_estaciones_porevento=len(fnmatch.filter(os.listdir(r),name_aux))

    #Genera el archivo que incluira todo los modelos
    name=nombre_directorio+'.txt'
    fsalida = open(name, 'w')
    fsalida.close()




    #print(name)


    nn=1024*cont

    MI=np.zeros(nn)
