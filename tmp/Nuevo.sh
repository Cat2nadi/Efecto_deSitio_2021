#!/bin/bash
#Leer el ID

##########################
## PATH ARCHIVOS  /TEMPORALES
##########################
/bin/rm -r  Datos_Iniciales.txt eventos.tmp *.sac identificador.tmp
p1=$(pwd)
cd ..
#### Path /EFECTO_DE_SITIO
p2=$(pwd)

########Crear BASE DE DATOS
/bin/rm -r DATOS DATOS_CORREGIDOS
mkdir DATOS
####Regreso a los temporales 
cd $p1

for line in $(cat ID_ENTRADA.txt);
       do 
          cd $p1
	  awk 'BEGIN{FS=","}'/$line/'{print $5,$9,$10,$11,$12,$13}' BaseDeDatosaux2.csv > Datos_Iniciales.txt
          # Crear carpeta con ID
          mkdir $p2/DATOS/$line
 	  fecha=$(awk '{print $1 }' Datos_Iniciales.txt)
	  hora=$(awk '{print $2 }' Datos_Iniciales.txt)
          lat=$(awk '{print $3 }' Datos_Iniciales.txt)
          lon=$(awk '{print $4 }' Datos_Iniciales.txt)
	  prof=$(awk '{print $5 }' Datos_Iniciales.txt)	
	  mag=$(awk '{print $6 }' Datos_Iniciales.txt)	

          #Empezar descarga, se debé entrar a la carpeta VEOX
          echo 'WIN TO % HH_ '$fecha','$hora'' '+3m' > eventos.tmp;
          
	  ./VEOXstp < eventos.tmp;
          mv *sac $p2/DATOS/$line

          ###########################################
          ##########    CORRECIÓN        ############
       
	  ## Crear el archivo correción en la carpeta tmp
          echo 'r ' '*.sac' >> correcion.tmp
	  echo 'ch EVLO' ''$lon'' >> correcion.tmp
	  echo 'ch EVLA' ''$lat'' >> correcion.tmp
          echo 'ch EVDP' ''$prof'' >> correcion.tmp
          echo 'wh' >> correcion.tmp
          echo 'rtrend  ' >> correcion.tmp
          echo 'rmean  ' >> correcion.tmp
          echo 'taper   ' >> correcion.tmp
          echo 'TRANS FROM POLEZERO S R3T.paz TO VEL freq 0.01 0.02 40.0 50.0' >> correcion.tmp
          echo 'w over' >> correcion.tmp
          echo 'quit' >> correcion.tmp

	  echo 'r ' '*.sac' > macro.tmp
	  echo 'ch t8  ' >> macro.tmp
          echo 'ch t9  ' >> macro.tmp
          echo 'wh   ' >> macro.tmp
          echo 'quit ' >> macro.tmp
          echo 'listo el macro'

          #Crear la carpeta de DATOS_CORREGIDOS
          cd $p2
          
          mkdir DATOS_CORREGIDOS
          cd DATOS_CORREGIDOS
          p4=$(pwd)
          mkdir $line
          cd $line
          #Carpeta del EVENTO 
          p3=$(pwd)
          

          #Mover los datos crudos a EVENTOS_CORREGIDOS
          cp $p2/DATOS/$line/*sac $p3
          #Mover archivos de tmp-->Carpeta de trabajo
          mv $p1/correcion.tmp $p3
          cp $p1/R3T.paz $p3
          cp $p1/Modelo_Inicial.py $p3
          sac < correcion.tmp
          
	  #Crear los directorios y copiar los componentes
	  mkdir HHZ HHE HHN
          #Copiar archivos de tmp-->Carpeta de trabajo
          mv *HHZ*.sac  $p3/HHZ/
          cp $p1/macro.tmp  $p3/HHZ/
          cp $p1/ventanas_spec.f $p1/sub_fftmasac.f $p3/HHZ/

	  mv *HHE*.sac  $p3/HHE/
          cp $p1/macro.tmp  $p3/HHE/
          cp $p1/ventanas_spec.f $p1/sub_fftmasac.f $p3/HHE/
          

          mv *HHN*.sac  $p3/HHN/
          mv $p1/macro.tmp  $p3/HHN/
          cp $p1/ventanas_spec.f $p1/sub_fftmasac.f $p3/HHN/


          ## Obtener el valor de la Vs en las componentes horizontales
          #Entrar a carpeta HHE
          cd $p3/HHE
          sac < macro.tmp
          taup_setsac -evdpkm -mod iasp91 -ph s-8,S-9  *E.sac
          saclst t8 t9 dist f *E.sac | sort -g -k4 | awk ' {if ($2 == -12345) print $1,$3,$4; else print $1,$2,$4 }' >> times1.tmp

         awk '{print $1 }' times1.tmp > names.tmp
         awk '{print $2 }' times1.tmp > cut.tmp
         awk '{print $2 }' times1.tmp > times.txt
         awk '{print $3 }' times1.tmp > dist.txt
         gfortran ventanas_spec.f sub_fftmasac.f  -L$p1 -lsacio -o salida -no-pie 
         ./salida
         mkdir ESP SEC
         mv *.sec $p3/HHE/SEC/
         mv *.esp  $p3/HHE/ESP/

          #Entrar a carpeta HHN
         cd $p3/HHN
         sac < macro.tmp
         taup_setsac -evdpkm -mod iasp91 -ph s-8,S-9  *N.sac
         saclst t8 t9 dist f *N.sac | sort -g -k4 | awk ' {if ($2 == -12345) print $1,$3,$4; else print $1,$2,$4 }' >> times1.tmp

         awk '{print $1 }' times1.tmp > names.tmp
         awk '{print $2 }' times1.tmp > cut.tmp
         awk '{print $2 }' times1.tmp > times.txt
         awk '{print $3 }' times1.tmp > dist.txt 
	 gfortran ventanas_spec.f sub_fftmasac.f  -L$p1 -lsacio -o salida -no-pie
         ./salida 
         mkdir ESP SEC
         mv *.sec $p3/HHN/SEC/
         mv *.esp  $p3/HHN/ESP/

         # Entrar a carpeta HHZ
         cd $p3/HHZ
         sac < macro.tmp
         taup_setsac -evdpkm -mod iasp91 -ph s-8,S-9  *Z.sac
         saclst t8 t9 dist f *Z.sac | sort -g -k4 | awk ' {if ($2 == -12345) print $1,$3,$4; else print $1,$2,$4 }' >> times1.tmp

         awk '{print $1 }' times1.tmp > names.tmp
         awk '{print $2 }' times1.tmp > cut.tmp
         awk '{print $2 }' times1.tmp > times.txt
         awk '{print $3 }' times1.tmp > dist.txt 
         gfortran ventanas_spec.f sub_fftmasac.f  -L$p1 -lsacio -o salida -no-pie
         ./salida
         mkdir ESP SEC
         mv *.sec $p3/HHZ/SEC/
         mv *.esp  $p3/HHZ/ESP/
         #Regresar a la carpeta de trabajo
         cd $p3
         echo $mag > mag.txt
         python3 Modelo_Inicial.py
         #####################################################
         # Mover los arhivos de H/V de ID--->DATOS_CORREGUIDOS 
         ######################################################
         cp **$line*.txt $p4
       done

######### Regresar a la carpeta EFECTO DE SITIO

cd $p4
cp $p1/HV_Promedio.py $p1/Estaciones_VEOX.txt $p1/ID_ENTRADA.txt $p4
python3 HV_Promedio.py

