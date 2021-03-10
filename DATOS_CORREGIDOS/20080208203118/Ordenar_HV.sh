#!bin/bash
#Programa que ordena
#rm Estaciones.txt p.txt
#ls ????.txt | awk '{print $0}'>> Estaciones.txt

while IFS= read -r line
do
  echo "$line"
  awk '{print $2}' $line >> p.txt
done < Estaciones.txt
