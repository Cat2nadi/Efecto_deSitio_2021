
#include <iostream>
using namespace std;


int main () {

    cout << "Bienvenido ingrese lo que quiere relizar:\n";
    cout<< "1. Elegir los eventos  \n";
    cout<< "2. Realizar la correción de los datos seleccionados \n";
    cout<< "3. Hacer la inversión de los datos \n";
    int opcion=0;
    cin >> opcion;

    switch(opcion){
          case 1: cout<<"Usted ha seleccionado la opción 1 \n" ;
          break;

          case 2: cout<<"Usted ha seleccionado la opción 2\n" ;
    //system("sh /home/nadia/Escritorio/Tesis_2019/Procesamiento/EFECTO_DE_SITIO/tmp/Correcion_Datos.sh");
            cout<< "La correción del los datos del dcorrectamente\n "<< endl;
            return 0;
          break;
          default: cout << "Usted ha seleccionado una opción inválida, vuelva a intentarlo\n";
    }
}
