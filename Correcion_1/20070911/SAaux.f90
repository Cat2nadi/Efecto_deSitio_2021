!Universidad Nacional Autonoma de Mexico
!Torres Perez Nadia Nayelly
!Programa de :Simulated Annealing 
!!! NO SE UTILIZA EL NNEPS NI NPUN REVISAR
PROGRAM SIMANNTOMO
	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER i, j, l, iev, is, iob, iter, iter1
	INTEGER :: NNEPS,NPUN, F
	INTEGER :: nobs,nev,nst,NNaux
	INTEGER N, NS, MAXEVL
	INTEGER  NT, NFCNEV, IER, ISEED1, ISEED2, IPRINT
!	INTEGER, INTENT(in):: ll
    	LOGICAL  MAX

        PARAMETER (nev=1)
	
	DOUBLE PRECISION FSTAR(2087020),T,EPS,RT,FOPT,slm1,slm2
	DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
        DOUBLE PRECISION, DIMENSION (:,:), allocatable ::dis,ts
	DOUBLE PRECISION, DIMENSION (:),allocatable ::frec
        DOUBLE PRECISION, DIMENSION (:),allocatable ::XOPT,D,VM,LB,UB,C,X1,XP
   	INTEGER, DIMENSION (:),allocatable ::NACP
	OPEN(78,FILE="frec2.txt",STATUS='OLD')
	OPEN(79,FILE="times2.txt",STATUS='OLD')
	
         npar=33827
	nst = 0 
	nobs=0
	!Número de eventos
!	Leer número de estaciones
           DO 
    		READ (79,*, END=101) 
    		nst = nst + 1 
	  END DO 
	101 CLOSE (79) 
		print*,nst
!	Número de observaciones
	DO 
    		READ (78,*, END=102) 
    		nobs = nobs + 1 
	  END DO 
	102 CLOSE (78) 
		print*,nobs
        
	  NNaux=(2)+(nst*(nobs+1))
!
         print*,NNaux

      
      allocate (uobs(nobs,nev,nst))
      allocate (frec(nobs))
      allocate (dis(nev,nst))
      allocate (ts(nev,nst)) 
      allocate (direc(nobs,nev,nst))
      allocate (NACP(NNaux))
      allocate (XOPT(NNaux))
      allocate (D(NNaux))
      allocate (VM(NNaux))
      allocate (LB(NNaux))
      allocate (UB(NNaux))
      allocate (C(NNaux))
      allocate (X1(NNaux))
      allocate (XP(NNaux))

		print*,size(XP)


!**********************************************************************/
!     LECTURA DE DATOS OBSERVADOS
!**********************************************************************/
      OPEN( 7,FILE="ampl.txt",STATUS='OLD')
      OPEN( 8,FILE="frec.txt",STATUS='OLD')
      OPEN( 9,FILE="dist.txt",STATUS='OLD')
      OPEN(10,FILE="times.txt",STATUS='OLD')
!*****************************************
! Lectura de amplitud
!*****************************************
      DO iev=1,nev
        do is=1,nst
          do iob=1,nobs
             READ( 7,*) uobs(iob,iev,is)
          enddo
        enddo
      ENDDO
!*****************************************
! Lectura de frecuencias
!*****************************************
      DO iobs=1, nobs
         READ( 8,*) frec(iobs)
      ENDDO
!*****************************************
! Lectura de distancias y tiempos por estaciones
!*****************************************
      DO j=1,nev
       do i=1,nst
          READ( 9,*) dis(j,i)
          READ(10,*) ts(j,i)
          
       enddo
      ENDDO
      CLOSE( 7)
      CLOSE( 8)
      CLOSE( 9)
      CLOSE(10)

!*********************************************************
!*********************************************************
!******** LEER LOS PARAMETROS(simann.dat)*****************
!*********MODELO INICIAL PYTHON **************************
!*********************************************************
      OPEN (11,FILE='simann.dat',STATUS='OLD')  
      OPEN (12,FILE='SA.res',STATUS='REPLACE')
      OPEN (13,FILE='Modelo_Inicial.txt',STATUS='OLD')
      OPEN (29,FILE='OBS_AUX.txt',STATUS='REPLACE')
      OPEN (28,FILE='CAL_AUX.txt',STATUS='REPLACE')
      OPEN (30,FILE='error_AUX.txt',STATUS='REPLACE')
      OPEN (31,FILE='EFECTO_AUX.txt',STATUS='REPLACE')    
      OPEN (32,FILE='TRAYECTO_AUX.txt',STATUS='REPLACE') 
      OPEN (33,FILE='FUENTE_AUX.txt',STATUS='REPLACE')       
   

      iter=0
         READ(11,*) NEPS
         READ(11,*) EPS
         READ(11,*) N
         READ(11,*) NT
         READ(11,*) NS
         READ(11,*) T
         READ(11,*) RT
         READ(11,*) MAXEVL
         close (11)
      DO i=1,NNaux
         READ(13,*) x1(i)

         LB(i) = 0.0d0!Limite inferior 
         D(i) = 1.0d0
!        UB(I) = LB(I)+D(I)
         UB(i) = 2.0d0*x1(i)
	 VM(i) = D(i) 
         C(i) = 2.0d0
! VM(I)=Paso ajustable para cada parametro, en funcion de los modelos aceptados       
! C(I)=Factor de ajuste del paso para el parametro i-esimo.
      ENDDO

         slm1=0.0d0

      close(13)

  KG=1
      CALL for_prob(uobs,frec,dis,ts,direc,x1,KG,F,NNaux,nobs,nev,nst)
     
      WRITE(*,*) 'ERROR',F, kg


END PROGRAM SIMANNTOMO











!****************************************************************************************************
!		PROBLEMA DIRECTO
!*****************************************************************************************************
 	SUBROUTINE for_prob(uobs,frec,dis,ts,direc,x1,KG,F,NNaux,nobs,nev,nst)
     	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER NNaux, nobs, nev, nst, ind, i, j, k, l, KG
     	DOUBLE PRECISION F
        DIMENSION uobs(nobs,nev,nst),frec(nobs),dis(nev,nst),ts(nev,nst)
        DIMENSION direc(nobs,nev,nst), x1(NNaux)
        DIMENSION sou(nobs,nev), path(nobs,nev,nst), site(nobs,nst)
       CHARACTER(LEN=30) :: Format

        Format = "(5X, I5.2, F10.3, A, ES14.7)"



      CALL fuente(sou,NNaux,nobs,nev,nst,frec,x1)
      WRITE(*,*) 'listo fuente'
 
      CALL trayec(path,NNaux,nobs,nev,nst,frec,dis,ts,x1)
      !WRITE(*,*) 'listo trayecto',path
! Leer del modelo inicial Efecto de sitio (site)
      DO i=1,nobs
       do j=1,nst
          ind=2*nev+nst+i+(j-1)*nobs
          !write(*,*)ind
          site(i,j)=x1(ind)
         
       enddo
      ENDDO
! Formulación de Field y Jacob(1995) para el calculo del espectro de amplitud A=SOURCE*PATH*SITE_EFECT

	DO i=1,nobs
		do j=1,nev
			do l=1,nst
				direc(i,j,l)=sou(i,j)*path(i,j,l)*site(i,l)
				write(29,*)uobs(i,j,l)
                                write(28,*)direc(i,j,l)
		                write(31,*)site(i,l)      
				write(32,*)path(i,j,l)  
				write(33,*)sou(i,j)           
        		enddo
       		enddo
         ENDDO
	close(29)
	close(28)
	close(31)
	close(32)
	close(33)
!write(*,*)direc

!************************************************************************
!Calculo del error
!***********************************************************************
      F=0.0d0
	DO i=1,nobs
		do j=1,nev
			do l=1,nst
				F=F+(uobs(i,j,l) - direc(i,j,l))**2.0d0
	!			F=(uobs(i,j,l) - direc(i,j,l))**2.0d0
				write(30,*)F
			enddo
		enddo
	ENDDO
	F=dsqrt(F)
!	write(81,*)F
	write(*,*) 'Termino calculo del problema directo'
        close (30)
      RETURN
END

!***********************************************************************
!                       FUENTE
!***********************************************************************
      	SUBROUTINE FUENTE(fu,NNaux,nobs,nev,nst,frec,x1)
        IMPLICIT REAL*8 (A-H,O-Z)
         INTEGER NNaux, nobs, nev, nst, i, j, l
         DIMENSION fu(nobs,nev), frec(nobs),x1(NNaux)
         DIMENSION udesp(nev), fesq(nev)

      pi=4.0d0*datan(1.0d0)
!******************************************************
! Lectura de los parametros de la fuente del archivo modelo inicial:
! Nivel medio espectra (Omega) y la Frecuencia esqina
!*****************************************************
      do i=1,nev
         udesp(i)=x1(i)!Nivel medio espectral 
         fesq(i)=x1(i+nev)
!	write(*,*)udesp(i),fesq(i)
      enddo
! **********************************************************************************
! MODELO TEORICO DE LA FUENTE 
! HASKELL(1966), BRUNE(1970), BOATWRIGHT(1980)
! ********************************************************************************

      DO j=1,nev
       do i=1,nobs
          fu(i,j)=(2.0d0*pi*frec(i)*udesp(j))/(dsqrt(1.0d0+(frec(i)/&
                  fesq(j))**4.0d0))
!		write(78,*)fu(i,j)
       enddo
      ENDDO
      RETURN
      END

!***************************************************************************************
! 			     TRAYECTO
!***************************************************************************************
! FACTOR DE CALIDAD Q=273*f^0.66 (se puede cambar)
!***************************************************************************************
	SUBROUTINE TRAYEC(tr,NNaux,nobs,nev,nst,frec,dis,ts,x1)
      	IMPLICIT REAL*8 (A-H,O-Z)
      	INTEGER NNaux, nobs, nev, nst, i, j, l
     	 DIMENSION tr(nobs,nev,nst), frec(nobs), dis(nev,nst), ts(nev,nst)
     	 DIMENSION x1(NNaux), fq(nobs), tloc(nst)
! Atenuacción local t* del ModeloInicial 
    	  pi=4.0d0*datan(1.0d0)
     	 do i=1,nst
         	tloc(i)=x1(i+2*nev)
         	!write(*,*)tloc(i)
      	enddo

!Factor de calidad Q (Garcia) 273*FREC^0.066
      	do i=1,nobs
         fq(i)=1.0d0/(273.0d0*frec(i)**0.66d0)
     	 enddo

! Trayecto 
     	 DO i=1,nobs
       	do j=1,nev
       	 do l=1,nst
           	tr(i,j,l)=(1.0d0/dsqrt(dis(j,l)*100.0d0))*&
                    dexp(-pi*frec(i)*(tloc(l)+ts(j,l)*fq(i)))
!		write(79,*)tr(i,j,l)
       	 enddo
       	enddo
      	ENDDO
      	RETURN
	 END




