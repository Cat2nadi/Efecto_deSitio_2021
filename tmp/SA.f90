!Universidad Nacional Autonoma de Mexico
!Torres Perez Nadia Nayelly
!Programa de :Simulated Annealing 
!!! NO SE UTILIZA EL NNEPS NI NPUN REVISAR
PROGRAM SIMANNTOMO
	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER i, j, l, iev, is, iob, iter, iter1
	INTEGER NN,nobs,nev,nst,NNEPS,NPUN,uobs, frec, dist, ts, F, direc
	INTEGER N, NS, MAXEVL
	INTEGER  NT, NFCNEV, IER, ISEED1, ISEED2, IPRINT
	INTEGER NACC, NOBDS, NPAR, NACP
	INTEGER
    	LOGICAL  MAX

	DOUBLE PRECISION FSTAR(2087020),T,EPS,RT,FOPT,slm1,slm2
	
	
	DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
    	DOUBLE PRECISION, DIMENSION (:,:), allocatable ::frec,dis,ts
	DOUBLE PRECISION, DIMENSION (:),allocatable :: NACP(:),XOPT(:),D(:),VM(:),LB(:),UB(:),C(:),X1(:),XP(:)
!
	OPEN(78,FILE="frec2.txt",STATUS='REPLACE')
	OPEN(79,FILE="times2.txt",STATUS='REPLACE')

	nst = 0 
	nobs=0
	nev=1!Número de eventos
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
		print*,nst
!
	NN=(2*nev)+(nst*(nobs+1))	


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
!write(*,*)uobs
!stop
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












END PROGRAM SIMANNTOMO
