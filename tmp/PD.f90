      PROGRAM PD
!    Cambiar el numero de estaciones (nst)
!    Cambiar número de parámetros  (npar) = (2*nev) + (nst*(nobs+1))
!    Ver el archivo de salidonew.es
!                Donde : nst=numero de sitios, nobs=numero de observaciones , nev: número de eventos
!     AGREGAR EL tiempo local ti igual a l numero

      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER i,j,l, iev, is, iob
      INTEGER :: nobs,nev,nst,npar
      DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
      DOUBLE PRECISION, DIMENSION (:,:), allocatable ::dis,ts
      DOUBLE PRECISION, DIMENSION (:), allocatable ::frec,x1

        nev=1
	nobs=1024
	nst = 0 
        npar=33827
      OPEN( 7,FILE="ampl.txt",STATUS='OLD')
      OPEN( 8,FILE="frec.txt",STATUS='OLD')
      OPEN( 9,FILE="dist.txt",STATUS='OLD')
      OPEN(10,FILE="times.txt",STATUS='OLD')
      OPEN(11,FILE="salidanew.res",STATUS='OLD')
      OPEN(12,FILE="pdnew.txt",STATUS='REPLACE')
      OPEN(13,FILE="times_2.txt",STATUS='OLD')



          DO 
    		READ (13,*, END=101) 
    		nst = nst + 1 
	END DO 
	101 CLOSE (13) 
         print*, nst


      allocate (uobs(nobs,nev,nst))
      allocate (frec(nobs))
      allocate(dis(nev,nst))
      allocate(ts(nev,nst)) 
      allocate (direc(nobs,nev,nst))
      allocate (x1(npar))
   
      DO iev=1,nev
        do is=1,nst
          do iob=1,nobs
             READ(7,*) uobs(iob,iev,is)
!quite los comentarios
!             WRITE(11,*) uobs (iob,iev,is)
!             21 FORMAT (F10.7)
          enddo
        enddo
      ENDDO

      DO iobs=1, nobs
         READ(8,*) frec(iobs)
       !  WRITE(12,*) frec(iobs)
       !  22 FORMAT (F10.8)
      ENDDO
      WRITE(*,*) 'listo frec'

      DO j=1,nev
       do i=1,nst
          READ( 9,*) dis(j,i)
          READ(10,*) ts(j,i)
!          WRITE(13,*) dis(j,i)
!          23 FORMAT (F8.4)
!          WRITE(14,*) ts(j,i)
!          24 FORMAT (F7.3)
       enddo
      ENDDO

      DO ipar=1, npar
         READ(11,*) x1(ipar)
        ! WRITE(12,*) frec(iobs)
        ! 22 FORMAT (F10.8)
      ENDDO

      CALL for_prob(uobs,frec,dis,ts,direc,x1,F,npar,nobs,nev,nst)
      write(*,*) F
    
         WRITE(12,22) direc
   !22    FORMAT (F11.8)
 WRITE(*,*) 'TERMINO '
   22    FORMAT (F11.8)


Deallocate(uobs)
deallocate(frec)
deallocate(dis)
deallocate(ts)
deallocate(direc)
deallocate(x1)



END PROGRAM PD
!****************************************************
!****************************************************
!*************** PROBLEMA DIRECTO *******************
!****************************************************
!****************************************************
SUBROUTINE for_prob(uobs,frec,dis,ts,direc,x1,F,npar,nobs,nev,nst)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER i,j,l, iev, is, iob
      INTEGER :: nobs,nev,nst,npar
      DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
      DOUBLE PRECISION, DIMENSION (:,:), allocatable ::dis,ts
      DOUBLE PRECISION, DIMENSION (:), allocatable ::frec,x1

        nev=1
	nobs=1024
	nst = 0 

      CALL fuente(sou,npar,nobs,nev,nst,frec,x1)
      WRITE(*,*) 'listo fuente'
      CALL trayec(path,npar,nobs,nev,nst,frec,dis,ts,x1)
      WRITE(*,*) 'listo trayecto'
     
	 DO i=1,nobs
       		do j=1,nst
          		ind=2*nev+nst+i+(j-1)*nobs
         		site(i,j)=x1(ind)
       		enddo
     	 ENDDO
      DO i=1,nobs
       do j=1,nev
        do l=1,nst
           direc(i,j,l)=sou(i,j)*path(i,j,l)*site(i,l)
        enddo
       enddo
      ENDDO
!!MINIMIZAR LA ECUACION DEL ESPECTRO DE AMPLITUD
      F=0.0d0
      DO i=1,nobs
       do j=1,nev
        do l=1,nst
           F=F+(uobs(i,j,l)-direc(i,j,l))**2.0d0
          ! F=(uobs(i,j,l)-direc(i,j,l))**2.0d0
        enddo
       enddo
      ENDDO
      F=dsqrt(F)

      RETURN
      END
!
!
!*********************************************
!*****		FUENTE   ********************
!*********************************************
 SUBROUTINE FUENTE(fu,npar,nobs,nev,nst,frec,x1)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER i,j,l, iev, is, iob
      INTEGER :: nobs,nev,nst,npar
      DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
      DOUBLE PRECISION, DIMENSION (:,:), allocatable ::dis,ts
      DOUBLE PRECISION, DIMENSION (:), allocatable ::frec,x1

        nev=1
	nobs=1024
	nst = 0 
      pi=4.0d0*datan(1.0d0)
      do i=1,nev
         udesp(i)=x1(i)
         fesq(i)=x1(i+nev)
      enddo
! Modelo de BRUNE 1970
! FUENTE EN DOMINIO DE LA FRECUENCIA BOATWRIGHT(1991)
      DO j=1,nev
       do i=1,nobs
          fu(i,j)=(2.0d0*pi*frec(i)*udesp(j))/(dsqrt(1.0d0+(frec(i)/&
                  fesq(j))**4.0d0))
       enddo
      ENDDO
      RETURN
END

!******************************************************
!******************************************************
!***************** TRAYECTO ***************************
!******************************************************
 SUBROUTINE TRAYEC(t,npar,nobs,nev,nst,frec,dis,ts,x1)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER i,j,l, iev, is, iob
      INTEGER :: nobs,nev,nst,npar
      DOUBLE PRECISION, DIMENSION (:,:,:), allocatable :: uobs,direc
      DOUBLE PRECISION, DIMENSION (:,:), allocatable ::dis,ts
      DOUBLE PRECISION, DIMENSION (:), allocatable ::frec,x1

        nev=1
	nobs=1024
	nst = 0 
        pi=4.0d0*datan(1.0d0)
           do i=1,nst
             tloc(i)=x1(i+2*nev)
           enddo
      do i=1,nobs
         fq(i)=1.0d0/(273.0d0*frec(i)**0.66d0)
      enddo
      DO i=1,nobs
       do j=1,nev
        do l=1,nst
           t(i,j,l)=(1.0d0/dsqrt(dis(j,l)*100.0d0))*&
                    dexp(-pi*frec(i)*(tloc(l)+ts(j,l)*fq(i)))
        enddo
       enddo
      ENDDO

      RETURN
END

