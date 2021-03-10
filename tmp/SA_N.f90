!     Last change:  CO   28 May 2003    1:08 pm
      PROGRAM SIMANNTOMO
!     Se debe de modificar el NN 
!     Número de estaciones nst
!     Número de eventos
!     NN = (2*nev) + (nst*(nobs+1))
      
	IMPLICIT REAL*8 (A-H,O-Z)
	PARAMETER (NN=5127)
	PARAMETER (nobs=1024)!1204
	PARAMETER (nev=1)
	PARAMETER (nst=5)
	PARAMETER (NNEPS = 2, NPUN = 1024)
      	INTEGER i, j, l, iev, is, iob, iter, iter1
	
	DOUBLE PRECISION uobs, frec, dist, ts, F, direc
	DOUBLE PRECISION XOPT(NN),D(NN),VM(NN),LB(NN),UB(NN),C(NN),X1(NN)
	DOUBLE PRECISION FSTAR(2087020),XP(NN),T,EPS,RT,FOPT,slm1,slm2
	
	DIMENSION uobs(nobs,nev,nst),frec(nobs),dis(nev,nst),ts(nev,nst)
	DIMENSION direc(nobs,nev,nst)
	DIMENSION NACP(NN)
      
      
	INTEGER N, NS, MAXEVL
	INTEGER NT, NFCNEV, IER, ISEED1, ISEED2, IPRINT
	INTEGER NACC, NOBDS, NPAR, NACP
    	LOGICAL  MAX

!	OPEN(78,FILE="fuente.txt",STATUS='REPLACE')
!	OPEN(79,FILE="trayecto.txt",STATUS='REPLACE')
!	OPEN(80,FILE="AMP_MD.txt",STATUS='REPLACE')
!	OPEN(81,FILE="ERROR.txt",STATUS='REPLACE')
	OPEN(51,FILE="parametros.txt",STATUS='REPLACE')

!***********************************************************
!  Lee desde un archivo de datos los parametros necesarios.
!  NEPS=No. de Iteraciones,Eps=Tolerancia,N=No. de Parametros
!  NT=No. de Iteraciones  antes de la reduccion de Temperatura
!  NS=No. de Iteraciones antes de ajustar el paso (VM)
!  T=Temperatura inicial.
!  RT=Factor de Reduccion de la Temperatura cada NT*NS*N
!  MAXEVL=No. Maximo de evaluaciones del problema directo.
!  VM(I)=Paso ajustable para cada parametro, en funcion de los modelos
!        aceptados.
!  LB(I)=Limite inferior para el parametro i-esimo.
!  UB(I)=Limite superior para el parametro i-esimo.
!  C(I)=Factor de ajuste del paso para el parametro i-esimo.
!  X1(I)=Modelo inicial.
!**********************************************************




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
!**********************************************************************/

      OPEN (11,FILE='simann.dat',STATUS='OLD')  
!      OPEN (12,FILE='salidanew.res',STATUS='REPLACE')
      OPEN (12,FILE='SA.res',STATUS='REPLACE')
      OPEN (15,FILE='modelinicial.dat',STATUS='OLD')
   

      iter=0
         READ(11,*) NEPS
         READ(11,*) EPS
         READ(11,*) N
         READ(11,*) NT
         READ(11,*) NS
         READ(11,*) T
         READ(11,*) RT
         READ(11,*) MAXEVL

      DO I=1,NN
         READ(15,*)x1(i)
!***********************************************
!Limites:  Superior(UB)  Inferior (LB)
!*************************************************
         lb(i) = 0.0d0
          d(i) = 1.0d0
!        UB(I) = LB(I)+D(I)
         UB(I) = 2.0d0*x1(i)
!	write(*,*)UB(I)

! VM(I)=Paso ajustable para cada parametro, en funcion de los modelos
!        aceptados.
         VM(I) = D(I) 
         C(I) = 2.0d0
       
! C(I)=Factor de ajuste del paso para el parametro i-esimo.
      ENDDO

         slm1=0.0d0

      close(15)

!OJOOOOOOOOOOOOO
!! UNICAMENTE LLEGA HASTA 1204X28 HAY Q VER EL REMUESTREO 

!******************************************************************
! CÁLCULO DEL PROBLEMA DIRECTO 
!*******************************************************************
      KG=1
      CALL for_prob(uobs,frec,dis,ts,direc,x1,KG,F,NN,nobs,nev,nst)

      WRITE(*,*) 'VALOR DE F',F, kg

!***********************************************************************
!    Establecer los parámetros de entrada.
!***********************************************************************
      MAX = .FALSE.
! COndicional en la generacion de numero aleatorios
      ISEED1 = 1
      ISEED2 = 2
      IPRINT = 1

      WRITE(*,1000)N,MAX,T,RT,EPS,NS,NT,NEPS,MAXEVL,IPRINT,ISEED1,ISEED2

      WRITE(*,*) '*END OF DRIVER OUTPUT* *BEFORE CALL TO SA*'
! !$OMP PARALLEL SECTION
      CALL SA(uobs,N,X1,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,&
      ISEED1,ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,FSTAR,XP,&
      NACP,iter,cij,frec,dis,ts,direc,KG,F,NN,nobs,nev,nst)
 !!$OMP END PARALLEL SECTION
write(*,*)'BONITA'

      WRITE(*,'(/,''  ****   Resultados después de SA   ****   '')')
      
      CALL PRTVEC(XOPT,N,'SOLUTION')
      CALL PRTVEC(VM,N,'FINAL STEP LENGTH')
      WRITE(*,1001) FOPT, NFCNEV, NACC, NOBDS, T, IER
      DO i=1,NN
         WRITE(12,*) XOPT(i)
      ENDDO

1000  FORMAT(/,' SIMULATED ANNEALING EXAMPLE',/,/,&
     ' Número de Parámetros: ',I10,'   MAXIMIZACIÓN: ',L5,/&
     'TEMPERATURA INICIAL: ', G9.2, '   FACTOR DE REDUCCIÓN DE TEMP(RT): ',G9.2, '   TOLERANCIA: ',G9.2,/,&
     ' NO. DE ITERACIONES ANTES DE AJUSTAR EL PASO: ',I3, '   NO. ITERACIONES ANTES DE AJUSTAR TEMPERATURA: ',&
      I2, '  No. Iteraciones: ',I8,&
     /,' MAXEVL: ',I10, '  IPRINT: ',I1, '  ISEED1: ',I4,' ISEED2: ',I4)
1001  FORMAT(/,' VALOR ÓPTIMO DE LA FUNCIÓNES: ',G20.13&
            /,' NÚMERO DE EVALUACIONES DE FUNCIONES:     ',I10,&
            /,' NÚMERO DE EVALUACIÓN ACEPTABLE:     ',I10,&
            /,' NÚMERO DE EVALUACIONES FUERA DE LOS LIMITES: ',I10,&
            /,' TEMPERATURA FINAL: ', G20.13,'  IER: ', I3)
      close(51)
      close(11)
      close(12)
      
      END PROGRAM SIMANNTOMO
!*************************************************************************************************************
!
!                		  SUBRUTINAS
!
!*************************************************************************************************************


!*************************************************************************************************************
!       SIMULETED ANNEALING 
!*************************************************************************************************************
      SUBROUTINE SA(uobs,N,X1,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,&
      IPRINT,ISEED1,ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,FSTAR,&
      XP,NACP,iter,cij,frec,dis,ts,direc,KG,F,NN,nobs,nev,nst)

	IMPLICIT REAL*8 (A-H,O-Z)
   
	DIMENSION uobs(nobs,nev,nst),frec(nobs),dis(nev,nst),ts(nev,nst)
	DIMENSION direc(nobs,nev,nst)
	DIMENSION sou(nobs,nev), path(nobs,nev,nst),site(nobs,nst)
	DIMENSION NACP(NN)
	
	DOUBLE PRECISION XOPT(NN),VM(NN),LB(NN),UB(NN),C(NN),X1(NN)
	DOUBLE PRECISION FSTAR(2087020),XP(NN),T,EPS,RT,FOPT,xp11(NN)
	DOUBLE PRECISION slm1,slm2

	INTEGER NN, nobs, nev, nst, i, j, l, iter, KG
	INTEGER NACP
        INTEGER N, NS, NT, NEPS, NACC, MAXEVL, IPRINT
        INTEGER NOBDS, IER, NFCNEV, ISEED1, ISEED2
     
	LOGICAL  MAX
	
	CHARACTER(len=25)::name

!     Variables Internas.
	DOUBLE PRECISION  FP, P, PP, RATIO, F
	DOUBLE PRECISION  EXPREP
	INTEGER  NUP, NDOWN, NREJ, NNEW, LNOBDS, H, M

	LOGICAL  QUIT
	REAL  RANMAR

      
      
write(*,*)'Entró a simulated annealing'

   
!     Initialize the random number generator RANMAR.
      CALL RMARIN(ISEED1,ISEED2)
 write(*,*)'GENERA NUMERO ALEATORIO',ISEED2

!     Set initial values.
      NACC = 0
      NOBDS = 0
      NFCNEV = 0
      IER = 99
!XOPT ASGINA TODA LA INFORMACION DEL MODELO INICIAL
      DO  I = 1, N
         XOPT(I) = X1(I)
         NACP(I) = 0
      ENDDO

      DO I = 1, NEPS
         FSTAR(I) = 1.0D+20
      ENDDO

!     If the initial temperature is not positive, notify the user and
!     return to the calling routine.
      IF (T .LE. 0.0) THEN
      WRITE(*,'(/,''THE INITIAL TEMP IS NOT POSITIV''/,''RESET T. ''/)')
         IER = 3
         RETURN
      END IF

!     If the initial value is out of bounds, notify the user and return
!     to the calling routine.
      DO  I = 1, N
         IF ((X1(I) .GT. UB(I)) .OR. (X1(I) .LT. LB(I))) THEN
            CALL PRT1
            IER = 2
            RETURN
         END IF
      ENDDO

!     Evaluate the function with input X1 and return value as F.
!     CALL DIREC(N,X,F,slm1,slm2)
      iter=iter+1
      write(*,*) iter
      write(*,*)'TOODO CHIDO HASTA AQUI'

! *********************************************************************************
! *********************************************************************************
! 			CALCULO DEL PROBLEMA DIRECTO
! *********************************************************************************
!**********************************************************************************
      CALL for_prob(uobs,frec,dis,ts,direc,x1,KG,F,NN,nobs,nev,nst)


!! Si la función se va a minimizar, cambie el signo de la función.
! Tenga en cuenta que todas las salidas intermedias y finales cambian el signo de vuelta
! para eliminar cualquier confusión posible para el usuario
      IF(.NOT. MAX) F = -F
        NFCNEV = NFCNEV + 1
        FOPT = F
        FSTAR(1) = F
      IF(IPRINT .GE. 1) CALL PRT2(MAX,N,X1,F)

!     Start the main loop. Note that it terminates if (i) the algorithm
!     succesfully optimizes the function or (ii) there are too many
!     function evaluations (more than MAXEVL).
100   NUP = 0
      NREJ = 0
      NNEW = 0
      NDOWN = 0
      LNOBDS = 0

      DO M = 1, NT
         DO J = 1, NS
            DO H = 1, N

!  Generate XP, the trial value of X. Note use of VM to choose XP.
               DO I = 1, N
                  IF (I .EQ. H) THEN
                     XP(I) = X1(I) + (dble(RANMAR()*2.0d0-1.d0)) * VM(I)
                  ELSE
                     XP(I) = X1(I)
                  END IF

!  If XP is out of bounds, select a point in bounds for the trial.
                  IF((XP(I) .LT. LB(I)) .OR. (XP(I) .GT. UB(I))) THEN
                    XP(I) = LB(I) + (UB(I) - LB(I))*dble(RANMAR())
                    LNOBDS = LNOBDS + 1
                    NOBDS = NOBDS + 1
                    IF(IPRINT .GE. 3) CALL PRT3(MAX,N,XP,X1,FP,F)
                  END IF
               ENDDO

!  Evaluate the function with the trial point XP and return as FP.
!              CALL DIREC(N,XP,FP,slm1,slm2)
              CALL for_prob(uobs,frec,dis,ts,direc,xp,KG,fp,NN,nobs,nev,nst)

!!!!!

!SE EVITA CALCULAR EL PROBLEMA DIRECTO SI LA PERMITIVIDAD VARIA UN % < TRESH
!                DO LL=1,N
!                  IF(DABS(XP(LL)-XP11(LL)).LT.(1.d-6))then
!                   NBAS=NBAS+1
!                  ENDIF
!                ENDDO
!                  IF (NBAS.EQ.N) THEN
!                  fp=-fp
!                  nahorro = nahorro + 1
!         write(*,*)'No se han calculado  ',nahorro,' problemas directos'
!                  do ll=1,N
!                      xp(ll)=xp11(ll)
!                  enddo
!                  goto 3020
!                  ENDIF
!                  iter=iter+1
!           CALL for_prob(uobs,frec,dis,ts,direc,xp,KG,Fp,NN,nobs,nev,nst)
!        if(mod(iter,NT*NS*N) == 0)then
!      write(name,222) iter
! 222      format('Cap',i6.6)
!      open(unit=51,file=name,status='unknown')
!          do i=1,N
!          write(51,*)xopt(i)
!          enddo
!      close(51)
!        endif

!3020           continue
               nbas=0
!              write(*,*)'nbas   ',nbas
               DO LL=1,N
                  XP11(LL)=XP(LL)
               ENDDO
!!!!!

               IF(.NOT. MAX) FP = -FP
               NFCNEV = NFCNEV + 1
               IF(IPRINT .GE. 3) CALL PRT4(MAX,N,XP,X1,FP,F)

!  If too many function evaluations occur, terminate the algorithm.
               IF(NFCNEV .GE. MAXEVL) THEN
                  CALL PRT5
                  IF (.NOT. MAX) FOPT = -FOPT
                  IER = 1
                  RETURN
               END IF

!  Accept the new point if the function value increases.
               IF(FP .GE. F) THEN
                  IF(IPRINT .GE. 3) THEN
                     WRITE(*,'(''  POINT ACCEPTED'')')
                  END IF
                  DO I = 1, N
                     X1(I) = XP(I)
                  ENDDO
                  F = FP
                  NACC = NACC + 1
                  NACP(H) = NACP(H) + 1
                  NUP = NUP + 1

!  If greater than any other point, record as new optimum.
                  IF (FP .GT. FOPT) THEN
                     IF(IPRINT .GE. 3) THEN
                        WRITE(*,'(''  NEW OPTIMUM'')')
                     END IF
                     DO I = 1, N
                        XOPT(I) = XP(I)
                     ENDDO
                     FOPT = FP
                     NNEW = NNEW + 1
                  END IF

!  If the point is lower, use the Metropolis criteria to decide on
!  acceptance or rejection.
               ELSE
                  P = dble(EXPREP((FP - F)/T))
                  PP = dble(RANMAR())
                  IF (PP .LT. P) THEN
                     IF(IPRINT .GE. 3) CALL PRT6(MAX)
                     DO I = 1, N
                        X1(I) = XP(I)
                     ENDDO
                     F = FP
                     NACC = NACC + 1
                     NACP(H) = NACP(H) + 1
                     NDOWN = NDOWN + 1
                  ELSE
                     NREJ = NREJ + 1
                     IF(IPRINT .GE. 3) CALL PRT7(MAX)
                  END IF
               END IF

            ENDDO !TERCER DO
         ENDDO !SEGUNDO DO

! Adjust VM so that approximately half of all evaluations are accepted.
         DO I = 1, N
            RATIO = DBLE(NACP(I)) /DBLE(NS)
            IF (RATIO .GT. .6) THEN
               VM(I) = VM(I)*(1.d0 + C(I)*(RATIO - .6d0)/.4d0)
            ELSE IF (RATIO .LT. .4d0) THEN
               VM(I) = VM(I)/(1.d0 + C(I)*((.4d0 - RATIO)/.4d0))
            END IF
            IF (VM(I) .GT. (UB(I)-LB(I))) THEN
               VM(I) = UB(I) - LB(I)
            END IF
         ENDDO

         IF(IPRINT .GE. 2) THEN
            CALL PRT8(N,VM,XOPT,X1)
         END IF

         DO I = 1, N
            NACP(I) = 0
         ENDDO

      ENDDO !PRIMER DO






      IF(IPRINT .GE. 1) THEN
         CALL PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)
      END IF

!  Check termination criteria.
      QUIT = .FALSE.
      FSTAR(1) = F
!     IF ((FOPT - FSTAR(1)) .LE. EPS) QUIT = .TRUE.
      DO 410, I = 1, NEPS
         IF (DABS(F - FSTAR(I)) .GT. EPS) QUIT = .FALSE.
410   CONTINUE
!     IF (FOPT.LE. EPS) QUIT = .TRUE.

!  Terminate SA if appropriate.
      IF (QUIT) THEN
         DO 420, I = 1, N
            X1(I) = XOPT(I)
420      CONTINUE
         IER = 0
         IF (.NOT. MAX) FOPT = -FOPT
         IF(IPRINT .GE. 1) CALL PRT10
         RETURN
      END IF

!  If termination criteria is not met, prepare for another loop.
      T = RT*T
WRITE(*,*)T
      DO 430, I = NEPS, 2, -1
         FSTAR(I) = FSTAR(I-1)
430   CONTINUE
      F = FOPT
      DO 440, I = 1, N
         X1(I) = XOPT(I)
440   CONTINUE


!      write(*,*)'file created'

!  Loop again.
      GO TO 100

      END

      FUNCTION  EXPREP(RDUM)
!  This function replaces exp to avoid under- and overflows and is
!  designed for IBM 370 type machines. It may be necessary to modify
!  it for other machines. Note that the maximum and minimum values of
!  EXPREP are such that they has no effect on the algorithm.

      DOUBLE PRECISION  RDUM, EXPREP

      IF (RDUM .GT. 174.d0) THEN
         EXPREP = 3.69D+75
      ELSE IF (RDUM .LT. -180.d0) THEN
         EXPREP = 0.0d0
      ELSE
         EXPREP = DEXP(RDUM)
      END IF

      RETURN
      END
!******************************************************************************************************************++
! Genera Numero aleatorios 
!*****************************************************************************************************************+
      subroutine RMARIN(IJ,KL)
!  This subroutine and the next function generate random numbers. See
!  the comments for SA for more information. The only changes from the
!  orginal code is that (1) the test to make sure that RMARIN runs first
!  was taken out since SA assures that this is done (this test didn't
!  compile under IBM's VS Fortran) and (2) typing ivec as integer was
!  taken out since ivec isn't used. With these exceptions, all following
!  lines are original.

! This is the initialization routine for the random number generator
!     RANMAR()
! NOTE: The seed variables can have values between:    0 <= IJ <= 31328
!                                                      0 <= KL <= 30081
      	INTEGER  ISEED1, ISEED2
	REAL	 U(97), C, CD, CM
	INTEGER  I97, J97
!common:define un bloque de almacenamiento de memoria principal para que diferentes unidades de programa puedan compartir los mismos !datos sin usar argumentos.
      common /raset1/ U, C, CD, CM, I97, J97
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .OR.&
          KL .lt. 0  .or.  KL .gt. 30081 ) then
          print '(A)',&
      'The first random seed must have a value between 0 and 31328'
       print '(A)','The second seed must have a value betwen 0 and30081'
          
      endif

      		i = mod(IJ/177, 177) + 2
      		j = mod(IJ    , 177) + 2
      		k = mod(KL/169, 178) + 1
      		l = mod(KL,     169)
      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      !J97 = 97
      return


      end



   function ranmar()
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
         uni = U(I97) - U(J97)

         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         RANMAR = uni

      return
      END

      SUBROUTINE PRT1
!  This subroutine prints intermediate output, as does PRT2 through
!  PRT10. Note that if SA is minimizing the function, the sign of the
!  function value and the directions (up/down) are reversed in all
!  output to correspond with the actual function optimization. This
!  correction is because SA was written to maximize functions and
!  it minimizes by maximizing the negative a function.

      WRITE(*,'(/,''THE STARTING VALUE IS OUTSIDE THE BOUNDS ''/)')

      RETURN
      END

      SUBROUTINE PRT2(MAX,N,X1,F)

      DOUBLE PRECISION  X1(*), F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
!     CALL PRTVEC(X1,N,'INITIAL X')
      IF (MAX) THEN
         WRITE(*,'(''  INITIAL F: '',/, G25.18)') F
      ELSE
         WRITE(*,'(''  INITIAL F: '',/, G25.18)') -F
      END IF

      RETURN
      END

      SUBROUTINE PRT3(MAX,N,XP,X1,FP,F)

      DOUBLE PRECISION  XP(*), X1(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
!     CALL PRTVEC(X1,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(*,'(''  CURRENT F: '',G25.18)') F
      ELSE
         WRITE(*,'(''  CURRENT F: '',G25.18)') -F
      END IF
!     CALL PRTVEC(XP,N,'TRIAL X')
      WRITE(*,'(''  POINT REJECTED SINCE OUT OF BOUNDS'')')

      RETURN
      END

      SUBROUTINE PRT4(MAX,N,XP,X1,FP,F)

      DOUBLE PRECISION  XP(*), X1(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(*,'(''  '')')
!     CALL PRTVEC(X1,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(*,'(''  CURRENT F: '',G25.18)') F
!        CALL PRTVEC(XP,N,'TRIAL X')
         WRITE(*,'(''  RESULTING F: '',G25.18)') FP
      ELSE
         WRITE(*,'(''  CURRENT F: '',G25.18)') -F
!        CALL PRTVEC(XP,N,'TRIAL X')
         WRITE(*,'(''  RESULTING F: '',G25.18)') -FP
      END IF

      RETURN
      END

      SUBROUTINE PRT5

      WRITE(*,'(/,''TOO MANY EVAL''/,''THESE RESULTS ARE POOR.'',/)')

      RETURN
      END

      SUBROUTINE PRT6(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(*,'(''  THOUGH LOWER, POINT ACCEPTED'')')
      ELSE
         WRITE(*,'(''  THOUGH HIGHER, POINT ACCEPTED'')')
      END IF

      RETURN
      END

      SUBROUTINE PRT7(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(*,'(''  LOWER POINT REJECTED'')')
      ELSE
         WRITE(*,'(''  HIGHER POINT REJECTED'')')
      END IF

      RETURN
      END

      SUBROUTINE PRT8(N,VM,XOPT,X1)

      DOUBLE PRECISION  VM(*), XOPT(*), X1(*)
      INTEGER  N

      WRITE(*,'(/,''INTERMEDIATE RESULTS AFTER STEP ADJUSTMENT'',/)')
!     CALL PRTVEC(VM,N,'NEW STEP LENGTH (VM)')
!     CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X')
!     CALL PRTVEC(X1,N,'CURRENT X')
      WRITE(*,'('' '')')

      RETURN
      END

      SUBROUTINE PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)

      DOUBLE PRECISION  XOPT(*), VM(*), T, FOPT
      INTEGER  N, NUP, NDOWN, NREJ, LNOBDS, NNEW, TOTMOV
      LOGICAL  MAX

      TOTMOV = NUP + NDOWN + NREJ

      WRITE(*,'(/,''INTERMEDIATE RESULTS BEFORE NEXT TEMP RED'',/)')
      WRITE(*,'(''  CURRENT TEMPERATURE:            '',G12.5)') T
      IF (MAX) THEN
         WRITE(*,'(''  MAX FUNCTION VALUE SO FAR:  '',G25.18)') FOPT
         WRITE(*,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(*,'(''     UPHILL:                  '',I8)') NUP
         WRITE(*,'(''     ACCEPTED DOWNHILL:       '',I8)') NDOWN
         WRITE(*,'(''     REJECTED DOWNHILL:       '',I8)') NREJ
         WRITE(*,'(''  OUT OF BOUNDS TRIALS:       '',I8)') LNOBDS
         WRITE(*,'(''  NEW MAXIMA THIS TEMPERATURE:'',I8)') NNEW
      ELSE
         WRITE(*,'(''  MIN FUNCTION VALUE SO FAR:  '',G25.18)') -FOPT
         WRITE(*,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(*,'(''     DOWNHILL:                '',I8)')  NUP
         WRITE(*,'(''     ACCEPTED UPHILL:         '',I8)')  NDOWN
         WRITE(*,'(''     REJECTED UPHILL:         '',I8)')  NREJ
         WRITE(*,'(''  TRIALS OUT OF BOUNDS:       '',I8)')  LNOBDS
         WRITE(*,'(''  NEW MINIMA THIS TEMPERATURE:'',I8)')  NNEW
      END IF
!     CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X')
!     CALL PRTVEC(VM,N,'STEP LENGTH (VM)')
      WRITE(*,'('' '')')

      RETURN
      END

      SUBROUTINE PRT10

      WRITE(*,'(/,''  SA ACHIEVED TERMINATION CRITERIA. IER = 0. '',/)')

      RETURN
      END

      SUBROUTINE PRTVEC(VECTOR,NCOLS,NAME)
!  This subroutine prints the double precision vector named VECTOR.
!  Elements 1 thru NCOLS will be printed. NAME is a character variable
!  that describes VECTOR. Note that if NAME is given in the call to
!  PRTVEC, it must be enclosed in quotes. If there are more than 10
!  elements in VECTOR, 10 elements will be printed on each line.

      INTEGER NCOLS
      DOUBLE PRECISION VECTOR(NCOLS)
      CHARACTER *(*) NAME

      WRITE(*,1001) NAME

      IF (NCOLS .GT. 10) THEN
         LINES = INT(NCOLS/10.)

         DO 100, I = 1, LINES
            LL = 10*(I - 1)
            WRITE(*,1000) (VECTOR(J),J = 1+LL, 10+LL)
  100    CONTINUE

         WRITE(*,1000) (VECTOR(J),J = 11+LL, NCOLS)
      ELSE
         WRITE(*,1000) (VECTOR(J),J = 1, NCOLS)
      END IF

 1000 FORMAT( 10(G12.5,1X))
 1001 FORMAT(/,25X,A)

      RETURN
      END
!****************************************************************************************************
!		PROBLEMA DIRECTO
!*****************************************************************************************************
      SUBROUTINE for_prob(uobs,frec,dis,ts,direc,x1,KG,F,NN,nobs,nev,&
      nst)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NN, nobs, nev, nst, ind, i, j, k, l, KG
      DOUBLE PRECISION F
      DIMENSION uobs(nobs,nev,nst),frec(nobs),dis(nev,nst),ts(nev,nst)
      DIMENSION direc(nobs,nev,nst), x1(NN)
      DIMENSION sou(nobs,nev), path(nobs,nev,nst), site(nobs,nst)

      CALL fuente(sou,NN,nobs,nev,nst,frec,x1)
      !WRITE(*,/n) 'listo fuente',sou
      CALL trayec(path,NN,nobs,nev,nst,frec,dis,ts,x1)
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
!				write(80,*)direc(i,j,l)
        		enddo
       		enddo
         ENDDO

!************************************************************************
!Calculo del error
!***********************************************************************
      F=0.0d0
	DO i=1,nobs
		do j=1,nev
			do l=1,nst
				F=F+(uobs(i,j,l) - direc(i,j,l))**2.0d0
	!			F=(uobs(i,j,l) - direc(i,j,l))**2.0d0
			enddo
		enddo
	ENDDO
	F=dsqrt(F)
!	write(81,*)F
	write(*,*) 'Termino calculo del problema directo'

      RETURN
      END
!***********************************************************************
!                       FUENTE
!***********************************************************************
      SUBROUTINE FUENTE(fu,NN,nobs,nev,nst,frec,x1)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NN, nobs, nev, nst, i, j, l
      DIMENSION fu(nobs,nev), frec(nobs),x1(NN)
      DIMENSION udesp(nev), fesq(nev)

      pi=4.0d0*datan(1.0d0)
!******************************************************
! Lectura de los parametros de la fuente del archivo modelo inicial:
! Nivel medio espectra (Omega) y la Frecuencia esqina
!*****************************************************
      do i=1,nev
         udesp(i)=x1(i)!Nivel medio espectral 
         fesq(i)=x1(i+nev)
	write(*,*)udesp(i),fesq(i)
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
      SUBROUTINE TRAYEC(tr,NN,nobs,nev,nst,frec,dis,ts,x1)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NN, nobs, nev, nst, i, j, l
      DIMENSION tr(nobs,nev,nst), frec(nobs), dis(nev,nst), ts(nev,nst)
      DIMENSION x1(NN), fq(nobs), tloc(nst)
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
