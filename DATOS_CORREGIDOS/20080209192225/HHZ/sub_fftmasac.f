      subroutine sub_fftmasac(kname,res1,res2,nn)
C *********************************************************************
C
C PROGRAMA PARA LA GENERACION DE ESPECTROS DE FOURIER DE AMPLITUD de 
c senales de tiempo, suavizados con smooth.
C
C *********************************************************************
C *   Original M.A. Santoyo
C *   Versión para archivos SAC, abril, 2002. A. Iglesias
C *   Convertido a subrutina para sp_ratio.f 2004 A. Iglesias
      CHARACTER*40 kname
      COMPLEX CX(524288)
      dimension res(524288)
      real res1(524288),res2(524288)

      WRITE(*,*)'NOMBRE DEL ARCHIVO DE ENTRADA Y SALIDA'
*     READ(*,'(A)')kname,FILE2 
*     OPEN(1,FILE=FILE1)
*     OPEN(2,FILE=FILE2)
*     WRITE(*,*)'QUE COMPONENTE DESEA NS=1,V=2,EW=3'
*     READ(*,*)MPO
*     WRITE(*,*)'INTERVALO DE MUESTREO'
*     READ(*,*)DET
*     write(*,*)'suavizar con 1/3,1/6,etc'
*     read(*,*)h
      write(*,*) ' Kname     ', kname
      h=6


      call lee(cx,kname,nn,det)
      WRITE(*,*)'NUMERO TOTAL DE MUESTRAS:',NN
      WRITE(*,*)'DELTA                    ',DET
*     do i=1,nn
*     write(71,*)cx(i)
*     enddo
      DEF=1.0/(NN*DET)
      WRITE(*,*)'HACIENDO LA TRANSFORMADA DE FOURIER'
      SPAS=-1.
      write(*,*)'nn  ',nn
      CALL FORK(NN,CX,SPAS)
      DO 11 I=1,NN/2
   11 res(i)=CABS(CX(I))*SQRT(float(NN))*DET
*     DO i=1,nn/2
*        res(i)=cabs(cx(i))
*     ENDDO
*     write(*,*)'¿ suavizar?, si=1,no=0'
*     read(*,*)masu
      masu=1
      if(masu.eq.0)goto 50      
c      write(*,*)'suavizando con 1/3 de octava'
      call smooth(res,nn,det,h)

50    continue
      do i=1,nn/2
         res1(i)=def*i
         res2(i)=res(i)
      enddo

*  50 do 12 i=2,nn/2+1
*  12 write(2,100)def*i,res(i)/(def*i)
*  12 write(2,100)def*i,res(i)
*
*     res1(i)=def*i 
*     res2(i)=res(i)
*  12 continue
*     write(*,*)'nn/2  ',nn/2
      CLOSE(1)
      CLOSE(2)
  100 FORMAT(2F18.10)
      END
c
c fft
c
      SUBROUTINE FORK (LX,CX,SIGNI)
      COMPLEX CX(LX),CARG,CTEMP,CW
      J=1
      SC=(1.0/FLOAT(LX))**0.5
      DO 30 I=1,LX
      IF (I .GT. J) GOTO 10
      CTEMP=CX(J)*SC
      CX(J)=CX(I)*SC
      CX(I)=CTEMP
   10 M=LX/2
   20 IF (J .LE. M) GOTO 30
      J=J-M
      M=M/2
      IF (M .GE. 1) GOTO 20
   30 J=J+M
      L=1
   40 ISTEP=2*L
      DO 50 M=1,L
      CARG=(0.0,1.0)*(3.1415926536*SIGNI*(M-1))/FLOAT(L)
      CW=CEXP(CARG)
      DO 50 I=M,LX,ISTEP
      CTEMP=CW*CX(I+L)
      CX(I+L)=CX(I)-CTEMP
   50 CX(I)=CX(I)+CTEMP
      L=ISTEP
      IF (L .LT. LX) GOTO 40
      RETURN
      end

c
c lectura de la senal
c
      subroutine lee(cx,kname,nn,del)
      complex cx(524288)
      real bb(524288)
      character*35 kname
      max=10000000
      i=0
      call RSAC1(kname,bb,i,beg,del,max,nerr)
*  10 READ(1,*,END=20)AA,BB,CC
*     I=I+1
*     IF(MPO.EQ.1)GOTO 1
*     IF(MPO.EQ.2)GOTO 2
*     CX(I)=CMPLX(CC,0.0)
*     GOTO 3
*   1 CX(I)=CMPLX(AA,0.0)
*     GOTO 3
*   2 CX(I)=CMPLX(BB,0.0)
*   3 GOTO 10
*  20 CONTINUE
      K=I
      DO J=1,K
         cx(j)=cmplx(bb(j),0.0)
      enddo
      close (40)
      NN=2048
      IF(K.GT.2048)NN=4096
      IF(K.GT.4096)NN=8192
      IF(K.GT.8192)NN=16384
      IF(K.GT.16384)NN=32768
      IF(K.GT.32768)NN=65536
      IF(K.GT.65536)NN=131072
      IF(K.GT.131072)NN=262144
      IF(K.GT.262144)NN=524288
      IF(K.GT.524288)NN=524288
      DO 40 I=K,NN
   40 CX(I)=(0.0,0.0)
      write(*,*)'I desde sub_lee',i,k,nn,del
      return
      end
c
c smooth
c subrutina para suavizar espectros con un filtro de 1/3 de octava
c John Orcutt 1979
c
      subroutine smooth(a,npts,deltim,h)
      dimension a(1),b(32768)
      df=1./(npts*deltim)
      top=2.**(1./H)
      bottom=2.**(-1./H)
      f=0.0
      nzot=npts/2
      do 1 i=1,nzot
    1 b(i)=a(i)
      fn=1./(2.*deltim)
      do 2 i=1,nzot
        f=f+df
        t=1./f
        tu=t*top
        tb=t*bottom
        fb=1./tu
        fu=1./tb
        do 3 j=i,nzot
          fc=F+df*(j-i+1)
          k=(j-i)+1
          if(fc.gt.fu)goto 4
          if(fc.gt.fn)goto 4
    3   continue
    4   k=k-1
        do 5 j=1,i
          fc=f-df*j
          l=j
          if(fc.lt.fb)goto 6
          if(fc.lt.df)goto 6
    5   continue
    6   l=l-1
        no=k+l+1
        sum=b(i)
        if(k.eq.0)goto 70
        do 7 j=1,k
    7   sum=sum+b(j+i)
   70   continue
        if(l.eq.0)goto 80
        do 8 j=1,l
    8   sum=sum+b(i-j)
   80   continue
        sum=sum/no
    2 a(i)=sum
      return
      end



