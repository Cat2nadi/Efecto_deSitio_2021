       character*40 archivo,archivo1,archivo2
       real ay1(524288),ay2(524288)
       real ax1(524288)

       open (11,file='names.tmp')
       open (12,file='cut.tmp')
       t0=2.0
       tf=18.0
       archivo(1:40)=''
       archivo1(1:40)=''
       archivo2(1:40)=''
       do i=1,10000
          do j=1,524288
             ay1(j)=0.0
          enddo
          read(11,'(a)',end=1000)archivo
          kk=lnblnk(archivo)
          write(*,*)'kk   ',kk
          archivo1(1:kk)=archivo(1:kk-3)
          archivo1(kk-2:kk)='sec'
          archivo2(1:kk)=archivo(1:kk-3)
          archivo2(kk-2:kk)='esp'
          write(*,*)archivo1
          write(*,*)archivo2
          read(12,*,end=1000)ts
          WRITE(*,*)'TEST1'
          write(*,*)'archivo ',archivo
          call rsac1(archivo,ay1,isiz,beg,del,524288,nerr)
          write(*,*)'del ', del
          call getfhv('dist',dist,err1)
          write(*,*)'test2'
          write(*,*)ts,del
c Preguntar el criterio de la distancia estaba en 200 pero lo pusimos a 600 
          if (dist.le.600.)then
             
             open (13,file=archivo2)
             isalta=ts/del-t0/del
             itotal=(tf+t0)/del
             write(*,*)isalta,itotal
             do j=1,itotal
                ay2(j)=ay1(j+isalta)
                
             enddo

             call wsac1(archivo1,ay2,itotal,0,del,nerr)

             call sub_fftmasac(archivo1,ax1,ay1,k1)
             do j=1,k1/2
                write(13,*)ax1(j),ay1(j),dist
             enddo
             close(13)
          endif
      enddo
1000  continue
       close(11)
       close(12)
10    format(a30,f7.3,f6.3)
      end
