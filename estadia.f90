      PROGRAM MEDIOS
      implicit none
      real,allocatable,dimension(:,:,:,:)::esta
      real,allocatable,dimension(:,:,:)::x,y,z
      integer nx,ny,nz,i,j,k,nv
      character(2)::tipo
      tipo='3D'
     write(*,*)'	LEYENDO ESTADISTICAS de "stat.dat"'
     OPEN(61,FILE='stat.data',FORM='unformatted')
         read(61)nx,ny,nz
         allocate(esta(nx,ny,nz,5))
         read(61)esta
     CLOSE(61)
         allocate(x(nx,ny,nz))
         allocate(y(nx,ny,nz))
         allocate(z(nx,ny,nz))

     write(*,*)'	LEYENDO GRID de "jet.grid"'
     OPEN(61,FILE='jet.grid',FORM='unformatted')
         read(61)nv
         read(61)nx,ny,nz
         read(61)x
         read(61)y
         read(61)z
     CLOSE(61)

     write(*,*)'	ESCRIBIENDO "jet_mean.dat" tipo=',tipo
     OPEN(61,FILE='jet_mean.dat',FORM='formatted')
     if(tipo.eq.'2D')then
     write(61,*)'VARIABLES = "X", "Y", "U", "V", "P", "T"'
     write(61,*)'ZONE I=',NX,' J=',NY,'  DATAPACKING=POINT'
      k=nz/2
      do j=1,ny
        do i=1,nx
           write(61,*)x(i,j,k),y(i,j,k),&
           esta(i,j,k,1),esta(i,j,k,2),esta(i,j,k,4),esta(i,j,k,5)
        end do
      end do

     else
      write(61,*)'VARIABLES = "X", "Y", "Z", "U", "V", "W", "P", "T"'
      write(61,*)'ZONE I=',NX,' J=',NY,' K=',nz,'  DATAPACKING=POINT'
       do k=1,nz
         do j=1,ny
           do i=1,nx
            write(61,*)x(i,j,k),y(i,j,k),z(i,j,k),&
            esta(i,j,k,1),esta(i,j,k,2),esta(i,j,k,3),&
            esta(i,j,k,4),esta(i,j,k,5)
           end do
         end do
       end do
      end if

      deallocate(x)
      deallocate(y)
      deallocate(z)
      deallocate(esta)
     
      END
