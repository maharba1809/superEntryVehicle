      use dimensiones
      use consderper
      use consdernper
      use consderper_f
      use consdernper_f
      use consrs
      use consrs_f
      use funciones 
      use derivvel
      use derivtools
      use velocidades
      use jacobtools
      use mallagrid
       real,  allocatable, dimension (:,:,:) :: p,t
       real,  allocatable, dimension (:,:,:) :: q,wx
       integer i_sample,n_serie
       integer n1,n2,n3
       integer i,j,k,l
      character(2) serie
      character(3) sample
      character(15) outputfield
      character(15) inputfield
      real :: rq,r


      CALL INI ()
      CALL MEMORIA ()
      CALL INIDERIV ()

       allocate(t(nx,ny,nz))
       allocate(p(nx,ny,nz))
       allocate(q(nx,ny,nz))
       allocate(wx(nx,ny,nz))

      open(11,file='jet.grid',form='unformatted')
      read(11)nd
      read(11)n1,n2,n3
      read(11)x
      read(11)y
      read(11)z
      close(11)

      CALL JACOBEANO ()


       n_serie=6
       i_sample=10
       WRITE(sample,'(i3.3)')i_sample
       WRITE(serie,'(i2.2)')n_serie
       outputfield='field_'//serie//'.'//sample
       inputfield='jet_'//sample//'.dat'
       OPEN(11,file=outputfield,form='unformatted')
       OPEN(61,FILE=inputfield,FORM='formatted')
       READ(11)nd
       READ(11)n1,n2,n3
       READ(11)u
       READ(11)v
       READ(11)w
       READ(11)t
       READ(11)p
       CLOSE(11)
       do i=1,nx
       do j=1,ny
       do k=1,nz
        p(i,j,k)=0.5*p(i,j,k)
       enddo
       enddo
       enddo

       OPEN(11,FILE='field_07.000',FORM='unformatted')
       Write(11)nd
       Write(11)n1,n2,n3
       Write(11)u
       Write(11)v
       Write(11)w
       Write(11)t
       Write(11)p
       close (11)
       end


      SUBROUTINE ini()

      use dimensiones
      use consfiltro
      use deltas

      nx=150
      ny=109
      nz=109
      cfilt=0.49
      deltax=float(nx-1)
      deltay=float(ny-1)
      deltaz=float(nz-1)

   
      end subroutine ini
