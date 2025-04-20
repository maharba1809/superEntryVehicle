
        IMPLICIT NONE
        integer nx,ny,i,j
        PARAMETER (NX=10,NY=10)

	real u(nx,ny),v(nx,ny),h(nx,ny)

	 DO j=1,ny
	  DO i=1,nx
	   v(i,j)=0.0
	   u(i,j)=1.0
	   h(i,j)=0.5
	  ENDDO
	 ENDDO

      open(11,file='field_01.000',form='unformatted')
      write(11)Nx,Ny
      write(11) u
      write(11) v
      write(11) h
      close(11)

	END

!

