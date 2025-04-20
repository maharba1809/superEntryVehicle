        program fronterax
        implicit none
! Initialisation couche de melange
        integer nx,ny,nz
        character(10)tit
        open(99,file='data.in',form='formatted') 
        read(99,*)tit
        read(99,200)nx
        read(99,200)ny
        read(99,200)nz
        close(99)
200     format(20x,i4)
        CALL init(nx,ny,nz)
        END

        SUBROUTINE init(nx,ny,nz)
        
        integer nx,ny,nz,n1,n2,n3
        integer i,j,k,nv
        real umoy,deltau,h_teta,cmoy,deltac,z
        real,allocatable,DIMENSION(:,:,:)::xx,yy,zz,uu,uv,vv,ww,temp,press,cc
        real,allocatable,DIMENSION(:,:,:)::frontin
        allocate(xx(nx,ny,nz))
        allocate(yy(nx,ny,nz))
        allocate(zz(nx,ny,nz))
        allocate(uu(nx,ny,nz))
        allocate(uv(nx,ny,nz))
        allocate(vv(nx,ny,nz))
        allocate(ww(nx,ny,nz))
        allocate(temp(nx,ny,nz))
        allocate(press(nx,ny,nz))
        allocate(cc(nx,ny,nz))
        allocate(frontin(ny,nz,6))

        umoy=1.0855/2.
        deltau=1.0/2.
        h_teta=30.0
        cmoy=0.5
        deltac=0.5
        open(11,file='jet.grid',form='unformatted')
        READ(11)nv
        READ(11)n1,n2,n3
        read(11)xx
        read(11)yy
        read(11)zz
        close(11)

        if(nx.ne.n1)write(*,*)'Error de Coincidencia de nodos en x'
        if(ny.ne.n2)write(*,*)'Error de Coincidencia de nodos en y'
        if(nz.ne.n2)write(*,*)'Error de Coincidencia de nodos en z'
        
        write(*,*)'x'
        write(*,*)'Dimensiones',size(xx,1),size(xx,2),size(xx,3)
        do i=1,nx
       !  write(*,*)i,xx(i,1,1)
        end do     
        write(*,*)'y'
        write(*,*)'Dimensiones',size(yy,1),size(yy,2),size(yy,3)
        do j=1,ny
        ! write(*,*)j,yy(1,j,1)
        end do     
        write(*,*)'z'
        write(*,*)'Dimensiones',size(zz,1),size(zz,2),size(zz,3)
        do k=1,nz
        !write(*,*)k,zz(1,1,k)
        end do     

        DO k=1,nz
         DO j=1,ny
            DO i=1,nx
             uv(i,j,k)=1.0
             uu(i,j,k)=1.0
             cc(i,j,k)=1.0
            END DO
         END DO
        END DO


        DO k=1,nz
         DO j=1,ny
            DO i=1,nx
            vv(i,j,k)=0.0
            ww(i,j,k)=0.0
            press(i,j,k)=1.0
            temp(i,j,k)=1.0
            ENDDO
         ENDDO
        ENDDO
        DO k=1,nz
         DO j=1,ny
          frontin(j,k,1)=temp(1,j,k)
          frontin(j,k,2)=uv(1,j,k)
          frontin(j,k,3)=vv(1,j,k)
          frontin(j,k,4)=ww(1,j,k)
          frontin(j,k,5)=press(1,j,k)
          frontin(j,k,6)=cc(1,j,k)
         ENDDO
        ENDDO

        open(54,file='frontx.dat')
!        write(54,*)'VARIABLES = "X", "Y", "Z"'!, "U", "V", "W", "T", "P"'
!        write(54,*)'zone I=',nx,'J=',ny,"K=",nz,' datapacking=point'
        do k=1,nz
        do j=1,ny
          do i=1,nx
!     write(54,*)xx(i,j,k),yy(i,j,k),zz(i,j,k),uu(i,j,k),vv(i,j,k),ww(i,j,k),temp(i,j,k),press(i,j,k)
          end do
        end do
        end do
        close(54)
        open(11,file='field_01.000',form='unformatted')
        write(11)nv
        write(11)nx,ny,nz
        write(11)uu
        write(11)vv
        write(11)ww
        write(11)temp
        write(11)press
        write(11)cc
        close(11)

        open(11,file='frontx.in',form='unformatted')
        write(11)nv
        write(11)nx,ny,nz
        write(11)frontin
        close(11)

        deallocate(xx)
        deallocate(yy)
        deallocate(zz)
        deallocate(uu)
        deallocate(uv)
        deallocate(vv)
        deallocate(ww)
        deallocate(temp)
        deallocate(press)
        deallocate(cc)
        deallocate(frontin)
        return
        end subroutine 

