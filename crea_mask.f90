module vargrid
implicit none
integer nv,nx,ny,nz,i,j,k
real lx,ly,lz,dx,dy,dz
real px1,px2,py1,py2,pz1,pz2
real xe,ye,ze,re,res,xce,yce,zce,res0,res1
real lc,rc,theta1,xt
real rcon1,xc1i,xc1f,teta1
real xi,xf,rcil
real rcon2,xc2i,xc2f,teta2
real,allocatable,dimension(:,:,:)::mask,x,y,z
end module
!---------------------------------------------
program mascara
use vargrid
open(53,file='mesh/jet.grid',form='unformatted')
      read(53)nv
      read(53)nx,ny,nz

      allocate(x(nx,ny,nz))
      allocate(y(nx,ny,nz))
      allocate(z(nx,ny,nz))
      allocate(mask(nx,ny,nz))

      read(53)x
      read(53)y
      read(53)z
close(53)
!write(*,*)'x'
!do i=1,nx
!write(*,*)x(i,1,1)
!end do

!write(*,*)'y'
!do i=1,ny
!write(*,*)y(1,i,1)
!end do

!write(*,*)'z'
!do i=1,nz
!write(*,*)z(1,1,i)
!end do
lx=x(nx,1,1)
ly=y(1,ny,1)
lz=z(1,1,nz)
dx=lx/float(nx-1)
dy=ly/float(ny-1)
dz=lz/float(nz-1)
!parametros cubo
px1=lx/8.
px2=px1+0.6
py1=ly/2.-0.25
py2=ly/2.+0.25
pz1=lz/2.-0.25
pz2=lz/2.+0.25
!-----------------------------------------------------
!parametros de capsula
!esfera
px1=0.5
re=0.25
xe=px1+re
ye=ly/2.0
ze=lz/2.0
teta1=50.0*3.1416/180.0
!cono1
xc1i=0.6
xc1f=xc1i+0.4/tan(teta1)
!cilindro
xi=xc1f
xf=xi+0.02
!cono2
teta2=70.0*3.1416/180.0
xc2i=xf
xc2f=xc2i+0.4/tan(teta2)
!esfera
res=1.35/3.4
xe=xc1i+0.2055/tan(teta1)
xce=xc1i+res/sin(teta1)
do i=1,nx
   do j=1,ny
      do k=1,nz
         mask(i,j,k)=1.0
!cubo
!if((x(i,j,k).ge.px1).and.(x(i,j,k).le.px2).and.(y(i,j,k).ge.py1).and.(y(i,j,k).le.py2).and.(z(i,j,k).ge.pz1).and.(z(i,j,k).le.pz2))mask(i,j,k)=0.0     
!esfera
!red=sqrt((x(i,j,k)-xe)**2.+(y(i,j,k)-ye)**2.+(z(i,j,k)-ze)**2.)
!capsula
 !cono  1
rcon1=sqrt((y(i,j,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
if((rcon1.le.tan(teta1)*(x(i,j,k)-xc1i)).and.(x(i,j,k).le.xc1f).and.x(i,j,k).ge.xe)mask(i,j,k)=0.0

 !cilindro
if((x(i,j,k).ge.xi).and.(x(i,j,k).le.xf).and.(rcon1.le.0.4))mask(i,j,k)=0.0
 !cono 2
if(rcon1.le.tan(teta2)*(xc2f-x(i,j,k)).and.x(i,j,k).ge.xc2i.and.x(i,j,k).le.xc2f-0.0)mask(i,j,k)=0.0
 !esfera
res0=sqrt((x(i,j,k)-xce)**2.0+(y(i,j,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
if(res0.le.res.and.x(i,j,k).le.xe)mask(i,j,k)=0.0
      end do   
   end do   
end do   

call suave()
!call suave2()

open(55,file='mesh/mask.grid',form='unformatted')
    write(55)mask
close(55)
write(*,*)'mask created-->mask.grid'
end  
!---------------------------------------------------------------------------

!suavizado cono 1
subroutine suave()
use vargrid

do i=2,nx-1
   do j=2,ny-1
      do k=2,nz-1
res=1.35/3.4
!suavizado semiesfera
res1=sqrt((x(i,j,k)-xce)**2.0+(y(i,j,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
!esfera superior izquierdo x-z
res0=sqrt((x(i-1,j,k)-xce)**2.0+(y(i,j,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).le.xe+dx.and.z(i,j,k).ge.ze-dz)mask(i-1,j,k+1)=(res-res0)/(res1-res0)
!esfera inferior izquierdo x-z
res0=sqrt((x(i-1,j,k)-xce)**2.0+(y(i,j,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).le.xe+dx.and.z(i,j,k).le.ze+dz)mask(i-1,j,k-1)=(res-res0)/(res1-res0)
!esfera superior izquierdo x-y
res0=sqrt((x(i-1,j,k)-xce)**2.0+(y(i,j+1,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).le.xe.and.y(i,j,k).ge.ye)mask(i-1,j+1,k)=(res-res0)/(res1-res0)
!esfera inferior izquierdo x-y
res0=sqrt((x(i-1,j,k)-xce)**2.0+(y(i,j-1,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).le.xe.and.y(i,j,k).le.ye)mask(i-1,j-1,k)=(res-res0)/(res1-res0)
!----------------------------------------------------------------------
!cono 1
res=tan(teta1)*(x(i,j,k)-xc1i)
res1=sqrt((y(i,j,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
!circ cuadrante superior izquierdo
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)

if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xe.and.z(i,j,k).ge.ze-dz.and.y(i,j,k).le.ye+dy.and.x(i,j,k).le.xc1f)mask(i,j-1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior izquierdo
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xe.and.z(i,j,k).le.ze+dz.and.y(i,j,k).le.ye+dy.and.x(i,j,k).le.xc1f)mask(i,j-1,k-1)=(res-res0)/(res1-res0)
!circ cuadrante superior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xe.and.z(i,j,k).ge.ze.and.y(i,j,k).ge.ye.and.x(i,j,k).le.xc1f)mask(i,j+1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xe.and.z(i,j,k).le.ze+dz.and.y(i,j,k).ge.ye.and.x(i,j,k).le.xc1f)mask(i,j+1,k-1)=(res-res0)/(res1-res0)
!----------------------------------------------------------------------
!cono 2
res=tan(teta2)*(xc2f-x(i,j,k))
!circ cuadrante superior izquierdo
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xc2i.and.x(i,j,k).le.xc2f.and.z(i,j,k).ge.ze-dz.and.y(i,j,k).le.ye+dy)mask(i,j-1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior izquierdo
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xc2i.and.x(i,j,k).le.xc2f.and.z(i,j,k).le.ze+dz.and.y(i,j,k).le.ye+dy)mask(i,j-1,k-1)=(res-res0)/(res1-res0)
!circ cuadrante superior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xc2i.and.x(i,j,k).le.xc2f.and.z(i,j,k).ge.ze.and.y(i,j,k).ge.ye)mask(i,j+1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xc2i.and.x(i,j,k).le.xc2f.and.z(i,j,k).le.ze+dz.and.y(i,j,k).ge.ye)mask(i,j+1,k-1)=(res-res0)/(res1-res0)

!----------------------------------------------------------------------
!suavizado cilindro
res=0.4
res1=sqrt((y(i,j,k)-ye)**2.0+(z(i,j,k)-ze)**2.0)
!esfera superior izquierdo x-z
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xi.and.x(i,j,k).le.xf.and.z(i,j,k).ge.ze-dz.and.y(i,j,k).le.ye+dy)mask(i,j-1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior izquierdo
res0=sqrt((y(i,j-1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xi.and.x(i,j,k).le.xf.and.z(i,j,k).le.ze+dz.and.y(i,j,k).le.ye+dy)mask(i,j-1,k-1)=(res-res0)/(res1-res0)
!circ cuadrante superior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k+1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xi.and.x(i,j,k).le.xf.and.z(i,j,k).ge.ze.and.y(i,j,k).ge.ye)mask(i,j+1,k+1)=(res-res0)/(res1-res0)
!circ cuadrante inferior derecho 
res0=sqrt((y(i,j+1,k)-ye)**2.0+(z(i,j,k-1)-ze)**2.0)
if(res1.le.res.and.res0.ge.res.and.x(i,j,k).ge.xi.and.x(i,j,k).le.xf.and.z(i,j,k).le.ze+dz.and.y(i,j,k).ge.ye)mask(i,j+1,k-1)=(res-res0)/(res1-res0)
      end do   
   end do   
end do   
return
end subroutine
!--------------------------------------------------
subroutine suave2()
use vargrid

do i=2,nx-1
do j=2,ny-1
do k=2,nz-1
if (mask(i-1,j,k).eq.1.0.and.mask(i,j,k).eq.0.0)then
mask(i,j,k)=0.6
mask(i+1,j,k)=0.1
mask(i-1,j,k)=1
!   mask(i,j,k)=(x(i+1,j,k)-x(i,j,k))/(x(i+1,j,k)-x(i-1,j,k))
end if

end do
end do
end do

return
end subroutine
