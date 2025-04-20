
      subroutine esta_alloc2()
      use stat
      use dimensiones

      IMPLICIT NONE
      
      allocate(st(nx,ny,nz,nl))
      allocate(st1(nx,ny,nz1,nl))
      allocate(sta(nx,ny,nz,nl))
      allocate(stb(nx,ny,nz,nl))
      allocate(stc(nx,ny,nz,nl))
      allocate(st2(nx,ny1,nz1,nl))
      allocate(st3(nx,nz,nl))
      allocate(urms(nx,nz))
      allocate(vrms(nx,nz))
      allocate(wrms(nx,nz))
      allocate(uv(nx,nz))
      allocate(uw(nx,nz))
      allocate(vw(nx,nz))
      allocate(deltau(nx))   
      allocate(urms3(nx,ny,nz))
      allocate(vrms3(nx,ny,nz))
      allocate(wrms3(nx,ny,nz))
      allocate(uv3(nx,ny,nz))
      allocate(uw3(nx,ny,nz))
      allocate(vw3(nx,ny,nz))

      allocate(xx(nx,ny,nz))
      allocate(yy(nx,ny,nz))
      allocate(zz(nx,ny,nz))
      return
      end subroutine esta_alloc2

      subroutine esta_dealloc2

      use stat
      use dimensiones

      IMPLICIT NONE

      deallocate(st)
      deallocate(st1)
      deallocate(sta)
      deallocate(stb)
      deallocate(stc)
      deallocate(st2)
      deallocate(st3)
      deallocate(urms)
      deallocate(vrms)
      deallocate(wrms)
      deallocate(uv)
      deallocate(uw)
      deallocate(vw)
      deallocate(deltau)

      deallocate(xx)
      deallocate(yy)
      deallocate(zz)
      return
      end subroutine esta_dealloc2

