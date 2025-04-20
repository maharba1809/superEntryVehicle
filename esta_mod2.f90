
      module stat
!-----------------------------------------------------------------------

      real, allocatable, dimension (:,:,:,:) :: st,st1,st2
      real, allocatable, dimension (:,:,:,:) :: sta,stb,stc
      real, allocatable, dimension (:,:,:) :: st3
      real, allocatable, dimension (:) :: deltau    
      real, allocatable, dimension (:,:) :: urms,vrms,wrms
      real, allocatable, dimension (:,:) :: uv,uw,vw
      real, allocatable, dimension (:,:,:) :: xx,yy,zz
      real, allocatable, dimension (:,:,:) :: urms3,vrms3,wrms3
      real, allocatable, dimension (:,:,:) :: uv3,uw3,vw3



      end module stat
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
      module dimensiones
!-----------------------------------------------------------------------

      integer nx,ny,nz,nl,ny1,nz1

      end  module dimensiones


