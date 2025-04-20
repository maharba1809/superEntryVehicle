subroutine readMask()
use maskMod
use dimensiones
implicit none

write(*,*)'mask allocated'
allocate(mask(nx,ny,nz))

open(54,file='mesh/mask.grid',form='unformatted')
    read(54)mask
close(54)
write(*,*)'mask loaded successfully'
return
end subroutine 
