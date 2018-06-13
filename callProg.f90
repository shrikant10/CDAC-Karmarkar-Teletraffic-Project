program callProg
use GraphMod
implicit none
  
  type(Graph) :: G
  integer, dimension(:,:), allocatable :: AdjMat
  integer, dimension(:,:), allocatable :: IncMat
  integer, dimension(:,:), allocatable :: AdjMat2
  
  integer :: i, j
  
  ! Reading the input file
  open(unit =1, file='input.dat')
  read(unit =1, FMT=*) G%v, G%e
  
  allocate(G%edgeList(G%e, 2))    
  do i = 1,G%e
    read(unit = 1,FMT=*) G%edgelist(i,1), G%edgelist(i,2)
  end do
  close(unit=1)
  
  
  ! Allocating Memory to matrices
  allocate(AdjMat(G%v,G%v))
  allocate(IncMat(G%v,G%e))  
  allocate(AdjMat2(G%v,G%e))
  
  
  ! Calling subroutines
  call AdjacencyMatGraph (G, AdjMat, dir = .false.)
  call IncidenceMatGraph (G, IncMat, dir = .false.)
  call AdjacencyIncidence(AdjMat2, IncMat)
  
  
  ! Writing the output file
  open(unit = 2, file = 'out_adj')
  do i = 1, G%v
    write(2, *), (AdjMat(i,j), j=1,G%v)
  end do
  close(unit = 2) 
  
  open(unit = 3, file = 'out_inc')  
  do i = 1, G%v
    write(3, *), (IncMat(i,j), j=1,G%e)
  end do
  close(unit =3)  

  open(unit = 4, file = 'out_adj2')  
  do i = 1, G%v
    write(4, *), (AdjMat2(i,j), j=1,G%v)
  end do
  close(unit =4)
  
  
  !Deallocating Memory
  deallocate(AdjMat2)
  deallocate(IncMat)    
  deallocate(AdjMat)    
  deallocate(G%edgeList)
  
end program callProg

subroutine readGraphFile(filename, G)

end subroutine 
