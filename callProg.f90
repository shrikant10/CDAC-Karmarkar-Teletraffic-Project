program callProg
use GraphMod
implicit none
    
  interface
    subroutine writeMatFile(filename, Mat)
      character (len=*), intent(in) :: filename
      integer, dimension(:,:), intent(in) :: Mat
    end subroutine  
  end interface
    
  type(Graph) :: G
  integer, dimension(:,:), allocatable :: AdjMat
  integer, dimension(:,:), allocatable :: IncMat
  integer, dimension(:,:), allocatable :: AdjMat2
  
  integer :: i, j
  
  call readGraphFile(G, 'input.dat')
  
  ! Allocating Memory to matrices
  allocate(AdjMat(G%v,G%v))
  allocate(IncMat(G%v,G%e))
  allocate(AdjMat2(G%v,G%v))
  
  
  ! Calling subroutines
  call AdjacencyMatGraph (G, AdjMat, dir = .false.)
  call IncidenceMatGraph (G, IncMat, dir = .false.)
  call AdjacencyIncidence(AdjMat2, IncMat)
  
  ! Writing the output file
  call writeMatFile('out_adj', AdjMat)
  call writeMatFile('out_inc', IncMat)
  call writeMatFile('out_adj2', AdjMat2) 
  
  !Deallocating Memory
  deallocate(AdjMat2)
  deallocate(IncMat)    
  deallocate(AdjMat)
  deallocate(G%edgeList)
  
    
end program callProg

subroutine writeMatFile(filename, Mat)

    ! Writing the matrix in a file
    character (len=*), intent(in) :: filename
    integer, dimension(:,:), intent(in) :: Mat
    integer :: i, j
    integer, dimension(2) :: shapeMat
    
    shapeMat = shape(Mat)
    
    open(unit = 11, file = filename)
    do i = 1, shapeMat(1)
      write(11, *), (Mat(i,j), j=1,shapeMat(2))
    end do
    close(unit = 11) 

end subroutine writeMatFile
