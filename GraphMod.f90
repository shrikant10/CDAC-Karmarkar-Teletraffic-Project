module GraphMod
implicit none
  
  type Graph
    integer :: v                                      ! Num of Vertices
    integer :: e                                      ! Num of edges
    integer, dimension(:,:), allocatable :: edgeList  ! List of edges
    !character (len = 2) :: graphType                 ! Directed / Undirected
    !logical :: isdirected = false
  end type Graph
  
  contains
  
  subroutine AdjacencyMatGraph (G, AdjMat, dir)
    
    type(Graph), intent(in) :: G
    integer, dimension(:,:), intent(out) :: AdjMat
    logical, intent(in):: dir
    integer :: i, j
    
    AdjMat(:,:) = 0
    
    if(dir == .true.) then
      do i = 1,G.e
        AdjMat(G.edgelist(i,1),G.edgelist(i,2)) = 1
        AdjMat(G.edgelist(i,2),G.edgelist(i,1)) = -1
      end do
    else
      do i = 1,G.e
        AdjMat(G.edgelist(i,1),G.edgelist(i,2)) = 1
        AdjMat(G.edgelist(i,2),G.edgelist(i,1)) = 1
      end do
    end if
    
  end subroutine AdjacencyMatGraph
  
  
  
  
  subroutine IncidenceMatGraph (G, IncMat, dir)
    
    type(Graph), intent(in) :: G
    integer, dimension(:,:), intent(out) :: IncMat
    logical, intent(in):: dir
    integer :: i, j
    
    IncMat(:,:) = 0
    
    if(dir == .true.) then
      do i = 1,G.e
        IncMat(G.edgelist(i,1),i) = -1
        IncMat(G.edgelist(i,2),i) = 1
      end do
    else
      do i = 1,G.e
        IncMat(G.edgelist(i,1),i) = 1
        IncMat(G.edgelist(i,2),i) = 1
      end do
    end if
    
  end subroutine IncidenceMatGraph
  
    
  
  subroutine AdjacencyIncidence(AdjMat, IncMat)
    integer, dimension(:,:), intent(in) :: IncMat
    integer, dimension(:,:), intent(out) :: AdjMat
    
    AdjMat = matmul(IncMat, transpose(IncMat))
    
  end subroutine AdjacencyIncidence


  
  subroutine readGraphFile(G, filename)
  
    ! Reading the input file
    type(Graph), intent(out) :: G 
    character (len=*), intent(in) :: filename
    integer :: i
    
    open(unit =10, file= filename, status='old')
    read(unit =10, FMT=*) G.v, G.e
    
    allocate(G.edgeList(G.e, 2))    
    do i = 1,G.e
      read(unit = 10,FMT=*) G.edgelist(i,1), G.edgelist(i,2)
    end do
    close(unit=10)
  
  end subroutine readGraphFile

end module GraphMod
