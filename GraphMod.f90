module GraphMod
implicit none
  
  type Graph
    integer :: v                                     ! Num of Vertices
    integer :: e                                     ! Num of edges
    integer, dimension(:,:), allocatable :: edgeList ! List of edges
    logical :: isdirected = .false.                  ! Edges directed/undirected 
  end type Graph
  
  contains
  
  subroutine AdjacencyMatGraph (G, AdjMat)  
    
    type(Graph), intent(in) :: G
    integer, dimension(:,:), intent(out) :: AdjMat
    integer :: i, j
    
    AdjMat(:,:) = 0
    
    if(G.isdirected == .true.) then
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
  
  
  
  
  subroutine IncidenceMatGraph (G, IncMat)
    
    type(Graph), intent(in) :: G
    integer, dimension(:,:), intent(out) :: IncMat
    integer :: i, j
    
    IncMat(:,:) = 0
    
    if(G.isdirected == .true.) then
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
  
    
  ! Derive adjacency matrix from inedence matrix  A=I*Transpose(I)
  ! Output Adjacency matrix contains degree of vertices in digonal elements

  subroutine AdjacencyIncidence(AdjMat, IncMat)
    
    integer, dimension(:,:), intent(in) :: IncMat
    integer, dimension(:,:), intent(out) :: AdjMat
    
    AdjMat = matmul(IncMat, transpose(IncMat))
    
  end subroutine AdjacencyIncidence


  ! Reading the input Graph file
  
  subroutine readGraphFile(G, filename)
  
    type(Graph), intent(out) :: G 
    character (len=*), intent(in) :: filename
    integer :: i, dir
    
    open(unit =10, file= filename, status='old')
    read(unit =10, FMT=*) G.v, G.e, dir
    
    if(dir == 1) then
        G.isdirected = .true.
    end if
        
    allocate(G.edgeList(G.e, 2))    
    do i = 1,G.e
      read(unit = 10,FMT=*) G.edgelist(i,1), G.edgelist(i,2)
    end do
    close(unit=10)
  
  end subroutine readGraphFile

end module GraphMod
