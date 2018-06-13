module GraphMod
  
  implicit none
  
  type Graph
    integer :: v                                      ! Num of Vertices
    integer :: e                                      ! Num of edges
    integer, dimension(:,:), allocatable :: edgeList  ! List of edges
    !character (len = 2) :: graphType
  end type Graph
  
end module GraphMod
