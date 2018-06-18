module NetworkFlowMod
use GraphMod
implicit none

	type NetworkFlow
	
		type(Graph) :: G
		integer, dimension(:), allocatable :: edgeCap		! Edge Capacity        e * 1
		integer, dimension(:), allocatable :: edgeCost	    ! Edge Cost            e * 1
        integer, dimension(:), allocatable :: flow          ! Flow vector          e * 1
		integer, dimension(:,:), allocatable :: IncMat      ! Constrainsts         v * e
		integer, dimension(:), allocatable :: RHS           ! Total Node Flow Vector v * 1
		
	end type NetworkFlow	

	contains
		
	!function readNetwork(filename) result()
		
		
end module NetworkFlowMod
