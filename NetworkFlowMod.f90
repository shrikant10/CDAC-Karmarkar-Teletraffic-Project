module NetworkFlowMod
use GraphMod
implicit none

	type NetworkFlow
	
		type(Graph) :: G
		integer, dimension(:), allocatable :: edgeCap		! Edge Capacity Vector 	 e * 1
		integer, dimension(:), allocatable :: edgeCost	    ! Edge Cost Vector     	 e * 1
        integer, dimension(:), allocatable :: edgeFlow      ! Edge Flow vector     	 e * 1
		integer, dimension(:,:), allocatable :: A      		! Incidence Matrix       v * e
		integer, dimension(:), allocatable :: B           	! Net Node Flow Vector   v * 1
		
	end type NetworkFlow	

    integer :: i, j
    integer :: s, d, traffic
	
	contains

	! Reading the input Graph file  
    subroutine readNetworkFile(N, filename)
  
        type(NetworkFlow), intent(out) :: N 
        character (len=*), intent(in) :: filename
        integer :: i, dir
    	s = 1
		d = 6
		traffic = 180
    	
        open(unit =10, file= filename, status='old')
        read(unit =10, FMT=*) N.G.v, N.G.e, dir
    
        if(dir == 1) then
            N.G.isdirected = .true.
        end if
        
        allocate(N.G.edgeList(N.G.e, 2))
        allocate(N.edgeCap(N.G.e))
        allocate(N.edgeCost(N.G.e))
        allocate(N.edgeFlow(N.G.e))
        allocate(N.A(N.G.v, N.G.e))
        allocate(N.B(N.G.v))
          
        do i = 1, N.G.e
          read(unit = 10,FMT=*) N.G.edgelist(i,1), N.G.edgelist(i,2), N.edgeCap(i), N.edgeCost(i)
        end do
        close(unit=10)
      
        call IncidenceMatGraph (N.G, N.A)
        
        N.B(:) = 0
        N.B(s) = -traffic
        N.B(d) = traffic
    end subroutine readNetworkFile
	


	
	subroutine writeAMPLFile(filename, N)
	    
	    type(NetworkFlow), intent(in) :: N 
        character (len=*), intent(in) :: filename
        
        open(unit = 11, file= filename)
        
        write(11, *) "param e :=", N.G.e, ";"
        write(11, *) "param v :=", N.G.v, ";"
        
        write(11, *) "param Cost :="  
        do i = 1,N.G.e
            write(11, *) i, N.edgeCost(i)
        end do
        write(11, *) ";"
        
        
        write(11, *) "param Cap :="  
        do i = 1,N.G.e
            write(11, *) i, N.edgeCap(i)
        end do
        write(11, *) ";"
        
        
        write(11, *) "param A:"  
        do i = 1,N.G.e
            write(11, '(I4)', advance = 'no') i
        end do
        write(11, *) ":="
        do i = 1, N.G.v
            write(11, '(I4)', advance = 'no') i
            do j = 1, N.G.e
                write(11, '(I4)', advance = 'no') N.A(i, j)
            end do
            write(11, *) ""
        end do
        write(11, *) ";"
        
        
        write(11, *) "param B 	:="  
        do i = 1,N.G.v
            write(11, *) i, N.B(i)
        end do
        write(11, *) ";"
        
        close(unit=11)
      
	end subroutine writeAMPLFile
	
end module NetworkFlowMod
