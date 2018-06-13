program graph_conv

  use GraphMod
  implicit none
  
  type(Graph) :: G
  integer, dimension(:,:), allocatable :: adj_mat
  integer, dimension(:,:), allocatable :: inc_mat
  
  integer :: i, j
  
  open(unit =1, file='input.dat')
  read(unit =1, FMT=*) G%v, G%e
  
  allocate(G%edgeList(G%e, 2))
  allocate(adj_mat(G%v, G%v))
  allocate(inc_mat(G%v, G%e))
    
  do i = 1,G%e
    read(unit = 1,FMT=*) G%edgelist(i,1), G%edgelist(i,2)
  end do

  close(unit=1)

  adj_mat = 0
  do i = 1,G%e
    adj_mat(G%edgelist(i,1),G%edgelist(i,2)) = 1
    adj_mat(G%edgelist(i,2),G%edgelist(i,1)) = 1
  end do
  
  inc_mat = 0
  do i = 1,G%e
    inc_mat(G%edgelist(i,1),i) = 1
    inc_mat(G%edgelist(i,2),i) = 1
  end do
  
  open(unit = 2, file = 'out_adj')
  do i = 1, G%v
    write(2, *), (adj_mat(i,j), j=1,G%v)
  end do
  close(unit = 2)
  
  open(unit = 3, file = 'out_inc')  
  do i = 1, G%v
    write(3, *), (inc_mat(i,j), j=1,G%e)
  end do
  close(unit =3)
  
  deallocate(G%edgeList)
  deallocate(adj_mat)
  deallocate(inc_mat)
  
end program graph_conv

