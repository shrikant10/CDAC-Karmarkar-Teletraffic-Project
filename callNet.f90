program callNet
use NetworkFlowMod
implicit none

    type(NetworkFlow) :: N
    
    call readNetworkFile(N, 'NetworkModel_In')
    call writeAMPLFile('data.dat', N)

end program callNet
