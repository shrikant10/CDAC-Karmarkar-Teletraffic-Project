all: run

run: GraphMod.o callProg.o
	ifort GraphMod.o callProg.o -o run

GraphMod.o: GraphMod.f90
	ifort -c GraphMod.f90

callProg.o: callProg.f90
	ifort -c callProg.f90

network: GraphMod.o NetworkFlowMod.o callNet.o
	ifort GraphMod.o NetworkFlowMod.o callNet.o -o run
	./run

NetworkFlowMod.o: NetworkFlowMod.f90
	ifort -c NetworkFlowMod.f90

callNet.o: callNet.f90
	ifort -c callNet.f90

exe:
	./run

clean:
	rm *.o *.mod out* run
