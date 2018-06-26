# Macros

COMPILER = ifort

all: run

run: GraphMod.o callProg.o
	${COMPILER} GraphMod.o callProg.o -o run

GraphMod.o: GraphMod.f90
	${COMPILER} -c GraphMod.f90

callProg.o: callProg.f90
	${COMPILER} -c callProg.f90

network: GraphMod.o NetworkFlowMod.o callNet.o
	${COMPILER} GraphMod.o NetworkFlowMod.o callNet.o -o run
	./run

NetworkFlowMod.o: NetworkFlowMod.f90
	${COMPILER} -c NetworkFlowMod.f90

callNet.o: callNet.f90
	${COMPILER} -c callNet.f90

exe:
	./run

clean:
	rm *.o *.mod out* run
