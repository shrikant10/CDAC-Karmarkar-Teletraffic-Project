all: run

run: GraphMod.o callProg.o
	ifort GraphMod.o callProg.o -o run

GraphMod.o: GraphMod.f90
	ifort -c GraphMod.f90

callProg.o: callProg.f90
	ifort -c callProg.f90

clean:
	rm *.o *.mod out* run
