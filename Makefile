
### For GFORTRAN ###
FC = gfortran
DEFAULT = -ffree-line-length-none

DEBUG=-DDEBUG -g
#DEBUG = -DDEBUG -g

### For NVFORTRAN ###
#FC = nvfortran
#DEFAULT = -Mconcur -Minfo
#OPTS = -g -fbounds-check -fbacktrace -ffpe-trap=zero,overflow,underflow
#OPTS = -g -C



all: ffa

constants.o: constants.F95
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c constants.F95

binarize.o: binarize.F95
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c binarize.F95
	
preprocess.o: preprocess.F95
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c preprocess.F95
	
store.o: store.F95
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c store.F95
	
fractals.o: fractals.F95
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c fractals.F95
	
	
main.o: main.F95 constants.o binarize.o preprocess.o store.o fractals.o 
	${FC} ${DEFAULT} ${DEBUG} ${OPTS} -c main.F95

ffa: constants.o binarize.o preprocess.o store.o fractals.o main.o 
	${FC}  ${DEFAULT} ${DEBUG} ${OPTS} -o ffa constants.o binarize.o preprocess.o store.o fractals.o main.o  
 
install: ffa
	cp ffa ~/bin

clean:
	rm -f *.o *.mod fire
