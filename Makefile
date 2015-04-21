FC = gfortran
FFLAGS = -ffast-math -Wall -march=native -O3 -fopenmp #-fbounds-check#-Warray-temporaries #compiler flags
LDFLAGS = -fopenmp #link flags
LIBS = -llapack -lblas

COMPILE = $(FC) $(FFLAGS)
LINK = $(FC) $(LDFLAGS)

PROG = main #program name

#required objects: 
OBJS =
OBJS += constants.o
OBJS += io.o
OBJS += plotroutines.o
OBJS += initialize.o
OBJS += evolve.o
OBJS += main.o

all: $(PROG)

main: $(OBJS)
	$(LINK) -o $@ $^ $(LIBS)

%.o: %.f95
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) $(PROG) $(OBJS) *.mod
	$(RM) *.png *.txt *.plt *.dat
