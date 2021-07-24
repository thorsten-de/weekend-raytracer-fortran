FC = gfortran
FCFLAGS = -O3

OBJS = mod_vectors.o mod_types.o mod_utils.o mod_camera.o

.PHONY: all clean
.SUFFIXES: .f90 .o

all: fray

fray: fray.f90 $(OBJS)
	$(FC) $(FCFLAGS) $< $(OBJS) -o $@

.f90.o:
	$(FC) -c $(FCFLAGS) $<

%.o: %.mod

clean:
	$(RM) fray *.o *.mod
