
CFLAGS = 

LFLAGS= -lpthread -lrt -lm

all: blurc thresc

clean:
	-$(RM) *.o blurc thresc om1.ppm

blurc: ppmio.o gaussw.o blurfilter.o blurmain.o
	cc -o $@ ppmio.o gaussw.o blurfilter.o blurmain.o $(LFLAGS)

thresc: thresmain.o ppmio.o thresfilter.o
	cc -Ofast -o $@ thresmain.o ppmio.o thresfilter.o $(LFLAGS)

arc:
	tar cf - *.c *.h *.f90 Makefile Makefile.sgi|gzip - > filters.tar.gz
