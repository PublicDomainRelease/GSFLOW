# MF2K5 Libraray makefile
# $Id: Makefile 2934 2006-11-07 16:50:04Z rsregan $

include ../makelist

MF2K5LIB = ../../lib/libmf2k5.a

LIBOBJS = \
        utl7.o \
        gwf2bas7.o \
        pcg7.o \
        sip7.o \
        de47.o \
        gmg7.o \
        mhc7.o \
        gwf2bcf7.o \
        gwf2huf7.o \
        gwf2chd7.o \
        gwf2ghb7.o \
        gwf2lpf7.o \
        gwf2hfb7.o \
        gwf2wel7.o \
        gwf2sfr7.o \
        gwf2lak7.o \
        gwf2uzf1.o \
        gwf2gag7.o \
        parutl7.o \
        mf2kgmg.o \
        hufutl7.o \
        r_vector.o \
        solvers.o \
        ccfd.o \
        gwf2fhb7.o \
        gwf2mnw7.o \
	obs2bas7.o \
	obs2chd7.o \
	obs2ghb7.o

all: $(PROGRAM) lib
	echo made $(MF2K5LIB) $(PROGRAM)

install: lib

lib: $(MF2K5LIB)

clean:
	$(RM) *.o *.mod *~ $(MF2K5LIB) $(PROGRAM)

$(MF2K5LIB): $(LIBOBJS)
	$(RM) $(MF2K5LIB)
	$(AR) $(MF2K5LIB) $(LIBOBJS)
	$(RANLIB) $(MF2K5LIB)

ccfd.o: ccfd.c ccfd.h
	$(CC) -c $(CFLAGS) ccfd.c

mf2kgmg.o: mf2kgmg.c mf2kgmg.h
	$(CC) -c $(CFLAGS) mf2kgmg.c

mf2kgmg_wrap.o: mf2kgmg_wrap.c mf2kgmg.h
	$(CC) -c $(CFLAGS) mf2kgmg_wrap.c

r_vector.o: r_vector.c r_vector.h
	$(CC) -c $(CFLAGS) r_vector.c

solvers.o: solvers.c solvers.h
	$(CC) -c $(CFLAGS) solvers.c

utl7.o: utl7.f openspec.inc
	$(FC) $(FFLAGS) -c utl7.f

gwf2bas7.o: gwf2bas7.f openspec.inc
	$(FC) $(FFLAGS) -c gwf2bas7.f

gwf2hfb7.o: gwf2hfb7.f openspec.inc
	$(FC) $(FFLAGS) -c gwf2hfb7.f

#  end
