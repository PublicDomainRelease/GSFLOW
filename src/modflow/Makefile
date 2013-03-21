# mf2k5 libraray makefile
# $Id: Makefile 5491 2013-03-14 18:27:53Z rsregan $

include ../makelist

MF2K5LIB = $(LIBDIR)/libmf2k5.a

LIBOBJS = \
        utl7.o \
        gwf2bas7_NWT.o \
        pcg7_NWT.o \
        pcgn_solve2.o \
        pcgn2.o \
        sip7_NWT.o \
        de47.o \
        gmg7.o \
        mhc7.o \
        gwf2bcf7.o \
        gwf2lpf7.o \
        gwf2huf7.o \
        hufutl7.o \
        NWT1_module.o \
        gwf2upw1.o \
        gwfuzfmodule_NWT.o \
        gwfsfrmodule_NWT.o \
        gwf2lak7_NWT.o \
        gwf2sfr7_NWT.o \
        gwf2uzf1_NWT.o \
        gwf2wel7_NWT.o \
        gwf2ghb7_NWT.o \
        gwf2fhb7.o \
        gwf2mnw17_NWT.o \
        gwf2mnw27_NWT.o \
        gwf2mnw2i7.o \
	mach_mod.o \
        NWT1_ilupc_mod.o \
        NWT1_xmdlib.o \
        NWT1_xmd.o \
        NWT1_gmres.o \
        NWT1_solver.o \
        gwf2chd7.o \
        gwf2hfb7_NWT.o \
        gwf2gag7.o \
        parutl7.o \
        mf2kgmg.o \
        r_vector.o \
        solvers.o \
        ccfd.o \
	obs2bas7.o \
	obs2chd7.o \
	obs2ghb7.o


$(MF2K5LIB): $(LIBOBJS)
	$(RM) $(MF2K5LIB)
	$(AR) $(MF2K5LIB) $(LIBOBJS)
	$(RANLIB) $(MF2K5LIB)

clean:
	$(RM) *.o *.mod *~ $(MF2K5LIB)

pcgn_solve2.o: pcgn_solve2.f90
	$(CC) -c $(CFLAGS) pcgn_solve2.f90

pcgn2.o: pcgn2.f90
	$(CC) -c $(CFLAGS) pcgn2.f90

mach_mod.o: mach_mod.f90
	$(CC) -c $(CFLAGS) mach_mod.f90

NWT1_gmres.o: NWT1_gmres.f90
	$(CC) -c $(CFLAGS) NWT1_gmres.f90

NWT1_ilupc_mod.o: NWT1_ilupc_mod.f90
	$(CC) -c $(CFLAGS) NWT1_ilupc_mod.f90

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

gwf2hfb7_NWT.o: gwf2hfb7_NWT.f openspec.inc
	$(FC) $(FFLAGS) -c gwf2hfb7_NWT.f

#  end
