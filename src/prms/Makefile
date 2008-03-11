# prms libraray makefile
# $Id: Makefile 3149 2007-02-16 23:22:41Z rsregan $

include ../makelist
PRMSLIB = ../../lib/libprms.a

# Define all object files which make up the library

LIBOBJS = \
        basin_prms.o \
        obs_prms.o \
        soltab_hru_prms.o \
        cascade_prms.o \
        temp_1sta_prms.o \
        temp_laps_prms.o \
        temp_dist2_prms.o \
        ddsolrad_hru_prms.o \
        ccsolrad_hru_prms.o \
        potet_jh_prms.o \
        potet_hamon_hru_prms.o \
        potet_pan_prms.o \
        intcp_prms.o \
        precip_prms.o \
        precip_laps_prms.o \
        precip_dist2_prms.o \
        xyz_dist.o \
        snowcomp_prms.o \
        srunoff_smidx_casc.o \
        srunoff_carea_casc.o \
        soilzone_gsflow.o \
        gwflow_casc_prms.o \
        strmflow_prms.o \
        basin_sum_prms.o \
        hru_sum_prms.o

all: lib

install: lib

lib: $(PRMSLIB)

$(PRMSLIB): $(LIBOBJS)
	$(RM) $(PRMSLIB)
	$(AR) $(PRMSLIB) $(LIBOBJS)
	$(RANLIB) $(PRMSLIB)

clean:
	$(RM) *.o $(PRMSLIB) *.mod *~ svn*.tmp
