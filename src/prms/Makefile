# prms libraray makefile
# $Id: Makefile 4874 2012-10-04 18:29:55Z rsregan $

include ../makelist
PRMSLIB = $(LIBDIR)/libprmsiv.a
MMFLIB	 = $(LIBDIR)/libmmf.a
LIBS	= $(PRMSLIB) $(MMFLIB) $(FLIBS)

####################################################
# Rules for targets
####################################################
all: $(PRMSLIB)

$(TARGET): $(PRMSLIB)
	$(RM) $(TARGET)

$(MMFLIB):
	$(CD) $(MMFDIR);make

#
# Define all object files which make up the library
#
LIBOBJS = \
	basin.o \
        cascade.o \
	climateflow.o \
        soltab.o \
        obs.o \
        temp_1sta.o \
        temp_laps.o \
        potet_jh.o \
        potet_pan.o \
        ddsolrad.o \
        ccsolrad.o \
        xyz_dist.o \
        temp_dist2.o \
        climate_hru.o \
        transp_tindex.o \
        potet_hamon.o \
        precip_1sta.o \
        precip_laps.o \
        precip_dist2.o \
        intcp.o \
        utils_prms.o \
        snowcomp.o \
        srunoff_carea.o \
        srunoff_smidx.o \
        soilzone.o \
        gwflow.o \
        strmflow.o \
        basin_sum.o \
        subbasin.o \
        map_results.o \
        write_climate_hru.o

all: lib

install: lib

lib: $(PRMSLIB)

$(PRMSLIB): $(LIBOBJS)
	$(RM) $(PRMSLIB)
	$(AR) $(PRMSLIB) $(LIBOBJS)
	$(RANLIB) $(PRMSLIB)

clean:
	$(RM) $(PRMSLIB)
	$(RM) *.o *.mod *~

gwflow.o: gwflow.f90
	gfortran -c $(FFLAGS) gwflow.f90

utils_prms.o: utils_prms.f90
	gfortran -c $(FFLAGS) utils_prms.f90

intcp.o: intcp.f90
	gfortran -c $(FFLAGS) intcp.f90

map_results.o: map_results.f90
	gfortran -c $(FFLAGS) map_results.f90

soltab.o: soltab.f90
	gfortran -c $(FFLAGS) soltab.f90

precip_1sta.o: precip_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_1sta.f90

transp_tindex.o: transp_tindex.f90
	gfortran -c $(FFLAGS) transp_tindex.f90

temp_1sta.o: temp_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_1sta.f90

temp_laps.o: temp_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_laps.f90

precip_laps.o: precip_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_laps.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh.f90

potet_pan.o: potet_pan.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) write_climate_hru.f90

climate_hru.o: climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) climate_hru.f90

climateflow.o: climateflow.f  prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) climateflow.f

basin.o: basin.f  prms_module.mod
	gfortran -c $(FFLAGS) basin.f

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_gwflow.mod: gwflow.o
prms_module.mod:
	$(CD) ../gsflow;make prms_module.mod
	$(CP) ../gsflow/prms_module.mod .
prms_obs.mod: obs.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
