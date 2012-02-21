# prms libraray makefile
# $Id: Makefile 4059 2011-12-13 19:17:38Z rsregan $

include ../makelist
PRMSLIB = ../../lib/libprms.a
GSFDIR = ../gsflow

# Define all object files which make up the library
#
LIBOBJS = \
        basin.o \
        climateflow.o \
        obs.o \
        soltab.o \
        cascade.o \
        transp_tindex.o \
        temp_1sta.o \
        temp_laps.o \
        temp_dist2.o \
        climate_hru.o \
        ddsolrad.o \
        ccsolrad.o \
        potet_jh.o \
        potet_hamon.o \
        potet_pan.o \
        intcp.o \
        precip_1sta.o \
        precip_laps.o \
        precip_dist2.o \
        xyz_dist.o \
        snowcomp.o \
        soilzone.o \
        srunoff_smidx.o \
        srunoff_carea.o \
        gwflow.o \
        strmflow.o \
        subbasin.o \
        basin_sum.o \
        hru_sum.o \
        map_results.o \
        basin_sum.o

all: prms_module.mod lib

install: lib

lib: prms_module.mod $(PRMSLIB)

prms_module.mod: $(GSFDIR)/prms_module.mod
		$(RM) prms_module.mod
		$(CP) $(GSFDIR)/prms_module.mod prms_module.mod

gsfmodule.mod: $(GSFDIR)/gsfmodule.mod
		$(RM) gsfmodule.mod
		$(CP) $(GSFDIR)/gsfmodule.mod gsfmodule.mod

gsfprms2mf.mod: $(GSFTRUNKDIR)/gsfprms2mf.mod
	$(RM) gsfmprms2mf.mod
	$(CP) $(GSFTRUNKDIR)/gsfprms2mf.mod gsfprms2mf.mod

prms_climatevars.mod: climateflow.o

$(GSFDIR)/prms_module.mod:
	$(RM) prms_module.mod
	$(CD) ../gsflow; make gsflow_prms.o

cascade.o: cascade.f prms_module.mod
	$(FC) -c $(FFLAGS) cascade.f
temp_1sta.o: temp_1sta.f90
	$(FC) -c $(FFLAGS) temp_1sta.f90
temp_laps.o: temp_laps.f90
	$(FC) -c $(FFLAGS) temp_laps.f90
temp_dist2.o: temp_dist2.f prms_module.mod
	$(FC) -c $(FFLAGS) temp_dist2.f
precip_1sta.o: precip_1sta.f90
	$(FC) -c $(FFLAGS) precip_1sta.f90
precip_laps.o: precip_laps.f90 prms_module.mod 
	$(FC) -c $(FFLAGS) precip_laps.f90
precip_dist2.o: precip_dist2.f prms_module.mod
	$(FC) -c $(FFLAGS) precip_dist2.f
xyz_dist.o: xyz_dist.f prms_module.mod
	$(FC) -c $(FFLAGS) xyz_dist.f
basin.o: basin.f prms_module.mod
	$(FC) -c $(FFLAGS) basin.f
snowcomp.o: snowcomp.f prms_module.mod
	$(FC) -c $(FFLAGS) snowcomp.f
srunoff_smidx.o: srunoff_smidx.f prms_module.mod
	$(FC) -c $(FFLAGS) srunoff_smidx.f
srunoff_carea.o: srunoff_carea.f prms_module.mod
	$(FC) -c $(FFLAGS) srunoff_carea.f
soilzone.o: soilzone.f prms_module.mod
	$(FC) -c $(FFLAGS) soilzone.f
subbasin.o: subbasin.f prms_module.mod
	$(FC) -c $(FFLAGS) subbasin.f
basin_sum.o: basin_sum.f prms_module.mod
	$(FC) -c $(FFLAGS) basin_sum.f
hru_sum.o: hru_sum.f90
	$(FC) -c $(FFLAGS) hru_sum.f90
strmflow.o: strmflow.f90
	$(FC) -c $(FFLAGS) strmflow.f90
transp_tindex.o: transp_tindex.f90
	$(FC) -c $(FFLAGS) transp_tindex.f90
climate_hru.o: climate_hru.f90
	$(FC) -c $(FFLAGS) climate_hru.f90
gwflow.o: gwflow.f90 prms_module.mod
	$(FC) -c $(FFLAGS) gwflow.f90
obs.o: obs.f prms_module.mod
	$(FC) -c $(FFLAGS) obs.f
climateflow.o: climateflow.f prms_module.mod
	$(FC) -c $(FFLAGS) climateflow.f
soltab.o: soltab.f90
	$(FC) -c $(FFLAGS) soltab.f90
ddsolrad.o: ddsolrad.f prms_module.mod
	$(FC) -c $(FFLAGS) ddsolrad.f
ccsolrad.o: ccsolrad.f prms_module.mod
	$(FC) -c $(FFLAGS) ccsolrad.f
potet_jh.o: potet_jh.f90
	$(FC) -c $(FFLAGS) potet_jh.f90
potet_jh2.o: potet_jh2.f90
	$(FC) -c $(FFLAGS) potet_jh2.f90
potet_hamon.o: potet_hamon.f90
	$(FC) -c $(FFLAGS) potet_hamon.f90
potet_pan.o: potet_pan.f90 prms_module.mod
	$(FC) -c $(FFLAGS) potet_pan.f90
intcp.o: intcp.f90 prms_module.mod
	$(FC) -c $(FFLAGS) intcp.f90
map_results.o: map_results.f90 prms_module.mod
	$(FC) -c $(FFLAGS) map_results.f90

$(PRMSLIB): $(GSFDIR)/prms_module.mod $(LIBOBJS)
	$(RM) $(PRMSLIB)
	$(AR) $(PRMSLIB) $(LIBOBJS)
	$(RANLIB) $(PRMSLIB)

clean:
	$(RM) *.o $(PRMSLIB) *.mod *~ svn*.tmp

