###################################################
# $Id: Makefile 7497 2015-07-17 16:38:50Z rsregan $
###################################################

include ../makelist
TARGET		= $(BINDIR)/gsflow
MF2K5LIB	= $(LIBDIR)/libmf2k5.a
LIBS 		= -L$(LIBDIR) -lmf2k5 -lprmsiv -lmmf $(FLIBS)

###################################################

MODOBJS =	\
		gsflow_prms.o \
		gsflow_modflow.o \
		gsflow_prms2mf.o \
		gsflow_mf2prms.o \
		gsflow_budget.o \
		gsflow_sum.o

MFMODULES = global.mod gwfbasmodule.mod pcgmodule.mod sipmodule.mod de4module.mod gmgmodule.mod gwfsfrmodule.mod gwfuzfmodule.mod gwflakmodule.mod

MFMODDEPN = $(MODFLOWDIR)/global.mod $(MODFLOWDIR)/gwfbasmodule.mod $(MODFLOWDIR)/pcgmodule.mod $(MODFLOWDIR)/sipmodule.mod $(MODFLOWDIR)/de4module.mod $(MODFLOWDIR)/gmgmodule.mod $(MODFLOWDIR)/gwfsfrmodule.mod $(MODFLOWDIR)/gwfuzfmodule.mod $(MODFLOWDIR)/gwflakmodule.mod

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MF2K5LIB) $(PRMSLIB) $(MODOBJS) $(LIBDIR)/libmmf.a
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) $(MODOBJS) $(LIBS)

clean:
	$(RM) $(TARGET)
	$(RM) *.o *.mod *~

####################################################
# Rules for the modules
###################################################
# if on windows comment out next 3 lines
openspec.inc:
	$(RM) openspec.inc
	$(CP) openspec_linux.inc openspec.inc

global.mod: $(MODFLOWDIR)/global.mod
	$(RM) global.mod
	$(LN) $(MODFLOWDIR)/global.mod global.mod

gwfbasmodule.mod: $(MODFLOWDIR)/gwfbasmodule.mod
	$(RM) gwfbasmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbasmodule.mod gwfbasmodule.mod

gwfbcfmodule.mod: $(MODFLOWDIR)/gwfbcfmodule.mod
	$(RM) gwfbcfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfbcfmodule.mod gwfbcfmodule.mod

gwfsfrmodule.mod: $(MODFLOWDIR)/gwfsfrmodule.mod
	$(RM) gwfsfrmodule.mod
	$(LN) $(MODFLOWDIR)/gwfsfrmodule.mod gwfsfrmodule.mod

gwfnwtmodule.mod: $(MODFLOWDIR)/gwfnwtmodule.mod
	$(RM) gwfnwtmodule.mod
	$(LN) $(MODFLOWDIR)/gwfnwtmodule.mod gwfnwtmodule.mod

gwflpfmodule.mod: $(MODFLOWDIR)/gwflpfmodule.mod
	$(RM) gwflpfmodule.mod
	$(LN) $(MODFLOWDIR)/gwflpfmodule.mod gwflpfmodule.mod

gwfuzfmodule.mod: $(MODFLOWDIR)/gwfuzfmodule.mod
	$(RM) gwfuzfmodule.mod
	$(LN) $(MODFLOWDIR)/gwfuzfmodule.mod gwfuzfmodule.mod

gwflakmodule.mod: $(MODFLOWDIR)/gwflakmodule.mod
	$(RM) gwflakmodule.mod
	$(LN) $(MODFLOWDIR)/gwflakmodule.mod gwflakmodule.mod

gmgmodule.mod: $(MODFLOWDIR)/gmgmodule.mod
	$(RM) gmgmodule.mod
	$(LN) $(MODFLOWDIR)/gmgmodule.mod gmgmodule.mod

pcgmodule.mod: $(MODFLOWDIR)/pcgmodule.mod
	$(RM) pcgmodule.mod
	$(LN) $(MODFLOWDIR)/pcgmodule.mod pcgmodule.mod

sipmodule.mod: $(MODFLOWDIR)/sipmodule.mod
	$(RM) sipmodule.mod
	$(LN) $(MODFLOWDIR)/sipmodule.mod sipmodule.mod

de4module.mod: $(MODFLOWDIR)/de4module.mod
	$(RM) de4module.mod
	$(LN) $(MODFLOWDIR)/de4module.mod de4module.mod

gwfhufmodule.mod: $(MODFLOWDIR)/gwfhufmodule.mod
	$(RM) gwfhufmodule.mod
	$(LN) $(MODFLOWDIR)/gwfhufmodule.mod gwfhufmodule.mod

prms_cascade.mod: $(PRMSDIR)/prms_cascade.mod
	$(RM) prms_cascade.mod
	$(LN) $(PRMSDIR)/prms_cascade.mod prms_cascade.mod

prms_snow.mod: $(PRMSDIR)/prms_snow.mod
	$(RM) prms_snow.mod
	$(LN) $(PRMSDIR)/prms_snow.mod prms_snow.mod

prms_intcp.mod: $(PRMSDIR)/prms_intcp.mod
	$(RM) prms_intcp.mod
	$(LN) $(PRMSDIR)/prms_intcp.mod prms_intcp.mod

prms_obs.mod: $(PRMSDIR)/prms_obs.mod
	$(RM) prms_obs.mod
	$(LN) $(PRMSDIR)/prms_obs.mod prms_obs.mod

prms_climatevars.mod: $(PRMSDIR)/prms_climatevars.mod
	$(RM) prms_climatevars.mod
	$(LN) $(PRMSDIR)/prms_climatevars.mod prms_climatevars.mod

prms_flowvars.mod: $(PRMSDIR)/prms_flowvars.mod
	$(RM) prms_flowvars.mod
	$(LN) $(PRMSDIR)/prms_flowvars.mod prms_flowvars.mod

prms_soilzone.mod: $(PRMSDIR)/prms_soilzone.mod
	$(RM) prms_soilzone.mod
	$(LN) $(PRMSDIR)/prms_soilzone.mod prms_soilzone.mod

prms_srunoff.mod: $(PRMSDIR)/prms_srunoff.mod
	$(RM) prms_srunoff.mod
	$(LN) $(PRMSDIR)/prms_srunoff.mod prms_srunoff.mod

prms_set_time.mod:
	$(RM) prms_set_time.mod
	$(CD) $(PRMSDIR);make prms_set_time.mod
	$(LN) $(PRMSDIR)/prms_set_time.mod prms_set_time.mod

prms_basin.mod: $(PRMSDIR)/prms_basin.mod
	$(RM) prms_basin.mod
	$(LN) $(PRMSDIR)/prms_basin.mod prms_basin.mod

gwfupwmodule.mod: $(MODFLOWDIR)/gwfupwmodule.mod
	$(RM) gwfupwmodule.mod
	$(LN) $(MODFLOWDIR)/gwfupwmodule.mod gwfupwmodule.mod

gsfprms2mf.mod: gsflow_prms2mf.o
gsfmf2prms.mod: gsflow_mf2prms.o
gsfmodflow.mod: gsflow_modflow.o
prms_module.mod: gsflow_prms.o

gsflow_prms2mf.o: gsflow_prms2mf.f90 global.mod gwfuzfmodule.mod gwfsfrmodule.mod gwflakmodule.mod prms_obs.mod prms_climatevars.mod prms_flowvars.mod gwfnwtmodule.mod prms_srunoff.mod prms_soilzone.mod gsfmodflow.mod prms_basin.mod
	$(FC) -c $(FFLAGS) gsflow_prms2mf.f90

gsflow_modflow.o: gsflow_modflow.f openspec.inc prms_module.mod $(MFMODDEPN) global.mod gwfbasmodule.mod gwfsfrmodule.mod gwflakmodule.mod gwfuzfmodule.mod gwfhufmodule.mod gmgmodule.mod gwfbcfmodule.mod gwfnwtmodule.mod prms_set_time.mod pcgmodule.mod sipmodule.mod de4module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) gsflow_modflow.f

gsflow_prms.o: gsflow_prms.f90 global.mod
	$(FC) -c $(FFLAGS) gsflow_prms.f90

gsflow_sum.o: gsflow_sum.f90 global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfsfrmodule.mod gsfprms2mf.mod gsfbudget.mod gsfmodflow.mod prms_cascade.mod prms_snow.mod prms_intcp.mod prms_obs.mod prms_module.mod prms_basin.mod prms_flowvars.mod gwflakmodule.mod prms_set_time.mod prms_snow.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) gsflow_sum.f90

gsflow_budget.o: gsflow_budget.f90 gsfprms2mf.mod global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfbcfmodule.mod gwflpfmodule.mod gwfsfrmodule.mod gwfupwmodule.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_srunoff.mod prms_soilzone.mod prms_module.mod gsfmodflow.mod prms_flowvars.mod
	$(FC) -c $(FFLAGS) gsflow_budget.f90

gsflow_mf2prms.o: gsflow_mf2prms.f90 global.mod gwfuzfmodule.mod gwflakmodule.mod gwfsfrmodule.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) gsflow_mf2prms.f90

