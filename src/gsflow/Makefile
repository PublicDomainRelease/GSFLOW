###################################################
# $Id: Makefile 3983 2011-11-23 00:42:21Z rsregan $
###################################################

include ../makelist

LN		= cp

TARGET		= ../../bin/gsflow

LIBDIR		= ../../lib
PRMSLIB		= $(LIBDIR)/libprms.a
MF2K5LIB	= $(LIBDIR)/libmf2k5.a
LIBS 		= -L$(LIBDIR) -lmf2k5 -lprms -lmms $(FLIBS)

PRMSDIR		= ../prms
MFDIR		= ../modflow
MMSINC		= -I$(MMSDIR)/src

###################################################

MODOBJS =	\
		gsflow_prms.o \
		gsflow_modflow.o \
		gsflow_setconv.o \
		gsflow_prms2mf.o \
		gsflow_mf2prms.o \
		gsflow_budget.o \
		gsflow_sum.o

FINCLUDES = openspec.inc $(MFDIR)/openspec.inc
MFMODULES = global.mod gwfbasmodule.mod pcgmodule.mod sipmodule.mod de4module.mod gmgmodule.mod gwfsfrmodule.mod gwfuzfmodule.mod gwflakmodule.mod

MFMODDEPN = $(MFDIR)/global.mod $(MFDIR)/gwfbasmodule.mod $(MFDIR)/pcgmodule.mod $(MFDIR)/sipmodule.mod $(MFDIR)/de4module.mod $(MFDIR)/gmgmodule.mod $(MFDIR)/gwfsfrmodule.mod $(MFDIR)/gwfuzfmodule.mod $(MFDIR)/gwflakmodule.mod

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MF2K5LIB) $(PRMSLIB) $(FINCLUDES) $(MODOBJS) $(LIBDIR)/libmms.a
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) $(MODOBJS) $(LIBS)

clean:
	$(RM) $(TARGET)
	$(RM) *.o openspec.inc *.mod *~

####################################################
# Rules for the modules
###################################################
$(MF2K5LIB):
	$(CD) ../modflow;make

$(PRMSLIB): gsflow_prms.o
	$(CD) ../prms;make

openspec.inc: $(MFDIR)/openspec.inc
	$(RM) openspec.inc
	$(LN) $(MFDIR)/openspec.inc openspec.inc

global.mod: $(MFDIR)/global.mod
	$(RM) global.mod
	$(LN) $(MFDIR)/global.mod global.mod

gwfbasmodule.mod: $(MFDIR)/gwfbasmodule.mod
	$(RM) gwfbasmodule.mod
	$(LN) $(MFDIR)/gwfbasmodule.mod gwfbasmodule.mod

gwfbcfmodule.mod: $(MFDIR)/gwfbcfmodule.mod
	$(RM) gwfbcfmodule.mod
	$(LN) $(MFDIR)/gwfbcfmodule.mod gwfbcfmodule.mod

gwfsfrmodule.mod: $(MFDIR)/gwfsfrmodule.mod
	$(RM) gwfsfrmodule.mod
	$(LN) $(MFDIR)/gwfsfrmodule.mod gwfsfrmodule.mod

gwfnwtmodule.mod: $(MFDIR)/gwfnwtmodule.mod
	$(RM) gwfnwtmodule.mod
	$(LN) $(MFDIR)/gwfnwtmodule.mod gwfnwtmodule.mod

gwflpfmodule.mod: $(MFDIR)/gwflpfmodule.mod
	$(RM) gwflpfmodule.mod
	$(LN) $(MFDIR)/gwflpfmodule.mod gwflpfmodule.mod

gwfuzfmodule.mod: $(MFDIR)/gwfuzfmodule.mod
	$(RM) gwfuzfmodule.mod
	$(LN) $(MFDIR)/gwfuzfmodule.mod gwfuzfmodule.mod

gwflakmodule.mod: $(MFDIR)/gwflakmodule.mod
	$(RM) gwflakmodule.mod
	$(LN) $(MFDIR)/gwflakmodule.mod gwflakmodule.mod

gmgmodule.mod: $(MFDIR)/gmgmodule.mod
	$(RM) gmgmodule.mod
	$(LN) $(MFDIR)/gmgmodule.mod gmgmodule.mod

pcgmodule.mod: $(MFDIR)/pcgmodule.mod
	$(RM) pcgmodule.mod
	$(LN) $(MFDIR)/pcgmodule.mod pcgmodule.mod

sipmodule.mod: $(MFDIR)/sipmodule.mod
	$(RM) sipmodule.mod
	$(LN) $(MFDIR)/sipmodule.mod sipmodule.mod

de4module.mod: $(MFDIR)/de4module.mod
	$(RM) de4module.mod
	$(LN) $(MFDIR)/de4module.mod de4module.mod

gwfhufmodule.mod: $(MFDIR)/gwfhufmodule.mod
	$(RM) gwfhufmodule.mod
	$(LN) $(MFDIR)/gwfhufmodule.mod gwfhufmodule.mod

prms_basin.mod: $(PRMSDIR)/prms_basin.mod
	$(RM) prms_basin.mod
	$(LN) $(PRMSDIR)/prms_basin.mod prms_basin.mod

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

pcgn.mod: $(MFDIR)/pcgn.mod
	$(RM) pcgn.mod
	$(LN) $(MFDIR)/pcgn.mod pcgn.mod

gwfupwmodule.mod: $(MFDIR)/gwfupwmodule.mod
	$(RM) gwfupwmodule.mod
	$(LN) $(MFDIR)/gwfupwmodule.mod gwfupwmodule.mod

gsfprms2mf.mod: gsflow_prms2mf.o
gsfmf2prms.mod: gsflow_mf2prms.o
gsfconvert.mod: gsflow_setconv.o
gsfmodflow.mod: gsflow_modflow.o

gsflow_prms2mf.o: gsflow_prms2mf.f gsfconvert.mod global.mod gwfuzfmodule.mod gwfsfrmodule.mod gwflakmodule.mod prms_obs.mod prms_climatevars.mod prms_flowvars.mod gwfnwtmodule.mod
	$(FC) -c $(FFLAGS) gsflow_prms2mf.f

gsflow_modflow.o: gsflow_modflow.f $(FINCLUDES) $(MFMODDEPN) $(MFMODULES) pcgn.mod gwflakmodule.mod gwfuzfmodule.mod global.mod gwfhufmodule.mod gwfbcfmodule.mod prms_basin.mod prms_soilzone.mod prms_obs.mod gwfnwtmodule.mod
	$(FC) -c $(FFLAGS) gsflow_modflow.f

gsflow_prms.o: gsflow_prms.f global.mod
	$(FC) -c $(FFLAGS) gsflow_prms.f

gsflow_sum.o: gsflow_sum.f global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfsfrmodule.mod gsfconvert.mod gsfprms2mf.mod gsfbudget.mod gsfmodflow.mod prms_cascade.mod prms_snow.mod prms_intcp.mod prms_obs.mod
	$(FC) -c $(FFLAGS) gsflow_sum.f

gsflow_budget.o: gsflow_budget.f gsfconvert.mod gsfprms2mf.mod global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfbcfmodule.mod gwflpfmodule.mod gwfsfrmodule.mod gwfupwmodule.mod
	$(FC) -c $(FFLAGS) gsflow_budget.f

gsflow_setconv.o: gsflow_setconv.f global.mod gwfbasmodule.mod
	$(FC) -c $(FFLAGS) gsflow_setconv.f

gsflow_mf2prms.o: gsflow_mf2prms.f global.mod gsfconvert.mod gwfuzfmodule.mod gwflakmodule.mod gwfsfrmodule.mod
	$(FC) -c $(FFLAGS) gsflow_mf2prms.f

