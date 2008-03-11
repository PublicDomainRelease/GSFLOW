###################################################
# $Id: Makefile 2927 2006-11-06 19:52:52Z rsregan $
###################################################

include ../makelist

LN		= cp

TARGET		= ../../bin/gsflow

LIBDIR		= ../../lib
PRMSLIB		= $(LIBDIR)/libprms.a
MF2K5LIB	= $(LIBDIR)/libmf2k5.a
LIBS 		= -L$(LIBDIR) -lmf2k5 -lprms -lmmf $(FLIBS)

PRMSDIR		= ../prms
MFDIR		= ../modflow
MMFINC		= -I$(MMFDIR)/src

###################################################

MODOBJS =	\
		gsflow_prms.o \
		gsflow_modflow.o \
		gsflow_setconv.o \
		gsflow_mf2prms.o \
		gsflow_prms2mf.o \
		gsflow_budget.o \
		gsflow_sum.o

FINCL = fmodules.inc $(PRMSDIR)/fmodules.inc
FINCLUDES = openspec.inc $(MFDIR)/openspec.inc
MFMODULES = global.mod gwfbasmodule.mod pcgmodule.mod sipmodule.mod de4module.mod gmgmodule.mod gwfsfrmodule.mod gwfuzfmodule.mod gwflakmodule.mod gwfhufmodule.mod

MFMODDEPN = $(MFDIR)/global.mod $(MFDIR)/gwfbasmodule.mod $(MFDIR)/pcgmodule.mod $(MFDIR)/sipmodule.mod $(MFDIR)/de4module.mod $(MFDIR)/gmgmodule.mod $(MFDIR)/gwfsfrmodule.mod $(MFDIR)/gwfuzfmodule.mod $(MFDIR)/gwflakmodule.mod $(MFDIR)/gwfhufmodule.mod

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): $(MF2K5LIB) $(PRMSLIB) $(FINCLUDES) $(FINCL) $(MODOBJS) $(LIBDIR)/libmmf.a
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) $(MODOBJS) $(LIBS)

clean:
	$(RM) $(TARGET)
	$(RM) *.o fmodules.inc openspec.inc *.mod *~

####################################################
# Rules for the modules
###################################################
$(MF2K5LIB):
	$(CD) ../modflow;make

$(PRMSLIB):
	$(CD) ../prms;make

fmodules.inc: $(PRMSDIR)/fmodules.inc
	$(RM) fmodules.inc
	$(LN) $(PRMSDIR)/fmodules.inc fmodules.inc

openspec.inc: $(MFDIR)/openspec.inc
	$(RM) openspec.inc
	$(LN) $(MFDIR)/openspec.inc openspec.inc

global.mod: $(MFDIR)/global.mod
	$(RM) global.mod
	$(LN) $(MFDIR)/global.mod global.mod

gwfbasmodule.mod: $(MFDIR)/gwfbasmodule.mod
	$(RM) gwfbasmodule.mod
	$(LN) $(MFDIR)/gwfbasmodule.mod gwfbasmodule.mod

gwfsfrmodule.mod: $(MFDIR)/gwfsfrmodule.mod
	$(RM) gwfsfrmodule.mod
	$(LN) $(MFDIR)/gwfsfrmodule.mod gwfsfrmodule.mod

gwflpfmodule.mod: $(MFDIR)/gwflpfmodule.mod
	$(RM) gwflpfmodule.mod
	$(LN) $(MFDIR)/gwflpfmodule.mod gwflpfmodule.mod

gwfuzfmodule.mod: $(MFDIR)/gwfuzfmodule.mod
	$(RM) gwfuzfmodule.mod
	$(LN) $(MFDIR)/gwfuzfmodule.mod gwfuzfmodule.mod

gwflakmodule.mod: $(MFDIR)/gwflakmodule.mod
	$(RM) gwflakmodule.mod
	$(LN) $(MFDIR)/gwflakmodule.mod gwflakmodule.mod

gwfhufmodule.mod: $(MFDIR)/gwfhufmodule.mod
	$(RM) gwfhufmodule.mod
	$(LN) $(MFDIR)/gwfhufmodule.mod gwfhufmodule.mod

gmgmodule.mod: $(MFDIR)/gmgmodule.mod
	$(RM) gmgmodule.mod
	$(LN) $(MFDIR)/gmgmodule.mod gmgmodule.mod

gwfbcfmodule.mod: $(MFDIR)/gwfbcfmodule.mod
	$(RM) gwfbcfmodule.mod
	$(LN) $(MFDIR)/gwfbcfmodule.mod gwfbcfmodule.mod

pcgmodule.mod: $(MFDIR)/pcgmodule.mod
	$(RM) pcgmodule.mod
	$(LN) $(MFDIR)/pcgmodule.mod pcgmodule.mod

sipmodule.mod: $(MFDIR)/sipmodule.mod
	$(RM) sipmodule.mod
	$(LN) $(MFDIR)/sipmodule.mod sipmodule.mod

de4module.mod: $(MFDIR)/de4module.mod
	$(RM) de4module.mod
	$(LN) $(MFDIR)/de4module.mod de4module.mod

prms_basin.mod: $(PRMSDIR)/prms_basin.mod
	$(RM) prms_basin.mod
	$(LN) $(PRMSDIR)/prms_basin.mod prms_basin.mod

gsfprms2mf.mod: gsflow_prms2mf.o
gsfmf2prms.mod: gsflow_mf2prms.o
gsfconvert.mod: gsflow_setconv.o
gsfmodflow.mod: gsflow_modflow.o

gsflow_prms2mf.o: gsflow_prms2mf.f $(FINCL) gsfconvert.mod global.mod gwfuzfmodule.mod gwfsfrmodule.mod gwflakmodule.mod
	$(FC) -c $(FFLAGS) gsflow_prms2mf.f

gsflow_modflow.o: gsflow_modflow.f $(FINCL) $(FINCLUDES) $(MFMODDEPN) $(MFMODULES) gsfprms2mf.mod gwflakmodule.mod gwfuzfmodule.mod global.mod gwfhufmodule.mod gwfbcfmodule.mod prms_basin.mod
	$(FC) -c $(FFLAGS) gsflow_modflow.f

gsflow_prms.o: gsflow_prms.f $(FINCL) global.mod
	$(FC) -c $(FFLAGS) gsflow_prms.f

gsflow_sum.o: gsflow_sum.f $(FINCL) global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfsfrmodule.mod gsfconvert.mod gsfprms2mf.mod gsfbudget.mod gsfmodflow.mod
	$(FC) -c $(FFLAGS) gsflow_sum.f

gsflow_budget.o: gsflow_budget.f $(FINCL) gsfconvert.mod gsfprms2mf.mod global.mod gwfbasmodule.mod gwfuzfmodule.mod gwfbcfmodule.mod gwflpfmodule.mod gwfsfrmodule.mod
	$(FC) -c $(FFLAGS) gsflow_budget.f

gsflow_setconv.o: gsflow_setconv.f $(FINCL) global.mod gwfbasmodule.mod
	$(FC) -c $(FFLAGS) gsflow_setconv.f

gsflow_mf2prms.o: gsflow_mf2prms.f $(FINCL) global.mod gsfconvert.mod gwfuzfmodule.mod gwflakmodule.mod gwfsfrmodule.mod
	$(FC) -c $(FFLAGS) gsflow_mf2prms.f

