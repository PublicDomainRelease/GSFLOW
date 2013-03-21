#
# Makefile --
#
# Top-level makefile for the GSFLOW
#
#-------------------------------------------------------------------
# $Id: Makefile 4215 2012-02-23 21:57:14Z rsregan $
#-------------------------------------------------------------------

include ./makelist
MMF      = ./mms
PRMS     = ./prms
GSFLOW   = ./gsflow
MODFLOW  = ./modflow

#
# Standard Targets for Users
#

all: standard

standard:
	cd $(MMF); $(MAKE);
	cd $(MODFLOW); $(MAKE);
	cd $(PRMS); $(MAKE);
	cd $(GSFLOW); $(MAKE)

clean:
	cd $(MMF); $(MAKE) clean;
	cd $(MODFLOW); $(MAKE) clean;
	cd $(PRMS); $(MAKE) clean;
	cd $(GSFLOW); $(MAKE) clean;
	cd $(LIB); rm -f *.a;
	cd $(BIN); rm -f gsflow

