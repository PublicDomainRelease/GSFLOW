#
# Makefile --
#
# Top-level makefile for the GSFLOW
#
#-------------------------------------------------------------------
# $Id: Makefile 7493 2015-07-17 16:20:50Z rsregan $
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

