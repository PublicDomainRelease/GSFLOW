#
# Makefile --
#
# Top-level makefile for the GSFLOW
#
#-------------------------------------------------------------------
# $Id: Makefile 1996 2006-03-06 19:39:43Z rsregan $
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: standard

standard:
	cd mms; $(MAKE);
	cd modflow; $(MAKE);
	cd prms; $(MAKE);
	cd gsflow; $(MAKE);

clean:
	cd mms; $(MAKE) clean;
	cd modflow; $(MAKE) clean;
	cd prms; $(MAKE) clean;
	cd gsflow; $(MAKE) clean;
	cd ../lib; rm -f *.a;
	cd ../bin; rm -f gsflow

