#
# Makefile for MMF
# markstro
# Tue Jul 26 08:43:10 MST 2005
#
# $Id: Makefile 4215 2012-02-23 21:57:14Z rsregan $
#

include ../makelist

SRCS = 	gsflow_main.c mms_util.c

MMFOBJS = ${SRCS:.c=.o}

.c.o:
	$(CC) $(CFLAGS) -c $<

#
# Private makefile defines
#
MMFLIB = $(LIBDIR)/libmmf.a

#
# Standard Targets for Users
#

$(MMFLIB): $(MMFOBJS)
	$(AR) $(MMFLIB) $(MMFOBJS)
	$(RANLIB) $(MMFLIB)

clean:
	$(RM) $(MMFLIB) *.o *.ln *~ *.a
