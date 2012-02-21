#
# Makefile for mms
# markstro
# Tue Jul 26 08:43:10 MST 2005
#
# $Id: Makefile 2155 2010-11-19 19:22:06Z rsregan $
#

include ../makelist
MLIBDIR  = ../../lib

SRCS = 	gsflow_main.c mms_util.c

MMSOBJS = ${SRCS:.c=.o}

.c.o:
	$(CC) $(CFLAGS) -c $<

#
# Private makefile defines
#
MMSLIB = $(MLIBDIR)/libmms.a

#
# Standard Targets for Users
#

$(MMSLIB): $(MMSOBJS)
	$(AR) $(MMSLIB) $(MMSOBJS)
	$(RANLIB) $(MMSLIB)

clean:
	${RM} $(MMSLIB) *.o *.ln *~ *.a ${OBJS} core lint \#*
