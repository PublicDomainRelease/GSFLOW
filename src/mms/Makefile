#
# Makefile for mms
# markstro
# Tue Jul 26 08:43:10 MST 2005
#
# $Id: Makefile 3116 2011-05-17 16:20:01Z rsregan $
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
