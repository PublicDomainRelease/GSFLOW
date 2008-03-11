#
# Makefile for mmf
# markstro
# Tue Jul 26 08:43:10 MST 2005
#
# $Id: Makefile 2939 2006-11-16 21:28:00Z markstro $
#

MLIBDIR  = ../../lib

CC       = gcc
RM       = rm -f
AR       = ar ruv
RANLIB   = ranlib

#CFLAGS   = -g -Wall -Werror-implicit-function-declaration
#CFLAGS   = -O -Wall -Werror-implicit-function-declaration
CFLAGS   = -O3 -Wall -Werror-implicit-function-declaration

SRCS = 	gsflow_main.c mms_util.c


MMSOBJS = ${SRCS:.c=.o}

.c.o:
	$(CC) $(CFLAGS) -c $<

#
# Private makefile defines
#
MMSLIB = $(MLIBDIR)/libmmf.a

#
# Standard Targets for Users
#

$(MMSLIB): $(MMSOBJS)
	$(AR) $(MMSLIB) $(MMSOBJS)
	$(RANLIB) $(MMSLIB)

clean:
	${RM} $(MMSLIB) *.o *.ln *~ *.a ${OBJS} core lint \#*
