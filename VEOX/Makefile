#             Makefile for STP client
#  There are two versions of the STP client that can be made:
#        1) standard version
#	 2) line-edit version - this version some limited editted and
#           history of the input lines in the style of EMACS
#
STP_SOURCES	= VEOXstp.c
EDIT_DEFS	= -DANSI_ARROWS -DHAVE_TCGETATTR -DHIDE -DUSE_DIRENT -DSYS_UNIX
EDIT_SOURCES	= editline.c complete.c sysunix.c
SUN_LIBS	= -lresolv -lsocket -lnsl
LINUX_LIBS	= -lresolv
HEADERS		= editline.h unix.h

VEOXstp:	VEOXstp.c
	cc -DEDITLINE $(EDIT_DEFS) -o VEOXstp $(STP_SOURCES) $(EDIT_SOURCES) $(SUN_LIBS)

VEOXstp-noedit:	VEOXstp.c
	cc -o VEOXstp $(STP_SOURCES) $(SUN_LIBS)

linux_VEOXstp-noedit:	VEOXstp.c
	cc -o VEOXstp $(STP_SOURCES) $(LINUX_LIBS)

linux_VEOXstp:	VEOXstp.c
	cc -DEDITLINE $(EDIT_DEFS) -o VEOXstp  $(STP_SOURCES) $(EDIT_SOURCES) $(LINUX_LIBS)

VEOXsubs:
	gcc -g testVEOXsubs.c VEOXsubs.c $(LINUX_LIB)

tar:
	tar cvf VEOXstp.tar README Makefile test $(STP_SOURCES) $(EDIT_SOURCES) $(HEADERS)
##  $Revision: 1.1.1.1 $
##
##  Unix makefile for editline library.
##

##  Set your options:
##	-DANSI_ARROWS		ANSI arrows keys work like emacs.
##	-DHAVE_STDLIB		Have <stdlib.h>.
##	-DHAVE_TCGETATTR	Have tcgetattr(), tcsetattr().
##	-DHIDE			Make static functions static (non debug).
##	-DHIST_SIZE=n		History size.
##	-DNEED_STRDUP		Don't have strdup().
##	-DUNIQUE_HISTORY	Don't save command if same as last one.
##	-DUSE_DIRENT		Use <dirent.h>, not <sys/dir.h>?
##	-DUSE_TERMCAP		Use the termcap library for terminal size
##				see LDFLAGS, below, if you set this.
##	-DNEED_PERROR		Don't have perror() (used in testit)
DEFS	= -DANSI_ARROWS -DHAVE_TCGETATTR -DHIDE -DUSE_DIRENT -DSYS_UNIX

##  Set your C compiler:
WARN	= -Wall -Wshadow -Wpointer-arith -Wcast-qual -Wwrite-strings \
	-Wunused -Wcomment -Wswitch
CC	= gcc -ansi $(WARN)
#CC	= cc
CFLAGS	= $(DEFS) -O -g

##  If you have -DUSE_TERMCAP, set this as appropriate:
#LDFLAGS = -ltermlib
#LDFLAGS = -ltermcap
