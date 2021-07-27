#  BEGIN MAKEFILE: BFT.MAK
#
#  HISTORY COMMENTS:
# 1) change(88-08-16,Flegel), approve(88-09-07,MCR7978),
#    audit(88-09-14,Lee):
#    Created for clarity and transportability.
#                                                  END HISTORY COMMENTS
#
#  Makefile for compose using MICROSOFT MAKE MACROS
#

#
# DEFINITIONS
#

INCL    = -iC:\lc\ -iC:\include\mowse\ -iC:\include\bft\\

LIBS    = C:\lib\mowse\mowslib+C:\lc\s\lc

HEADER  = C:\lc\c\c

#
# MACROS
#

.c.obj:
     lc1 $*.c  $(INCL) -d -n32
     lc2 $*.q

#
# OBJECT
#

bft.obj:      bft.c bft.h

subrs.obj:    subrs.c bft.h

bft_load.obj: bft_load.c bft.h

bfterror.obj: bfterror.c bft.h

#
# EXECUTABLE
#

bft.com:      bft.obj subrs.obj bfterror.obj
     link $(HEADER) bft subrs bfterror,bft,bft,$(LIBS)
     exe2bin bft bft.com
     del bft.exe

bft_load.com: bft_load.obj subrs.obj bfterror.obj
     link $(HEADER) bft_load subrs bfterror,bft_load,bft_load,$(LIBS)
     exe2bin bft_load bft_load.com
     del bft_load.exe

#  END MAKEFILE: BFT.MAK
