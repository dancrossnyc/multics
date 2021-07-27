#  BEGIN MAKEFILE: WSTERM.MAK
#
#  Richard Lee - May 18, 1988
#  Makefile for compose using MICROSOFT MAKE
#
#  The following programs/utilities are needed to make WSTERM.EXE:
#    Lattice C compiler (v 2.15):     LC.COM
#    Macro Assembler:               MASM.EXE
#    Object librarian:            PLIB86.EXE
#    Linker:                        LINK.EXE
#
#  The following include files need to be kept in the current directory:
#     dos.mac
#  The following makefile variables should be assigned appropriate values:
#     SOURCE     = <directory containing all source files>
#     C_INCL     = <directory containing all C includes>
#     MOWSE_INCL = <directory containing all MOWSE includes>
#     WSTERM_INCL= <directory containing all WSTERM includes>
#     C_OBJ      = <pathname for c.obj for linking>
#     DB_OBJ     = <pathname for dblog.obj for debugging>
#     CLIB       = <pathname for C library for linking>
#     MLIB       = <pathname for MOWSE library for linking>
#

SOURCE      = \rich\wsterm\ph6_rel\\

C_INCL      = \lc\\

MOWSE_INCL  = \mowse\i\\

WSTERM_INCL = \rich\wsterm\ph6_rel\\

C_OBJ       = \lc\s\c.obj

DB_OBJ      = dblog.obj

CLIB        = \lc\s\lc.lib

MLIB        = \mowse\o\mowslib.lib

#
# COMPILATION DEFINITIONS
#

.c.obj:
        lc -ms -i$(C_INCL) -i$(MOWSE_INCL) -i$(WSTERM_INCL) -n -b $*

.asm.obj:
        masm $*,$*;

#
# OBJECTS
#

kbdisp.obj:	kbdisp.c wstdefs.h wstglob.h 

kbedit.obj:	kbedit.c wstdefs.h wstglob.h

kbinit.obj:	kbinit.c wstdefs.h 

kbmvcur.obj:	kbmvcur.c wstdefs.h wstglob.h 

kbprim.obj:	kbprim.c wstdefs.h wstglob.h 

kbprocel.obj:	kbprocel.c wstdefs.h wstglob.h 

kbredraw.obj:	kbredraw.c wstdefs.h wstglob.h 

save.obj:		save.asm

status.obj:	status.c wstdefs.h wstglob.h 

tbreak.obj:	tbreak.asm

wstaudit.obj:	wstaudit.c wstdefs.h wstglob.h

wstbkgnd.obj:   wstbkgnd.c wstdefs.h wstglob.h

wstedit.obj:    wstedit.c wstdefs.h wstglob.h

wsterm.obj:     wsterm.c wstdefs.h wstglob.h

wstfrgnd.obj:   wstfrgnd.c wstdefs.h wstglob.h

wstglob.obj:    wstglob.c wstdefs.h

wsthelp.obj:	wsthelp.c wstdefs.h wstglob.h

wsthist.obj:	wsthist.c wstdefs.h wstglob.h wsthist.h 

wstinit.obj:    wstinit.c wstdefs.h

wstkbscr.obj:   wstkbscr.c wstdefs.h wstglob.h

wstkill.obj:	wstkill.c wstkill.h wstdefs.h

wstscrn.obj:	wstscrn.c wstdefs.h wstglob.h

wstutil.obj:    wstutil.c wstglob.h

#
# LIBRARIES
#

kblib.lib:      kbdisp.obj kbedit.obj kbinit.obj kbmvcur.obj kbprim.obj \
                kbprocel.obj kbredraw.obj
	plib86 bu kblib.lib fi kbdisp,kbedit,kbinit,kbmvcur,kbprim,kbprocel,kbredraw

wstlib3.lib:     status.obj wsthelp.obj wstscrn.obj save.obj \
                tbreak.obj wstaudit.obj wsthist.obj wstkill.obj
        plib86 bu wstlib3.lib fi status,wsthelp,wstscrn,save,tbreak,wstaudit,wsthist,wstkill

wstlib1.lib:	wstedit.obj wstutil.obj
        plib86 bu wstlib1.lib fi wstedit.obj,wstutil.obj

wstlib2.lib:    wstbkgnd.obj wstfrgnd.obj wstglob.obj wstinit.obj wstkbscr.obj
        plib86 bu wstlib2.lib fi wstbkgnd,wstfrgnd,wstglob,wstinit,wstkbscr

#
# EXECUTABLE
#

wsterm.exe:     wsterm.obj wstlib3.lib wstlib1.lib wstlib2.lib kblib.lib
        link $(C_OBJ) wsterm,wsterm,,wstlib3+wstlib1+wstlib2+kblib+$(MLIB)+$(CLIB)/map
	mapsym wsterm.map

#   END MAKEFILE: WSTERM.MAK
