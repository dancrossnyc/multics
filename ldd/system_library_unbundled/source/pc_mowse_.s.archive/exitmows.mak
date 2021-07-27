#  BEGIN MAKEFILE: EXITMOWS.MAK
#
#  Created June 29, 1986 by Flegel
#  Makefile for compose using MICROSOFT MAKE
#   MACROS
#
#  EXITMOWS
#
exitmows.obj: exitmows.c ws_error.h
        lc1 exitmows.c -d -n32 -i\lc\
        lc2 exitmows.q
        
 
exitmows.com: exitmows.obj mowslib.lib
        link \lc\c\c exitmows,exitmows,exitmows,MOWSLIB+\lc\s\lc/map
        exe2bin exitmows exitmows.com
        del exitmows.exe

#   END MAKEFILE: EXITMOWS.MAK
