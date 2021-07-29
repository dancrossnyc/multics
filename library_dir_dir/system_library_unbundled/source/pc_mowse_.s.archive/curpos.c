/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-09-05,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (get_cursor_pos)

Return the absolute coordinates of the cursor in x,y
*/

#include <dos.h>
#include <emulator.h>

#define VIDEO_IO    0x10
#define GET_CURSOR  0x03
#define GET_WIDTH   0x0F

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

get_cursor_pos(x,y)

int *x;
int *y;
{
union REGS inregs;
union REGS outregs;

   inregs.h.ah = GET_CURSOR;
   inregs.h.bh = 0;

   int86(VIDEO_IO,&inregs,&outregs);

   *x = outregs.h.dh;
   *y = outregs.h.dl;
}

/* : PROCEDURE FUNCTION (get_screen_width)

Return the width of screen in columns
*/

get_screen_width()
{
union REGS inregs;
union REGS outregs;
int screen_width;

   inregs.h.ah = GET_WIDTH;
   int86(VIDEO_IO,&inregs,&outregs);
   screen_width = outregs.h.ah;

   return(screen_width);
}

#else
get_cursor_pos()
{}
get_screen_width()
{}
#endif
