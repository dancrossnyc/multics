/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-06-13,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Created.
  2) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  3) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  4) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Separated input and output parameters to int86() to avoid confusion
     when parameters are used.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
**********************************************************************

  Routine:            GETKEY 

  Function:
      This routine will fetch a keyboard key press. A flag is 
  specified to determine whether the routine should wait blocked if 
  no key was hit or whether to return -1. 

  Parameters:
     (input)          block_flag - if this value is TRUE, the routine 
                          will wait in a blocked state for input; if 
                          false, the routine will return immediately 
                          with an error code if there is not keyboard 
                          key hit 

  Returns:            -1 if 'block_flag' is FALSE and no keyboard key 
                          was pressed 
                      the ASCII character entered from the keyboard, 
                          if otherwise successful 

**********************************************************************/

getkey(block_flag)
int block_flag;
{
    union REGS reg, outreg;
    int cpu_flags;
    int code;

    /* break key hit buffered? */
    if (break_flag) {
        break_flag = FALSE;
        return(BREAK_KEY);
    }

    /* if not block, just do a quick check to see if a hit was hit */
    if (!block_flag) {
        reg.h.ah = BIOS_KB_STATUS;
        cpu_flags = int86(BIOS_KB,&reg,&outreg);
        if (cpu_flags & Z_FLAG_MASK)
            return(-1);
    }

    /* fetch a key, waiting if necessary */
    reg.h.ah = BIOS_KB_READ;
    int86(BIOS_KB,&reg,&outreg);

    /* break key hit after waiting blocked? */
    if (break_flag) {
        break_flag = FALSE;
        return(BREAK_KEY);
    }

    /* if not an extended ASCII code, just return the ASCII code */
    if (outreg.h.al)
        return((int)outreg.h.al);

    /* encode to get a unique extended ASCII value */
    code = ASCII_EXTEND_CODE + outreg.h.ah;
    return(code);
}



/*
**********************************************************************

  Routine:            SET_CURSOR 

  Function:
      This routine moves the physical cursor (the cursor on the 
  screen) to a specified screen coordinate. 

  Parameters:
     (input)          row - specifies which row (with 0 being the top 
                          of the screen) to move to 
     (input)          col - specifies which column (with 0 being the 
                          left of the screen) to move to 

  Returns:            NONE 

  Note 1 - This routine is called by routines other than edit mode 
      keyboard routines. 
**********************************************************************/

set_cursor(row,col)
int row;
int col;
{
    union REGS reg, outreg;

    reg.h.ah = VIDEO_CURSOR_MOVE_FUNC;   /* VIDEO cursor set */
    reg.h.dh = row;
    reg.h.dl = col;
    reg.h.bh = get_active_page();
    int86(BIOS_VIDEO,&reg,&outreg);
}



/*
**********************************************************************

  Routine:            SCROLL_DISPLAY 

  Function:
      This routine scrolls a particular region on the screen one line 
  up, filling the bottom line of the region with a blank line. 

  Parameters:         NONE

  Returns:            NONE 

**********************************************************************/

scroll_display()
{
    union REGS reg,outreg;

    reg.h.ah = VIDEO_SCROLL_UP_FUNC;    /* scroll up function */
    reg.h.al = DEFAULT_LINES_TO_SCROLL; /* number of lines to scroll up */
    reg.h.ch = CURSOR_HOME_ROW;   /* upper left row coordinate to scroll */
    reg.h.cl = CURSOR_HOME_COL;   /* upper left col coordinate to scroll */
    reg.h.dh = MAX_SCREEN_LINE;   /* lower right row coordinate to scroll */
    reg.h.dl = MAX_SCREEN_COL;    /* lower right row coordinate to scroll */
    reg.h.bh = wst_fg_screen.attr;      /* attribute of blank scroll line */
    int86(BIOS_VIDEO,&reg,&outreg);
}



/*
**********************************************************************

  Routine:            DELAY

  Function:
      This function delays for several milliseconds in a tight loop.
  The DOS time function is called to get the current time in hundredth
  of seconds. The resolution of the clock is typically 18.2 ticks
  per second so this routine delays for between 1 and 2 ticks.
  A limit is placed on the number of iterations in the event that
  the clock does not support hundredth of seconds.

  Parameters:         NONE

  Returns:            NONE 

**********************************************************************/

delay ()
{   
    int current_hundredth;
    union REGS reg;
    int ticks;
    int iterations;

    ticks = 0;
    current_hundredth = -1;

    /* limit iterations to 500 */
    for (iterations = 0; iterations < 500; iterations++) {

        /* get system time */
        reg.h.ah = SYSTEM_TIME;
        intdos (&reg, &reg);

        /* clock changed? */
        if (current_hundredth != reg.h.dl) {
            current_hundredth = reg.h.dl;

            /* tally and check for timeout */
            ticks++;
            if (ticks >= 2) break;
        }
    }
}
