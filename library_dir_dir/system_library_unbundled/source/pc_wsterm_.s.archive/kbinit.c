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
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
**********************************************************************

  Routine:            INIT_KB_SCREEN 

  Function:
      This routine initializes a structure used by the edit mode 
  editing routines which specifies the size of the display screen. 

  Parameters:
     (input)          rows - specifies the height of the screen in 
                          rows 
     (input)          cols - specifies the width of the screen in 
                          columns 

  Returns:            NONE 

**********************************************************************/

init_kb_screen(rows,cols)
int rows;  /* screen height in rows */
int cols;  /* screen width in columns */
{
    ss.top = CURSOR_HOME_ROW;   /* top screen edge coordinate */
    ss.left = CURSOR_HOME_COL;  /* left screen edge coordinate */
    ss.bottom = rows-1;         /* bottom screen edge coordinate */
                                /* (subtract 1 to get coordinate value) */
    ss.right = cols-2;          /* right screen edge coordinate */
                                /* (subtract 1 to get coordinate value */
                                /* and another to prevent last screen */
                                /* column to be written since it may */
                                /* automatic cursor advancement or  */
                                /* scrolling which is not desirable) */
}


/*
**********************************************************************

  Routine:            INIT_LINE 

  Function:
      This routine initializes the structure containing the line 
  being edited and information about the line in edit mode. The 
  values that are used for initializing will indicate an empty line 
  being edited. 

  Parameters:
     (input/output)   line - a pointer to the structure which 
                          contains the line and information about the 
                          line being edited 

  Returns:            NONE 

**********************************************************************/

init_line(line)
EDIT_LINE *line;
{

    line->mode = KB_REPLACE_MODE;     /* insert/replace mode is replace */
    line->orig_row = CURSOR_HOME_ROW; /* assume starting at top row */
    line->orig_col = CURSOR_HOME_COL; /* assume starting at leftmost column */
    line->length = 0;                 /* line length is 0 (empty line) */
    line->index = 0;                  /* logical cursor position is 0 */
    line->literal_dex = 0;         /* count of numeric digits for escape arg */
    line->line[0] = NUL_TERMINATOR;   /* NUL terminate keyboard buffer */
    line->escape_arg = NO_ESC_ARG;    /* initialize escape argument counter */
    line->escape_flag = NO_ESC_FUNC;  /* initialize escape/literal key flag */
}


/*
**********************************************************************

  Routine:            INIT_CURPOS 

  Function:
      This routine initializes the cursor position fields of the 
  structure containing the edit mode line. The position is obtained 
  by fetching the current location of the physical cursor. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line being edited in edit mode 

  Returns:            NONE 

**********************************************************************/

init_curpos(line)
EDIT_LINE *line;
{
    /* get cursor position as starting cursor coordinates */
    cursor_pos(&(line->orig_row),&(line->orig_col));

    /* assume empty line, finishing coordinates same as current
       coordinates same as starting coordinates
    */
    line->cur_row = line->max_row = line->orig_row;
    line->cur_col = line->max_col = line->orig_col;

    /* empty line, keyboard display did not scroll */
    line->scrolled_flag = FALSE;

    /* empty line, nothing in keyboard buffer beyond what is displayed
       at bottom of screen
    */
    line->off_screen = FALSE;
}


/*
**********************************************************************

  Routine:            INIT_TEMP_CURPOS 

  Function:
      This routine initializes a structure used for temporarily 
  keeping track of cursor positioning. The values used for 
  initialization are taken from the structure containing the edit 
  mode line and line information. 

  Parameters:
     (input/output)   pos - pointer to the structure for temporarily 
                          keeping track of cursor position 
     (input)          line - pointer to the structure containing the 
                          line and line information for the edit mode 
                          line 

  Returns:            NONE 

**********************************************************************/

init_temp_curpos(pos,line)
CURPOS *pos;
EDIT_LINE *line;
{
    pos->orig_row = line->orig_row;    /* get starting coordinates */
    pos->orig_col = line->orig_col;

    pos->cur_row = line->cur_row;       /* get current coordinates */
    pos->cur_col = line->cur_col;

    pos->max_row = line->max_row;       /* get ending coordinates */
    pos->max_col = line->max_col;

    pos->scroll_flag = FALSE;
}

