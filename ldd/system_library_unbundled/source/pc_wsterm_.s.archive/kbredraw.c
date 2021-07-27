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
     phx21233 - Changed to display only echoed input characters when redrawing
     input line.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
**********************************************************************

  Routine:            ERASE_EDIT_LINE 

  Function:
      This routine erases the portion of the screen which displays 
  the input line; the contents and the state of the line buffer 
  itself is not changed. 

  Parameters:
     (input)          line - pointer to the structure containing the 
                          line (buffer) and information about the 
                          line being edited in edit mode 

  Returns:            NONE 

**********************************************************************/

erase_edit_line(line)
EDIT_LINE *line;
{
    int row;
    int col;

    /* determine if input line displayed has scrolled off the top */
    if (line->scrolled_flag) {
        /* yes, start at beginning of display */
        row = ss.top;
        col = ss.left;
    }

    /* did not scroll off top */
    else {
        /* start at beginning line coordinates */
        row = line->orig_row;
        col = line->orig_col;
    }

    /* update physical location to starting coordinates for erase */
    line->cur_row = row;
    line->cur_col = col;
    set_cursor(row,col);

    /* blank pad the screen until ending input line coordinates reached */
    while (row < line->max_row) {
        if (col > ss.right) {
            col = ss.left;
            row++;
            set_cursor(row,col);
        }
        else {
            putch(' ');
            col++;
        }
    }

    if (row == line->max_row) {
        while (col <= line->max_col) {
            putch(' ');
            col++;
        }
    }

    /* move physical cursor back to current line position */
    set_cursor(line->cur_row,line->cur_col);

    /* ending screen coordinates now just the current line coordinates */
    line->max_row = line->cur_row;
    line->max_col = line->cur_col;
}



/*
**********************************************************************

  Routine:            REDRAW_EDIT_LINE 

  Function:
      This routine redisplays the edit mode line. Its function is to 
  allows redrawing of the edited line at another location. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 

  Returns:            TRUE always 

  Note 1 - The cursor position fields are updated. 
  Note 2 - The user may be in the middle of entering a literal escape 
      sequence (e.g. \030) when this routine is called so care must 
      be taken to identify and replicate the way the line was 
      displayed for this case as well. 
**********************************************************************/

redraw_edit_line(line)
EDIT_LINE *line;
{
    CURPOS tpos;
    int pos;
    int i;

    /* initialize structure for keeping track of cursor */
    init_temp_curpos(&tpos,line);

    /* redisplay from beginning of line until current index position */
    for (i = 0; i < line->index; i++)
        /* phx21233 R.L. - redisplay and re-initialize only echoed input */
        if (line->size[i])
            line->size[i] = display_char_prim(&tpos,line->line[i]);

    /* in the middle of handling literal escape sequence? */
    if (line->escape_flag == KB_LITERAL_ESC && line->literal_dex > 0) {

        /* if so, display the portion of the literal escape sequence */
        display_char(&tpos,line->line[i]);

        /* finish redraw at next character position */
        pos = i + 1;
        for (i = 0; i < line->literal_dex; i++)
            display_char(&tpos,line->literal_buff[i]);
    }

    /* not processing literal escape, just finish redraw at current position */
    else
        pos = i;

    /* update the cursor coordinate information */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* redisplay rest of line (or until end of screen reached) */
    redisplay(line,pos,&(line->max_row),&(line->max_col));
    return(TRUE);
}

