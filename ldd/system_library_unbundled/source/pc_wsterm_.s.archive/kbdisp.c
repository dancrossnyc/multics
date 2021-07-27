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
     phx21233 - Converted display_char() to display_char_prim() for handling
     primitive display function; new display_char() added which calls the
     primitive and displays only when keyboard echoing is enabled. Added
     routine get_echoed_input() for extracting only echoed input from line
     buffer.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
***********************************************************************

  Routine:            DISPLAY_CHAR_STR 

  Function:
      This routine returns the display string for a character 
  displayed in line editing. In particular, it determines how 
  non-printable characters are displayed. 

  Parameters:
     (input)          ch - specifies the ASCII character being 
                          displayed 

  Returns:            a pointer to a static string containing the 
                          displayed form of the ASCII character 

**********************************************************************/

char *display_char_str(ch)
int ch;  /* character to display */
{
    static char display_string[MAX_DISPLAY_CHAR_SIZE+1];     /* string to pass back result */
    int octval;

    /* check for non-printable control character */
    if (ch < MIN_PRINTABLE_ASCII) {
        octval = ch;
        display_string[0] = '\\';
        display_string[3] = (ch & 07) + '0';
        ch >>= 3;
        display_string[2] = (ch & 07) + '0';
        ch >>= 3;
        display_string[1] = (ch & 07) + '0';
        display_string[4] = NUL_TERMINATOR;
    }

    /* check for printable ASCII */
    else if (ch < ASCII_DEL) {
        display_string[0] = ch;
        display_string[1] = NUL_TERMINATOR;
    }

    /* check for ASCII DEL character */
    else if (ch == ASCII_DEL) {
        strcpy(display_string,"\\177");
    }

    /* extended ASCII character */
    else {
        sprintf(display_string,"<%d>", ch);
    }

    return(display_string);
}


/*
***********************************************************************

  Routine:            DISPLAY_TAB_STR 

  Function:
      This routine returns a string filled with enough blanks to pad 
  to the next tab column. 

  Parameters:
     (input)          cur_col - specifies the current column on the 
                          screen to tab from 

  Returns:            a pointer to a static string containing enough 
                          spaces to pad to the next tab position on 
                          the screen 

**********************************************************************/

char *display_tab_str(cur_col)
int cur_col;  /* specifies current cursor column coordinate */
{
    static char display_string[TAB_SIZE+1];  /* result string to pass back */
    int n_spaces;                       /* number of spaces to pad */
    register int i;                     /* working index */

    /* if column exceeds right screen edge, assume wrap around to
       leftmost edge on next line
    */
    if (cur_col >= ss.right)
        n_spaces = TAB_SIZE;

    /* handle current column within screen */
    else {
        /* calculate padding to next tab column */
        n_spaces = TAB_SIZE - ((cur_col - ss.left) % TAB_SIZE);

        /* if exceeds right edge, truncate to get to leftmost
           column on next line; this handles the case where the
           screen width is not a multiple of the tab size and
           guarantees wrap around to the leftmost column
        */
        if (cur_col + n_spaces >= ss.right)
            n_spaces = ss.right - cur_col + 1;
    }

    /* initialize the string with the correct number of spaces */
    for (i = 0; i < n_spaces; i++)
        display_string[i] = ' ';

    /* null-terminate the string */
    display_string[i] = NUL_TERMINATOR;

    return(display_string);
}


/*
***********************************************************************

  Routine:            DISPLAY_CHAR_PRIM

  Function:
      This routine displays a character entered from the keyboard in 
  an appropriate representation. It keeps track of cursor position, 
  line wrap arounds and scrolling. 

  Parameters:
     (input/output)   pos - pointer to a structure which maintains 
                          cursor position information; information is 
                          updated after the display 
     (input)          ch - specifies the ASCII character to display 

  Returns:            The number of characters displayed to represent 
                          the keyboard character 

**********************************************************************/

display_char_prim(pos,ch)
CURPOS *pos;   /* pointer to structure for keeping track of cursor position */
int ch;        /* character to display */
{
    char *display_char_str();
    char *ptr;            /* pointer to display string */
    int  display_length;  /* holds length of display string */

    /* character is the TAB character ? */
    if (ch == TAB)
        /* get the string to display for the tab */
        ptr = display_tab_str(pos->cur_col);

    /* not tab, get the string to display for the character */
    else
        ptr = display_char_str(ch);

    /* determine the length of the display string */
    display_length = strlen(ptr);

    /* display the display string, keeping track of cursor position */
    while (TRUE) {

        /* cursor column past right screen edge */
        if (pos->cur_col > ss.right) {

            /* wrap around to left edge */
            pos->cur_col = ss.left;

            /* at bottom of screen? */
            if (pos->cur_row >= ss.bottom) {

                /* scroll and tally the scroll */
                pos->scroll_flag++;
                wst_scroll();
            }

            /* not at bottom of screen, at next line */
            else
                pos->cur_row++;

            /* update the physical cursor */
            set_cursor(pos->cur_row,pos->cur_col);
        }

        /* if NUL terminator reached, exit print loop */
        if (!*ptr) break;

        /* display the character */
        putch(*ptr);

        /* point to next character in display string */
        ptr++;

        /* move the cursor forward */
        pos->cur_col++;

    }

    return(display_length);
}


/*
***********************************************************************

  Routine:            REDISPLAY 

  Function:
      This routine redisplays the edited line from a particular 
  position in the line until the end or the end of the screen is 
  reached, whichever is reached first. The fields in the line 
  structure passed to this routine are not directly altered. 

  Parameters:
     (input)          line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          pos - specifies what position in the line to 
                          begin redisplaying 
     (output)         max_row - specifies the integer to pass back 
                          the ending row coordinate after the 
                          redisplay 
     (output)         max_col - specifies the integer to pass back 
                          the ending column coordinate after the 
                          redisplay 

  Returns:            NONE 

  Note 1 - It is assumed that the physical cursor on the screen is at 
      the line position specified by the parameter 'pos'. 
**********************************************************************/

redisplay(line,pos,max_row,max_col)
EDIT_LINE *line;
int pos;
int *max_row;
int *max_col;
{
    char *display_char_str();
    char *display_tab_str();
    char *ptr;
    int row;
    int col;
    int i;
    int tmp_row;
    int tmp_col;

    /* get a working copy of current cursor row and column coordinates */
    row = line->cur_row;
    col = line->cur_col;

    /* check that position in line buffer to start redisplay is valid */
    if (pos >= 0 && pos < line->length) {

        /* display each character until end of line */
        for (i = pos; i < line->length; i++) {

            /* check if character is a tab */
            if (line->line[i] == TAB)
                /* get tab string and update size of tab */
                ptr = display_tab_str(col);

            /* otherwise just get character string */
            else
                ptr = display_char_str(line->line[i]);

            /* phx21233 R.L. - size of 0 means character not echoed and */
            /*      should not be displayed  */
            if (line->size[i])
                line->size[i] = strlen(ptr);

            else
                continue;

            /* display character string until NUL terminator reached */
            while (*ptr) {

                /* check for wrap around past right edge of screen */
                if (check_line_wrap(line,&row,&col,max_row,max_col) < 0)
                    return;

                /* display the current character in the string */
                putch(*ptr);

                /* point to next character in the string */
                ptr++;

                /* increment cursor position */
                col++;

                /* check if a text key was hit */
                if (stop_display(line)) {

                    /* stop redisplay routine because another character
                       has been entered from the keyboard which will
                       call redisplay again to finish updating the screen
                    */
                    set_cursor(line->cur_row,line->cur_col);
                    return;
                }
            }
        }

        /* check for wrap around past right edge of screen */
        if (check_line_wrap(line,&row,&col,max_row,max_col) < 0)
            return;
    }

    /* save ending coordinates (coordinates of where line
       displayed ends)
    */
    tmp_row = row;
    tmp_col = col;

    /* reached the end of line, but must pad to ending
       coordinates with blanks to erase leftover characters
    */
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

    /* update the physical location of the cursor */
    set_cursor(line->cur_row,line->cur_col);
    *max_row = tmp_row;
    *max_col = tmp_col;
}



/*
***********************************************************************

  Routine:            CHECK_LINE_WRAP

  Function:
      This routine checks to see if the cursor has gone past the
  defined right edge of the screen and moves the cursor to the left
  most column of the next line. If the line is wrapped at the bottom
  most line of the defined screen area, a flag in the line structure
  is set and the maximum row and col coordinates of the screen are
  past back to the caller.

  Parameters:
     (input)          line - pointer to the structure containing the 
                          line and information about the line being 
                          edited
     (input/output)   row - contains the address of the variable containing
                          the current row coordinates of the cursor
     (input/output)   col - contains the address of the variable containing
                          the current column coordinates of the cursor
     (output)         max_row - this variable is initialized to the
                          maximum screen row coordinate if the line
                          wraps on the last row
     (output)         max_col - this variable is initialized to the
                          maximum screen column coordinate if the
                          line wraps on the last row

  Returns:             0 if screen not completely filled 
                      -1 if the line extends beyond the end of the
                         screen

**********************************************************************/

check_line_wrap(line,row,col,max_row,max_col)
EDIT_LINE *line;
int *row;
int *col;
int *max_row;
int *max_col;
{
    /* check for wrap around past right edge of screen */
    if (*col > ss.right) {
        /* wrap around to left side of screen */
        *col = ss.left;

        /* reached bottom of screen? */
        if (*row >= ss.bottom) {

            /* update max row and col variables */
            *max_row = ss.bottom;
            *max_col = ss.right;

            /* flag not all characters displayed */
            line->off_screen = TRUE;

            /* restore physical cursor position */
            set_cursor(line->cur_row,line->cur_col);
            return(-1);
        }

        /* not bottom of screen, go to next line */
        else
            (*row)++;

        /* update location of physical cursor */
        set_cursor(*row,*col);
    }
    return(0);
}


/*
***********************************************************************

  Routine:            STOP_DISPLAY 

  Function:
      This routine is used to optimize screen updates by checking to 
  see if updating of the screen may be stopped before it is 
  completed. This is possible if another key is detected that 
  guarantees the redisplay routine will be called again to update the 
  screen. 

  Parameters:
     (input)          line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 

  Returns:            TRUE if a key was detected that guarantees a 
                          subsequent screen update 
                      FALSE otherwise 

**********************************************************************/

stop_display(line)
EDIT_LINE *line;
{
    int ch;

    /* don't stop update if no key hit or key is not ASCII */
    if ((ch = checkkey()) < 0 || ch > ASCII_EXTEND_CODE) return(FALSE);

    switch (ch) {
        /* kill to beginning of line will update if not already at  */
        /* beginning of line                                        */
        case '@':
            if (line->index > ZERO_INDEX_VALUE) return(TRUE);
            return(FALSE);

        /* literal escape or escape does not guarantee subsequent   */
        /* redisplay                                                */
        case '\\':
        case ESC:
            return(FALSE);

        /* backspace and delete key redisplays if a character is    */
        /* deleted                                                  */
        case BACKSPACE:
        case DEL_KEY:
            if (line->index > ZERO_INDEX_VALUE) return(TRUE);
            return(FALSE);

        /* delete character key redisplays if cursor is before end  */
        /* of line                                                  */
        case CTRL_D:
            if (line->index < line->length) return(TRUE);
            return(FALSE);

        /* kill to end of line redisplays if not already at end of  */
        /* line                                                     */
        case CTRL_K:
            if (line->index < line->length) return(TRUE);
            return(FALSE);

        default:

            /* if not a tab and not a printable character, may not  */
            /* force a redraw                                       */
            if (ch != TAB &&
                (ch < MIN_PRINTABLE_ASCII || ch > MAX_PRINTABLE_ASCII))
                return(FALSE);

            /* a printable character; if in replace mode, redraws   */
            /* if not at end of line                                */
            if (line->mode) {
                if (line->length < MAX_LINE_SIZE) return(TRUE);
                return(FALSE);
            }

            /* in insert mode, redraws if maximum line size has     */
            /* not been reached                                     */
            else {
                if (line->index < MAX_LINE_SIZE) return(TRUE);
                return(FALSE);
            }
    }
}


/*
***********************************************************************

  Routine:            GET_ECHOED_INPUT

  Function:
      This routine is used to extract echoed input from a specified
  part of the edit-mode input line buffer. Only characters echoed will
  be extracted. The results are passed back in a static string, along
  with the length of the extracted input item.
  
  Parameters:
     (input)          line - pointer to the structure containing the 
                          line and information about the line being 
                          edited
     (input)          item_pos - index to position in line buffer
                          to begin extracting
     (input)          item_size - number of characters in line buffer
                          from which to extract
     (output)         display_str - pointer to the string extracted
     (output)         display_len - length of string extract

  Returns:            None

**********************************************************************/

get_echoed_input(line,item_pos,item_size,display_str,display_len)
EDIT_LINE *line;
int item_pos;
int item_size;
char **display_str;
int *display_len;
{
    static char echoed_str[MAX_LINE_SIZE+1];
    register int i;
    register int count;

    /* ensure specified position for extraction is valid */
    if (item_pos > line->length) item_pos = line->length;
    else if (item_pos < 0) item_pos = 0;

    /* ensure specified number of chars for extraction is valid */
    if (item_pos+item_size > line->length)
        item_size = line->length - item_pos;

    /* tally and copy over the echoed input characters from input buffer */
    count = 0;
    for (i = item_pos; i < item_size; i++)
        /* if represented by at least 1 character */
        if (line->size[i] > 0)
            echoed_str[count++] = line->line[i];
    echoed_str[count] = 0;

    /* pass back the results */
    *display_str = echoed_str;
    *display_len = count;
}



/*
***********************************************************************

  Routine:            DISPLAY_CHAR

  Function:
      This routine displays a character entered from the keyboard in 
  an appropriate representation by calling display_char_prim() if
  keyboard echoing is enabled. If echoing is disabled, nothing is
  displayed and 0 is returned.

  Parameters:
     (input/output)   pos - pointer to a structure which maintains 
                          cursor position information; information is 
                          updated after the display 
     (input)          ch - specifies the ASCII character to display 

  Returns:            The number of characters displayed to represent 
                          the keyboard character if keyboard echoing enabled
                      0 if keyboard character echoing is disabled

**********************************************************************/

display_char(pos,ch)
CURPOS *pos;   /* pointer to structure for keeping track of cursor position */
int ch;        /* character to display */
{
    if (!kb.echo)
        return(0);

    return(display_char_prim(pos,ch));
}

