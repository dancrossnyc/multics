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
#include <ctype.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
**********************************************************************

  Routine:            CURSOR_BOL 

  Function:
      This routine moves the cursor to the beginning of the line. 
  Since the entire input line may span several rows on the screen, 
  this routine may be required to handle the case where the beginning 
  of the line has scrolled off the top of the screen. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 

  Returns:            NONE 

**********************************************************************/

cursor_bol(line)
EDIT_LINE *line;
{
    int i;

    /* check that line is not empty */
    if (line->index > 0) {

        /* check if beginning of line displayed has scrolled */
        if (line->scrolled_flag) {

            /* need to redraw from beginning of display screen */
            set_cursor(ss.top,ss.left);

            /* blank pad to original display column */
            for (i = ss.left; i < line->orig_col; i++)
                putch(' ');

            /* start coordinates now beginning of screen */
            line->orig_row = ss.top;

            /* no longer scrolled, redisplaying the beginning */
            line->scrolled_flag = FALSE;

            /* set current cursor coordinates */
            line->cur_row = ss.top;
            line->cur_col = line->orig_col;

            /* update the physical cursor location */
            set_cursor(line->cur_row,line->cur_col);
            line->index = 0;

            /* redisplay from beginning of line */
            redisplay(line,line->index,&(line->max_row),&(line->max_col));
        }

        /* no, beginning of line still on display */
        else {

            /* set keyboard buffer index to beginning of line */
            line->index = 0;

            /* move the physical cursor to the beginning of line on
               the display
            */
            line->cur_row = line->orig_row;
            line->cur_col = line->orig_col;
            set_cursor(line->cur_row,line->cur_col);
        }
    }
}


/*
**********************************************************************

  Routine:            CURSOR_EOL 

  Function:
      This routine moves the cursor to the end of the line in edit 
  mode. Since it is possible for the end of the line to be shifted 
  past the end of the screen due to inserting text, this routine has 
  to determine and handle scrolling in this case. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 

  Returns:            NONE 

  Note 1 - Some care must be taken to maintain consistency in the 
      appearance of the screen when using this and other cursor 
      movement routines. Going to the end of the line using a series 
      of commands to move the cursor to the right or going directly 
      to the end of the line using this routine should yield the same 
      display of the line being edited. 
**********************************************************************/

cursor_eol(line)
EDIT_LINE *line;
{
    int cnt;
    int row;
    int col;
    int screen_width;
    int screen_height;
    int chars_to_pad;
    int i;
    char *ptr, *display_char_str();
    int offset;
    int lines_scrolled;

    /* determine screen width */
    screen_width = ss.right - ss.left + 1;
    screen_height = ss.bottom - ss.top + 1;

    /* get absolute row coordinate */
    row = line->cur_row - ss.top;

    /* get absolute column coordinate */
    col = line->cur_col - ss.left;
    cnt = 0;

    /* determine number of character positions needed to
       display from current cursor position (index) to end of line (in cnt);
       keep track of where the cursor will end up when all text to end
       of line is displayed (in row, col).
    */
    for (i = line->index; i < line->length; i++) {
        cnt += line->size[i];
        row += (col + line->size[i]) / screen_width;
        col = (col + line->size[i]) % screen_width;
    }

    /* if cursor would still be within screen boundary */
    if (row < screen_height) {

        /* just move the cursor to the calculated position */
        line->cur_row = row + ss.top;
        line->cur_col = col + ss.left;
        set_cursor(line->cur_row,line->cur_col);

        /* set current position (index) to end of line position */
        line->index = line->length;
        return;
    }

    else {
        lines_scrolled = row - screen_height + 1;

        /* We want to cursor to end up at the column position
           calculated above (in col) but displayed at the bottom row of the
           screen; determine number of character positions needed
           to fill from the beginning of the screen
        */
        chars_to_pad = (screen_height - 1) * screen_width;
        chars_to_pad += col;

        /* "cnt" contains the number of display locations
           required to display from the end of the line to
           the "i"th character
        */
        cnt = 0;
        for (i = line->length-1; i >= 0; i--) {
            cnt += line->size[i];
            if (cnt >= chars_to_pad)
                break;
        }

        /* if the character locations needed to display the
           entire line is less than that needed to pad to the
           beginning of the line, then line starts within the
           screen display but part of the line extends beyond
           what is displayed on the screen
        */
        if (i < 0 || cnt < chars_to_pad) {

            /* scroll enough lines for the last line to be displayed */
            for (i = 0; i < lines_scrolled; i++)
                wst_scroll();

            /* calculate where the cursor should end up */
            i = 0;
            offset = ((screen_height - 1) * screen_width) + col;
            offset -= cnt;
            row = offset / screen_width;
            col = offset % screen_width;

            row += ss.top;
            col += ss.left;

            /* move the physical cursor to the calculated location */
            set_cursor(row,col);

        }

        /* enough character to pad from the beginning of display to
           end of line starting from the "i"th position
        */
        else {

            /* move physical cursor to start of display screen */
            row = ss.top;
            col = ss.left;
            set_cursor(row,col);

            /* if total display locations starting from character the
               "i"th position is more than that needed to pad to the
               beginning of the screen, then only part of the "i"th
               character displayed needs to be displayed at the beginning
               of the display screen
            */
            if (cnt > chars_to_pad) {

                /* get a pointer to the display string of that character */
                ptr = display_char_str(line->line[i]);

                /* index into the portion of the display string
                   needed to pad to fill the screen
                */
                ptr += strlen(ptr) - (cnt - chars_to_pad);

                /* display that portion of the character string */
                while (*ptr) {
                    putch(*ptr);
                    ptr++;
                    col++;
                }

                /* start redisplay from next character */
                i++;
            }
        }


        /* move keyboard buffer index to end of line */
        line->index = line->length;

        /* initialize coordinates values to where to begin redisplay */
        line->cur_row = row;
        line->cur_col = col;

        /* do the redisplay */
        redisplay(line,i,&(line->max_row),&(line->max_col));

        /* update the start and current cursor coordinates */
        line->cur_row = line->max_row;
        line->cur_col = line->max_col;

        /* check if screen has scrolled */
        line->orig_row -= lines_scrolled;
        if (line->orig_row < ss.top)
            line->scrolled_flag = TRUE;

        /* update location of physical cursor */
        set_cursor(line->cur_row,line->cur_col);
    }
}


/*
**********************************************************************

  Routine:            CURSOR_LEFT 

  Function:
      This routine moves the physical and logical cursor to the 
  previous character in the edit mode line, if that character exists 
  (i.e. when not at the beginning of the line). Since it is possible 
  for part of the line to have scrolled off the top of the screen, 
  this routine has to detect and handle scrolling the line back down. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          n_times - specifies the number of character 
                          locations to move the cursor backwards (to 
                          the left) 

  Returns:            The actual number of characters that the cursor 
                          was moved back. 

  Note 1 - If the number of characters from the current cursor 
      location to the beginning of the line is less than value 
      specified by 'n_times', the cursor is moved to the beginning of 
      the line. This condition may be checked by the caller by 
      checking if the value passed for 'n_times' is less than the 
      return value. 
**********************************************************************/

cursor_left(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;
    int size;
    int row_diff;
    int col_diff;
    int chars_to_bos;
    int screen_width;
    int chars_to_pad;
    int new_index;
    int pad;
    int new_col;
    char *ptr, *display_char_str();
    int actual_times;

    /* don't do anything if at beginning of line or moving left 0 times */
    if (line->index < 1 || n_times < 1)
        return(0);

    /* limit the number of times to move to the number number of */
    /* characters from current position to beginning of line */
    if (n_times > line->index)
        actual_times = line->index;
    else
        actual_times = n_times;

    /* determine the number of display characters to move left from the
       current location and position the line index there
    */
    size = 0;
    for (i = 0; i < actual_times; i++) {
        line->index--;
        size += line->size[line->index];
    }

    /* determine the number of display characters from the current location
       to the beginning of the display window
    */
    row_diff = line->cur_row - ss.top;
    col_diff = line->cur_col - ss.left;
    screen_width = ss.right - ss.left + 1;
    chars_to_bos = (row_diff * screen_width) + col_diff;

    /* handle case for moving left beyond display window origin:
           redisplay() will automatically update to the right of
           the cursor if the current coordinates in line->cur_row
           and line->cur_col are set and the index into the actual
           line buffer is given. The cursor will be on the first
           row of the display window. We need to calculate the column
           position based on the number of times moved left. Also,
           we need to fill the beginning of the first line up to the
           calculated column position since redisplay() won't do it.
    */

    if (chars_to_bos < size) {

        /* need to scroll back, so redraw always start at window origin */

        line->cur_row = ss.top;
        line->cur_col = ss.left;
        set_cursor(line->cur_row,line->cur_col);

        /* determine which column on the starting row that cursor
           should be positioned at
        */

        chars_to_pad = screen_width -
           ((size - chars_to_bos) % screen_width);

        /* handle wrap around case */
        if (chars_to_pad == screen_width) chars_to_pad = 0;

        /* calculate absolute screen coordinates */
        new_col = ss.left + chars_to_pad;

        /* check if updating to the left of cursor is necessary */
        if (chars_to_pad > 0) {

            new_index = line->index - 1;

            pad = 0;
            for (i = new_index; i >= 0; i--) {
                pad += line->size[i];
                if (pad >= chars_to_pad)
                    break;
            }

            /* if not enough characters to pad to beginning of line, then
               line must have started somewhere in the middle of the display
               window, just blank fill the beginning of the line and then
               redisplay
            */
            if (i < 0) {

                for (i = 0; i < chars_to_pad - pad; i++) {
                    line->cur_col++;
                    putch(' ');
                }
                redisplay(line,0,&(line->max_row),&(line->max_col));

                /* set cursor to the column position determined */
                line->cur_col = new_col;
                set_cursor(line->cur_row,line->cur_col);

                return(actual_times);
            }

            /* a single character may need 2 or more characters to display
               the character, e.g. 10 blanks for a tab or \ 0 0 0
               for a NUL. Check if line starts in the middle of
               one of these and if so, print out the portion needed to
               pad the beginning of the line
            */
            if (pad > chars_to_pad) {

                /* get the string representing the current character */
                ptr = display_char_str(line->line[i]);

                /* index into the string the portion needed to pad the line */
                ptr += (strlen(ptr) - (pad - chars_to_pad));

                /* display this portion of the string */
                while (*ptr) {
                    putch(*ptr);
                    ptr++;
                    line->cur_col++;
                }
                /* start redisplaying at the next character in buffer */
                i++;
            }
        }

        /* no padding at beginning of line necessary, redisplay at current
           index position */
        else
            i = line->index;

        redisplay(line,i,&(line->max_row),&(line->max_col));

        /* position the cursor to the calculated location */
        line->cur_row = ss.top;
        line->cur_col = new_col;
        set_cursor(line->cur_row,line->cur_col);

        return(actual_times);
               
    }

    /* final location is still on the screen, back up and reposition there */
    else {
        for (i = 0; i < size; i++) {
            line->cur_col--;
            if (line->cur_col < ss.left) {
                line->cur_col = ss.right;
                line->cur_row--;
            }
        }
        set_cursor(line->cur_row,line->cur_col);
        return(actual_times);
    }
}


/*
**********************************************************************

  Routine:            CURSOR_RIGHT 

  Function:
      This routine moves the physical and logical cursor one 
  character forward (to the right). The routine detects and handles 
  the case of scrolling the screen when the end of the screen is 
  reached. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          n_times - specifies the number of times to the 
                          cursor a character position to the right 

  Returns:            The actual number of times the cursor was moved 
                          forward (to the right) 

  Note 1 - If the number of characters from the current cursor 
      location to the end of the line is less than the value 
      specified by 'n_times', the cursor is moved to the end of the 
      line. This condition may be checked by the caller by checking 
      if the value passed for 'n_times' is less than the return 
      value. 
**********************************************************************/

cursor_right(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;
    int size;
    int row_diff;
    int col_diff;
    int chars_to_eos;
    CURPOS tpos;
    int actual_times;
    int chars_to_eol;

    /* check that we're not already at the end of the line */
    if (line->index >= line->length)
        return(0);

    /* determine the number of characters from current cursor position to
       the end of the line
    */
    chars_to_eol = line->length - line->index;

    /* if number of times to move cursor right exceeds number of
       characters to the right of the cursor, set the actual number
       of times to move to the number of characters to the right of cursor
    */
    if (chars_to_eol > n_times)
        actual_times = n_times;
    else
        actual_times = chars_to_eol;

    /* calculate the display columns to move to the right (a character
       may require more than one display column, e.g. tab may require 10)
    */
    size = 0;
    for (i = 0; i < actual_times; i++)
        size += line->size[line->index+i];

    /* calculate the number of display columns to end of screen */
    row_diff = ss.bottom - line->cur_row;
    col_diff = ss.right - line->cur_col;
    chars_to_eos = (row_diff * (ss.right - ss.left + 1)) + col_diff;

    /* determine if new cursor location passes end of screen */
    if (chars_to_eos >= size) {

        /* no, just move calculate where the cursor will end up */
        for (i = 0; i < size; i++) {
            line->cur_col++;
            if (line->cur_col > ss.right) {
                line->cur_col = ss.left;
                line->cur_row++;
            }
        }

        /* move the physical cursor there */
        set_cursor(line->cur_row,line->cur_col);

        /* move keyboard buffer index to the right */
        line->index += actual_times;
    }

    /* new cursor location passes end of screen */
    else {

        /* initialize structure for keeping track of cursor */
        init_temp_curpos(&tpos,line);

        /* display each character (scrolling if necessary)
           until the specified number of characters to the right
           has been displayed
        */
        for (i = 0; i < actual_times; i++) {
            display_char(&tpos,line->line[line->index]);
            line->index++;
        }

        /* update current cursor coordinate values */
        line->cur_row = tpos.cur_row;
        line->cur_col = tpos.cur_col;
        line->orig_row -= tpos.scroll_flag;

        if (line->orig_row < ss.top)
            line->scrolled_flag = TRUE;

        /* redisplay rest of line to end of screen */
        redisplay(line,line->index,&(line->max_row),&(line->max_col));
    }

    /* return the actual amount moved to the right */
    return(actual_times);
}



/*
**********************************************************************

  Routine:            FORWARD_WORD 

  Function:
      This routine moves the physical and logical cursor forward 
  until the end of a word is reached. The number of times this 
  operation is repeated may be specified. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          n_times - specifies the number of times to move 
                          the cursor forward a word 

  Returns:            NONE 

**********************************************************************/

forward_word(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;
    int tmp_index;
    int loop_cnt;

    /* do nothing if already at end of line */
    if (line->index >= line->length) return;

    /* get copy of keyboard buffer index */
    i = line->index;

    loop_cnt = 0;

    /* perform the specified number of times or until end of line reached */
    while (loop_cnt < n_times && i < line->length) {

        /* save index in case no words skipped */
        tmp_index = i;

        /* skip any word delimiters */
        while (i < line->length && is_word_delim(line->line[i]))
            i++;

        /* no next word, do nothing */
        if (i >= line->length) {
            /* restore to position before word delimiters skipped */
            i = tmp_index;
            break;
        }

        /* skip any characters until word delimiter reached */
        while (i < line->length && !is_word_delim(line->line[i]))
            i++;

        /* tally the word skipped */
        loop_cnt++;

    }

    /* move and update cursor only if moved forward at least 1 word */
    if (loop_cnt > 0) {
        /* if keyboard buffer did not change, just return */
        if (i == line->index)
            return;

        /* move the keyboard right the calculated number of times */
        cursor_right(line,i-line->index);
    }
}



/*
**********************************************************************

  Routine:            BACKWARD_WORD 

  Function:
      This routine moves the physical and logical cursor to the 
  beginning of the previous word. The number of times this operation 
  is repeated may be specified. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          n_times - specifies the number of times to move 
                          the cursor backward a word 

  Returns:            NONE 

**********************************************************************/

backward_word(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;
    int tmp_index;
    int loop_cnt;

    /* do nothing if already at beginning of line */
    if (line->index < 1) return;

    /* assume starting at one character before cursor */
    i = line->index - 1;

    loop_cnt = 0;
   /* move specified number of words back or until beginning of line reached */
    while (loop_cnt < n_times && i >= 0) {

        /* save index in case no words skipped */
        tmp_index = i;

        /* skip over any word delimiters */
        while (i >= 0 && is_word_delim(line->line[i]))
            i--;

        /* do nothing if no previous word */
        if (i < 0) {
            /* don't move cursor position if just word delimiters */
            i = tmp_index;
            break;
        }

        /* skip until a word delimiter is reached */
        while (i >= 0 && !is_word_delim(line->line[i]))
            i--;

        /* tally the word skipped */
        loop_cnt++;
    }

    /* move and update cursor only if moved backwards least one word */
    if (loop_cnt > 0) {
        /* restore index position from delimiter reached (or past beginning
           of line) to beginning of word (or start of line)
        */
        i++;

        /* move the cursor left the calculated number of times */
        cursor_left(line,line->index-i);
    }
}



/*
**********************************************************************

  Routine:            IS_WORD_DELIM 

  Function:
      This function, given an ASCII character, specifies whether that 
  character is a word delimiter or not. This function is used by the 
  forward_word and backward_word routines. 

  Parameters:
     (input)          ch - specifies the ASCII character being 
                          checked as a word delimiter character 

  Returns:            TRUE if the specifies character is a word 
                          delimiter 
                      FALSE if the character is NOT a word delimiter 

**********************************************************************/

is_word_delim(ch)
int ch;
{
    /* if valid ASCII character and is a space character, then word delimter */
    if (ch < ASCII_DEL && isspace(ch)) return(TRUE);

    /* check for special punctuations that are not word delimiters */
    if (ch < ASCII_DEL && ispunct(ch)) {
        if (ch == '_' || ch == '$' || ch == '-')
            return(FALSE);
        return(TRUE);
    }
    return(FALSE);
}



/*
**********************************************************************

  Routine:            BACKUP_CURSOR 

  Function:
      This routine just moves the physical cursor backwards a 
  specified number of character positions on the screen and updates 
  the cursor position fields in the structure containing the line and 
  information about the line being edited in edit mode. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          n_times - specifies the number of times to move 
                          the cursor backwards a character 

  Returns:            NONE 

**********************************************************************/

backup_cursor(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;

    /* perform the specified number of times */
    for (i = 0; i < n_times; i++) {

        /* decrement cursor column value */
        line->cur_col--;

        /* past left edge? */
        if (line->cur_col < ss.left) {

            /* go to end of previous line */
            line->cur_col = ss.right;
            line->cur_row--;
        }
    }

    /* update the physical cursor location */
    set_cursor(line->cur_row,line->cur_col);
}
