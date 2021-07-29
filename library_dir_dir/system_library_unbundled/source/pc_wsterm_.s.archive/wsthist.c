/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-07-11,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
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
     phx21233 - Updated flags to control visibility of chars redrawn, depending
     on state of keyboard echoing, for lines fetched from history buffer.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"
#include "wsthist.h"

/*
**********************************************************************

  Routine:            HISTORY_SCREEN 

  Function:
      This routine enters the history screen, allowing several 
  several options. These include displaying the next page (if enough 
  history items exist), the previous page, shifting the screen left 
  or right to allow viewing of a long history command, selecting a 
  history command or quitting the history screen. 

  Parameters:
     (output)         cur_scr - specifies the screen buffer to use 
                          for saving the screen contents to before 
                          the history screen is displayed 

  Returns:            NONE 

**********************************************************************/

history_screen(cur_scr)
SCREEN *cur_scr;
{
    int ch;
    ITEM_INFO item;
    int offset;
    int code;
    char line[SCREEN_COLS+2];
    char stat_mess[SCREEN_COLS+1];
    int selection;

    /* save the contents of the screen to the user specified buffer */
    save_wst_screen(cur_scr);

    /* hide the cursor */
    cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

    ch = NONE;
    offset = HIST_DEFAULT_0_ARG;

    /* clear the temporary screen buffer */
    wst_screen_clear(&wst_tmp_screen);
    wst_screen_printline(&wst_tmp_screen,"");

    /* initialize structure for displaying history items */
    init_history_display(&item);

    /* loop until user enters 'q' to quit or no more to display */
    while (uppercase(ch) != 'Q' && item.start_chars_in_buff > NONE) {

        /* try to extract and display a history line */
        code = extract_hist_lines(&item,offset,line);

        if (code >= 0)
            /* write a line of help info to the temporary screen buffer */
            wst_screen_printline(&wst_tmp_screen,line);

        /* if no more history items or a pageful */
        if (code < 0 || item.cur_display_count >= HIST_MAX_LINES) {

            /* screen full, dump the contents of the temporary screen buffer */
            restore_wst_screen(&wst_tmp_screen);

            /* hide the cursor */
            cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

            /* update the status line */
            if (item.cur_chars_in_buff < 1)
                sprintf(stat_mess,
                    "HISTORY SCREEN:       offset %3d  <ALT-H>-help  [<Q>,<RETURN>,<P>,<N>,<L>,<R>]",
                    offset);
            else
                sprintf(stat_mess,
                    "HISTORY SCREEN(more): offset %3d  <ALT-H>-help  [<Q>,<RETURN>,<P>,<N>,<L>,<R>]",
                    offset);

            status_line(stat_mess);

            /* wait for input from keyboard */
            selection = HIST_DEFAULT_0_ARG;

            while (TRUE) {
                ch = getkey(BLOCK);

                /* input is part of a numeric argument */
                if (ch >= SMALLEST_DECI_DIGIT && ch <= LARGEST_DECI_DIGIT) {
                    if (selection >= MAX_ARG_LIMIT) {
                        selection = HIST_DEFAULT_0_ARG;
                        beep();
                    }
                    else {
                        selection *= 10;
                        selection += ch - '0';
                    }
                }

                /* shift screen right */
                else if (uppercase(ch) == 'R') {

                    /* do only if screen is shifted left */
                    if (offset > NONE) {

                        /* examine numeric argument to determine amount to shift */
                        if (selection < 1) selection = 1;
                        if (offset >= selection)
                            offset -= selection;
                        else
                            offset = NONE;

                        /* redisplay from beginning of current page */
                        reset_hist_page(&item);
                        selection = HIST_DEFAULT_0_ARG;

                        /* quick check to optimize consecutive hitting
                           of screen shift keys; if next key buffered is
                           a screen shift key, just recalculate the offset
                           to minimize the number of screen redraws */

                        while (uppercase((ch = checkkey())) == 'R' ||
                            uppercase(ch) == 'L') {
                            ch = getkey(BLOCK);
                            if (uppercase(ch) == 'L') {
                                if (offset < HIST_MAX_SCREEN_SHIFT)
                                    offset++;
                            }
                            else {
                                if (offset > NONE)
                                    offset--;
                            }
                        }

                        break;
                    }
                    selection = HIST_DEFAULT_0_ARG;
                }

                /* shift screen left */
                else if (uppercase(ch) == 'L') {

                    /* do only if screen can be shift left some more */
                    if (offset < HIST_MAX_SCREEN_SHIFT) {

                        /* examine numeric argument to determine amount to shift */
                        if (selection < 1) selection = 1;
                        if (offset + selection <= HIST_MAX_SCREEN_SHIFT)
                            offset += selection;
                        else
                            offset = HIST_MAX_SCREEN_SHIFT;

                        /* redisplay from beginning of current page */
                        reset_hist_page(&item);
                        selection = HIST_DEFAULT_0_ARG;

                        /* quick check to optimize consecutive hitting
                           of screen shift keys; if next key buffered is
                           a screen shift key, just recalculate the offset
                           to minimize the number of screen redraws */

                        while (uppercase((ch = checkkey())) == 'L' ||
                            uppercase(ch) == 'R') {
                            ch = getkey(BLOCK);
                            if (uppercase(ch) == 'L') {
                                if (offset < HIST_MAX_SCREEN_SHIFT)
                                    offset++;
                            }
                            else {
                                if (offset > NONE)
                                    offset--;
                            }
                        }

                        break;
                    }
                    selection = HIST_DEFAULT_0_ARG;
                }

                /* handle selection of history command */
                else if (ch == RETURN_KEY && selection > NONE) {

                    /* if no such command */
                    if (fetch_nth_command(selection,
                        edlin.line, &edlin.length) < 0) {
                        selection = HIST_DEFAULT_0_ARG;
                    }

                    /* command successfully fetched in edlin.line */
                    else {
                        /* set flag to break out of outer loop */
                        ch = 'Q';
                        /* break out of first loop */
                        break;
                    }
                }

                /* display next history page */
                else if (ch == ' ' || uppercase(ch) == 'N') {
                    selection = HIST_DEFAULT_0_ARG;
                    if (next_hist_page(&item) >= 0)
                        break;
                    else
                        beep();
                }

                /* display previous history page */
                else if (uppercase(ch) == 'P') {
                    selection = HIST_DEFAULT_0_ARG;
                    if (previous_hist_page(&item) >= 0)
                        break;
                    else
                        beep();
                }

                /* quit from history screen */
                else if (uppercase(ch) == 'Q') {
                    /* set to 0 to indicate no history line selected */
                    selection = HIST_DEFAULT_0_ARG;
                    break;
                }

                /* display help information */
                else if (ch == ALT_H) {
                    help(HISTORY_HELP);
                    status_line(stat_mess);
                }

                /* unrecognized command, beep() */
                else {
                    selection = HIST_DEFAULT_0_ARG;
                    beep();
                }
            }

            /* clear screen buffer and leave blank line at top */
            wst_screen_clear(&wst_tmp_screen);
            wst_screen_printline(&wst_tmp_screen,"");
        }
    }

    /* restore contents of original screen and turn cursor back on */
    restore_wst_screen(cur_scr);

    /* history command selected, erase previously displayed command
       and draw the new one
    */
    if (selection > NONE) {
        /* phx21233 R.L. - update flags to determine visibility of chars redrawn */
        update_echo_flags(&edlin);

        erase_edit_line(&edlin);

        /* cursor at beginning of line */
        edlin.index = ZERO_INDEX_VALUE;
        /* reset escape flag and argument */
        edlin.escape_flag = NO_ESC_FUNC;
        edlin.escape_arg = NO_ESC_ARG;
        redraw_edit_line(&edlin);
        cursor_eol(&edlin);
    }
}



/*
**********************************************************************

  Routine:            SAVE_TO_HISTBUFF 

  Function:
      This routine saves a command to the history buffer. 

  Parameters:
     (input)          str - specifies the buffer containing the 
                          command 
     (input)          item_size - specifies the number of bytes in 
                          the command to be saved 

  Returns:            NONE 

**********************************************************************/

save_to_histbuff(str,item_size)
char *str;
int item_size;
{
    int i, j;
    int lo_byte;
    int hi_byte;
    int first_chunk;
    int buffer_empty;

    /* if item cannot fit into hist buffer */
    if (item_size+TOTAL_SIZE_SPEC_BYTES >= HIST_BUFF_SIZE) {
        beep();
        return;
    }

    /* delete items from tail until enough room */
    while (hbi.chars_free < item_size+TOTAL_SIZE_SPEC_BYTES) {

        i = (hbi.tail + hbi.tail_item_size) % HIST_BUFF_SIZE;

        /* if insertion will overwrite all existing items */
        if (i == hbi.head && hbi.chars_free == NONE) {
            /* re-initialize; otherwise, corrupted values will be used */
            init_histbuff();
            break;
        }

        /* update pointer to tail of circular buffer */
        hbi.tail = i;

        /* get size of item stored in first two bytes of item */
        lo_byte = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi_byte = hbi.kb[i];

        /* update amount of chars freed */
        hbi.chars_free += hbi.tail_item_size;

        /* update size of item pointed to by tail pointer */
        hbi.tail_item_size = (hi_byte * MAX_7_BIT_VAL) + lo_byte;
    }

    /* determine if the history buffer is empty */
    if (hbi.head == hbi.tail && hbi.chars_free == HIST_BUFF_SIZE)
        buffer_empty = TRUE;
    else
        buffer_empty = FALSE;

    /* update size of item pointed to by head pointer */
    hbi.head_item_size = item_size+TOTAL_SIZE_SPEC_BYTES;
    /* update buffer space left after adding this new item */
    hbi.chars_free -= hbi.head_item_size;

    /* encode the size of new item into two bytes */
    lo_byte = hbi.head_item_size % MAX_7_BIT_VAL;
    hi_byte = hbi.head_item_size / MAX_7_BIT_VAL;

    /* copy size of items as first two bytes of item */
    i = hbi.head;
    hbi.kb[i] = lo_byte;
    i = (i+1)%HIST_BUFF_SIZE;
    hbi.kb[i] = hi_byte;
    i = (i+1)%HIST_BUFF_SIZE;

    /* copy across end boundary of buffer to beginning of buffer? */
    if (i+item_size > HIST_BUFF_SIZE) {

        /* determine amount to copy up to end of buffer */
        first_chunk = HIST_BUFF_SIZE - i;
        /* determine amount to copy starting from beginning of buffer */
        item_size -= first_chunk;

        /* copy to history buffer */
        for (j = ZERO_INDEX_VALUE; j < first_chunk; j++)
            hbi.kb[i++] = str[j];
        for (i = ZERO_INDEX_VALUE; i < item_size; i++)
            hbi.kb[i] = str[j++];

    }

    /* does not wrap around history buffer, do straight copy */
    else {
        for (j = ZERO_INDEX_VALUE; j < item_size; j++)
            hbi.kb[i++] = str[j];
    }

    /* last two bytes of item must also hold the size of the item */
    i = i % HIST_BUFF_SIZE;
    hbi.kb[i] = lo_byte;
    i = (i+1) % HIST_BUFF_SIZE;
    hbi.kb[i] = hi_byte;
    i = (i+1) % HIST_BUFF_SIZE;

    /* if buffer was empty, make tail and head the same */
    if (buffer_empty) {
        hbi.tail = hbi.head;
        hbi.tail_item_size = hbi.head_item_size;
    }
    /* head pointer to next location to add */
    hbi.head = i;

    /* reset the current pointer to head on save to history buffer */
    hbi.current = hbi.head;
    hbi.current_size = hbi.head_item_size;
    hbi.head_of_histbuff = TRUE;

    /* reset direction flag */
    hbi.direction = HIST_START_DIR;

}



/*
**********************************************************************

  Routine:            INIT_HISTBUFF 

  Function:
      This routine initializes all fields in the history buffer 
  structure to indicate an empty history buffer. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

init_histbuff()
{
    
    /* all indices point to beginning of history buffer */
    hbi.head = ZERO_INDEX_VALUE;
    hbi.tail = ZERO_INDEX_VALUE;
    hbi.current = ZERO_INDEX_VALUE;

    /* all items have zero size in empty history buffer */
    hbi.head_item_size = ZERO_BYTES;
    hbi.tail_item_size = ZERO_BYTES;
    hbi.current_size = ZERO_BYTES;

    /* chars free is same as size of history buffer */
    hbi.chars_free = HIST_BUFF_SIZE;

    /* currently at head of history buffer */
    hbi.head_of_histbuff = TRUE;

    /* no particular direction */
    hbi.direction = HIST_START_DIR;
}



/*
**********************************************************************

  Routine:            INIT_HISTORY_DISPLAY 

  Function:
      This routine initializes the structure used to control history 
  screen displaying. 

  Parameters:
     (output)         item - specifies the structure to initialize 

  Returns:            NONE 

**********************************************************************/

init_history_display(item)
ITEM_INFO *item;
{

    /* unique number associated with each history command */
    item->start_item_no = ZERO_BYTES;

    /* chars in the history buffer, displaying ends when no more chars */
    item->start_chars_in_buff = HIST_BUFF_SIZE - hbi.chars_free;

    /* index into history buffer of what item to display next */
    item->start_item_index = hbi.head;

    /* count of how many items displayed in current page */
    item->start_display_count = ZERO_BYTES;

    /* size of item to be displayed next */
    item->start_size = hbi.head_item_size;

    /* the running values for the above starting values */
    item->cur_item_no = item->start_item_no;
    item->cur_chars_in_buff = item->start_chars_in_buff;
    item->cur_item_index = item->start_item_index;
    item->cur_display_count = item->start_display_count;
    item->cur_size = item->start_size;
}



/*
**********************************************************************

  Routine:            RESET_HIST_PAGE 

  Function:
      This routine resets the history display control structure for 
  redisplaying the current history page. 

  Parameters:
     (input/output)   item - specifies the history display control 
                          structure to use 

  Returns:            0 always

**********************************************************************/

reset_hist_page(item)
ITEM_INFO *item;
{

    /* re-assign the running values with the starting values */
    item->cur_item_no = item->start_item_no;
    item->cur_chars_in_buff = item->start_chars_in_buff;
    item->cur_item_index = item->start_item_index;
    item->cur_display_count = item->start_display_count = ZERO_BYTES;
    item->cur_size = item->start_size;

    return(0);
}



/*
**********************************************************************

  Routine:            NEXT_HIST_PAGE 

  Function:
      This routine sets the history display control structure for 
  displaying the next history page. 

  Parameters:
     (input/output)   item - specifies the history display control 
                          structure to use 

  Returns:            -1 if no items are on the next page
                      0 if otherwise 

**********************************************************************/

next_hist_page(item)
ITEM_INFO *item;
{
    /* no action of nothing more to display */
    if (item->cur_chars_in_buff < 1)
        return(-1);

    /* starting values are assigned results of previous running values */
    item->start_item_no = item->cur_item_no;
    item->start_chars_in_buff = item->cur_chars_in_buff;
    item->start_item_index = item->cur_item_index;
    item->start_display_count = item->cur_display_count = ZERO_BYTES;
    item->start_size = item->cur_size;

    return(0);
}


/*
**********************************************************************

  Routine:            PREVIOUS_HIST_PAGE 

  Function:
      This routine sets the history display control structure for 
  displaying the previous history page. 

  Parameters:
     (input/output)   item - specifies the history display control 
                          structure to use 

  Returns:            -1 if no items are in previous page 
                      0 otherwise 

**********************************************************************/

previous_hist_page(item)
ITEM_INFO *item;
{
    int i;
    int hi, lo;
    int cnt;
    int times;

    /* no previous page if current page does not start past last line
       of first page
    */
    if (item->start_item_no < HIST_MAX_LINES)
        return(-1);

    /* traverse backward through history buffer to update item fields */
    for (times = 0; times < HIST_MAX_LINES; times++) {

        /* update pointer to next item to display */
        i = item->start_item_index;

        /* extract size of item from kill buffer */
        lo = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;

        cnt = (hi * MAX_7_BIT_VAL) + lo;

        /* decrement item index circularly by size */
        item->start_item_index = (item->start_item_index + cnt)%HIST_BUFF_SIZE;

        /* decrement id number of history command item */
        item->start_item_no--;

        /* tally characters to be displayed in history buffer */
        item->start_chars_in_buff += cnt;

        /* update item size */
        item->start_size = cnt;

    }

    /* current values are same as starting values */
    item->cur_item_index = item->start_item_index;
    item->cur_item_no = item->start_item_no;
    item->cur_chars_in_buff = item->start_chars_in_buff;
    item->cur_display_count = item->start_display_count = ZERO_BYTES;
    item->cur_size = item->start_size;

    return(0);
}



/*
**********************************************************************

  Routine:            EXTRACT_HIST_LINES 

  Function:
      This function extracts commands from the history buffer and 
  copies it into a string. The length of the string will be at most, 
  the width of the screen. If a command exceeds the width of the 
  screen, the string will be terminated by a '$' to indicate there is 
  more that is not shown. A value is specified to allow portions of 
  long commands to be displayed starting from an offset from the 
  beginning of the command. 

  Parameters:
     (input)          item - structure containing information as to 
                          which item in the history buffer to extract 
     (input)          offset - specifies the offset from the 
                          beginning of the line to begin extracting 
     (output)         line - pointer to the string to pass back the 
                          history command; if the length of the 
                          command is less than the value of 'offset', 
                          an empty string is passed back 

  Returns:            -1 if unsuccessful 
                      0 if successful 

**********************************************************************/

extract_hist_lines(item,offset,line)
ITEM_INFO *item;
int offset;
char *line;
{
    int i, j;
    int lo, hi;
    int cnt;
    int linelen;
    int previous;

    /* entire buffer is free, nothing in buffer */
    if (item->cur_chars_in_buff < 1)
        return(-1);

    /* get index to next item to extract */
    i = item->cur_item_index;

    /* check if no more previous items */
    if (i == hbi.tail && !hbi.head_of_histbuff)
        return(-1);

    /* skip over current item and point to previous item */
    previous = (item->cur_item_index + HIST_BUFF_SIZE - item->cur_size)
               % HIST_BUFF_SIZE;
    i = previous;

    /* if history buffer is not empty */
    if (item->cur_chars_in_buff > NONE) {

        /* determine the size of this previous item */
        lo = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;

        cnt = (hi * MAX_7_BIT_VAL) + lo;

        /* 4 bytes are used store history item size; subtract to    */
        /* get command length                                       */
        cnt -= TOTAL_SIZE_SPEC_BYTES;
        item->cur_chars_in_buff -= TOTAL_SIZE_SPEC_BYTES;

        /* format line number into resulting line */
        sprintf(line,"%3d  ", item->cur_item_no+1);
        linelen = strlen(line);

        /* copy the history command if offset does not exceed       */
        /* command's length                                         */
        if (cnt > offset) {

            /* index into specified offset of history command */
            i = (i+offset) % HIST_BUFF_SIZE;
            item->cur_chars_in_buff -= offset;
            cnt -= offset;

            /* copy the command a byte at a time */
            for (j = 0; j < cnt; j++) {

                /* make sure copies only up to screen width in      */
                /* length                                           */
                if (linelen < SCREEN_COLS) {

                    /* display control character using ^ notation   */
                    /* (e.g. NUL is ^@)                             */
                    if (hbi.kb[i] < ' ') {
                        line[linelen++] = '^';
                        line[linelen++] = hbi.kb[i] + '@';
                    }
                    else {
                        line[linelen++] = hbi.kb[i];
                    }
                }

                i = (i+1)%HIST_BUFF_SIZE;
                item->cur_chars_in_buff--;
            }
        }

        else {
            i = (i+cnt) % HIST_BUFF_SIZE;
            item->cur_chars_in_buff -= cnt;
        }

        /* overwrite last character of line with '$' if greater     */
        /* than screen width                                        */
        if (linelen > SCREEN_COLS-1) {
            line[SCREEN_COLS-1] = '$';
            line[SCREEN_COLS] = NUL_TERMINATOR;
        }
        else
            line[linelen] = NUL_TERMINATOR;

        /* tally item and update item index in history display      */
        /* control structure                                        */
        item->cur_item_no++;
        item->cur_item_index = previous;
        item->cur_display_count++;

        /* update item size field if not last item in history       */
        /* buffer                                                   */
        if (previous != hbi.tail) {
            previous = (previous + HIST_BUFF_SIZE - N_SIZE_SPEC_BYTES)
                % HIST_BUFF_SIZE;
            lo = hbi.kb[previous];
            previous = (previous+1) % HIST_BUFF_SIZE;
            hi = hbi.kb[previous];
            item->cur_size = (hi * MAX_7_BIT_VAL) + lo;
        }

        return(0);
    }
    return(-1);
}



/*
**********************************************************************

  Routine:            FETCH_NEXT_COMMAND 

  Function:
      This routine allows the next command in the history buffer 
  (entered chronologically) to be fetched from the history buffer. 

  Parameters:
     (input)          n_times - specifies the number of times to 
                          repeat the operation 
     (output)         cmd - specifies the target string for placing 
                          the extracted command 
     (output)         cmd_len - specifies the integer for passing 
                          back the length of the fetched command 

  Returns:            -1 if unsuccessful 
                      0 otherwise 

  Note 1 - if an error occurs, this routine will sound a beep. 
**********************************************************************/

fetch_next_command(n_times,cmd,cmd_len)
int n_times;
char *cmd;
int *cmd_len;
{
    int i, j;
    int cnt;
    int lo, hi;
    int times;
    int prev_index;

    /* if nothing in history buffer */
    if (hbi.chars_free == HIST_BUFF_SIZE)
        return(-1);

    prev_index = (hbi.head+HIST_BUFF_SIZE-hbi.head_item_size) % HIST_BUFF_SIZE;

    /* if changing directions and not single item in history buffer */
    if (hbi.direction == HIST_PREVIOUS_DIR && prev_index != hbi.tail)
        n_times++;

    hbi.direction = HIST_NEXT_DIR;

    for (times = 0; times < n_times; times++) {

        if (hbi.current == hbi.head) {
            hbi.head_of_histbuff = TRUE;
            beep();
            return(-1);
        }

        i = hbi.current;

        lo = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;

        cnt = (hi * MAX_7_BIT_VAL) + lo;
        hbi.current_size = cnt;
        cnt -= TOTAL_SIZE_SPEC_BYTES;

        /* do modify parameters only last time through loop */
        if (times == n_times-1) {
            *cmd_len = cnt;
            for (j = 0; j < cnt; j++) {
                cmd[j] = hbi.kb[i];
                i = (i+1)%HIST_BUFF_SIZE;
            }
        }
        /* otherwise just skip this particular item */
        else
            i = (i+cnt)%HIST_BUFF_SIZE;

        i = (i+1)%HIST_BUFF_SIZE;
        i = (i+1)%HIST_BUFF_SIZE;

        hbi.current = i;
    }

    return(0);
}



/*
**********************************************************************

  Routine:            FETCH_PREVIOUS_COMMAND 

  Function:
      This routine allows the previous command in the history buffer 
  (reversed chronologically) to be fetched from the history buffer. 

  Parameters:
     (input)          n_times - specifies the number of times to 
                          repeat the operation 
     (output)         cmd - specifies the target string for placing 
                          the fetched command 
     (output)         cmd_len - specifies the integer for passing 
                          back the length of the fetched command 

  Returns:            -1 if unsuccessful 
                      0 otherwise 

  Note 1 - If an error occurs, this routine will sound a beep. 
**********************************************************************/

fetch_previous_command(n_times,cmd,cmd_len)
int n_times;
char *cmd;
int *cmd_len;
{
    int i, j;
    int cnt;
    int lo, hi;
    int previous;
    int times;
    int prev_index;

    /* nothing in history buffer */
    if (hbi.chars_free == HIST_BUFF_SIZE)
        return(-1);

    prev_index = (hbi.head+HIST_BUFF_SIZE-hbi.head_item_size) % HIST_BUFF_SIZE;

    /* if change of directions while extracting, then must extract  */
    /* an extra to prevent the current item to be refetched;        */
    /* check for direction change and not last item in history      */
    /* buffer                                                       */
    if (hbi.direction == HIST_NEXT_DIR && prev_index != hbi.tail)
        n_times++;

    hbi.direction = HIST_PREVIOUS_DIR;

    /* perform extraction specified number of times */
    for (times = 0; times < n_times; times++) {

        /* check if at last item */
        if (hbi.current == hbi.tail && !hbi.head_of_histbuff) {
            beep();
            return(-1);
        }

        /* index to previous item */
        previous = (hbi.current + HIST_BUFF_SIZE - hbi.current_size) %
            HIST_BUFF_SIZE;

        i = previous;

        /* get size of previous item */
        lo = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;

        cnt = (hi * MAX_7_BIT_VAL) + lo;

        /* subtract item size bytes to get bytes in command */
        cnt -= TOTAL_SIZE_SPEC_BYTES;

        /* do extraction only for last iteration of loop */
        if (times == n_times-1) {
            *cmd_len = cnt;

            /* copy from history buffer to target string */
            for (j = 0; j < cnt; j++) {
                cmd[j] = hbi.kb[i];
                i = (i+1)%HIST_BUFF_SIZE;
            }
        }

        /* skip index over this item */
        else
            i = (i+cnt)%HIST_BUFF_SIZE;

        /* update history buffer index to previous item for next    */
        /* fetch                                                    */
        hbi.current = previous;
        if (previous != hbi.tail) {
            previous = (previous + HIST_BUFF_SIZE - N_SIZE_SPEC_BYTES)
                % HIST_BUFF_SIZE;
            lo = hbi.kb[previous];
            previous = (previous+1) % HIST_BUFF_SIZE;
            hi = hbi.kb[previous];
            hbi.current_size = (hi * MAX_7_BIT_VAL)+lo;
        }

        /* current is no longer at head of list, set flag to FALSE */
        hbi.head_of_histbuff = FALSE;
    }

    return(0);
}



/*
**********************************************************************

  Routine:            FETCH_NTH_COMMAND 

  Function:
      This routine fetches the Nth command from the history buffer. 
  Commands in the history buffer are numbered with the most recently 
  entered command being item 1, the next most recent being item 2, 
  etc. 

  Parameters:
     (input)          cmd_num - specifies the number of the history 
                          command to extract 
     (output)         cmd - specifies the target string to copy the 
                          fetched history command to 
     (output)         cmd_len - pointer to the integer for passing 
                          back the length of the fetched command 

  Returns:            -1 if unsuccessful 
                      0 otherwise 

**********************************************************************/

fetch_nth_command(cmd_num,cmd,cmd_len)
int cmd_num;
char *cmd;
int *cmd_len;
{
    int i, j;
    int cnt;
    int lo, hi;
    int previous;
    int times;
    int at_head;
    int cur_index;
    int cur_size;

    /* nothing in history buffer */
    if (hbi.chars_free == HIST_BUFF_SIZE)
        return(-1);

    /* initialize to start of history buffer */
    cur_index = hbi.head;
    cur_size = hbi.head_item_size;
    at_head = hbi.head_of_histbuff;

    /* loop specified number of times to get specified command */
    for (times = ZERO_INDEX_VALUE; times < cmd_num; times++) {

        /* if at end of history buffer */
        if (cur_index == hbi.tail && !at_head) {
            beep();
            return(-1);
        }

        /* index to previous item in circular buffer */
        previous = (cur_index + HIST_BUFF_SIZE - cur_size) %
            HIST_BUFF_SIZE;
        i = previous;

        /* get size of previous item */
        lo = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;
        hi = hbi.kb[i];
        i = (i+1)%HIST_BUFF_SIZE;

        cnt = (hi * MAX_7_BIT_VAL) + lo;

        /* subtract size storage bytes to get command size */
        cnt -= TOTAL_SIZE_SPEC_BYTES;

        *cmd_len = cnt;

        /* fetch the command */
        for (j = ZERO_INDEX_VALUE; j < cnt; j++) {
            cmd[j] = hbi.kb[i];
            i = (i+1)%HIST_BUFF_SIZE;
        }

        /* update current index to previous item for next time thru loop */
        cur_index = previous;

        /* update item size if no end of history buffer */
        if (previous != hbi.tail) {
            previous = (previous + HIST_BUFF_SIZE - N_SIZE_SPEC_BYTES)
                % HIST_BUFF_SIZE;
            lo = hbi.kb[previous];
            previous = (previous+1) % HIST_BUFF_SIZE;
            hi = hbi.kb[previous];
            cur_size = (hi * MAX_7_BIT_VAL)+lo;
        }

        /* no longer at start of history buffer */
        at_head = FALSE;
    }

    /* update history information */
    hbi.current = cur_index;
    hbi.current_size = cur_size;
    hbi.head_of_histbuff = at_head;
    hbi.direction = HIST_PREVIOUS_DIR;

    return(0);
}

