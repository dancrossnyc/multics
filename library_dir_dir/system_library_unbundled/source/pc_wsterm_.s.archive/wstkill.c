/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-08-04,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Created.
  2) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  3) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Modified line input manipulations to update the character
     display length flags as well to control echoing of echoed and non-echoed
     input.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"
#include "wstkill.h"

/*
**********************************************************************

  Routine:            YANK 

  Function:
      This routine yanks the last item that was saved to the kill 
  buffer and inserts the item into the edit mode line buffer. The 
  display is updated. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line being edited and information about the 
                          line in edit mode 

  Returns:            NONE 

  Note 1 - If an error occurs (e.g. there is not enough room in the 
      line to contain the item being yanked), a beep is sounded and 
      no processing takes place. 
**********************************************************************/

yank(line)
EDIT_LINE *line;
{
    int line_chars_free;
    int i, j;
    int hi, lo;
    int cnt;
    int previous;

    /* determine amount of free space in line being edited */
    line_chars_free = MAX_LINE_SIZE - line->length;

    /* check and handle for nothing in kill buffer */
    if (kill_info.chars_free == KILL_BUFF_SIZE ||
        (kill_info.current == kill_info.tail && !kill_info.head_of_killbuff)) {
        beep();
        return;
    }

    /* index to previous item in kill buffer */
    previous = (kill_info.current + KILL_BUFF_SIZE - kill_info.current_size) %
        KILL_BUFF_SIZE;

    i = previous;

    /* determine total size of this previous item */
    lo = kill_info.kb[i];
    i = (i+1)%KILL_BUFF_SIZE;
    hi = kill_info.kb[i];
    i = (i+1)%KILL_BUFF_SIZE;

    cnt = (hi * MAX_7_BIT_VAL) + lo;

    /* subtract item size bytes to get number of data bytes */
    cnt -= TOTAL_SIZE_SPEC_BYTES;

    /* check and handle insufficient space in line to insert item */
    if (line_chars_free < cnt) {
        beep();
        return;
    }

    /* create gap for inserting yank item */
    for (j = line->length; j >= line->index; j--) {
        line->line[j+cnt] = line->line[j];
        line->size[j+cnt] = line->size[j];    /* phx21233 R.L. - update display length flags */
    }

    line->length += cnt;
    kill_info.yank_index = line->index;
    kill_info.yank_size = cnt;    /* reset on save or RETURN */

    /* copy item into gap */
    for (j = 0; j < cnt; j++) {
        line->line[line->index+j] = kill_info.kb[i];
        line->size[line->index+j] = (kb.echo) ? TRUE : FALSE; /* phx21233 R.L. updated echo status */
        i = (i+1)%KILL_BUFF_SIZE;
    }

    /* update the display and cursor */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));
    cursor_right(line,kill_info.yank_size);
}



/*
**********************************************************************

  Routine:            YANK_PREVIOUS 

  Function:
      This routine allows each item in the kill buffer to be yanked 
  in reversed chronological order, each time the routine is called. 
  The routine "yank()" must have been called initially to yank the 
  most recently killed item in order for previous items to be yanked. 
  When the previous item is yanked, it replaces the item that was 
  yanked immediately before by deleting that item first before 
  inserting the newly yanked item. The display and cursor is updated 
  with each yank. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line being edited and information about the 
                          line in edit mode 

  Returns:            NONE 

  Note 1 - If an error occurs (e.g. there is no room in the line 
      being edited to contain the item yanked), the routine will 
      sound a beep and no further processing takes place. 
**********************************************************************/

yank_previous(line)
EDIT_LINE *line;
{
    int i, j;
    int hi, lo;
    int cnt;
    int previous;
    int diff;

    /* check that a yank was previously made */
    if (kill_info.yank_size < 1) {
        beep();
        return;
    }

    /* check that cursor has not moved since last yank */
    if (line->index != kill_info.yank_index+kill_info.yank_size) {
        beep();
        return;
    }

    /* check and handle for nothing in the kill buffer */
    if (kill_info.chars_free == KILL_BUFF_SIZE ||
        (kill_info.current == kill_info.tail && !kill_info.head_of_killbuff)) {
        beep();
        return;
    }

    /* get index to previous item in the kill buffer */
    previous = (kill_info.current + KILL_BUFF_SIZE -
        kill_info.current_size) % KILL_BUFF_SIZE;

    /* check and handle reaching last item of kill buffer */
    if (previous == kill_info.tail)
        previous = kill_info.head;

    /* index of current item now indexes to previous item */
    kill_info.current = previous;

    /* update size of current item with size of previous item */
    if (previous != kill_info.tail) {

        /* get size of previous item */
        previous = (previous + KILL_BUFF_SIZE - N_SIZE_SPEC_BYTES) % KILL_BUFF_SIZE;
        lo = kill_info.kb[previous];
        previous = (previous+1) % KILL_BUFF_SIZE;
        hi = kill_info.kb[previous];
        kill_info.current_size = (hi * MAX_7_BIT_VAL)+lo;
    }

    /* current does not index to first item of kill buffer */
    kill_info.head_of_killbuff = FALSE;

    /* index to previous item */
    previous = (kill_info.current + KILL_BUFF_SIZE -
        kill_info.current_size) % KILL_BUFF_SIZE;

    /* move cursor to beginning of previously yanked item */
    cursor_left(line,kill_info.yank_size);

    i = previous;

    /* get size of item to yank */
    lo = kill_info.kb[i];
    i = (i+1)%KILL_BUFF_SIZE;
    hi = kill_info.kb[i];
    i = (i+1)%KILL_BUFF_SIZE;

    cnt = (hi * MAX_7_BIT_VAL) + lo;
    kill_info.current_size = cnt;

    cnt -= TOTAL_SIZE_SPEC_BYTES;

    /* compare size of items to be yanked and previously yanked to  */
    /* determine whether to expand or shrink the line being edited  */
    if (cnt < kill_info.yank_size) {

        /* determine amount to shrink the line */
        diff = kill_info.yank_size - cnt;

        /* shrink over the previous yanked item */
        for (j = kill_info.yank_index+cnt; j < line->length-diff; j++) {
            line->line[j] = line->line[j+diff];
            line->size[j] = line->size[j+diff];    /* phx21233 R.L. - update display length flags */
        }

        /* update line length to reflect shrinkage */
        line->length -= diff;
    }

    else if (kill_info.yank_size < cnt) {

        /* determine amount to expand */
        diff = cnt - kill_info.yank_size;

        /* check and handle not enough room to contain yanked item */
        if (line->length + diff >= MAX_LINE_SIZE) {
            beep();
            return;
        }

        /* create gap at previously yanked item */
        for (j = line->length; j >= kill_info.yank_index; j--) {
            line->line[j+diff] = line->line[j];
            line->size[j+diff] = line->size[j];    /* phx21233 R.L. - update display length */
        }

        /* update the line length to reflect gap inserted */
        line->length += diff;
    }

    kill_info.yank_index = line->index;
    kill_info.yank_size = cnt;    /* reset on save or RETURN */

    /* copy the item to be yanked over the previously yanked item */
    for (j = 0; j < cnt; j++) {
        line->line[line->index+j] = kill_info.kb[i];
        line->size[line->index+j] = (kb.echo) ? TRUE : FALSE;  /* phx21233 R.L. - update display length */
        i = (i+1)%KILL_BUFF_SIZE;
    }

    /* update the screen and cursor */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));
    cursor_right(line,kill_info.yank_size);

    /* update flag to indicate current item is not first item in    */
    /* kill buffer                                                  */
    kill_info.head_of_killbuff = FALSE;
}



/*
**********************************************************************

  Routine:            SAVE_TO_KILLBUFF 

  Function:
      This routine saves an item (one or more characters) to the kill 
  buffer for subsequent retrieval via the yank() and yank_previous() 
  routines. The kill buffer is of fixed sized and when an item being 
  saved is larger than the space available in the kill buffer, then 
  room is created by freeing up previously stored items (from the 
  least recently saved to the most recently saved) until enough room 
  has been created. 

  Parameters:
     (input)          str - specifies the buffer containing the item 
                          to save 
     (input)          item_size - specifies the number of characters 
                          from the specified buffer to save 

  Returns:            NONE 

**********************************************************************/

save_to_killbuff(str,item_size)
char *str;
int item_size;
{
    int i, j;
    int lo_byte;
    int hi_byte;
    int first_chunk;
    int buffer_empty;

    /* check for item too big to fit into killbuffer; this should */
    /* not normally happen since the size of largest item is an */
    /* input line that is completely full, and the size of the line */
    /* buffer is currently less than the size of the kill buffer */
    if (item_size+TOTAL_SIZE_SPEC_BYTES > KILL_BUFF_SIZE)

        /* truncate size of item so save so that it fits and continue */
        item_size = KILL_BUFF_SIZE - TOTAL_SIZE_SPEC_BYTES;


    /* delete items from tail until enough room */
    while (kill_info.chars_free < item_size+TOTAL_SIZE_SPEC_BYTES) {

        i = (kill_info.tail + kill_info.tail_item_size) % KILL_BUFF_SIZE;

        /* if insertion will overwrite all existing items */
        if (i == kill_info.head && kill_info.chars_free == NONE) {

            /* reinitialize everything */
            init_killbuff();
            break;
        }

        /* determine size of least recently saved item */
        kill_info.tail = i;
        lo_byte = kill_info.kb[i];
        i = (i+1)%KILL_BUFF_SIZE;
        hi_byte = kill_info.kb[i];

        /* delete least recently saved item from kill buffer */
        kill_info.chars_free += kill_info.tail_item_size;
        kill_info.tail_item_size = (hi_byte * MAX_7_BIT_VAL) + lo_byte;
    }

    /* set flag to determine whether kill buffer is now empty */
    if (kill_info.head == kill_info.tail && kill_info.chars_free == KILL_BUFF_SIZE)
        buffer_empty = TRUE;
    else
        buffer_empty = FALSE;

    /* determine space taken up in kill buffer for new item */
    kill_info.head_item_size = item_size+TOTAL_SIZE_SPEC_BYTES;
    kill_info.chars_free -= kill_info.head_item_size;

    /* size of item is to be stored in first two and last two       */
    /* bytes of item in kill buffer; determine the values of the    */
    /* size bytes                                                   */
    lo_byte = kill_info.head_item_size % MAX_7_BIT_VAL;
    hi_byte = kill_info.head_item_size / MAX_7_BIT_VAL;

    /* store into first two bytes of item */
    i = kill_info.head;
    kill_info.kb[i] = lo_byte;
    i = (i+1)%KILL_BUFF_SIZE;
    kill_info.kb[i] = hi_byte;
    i = (i+1)%KILL_BUFF_SIZE;

    /* copy across end of buffer to beginning of buffer? */
    if (i+item_size > KILL_BUFF_SIZE) {

        /* determine amount to copy up to end of buffer */
        first_chunk = KILL_BUFF_SIZE - i;

        /* determine amount to copy starting from beginning of */
        /* buffer */
        item_size -= first_chunk;

        /* copy item to kill buffer */
        for (j = 0; j < first_chunk; j++)
            kill_info.kb[i++] = str[j];
        for (i = 0; i < item_size; i++)
            kill_info.kb[i] = str[j++];

    }

    /* no buffer wrap around, do straight copy */
    else {
        for (j = 0; j < item_size; j++)
            kill_info.kb[i++] = str[j];
    }

    /* last two bytes of item holds size of item in kill buffer */
    i = i % KILL_BUFF_SIZE;
    kill_info.kb[i] = lo_byte;
    i = (i+1) % KILL_BUFF_SIZE;
    kill_info.kb[i] = hi_byte;
    i = (i+1) % KILL_BUFF_SIZE;

    /* if buffer was empty, make tail and head the same */
    if (buffer_empty) {
        kill_info.tail = kill_info.head;
        kill_info.tail_item_size = kill_info.head_item_size;
    }

    /* head index points to next location in kill buffer to add */
    /* next item */
    kill_info.head = i;

    /* set current item index to most currently saved item index */
    kill_info.current = kill_info.head;

    /* current item size to currently saved item size */
    kill_info.current_size = kill_info.head_item_size;

    /* index 'current' is now same as index 'head' */
    kill_info.head_of_killbuff = TRUE;

    /* reset values used for yanking */
    kill_info.yank_index = ZERO_INDEX_VALUE;
    kill_info.yank_size = ZERO_BYTES;

}



/*
**********************************************************************

  Routine:            INIT_KILLBUFF 

  Function:
      This routine initializes the kill buffer to indicate an empty 
  kill buffer. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

init_killbuff()
{

    /* all item indices set to 0 (beginning of kill buffer) and     */
    /* all item sizes set to 0 (no items)                           */

    kill_info.head = ZERO_INDEX_VALUE;
    kill_info.head_item_size = ZERO_BYTES;
    kill_info.tail = ZERO_INDEX_VALUE;
    kill_info.tail_item_size = ZERO_BYTES;
    kill_info.current = ZERO_INDEX_VALUE;
    kill_info.current_size = ZERO_BYTES;
    kill_info.chars_free = KILL_BUFF_SIZE;

    /* 'current' is the same as 'head' */
    kill_info.head_of_killbuff = TRUE;

    /* no items previously yanked */
    kill_info.yank_index = ZERO_INDEX_VALUE;
    kill_info.yank_size = ZERO_BYTES;
}

