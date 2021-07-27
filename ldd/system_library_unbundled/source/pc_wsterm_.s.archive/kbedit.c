/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-06-13,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Created.
  2) change(88-07-22,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added support routines to capitalize, uppercase, lowercase
     and transpose words; added support routine to transpose characters.
  3) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  4) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  5) change(88-09-16,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Modified line terminator to be always a linefeed for edit-mode;
     lfecho and crecho only applies to fullduplex modes.
  6) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Modified to save only echoed input when items are killed to the
     kill buffer and when lines are saved to history buffer, kill buffer and
     audit buffers.
                                                   END HISTORY COMMENTS */

#include <stdio.h>
#include <dos.h>
#include <ctype.h>
#include "wstdefs.h"
#include "wstglob.h"
#include <wsmincap.h>

/*
**********************************************************************

  Routine:            DELETE_CHARS 

  Function:
      This routine will delete a specified number of characters at 
  the logical cursor position on the line and force the line to be 
  updated on the screen. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          n_chars - specifies the number of characters to 
                          delete at the current cursor position
     (input)          save_sw - if TRUE, specifies that the deleted
                          characters are to be put into the kill buffer

  Returns:            NONE 

**********************************************************************/

delete_chars(line,n_chars,save_sw)
EDIT_LINE *line;
int n_chars;
{
    int chars_to_eol;
    int chars_to_del;
    int i, j;
    char *echoed_input;
    int echoed_length;

    /* determine number of characters from logical cursor position  */
    /* to end of line                                               */
    chars_to_eol = line->length - line->index;

    /* make sure number of characters to delete does not exceed     */
    /* number of characters to the end of line                      */
    if (chars_to_eol < n_chars)
        chars_to_del = chars_to_eol;
    else
        chars_to_del = n_chars;

    /* save item to killbuffer if save switch is on */
    if (save_sw && chars_to_del > 0 && kb.echo) {
        /* phx21233 R.L. - save only echoed input to kill buffer */
        get_echoed_input(line,line->index,chars_to_del,&echoed_input,
            &echoed_length);
        save_to_killbuff(echoed_input,echoed_length);
    }

    /* copy rest of line over the deleted part of the line */
    for (i = line->index, j = line->index+chars_to_del;
        j < line->length; i++,j++) {

        /* copy over text in line buffer */
        line->line[i] = line->line[j];

        /* copy over count of display chars for text characters */
        line->size[i] = line->size[j];
    }

    /* update line length */
    line->length -= chars_to_del;

    /* force redraw from logical cursor position to end of line */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));
}



/*
**********************************************************************

  Routine:            KILL_TO_BOL 

  Function:
      This routine deletes all text from the current cursor position 
  to the end of the line and then updates the screen. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 

  Returns:            NONE 

**********************************************************************/

kill_to_bol(line)
EDIT_LINE *line;
{
    int i;
    int n_chars;

    /* do only if logical cursor not already at beginning of line */
    if (line->index > 0) {

        /* number of characters to delete is logical cursor         */
        /* position                                                 */
        n_chars = line->index;

        /* check if beginning of line has scrolled off top of       */
        /* screen                                                   */
        if (line->scrolled_flag) {

            /* redisplay line from top of the screen */
            set_cursor(ss.top,ss.left);

            /* pad over to original display column */
            for (i = ss.left; i < line->orig_col; i++)
                putch(' ');

            /* update line cursor position information */
            line->orig_row = ss.top;
            line->scrolled_flag = FALSE;

            /* move physical and logical cursor to beginning of     */
            /* line                                                 */
            line->cur_row = ss.top;
            line->cur_col = line->orig_col;
            set_cursor(line->cur_row,line->cur_col);
            line->index = 0;

            /* delete the characters and force a redraw */
            delete_chars(line,n_chars,KILLBUFF_SAVE);
        }

        /* coordinates for beginning of line on screen */
        else {

            /* move logical and physical cursor to beginning of     */
            /* line                                                 */
            line->index = 0;
            line->cur_row = line->orig_row;
            line->cur_col = line->orig_col;
            set_cursor(line->cur_row,line->cur_col);

            /* perform the delete and force a screen update */
            delete_chars(line,n_chars,KILLBUFF_SAVE);
        }
    }
}


/*
**********************************************************************

  Routine:            KILL_TO_EOL 

  Function:
      This routine deletes all text from the current cursor position 
  to the end of the line, updating the screen. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited 

  Returns:            NONE 

**********************************************************************/

kill_to_eol(line)
EDIT_LINE *line;
{

    /* if not already at end of the line */
    if (line->index < line->length)

        /* delete the number of characters from the end of the      */
        /* line to the current logical cursor position              */

        delete_chars(line,line->length-line->index,KILLBUFF_SAVE);
}



/*
**********************************************************************

  Routine:            ADD_TEXT 

  Function:
      This routine adds text into the line being edited 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line 
     (input)          ch - specifies the character to add as text to 
                          the line being edited 
     (input)          n_times - specifies the number of times that 
                          the text character is to be added 

  Returns:            NONE 

**********************************************************************/

add_text(line,ch,n_times)
EDIT_LINE *line;
int ch;
int n_times;
{

    /* determine if editing is in replace mode */
    if (!line->mode)
        replace_text(line,ch,n_times);

    /* if not, then is in insert mode */
    else
        insert_text(line,ch,n_times);
}



/*
**********************************************************************

  Routine:            INSERT_TEXT 

  Function:
      This routine displays and inserts one or more characters into 
  the edit line buffer and updates the screen. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line 
     (input)          ch - specifies the ASCII character to insert 
                          into the line edited as text 
     (input)          n_times - specifies the number of times that 
                          the specified character is to be inserted 

  Returns:            TRUE if successful 
                      FALSE if unsuccessful 

**********************************************************************/

insert_text(line,ch,n_times)
EDIT_LINE *line;
int ch;
int n_times;
{
    CURPOS tpos;
    int i, j;
    int actual_times;
    int chars_to_eol;

    /* determine how much space is available to insert text */
    chars_to_eol = MAX_LINE_SIZE - line->length;

    /* check for 0 or less characters to insert */
    if (n_times < 1)
        return(FALSE);

    /* check for no more space in edit line buffer for insert */
    if (chars_to_eol < 1) {
        beep();
        return(FALSE);
    }

    /* make sure number of characters to insert does not exceed     */
    /* space available for insert                                   */
    if (n_times > chars_to_eol) {
        actual_times = chars_to_eol;
        beep();
    }
    else
        actual_times = n_times;

    /* initialize cursor position information */
    init_temp_curpos(&tpos,line);

    /* make a gap in the line buffer for the insert */
    for (i = line->length+actual_times, j = line->length;
        j >= line->index; i--, j--) {
        line->line[i] = line->line[j];
        line->size[i] = line->size[j];
    }

    /* copy the text into the gap */
    for (i = line->index, j = 0; j < actual_times; i++, j++) {
        line->line[i] = ch;
        line->size[i] = display_char(&tpos,ch);
    }

    /* update the line length and logical cursor position */
    line->length += actual_times;
    line->index += actual_times;

    /* update cursor position information */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* update the changes to the screen */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));
    return(TRUE);
}



/*
**********************************************************************

  Routine:            REPLACE_TEXT 

  Function:
      This routine replaces the character(s) at the cursor position 
  in the edit line and updates the screen 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          key - specifies the ASCII character to replace 
                          the current character(s) with 
     (input)          n_times - specifies the number of characters to 
                          replace with the specified ASCII character 

  Returns:            FALSE if unsuccessful 
                      TRUE if successful 

**********************************************************************/

replace_text(line,key,n_times)
EDIT_LINE *line;
int key;
int n_times;
{
    CURPOS tpos;
    int i;
    int ch;
    int actual_times;
    int chars_to_eol;

    /* determine space available for adding text */
    chars_to_eol = MAX_LINE_SIZE - line->index;

    /* do nothing if replacing 0 or less characters */
    if (n_times < 1)
        return(FALSE);

    /* beep and return if no more space available to add text */
    if (chars_to_eol < 1) {
        beep();
        return(FALSE);
    }

    /* make sure number of characters to replace does not exceed    */
    /* space available; beep if it does                             */
    if (n_times > chars_to_eol) {
        actual_times = chars_to_eol;
        beep();
    }
    else
        actual_times = n_times;

    /* get character to replace */
    ch = key;

    /* initialize structure for keeping track of cursor position */
    init_temp_curpos(&tpos,line);

    /* perform the replace and display the character(s) being       */
    /* added                                                        */
    for (i = 0; i < actual_times; i++) {
        line->line[line->index] = ch;
        line->size[line->index] = display_char(&tpos,ch);
        line->index++;
    }

    /* update line length if line got extended */
    if (line->index > line->length) line->length = line->index;

    /* update cursor position information in line structure */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* update the changes to the screen */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));

    return(TRUE);
}



/*
**********************************************************************

  Routine:            FORWARD_DELETE_WORD 

  Function:
      This routine deletes text to the right of the cursor up to the 
  end of the next word in the line. The screen is then updated. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          n_times - specifies the number of times to 
                          repeat this delete function 

  Returns:            NONE 

**********************************************************************/

forward_delete_word(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;

    /* do nothing if already at end of the line */
    if (line->index >= line->length) return;

    /* get current logical cursor position */
    i = line->index;

    /* perform for the specified number of times or until end of    */
    /* line reached                                                 */
    while (n_times > 0 && i < line->length) {

        /* skip any word delimiters */
        while (i < line->length && is_word_delim(line->line[i]))
            i++;

        /* skip until word delimiter reached */
        while (i < line->length && !is_word_delim(line->line[i]))
            i++;

        /* tally number of times performed */
        n_times--;

    }

    /* check if new position is different */
    if (i == line->index)
        return;

    /* delete the difference in positions and update the screen */
    delete_chars(line,i-line->index,KILLBUFF_SAVE);
}


/*
**********************************************************************

  Routine:            BACKWARD_DELETE_WORD 

  Function:
      This routine deletes text to the left of the cursor up to the 
  beginning of the previous word. The screen is updated. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          n_times - specifies the number of times to 
                          perform this function 

  Returns:            NONE 

**********************************************************************/

backward_delete_word(line,n_times)
EDIT_LINE *line;
int n_times;
{
    int i;
    int n_chars;

    /* do only if not at beginning of line */
    if (line->index < 1) return;

    /* begin at one position before the cursor */
    i = line->index - 1;

    /* do the specified number of times or until beginning of line  */
    /* reached                                                      */
    while (n_times > 0 && i >= 0) {

        /* skip word delimiters to the left */
        while (i >= 0 && is_word_delim(line->line[i]))
            i--;

        /* skip word to the left */
        while (i >= 0 && !is_word_delim(line->line[i]))
            i--;

        /* tally the times performed */
        n_times--;
    }

    /* restore cursor position one position to the right */
    i++;

    /* determine the number of characters to the left to delete */
    n_chars = line->index - i;

    /* move the physical and logical cursor there, updating the     */
    /* screen                                                       */
    cursor_left(line,n_chars);

    /* perform the delete and update the screen */
    delete_chars(line,n_chars,KILLBUFF_SAVE);
}



/*
**********************************************************************

  Routine:            ADD_CHAR 

  Function:
      This routine adds a character into the line being edited and 
  updates the screen without moving the cursor forward. This routine 
  is used for handling literal escaping of characters. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          ch - specifies the ASCII character to add into 
                          the line 

  Returns:            TRUE if successful 
                      FALSE if unsuccessful 

**********************************************************************/

add_char(line,ch)
EDIT_LINE *line;
int ch;
{
    int code;

    /* determine if in edit replace mode */
    if (!line->mode)
        code = replace_char(line,ch);

    /* no, in edit insert mode */
    else
        code = insert_char(line,ch);

    /* return the status to caller */
    return(code);
}



/*
**********************************************************************

  Routine:            REPLACE_CHAR 

  Function:
      This routine will replace the current character of the line 
  being edited. The cursor position is NOT changed. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          key - specifies the ASCII character to replace 
                          the current character with 

  Returns:            FALSE if unsuccessful 
                      TRUE if successful 

**********************************************************************/

replace_char(line,key)
EDIT_LINE *line;
int key;
{
    CURPOS tpos;
    int ch;
    int chars_to_eol;

    /* determine space available for replacing line buffer text */
    chars_to_eol = MAX_LINE_SIZE - line->index;

    /* if no more space left, beep and return */
    if (chars_to_eol < 1) {
        beep();
        return(FALSE);
    }

    /* get character to insert */
    ch = key;

    /* initialize cursor positioning information */
    init_temp_curpos(&tpos,line);

    /* do the replace and display the new character */
    line->line[line->index] = ch;
    line->size[line->index] = display_char(&tpos,ch);

    /* update the line length if line is extended */
    if (line->index+1 > line->length) line->length = line->index+1;

    /* update cursor position */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* update the line to the screen */
    redisplay(line,line->index+1,&(line->max_row),&(line->max_col));

    return(TRUE);
}



/*
**********************************************************************

  Routine:            INSERT_CHAR 

  Function:
      This routine inserts a character into the line being edited. 
  The cursor position is not moved forward. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          ch - specifies the ASCII character to insert 
                          into the line 

  Returns:            TRUE if successful 
                      FALSE if unsuccessful 

**********************************************************************/

insert_char(line,ch)
EDIT_LINE *line;
int ch;
{
    CURPOS tpos;
    int i, j;

    int chars_to_eol;

    /* determine space available in line for inserting */
    chars_to_eol = MAX_LINE_SIZE - line->length;

    /* beep and return if no space for insert */
    if (chars_to_eol < 1) {
        beep();
        return(FALSE);
    }

    /* initialize cursor position information */
    init_temp_curpos(&tpos,line);

    /* create gap in line buffer for the character to insert */
    for (i = line->length+1, j = line->length;
        j >= line->index; i--, j--) {
        line->line[i] = line->line[j];
        line->size[i] = line->size[j];
    }

    /* copy in the character */
    line->line[line->index] = ch;
    line->size[line->index] = display_char(&tpos,ch);

    /* update the line length */
    line->length++;

    /* update the cursor position information */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* update the line to the screen */
    redisplay(line,line->index+1,&(line->max_row),&(line->max_col));
    return(TRUE);
}



/*
**********************************************************************

  Routine:            INSERT_DISPLAY_CHAR 

  Function:
      This routine inserts characters on the screen only. It allows 
  partial input of a backslash sequence to be displayed before it is 
  entered into the line being edited. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited 
     (input)          ch - specifies the character to insert at the 
                          cursor position in the screen 

  Returns:            NONE 

**********************************************************************/

insert_display_char(line,ch)
EDIT_LINE *line;
int ch;
{
    CURPOS tpos;

    /* initialize cursor position information */
    init_temp_curpos(&tpos,line);

    /* store the character in the buffer for handling literal       */
    /* escaping                                                     */
    line->literal_buff[line->literal_dex++] = ch;

    /* display the character */
    display_char(&tpos,ch);

    /* update the line structure with cursor position information */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* update the line to the screen */
    redisplay(line,line->index+1,&(line->max_row),&(line->max_col));
}



/*
**********************************************************************

  Routine:            CHANGE_CASE_WORD 

  Function:
      This routine converts all alphabetic characters in the current 
  word of the line being edited (in edit mode) to a specified case. If
  the cursor is not on a word, the word to the left of the cursor is 
  converted to the specified case. The screen is then updated and the
  cursor is positioned at the end of the word that has been converted.
  The cases specified may be upper case, lower case or capitalized.

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited (in edit mode) 
     (input)          case_flag - specifies the case to convert the
                          word(s) to:
                          KB_UPPER_CASE for upper case,
                          KB_LOWER_CASE for lower case,
                          KB_CAPITALIZED for first letter of word
                              in upper case, and rest in lower case

  Returns:            NONE 

**********************************************************************/

change_case_word(line,case_flag)
EDIT_LINE *line;
int case_flag;
{
    int i;
    int orig_index;

    /* get copy of keyboard buffer index */
    i = line->index;
    if (i >= line->length) i--;
    orig_index = i;

    /* cursor on word delimiter or space? */
    if (is_word_delim(line->line[i])) {

        /* go left to previous word */
        while (i >= 0 && is_word_delim(line->line[i])) i--;

        /* check for no word on left */
        if (i < 0) {

            /* no previous word, move right to use next word */
            i = orig_index;
            while (i < line->length && is_word_delim(line->line[i])) i++;

            /* past end of line, is a blank line, do nothing */
            if (i >= line->length)
                return;
        }
    }

    /* make sure is at beginning of word */
    while (i >= 0 && !is_word_delim(line->line[i])) i--;
    i++;

    /* move physical cursor to beginning of the word for case change */
    if (i < line->index)
        cursor_left(line,line->index-i);
    else if (i > line->index)
        cursor_right(line,i-line->index);

    /* lower case first character if LOWER CASE specified */
    if (case_flag == KB_LOWER_CASE)
        line->line[i] = tolower(line->line[i]);

    /* upper case first character for UPPER CASE or CAPITALIZE */
    else
        line->line[i] = toupper(line->line[i]);
    i++;

    /* handle rest of word */
    while (i < line->length && !is_word_delim(line->line[i])) {

        /* upper case rest of word if UPPER CASE */
        if (case_flag == KB_UPPER_CASE)
            line->line[i] = toupper(line->line[i]);

        /* lower case rest of word if LOWER CASE or CAPITALIZE */
        else
            line->line[i] = tolower(line->line[i]);

        i++;

    }

    /* update the screen */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));

    /* position cursor to end of last word upper cased */
    cursor_right(line,i-line->index);
}



/*
**********************************************************************

  Routine:            TRANSPOSE_CHARS 

  Function:
      This routine exchanges the positions of the two characters to 
  the left of the cursor. If there isn't two characters to the left 
  of the cursor, the routine will sound a beep and do nothing. The 
  screen is updated and the cursor position is unchanged. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited (in edit mode) 

  Returns:            NONE 

**********************************************************************/

transpose_chars(line)
EDIT_LINE *line;
{
    int i;
    int tmp_ch;

    /* check if two characters exist to left of cursor */
    if (line->length < 2 || line->index < 2) {
        beep();
        return;
    }

    /* position temporary index to two positions before cursor */
    i = line->index - 2;

    /* move cursor two characters back */
    cursor_left(line,2);

    /* swap the two characters */
    tmp_ch = line->line[i];
    line->line[i] = line->line[i+1];
    line->line[i+1] = tmp_ch;

    /* update the screen */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));

    /* restore cursor position */
    cursor_right(line,2);
}



/*
**********************************************************************

  Routine:            TRANSPOSE_WORDS 

  Function:
      This routine interchanges the positions of the current word 
  (under the cursor) and the word to its left. If the cursor is not 
  on a word, the two words to the left of the cursor are 
  interchanged. The screen is updated and the cursor is positioned at 
  the end of the interchanged words. If there are no words to 
  interchange (e.g. at beginning of the line), the routine will beep 
  and do nothing. 

  Parameters:
     (input/output)   line - specifies the structure containing the 
                          line and information about the line being 
                          edited (in edit mode) 

  Returns:            NONE 

**********************************************************************/

transpose_words(line)
EDIT_LINE *line;
{
    int i;
    int word1_index, word1_len;
    int word2_index, word2_len;
    int delim_len;
    char *tmp_buff, *malloc();
    int tmp_index;
    int final_index;

    /* get cursor position and make sure it points to valid data */
    /* if at end of line */
    i = line->index;
    if (i >= line->length) i--;

    /* check if in middle of a word */
    if (!is_word_delim(line->line[i]))
        /* go to end of word */
        while (i < line->length && !is_word_delim(line->line[i]))
            i++;

    /* go left to right, skip trailing blanks */
    i--;
    while (i >= 0 && is_word_delim(line->line[i]))
       i--;

    /* save the index to the end of this second word */
    final_index = i+1;

    /* determine length and starting position of this second word */
    word2_len = 0;
    while (i >= 0 && !is_word_delim(line->line[i])) {
        i--;
        word2_len++;
    }
    word2_index = i+1;

    /* no word to transpose with if past beginning of line */
    if (i < 0) {
        beep();
        return;
    }

    /* determine number of delimiter characters to left of second word */
    delim_len = 0;
    while (i >= 0 && is_word_delim(line->line[i])) {
       delim_len++;
       i--;
    }

    /* no words to transpose with if past beginning of line */
    if (i < 0) {
        beep();
        return;
    }

    /* determine length and position of first word */
    word1_len = 0;
    while (i >= 0 && !is_word_delim(line->line[i])) {
        i--;
        word1_len++;
    }
    word1_index = i+1;

    /* allocate buffer big enough to hold both words and delimiter */
    tmp_buff = malloc(word1_len+delim_len+word2_len+1);
    if (tmp_buff == NULL) return;

    /* move cursor to beginning of first word */
    i = line->index - word1_index;
    cursor_left(line,i);

    /* copy the two words and delimiter in between with the word positions
       interchanged to temporary buffer
    */
    tmp_index = 0;
    for (i = 0; i < word2_len; i++)
        tmp_buff[tmp_index++] = line->line[word2_index+i];
    for (i = 0; i < delim_len; i++)
        tmp_buff[tmp_index++] = line->line[word1_index+word1_len+i];
    for (i = 0; i < word1_len; i++)
        tmp_buff[tmp_index++] = line->line[word1_index+i];

    /* copy back from temporary buffer to line buffer */
    for (i = 0; i < tmp_index; i++)
        line->line[word1_index+i] = tmp_buff[i];

    /* deallocate temporary buffer */
    free(tmp_buff);

    /* update the screen */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));

    /* position cursor at end of words */
    cursor_right(line,final_index-line->index);
}



/*
**********************************************************************

  Routine:            KB_EDIT_LF_RETURN 

  Function:
      This function passes processes a line in edit-mode when a
  linefeed or carriage return is entered, before the line is to
  be sent to the host. Processing includes moving the cursor to
  the end of the line, saving the line in the history and kill
  buffer, file or printer audit the lines if auditing is enabled,
  and updating any cursor position variables. The line is then
  sent to the host.

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line to be 
                          saved and sent to the host 
     (input)          key - specifies the line terminator key that
                          was entered

  Returns:            NONE 

**********************************************************************/

kb_edit_lf_return(line,key)
EDIT_LINE *line;
int key;
{
    char *dptr;
    char *echoed_input;
    int  echoed_length;

    /* position cursor to end of line (to prevent incoming messages
       from overwriting the input
    */
    cursor_eol(line);

    /* Handle moving the cursor to the next line. */
    /* Don't move to beginning of next line if CR hit and mowse is */
    /* not active; in glass tty mode, when using edit mode, */
    /* the terminal modes should be ^echoplex,lfecho. A linefeed */
    /* will be echoed from Multics anyways. */

    if (!mowse_active && key == CR)
        line->cur_col = ss.left;

    else if (line->cur_row >= ss.bottom) {
        wst_scroll();
        line->cur_row = ss.bottom;
        line->cur_col = ss.left;
    }
    else {
        line->cur_row++;
        line->cur_col = ss.left;
    }

    cursor_move(line->cur_row,line->cur_col);

    /* re-initialize screen information */
    screen.curlin = line->cur_row;
    screen.curcol = line->cur_col;
    screen.EOP_ct = 0;                 /* input resets page line counter */

    /* phx21233 R.L. - saved only echoed input to history and kill buffers */
    get_echoed_input(line,0,line->length,&echoed_input,&echoed_length);

    /* non-empty lines saved to history buffer */
    if (echoed_length > 0) {
        save_to_histbuff(echoed_input,echoed_length);
        save_to_killbuff(echoed_input,echoed_length);
    }

    if (mowse_active)

        /* terminate line with line delimiter; a linefeed character is */
        /* always used so the line gets interpreted by the host; the */
        /* modes lfecho and crecho do not affect edit-mode since */
        /* characters are echoed locally, and not by the host. The lfecho */
        /* and crecho modes are meaningful in full duplex */

        line->line[line->length++] = LF;

    /* otherwise send whatever line terminator was entered */
    else
        line->line[line->length++] = key;

    /* terminate string */
    line->line[line->length] = NUL_TERMINATOR;

    /* phx21233 R.L. - LF terminate echoed input and audit only echoed input */
    echoed_input[echoed_length++] = LF;

    /* audit input if necessary */
    if (wst_f_audit)
        f_audit_msg(echoed_input,echoed_length);

    if (wst_p_audit)
        p_audit_msg(echoed_input,echoed_length,NULL);

    /* just send the message to host if not in glass tty mode */
    if (mowse_active)
        puttdata (FG_TERMINAL_DATA, line->line, line->length);

    /* kludge to minimize line overrun for long input lines when */
    /* Multics is echoing; this problems shows up when the user */
    /* is in non-packet (glass tty) mode and long input lines are */
    /* echoed. The echoed input from Multics sometimes causes the PC */
    /* to stall, resulting in a Multics interpreting a QUIT signal. */
    /* This fix slows long input from being sent too quickly so that */
    /* echoed characters are received less quickly. */
    else {
        /* send characters out in 4 character chunks, delaying in between */
        dptr = line->line;
        while (line->length > 4) {

            puttdata(FG_TERMINAL_DATA, dptr, 4);
            dptr += 4;
            line->length -= 4;
            delay();
        }

        puttdata(FG_TERMINAL_DATA, dptr, line->length);
    }

    /* reset edit mode line length and buffer index to indicate 0 length */
    line->length = 0;
    line->index = 0;
}
