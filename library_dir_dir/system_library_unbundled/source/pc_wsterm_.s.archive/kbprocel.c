/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-06-13,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     created.
  2) change(88-07-11,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added calls to history recall routines.
  3) change(88-07-22,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added command keys to capitalize, upper case, lower case,
     transpose words and to transpose characters.
  4) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  5) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  6) change(88-09-16,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Linefeed key (^J) modified to send input line, just like carriage
     return.
  7) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Modified to support the displaying and re-displaying of echoed
     input only.
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include <ctype.h>
#include "wstdefs.h"
#include "wstglob.h"
#include <wsmincap.h>

/*
**********************************************************************

  Routine:            PROCESS_EDIT_LINE 

  Function:
      This routine calls checks for keyboard input and if detected, 
  passes it to the edit mode keyboard routines for processing. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line being edited in edit mode 

  Returns:            1 if the <RETURN> key was processed, signifying 
                          the end of editing for the current input 
                          line. 
                      0 otherwise 

  Note 1 - All error handling is done within this module. Line 
      editing errors such as buffer overflow, invalid key, etc. are 
      normally handled by beeping and discarding of the invalid 
      input. 
**********************************************************************/

process_edit_line(line)
EDIT_LINE *line;
{
    int ch;      /* character read in */
    int code;    /* error code */

    /* process only if keyboard key hit */
    if ((ch = getkey(KB_NO_BLOCK)) < 0)
        return(0);

    /* initialize if getting a new line of input; new line if not
       currently handling escape arguments and no input buffered
    */
    if (line->length == 0 && !line->escape_flag && line->escape_arg < 0) {
        init_curpos(line);
        line->index = ZERO_INDEX_VALUE;
    }

    /* handle the character read in */
    code = proc_input(line,ch);
    return(code);
}



/*
**********************************************************************

  Routine:            UPDATE_NUM_LITERAL 

  Function:
      This routine is called to handle the case of the backslash 
  character followed by an octal value. The octal value is converted 
  to an ASCII character and placed into the line buffer. The screen 
  is then updated. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line (buffer) and information about the 
                          line being edited in edit mode 

  Returns:            NONE 

**********************************************************************/

update_num_literal(line)
EDIT_LINE *line;
{
    CURPOS tpos;

    /* if there are octal digits in backslash escape */
    if (line->literal_dex > 0) {

        /* back up the cursor to whatever number of octal digits were
           entered plus one more for the backslash in preparation for
           over-writing the backslash escape sequence
        */
        if (kb.echo)    /* phx21233 R.L. - backup only if echo enabled */
            backup_cursor(line,line->literal_dex+1);

        /* initialize cursor information structure */
        init_temp_curpos(&tpos,line);

        /* null terminate the octal string entered */
        line->literal_buff[line->literal_dex] = NUL_TERMINATOR;

        /* convert octal string to ASCII value and assign to current
           character in keyboard buffer
        */
        line->line[line->index] = octal_val(line->literal_buff);

        /* redisplay it */
        line->size[line->index] = display_char(&tpos,line->line[line->index]);

        /* position to next keyboard character */
        line->index++;

        /* update screen cursor position in line structure */
        line->cur_row = tpos.cur_row;
        line->cur_col = tpos.cur_col;
        line->orig_row -= tpos.scroll_flag;
        if (line->orig_row < ss.top)
            line->scrolled_flag = TRUE;

        /* redisplay rest of line after the current character */
        redisplay(line,line->index,&(line->max_row),&(line->max_col));

        /* reset index to indicate not processing backslash escape */
        line->literal_dex = ZERO_INDEX_VALUE;
    }

    /* no octal digits, only backslash entered; backslash is already
       displayed, just position to next keyboard character
    */
    else
        line->index++;

    /* reset escape argument flags */
    line->escape_flag = NO_ESC_FUNC;
    line->escape_arg = NO_ESC_ARG;
}



/*
**********************************************************************

  Routine:            UPDATE_NON_PRT_LITERAL 

  Function:
      This routine handles input from the keyboard for the case of a 
  backslash followed by a non-printable character. The character is 
  put into the line buffer and the screen is then updated. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line (buffer) and information about the 
                          line being edited in edit mode 
     (input)          ch - specifies the non-printable ASCII 
                          character following the backslash that was 
                          entered 

  Returns:            NONE 

**********************************************************************/

update_non_prt_literal(line,ch)
EDIT_LINE *line;
int ch;          /* the non-printable character following backslash */
{
    CURPOS tpos;

    /* back the cursor over the backslash */
    if (kb.echo)    /* phx21233 R.L. - backup only if echo enabled */
        backup_cursor(line,1);

    /* initialize cursor information */
    init_temp_curpos(&tpos,line);

    /* current keyboard character is assigned the non-printable character */
    line->line[line->index] = ch;

    /* display the non-printable character */
    line->size[line->index] = display_char(&tpos,ch);

    /* position to next keyboard character */
    line->index++;

    /* update the cursor information in the line structure */
    line->cur_row = tpos.cur_row;
    line->cur_col = tpos.cur_col;
    line->orig_row -= tpos.scroll_flag;
    if (line->orig_row < ss.top)
        line->scrolled_flag = TRUE;

    /* display the line following the non-printable character */
    redisplay(line,line->index,&(line->max_row),&(line->max_col));

    /* reset escape argument flags */
    line->escape_flag = NO_ESC_FUNC;
    line->escape_arg = NO_ESC_ARG;
}



/*
**********************************************************************

  Routine:            PROC_INPUT 

  Function:
      This routine handles keyboard input in edit mode a key at a 
  time. The module examines the key to determine whether it is to be 
  handled as text input or as an editing key. Also, numeric escape 
  argument (such as "<ESC> 3" to repeat a command 3 times) and 
  literal escaping (such as "\ <BACKSPACE>") is detected and 
  processed by this routine. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          key - specifies the keyboard key, ASCII or 
                          function or ALT key, which was entered 
                          while in edit mode 

  Returns:            1 if the <RETURN> key was processed, signifying 
                          the end of editing for the current line 
                      0 otherwise 

  Note 1 - All error handling is done within this module. Line 
      editing errors such as buffer overflow, invalid key, etc. are 
      normally handled by beeping and discarding the invalid input. 
**********************************************************************/

proc_input(line,key)
EDIT_LINE *line;
int key;
{
    int ch;
    int code;

    ch = key;

    /* check and map DEL, ^2 and ^BREAK keys */
    if (ch > ASCII_EXTEND_CODE) {

        /* map PC's DEL key to ASCII DEL */
        if (ch == DEL_KEY)
            ch = ASCII_DEL;

        /* map PC's ^2 key to ASCII NUL */
        else if (ch == NULL_KEY)
            ch = ASCII_NUL;

        /* map PC's ^BREAK to ALT_B; BREAK_KEY code is generated only
           if WSTERM's /B command argument is specified
        */
        else if (ch == BREAK_KEY)
            ch = ALT_B;
    }
 
    /* check if previous key was an escape or literal escape and
       that current key is not a function or ALT key
    */
    if (line->escape_flag > NO_ESC_FUNC) {

        /* previous key was a backslash */
        if (line->escape_flag == KB_LITERAL_ESC) {

            /* if function or ALT key, update the backslash and fall through */
            if (ch > ASCII_EXTEND_CODE)
                update_num_literal(line);

            /* check for octal digit */
            else if (ch >= SMALLEST_OCTAL_DIGIT && ch <= LARGEST_OCTAL_DIGIT) {
                /* display the digit and insert keep track in line structure */
                insert_display_char(line,ch);

                /* if three octal digits entered, process octal sequence */
                if (line->literal_dex >= MAX_OCTAL_SEQ_DIGITS) {
                    update_num_literal(line);
                }
                return(0);
            }

            /* handle backslash followed by non-printable character */
            else if (line->literal_dex == ZERO_INDEX_VALUE &&
                (ch < MIN_PRINTABLE_ASCII ||
                 ch > MAX_PRINTABLE_ASCII ||
                 ch == char_erase ||
                 ch == line_kill)) {
                update_non_prt_literal(line,ch);
                return(0);
            }

            /* backslash followed by a printable character */
            else if (ch < ASCII_DEL) {

                if (line->literal_dex == ZERO_INDEX_VALUE) {

                    /* update line structure */
                    update_num_literal(line);

                    /* if backslash followed by backslash, don't process
                       second backslash
                    */
                    if (ch == lnc) return(0);

                    /* fall through and handle the printable character */
                }

                /* backslash octal sequence followed by printable */
                else
                    /* update backslash sequence character first */
                    update_num_literal(line);

                    /* fall through and handle the printable char */
            }
        }

        /* check for escape key numeric argument */
        else if (ch >= SMALLEST_DECI_DIGIT && ch <= LARGEST_DECI_DIGIT) {

            /* if previous key was the escape key and no previous arguments */
            if (line->escape_flag == KB_PREV_KEY_ESC && line->escape_arg < 0)
                /* set initial escape argument */
                line->escape_arg = ch - '0';

            /* calculate into previously initialize escape argument */
            else if (line->escape_arg < MAX_ARG_LIMIT) {
                line->escape_arg *= 10;
                line->escape_arg += ch - '0';
            }

            /* change flag to indicate currently handling numeric arg */
            line->escape_flag = KB_HANDLING_ESC_ARG;
            return(0);
        }

        /* previous key was escape key, check for escape editing sequence */
        else if (line->escape_flag == KB_PREV_KEY_ESC) {
            code = handle_esc_editkeys(line,ch);
            return(code);
        }
    }

    /* handle the character as an edit key */
    code = handle_editkeys(line,ch);

    /* return if handled as an edit key */
    if (code >= 0)
        return(code);

    /* Handle adding straight text */
    if (ch < ASCII_DEL) {
        if (line->escape_arg < 0)
            add_text(line,ch,1);
        else
            add_text(line,ch,line->escape_arg);

        line->escape_flag = NO_ESC_FUNC;
        line->escape_arg = NO_ESC_ARG;
        return(0);
    }

    /* handle remaining function or ALT key hit */
    else if (ch > ASCII_EXTEND_CODE) {
        local_esc(ch - ASCII_EXTEND_CODE); /* pass it decoded scan code */
        return(0);
    }

    /* reinitialize escape argument flags and beep */
    line->escape_flag = NO_ESC_FUNC;
    line->escape_arg = NO_ESC_ARG;
    beep();
    return(0);
}



/*
**********************************************************************

  Routine:            HANDLE_ESC_EDITKEYS 

  Function:
      This routine handles editing keys which are preceeded by the 
  ESC key, e.g. ESC D (delete word). 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          ch - specifies which escape edit key is being 
                          handled 

  Returns:            -1 if the key specified is not a recognized 
                          escape editing key 
                      0 otherwise 

  Note 1 - Unrecognized keys are handled by beeping. Flags in the 
      line structure indicating numeric escape arguments and esc key 
      being hit are all reset regardlessly before this routine 
      returns. 
**********************************************************************/

handle_esc_editkeys(line,ch)
EDIT_LINE *line;
int ch;
{
    int arg_val;
    int error_code;

    /* initialize error code to pass back */
    error_code = 0;

    /* if no numeric argument given, assume action is done once */
    if (line->escape_arg < 0)
        arg_val = 1;

    /* otherwise perform as many times as specified by numeric argument */
    else
        arg_val = line->escape_arg;

    /* handle any recognized escape edit keys */
    /*  ... the variable 'code' is set to zero if key is recognized */
    switch (ch) {

        /* esc b, backward cursor word */
        case 'b':
        case 'B':
            /* perform for specified number of times */
            backward_word(line,arg_val);
            break;

        /* esc f, forward cursor word */
        case 'f':
        case 'F':
            /* perform for specified number of times */
            forward_word(line,arg_val);
            break;

        /* esc d, delete forward word */
        case 'd':
        case 'D':
            /* perform for specified number of times */
            forward_delete_word(line,arg_val);
            break;

        /* esc DEL, delete backward word */
        case BACKSPACE:
        case ASCII_DEL:
            /* perform for specified number of times */
            backward_delete_word(line,arg_val);
            break;

        /* esc U, upper case word */
        case 'u':
        case 'U':
            change_case_word(line,KB_UPPER_CASE);
            break;

        /* esc L, lower case word */
        case 'l':
        case 'L':
            change_case_word(line,KB_LOWER_CASE);
            break;

        /* esc C, capitalize word */
        case 'c':
        case 'C':
            change_case_word(line,KB_CAPITALIZED);
            break;

        /* esc T, transpose positions of previous two words */
        case 't':
        case 'T':
            transpose_words(line);
            break;

        /* esc Y, yank previous item from yank buffer */
        case 'y':
        case 'Y':
            yank_previous(line);
            break;

        /* key not a recognized escape edit key */
        default:
            beep();
            error_code = -1;
    }

    /* reset escape flag and argument */
    line->escape_flag = NO_ESC_FUNC;
    line->escape_arg = NO_ESC_ARG;

    return(error_code);
}



/*
**********************************************************************

  Routine:            HANDLE_EDITKEYS 

  Function:
      This routine examines a specified key to see if it is an 
  editing or ALT key. If so, it calls the appropriate routines to 
  handle the editing or ALT function, otherwise nothing is changed 
  and an error code is returned. 

  Parameters:
     (input/output)   line - pointer to the structure containing the 
                          line and information about the line being 
                          edited in edit mode 
     (input)          ch - specifies the key whose function needs to 
                          be identified and handled 

  Returns:            -1 if the key is not an editing key or special 
                          ALT key 
                      0 otherwise 

**********************************************************************/

handle_editkeys(line,ch)
EDIT_LINE *line;
int ch;
{
    int n_times;

    /* check for erase, kill or lnc; these characters are variable */
    if (ch == lnc) {

        /* handle literal escape character */
        /* add and display the literal escape character */
        if (add_char(line,ch))
            line->escape_flag = KB_LITERAL_ESC;

        /* error, must line must be full */
        else {
            /* reset escape argument and flag and beep */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            beep();
        }
        return(0);
    }

    else if (ch == char_erase || ch == BACKSPACE || ch == ASCII_DEL) {
        /* delete previous character */
        /* check for escape numeric argument */
        if (line->escape_arg < 0) {
            if (cursor_left(line,1) > 0)
                delete_chars(line,1,NO_KILLBUFF_SAVE);
        }
        else {
            n_times = cursor_left(line,line->escape_arg);
            delete_chars(line,n_times,NO_KILLBUFF_SAVE);
        }

        /* reset escape argument and flag */
        line->escape_flag = NO_ESC_FUNC;
        line->escape_arg = NO_ESC_ARG;
        return(0);
    }

    else if (ch == line_kill) {
        /* kill line */
        /* preceeded by literal escape char? */

        /* kill to beginning of line */
        kill_to_bol(line);

        /* reset escape argument and flag */
        line->escape_flag = NO_ESC_FUNC;
        line->escape_arg = NO_ESC_ARG;
        return(0);
    }

    switch (ch) {

        /* handle escape character */
        case ESC:
            /* if not handling an escape sequence or handling an
               escape numeric argument
            */
            if (line->escape_flag == NO_ESC_FUNC || line->escape_flag == 2)

                /* set to indicate previous key was an escape key */
                line->escape_flag = KB_PREV_KEY_ESC;

            /* escape followed by escape, reset flags and beep */
            else {
                line->escape_flag = NO_ESC_FUNC;
                line->escape_arg = NO_ESC_ARG;
                beep();
            }
            return(0);

        case TAB:
            return(-1);   /* not an editing key, treat as text */

        /* handle return key */
        case LINEFEED:
        case RETURN_KEY:
            kb_edit_lf_return(line,ch);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(1);

        /* move cursor one character to the left */
        case LEFT_ARROW_KEY:
        case CTRL_B:
            /* check for escape numeric argument */
            if (line->escape_arg < 0)
                cursor_left(line,1);
            else
                cursor_left(line,line->escape_arg);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* move cursor one character to the right */
        case RIGHT_ARROW_KEY:
        case CTRL_F:
            /* check for escape numeric argument */
            if (line->escape_arg < 0)
                cursor_right(line,1);
            else
                cursor_right(line,line->escape_arg);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* toggle editing replace/insert mode */
        case ALT_I:
        case INS_KEY:

            /* toggle line edit flag */
            line->mode = !line->mode;

            /* update the mode change on status line */
            update_status();
            return(0);

        /* move cursor to beginning of line */
        case CTRL_A:
        case HOME_KEY:
            cursor_bol(line);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* delete current character */
        case CTRL_D:
            /* check for escape numeric argument */
            if (line->escape_arg < 0)
                delete_chars(line,1,NO_KILLBUFF_SAVE);
            else
                delete_chars(line,line->escape_arg,NO_KILLBUFF_SAVE);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* move cursor to end of the line */
        case CTRL_E:
        case END_KEY:
            cursor_eol(line);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* kill to the end of the line */
        case CTRL_K:
            kill_to_eol(line);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* forward word */
        case CTRL_RIGHT_ARROW_KEY:

            /* handle no numeric argument */
            if (line->escape_arg < 0)
                forward_word(line,1);

            /* handle numeric argument */
            else
                forward_word(line,line->escape_arg);

            /* reset escape flag and argument */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* backward word */
        case CTRL_LEFT_ARROW_KEY:

            /* handle no numeric argument */
            if (line->escape_arg < 0)
                backward_word(line,1);

            /* handle numeric argument */
            else
                backward_word(line,line->escape_arg);

            /* reset escape flag and argument */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* fetch and display previous history command */
        case CTRL_P:
        case UP_ARROW_KEY:

            /* no escape argument specified? */
            if (line->escape_arg < 0) {

                /* check for unsuccessful retrieval of history      */
                /* command                                          */
                if (fetch_previous_command(1,line->line,&(line->length)) < 0) {
                    line->escape_flag = NO_ESC_FUNC;
                    line->escape_arg = NO_ESC_ARG;
                    return(0);
                }
            }

            /* handle escape argument */
            else {
                /* fetch unsuccessful? */
                if (fetch_nth_command(line->escape_arg,
                    line->line,&(line->length)) < 0) {
                    line->escape_flag = NO_ESC_FUNC;
                    line->escape_arg = NO_ESC_ARG;
                    return(0);
                }
            }

            /* phx21233 R.L. - update echo flags to determine visibility */
            /*    of chars redrawn */
            update_echo_flags(line);

            /* history command successfully extracted, erase the    */
            /* existing command from the screen, reset the escape   */
            /* flags and display the new command                    */

            erase_edit_line(line);
            line->index = 0;
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            redraw_edit_line(line);
            cursor_eol(line);
            return(0);


        /* fetch next history command */
        case CTRL_N:
        case DOWN_ARROW_KEY:

            /* no escape argument specified? */
            if (line->escape_arg < 0) {

                /* unsuccessful history command retrieval? */
                if (fetch_next_command(1,line->line,&(line->length)) < 0) {
                    line->escape_flag = NO_ESC_FUNC;
                    line->escape_arg = NO_ESC_ARG;
                    return(0);
                }
            }

            /* handle escape argument */
            else {

                /* unsuccessful history command retrieval? */
                if (fetch_next_command(line->escape_arg,
                    line->line,&(line->length)) < 0) {
                    line->escape_flag = NO_ESC_FUNC;
                    line->escape_arg = NO_ESC_ARG;
                    return(0);
                }
            }

            /* phx21233 R.L. - update echo flags to determine visibility */
            /*    of chars redrawn */
            update_echo_flags(line);

            /* history line successfully retrieved, erase existing  */
            /* input line from screen, initialize line flags and    */
            /* display the new input line                           */

            erase_edit_line(line);
            line->index = 0;
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            redraw_edit_line(line);
            cursor_eol(line);
            return(0);

        /* transpose the two characters preceeding the cursor */
        case CTRL_T:
            transpose_chars(line);

            /* reset escape argument and flag */
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* yank item from the kill buffer */
        case CTRL_Y:
            /* phx21233 R.L. - kill/yank disabled while not echoing */
            if (!kb.echo) {
                beep();
                return(0);
            }
            yank(line);
            line->escape_flag = NO_ESC_FUNC;
            line->escape_arg = NO_ESC_ARG;
            return(0);

        /* handle invalid editing key */
        default:
            /* handle any other control character by beeping */
            if (ch < MIN_PRINTABLE_ASCII) {
                line->escape_flag = NO_ESC_FUNC;
                line->escape_arg = NO_ESC_ARG;
                beep();
                return(0);
            }
            /* Could be text or ALT or FUNCTION key; indicate not an */
            /* editing key */
            return(-1);
    }
}



/*
**********************************************************************

  Routine:            OCTAL_VAL 

  Function:
      This routine converts the ASCII string containing an octal 
  representation to an integer value. 

  Parameters:
     (input)          str - pointer to the ASCII string containing 
                          the representation of an octal value 

  Returns:            the integer value after conversion 

  Note 1 - The string is assumed to contain only octal digits. 
**********************************************************************/

octal_val(str)
char *str;
{
    int sum;

    /* initialize sum */
    sum = 0;

    /* while not end of string */
    while (*str) {

        /* multiply by octal base */
        sum *= OCTAL_BASE_VALUE;

        /* add value of current octal digit */
        sum += *str - ASCII_ZERO_BASE;

        /* increment to next character */
        str++;
    }

    /* return the converted value */
    return(sum);
}


/*
**********************************************************************

  Routine:            UPDATE_ECHO_FLAGS

  Function:
     This function updates the echo status of all input characters
  buffered in the edit mode input line.  If keyboard echoing is enabled,
  all characters are flagged as visible otherwise they are flagged
  invisible.

  Parameters:
     (input)          line - pointer to the structure containing the
                          edit mode input line

  Returns:            NONE

**********************************************************************/

update_echo_flags(line)
EDIT_LINE *line;
{
    int i;
    int echo_flag;

    /* determine echo status */
    if (kb.echo)
        echo_flag = TRUE;
    else
        echo_flag = FALSE;

    /* update display lengths of each character in the input line */
    for (i = 0; i < line->length; i++)
        line->size[i] = echo_flag;
}
