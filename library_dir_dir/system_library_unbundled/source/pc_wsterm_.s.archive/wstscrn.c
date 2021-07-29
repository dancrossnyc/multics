/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-04-29,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
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
  4) change(89-01-30,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Separated input and output arguments to int86() to avoid confusion
     when parameters are used.
                                                   END HISTORY COMMENTS */

/* WSTSCRN - This module contains routines for manipulating WSTERM's
   various screens. The copy of what should be contained on the screen
   is maintained in screen buffers which may be "swapped" to the screen.
*/

#include <dos.h>
#include <ctype.h>
#include "wstdefs.h"
#include "wstglob.h"

/*
**********************************************************************

  Routine:            WST_SCREEN_CLEAR 

  Function:
      This routine blank fills the contents of a specified screen 
  buffer and initializes its cursor coordinates to the home position. 

  Parameters:
     (output)         scr - pointer to the screen buffer to clear 

  Returns:            NONE 

**********************************************************************/

wst_screen_clear(scr)
SCREEN *scr;    /* pointer to screen buffer */
{
    int i;

    /* home cursor position */
    scr->cursor_row = CURSOR_HOME_ROW;
    scr->cursor_col = CURSOR_HOME_COL;

    /* clear the screen by writing blanks to entire screen buffer */
    for (i = 0; i < SCREEN_BUFFER_BYTES;) {
        scr->screen[i++] = ' ';
        scr->screen[i++] = scr->attr;
    }
}



/*
**********************************************************************

  Routine:            WST_SCREEN_PRINTLINE 

  Function:
      This routine writes a line of text to the screen buffer at the 
  screen buffer's current row coordinate. The screen buffer's row 
  coordinate variable is then incremented. 

  Parameters:
     (output)         scr - pointer to the screen buffer to write the 
                          string to 
     (input)          str - the character string to write 

  Returns:            NONE 

**********************************************************************/

wst_screen_printline(scr,str)
SCREEN *scr;    /* pointer to screen buffer */
char *str;      /* string to write to screen buffer */
{
    register int col;
    register int offset;

    /* write the string at the cursor line starting at column 0 */
    col = 0;

    /* index into appropriate location into screen buffer */
    /* (multiply by 2 to account for character and attribute bytes) */
    offset = scr->cursor_row * SCREEN_COLS * 2;

    /* put each character to the screen buffer until end of string */
    /* or until end of line */
    for (col = 0; *str && col < SCREEN_COLS; col++) {
        scr->screen[offset++] = *str++;
        offset++;
    }

    /* increment the cursor line */
    scr->cursor_row++;
}



/*
**********************************************************************

  Routine:            PUT_WST_SCREEN 

  Function:
      This routine displays a character, updating the position of the 
  physical cursor in the cursor position variables of the specified 
  screen buffer. Boundary checks are made to ensure that the cursor 
  stays within the defined screen boundary. 

  Parameters:
     (output)         scr - specifies the screen buffer whose cursor 
                          coordinate variables are to be updated 
     (input)          ch - specifies the character to display 

  Returns:            NONE 

**********************************************************************/

put_wst_screen(scr,ch)
SCREEN *scr;   /* pointer to screen buffer */
int ch;        /* character to */
{

    /* save current cursor position */
    cursor_pos(&scr->cursor_row,&scr->cursor_col);

    /* overwriting status line? */
    if (scr->cursor_row > MAX_SCREEN_LINE) {

        /* scroll the display */
        wst_scroll();

        /* "scroll" cursor as well */
        scr->cursor_row = MAX_SCREEN_LINE;
        cursor_move(scr->cursor_row,scr->cursor_col);
    }

    /* display the character */
    putch(ch);

    /* get cursor position */
    cursor_pos(&scr->cursor_row,&scr->cursor_col);

    /* on status line? */
    if (scr->cursor_row > MAX_SCREEN_LINE) {

        /* scroll display up */
        wst_scroll();

        /* "scroll" cursor up */
        scr->cursor_row = MAX_SCREEN_LINE;
        cursor_move(scr->cursor_row,scr->cursor_col);
    }    
}



/*
**********************************************************************

  Routine:            WST_SCREENS_INIT 

  Function:
      This routine initializes wsterm's screen buffers (foreground, 
  background and temporary) when WSTERM starts up. 

  Parameters:
     (input)          ch - specifies the character to initialize the 
                          contents of the screen buffer with 
     (input)          attr - specifies the attribute to initialize 
                          the contents of the screen buffer with 

  Returns:            NONE 

  Note 1 - Only the attribute and cursor position fields are 
      initialized for the temporary screen buffer as the screen 
      contents should always be initialized before it is used. 
**********************************************************************/

wst_screens_init(ch,attr)
int ch;   /* character to fill the screens with on initialization */
int attr; /* attribute to fill the screens with on initialization */
{
    int i;

    /* set the attribute to be used for a particular screen */
    wst_fg_screen.attr = attr;
    wst_bg_screen.attr = attr;
    wst_tmp_screen.attr = attr;
    wst_help_screen.attr = attr;

    /* home the cursor positions */
    wst_fg_screen.cursor_row = CURSOR_HOME_ROW;
    wst_fg_screen.cursor_col = CURSOR_HOME_COL;

    wst_bg_screen.cursor_row = CURSOR_HOME_ROW;
    wst_bg_screen.cursor_col = CURSOR_HOME_COL;

    wst_tmp_screen.cursor_row = CURSOR_HOME_ROW;
    wst_tmp_screen.cursor_col = CURSOR_HOME_COL;

    wst_help_screen.cursor_row = CURSOR_HOME_ROW;
    wst_help_screen.cursor_col = CURSOR_HOME_COL;

    /* initialize the contents of the screens; */
    /* temporary and help screen is not initialized, it should be */
    /* explicitly initialized each time it is used */
    for (i = 0; i < SCREEN_BUFFER_BYTES;) {
        wst_fg_screen.screen[i] = ch;
        wst_bg_screen.screen[i++] = ch;
        wst_fg_screen.screen[i] = attr;
        wst_bg_screen.screen[i++] = attr;
    }
}



/*
**********************************************************************

  Routine:            SAVE_WST_SCREEN 

  Function:
      This routine saves the contents of the screen and the position 
  of the cursor to a screen buffer structure. 

  Parameters:
     (output)         scr - pointer to the screen buffer structure to 
                          save the screen to 

  Returns:            NONE 

  Note 1 - Only the first 24 lines are saved as the 25th line is the 
      status line 
**********************************************************************/

save_wst_screen(scr)
SCREEN *scr;   /* pointer to the screen buffer */
{
    cursor_pos(&scr->cursor_row,&scr->cursor_col);
    save_screen(scr->screen);
}



/*
**********************************************************************

  Routine:            RESTORE_WST_SCREEN 

  Function:
      This routine restores the contents of the screen and cursor 
  position which was previously saved in a screen buffer structure. 

  Parameters:
     (input)          scr - specifies the screen buffer structure to 
                          restore the screen from 

  Returns:            NONE 

  Note 1 - Only the first 24 lines of the screen are restored as the 
      25th line is the status line 
**********************************************************************/

restore_wst_screen(scr)
SCREEN *scr;
{
    restore_screen(scr->screen);
    cursor_move(scr->cursor_row,scr->cursor_col);
}



/*
**********************************************************************

  Routine:            CURSOR_MOVE 

  Function:
      This routine moves the physical cursor on the screen to a 
  specified screen coordinate. 

  Parameters:
     (input)          row - specifies the row to move to (0 = top of 
                          screen) 
     (input)          col - specifies the column to move to (0 = left 
                          most column) 

  Returns:            NONE 

**********************************************************************/

cursor_move(row,col)
int row;
int col;
{
    union REGS reg, outreg;

    reg.h.ah = VIDEO_CURSOR_MOVE_FUNC;   /* VIDEO cursor set */
    reg.h.dh = row;                /* coordinates to move to */
    reg.h.dl = col;
    reg.h.bh = get_active_page();  /* in current active page */
    int86(BIOS_VIDEO,&reg,&outreg);   /* call BIOS routine to do actual move */
}


/*
**********************************************************************

  Routine:            CURSOR_POS 

  Function:
      This routine reads the coordinates of the physical cursor on 
  the screen. 

  Parameters:
     (output)         row - pointer to the integer for passing back 
                          the row coordinate of the cursor 
     (output)         col - pointer to the integer for passing back 
                          the column coordinate of the cursor 

  Returns:            NONE 

**********************************************************************/

cursor_pos(row,col)
int *row;
int *col;
{
    union REGS reg, outreg;

    reg.h.ah = VIDEO_CURSOR_READ_FUNC;   /* VIDEO cursor read */
    reg.h.bh = get_active_page();   /* from current active page */
    int86(BIOS_VIDEO,&reg,&outreg);    /* call BIOS to get coordinates */
    *row = outreg.h.dh;
    *col = outreg.h.dl;
}



/*
**********************************************************************

  Routine:            WAIT_FOR_KEY 

  Function:
      This routine will get a key, waiting blocked if necessary. 

  Parameters:         NONE 

  Returns:            an ASCII code if an ASCII keyboard character 
                          was pressed 
                      an encoded value greater than 255 if a function 
                          or ALT key was pressed 

**********************************************************************/

wait_for_key()
{
    union REGS reg, outreg;
    int code;

    /* break key before this routine got called? */
    if (break_flag) {
        break_flag = FALSE;
        return(BREAK_KEY);
    }

    /* read the keyboard */
    reg.h.ah = BIOS_KB_READ;
    int86(BIOS_KB,&reg,&outreg);

    /* break key hit ? */
    if (break_flag) {
        break_flag = FALSE;
        return(BREAK_KEY);
    }

    /* non zero indicates regular ASCII character */
    if (outreg.h.al)
        return((int)outreg.h.al);

    /* otherwise extended ASCII code, generate unique extension code */
    code = ASCII_EXTEND_CODE + outreg.h.ah;
    return(code);
}




/*
**********************************************************************

  Routine:            CHECKKEY 

  Function:
      This routine checks to see if the a keyboard key has already 
  been pressed. 

  Parameters:         NONE 

  Returns:            an ASCII code if an ASCII keyboard character 
                          was pressed 
                      an encoded value greater than 255 if a function 
                          or ALT key was pressed 
                      -1 if no key was pressed 

**********************************************************************/

checkkey()
{
    union REGS reg, outreg;
    int cpu_flags;

    if (break_flag)    /* break key was hit */
        return(BREAK_KEY);

    reg.h.ah = BIOS_KB_STATUS;   /* call BIOS routine to see if key was hit */
    cpu_flags = int86(BIOS_KB,&reg,&outreg);

    /* check if key was hit */
    if (cpu_flags & Z_FLAG_MASK)
        return(-1);

    /* regular ASCII character? */
    if (outreg.h.al)
        return((int)outreg.h.al);

    /* encode as an extended ASCII value */
    return (ASCII_EXTEND_CODE + outreg.h.ah);
}



/*
**********************************************************************

  Routine:            ENABLE_CTL_C_CHECK 

  Function:
      This routine turns on CTRL-C checking and saves the original 
  CTRL-C checking status (either enabled or disabled) in the global 
  variable 'orig_break_status'. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

enable_ctl_c_check()
{
    union REGS reg, outreg;

    reg.h.ah = DOS_CTRL_C_FUNCTION;    /* function to get/set ctrl-c checking */

    /* exchange current CTRL-C checking state (enabled or disabled)
       with ENABLED state
    */
    reg.h.al = DOS_CTRL_C_EXCHANGE;    /* exchange states */
    reg.h.dl = DOS_CTRL_C_ENABLE;      /* checking enabled after exchange */

    int86(DOS_INT,&reg,&outreg);   /* call DOS interrupt to set ctrl-c checking */
    orig_break_status = outreg.h.dl;  /* save original ctrl-c checking status */
}



/*
**********************************************************************

  Routine:            RESTORE_CTL_C_CHECK 

  Function:
      This routine restores CTRL-C checking to the state (enabled or 
  disabled) previously saved in the global variable 
  'orig_break_status'. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

restore_ctl_c_check()
{
    union REGS reg, outreg;

    /* if originally ON, then no need to turn OFF ctl-c checking */
    if (orig_break_status) return;

    /* turn off ctl-c checking */
    reg.h.ah = DOS_CTRL_C_FUNCTION; /* function to get/set ctrl-c checking */
    reg.h.al = DOS_CTRL_C_SET;      /* switch to set ctrl-c checking */
    reg.h.dl = DOS_CTRL_C_DISABLE;  /* indicate ctrl-c checking turned off */
    int86(DOS_INT,&reg,&outreg);    /* call BIOS to turn ctrl-c checking off */
}



/*
**********************************************************************

  Routine:            PUT_TO_SCREEN 

  Function:
      This routine displays a character to the screen with boundary 
  checking. If the character goes beyond the defined screen size, the 
  cursor is forced back inside the screen area. In particular, this 
  ensures that no character, be it part of a long string or escape 
  sequence, will be placed outside the defined screen area. 

  Parameters:
     (input)          ch - specifies the character to display; the 
                          character to passed to the screen driver 

  Returns:            NONE 

**********************************************************************/

put_to_screen(ch)
int ch;
{
    union REGS reg, outreg;

    /* save current cursor coordinates; assume in foreground */
    cursor_pos(&wst_fg_screen.cursor_row,&wst_fg_screen.cursor_col);

    /* cursor already out of bounds? */
    if (wst_fg_screen.cursor_row > MAX_SCREEN_LINE) {

        /* "scroll" cursor up */
        wst_fg_screen.cursor_row = MAX_SCREEN_LINE;
        cursor_move(wst_fg_screen.cursor_row,wst_fg_screen.cursor_col);
    }    

    /* display the character */
    reg.h.al = ch;
    int86(0x29,&reg,&outreg);

    /* check again */
    cursor_pos(&wst_fg_screen.cursor_row,&wst_fg_screen.cursor_col);

    if (wst_fg_screen.cursor_row > MAX_SCREEN_LINE) {

        /* "scroll" cursor up */
        wst_fg_screen.cursor_row = MAX_SCREEN_LINE;
        cursor_move(wst_fg_screen.cursor_row,wst_fg_screen.cursor_col);
    }    
}



/*
**********************************************************************

  Routine:            WST_GETLINE 

  Function:
      This routine will get a line of input from the keyboard. The 
  line is terminated when the user hits <CARRIAGE RETURN>. This 
  routine is used instead of gets() to prevent ^C from breaking out 
  of the program. 

  Parameters:
     (output)         scr - pointer to the screen buffer to use for 
                          saving cursor position information; while 
                          in background screen, background screen 
                          buffer is used; if in foreground screen, 
                          foreground screen buffer should be used 
     (output)         str - specifies the string for passing back the 
                          line entered 

  Returns:            NONE 

**********************************************************************/

wst_getline(scr,str,stat_line)
SCREEN *scr;
char *str;   /* string to pass back the value entered */
char *stat_line;  /* status line message being displayed */
{
    int ch;
    int i;   /* index to where data is to be placed in string */

    i = 0;

    /* loop until <RETURN> hit */
    while ((ch = getkey(BLOCK)) != RETURN_KEY) {

        /* check BACKSPACE key */
        if (ch == BACKSPACE) {

            /* only if something exists in the string */
            if (i > ZERO_INDEX_VALUE) {
                /* decrement index */
                i--;

                /* rubout the character displayed */
                put_wst_screen(scr,BACKSPACE);
                put_wst_screen(scr,' ');
                put_wst_screen(scr,BACKSPACE);
            }
        }

        /* invoke background help */
        else if (ch == ALT_H) {
            help(BG_HELP);
            status_line(stat_line);
        }

        /* if is a printable character and line length of 127 not exceeded */
        else if (i < 127 && ch < ASCII_DEL && isprint(ch)) {

            /* add character to string */
            str[i] = ch;
            i++;

            /* display the character */
            put_wst_screen(scr,ch);
        }

        else
            beep();
    }

    /* echo the linefeed at end of line */
    put_wst_screen(scr,'\n');

    /* terminate the string */
    str[i] = NUL_TERMINATOR;
}



/*
**********************************************************************

  Routine:            INIT_WST_SCROLL

  Function:
      This function initializes a global structure "scroll_regs"
  which is used for scrolling the working area (first 24 lines)
  of the screen.

  Parameters:         NONE

  Returns:            NONE

**********************************************************************/

init_wst_scroll(attr)
int attr;
{
    /* scroll display up */
    scroll_regs.h.ah = VIDEO_SCROLL_UP_FUNC;
    scroll_regs.h.al = DEFAULT_LINES_TO_SCROLL;
    scroll_regs.h.ch = CURSOR_HOME_ROW;
    scroll_regs.h.cl = CURSOR_HOME_COL;
    scroll_regs.h.dh = SCREEN_LINES-1;
    scroll_regs.h.dl = SCREEN_COLS-1;
    scroll_regs.h.bh = attr;
}



/*
**********************************************************************

  Routine:            WST_SCROLL

  Function:
      This function scrolls the first 24 lines of the screen up a
  row, filling the 24th row with blanks.

  Parameters:         NONE

  Returns:            NONE

**********************************************************************/

wst_scroll()
{
    union REGS out;

    /* scroll display up */
    int86(BIOS_VIDEO,&scroll_regs,&out);
}



/*
**********************************************************************

  Routine:            PUT_SCREEN_STR 

  Function:
      This routine write a string directly to the screen driver. 

  Parameters:
     (input)          str - pointer to the string to write 

  Returns:            NONE 

**********************************************************************/

put_screen_str(str)
char *str;
{
    while (*str) putch(*str++);
}



/*
**********************************************************************

  Routine:            UPPERCASE

  Function:
      This routine returns the uppercased ASCII value of a character
  if that character is a lowercased letter. This function is used
  instead of the normal ctype.h toupper() because the character may
  be an extended ASCII value, i.e. the value of the character is not
  restricted to be in the range of 0 to 127.

  Parameters:
     (input)          ch - the character to convert to uppercase

  Returns:            the value of 'ch' if 'ch' is not a lowercase
                          letter;
                      otherwise the uppercase value of 'ch'

**********************************************************************/

uppercase(ch)
int ch;
{
    if (ch < 'a' || ch > 'z') return(ch);
    return(ch - ('a' - 'A'));
}



/*
**********************************************************************

  Routine:            WST_RESET

  Function:
      This function handles the reset escape sequence "ESC c" which
  causes the colors, scrolling region and other screen attributes to
  be reset. This routine restores the attributes needed by WSTERM
  after the reset and updates variables which are affected by the
  reset.

  Parameters:         NONE

  Returns:            NONE 

**********************************************************************/

wst_reset()
{
    int attr;

    /* update attributes of screen */
    getattr(&attr);
    wst_fg_screen.attr = attr;
    wst_bg_screen.attr = attr;
    wst_tmp_screen.attr = attr;
    wst_help_screen.attr = attr;
    scroll_regs.h.bh = attr;

    /* restore status line and WSTERM version */
    update_status();
    clear_screen ();
    cursor_move (CURSOR_HOME_ROW, 60);
    putscr("WSTERM Vers ",12);
    putscr(version,strlen(version));
    cursor_move (CURSOR_HOME_ROW,CURSOR_HOME_COL);        

    /* tell edit mode that screen has been cleared/reset */
    if (edlin.length > 0)
        erase_edit_line(&edlin);
    edlin.orig_row = edlin.cur_row = edlin.max_row = CURSOR_HOME_ROW;
    edlin.orig_col = edlin.cur_col = edlin.cur_row = CURSOR_HOME_COL;

}

