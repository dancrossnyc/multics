/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-04-26,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
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

#include <stdio.h>
#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

/* status line should look like:
A  Edit Replace Audit(FILE,PRINTER) Paging No-Scroll [Background] [Foreground]
*/


/*
**********************************************************************

  Routine:            UPDATE_STATUS 

  Function:
      This routine updates the status line to reflect the current 
  mode settings. The current mode settings are obtained by examining 
  global flags. 

  Parameters:         NONE 

  Returns:            NONE 

  Note 1 - This routine should only be called while in the foreground 
      screen as the status line is used to display other information 
      while in other screens. 
**********************************************************************/

update_status()
{
    char *wsterm_mode_str;
    char *edit_mode_str;
    char *kb_mode_str;
    char audit_str[32];  /* enough to hold "Audit(FILE,PRINTER)" */
    char *page_str;
    char *scroll_str;
    char printbuff[SCREEN_COLS+1];

    /* determine WSTERM's mode */
    if (~mowse_active)           /* GLASS TTY MODE */
        wsterm_mode_str = "G";
    else if (sync)
        wsterm_mode_str = "S";   /* SYNC MODE */
    else
        wsterm_mode_str = "A";   /* ASYNC MODE */

    kb_mode_str = "";

    /* edit mode disabled in synchronous packet mode */
    if (mowse_active && sync)
        edit_mode_str = "";

    /* if edit mode is otherwise enabled */
    else if (wst_edit_mode) {
        edit_mode_str = "Edit";
        /* update kb ins/repl mode here as well */
        if (edlin.mode)
            kb_mode_str = "Insert";
        else
            kb_mode_str = "Replace";
    }

    /* edit mode is disabled */
    else
        edit_mode_str = "";

    /* determine audit mode modes */
    *audit_str = 0;
    if (wst_f_audit) {
        strcpy(audit_str,"Audit(FILE");
        if (wst_p_audit)
            strcat(audit_str,",PRINTER)");
        else
            strcat(audit_str,")");
    }
    else if (wst_p_audit) {
        strcpy(audit_str,"Audit(PRINTER)");
    }

    /* determine no-scroll and paging modes */
    scroll_str = wst_freeze ? "No-Scroll" : "";
    page_str = wst_paging ? "Page" : "";

    /* format to a string for displaying */
    sprintf(printbuff,
        "%1.1s %-4.4s %-7.7s %-19.19s %-6.6s %-9.9s",
            wsterm_mode_str,
            edit_mode_str,
            kb_mode_str,
            audit_str,
            page_str,
            scroll_str);

    /* display the string on the status line */
    status_line(printbuff);

    /* check for foreground message signal */
    if (fg_msg_showing)
        signal_fg(ON);

    /* check for background message signal */
    if (bk_msg_showing)
        signal_bg(ON);
}



/*
**********************************************************************

  Routine:            STATUS_LINE 

  Function:
      This routine displays a string on the status line. The cursor 
  position is preserved. 

  Parameters:
     (input)          str - pointer to the string to display 

  Returns:            NONE 

**********************************************************************/

status_line(str)
char *str;
{
    union REGS reg, outreg;
    int row, col;
    int i, j;
    int attribute;

    /* save current cursor position */
    cursor_pos(&row,&col);

    /* get default foreground screen attributes for status line */
    attribute = wst_fg_screen.attr;

    reg.h.ah = WRT_SCREEN;   /* select function to write a character */
    reg.h.bh = get_active_page();   /* write to current active page */
    reg.x.cx = 1;            /* write 1 character at a time */

    /* invert the foreground and background attributes for reverse video */
    reg.h.bl = ((attribute << N_FG_ATTR_BITS) |
        (attribute >> N_BG_ATTR_BITS)) & LOW_7_BITS;

    /* display until last screen column or end of string */
    for (i = 0; i <= MAX_SCREEN_COL && *str; i++, str++) {
        cursor_move(SCREEN_LINES,i);     /* position cursor */
        reg.h.al = *str;                  /* get character to write */
        int86(BIOS_VIDEO,&reg,&outreg);          /* write the character out */
    }

    /* blank pad the rest of the line */
    reg.h.al = ' ';
    for (j = i; j <= MAX_SCREEN_COL; j++) {
        cursor_move(SCREEN_LINES,j);     /* position cursor */
        int86(BIOS_VIDEO,&reg,&outreg);        /* write the blank */
    }

    /* restore original cursor position */
    cursor_move(row,col);
}



/*
**********************************************************************

  Routine:            SIGNAL_BG 

  Function:
      This routine turns on or off the background message signal on 
  the status line. 

  Parameters:
     (input)          sw - if this value is TRUE, the background 
                          signal is turned on; if FALSE, the signal 
                          is turned OFF 

  Returns:            NONE 

**********************************************************************/

signal_bg(sw)
int sw;
{
    int row, col;
    union REGS reg, outreg;
    int i;
    char *str;
    int attribute;

    /* save original position of cursor */
    cursor_pos(&row,&col);

    /* get default background screen attributes */
    attribute = wst_bg_screen.attr;

    reg.h.ah = WRT_SCREEN;   /* select function to write a character */
    reg.h.bh = get_active_page();  /* write to current active page */
    reg.x.cx = 1;            /* write only one character */

    /* invert the foreground and background attributes for reverse video */
    reg.h.bl = ((attribute << N_FG_ATTR_BITS) |
        (attribute >> N_BG_ATTR_BITS)) & LOW_7_BITS;

    /* if turning signal ON */
    if (sw) {

        /* set hi bit for blinking */
        reg.h.bl |= BLINK_ATTRIBUTE;

        /* initialize the message string pointer */
        str = "[Background]";

    }

    /* else turning the signal OFF, overwrite with blanks */
    else
        str = "            ";

    /* write the string out at the status line background message position; */
    /* starting coordinates on screen for background message is 24,53 */
    for (i = 53; *str; i++, str++) {
        cursor_move(SCREEN_LINES,i);
        reg.h.al = *str;
        int86(BIOS_VIDEO,&reg,&outreg);
    }

    /* restore the original cursor position */
    cursor_move(row,col);
}



/*
**********************************************************************

  Routine:            SIGNAL_FG 

  Function:
      This routine turns the foreground message signal on the status 
  line on or off. 

  Parameters:
     (input)          sw - if this value is TRUE, the foreground 
                          signal is turned ON, otherwise the signal 
                          is turned OFF. 

  Returns:            NONE 

**********************************************************************/

signal_fg(sw)
int sw;
{
    int row, col;
    union REGS reg, outreg;
    int i;
    char *str;
    int attribute;

    /* save the original cursor position */
    cursor_pos(&row,&col);

    /* get default foreground screen attributes */
    attribute = wst_fg_screen.attr;

    reg.h.ah = WRT_SCREEN;   /* select function to write a character */
    reg.h.bh = get_active_page();  /* write to current active page */
    reg.x.cx = 1;            /* write only one character */

    /* invert the foreground and background attributes for reverse video */
    reg.h.bl = ((attribute << N_FG_ATTR_BITS) |
        (attribute >> N_BG_ATTR_BITS)) & LOW_7_BITS;

    /* if turning signal ON */
    if (sw) {

        /* set hi bit for blinking */
        reg.h.bl |= BLINK_ATTRIBUTE;

        /* initialize the message string pointer */
        str = "[Foreground]";
    }

    /* else turning the signal OFF, overwrite with blanks */
    else
        str = "            ";

    /* write the string out at the status line foreground message position */
    /* starting coordinates on screen for foreground message is 24,66 */
    for (i = 66; *str; i++, str++) {
        cursor_move(SCREEN_LINES,i);
        reg.h.al = *str;
        int86(BIOS_VIDEO,&reg,&outreg);
    }

    /* restore the original cursor position */
    cursor_move(row,col);
}
