/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1987 *
   *                                          *
   ******************************************** */


/* HISTORY COMMENTS:
  1) change(87-05-04,Wallman), approve(87-05-04,MCR7586),
     audit(87-08-10,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(87-08-14,Wallman), approve(87-08-14,PBF7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     PBF increasing length of msg_line to 256 to fix stringsize condition.
  3) change(87-09-02,Wallman), approve(87-09-02,PBF7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     PBF to improve robustness of string handling by avoiding all str*
     functions.
  4) change(87-09-17,Wallman), approve(87-09-17,PBF7586),
     audit(87-09-17,LJAdams), install(87-09-18,MR12.1-1109):
     Added support for Cursor Horizontal Absolute ANSI control sequence.
  5) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables,
     re-formatting.
  6) change(88-04-11,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added routine to flush keyboard input after processing BREAK
     function. Replaces a "while read_keyboard()" statement which
     did not work properly.
  7) change(88-04-20,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added support for help screen in local_esc().
  8) change(88-04-22,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added support for status line; output to screen is filtered to
     trap some invalid escape sequences and to trap sequences which
     corrupts the status line.
  9) change(88-05-18,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Deleted hide_sw reference to function replay().
 10) change(88-05-26,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Handling of ALT-S, ALT-O, ALT-F and ALT-P.
 11) change(88-07-20,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Modified to make edit mode default after atm.
 12) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
 13) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
 14) change(88-08-30,Lee), approve(88-09-12,MCR7986), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed non-edit async mode and references to non-edit async mode
     line editing routines.
 15) change(89-01-30,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Updated kb.echo when toggling between edit and non-edit mode
     while MOWSE is not attached. Updated end of page handling to allow
     variable page size.
                                                   END HISTORY COMMENTS */

/* WSTUTIL - WSTERM utility routines */

/* This module holds functions used by more than one WSTERM module */

#include  <stdio.h>
#include  <dos.h>
#include  <ws.h>
#include  <cat.h>
#include  <wsmincap.h>
#include  "wstdefs.h"
#include  "wsttype.h"
#include  "wstglob.h"

/*
**********************************************************************

  Routine:            BYTESHIFT 

  Function:
      When loading integer values, different machines will load 
  either the high or the low byte first. Multics always wants the 
  data loaded in the low byte. This routine will shift the bits if 
  needed regardless of machine type. 

  Parameters:
     (input)          number - the integer value to load 
     (output)         high - specifies the contents of the high byte 
     (output)         low - specifies the contents of the low byte 

  Returns:            NONE 

**********************************************************************/

byteshift (number, high, low)
int   number;
byte  *high,
      *low;

{ 
    if (number < MAX_8_BIT_VALUE) { 
        *high = 0;
        *low  = number;
    }
    else { 
        *high = number / MAX_8_BIT_VALUE;
        *low  = number % MAX_8_BIT_VALUE;
    }
}                                       /* End of byteshift */



/*
**********************************************************************

  Routine:            CATSTR 

  Function:
      This routine catenates strings with stringsize checking. 

  Parameters:
     (input/output)   target - target string 
     (input)          add_on - add-on string 
     (input)          target_name - target string name 
     (input)          add_on_size - character count of 'add_on' 
     (input)          target_size - target string size 

  Returns:            NONE 

  Note 1 - This routine will abort WSTERM with an error message if a 
      string size overflow is encountered!!! 
**********************************************************************/

catstr (target, add_on, add_on_size, target_name, target_size)
char    *target,            /* Target string */
        *add_on,            /* Add-on string */
        target_name [];     /* Target string name */
int     add_on_size,        /* Character count of 'add_on' */
        target_size;        /* Target string size */
{
    register int old_length;

    old_length = strlen(target);

    if (old_length + add_on_size > target_size - 1) { 

        put_screen_str ("Program error: A string size overflow has been detected in ");
        put_screen_str (target_name);

        exit (1);
    }

    movmem (add_on, &target [strlen (target)], add_on_size);
    target [ old_length + add_on_size] = NUL_TERMINATOR;
    return;
}                                       /* End of catstr */



/*
**********************************************************************

  Routine:            CLEAR_SCREEN 

  Function:
      This routine clears the data area of the screen (the first 24 
  lines) and initializes the global screen information variables 
  accordingly. The status line area (25th line) is left untouched. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

clear_screen ()

{ 
    clear_window();

    screen.curcol = CURSOR_HOME_COL;
    screen.curlin = CURSOR_HOME_ROW;
    screen.EOP_ct = 0;
    ds.ccol = 0;
    ds.lct = 0;
    ds.splct [0] = NUL;
}                                       /* End of clear_screen */



/*
**********************************************************************

  Routine:            EMIT_MSG 

  Function:
      This routine displays a host or local message on the screen. 
  The message may contain ANSI escape sequences. It calls the routine 
  EMIT_MSG_2 to do the real work since the message is parsed and 
  accumulated while checking for ANSI escape sequences so it must not 
  let the message become too large for EMIT_MSG_2 to handle. 

  Parameters:
     (input)          msg - specifies the buffer containing the 
                          message to display 
     (input)          msgl - specifies the number of characters from 
                          the message buffer to display 
     (input)          pstr_sw - switch to specify if the message is a 
                          prompt string 

  Returns:            NONE 

**********************************************************************/

emit_msg(msg,msgl,pstr_sw)
char msg[];
int msgl, pstr_sw;
{
    char *msg_ptr;

    msg_ptr = msg;

    /* display message in chunks up to 20 bytes in size */
    while (msgl > 20) {
        emit_msg_2(msg_ptr, 20, pstr_sw);
        msg_ptr += 20;
        msgl -= 20;
    }
    emit_msg_2(msg_ptr,msgl,pstr_sw);

    /* clear prompt string after displaying entire prompt string */
    if (screen.curcol < 1 || pstr_sw) { 
        ds.pstrl = 0;
        setmem (ds.pstr, sizeof (ds.pstr), NUL);
        kb.pos [0] = NUL;
    }
}



/*
**********************************************************************

  Routine:            EMIT_MSG_2

  Function:
      This routine displays a host or local message on the screen. 
  The message may contain ANSI escape sequences. 

  Parameters:
     (input)          msg - specifies the buffer containing the 
                          message to display 
     (input)          msgl - specifies the number of characters from 
                          the message buffer to display 
     (input)          pstr_sw - switch to specify if the message is a 
                          prompt string 

  Returns:            NONE 

**********************************************************************/

emit_msg_2 (msg, msgl, pstr_sw)
char    msg [];             /* The message */
int     msgl,               /* Message length */
        pstr_sw;            /* Prompt string */

{         
    int curc, curl,         /* For tracking cursor position */
        ipstr,              /* klin index for start of pstr */
        newc, newl,         /* For tracking cursor position */
        tab_width;          /* SPs needed for tab expansion */
    register int    i;               /* Working index */
    char    msg_line [256],     /* For message display */
            tab_space [12];

    /* We must parse display message for ANSI control sequences in order that
       their side effects are handled properly.

       Presumption:
       Display messages contain ONLY alphamerics & ANSI control sequences */

    ipstr = 0;
    setmem (msg_line, sizeof (msg_line), NUL);

    curc = screen.curcol;
    curl = screen.curlin;

    /* Scan the message */

    for (i = 0; i < msgl; i++) {           /* Begin message parse loop */

        if (checkkey() == ALT_B)
            return;

        switch (msg [i]) {
            case 'A':                         /* CUU - cursor up */
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) { 
                    curl -= max (newl, 1);
                    ds.do_ctl = OFF;
                }
                else
                    curc++;
                break;

            case 'B':                         /* CUD - cursor down */
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) { 
                    curl += max (newl, 1);
                    ds.do_ctl = OFF;
                }
                else
                    curc++;
                break;

            case BEL:                         /* ASCII BEL */
                catstr (msg_line, &msg [i], 1, "msg_line",
                              sizeof (msg_line));
                break;

            case BSP:                         /* ASCII BS */
                if (curc > 0) { 
                    catstr (msg_line, &msg [i], 1,
                         "msg_line", sizeof (msg_line));
                    curc = max (curc - 1, 0);
                }
                break;

            case 'C':                         /* CUF - cursor forward */
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) { 
                    curl += max (newl, 1);
                    ds.do_ctl = OFF;
                }
                else
                    curc++;
                break;

            case CR:                          /* ASCII CR */
                if (strlen (msg_line) > 0) { 
                    catstr (msg_line, &msg [i], 1,
                         "msg_line", sizeof (msg_line));

                    putscr (msg_line, strlen (msg_line));
                    setmem (msg_line, sizeof (msg_line), NUL);
                }

                cursor_move (curl, 0);  /* Go to left margin */
                curc = 0;

                ds.pstrl = 0;
                setmem (ds.pstr, sizeof (ds.pstr), NUL);
                ipstr = i + 1;                /* Next char starts a prompt string */
                break;

            case 'D':                         /* CUB - cursor backward */
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) { 
                    curc -= max (newl, 1);
                    ds.do_ctl = OFF;
                }
                else
                    curc++;
                break;

            case ESC:                         /* Starting display control */
                if (strlen (msg_line) > 0) {

                    putscr (msg_line, strlen (msg_line));
                    setmem (msg_line, sizeof (msg_line), NUL);
                }

                catstr (msg_line, &msg [i], 1, "msg_line-2",
                              sizeof (msg_line));
                newc = 0;                       /* Reset accumulators */
                newl = 0;
                ds.do_ctl = ON;                 /* Now parsing a display control */
                break;

            case 'G':
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) {        /* ANSI CHA - record value & restart */
                    curc = max (newc - 1, 0);
                    kb.pos [0] = curc;
                    ds.do_col = OFF;
                    ds.do_ctl = OFF;
                    ds.pstrl = 0;
                    setmem (ds.pstr, sizeof (ds.pstr), NUL);
                    ipstr = i + 1;        /* Next char starts a prompt string */

                }
                else
                    curc++;
                break;

            case 'H':
                catstr (msg_line, &msg [i], 1, "msg_line-3",
                              sizeof (msg_line));

                if (ds.do_ctl) {         /* ANSI CUP - record values & restart */
                    curl = max (newl - 1, 0);
                    curc = max (newc - 1, 0);
                    kb.pos [0] = curc;
                    ds.do_col = OFF;
                    ds.do_ctl = OFF;
                    ds.pstrl = 0;
                    setmem (ds.pstr, sizeof (ds.pstr), NUL);
                    ipstr = i + 1;         /* Next char starts a prompt string */

                }
                else
                    curc++;
                break;

            case HT:                          /* ASCII HT */
                tab_width = next_tab (curc);
                curc += tab_width;
                catstr (msg_line, strncpy (tab_space,
                    spaces, tab_width), tab_width, "msg_line-4", sizeof (msg_line));
                break;

            case 'J':
                catstr (msg_line, &msg [i], 1, "msg_line-5",
                              sizeof (msg_line));

                if (ds.do_ctl) {                 /* ANSI ED - start over */
                    curl = 0;
                    curc = 0;
                    kb.pos [0] = 0;
                    ds.do_col = OFF;
                    ds.do_ctl = OFF;
                    bk_msg_showing = OFF;
                    fg_msg_showing = OFF;
                }
                else
                    curc++;
                break;

            case NL:                          /* ASCII LF */
                if (strlen (msg_line) > 0) {

                    putscr (msg_line, strlen (msg_line));
                    setmem (msg_line, sizeof (msg_line), NUL);
                }

                scroll ();
                curl = screen.curlin;

                if (local_action == 'b')
                    return;

                curc = 0;
                ds.pstrl = 0;
                setmem (ds.pstr, sizeof (ds.pstr), NUL);
                ipstr = i + 1;
                break;

            case ';':
                catstr (msg_line, &msg [i], 1, "msg_line-6",
                      sizeof (msg_line));

                if (ds.do_ctl)                  /* ANSI field separator */
                    ds.do_col = ON;
                else
                    curc++;
                break;

            case '[':
                catstr (msg_line, &msg [i], 1, "msg_line-7",
                              sizeof (msg_line));

                if (ds.do_ctl)
                    ;                 /* ANSI field opener */
                else
                    curc++;
                break;

            default:                          /* Anything else */

                /**** NOTE: Cases for char/line insert/delete may be needed */

                if (ds.do_ctl) { 
                    catstr (msg_line, &msg [i], 1,
                         "msg_line-8", sizeof (msg_line));

                    if (isdigit (msg [i])) { 
                        if (ds.do_col)
                            newc = 10 * newc + (int) msg [i] -
                                060;
                        else 
                            newl = 10 * newl + (int) msg [i] -
                                060;
                    }
                    else { /* Nothing else matters */
                        ds.do_col = OFF;
                        ds.do_ctl = OFF;
                    }
                }
                else if (isprint (msg [i])) { 
                    catstr (msg_line, &msg [i], 1,
                         "msg_line-9",  sizeof (msg_line));
                    curc++;
                }
                break;
        }                                   /* End of switch cases */

        /* Do the EOL processing */

        if (screen.maxcol > 0 && curc == screen.maxcol) {   /* phx21233 RL - skip if line length 0 */
            /* In packet mode; just wrap the line */

            if (mowse_active) { 
                if (~sync) {

                putscr (msg_line,
                         strlen (msg_line));
                    setmem (msg_line, sizeof (msg_line),
                         NUL);

                    scroll ();
                    curl = screen.curlin;

                    if (local_action == 'b')
                        return;

                    setmem (msg_line, sizeof (msg_line),
                         NUL);
                    catstr (msg_line,
                         "\\c", 2, "wrap flag", 
                        sizeof (msg_line));
                    curc = 2;
                    ipstr = i;
                }                             /* End of ~sync code */
            }                               /* End of mowse_active code */ 


            /* This is used so that the cursor never goes
               beyond screen.maxcol */

            else {
                catstr (msg_line, bs_str,
                     1, "msg_line",  sizeof (msg_line));
                curc--;
            }
        }                                 /* End of maxcol code */
    }                                     /* End of msg parse loop */

    /* Display any remaining message text */

    if (strlen (msg_line) > 0) { 
        putscr (msg_line, strlen (msg_line));
        setmem (msg_line, sizeof (msg_line), NUL);
    }

    /* If the message doesn't have a trailing NL and isn't the prompt string,
       it may be (at least part of) a prompt string. */

    if (curc > 0 && ~pstr_sw) { 
        i -= ipstr;

        if (ds.pstrl + i < MAX_SCREEN_COL) {         /* Only if the shoe fits ... */
            catstr (ds.pstr, &msg [ipstr], i, "ds.pstr",
                 sizeof (ds.pstr));
            ds.pstrl += i;
            kb.pos [0] = curc;
        }

    }

    screen.curlin = curl;
    screen.curcol = curc;

}                                       /* End of emit_msg */



/*
**********************************************************************

  Routine:            LOCAL_ESC 

  Function:
      This routine handles local escape functions requested when the 
  user hits certain ALT keys. 

  Parameters:
     (input)          scan_code - specifies the scan code for the 
                          particular ALT key entered; it determines 
                          what local escape function is being 
                          requested 

  Returns:            NONE 

**********************************************************************/

local_esc (scan_code)
int scan_code;
{
    register int    i;                  /* Working index */

    /* Move the action character to static so everybody knows what it is */

    local_action = tolower (kb.chr [0]);

    switch (scan_code) {

    case Q_KEY_CODE:                    /* Exit wsterm */

        if (read_active) {              /* Terminate any active read */
            term_read = ON;
            send_msg (nul_str, 0);
        }

        local_action = QUIT;            /* Force the defined QUIT */
        break;

    case S_KEY_CODE:
        wst_freeze = !wst_freeze;   /* toggle flag which control scrolling */
        update_status();            /* update change on status line */
        if (wst_freeze) {
            wait_for_key();
            wst_freeze = FALSE;
            update_status();
        }
        break;

    /* toggle edit mode */
    case E_KEY_CODE:
        if (mowse_active)
            status_err_msg("Cannot toggle edit in packet mode. <any key>-resume");

        else if (wst_edit_mode) {
            if (edlin.length > 0)
                status_err_msg("Line not sent, enter or kill line first. <any key>-resume");

            else {
                wst_edit_mode = FALSE;
                /* set the default to be NOT in edit mode */
                glass_edit_mode = FALSE;
                kb.echo = FALSE;    /* phx21233 R.L. update echo flag for glass tty edit mode */
            }
        }
        else if (mowse_active && kb.cndx > 0)
            status_err_msg("Line not sent, enter or kill line first. <any key>-resume");

        else {
            wst_edit_mode = ON;
            /* set the default to be in edit mode */
            glass_edit_mode = TRUE;
            kb.echo = TRUE;    /* phx21233 R.L. update echo flag for glass tty edit mode */
        }
        update_status();
        break;


    case O_KEY_CODE:
        wst_paging = !wst_paging;   /* toggle flag for displaying EOPs */
        update_status();            /* update change on status line */
        break;

    case F_KEY_CODE:
        wst_f_audit = !wst_f_audit;  /* toggle flag for file audit */
        if (wst_f_audit) {
            if (begin_file_audit() < 0)
                wst_f_audit = OFF;
        }
        else
            end_file_audit();
        update_status();             /* update change on status line */
        break;

    case P_KEY_CODE:
        wst_p_audit = !wst_p_audit;  /* toggle flag for printer audit */
        if (wst_p_audit) {
            if (begin_printer_audit() < 0)
                wst_p_audit = OFF;
        }
        else
            end_printer_audit();
        update_status();             /* update change on status line */
        break;

    case M_KEY_CODE:                    /* Display minibuffer */

        display_bkmsg ();
        break;

    case B_KEY_CODE:                    /* Line break */

        /* Wipe out all input typeahead */

        kb.cndx = 0;
        kb.endx = 0;
        kb.klin [0] = NUL;

        if (wst_edit_mode && !sync) {
            cursor_eol(&edlin);
            screen.curlin = edlin.cur_row;
            screen.curcol = edlin.cur_col;
        }

        edlin.index = 0;
        edlin.length = 0;

        flush_dos_keys();

        if (mowse_active) {
            put_to_screen(CR);
            put_to_screen(LF);
            screen.curlin = wst_fg_screen.cursor_row;
            screen.curcol = wst_fg_screen.cursor_col;
            ds.ccol = 0;
            ds.lct = 0;
            ds.lndx = 0;
            setmem(ds.dlin,sizeof(ds.dlin),NUL);
            ds.pstrl = 0;
            ds.pstr[0] = 0;
            ds.splct[0] = 0;
            screen.EOP_ct = 0;
        }

        puttdata (FG_BREAK, NUL, 0);
        break_sent = mowse_active;
        break;

    case C_KEY_CODE:                    /* Clear screen */

        clear_screen ();
        cursor_move (CURSOR_HOME_ROW, 60);
        putscr("WSTERM Vers ",12);
        putscr(version,strlen(version));
        cursor_move (CURSOR_HOME_ROW,CURSOR_HOME_COL);        

        if (ds.pstrl > 0)
            emit_msg (ds.pstr, ds.pstrl, PSTR);
        else if (wst_edit_mode) {
            edlin.orig_row = edlin.cur_row = edlin.max_row = CURSOR_HOME_ROW;
            edlin.orig_col = edlin.cur_col = edlin.cur_row = CURSOR_HOME_COL;
            redraw_edit_line(&edlin);
        }

        break;

    case zero_KEY_CODE:                 /* Send NUL (set mark) */
    case 129:

        if (mowse_active)
            send_msg (nul_str, 1);
        else 
            puttdata (FG_TERMINAL_DATA, nul_str, 1);
        break;

    case D_KEY_CODE:                    /* Display message */

        if (~fg_msg_showing) { 
            status_err_msg("No foreground messages. <any key>-resume");
            update_status();
        }
        else { 
            if (wst_edit_mode || (mowse_active && ~sync)) { 
                /* redraw for edit mode */
                erase_edit_line(&edlin);
                emit_msg(fg_msg.text,fg_msg_len,~PSTR);
                fg_msg_len = 0;
                signal_fg(OFF);
                fg_msg_showing = OFF;
                cursor_pos(&edlin.cur_row,&edlin.cur_col);
                edlin.orig_row = edlin.cur_row;
                edlin.orig_col = edlin.cur_col;
                redraw_edit_line(&edlin);
            }
        }
        break;

    case H_KEY_CODE:    /* invoke the help screen */
        help(GENERAL_HELP);
        update_status();  /* restore the status line before returning */
        break;

    case V_KEY_CODE:  /* invoke history screen */
        if (wst_edit_mode) {
            history_screen(&wst_fg_screen);
            update_status();
        }
        else {
            status_err_msg("History functions available only in edit mode. <any key>-resume");
            update_status();
        }
        break;

    default:                            /* Anything else is an error */
        status_err_msg("Unrecognized WSTERM request, enter ALT-H for help.  <any key>-resume");
        update_status();
        break;
    }
}                                       /* End of local_esc */



/*
**********************************************************************

  Routine:            PUTSCR 

  Function:
      This routine writes a string to the screen driver. Undesirable 
  escape sequences are filtered out. The ANSI clear screen "ESC [ 2 
  J" and clear to end of page "ESC [ J" sequences are intercepted to 
  prevent the 25th line from being cleared. Instead, appropriate 
  routines are called to clear all but the 25th line and to clear up 
  to but not including the 25th line. Clearing the 25th line (the 
  status line) causes flickering each time the screen is cleared. 

  Parameters:
     (input)          str - specifies the buffer containing the 
                          message to display 
     (input)          strl - specifies the number of characters from 
                          the message buffer to display 

  Returns:            NONE 

**********************************************************************/

putscr(str,strl)
char *str;
int strl;
{
    int ch;
    int i;

    /* process each character */
    for (i = 0; i < strl; i++) {

        if (checkkey() == ALT_S) {
            wait_for_key();
            wst_freeze = !wst_freeze; /* toggle flag which control scrolling */
            update_status();            /* update change on status line */
        }

        if (wst_freeze) {
            wait_for_key();
            wst_freeze = FALSE;
            update_status();
        }
            
        ch = *str++;

/* algorithm: increment index each time the character received
    matches the next character in the escape sequence until the
    entire escape sequence is received. Once the entire sequence
    is recognized, handle it. */

        /* matching first character of escape sequence? */
        if (clr_index == 0) {
            /* first character an escape char? */
            if (ch == ESC)
                /* yes, match next character in sequence */
                clr_index++;
            /* not escape character, just display the character */
            else
                put_to_screen(ch);
        }

        /* check if second character of sequence */
        else if (clr_index == 1) {

            /* match second char in sequence ? */
            if (ch == '[')
                clr_index++;

            /* check if still part of valid escape sequence */
            else if (is_esc[ch]) {
                clr_index = 0;
                put_to_screen(ESC);
                put_to_screen(ch);

                /* if reset sequence ESC c, recover for WSTERM */
                if (ch == 'c')
                    wst_reset();
            }
            /* not the sequence we are looking for, flush */
            else {
                clr_index = 0;
                put_to_screen(ch);
            }
        }

        /* check if third character of sequence */
        else if (clr_index == 2) {

            /* last char in "ESC [ J" sequence? */
            if (ch == 'J') {
                clear_eow();
                clr_index = 0;
            }

            /* the '2' in "ESC [ 2 J" sequence? */
            else if (ch == '2')
                clr_index++;

            /* not a recognized sequence, flush */
            else {
                put_to_screen(ESC);
                put_to_screen('[');
                put_to_screen(ch);
                clr_index = 0;
            }
        }

        /* the 'J' in "ESC [ 2 J" sequence */
        else if (clr_index == 3) {
            if (ch == 'J') {
                clear_window();
                clr_index = 0;
            }

            /* not recognized, flush */
            else {
                put_to_screen(ESC);
                put_to_screen('[');
                put_to_screen('2');
                put_to_screen(ch);
                clr_index = 0;
            }
        }

        /* not recognized, flush */
        else {
            clr_index = 0;
            put_to_screen(ch);
        }
    }
}



/*
**********************************************************************

  Routine:            CLEAR_EOW 

  Function:
      This routine is used to handle the ANSI escape sequence "ESC [ 
  J" to clear the screen from the cursor position to the end of the 
  screen. This routine simulates clearing to the end of the screen 
  but does not damage the 25th line (the status line). 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

clear_eow()
{
    union REGS reg, outreg;
    int row, col;

    cursor_pos(&row,&col);
    if (row < 24) {

        put_screen_str(EL);  /* display Erase Line escape sequence */

        if (row+1 < SCREEN_LINES) {

            reg.h.ah = VIDEO_SCROLL_UP_FUNC;
            reg.h.al = 0;   /* lines to scroll; 0 means clear scroll region */
            reg.h.ch = row+1;  /* start on next line */
            reg.h.cl = 0;      /* left most column */
            reg.h.dh = SCREEN_LINES-1;    /* to end of screen */
            reg.h.dl = SCREEN_COLS-1;     /* to last column */
            reg.h.bh = wst_fg_screen.attr;
            int86(BIOS_VIDEO,&reg,&outreg);

        }
    }
    cursor_move(row,col);
}



/*
**********************************************************************

  Routine:            CLEAR_WINDOW 

  Function:
      This routine is called to clear the screen and to initialize 
  the global screen information variables accordingly. The contents 
  of the 25th line (status line) remains untouched. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

clear_window()
{
    union REGS reg, outreg;

    reg.h.ah = VIDEO_SCROLL_UP_FUNC;
    reg.h.al = 0;  /* line to scroll, 0 means clear scroll region */
    reg.x.cx = 0;  /* home row and column coordinates */
    reg.h.dh = SCREEN_LINES-1;   /* end of screen row and column coordinates */
    reg.h.dl = SCREEN_COLS-1;
    reg.h.bh = wst_fg_screen.attr;

    int86(BIOS_VIDEO,&reg,&outreg);
    cursor_move(CURSOR_HOME_ROW,CURSOR_HOME_COL);
    screen.curcol = CURSOR_HOME_COL;
    screen.curlin = CURSOR_HOME_ROW;
}



/*
**********************************************************************

  Routine:            SCROLL 

  Function:
      This function manages the advancement of the display down the 
  screen. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

scroll ()

{         
    union regs_struct regs, outregs;

    /* Clear local action character */

    /*  action = NUL;*/

    /* If we're at the bottom of the main screen area, scroll up one line */

    if (screen.curlin >= MAX_SCREEN_LINE) {
        wst_scroll();

        regs.hreg.ah = SET_CURSOR_POS;      /* Position cursor at blank line */
        regs.hreg.bh = get_active_page();   /* in current active page */
        regs.xreg.dx = LINE24_COL0;
        int86 (BIOS_VIDEO, &regs, &outregs);

    }

    /* Otherwise, emit NL and count a screen line */
    else {
        putscr (nl, 2);
        screen.curlin++;
        screen.curcol = 0;

    }

    if (mowse_active && wst_paging)           /* Count an EOP line, too */
        screen.EOP_ct++;
    ds.pstrl = 0;
    setmem (ds.pstr, sizeof (ds.pstr), NUL);

    /* If mowse is attached, do local EOP processing */

    if (mowse_active) { 
        if (screen.EOP_ct == screen.maxlin && screen.maxlin > 0) {
            putscr ("EOP", 3);

            /* Wait for a character from the keyboard */
            screen.EOP_ct = 0;
            while (checkkey() < 0);
            if (checkkey() == ALT_B || checkkey() == BREAK_KEY)
                return;
            getkey(BLOCK);
            put_to_screen(CR);
            put_to_screen(LF);
        }
    }
}                                       /* End of scroll */



/*
**********************************************************************

  Routine:            FLUSH_DOS_KEYS 

  Function:
      This routine discards any keys pending in DOS's keyboard 
  buffer. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

flush_dos_keys()
{
    union REGS r, outr;

    r.h.ah = BIOS_KB_STATUS;

    /* while key in keyboard buffer, get the key from keyboard
       buffer */
    while (!(int86(BIOS_KB,&r,&outr) & Z_FLAG_MASK)) {
        r.h.ah = BIOS_KB_READ;
        int86(BIOS_KB,&r,&outr);
        r.h.ah = BIOS_KB_STATUS;
    }
}



/*
**********************************************************************

  Routine:            STATUS_ERR_MSG 

  Function:
      This routine displays an error message on the status line and 
  waits for any key hit to resume. 

  Parameters:
     (input)          msg - the null-terminated string containing the 
                          error message to display 

  Returns:            NONE 

**********************************************************************/

status_err_msg(msg)
char *msg;
{
    int row, col;

    cursor_pos(&row,&col);
    cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);
    status_line(msg);
    wait_for_key();
    cursor_move(row,col);
}


/* End of WSTUTIL */
