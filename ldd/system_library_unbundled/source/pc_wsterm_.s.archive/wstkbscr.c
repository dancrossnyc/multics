/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1987 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(87-05-04,Wallman), approve(87-05-04,MCR7586),
     audit(87-07-16,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(87-09-02,Wallman), approve(87-09-02,MCR7586),
     audit(87-07-16,Flegel), install(87-09-10,MR12.1-1103):
     PBF to improve robustness of string handling by avoiding all str*
     functions.
  3) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables; code re-formatting.
  4) change(88-04-11,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added mapping of ^2 key to ALT-0, DEL key to ASCII del and
     ^BREAK (if break flag is set) to ALT-B.
  5) change(88-05-18,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Deleted parameter hide_sw in calls to replay().
  6) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  7) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  8) change(88-08-30,Lee), approve(88-09-12,MCR7986), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed non-edit async mode and references to non-edit async mode
     line editing routines.
  9) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Separated input and output parameters to int86() to avoid confusion
     when parameters are used.
                                                   END HISTORY COMMENTS */

/* WSTKBSCR - keyboard/screen manager for WSTERM */

/* This module holds functions needed to handle keyboard input and to manage
   the screen display. */

#include    <stdio.h>
#include    <dos.h>
#include    <ws.h>
#include    <ws_mcb.h>
#include    <wsmincap.h>
#include    "wstdefs.h"
#include    "wsttype.h"
#include    "wstglob.h"


/**** PUBLIC FUNCTIONS ******************************************************/

/*
**********************************************************************

  Routine:            EXTRACT_MSG 

  Function:
      This routine extracts sync mode messages from kb.klin. Sync 
  mode messages are delimited by break characters, screen line length 
  overflow, and read count exhaust. 

  Parameters:
     (input)          no_block - specifies to send zero length 
                          messages 
     (input)          tro_sw - timer run out switch 
     (input)          txmt_sw - specifies to transmit the message 
     (input)          hide_sw - switch to suppress echoing 

  Returns:            NONE 

**********************************************************************/

extract_msg (no_block, tro_sw, txmt_sw, hide_sw)
int no_block,   /* Send 0-length messages */
tro_sw,         /* Timer run out switch */
txmt_sw,        /* Transmit the message */
hide_sw;        /* Switch to suppress echoing */

{   
    int curc,       /* Local column value */
        echo;       /* Local echo control */

    register int    cndx;        /* Working index */

    /* Scan the keyboard buffer. We will hit one of:
       1) a break character (msgs were backed up)
       2) end of kb.klin (a timer runout has occurred)
       3) read_count exhaust
       4) screen.maxcol overflow */

    echo = kb.echo;
    curc = screen.curcol;
    for (cndx = kb.endx;  cndx < strlen (kb.klin) && (read_count > 0)
        && ((echo && curc <= screen.maxcol) || ~echo);  cndx++) { 
        read_count--;           /* Decrement the read count */

        /* If WSTERM is echoing, count and echo all printing characters */

        if (echo && ~hide_sw && isprint (kb.klin [cndx])) { 
            curc++;
            putscr (&kb.klin [cndx], 1);
        }

        /* If the klin character is a break character or a control
           character, we're at the linelength limit, or the read
           count has exhausted, end the read, send that much, force
           echoing OFF, and ltrim the data sent from kb.klin */

        if (stpchr (brk_table, kb.klin [cndx]) || iscntrl (kb.klin [cndx]) ||
            (echo && curc == screen.maxcol) || read_count == 0) { 
            term_read = ON;     /* Terminate the read */

            send_msg (kb.klin, ++cndx);
            strcpy (kb.klin, &kb.klin [cndx]);
            kb.cndx = strlen (kb.klin);
            cndx = kb.cndx;
            kb.endx = 0;
            echo = OFF;         /* Force echoing OFF */
            read_active = OFF;
        }
    }               /* End of kb.klin scan loop */

    /* If we fall out of the kb.klin scan loop with anything left we
       have reached the end of kb.klin */

    if (strlen (kb.klin) > 0) {

        /* Send the leftovers if not echoing, otherwise, update the last echoed
           character index (kb.endx) */

        if (txmt_sw && ~kb.echo && read_active) { 
            send_msg (kb.klin, strlen (kb.klin));
            kb.cndx = 0;
            kb.endx = 0;
            setmem (kb.klin, sizeof (kb.klin), NUL);
        }
        else if (kb.echo && ~hide_sw)
            kb.endx = kb.cndx;
    }

    screen.curcol = curc;

}               /* End of extract_msg */



/*
**********************************************************************

  Routine:            NEXT_TAB 

  Function:
      This routine computes the distance (in columns) to the next tab 
  column. 

  Parameters:
     (input)          ccol - specifies the current screen column 

  Returns:            the number of columns to the next tab column 

**********************************************************************/

next_tab (ccol)
int ccol;       /* Current column */

{ 
    int space;

    /* assume that the tabs are set every 10 columns; */
    /* this calculation may have to work for negative */
    /* values. */

    space = TAB_SIZE * ((ccol + TAB_SIZE) / TAB_SIZE) - ccol;
    return (space);
}               /* End of next_tab */



/*
**********************************************************************

  Routine:            PROCESS_KBRD_CHAR 

  Function:
      This routine handles keyboard input for non-edit mode and sync 
  mode. 

  Parameters:
     (input)          hide_sw - switch to suppress echoing 

  Returns:            NONE 

**********************************************************************/

process_kbrd_char (hide_sw)
int hide_sw;        /* Switch to suppress echoing */

{
    read_keyboard (BLOCK);

    /* Processing an octal escape? */

    if (octsw > 0) {

        /* Is this a meaningful octal digit? */

        if (isdigit (kb.chr [0]) && kb.chr [0] < '8' && octsw < 3) { 
            octval = 8 * octval + (int) kb.chr [0] - 060;
            octsw++;

            return;
        }
        else {  /* Not part of \nnn */
            sprintf (octstr, "%c%03o", LNC, octval);
            sprintf (kb.dstr, "%c", kb.chr [0]);

            put_kchr (hide_sw);
            octsw = 0;
        }

        return;
    }               /* End of octal processing */

    /* if control char or ALT key or function key */

    if (iscntrl (kb.chr [0]) || (kb.key_status & ALT) || kb.fkey) { 
        process_ctl_char (hide_sw);
        return;
    }

    if (~mowse_active) {     /* Glass TTY mode? */

        /* Send any leftover typeahead */

        if (kb.cndx > 0) { 
            puttdata (FG_TERMINAL_DATA, kb.klin, kb.cndx);
            kb.cndx = 0;
        }

        if (kb.echo && ~hide_sw) {

            putscr (kb.dstr, 1);
        }
        puttdata (FG_TERMINAL_DATA, kb.chr, 1);
    }               /* End of glass TTY mode */

    /* Otherwise, we are in packet mode */

    else {
        /* Transmit sync mode message for control and break characters */

        if (sync && (iscntrl (kb.chr [0]) || stpchr (brk_table, kb.chr [0]))) {

            put_kchr (hide_sw);
            if (read_active)
                extract_msg (~NO_BLOCK, ~TRO,
                    TXMT_MSG, hide_sw);
            return;
        }

        /* we're in sync mode */

        sprintf (kb.dstr, "%c", kb.chr [0]);
        put_kchr (hide_sw);

    }               /* End of packet mode */
}               /* End of process_kbrd_char */



/*
**********************************************************************

  Routine:            READ_KEYBOARD 

  Function:
      This routine reads the keyboard for key presses. If a key is 
  read, it is kept and maintained in the global structure 'kb'. The 
  caller may specify that the routine waits if no keyboard input or 
  returns immediately instead. 

  Parameters:
     (input)          block_sw - if TRUE, routine will wait if no 
                          keyboard key was pressed, otherwise routine 
                          returns immediately 

  Returns:            OFF if no key in the key queue 
                      ON if a key was fetched from the key queue 

**********************************************************************/

read_keyboard (block_sw)
int block_sw;       /* Block-for-input switch */

/*  If block_sw is ON, return the key code of the first available
    keystroke, waiting if there isn't one waiting the DOS key queue.
    The key code is (scan code || character code).

    If block_sw is OFF, return OFF if the DOS key queue is empty;
    otherwise, return the key code if the first queued keystroke.
*/

{   
    int flags,      /* CPU flags */
    key_q_sw;
    union regs_struct regs, outregs;
    int ch;

    /* check if break key has been hit */
    if (break_flag) {

        /* if block switch is off, just return key hit status */
        if (~block_sw)
            return(ON);

        /* handle the break key by mapping it to ALT-B for BREAK
           processing */
        kb.key_status = ALT;
        kb.key_ndx = B_KEY_CODE;
        kb.chr [0] = NUL;
        kb.chr [1] = NUL;
        kb.fkey = OFF;
        break_flag = 0;
        return(ON);
    }

    setmem (&regs, sizeof (regs), 0);

    /* Get key queue status */

    regs.hreg.ah = BIOS_KB_STATUS;
    flags = int86 (BIOS_KB, &regs, &outregs) & LOW_8_BITS;
    key_q_sw = -((flags & Z_FLAG_MASK) == 0);    /* ZF = no character */

    /* Get key shift status */

    regs.hreg.ah = BIOS_KB_SHIFTS;
    int86 (BIOS_KB, &regs, &outregs);
    if (kb.key_status == -1)
        kb.key_status = outregs.hreg.al;

    /* Return changes if we're not asked to block */

    if (~block_sw) {

        return (key_q_sw);
    }

    /* Get keystroke from DOS */

    regs.hreg.ah = BIOS_KB_READ;
    int86 (BIOS_KB, &regs, &outregs);
    kb.key_ndx = outregs.hreg.ah;
    kb.chr [0] = (char) outregs.hreg.al;
    kb.chr [1] = NUL;
    kb.fkey = OFF;

    /* encode keyboard character into unique code */
    if (outregs.hreg.al)
        ch = outregs.hreg.al;
    else
        ch = outregs.hreg.ah + ASCII_EXTEND_CODE;

    /* if ^2 (same as ^@) key hit, map it ALT-0 to send a NUL */
    if (ch == CTRL_2) {
        kb.key_status = ALT;
        kb.key_ndx = zero_KEY_CODE;
        kb.chr [0] = NUL;
        kb.chr [1] = NUL;
        kb.fkey = OFF;
        return(ON);
    }

    /* if DEL key hit, map it onto ASCII DEL */
    else if (ch == DEL_KEY) {
        kb.key_status = 0;
        kb.key_ndx = 0;
        kb.chr [0] = DEL;
        kb.chr [1] = NUL;
        kb.fkey = OFF;
        return(ON);
    }

    /* Refetch shift status in case it changed during the wait for the character */

    regs.hreg.ah = BIOS_KB_SHIFTS;
    int86 (BIOS_KB, &regs, &outregs);
    kb.key_status = outregs.hreg.al;


    /* Is this a function key? */

    if (kb.key_ndx >= F1_KEY_CODE && kb.key_ndx < F1_KEY_CODE + 10) { 
        kb.fkey = ON;
        strcpy (kb.chr, &F1_10 [kb.key_ndx - F1_KEY_CODE - 1][4]);
    }

    if (kb.key_ndx >= F11_KEY_CODE && kb.key_ndx < F11_KEY_CODE + 2) { 
        kb.fkey = ON;
        strcpy (kb.chr, &F11_12 [kb.key_ndx - F11_KEY_CODE - 1][4]);
    }

    return (ON);
}               /* End of read_keyboard */



/*
**********************************************************************

  Routine:            SEND_MSG 

  Function:
      This routine sends a control message to the host. 

  Parameters:
     (input)          msg_str - specifies the message buffer 
                          containing the message to send 
     (input)          msg_len - specifies the number of bytes in the 
                          message buffer to send 

  Returns:            NONE 

**********************************************************************/

send_msg (msg_str, msg_len)
int     msg_len;        /* Number of bytes to send */
char    *msg_str;       /* The byte string */

{   
    struct snd_msg_struct snd_msg;

    setmem (&snd_msg, sizeof (snd_msg), 0);

    if (~kb.echo) {          /* RNE */
        if (term_read) { 
            snd_msg.hdr.id [0] = 'E';
            snd_msg.hdr.id [1] = 'N';
            snd_msg.hdr.id [2] = 'I';
            term_read = OFF;
            read_active = OFF;
            kb.echo = OFF;
        }
        else { 
            snd_msg.hdr.id [0] = 'U';
            snd_msg.hdr.id [1] = 'I';
            snd_msg.hdr.id [2] = 'C';
        }
    }
    else {  /* RWE */
        if (term_read) { 
            snd_msg.hdr.id [0] = 'E';
            snd_msg.hdr.id [1] = 'E';
            snd_msg.hdr.id [2] = 'I';
            term_read = OFF;
            read_active = OFF;
            kb.echo = OFF;
        }
        else { 
            snd_msg.hdr.id [0] = 'E';
            snd_msg.hdr.id [1] = 'I';
            snd_msg.hdr.id [2] = 'C';
        }
    }

    byteshift (msg_len, &snd_msg.hdr.msb_size, &snd_msg.hdr.lsb_size);
    snd_msg.data [0] = NUL;     /* Erase target string */
    strncpy (snd_msg.data, msg_str, msg_len);

    puttdata (FG_CONTROL_MESSAGE, &snd_msg, msg_len + 5);
}               /* End of send_msg */



/**** LOCAL FUNCTIONS *******************************************************/

/*
**********************************************************************

  Routine:            PROCESS_CTL_CHAR 

  Function:
      This routine handles control, ALT and function keys in non-edit 
  mode (and sync) mode. 

  Parameters:
     (input)          hide_sw - if TRUE, echoing is suppressed 

  Returns:            NONE 

**********************************************************************/

process_ctl_char (hide_sw)
int hide_sw;        /* Switch to suppress echoing */

{
    /* Check for local escapes */

    if (kb.key_status & ALT) {

        local_esc (kb.key_ndx);
        return;
    }

    /* For glass TTY mode, just send the 'character' */

    if (~mowse_active) { 
        puttdata (FG_TERMINAL_DATA, kb.chr, strlen (kb.chr));

    }  /* For sync mode, they are break characters */

    else if (sync) { 
        catstr (&kb.klin [kb.cndx], kb.chr, strlen (kb.chr),
             "kb.klin", sizeof (kb.klin));
        kb.cndx += strlen (kb.chr);
        kb.klin [kb.cndx] = NUL;

        if (read_active) { 
            term_read = ON;
            extract_msg (~NO_BLOCK, ~TRO, TXMT_MSG,
                 hide_sw);
        }
    }
}               /* End of process_ctl_char */



/*
**********************************************************************

  Routine:            PUT_KCHR 

  Function:
      When a key or an octal value (specified by entering \ followed 
  by octal digits) is entered, it is stored for examination before 
  further processing takes place. This routine takes the stored value 
  and places it into the global buffer containing the input line 
  (kb.klin). The routine is called when not in edit mode.

  Parameters:
     (input)          hide_sw - if TRUE, specifies echoing is to be 
                          suppressed 

  Returns:            NONE 

**********************************************************************/

put_kchr (hide_sw)
int hide_sw;        /* Switch to suppress echoing */

{   

    /* Is there a pending octal escape */

    if (octsw > 0) { 
        if (kb.cndx == strlen (kb.klin)) {   /* Tack octval on the end */
            kb.klin [kb.cndx] = (char) octval;
            kb.pos [kb.cndx] = ds.ccol;
            kb.klin [++kb.cndx] = NUL;
        }
        else  /* Replace buffered character */
            kb.klin [kb.cndx] = (char) octval;
    }

    if (kb.cndx == strlen (kb.klin)) {      /* Tack kb.chr on the end */
        kb.klin [kb.cndx] = kb.chr [0];
        kb.pos [kb.cndx] = ds.ccol + 4 * (octsw > 0);
        kb.klin [kb.cndx + 1] = NUL;
    }
    else 
        kb.klin [kb.cndx] = kb.chr [0]; /* Replace buffered character */

    if (~sync) { 
        if (put_kstr (hide_sw)) {       /* Nonzero if kb.dstr wont fit */
            kb.klin [kb.cndx] = NUL;
            return;
        }
    }

    kb.cndx++;          /* Advance buffer index */
}               /* End of put_kchr */



/*
**********************************************************************

  Routine:            PUT_KSTR 

  Function:
      This routine displays the keyboard character string in the 
  global variable kb.dstr. 

  Parameters:
     (input)          hide_sw - if TRUE, specifies that echoing is to 
                          be suppressed 

  Returns:            NONE 

**********************************************************************/

put_kstr (hide_sw)
int hide_sw;        /* Switch to suppress echoing */

{   
    int err_flg;        /* Error flag */

    err_flg = OFF;

    /* Replacing a character */

    if (ds.ccol < strlen (ds.dlin) && ~hide_sw && kb.endx == kb.cndx) { 
        if (replay (strlen (kb.dstr)))
            err_flg = ON;
    }

    /* Appending a character */

    else {

        /* Wrap line if last column */

        if (ds.ccol == screen.maxcol) { 
            if (wrap_line (ds.lndx, ds.ccol))
                err_flg = ON;       /* Error if line wont fit */
            else 
                ds.lndx++;
        }

        /* If kb.dstr overflows */

        if (strlen (ds.dlin) + strlen (kb.dstr) > screen.maxcol &&
            ~err_flg) { 
            if (replay (strlen (kb.dstr)))
                err_flg = ON;
        }
        else if (~err_flg)       /* Doesn't overflow */ { 
            if (octsw > 0) { 
                catstr (ds.dlin, octstr, 4, "ds.dlin",
                     sizeof (ds.dlin));
                ds.ccol += strlen (octstr);
                if (kb.echo && ~hide_sw)
                    putscr (octstr, 4);
            }

            if (kb.echo && ~hide_sw) { 
                if (ds.ccol < strlen (ds.dlin)) { 
                    putscr (&ds.dlin [ds.ccol],
                         strlen (&ds.dlin [ds.ccol]));

                    ds.ccol = strlen (ds.dlin);
                }
                else { 
                    catstr (ds.dlin, kb.dstr,
                         strlen (kb.dstr), "ds.dlin", sizeof (ds.dlin));
                    ds.spill [ds.lndx] = 0;
                    ds.ccol += strlen (kb.dstr);
                    putscr (kb.dstr, strlen (kb.dstr));

                }
            }
        }
    }

    kb.endx++;

    return (err_flg);
}               /* End of put_kstr */



/*
**********************************************************************

  Routine:            WRAP_LINE 

  Function:
      This routine wraps a keyboard display line. 

  Parameters:
     (input)          lin - specifies the current line 
     (input)          col - specifies the current column 

  Returns:            NONE 

**********************************************************************/

wrap_line (lin, col)
int lin,        /* Current line */
col;        /* Current column */

{

    /* Copy ds.dlin to ds.map */

    strcpy (&ds.map [lin][0], ds.dlin);

    /* Is the display map full? */

    if (lin == DS_LCT - 1) { 
        putscr (SCP, strlen (SCP));
        cursor_move (MINI_LIN, 0);
        printf ("%s%s", "Input line cannot be processed because it ",
                    "exceeds screen size. (any key)");

        /* Wait for a character */

        while (read_keyboard (~BLOCK))
            ;

        cursor_move (MINI_LIN, 0);
        putscr (EL, strlen (EL));
        putscr (RCP, strlen (RCP));

        cursor_move (screen.curlin + lin, col);
        putscr (EL, strlen (EL));
        return (ON);
    }

    scroll ();
    ds.ccol = 0;
    ds.splct [lin + 1] = 0;
    setmem (ds.dlin, sizeof (ds.dlin), NUL);
    if (lin + 1 > ds.lct)
        ds.lct++;

    return (OFF);
}               /* End of wrap_line */


