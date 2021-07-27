/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1987 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(87-03-21,Wallman), approve(87-03-21,MCR7586),
     audit(87-08-10,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(87-09-02,Wallman), approve(87-09-02,MCR7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     PBF to support the ABT order by flushing keyboard input.
  3) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables, re-formatting.
  4) change(88-05-17,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed code for accessing the mini-buffer on the 25th line
     which is now used as a status line.
  5) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation only, added header comments.
  6) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  7) change(89-01-30,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Initialize edit-mode screen size and line length variables from
     incoming mowse_io control (STM) message.
                                                   END HISTORY COMMENTS */

/* WSTFRGND - WSTERM module to process incoming foreground messages */

/* This modules holds the functions needed to process foreground messages. */

#include    <stdio.h>
#include    <dos.h>
#include    <ws.h>
#include    <ws_mcb.h>
#include    <wsmincap.h>
#include    "wstdefs.h"
#include    "wstglob.h"

/*
**********************************************************************

  Routine:            ASYNC_MSG 

  Function:
      This routine is called to handle an async message from the 
  host. 

  Parameters:
     (input)          force_sw - if TRUE, specifies that terminal 
                          messages are forced to be displayed; 
                          terminal messages are usually not displayed 
                          while handling partial keyboard input 

  Returns:            NONE 

**********************************************************************/

async_msg (force_sw)
int force_sw;       /* Force the message */

{
    /* If there's typeahead in kb.klin, the fg_msg signal is showing,
       and the force_sw is off, just return */

    if (fg_msg_showing && strlen (kb.klin) > 0 && ~force_sw)
        return;

    /* Is this a control message? */

    if (tdata_arg.minor_capability == FG_CONTROL_MESSAGE) { 
        ctrl_msg ();
        fg_msg_len = 0;
        return;
    }

    /* Its a display message */

    /* Discard message while waiting for FG_BREAK */

    if (break_sent)
        fg_msg_len = 0;

    else {

        /* Display the message if there's no typeahead or it is to be forced */
        if ((strlen (kb.klin) == 0 && edlin.length < 1) || force_sw) { 

            /* audit the message to file if file audit enabled */
            if (wst_f_audit)
                f_audit_msg(fg_msg.text,fg_msg_len);

            /* audit the message to printer if printer audit enabled */
            if (wst_p_audit)
                p_audit_msg(fg_msg.text,fg_msg_len,NULL);

            /* display the terminal message */
            emit_msg (fg_msg.text, fg_msg_len, ~PSTR);
            fg_msg_len = 0;

            if (fg_msg_showing) {    /* Erase fg_msg signal */
                signal_fg(OFF);
                fg_msg_showing = OFF;
            }
        }

        /* Otherwise, show the fg_msg signal */

        else if (~fg_msg_showing) { 
            signal_fg(ON);
            fg_msg_showing = ON;
        }
    }
}               /* End of async_msg */



/*
**********************************************************************

  Routine:            SYNC_MSG 

  Function:
      This routine is called to handle a sync message from the host. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

sync_msg ()
{

    /* Send any typeahead */

    if (read_active && kb.cndx > 0)
        extract_msg (~NO_BLOCK, ~TRO, TXMT_MSG, ~HIDE);

    /* Is this a control message? */

    if (tdata_arg.minor_capability == FG_CONTROL_MESSAGE)
        ctrl_msg ();

        /* Its a display message */

    else {
        if (~break_sent) { 
            emit_msg (fg_msg.text, fg_msg_len, ~PSTR);

            /* audit to file if file audit enabled */
            if (wst_f_audit)
                f_audit_msg(fg_msg.text,fg_msg_len);

            fg_msg_len = 0;
        }
    }

    fg_msg_len = 0;         /* Message is procesed */
}               /* End of sync_msg */



/*
**********************************************************************

  Routine:            CTRL_MSG 

  Function:
      This routine is called to handle control messages from the 
  host. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

ctrl_msg ()
{   
    char    msg_id [4]; /* Message ID */
    int no_blk_sw;      /* For no-block reads */

    strncpy (msg_id, fg_msg.ctl.hdr.id, 3);
    msg_id[3] = 0;

    /* ABORT */

    if (strcmp (msg_id, "ABT") == 0) { 
        kb.cndx = 0;
        kb.endx = 0;
        setmem (kb.pos, sizeof (kb.pos), NUL);
        setmem (kb.klin, sizeof (kb.klin), NUL);
        edlin.length = 0;           /* phx21233 R.L. - flush edit mode input */
        edlin.escape_flag = 0;
        edlin.escape_arg = -1;
        edlin.index = 0;
    }  /* PRINTER ON/OFF */

    else if (strcmp (msg_id, "PON") == 0 || strcmp (msg_id, "POF") ==
        0) { 
        kb.echo = -(strcmp (msg_id, "PON") == 0);

    }  /* SET TTY MODES */

    else if (strcmp (msg_id, "STM") == 0) { 
        line_kill = fg_msg.ctl.data.stm.kill;
        char_erase = fg_msg.ctl.data.stm.erase;
        lnc = fg_msg.ctl.data.stm.lnc;
        strncpy (erkl_chars, &fg_msg.ctl.data.stm.kill, 3);

        /* phx21233 RL - remove restriction of page and line size to screen size */
        screen.maxcol = fg_msg.ctl.data.stm.maxcol;
        screen.maxlin = fg_msg.ctl.data.stm.maxlin;

        /* Mode switches */
        sync = -((fg_msg.ctl.data.stm.modes & 1) != 0); /* Sync mode */
        crecho = -((fg_msg.ctl.data.stm.modes & 2) != 0);   /* CRECHO mode */
        lfecho = -((fg_msg.ctl.data.stm.modes & 4) != 0);   /* LFECHO mode */


    }  /* SET BREAK TABLE */

    else if (strcmp (msg_id, "SBT") == 0) { 
        strcpy (brk_table, fg_msg.ctl.data.break_table);

    }  /* ENTER SYNC MODE */

    else if (strcmp (msg_id, "ESM") == 0) { 
        strcpy (brk_table, fg_msg.ctl.data.break_table);
        sync = ON;
        kb.cndx = 0;
        kb.endx = 0;
        setmem (kb.pos, sizeof (kb.pos), NUL);
        setmem (kb.klin, sizeof (kb.klin), NUL);

        puttdata (FG_CONTROL_MESSAGE, "SME", 3);
    }  /* EXIT SYNC MODE */

    else if (strcmp (msg_id, "XSM") == 0) { 

        sync = OFF;
        kb.echo = ON;
        kb.cndx = 0;
        kb.endx = 0;
        setmem (kb.klin, sizeof (kb.klin), NUL);

        puttdata (FG_CONTROL_MESSAGE, "SMX", 3);
        clear_screen ();

    }  /* READ ... */

    else if (strcmp (msg_id, "RNE") == 0 || strcmp (msg_id, "RWE") ==
        0) {

        /* Terminate any active read */

        if (read_active) { 
            term_read = ON;
            if (kb.cndx > 0)
                extract_msg (~NO_BLOCK, ~TRO, TXMT_MSG,
                     ~HIDE);

            else
                send_msg (nul_str, 0);
        }

        read_count =  HI_BYTE_VALUE * fg_msg.ctl.data.rd_ct [0] +
            fg_msg.ctl.data.rd_ct [1];
        no_blk_sw = -(fg_msg.ctl.hdr.msb_size && NO_BLOCK_MASK);

        /* Set up the new read */

        read_active = ON;
        kb.echo = -(strcmp (msg_id, "RWE") == 0);

        /* Terminate again for a no-block read */

        if (no_blk_sw) { 
            term_read = ON;
            if (kb.cndx > 0)        /* Send typeahead */
                extract_msg (~NO_BLOCK, ~TRO, TXMT_MSG,
                     ~HIDE);

            else
                send_msg (nul_str, 0);
        }
    }  else /* Unsupported control order */  {

    }
}               /* End of ctrl_msg */


/* End of WSTFRGND */
