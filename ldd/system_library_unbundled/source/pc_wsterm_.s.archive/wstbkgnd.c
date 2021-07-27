/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(87-05-04,Wallman), approve(87-05-04,MCR7586),
     audit(87-07-16,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(88-04-28,Lee), approve(88-05-16,MCR7897),
     audit(88-09-19,Flegel), install(88-10-12,MR12.2-1160):
     Created. The previous WSTBKGND.C has been rewritten to
     handle background messages using a background screen
     instead of a 1 line minibuffer.
  3) change(88-05-31,Lee), approve(88-05-16,MCR7897),
     audit(88-09-19,Flegel), install(88-10-12,MR12.2-1160):
     Added file and printer auditing support.
  4) change(88-07-25,Lee), approve(88-05-16,MCR7897),
     audit(88-09-19,Flegel), install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  5) change(88-08-09,Lee), approve(88-05-16,MCR7897),
     audit(88-09-19,Flegel), install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
                                                   END HISTORY COMMENTS */

/* WSTBKGND - handles processing of background messages using a
              background screen. Replaces previous modules which
              used 25th line to display background messages.
*/

#include    <stdio.h>
#include    <dos.h>
#include    <ws.h>
#include    <ws_mcb.h>
#include    <ws_error.h>
#include    "wstdefs.h"
#include    "wstglob.h"


/*
**********************************************************************

  Routine:            SHOW_BKMSG 

  Function:
      This routine signals the arrival of a background message 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

show_bkmsg ()
{
    signal_bg(ON);
    bk_msg_showing = ON;
}				/* End of show_bkmsg */



/*
**********************************************************************

  Routine:            DISPLAY_BKMSG 

  Function:
      This routine saves the foreground screen and enters the 
  background screen. While in the background screen, the user may 
  display the next background message, quit, invoke the help screen 
  or invoke the background polling screen. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

display_bkmsg ()
{   
    char    message [MAX_BG_MESS_LENGTH+1];  /* A background message */
    int     msg_type,       /* WSINFO or WSQUERY */
            sender;         /* Sender's majcap */
    int     bg_msg_pending;          /* count of pending background messages */
    char    statline[SCREEN_COLS+1]; /* string to display in status line */
    int     ch;             /* the key hit when waiting for input */
    char    *msg_ptr;

    /* get number of background messages pending */
    bg_msg_pending = tdata_arg.background_pending_flag;

    /* turn background messages off for now, will be reset in wsterm main */
    bk_msg_showing = OFF;

    /* save foreground screen and swap to background screen */
    save_wst_screen(&wst_fg_screen);
    restore_wst_screen(&wst_bg_screen);

    /* Loop here until getbkmes says there aren't any. */
    while (ON) {

        /* if background messages in queue */
        if (bg_msg_pending > 0) {

            /* display number of background messages in queue on status line */
            sprintf(statline,"Background Screen: %d message(s). <ALT-H>-help  [<q>,<p>,<ALT-M>,<ALT-D>]",
                bg_msg_pending);
            status_line(statline);

            /* hide cursor */
            cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

            /* wait for user to hit a valid key */
            while (TRUE) {
                ch = getkey(BLOCK);

                /* if key to quit hit */
                if (uppercase(ch) == 'Q') {
                    bg_msg_pending = 0;
                    cursor_move(wst_bg_screen.cursor_row,
                        wst_bg_screen.cursor_col);
                    exit_bg_screen();
                    return;
                }

                /* if key to display next background message */
                else if (ch == ALT_M || ch == ALT_D)
                    break;

                /* if key to enter poll loop */
                else if (uppercase(ch) == 'P') {
                    poll_bg_msg();
                    cursor_move(wst_bg_screen.cursor_row,
                        wst_bg_screen.cursor_col);                    
                    exit_bg_screen();
                    return;
                }

                /* enter help screen, giving background screen info */
                else if (ch == ALT_H) {
                    help(BG_HELP);

                    /* restore status line message when done */
                    status_line(statline);
                }

                else
                    beep();
            }

            /* get and display the next background message */
            if (getbgmes (message, &msg_type, &sender) == WSNOMESS) {
                /* if couldn't get it, continue at top of loop to */
                /* exit gracefully */
                bg_msg_pending = 0;
                continue;
            }

            /* tally background message and display it */
            bg_msg_pending--;
            msg_ptr = message;

            cursor_move(wst_bg_screen.cursor_row,
                wst_bg_screen.cursor_col);
            
            while (*msg_ptr)
                put_wst_screen(&wst_bg_screen,*msg_ptr++);
            put_wst_screen(&wst_bg_screen,'\n');

            /* hide cursor */
            cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

            /* perform auditing if auditing enabled */
            if (wst_f_audit) {
                f_audit_msg("==BG==> ",8);
                f_audit_msg(message,strlen(message));
                f_audit_msg("\n",1);
            }

            if (wst_p_audit) {
                p_audit_msg("==BG==> ",8,statline);
                p_audit_msg(message,strlen(message),statline);
                p_audit_msg("\n",1,statline);
            }

            /* If this is a query, contruct and send the reply */
            if (msg_type == WSQUERY) {
                /* prompt user for input on status line */
                strcpy(statline,"Background Screen: Enter reply     <ALT-H>-help");
                status_line(statline);

                /* restore cursor */
                cursor_move(wst_bg_screen.cursor_row,
                    wst_bg_screen.cursor_col);

                /* read message from keyboard */
                wst_getline(&wst_bg_screen,message,statline);

                /* hide cursor */
                cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

                /* send reply to sender of background message */
                sendqrep (message, sender);

                /* perform auditing if auditing enabled */
                if (wst_f_audit) {
                    f_audit_msg("==BGR=> ",8);
                    f_audit_msg(message,strlen(message));
                    f_audit_msg("\n",1);
                }

                if (wst_p_audit) {
                    p_audit_msg("==BGR=> ",8,statline);
                    p_audit_msg(message,strlen(message),statline);
                    p_audit_msg("\n",1,statline);
                }

            }
        }

        /* no messages */
        else {
            strcpy(statline,"Background Screen: No more messages. <ALT-H>-help  [<p>,<q>]");
            status_line(statline);

            /* hide cursor */
            cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

            /* allow user to enter poll loop or to quit */
            while (TRUE) {
                ch = getkey(BLOCK);
                if (ch == ALT_H) {
                    help(BG_HELP);
                    status_line(statline);
                }
                else if (uppercase(ch) == 'P' || uppercase(ch) == 'Q')
                    break;
                else
                    beep();
            }

            /* background polling requested? */
            if (uppercase(ch) == 'P')
                poll_bg_msg();

            cursor_move(wst_bg_screen.cursor_row,
                wst_bg_screen.cursor_col);

            exit_bg_screen();

            return;
        }
    }
}



/*
**********************************************************************

  Routine:            EXIT_BG_SCREEN 

  Function:
      This routine saves the contents of the background screen and 
  restores the contents of the foreground screen.

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

exit_bg_screen()
{
    save_wst_screen(&wst_bg_screen);
    restore_wst_screen(&wst_fg_screen);
    update_status();
}



/*
**********************************************************************

  Routine:            POLL_BG_MSG 

  Function:
      This routine puts the user into the background polling screen. 
  Any background messages are immediately displayed upon arrival. 
  Keyboard input allows the user to quit the background screen, 
  invoke the help screen or toggle between scroll and no scroll. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

poll_bg_msg()
{
    int msg_type;        /* holds background message type */
    int sender;          /* holds sender of background message */
    char message[MAX_BG_MESS_LENGTH+1]; /* holds background message */
    int scroll;          /* scroll switching to control local scrolling */
    int ch;              /* holds key hit */
    char *bgp_stat_ns;   /* points to status line message in no-scroll */
    char *bgp_stat_s;    /* points to status line message in scroll */
    char *bgp_stat_q;    /* points to status line message replying to query */
    char *msg_ptr;       /* scratch message ptr */

    scroll = 1;
    bgp_stat_ns = "Background Screen(Poll): [NO-SCROLL] <ALT-H>-help  [<q>,<other>]";
    bgp_stat_s = "Background Screen(Poll):             <ALT-H>-help  [<q>,<other>]";
    bgp_stat_q = "Background Screen(Poll): Enter reply     <ALT-H>-help";

    /* update the status line */
    status_line(bgp_stat_s);

    /* poll until the user hits 'q' to quit */
    while (TRUE) {
        if (scroll && getbgmes (message, &msg_type, &sender) != WSNOMESS) {

            /* restore cursor */
            cursor_move(wst_bg_screen.cursor_row,
                wst_bg_screen.cursor_col);

            /* display the background message */
            msg_ptr = message;
            while (*msg_ptr)
                put_wst_screen(&wst_bg_screen,*msg_ptr++);
            put_wst_screen(&wst_bg_screen,'\n');

            /* perform auditing if enabled */
            if (wst_f_audit) {
                f_audit_msg("==BG==> ",8);
                f_audit_msg(message,strlen(message));
                f_audit_msg("\n",1);
            }

            if (wst_p_audit) {
                p_audit_msg("==BG==> ",8,bgp_stat_s);
                p_audit_msg(message,strlen(message),bgp_stat_s);
                p_audit_msg("\n",1,bgp_stat_s);
            }

            /* hide cursor */
            cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

            /* If this is a query, contruct and send the reply */
            if (msg_type == WSQUERY) {

                /* restore cursor position */
                cursor_move(wst_bg_screen.cursor_row,
                    wst_bg_screen.cursor_col);

                /* update status line to indicate input state */
                status_line(bgp_stat_q);

                /* get the user input (reply) */
                wst_getline(&wst_bg_screen,message,bgp_stat_q);

                cursor_pos(&wst_bg_screen.cursor_row,
                    &wst_bg_screen.cursor_col);

                /* hide cursor */
                cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

                /* send reply message back to sender of background message */
                sendqrep (message, sender);

                /* audit if auditing enabled */
                if (wst_f_audit) {
                    f_audit_msg("==BGR=> ",8);
                    f_audit_msg(message,strlen(message));
                    f_audit_msg("\n",1);
                }

                if (wst_p_audit) {
                    p_audit_msg("==BGR=> ",8,bgp_stat_q);
                    p_audit_msg(message,strlen(message),bgp_stat_q);
                    p_audit_msg("\n",1,bgp_stat_q);
                }

                /* update the status line to indicate polling state */
                status_line(bgp_stat_s);
            }
        }

        /* if a key has been hit */
        if (checkkey() >= 0) {

            /* get the key */
            ch = getkey(BLOCK);

            /* check if key hit was 'q' to quit */
            if (uppercase(ch) == 'Q')
                return;

            /* check if help requested */
            else if (ch == ALT_H) {
                help(BG_HELP);
                if (scroll)
                    status_line(bgp_stat_s);
                else
                    status_line(bgp_stat_ns);
            }

            /* any other key hit toggles scroll/no_scroll */
            else {

                /* toggle scroll value and update the status line */
                scroll = !scroll;
                if (scroll)
                    status_line(bgp_stat_s);
                else
                    status_line(bgp_stat_ns);
            }
        }
    }
}

