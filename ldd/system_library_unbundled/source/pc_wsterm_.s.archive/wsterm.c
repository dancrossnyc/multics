/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1987 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(87-03-08,Wallman), approve(87-03-08,MCR7586),
     audit(87-07-16,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables, formatting.
  3) change(88-04-11,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added calls to handled BREAK key; mapped DEL key to ASCII DEL,
     mapped ^@ (or ^2) to ASCII NUL.
  4) change(88-04-26,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Created routine check_wst_status() to update the status when
     WSTERM changes its mode (glass tty, sync or async).
  5) change(88-04-27,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added calls to scroll_1_to_24() to prevent the status line from
     being overwritten when using FANSI-CONSOLE.
  6) change(88-05-12,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added command line handling for /B option.
  7) change(88-05-31,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added command line handling for /A, /P; support for file and
     printer auditing.
  8) change(88-07-20,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Default after atm changed edit mode.
  9) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
 10) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
 11) change(88-08-30,Lee), approve(88-09-12,MCR7986), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed non-edit async mode and references to non-edit async mode
     line editing routines.
 12) change(89-02-22,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Separated input and output parameters to int86() to avoid confusion
     when parameters are used; enabled kb.echo flag when mowse is not active
     and in edit-mode.
                                                   END HISTORY COMMENTS */

/* WSTERM -- WorkStation TERminal Manager for Multics MOWSE */


#include  <stdio.h>
#include  <dos.h>
#include  <ws.h>
#include  <ws_error.h>
#include  <wsmincap.h>
#include  <cat.h>
#include  "wstdefs.h"
#include  "wstglob.h"

/****************************************************************************/

/* Program entrypoint */

/*
**********************************************************************

  Routine:            MAIN 

  Function:
      This is the command module for WSTERM. It analyzes command line 
  arguments, setting appropriate flags and initializes the execution 
  environment. It then enters a listener loops that continually 
  checks and handles foreground messages, background messages, and 
  keyboard input. It stays in the listener loop until the user enters 
  a local escape sequence that signals that the program should 
  terminate. While inside the listener loop, various modes and 
  facilities may be called. 

  Parameters:
     (input)          argc - indicates the number of command line 
                          arguments; standard argc argument passed to 
                          main() 
     (input)          argv - pointer to the character pointer array 
                          containing the actual command line 
                          arguments; standard argv argument passed to 
                          main() 

  Returns:            NONE 

**********************************************************************/

main (argc,argv)
int argc;
char *argv[];
{   
    /* handle command line arguments */
    if (parse_args(argc,argv) < 0)
        exit(0);

    /* if /B option specified to send break to host */
    if (break_sends_to_host)
        set_break();   /* set up our ^BREAK handler */
    else
        enable_ctl_c_check();   /* turn ^C/BREAK checking ON */

    /* initialize screen structures for screen swapping */
    wst_init_modes();

    init_kb_screen(SCREEN_LINES,SCREEN_COLS);
    init_line(&edlin);

    /* initialize wsterm structures and variables */
    if (wstinit () < 0) {
        /* MOWSE not invoked error, clean up and exit */

        /* restore scrolling region to full screen */
        reset_scroll();

        if (break_sends_to_host)
            reset_break();   /* restore original ^BREAK handler */
        else
            restore_ctl_c_check();  /* restore original ^C/BREAK checking */
        exit(0);
    }


    /* initialize wsterm's line mode flag to update on next check */
    wst_status = WST_UNKNOWN_MODE;

    /* Listener loop -- control stays in this loop until the user
       enters ALT-Q to quit the invocation. */

    while (local_action != QUIT) { 

        /* check WSTERM's status and update status line if necessary */
        check_wst_status();

        local_action = NUL;     /* Erase possible leftovers */

        /* If there is no pending foreground message, see if
           one is waiting in MOWSE */

        if (fg_msg_len == 0) { 
            fg_msg_len = gettdata (&tdata_arg);

            /* If MOWSE doesnt have a message either, check MOWSE's status */
            if (fg_msg_len == 0)
                check_MOWSE_status ();
        }

        /* Is there a foreground message? */
        if (fg_msg_len > 0) {

            /* If FB_BREAK has been sent and this is the return FG_BREAK */
            if (break_sent && tdata_arg.minor_capability == FG_BREAK) {

                break_sent = OFF;
                fg_msg_len = 0;     /* Discard the FG_BREAK message */

                if (read_active) {     /* Terminate any active read */
                    term_read = ON;
                    send_msg (nul_str, 0);
                }
            }
            else if (~sync)      /* An async message */
                async_msg (~FORCE);
            else 
                sync_msg ();    /* A sync message */
        }

        /* If mowse is active ... */
        if (mowse_active) {

            /* Is there a background message that hasn't been
               shown in line 25? */

            if (tdata_arg.background_pending_flag && ~bk_msg_showing)
                show_bkmsg ();

            /* If we're in sync mode with characters in the
               keyboard buffer and there is an active read command,
               check for echo delay timer runout */

            if (sync && read_active) { 
                if (kb.cndx > 0 && check_echo_delay ()) { 
                    if ((kb.echo && kb.cndx > kb.endx) ||
                        ~kb.echo)
                        extract_msg (~NO_BLOCK,
                             TRO, ~kb.echo, ~HIDE);
                }
            }
        }

        /* Anything from the keyboard? */

        if (!wst_edit_mode || sync) {
            if (read_keyboard (~BLOCK))
                process_kbrd_char (~HIDE);
        }
        else
            process_edit_line(&edlin);
    }               /* End of listener loop */

    /* Bid adieu to MOWSE and return to DOS */

    destinst (&wst_mcb);

    /* restore scrolling region to full screen */
    reset_scroll();

    /* move cursor to bottom of screen and display a new line */
    /* so that the DOS prompt will appear at the bottom of the screen */
    leave_screen();

    set_printer_timeout(wst_prt_timeout);

    /* if /B specified to send break to host */
    if (break_sends_to_host)
        reset_break();   /* restore original ^BREAK handler */

    else
        restore_ctl_c_check();  /* restore original ^C/BREAK checking */

    if (wst_f_audit)
        end_file_audit();

    if (wst_p_audit)
        end_printer_audit();

    exit ();

}               /* End of main */


/**** LOCAL FUNCTIONS *********************************************************/


/*
**********************************************************************

  Routine:            CHECK_ECHO_DELAY 

  Function:
      This function checks for echo delay timer runout. The echo 
  delay timer (currently set at 1 second) controls the echoing of 
  typeahead so that the user doesn't get too far ahead of the screen 
  display. 

  Parameters:         NONE 

  Returns:            ON if timer runout occurred 
                      OFF is no timer runout occurred 

**********************************************************************/

check_echo_delay ()
{   
    int newtime;        /* Current clock */
    union regs_struct regs, outregs;

    /* Record current DOS system time in 'newtime' */

    regs.hreg.ah = SYSTEM_TIME;
    intdos (&regs, &outregs);
    newtime = regs.hreg.dh & LO_BYTE_MASK; /* Use only seconds value */

    if (oldtime == -1)      /* Also set oldtime for first call */
        oldtime = newtime;
    if (newtime < oldtime)      /* Adjust for clock rollover */
        oldtime -= 60;

    /* Check for timer runout */

    if (newtime - oldtime >= echo_delay) {

        oldtime = newtime;      /* Timer runout - record newtime */
        return (ON);            /* Return true */
    }

    else 
        return (OFF);       /* No timer runout - return false */
}               /* End of check_echo_delay */



/*
**********************************************************************

  Routine:            CHECK_MOWSE_STATUS 

  Function:
      This routine checks MOWSE's status flag with WSTERM's static 
  switch to determine if there has been a change in MOWSE's status 
  (either attached or detached). If MOWSE's status has changed to 
  attached, a WSTERM capability is created; if MOWSE's status has 
  changed to detached, the WSTERM capability is destroyed; otherwise 
  there is no change in status and nothing happens. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

check_MOWSE_status ()
{

    /* If WSTERM's static switch says MOWSE is active but Multics MOWSE is
   detached, free WSTERM's MCB and set various static switches */

    if (mowse_active && tdata_arg.minor_capability == DETACHED) { 

        destinst(&wst_mcb);

        mowse_active = OFF;

        /* in glass tty mode, use edit mode only if the default */
        /* for glass tty mode is edit mode */
        wst_edit_mode = glass_edit_mode;

        /* RL: phx21233 - always echo in edit mode when MOWSE is not */
        /* attached since no mowse_io_ to control printer on/off */
        if (wst_edit_mode)
            kb.echo = ON;

        /* RL: phx21233 - OFF in non-edit mode to prevent double echoing */
        else
            kb.echo = OFF;

        /* Send any typeahead */

        if (kb.cndx > 0) {

            puttdata (FG_TERMINAL_DATA, kb.klin, strlen (kb.klin));

            kb.cndx = 0;            /* Wipe keyboard buffer */
            kb.endx = 0;
            kb.klin [0] = NUL;

            ds.ccol = 0;            /* Wipe keyboard display */
            ds.lct = 0;
            ds.lndx = 0;
            ds.dlin [0] = NUL;
            ds.pstrl = 0;
            ds.pstr [0] = NUL;
            screen.EOP_ct = 0;
            ds.splct [0] = NUL;
        }
    }

    /* If WSTERM's static switch says MOWSE is not active but
       Multics MOWSE is attached, tell MOWSE to put WSTERM into
       the CAT and set static switches */

    else if (~mowse_active && tdata_arg.minor_capability == ATTACHED) { 

        cretinst ("WSTERM",   /* capability name */
            NULL,             /* NULL entry function */
            0,                /* zero inbuff length */
            0,                /* zero outbuff length */
            NULL,             /* NULL data ptr */
            &wst_mcb);        /* MCB ptr */

        mowse_active = ON;
        kb.echo = ON;

        /* mowse now active, determine mode for editing; */
        /* use edit mode only if default for async is edit mode */
        wst_edit_mode = TRUE;
        crecho = OFF;
        lfecho = OFF;

    }
}               /* End of check_MOWSE_status */



/*
**********************************************************************

  Routine:            CHECK_WST_STATUS 

  Function:
      This routine checks WSTERM's MOWSE status flag against the 
  global flag 'wst_status' (which indicates the status that is 
  displayed on the status line). If a change is detected, the 
  wst_status flag is updated and the new status is displayed in the 
  status line. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

check_wst_status()
{
    int status;

    status = WST_UNKNOWN_MODE;

    /* determine the current mode */
    if (~mowse_active) status = WST_GLASS_TTY_MODE;
    else if (sync) status = WST_SYNC_MODE;
    else status = WST_ASYNC_MODE;

    /* if status is the same since last checked, do nothing */
    if (wst_status == status) return;

    /* update the status flag for the next check */
    wst_status = status;

    /* update the new status on the status line */
    update_status();
}



/*
**********************************************************************

  Routine:            PARSE_ARGS 

  Function:
      This routine parses WSTERM's command line arguments, setting 
  appropriate flags and variables if the command line arguments are 
  recognized. Default values are assigned to flags and variables not 
  affected by the command line arguments. The following table 
  indicates the flag/variables affected by their corresponding 
  control argument specifier: 

  global flag         default value   control argument   new value
  -----------         -------------   ----------------   ---------
  break_sent_to_host  OFF             /B                 ON
  audit_file_name     "wsterm.log"    /A                 <user file name>
  wst_printer_card    0               /P                 0-2

  Parameters:
     (input)          argc - the argc value passed to main() 
     (input)          argv - the argv value passed to main 

  Returns:            -1 if a parsing error occurred 
                      0 if no error occurred 

  Note 1 - If a parsing error is encountered, an error message is 
      displayed by this module. 
**********************************************************************/

parse_args(argc,argv)
int argc;
char *argv[];
{
    int i;
    char tmp[133]; /* trash variable for trapping invalid input */

    /* initialize with default values */
    break_sends_to_host = OFF;
    audit_file_name = DEFAULT_AUDIT_FILE;
    wst_printer_card = DEFAULT_PRINTER_CARD;

    /* loop through each argument from left to right on command line */
    for (i = 1; i < argc; i++) {

        /* check for /B */
        if (!strcmp(argv[i],"/B") || !strcmp(argv[i],"/b"))
            break_sends_to_host = ON;

        /* check for /A */
        else if (!strcmp(argv[i],"/A") || !strcmp(argv[i],"/a")) {

            i++;
            if (argc <= i || argv[i][0] == '/') {
                printf("Missing audit file name specifier for /A argument\n");
                return(-1);
            }
            audit_file_name = argv[i];
            
        }

        /* check for /P */
        else if (!strcmp(argv[i],"/P") || !strcmp(argv[i],"/p")) {

            i++;
            if (argc <= i || argv[i][0] == '/') {
                printf("Missing printer card number (0-2) for /P argument\n");
                return(-1);
            }
            if (sscanf(argv[i],"%d%s",&wst_printer_card,tmp) != 1 ||
                wst_printer_card < MIN_PRINTER_CARD_VAL ||
                wst_printer_card > MAX_PRINTER_CARD_VAL) {
                printf("Invalid printer card value; must be 0, 1 or 2.\n");
                wst_printer_card = DEFAULT_PRINTER_CARD;
                return(-1);
            }
        }

        /* not a recognized command line argument */
        else {
            printf("Invalid WSTERM argument: %s\n", argv[i]);
            return(-1);
        }
    }
    return(0);
}

    

/*
**********************************************************************

  Routine:            LEAVE_SCREEN 

  Function:
      This routine is called before exiting WSTERM to leave most of 
  WSTERM's screen on the display upon exiting. This is done by 
  scrolling the screen up one line and positioning the cursor at the 
  bottom left hand corner of the screen. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

leave_screen()
{
    union REGS reg, outreg;

    /* scroll screen up a line */
    reg.h.ah = VIDEO_SCROLL_UP_FUNC;
    reg.h.al = DEFAULT_LINES_TO_SCROLL;
    reg.h.ch = CURSOR_HOME_ROW;
    reg.h.cl = CURSOR_HOME_COL;
    reg.h.dh = SCREEN_LINES;
    reg.h.dl = SCREEN_COLS-1;
    reg.h.bh = wst_fg_screen.attr;
    int86(BIOS_VIDEO,&reg,&outreg);

    /* move cursor to bottom line of screen, column 0 */
    cursor_move(SCREEN_LINES,0);
}


/* End of WSTERM */
