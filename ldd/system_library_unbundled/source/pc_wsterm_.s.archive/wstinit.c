/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1987 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(87-03-21,Wallman), approve(87-03-21,MCR7586),
     audit(87-07-13,Flegel), install(87-08-07,MR12.1-1072):
     First release.
  2) change(88-06-06,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Return error code instead of exiting when MOWSE not yet invoked
     for cleanup by main().
  3) change(88-07-12,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Replaced calls to redundant move_abs() with calls to cursor_move().
  4) change(88-07-20,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Made edit mode default after atm.
  5) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation only, added header comment.
  6) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  7) change(88-08-23,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added WSTERM edit mode and other mode initialization routines.
  8) change(88-08-30,Lee), approve(88-09-12,MCR7986), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed non-edit async mode and references to non-edit async mode
     line editing routines.
  9) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     phx21233 - Modified to initialize default glass_edit_mode value;
     separated input and output parameters to int86() to avoid confusion
     when parameters are used.
                                                   END HISTORY COMMENTS */

/* WSTINIT - Initialization module for WSTERM */

/* This module prepares the execution evironment for a WSTERM session */

#include    <stdio.h>
#include    <dos.h>
#include    <alloc.h>
#include    <ws_error.h>
#include    <wsmincap.h>
#include    "wstdefs.h"
#include    "wstglob.h"

/*
**********************************************************************

  Routine:            WSTINIT 

  Function:
      This routine initializes the screen and the execution 
  environment variables which are used by WSTERM. 

  Parameters:         NONE 

  Returns:            -1 if an error occurs during initialization; in 
                          particular, if MOWSE has not been invoked 
                          on the PC 
                      0 if no errors 

**********************************************************************/

wstinit ()
{   

    /* Clear the screen and display version id */

    clear_screen ();
    cursor_move (CURSOR_HOME_ROW, 60);
    printf ("WSTERM Vers %s", version);
    cursor_move (CURSOR_HOME_ROW, CURSOR_HOME_COL);

    /* Initialize static structures with defaults */

    setmem (&screen, sizeof (screen), 0);
    screen.maxcol = MAX_SCREEN_COL;
    screen.maxlin = MAX_SCREEN_LINE;

    setmem (&kb, sizeof (kb), 0);
    kb.key_status = -1;
    setmem (&ds, sizeof (ds), 0);
    setmem (&tdata_arg, sizeof (tdata_arg), 0);

    tdata_arg.local_buffer_pointer = (char *) fg_msg.text;
    tdata_arg.local_buffer_size = FG_MSG_SIZE;

    setmem (&fg_msg, sizeof (fg_msg), 0);
    setmem (brk_table, sizeof (brk_table), 0);

    /* Tell mowse we're here */
    mowse_active = cretinst ("WSTERM",  /* capability name */
        NULL,                           /* NULL entry function */
        0,                              /* zero inbuff length */
        0,                              /* zero outbuff length */
        NULL,                           /* NULL data block ptr */
        &wst_mcb);                      /* MCB pointer */

    if (mowse_active == WSNOTRES) { 
        printf ("MOWSE has not been invoked.\n");
        return(-1);
    }

    /****************  WSNOTPKT goes here */

    mowse_active = -(mowse_active == 0);

    /* is mowse active? */
    if (~mowse_active) {

        kb.echo = OFF;
        glass_edit_mode = FALSE;    /* phx21233 R.L. - initialize mode flags */
        wst_edit_mode = FALSE;

    }
    else  {
        kb.echo = ON;

        /* edit mode is always enabled in async packet mode */
        wst_edit_mode = TRUE;
    }

    return(0);
}               /* End of wstinit */



/*
**********************************************************************

  Routine:            WST_INIT_MODES

  Function:
      This routine initializes structures and variables associated
  with WSTERM's various modes.

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

wst_init_modes()
{
    int attr;

    wst_paging = ON;                 /* display EOP after screenful */
    wst_freeze = OFF;                /* allow scrolling */
    wst_f_audit = OFF;               /* file audit off */
    wst_p_audit = OFF;               /* line printer audit off */
    wst_edit_mode = OFF;             /* disable line edit mode */
    break_flag = FALSE;              /* no break hit */
    wst_status = WST_UNKNOWN_MODE;   /* mode to be determined */
    clr_index = 0;                   /* not currently in escape sequence */
    cursor_type = CURSOR_UNDERSCORE; /* assume underscore cursor */

    wst_fansi_flag = FALSE;          /* assume FANSI-CONSOLE not used */

    init_fconsole();       /* set scroll region and wst_fansi_flag */
                           /*   if device driver is FANSI-CONSOLE  */

    printf(ED);            /* (EraseDisplay) clear screen and home cursor */
    getattr(&attr);        /* get character attributes */
    wst_screens_init(' ',attr);  /* initialize the screen variables */
    init_wst_scroll(attr);   /* initialize structure for scrolling display */
    p_aud_init();
    f_aud_init();
    init_histbuff();
    init_killbuff();
    save_printer_timeout();
    set_printer_timeout(1);
}



/*
**********************************************************************

  Routine:            GETATTR 

  Function:
      This routine gets the current screen attributes. The screen 
  attributes read can then be used to determine what color and 
  intensity is to be used for subsequent displaying. 

  Parameters:
     (output)         attr - pointer to the integer variable for 
                          passing back the current attributes 

  Returns:            NONE 

**********************************************************************/

getattr(attr)
int *attr;
{
    union REGS reg, outreg;

    cursor_move(CURSOR_HOME_ROW,CURSOR_HOME_COL);    /* home cursor */
    reg.h.ah = RD_SCREEN;           /* read character and attributes */
    reg.h.bh = get_active_page();   /* from current active page */
    int86(BIOS_VIDEO,&reg,&outreg); /* call BIOS to get character attributes */
    *attr = (int)outreg.h.ah;    /* set attribute parameter with attributes */
}



/*
**********************************************************************

  Routine:            INIT_FCONSOLE

  Function:
      This routine determines if FANSI-CONSOLE is being used as the 
  screen driver. If so, the global flag "wst_fansi_flag" is set and 
  an escape sequence is sent to initialize the first 24 lines of the 
  screen as the scrolling region. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

init_fconsole()
{
    int fd;

    /* if device driver for FANSI-CONSOLE exists */
    if ((fd = open("fcon",0)) >= 0) {
        close(fd);           /* we don't need file descriptor any more */

        wst_fansi_flag = TRUE;  /* set to indicate fansi-console installed */

        /* print sequence to set the scroll region from lines 1 to 24 */
        put_screen_str(FCON_SET_SCROLL_24);
    }
}



/*
**********************************************************************

  Routine:            RESET_SCROLL 

  Function:
      This routine checks to see if the global flag "wst_fansi_flag" 
  has been set and if so, will send an escape sequence to the screen 
  driver to reset the scrolling region. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

reset_scroll()
{
    /* if fansi console installed */
    if (wst_fansi_flag)
        /* print the escape sequence to set the scroll
           region to the default (the entire screen)
        */
        put_screen_str(FCON_RESET_SCROLL_STR);
}


/* End of WSTINIT */

