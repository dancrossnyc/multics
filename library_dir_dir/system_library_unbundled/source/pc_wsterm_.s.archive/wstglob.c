/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1988                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1987 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(87-03-13,Wallman), approve(87-03-13,MCR7586),
     audit(87-08-10,Flegel), install(87-08-07,MR12.1-1072):
     First release
  2) change(87-08-14,Wallman), approve(87-08-14,MCR7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     Change version to 1.01 for PBF to wstutil.
  3) change(87-09-02,Wallman), approve(87-09-02,MCR7586),
     audit(87-08-17,Flegel), install(87-09-10,MR12.1-1103):
     Change version to 1.02 for PBFs to wstutil, wstkbscr, wstedit, and
     wstfrgnd.
  4) change(87-09-17,Wallman), approve(87-09-17,PBF7586),
     audit(87-09-17,LJAdams), install(87-09-18,MR12.1-1109):
     Changed version to 1.03 for PBF to wstutil.
  5) change(88-02-24,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Removed debugging code and unused variables, re-formatting.
  6) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  7) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  8) change(88-08-23,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added declarations for global variables for edit mode and
     WSTERM's various other modes.
  9) change(89-01-18,Lee), approve(89-01-18,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Updated version number to 1.08.
                                                   END HISTORY COMMENTS */


/* WSTGLOB - GLobal static data for WSTERM */

/* This module declares storage for all the global static variables used
   by WSTERM. The include file wstglob.h provides descriptions of this
   storage to the active modules. */

#include    <stdio.h>
#include    <dos.h>
#include    <ws.h>
#include    <ws_mcb.h>
#include    <ws_dcls.h>
#include    "wstdefs.h"

char    brk_table [96],         /* Break table characters */
        bs_str [2] = "\010",    /* ASCII BS as a string */
        char_erase = '#',       /* Character erase */
        erkl_chars [4],         /* Local edit chars as a string */
        F1_10 [10][4] = {       /* Normal function keys */
            "0", "2", "6", "8", ":",
            "<", ">", "P", "R", "T"  },
        F11_12 [2][4] = {       /* Shifted function keys */
            "1", "5" },

        line_kill = '@',        /* Line kill */
        lnc = '\\',             /* Literal escape character */
        local_action = NUL,     /* Local escape action char */
        nl [3] = "\015\012",    /* Multics NL as a string */
        nul_str [2] = "",       /* A null string */
        octstr [6] = "",        /* Display string for octval */
        spaces [11] = "          ",     /* Spaces for tabbing */
        version [6] = "1.08";   /* WSTERM version */

        int bk_msg_showing = OFF,   /* Message display control */
        break_sent = OFF,       /* An FG_BREAK is outstanding */
        crecho = OFF,           /* CRECHO mode switch */
        echo_delay = 1,         /* Max allowable delay for sync echo */
        fg_msg_len = 0,         /* Length of a foreground message */
        fg_msg_showing = OFF,   /* Message display control */
        lfecho = OFF,           /* LFECHO mode switch */
        mowse_active,           /* Switch showing MOWSE health */
        octsw = OFF,            /* Octal escape switch */
        octval = 0,             /* Octal escape value */
        oldtime = -1,           /* Previous clock */
        read_active = OFF,      /* Sync mode read is active */
        read_count = 0,         /* Number characters able to send */
        sync = OFF,             /* Sync (video) mode flag */
        term_read = OFF;        /* Terminate a read command */

mcb  * wst_mcb = NULL;

struct ds_struct ds;        /* Keyboard display data */

union fg_msg_struct fg_msg; /* Foreground (incoming) messages */

struct get_struc tdata_arg; /* Gettdata argument */

struct kb_struct kb;        /* Keyboard input data */

struct screen_struct screen;    /* Screen bookkeeping */



/*****
 variables used to maintain WSTERM's various modes
******/

/* set by our ^BREAK handler to indicate ^BREAK was hit */
int break_flag;

/* flag indicates WSTERM's displayed status (glass,sync,async) */
int wst_status;

/* use while filtering out escape codes */
int clr_index;

/* indicates if FANSI-CONSOLE is being used */
int wst_fansi_flag;

/* indicates if /B was specified for using ^BREAK to send a break */
int break_sends_to_host;

/* indicates if WSTERM should display EOP after a screenful */
int wst_paging;

/* indicates if WSTERM should freeze display (no scroll) */
int wst_freeze;

/* indicates if WSTERM should audit to file */
int wst_f_audit;

/* indicates if WSTERM should audit to line printer */
int wst_p_audit;

/* pointer to string containing audit file name */
char *audit_file_name;

/* specifies printer card (0-2) to use */
int wst_printer_card;

int wst_edit_mode;
int glass_edit_mode;

/* This table is used to determine whether a particular character
   is valid following an escape character. This restricts the
   number of invalid escape sequences from getting through which
   may possibly kill the screen driver.
*/
char is_esc[] = {
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 

     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 1, 
     1, 0, 0, 0, 1, 1, 1, 0, 

     0, 0, 0, 0, 1, 1, 0, 0, 
     1, 0, 1, 0, 0, 1, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 1, 0, 1, 1, 1, 1, 

     0, 0, 1, 1, 0, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 1, 1, 
     0, 0, 0, 0, 0, 0, 0, 0, 
     0, 0, 0, 0, 1, 1, 1, 0
};


/* Screen buffers for scratch, background, foreground and help screens */
/* These structures are used to save the contents of a particular
   screen and the cursor position before switching to another screen.
*/
SCREEN wst_tmp_screen;
SCREEN wst_bg_screen;
SCREEN wst_fg_screen;
SCREEN wst_help_screen;

/* edit mode line and line information */
EDIT_LINE edlin;

/* edit mode screen size */
SCREEN_SIZE ss;

/* flag to indicate block or under line cursor type */
int cursor_type;

/* list of printer errors detected */
char *print_error_list[] = {
    "OFF Line",
    "No paper",
    "Printer not ready"
};

/* table of months for formatting into audit begin/end messages */
char *month_tab[] = {
"Jan.",
"Feb.",
"Mar.",
"Apr.",
"May",
"June",
"July",
"Aug.",
"Sep.",
"Oct.",
"Nov.",
"Dec."
};

int wst_prt_timeout;    /* original printer timeout value */
int orig_break_status;  /* original CTRL-C checking status */
union REGS scroll_regs; /* registers for scrolling up 1 line */

/* End of WSTGLOB */
