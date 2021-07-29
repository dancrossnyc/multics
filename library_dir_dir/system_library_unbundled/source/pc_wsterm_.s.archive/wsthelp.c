/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-04-21,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
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
                                                   END HISTORY COMMENTS */

#include <dos.h>
#include "wstdefs.h"
#include "wstglob.h"

char *general_help_lines[] = {
"-------------------------- WSTERM FUNCTIONS ----------------------------------",
"ALT-0  Send an ASCII NUL             |  ALT-D  Display pending foreground",
"ALT-B  Send a break to host          |         message",
"^BREAK Send a break if /B control    |  ALT-H  Enter help screen",
"       argument specified            |  ALT-M  Enter background screen",
"ALT-C  Clear the display             |  ALT-Q  Exit (quit) wsterm",
"                                     |  ALT-S  No scroll (any key resumes)",
"",
"",
"------------------------- WSTERM MODES (toggles) -----------------------------",
"ALT-I,<INS> keyboard insert/replace -|  ALT-E      edit mode ON/OFF",
"            (in edit mode only)      |  ALT-O      local paging ON/OFF",
"ALT-F       file audit ON/OFF        |  ALT-P      line printer audit ON/OFF",
"",
"",
"",

"-------------------------------- EDIT MODE -----------------------------------",
"A variety of editing and history functions are available in edit mode. While",
"in edit mode, all keyboard input is buffered and echoed, allowing various",
"editing to take place until the <RETURN> key is hit, sending the line to the",
"host. Note that before the attach_mowse/atm command is invoked, the host",
"also echoes input, resulting in a \"double echo\". Thus it may be desirable",
"to turn off echoing by the host (e.g. stty -modes ^echoplex) in this case.",
"",
"---------------------- Edit Mode cursor movement keys ------------------------",
"^A,<HOME> cursor to beginning of line|  ^E,<END>  move cursor to end of line",
"^F,->     move cursor forward a char |  ^B,->     move cursor backward a char",
"^->,esc F move cursor forward a word |  ^<-,esc B move cursor backward a word",
"",
"-------------------------- Edit Mode editing keys ----------------------------",
"Backspace erase (previous char)      |  @          kill (to beginning of line)",
"<DEL>   delete previous char         |  esc <DEL>  delete to previous word",
"^D      delete current char          |  esc D      delete to end of word",
"^K      kill to end of line          |  \\          literal escape character",
"",
"*   cursor movement and edit keys may be repeated N times by entering",
"    <ESC> N before the key. N is some unsigned integer value.",
"",
"----------------------- Edit Mode History Functions --------------------------",
"ALT-V    enter history screen        |   ^P     recall previous command",
"NN ^P    select the NNth previous    |   ^N     recall the next command",
"         command",
"",
"",
"",
"",
"",
"",
"----------------------------- Status Line ------------------------------------",
"The bottom line of the screen is the status line. It is displayed in reverse",
"video and is used to display local messages to the user. In the foreground",
"screen, WSTERM's current mode settings are displayed as follows:",
"",
"G Edit Replace Audit(FILE,PRINTER) Page No-Scroll [background] [foreground]",
"1  2     3              4            5      6           7            8",
"",
"1) G=Glass tty (before atm); A=Async (after atm); S=Sync (atm & video)",
"2) Edit=Edit mode enabled; <BLANK>=Edit mode disabled",
"3) Replace=text overstrikes; Insert=text inserts; <BLANK>=not in Edit mode",
"4) <BLANK>=No audit; Audit(FILE)=File audit; Audit(PRINTER)=Printer audit;",
"   Audit(FILE,PRINTER)=File and Printer audit",
"5) Page=local paging after atm; <BLANK>=no local page after atm",
"6) <BLANK>=scrolling; No-Scroll=local stopping of scrolling",
"7) <BLANK>-no background messages; [background]=background message(s) pending,",
"   enter background screen to examine messages.",
"8) <BLANK>-no foreground messages; [foreground]=foreground message(s) pending",
"   while entering input, hit ALT-D to display message, then resume input",
"",
"Note: The status line is also used to temporarily display error messages.",
"      Status line information is also different in other screens. See help",
"      for other screens."
};


#define N_GENERAL_HELP_LINES    (sizeof(general_help_lines)/sizeof(char *))

char *bg_help_lines[] = {
"----------------------------BACKGROUND SCREEN---------------------------------",
"This screen displays the number of pending background messages and displays",
"the next background message whenever <ALT-M> or <ALT-D> is hit. The user",
"may also enter this screen just to look at background messages which were",
"previously displayed but have not been scrolled off.",
"",
"    ALT-M,ALT-D - display next background messages if it exists",
"    Q           - quit from background screen",
"    P           - enter background polling mode",
"",
"----------------------------BACKGROUND POLLING--------------------------------",
"This screen displays all pending background messages and sits in a polling",
"loop, displaying any background message whenever it is received.",
"",
"    Q           - quit from background screen",
"    <other>     - toggle between scroll and no scroll",
"",
"-------------------- Replying to background query messages -------------------",
"When the message \"Enter Reply:\" appears on the status line, the most recent",
"background message displayed is a background query message. Any text until",
"a <RETURN> is hit will then be sent as a reply to this query.",
"",
"NOTE: Acceptable keys are listed between \"[\" and \"]\" on the status line"
};


#define N_BG_HELP_LINES    (sizeof(bg_help_lines)/sizeof(char *))

char *history_help_lines[] = {
"---------------- HISTORY SCREEN FUNCTIONS (in edit mode only) ----------------",
"This screen allows previously entered lines of input to be examined and/or",
"selected. Selecting an input line copies that line as current input, just",
"as if entered from the keyboard. Further editing may take place before",
"sending the line to the host by hitting the <RETURN> key.",
"",
" NN <RETURN> - exit the history screen and select the specified history",
"               line (beeps if invalid selection)",
" NN <L>      - shift screen left NN columns; NN defaults to 1",
" NN <R>      - shift screen right NN columns; NN defaults to 1",
" <P>         - display previous history page (beeps if no previous page)",
" <N>         - display next history page (beeps if no next page)",
" <Q>         - quit from the history screen",
"",
"",
"NOTE 1: NN refers to an unsigned integer value and is specified by entering",
"        any of the digit keys (0 thru 9).",
"NOTE 2: The status line will display the current screen offset.",
"NOTE 3: Recognized command keys are listed between \"[\" and \"]\" on the",
"        status line."
};

#define N_HISTORY_HELP_LINES    (sizeof(history_help_lines)/sizeof(char *))

/* help - this routine displays the help screen in a manner
          transparent to the caller. The parameter "cur_scr"
          specifies the screen to save the current screen
          contents to before displaying the help information.
*/


/*
**********************************************************************

  Routine:            HELP 

  Function:
      This routine saves the contents of the current screen and 
  invokes the help screen. The status line provides further 
  instructions to allow more help pages to be displayed or quitting 
  from the help screen. The original screen contents are restored on 
  quitting from help, making the invokation of help transparent to 
  the caller. 

  Parameters:
     (input)          order - flag to specify in what order the help 
                          topics should be displayed 

  Returns:            NONE 

**********************************************************************/

help(order)
int order;
{
    int i;
    int cnt;
    int ch;
    char **hlp[N_HELP_TOPICS];
    int  hlp_cnt[N_HELP_TOPICS];
    int  help_index;

    /* determine what order the topics are to be displayed */
    if (order == BG_HELP) {
        hlp[FIRST_TOPIC_INDEX] = bg_help_lines;
        hlp_cnt[FIRST_TOPIC_INDEX] = N_BG_HELP_LINES;
        hlp[SECOND_TOPIC_INDEX] = general_help_lines;
        hlp_cnt[SECOND_TOPIC_INDEX] = N_GENERAL_HELP_LINES;
        hlp[THIRD_TOPIC_INDEX] = history_help_lines;
        hlp_cnt[THIRD_TOPIC_INDEX] = N_HISTORY_HELP_LINES;
    }

    else if (order == HISTORY_HELP) {
        hlp[FIRST_TOPIC_INDEX] = history_help_lines;
        hlp_cnt[FIRST_TOPIC_INDEX] = N_HISTORY_HELP_LINES;
        hlp[SECOND_TOPIC_INDEX] = general_help_lines;
        hlp_cnt[SECOND_TOPIC_INDEX] = N_GENERAL_HELP_LINES;
        hlp[THIRD_TOPIC_INDEX] = bg_help_lines;
        hlp_cnt[THIRD_TOPIC_INDEX] = N_BG_HELP_LINES;
    }

    else {
        hlp[FIRST_TOPIC_INDEX] = general_help_lines;
        hlp_cnt[FIRST_TOPIC_INDEX] = N_GENERAL_HELP_LINES;
        hlp[SECOND_TOPIC_INDEX] = bg_help_lines;
        hlp_cnt[SECOND_TOPIC_INDEX] = N_BG_HELP_LINES;
        hlp[THIRD_TOPIC_INDEX] = history_help_lines;
        hlp_cnt[THIRD_TOPIC_INDEX] = N_HISTORY_HELP_LINES;
    }

    /* save the contents of the screen to the user specified buffer */
    save_wst_screen(&wst_tmp_screen);

    /* hide the cursor */
    cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

    /* initialize input char, working index and line counter */
    ch = 0;
    i = 0;
    cnt = 0;

    /* initialize topic index */
    help_index = 0;

    /* clear the temporary screen buffer */
    wst_screen_clear(&wst_help_screen);

    while (TRUE) {
            if (help_index >= LAST_HELP_TOPIC_INDEX && i >= hlp_cnt[help_index])
                break;

            /* increment line counter and see if screen is full */
            if (cnt >= N_HELP_PAGE_LINES || i >= hlp_cnt[help_index]) {

                /* screen full, dump the contents of the temporary screen buffer */
                restore_wst_screen(&wst_help_screen);

                /* hide the cursor */
                cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

                /* update the status line */
                status_line("HELP SCREEN: <q> - quit, <any other> - next");

                /* wait for key press and determine whether to quit */
                ch = getkey(BLOCK);
                if (uppercase(ch) == 'Q')
                    break;

                /* reset line counter and clear the temporary screen buffer */
                cnt = 0;
                wst_screen_clear(&wst_help_screen);

                if (i >= hlp_cnt[help_index]) {
                    help_index++;
                    i = 0;
                    if (help_index > LAST_HELP_TOPIC_INDEX) break;
                }
            }

            /* write a line of help info to the temporary screen buffer */
            wst_screen_printline(&wst_help_screen,hlp[help_index][i]);
            i++;
            cnt++;
    }

    /* if user did not hit quit to exit help screen */
    if (uppercase(ch) != 'Q') {

        /* dump contents of temporary screen buffer */
        restore_wst_screen(&wst_help_screen);

        /* hide the cursor */
        cursor_move(HIDE_CURSOR_ROW,HIDE_CURSOR_COL);

        /* update status line */
        status_line("HELP SCREEN: <any key> - quit");
        wait_for_key();
    }

    /* restore contents of original screen and turn cursor back on */
    restore_wst_screen(&wst_tmp_screen);
}
