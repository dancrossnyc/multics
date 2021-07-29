/****	BEGIN INCLUDE FILE wstglob.h				       */

/* HISTORY COMMENTS:
  1) change(87-03-13,Wallman), approve(87-03-13,MCR7586),
     audit(87-07-13,Flegel), install(87-08-07,MR12.1-1072):
     First release
  2) change(88-02-23,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     Removed debugging and unused declarations.
  3) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     Added miscellaneous global declarations for WSTERM
     enhancements (edit-mode, alternate screens, additional
     wsterm modes).
                                                   END HISTORY COMMENTS */

/* External data references for WSTERM

   See wstglob.c for descriptions of variables */

#ifndef	MAXMSG
#include  "wstdefs.h"
#endif


#ifndef WSTGLOB

#include	"ws_dcls.h"

extern	char	brk_table [96], bs_str [2], char_erase, erkl_chars [4], 
		F1_10 [10][4], F11_12 [2][4], line_kill, lnc, local_action, 
		nl [3], nul_str [2], octstr [6], spaces [10], version [6];

extern	int	bk_msg_showing, break_sent, crecho, echo_delay, fg_msg_len, 
		fg_msg_showing, octsw, octval, lfecho, mowse_active, 
		oldtime, sync, read_active, read_count, term_read;

extern	mcb	*wst_mcb;

extern	struct	ds_struct ds;
extern	union	fg_msg_struct fg_msg;
extern	struct	kb_struct kb;
extern	struct	screen_struct screen;
extern	struct	get_struc tdata_arg;

extern	char	*strncpy ();


extern int break_flag;  /* indicates break to host requested via ^BREAK */
extern int wst_status;  /* indicates current operating mode (glass,async, sync) */

extern char is_esc[];
extern int clr_index;   /* index for parsing and intercepting bad sequences */
extern int wst_fansi_flag;  /* indicates FANSI-CONSOLE used */
extern char *audit_file_name; /* name of audit file */
extern int wst_printer_card;  /* printer card no. */
extern int wst_prt_timeout;   /* default printer timeout value */

extern SCREEN wst_tmp_screen;   /* temporary screen for scratch usage */
extern SCREEN wst_bg_screen;    /* background screen for background messages */
extern SCREEN wst_fg_screen;    /* foreground screen for foreground messages */
extern SCREEN wst_help_screen;  /* screen used by help facility */

extern char *help_lines[];      /* on-line help info */
extern int cursor_type;         /* current cursor type */
extern int break_sends_to_host; /* set if /B option was specified */

extern int wst_edit_mode;     /* boolean flag to enable/disable line editing */
extern int wst_freeze;          /* boolean flag to control scrolling */
extern int wst_paging;          /* boolean flag to enable/disable local EOPs */
extern int wst_f_audit;         /* boolean flag to indicate file audit */
extern int wst_p_audit;         /* boolean flag to indicate printer audit */
extern int glass_edit_mode;

extern EDIT_LINE edlin;         /* edit mode line and line information */
extern SCREEN_SIZE ss;          /* edit mode screen size */

extern char *print_error_list[]; /* printer audit error messages */
extern char *month_tab[];        /* lookup for audit begin/end messages */

extern int orig_break_status;
extern union REGS scroll_regs;

#define WSTGLOB
#endif WSTGLOB

/****	END INCLUDE FILE wstglob.h
*/
