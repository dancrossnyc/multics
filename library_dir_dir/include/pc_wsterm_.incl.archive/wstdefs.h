/****	BEGIN INCLUDE FILE wstdefs.h				       */

/* HISTORY COMMENTS:
  1) change(87-03-13,Wallman), approve(87-03-13,MCR7586),
     audit(87-07-16,Flegel), install(87-08-07,MR12.1-1072):
     First release
  2) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     Separated redefinitions of ctype.h defines
     and placed them in separate include file "wsttype.h"; added
     miscellaneous defines used.
                                                   END HISTORY COMMENTS */

/* Function:
    This include file defines constants and structures used in WSTERM.
*/

#ifndef WSTDEFS
/* Constant definitions and structure skeletons for WSTERM */

/* ASCII characters and definitions */

#define BEL				'\007'	/* ASCII BEL */
#define BSP				'\010'	/* ASCII BS */
#define CR				'\015'	/* ASCII CR */
#define DEL				'\177'	/* ASCII DEL - Default char erase */
#define ESC				'\033'	/* ASCII ESC */
#define HT				'\011'	/* ASCII HT */
#define KILL				'\100'	/* ASCII AT - Default line kill */
#define LF				'\012'	/* ASCII LF */
#define LNC				'\134'	/* ASCII \ - Default literal next char */
#define NL				'\012'	/* ASCII NL */
#define NUL				'\000'	/* ASCII NUL */
#define ASCII_DEL			127		/* ASCII value for DEL */
#define ASCII_NUL			0		

#define CTRL_A				1		/* ASCII value for CTRL A */
#define CTRL_B				2		/* ASCII value for CTRL B */
#define CTRL_D				4		/* ASCII value for CTRL D */
#define CTRL_E				5		/* ASCII value for CTRL E */
#define CTRL_F				6		/* ASCII value for CTRL F */
#define CTRL_K				11		/* ASCII value for CTRL K */
#define CTRL_N				14		/* ASCII value for CTRL N */
#define CTRL_P				16		/* ASCII value for CTRL P */
#define CTRL_T				20		/* ASCII value for CTRL T */
#define CTRL_Y				25		/* ASCII value for CTRL Y */

#define RETURN_KEY			13		/* ASCII value for RETURN key */
#define BACKSPACE			8		/* ASCII value for BACKSPACE */
#define TAB				9		/* ASCII value for TAB */
#define LINEFEED			10		/* ASCII value for line feed character */

#define MIN_PRINTABLE_ASCII	' '		/* lowest printable ascii char */
#define MAX_PRINTABLE_ASCII	126		/* maximum printable ascii char */
#define MAX_CNTRL_CHAR		31		/* highest ASCII control character */

/* Keyboard key codes */
#define DEL_KEY			339		/* unique key code for DEL key */
#define CTRL_2				259		/* unique key code for ^2 */
#define NULL_KEY			259		/* another name for ^2 */
#define ALT_D				288		/* unique key code for ALT-D */
#define ALT_M				306		/* unique key code for ALT-M */
#define ALT_H				291		/* unique key code for ALT-H */
#define ALT_B				304		/* unique key code for ALT-B */
#define ALT_S				287		/* unique key code for ALT_S */
#define ALT_I				279		/* unique key code for ALT_I */
#define BREAK_KEY			512		/* unique key code for ^BREAK */

/* define values for keypad characters */

#define CTRL_LEFT_ARROW_KEY	371		/* control left arrow key */
#define CTRL_RIGHT_ARROW_KEY	372		/* control right arrow key */
#define DOWN_ARROW_KEY		336		/* the down arrow key */
#define END_KEY			335		/* the End key */
#define HOME_KEY			327		/* Home key */
#define INS_KEY			338		/* the Ins key */
#define LEFT_ARROW_KEY		331		/* the left arrow key */
#define PAGE_DOWN_KEY		337		/* the PgDn key */
#define RIGHT_ARROW_KEY		333		/* the right arrow key */
#define UP_ARROW_KEY		328		/* the up arrow key */


/* ANSI functions */

#define ED				"\033[2J"	/* Erase display */
#define EL				"\033[K"	/* Erase line */
#define RCP				"\033[u"	/* Restore cursor position */
#define SCP				"\033[s"	/* Save cursor position */


/* DOS/BIOS constants */

#define BIOS_KB			0x16		/* BIOS keyboard request */
#define BIOS_KB_READ		0		/* Keyboard read function */
#define BIOS_KB_SHIFTS		2		/* Keyboard shift status function */
#define BIOS_KB_STATUS		1		/* Keyboard status function */
#define BIOS_PRT			0x17		/* BIOS printer interrupt request */
#define BIOS_PRT_PRINT		0		/* BIOS print character function */
#define BIOS_PRT_STATUS		2		/* BIOS printer status function */
#define BIOS_SEGMENT		0x40		/* segment location of BIOS variables */
#define BIOS_VIDEO			0x10		/* BIOS video request */
#define BLINK_ATTRIBUTE		0x80		/* bit that enables blink attribute */
#define CUP				2		/* BIOS cursor position function */
#define DOS_CTRL_C_DISABLE	0		/* ctrl-c checking disabled */
#define DOS_CTRL_C_ENABLE	1		/* ctrl-c checking enabled */
#define DOS_CTRL_C_EXCHANGE	2		/* ctrl-c exchange function */
#define DOS_CTRL_C_FUNCTION	0x33		/* DOS set/check ctrl-c function */
#define DOS_CTRL_C_SET		1		/* ctrl-c set function */
#define DOS_GET_DATE		0x2a		/* DOS get date function */
#define DOS_GET_TIME		0x2c		/* DOS get time function */
#define DOS_INT			0x21		/* DOS function interrupt */
#define HIDE_CURSOR_COL		0		/* hide cursor column coordinate */
#define HIDE_CURSOR_ROW		25		/* hide cursor row coordinate */
#define LINE24_COL0			0x1700	/* Start of blank scroll line */
#define N_BG_ATTR_BITS		4		/* number of background video attribute bits */
#define N_FG_ATTR_BITS		4		/* number of foreground video attribute bits */
#define PRT_NO_PAPER_MODE	070		/* bit pattern for printer no paper */
#define PRT_OFF_LINE_MODE	030		/* bit pattern for printer off line */
#define PRT_OFF_MODE		0210		/* bit pattern for printer off */
#define PRT_READY_MODE		0220		/* bit pattern for printer ready */
#define PRT_TIMEOUT_BASE		0x78		
#define RD_SCREEN			0x8		/* Read screen char and attributes */
#define SCROLL_LINE_ATTRS	0x700	/* Attributes for blank scroll line */
#define SCROLL_UP_1_LINE		0x601	/* BIOS scroll up function */
#define SCROLL_WINDOW_END	0x174F	/* End of scrolling window */
#define SCROLL_WINDOW_START	0		/* Start of scrolling window */
#define SET_CURSOR_POS		2		/* BIOS set cursor position function */
#define SYSTEM_TIME			0x2C		/* BIOS system time function */
#define VIDEO_CURSOR_MOVE_FUNC	0x2		/* video cursor move function */
#define VIDEO_CURSOR_READ_FUNC	0x3		/* video read cursor function */
#define VIDEO_SCROLL_UP_FUNC	0x6		/* video scroll up function */
#define WRT_SCREEN			0x9		/* Write screen char and attributes */


/* Keystroke status bits */

#define ALT				0x8		/* ALT key status bit */
#define CAPSLCK			0x40		/* Caps lock */
#define CTRL				0x4		/* CTRL key status bit */
#define INSACT				0x80		/* Insert mode active */
#define LSHIFT				0x2		/* LEFT SHIFT key status bit */
#define NUMLCK				0x20		/* Numeric lock */
#define RSHIFT				0x1		/* RIGHT SHIFT key status bit */
#define SCRLLCK			0x10		/* Scroll lock */


/* Keyboard scan codes */

#define B_KEY_CODE			48		/* keyboard scan code for B key */
#define C_KEY_CODE			46		/* keyboard scan code for C key */
#define D_KEY_CODE			32		/* keyboard scan code for D key */
#define E_KEY_CODE			18		/* keyboard scan code for E */
#define F11_KEY_CODE		84		/* keyboard scan code, F11 func key */
#define F1_KEY_CODE			59		/* keyboard scan code, F1 func key */
#define F_KEY_CODE			33		/* keyboard scan code for F */
#define H_KEY_CODE			35		/* keyboard scan code for H */
#define M_KEY_CODE			50		/* keyboard scan code for M key */
#define N_KEY_CODE			49		/* keyboard scan code for N key */
#define O_KEY_CODE			24		/* keyboard scan code for O */
#define P_KEY_CODE			25		/* keyboard scan code for P */
#define Q_KEY_CODE			16		/* keyboard scan code for Q key */
#define S_KEY_CODE			31		/* keyboard scan code for S */
#define V_KEY_CODE			47		/* keyboard scan code for V */
#define zero_KEY_CODE		11		/* keyboard scan code for 0 key */


/* Other constants */

#define ASCII_EXTEND_CODE	256		/* assigns unique key codes */
#define ASCII_ZERO_BASE		'0'		/* to convert binary to ascii digit */
#define BLINK				-1		/* Blinking attribute for chars */
#define BLOCK				-1		/* Wait for function completion */
#define CURSOR_BLOCK		2		/* WSTERM block cursor type */
#define CURSOR_HOME_COL		0		/* cursor coordinates for home col */
#define CURSOR_HOME_ROW		0		/* cursor coordinates for home row */
#define CURSOR_UNDERSCORE	1		/* WSTERM underscore cursor type */
#define DEFAULT_ACTIVE_PAGE	0		/* default active page */
#define DEFAULT_AUDIT_FILE	"wsterm.log"	/* default audit file name */
#define DEFAULT_LINES_TO_SCROLL	1	/* normally only 1 line scrolled */
#define DEFAULT_PRINTER_CARD	0		/* default printer card value */
#define DS_LCT				8		/* Display map size */
#define FALSE				0		/* logical false */
#define FG_MSG_SIZE			3000		/* Size of foreground message buffer */
#define FORCE				-1		/* Switch value to force stuff */
#define F_AUDIT_BUFF_SIZE	1024		/* size of file audit buffer */
#define HIDE				-1		/* Switch value to hide stuff */
#define HI_BYTE_VALUE		0x100	/* Value of high byte in size field */
#define KB_CAPITALIZED		3		/* flag for capitalize conversion */
#define KB_HANDLING_ESC_ARG	2		/* handling numeric escape argument */
#define KB_INSERT_MODE		1		/* input chars are inserted */
#define KB_LITERAL_ESC		4		/* handling literal character escape */
#define KB_LOWER_CASE		2		/* flag for lower case conversion */
#define KB_NO_BLOCK			0		/* no block flag for edit mode keyboard read */
#define KB_PREV_KEY_ESC		1		/* previous key hit was escape key */
#define KB_REPLACE_MODE		0		/* input chars overstrike/replace */
#define KB_UPPER_CASE		1		/* flag for upper case conversion */
#define LARGEST_DECI_DIGIT	'9'		/* largest ascii decimal digit */
#define LARGEST_OCTAL_DIGIT	'7'		/* largest ascii octal digit */
#define LOW_7_BITS			0177		/* mask for lowest 7 bits */
#define LOW_8_BITS			0xff		/* mask for lowest 8 bits */
#define LO_BYTE_MASK		0xFF		/* Mask for low order bytes */
#define MAXINT				0x7FFF	/* Maximum integer value */
#define MAXMSG				650		/* Length of message buffers */
#define MAX_7_BIT_VAL		128		/* maximum value for 7 bits */
#define MAX_8_BIT_VALUE		256		/* maximum value for 8 bits */
#define MAX_ARG_LIMIT		3266		/* max signed int value / 10 */
#define MAX_BG_MESS_LENGTH	256		/* maximum background message length */
#define MAX_DATE_TIME_STR_LENGTH	32		/* max. length for date/time string */
#define MAX_DISPLAY_CHAR_SIZE	10		/* maximum length of char displayed */
#define MAX_LINE_SIZE		512		/* size of edit mode line buffer */
#define MAX_OCTAL_SEQ_DIGITS	3		/* max. digits in octal sequence */
#define MAX_PRINTER_CARD_VAL	2		/* highest printer card value */
#define MAX_SCREEN_COL		79		/* Dimensions for 80x24 screen */
#define MAX_SCREEN_LINE		23
#define MINI				-1		/* Mini switch setting */
#define MINI_LIN			24		/* The minibuffer line */
#define MIN_PRINTER_CARD_VAL	0		/* lowest printer card value */
#define NONE				0		/* descriptive define for 0 */
#define NO_BLOCK			-1		/* Dont block for keyboard input */
#define NO_BLOCK_MASK		0x80		/* No block flag for sync reads */
#define NO_ESC_ARG			-1		/* no numeric arguments */
#define NO_ESC_FUNC			0		/* not handling special escaping */
#define NUL_TERMINATOR		0		/* descriptive, string terminator */
#define N_SIZE_SPEC_BYTES	2		/* size specifier word bytes */
#define OCTAL_BASE_VALUE		8		/* multiplier for octal base */
#define OFF				0		/* Switch OFF value */
#define ON				-1		/* Switch ON value */
#define ONE_BYTE			1		/* descriptive define for 1 byte */
#define PEL_OFF_LINE		0		/* printer error list OFF LINE */
#define PEL_NOT_READY		2		/* printer error list NOT READY */
#define PEL_NO_PAPER		1		/* printer error list NO PAPER */
#define PSTR				-1		/* Switch value for prompt strings */
#define P_AUDIT_BUFF_SIZE	256		/* size of printer audit buffer */
#define QUIT				'Q'		/* Quit emulator & return to DOS */
#define SCREEN_COLS			80		/* number of screen columns */
#define SCREEN_LINES		24		/* number of screen rows or lines */
#define SET_MARK			"\000"	/* Emacs "set-the-mark" */
#define SMALLEST_DECI_DIGIT	'0'		/* smallest ascii decimal digit */
#define SMALLEST_OCTAL_DIGIT	'0'		/* smallest ascii octal digit */
#define STATUS				-1		/* Argument value for rd_key */
#define TAB_SIZE			10		/* size (in columns) of a tab */
#define TOTAL_SIZE_SPEC_BYTES	4		/* total bytes for size specifier */
#define TRO				-1		/* Delay timer has run out */
#define TRUE				1		/* logical true */
#define TXMT_MSG			-1		/* Transmit a message */
#define WST_ASYNC_MODE		2		/* asynchronous packet mode */
#define WST_BACKGROUND_SCREEN	1		
#define WST_FOREGROUND_SCREEN	2		
#define WST_GLASS_TTY_MODE	1		/* glass tty mode */
#define WST_SYNC_MODE		3		/* synchronous packet mode */
#define WST_UNKNOWN_MODE		0		/* unknown wsterm protocol mode */
#define ZERO_BYTES			0		/* descriptive define for 0 bytes */
#define ZERO_INDEX_VALUE		0		/* descriptive define for index 0 */
#define Z_FLAG_MASK			0100		/* mask for the CPU Z bit */

#define KILLBUFF_SAVE		TRUE		/* enabled saving to kill buffer */
#define NO_KILLBUFF_SAVE		FALSE	/* disable saving to kill buffer */

/* total number of characters on the screen */
#define SCREEN_CHARS		(SCREEN_LINES*SCREEN_COLS)	

/* total number of characters in each screen buffer */
#define SCREEN_BUFFER_BYTES	(SCREEN_LINES*SCREEN_COLS*2)	

/* constants which define help topics */

#define BG_HELP			1		/* background screen help */
#define GENERAL_HELP		0		/* general help */
#define HISTORY_HELP		2		/* history screen help */
#define N_HELP_TOPICS		3		/* number of help topics */
#define N_HELP_PAGE_LINES	24		/* number of lines per help screen */

#define FIRST_TOPIC_INDEX	0		/* descriptive define for index 0 */
#define SECOND_TOPIC_INDEX	1		/* descriptive define for index 1 */
#define THIRD_TOPIC_INDEX	2		/* descriptive define for index 2 */

#define LAST_HELP_TOPIC_INDEX	(N_HELP_TOPICS-1)	/* last help topic index */


/* FANSI-CONSOLE escape sequences */
#define FCON_RESET_SCROLL_STR	"\033[0;0r"	/* default scroll region */
#define FCON_SET_SCROLL_24	"\033[1;24r"	/* scroll region rows 1 to 24 */

#define beep()				putch('\007')	/* sound a beep */


/* Structures */

struct	ctl_msg_hdr		/* Control message header */
	{ char	id [3];		/* Message ID */
	  byte    msb_size, lsb_size; /* Message size */
	};


struct	ds_struct			/* Keyboard display data */
	{ int	ccol,		/* Current column position */
		do_col, do_ctl,	/* For ANSI control sequences */
		lct,		/* Number of ds_map lines */
		lndx,		/* Current display map line index */
		pstrl,		/* Prompt string length */
		spill [DS_LCT],	/* Last ds.dlin char spills */
		splct [DS_LCT];	/* Char spill from previous line */
	  char	dlin [SCREEN_COLS+1],	/* Current display line */
		map [DS_LCT] [SCREEN_COLS+1], /* Display line map */
		pstr [SCREEN_COLS+1];	/* The prompt string */
	};

struct	kb_struct			/* Keyboard input data */
	{ int	echo,		/* Effective echo switch */
		cndx,		/* Next kb.klin character index */
		endx,		/* First kb.klin unechoed character */
		fkey,		/* Function key switch */
		key_ndx,		/* Keyboard scan index */
		key_status,	/* Keyboard status */
		insert_mode,	/* Insert mode switch */
		pos [MAXMSG];	/* Display position of each char */
	  char	chr [10],		/* A keyboard "character" */
		klin [MAXMSG+1],	/* Keyboard input line */
		dstr [12];	/* Display string for kb.chr */
	};

struct	screen_struct		/* Screen display data */
	{ int	curcol, curlin,	/* Current cursor position */
		EOP_ct,		/* EOP line counter */
		maxcol, maxlin;	/* Screen size from tty_ modes */
	  char	mini_buf [MAX_SCREEN_COL+1]; /* 25th line minibuffer */
	};

union	regs_struct
	{ struct	HREG hreg;
	  struct	XREG xreg;
	};

struct	snd_msg_struct
	{ struct	ctl_msg_hdr hdr;
	  char	data [MAXMSG + 1];
	};

struct	stm_data			/* Data for STM message */
	{ byte	modes,		/* '00000'|lfecho|crecho|sync */
		kill,
		erase,
		lnc,
		maxcol,
		maxlin;
	};

union	fg_msg_struct
	{ char	text [FG_MSG_SIZE];
	  struct	{ struct	ctl_msg_hdr hdr;
		  union	{ byte	rd_ct [2];
			  char	break_table [96];
			  struct	stm_data stm;
			} data;
		} ctl;
	};


/* screen buffer for saving contents of screen; screen characters
and attributes are saved
*/
typedef struct wst_screen_struct {
    int cursor_row;     /* cursor row coordinate to save */
    int cursor_col;     /* cursor column coordinate to save */
    int attr;           /* default attribute to use */
    char screen[SCREEN_BUFFER_BYTES]; /* buffer to hold screen contents */
} SCREEN;


/* structure which defines the screen dimensions for edit mode
   keyboard display
*/
typedef struct screen_size_struct {
    int left;     /* left coordinate of keyboard window */
    int right;    /* right coordinate of keyboard window */
    int top;      /* top coordinate of keyboard window */
    int bottom;   /* bottom coordinate of keyboard window */
} SCREEN_SIZE;

/* structure for keeping temporary track of cursor positioning
   information
*/
typedef struct curpos_struct {
    int  orig_row;   /* row coordinates where displaying first begins */
    int  orig_col;   /* col coordinates where displaying first begins */
    int  cur_row;    /* row coordinates of where cursor currently is */
    int  cur_col;    /* col coordinates of where cursor currently is */
    int  max_row;    /* maximum row coordinates of characters displayed */
    int  max_col;    /* maximum col coordinates of characters displayed */
    int  scroll_flag; /* indicates screen has scrolled */
} CURPOS;


/* structure containing the line and information about the line
   being edited in edit mode
*/
typedef struct line_struct {
    int mode;        /* flag indicating replace/insert */
    int  orig_row;   /* row coordinate where displaying originates */
    int  orig_col;   /* col coordinate where displaying originates */
    int  cur_row;    /* row coordinate of current cursor position */
    int  cur_col;    /* col coordinate of current cursor position */
    int  max_row;    /* maximum row coordinates of characters displayed */
    int  max_col;    /* maximum col coordinates of characters displayed */
    int scrolled_flag;   /* flag indicating screen has scrolled */
    int off_screen;   /* flag indicating text beyond what is displayed */
    int escape_flag;  /* flag indicating whether doing escape handling */
    int escape_arg;   /* flag containing numeric escape argument */

    char line[MAX_LINE_SIZE+2]; /* command line, extra bytes for LF and '\0' */
    char size[MAX_LINE_SIZE+2]; /* command line, extra bytes for LF and '\0' */
    char literal_buff[4];  /* for holding literal character strings */
    int index;        /* index representing cursor location in line */
    int length;      /* line length */
    int literal_dex;  /* length of literal character string */
} EDIT_LINE;

#define WSTDEFS
#endif

/* END INCLUDE FILE: wstdefs.h */
