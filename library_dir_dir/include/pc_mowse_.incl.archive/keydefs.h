/* BEGIN INCLUDE FILE: keydefs.h */

/* HISTORY COMMENTS:
  1) change(86-09-01,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION:
    This include file assigns a unique value to each key sequence
on the PC keyboard.  A key sequence consists of a single key press
or a key press while the control or the ALT key is depressed.
For keys not explicitly assigned a value, the ASCII value for that
key character is assumed.  Some keys are represented as extended
ASCII codes (such as the BREAK key, function keys, arrow keys, etc.).
These generate a two character sequence from the keyboard with the
first character being a NULL byte.  The value assigned for extended
ASCII characters is the value of the second byte plus 256.

*/


/* define values for functions keys F1 to F10 */

#define F1                      315
#define F2                      316
#define F3                      317
#define F4                      318
#define F5                      319
#define F6                      320
#define F7                      321
#define F8                      322
#define F9                      323
#define F10                     324

/* define values for function keys F1 to F10 while the CTRL key is depressed */

#define CTRL_F1                 350
#define CTRL_F2                 351
#define CTRL_F3                 352
#define CTRL_F4                 353
#define CTRL_F5                 354
#define CTRL_F6                 355
#define CTRL_F7                 356
#define CTRL_F8                 357
#define CTRL_F9                 358
#define CTRL_F10                359

/* define values for functions keys F1 to F10 while the ALT key is depressed */

#define ALT_F1                  360
#define ALT_F2                  361
#define ALT_F3                  362
#define ALT_F4                  363
#define ALT_F5                  364
#define ALT_F6                  365
#define ALT_F7                  366
#define ALT_F8                  367
#define ALT_F9                  368
#define ALT_F10                 369

/* define values for miscellaneous extended ASCII characters */

#define HOME_KEY                327   /* Home key */
#define UP_ARROW_KEY            328   /* the up arrow key */
#define PAGE_UP_KEY             329   /* the page up key */
#define LEFT_ARROW_KEY          331   /* the left arrow key */
#define RIGHT_ARROW_KEY         333   /* the right arrow key */
#define END_KEY                 335   /* the End key */
#define DOWN_ARROW_KEY          336   /* the down arrow key */
#define PAGE_DOWN_KEY           337   /* the PgDn key */
#define INS_KEY                 338   /* the Ins key */
#define DEL_KEY                 339   /* the Del key */

/* define values for characters with the CTRL key depressed */

#define CTRL_A                  1     /* CTRL A */
#define CTRL_B                  2     /* CTRL B */
#define CTRL_C                  3     /* CTRL C */
#define CTRL_D                  4     /* etc ... */
#define CTRL_E                  5
#define CTRL_F                  6
#define CTRL_G                  7
#define CTRL_H                  8
#define CTRL_I                  9
#define CTRL_J                  10
#define CTRL_K                  11
#define CTRL_L                  12
#define CTRL_M                  13
#define CTRL_N                  14
#define CTRL_O                  15
#define CTRL_P                  16
#define CTRL_Q                  17
#define CTRL_R                  18
#define CTRL_S                  19
#define CTRL_T                  20
#define CTRL_U                  21
#define CTRL_V                  22
#define CTRL_W                  23
#define CTRL_X                  24
#define CTRL_Y                  25
#define CTRL_Z                  26    /* CTRL Z */
#define CTRL_RSB                29    /* CTRL ]  (control right square */
                                      /*          bracket)             */

/* define ALIASES for commonly used keys */

#define BELL                    7     /* same as CTRL G */
#define BS                      8     /* same as CTRL H */
#define TAB                     9     /* same as CTRL I */
#define LF                      10    /* same as CTRL J */
#define CR                      13    /* same as CTRL M */
#define ESC                     27    /* the Esc key */
#define SPACE                   32    /* the space bar */

/* END INCLUDE FILE: keydefs.h */
