/* BEGIN INCLUDE FILE: emulator.h */

/* HISTORY COMMENTS:
  1) change(86-09-01,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-12-04,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added DELAY_INTERVAL constant.
                                                   END HISTORY COMMENTS */


/* FUNCTION:
    Define values and modes used by the MOWSE terminal emulator
*/

#define DELAY_INTERVAL   300           /* Waste time loop count */

#define DEL_LINE         512           /* key value defined for deleting */
                                       /*    a line */

#define KB_BUFFERSIZE    1024          /* size of keyboard buffer */
#define KB_STACKSIZE     1024          /* size of keyboard stack (should */
                                       /*    be same as keyboard buffer) */
#define KB_TABSIZE       10            /* default tab size used by emulator */

/* keyboard display modes */

#define ASCII_ONLY       0            /* flag meaning display only printable chars */
#define NON_ASCII_OCTAL  1            /* flag - non printable chars displayed as octal */
#define ANY_CHAR         2            /* flag - display all characters */

#ifndef TRUE                          /* define TRUE if not already defined */
#define TRUE             1
#endif

#ifndef FALSE                         /* define FALSE if not already defined */
#define FALSE            0
#endif


/* define number buffers for storing background messages */

#define BG_SENDBUFF_SIZE (NUMBER_OF_CAT_ENTRIES+1)

#define OP_FREEZE        1             /* code value to "freezing" emulator */
#define OP_BREAK         2             /* code value to sending a break char */
#define OP_EXEDOSCMD     3             /* code value to executing DOS command */
#define OP_BG_REPLY      4             /* code value to send background reply */
#define OP_EXIT          5             /* code value to exit the emulator */

/* : Set EM_DEBUG to 1 for debugging version,
     set to 0 for non-debugging version */

#define EM_DEBUG         0


/* END INCLUDE FILE: emulator.h */
