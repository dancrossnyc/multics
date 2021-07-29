/* BEGIN INCLUDE FILE  ws_func.h */

/* HISTORY COMMENTS:
  1) change(86-05-31,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-10-24,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added I$CONNECT minor.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Defines the MOWSE functions numbers used by the software interrupt handler.
Equivalent include file ws_func.mac
*/

/* Application interrupts */

#define I$EXECOM          2      /* execute command */
#define I$EXECAP          3      /* execute capability */
#define I$CRETINST        4      /* create instance */
#define I$DESTINST        5      /* destroy instance */
#define I$FINDNAME        6      /* find capability name */
#define I$FINDNUMB        7      /* find capability number */
#define I$GETTDATA        9      /* get terminal data */
#define I$PUTTDATA        10     /* put terminal data */
#define I$GETBGMES        11     /* get background message */
#define I$PUTBGMES        12     /* put background message */
#define I$SENDBG          13     /* send message to background */
#define I$PUTQUERY        13     /* put query message */
#define I$STATUS          14     /* status request/reply */
#define I$RESET           15     /* send reset or resume request */
#define I$SLEEP           16     /* put application to sleep */
#define I$DISCONNECT      17     /* disconnect request */
#define I$FOREBREAK       18     /* Foreground break */
#define I$SUSPEND         19     /* Suspend application */
#define I$CONNECT         20     /* Connect to application */

/* END INCLUDE FILE ws_func.h */
