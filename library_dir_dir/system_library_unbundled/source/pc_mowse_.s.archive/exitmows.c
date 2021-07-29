/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-12,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Modified error display.
  3) change(87-01-22,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added /F(orce) option.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION

Destroy the MOWSE environment on the PC if Multics has detached (dtm).

Arguments:

   /F - (Force) destruction whether or not Multics has detached.
*/

#include <stdio.h>
#include <ws_error.h>
#include <ws_func.h>

#define EXTRA_ARG   -1                 /* Unexpected argument */
#define BAD_ARG     -2                 /* Invalid argument */

#define TIME_OUT        1              /* Disconnect timed out */
#define DISCONNECT_OUT  2              /* Disconnect verified */

#define FORCE        0x01              /* Force requested */

main (argc, argv)

int  argc;                             /* Argument count */
char *argv[];                          /* Argument strings */
{
int  code;                             /* Error code */
int  i;
char flag;                             /* Force flag */

/* INITIALIZATION */

/* : Parse through the arguments */

   for (i = 1, code = 0, flag = 0; (i < argc) && (!code); i++)
   {  if (argv[i][0] != '/')
         code = BAD_ARG;
      else switch (argv[i][1])
      {  
         case 'F': case 'f':           /* FORCE */
            if (argv[i][2])
               code = BAD_ARG;
            else
               flag |= FORCE;
            break;

         default:                      /* ERROR */
            code = BAD_ARG;
            break;
      }
   }

/* : Call mowse_interrupt to disconnect mowse */

   if (!code)
   {  printf ("EXITMOWS ...");
      while ((code = call_mowse_int (I$DISCONNECT, &flag, 1)) == WSDISPEN);
      if (code == DISCONNECT_OUT)
         code = WSACTIVE;
      else if (code == TIME_OUT)
      {  flag = FORCE;
         code = call_mowse_int (I$DISCONNECT, &flag, 1);
      }
   }


/* : Parse the error codes */

   switch (code)
   {
      case BAD_ARG:
         printf ("exitmows: bad argument %s.", argv[i-1]);
         break;

      case EXTRA_ARG:
         printf ("exitmows: unexpected option %s.", argv[i-1]);
         break;

      case 0:
         printf (" MOWSE terminated.");
         break;

      case WSACTIVE:
         printf (" MOWSE is not detached on Multics.");
         break;

      case WSNOTACT:
         printf (" MOWSE is not resident.");
         break;

      default:
         printf (" error %d.", code);
         break;
   }
}
