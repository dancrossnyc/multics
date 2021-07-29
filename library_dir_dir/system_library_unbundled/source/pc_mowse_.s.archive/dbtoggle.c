/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(87-03-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

#include <ws_error.h>

#define PACKETS 125
#define REJECTS 126
#define EXTRA   127

extern int dbgpkts;
extern int dbgrejs;
extern int dbgxchr;

/* : PROCEDURE FUNCTION (toggle_debug_switches)

Toggle the debug switches packet (125), rejects (126), and extr-chars (127)
*/

toggle_debug_switches (db_switch)

int  db_switch;
{

   switch (db_switch)
   {
      case PACKETS:
         dbgpkts = !dbgpkts;
         return (0);

      case REJECTS:
         dbgrejs = !dbgrejs;
         return (0);

      case EXTRA:
         dbgxchr = !dbgxchr;
         return (0);

      default:
         return (WSERROR);
   }

   return (0);
}
