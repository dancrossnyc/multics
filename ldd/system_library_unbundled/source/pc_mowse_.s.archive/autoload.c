/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-10-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION

Perform the autoload of all specified capabilities, capabilities are specified
for autoload through the /L option to the MOWSE command.  These are destined
to be executed when MOWSE has entered packet mode.
*/

/* : NOTES

The capabilities specified must be in the PC PATH search list.
*/

#include <stdio.h>
#include <ws.h>
#include <ws_auto.h>
#include <ws_dcls.h>

#define CHAR_SHIFT   8

AUTO   load_list[AUTO_LIMIT];          /* List of capabilities to be loaded */
int    load_list_pending;              /* Indicates load list is not empty */

int autoload ()
{
int    i;
int    error;
struct putbg_struc error_msg;          /* Error message struct for reporting */

   error_msg.type = WSINFO;
   error_msg.sender_major =(int)(((int)WSIBMPC << CHAR_SHIFT) | (int)WSMAJCAP);

   for (i = 0; (i < AUTO_LIMIT) && (load_list[i].flags & AUTO_ON); i++)
   {  set_dta();                       /* use Mowse's DTA */
      set_trap();                      /* use MOWSE's trap rotuines */
      error = system(load_list[i].name);
      rst_trap();
      rst_dta();

      if (error)
         sprintf (error_msg.bgmsg, "autoload: %s failed.\n", load_list[i].name);
      else
         sprintf (error_msg.bgmsg, "autoload: %s attempted.\n", load_list[i].name);

/* : Send a background message to indicate what has happened */

      error_msg.length = strlen (error_msg.bgmsg);
      i_putbgm (&error_msg);
   }
}
