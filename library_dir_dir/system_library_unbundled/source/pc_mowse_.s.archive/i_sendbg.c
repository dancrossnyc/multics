/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-13,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_sendbg)

Sends a message over the back ground channel from a foreground program.
*/

#include <stdio.h>
#include <ws.h>
#include <ws_error.h>
#include <ws_msg.h>

extern char mysystem;                  /* ID of this system */

i_sendbg (p_msg, p_length)

struct packet_msg *p_msg;
int p_length;
{

   if ((p_msg -> system == 0) || (p_msg -> major == 0))
   {  p_msg -> source_system = mysystem;
      p_msg -> source_major = WSMAJCAP;
   }

   return (send_i_mess (p_msg, p_length, NULL, 0));
}
