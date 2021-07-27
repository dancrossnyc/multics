/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (disrqst)

Sends a "DISCONNECT_REQUEST" minor capability message to the addressed 
major capability. This message is used by the major capability to destroy 
the instance indicated by the major capability number.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_error.h>

disrqst(p_cap_num,p_mcb_ptr)

int p_cap_num;                           /* Capability to send request to */
mcb *p_mcb_ptr;                          /* Caller's MCB */
{
   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* : Pass the message through MOWSE */

   return (execap (p_cap_num,REQUEST_DISCONNECT,NULL,0,p_mcb_ptr));
}
