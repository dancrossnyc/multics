/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-08,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (disresp)

Sends a "DISCONNECT_REPONSE" minor capability message to the addressed major 
capability. This message is used by the major capability to inform the caller 
whether the request has been accepted or rejected.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_error.h>

disresp (p_status, p_cap_num, p_mcb_ptr)

int p_status;                          /* WSACCEPT/WSREJECT */
int p_cap_num;                         /* Capability to send response to */
mcb *p_mcb_ptr;                        /* Caller's MCB */
{
   if ((p_status != WSACCEPT) && (p_status != WSREJECT))
      return (WSINVCON);

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* : Send the message out through MOWSE */

   return (execap (p_cap_num, RESPONSE_DISCONNECT, &p_status, 1, p_mcb_ptr));
}
