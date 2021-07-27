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

/* : PROCEDURE FUNCTION (conresp)

Sends a "CONNECT_REPONSE" minor capability message to the addressed major 
capability. This message is used by the major capability to either accept 
or reject a requested connection.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_error.h>

conresp (p_status, p_cap_num, p_mcb_ptr)

int p_status;                          /* WSACCEPT or WSREJECT */
int p_cap_num;                         /* Capability number to respond to */
mcb *p_mcb_ptr;                        /* MCB of caller */
{
int sysid;
int major;

/* : call execap to send message to application */

   sysid = ((p_cap_num & 0xff00) >> 8);
   major = (p_cap_num & 0xff);

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

   if ((sysid != WSIBMPC) && (sysid != WSMULTICS))
      return(WSINVSYS);

   if ((major < WSMAJCAP) && (major > MAX_CAPABILITY_NUMBER))
      return(WSINVNUM);

   if ((p_status != WSACCEPT) && (p_status != WSREJECT))
      return (WSINVCON);
        
   return (execap (p_cap_num, RESPONSE_CONNECT, &p_status, 1, p_mcb_ptr));
}
