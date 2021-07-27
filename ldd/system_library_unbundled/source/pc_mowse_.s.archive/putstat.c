/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-12,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (putstat)

Sends a message to the foreground application that contains a null terminated 
character string that contains status information.
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>
#include <wsmincap.h>

putstat (p_status_reply,p_mcb_ptr)

char *p_status_reply;                    /* Reply message */
mcb  *p_mcb_ptr;                         /* Caller's MCB */
{
int code;               /* return code */
int i;                  /* loop counter*/
int system;
int major;
int param_size;
struct putbg_struc msg; /* structure for message */

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* : copy status argument to mowse buffer */

   msg.length = stccpy (msg.bgmsg, p_status_reply, WSPAKSIZ);

/* : create caller's major capability number */

   system = (int)(p_mcb_ptr -> system_id);
   major = (int)(p_mcb_ptr -> major_capability);
   if (code = c_pack (system, major, &msg.sender_major))
      return (code);

/* : call user_interrupt to send the status */

   msg.type = STATUS_REPLY;
   param_size = msg.length + sizeof (struct putbg_struc) - WSPAKSIZ;
   return (call_mowse_int (I$PUTBGMES, &msg , param_size));
}
