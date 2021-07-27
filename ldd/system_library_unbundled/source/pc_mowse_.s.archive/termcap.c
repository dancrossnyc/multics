/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-10-29,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (termcap)

Send a TERMINATE_APPLICATION minor cap to the specified capability.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_error.h>

#define BYTE_SHIFT 8

termcap (cap_num,mcb_ptr)

int cap_num;                           /* Capability to terminate */
mcb *mcb_ptr;                          /* MCB of caller */
{
int sysid;
int major;

/* : call execap to send message to application */

   sysid = ((cap_num & 0xff00) >> BYTE_SHIFT);
   major = (cap_num & 0xff);

   if (mcb_ptr == NULL)
      return (WSINVMCB);

   if ((sysid != WSMULTICS) && (sysid != WSIBMPC))
      return(WSINVSYS);

   if ((major < WSMAJCAP) && (major > MAX_CAPABILITY_NUMBER))
      return(WSINVNUM);

   return (execap (cap_num, TERMINATE_APPLICATION, NULL, 0, mcb_ptr));
}
