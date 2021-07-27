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
  2) change(86-11-11,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Source capability needs to be passed in the
     message.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (resetcap)

Sends a "RESET_APPLICATION" minor capability message to the addressed major 
capability. This message is used by the major capability as a signal to 
discard all un-processed buffers amd to reset all counters, etc.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>

resetcap (p_major_number, p_mcb_ptr)

int p_major_number;
mcb *p_mcb_ptr;
{
int code;                /* return code  */
int system;              /* Temp system id */
int major;               /* Temp major cap */
struct xcap_struc xcap;  /* parameter area for call to MOWSE */

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* : send message to application */

   if (code = c_unpack (&system, &major, p_major_number))
      return (code);

   xcap.system = (char)system;
   xcap.major = (char)major;
   xcap.minor = RESET_APPLICATION;
   xcap.source_system = p_mcb_ptr -> system_id;
   xcap.source_major  = p_mcb_ptr -> major_capability;

   return (call_mowse_int (I$RESET, &xcap, sizeof (xcap) - 1));
}
