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

/* : PROCEDURE FUNCTION (resume)

Sends a "RESUME_APPLICATION" minor capability message to the addressed major 
capability. This message is used by the major capability as a signal to begin 
processing messages again.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>

extern char local_system;

resume (p_major_number, p_mcb_ptr)

int p_major_number;
mcb *p_mcb_ptr;
{
int code;                              /* return code  */
int system;                            /* Temporary system id */
int major_cap;                         /* Temporary capability index */
struct xcap_struc xcap;                /* parameter area for call to MOWSE */

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* :  Send message to application via MOWSE */

   code = c_unpack (&system, &major_cap, p_major_number);
   if (code != 0) 
      return (code);

   xcap.system        = system;
   xcap.major         = major_cap;
   xcap.minor         = RESUME_APPLICATION;
   xcap.source_system = p_mcb_ptr -> system_id;
   xcap.source_major  = p_mcb_ptr -> major_capability;

   return (call_mowse_int (I$RESET, &xcap , sizeof (xcap) - 1));
}
