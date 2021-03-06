/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-13,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (wssleep):

Requests MOWSE to suspend the application for a fixed period of time. While 
the application is sleeping, all messages received from other applications 
(except MOWSE) will be ignored. Mowse will send a message to the remote 
system(s) to inform them that the application is sleeping.  The sleeping 
application will be awakened by the minor capability WAKE_UP when the time has 
expired.
*/

/* : RETURNS:

     0, if message sent
     WSINVNUM, if capability number is invalid
     WSNOSPND, if capability is not currently suspended
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>

extern char local_system;

wssleep (time, mcb_ptr)
int time;
mcb *mcb_ptr;
{
struct sleep_struc sleep_data; /* structure to hold sleep request */

/* : Check that the sleep time is valid */
   if (time <= 0)
      return (WSINVTIM);

/* :  Send sleep request to MOWSE */
   sleep_data.time = time;
   sleep_data.source_system = mcb_ptr -> system_id;
   sleep_data.source_major  = mcb_ptr -> major_capability;
   sleep_data.minor = WS_SET_SLEEP_FLAG;

   return (call_mowse_int (I$SLEEP, &sleep_data , sizeof (sleep_data)));

/* : END */
}
 