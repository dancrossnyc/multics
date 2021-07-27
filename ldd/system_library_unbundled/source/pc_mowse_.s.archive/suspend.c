/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */


/* HISTORY COMMENTS:
  1) change(86-09-27,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (suspend)

Tells MOWSE to suspend the specified application.  The suspended application 
is notified by the dedicated minor capability SUSPEND_APPLICATION and will 
not be allowed to send or receive messages.
*/

/* : NOTES
       
A call to the mowse library routine 'resume' by some other application is
required to allow the application suspended to send and receive messages.
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_mcb.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>

extern char local_system;

suspend (major_number, mcb_ptr)

int major_number;
mcb *mcb_ptr;
{
int code;                              /* return code */
int system;                            /* Temp system id */
int major;                             /* Temp major number */
struct xcap_struc suspend_message;     /* MOWSE parameters */

/* : Verify the validity of the caller */

   if (mcb_ptr == NULL)
      return (WSINVMCB);

   if (code = c_unpack (&system, &major, major_number))
      return (code);


/* : Send the message */

   suspend_message.system        = system;
   suspend_message.major         = major;
   suspend_message.minor         = SUSPEND_APPLICATION;
   suspend_message.source_system = mcb_ptr -> system_id;
   suspend_message.source_major  = mcb_ptr -> major_capability;
   return (call_mowse_int (I$SUSPEND, &suspend_message , sizeof (suspend_message)));
}

