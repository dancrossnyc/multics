/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-11,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-11-15,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Removed removal of pending messages for
     the application to be suspended.
  3) change(86-11-21,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added message for SET_SUSPEND to remote.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_suspnd)

Perform the necessary action to suspend an application, setting its suspend 
flag and forwarding a message to the remote system indicating the change.
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <ws_dcls.h>
#include <ws_fgb.h>
#include <cat.h>
#include <wsmincap.h>

#define HEADER_LENGTH    5             /* Length of message header */

extern local_cat  l_CAT[];             /* Local CAT */
extern remote_cat r_CAT[];             /* Remote CAT */
extern char mysystem;

i_suspnd (p_suspend_message)

struct xcap_struc *p_suspend_message;
{
char *flags;                           /* Pointer to flags field in CAT */
int  cap_num;                          /* Capability index */
int  code;
struct input_msg message;              /* Message to remote to set bit */

/* : Verify that major capability number is valid */

   cap_num = p_suspend_message -> major - MIN_CAPABILITY_NUMBER;
   if ((cap_num < 0) || (cap_num > NUMBER_OF_CAT_ENTRIES))
      return(WSINVNUM);

/* : Verify that the application that the capability number refers to exists
     and get the flag field */

   if (p_suspend_message -> system == mysystem)
   {  if (l_CAT[cap_num].mcb_ptr  == NULL)
         return (WSINVNUM);
      flags = &(l_CAT[cap_num].flags);
   }
   else
   {  if (r_CAT[cap_num].major_capability == 0)
         return (WSINVNUM);
      flags = &(r_CAT[cap_num].flags);
   }

/* : Verify that the application is not already suspended */

   if (*flags & SUSPENDED_BIT)
      return (WSSUSPND);

/* : Set and send suspend message to the remote MOWSE */

   *flags |= SUSPENDED_BIT;
   if (code = send_i_mess (p_suspend_message, HEADER_LENGTH, NULL, 0))
      return (code);

/* : If the destination of the message is local, then send a message to the
     remote indicating the application is suspended */

   if (p_suspend_message -> system == mysystem)
   {  message.system        = WSMULTICS;
      message.major         = WSMAJCAP;
      message.minor         = SET_SUSPEND;
      message.source_system = mysystem;
      message.source_major  = p_suspend_message -> major;
      message.msg_data[0]   = 0;
      code = send_i_mess (&message, HEADER_LENGTH, NULL, 0);
   }

   return (code);
}
