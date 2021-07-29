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
     Moved buffer clearing to when the destination is
     called to be compatible with same message type from remote.
  3) change(86-11-21,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added message for RESET_SUSPEND to the remote
     to be generated.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_reset)

Send a reset or resume message to a background application from a
foreground program.
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

extern local_cat  l_CAT[];
extern remote_cat r_CAT[];
extern char mysystem;

i_reset (p_msg, p_length)

struct xcap_struc *p_msg;
int p_length;
{
int cap_num;                           /* capability number of caller */
char *flags;                           /* Address of flags to adjust */
struct input_msg message;              /* Extrac message to be generated */
int  code;

/* : Verify that major capability number is valid */

   cap_num = p_msg -> major - MIN_CAPABILITY_NUMBER;
   if ((cap_num < 0) || (cap_num >= NUMBER_OF_CAT_ENTRIES))
      return(WSINVNUM);

   if (p_msg -> system == mysystem)
   {  if (l_CAT[cap_num].mcb_ptr  == NULL )
         return (WSINVNUM);
   }
   else
   {  if (r_CAT[cap_num].major_capability == 0)
         return (WSINVNUM);
   }

/* : Set the appropriate reset bit for the destination capability */

   if (p_msg -> system == mysystem )
      flags = &(l_CAT[cap_num].flags);
   else
      flags = &(r_CAT[cap_num].flags);

   if (p_msg -> minor == RESET_APPLICATION)
      *flags |= RESET_BIT;
   else if (p_msg -> minor == RESUME_APPLICATION)
   {  if (*flags & SUSPENDED_BIT)
         *flags &= ~SUSPENDED_BIT;
      else
         return (WSNOSPND);
   }

/* : Send the message */

   if (code = send_i_mess (p_msg, p_length, NULL, 0))
      return (code);

/* : If it was a RESUME for a local capability, send a RESET_SUSPEND to
     the remote */

   if ((p_msg -> minor == RESUME_APPLICATION) && (p_msg -> system == mysystem))
   {  message.minor         = RESET_SUSPEND;
      message.major         = WSMAJCAP;
      message.system        = WSMULTICS;
      message.source_system = mysystem;
      message.source_major  = p_msg -> major;
      message.msg_data[0]   = 0;
      code = send_i_mess (&message, 5, NULL, 0);
   }

   return (code);
}
