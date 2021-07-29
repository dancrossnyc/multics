/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-11-11,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (external)

Examine the application's message in MOWSE's address space to reset appropriate
flags (stored in MOWSE data space).
*/

#include <stdio.h>
#include <dos.h>
#include <wsmincap.h>
#include <cat.h>
#include <ws_msg.h>
#include <ws_error.h>

#define MESSAGE_HEADER_LENGTH 5

extern char mysystem;
extern local_cat l_cat[];
extern remote_cat r_cat[];

external_mowse (p_cat, p_message, p_message_len)

local_cat *p_cat;                      /* Pointer to applciation cat entry */
char *p_message;                       /* Message */
int  p_message_len;                    /* Length of message */
{
struct input_msg *message;

   message = (struct input_msg *) p_message;

/* : Switch to the appropriate handler for each minor */

   switch (message -> minor)
   {
      case TERMINATE_APPLICATION:

         return (extern_terminate (p_cat, message, p_message_len));

      case RESET_APPLICATION:

         return (extern_reset (p_cat, p_message, p_message_len));

      case SUSPEND_APPLICATION:

         return (extern_suspend (p_cat, p_message, p_message_len));

      case RESUME_APPLICATION:

         return (extern_resume (p_cat, p_message, p_message_len));

      default:

         return (wsexecap (p_cat, p_message, p_message_len));
   }
}
/**/
/* : PROCEDURE FUNCTION (extern_reset)

Perform the operations necessary to handle resetting the application.
*/

extern_reset (p_cat, p_message, p_message_len)

local_cat *p_cat;
struct input_msg *p_message;
int p_message_len;
{
struct input_msg message;

/* : Pass the message on to the application */

   wsexecap (p_cat, p_message, p_message_len);

/* : Send the reset reply to the source only if it is from a remote system */

   if (p_message -> source_system == mysystem)
   {  p_cat -> flags &= ~RESET_BIT;
      return (0);
   }

   message.system        = WSMULTICS;
   message.major         = WSMAJCAP;
   message.minor         = RESET_REPLY;
   message.source_system = mysystem;
   message.source_major  = p_message -> major;
   message.msg_data[0]   = 0;

   return (send_i_mess (&message, MESSAGE_HEADER_LENGTH, NULL, 0));
}
/**/
/* : PROCEDURE FUNCTION (extern_suspend)

Perform the operations necessary to suspending the application.
*/

/* : NOTES

POTENTIAL PROBLEM:  if a suspended application is refused messages then
a calling application which is not aware of this will not see an answer
until this is resumed.
*/

extern_suspend (p_cat, p_message, p_message_len)

local_cat *p_cat;                      /* CAT entry of application */
struct input_msg *p_message;           /* Message */
int  p_message_len;                    /* Length of message */
{
int major_sender;

/* : Set the suspend bit in the flags */

   p_cat -> flags |= SUSPENDED_BIT;

/* : Send the message on to the application */

   return (wsexecap (p_cat, p_message, p_message_len));
}
/**/
/* : PROCEDURE FUNCTION (extern_resuem)

Perform the operations necessary to resuming the application.
*/

/* : NOTES

POTENTIAL PROBLEM:  if a suspended application is refused messages then
a calling application which is not aware of this will not see an answer
until this is resumed.
*/

extern_resume (p_cat, p_message, p_message_len)

local_cat *p_cat;                      /* CAT entry of application */
struct input_msg *p_message;           /* Message */
int  p_message_len;                    /* Length of message */
{
int major_sender;

/* : Set the suspend bit in the flags */

   p_cat -> flags &= ~SUSPENDED_BIT;

/* : Send the message on to the application */

   return (wsexecap (p_cat, p_message, p_message_len));
}
/**/
/* : PROCEDURE FUNCTION (extern_terminate)

Perform the operations necessary to terminating the application.
*/

extern_terminate (p_cat, p_message, p_message_len)

local_cat *p_cat;                      /* CAT entry of application */
struct input_msg *p_message;           /* Message */
int  p_message_len;                    /* Length of message */
{

   p_cat -> flags |= MCB_TERMINATE;
   p_cat -> flags &= ~MCB_SUSPEND;

   return (wsexecap (p_cat, p_message, p_message_len));
}
