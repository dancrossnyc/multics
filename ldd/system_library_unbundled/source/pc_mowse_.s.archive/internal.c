/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-12-26,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-06-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     remove puttdata support.
  3) change(86-10-21,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     connect request support.
  4) change(86-10-22,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     handle invalid requests.
  5) change(86-11-14,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     handle set/reset sleep bit
                                                   END HISTORY COMMENTS */
/* : PROCEDURE FUNCTION (internal_mowse)
                                                                
Determine the destination of the internal mowse message depending on the 
minor capability number and direct control appropriately.
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <ws_msg.h>
#include <ws_error.h>
#include <wsmincap.h>
#include <cat.h>

#define HEADER_LENGTH 5                /* Length of message header */

extern remote_cat r_CAT[];
extern local_cat l_CAT[];

internal_mowse(length,mowse_msgp)

int     length;                        /* Length of message */
struct input_msg *mowse_msgp;          /* message */
{
int  err_code;
int  i;
int  cap_index;
char return_status;
char capname[33];
int  capname_length;
int  cap_num;
int  connect_request_len;
char connect_request_string[WSPAKSIZ];

struct alter_cat_msg *acp;
struct null_msg connect_response_msg;
struct null_msg mowse_message;

/* : switch(minor_capability) */

   switch(mowse_msgp -> minor)
   {

/* : - case EXECUTE_COMMAND   */

      case WS_EXECUTE_COMMAND :

         EXECUTE_COMMAND(length,mowse_msgp);
         break;

/* : - case ADD_TO_REMOTE_CAT           */

      case WS_ADD_TO_RAT:

         acp = (struct alter_cat_msg *) mowse_msgp;
         return (add_to_remote_cat(acp -> source_system,acp -> rat_major,&acp -> major_name[0]));

/* : DELETE_FROM_REMOTE_CAT */

      case WS_DELETE_FROM_RAT:

         acp = (struct alter_cat_msg *) mowse_msgp;
         if (acp -> source_system == WSIBMPC)
            return(WSINVSYS);
         cap_index = acp -> rat_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return(WSINVNUM);
         r_CAT[cap_index].major_capability = 0;
         r_CAT[cap_index].system_id = 0;
         r_CAT[cap_index].flags = 0;
         i = 0;
         while (r_CAT[cap_index].capability_name[i] != 0)
            r_CAT[cap_index].capability_name[i++] = 0;
         return(0);

/* : - case WS_SET_SLEEP_FLAG
     -- set the sleep flag on the appropriate remote appllcation */

      case WS_SET_SLEEP_FLAG:

         cap_index = (int)mowse_msgp -> source_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return (0);
         r_CAT[cap_index].flags |= SLEEPING_BIT;
         return (0);

/* : - case WS_RESET_SLEEP_FLAG
     -- set the sleep flag on the appropriate remote appllcation */

      case WS_RESET_SLEEP_FLAG:

         cap_index = (int)mowse_msgp -> source_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return (0);
         r_CAT[cap_index].flags &= ~SLEEPING_BIT;
         return (0);

/* : - case RESET_REPLY
     -- clear the flag on the source capability (message always comes
        from remote) */

      case RESET_REPLY:

         cap_index = mowse_msgp -> source_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return (0);
         r_CAT[cap_index].flags &= ~RESET_BIT;
         return (0);

/* : - case REQUEST_CONNECT
     -- get the name of the capability to send connect request to */

      case REQUEST_CONNECT:

         capname_length = min(length - HEADER_LENGTH, CAPABILITY_NAME_LENGTH);
         for (i=0; i < capname_length && mowse_msgp -> msg_data[i] != ' '; i++)
            capname[i] = mowse_msgp -> msg_data[i];
         capname[i] = '\0';

         connect_request_len = min(length-HEADER_LENGTH,WSPAKSIZ);

         for (i=0; i < connect_request_len; i++)
            connect_request_string[i] = mowse_msgp -> msg_data[i];
         connect_request_string[i] = '\0';

/* : - try to find the capability
     - If it is not found then create it */

         cap_num = find_local_capability(capname);
         err_code = 0;
         if (cap_num == 0) {
            set_dta();
            set_trap();
            err_code = system(connect_request_string);
            rst_trap();
            rst_dta();
            cap_num = find_local_capability(capname);
         }

/* : - If the capability still can't be found then send a message
       back to the capability requesting the connection indicating
       that the connect request has failed */

         if ((cap_num == 0) || (err_code != 0)) {
            connect_response_msg.system = mowse_msgp -> source_system;
            connect_response_msg.major = mowse_msgp -> source_major;
            connect_response_msg.minor = RESPONSE_CONNECT;
            connect_response_msg.source_system = WSIBMPC;
            connect_response_msg.source_major = WSMAJCAP;
            return_status = WSREJECT;
            return (send_i_mess(&connect_response_msg,HEADER_LENGTH,&return_status,1));
         }

/* : otherwise send the connect request message onto the capability
     found locally */

         mowse_message.system = WSIBMPC;
         mowse_message.major = cap_num & 0xff;
         mowse_message.minor = REQUEST_CONNECT;
         mowse_message.source_system = mowse_msgp -> source_system;
         mowse_message.source_major = mowse_msgp -> source_major;
         return (send_i_mess(&mowse_message,HEADER_LENGTH,NULL,0));

/* : case SET_SUSPEND
     - set the suspend flag in the remote cat for the sepcified cap */

      case SET_SUSPEND:

         cap_index = mowse_msgp -> source_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return (0);
         r_CAT[cap_index].flags |= SUSPENDED_BIT;
         return (0);

/* : case RESET_SUSPEND
     - reset the suspend flag in the remote cat for the sepcified cap */

      case RESET_SUSPEND:

         cap_index = mowse_msgp -> source_major - MIN_CAPABILITY_NUMBER;
         if ((cap_index < 0) || (cap_index > NUMBER_OF_CAT_ENTRIES))
            return (0);
         r_CAT[cap_index].flags &= ~SUSPENDED_BIT;
         return (0);

/* : case FAIL CAPABILITY:
     - ignore the message */

      case FAIL_CAPABILITY:

         return (0);

/* : default: Return (invalid minor capability) only if source was not internal */

      default:

         if (mowse_msgp -> source_major == WSMAJCAP)
            return (0);
         mowse_message.system = mowse_msgp -> source_system;
         mowse_message.major = mowse_msgp -> source_major;
         mowse_message.minor = FAIL_CAPABILITY;
         mowse_message.source_system = WSIBMPC;
         mowse_message.source_major = WSMAJCAP;
         return (send_i_mess(&mowse_message,HEADER_LENGTH,NULL,0));
   }
   return(0);
}
