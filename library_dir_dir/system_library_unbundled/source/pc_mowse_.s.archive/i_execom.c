/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-01,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-07-24,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Call to send_i_mess.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_execom)

User interrupt routine that creates an execute_command message.
*/

#include <ws.h>
#include <ws_error.h>
#include <ws_dcls.h>
#include <ws_msg.h>
#include <wsmincap.h>

#define MAX_CMD_LEN     128

extern char mysystem;

i_execom (p_param)

struct execom_struc *p_param;
{
static int CMD_ID_CNT;
struct execom_msg exemsg;

/* : Increment command counter  */

   p_param -> cmd_id = ++CMD_ID_CNT;

/* : Initialize message */

   exemsg.system = (char) p_param -> system;
   exemsg.major = WSMAJCAP;
   exemsg.minor = WS_EXECUTE_COMMAND;
   exemsg.source_major = (char) p_param -> major;
   exemsg.source_system = mysystem;
   exemsg.cmd_id = p_param -> cmd_id;

/* : Send message */

   p_param -> status = send_i_mess (&exemsg.system, sizeof(exemsg) - 1,
      &p_param -> command,p_param -> com_len);

   return (p_param -> status);
}
