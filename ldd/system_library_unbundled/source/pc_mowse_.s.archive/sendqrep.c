/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-12,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (sendqrep)

Send a user' response to a query message to the originator of the background 
query message. The reponse must be less than or equal to WSPAKSIZ characters 
in length.
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <ws_msg.h>
#include <ws_func.h>
#include <ws_mcb.h>
#include <wsmincap.h>
#include <ws_error.h>

#define HEADER_SIZE 5

sendqrep (p_reply_string, p_major_num)

char *p_reply_string;     /* Reply to be sent */
int p_major_num;          /* Destination of reply */
{
int code;                 /* return code */
int msg_len;              /* Length of reply string */
int system_id;            /* Temporary system id */
int major;                /* Temporary major */
struct packet_msg msg;    /* structure for message */

/* : copy reply string to mowse buffer */

   msg_len = stccpy (msg.msg_data, p_reply_string, WSPAKSIZ);

   c_unpack (&system_id, &major, p_major_num);
   msg.system = system_id;
   msg.major = major;
   msg.minor = QUERY_REPLY;
   msg.source_system = WSIBMPC;
   msg.source_major = WSMAJCAP;

/* : call user_interrupt to send data (don't send the null) */

   return (call_mowse_int (I$SENDBG, &msg , msg_len - 1 + HEADER_SIZE));
}
