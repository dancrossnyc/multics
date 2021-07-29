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
  2) change(86-12-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Numerous minor changes to get it to work.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (getstat)

Sends a message to the specified background application requesting status.
*/

/* : NOTES

This routine will loop while waiting for a reply and can therefore only be 
called by a foreground application.
*/

#include <dos.h>
#include <ws.h>
#include <ws_msg.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <ws_error.h>
#include <wsmincap.h>

#define  POLL_TIMER   4000

getstat (p_major_num, p_status_request, p_status_reply)

int  p_major_num;                        /* Capability number to get status from */
char *p_status_request;                  /* Request string */
char *p_status_reply;                    /* Reply string */
{
int code;                              /* return code */
int i;                                 /* loop counter*/
int system;                            /* System of destination */
int major;                             /* Major index of destination */
int msg_len;                           /* Length of status request */
char *s1;                              /* pointer to source string for copy */
char *d1;                              /* pointer to destination string for copy */
struct packet_msg msg;                 /* structure for message */
struct gbgmsg_struc smsg;              /* status reply structure */

/* : copy reply string to mowse buffer and extract destination system and cap */

   msg_len = stccpy (msg.msg_data, p_status_request, WSPAKSIZ);
   c_unpack (&system, &major, p_major_num);

   msg.system = (char)(system);
   msg.major = (char)(major);
   msg.minor = GET_STATUS;
   msg.source_system = WSIBMPC;
   msg.source_major = WSMAJCAP;

/* : call user_interrupt to send data afet getting the length of the message
     (128 is the number of chars in the data field, 2 removes the address 
     overhead) */

   msg_len = msg_len + sizeof (struct packet_msg) - 128 - 2;
   if (code = call_mowse_int (I$SENDBG, &msg , msg_len))
      return (code);

/* : Now ask MOWSE to give us the status message when it arrives. This involves 
     looping in a request loop for status. In order to allow MOWSE interrupts, 
     there is a "do nothing" loop. */

   smsg.bg_type = STATUS_REPLY;
   code = WSNOMESS;
   while (code == WSNOMESS) {
      for (i = 0; i < POLL_TIMER; i++);
      code = call_mowse_int (I$GETBGMES, &smsg , sizeof (struct gbgmsg_struc));
   }

/* : Copy reply status back to caller's buffer */

   s1 = smsg.bgmsg;
   d1 = p_status_reply;
   for (i= 0; i < smsg.length; i++)
      *d1++ = *s1++;

   *d1++ = 0;

   return(0);
}
