/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */


/* : PROCEDURE FUNCTION (getbgmes)

Retrieves a background message from the MOWSE background message queue. 
A background message is one which is sent to the foreground application by 
a background application.  There are two types of background messages, those 
that do require a response (WSINFO), and those that require a response 
(WSQUERY) from the foreground application.
*/

#include <dos.h>
#include <ws.h>
#include <ws_dcls.h>
#include <ws_func.h>

getbgmes (p_string, p_type, p_sender_major)

char *p_string;                        /* Message */
int *p_type;                           /* Type of message */
int *p_sender_major;                   /* Sender of message */
{
int code;
int i;
char *c1;
char *c2;
struct gbgmsg_struc gbgmsgs;

/* : call user_interrupt to get data returning on any errors */

   gbgmsgs.bg_type = 0;  /* ask for any background message, except status */
   code = call_mowse_int (I$GETBGMES,&gbgmsgs,sizeof(gbgmsgs));
   if (code)
      return (code);

/* : copy the message into the caller's buffer and
     set the last position in buffer to null. */

   c1 = &gbgmsgs.bgmsg[0];
   c2 = p_string;
   for (i= 0; i<gbgmsgs.length; i++)
      *c2++ = *c1++;

   *c2 = '\0';

/* : return the message type and sender major to the caller */

   *p_type = gbgmsgs.bg_type;
   *p_sender_major = gbgmsgs.sender_major;

   return(0);
}
