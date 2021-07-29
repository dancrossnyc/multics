/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-11-24,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Check to make sure that it is safe to send
     to the destination (is it suspended?).
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (sendimsg):

This module is used by internal mowse programs to send an internal mowse 
minor capability message to another application.
*/

#include <stdio.h>
#include <dos.h>
#include <ws.h>
#include <alloc.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <ws_fgb.h>
#include <cat.h>
#include <wsmincap.h>

extern struct allocstr *lcaptr;
extern struct fgbstr   *lcbfptr,
                       *lcblptr;
extern char            mysystem;
extern local_cat       l_CAT[];        /* Local CAT */
extern remote_cat      r_CAT[];        /* Remote CAT */

send_i_mess (p_message,p_message_len,p_data,p_data_len)

struct input_msg  *p_message;          /* Message header */
int     p_message_len;                 /* length of header */
char    *p_data;                       /* Data in message */
int     p_data_len;                    /* Length of data in message */
{
char *flags;                           /* Flags of application */
char header[6];                        /* Header string */
char *data_ptrs[2];                    /* Message pieces */
int  data_lens[2];                     /* Message piece lengths */
int  major;                            /* Major capability index into CAT */
int  error;
int  total_len;                        /* Combined length of message */
int  i;
char *c1;                              /* Copy pointers */
char *c2;
struct fgbstr *bufp;                   /* Mesage buffer for local messages */
struct fgbstr *lcbp1;                  /* Temp pointer for local message insertion */

char *wsalloc();


/* : Get the flags of the destination application, at the same time, make
     sure the capability exists */

   major = (int) (p_message -> major) - MIN_CAPABILITY_NUMBER;
   if ((major < 0) || (major > NUMBER_OF_CAT_ENTRIES))
   {  if (p_message -> major != WSMAJCAP)
         return (WSINVNUM);
      flags = NULL;
   }
   else if (p_message -> system == mysystem)
   {  if (!l_CAT[major].mcb_ptr)
         return (WSINVNUM);
      flags = &(l_CAT[major].flags);
   }
   else
   {  if (!r_CAT[major].major_capability)
         return (WSINVNUM);
      flags = &(r_CAT[major].flags);
   }

/* : If something about the flags says that we shouldn't send, return suspended */

   if (flags)
   {  if ((*flags & (SUSPENDED_BIT | RESET_BIT))
         && (p_message -> minor != TERMINATE_APPLICATION)
         && (p_message -> minor != RESUME_APPLICATION)
         && (p_message -> minor != SUSPEND_APPLICATION)
         && (p_message -> minor != RESET_APPLICATION)
         && (p_message -> minor != FAIL_CAPABILITY))
      {
         return (WSSUSPND);
      }
   }

/* : if message is addressed to this system  */

   if (p_message -> system == mysystem) {
      total_len = p_message_len + p_data_len;
      bufp = (struct fgbstr *)wsalloc (lcaptr,total_len+sizeof(struct fgbstr));

/* : - copy to local capability buffer */

      if (bufp != NULL) {
         c1 = (char *) p_message;
         c2 = &bufp -> fgb_char[0];
         for (i = 0;i < p_message_len;i++) {
            *c2++ = *c1++;
         }
         c1 = p_data;
         for (i = 0;i < p_data_len;i++) {
            *c2++ = *c1++;
         }
         bufp -> fgb_length = total_len;

/* : - link to previous local capability buffers  */

         bufp -> fgb_next = NULL;
         if (lcbfptr == NULL) {        /* Head */
            lcbfptr = bufp;
            lcblptr = bufp;
         }
         else {                        /* Tail */
            lcbp1 = lcblptr;
            lcbp1 -> fgb_next = bufp;
            lcblptr = bufp;
         }
         return(0);
      }
      return(WSBUFOVR);
   }
/* : else message is addressed to remote system
     - set up structure for snddat */

   else {
      data_ptrs[0] = (char *) p_message;
      data_lens[0] = p_message_len;
      data_ptrs[1] = p_data;
      data_lens[1] = p_data_len;

/* : - while (message not sent because window full) try and send 2 messages */
   while (snddat (BG, 2 ,&data_ptrs,&data_lens) == 1);

/* : - return with error code
     -- = 0, if message sent
     -- = WSBUFOVR, if message too long */

   if (error == -1)
      return (WSBUFOVR);
   return(0);
   }
}
