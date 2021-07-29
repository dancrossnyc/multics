/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-05,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* PROCEDURE FUNCTION (execap):

Sends a minor capability message to the specified major capability with the
arguments provided.
*/

#include <stdio.h>
#include <ws.h>
#include <alloc.h>
#include <ws_mcb.h>
#include <ws_error.h>
#include <ws_func.h>
#include <ws_dcls.h>
#include <wsmincap.h>

#define HEADER_LENGTH 6                /* Header information length */

extern char local_system;              /* Identity of PC system */

execap (p_major_num, p_minor_num, p_arg_ptr, p_arg_len, p_mcb_ptr)

int  p_major_num;                      /* Destination major capability number */
int  p_minor_num;                      /* Destination minor capability number */
char *p_arg_ptr;                       /* Pointer to argument data */
int  p_arg_len;                        /* Length of argument data */
mcb  *p_mcb_ptr;                       /* Pointer to application's MCB */
{
struct xcap_struc xcap;
char              *c1;
char              *c2;
char              *temp_p_arg_ptr;
int               i;
int               code;
int               copylen;
int               header_size;
struct linklst    *new_msg_portion;
struct linklst    *dead_buffer;
struct linklst    *pending_msg_buffer;
struct linklst    *pending_msg_link;
struct linklst    *portion;
struct linklst    *buffer_portion();
int j;

   code = 0;

/* Validate the mcb pointer */

   if (p_mcb_ptr == NULL)
      return (WSINVMCB);

/* Move and validate the passed arguments into the execap structure */

   xcap.system =  (p_major_num >> 8);
   if ((xcap.system != WSIBMPC) && (xcap.system != WSMULTICS))
      return (WSINVSYS);

   xcap.major = (char) (0xff & p_major_num);
   if ((xcap.major < WSMAJCAP) || (xcap.major > MAX_CAPABILITY_NUMBER - 1))
      return (WSINVNUM);

/* If there are any pending messages from the caller and the message to
   be sent is longer than on packet, then return WSBUFOVR because we can't
   have multiple long messages pending due to protocol */

   if ((p_arg_len > WSPAKSIZ) && (p_mcb_ptr -> outbuff != NULL))
      return (WSBUFOVR);

/* Copy the argument string into the structure. If the string is longer than
   the maximum packet size, then copy the first portion of the string to the
   structure, and the remainder of the string into as many buffers as are
     required to contain the entire message. */

/* Find end of buffer list */

   pending_msg_buffer = p_mcb_ptr -> outbuff;
   if (pending_msg_buffer != NULL)
       while (pending_msg_buffer->nextbuf != NULL)
          pending_msg_buffer = pending_msg_buffer -> nextbuf;
      
   pending_msg_link = NULL;
   temp_p_arg_ptr = p_arg_ptr;

/* Split the message into portions */

   while (p_arg_len >= 0)
   {  if (p_arg_len > (WSPAKSIZ - HEADER_LENGTH))
      {  copylen = WSPAKSIZ - HEADER_LENGTH;
         xcap.minor = PARTIAL_MESSAGE;
         xcap.source_system = p_minor_num;
         xcap.source_major = p_mcb_ptr->system_id;
         xcap.xcapstr[0] = p_mcb_ptr->major_capability;
         header_size = HEADER_LENGTH;
      }
      else
      {  copylen = p_arg_len;
         xcap.minor = p_minor_num;
         xcap.source_system = p_mcb_ptr->system_id;
         xcap.source_major = p_mcb_ptr->major_capability;
         header_size = 5;
         p_arg_len = -1;                                /* terminate loop */
      }

/* Copy data from user space into his message buffer space */

      new_msg_portion = 
         (struct linklst *) wsalloc (p_mcb_ptr -> outalloc, header_size+copylen+sizeof(struct linklst));
      
/* Copy header information */

      if (new_msg_portion != NULL)
      {  c1 = (char *) &xcap;
         c2 = &(new_msg_portion -> lldata[0]);
         for (i = 0; i < header_size; i++)
            c2[i] = c1[i];

/* Copy data information */

         c1 = temp_p_arg_ptr;
         for (i = 0; i < copylen; i++)
            c2[i + header_size] = c1[i];

         p_arg_len = p_arg_len - copylen;
         temp_p_arg_ptr = &(c1[i]);

/* Initialize link to hold portion of message */

         new_msg_portion->nextbuf = NULL;
         new_msg_portion->nextlink = NULL;
         new_msg_portion->linkused = copylen + header_size;


/* Insert buffer into linked list */

         if (pending_msg_buffer == NULL)              /* First in chain */
         {  p_mcb_ptr->outbuff = new_msg_portion;
            pending_msg_buffer = new_msg_portion;
            pending_msg_link = new_msg_portion;
         }
         else
         {  if (pending_msg_link == NULL)             /* Last in chain */
            {  pending_msg_link = pending_msg_buffer;
               while (pending_msg_link->nextlink != NULL)
                  pending_msg_link = pending_msg_link->nextlink;
            }
            pending_msg_link->nextlink = new_msg_portion;
            pending_msg_link = new_msg_portion;
         }
      }

/* If can't copy into message buffer space then return all buffers back to
       free queue */

      else
      {  dead_buffer = p_mcb_ptr->outbuff;
         while (dead_buffer != NULL)
         {  pending_msg_buffer = dead_buffer->nextlink;
            llpfree(p_mcb_ptr -> outalloc,&p_mcb_ptr->outbuff,dead_buffer);
            dead_buffer = pending_msg_buffer;
         }
         return (WSBUFOVR);
      }
   }

/* Extract the first portion of the message to send */

   portion = buffer_portion(p_mcb_ptr, pending_msg_buffer);
   code = call_mowse_int (I$EXECAP, portion->lldata, portion->linkused);
   wsfree (p_mcb_ptr -> outalloc, portion);
   if (code)
      llpfree (p_mcb_ptr -> outalloc, &(p_mcb_ptr -> outbuff), portion -> nextlink);
   return (code);
}

/* PROCEDURE FUNCTION (buffer_portion)

Extract a portion of a message from the head of the pending portion queue.
*/

struct linklst *buffer_portion(p_mcb_ptr, p_buffer)

mcb *p_mcb_ptr;
struct linklst *p_buffer;
{
struct linklst *portion;
struct linklst *msg_buffer_prev;

/* If p_mcb_ptr is null then return */

   if (p_mcb_ptr == NULL)
      return(NULL);

/* If buffer is null then return */

   if (p_buffer == NULL)
      return(NULL);

/* Get portion of buffer to send */

   portion = p_buffer;

/* Fix up chain from which portion was taken */

   msg_buffer_prev = p_mcb_ptr->outbuff;
   if (msg_buffer_prev == NULL)
      return(NULL);

/* Search for either an equality to the current buffer, or NULL which signals
   the trailing end of the message. */

   while (msg_buffer_prev->nextbuf != p_buffer)
      if ((msg_buffer_prev = msg_buffer_prev->nextbuf) == NULL)
         break;

   if (msg_buffer_prev == NULL)
      p_mcb_ptr->outbuff = p_buffer->nextlink;
   else
   {  msg_buffer_prev->nextbuf = p_buffer->nextlink;
      p_buffer->nextlink->nextbuf = p_buffer->nextbuf;
   }

   return(portion);
}    
