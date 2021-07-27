/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-11-15,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Reimplementation of reset,resume,suspend,
     terminate.
  3) change(86-11-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Empty buffers when a fail is returned.
                                                   END HISTORY COMMENTS */

/* :  PROCEDURE FUNCTION (pe)

pe (pre-entry) is the MOWSE routine which is forced into the application's
address space and called before the application is called.  It is responsible
for the following:

   1) packet assembly/disassembly
   2) specialized minor capability manipulations required to be done in the
      application's space
   3) call user application with data messages

*/

#include <stdio.h>
#include <ws.h>                        /* General constants */
#include <ws_msg.h>                    /* Message structures */
#include <wsmincap.h>                  /* Minor capability definitions */
#include <ws_mcb.h>                    /* MOWSE Control BLock def */
#include <ws_func.h>                   /* User interrupt MOWSE functions */
#include <ws_error.h>                  /* Error codes */

#define TRUE  1
#define FALSE 0

pe (p_mcb_ptr, p_minor_cap, p_bufseg, p_bufoff, p_buflen)

mcb  *p_mcb_ptr;                       /* Capability's MCB */
char p_minor_cap;                      /* Minor cap of message */
int  p_bufseg;                         /* Segment address of message */
int  p_bufoff;                         /* Offset of message within segment */
int  p_buflen;                         /* Length of message */
{
int  link_info_size;
int  msg_header_size;
int  sender_major;
int  code;
struct more_msg more_message;          /* Message types for passed message */
struct input_msg *message;

/* : - if there is no handler for the message, ignore it */

   if (p_mcb_ptr -> mcb_flag & MCB_NULL_ROUTINE)
      return (0);

/* : - copy message into user space. */

   msg_header_size = sizeof(more_message)-2;
   link_info_size = sizeof(struct linklst)-2;
   peek (p_bufseg,p_bufoff,&more_message, msg_header_size);
   message = (struct input_msg *) (&more_message);

/* : - switch on the minor capability */

   switch ((int) p_minor_cap)
   {
      case CONTINUE_MESSAGE:

         return (handle_continue (p_mcb_ptr, &more_message));

      case PARTIAL_MESSAGE:

         return (handle_partial (p_mcb_ptr, p_buflen, p_bufseg, p_bufoff, 
            msg_header_size, link_info_size, &more_message));

      case RESUME_APPLICATION:

         if (!(p_mcb_ptr -> mcb_flag & MCB_SUSPEND))
            return (WSNOSPND);
         p_mcb_ptr -> mcb_flag &= ~MCB_SUSPEND;
         code = forward (p_mcb_ptr, message -> minor, &(message -> msg_data[0]),
            p_buflen, message -> source_system, message -> source_major);
         return (code);

      case SUSPEND_APPLICATION:

         if (p_mcb_ptr -> mcb_flag & MCB_SUSPEND)
            return (WSSUSPND);
         code = forward (p_mcb_ptr, message -> minor, &(message -> msg_data[0]),
            p_buflen, message -> source_system, message -> source_major);
         p_mcb_ptr -> mcb_flag |= MCB_SUSPEND;
         return (code);

      case RESET_APPLICATION:

         return (handle_reset (p_mcb_ptr, message, p_buflen));

      case FAIL_CAPABILITY:
      case SYSTEM_ERROR:

         return (handle_fail (p_mcb_ptr, message, p_buflen));

      case TERMINATE_APPLICATION:

         code = forward (p_mcb_ptr, message -> minor, &(message -> msg_data[0]), 
            p_buflen, message -> source_system, message -> source_major);
         return (code);

      default:

         return (handle_default (p_mcb_ptr, p_buflen, p_bufseg, p_bufoff, p_minor_cap,
            msg_header_size, link_info_size, &more_message));
   }
}
/**/
/* : PROCEDURE FUNCTION (llpfree)

Free the space occupied by the link list pointed to by llp.  The space that
is to be freed is a list that is trailing behind the the llp node:
         -------------------
first ->|   |   |   |   |   |
         -------------------
          |
          V
         -------------------
  llp ->|   |   |   |   |   |        <= free this list into core
         -------------------
          |
          V
         -------------------
        |   |   |   |   |   |
         -------------------
*/

llpfree (core,first,llp)

struct allocstr *core;                 /* Where to free into */
struct linklst **first;                /* First node in list */
struct linklst *llp;                   /* Node to free */
{
struct linklst *llp1;                  /* Search node pointer */

/* : Find the predecessor to the node to be freed and adjust the pointers
     to go around the node to be freed */

   llp1 = *first;
   if (llp1 == llp)
      *first = llp1 -> nextbuf;
   else
   {  while (llp1 != NULL)
      {  if (llp1->nextbuf == llp)
         {  llp1->nextbuf = llp->nextbuf;
            break;
         }
         llp1 = llp1->nextbuf;
      }
   }

/* : Free all of the "sub-nodes" in the trailing list */

   while (llp != NULL)
   {  llp1 = llp->nextlink;
      wsfree(core,llp);
      llp = llp1;
   }
}
/**/
/* : PROCEDURE FUNCTION (forward)

Simply pass the message immediately to the application.  It shouldn't be
stored in the buffers as there is no need and the application needs immediate
knowledge.
*/

forward (p_mcb_ptr, p_minor, p_data, p_data_len, p_source_system, p_source_major)

mcb  *p_mcb_ptr;
char p_minor;
char *p_data;
int  p_data_len;
char p_source_system;
char p_source_major;
{
int major_sender;

   major_sender = (p_source_system << 8) | p_source_major;

/* : If suspended and not TERMINATE or RESUME, send FAIL_CAPABILITY to source */

   if ((p_mcb_ptr -> mcb_flag & MCB_SUSPEND) && (p_minor != TERMINATE_APPLICATION)
      && (p_minor != RESUME_APPLICATION) && (p_minor != SUSPEND_APPLICATION))
   {
      return (execap (major_sender, FAIL_CAPABILITY, NULL, 0, p_mcb_ptr));
   }

/* : Else forward to application */

   (*p_mcb_ptr -> application_entry)
      ((int)p_minor, major_sender, p_data, p_data_len,
      p_mcb_ptr -> data_block_ptr, p_mcb_ptr);

   return (0);
}
/**/
/* : PROCEDURE FUNCTION (handle_reset)

Perform the operations necessary to handle resetting the application.
*/

/* : NOTES

POTENTIAL BUG: reset clears out all of the applications input and output
   buffers.  If there are partial long messages being sent/received, this
   may cause problems in the assembly/dissasembly
*/

handle_reset (p_mcb_ptr, p_message, p_message_len)

mcb  *p_mcb_ptr;
struct input_msg *p_message;
int p_message_len;
{
struct linklst *msg_buffer;
struct linklst *next_buf;

/* : Clear out the applications input buffers */

   msg_buffer = p_mcb_ptr -> inbuff;
   while (msg_buffer != NULL)
   {  next_buf = msg_buffer -> nextbuf;
      llpfree (p_mcb_ptr -> inalloc, &(p_mcb_ptr -> inbuff), msg_buffer);
      msg_buffer = next_buf;
   }
   p_mcb_ptr -> inbuff = NULL;

/* : Clear out the applications output buffers */

   msg_buffer = p_mcb_ptr -> outbuff;
   while (msg_buffer != NULL)
   {  next_buf = msg_buffer -> nextbuf;
      llpfree (p_mcb_ptr -> outalloc, &(p_mcb_ptr -> outbuff), msg_buffer);
      msg_buffer = next_buf;
   }
   p_mcb_ptr -> outbuff = NULL;

/* : Pass the RESET_APPLICATION message to the capability */

   forward (p_mcb_ptr, p_message -> minor, &(p_message -> msg_data [0]), 
      p_message_len, p_message -> source_system, p_message -> source_major);
}
/**/
/* : PROCEDURE FUNCTION (handle_fail)

Perform the operations necessary to handle a fail capability,  this means
clearing out the applications output buffers.
*/

handle_fail (p_mcb_ptr, p_message, p_message_len)

mcb  *p_mcb_ptr;
struct input_msg *p_message;
int p_message_len;
{
struct linklst *msg_buffer;
struct linklst *next_buf;

/* : Clear out the applications output buffers */

   msg_buffer = p_mcb_ptr -> outbuff;
   while (msg_buffer != NULL)
   {  next_buf = msg_buffer -> nextbuf;
      llpfree (p_mcb_ptr -> outalloc, &(p_mcb_ptr -> outbuff), msg_buffer);
      msg_buffer = next_buf;
   }
   p_mcb_ptr -> outbuff = NULL;

/* : Pass the FAIL_CAPABILITY message to the capability */

   forward (p_mcb_ptr, p_message -> minor, &(p_message -> msg_data [0]), 
      p_message_len, p_message -> source_system, p_message -> source_major);
}
/**/
/* : PROCEDURE FUNCTION (handle_partial)

Perform the necessary functions when a partial message for the application
arrives.

     -- header_format = system|major|CONTINUE|source_system|source_minor|minor

*/

handle_partial (p_mcb_ptr, p_buflen, p_bufseg, p_bufoff, p_msg_hdr_size, p_link_info_size, p_more_message)

mcb  *p_mcb_ptr;                       /* Capability's MCB */
int  p_bufseg;                         /* Segment address of message */
int  p_bufoff;                         /* Offset of message within segment */
int  p_buflen;                         /* Length of message */
int  p_msg_hdr_size;
int  p_link_info_size;
struct more_msg *p_more_message;

{
int  found;
int  done;
int  buffer_size;
int  amount_to_copy;
int  major_sender;
int  i;
char *more_msg_header1;
char *more_msg_header2;
struct linklst *msg_buffer;
struct linklst *prev_msg_buffer;
struct linklst *new_msg_buffer;


   msg_buffer = p_mcb_ptr->inbuff;
   prev_msg_buffer = p_mcb_ptr->inbuff;

/* : Find if this belongs to a message currently being built */

   found = done = FALSE;
   while ((found == FALSE) && (done == FALSE))
   {  if (msg_buffer == NULL)
         done = TRUE;
      else
      {  more_msg_header1 = (char *) msg_buffer->lldata;
         more_msg_header2 = (char *) p_more_message;

         if (!strncmp(more_msg_header1,more_msg_header2,p_msg_hdr_size))
            found = done = TRUE;
         else
         {  prev_msg_buffer = msg_buffer;
            msg_buffer = msg_buffer->nextbuf;
         }
      }
   }

/* : If done & !found then this is a new long message and allocate space */

   if (found == FALSE)
   {  buffer_size = p_mcb_ptr->inbuff_length + p_link_info_size + p_msg_hdr_size;
      new_msg_buffer = (struct linklst *) wsalloc(p_mcb_ptr->inalloc, buffer_size);

/* : - if succeeded then copy the message in */

      if (new_msg_buffer != NULL)
      {  new_msg_buffer->linksize = buffer_size;
         amount_to_copy = min(buffer_size-p_link_info_size,p_buflen);
         peek(p_bufseg,p_bufoff,new_msg_buffer->lldata,amount_to_copy);
         new_msg_buffer->nextlink = NULL;
         new_msg_buffer->nextbuf = NULL;
         new_msg_buffer->linkused = amount_to_copy;

         if (prev_msg_buffer == p_mcb_ptr->inbuff)
            p_mcb_ptr->inbuff = new_msg_buffer;
         else 
            prev_msg_buffer->nextbuf = new_msg_buffer;
      }

/* : - else make a new message header for it for later */

      else
      {  buffer_size = p_msg_hdr_size+p_link_info_size+1;
         new_msg_buffer = (struct linklst *) wsalloc(p_mcb_ptr->inalloc,buffer_size);

         if (new_msg_buffer != NULL)
         {  for (i=0; i<p_msg_hdr_size; i++)
               new_msg_buffer->lldata[i] = *(((char *) p_more_message)+i);

            new_msg_buffer->nextlink = NULL;
            new_msg_buffer->nextbuf = NULL;
            new_msg_buffer->linkused = buffer_size;
            new_msg_buffer->linksize = buffer_size;

            if (prev_msg_buffer == p_mcb_ptr->inbuff)
               p_mcb_ptr->inbuff = new_msg_buffer;
            else 
               prev_msg_buffer->nextbuf = new_msg_buffer;
         }

/* : -- if can't do that then send FAIL_CAPABILITY to the source */

         else
         {  major_sender = p_more_message -> source_major;
            major_sender = (major_sender << 8) | p_more_message -> minor;
            return (execap(major_sender, FAIL_CAPABILITY, NULL, 0, p_mcb_ptr));
         }
      }
   }
            
/* : - If a buffer is found then add as much as possible to the buffer */

   if (found == TRUE)
   {  amount_to_copy = min((p_buflen-p_msg_hdr_size),(msg_buffer->linksize-msg_buffer->linkused-p_link_info_size));
      peek(p_bufseg,p_bufoff+p_msg_hdr_size,&(msg_buffer->lldata[msg_buffer->linkused]),
         amount_to_copy);
      msg_buffer->linkused += amount_to_copy;
   }

/* : - Send a <CONTINUE_MESSAGE> to the source of the message thus requesting
        more data from the capability sending the long message. */

   major_sender = p_more_message -> source_major;
   major_sender = (major_sender << 8) | p_more_message -> minor;
   return (execap (major_sender, CONTINUE_MESSAGE,&(p_more_message -> source_system),1,p_mcb_ptr));
}
/**/
/* : PROCEDURE FUNCTION (handle_continue)

Handle the continue message for the capability.  This means extracting another
portion of the currently long message from the capability's outbuff.

*/

handle_continue (p_mcb_ptr, p_more_message)

mcb  *p_mcb_ptr;                       /* Capability's MCB */
struct more_msg *p_more_message;

{
int  found;
int  done;
int  major_sender;
int  i;
int  code;
struct linklst *msg_buffer;
struct linklst *prev_msg_buffer;
struct more_msg *message;


   code = 0;
   msg_buffer = p_mcb_ptr->outbuff;
   prev_msg_buffer = p_mcb_ptr->outbuff;
   found = FALSE;
   done = FALSE;

/* : Look for a the message that is to be continued */

   while ((found == FALSE) && (done == FALSE))
   {  if (msg_buffer == NULL)
         done = TRUE;
      else
      {  message = (struct more_msg *) msg_buffer->lldata;
         if ((message->system == p_more_message -> source_system)
            && (message->major == p_more_message -> source_major)
            && (message->source_system == p_more_message -> minor))
         {
            found = TRUE;
            done = TRUE;
         }
         else if ((message->system == p_more_message -> source_system)
            && (message->major == p_more_message -> source_major)
            && (message->more_minor == p_more_message -> minor))
         {
            found = TRUE;
            done = TRUE;
         }
         else
         {  prev_msg_buffer = msg_buffer;
            msg_buffer = msg_buffer->nextbuf;
         }
      }
   }

/* : If found then get the next portion and send it to the destination */

   if (found)
   {  if (prev_msg_buffer == p_mcb_ptr->outbuff)      /* Head of list */
      {  p_mcb_ptr->outbuff = msg_buffer->nextlink;
         if (msg_buffer->nextlink != NULL)
            p_mcb_ptr->outbuff->nextlink->nextbuf = msg_buffer->nextbuf;
      }
      else                                            /* Elsewhere in list */
      {  prev_msg_buffer->nextbuf = msg_buffer->nextlink;
         if (msg_buffer->nextlink != NULL)
            msg_buffer->nextlink->nextbuf = msg_buffer->nextbuf;
      }

/* : - I$EXECAP knows what's going on with the messages, so call it */

      code = call_mowse_int(I$EXECAP,msg_buffer->lldata, msg_buffer->linkused);
      wsfree(p_mcb_ptr->outalloc,msg_buffer);

   }

/* : Otherwise we are trying to continue a message that does not exist so send
     a FAIL_CAPABILITY to the application (this one is sending it) */

   else
   {  code = forward (p_mcb_ptr, FAIL_CAPABILITY, NULL, 0, (char)WSIBMPC, 
         (char)WSMAJCAP);
   }

   return (code);
}
/**/
/* : PROCEDURE FUNCTION (handle_default)

Handle passing the message on to the application.
*/

handle_default (p_mcb_ptr, p_buflen, p_bufseg, p_bufoff, p_minor_cap, p_msg_hdr_size, p_link_info_size, p_more_message)

mcb  *p_mcb_ptr;                       /* Capability's MCB */
int  p_bufseg;                         /* Segment address of message */
int  p_bufoff;                         /* Offset of message within segment */
int  p_buflen;                         /* Length of message */
int  p_minor_cap;
int  p_msg_hdr_size;
int  p_link_info_size;
struct more_msg *p_more_message;
{
int  found;
int  done;
int  buffer_size;
int  amount_to_copy;
int  major_sender;
int  i;
int  all_copied;
int  start_of_message;
int  data_length;
char *more_msg_header1;
char *more_msg_header2;
struct input_msg *input_message;
struct linklst *msg_buffer;
struct linklst *prev_msg_buffer;
struct more_msg *message;

/* : Look for a buffer containing previous portion of this message */

   msg_buffer = p_mcb_ptr->inbuff;
   prev_msg_buffer = NULL;

   found = FALSE;
   done = FALSE;
   while ((found == FALSE) && (done == FALSE))
   {  if (msg_buffer == NULL)
         done = TRUE;
      else
      {  message = (struct more_msg *) msg_buffer->lldata;
         if ((message->system == p_more_message -> system)
            && (message->major == p_more_message -> major)
            && (message->source_system == p_more_message -> more_minor))
         {
            found = TRUE;
            done = TRUE;
         }
         else
         {  prev_msg_buffer = msg_buffer;
            msg_buffer = msg_buffer->nextbuf;
         }
      }
   }

/* : If one exists the copy as much of the current message into the buffer 
     located otherwise allocate a buffer and try to copy all of the message 
     into it. */
        
   if (found == TRUE)
   {  amount_to_copy = min((p_buflen-p_msg_hdr_size+1),(msg_buffer->linksize - msg_buffer->linkused-p_link_info_size));

      if (amount_to_copy == (p_buflen-p_msg_hdr_size+1))
      {  peek(p_bufseg,p_bufoff+p_msg_hdr_size-1,&(msg_buffer->lldata[msg_buffer->linkused]),
            amount_to_copy);
         msg_buffer->linkused += amount_to_copy;
         data_length = msg_buffer->linkused - p_msg_hdr_size;
         start_of_message = p_msg_hdr_size;
         all_copied = TRUE;
      }
      else
         all_copied = FALSE;
   }
   else
   {  buffer_size = p_buflen+p_link_info_size;
      msg_buffer = (struct linklst *) wsalloc(p_mcb_ptr->inalloc,buffer_size);

      if (msg_buffer != NULL)
      {  peek(p_bufseg,p_bufoff,msg_buffer->lldata,p_buflen);
         msg_buffer->nextlink = NULL;
         msg_buffer->nextbuf = NULL;
         msg_buffer->linkused = buffer_size;
         msg_buffer->linksize = buffer_size;
         data_length = p_buflen - (p_msg_hdr_size -1);
         start_of_message = p_msg_hdr_size -1;
         all_copied = TRUE;
      }
      else
         all_copied = FALSE;
   }

             
/* : If all of the current message was copied then call the application
     with the information in the buffer and the minor specified otherwise, 
     call the application with the buffer and the minor = BUFFER_OVERFLOW. */

   major_sender = p_more_message -> source_system;
   major_sender = (major_sender << 8) | p_more_message -> source_major;

   if (all_copied == TRUE)
   {  input_message = (struct input_msg *) msg_buffer->lldata;
      if (input_message->system != WSIBMPC)
         return(-1);

/* : - pass the message on to the application */

      forward (p_mcb_ptr, p_minor_cap, (&(msg_buffer -> lldata[start_of_message])),
         data_length, p_more_message -> source_system, p_more_message -> source_major);
   }
   else
   {  forward (p_mcb_ptr, WSOVRFLW, NULL, 0, (char)WSIBMPC, (char)WSMAJCAP);
   }
                  
/* : Free the buffer just sent to the application if necessary. */

   if ((found == TRUE) && (msg_buffer != NULL))
   {  if ((msg_buffer == p_mcb_ptr->inbuff) ||
         (prev_msg_buffer == p_mcb_ptr->inbuff))
      {
         p_mcb_ptr->inbuff = msg_buffer->nextbuf;
      }
      else
         prev_msg_buffer->nextbuf = msg_buffer->nextbuf;

      wsfree(p_mcb_ptr->inalloc,msg_buffer);
   }
   else
   {  if (msg_buffer != NULL)
         wsfree(p_mcb_ptr->inalloc,msg_buffer);
   }

   return(0);
}
