/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-06,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-05-12,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     global variable holding address of application
     routine now replaced by field in mcb called application_entry.
  3) change(86-07-07,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Support linked lists for inbuff and outbuff.
  4) change(86-11-14,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Initialized the mcb_flag field.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (cretinst)

Register the calling routine with MOWSE by assigning it a major capability 
number and adding it to MOWSE's capability table. The calling routine must 
supply the name by which it is to be known to MOWSE, and a pointer to the 
function to be invoked when a message is received.

Allocates and initializes the mcb in the caller's address space.

Allocates and initializes the input and output buffers in the caller's 
address space.
*/

/* : NOTES:

All applications which expect to receive messages must have registered with 
MOWSE (through create_instance) in order to receive messages. A message is 
provided to the application when a the destination of the message specifies 
the major capability number of the application. The application will then be 
invoked at the entry name provided with the message (argument data), its 
length, and a pointer to the applications data_block as follows:

   process_event (minor_capability, major_sender, arg_ptr,
                  arg_length, mcb_ptr, data_block_ptr);

The buffers inbuff and outbuff allow MOWSE to send and receive messages longer 
than one communications packet (defined by WSPAKSIZ) in a manner transparent 
to the capability.
*/

#include <dos.h>
#include <ws.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_error.h>
#include <ws_func.h>

#define NULL 0

cretinst (capability_name, entry_name, inbuff_length, outbuff_length, data_block_ptr, mcb_ptr)

char *capability_name;
int  (*entry_name)();
int  inbuff_length;                    /* Length of input buffer */
int  outbuff_length;                   /* Length of output buffer */
char *data_block_ptr;                  /* Application data to be saved */
mcb  **mcb_ptr;                        /* Address of new mcb_ptr */
{
int  i;
int  inbuff_size;
int  outbuff_size;
int  cap_num;
int  code;
mcb *lmcb_ptr;                         /* local mcb pointer */
struct SREGS segregs;
struct allocstr *allocp;
struct cretinst_param_struct cips;

int  pe_entry();                       /* pointer to function to receive messages */

   code = 0;

/* : allocate and initialize the mcb */

   lmcb_ptr = (mcb *) malloc (sizeof (mcb));
   if (lmcb_ptr == NULL)
      return (WSCNTCRE);

/* : initialize the capability name field of mcb with capability name passed */

   cips.mcb_ptr = lmcb_ptr;
   stccpy (lmcb_ptr -> capability_name, capability_name, CAPABILITY_NAME_LENGTH);
   i = stccpy (cips.capability_name, capability_name, CAPABILITY_NAME_LENGTH);
   for (i = i; i < CAPABILITY_NAME_LENGTH; i++)
   {  lmcb_ptr -> capability_name[i] = 0;
      cips.capability_name[i] = 0;
   }

/* : store address of application routine (if it was NULL, then set the
     appropriate mcb_flag) to be called and the application's data block
     pointer in the mcb, clear the flags */

   lmcb_ptr -> data_block_ptr = data_block_ptr;
   lmcb_ptr -> mcb_flag = 0;

   if (!((int)(entry_name)))
   {  lmcb_ptr -> application_entry = NULL;
      lmcb_ptr -> mcb_flag |= MCB_NULL_ROUTINE;
   }
   else
      lmcb_ptr -> application_entry = entry_name;

/* : determine the actual input buffer size. The input buffer size must be
     at least as large as the minimum packet size.  We must also allow
     space for the linked list overhead */

   if (inbuff_length < WSPAKSIZ)
      inbuff_size = WSPAKSIZ;
   else
      inbuff_size = inbuff_length;

   inbuff_size += (sizeof (struct allocstr))*2;
   inbuff_size += (sizeof (struct linklst))*2;
   inbuff_size += WSPAKSIZ;

/* : allocate the input buffer and initialize fields associated with the
     input buffer.  If cant allocate, then clean up and return error */

   lmcb_ptr -> inbuff_length = inbuff_length;
   allocp = (struct allocstr *) malloc (inbuff_size);
   if (allocp != NULL)
   {  lmcb_ptr -> inalloc = allocp;
      allocp -> memory_used = 0;
      allocp -> memory = (char *) ((int)allocp + sizeof (struct allocstr));
      allocp -> m_allocp = NULL;
      allocp -> memory_size = inbuff_size - sizeof (struct allocstr);
      lmcb_ptr -> inbuff = NULL;
   }
   else
   {  free (lmcb_ptr);
      *mcb_ptr = NULL;
      return (WSINVBUF);
   }

/* : determine the actual output buffer size. We must allow space for the
     overhead required to store enough packets to save the requested number
     of characters, plus space for the allocation structure. */

   outbuff_size = 1 + outbuff_length / WSPAKSIZ;
   outbuff_size = outbuff_size * (WSPAKSIZ + sizeof(struct linklst));
   outbuff_size += sizeof (struct allocstr);

/* : allocate the output buffer and initialize the fields associated with
     the output buffer.  If cant allocate it hten clean up and return error */

   lmcb_ptr -> outbuff_length = outbuff_length;
   allocp = (struct allocstr  *) malloc (outbuff_size);
   if (allocp != NULL) {
      lmcb_ptr -> outalloc = allocp;
      allocp -> memory_used = 0;
      allocp -> memory = (char *) ((int)allocp + sizeof (struct allocstr));
      allocp -> m_allocp = NULL;
      allocp -> memory_size = outbuff_size - sizeof (struct allocstr);
      lmcb_ptr -> outbuff = NULL;
   }
   else {
      free (lmcb_ptr -> inalloc);
      free (lmcb_ptr);
      *mcb_ptr = NULL;
      return (WSINVBUF);
   }

/* : set the low and high memory values get the segment register so we can
     save it in the CAT store address of entry_pt routine in mowse
     structure */

   segread(&segregs);
   cips.cs_reg = segregs.cs;
   cips.entry_pt = NULL;
   if (!(lmcb_ptr -> mcb_flag & MCB_NULL_ROUTINE))
      cips.entry_pt = pe_entry;

/* : call the interrupt handler in mowse,  if an error occurred free up 
     allocated buffers and pass back NULL pointer to caller */

   if (code = call_mowse_int (I$CRETINST, &cips, sizeof(cips)))
   {  if (lmcb_ptr -> inalloc != NULL)
         free (lmcb_ptr -> inalloc);
      if (lmcb_ptr -> outalloc != NULL) 
         free (lmcb_ptr -> outalloc);
      free (lmcb_ptr);
      *mcb_ptr = NULL;
   }
   else
   {  lmcb_ptr -> major_capability = cips.major_capability;
      lmcb_ptr -> system_id = cips.system_id;
      *mcb_ptr = lmcb_ptr;
   }
   return (code);
}
