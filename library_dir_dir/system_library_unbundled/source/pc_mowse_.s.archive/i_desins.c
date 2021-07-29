/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-09-14,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Fixed CAT table.
  2) change(86-12-08,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Check for sleep queue.
                                                   END HISTORY COMMENTS */
/* :  PROCEDURE FUNCTION (i_desins)

Interrupt routine to free up the cat entry in the local CAT for a given 
capability and to send a message to the remote machine to get rid of the 
same capability in its CAT table.
*/

#include <dos.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <wsmincap.h>

#define NULL 0

extern local_cat l_CAT[];              /* Local CAT */
extern char mysystem;                  /* System id */
extern local_cat *sleepq;              /* List of sleepers */

i_desins (param)

struct destinst_param_struct *param;
{
int cap_index;                         /* Index into CAT */
struct alter_cat_msg alter_cat;        /* Message to remote */
local_cat *catp;                       /* For traversiong the cat */
int  code;                             /* Error code */

/* : if mcb pointer passed is not the same as the mcb pointer in the cat entry 
     given by the derived index, return with code for invalid mcb */

   cap_index = param -> cap_index;
   if ((cap_index < 0) || (cap_index >= NUMBER_OF_CAT_ENTRIES))
      return(WSINVMCB);
   if (l_CAT[cap_index].mcb_ptr == NULL)
      return(WSINVMCB);
   if (l_CAT[cap_index].mcb_ptr != param -> mcb_ptr)
      return(WSINVMCB);

/* : Null out the mcb_ptr in the CAT (destinst will free it)
     Adjust the sleeping queue (this capability might have been in it) */

   l_CAT[cap_index].mcb_ptr = NULL;

   for (catp = sleepq; catp != NULL; catp = catp -> next_cat)
   {  if (catp -> next_cat == &(l_CAT[cap_index]))
      {  catp -> next_cat = catp -> next_cat -> next_cat;
         break;
      }
   }

/* : Send the message to the remote */

   alter_cat.system = WSMULTICS;
   alter_cat.major = WSMAJCAP;
   alter_cat.minor = WS_DELETE_FROM_RAT;
   alter_cat.source_system = mysystem;
   alter_cat.source_major = WSMAJCAP;
   alter_cat.rat_major = cap_index + MIN_CAPABILITY_NUMBER;

   code = send_i_mess (&alter_cat, sizeof(alter_cat) - 2,NULL,0);

/* : Release the program memory */

   if (!(l_CAT[cap_index].flags & NULL_BIT))
      free_cat_program (&l_CAT[cap_index]);

   return (code);
}
