/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-11-07,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (dtm_mowse)

When MOWSE receives information from Multics that MOWSE on Multics is detached
then MOWSE must be cleaned up on the PC.
*/

#include <stdio.h>
#include <dos.h>
#include <mowse.h>
#include <cat.h>
#include <ws_msg.h>

extern local_cat l_cat[];              /* Currently loaded applications */
extern char      mysystem;             /* PC system id */

dtm_mowse ()
{
int  i;
struct input_msg message;              /* Terminate message to capability */

/* : For each loaded application (mcb_ptr is not NULL)
     - send TERMINATE_APPLICATION to it */

   message.source_system = mysystem;
   message.source_major  = WSMAJCAP;
   message.msg_data[0]   = 0;
   message.system        = mysystem;
   message.minor         = TERMINATE_APPLICATION;

/* For each of the capabilities which are loaded in the CAT, generate a
   terminate_application message and release the memory to the system. */

   for (i = 0; i < NUMBER_OF_CAT_ENTRIES; i++)
   {  if (l_cat[i].mcb_ptr)
      {  message.major = i + MIN_CAPABILITY_NUMBER;
         if (!(l_cat[i].flags & NULL_BIT))
         {  wsexecap (&l_cat [i], &message, sizeof (struct input_msg) - 1);
            free_cat_program (&l_cat[i]);
         }
      }
   }

/* Reinitialize the CATS */

   init_cat ();

   return (0);
}
