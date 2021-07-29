/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-13,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-10-04,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Corrected Q management.
  3) change(86-11-24,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Make sure source is not already sleeping.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_sleep):

Places the caller's CAT entry onto a queue of entries that are sleeping. The 
entry's position on the queue will be in ascending order of the time remaining 
until wakeup. The SLEEPING_BIT flag in the CAT will also be set.  A
SET_SLEEP_FLAG minor capability message will be sent to the remote system 
indicating the change of status.
*/

/* : RETURNS:

       0, if no error
       WSNOTACT, if MOWSE not active
       WSINVCAT, if CAT entry not correct
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <ws_dcls.h>
#include <ws_fgb.h>
#include <cat.h>
#include <wsmincap.h>

extern int       packet_mode;   /* determines if MOWSE is attached */
extern local_cat *sleepq;       /* pointer to queue of sleeping CATs */
extern local_cat l_CAT[];       /* CAT table for local capabilities */
extern char      mysystem;      /* system id of PC */
           
i_sleep (p_sleep_ptr)
struct sleep_struc *p_sleep_ptr;  /* Pointer to the sleep info structure */
{
int  sysid;                 /* system id extracted from param->sender_major */
int  major;                 /* Major index of caller */
unsigned int wtime;         /* time when wakeup will occur */
local_cat *scat1;           /* pointer to queue of sleeping CATs */
local_cat *scat2;           /* pointer to queue of sleeping CATs */
local_cat *mycat;           /* pointer to this caller's CAT */
struct null_msg  sleep_msg; /* message to update remote sleep info */
unsigned int wstime();      /* returns current value of tick counter */

/* : Return if MOWSE not active */

   if (packet_mode == 0)
      return (WSNOTACT);

/* : Verify that the CAT is in use */

   major = p_sleep_ptr -> source_major - MIN_CAPABILITY_NUMBER;
   if ((major < 0) || (major > NUMBER_OF_CAT_ENTRIES))
      return (WSINVNUM);

   mycat = &l_CAT[major];
   if (mycat -> mcb_ptr  == NULL)
      return (WSINVMCB);

/* : Make sure caller is not already sleeping */

   if (mycat -> flags & SLEEPING_BIT)
      return (WSSLPING);

   mycat -> flags |= SLEEPING_BIT;

/* : Insert the information into the sleep queue */

   wtime = p_sleep_ptr -> time;
   wtime += wstime();
   mycat -> flags |= SLEEPING_BIT;
   mycat -> sleep_time = wtime;

/* : - If queue is empty insert at top */

   if (sleepq == NULL)
   {  sleepq = mycat;
      mycat -> next_cat = NULL;
   }

/* : - Head of list */

   else if (sleepq -> sleep_time > wtime)
   {  mycat -> next_cat = sleepq;
      sleepq = mycat;
   }

/* : Elsewhere in list */

   else for (scat1 = sleepq, scat2 = NULL; scat1 != NULL;)
   {  if (scat1 -> sleep_time > wtime)             /* Middle */
      {  mycat -> next_cat = scat1;
         scat2 -> next_cat = mycat;
         scat1 = NULL;
      }

      else if (scat1 -> next_cat == NULL)          /* Tail */
      {  scat1 -> next_cat = mycat;
         scat1 = NULL;
      }

      else                                         /* Elsewhere */
      {  scat2 = scat1;
         scat1 = scat2 -> next_cat;
      }
   }

/* : Send message to remote system to set sleep flag */

   sleep_msg.system = WSMULTICS;
   sleep_msg.major  = WSMAJCAP;
   sleep_msg.minor  = WS_SET_SLEEP_FLAG;
   sleep_msg.source_major = p_sleep_ptr -> source_major;
   sleep_msg.source_system = mysystem;
   return (send_i_mess(&sleep_msg, sizeof(struct null_msg)-1, NULL, 0));
}
