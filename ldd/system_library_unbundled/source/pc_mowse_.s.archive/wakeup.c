/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-09-02,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created from extract of rcvdata.c
  2) change(86-11-11,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Call to external_mowse before wsexecap.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (wakeup):

Detect whether or not an application is to be awoken because its sleep time has
expired.  If so, a message is generated to the sleeping application.
*/

/* : NOTES:

This procedure must ONLY be called if it is safe to execute a capability.
*/

#include <stdio.h>
#include <dos.h>
#include <ws.h>
#include <alloc.h>
#include <ws_mcb.h>
#include <cat.h>
#include <ws_fgb.h>
#include <ws_msg.h>
#include <wsmincap.h>

extern local_cat *sleepq;
extern local_cat l_CAT[];
extern char mysystem;
extern struct allocstr *lcaptr;

wakeup ()
{
struct null_msg  exerep;      /* Remote message indicating sleep awakeninig */
struct input_msg *msg_ptr;    /* pointer to input message header structure  */
struct fgbstr    *bufp;       /* allocation buffer */
local_cat        *catp;       /* pointer to CAT entry */
char             *p1;         /* Temporary pointers */
int              i;
char             major;       /* Major cap of awakening application */

/* EXTERNAL CALLS */
unsigned int wstime();
char  *wsalloc();


/* MAIN */

/* : While there are still things to check in the sleep queue */

   catp = sleepq;
   while (catp != NULL)

/* : - if timer not expired then return */

   {  if (catp -> sleep_time > wstime())
         return (0);

/* : -- calculate the major number from the relative offset of the
        current one to the start of the table */

      major = (char)((((int)catp - (int)l_CAT) / sizeof (local_cat)) + MIN_CAPABILITY_NUMBER);

/* : - Allocate message space, if cant then return */

      bufp = (struct fgbstr *) wsalloc (lcaptr,sizeof(struct fgbstr)+sizeof(struct input_msg)-1);
      if (bufp == NULL)
         return (0);

/* : - notify remote system that wakeup has occurred */

      exerep.system = WSMULTICS;
      exerep.major = WSMAJCAP;
      exerep.source_major = major;
      exerep.source_system = mysystem;
      exerep.minor = WS_RESET_SLEEP_FLAG;
      send_i_mess(&exerep, sizeof(struct null_msg) - 1, NULL, 0);

/* : - create message to wakeup application */

      bufp -> fgb_length = sizeof (struct input_msg);
      bufp -> fgb_next = NULL;

      msg_ptr = (struct input_msg *) &bufp -> fgb_char[0];
      msg_ptr -> system = mysystem;
      msg_ptr -> major = major;
      msg_ptr -> minor = WAKE_UP;
      msg_ptr -> source_system = mysystem;
      msg_ptr -> source_major = WSMAJCAP;

/* : - Adjust the queue for sleeping capabilities */

      catp -> flags &= ~SLEEPING_BIT;
      catp -> sleep_time = 0;
      sleepq = catp -> next_cat;
      catp -> next_cat = NULL;

/* : - Send the message to the application */

      external_mowse (catp, msg_ptr, sizeof(struct input_msg)-1);
      wsfree (lcaptr, bufp);

/* : - get next CAT entry from sleepq */

      catp = sleepq;
   }
   return (0);
}
