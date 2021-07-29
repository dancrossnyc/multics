/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-09-26,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Routines extracted from proc_op.c,
      now used by in proc_op.c and dbprocop.c
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (freeze)

loop until any keyboard character is hit
*/

#include <ctype.h>
#include <ws.h>
#include <ws_dcls.h>
#include <wsmincap.h>
#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

extern int em_tab_size;
extern int bg_sender_in;
extern int bg_sender_out;
extern int bg_sender_buff[];
extern char *buff;

freeze()
{
int i;
int ch;

/* : if no key was pressed, loop for a while to give clock interrupts a 
     chance */

   while (!get_kb_key(&ch))
      for (i = 0; i < 100; i++);

}

/* : PROCEDURE FUNCTION (menu)

print a summary of the emulator functions
*/

menu()
{
int ch;

   printf ("----------------------------------------------------------\n");
   printf ("  * Background messages preceeded by ^\n");
   printf ("  * Preceed input with ^ to write to background\n\n");
   printf ("\n");
   printf ("F1 - Freeze display          F2 - Select display mode\n");
   printf ("F3 - Help menu               F4 - Toggle local edit\n");
   printf ("F5 - Toggle term data only   F6 - Show query status\n");
   printf ("F7 - Set tab size            F8 - Execute DOS command\n");
   printf ("F9 - send BREAK              F10 - Exit Emulator\n");
   printf ("\n");
   printf ("ALT F1 - Toggle Show Packets\n");
   printf ("ALT F2 - Toggle Diagnose Rejects\n");
   printf ("ALT F3 - Toggle Extra RCV Chars\n");
   printf ("----------------------------------------------------------\n");
}

/* : PROCEDURE FUNCTION (set_tab_size)

set a new tab size; if the tab size entered by the user is invalid, do not 
change existing tab size
*/

set_tab_size()
{
int size;

/* : prompt for a new tab size and read in user's response */

   printf("$$ Enter new tab size: ");
   gets(buff);

/* : check that only a signle number is entered; the size must be at least one 
     and less than the width of the screen */

   if (sscanf(buff,"%d%s",&size) != 1 || size < 1 || size > get_screen_width())
      printf("$$ -- Invalid tab size, tab size is now %d -- $$\n", em_tab_size);
   else {
      em_tab_size = size;
      printf("$$ New tab size = %d $$\n", em_tab_size);
   }
}

/* : PROCEDURE FUNCTION (execute_dos_cmd)

get and execute a command for the command interpreter 
*/

execute_dos_cmd()
{

/* : prompt and get user's command for command interpreter */

   printf ("$$ Enter DOS command: ");
   gets(buff);

/* : execute it and return to the emulator */

   system(buff);
   printf ("$$ Returning to normal emulation $$\n");
}

/* : PROCEDURE FUNCTION (send_query_status)

display the contents of the background query queue 
*/

sender_query_status()
{
int pending;
int sys;
int major;
int i;
int index;

/* : check to see if the queue is empty */

   if (bg_sender_in == bg_sender_out)
      printf ("$$ No background queries pending $$\n");

/* : get count of queries pending in the queue */

   else {
      if (bg_sender_in > bg_sender_out)
         pending = bg_sender_in - bg_sender_out;
      else
         pending = bg_sender_in - bg_sender_out + BG_SENDBUFF_SIZE;

/* : display the system id and major capability number of all query senders 
     in the queue */

      printf ("$$ %d queries pending:\n", pending);
      for (i = 0; i < pending; i++) {
         index = (bg_sender_out + i) % BG_SENDBUFF_SIZE;
         c_unpack(&sys,&major,bg_sender_buff[index]);
         printf ("  Query %2d>  system = %4d  major = %d\n", i, sys, major);
      }
      printf ("$$ Returning to normal emulation $$\n");
   }
}

/* : PROCEDURE FUNCTION (reply_to_query)

check the background query queue and if not empty, reply to first pending 
message in queue with message from user.
*/

reply_to_query()
{

/* : if background query queue is empty */  

   if (bg_sender_in == bg_sender_out)
      printf ("$$ No background queries pending $$\n");

/* : else background query queue not empty */

   else {
      printf ("$$ Enter reply message: ");
      gets(buff);
      sendqrep(buff,bg_sender_buff[bg_sender_out]);

/* : reply made, remove query sender from queue */

      bg_sender_out = (bg_sender_out+1) % BG_SENDBUFF_SIZE;
   }
}
#else
freeze ()
{}
menu ()
{}
set_tab_size ()
{}
execute_dos_cmd ()
{}
sender_query_status ()
{}
reply_to_query ()
{}
#endif
