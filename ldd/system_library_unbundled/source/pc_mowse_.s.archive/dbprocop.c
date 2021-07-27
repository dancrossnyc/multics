/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-22,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (proc_op)

Processes options in the emulator, depending on the key pressed.  The value 
of the terminate flag passed will always be FALSE unless the 'exit emulator' 
key was pressed.  This routine is called the MOWSE dumb emulator in the 
debug version.
*/


#include <ctype.h>
#include <ws.h>
#include <ws_dcls.h>
#include <wsmincap.h>
#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

extern char *print_mode_type[];
extern char *on_off_status[];
extern int em_print_mode;
extern int em_tab_size;
extern int local_edit;
extern int fg_data_only;
extern int bg_sender_in;
extern int bg_sender_out;
extern int bg_sender_buff[];
extern int dbgpkts;                    /* Debug packet flag */
extern int dbgrejs;                    /* Debug rejected packetsflag */
extern int dbgxchr;                    /* Debug extra chars flag */
extern char *buff;

db_process_options(option,terminate_flag)

/* INPUT */
   int option;

/* OUTPUT */
   int *terminate_flag;
{

/* : process special options */

   switch(option) {

/* : - sit in a tight loop until any key is pressed */
      
      case F1:
         freeze();
         break;

/* : - select a different display mode; the flag em_print_mode is used by 
       the em_put and em_print_str routines */

      case F2:
         em_print_mode = (em_print_mode + 1) % 3;
         printf ("$$ print mode = %s $$\n",
         print_mode_type[em_print_mode]);
         break;

/* : - display summary of function key functions */

      case F3:
         menu();
         break;

/* : - toggle local_edit option flag; local_edit flag is used by get_kb_line */

      case F4: 
         local_edit = !local_edit;
         printf ("$$ local edit = %s $$\n", on_off_status[local_edit]);
         break;

/* : - toggle foreground terminal data only/any foreground data;
       fg_data_only flag is used in the main emulator routine */

      case F5: 
         fg_data_only = !fg_data_only;
         printf ("$$ terminal data only = %s $$\n", on_off_status[fg_data_only]);
         break;

/* : - display the status of the background query queue */

      case F6: 
         sender_query_status();
         break;

/* : - display or set a new tab size; used by em_print_str and
       get_kb_line routines */

      case F7: 
         set_tab_size();
         break;

/* : - pass a command to the command interpreter */

      case F8: 
         execute_dos_cmd();
         break;

/* : - send a foreground break */
      
      case F9:
         puttdata(FG_BREAK,0,0);
         break;

/* : - set flag to exit the emulator */

      case F10:
         *terminate_flag = FALSE;
         break;

/* : - toggle debug packets */

      case ALT_F1: 
         dbgpkts = !dbgpkts;
         printf ("$$ DBG Show Packets = %s $$\n", on_off_status[dbgpkts]);
         break;

/* : - toggle reject packets */

      case ALT_F2: 
         dbgrejs = !dbgrejs;
         printf ("$$ DBG Diagnose Rejects = %s $$\n", on_off_status[dbgrejs]);
         break;

/* : - toggle extra character debug */

      case ALT_F3: 
         dbgxchr = !dbgxchr;
         printf ("$$ DBG Show Extraneous Receive Characters = %s $$\n", on_off_status[dbgxchr]);
         break;
   }
}

#else
db_process_options()
{}
#endif
