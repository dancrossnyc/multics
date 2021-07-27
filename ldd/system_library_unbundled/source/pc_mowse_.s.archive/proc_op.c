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

/* : PROCEDURE FUNCTION (process_options)

Processes options in the emulator, depending on the key pressed.  The value 
of the terminate flag passed will always be FALSE unless the 'exit emulator' 
key was pressed.
*/

#include <ctype.h>
#include <ws.h>
#include <ws_dcls.h>
#include <wsmincap.h>
#include <keydefs.h>
#include <emulator.h>

extern char *print_mode_type[];
extern char *on_off_status[];
extern int packetize_flag;
extern int em_print_mode;
extern int em_tab_size;
extern int local_edit;
extern int fg_data_only;
extern int bg_sender_in;
extern int bg_sender_out;
extern int bg_sender_buff[];
extern int dbgpkts;
extern int dbgrejs;
extern int dbgxchr;
extern char *buff;

process_options(option,terminate_flag)

int option;
int *terminate_flag;
{

/* : process special options */

   switch(option) {

/* : sit in a tight loop until any key is pressed */

      case OP_FREEZE:
         freeze();
         break;

/* : pass a command to the command interpreter */

      case OP_EXEDOSCMD: 
         execute_dos_cmd();
         break;

/* : send a foreground break */

      case OP_BREAK:
         puttdata(FG_BREAK,0,0);
         break;

/* : set flag to exit the emulator */

      case OP_EXIT:
         *terminate_flag = FALSE;
         break;

      case OP_BG_REPLY:
         reply_to_query();
         break;
   }
}
