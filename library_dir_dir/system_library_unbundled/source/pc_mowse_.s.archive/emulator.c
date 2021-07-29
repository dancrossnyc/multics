/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-23,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-07-25,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Modifed command line args, saves to script file
  3) change(86-08-24,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Added menus
  4) change(86-09-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Added tab handling, local editing
  5) change(86-09-11,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     DOS commands, query status
  6) change(86-09-26,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Split emulator into two versions, a debugging
     and a non-debugging version; change the define EM_DEBUG in emulator.h
     to 0 for non-debugging version, change to 1 for debugging version
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (emulator)

Allows a user to interact through MOWSE foreground and background channels 
through a simple terminal interface.  This terminal emulator treats foreground
data as would a normal emulator.  However, to distinguish background data from 
foreground data, all background data is preceeded by a carat (^).  Thus any
background data going onto the screen will be preceeded by a carat.  Similarly, 
if keyboard input is preceed by a carat, then it will be sent to the background 
channel. No priority will be given to either channel, information that is
displayed first will be displayed.  If there are several background queries, 
then background data from the keyboard will go to each background query in
sequence.

Unless otherwise specified, all function keys may be invoked in either
packet or non-packet mode (i.e. after or before attaching to MOWSE).

F1 - debugging mode only;
     Suspend processing of terminal data; when this key is pressed
     while foreground or background data is being received and displayed,
     the emulator will sit in a tight loop, resulting in temporary
     stoppage in the handling of terminal data.  Hitting any other key
     will resume processing.

F2 - Debugging mode only. Toggle display mode; there are 3 modes:
        Printable ASCII only - only printable ascii characters except
            for the linefeed, carriage return and tab characters will
            be discarded.
        Non-printable octal - any non-printable ascii character except
            for linefeed, carriage return and tab characters will be
            displayed as a 3 digit octal value preceeded by a backslash.
            Any backslash character as terminal data will appear as
            a double backslash.
        Any character - displays all characters with character image in the
            character set of the machine

F3 - debugging mode only. Help menu.
     Summary of what function keys do, how to handle background data

F4 - debugging mode only. Toggle local editting flag;
     the flag is only used when in packet-mode.
     When local editting is enabled, pressing ESC or @ will kill and erase
     the current line of input; pressing BACKSPACE or # will erase the
     previous character and the \ key will serve to escape the @, #, ESC
     and \ keys.  With local editting disabled, key values are entered
     as straight data.

F5 - Toggle between foreground terminal data only or any foreground data;
     debugging mode only;
     By default, foreground terminal data only is enabled.

F6 - debugging mode, CTRL-]R - non-debugging mode;
     Show query status; when background queries are received by the
     emulator, they are displayed immediately but the senders' capability
     are saved in a queue until the user responds to these queries
     (by preceeding the input with the ^ character).  Thus, any data
     entered as a response to background queries will be sent to
     the earliest background query in the queue.  Once, a background
     message has been responded to, the capability is removed from the
     queue.  The F6 key will display the system id and the major
     capability of the sender of all unanswered background queries.

F7 - debugging mode only;
     Set tab size; used to change or examine the tab size.  This tab
     value determines both the amount of padding to use for displaying
     and for the tab key (i.e. it is used in both packet and non-packet
     mode).

F8 - debugging mode, CTRL-]C - non-debugging mode;
     Execute DOS command; pass a command to the command interpreter on
     the PC and resume normal terminal emulation upon completion.
     Note that the F1 key will not work since the command interpreter
     has control; however, ^S and ^Q will serve to suspend and resume
     while control is not passed back to the emulator.

F9 - debugging mode; CTRL-]B - non-debugging mode;
     Send foreground break; a foreground break is sent through the
     foreground channel.  This works only when MOWSE is attached on
     the other side.

F10 - debugging mode; CTRL-]Q - non-debugging mode;
     Exit the terminal emulator.

CTRL-]R - non-debugging mode only
     Send a query reply

CTRL-]0 - non-debugging mode only
     Send a null over the foreground channel

CTRL-]CTRL-] - non-debugging mode only
     Send a CTRL-] over the foreground channel

Local editing is always on for non-debugging version; the # functions
the same as backspace and the @ functions the same as the ESC key
which wipes out the line.  These may be escaped by the \ key.

*/

#include <ws.h>
#include <ws_dcls.h>
#include <wsmincap.h>
#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

char NULLBYTE;
char *print_mode_type[] = {            /* Message strings */
   "PRINTABLE ASCII ONLY",
   "OCTAL NON-PRINTABLE ASCII",
   "ANY ASCII"
};

char *on_off_status[] = {              /* Status message strings */
   "OFF",
   "ON"
};

struct get_struc gettdatas;
char buff[KB_BUFFERSIZE];
char kb_stack[KB_STACKSIZE];
int  bg_sender_buff[BG_SENDBUFF_SIZE];
int bg_sender_in = 0;
int bg_sender_out = 0;
extern int packetize_flag;
int em_print_mode;
int em_tab_size;
int local_edit;
int fg_data_only;

em()
{

/* MISC */
   int str_len;
   int i;
   int sender_major;
   int message_type;
   int bg_query_flag;
   int j;
   int err_code;
   int option;
   int do_te_loop = TRUE;
   int delay_i;
   int ch;
   int sys;
   int major;

/* INITIALIZATION */
   str_len = 0;
   bg_query_flag = 0;

/* : set default modes */

   local_edit = TRUE;
   em_print_mode = ANY_CHAR;
   fg_data_only = TRUE;
   em_tab_size = KB_TABSIZE;

/* : initialize the structure for getting terminal data */

   gettdatas.local_buffer_pointer = &buff[0];
   gettdatas.local_buffer_size = sizeof(buff);

/* : print out terminal emulator banner and instructions */

   printf("===== MOWSE Terminal Emulator\n");
   printf("===== Hit F3 to display emulator functions.\n");

/* : While (not asked to quit by user) */

   while (do_te_loop) {

/* : - check terminal buffer for data character from the remote machine 
       and display it if it exists */

      str_len = gettdata(&gettdatas);
      if (gettdatas.minor_capability == FG_TERMINAL_DATA || !fg_data_only)
         em_print_buff(buff,str_len);

/* : - loop for a while here to allow clock interrupt time to happen */

      for (delay_i = 0; delay_i < DELAY_INTERVAL; delay_i++);

/* : - check for background messages
     - if there is background data then */

      while (gettdatas.background_pending_flag > 0) {
         err_code = getbgmes(buff, &message_type, &sender_major);

/* : -- display the data preceeded by a carat (^) */

         if (message_type == WSINFO || message_type == WSQUERY) {
            c_unpack(&sys, &major, sender_major);
            printf("^[%2d:%2d]%s\n",sys,major,buff);
         }

/* : -- if it was a query message, save sender's major in the FIFO
     --- check for queue overflow */

         if (message_type == WSQUERY) {
            if (bg_sender_out == ((bg_sender_in+1) % BG_SENDBUFF_SIZE))
               printf ("$$ query buffer overflow $$\n");
            else {
               bg_sender_buff[bg_sender_in] = sender_major;
               bg_sender_in = (bg_sender_in + 1) % BG_SENDBUFF_SIZE;
            }
         }

/* : -- decrement count of background messages left */

         gettdatas.background_pending_flag--;
      }


/* : - check for input from the user
     - if in packet-mode */

      if (packetize_flag) {

/* : -- check for a line of input from user */

         str_len = db_get_kb_line(buff, kb_stack, &option);
         if (str_len > 0) {

/* : --- if input is not preceeded by ^, destined for foreground */

            if (buff[0] != '^')
               puttdata(FG_TERMINAL_DATA,buff,str_len);

/* : --- if user wants to send input with ^ as the first character over 
         foreground, he enters ^^ and the first one is stripped, rest is 
         sent to foreground */

            else if (str_len > 1 && buff[0] == '^' && buff[1] == '^')
               puttdata(FG_TERMINAL_DATA,buff+1,str_len-1);

/* : --- data is destined for background, get the sender major off stack and 
         send back the response */

            else if (bg_sender_in != bg_sender_out) {
               buff[str_len] = '\0';
               sendqrep (buff+1,bg_sender_buff[bg_sender_out]);
               c_unpack(&sys,&major,bg_sender_buff[bg_sender_out]);
               printf ("$$ Reply sent to [%d:%d]\n", sys, major);
               bg_sender_out = (bg_sender_out + 1) % BG_SENDBUFF_SIZE;
            }
            else
               printf ("$$ No queries pending $$\n");
         }

/* : -- check if special function key was pressed */

         else if (option >= 0)
         db_process_options(option,&do_te_loop);
      }

/* : -- else in non-packet mode
     --- check if a key was pressed
     ---- check and handle any special function key
     ---- otherwise send character over foreground channel */

      else {
         if (get_kb_key(&ch)) {
            if (ch > 255)
               db_process_options(ch,&do_te_loop);
            else
               puttdata(FG_TERMINAL_DATA,&ch,1);
         }
      }
   }

/* : print message to indicate exit from terminal emulator */

   printf("\n==== MOWSE Terminal Emulator returning to DOS ====\n");
}

#else
em ()
{}
#endif
