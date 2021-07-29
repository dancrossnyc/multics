/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-23,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-07-24,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Added function key handling.
  3) change(86-08-22,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Passed all function key handling to call routine;
     a -1 is returned if a function key was pressed and the parameter 'option'
     will contain the key pressed for the special function.
  4) change(86-09-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Added local editing
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (get_kb_line)

If data exists in the keyboard buffer, this routine will echo
the data and wait for additional data until a carriage return is
entered; the line entered will be passed back and the length of
the line is returned.  If nothing exists in the keyboard buffer,
this routine just returns 0.  If a function key press was detected,
a -1 is returned and the value of the function key press is put in
the parameter 'option'.  While in this routine, the user may use
the backspace key to erase the previous character entered.  If the
entire line is erased, the routine will return immediately with the
value 0. (This allows the emulator in the main loop to poll for
foreground data again.)  The local_edit flag determines whether
the \, @, # and ESC keys have special editing functions.  If
local_edit is TRUE, ESC and @ will kill the entire line, # will also
serve as a backspace and the \ serves to escape all four keys.
If the local_edit key is FALSE, all characters will be passed back
unchanged.  Note that tabs will expand when the tab key is pressed
(according to the size in em_tab_size) but the tab character is passed
back to the caller unaltered.

*/

#include <ctype.h>
#include <ws.h>
#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

extern int bg_sender_stack[NUMBER_OF_CAT_ENTRIES];
extern int packetize_flag;
extern int em_print_mode;
extern int em_tab_size;
extern int local_edit;


db_get_kb_line(kb_buffer,kb_stack,option)

/* INPUT */
   char kb_buffer[];
   char kb_stack[];

/* OUTPUT */
   int *option;
{

/* MISC */
   int done;
   int ch;
   int buff_char;
   int i;
   int counter;
   int kb_index;
   int kb_posit;
   int kb_tstptr;
   int hidden_pos;

/* INITIALIZATION */

   done = 0;
   kb_index = 0;
   kb_posit = 0;
   kb_tstptr = 0;
   hidden_pos = 0;

/* : initialize function key value and buffer */

   *option = -1;
   *kb_buffer = '\0';

/* : loop unconditionally */

   while (1) {

/* : - if a key was pressed, process that character
     -- filter special characters if in local edit mode */

      if (get_kb_key(&ch)) {
         if (local_edit) {
            switch (ch) {
               case '#': ch = BS;        break;
               case '@': ch = DEL_LINE;  break;
               case ESC: ch = DEL_LINE;  break;

/* : -- handle escaping of special characters
     --- '\' pressed, wait for another key
     --- if not function key or special character put '\' in buffer and pass 
         new key on otherwise discard '\' and pass new key on */

               case '\\':
                  while(!get_kb_key(&ch));

                  if (ch < 256 && ch != '#' && ch != '@' && ch != ESC && ch != '\\') {
                     if (kb_index < KB_BUFFERSIZE) {
                        kb_buffer[kb_index++] = '\\';
                        kb_posit++;
                        putch('\\');
                     }
                     else
                        beep();
                  }
                  break;
            }
         }

/* : -- check for extended ascii code */

         if (ch > 255 && ch < 512) {
            if (kb_index == 0) {
               *option = ch;
               return(-1);
            }
         }

/* : -- process normal keyboard data */

         else {
            switch (ch) {

/* : --- handle carriage return pressed
     ---- terminate line with a linefeed
     ---- return the length of keyboard input */

               case CR:
                  kb_buffer[kb_index++] = LF;
                  putch(LF);
                  putch(CR);
                  return(kb_index);

/* : --- handle backspace
     ---- if something in keyboard buffer
     ---- else nothing in keyboard buffer, return to allow scanning of
          incoming data */

               case BS:
                  if (kb_index > 0) {
                     buff_char = kb_buffer[--kb_index];
                     if (buff_char == TAB) {
                        counter = Pop (&kb_tstptr,kb_stack);
                        for (i = 0; i < counter; i++)
                           Screen_Erase_Char();
                     }
                     else 
                        Screen_Erase_Char();
                  }
                  else 
                     return(0);
                  break;

/* : --- wipe out entire line and return(0)
     ---- for each character entered */

               case DEL_LINE:
                  while (kb_index > 0) {
                     buff_char = kb_buffer[--kb_index];

                     if (buff_char == TAB) {
                        counter = Pop (&kb_tstptr,kb_stack);
                        for (i = 0; i < counter; i++)
                           Screen_Erase_Char();
                     }
                     else 
                        Screen_Erase_Char();
                  }
                  return(0);


/* : --- handle tabs */

               case TAB:
                  if (kb_index < KB_BUFFERSIZE) {
                     kb_buffer[kb_index++] = TAB;
                     counter = em_tab_size - (kb_posit % em_tab_size);
                     kb_posit += counter;
                     Push (counter,&kb_tstptr,kb_stack);
                     for (i = 0; i < counter; i++)
                        putch(' ');
                  }
                  else
                     beep();
                  break;

/* : --- handle normal keyboard character */
               default:
                  if (kb_index < KB_BUFFERSIZE) {
                    kb_buffer[kb_index++] = ch;
                    kb_posit++;
                    putch(ch);
                  }
                  else
                     beep();
            }
         }
      }

/* : -- if keyboard buffer empty, return to allow scanning of incoming data */

      else if (kb_index <= 0) return(0);
   }
}

#else
db_get_kb_line ()
{}
#endif
