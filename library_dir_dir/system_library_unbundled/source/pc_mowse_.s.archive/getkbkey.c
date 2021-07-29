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

/* : PROCEDURE FUNCTION (get_kb_key)

returns 0 if no key was pressed
returns 1 if a key was pressed; the value of the key will be
   placed in 'ch'; an extended ASCII character code is encoded
   by adding 256
*/

#include <emulator.h>

#if EM_DEBUG

get_kb_key (ch)

int *ch;
{
   if (!kbhit()) 
      return(0);
   *ch = getch();

/* : if extended character, added 256 to next character */

   if (*ch == 0 && kbhit())
      *ch = 0x100 | getch();
   return(1);
}
#else
get_kb_key ()
{}
#endif
