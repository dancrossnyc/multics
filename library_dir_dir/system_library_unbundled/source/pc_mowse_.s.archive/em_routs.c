/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (Push)

This routine pushes a value onto the keyboard tab stack to keep track of 
how many blanks were used to replace a particular tab.
*/

#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

Push (value,kb_tstptr,kb_stack)

int  value;
int  *kb_tstptr;
char kb_stack[];
{
   if (*kb_tstptr < KB_STACKSIZE)
      kb_stack[(*kb_tstptr)++] = value;
}



/* : PROCEDURE FUNCTION (Pop)

This function pops a value off the keyboard tab stack.
*/

Pop (kb_tstptr,kb_stack)

int  *kb_tstptr;
char kb_stack[];
{
int return_value;

   if (*kb_tstptr > 0)
      return_value = kb_stack[--(*kb_tstptr)];

   return(return_value);
}

/* : PROCEDURE FUNCTION (beep)

This function causes a beep to sound.
*/

beep()
{
   putch(BELL);
}



/* : PROCEDURE FUNCTION (Screen_Erase_Char)

This function prints out a backspace on the screen that erases the previous 
character (displaying a blank in its place).
*/

Screen_Erase_Char()
{
   putch(BS);
   putch(SPACE);
   putch(BS);
}

#else
Push ()
{}
Pop ()
{}
beep ()
{}
Screen_Erase_Char ()
{}
#endif
