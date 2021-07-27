/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-26,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Tab handling.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (em_print_buff)

Displays the contents of a buffer to the display.  Tabs are expanded according 
to the position of the cursor on the screen.  The size of the tabs are 
determined by the 'em_tab_size' variable.  Three print modes exist:

    Printable ASCII only: any non-printable ascii characters.

    Octal non-printables: any non-printable ascii characters are
      displayed as 3 digit octal values preceeded by a backslash.

    Any character: all characters are displayed with the machine's
      character set.

    NOTE: the linefeed, tab, backsapce and carriage return characters
        are not treated as non-printable and are always processed.

    The print mode is determined by the variable em_print_mode.
*/ 

#include <ctype.h>
#include <keydefs.h>
#include <emulator.h>

#if EM_DEBUG                           /* Do NOT compile if !EM_DEBUG */

extern int em_print_mode;
extern int em_tab_size;

em_print_buff(data,datalen)

char *data;
int  datalen;
{
int i;

   for (i = 0; i < datalen; i++)
      em_putch(data[i]);
}

/* : PROCEDURE FUNCTION (em_putch)

See above.
*/

em_putch(ch)

int ch;
{
int x, y;
int i,pad;

   ch &= 0xff;

/* : filter and process printable control characters */

   switch (ch) {
      case CR:
      case LF:
      case BS:
         putch(ch);
         return;

      case TAB:
         get_cursor_pos(&x,&y);
         pad = em_tab_size - (y % em_tab_size);
         for (i = 0; i < pad; i++)
            putch(SPACE);
         return;

   }

/* : anything that filters through is ASCII data */

   switch(em_print_mode) {
      case ASCII_ONLY:
         if (isprint(ch))
            putch(ch);
         break;

      case NON_ASCII_OCTAL:
         if (isprint(ch))
            putch(ch);
         else
            printf("\\%03o",ch);
         break;

      case ANY_CHAR:
         putch(ch);
         break;
   }
}

#else
em_print_buff ()
{}
em_putch ()
{}
#endif
