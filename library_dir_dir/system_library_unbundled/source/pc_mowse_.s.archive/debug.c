/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-05,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (sdbgnum)

Display a number directly to the screen (bypassing DOS print functions).
*/

#define LF 10
#define CR 13

static int  row = 0,
            col = 0;
static char temp[2] = " ";

sdbgnum (p_num)

int p_num;
{
char temp[32];

   sprintf(temp,"\nNUM = %x\n",p_num);
   sdbgmsg(temp);
}

/* : PROCEDURE FUNCTION (sstmsg)

Send a status message to the screen (for /g MOWSE command option)
*/

sstmsg (p_msg)

char p_msg[];
{
int  i;
char chr;

/* : Send CR/LF and a reverse video '('. */

   sdbgchr(CR);
   sdbgchr(LF);

/* : Send the message, if character is not printable convert it to octal */

   for (i = 0; p_msg[i] != '\000'; i += 1) {
      chr = p_msg[i];
      if (chr >= ' ' && chr <= '~')
         sdbgchr(chr);
      else {
         sdbgchr('\\');                     /* Escape character */
         sdbgchr(((chr>>6) & 3) + '0');     /* High order octal */
         sdbgchr(((chr>>3) & 7) + '0');     /* Med. order octal */
         sdbgchr((chr & 7) + '0');          /* Low  order octal */
      }
   }

   sdbgchr(CR);
   sdbgchr(LF);
}
