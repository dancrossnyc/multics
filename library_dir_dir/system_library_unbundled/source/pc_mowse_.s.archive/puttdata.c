/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-04-26,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */
/* : PROCEDURE FUNCTION (puttdata)

Sends a data string to the Multics user i/o switch over the foreground channel.
*/

#include <dos.h>
#include <ws.h>
#include <ws_dcls.h>
#include <ws_func.h>
#include <wsmincap.h>

#define LOW_BYTE   0x00FF

puttdata (p_min_cap, p_buffer_ptr, p_buffer_len)

int  p_min_cap;                        /* Minor capability of foreground data */
char *p_buffer_ptr;                    /* Pointer to data to be sent */
int p_buffer_len;                      /* Length of data to send */
{
struct putt_struc ps;
int  error_code;
int  length;
int  i;
char *sp;


/* : if minor cap is FG_BREAK, then no data and call mowse */

   if (p_min_cap == FG_BREAK)
      return (call_mowse_int (I$FOREBREAK, 0, 0));

/* : If minor cap is 125-127, then submit a request to turn on the appropriate
     debug packet switch */

   if ((p_min_cap & LOW_BYTE) >= 125)
      return (call_mowse_int (8, &p_min_cap, 2));

/* : call user_interrupt to put data */

   ps.minor_cap = p_min_cap;

/* : while (string length > 0) send data in packet size chunks */

   while (p_buffer_len > 0)
   {  if (p_buffer_len > WSPAKSIZ) 
         length = WSPAKSIZ;
      else
         length = p_buffer_len;

      sp = &ps.putstr[0];
      for (i=0; i < length; i++)
      {  *sp++ = *p_buffer_ptr++;
      }

/* : - send  packet to mowse */

      ps.putstrl = length;
      error_code = call_mowse_int (I$PUTTDATA,&ps ,sizeof(ps));
      p_buffer_len -= length;
   }

   return(error_code);
}
