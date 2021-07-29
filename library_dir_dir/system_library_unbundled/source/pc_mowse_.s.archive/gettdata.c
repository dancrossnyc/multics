/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-01-01,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-01-20,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Documentation and added call to getc_buff
     and allocated space for unions.
  3) change(86-05-26,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Support call to call_mowse_int.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (gettdata)

Copies a number of characters from the display buffer (the buffer containing 
information to be displayed on the screen) to a user specified buffer 
*/

#include <dos.h>
#include <ws.h>
#include <ws_dcls.h>
#include <ws_func.h>

gettdata (p_buffer_ptr)

struct get_struc *p_buffer_ptr;
{
int length;

/* : call user_interrupt to get data */

   length = call_mowse_int (I$GETTDATA,p_buffer_ptr,sizeof(struct get_struc));

/* : set the last position in buffer to null and return the number of 
     characters read. */

   if (length >= 0)
      *(p_buffer_ptr->local_buffer_pointer + length) = '\0';

   return(length);
}
