/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created from extraction of rcvdata.c
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (rcvmsg):

Place the specified message into the specified linked list.
*/

#include "dos.h"
#include "stdio.h"
#include "ws.h"
#include "alloc.h"
#include "cat.h"
#include "ws_fgb.h"
#include "ws_buf.h"
#include "ws_msg.h"
#include "wsmincap.h"
#include "ws_error.h"

/**/

rcvmsg (p_msg_ptr, p_first_ptr, p_last_ptr)
struct fgbstr *p_msg_ptr,           /* Message to be inserted */
              **p_first_ptr,        /* First element in list */
              **p_last_ptr;         /* Last element in list */
{
struct fgbstr    *temp_ptr;         /* Temp holding for pointer */

/* MAIN */

/* : If no data, return */
   if (p_msg_ptr == NULL)
      return (0);

/* : Link to previous background message buffers */
   if (*p_first_ptr == NULL)
   {  *p_first_ptr = p_msg_ptr;
      *p_last_ptr  = p_msg_ptr;
   }
   else
   {  temp_ptr = *p_last_ptr;
      temp_ptr -> fgb_next = p_msg_ptr;
      *p_last_ptr = p_msg_ptr;
   }
}
