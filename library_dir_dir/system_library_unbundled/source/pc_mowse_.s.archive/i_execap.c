/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-07,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_execap)

Software interrupt handler for execap user call.
*/

#include <stdio.h>
#include <ws.h>
#include <ws_error.h>
#include <ws_dcls.h>

i_execap (p_param, p_length)

struct xcap_struc *p_param;            /* Execap paramter structure */
int               p_length;            /* Length of structure */
{
   return (send_i_mess (&p_param->system, p_length, NULL, 0));
}
