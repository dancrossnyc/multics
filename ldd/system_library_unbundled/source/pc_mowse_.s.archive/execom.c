/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-04-04,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (execom)

Performs the execution of a command on either the remote or local systems.
*/

#include <stdio.h>
#include <ws.h>
#include <ws_mcb.h>
#include <ws_error.h>
#include <ws_func.h>
#include <ws_dcls.h>

extern char local_system;

execom (p_command, p_ws_system, p_cmd_id, p_mcb_ptr)

char *p_command;                       /* Command to execute */
int p_ws_system;                       /* System to execute on */
unsigned *p_cmd_id;                    /* Command id */
mcb *p_mcb_ptr;                        /* Caller's MCB */
{
struct execom_struc param;


   if ((p_ws_system != WSLOCAL) && (p_ws_system != WSREMOTE))
      return(WSINVSYS);

   if (p_mcb_ptr == NULL) 
      return(WSINVMCB);

   param.com_len = strlen(p_command);
   if (param.com_len > WSPAKSIZ)
      return (WSINVBUF);
   strcpy(&param.command[0],p_command);

   if (p_ws_system == WSLOCAL) 
      p_ws_system = local_system;
   else if (p_ws_system == WSREMOTE) 
      p_ws_system = WSMULTICS;

   param.system = p_ws_system;
   param.major = p_mcb_ptr->major_capability;

   call_mowse_int(I$EXECOM,&param,sizeof(param));

/* : Return p_command id and status */

   *p_cmd_id = param.cmd_id;
   return(param.status);
}
