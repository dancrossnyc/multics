/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-10-21,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-10-29,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Reduced to a simple send message to the
     appropriate internal MOWSE on the necessary system.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_connect)

Generate a message to the appropriate system for connect request.
*/

#include <dos.h>
#include <stdio.h>
#include <ws_msg.h>
#include <wsmincap.h>
#include <cat.h>
#include <ws_dcls.h>

#define INTERNAL         32            /* MOWSE's capability number */
#define HEADER_LENGTH    5             /* Length of message header */

extern local_cat l_CAT[];

i_connect (p_params)

struct i_connect_request *p_params;
{
struct null_msg message;

   message.system        = p_params -> system;
   message.major         = INTERNAL;
   message.minor         = REQUEST_CONNECT;
   message.source_system = p_params -> source_system;
   message.source_major  = p_params -> source_major;

   return (send_i_mess (&message,HEADER_LENGTH,p_params->connect_command,strlen(p_params->connect_command)));
}

/* : PROCEDURE FUNCTION (find_local_capability)

Find the number of the capability given its name from within the user's 
data space.
*/

find_local_capability (p_name)

char *p_name;                          /* Name of capability to find */
{
int i,j;
char cap_name[CAPABILITY_NAME_LENGTH];
int  cap_num;
int  code;

   for (i = 0; i < CAPABILITY_NAME_LENGTH; i++)
   {  code = get_mcb_values (&l_CAT[i],cap_name,&cap_num);
      if (code == 0)
      {  for (j = 0; (p_name[j] == cap_name[j]) && (j < CAPABILITY_NAME_LENGTH); j++)
         {  if (p_name[j] == NULL)
               return(cap_num);
         }
      }
   }
   return(0);
}
