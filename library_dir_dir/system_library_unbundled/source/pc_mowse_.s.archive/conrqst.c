/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-10-21,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Changed format to connect to a name rather than
     a number and to supply args to the connect.
  3) change(86-10-29,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Reduced to generate a message to the internal
     MOWSE function to handle connect request.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (conrqst)

Send a REQUEST_CONNECT minor capability to the application specified.  If the
application does not exist, it will be created through an execom with the
name provided and the args specified.  The REQUEST_CONNECT message will then
be generated if the name became active (CAT).
*/

#include <dos.h>
#include <cat.h>
#include <stdio.h>
#include <ws_func.h>
#include <wsmincap.h>
#include <ws_error.h>
#include <ws_dcls.h>

#define HEADER_LENGTH 4

conrqst(p_cap_name,p_args,p_system,p_mcb_ptr)

char p_cap_name[];                     /* Name of capability to connect ot */
char p_args[];                         /* Argument string */
int  p_system;                         /* System of capability to connect to */
mcb  *p_mcb_ptr;                       /* MCB of caller */
{
int  system;
int  length;
struct i_connect_request xparam;

/* : Check system value */

   if (p_system == WSLOCAL)
      system = WSIBMPC;
   else if (p_system == WSREMOTE)
      system = WSMULTICS;
   else 
      return(WSINVSYS);

/* : Check mcb_ptr */

   if (p_mcb_ptr == NULL)
      return(WSINVMCB);

/*  : set up parameters to be passed to user interrupt handler */

   xparam.system        = system;
   xparam.source_system = p_mcb_ptr->system_id;
   xparam.source_major  = p_mcb_ptr->major_capability;
   sprintf (xparam.connect_command, "%.32s %s", p_cap_name, p_args);
   length = strlen (xparam.connect_command) + HEADER_LENGTH;

/* : generate user interrupt */

   return (call_mowse_int (I$CONNECT,&xparam,length));
}
