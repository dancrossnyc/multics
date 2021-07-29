/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-09,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (destinst)

Removes the reference to the application from the CAT, deallocates the message 
buffers, and generates a message to remove reference to the in the remote 
system's CAT.
*/

#include <dos.h>
#include <cat.h>
#include <ws_func.h>
#include <ws_dcls.h>
#include <ws_error.h>

#define NULL 0

extern int local_system;               /* Local system id */

destinst (mcb_ptr)

mcb **mcb_ptr;                         /* MCB to be destroyed */
{
int  sysid;                            /* System ID of capability */
int  cap_index;                        /* Index into CAT */
int  code;
struct destinst_param_struct param;    /* Parameter structure to i_desins */

/* : if mcb_ptr passed is NULL  return error_code = WSINVMCB */

   if (*mcb_ptr == NULL)
      return (WSINVMCB);

/* : otherwise
     - get the system id and index into the CAT table.
     - if invalid capability index or not the local system
     -- set the error code */

   sysid = (int)((*mcb_ptr) -> system_id);
   cap_index = (int)((*mcb_ptr) -> major_capability) - MIN_CAPABILITY_NUMBER;

   if (cap_index < 0 || cap_index >= NUMBER_OF_CAT_ENTRIES)
      return (WSINVMCB);
   if (sysid != local_system)
      return (WSINVMCB);

/* : Call into i_desins which accesses MOWSE data segment to let MOWSE know
     of the destruction */

   param.cap_index = cap_index;
   param.mcb_ptr = *mcb_ptr;
   if (code = call_mowse_int (I$DESTINST, &param, sizeof(param)))
      return (code);

/* : free up buffers allocated by create_instance and set mcb pointer passed 
     from caller to NULL */

   free ((*mcb_ptr) -> inalloc);
   free ((*mcb_ptr) -> outalloc);
   free (*mcb_ptr);

   *mcb_ptr = NULL;

   return (0);
}
