/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_fndcna)

Interrupt routine which searches either the local or remote CAT for the 
capability name given the capability number.  Encoded in the capability
number will be the system to search.
*/

/* : RETURNS

Returns 0 if successful, an ws_error otherwise.
*/

#include <dos.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_error.h>

#define NULL 0

extern local_cat l_CAT[];
extern remote_cat r_CAT[];
extern char mysystem;

i_fndcna (p_param)

struct findname_param_struct *p_param;
{
int sysid;              /* system id of CAT entry to be used     */
int cap_num;            /* CAT entry to be found                 */
int code;               /* return code                           */
int maj_number;         /* major capability number stored in mcb */

/* : extract the system id and CAT index
     - if error in extracting, return the error code */

   code = c_unpack (&sysid, &cap_num, p_param -> major_capability_number);
   if (code != 0)
      return (code);

/* : if local cat then
     - check for validity of capability found
     - if invalid, set error code to invalid capability and return
     -- copy the capability name into the output parameter */

   if (sysid == mysystem) {
      cap_num = cap_num - MIN_CAPABILITY_NUMBER;

      if (cap_num < 0 || cap_num >= NUMBER_OF_CAT_ENTRIES)
         return (WSINVNUM);            /* OUT OF RANGE */

      if (l_CAT[cap_num].mcb_ptr == NULL)
         return (WSINVNUM);            /* MCB NOT ALLOCATED */

      code = get_mcb_values (&(l_CAT[cap_num]),&(p_param -> capability_name[0]),
         &maj_number);
      return(code);
   }

/* : else search the remote cat
     - check for validity of capability found
     - if invalid, set error code to invalid capability and return
     -- copy capability name into caller's buffer */

   else {
      cap_num = cap_num - MIN_CAPABILITY_NUMBER;

      if ((cap_num < 0) || (cap_num >= NUMBER_OF_CAT_ENTRIES))
         return (WSINVNUM);            /* OUT OF RANGE */

      if (r_CAT[cap_num].major_capability == 0 ) 
         return (WSINVNUM);            /* MOWSE HAS NO INFORMATION */

      stccpy (p_param -> capability_name, r_CAT[cap_num].capability_name, CAPABILITY_NAME_LENGTH);
      return(0);
   }
}
