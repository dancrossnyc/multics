/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-06,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (findname)

Find the name of a capability given its major capability number.  The 
code for this routine sits in the application's memory space and simply 
passes its parameters to the internal mowse interrupt routine which finds 
and returns the return code and capability name.
*/

#include <dos.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_func.h>

#define NULL 0

extern char local_system;              /* Local system ID */

findname (p_capability_number, p_capability_name)

int  p_capability_number;              /* Capability number to find */
char *p_capability_name;               /* Name of found capability */
{
struct findname_param_struct param;    /* parameter structure for user interrupt */
int code;                              /* return code from user interrupt */
char *s1;                              /* pointer to source string */
char *d1;                              /* pointer to destination string */
int i;                                 /* counter for copy */

   param.major_capability_number = p_capability_number;

/* : call the mowse interrupt routine for finding capability name
     - if the capability name was found
     -- copy capability name into caller's buffer */

   code = call_mowse_int (I$FINDNAME, &param, sizeof(param));
   if (code)
      return (code);

   s1 = &(param.capability_name[0]);
   d1 = p_capability_name;
   for (i = 0; i < CAPABILITY_NAME_LENGTH; i++) {
      *d1++ = *s1++;
   }

   return(0);
}
