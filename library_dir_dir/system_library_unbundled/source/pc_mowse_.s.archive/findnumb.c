/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-10,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (findnumb):

Find the major capability number of an application given a capability name.
*/

/* : RETURNS

     0, if major capability number found
     WSINVNAM, if major capability number not found
     WSINVSYS, if the system id is not valid.
*/

#include <dos.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_func.h>

#define LOW_8_BITS  0x00FF

findnumb (capability_name,system_id ,major_capability_number)

char *capability_name;                 /* Name of capability to find */
int  system_id;                        /* Id of system to find capability on */
int  *major_capability_number;         /* Address of retrun value for capability number */
{
struct findnumb_param_struct param;    /* structure for user interrupt call */
int  code;                             /* return code */
int  cap_num;                          /* major capability extracted from major_capability */

/* : create major capability number, where system id occupies high 8 bits, 
     major_number low 8 bits */

   cap_num = *major_capability_number & LOW_8_BITS;
   param.major_capability_number = (system_id << 8) | cap_num;

/* : call the mowse interrupt routine to find the capability number */

   strncpy (param.capability_name, capability_name, CAPABILITY_NAME_LENGTH);
   if (code = call_mowse_int (I$FINDNUMB, &param, sizeof(param)))
      return (code);

   *major_capability_number = param.major_capability_number;

   return(0);
}

/* : END findnumb */
