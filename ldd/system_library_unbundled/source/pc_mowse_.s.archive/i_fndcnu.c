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

/* : PROCEDURE FUNCTION (i_fndcnu):

Interrupt routine to find a major capability number from the local and remote 
CAT in mowse's memory space given a capability name.
*/

/* : NOTES

MOWSE looks for the capability name in its capability table in the following 
fashion:

     1) If the provided major capability number is 0, MOWSE will search the 
        specified system from the top of the CAT.
     2) If the major capability number is valid and not 0, MOWSE will search
        the specified system from the NEXT position in the CAT.
     3) If the provided system_id is invalid, MOWSE will search the local 
        system CAT.

If MOWSE does not find the name between its starting point and the end of the
CAT, an error code will be returned.

param points to a structure containing 
   char capability_name [CAPABILITY_NAME_LENGTH]; /* Name to be searchedfor */
   int  major_capability_number;                  /* Return value of name */
*/

#include <dos.h>
#include <ws.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_error.h>

#define NULL 0

extern local_cat  l_CAT[];
extern remote_cat r_CAT[];
extern char mysystem;

i_fndcnu (param)

struct findnumb_param_struct *param;  /* Parameter structure */
{
int  sysid;              /* system ID specified by caller */
int  cap_num;            /* capability number specified by caller */
int  cap_index;          /* temporary count of cat entries examined */
int  entry_number;       /* major capability number found        */
char entry_name[CAPABILITY_NAME_LENGTH];  /* major name found */
int  code;               /* return codes */

/* : if invalid major capability number, start at top of CAT */

   if (code = c_unpack (&sysid, &cap_num, param->major_capability_number))
      cap_num = 0;

/* : if index specified is not 0, search next entry */

   cap_num = (cap_num < MIN_CAPABILITY_NUMBER) 
      ? 0 
      : cap_num + 1 - MIN_CAPABILITY_NUMBER;

/* : If searching local table */

    if ((sysid == mysystem) || (sysid == WSLOCAL))
    {  for (cap_index = cap_num; cap_index < NUMBER_OF_CAT_ENTRIES; cap_index++)
       {  if (l_CAT[cap_index].mcb_ptr != NULL)
          {  if (code = get_mcb_values (&l_CAT[cap_index],entry_name,&entry_number))
                return (code);
             if (!strncmp (entry_name,param->capability_name,CAPABILITY_NAME_LENGTH))
             {  c_pack (mysystem,cap_index + MIN_CAPABILITY_NUMBER,&param->major_capability_number);
                return(0);
             }
         }
      }
      return (WSINVNAM);
   }

/* : Else search remote CAT */

   for (cap_index = cap_num, sysid = WSMULTICS; cap_index < NUMBER_OF_CAT_ENTRIES; cap_index++)
   {  if (strncmp(param->capability_name,r_CAT[cap_index].capability_name,CAPABILITY_NAME_LENGTH) == 0)
      {  c_pack(sysid,cap_index + MIN_CAPABILITY_NUMBER,&param->major_capability_number);
         return(0);
      }
   }

/* : Nothing found, set code to indicate this */

   return (WSINVNAM);
}
