/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-05,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */
/* : PROCEDURE FUNCTION (init_cat)

Initialize the local and remote Capability Address Tables (CAT) to a state 
which indicates that there are NO current entries in the tables. This is 
done by setting the "mcb" field of local table to "NULL". And setting the
"major_capability" field of the remote table to "0".
*/

/* : NOTES:

A call must be made to this routine before any accessing of the CAT tables 
can be made. Also a call to this routine will wipe out any entries that may
currently exist in the Tables.
*/

#include <dos.h>
#include <ws.h>
#include <cat.h>
#include <ws_error.h>

#define LOW_8_BITS  0x00FF
#define NULL 0

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

extern char       MYSYSTEM;            /* System ID */
extern local_cat  l_CAT[];             /* Local CAT */
extern remote_cat r_CAT[];             /* Remote CAT */
extern local_cat  *sleepq;             /* Sleeping queue */

init_cat ()
{
int  i;

/* : Empty the sleepq */

   sleepq = NULL;

/* : for each entry in the cat tables
     - set the mcb pointer in the local cat entry to NULL
     - set the major capability number in remote cat entry to 0 */

   for (i = 0; i < NUMBER_OF_CAT_ENTRIES; i++)
   {  l_CAT[i].mcb_ptr            = NULL;
      l_CAT[i].flags              = 0;
      l_CAT[i].next_cat           = NULL;
      l_CAT[i].sleep_time         = 0;
      l_CAT[i].waitreg            = 0;
      l_CAT[i].ws_entry           = NULL;

      r_CAT[i].major_capability   = 0;
      r_CAT[i].system_id          = 0;
      r_CAT[i].capability_name[0] = 0;
      r_CAT[i].flags              = 0;
   }
}

/* : PROCEDURE FUNCTION (find_free_cat)

Find a free entry into the local CAT table, i.e one that is not being used. 
Return the index for this local CAT entry if one is found, otherwise return 
an error code.
*/

find_free_cat ()
{
int  cat_entry_index;
int  i;

/* : search local cat from 1st entry to last entry
     - if the mcb pointer is NULL (not being used)
     -- set output parameter cat entry index to current entry
     -- free cat entry found, set code to 0 and return */

   for (i = 0; i < NUMBER_OF_CAT_ENTRIES; i++)
   {  if (l_CAT[i].mcb_ptr == NULL)
      {  cat_entry_index = i;
         return(cat_entry_index);
      }
   }

/* : else search for free cat entry fails, set code for failed */

   return(WSCNTCRE);
}

/* : PROCEDURE FUNCTION (add_to_remote_cat)

Used to update the remote CAT entry, this is an internal MOWSE function call.
*/

add_to_remote_cat (p_system_id, p_major_capability_number, p_capability_name)

char p_system_id;
char p_major_capability_number;
char *p_capability_name;
{
int  cap_num;
int  i;
int  pad;

/* : if system_id is not 'remote', set error code and return */

   cap_num = (int) p_major_capability_number - MIN_CAPABILITY_NUMBER;
   if ((cap_num < 0) || (cap_num > NUMBER_OF_CAT_ENTRIES))
      return (WSINVNUM);

   if (p_system_id == MYSYSTEM)
      return (WSINVSYS);

/* : if capability is already used, set error code and return */

   if (r_CAT[cap_num].major_capability != 0)
      return(WSINVNUM);

/* : - initialize the remote cat entry with given data */

   r_CAT[cap_num].major_capability = p_major_capability_number;
   r_CAT[cap_num].system_id = p_system_id;

/* : copy the capability name into the remote cat, padding nulls on the
     right of the string */

   i = stccpy (r_CAT[cap_num].capability_name, p_capability_name, CAPABILITY_NAME_LENGTH);
   for (i = i; i < CAPABILITY_NAME_LENGTH; r_CAT[cap_num].capability_name[i++] = 0);

/* : reset all flags */

   r_CAT[cap_num].flags = 0;

/* : - no errors, set code returned to 0 */

   return (0);
}
