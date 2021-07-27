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
     Created
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (get_mcb_values)

get the major capability number and capability name from the mcb in the 
application's memory space given a pointer to a local cat entry
*/

#include <dos.h>
#include <cat.h>
#include <ws_error.h>

#define NULL 0

extern local_cat l_CAT[];

get_mcb_values (l_cat_ptr, entry_name, entry_number)

local_cat *l_cat_ptr;                  /* Local CAT table */
char *entry_name;                      /* Name of entry */
int *entry_number;                     /* Number of entry */
{
int  data_segment;
char *segment_offset;

/* : if the cat pointer is valid then
     - get the data segment of the mcb in the cat entry
     - if the mcb pointer in the cat entry is valid then
     -- copy the value of the major capability from the mcb
     -- copy the value of the capability name from the mcb
     -- set code to no error and return */

   if (l_cat_ptr != NULL) {
      data_segment = l_cat_ptr->sregs.ds;
      if (l_cat_ptr->mcb_ptr != NULL) {
         segment_offset = (char *) &(l_cat_ptr->mcb_ptr->major_capability);
         peek (data_segment, segment_offset, entry_number, sizeof(int));
         segment_offset = l_cat_ptr->mcb_ptr->capability_name;
         peek (data_segment, segment_offset, entry_name, CAPABILITY_NAME_LENGTH);
         return(0);
      }
      else
         return(WSINVMCB);
   }
   else
      return(WSINVNUM);
}
