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
  2) change(86-06-10,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Changed to support ws_stack.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_creins):

Create an instance of the calling application within MOWSE.
Interrupt routine to service a request from the caller for putting its mcb 
into an entry in the local CAT.  Upon successfully finding a free entry in 
the local cat, the fields in the entry are initialized, the mcb pointer field 
in the local CAT is set to the one passed from the caller and the major
capability number (obtained from searching the local CAT for a free entry 
and from the local system id) is placed in the major capability field of 
the mcb in the caller.
*/

/* : NOTES
*/

#include <dos.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_stack.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <wsmincap.h>

extern local_cat l_CAT[];              /* Local cat table */
extern char      mysystem;             /* IBMPC system id */
extern int       packet_mode;          /* Flags that mowse is active on remote */

i_creins (p_ws_ptr,p_cips)

ws_stack *p_ws_ptr;                    /* Callers registers */
struct cretinst_param_struct *p_cips;  /* Create instance information */
{
int cap_index;                         /* Cat index */
int cap_num;                           /* Capability number */
int code;                              /* Error code */
int namelen;                           /* character counter */
struct alter_cat_msg alter_cat;

/* : If MOWSE is not active on the remote, return error */

   if (!packet_mode)
      return (WSNOTACT);

/* : find an unused CAT entry in the local CAT
     - if no available entry in local CAT, return the error code */

   if ((cap_index = find_free_cat() ) < 0)
      return(cap_index);

   p_cips -> system_id = WSIBMPC;
   p_cips -> major_capability = cap_index + MIN_CAPABILITY_NUMBER;

/* : reset all flags */

   l_CAT[cap_index].flags = 0;
   l_CAT[cap_index].pad = 0;

   if (!(p_cips -> entry_pt))
      l_CAT[cap_index].flags |= NULL_BIT;

/* : initialize sleep time to 0 */

   l_CAT[cap_index].sleep_time = 0;

/* : initialize the mcb pointer with the one passed from caller */

   l_CAT[cap_index].mcb_ptr = p_cips -> mcb_ptr;
   l_CAT[cap_index].ws_entry = p_cips -> entry_pt;
   l_CAT[cap_index].sregs.cs = p_cips -> cs_reg;;

/* : initialize the register values */

   l_CAT[cap_index].sregs.es = p_ws_ptr -> esreg;
   l_CAT[cap_index].sregs.ds = p_ws_ptr -> dsreg;
   l_CAT[cap_index].sregs.ss = p_ws_ptr -> ssreg;
   l_CAT[cap_index].bpreg = p_ws_ptr -> bpreg;
   l_CAT[cap_index].spreg = p_ws_ptr -> spreg;
   l_CAT[cap_index].waitreg  = 0;

/* : SEND A MESSAGE TO REMOTE MACHINE TO ADD TO ITS REMOTE CAT */

   alter_cat.system = WSMULTICS;
   alter_cat.major = WSMAJCAP;
   alter_cat.minor = WS_ADD_TO_RAT;
   alter_cat.source_system = mysystem;
   alter_cat.source_major = WSMAJCAP;
   alter_cat.rat_major = cap_index + MIN_CAPABILITY_NUMBER;

   for(namelen=0; namelen < 32; namelen++) {
      if (p_cips -> capability_name[namelen] < ' ')
         break;
   }
   send_i_mess(&alter_cat, sizeof(struct alter_cat_msg) - 2,
   &p_cips -> capability_name[0],namelen);

   return(WSNOERR);
}
