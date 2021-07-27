/* BEGIN INCLUDE FILE: ws_dcls.h */

/* HISTORY COMMENTS:
  1) change(86-07-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-10-24,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added i_connect_request structure.
                                                   END HISTORY COMMENTS */

/* FUNCTION:

Defines the structures that are passed to MOWSE when the user calls an entry_
point in the MOWSE application library. The address and length of the 
appropriate structure is given to call_mowse_int, which calls the mowse user 
interrupt. The user interrupt handler will transfer the entire structure into 
MOWSE's address space. At the conclusion of the interrupt, the structure will 
be copied back to the user's address space.

Equivalent include file ws_dcls.mac
*/

#ifndef CAPABILITY_NAME_LENGTH
#include "ws.h"
#endif

#ifndef MCB_DEFINED
#include "ws_mcb.h"
#endif

/* Find name parameter */

struct findname_param_struct {
    int major_capability_number;       /* major capability number to be found */
    char capability_name[CAPABILITY_NAME_LENGTH];
                                       /* name of capability, if found  */
};

/* Find number parameter */

struct findnumb_param_struct {
    int major_capability_number;       /* lowest capability number to try  */
    char capability_name[CAPABILITY_NAME_LENGTH];
                                       /* name of capability to be found */
};

/* Destroy instance parameter */

struct destinst_param_struct {
    int cap_index;                     /* Capability index of caller */
    mcb *mcb_ptr;                      /* MCB of caller */
};

/* Create instance parameter */

struct cretinst_param_struct {
        mcb     *mcb_ptr;              /* address of caller's mcb */
        short   cs_reg;                /* caller's stack register */
        int     (*entry_pt)();         /* address of ws_entry */
        char    major_capability;      /* returned to caller */
        char    system_id;             /* returned to caller */
        char    capability_name[CAPABILITY_NAME_LENGTH];
};

/* Get data structure */

struct get_struc {
        char *local_buffer_pointer;    /* Destination buffer */
        int local_buffer_size;         /* Buffer size */
        int minor_capability;          /* minor capability of data */
        int background_pending_flag;   /* Are there BG messages flag */
};

/* Put data structure */

struct putt_struc {
        int     minor_cap;             /* Minor cap of data */
        int     putstrl;               /* Length of data */
        char    putstr[WSPAKSIZ];      /* Data to send */
};

/* Execute capability structure */

struct xcap_struc {
        char    system;                /* System of execution */
        char    major;                 /* Major to be executed */
        char    minor;                 /* Minor of execution */
        char    source_system;         /* source system */
        char    source_major;          /* source major */
        char    xcapstr[1];            /* Message */
};

/* Get background message structure */

struct gbgmsg_struc {
        int     bg_type;               /* type of message */
        int     sender_major;          /* who sent message */
        int     length;                /* Length of message */
        char    bgmsg[WSPAKSIZ];       /* message */
};

/* Put background message structure */

struct putbg_struc {
        int     type;                 /* type of background message */
        int     sender_major;         /* sender's major capability */
        int     length;               /* length of message */
        char    bgmsg[WSPAKSIZ];      /* background message */
};

/* Execute command structure */

struct execom_struc {
        int     com_len;               /* length of command */
        int     system;                /* system id */
        int     major;                 /* major capability */
        int     cmd_id;                /* command id */
        int     status;                /* execution status */
        char    command[WSPAKSIZ];     /* command string */
};

/* Sleep structure */

struct sleep_struc {
        int     time;                  /* Number of seconds to sleep */
        char    system;                /* System sleeping on */
        char    major;                 /* Major of caller */
        char    minor;                 /* Minor of caller */
        char    source_system;         /* Source of caller */
        char    source_major;          /* source of caller */
        char    xcapstr[1];            /* data */
};

/* connect request structure */

struct i_connect_request {
   char system;                        /* system of destination connectoin */
   char source_system;                 /* Source system of request */
   char source_major;                  /* Major of requesting application */
   char connect_command[WSPAKSIZ];     /* Command to connect application with */
};

/* END INCLUDE FILE: ws_dcls.h */
