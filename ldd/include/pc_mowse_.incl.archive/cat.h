/* BEGIN INCLUDE FILE: cat.h */

/* HISTORY COMMENTS:
  1) change(86-06-15,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-09-04,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Changed sleep time to int (long is too compiler dependent)
                                                   END HISTORY COMMENTS */


/* FUNCTION:

Defines the structures of the local and remote capability address tables.
Equivalent include file cat.mac
*/

#ifndef CAPABILITY_NAME_LENGTH
#include "ws.h"
#endif

#ifndef MCB_DEFINED
#include "ws_mcb.h"
#endif

#define RESET_BIT     01               /* Reset state of application */
#define SUSPENDED_BIT 02               /* Suspended state ... */
#define SLEEPING_BIT  04               /* Sleeping state ... */
#define WAITING_BIT   010              /* Waiting state ... */
#define NULL_BIT      020              /* No application entry */

/* Remote CAT table structure element */

typedef struct remote_cat_struct {
    char major_capability;                        /* CAT index of capability  */
    char system_id;                               /* System id field          */
    char capability_name[CAPABILITY_NAME_LENGTH]; /* Name of capability       */
    char flags;                                   /* State of capability      */
} remote_cat;

/* Local CAT table structure element */

typedef struct local_cat_struct {
    struct local_cat_struct *next_cat;  /* Next capability sleeping         */
    char  flags;                        /* State of capability              */
    char  pad;                          /* to avoid alignment problems      */
    long  sleep_time;                   /* Awakening time of capability     */
    struct SREGS sregs;                 /* Capabilities segment registers   */
    union REGS  regs;                   /* Capabilities registers           */
    short bpreg;                        /* BasePtr register                 */
    short spreg;                        /* StackPtr register                */
    int   (*ws_entry)();                /* Entry point of capability action */
    short waitreg;                      /* Wait register                    */
    mcb   *mcb_ptr;                     /* MCB of capability                */
} local_cat;

/* END INCLUDE FILE: cat.h */
