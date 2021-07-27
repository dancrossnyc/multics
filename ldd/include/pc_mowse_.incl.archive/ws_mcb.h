/* BEGIN INCLUDE FILE  ws_mcb.h */

/* HISTORY COMMENTS:
  1) change(86-05-31,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Defines the mowse control block structure Equivalent include file ws_mcb.mac
*/

#define MCB_DEFINED         1

#ifndef ALLOCH
#include <alloc.h>
#endif

#define MCB_SUSPEND         1          /* Suspended flag */
#define MCB_TERMINATE       2          /* Terminating flag */
#define MCB_SLEEP           4          /* Sleeping flag */
#define MCB_NULL_ROUTINE    8          /* Null routine flag */

struct linklst {
   struct linklst *nextbuf;            /* pointer to next buffer chain */
   struct linklst *nextlink;           /* pointer to next buffer link  */
   int    linksize;                    /* size of this link            */
   int    linkused;                    /* amount of link used          */
   char   lldata[1];                   /* data place holder            */
};

typedef struct mcb_struct {
   char   major_capability;            /* Major capability number */
   char   system_id;                   /* System of capability */
   char   mcb_flag;                    /* MCB information */
   char   capability_name[CAPABILITY_NAME_LENGTH];
   int    (*entry_point_offset)();     /* pre-entry point of application */
   int    (*application_entry)();      /* Actual entry of application */
   char   *data_block_ptr;             /* Application data */
   struct allocstr *inalloc;           /* Input buffer */
   int    inbuff_length;               /* Length of input buffer */
   struct linklst *inbuff;             /* Inbuffer data */
   struct allocstr *outalloc;          /* Outbuffer */
   int    outbuff_length;              /* Length of outbuffer */
   struct linklst *outbuff;            /* Outbuffer data */
   double low_memory;                  /* Memory bounds of application */
   double high_memory;                 /* ... */
} mcb;

/* END INCLUDE FILE  ws_mcb.h */
