/* BEGIN INCLUDE FILE ws_stack.h */

/* HISTORY COMMENTS:
  1) change(86-06-10,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Defines mowse stack format.  Equivalent include file ws_stack.mac
*/

typedef struct ws_stack_struct  {
   int   ipreg;         /* register save area */
   int   axreg;
   int   bxreg;
   int   cxreg;
   int   dxreg;
   int   sireg;
   int   direg;
   int   bpreg;
   int   spreg;
   int   flreg;
   int   esreg;
   int   csreg;
   int   ssreg;
   int   dsreg;
   int   bpsave;        /* just here for debugging */
   int   chan;          /* mowse channel */
   int   datac;         /* count of datap to use for snddat */
   int   datap[3];      /* pointers to data strings for snddat */
   int   datal[3];      /* length of data strings for snddat */
   char  pkthdr[8];     /* packet header work area */
   int   wsparm;        /* base address for local data */
} ws_stack;

/* END INCLUDE FILE ws_stack.h */
