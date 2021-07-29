/* BEGIN INCLUDE FILE ws_buf.h */

/* HISTORY COMMENTS:
  1) change(86-06-01,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Defines the structure of a circular buffer.  Equivalent include file ws_buf.mac
*/

struct bstruc  {                            /* circular buffer structure */
   int     bsize;                           /* Size of buffer */
   char    *bfirst;                         /* First char in buffer */
   char    *blast;                          /* Last char in buffer */
   char    *bin;                            /* First data in buffer */
   char    *bout;                           /* Last data in buffer */
   char    bminor;                          /* Minor of data */
   char    mbuffer[1];                      /* data holding place */
};

/* END INCLUDE FILE ws_buf.h */
