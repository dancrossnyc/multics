/* BEGIN INCLUDE FILE  ws_fgb.h */

/* HISTORY COMMENTS:
  1) change(86-05-31,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Defines the foreground linked buffer structure.  Equivalent include file 
ws_fgb.mac
*/


/* foreground buffer structure */
struct fgbstr {
   struct fgbstr   *fgb_next;       /* pointer to next fg buffer    */
   int             fgb_length;      /* length of data in buffer     */
   char            fgb_minor;       /* minor capability number      */
   char            fgb_char[1];     /* first character if fg data   */
};

/* END INCLUDE FILE  ws_fgb.h */
