/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-07-11,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_getbgm)

Get the next enqueued message from a background application that was sent 
to the foreground application. This routine should only be called by a 
foreground application since the messages are assumed to be intended for 
a human being.
*/

#include <dos.h>
#include <alloc.h>
#include <cat.h>
#include <ws_dcls.h>
#include <ws_error.h>
#include <ws_msg.h>
#include <ws_fgb.h>
#include <wsmincap.h>

#define NULL 0

extern struct fgbstr *bgbfptr;         /* first buffer on linked list */
extern struct fgbstr *bgblptr;         /* last buffer on linked list */
extern struct fgbstr *sgbfptr;         /* first buffer on linked list */
extern struct fgbstr *sgblptr;         /* last buffer on linked list */
extern struct allocstr *bgaptr;        /* allocation data for linked list*/
extern int bgcount;                    /* count of bg messages in linked list */

i_getbgm (p_param)

struct gbgmsg_struc *p_param;
{
int i;                  /* copy counter */
char *s1;               /* pointer to source for copy */
char *d1;               /* pointer to destination of copy */
struct fgbstr *bgbp;    /* pointer to current message structure */
struct query_msg *qrp;  /* definition of query/reply message */

/* : if a status message is desired
     - If no status messages on queue return WSNOMESS
     - Remove bg buffer from queue    */

   if (p_param -> bg_type == STATUS_REPLY) {
      if (sgbfptr == NULL) {
         sgblptr = NULL;
         return (WSNOMESS);
      }

      bgbp = sgbfptr;
      sgbfptr = bgbp -> fgb_next;
      if (sgbfptr == NULL)
         sgblptr = NULL;
   }

/* : else if a background message is desired
     - If no bg messages on queue, reset bgcount, and return WSNOMESS
     - Remove bg buffer from queue    */

   else {
      if (bgbfptr == NULL) {
         bgcount = 0;
         bgblptr = NULL;
         return (WSNOMESS);
      }

      bgbp = bgbfptr;
      bgbfptr = bgbp -> fgb_next;
      --bgcount;
      if (bgbfptr == NULL) {
         bgcount = 0;
         bgblptr = NULL;
      }
   }

/* : copy buffer contents to p_param     */

   p_param -> length = bgbp -> fgb_length - 2;        /* Subtract out sysid, cap */
   qrp = (struct query_msg *) &bgbp -> fgb_minor;
   d1 = &p_param -> bgmsg[0];
   s1 = &qrp -> msg_data[0];
   for(i = 0; i < p_param -> length; i++)
      *d1++ = *s1++;
 
/* : insert sender major and type into structure */

   c_pack (qrp -> source_system, qrp -> source_major, &p_param -> sender_major);
   p_param -> bg_type = (int) bgbp -> fgb_minor;

/* : Free the buffer */

   wsfree(bgaptr,bgbp);

   return(0);
}
