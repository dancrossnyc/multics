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
  2) change(86-12-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     When called with a status_reply, place message
     into status list.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (i_putbgm)

Places a message on the background message queue for subsequent retrieval by 
a foreground program. In addition to placing the message on the queue, a 
count of the number of messages on the queue is incremented.
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
extern struct fgbstr *sgbfptr;         /* first buffer on Status linked list */
extern struct fgbstr *sgblptr;         /* last buffer on Status linked list */
extern struct allocstr *bgaptr;        /* allocation data for linked list*/
extern int bgcount;                    /* count of bg messages in linked list */

i_putbgm (p_param)

struct putbg_struc *p_param;
{
int i;                  /* copy counter */
char *s1;               /* pointer to source for copy */
char *d1;               /* pointer to destination of copy */
struct fgbstr *bufp;    /* pointer to current message structure */
struct fgbstr *fgbp1;   /* temporary pointer to  message structure */
struct query_msg *qrp;  /* definition of query/reply message */
int sysid;              /* system id extracted from sender major */
int cap_num;            /* capability number extracted from sender major */

/* : Allocate a buffer and place it on the linked list, 1 extra for message type */

   bufp = (struct fgbstr *) wsalloc (bgaptr,p_param -> length + 1 + sizeof(struct fgbstr));

/* : if allocation successful
     - copy to background message buffer     */

   if (bufp != NULL) {
      d1 = &bufp -> fgb_minor;
      *d1++ = (char) p_param -> type;
      c_unpack (&sysid, &cap_num, p_param -> sender_major);
      *d1++ = (char) sysid;
      *d1++ = (char) cap_num;
      s1 = p_param -> bgmsg;
      for (i = 0;i < p_param -> length;i++) {
         *d1++ =  *s1++;
      }
      bufp -> fgb_length = p_param -> length + 2;     /* add in sysid, cap_num */

/* : - link to previous status buffers OR background buffers  */

      bufp -> fgb_next = NULL;

      if (p_param -> type == STATUS_REPLY)
      {  if (sgbfptr == NULL) {
            sgbfptr = bufp;
            sgblptr = bufp;
         }
         else {
            fgbp1 = bgblptr;
            fgbp1 -> fgb_next = bufp;
            sgblptr = bufp;
         }
      }
      else
      {  if (bgbfptr == NULL) {
            bgbfptr = bufp;
            bgblptr = bufp;
            bgcount = 1;
         }
         else {
            fgbp1 = bgblptr;
            fgbp1 -> fgb_next = bufp;
            bgblptr = bufp;
            bgcount++;
         }
      }
   }

/* : return parameter structure to caller */

   return(0);
}
