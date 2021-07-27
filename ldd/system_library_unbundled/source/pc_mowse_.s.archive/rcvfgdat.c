/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created from extraction of rcvdata.c
  2) change(86-12-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Placed status reply messages into Status buffer .
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (rcvfgdat):

Receive foreground data and dump it into the foreground terminal data queue
or into the background message queue accordingly.
*/

#include "dos.h"
#include "stdio.h"
#include "ws.h"
#include "alloc.h"
#include "cat.h"
#include "ws_fgb.h"
#include "ws_buf.h"
#include "ws_msg.h"
#include "wsmincap.h"
#include "ws_error.h"

extern struct fgbstr   *fgbfptr;       /* FG buffer first pointer */
extern struct fgbstr   *fgblptr;       /* FG buffer last pointer */
extern struct fgbstr   *bgbfptr;       /* BG buffer first pointer */
extern struct fgbstr   *bgblptr;       /* BG buffer last pointer */
extern struct fgbstr   *sgbfptr;       /* Status buffer first pointer */
extern struct fgbstr   *sgblptr;       /* Status buffer last pointer */
extern struct allocstr *fgaptr;        /* FG allocation buffer */
extern struct allocstr *bgaptr;        /* BG allocation buffer */
extern int             bgcount;        /* Number of messages in BG buffer */

rcvfgdat (p_fg_buff_ptr)
struct bstruc *p_fg_buff_ptr;
{
int i;
int length;
struct fgbstr *bufp;
char *p1;
char *f1;

/* EXTERNAL PROCEDURES */
int  rcvmsg();
char *wsalloc();


/* : If no foreground data, return */
   if (p_fg_buff_ptr -> bin == p_fg_buff_ptr -> bout)
      return (0);

/* : Allocate space for buffer */
   length = p_fg_buff_ptr -> bin - p_fg_buff_ptr -> bout;
   if (length < 0)
      length += p_fg_buff_ptr ->bsize;

   if (length >WSMINBUF)
      length = WSMINBUF;

   if ((p_fg_buff_ptr -> bminor == BG_MESSAGE)
      || (p_fg_buff_ptr -> bminor == BG_QUERY) 
      || (p_fg_buff_ptr -> bminor == STATUS_REPLY))
   {
      bufp = (struct fgbstr *) wsalloc (bgaptr,length + sizeof(struct fgbstr));
   }
   else
      bufp = (struct fgbstr *) wsalloc (fgaptr,length + sizeof(struct fgbstr));

/* : If allocation failed, lose the message and return */
   if (bufp == NULL)
      return (0);

/* : copy to foreground buffer */
   p1    = &bufp->fgb_minor;
   *p1++ = p_fg_buff_ptr -> bminor;
   f1    = p_fg_buff_ptr -> bout;

   for (i = 0; i < length; i++)
   {  *p1++ =  *f1++;
      if (f1 > p_fg_buff_ptr -> blast)
         f1 = p_fg_buff_ptr -> bfirst;
   }

   p_fg_buff_ptr -> bout = f1;
   bufp->fgb_length = length;
   bufp->fgb_next = NULL;

/* : Link to previous background message buffer */
   if ((p_fg_buff_ptr -> bminor == BG_MESSAGE)
      || (p_fg_buff_ptr -> bminor == BG_QUERY))
   {  if (bgbfptr == NULL)
         bgcount = 1;
      else
         bgcount++;
      rcvmsg (bufp, &bgbfptr, &bgblptr);
   }
   else if (p_fg_buff_ptr -> bminor == STATUS_REPLY)
      rcvmsg (bufp, &sgbfptr, &sgblptr);

/* : Else link to foreground message buffer */
   else
      rcvmsg (bufp, &fgbfptr, &fgblptr);
}
