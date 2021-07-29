/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-06,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION

The following routines are general purpose "memory management" routines used
to manage internal memory buffers.  They are direct copies from 
Kernigan & Ritchie.
*/

#include <stdio.h>
#include <alloc.h>

char *wsalloc (mptr,nbytes)

struct allocstr *mptr;
unsigned nbytes;
{
HEADER *morecore();
register HEADER *p, *q;
register int nunits;

   nunits = 1+(nbytes+sizeof(HEADER) -1)/sizeof (HEADER);
   if ((q = mptr->m_allocp) == NULL) {  /* no free list yet */
      mptr->m_base.s.ptr = mptr->m_allocp =q = &mptr->m_base;
      mptr->m_base.s.size = 0;
   }

   for (p=q->s.ptr; ;q=p, p=p->s.ptr) {
      if (p->s.size >= nunits) {  /* big enough */
         if (p->s.size == nunits)    /* exactly */
            q->s.ptr = p->s.ptr;
         else {  /* allocate tail end */
            p->s.size -= nunits;
            p += p->s.size;
            p->s.size = nunits;
         }
         mptr->m_allocp = q;
         return ((char *) (p+1));
      }
      if ( p == mptr->m_allocp)  /* wrapped around free list */
         if ((p = morecore (mptr,nunits)) == NULL)
            return (NULL);  /* none left */
   }
}

wsfree (mptr,ap)

struct allocstr *mptr;
char *ap;
{
register HEADER *p, *q;

   p = (HEADER *) ap -1;  /* point to header */
   for (q=mptr->m_allocp; !(p > q && p < q->s.ptr); q=q->s.ptr)
      if (q >= q->s.ptr && (p > q || p < q->s.ptr))
         break;   /* at one end or other */

   if (p+p->s.size == q->s.ptr) {  /* join to upper neighbor */
      p->s.size += q->s.ptr->s.size;
      p->s.ptr = q->s.ptr->s.ptr;
   }
   else
      p->s.ptr = q->s.ptr;
   if (q+q->s.size == p) {  /* join to lower neighbor */
      q->s.size += p->s.size;
      q->s.ptr = p->s.ptr;
   }
   else
      q->s.ptr = p;
   mptr->m_allocp = q;
}

HEADER *morecore (mptr,nu)

struct allocstr *mptr;
unsigned nu;
{
register HEADER *up;
char *cp;

   if (mptr->memory_used == 1) 
      return(NULL);
   cp = mptr->memory;
   mptr->memory_used = 1;
   up = (HEADER *)cp;
   up->s.size = mptr->memory_size / sizeof (HEADER);
   wsfree (mptr,(char *) (up+1));
   return (mptr->m_allocp);
}
