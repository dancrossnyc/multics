/* BEGIN INCLUDE FILE alloc.h */

/* HISTORY COMMENTS:
  1) change(86-07-04,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */


/* FUNCTION

Defines the necessary structures for wsalloc, a memory allocation scheme 
written for MOWSE.
*/

/* NOTES

This is essentially a copy of the allocation scheme described in K&R, 
page 173.
*/

#define ALLOCH  1
typedef int ALIGN;               /* force alignment */

union header {                   /* free block header */
   struct {
      union header *ptr;         /* next free block */
      unsigned size;             /* size of this free block */
   } s;                          
   ALIGN   x;                    /* force alignment of blocks */
};

typedef union header HEADER;

struct allocstr {
   int memory_used;              /* non-zero, if all memory used */
   int memory_size;              /* size of memory block */
   char *memory;                 /* pointer to memory block */
   HEADER m_base;                /* base free block header */
   HEADER *m_allocp;             /* last allocated block */
};

/* END INCLUDE FILE alloc.h */
