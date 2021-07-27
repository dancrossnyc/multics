/*** BEGIN INCLUDE FILE: wstkill.h ***/

/* HISTORY COMMENTS:
  1) change(88-08-08,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     Created.
                                                   END HISTORY COMMENTS */

/* Function:
        define structures and constants used in implementing the
    kill ring for edit mode editing.
*/

#ifndef WSTKILL

/* size of kill buffer; should be at least maximum size of line edited */
#define KILL_BUFF_SIZE 1024

/* This structure maintains a circular buffer which is used as a
   kill ring. Each item placed in the kill ring has the following
   format:
      [2 bytes for item size] + [data bytes] + [2 bytes for item size]

   Thus, a total of four additional bytes are stored with each item.
   The size bytes at the beginning and end of the item allows for
   bi-directional traversal of the kill ring.

   head, tail, current and yank_index are indices to a circular buffer
   and as such, their values are incremented or decremented using
   modular arithmetic (of kill buffer size) to ensure they access
   only valid kill buffer locations.

*/

typedef struct kill_buff_struct {
    int head;             /* indexes past most recently saved item */
    int head_item_size;   /* size of most recently saved item */
    int tail;             /* indexes to the least recently saved item */
    int tail_item_size;   /* size of least recently saved item */
    int chars_free;       /* number of bytes unused in kill buffer */
    int current;          /* index to next item to retrieve */
    int current_size;     /* size of next item to retrieve */
    int head_of_killbuff; /* flag indicating current = head */
    int yank_index;       /* index into line edited of where last yanked */
    int yank_size;        /* size of last item yanked */
    char kb [KILL_BUFF_SIZE];  /* kill buffer space */
} KILL_BUFF_INFO;


/* global structure which serves as the kill ring */
KILL_BUFF_INFO kill_info;

#define WSTKILL
#endif WSTKILL

/*** END INCLUDE FILE: wstkill.h ***/
