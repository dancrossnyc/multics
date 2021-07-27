/*** BEGIN INCLUDE FILE: wsthist.h ***/

/* HISTORY COMMENTS:
  1) change(88-07-29,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
     Created.
                                                   END HISTORY COMMENTS */

/* Function:
    This include defines constants and structures for WSTERM's
    history recall facility.
*/

#ifndef WSTHIST

#define HIST_BUFF_SIZE         4096   /* size of history buffer */
#define HIST_MAX_LINES           22   /* history lines displayed per screen */
#define HIST_START_DIR            0   /* no history buffer traversal flag */
#define HIST_PREVIOUS_DIR         1   /* backward history traversal flag */
#define HIST_NEXT_DIR             2   /* forward history traversal flag */
#define HIST_MAX_SCREEN_SHIFT   980   /* maximum offset for screen shift */
#define HIST_DEFAULT_0_ARG        0   /* no history command selected flag */


/* This structure maintains a circular buffer which is used as the
   history buffer. Each command placed in the history buffer has the
   following format:

      [2 bytes for item size] + [command bytes] + [2 bytes for item size]

   Thus, a total of 4 additional bytes are stored with each command.
   The size bytes at the beginning and end of each item allows for
   bi-directional traversal of the history buffer.

   head, tail, and current are indices to a circular buffer and as
   such, their values are incremented or decremented using modular
   arithmetic (of history buffer size) to ensure they access only
   valid history buffer locations.

*/

typedef struct hist_buff_struct {
    int head;             /* indexes past most recently saved command */
    int head_item_size;   /* size of most recently saved command+4 */
    int tail;             /* indexes to the least recently saved command */
    int tail_item_size;   /* size of least recently saved command+4 */
    int chars_free;       /* number of bytes unused in kill buffer */
    int current;          /* index to next item to retrieve */
    int current_size;     /* size of next command to retrieve+4 */
    int head_of_histbuff; /* flag indicating current = head */
    int direction;        /* previous direction of traversal */
    char kb [HIST_BUFF_SIZE];   /* history buffer space */
} HIST_BUFF_INFO;



/* this structure contains display control information for displaying
   the history screen. The 'start' values are initial values and the
   'cur' values are running values.
*/

typedef struct item_info_struct {
    int start_item_no;       /* id of history command, most recent=1 */
    int start_chars_in_buff; /* count of chars not yet displayed */
    int start_display_count; /* count of commands displayed on screen */
    int start_item_index;    /* history buffer index to next item to fetch */
    int start_size;          /* size of next history buffer item to fetch */
    int cur_item_no;         /* id of current history command fetched */
    int cur_chars_in_buff;   /* count of chars not yet displayed */
    int cur_display_count;   /* count of commands currently displayed */
    int cur_item_index;      /* history buffer index of current item fetched */
    int cur_size;            /* size of current history buffer item to fetch */
} ITEM_INFO;


/* global structure which serves as the history buffer */
HIST_BUFF_INFO hbi;

#define WSTHIST
#endif WSTHIST

/*** END INCLUDE FILE: wsthist.h */
