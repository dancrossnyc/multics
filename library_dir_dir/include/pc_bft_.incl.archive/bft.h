/* BEGIN INCLUDE FILE: BFT.H */

/* COMPILER: Lattice C, V2.15 */


/* HISTORY COMMENTS:
  1) change(86-10-31,Rohs), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(87-10-23,Flegel), approve(87-10-23,MCR7787),
     audit(88-01-26,DGHowe):
     Added appropriate changes to support multiple queue entries and having
     Multics perform ALL of the queue handling.
                                                   END HISTORY COMMENTS */

/* NOTES

In this implementation of the compiler (Lattice C, V2.15), the type "long"
is a 4-character / 32-bit entity.  This is important as the flags in the 
data_block MUST be 32 bits long - hence the use of type "long".
*/

/**************************************************************
                         Constants
***************************************************************/

#define MAXARGSTRING         256 /* size of argument string passed to remote */
#define STRINGSIZE           118 /* string size of error messages and such */
#define PATHNAMESIZE         168 /* size of maximum pathname expected */
#define ENTRYNAMESIZE         13 /* Size of a PC entryname */
#define STACKSIZE          25000 /* amount of stack space allocated to BFT */
#define DISKBUFF            1024 /* number bytes read in during a disk access */
#define BUFFSIZE             110 /* number bytes of the file in each "packet" */
#define MU_ENTRYNAME_SIZE     33 /* Size of Multics entrynames */

#define BFT_MIN_PRIORITY       1 /* Lowest priority level */
#define BFT_MAX_PRIORITY       4 /* Highest priority level */
#define BFT_PATH_ID           32 /* Request ID = pathname */
#define BFT_TIME_ID           33 /* Request ID = ID */
#define BFT_ENTRY_ID          34 /* Request ID = Entry */

#define BFT_LOUD              32 /* Loud shutdown message */
#define BFT_QUIET             33 /* Quiet shutdown */

#define BFT_FIRST             32 /* Find first match */
#define BFT_NEXT              33 /* Find next match */

#define DELIMITER         "\377" /* delimter between args in the argstring */

/**************************************************************
                         Error Codes
***************************************************************/
/* The sys_error_table and bft_error_table exists in bfterror.c */

#define BFT_BASE_ERROR       100 /* Base error number */

#ifndef D_SYSERR
extern  char *sys_error_table[];
extern  int   sys_error_table_size;
extern  int   errno;
#endif

#ifndef D_BFTERR
extern  char *bft_error_table[];
extern  int   bft_error_table_size;
#endif

#define issyserr(c)          ((c >= 0)&&(c <= sys_error_table_size))
#define isbfterr(c)          ((c >= BFT_BASE_ERROR)&&(c <= bft_error_table_size + BFT_BASE_ERROR))
#define getsyserr(c)         ((issyserr(c)) ? sys_error_table[c] : NULL)
#define getbfterr(c)         ((isbfterr(c)) ? bft_error_table[c-BFT_BASE_ERROR] : NULL)

#define BFT_NOERROR            0 /* No error */
#define BFT_INVALID_PRIORITY 101 /* Invalid priority */
#define BFT_INVALID_MINOR    102 /* Invalid minor capability */
#define BFT_BADARG           103 /* Bad command argument */
#define BFT_BADOPT           104 /* Bad option to argument */
#define BFT_EXPECTING        105 /* Argument expected */
#define BFT_NOARG            106 /* No argument */
#define BFT_BAD_REQUEST_ID   107 /* Invalid request id type */
#define BFT_BAD_PATH         108 /* Bad pathname specified */
#define BFT_INCOMPATIBLE     109 /* Incomaptible control args */

/**************************************************************
                         Control Flags
***************************************************************/
/* Transfer mode flags - used to mask with BFT_DATA.x_flags */

#define BFT_NOTIFY        0x01   /* Completion notification */
#define BFT_BINARY        0x02   /* Binary mode */
#define BFT_INITIATED     0x04   /* Compatibility with Multics switches */
#define BFT_ALLOCATED     0x08   /* Compatibility with Multics switches */

/**************************************************************
                         Structures
***************************************************************/

/* Data pointer structure for use with BFT. */

typedef struct
{
  long inpos;                            /* source file input position */
  int  inbuffpos;                        /* source file buffer input position */
  int  inbufflen;                        /* source file input buffer length */
  int  outbuffpos;                       /* destination file buffer output position */
  long outpos;                           /* file position of next write */
  char source_file     [PATHNAMESIZE];   /* source file name */
  long source_flags;                     /* source transfer modes */
  char destination_file[PATHNAMESIZE];   /* destination file name */
  long destination_flags;                /* destination transfer modes */
  char inbuffer        [DISKBUFF+1];     /* buffer for file Input */
  char outbuffer       [DISKBUFF+1];     /* buffer for file Output */
  char expand_dirname  [PATHNAMESIZE];   /* Path for expanding paths with */
} bft_data, *bft_data_ptr;

/**************************************************************
                BFT minor capability numbers
***************************************************************/

/* BFT internal minor capabilities */

#define ADD_TO_FETCH_QUEUE    64  /* adds the transfer request to the queue */
#define ADD_TO_STORE_QUEUE    65  /* adds the transfer request to the queue */
#define CHECK_FILE_LENGTH     66  /* finds the length of the destination file */
#define INITIATE_FETCH        67  /* opens the destination file on the remote */
#define BFT_SHUT_DOWN         68  /* Shutdown bft */
#define POSITION_FILE_POINTER 69  /* opens a file and for read at location x */
#define REC_DATA              70  /* receive a packet of a file a writes it */
#define REC_EOF               71  /* closes the destination file */
#define REC_FETCH             72  /* recover fetch */
#define REC_STORE             73  /* recover store */
#define READ_ERROR            74  /* read error encounted on the remote */
#define INITIATE_STORE        75  /* see if the remote can start a store */
#define WRITE_ERROR           76  /* called when the remote couldnt write */
#define SEND_DATA             77  /* reads a packet of the file and sends it */
#define CANCEL_REQUEST        78  /* Cancel a request from the queue */
#define EXPAND_PC_PATH        80  /* expand pathname */
#define FULL_PC_PATH          81  /* expanded pc path */

/* END INCLUDE FILE: BFT.H */
