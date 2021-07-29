/* BEGIN INCLUDE FILE ws_load.h */

/* HISTORY COMMENTS:
  1) change(86-10-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* FUNCTION

Define the necessary information to support the autoload facility.  The
capability to be autoloaded MUST be in the search path of the PC
*/

#define AUTO_LIMIT       32        /* Maximum number of capabilities to load */
#define AUTO_LENGTH      9         /* Maximum number of characters in load name */
#define AUTO_ACTIVE      001       /* Autoload in progress */
#define AUTO_COMPLETE    002       /* Autoload complete */
#define AUTO_ON          004       /* Specifies that the entry is valid */
#define AUTO_PENDING     001       /* General flag indicates pending loads */

typedef struct
{  char   name[AUTO_LENGTH];       /* Name of capability to autoload */
   int    flags;                   /* Flags associated with autoload */
} AUTO, *AUTO_PTR;

/* END INCLUDE FILE ws_load.h */
