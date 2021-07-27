/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1987 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(87-12-18,Flegel), approve(87-10-04,MCR7787),
     audit(88-01-26,DGHowe), install(88-02-24,MR12.2-1028):
     Created for the building of error messages.
                                                   END HISTORY COMMENTS */

/* COMPILER: Lattice C, V2.15 */

#include <stdio.h>
#include <bft.h>
#include <mowse.h>

/* This error table was created from the standard Lattice C "error.h" */

#define D_SYSERR
char *sys_error_table[] =
     {
          "No error",
          "User is not owner",
          "No such file or directory",
          "No such process",
          "Interrupted system call",
          "I/O error",
          "No such device or address",
          "Arg list is too long",
          "Exec format error",
          "Bad file number",
          "No child process",
          "No more processes allowed",
          "No memory available",
          "Access denied",
          "Bad address",
          "Bulk device required",
          "Resource is busy",
          "File already exists",
          "Cross-device link",
          "No such device",
          "Not a directory",
          "Is a directory",
          "Invalid argument",
          "No more files (units) allowed",
          "No more files (units) allowed for this process",
          "Not a terminal",
          "Text file is busy",
          "File is too large",
          "No space left",
          "Seek issued to pipe",
          "Read-only file system",
          "Too many links",
          "Broken pipe",
          "Math function argument error",
          "Math function result is out of range"
     };

int  sys_error_table_size =
     {
          sizeof (sys_error_table) / sizeof (sys_error_table[0])
     };

/* This error table is created from the error codes in "BFT.H" */

#define D_BFTERR
char *bft_error_table[] =
     {
          "No error",
          "Invalid priority",
          "Invalid minor capability",
          "Bad command argument",
          "Bad option to argument",
          "Argument expected",
          "No argument",
          "Invalid request id type",
          "Bad pathname specified",
          "Incomaptible control args"
     };

int  bft_error_table_size =
     {
          sizeof (bft_error_table) / sizeof (bft_error_table[0])
     };
/**/
/***************************************************************

     BFTERROR

     PARAMETERS: CODE      - Error code.
                 MESSAGE   - Error message.
                 MCB_PTR   - NULL if it is a stderr, otherwise a BG message.

     FUNCTION:   Display an error message to the stderr switch.

*****************************************************************/

bfterror (p_code, p_message, p_mcb_ptr)

int  p_code;                           /* Error code */
char p_message[];                      /* Error message */
mcb  *p_mcb_ptr;
{
char error_message[MAXARGSTRING];      /* Message generated */


  if (p_code == 0)
    return (p_code);

  if (issyserr (p_code))
  {
    strcpy (error_message, getsyserr (p_code));
  }
  else if (isbfterr (p_code))
  {
    strcpy (error_message, bft_error_table[p_code-BFT_BASE_ERROR]/*getbfterr (p_code)*/);
  }
  else switch (p_code)
  {
    case (WSINVBUF):
      strcpy (error_message, "Invalid buffer size");
      break;

    case (WSCNTCRE):
      strcpy (error_message, "Can't create instance");
      break;

    case (WSINVNAM):
      strcpy (error_message, "Invalid entry name");
      break;

    case (WSNOTACT):
      strcpy (error_message, "MOWSE not active");
      break;

    case (WSNOTRES):
      strcpy (error_message, "MOWSE is not resident");
      break;

    case (WSINVSYS):
      strcpy (error_message, "Invalid system");
      break;

    case (WSINVMCB):
      strcpy (error_message, "Invalid MCB pointer");
      break;

    default:
      sprintf (error_message, "Error %d", p_code);
      break;
  }

  if (p_mcb_ptr != NULL)
    putbgmes (p_mcb_ptr, 0,"BFT", "%s.  %s", error_message, p_message);
  else
    fprintf (stderr, "BFT: %s.  %s\n", error_message, p_message);

  return (p_code);
}
