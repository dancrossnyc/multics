/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1987 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(86-10-31,Rohs), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(87-10-04,Flegel), approve(87-10-04,MCR7787),
     audit(88-01-26,DGHowe), install(88-02-24,MR12.2-1028):
     Implemented multiple queue entry strategy with Multics as the controller.
                                                   END HISTORY COMMENTS */

/* COMPILER: Lattice C, V2.15 */

/**************************************************************

     SUBRS.C

     This file contains the various support routines for BFT.

***************************************************************/

#include <stdio.h>
#include <dos.h>
#include <mowse.h>
#include <bft.h>

/* DOS Function calls */

#define DOS_GET_DRIVE     0x1900               /* Get current drive */
#define DEFAULT_DRIVE     0x4700               /* Default drive mask */

/* Masks */

#define CHAR_MASK         0xFF                 /* Zero non-char bits */

/**/
/***************************************************************

     ADDTOKEN

     PARAMETERS: ARGLIST   - a pointer to a string into which the token is 
                             to be placed. (input,output)
                 ARGLEN    - current length of arg string. If 0, then assume 
                             NULL terminated.  (input)
                 TOKEN     - a pointer to a string in which the input token 
                             resides. (input)
                 TOKEN_LEN - length of token.  If 0, then assume NULL 
                             terminated.  (input)

     FUNCTION:   Places a token at the end of the arglist followed by 
                 the DELIMITER character and returns the length of the 
                 arglist.

***************************************************************/

addtoken (p_arglist, p_arg_len, p_token, p_token_len)

char *p_arglist;       /* pointer to string to add the token to */
int  p_arg_len;        /* length of arg_list */
char *p_token;         /* pointer string where token is placed */
int  p_token_len;      /* length of token */
{
int arg_pos;           /* append position in arg_list */
int token_len;         /* actual length of token */
int i;

/* Determine actual lengths */

  arg_pos = (p_arg_len > 0) ? p_arg_len : strlen (p_arglist);
  token_len = (p_token_len > 0) ? p_token_len : strlen (p_token);

/* Append the token to the arg_list */

  for (i = 0; i < token_len; i++, arg_pos++)
    p_arglist[arg_pos] = p_token[i];

/* Append the delimeter */

  for (i = 0; i < strlen (DELIMITER); i++, arg_pos++)
    p_arglist[arg_pos] = DELIMITER[i];
  p_arglist[arg_pos] = 0;

/* Return the next position */

  return (arg_pos);
}
/**/
/***************************************************************

     BFTCAN

     PARAMETERS: ID    - request identifier of cancellation
                       - either PATH or ID
                 ID_SW - PATH_ID || TIME_ID || ENTRY_ID

     FUNCTION:   Performs the call to execute capability for
                 the cancel command.

*****************************************************************/

bftcan (p_id_sw, p_id)

int  p_id_sw;                           /* ID Type */
char *p_id;                             /* ID string */
{
char arg_string[MAXARGSTRING];          /* Message */
int  arg_len;                           /* Message length */
char token[PATHNAMESIZE];               /* Message portion */
char id_sw;                             /* Character version of ID type */

/* Verify the request id type */

  id_sw = (char)p_id_sw;
  if (!((id_sw == BFT_PATH_ID) || (id_sw == BFT_TIME_ID) || (id_sw == BFT_ENTRY_ID)))
    return (BFT_BAD_REQUEST_ID);

/* If the request type is a path, then expand the pathname */

  strncpy (token, p_id, PATHNAMESIZE);
  if (id_sw == BFT_PATH_ID)
    getpath (token);

/* make the arglist for the cancel operation: ID; */

  arg_string[0] = 0;
  arg_len = 0;
  arg_len = addtoken (arg_string, arg_len, &id_sw, 1);
  arg_len = addtoken (arg_string, arg_len, token, 0);

/* execute the CANCEL_REQUEST capability */

  return (execute (CANCEL_REQUEST, WSREMOTE, arg_string, arg_len));
}
/**/
/***************************************************************

     BFTFETCH

     PARAMETERS: MU_PATH  - source file name (input)
                 PC_PATH  - destination file name (input)
                 FLAGS    - transfer modes
                 PRIORITY - transfer priority

     FUNCTION:   Performs the call to execute capability for
                 the fetch command.

*****************************************************************/

bftfetch (p_mu_path, p_pc_path, p_flags, p_priority)

char *p_mu_path;                      /* source file name */
char *p_pc_path;                      /* destination file name */
long p_flags;                         /* Transfer modes */
int  p_priority;                      /* Transfer priority */
{
char arg_string[MAXARGSTRING];        /* holds arguments to be passed to execap  */
int  arg_len;                         /* length of the argument string */
char priority;                        /* Character representation of priority */
char flags[4];                        /* Character representation of flags */
char full_path[PATHNAMESIZE];         /* PC full pathname */
int  i;
int  j;


/* Verify the priority level */

  if ((p_priority < BFT_MIN_PRIORITY) || (p_priority > BFT_MAX_PRIORITY))
     return (BFT_INVALID_PRIORITY);

  priority = (char) p_priority;
  long2char (flags, &p_flags);

/* Expand the PC path to an absolute */

  strncpy (full_path, p_pc_path, PATHNAMESIZE);
  getpath (full_path);

/* make the arglist for the fetch operation: MU_PATH;PC_PATH;FLAGS;PRIORITY */

  arg_string[0] = 0;
  arg_len = 0;
  arg_len = addtoken (arg_string, arg_len, p_mu_path, 0);
  arg_len = addtoken (arg_string, arg_len, full_path, 0);
  arg_len = addtoken (arg_string, arg_len, flags, 4);
  arg_len = addtoken (arg_string, arg_len, &priority, 1);

/* execute the ADD_TO_STORE_QUEUE capability (store is in respect to Multics) */

  return (execute (ADD_TO_STORE_QUEUE, WSREMOTE, arg_string, arg_len));
}
/**/
/***************************************************************

     BFTRECFE

     PARAMETERS: None.

     FUNCTION:   Performs the call to execute capability for
                 the recover fetch command.

*****************************************************************/

bftrecfe ()
{

/* execute the RECOVER_STORE capability to recover PC_to_MULTICS transfers */

  return (execute (REC_STORE, WSREMOTE, NULL, 0));
}
/**/
/***************************************************************

     BFTRECST

     PARAMETERS: none.

     FUNCTION:   Performs the call to execute capability for
                 the recover store command.

*****************************************************************/

bftrecst ()
{

/* execute the RECOVER_FETCH capability to recover MULTICS_to_PC transfers */

  return (execute (REC_FETCH, WSREMOTE, NULL, 0));
}
/**/
/***************************************************************

     BFTSTORE

     PARAMETERS: PC_PATH  - source file name (input)
                 MU_PATH  - destination file name (input)
                 FLAGS    - transfer modes
                 PRIORITY - transfer priority

     FUNCTION:   Performs the call to execute capability for
                 the store command.

*****************************************************************/

bftstore (p_pc_path, p_mu_path, p_flags, p_priority)

char *p_pc_path;                      /* source file name */
char *p_mu_path;                      /* destination file name */
long p_flags;                         /* Transfer modes */
int  p_priority;                      /* Transfer priority */
{
char arg_string[MAXARGSTRING];        /* holds arguments to be passed to execap  */
int  arg_len;                         /* length of the argument string */
char priority;                        /* Character representation of priority */
char flags[4];                        /* Character representation of flags */
char full_path[PATHNAMESIZE];         /* PC full pathname */
int  i;
int  j;


/* Verify the priority level */

  if ((p_priority < BFT_MIN_PRIORITY) || (p_priority > BFT_MAX_PRIORITY))
     return (BFT_INVALID_PRIORITY);

  priority = (char) p_priority;
  long2char (flags, &p_flags);

/* Expand the PC path to an absolute */

  strncpy (full_path, p_pc_path, PATHNAMESIZE);
  getpath (full_path);

/* make the arglist for the fetch operation */

  arg_string[0] = 0;
  arg_len = 0;
  arg_len = addtoken (arg_string, arg_len, p_mu_path, 0);
  arg_len = addtoken (arg_string, arg_len, full_path, 0);
  arg_len = addtoken (arg_string, arg_len, flags, 4);
  arg_len = addtoken (arg_string, arg_len, &priority, 1);

/* execute the ADD_TO_FETCH_QUEUE capability (fetch is in respect to Multics) */

  return (execute (ADD_TO_FETCH_QUEUE, WSREMOTE, arg_string, arg_len));
}
/**/
/***************************************************************

     BFTUNLD

     PARAMETERS: none

     FUNCTION:   Unloads BFT from the PC

*****************************************************************/

bftunld ()
{

/* execute the BFT_SHUT_DOWN capability */

  return (execute (BFT_SHUT_DOWN, WSLOCAL, NULL, 0));
}
/*  */
/*******************************************************************

     EXECUTE

     PARAMETERS: MINOR      - Minor capability number to execute
                 SYSTEM     - the system of the capability
                 ARG_STRING - The arguments passed to the minor
                 ARG_LEN    - The length of the argument string

     FUNCTION:   Executes the specified minor capability on the
                 remote system with the specified arguments.

**********************************************************************/

execute (p_minor,p_system,p_arg_string,p_arg_len)

int  p_minor;                  /* minor capability number to call */
int  p_system;                 /* system to execute on */
char p_arg_string[];           /* string containing the arguments */
int  p_arg_len;                /* length of the arg string */
{
mcb *mcb_ptr;                  /* pointer to the mowse control block */
int bftarghandler();
int majnum;
int bft_major;
int code;

/* Find the appropriate bft capability */

  bft_major = 0;
  if (p_system = WSLOCAL)
  {
    if ((code = findnumb ("BFT", WSLOCAL, &bft_major)) != 0)
      return (code);
  }
  else
  {
    if ((code = findnumb ("bft_main_", WSREMOTE, &bft_major)) != 0)
      return (code);
  }

/* create the bft arg analyser instance */

  if ((code = cretinst ("BFT_ARG", bftarghandler, MAXARGSTRING, MAXARGSTRING, 0, &mcb_ptr)) != 0)
    return (code);

/* check to see if BFT_ARG is on the local system */

  majnum = 0;
  if ((code = findnumb ("BFT_ARG", WSLOCAL, &majnum)) != 0)
  {
    destinst (&mcb_ptr);
    return (code);
  }

/* execute the capability */

  if ((code = execap (bft_major, p_minor, p_arg_string, p_arg_len, mcb_ptr)) != 0)
  {
    destinst (&mcb_ptr);
    return (code);
  }

  destinst (&mcb_ptr);
  return (0);
}
/**/
/************************************************************

     BFTARGHANDLER

     PARAMETERS: none.

     FUNCTION:   This is the internal minor capability handler
                 for the BFT argument capability.  It is a NULL
                 routine as nothing is expected of it.

*************************************************************/

bftarghandler ()
{
}
/*  */
/********************************************************************

     CHAR2LONG

     PARAMETERS: STRING - character string with long in it.
                 FLAGS  - long to be coppied into.

     FUNCTION:   Copy the 4 character string into a long.

**********************************************************************/

char2long (p_string, p_flags)

char *p_string;
long *p_flags;
{
int  i;
int  j;
char *flags_ptr;

/* Copy into flags; NOTE that the characters must be coppied in backwards
   as the HIGH character is the LEAST significant */

  flags_ptr = (char *)(p_flags);
  for (i = 0, j = 3; i < 4; i++, j--)
    flags_ptr[j] = p_string[i];
}
/**/
/***********************************************************

     NAME:       GETPATH

     PARAMETERS: RELPATH- A pointer to a string which
                          contains the relative pathname
                          (input,output)

     FUNCTION:   Expands the relative pathname into an
                 absolute pathname.
*************************************************************/

getpath (relpath)
char *relpath;                            /* relative pathname to be made absolute */
{
union REGS inregs;                        /* input registers for DOS calls  */
union REGS outregs;                       /* output registers for DOS calls */
char abspath[PATHNAMESIZE+ENTRYNAMESIZE]; /* absolute pathname */
char tok[PATHNAMESIZE];                   /* token parsed out of the relative path */
char *parseptr;                           /* pointer to path while parsing */
char *stptok();                           /* token stripping routine */
int  relindex;                            /* pointer to characters in the pathname */


/* Set the drive spec */

  if (relpath[1] != ':')
  {
    relindex = 0;
    inregs.x.dx = 0;
    inregs.x.ax = DOS_GET_DRIVE;
    intdos (&inregs,&outregs);
    abspath[0] = (char) ((outregs.x.ax & CHAR_MASK)+ (int)'A');
    abspath[1] = ':';
  }
  else
  {
    strncpy(abspath,relpath,2);
    outregs.x.ax = toupper(relpath[0]) - (int)'A';
    relindex=2;
  }

/* set the pathname, user may have already given an absolute one */

  if (relpath[relindex]=='\\')
    strcpy (abspath+2,&relpath[relindex]);

  else
  {

/* Set the absolute path to the current working dir */

    abspath[2] = '\\';
    inregs.x.dx = (outregs.x.ax & CHAR_MASK) + 1;
    inregs.x.ax = DEFAULT_DRIVE;
    inregs.x.si = (short) (&abspath[3]);
    intdos (&inregs,&outregs);

/* Traverse each name in the relative path figuring out what to do with the absolute path */

    parseptr = &(relpath[relindex]);
    while ((parseptr < (relpath + strlen (relpath)))&&(strlen (abspath) < PATHNAMESIZE))
    {
      parseptr = stptok (parseptr, tok, PATHNAMESIZE, "\\") + 1;

/* if the token is not ".." then tack the dir to the end of the abspath, else back up */

      if (strcmp (tok, ".."))
      {
        if (abspath[strlen (abspath) - 1] != '\\')
          strcat (abspath, "\\");

        strncat (abspath, tok, strlen(tok));
      }
      else
      {
        relindex = strlen(abspath) - 1;
        while ((abspath[--relindex] != '\\')&&(relindex != 2));
        abspath[relindex]=0;
      }
    }
  }

/* make the pathname uppercase */

  for (relindex = 0; relindex < strlen (abspath); abspath[relindex] = toupper(abspath[relindex++]));

/* copy the absolute path back to the relative path for return */

  strncpy (relpath, abspath, PATHNAMESIZE);
}
/**/
/***********************************************************

     LONG2CHAR

     PARAMETERS: STRING - character string to put into.
                 NUM    - long to copy

     FUNCTION:   Copy into num into string, NOTE that the 
                 characters must be coppied in backwards
                 as the HIGH character is the LEAST significant.

*************************************************************/

long2char (p_string, p_num)

char *p_string;
long *p_num;
{
char *num_ptr;
int  i;
int  j;

  num_ptr = (char *)(p_num);
  for (i = 0, j = 3; i < 4; i++, j--)
    p_string[i] = num_ptr[j];
}
/**/
/***********************************************************
     NAME:       STRIPQUOTE

     PARAMETERS: STRING - a pointer to a string from which
                          the quotes are to be stripped
                          (input)

     FUNCTION:   If there are leading and trailing quotes
                 on a string then they are removed and a pointer to
                 the new string is returned.
*************************************************************/

char *stripquote (string)

char *string;                         /* input string */
{
char tempstr[PATHNAMESIZE];           /* temporary string */

/* if there are leading and trailing quotes then remove them */

  if (string[0]=='"' && string[strlen(string)-1]=='"')
  {
    strcpy (tempstr,string);
    strncpy(string,tempstr+1,strlen(tempstr)-2);
  }

  return(string);
}
/**/
/***********************************************************

     STRIPTOK

     PARAMETERS: STRING - a pointer to a string from which
                          the token is to be parsed from.
                          (input)
                 TOKEN  - a pointer to a string in which the
                          parsed token is placed. (output)

     FUNCTION:   Gets the next token out of a string (which
                 is separated by the DELIMITER character) and 
                 returns a pointer to the character after the 
                 delimiter.

*************************************************************/

char *striptok (string, token)

char *string;                         /* input string */
char *token;                          /* string where token is placed */
{
int i;                                /* counter variable */

/* find position of delimiter in the source string */

  for (i = 0; string[i] != DELIMITER[0]; i++)
    token[i] = string[i];

  token[i] = 0;

/* return the address of the character after the delimiter */

  return (string + i + 1);
}
/**/
/***********************************************************

     SYSTEM SHUTDOWN

     PARAMETERS: None.

     FUNCTION:   Crash the system.

*************************************************************/

system_shutdown()
{
  fprintf (stderr, "\nBFT:  Fatal error encountered.\n");
}
