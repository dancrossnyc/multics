/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1985 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-10-31,Rohs), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(87-10-27,Flegel), approve(87-10-29,MCR7787),
     audit(88-01-26,DGHowe), install(88-02-24,MR12.2-1028):
     Re-designed to have Multics BFT maintain ALL queues.
  3) change(88-02-19,Flegel), approve(88-09-07,MCR7978), audit(88-09-14,Lee),
     install(88-10-07,MR12.2-1145):
     Repair expand_path to correctly return the appropriate second component.
  4) change(88-09-28,Flegel), approve(88-09-28,MCR7998), audit(88-09-28,Lee),
     install(88-10-07,MR12.2-1145):
     Repair ascii mode for the transfer of small files (<512 Kbytes) to the PC.
                                                   END HISTORY COMMENTS */

/* COMPILER: Lattice C, V2.15 */

/**************************************************************

     BFT_LOAD.C

     This file contains the minor capabilites for 
     Background File Transfer, and the routines needed to load
     BFT as a major capability on the PC.

***************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <dos.h>
#include <mowse.h>
#include <bft.h>

#define SEEK_START     0      /* Beginning positioning in lseek */
#define SEEK_CURRENT   1      /* Current positioning in lseek */
#define SEEK_EOF       2      /* EOF positioning in lseek */

#define DOS_STAR_FIRST 0x4E   /* DOS Function to locate first starname entry */
#define DOS_STAR_NEXT  0x4F   /* DOS Function to locate next starname entry */
#define DOS_SET_DTA    0x1A   /* DOS Function to set DTA */

#define MAXLONG        0x7FFFFFFF

int _stack = STACKSIZE;     /* Lattice C constant to set stack size of a program */

struct dta_struct           /* DTA structure for starnames */
{
   char filler[30];
   char name[13];
} dta;

long lseek ();              /* File positioning */
char *stptok();             /* C Runtime support */
char *striptok();           /* Extracting tokens from messages */
int  bftmajcap();           /* bft major capability handler */

/*  */
/************************************************************

     BFT_LOAD  (MAIN)

     PARAMETERS: ARGC - number of arguments on the command
                        line. (input)

     FUNCTION:   Loads the BFT major capbility into the CAT.

*************************************************************/

main (argc)

int  argc;                            /* number of args */
{
int majnum;                           /* major capability number of the local system */
int majnumr;                          /* major capability number of the remote system */
int com_id;                           /* command id */
int code;                             /* error code */
mcb *mcb_ptr;                         /* mowse control block pointer */
bft_data_ptr data_ptr;                /* pointer to bft's "static" data */


/* make sure no additional args are given on the command line */

  if (argc > 1)
  {
     printf ("BFT:  Unexpected argument.\n   Usage: bft_load\n");
     return;
  }

/* check to see if BFT is already on the local system */

  majnum = 0;
  if (findnumb ("BFT", WSLOCAL, &majnum) == 0)
  {
    fprintf (stderr, "BFT:  Already loaded.\n");
    return;
  }

/* allocate space for bft's data */

  data_ptr = (bft_data_ptr) malloc (sizeof (bft_data));
  if (data_ptr == NULL)
  {
     fprintf (stderr, "BFT:  Memory allocation error.\n");
     return;
  }

/* initialize variables that need it */

  data_ptr->source_file[0] = 0;
  data_ptr->source_flags = 0L;
  data_ptr->destination_file[0] = 0;
  data_ptr->destination_flags = 0L;
  data_ptr->inbuffpos = 0;
  data_ptr->outbuffpos = 0;
  data_ptr->inpos = 0L;
  data_ptr->outpos = 0L;
  data_ptr->inbufflen = 0;
  data_ptr->inbuffer[0] = '\0';
  data_ptr->outbuffer[0] = '\0';
  data_ptr->expand_dirname[0] = '\0';

/* create the bft instance */

  if ((code = cretinst("BFT", bftmajcap, MAXARGSTRING, MAXARGSTRING, data_ptr, &mcb_ptr)) != 0)
  {
    free (data_ptr);
    return (bfterror (code, "Loading BFT.", NULL));
  }

/* check to see if BFT is on the remote system, if not then load it */

  majnumr = 0;
  if (findnumb ("bft_main_", WSREMOTE, &majnumr) != 0)
  {
    if ((code = execom ("bft_main_", WSREMOTE, &com_id, mcb_ptr)) != 0)
    {
      destinst (&mcb_ptr);
      free (data_ptr);
      bfterror (code, "Loading remote bft_main_.", NULL);
      return;
    }
  }

/* Make BFT resident */

  stayres (mcb_ptr);
}
/*  */
/********************************************************************

     BFTMAJCAP

     PARAMETERS: MINUM      - Minor capability number to execute (input)
                 SENDER     - Major capability number of the sender (input)
                 ARG_STRING - The arguments passed to the minor (input)
                 ARG_LEN    - The length of the argument string (input)
                 DATA_PTR   - Pointer to the BFT data block (input)
                 MCB_PTR    - A pointer to the mowse control block (input)

     FUNCTION:   Executes the specified bft minor capability each of
                 which is described below.

**********************************************************************/

bftmajcap (minum, sender, arg_string, arg_len, data_ptr, mcb_ptr)

int minum;                            /* minor capability number to call */
int sender;                           /* major cap of the caller */
int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
bft_data_ptr *data_ptr;               /* pointer to the BFT data block */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
{
char temp_str[MAXARGSTRING];          /* temporary message */


/* switch on the minor capability */

  switch(minum)
  {
    case(BFT_SHUT_DOWN):
      bft_shut_down(mcb_ptr);
      break;

    case(CHECK_FILE_LENGTH):
      check_length(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

   case(EXPAND_PC_PATH):
      expand_pc_path(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

    case(INITIATE_FETCH):
      init_fetch (arg_string, arg_len, mcb_ptr, data_ptr);
      break;

    case(INITIATE_STORE):
      init_store (arg_string, arg_len, mcb_ptr, data_ptr);
      break;

    case(POSITION_FILE_POINTER):
      position_pointer (arg_string, arg_len, mcb_ptr, data_ptr);
      break;

    case(READ_ERROR):
      read_err (arg_string, arg_len, mcb_ptr, data_ptr);
      break;

    case(REC_DATA):
      rec_data(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

    case(REC_EOF):
      receive_eof(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

    case(SEND_DATA):
      send_data(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

    case(SYSTEM_ERROR):
      system_shutdown();
      break;

    case(WRITE_ERROR):
      write_err (arg_string, arg_len, mcb_ptr, data_ptr);
      break;

    case(MESSAGE_TOO_LONG):
      bfterror (BFT_INVALID_MINOR, "message_too_long", mcb_ptr);
      system_shutdown();
      break;

    case(EXECUTE_COMMAND_REPLY):
      execute_command_reply(arg_string,arg_len,mcb_ptr,data_ptr);
      break;

    case(FAIL_CAPABILITY):
      bfterror (BFT_INVALID_MINOR, "fail_capability", mcb_ptr);
      system_shutdown();
      break;

    case(GET_STATUS):
      bfterror (BFT_INVALID_MINOR, "get_status", mcb_ptr);
      system_shutdown();
      break;

    case(QUERY_REPLY):
      bfterror (BFT_INVALID_MINOR, "query_reply", mcb_ptr);
      system_shutdown();
      break;

    case(RESET_APPLICATION):
      bfterror (BFT_INVALID_MINOR, "reset_application", mcb_ptr);
      system_shutdown();
      break;

    case(RESUME_APPLICATION):
      bfterror (BFT_INVALID_MINOR, "resume_application", mcb_ptr);
      system_shutdown();
      break;

    case(RESPONSE_CONNECT):
      bfterror (BFT_INVALID_MINOR, "response_connect", mcb_ptr);
      system_shutdown();
      break;

    case(RESPONSE_DISCONNECT):
      disconnect_response(mcb_ptr);
      break;

    case(REQUEST_CONNECT):
      bfterror (BFT_INVALID_MINOR, "request_connect", mcb_ptr);
      system_shutdown();
      break;

    case(REQUEST_DISCONNECT):
      disconnect_request(mcb_ptr,sender);
      break;

    case(SUSPEND_APPLICATION):
      bfterror (BFT_INVALID_MINOR, "suspend_application", mcb_ptr);
      system_shutdown();
      break;

    case(TERMINATE_APPLICATION):
      putbgmes(mcb_ptr,0,"BFT","BFT terminating.");
      destinst(&mcb_ptr);
      break;

    case(WAKE_UP):
      bfterror (BFT_INVALID_MINOR, "wake_up", mcb_ptr);
      system_shutdown();
      break;

    default:
      sprintf (temp_str, "%d", minum);
      bfterror (BFT_INVALID_MINOR, temp_str, mcb_ptr);
      system_shutdown();
      break;
  }
}                                       
/*  */
/******************************************************************

BFT MINOR CAPABILITIES

********************************************************************/

/******************************************************************

    BFT_SHUT_DOWN

    PARAMETERS: MCB_PTR    - pointer to the mowse control block (input)

    INFO EXTRACTED FROM ARGLIST:  NONE

    FUNCTION:   Shuts bft down.

********************************************************************/

bft_shut_down(mcb_ptr)

mcb  *mcb_ptr;                      /* pointer to the mowse control block */
{
int   majnum;                       /* major capability number of remote */


/* Display the shutdown message */

  putbgmes (mcb_ptr, 0, "BFT", "BFT is shutting down.");

/* check to see if BFT is on the remote system */

  majnum = 0;
  if (findnumb("bft_main_",WSREMOTE,&majnum) != 0)
  {
    putbgmes (mcb_ptr, 0, "BFT", "BFT major capability not found. BFT aborted.");
    return(0);
  }

  disrqst (majnum, mcb_ptr);
}
/*  */
/******************************************************************

    CHECK_FILE_LENGTH

    PARAMETERS: ARG_STRING - string containing the argument (input)
                ARG_LEN    - length of the arg string (input)
                MCB_PTR    - pointer to the mowse control block (input)
                DATA_PTR   - pointer to BFT data structure (input)

    INFO EXTRACTED FROM ARGLIST:
                DESTINATION FILE NAME - filename on local system
                DESTINATION FLAGS     - transfer flags

    FUNCTION:   Finds out how much is in the destination file
                and calls position file pointer on the remote system.
                This is indicating that a recover store is in progress, so
                remember the stuff that was given to us.

********************************************************************/

check_length (p_arg_string, p_arg_len, p_mcb_ptr, p_data_ptr)

int  p_arg_len;                       /* length of the arg string */
char *p_arg_string;                   /* string containing the argument */
mcb  *p_mcb_ptr;                      /* pointer to the mowse control block */
bft_data_ptr p_data_ptr;              /* pointer to BFT data structure */
{
char  token[MAXARGSTRING];            /* destination file being checked */
char  *arg_pos;                       /* argument position */
int   fp_dest;                        /* incoming file pointer */
char  message[MAXARGSTRING];          /* Output message */
int   message_len;                    /* Length of message */
char  temp_str[MAXARGSTRING];         /* Temporary message holder */
int   buff_pos;                       /* Position in buffer */
int   buff_len;                       /* Length of buffer */
int   line_cnt;                       /* Lines in file */
int   open_mode;                      /* Modes for opening */


/* extract the source and destination file */

  arg_pos = p_arg_string;
  arg_pos = striptok (arg_pos, p_data_ptr->destination_file);
  arg_pos = striptok (arg_pos, token);
  char2long (token, &p_data_ptr->destination_flags);

/* if the destination file is not there return an error */

  open_mode = O_RDONLY;
  if (p_data_ptr->destination_flags & BFT_BINARY)
    open_mode |= O_RAW;

  if ((fp_dest = open (p_data_ptr->destination_file, open_mode)) == -1)
  {
    sprintf (temp_str, "%s opening %s.", getsyserr (errno), p_data_ptr->destination_file);
    message[0] = 0;
    message_len = 0;
    message_len = addtoken (message, message_len, temp_str, 0);
    executeb (WRITE_ERROR, message, message_len, p_mcb_ptr);
    return;
  }

/* Calculate the length of the file:  chars for binary, lines for ascii */

  if (p_data_ptr->destination_flags & BFT_BINARY)
    sprintf (temp_str, "%ld", lseek (fp_dest, 0L, SEEK_EOF));
  else
  {
    p_data_ptr->outpos = 0;
    if ((line_cnt = count_lines (fp_dest, p_data_ptr->outbuffer, &p_data_ptr->outpos, &buff_len, &buff_pos)) == -1)
    {
      sprintf (temp_str, "%s line counting %s.", getsyserr (errno), p_data_ptr->destination_file);
      message[0] = 0;
      message_len = 0;
      message_len = addtoken (message, message_len, temp_str, 0);
      executeb (WRITE_ERROR, message, message_len, p_mcb_ptr);
      close (fp_dest);
      return;
    }
    sprintf (temp_str, "%d", line_cnt);

/* Subtract the half line from the positioning count */

    p_data_ptr->outpos -= (buff_len - buff_pos);
  }
  close(fp_dest);

/* Because we are positioning, we are setting up a new transfer, so store it */

  p_data_ptr->outbuffpos = 0;

/* call position file pointer on the remote system */

  message_len = 0;
  message[0] = 0;
  message_len = addtoken (message, message_len, temp_str, 0);

  executeb (POSITION_FILE_POINTER, message, message_len, p_mcb_ptr);
}
/*  */
/*******************************************************************

     COUNT_LINES

     PARAMETERS: FP       - file to examine                (INPUT)
                 BUFF     - buffer to use                  (INPUT)
                 FILE_POS - file position of last eol+1    (OUTPUT)
                          - maximum eols to pass           (INPUT)
                 BUFF_LEN - length of remaining buffer     (OUTPUT)
                 BUFF_POS - buffer position of last eol+1  (OUTPUT)

     FUNCTION:   Count the number of lines in the file, already opened.
                 If the initial FILE_POS is 0, then count to the last EOL.
                 Ultimately, the actual file_pointer (FP) is set to the next
                 character after the last block read into buff.

********************************************************************/

count_lines (fp, buff, file_pos, buff_len, buff_pos)

int  fp;                     /* File pointer */
char *buff;                  /* Buffer to use fo counting */
long *file_pos;              /* File position */
int  *buff_len;              /* Buffer length */
int  *buff_pos;              /* Position in buffer */
{
long line_cnt;               /* Number of lines */
int  last_eol;               /* Position in buffer of last EOL */
int  status;                 /* Read status */
long max_count;              /* How far to count */
int  i;


/* Read chunks of the file counting LFs until either MAX or EOF */

  max_count = (*file_pos == 0) ? MAXLONG : *file_pos;

  line_cnt = 0;
  while (line_cnt < max_count)
  { 
    if ((status = read (fp, buff, DISKBUFF)) <= 0)
    { 
      if (*file_pos != 0)                             /* Error encountered */
        return (-1);
      else                                            /* Counted all thats there */
      { 
        *file_pos = lseek (fp, 0L, SEEK_CURRENT);
        return ((int)line_cnt);
      }
    }

/* Count of LFs */

    for (i = 0, last_eol = 0; (i < status) && (line_cnt < max_count); i++)
    { 
      if (buff[i] == LF)
      { 
        line_cnt += 1;
        last_eol = i+1;
      }
    }

/* Update the return values */
 
    *buff_len = status;
    *buff_pos = last_eol;
  }

/* Return the line count */

  *file_pos = lseek (fp, 0L, SEEK_CURRENT);
  return ((int)line_cnt);
}
/*  */
/*******************************************************************

     EXPAND_PC_PATH

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 LOCATOR    - position in starname matching (FIRST || NEXT)
                 PATH       - path to be expanded
                 REQUEST_ID - major ID of request
                 DIRECTION  - direction of request

     FUNCTION:   Expand the PC pathname to an absolute pathname and
                 return the results to FULL_PC_PATH on MU:BFT.

********************************************************************/

expand_pc_path (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char path[PATHNAMESIZE];              /* Path to expand */
char *arg_pos;                        /* Argument positioning */
char message[MAXARGSTRING];           /* Return message */
int  message_len;                     /* Return message length */
char request_id[2];                   /* Request ID of expansion */
char entryname[ENTRYNAMESIZE];        /* Entryname porition of request */
char direction[2];                    /* Direction of transfer */
char locator[2];                      /* Starname mathc position */
int  code;


/* Strip the relative path, major, minor IDs from the list */

   arg_pos = arg_string;
   arg_pos = striptok (arg_pos, locator);
   arg_pos = striptok (arg_pos, path);
   arg_pos = striptok (arg_pos, request_id);
   arg_pos = striptok (arg_pos, direction);

/* Expand the path, if one given, otherwise get next match */

   set_dta (&dta);

   if (locator[0] == BFT_FIRST)
   {
      getpath (path);
      expand_path (path, data_ptr->expand_dirname, entryname);
      sprintf (path, "%s\\%s", data_ptr->expand_dirname, entryname);
      code = 1;

      if (stpbrk (path, "*?") != NULL)
      {
         code = findfirst (path);
         sprintf (path, "%s\\%s", data_ptr->expand_dirname, dta.name);
      }
   }
   else
   {
      code = findnext ();
      sprintf (path, "%s\\%s", data_ptr->expand_dirname, dta.name);
   }

   if (code == 0)
      path[0] = '\0';

/* Send the absolute path to MU:FULL_PC_PATH */

   message[0] = 0;
   message_len = 0;
   message_len = addtoken (message, message_len, path, strlen (path));
   message_len = addtoken (message, message_len, request_id, 0);
   message_len = addtoken (message, message_len, direction, 1);
   executeb (FULL_PC_PATH, message, message_len, mcb_ptr);

   return;
}
/*  */
/********************************************************************

     INITIATE_FETCH

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 SOURCE FILE NAME - filename on local system
                 SOURCE FLAGS     - transfer modes

     FUNCTION:   Multics is beginning a PC -> Multics transfer

********************************************************************/

init_fetch (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char  *arg_pos;                       /* argument position */
char  token         [MAXARGSTRING];   /* Argument token */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
char  temp_str      [MAXARGSTRING];   /* arguments to remote routines */
int   temp_len;                       /* length of temp_str */
int   fp_source;                      /* outgoing file pointer */


/* extract the source file */

  arg_pos = arg_string;
  arg_pos = striptok (arg_pos, data_ptr->source_file);
  arg_pos = striptok (arg_pos, token);
  char2long (token, &data_ptr->source_flags);

/* if the file is not there return an error */

  fp_source = open (data_ptr->source_file, O_RDONLY | O_RAW);
  if (fp_source == -1)
  { 
    sprintf (out_str, "%s opening %s.", getsyserr (errno), data_ptr->source_file);
    temp_str[0]=0;
    temp_len = 0;
    temp_len = addtoken (temp_str, temp_len, out_str, 0);
    data_ptr->source_file[0]=0;
    executeb (READ_ERROR, temp_str, temp_len, mcb_ptr);
    return;
  }

/* reset all the input buffers for file input */

  data_ptr->inbuffpos = 0;
  data_ptr->inpos = 0L;
  close(fp_source);

/* Call send data on the local system */

  out_str[0] = 0;
  out_len = 0;
  bftmajcap (SEND_DATA, 0, out_str, out_len, data_ptr, mcb_ptr);
  return;
}
/*  */
/********************************************************************

     INITIATE_STORE

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 DESTINATION FILENAME - the filename that the remote
                                        requests to transfer to.
                 FLAGS                - transfer modes

     FUNCTION:   Multics is beginning a transfer Multics -> PC transfer.

*******************************************************************/

init_store (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char  *arg_pos;                       /* Argumen position */
char  token         [MAXARGSTRING];   /* Stripped token */
char  temp_str      [STRINGSIZE];     /* temp string */
int   temp_len;                       /* length of temp string */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
int   fp_dest;                        /* outgoing file pointer */


/* get the destination filename and transfer flags out of the arglist */

  arg_pos = arg_string;
  arg_pos = striptok (arg_pos, data_ptr->destination_file);
  arg_pos = striptok (arg_pos, token);
  char2long (token, &data_ptr->destination_flags);

/* call remote write error if unable to open the file */

  fp_dest = open (data_ptr->destination_file, O_WRONLY | O_RAW | O_CREAT | O_TRUNC);
  if (fp_dest == -1)
  { 
    sprintf (out_str, "%s opening %s.", getsyserr (errno), data_ptr->destination_file);
    temp_str[0]=0;
    temp_len = 0;
    temp_len = addtoken (temp_str, temp_len, out_str, 0);

    executeb (WRITE_ERROR, temp_str, temp_len, mcb_ptr);
    return;
  }

/* make the file the current destination if all is ok */

  data_ptr->outbuffpos = 0;
  close (fp_dest);

/* call SEND_DATA */

  executeb (SEND_DATA, out_str, 0, mcb_ptr);
}
/*  */
/******************************************************************

     POSITION_FILE_POINTER

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 SOURCE FILE NAME - the source file to open
                 FILE SIZE        - position the pointer this many bytes into the file
                 SOURCE FLAGS     - transfer flags

     FUNCTION:   Opens the source file name to the position passed in
                 the file size and then calls send data locally.

********************************************************************/

position_pointer (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char  token[MAXARGSTRING];            /* Token from message */
char  *arg_pos;                       /* Arg list position */
char  temp_str      [STRINGSIZE];     /* temp string */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
int   fp_source;                      /* outgoing file pointer */
int   line_pos;                       /* Line count positioning */
int   i;
int   open_mode;


/* extract the path and and new position */

  arg_pos = arg_string;
  arg_pos = striptok (arg_pos, data_ptr->source_file);
  arg_pos = striptok (arg_pos, token);
  data_ptr->inpos = (long) atoi (token);
  arg_pos = striptok (arg_pos, token);
  char2long (token, &data_ptr->source_flags);

/* if the file is not there return an error */

  open_mode = O_RDONLY;
  if (data_ptr->source_flags & BFT_BINARY)
    open_mode |= O_RAW;
  fp_source = open (data_ptr->source_file, open_mode);
  if (fp_source == -1)
  {
    sprintf (temp_str, "%s opening %s.", getsyserr (errno), data_ptr->source_file);
    out_str[0]=0;
    out_len = 0;
    out_len = addtoken (out_str, out_len, temp_str, 0);
    executeb (READ_ERROR, out_str, out_len, mcb_ptr);
    data_ptr->source_file[0]=0;
    return;
  }

/* position input buffers to the appropriate position in the file */

  data_ptr->inbufflen = 0;
  data_ptr->inbuffpos = 0;

/* Set the input file pointer position to the appropriate line count for ascii mode */

  if (!(data_ptr->source_flags & BFT_BINARY))
  {
    if (count_lines (fp_source, data_ptr->inbuffer, &data_ptr->inpos, &data_ptr->inbufflen, &data_ptr->inbuffpos) == -1)
    {
      sprintf (temp_str, "%s positioning %s.", getsyserr (errno), data_ptr->source_file);
      out_str[0]=0;
      out_len = 0;
      out_len = addtoken (out_str, out_len, temp_str, 0);
      executeb (READ_ERROR, out_str, out_len, mcb_ptr);
      data_ptr->source_file[0]=0;
      close (fp_source);
      return;
    }
  }
  close(fp_source);

/* call send data on the local system */

  out_str[0] = 0;
  out_len = 0;
  bftmajcap (SEND_DATA, 0, out_str, out_len, data_ptr, mcb_ptr);
}
/*  */
/******************************************************************

     RECEIVE_DATA

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 PART OF FILE - x number of bytes sent from the remote.

     FUNCTION:   Writes out x number of bytes to the destination file.
                 if any error has occured then remote write error is called
                 otherwise send data is called on the remote.

********************************************************************/

rec_data (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
int   i;                              /* temporary vars */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
char  temp_str      [MAXARGSTRING];   /* arguments to remote routines */
int   temp_len;                       /* length of temp_str */


/* write out the info in the arg_string to the file buffer */

  i = put_data (arg_string, arg_len, data_ptr);

/* if there were any problems then call remote write error */

  if (i != arg_len)
  {
    sprintf (out_str, "%s writing %s.", getsyserr (errno), data_ptr->destination_file);
    temp_str[0]=0;
    temp_len = 0;
    temp_len = addtoken (temp_str, temp_len, out_str, 0);
    executeb (WRITE_ERROR, temp_str, temp_len, mcb_ptr);
    data_ptr->destination_file[0]=0;
    return;
  }

/* otherwise call send data on the remote */

  out_str[0]=0;
  out_len = 0;
  executeb (SEND_DATA, out_str, out_len, mcb_ptr);
  return;
}
/*  */
/******************************************************************

     RECEIVE_EOF

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST: - none

     FUNCTION:   Writes out any remaining bytes in the output buffer
                 to the file, the file is closed and and the queue emptied

********************************************************************/

receive_eof (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
int   fp_dest;                        /* incoming file pointer */
int   open_mode;                      /* Opening modes */
int   status;                         /* error codes from reads and writes */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
char  temp_str      [MAXARGSTRING];   /* arguments to remote routines */
int   temp_len;                       /* length of temp_str */


/* open the file for output */

  open_mode = O_RDWR;
  if (data_ptr->destination_flags & BFT_BINARY)
    open_mode |= O_RAW;
  fp_dest = open (data_ptr->destination_file, open_mode);

/* if output file could not be opened call remote write error */

  if (fp_dest == -1)
  {
    sprintf (temp_str, "%s opening %s.", getsyserr (errno), data_ptr->destination_file);
    out_str[0]=0;
    out_len = 0;
    out_len = addtoken (out_str, out_len, temp_str, 0);
    executeb (WRITE_ERROR, out_str, out_len, mcb_ptr);
    return;
  }

/* write out the buffer to the end of the file */

  lseek (fp_dest, 0L, SEEK_EOF);
  status = write (fp_dest, data_ptr->outbuffer, data_ptr->outbuffpos);

/* if any problems writing to the file call remote write error */

  if (status < 0)
  {
    close(fp_dest);
    sprintf (out_str, "%s writing %s", getsyserr (errno), data_ptr->destination_file);
    temp_str[0]=0;
    temp_len = 0;
    temp_len = addtoken (temp_str, temp_len, out_str, 0);
    executeb (WRITE_ERROR, temp_str, temp_len, mcb_ptr);
    return;
  }
  else if (data_ptr->destination_flags & BFT_NOTIFY)
    putbgmes (mcb_ptr, 0, "BFT", "Completed transfer of %s.", data_ptr->destination_file);

/* reset the buffer and close the file */

  data_ptr->outbuffpos = 0;
  close (fp_dest);

/* clean up the destination and check for any remote to local transfers */

  data_ptr->destination_file[0] = 0;

  return;
}
/*  */
/******************************************************************

     READ_ERROR

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 ERROR STRING - an error message describing the problem

     FUNCTION:   A reading error has occurred on Multics.  Remove current
                 store source from the data_block and initiate a store
                 through the remote.

********************************************************************/

read_err (arg_string, arg_len, mcb_ptr, data_ptr)

char *arg_string;                     /* string containing the argument */
int arg_len;                          /* length of the arg string */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char message[MAXARGSTRING];           /* Return message */
int  message_len;                     /* Return message length */


/* Display the error */

  message_len = (arg_len > WSPAKSIZ) ? WSPAKSIZ : arg_len;
  strncpy (message, arg_string, message_len);
  message[message_len] = 0;
  putbgmes (mcb_ptr, 0, "BFT", message);

/* Close the destination file */

  data_ptr->destination_file[0] = 0;

/* Initiate another store */

  message[0] = 0;
  message_len = 0;
  executeb (INITIATE_STORE, message, message_len, mcb_ptr);
}
/*  */
/******************************************************************

     WRITE_ERROR

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST:
                 ERROR STRING -  error message describing the problem

     FUNCTION:   clears flags and queues after error, calls putbgmes
                 with the error message and calls check store flags
                 to see if any other transfers can be initiated.

********************************************************************/

write_err (arg_string, arg_len, mcb_ptr, data_ptr)

char *arg_string;                     /* string containing the argument */
int arg_len;                          /* length of the arg string */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
char  message[MAXARGSTRING];          /* Return message */
int   message_len;                    /* Return message length */


/* Display the error */

  message_len = (arg_len > WSPAKSIZ) ? WSPAKSIZ : arg_len;
  strncpy (message, arg_string, message_len);
  message[message_len] = 0;
  putbgmes(mcb_ptr, 0, "BFT", message);

/* Close the source file */

  data_ptr->source_file[0] = 0;

/* Initiate another fetch */

  message[0] = 0;
  message_len = 0;
  executeb (INITIATE_FETCH, message, message_len, mcb_ptr);
}
/*  */
/*******************************************************************

     SEND_DATA

     PARAMETERS: ARG_STRING - string containing the argument (input)
                 ARG_LEN    - length of the arg string (input)
                 MCB_PTR    - pointer to the mowse control block (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     INFO EXTRACTED FROM ARGLIST: - none.

     FUNCTION:   Read in x number of bytes from the source file.
                 if any error has occured then remote read error is called
                 otherwise receive_data is called on the remote.

********************************************************************/

send_data (arg_string, arg_len, mcb_ptr, data_ptr)

int arg_len;                          /* length of the arg string */
char *arg_string;                     /* string containing the argument */
mcb *mcb_ptr;                         /* pointer to the mowse control block */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
int   i;                              /* temporary vars */
char  out_str       [MAXARGSTRING];   /* arguments to remote routines */
int   out_len;                        /* length of out_str */
char  temp_str      [MAXARGSTRING];   /* temp messages */


/* read in some of the file from the input buffer */

  i = get_data(out_str,BUFFSIZE,data_ptr);
  out_str[i]=0;
  out_len = 0;

/* if there are more characters then call receive data */

  if (i>0)
  {
    out_len = i;
    executeb(REC_DATA,out_str,out_len,mcb_ptr);
    return;
  }

/* if there are no more characters then call receive eof */

  else if (i==0)
    executeb(REC_EOF,out_str,out_len,mcb_ptr);

/* if there was an error reading from the buffer call remote read error */

  else 
  {
    sprintf (temp_str, "%s reading %s.", getsyserr (errno), data_ptr->source_file);
    out_str[0]=0;
    out_len = 0;
    out_len = addtoken (out_str, out_len, temp_str, 0);
    executeb (READ_ERROR, out_str, out_len, mcb_ptr);
  }

  data_ptr->source_file[0]=0;
}
/*  */
/******************************************************************

     REQUEST DISCONNECT

     PARAMETERS: DATA_PTR   - pointer to BFT data structure (input)
                 SENDER     - major capability number of the calling routine (input)

     INFO EXTRACTED FROM ARGLIST: - none.

     FUNCTION:   Shuts BFT down.

********************************************************************/

disconnect_request (mcb_ptr,sender)

mcb  *mcb_ptr;                               /* BFT's MOWSE Control Block */
int  sender;                                 /* Who sent this message */
{
  disresp (WSACCEPT, sender, mcb_ptr);
  destinst (&mcb_ptr);
}
/*  */
/******************************************************************

     RESPONSE DISCONNECT

     PARAMETERS: MCB_PTR    - pointer to the mowse control block (input)

     INFO EXTRACTED FROM ARGLIST: - none.

     FUNCTION:   Shuts BFT down.

********************************************************************/

disconnect_response (mcb_ptr)

mcb  *mcb_ptr;                                /* MOWSE Control block */
{
  destinst(&mcb_ptr);
}
/*  */
/******************************************************************

     EXECUTE COMMAND REPLY

     PARAMETERS: NONE.

     INFO EXTRACTED FROM ARGLIST: NONE

     FUNCTION:   Handler for the execute command reply. Currently
                 ignored.

********************************************************************/

execute_command_reply()
{
  return;
}
/*  */
/*********************************************************************

     INTERNAL FUNCTIONS

**********************************************************************/
/*  */
/*********************************************************************

     GET_DATA

     PARAMETERS: BUFFER     - Pointer to a character string (input)
                 LENGTH     - The number of bytes to try to read in (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     FUNCTION:   Tries to read in "length" bytes from the file into
                 buffer and returns the actual number of bytes read

     WARNINGS:   If a file is opened as ASCII, then the ^Z character
                 indicates to read an EOF.  If there are more characters
                 after the ^Z, then they will NOT be transferred.

**********************************************************************/

int get_data (buffer, length, data_ptr)

int length;                           /* the number of bytes to try to read in */
char *buffer;                         /* pointer to a character string */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
int fp_source;                        /* file descriptor to source file */
int status;                           /* error code returned from reads */
int i;                                /* temporary variable*/
int open_mode;                        /* Opening mode of file */
long file_pos;


/* if the buffer is empty then read a chunk */

   if (data_ptr->inbuffpos == 0)
   {
     open_mode = O_RDONLY;
     if (data_ptr->source_flags & BFT_BINARY)
       open_mode |= O_RAW;
     fp_source = open (data_ptr->source_file, open_mode);

/* if the source file cant be opened return an error */

     if (fp_source == -1)
       return (-1);

/* seek to the correct spot in the file */

    file_pos = lseek (fp_source, data_ptr->inpos, SEEK_START);
    if (file_pos == -1L)
    {
      close (fp_source);
      return (-1);
    }

/* if the read fails give an error message */

    status = read (fp_source, data_ptr->inbuffer, DISKBUFF);
    if (status < 1)
    {
      close (fp_source);
      return (status);
    }

/* set pointers to correct places and close files */

    data_ptr->inpos = lseek (fp_source, 0L, SEEK_CURRENT);
    data_ptr->inbufflen = status;
    close (fp_source);
  }

/* determine actual length of the buffer that will be returned */

  if ((data_ptr->inbuffpos + length) > data_ptr->inbufflen)
    length = data_ptr->inbufflen - data_ptr->inbuffpos;

/* copy portion of the file from the input buffer to the arg */

  for (i=0;i<length;i++)
    buffer[i] = data_ptr->inbuffer[i+data_ptr->inbuffpos];

/* set input buffer pointer to point to next portion of the file */

  if ((data_ptr->inbuffpos + length) == data_ptr->inbufflen)
    data_ptr->inbuffpos = 0;
  else
    data_ptr->inbuffpos = data_ptr->inbuffpos + length;

  return (length);
}
/*  */
/********************************************************************

     PUT_DATA

     PARAMETERS: BUFFER - Pointer to a character string (input)
                 LENGTH - The number of bytes to try to write out (input)
                 DATA_PTR   - pointer to BFT data structure (input)

     FUNCTION:   Tries to write out length bytes from the buffer into
                 the file and returns the actual number of bytes
                 written.

**********************************************************************/

put_data (buffer, length, data_ptr)

int length;                           /* the number of bytes to try to write out */
char *buffer;                         /* pointer to a character string */
bft_data_ptr data_ptr;                /* pointer to BFT data structure */
{
int fp_dest;                          /* file descriptor to source file */
int status;                           /* error code returned from reads */
int i;                                /* temporary variable */
char temp_str[100];
int  open_mode;                       /* Opening modes */
long file_pos;


/* if the incoming data will overflow the buffer then write the buffer out */

  if ((data_ptr->outbuffpos+length ) > DISKBUFF)
  {
    open_mode = O_RDWR;
    if (data_ptr->destination_flags & BFT_BINARY)
      open_mode |= O_RAW;
    fp_dest = open (data_ptr->destination_file, open_mode);
    if (fp_dest == -1)
      return (-1);

/* seek to the write position in the file */

    if (data_ptr->outpos == 0)
      file_pos = lseek (fp_dest, 0L, SEEK_EOF);
    else
    { 
      file_pos = lseek (fp_dest, data_ptr->outpos, SEEK_START);
      data_ptr->outpos = 0;
    }
    if (file_pos == -1L)
    {
      close (fp_dest);
      return (-1);
    }

/* if the write was unsuccessful then return with an error code */

    status = write (fp_dest, data_ptr->outbuffer, data_ptr->outbuffpos);
    if (status < 1)
    {
      close (fp_dest);
      return (status);
    }

    data_ptr->outbuffpos = 0;
    close (fp_dest);
  }


/* copy data passed to put_data into the output buffer */

  for (i=0;i<length;i++)
    data_ptr->outbuffer[i+data_ptr->outbuffpos] = buffer[i];

  data_ptr->outbuffpos += length;

  return (length);
}
/*  */
/********************************************************************

     EXECUTEB

     PARAMETERS: MINUM      - Minor capability number to execute (input)
                 ARG_STRING - The arguments passed to the minor (input)
                 ARG_LEN    - The length of the argument string (input)
                 MCB_PTR    - A pointer to the mowse control block (input)

     FUNCTION:   Executes the specified minor capability on the
                 specified system with the specified arguments.

**********************************************************************/

executeb (minum, arg_string, arg_len, mcb_ptr)

int minum;                   /* minor capability number to call */
int arg_len;                 /* length of the arg string */
mcb *mcb_ptr;                /* pointer to the mowse control block */
char *arg_string;            /* string containing the arguments */
{
int majnum;
int code;


  majnum = 0;
  if ((code = findnumb ("bft_main_", WSREMOTE, &majnum)) != 0)
  {
    putbgmes (mcb_ptr, code, "BFT", "Finding Multics BFT capability.");
    return(0);
  }
  else
     return (execap (majnum, minum, arg_string, arg_len, mcb_ptr));
}
/*  */
/********************************************************************

     EXPAND_PATH

     PARAMETERS:
          PATH      - path to breakdown.   (input)
          DIRNAME   - directory component. (output)
          ENTRYNAME - entry component.     (output)

     FUNCTION:   Initiate a traversal of a directory for pattern matching
                 on filenames.

********************************************************************/

expand_path (path, dirname, entryname)

char *path;                            /* Path to expand */
char *dirname;                         /* Directory component */
char *entryname;                       /* Entryname component */
{
int  i;
int  indx;                             /* Length of directory component */
char token[MU_ENTRYNAME_SIZE];         /* Entryname component component */
char *sp;                              /* String pointer */


   for (i = 0, indx = 0; path[i] != '\0'; i++)
   {  
      if (path[i] == '\\')
         indx = i;
   }

/* Dirname */

   indx = (indx <= PATHNAMESIZE) ? indx : PATHNAMESIZE;
   strncpy (dirname, path, indx);

/* First component of entryname XXX.xxx */

   sp = &(path[indx+1]);
   sp = stptok (sp, token, sizeof (token), ".");
   strncpy (entryname, token, 8);

/* Another component? Then copy in the break, and skip the break char */

   if (*sp == '.')
   {
      sp += 1;
      strcat  (entryname, ".");
   }

/* Second component of entryname xxx.XXX */

   sp = stptok (sp, token, sizeof (token), ".");
   if (strlen (token) > 0)
      strncat (entryname, token, 3);
}
/*  */
/********************************************************************

     FINDFIRST

     PARAMETERS: STR - path spec to initiate traversal

     FUNCTION:   Initiate a traversal of a directory for pattern matching
                 on filenames.

********************************************************************/

findfirst(str)

short str;
{
union REGS sysregs;
int code;

   sysregs.h.ah = DOS_STAR_FIRST;
   sysregs.x.cx = 0;
   sysregs.x.dx = str;
   code = intdos (&sysregs,&sysregs);
   code &= 01;
   return (!code);
}
/*  */
/********************************************************************

     FINDNEXT

     FUNCTION:   Continue on traversal of file matching.

********************************************************************/

findnext()
{
union REGS sysregs;
int code;

    sysregs.h.ah = DOS_STAR_NEXT;
    code = intdos (&sysregs,&sysregs);
    code &= 01;
    return (!code);
}
/*  */
/********************************************************************

     SET_DTA

     ARGUMENTS: OFFSET - Address of dta structure.

     FUNCTION:  Set the DTA address to mine so I can reference it.

********************************************************************/

set_dta (offset)

short offset;                          /* DTA structure */
{
union REGS sysregs;

   sysregs.h.ah = DOS_SET_DTA;
   sysregs.x.dx = offset;
   intdos (&sysregs,&sysregs);
}
