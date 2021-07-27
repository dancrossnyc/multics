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
  2) change(87-10-31,Flegel), approve(87-10-31,MCR7787),
     audit(88-01-26,DGHowe), install(88-02-24,MR12.2-1028):
     Added support for control_args to accomodate new queueing strategy.
  3) change(87-12-12,Flegel), approve(87-12-12,MCR7819),
     audit(88-01-26,DGHowe), install(88-02-24,MR12.2-1028):
     Added control arguments for a wider scope of request capabilities.
  4) change(89-02-15,Lee), approve(89-02-14,MCR8061), audit(89-02-27,Vu),
     install(89-04-24,MR12.3-1034):
     phx21250 (MOWSE 11) - fixed typo "requesst" in bft_fetch message.
                                                   END HISTORY COMMENTS */

/* COMPILER: Lattice C, V2.15 */

/***************************************************************

     BFT ARGUMENT ANAYLSER

     SYNTAX:     BFT KEY {path1 {path2...path1N path2N}} {/Control_args}

     PARAMETERS: argc - the number of arguments on the command
                        line. (input)
                 argv - a pointer to an array of pointers to
                        strings which contain the arguments
                        that follow the fetch keyword. (input)

     FUNCTION:   Parses the BFT command line for a valid keyword.
                 If one is found then the appropriate routine is
                 called to parse the remainder of the command
                 for filenames. If the command if syntactically
                 correct, then a call to the appropriate BFT
                 minor capability is made.

*****************************************************************/

#include <stdio.h>
#include <mowse.h>
#include <bft.h>

#define CANCEL_SW     0x001
#define DISPLAY_SW    0x002
#define FETCH_SW      0x004
#define LOAD_SW       0x008
#define RECOVER_SW    0x010
#define STORE_SW      0x020
#define UNLOAD_SW     0x040

/* ERROR MESSAGE */

#define NAME          "BFT"
#define USAGE_CANCEL  "\n   Usage: bft cancel request_id {request_id ...}\n"
#define USAGE_UNLOAD  "\n   Usage: bft unload\n"
#define USAGE_RECOVER "\n   Usage: bft unload\n"
#define USAGE_BFT     "\n   Usage: bft KEY {path1 {path2...path1N path2N}} {/Control_args}\n"
#define USAGE_FETCH   "\n   Usage: bft FETCH {path1 {path2...path1N path2N}} {/Control_args}\n"
#define USAGE_STORE   "\n   Usage: bft STORE {path1 {path2...path1N path2N}} {/Control_args}\n"
#define USAGE_KEYS    "\n     (s)tore, (f)etch, (c)ancel, (ls) list, (l)oad, (u)nload, (r)ecover\n"

/* GLOBAL REFERENCE */

int  argp;                            /* Current position in argument list */
int  argcount;                        /* Argument count */
char **argval;                        /* Arguments */
char *arg;                            /* Current argument */
long flags;                           /* Transfer modes */
int  priority;                        /* Transfer priority level */
int  control_sw;                      /* Argument parsing control */

char *find_option ();                 /* Locates a control_arg option */
char *stripquote();                   /* function to remove surrounding quotes */
char *upper();                        /* function to convert string to upper case */
int  bft_cancel ();
int  bft_fetch ();
int  bft_load ();
int  bft_display ();
int  bft_recover ();
int  bft_store ();
int  bft_unload ();

/**/

main (argc, argv)

int  argc;                            /* number of args on the command line */
char **argv;                          /* pointer to the args */
{
int  code;                            /* Error code */
int  (*key_procedure)();              /* Key function handler */
  
/* Initialize */

  code = 0;
  flags = 0;
  priority = 3;
  control_sw = 0;

  argcount = argc;
  argval = argv;
  flags = 0;

/* How many args are there ? */

  if (argcount < 1)
  {
    fprintf (stderr, "%s: Wrong number of arguments.\n%s", NAME, USAGE_BFT);
    return;
  }

/* Extract the keyword */

  argp = 0;
  if ((code = get_arg (0)) != 0)
  {
    bfterror (code, USAGE_BFT, NULL);
    return;
  }

/* This argument MUST be a keywrd, otherwise what are we to do ? */

  if      (!strcmp (argval[argp],"cancel")  || !strcmp (argval[argp],"c"))
  {
    control_sw |= CANCEL_SW;
    key_procedure = bft_cancel;
  }
  else if (!strcmp (argval[argp],"fetch")   || !strcmp (argval[argp],"f"))
  {
    control_sw |= FETCH_SW;
    key_procedure = bft_fetch;
  }
  else if (!strcmp (argval[argp],"load")    || !strcmp (argval[argp],"l") || !strcmp (argval[argp],"ld"))
  {
    control_sw |= LOAD_SW;
    key_procedure = bft_load;
  }
  else if (!strcmp (argval[argp],"list")    || !strcmp (argval[argp],"ls"))
  {
    control_sw |= DISPLAY_SW;
    key_procedure = bft_display;
  }
  else if (!strcmp (argval[argp],"recover") || !strcmp (argval[argp],"r"))
  {
    control_sw |= RECOVER_SW;
    key_procedure = bft_recover;
  }
  else if (!strcmp (argval[argp],"store")   || !strcmp (argval[argp],"s"))
  {
    control_sw |= STORE_SW;
    key_procedure = bft_store;
  }
  else if (!strcmp (argval[argp],"unload")  || !strcmp (argval[argp],"u") || !strcmp (argval[argp],"uld"))
  {
    control_sw |= UNLOAD_SW;
    key_procedure = bft_unload;
  }
  else
  {
    fprintf (stderr, "BFT: Keyword not accepted: %s.%s", arg, USAGE_KEYS);
    return;
  }

/* Parse through control arguments */

  if ((code = parse_args ()) != 0)
  {
    bfterror (code, USAGE_BFT, NULL);
    return;
  }

/* We made it here, so call the handler, skipt the first real argument as it is the keyword */

  argp = 0;
  get_arg (0);
  (*key_procedure) ();
}
/**/
/***************************************************************

     BFT_CANCEL

     SYNTAX:     bft cancel REQUEST_IDENTIFIER {REQUEST_IDENTIFIER ...}
                 bft c      REQUEST_IDENTIFIER {REQUEST_IDENTIFIER ...}

     PARAMETERS: None.

     FUNCTION:   Cancel an element from the request queue.

*****************************************************************/

bft_cancel ()
{
int  code;                             /* Error code */
char id[PATHNAMESIZE];                 /* Request id */
int  passed;                           /* Count of satisfied requests */
char request_type;                     /* Cancellation request type */


  passed = 0;

/* Repeat until all args are exhausted */

  while (argp < argcount - 1)
  {
    if (code = get_arg (1))
    {
      bfterror (code, arg, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for cancellation.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }

/* Verify and cancel the request */

    if (arg[0] != '/')
    {
      strncpy (id, arg, PATHNAMESIZE);
      code = bftcan (BFT_PATH_ID, stripquote (id));
    }
    else
    {
      request_type = arg[1];

      arg = find_option ();
      if (arg == NULL)
        code = BFT_EXPECTING;
      else
      {
        strncpy (id, arg, PATHNAMESIZE);

        if (request_type == 'I')
          code = bftcan (BFT_TIME_ID, stripquote (id));
        else if (request_type == 'E')
          code = bftcan (BFT_ENTRY_ID, stripquote (id));
      }
    }

/* Something went wrong */

    if (code != 0)
    {
      bfterror (code, "Executing bft cancel.", NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for cancellation.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }
    else
      passed += 1;
  }

  if (passed > 0)
    printf ("BFT: %d reques%s submitted for cancellation.\n", passed, ((passed == 1) ? "t" : "ts"));

  return (0);
}
/**/
/***************************************************************

     BFT_DISPLAY

     SYNTAX:     BFT LIST {/Control_args}
                 BFT LS   {/Control_args}

     PARAMETERS: None.

     FUNCTION:   Display the request queues.

*****************************************************************/

bft_display ()
{
  fprintf (stderr, "BFT: PC version of BFT does not implement queue list.  Use Multics list.\n");
}
/**/
/***************************************************************

     BFT_FETCH

     SYNTAX:     bft fetch file1 {file2...file1N file2N} {/Control_args}
                 bft f     file1 {file2...file1N file2N} {/Control_args}

     PARAMETERS: None.

     FUNCTION:   parses the BFT command line after the "fetch"
                 keyword for a source and destination filename.
                 If only one filename is given then it is taken
                 as both the source and destination filename.
                 If the syntax is correct then the bft fetch
                 minor capability is called.

*****************************************************************/

bft_fetch ()
{
char mu_path[MAXARGSTRING];           /* holds arguments to be passed to execap  */
char pc_path[PATHNAMESIZE];           /* source file name */
int  passed;                          /* Number of requests submitted */
int  code;

  passed = 0;

/* Repeat until all args are exhausted */

  while (argp < argcount - 1)
  {

/* Get Multics path */

    if (code = get_arg (0))
    {
      bfterror (code, USAGE_FETCH, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for fetching.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }

    strncpy (mu_path, arg, PATHNAMESIZE);
    strncpy (pc_path, "===", PATHNAMESIZE);

/* Get the Multics path, if it was specified */

    code = get_arg (0);
    if ((code != 0)&&(code != BFT_NOARG))
    {
      bfterror (code, arg, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for fetching.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }
    else if (code == 0)
      strcpy (pc_path, arg);

/* expand the destination filename to a pathname for the fetch */

    getpath (pc_path);

    code = bftfetch (stripquote (mu_path), pc_path, flags, priority);
    if (code != 0)
    {
      bfterror (code, pc_path, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for fetching.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }
    else
      passed += 1;
  }

  /* RL: phx21250 - fixed display message typo "requesst" for singular form */
  if (passed > 0)
    printf ("BFT: %d reques%s submitted for fetching.\n", passed, ((passed == 1) ? "t" : "ts"));

  return (0);
}
/**/
/***************************************************************

     BFT_LOAD

     SYNTAX:     bft load

     PARAMETERS: none

     FUNCTION:   Because of the difference in nature of loading
                 an application between Multics and the PC, this
                 merely tells the user that (s)he must use "bft_load"
                 command.

*****************************************************************/

bft_load ()
{

/* Inform the user of the bft_load command*/

  printf ("BFT: load is not implemented.  Use bft_load.\n");
}
/**/
/**************************************************************
 
     BFT_RECOVER

     SYNTAX:     bft recover
                 bft r

     PARAMETERS: None.

     FUNCTION:   Initiates the appropriate recover procedures
                 by emitting a message to the Multics bft to
                 begin recovery procedures.

*****************************************************************/

bft_recover ()
{
int code;

/* call the recover fetch entry point */

  if ((code = bftrecfe ()) != 0)
    return (bfterror (code, "Recovering fetch requests.", NULL));

/* call the recover store entry point */

  if ((code = bftrecst ()) != 0)
    return (bfterror (code, "Recovering store requests.", NULL));
}
/**/
/***************************************************************

     BFT_STORE

     SYNTAX:     bft store file1 {file2...file1N file2N} {/Control_args}
                 bft s     file1 {file2...file1N file2N} {/Control_args}

     PARAMETERS: None.

     FUNCTION:   Parses the BFT command line after the "store"
                 keyword for a source and destination filename
                 if only one filename is given then it is taken
                 as both the source and destination filename.
                 If the syntax is correct then the bft store
                 minor capability is called with the filenames.

*****************************************************************/

bft_store ()
{
char mu_path[MAXARGSTRING];           /* holds arguments to be passed to execap  */
char pc_path[PATHNAMESIZE];           /* source file name */
int  passed;                          /* Number of requests submitted */
int  code;


  passed = 0;

/* Repeat until there are no argument */

  while (argp < argcount - 1)
  {

/* Get PC path */

    if ((code = get_arg (0)) != 0)
    {
      bfterror (code, USAGE_STORE, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for storing.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }

    strncpy (pc_path, arg, PATHNAMESIZE);
    strncpy (mu_path, "===", PATHNAMESIZE);

/* Get the Multics path, if it was specified */

    code = get_arg (0);
    if ((code != 0)&&(code != BFT_NOARG))
    {
      bfterror (code, arg, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for storing.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }
    else if (code == 0)
      strcpy (mu_path, arg);

/* expand the destination filename to a pathname for the fetch */

    getpath (pc_path);

    code = bftstore (pc_path, stripquote (mu_path), flags, priority);
    if (code != 0)
    {
      bfterror (code, pc_path, NULL);
      if (passed > 0)
        printf ("BFT: %d reques%s submitted for storing.\n", passed, ((passed == 1) ? "t" : "ts"));
      return (code);
    }
    else
      passed += 1;
  }

  if (passed > 0)
    printf ("BFT: %d reques%s submitted for storing.\n", passed, ((passed == 1) ? "t" : "ts"));

  return (0);
}

/**/
/***************************************************************

     BFT_UNLOAD

     SYNTAX:     bft unload
                 bft u

     PARAMETERS: None.

     FUNCTION:   Unloads bft from MOWSE.

*****************************************************************/

bft_unload ()
{
int code;


/* unload the BFT major capability */

  if ((code = bftunld ()) != 0)
    return (bfterror (code, "Unloading.", NULL));
}
/**/
/***************************************************************

     FIND_OPTION

     PARAMETERS: None.

     FUNCTION:   Returns a pointer to the option string to an argument.
                 This is either the [2] character in the current arg,
                 or the next arg.

*****************************************************************/

char *find_option ()
{

  if (arg[2] != 0)
    return (&(arg[2]));

  argp += 1;
  arg = argval[argp];

  if (arg[0] == '/')
    return (NULL);
  else if (argp < argcount)
    return (arg);
  else
    return (NULL);
}
/**/
/***************************************************************

     GET_ARG

     PARAMETERS: SPECIAL - If the special control_args are to be
                           treated as a regular arg.

     FUNCTION:   Sets arg and argp to the next valid non-control
                 argument.  If "special" is set to True, then accept
                 "/C STR" control arguments as a non-control argument.

*****************************************************************/

get_arg (special)

int  special;                         /* Accept "/C STR" */
{
int  queue;                           /* Priority queue */
int  code;


/* Process all control args until a non-control arg is found */

  while (1)
  {
    argp += 1;

    if (argp >= argcount)
      return (BFT_NOARG);

/* Is this an argument, no then return 0 */

    arg = argval[argp];
    if (arg[0] != '/')
      return (0);

/* Else check for special cases */

    switch (toupper (arg[1]))
    {
      case 'E':                       /* These are special */
      case 'I':
        if (special)                  /* Caller wants one of these */
          return (0);
        else                          /* Skip the param */
          find_option ();
        break;

      case 'N':                       /* Skip param */
      case 'F':
      case 'Q':
        find_option ();
        break;

      default:                        /* No param */
        break;
    }
  }
}
/**/
/***************************************************************

     PARSE_ARGS

     PARAMETERS: None.

     FUNCTION:   Processes a control argument.

                 Acceptable control args:

                    /F ascii | binary   - file type {ascii}
                    /N on | off         - notify {off}
                    /Q N                - priority {3}

                 Control arguments skipped (as they are really "single"
                 arguments):

                    /E NAME             - Request entry
                    /I NAME             - Request ID

*****************************************************************/

parse_args ()

{
int  code;
int  queue;                           /* Transfer priority */

  code = 0;

  for (argp = 1; argp < argcount; argp++)
  {
    arg = argval[argp];

    if (arg[0] == '/')
    {
      switch (toupper (arg[1]))
      {
        case 'F':                                         /* File_type */
          if (!(control_sw & STORE_SW || control_sw & FETCH_SW))
            code = BFT_INCOMPATIBLE;
          else if ((arg = find_option ()) == NULL)
            code = BFT_EXPECTING;
          else if (!strcmp (upper (arg), "ASCII"))
            flags &= ~BFT_BINARY;
          else if (!strcmp (upper (arg), "BINARY"))
            flags |= BFT_BINARY;
          else
            code = BFT_BADOPT;
          break;

        case 'N':                                         /* Notify */
          if (!(control_sw & STORE_SW || control_sw & FETCH_SW))
            code = BFT_INCOMPATIBLE;
          else if ((arg = find_option ()) == NULL)
            code = BFT_EXPECTING;
          else if (!strcmp (upper (arg), "OFF"))
            flags &= ~BFT_NOTIFY;
          else if (!strcmp (upper (arg), "ON"))
            flags |= BFT_NOTIFY;
          else
            code = BFT_BADOPT;
          break;

        case 'Q':                                         /* Queue */
          if (!(control_sw & STORE_SW || control_sw & FETCH_SW))
            code = BFT_INCOMPATIBLE;
          else if ((arg = find_option ()) == NULL)
            code = BFT_EXPECTING;
          else if (strlen (arg) != 1)
            code = BFT_BADOPT;
          else if (!isdigit (arg[0]))
            code = BFT_BADOPT;
          else
          {
            queue = (int) (arg[0] - '0');
            if ((queue < BFT_MIN_PRIORITY) || (queue > BFT_MAX_PRIORITY))
              code = BFT_INVALID_PRIORITY;
            else
              priority = queue;
          }
          break;

        case 'I':                                         /* ID */
          if (!(control_sw & CANCEL_SW))
            code = BFT_INCOMPATIBLE;
          break;

        case 'E':                                         /* Entry */
          if (!(control_sw & CANCEL_SW))
            code = BFT_INCOMPATIBLE;
          break;

        default:                                          /* Error */
          code = BFT_BADARG;
          break;
      }

      if (code != 0)
        return (code);
    }
  }

  return (0);
}
/**/
/***************************************************************

     UPPER

     PARAMETERS: STRING - string to convert to upper.

     FUNCTION:   Convert a character string to upper case.

*****************************************************************/

char *upper (str)

char *str;
{
int  i;

  for (i = 0; str[i]; i++)
    str[i] = toupper (str[i]);

  return (str);
}
