/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-01-01,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-01-20,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Documentation and added call to getc_buff and allocated space for unions.
  3) change(86-05-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Added call to init_cat, allocated cat tables.
  4) change(86-08-25,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Modified documentation.
  5) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Installed /M option.
  6) change(86-09-17,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Extracted option parsing into separate routine and installed start-up
     file support (/F).
  7) change(87-03-15,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Re-wrote argument processing, it was wrong before.
  8) change(87-03-24,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added /E and /N options to allow access to escaping special characters
     and specifying that the connection will be via network.
  9) change(88-01-26,Flegel), approve(88-02-29,MCR7853),
     audit(88-03-10,Nakaska), install(88-03-15,MR12.2-1036):
     Added /H option to hold DTR high when connecting.
 10) change(88-06-17,Lee), approve(88-07-18,MCR7936), audit(88-08-10,Flegel),
     install(88-08-23,MR12.2-1091):
     Added support for Mark and Space parity.
                                                   END HISTORY COMMENTS */

/* :  PROCEDURE FUNCTION (MOWSE):

MOWSE command which loads MOWSE into PC memory.
*/

/* : NOTES:

The following is a list of command line options:

         /B - baud
         /C - communications port
         /D - data bits
         /E - escape character
         /F - start_up file
         /G - debug packets
         /H - hold DTR
         /I - user interrupt
         /L - autoload capability
         /M - turn on communications' error messages
         /N - Network connection
         /P - parity
         /S - stop bits
*/

#include <dos.h>
#include <stdio.h>
#include <fcntl.h>
#include <ws.h>
#include <mowsdefs.h>
#include <alloc.h>
#include <cat.h>
#include <ws_fgb.h>
#include <ws_auto.h>
#include <emulator.h>

#define BADARG       -1                /* Invalid parameter */
#define BADOPTION    -2                /* Invalid option */
#define OVERLOAD     -3                /* Too many autoload capabilities */
#define EXPECTING    -4                /* Argument expected */
#define BADCONTROL   -5                /* Option specifier ('/') expected */

#define OPTION_BUILT         001        /* Completed option */
#define PARAM_BUILT          002        /* Completed parameter */
#define OPTION_PENDING       004        /* Expecting option char */
#define PARAM_PENDING        010        /* Expecting one word of parameter */
#define READ_FILE            020        /* Start_up file specified */

#define DEFAULT_BAUD         B9600
#define DEFAULT_PARITY       EVEN_PAR
#define DEFAULT_STOP         STOP1
#define DEFAULT_DATAB        DATA7
#define DEFAULT_SOFTNO       97
#define DEFAULT_COM_PORT     0x3f8
#define DEFAULT_COM_NO       0
#define DEFAULT_MASK8259     0xef
#define DEFAULT_HARDINTRPT   0xc
#define DEFAULT_ERROR_MODE   0

#define WS_MEMORY_SIZE   2048 /* Size of MOWSE memory (4096 until MOWSE got too big) */

extern AUTO load_list[];      /* Autoload list of capabilities */
extern int load_list_pending; /* Autoload pending */
extern int _TOP;              /* TOP of stack as defined by C compiler */
extern int dbgpkts;           /* =1, to show level 2 packets */
extern int dbgrejs;           /* =1, to show rejected level 2 packets */
extern int dbgxchr;           /* =1, tp show extraneous received characters */
extern char s_escreq[256];    /* escape character flags */
extern char s_EOP;            /* Sender EOP character */
extern char r_EOP;            /* Receiver EOP character */
int        datab;             /* Data bit parameter */
int        _stack = 12288;    /* MOWSE needs a large runtime stack */
int        SOFTNO;            /* Software interrupt number for sub. library */
int        COM_PORT;          /* hardware address of comm port */
int        COM_NO;            /* number of comm port (COM1 or COM2)   */
char       MASK8259;          /* mask for setting enabling hardware interrupts */
int        HARDINTRPT;        /* hardware interrupt number */
int        packetize_flag;    /* Current packet mode */

local_cat       l_CAT [ NUMBER_OF_CAT_ENTRIES ]; /* Local CAT */
remote_cat      r_CAT [ NUMBER_OF_CAT_ENTRIES ]; /* Remote CAT */
int             NEW_INSTANCE_;

char            fg_memory[WS_MEMORY_SIZE];  /* Foreground linked list buffer */
struct allocstr fgastr,
                *fgaptr;

char            bg_memory[WS_MEMORY_SIZE];  /* Background linked list buffer */
struct allocstr bgastr,
                *bgaptr;

struct allocstr lcastr,
                *lcaptr;

struct fgbstr *lcbfptr;                /* first entry on local_cap queue */
struct fgbstr *lcblptr;                /* last message on local_cap queue */
struct fgbstr *fgbfptr;                /* first entry on foreground queue */
struct fgbstr *fgblptr;                /* last message on foreground queue */
struct fgbstr *bgbfptr;                /* first entry on background queue */
struct fgbstr *bgblptr;                /* last message on background queue */
struct fgbstr *sgbfptr;                /* first entry on status queue */
struct fgbstr *sgblptr;                /* last message on status queue */

int  bgcount;                          /* count of number of background messages */
int  error_mode;                       /* whether or not to print modem error messages */
char lc_memory[WS_MEMORY_SIZE];        /* memory for local cap  linked list buffer */

int  startup_flags = 0;                /* Flags indicating which options have been set */

main(argc,argv)
int argc;
char *argv[];
{
int          i;
int          baud;                     /* Communication line configuration */
int          parity;
int          stop;
int          comm_parameter;
int          flags;                   /* Flags for control information */
int          arg_count;               /* Number of parameters used in an option */
char         option;                  /* Command line option */
char         parameter[32];           /* Command line parameter */
char         start_file[32];
unsigned     highmem;                 /* Terminate and stay resident parameters */
union REGS   in_reg;                  /*    Register values of MOWSE */
struct SREGS segregs;                 /*    Segment register values of MOWSE */

/* : Initialzation */

   printf ("MOWSE version %d.%d ...", VERSION, SUBVERSION);

   fgaptr             = &fgastr;
   fgastr.memory_used = 0;
   fgastr.memory      = &fg_memory[0];
   fgastr.m_allocp    = NULL;
   fgastr.memory_size = WS_MEMORY_SIZE;
   fgbfptr            = NULL;
   fgblptr            = NULL;
   bgaptr             = &bgastr;
   bgastr.memory_used = 0;
   bgastr.memory      = &bg_memory[0];
   bgastr.m_allocp    = NULL;
   bgastr.memory_size = WS_MEMORY_SIZE;
   bgbfptr            = NULL;
   bgblptr            = NULL;
   bgcount            = 0;
   lcaptr             = &lcastr;
   lcastr.memory_used = 0;
   lcastr.memory      = &lc_memory[0];
   lcastr.m_allocp    = NULL;
   lcastr.memory_size = WS_MEMORY_SIZE;
   lcbfptr            = NULL;
   lcblptr            = NULL;
   sgbfptr            = NULL;
   sgblptr            = NULL;

/* : set default com port parameters. */

   baud               = DEFAULT_BAUD;
   parity             = DEFAULT_PARITY;
   stop               = DEFAULT_STOP;
   datab              = DEFAULT_DATAB;
   comm_parameter     = DEF_COMM;

   SOFTNO             = DEFAULT_SOFTNO;
   COM_PORT           = DEFAULT_COM_PORT;  /* hardware address of COM1 port */
   COM_NO             = DEFAULT_COM_NO;    /* set com1 port */
   MASK8259           = DEFAULT_MASK8259;  /* reset: IRQ4 */
   HARDINTRPT         = DEFAULT_HARDINTRPT;/* hardware (communications) interrupt  */
   error_mode         = DEFAULT_ERROR_MODE;

   flags              = 0;
   startup_flags      = 0;
   start_file[0]      = 0;

   load_list_pending  = 0;
   for (i = 0; i < AUTO_LIMIT; load_list[i++].flags = 0);

/* : Parse command line parameters */

   for (i = 1; i < argc;)
   {  if (argv[i][0] == '/')
      {  option = argv[i][1];
         if (argv[i][2])
            strcpy (parameter, &(argv[i][2]));
         else
            strcpy (parameter, argv[i+1]);

         arg_count = parse_options (option, parameter, &stop, &parity, &baud,
            &datab, start_file);
         if (arg_count < 0)
            exit (0);

         if (argv[i][2])
            i += 1;
         else
            i += (1 + arg_count);
      }
      else
      {  parse_error (BADCONTROL, argv[i][0], NULL);
         exit (0);
      }
   }

/* : If start_up file specified, parse through it */

   if (start_file[0])
      if (start_up_parser (start_file, &stop, &parity, &baud, &datab) < 0)
         exit (0);

/* : Initialize_mowse if MOWSE not already active
     - set communications port parameter
     - initialize the local and remote cat tables
     - start up MOWSE dumb terminal emulator
     - setup the memory location to be used to delimit code which
       is to remain resident
     - terminate and stay resident */

   /* set bits 8 to 10 for handling Mark and Space parity */
   if (parity == MARK_PAR || parity == SPAC_PAR) {
       comm_parameter = ((baud<<5)|(NO_PAR<<3)|(stop<<2)|(datab));
       comm_parameter |= parity << 8;
   }
   else
       comm_parameter = ((baud<<5)|(parity<<3)|(stop<<2)|(datab));

   if (initialize_mowse(comm_parameter) == 1)
   {  init_cat();
      printf ("\n");                                  /* We're done initializing */

#if EM_DEBUG
      em();
#endif
      printf ("\nResident portion of MOWSE installed on COM%d:\n", COM_NO + 1);

      segread(&segregs);
      highmem = _TOP + 16;

      in_reg.x.dx = (int) (highmem >> 4)+(segregs.ds-segregs.cs);
      in_reg.h.ah = 0x31;
      int86(33,&in_reg,0);
   }
   else
      printf (" already resident.\n");
}

/**/

/* : PROCEDURE FUNCTION (start_up_parser):

Parse through the start-up file specified.  The start-up file is interpretted
as follows:

        /X  parameter

where X is one of

        B,C,D,F,G,I,L,M,S

as defined for the parameter specifications for the command MOWSE.
*/

/* : RETURNS

      0 - all OK
    < 0 - an error occurred
*/

/* : NOTES

Capabilites that are to be autoloaded areshuffled off until MOWSE is active on
both Multics and the PC.

Command line options have precedence over start-up file specifications.  Thus
if for example /D is specified in the command line, then it will override any
mention of the option in the start up file.

If MOWSE is exitted normally BEFORE MOWSE is started up on Multics, all autoload
specifications will be lost.

/F options specified in the start_up file are treated as errors as recursive
start_up files are not acceptable.
*/

start_up_parser (p_file, p_stop, p_parity, p_baud, p_datab)

char p_file[];        /* NULL terminated string specifying startup file name */
int  *p_stop;         /* Stop bits */
int  *p_parity;       /* Parity */
int  *p_baud;         /* Baud rate */
int  *p_datab;        /* Data bits */
{
int  fd;              /* File Descriptor to start_up file */
int  i;
int  param_idx;       /* Position in parameter construction */
char buffer[257];     /* Input line from file */
char parameter[33];   /* Parmater being built */
int  n_chars;         /* Chars read from input file */
int  code;            /* Error code */
int  flags;           /* Current flag settings */
char option;          /* Option specified */
char junk[32];        /* Useless space */

/* : Open the file to read */
   if ((fd = open (p_file, O_RDONLY)) == -1)
   {  printf (" Error opening %s.\n", p_file);
      return (-1);
   }

/* : While not EOF
     - For each character extracted from the file
     -- If char = '/', done getting last parameter, process option, clear flags
     -- Else if char non-printable, done collection of parameter, process option
     -- Else (printable character), build option or parameter */

   flags = 0;
   while ((n_chars = read (fd, buffer, 256)) > 0)
   {  for (i = 0; i < n_chars; i++)
      {  if (buffer[i] == '/')
         {  if (flags & PARAM_PENDING)
            {  if (option == 'F' || option == 'f')
               {  printf (" Cannot load start_up files recursively.\n");
                  close (fd);
                  return (-1);
               }
               parameter[param_idx++] = 0;
               if ((code = parse_options (option, parameter, p_stop, p_parity, p_baud, p_datab, junk)) < 0)
               {  close (fd);
                  return (code);
               }
            }
            flags = OPTION_PENDING;
            option = 0;
            param_idx = 0;
            parameter[0] = 0;
         }

         else if (buffer[i] <= '\040' || buffer[i] >= '\177')
         {  if (flags & PARAM_PENDING && param_idx)
            {  flags = (flags ^ PARAM_PENDING) | PARAM_BUILT;
               if (option == 'F' || option == 'f')
               {  printf (" Cannot load start_up files recursively.\n");
                  close (fd);
                  return (-1);
               }
               parameter[param_idx++] = 0;
               if ((code = parse_options (option, parameter, p_stop, p_parity, p_baud, p_datab, junk)) < 0)
               {  close (fd);
                  return (code);
               }
            }
         }

         else
         {  if (flags & OPTION_PENDING)
            {  option = buffer[i];
               flags = (flags ^ OPTION_PENDING) | OPTION_BUILT | PARAM_PENDING;
            }
            else if (flags & PARAM_PENDING)
               parameter[param_idx++] = buffer[i];
         }
      }
   }

   close (fd);
   return (0);
}

/**/

/* : PROCEDURE FUNCTION (parse_options):

Parse the option and its parameter and handle accordingly.
*/

/* : RETURNS

     0         - OK
     BADARG    - Invalid parameter
     BADOPTION - Invalid option
     OVERLOAD  - Too many autoload capabilities
     EXPECTING - Argument expected
*/

/* : NOTES

If an error occurs, the error message is printed from parse_options and the
error code (above) is returned.
*/

parse_options (p_option, p_param, p_stop, p_parity, p_baud, p_datab, p_start)

char p_option;           /* Option */
char p_param[];          /* Parameter to option (may be 0 length) */
int  *p_stop;            /* Stop bits */
int  *p_parity;          /* Parity */
int  *p_baud;            /* Baud rate */
int  *p_datab;           /* Data bits */
char p_start[];          /* Start_up file name */
{
int  j;                  /* Temp */
int  i;
int  code;               /* Error code */
int  arg_count;          /* Number of arguments extracted */
int  index;              /* Escape character value */
int  convert_code;       /* Escape char conversion code */

   arg_count = 0;
   code = 0;

   switch (p_option)
   {  case 'B':                        /* BAUD */
      case 'b':

         arg_count = 1;
         if (startup_flags & OPTION_B)
            break;
         else if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (atoi(p_param))
         {  case 110:  *p_baud = B110;  break;
            case 150:  *p_baud = B150;  break;
            case 300:  *p_baud = B300;  break;
            case 600:  *p_baud = B600;  break;
            case 1200: *p_baud = B1200; break;
            case 2400: *p_baud = B2400; break;
            case 4800: *p_baud = B4800; break;
            case 9600: *p_baud = B9600; break;
            default:
               code = BADARG;
         }
         startup_flags |= OPTION_B;
         break;

      case 'C':                        /* COM PORT */
      case 'c':

         if (startup_flags & OPTION_C)
            break;

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (atoi(p_param))
         {  case 1:

               COM_PORT = DEFAULT_COM_PORT;    /* hardware address of COM1 port */
               COM_NO = DEFAULT_COM_NO;        /* set com1 port */
               MASK8259 = DEFAULT_MASK8259;    /* reset: IRQ4 */
               HARDINTRPT= DEFAULT_HARDINTRPT; /* hardware interrupt  */
               break;

            case 2:

               COM_PORT = 0x2f8;       /* hardware address of COM2 port */
               COM_NO = 1;             /* set com2 port */
               MASK8259 = (char) 0xf7; /* reset: IRQ3 */
               HARDINTRPT= 0xb;        /* hardware interrupt  */
               break;

            default:

               code =  BADARG;
         }
         startup_flags |= OPTION_C;
         break;

      case 'D':                        /* DATA BITS */
      case 'd':

         if (startup_flags & OPTION_D)
            break;

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (atoi(p_param))
         {  case 7:  *p_datab = DATA7; break;
            case 8:  *p_datab = DATA8; break;
            default: code = BADARG;
         }
         startup_flags |= OPTION_D;
         break;

      case 'E':                        /* Escape character */
      case 'e':

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else
         {  convert_code = sscanf (p_param, "%o", &index);
            if (convert_code != 1)
               code = BADARG;
            else if ((index < 0) || (index > 255))
               code = BADARG;
            else
               s_escreq[index] = 1;
         }

         break;

      case 'F':                        /* START_UP FILE */
      case 'f':

         if (!(p_param[0]) || (p_param[0] == '/'))
            strcpy (p_start, "MOWSE.INI");
         else
         {  stccpy (p_start, p_param, 31);
            p_start[31] = 0;
            arg_count = 1;
         }
         break;

      case 'G':                        /* DEBUG PACKETS */
      case 'g':

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (p_param[0])
         {  case 'P':                  /* Show all packets */
            case 'p':

               if (startup_flags & OPTION_GP)
                  break;
               startup_flags |= OPTION_GP;
               dbgpkts = 1; 
               break;

            case 'R':                  /* Show rejected packets */
            case 'r':

               if (startup_flags & OPTION_GR)
                  break;
               startup_flags |= OPTION_GR;
               dbgpkts = 1; 
               break;

            case 'X':                  /* Show extra characters */
            case 'x':

               if (startup_flags & OPTION_GX)
                  break;
               startup_flags |= OPTION_GX;
               dbgpkts = 1; 
               break;

            default:  
               code = BADARG;
         }
         break;

      case 'H':                        /* HOLD DTR */
      case 'h':
         startup_flags |= OPTION_H;
         break;

      case 'I':                        /* USER MOWSE INTERRUPT */
      case 'i':

         if (startup_flags & OPTION_I)
            break;

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else
         {  j = atoi(p_param);
            if ((j > 10) && (j < 256))
               SOFTNO = j;
            else
               code = BADARG;
         }
         startup_flags |= OPTION_I;
         break;

      case 'L':                        /* AUTOLOAD CAPABILITY */
      case 'l':

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;

         if (!code)
         {  for (i = 0; (i < AUTO_LIMIT) && (load_list[i].flags & AUTO_ON); i++);
            if (i == AUTO_LIMIT)
               code = OVERLOAD;
         }

         if (!code)
         {  strncpy (load_list[i].name, p_param, AUTO_LENGTH - 1);
            load_list_pending = AUTO_PENDING;
            load_list[i].name[AUTO_LENGTH] = 0;
            load_list[i].flags = AUTO_ON;
         }
         break;

      case 'M':                        /* MODEM ERROR MESSAGES */
      case 'm':

         error_mode = 1;
         break;

      case 'N':                        /* Network Connection */
      case 'n':

         arg_count = 0;
         s_EOP = CR;
         r_EOP = CR;
         s_escreq[CR] = 1;
         break;

      case 'P':                        /* PARITY */
      case 'p':

         if (startup_flags & OPTION_P)
            break;

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (p_param[0])
         {  case 'E':
            case 'e':

               *p_parity = EVEN_PAR;                /* even parity */
               break;

            case 'O':
            case 'o':

               *p_parity = ODD_PAR;                 /* odd parity */
               break;

            case 'M':
            case 'm':

               *p_parity = MARK_PAR;                /* mark parity */
               break;

            case 'S':
            case 's':
            
               *p_parity = SPAC_PAR;                /* space parity */
               break;

            case 'N': 
            case 'n':

               *p_parity = NO_PAR;                  /* no parity */
               *p_datab = DATA8; 
               break;

            default:  
               code = BADARG;
         }

         startup_flags |= OPTION_P;
         break;

      case 'S':                        /* STOP BITS */
      case 's':

         if (startup_flags & OPTION_S)
            break;

         arg_count = 1;
         if (!(p_param[0]) || (p_param[0] == '/'))
            code = EXPECTING;
         else switch (atoi(p_param))
         {  case 1:  *p_stop = STOP1; break;
            case 2:  *p_stop = STOP2; break;
            default: code = BADARG;
         }
         startup_flags |= OPTION_S;
         break;

      default:

         code =  BADOPTION;
   }

/* : If there was an error code, then it will cause an exit so return it */

   if (code) 
   {  parse_error (code, p_option, p_param);
      return (code);
   }

/* : Else it went OK and return the number of parameters used */

   return (arg_count);

}

/**/

/* : PROCEDURE FUNCTION (parse_error):

Display an appropriate error message for the code.
*/

parse_error (p_code, p_option, p_param)

int  p_code;
char p_option;
char p_param[];
{

   if (p_code == BADARG)
      printf (" Invalid parameter %s.\n", p_param);
   else if (p_code == BADOPTION)
      printf (" Invalid option /%c.\n", p_option);
   else if (p_code == OVERLOAD)
      printf (" Too many capabilities to autoload.\n");
   else if (p_code == EXPECTING)
      printf (" Argument expected.\n");
   else if (p_code == BADCONTROL)
      printf (" Invalid option delimiter: '%c'.\n", p_option);
}
