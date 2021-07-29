/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-02-24,ASmith), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-06-05,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Support for execom structure.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (EXECUTE_COMMAND)

This Internal MOWSE software interrupt handler executes a command (included 
in a message) sent from some remote system.  Errors encountered in processing 
the command are sent back to a specified capability.
*/

/* : NOTES
*/

#include <stdio.h>
#include <ws.h>
#include <wsmincap.h>
#include <ws_error.h>
#include <ws_msg.h>

#define STATUS_SUCCESS 32
#define STATUS_FAILED 33

extern char mysystem;

EXECUTE_COMMAND(com_len,execmp)

int     com_len;                /* Length of message */
struct execom_msg *execmp;      /* Execute command message */
{
int error;
char *cmd;                      /* pointer to command string */
struct exerep_msg exerep;       /* reply message */
struct execom_msg *execmd;      /* pointer to execute command message */


/* : Try to execute command locally. */

   cmd = &execmp->command[0];
   com_len = com_len - sizeof(struct execom_msg) + 1;
   cmd[com_len] = '\0';         /* ensure command ends with zero byte */
   set_dta();                   /* use Mowse's DTA */
   set_trap();                  /* use MOWSE's trap rotuines */
   error = system(cmd);
   rst_trap();
   rst_dta();

/* : If execution failed then formulate message_1 to send to err_handler. */

   if (error)
      exerep.status = (char) STATUS_FAILED;
   else
      exerep.status = STATUS_SUCCESS;

/* : Send the reply to the source of the request */

   exerep.system = execmp->source_system;
   exerep.major = execmp->source_major;
   exerep.minor = EXECUTE_COMMAND_REPLY;
   exerep.source_major = WSMAJCAP;
   exerep.source_system = mysystem;
   exerep.cmd_id = execmp->cmd_id;

   send_i_mess(&exerep.system,sizeof(exerep),NULL,0);
}
/* : END */
