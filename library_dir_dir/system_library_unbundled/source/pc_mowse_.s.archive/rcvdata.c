/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(85-12-22,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-01-30,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     New parameter format.
  3) change(86-05-26,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Suport for subchannels.
  4) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Extracted queue insertion sections and FG
     buffer manipulations.
  5) change(86-08-29,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Removed call to receive_byte.
  6) change(86-09-02,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Extracted section for wakeup.
  7) change(86-10-10,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Support for autoload.
  8) change(86-11-11,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Call to external_mowse before wsexecap.
  9) change(86-12-09,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Check on packetize_flag rather than packet_mode
     for autoload.
 10) change(86-12-11,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Guaruntee a reply is sent to the source of a
     REQUEST_DISCONNECT message if the destination does not exist.
 11) change(87-01-06,Flegel), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Added guaruntee to send a status reply in the
     event that the specified capability does not exist for a status request.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (rcvdata):

Get a message from the Protocol Handler . If the message is from the foreground
channel, then insert it into the foreground buffer (terminal_buf). If the
message is from the background channel, then call message_parser to handle it.
*/

#include <dos.h>
#include <stdio.h>
#include <ws.h>
#include <alloc.h>
#include <cat.h>
#include <ws_fgb.h>
#include <ws_dcls.h>
#include <ws_buf.h>
#include <ws_msg.h>
#include <wsmincap.h>
#include <ws_error.h>
#include <ws_auto.h>

extern struct fgbstr *lcbfptr;         /* First entry in application global buffer */
extern struct fgbstr *lcblptr;         /* ... last */
extern struct bstruc fg_buf;           /* Foreground intermediate buffer */
extern struct allocstr *lcaptr;        /* Application global allocation area */

extern int bg_in_use;                  /* Background active flag */
extern int packetize_flag;             /* MOWSE protocol active flag */
extern int mowse_terminating;          /* True when Multics MOWSE shutting down */
extern int load_list_pending;          /* Whether there are autoloads to perform */

extern char bg_sav_buf[];             /* Intermediate background message buffer */
extern char mysystem;                 /* System id */

extern local_cat *sleepq;             /* Sleeping application list */
extern local_cat l_CAT[];             /* Local cat */

/**/

rcvdata (p_dosflag)
int  p_dosflag;                /* =1, if entry through int28 (DOS)  */
{
int loopflag;                  /* Determinas when we are done processing */
int size;                      /* Size calculation space */
int ecode;                     /* Error code */
int i;                         /* Counter */
int length;                    /* Length calculation space */
int majorcap;                  /* Major capability number */
int inpl;                      /* length of input message */

struct input_msg *ip;          /* pointer to input message header structure  */

struct fgbstr *bufp;           /* Temporary buffer pointer */
struct fgbstr *fgbp1;          /* Capability buffer pointer */

local_cat *catp;               /* pointer to a CAT entry */

char *p1;                      /* Temporary pointers */
char *f1;

char  *wsalloc();
int   rcvfgdat();


/* : Clear out input buffer */

   receive_byte();

/* : while (more data available from protocol) */

   loopflag = 1;
   while (loopflag) {
      loopflag = 0;

/* : - if room in foreground save buffer (empty) (buffer begins at bminor) */

      if (fg_buf.bin == fg_buf.bout) {
         length = getdat(FG,&fg_buf.bminor);
         if (length > 0) {
            loopflag = 1;
            fg_buf.bout = fg_buf.bfirst;
            fg_buf.bin = fg_buf.bfirst + length - 1;
         }
      }

/* : - if room in background save buffer */

      if (!bg_in_use)
      {  bg_in_use = getdat(BG,&bg_sav_buf);
         if (bg_in_use > 0)
         {  loopflag = 1;
            bufp = (struct fgbstr *)wsalloc (lcaptr,bg_in_use + 1 + sizeof(struct fgbstr));
            if (bufp != NULL)

/* : -- copy to local capability buffer */

            {  p1 = &bg_sav_buf[0];
               f1 = &bufp -> fgb_char[0];
               for (i = 0; i < bg_in_use; i++)
               {  *f1++ = *p1++;
               }
               *f1++ = '\0';             /* terminate all messages with NULL */
               bufp -> fgb_length = bg_in_use;
               bufp -> fgb_next = NULL;
               bg_in_use = 0;

/* : -- link to previous local capability buffers */

               rcvmsg (bufp, &lcbfptr, &lcblptr);
            }
         }
      }

/* : - if foreground data available */

      if (fg_buf.bin != fg_buf.bout)
         rcvfgdat (&fg_buf);

/* : - if mowse terminating on Multics, dtm_mowse */

      if (mowse_terminating && !p_dosflag)
      {  dtm_mowse ();
         mowse_terminating = 0;
      }

/* : - if sleep queue is not empty and dos was NOT interrupted
     -- call wakeup */

      if (sleepq != NULL && !p_dosflag)
         wakeup ();

/* : - if there are any autoloads pending then load them */

      if (!p_dosflag && load_list_pending && packetize_flag)
      {  load_list_pending = 0;
         autoload ();
      }

/* : - if background data available */

      if ((lcbfptr != 0) && (!p_dosflag) ) {
         bufp = lcbfptr;
         inpl = bufp -> fgb_length;
         ip = (struct input_msg *) &bufp -> fgb_char[0];

/* : -- if major_capability number = mowse major capability
     --- call internal to parse and deal with the message */

         if (ip -> major == WSMAJCAP) {
            internal_mowse(inpl,ip);
         }

/* : -- Else */

         else {
            majorcap = ((int) ip -> major) - MIN_CAPABILITY_NUMBER;

/* : --- if capability is not active (does not exist)
     ---- send error message back to sender(FAIL_CAPABILITY) */

            if ((majorcap < 0) || (majorcap > NUMBER_OF_CAT_ENTRIES))
               message_error (ip -> source_system, ip -> source_major, FAIL_CAPABILITY);
            else if (l_CAT[majorcap].mcb_ptr == NULL)
            {  if (ip -> minor == PARTIAL_MESSAGE)
                  message_error (ip -> source_major, ip -> msg_data[0], FAIL_CAPABILITY);
               else if (ip -> minor == REQUEST_DISCONNECT)
                  message_error (ip -> source_system, ip -> source_major, RESPONSE_DISCONNECT);
               else if (ip -> minor == GET_STATUS)
                  message_error (ip -> source_system, ip -> source_major, STATUS_REPLY);
               else
                  message_error (ip -> source_system, ip -> source_major, FAIL_CAPABILITY);
            }

/* : --- else pass message to the application */

            else {
               external_mowse (&l_CAT[majorcap],ip,inpl);
            }
         }
         loopflag = 1;
         lcbfptr = bufp -> fgb_next;
         if (lcbfptr == NULL)
            lcblptr = NULL;
         wsfree (lcaptr,bufp);
      }
   }
}

/**/

/* : PROCEDURE FUNCTION (message_error)

Send the appropriate error to the source.
*/

message_error (p_system, p_major, p_minor)

char p_system;                         /* Destination system of error */
char p_major;                          /* Destination major */
char p_minor;                          /* Minor to send */
{
struct input_msg exerep;               /* Message */
char   data;                           /* Message data */
struct putbg_struc status_msg;         /* Status reply message */

   exerep.system        = p_system;
   exerep.major         = p_major;
   exerep.minor         = p_minor;
   exerep.source_major  = WSMAJCAP;
   exerep.source_system = mysystem;

/* : If minor was RESPONSE_DISCONNECT, then it failed */

   if (p_minor == RESPONSE_DISCONNECT)
   {  data = WSREJECT;
      send_i_mess(&exerep.system, sizeof(exerep) - 1, &data, 1);
   }

/* : Else if it was GET_STATUS, send an error message back */

   else if (p_minor == STATUS_REPLY)
   {  status_msg.type = STATUS_REPLY;
      status_msg.sender_major = (mysystem << 8) | WSMAJCAP;
      strcpy (status_msg.bgmsg, "Capability does not exist.");
      status_msg.length = strlen (status_msg.bgmsg);

      i_putbgm (&status_msg);
   }

/* : Else pass on the minor capability provided */

   else
      send_i_mess(&exerep.system, sizeof(exerep) - 1,NULL,0);
}
