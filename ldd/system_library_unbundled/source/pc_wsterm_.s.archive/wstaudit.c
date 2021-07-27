/* ********************************************
   *                                          *
   * Copyright, (C) Honeywell Bull Inc., 1988 *
   *                                          *
   ******************************************** */

/* HISTORY COMMENTS:
  1) change(88-03-02,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Created routines for buffering.
  2) change(88-05-31,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Added support for file and line printer auditing.
  3) change(88-07-25,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Documentation additions only. Added header comments to all routines.
  4) change(88-08-09,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel),
     install(88-10-12,MR12.2-1160):
     Fix references to include files; "wstdefs.h" was split into
     "wstdefs.h" and "wsttype.h" to avoid declaration clash; also,
     various constants defined to descriptive names.
  5) change(89-01-18,Lee), approve(89-01-02,MCR8043), audit(89-03-14,Flegel),
     install(89-04-24,MR12.3-1033):
     Separated input and output parameters to int86() to avoid confusion
     when parameters are used.
                                                   END HISTORY COMMENTS */

#include <stdio.h>
#include <fcntl.h>
#include <dos.h>
#include <ctype.h>
#include "wstdefs.h"
#include "wstglob.h"


/* used globally but only by these routines */

static char wst_f_aud_buff[F_AUDIT_BUFF_SIZE];
static int  wst_f_aud_dex = 0;
static int  wst_f_aud_fd = -1;

static char wst_p_aud_buff[P_AUDIT_BUFF_SIZE];
static int wst_p_aud_dex;


/*
**********************************************************************

  Routine:            F_AUD_INIT 

  Function:
      This routine intializes any global file audit variables used. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

f_aud_init()
{
     /* file audit buffer index set to beginning */
     wst_f_aud_dex = 0;

     /* initialize file descriptor for audit file */
     wst_f_aud_fd = -1;

}



/*
**********************************************************************

  Routine:            F_AUD_OPEN 

  Function:
      This routine opens the audit file and sets up the file audit 
  buffer. 

  Parameters:         NONE 

  Returns:            -1 if unsuccessful in opening the audit file 
                      0 if no errors 

**********************************************************************/

f_aud_open()
{
    wst_f_aud_fd = open(audit_file_name,O_CREAT|O_WRONLY|O_APPEND);

    /* check for successful open or create */
    if (wst_f_aud_fd < 0)
        return(-1);

    /* initialize buffer index to beginning of buffer */
    wst_f_aud_dex = 0;

    return(0);
}



/*
**********************************************************************

  Routine:            F_AUD_CLOSE 

  Function:
      This routine flushes the contents of the audit buffer to file 
  and closes the audit file. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

f_aud_close()
{
    f_aud_flush();
    close(wst_f_aud_fd);
}



/*
**********************************************************************

  Routine:            BEGIN_FILE_AUDIT 

  Function:
      This routine is called to initiate a file audit session. If set 
  up and opening of the audit file and audit file buffer is 
  successful, an audit header containing the current date and time is 
  written to the audit buffer. If set up or opening of audit file is 
  unsuccessful, an error message is displayed and control passes back 
  to the call when a key is entered. 

  Parameters:         NONE 

  Returns:            -1 if unsuccessful 
                      0 if successful 

**********************************************************************/

begin_file_audit()
{
    char message[81];    /* local message buffer */

    /* if opening of audit file is unsuccessful */
    if (f_aud_open() < 0) {

        /* display an error message and wait for any key */
        sprintf(message,"Cannot open audit file: %s  <any key>-resume",
            audit_file_name);
        status_err_msg(message);

        return(-1);
    }

    /* open is successful, log an audit trace begin message containing */
    /*   the current date and time   */

    f_audit_msg("======= BEGIN WSTERM AUDIT TRACE: ",34);
    get_date_time_str(message);
    f_audit_msg(message,strlen(message));
    f_audit_msg(" =======\n",9);

    return(0);
}



/*
**********************************************************************

  Routine:            END_FILE_AUDIT 

  Function:
      This routine is called to terminate a file audit session. The 
  routine logs a terminating audit message containing the current 
  date and time, flushes the audit buffer and closes the audit file. 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

end_file_audit()
{
    char str[MAX_DATE_TIME_STR_LENGTH];     /* local message buffer */

    /* log audit trace end message containing current date and time */
    f_audit_msg("======= END WSTERM AUDIT TRACE: ",32);
    get_date_time_str(str);
    f_audit_msg(str,strlen(str));
    f_audit_msg(" =======\n",9);

    /* close the audit file */
    f_aud_close();
}



/*
**********************************************************************

  Routine:            F_AUDIT_MSG 

  Function:
      This routine writes data to the audit file buffer which gets 
  flushed when full or when file audit is terminated. 

  Parameters:
     (input)          buff - pointer to the buffer containing the 
                          audit data 
     (input)          bytes - specifies the number of bytes of audit 
                          data to write 

  Returns:            NONE 

**********************************************************************/

f_audit_msg(buff,bytes)
char *buff;   /* data buffer */
int  bytes;   /* number of data bytes */
{
    /* fill file buffer with data until no more data or file buffer full */
    while (bytes > 0 && wst_f_aud_dex < F_AUDIT_BUFF_SIZE) {
        wst_f_aud_buff[wst_f_aud_dex++] = *buff++;
        bytes--;
    }

    /* if no more data, done */
    if (wst_f_aud_dex < F_AUDIT_BUFF_SIZE)
        return;

    /* else buffer was full, flush it */
    write(wst_f_aud_fd,wst_f_aud_buff,F_AUDIT_BUFF_SIZE);

    /* reset the file buffer index to start of buffer */
    wst_f_aud_dex = 0;

    /* will rest of data fit into buffer? */
    if (bytes < F_AUDIT_BUFF_SIZE) {

        /* yes, fill the buffer with rest of data */
        while (bytes > 0) {
            wst_f_aud_buff[wst_f_aud_dex++] = *buff++;
            bytes--;
        }
    }
    /* no, quicker to just flush all data straight to file */
    else
        write(wst_f_aud_fd,buff,bytes);
}



/*
**********************************************************************

  Routine:            F_AUD_FLUSH 

  Function:
      This routine flushes the contents of the audit buffer to the 
  audit file. 

  Parameters:         NONE 

  Returns:            NONE 

  Note 1 - The caller must ensure that the audit file is opened 
      before calling this routine as no checks are made before 
      writing to file. 
**********************************************************************/

f_aud_flush()
{
    /* if data exists in file buffer */
    if (wst_f_aud_fd > 0) {

        /* write it out */
        write(wst_f_aud_fd,wst_f_aud_buff,wst_f_aud_dex);

        /* reset the buffer index to start of buffer */
        wst_f_aud_dex = 0;
    }
}



/*
**********************************************************************

  Routine:            GET_DATE_TIME_STR 

  Function:
      This routine formats a string containing the current date and 
  time and passes the string back to the caller. 

  Parameters:
     (output)         str - pointer to the string to which the 
                          formatted date and date is to be passed 
                          backed 

  Returns:            NONE 

  Note 1 - The date and time are obtained from DOS and are meaningful 
      only if the date and time are correctly set before WSTERM is 
      invoked. 
**********************************************************************/

get_date_time_str(str)
char *str;
{
    union REGS in, date, time;

    /* call DOS routine to get the date */
    in.h.ah = DOS_GET_DATE;
    intdos(&in,&date);

    /* call DOS routine to get the time */
    in.h.ah = DOS_GET_TIME;
    intdos(&in,&time);

    /* format the date and time into the user-specified string */
    sprintf(str,"%s %d, %04d - %02d:%02d.%02d",
        month_tab[date.h.dh-1],
        date.h.dl,
        date.x.cx,
        time.h.ch,
        time.h.cl,
        time.h.dh);
}



/*
**********************************************************************

  Routine:            BEGIN_PRINTER_AUDIT 

  Function:
      This routine initiates a printer audit session by checking that 
  the printer is ready and then printing a printer audit message 
  containing the current date and time. 

  Parameters:         NONE 

  Returns:            -1 if printer error occurred 
                      0 if otherwise successful 

  Note 1 - If an error occurs, an error message is displayed on the 
      status line and the routine returns when a key is pressed. 
**********************************************************************/

begin_printer_audit()
{
    char message[SCREEN_COLS+1];        /* local message buffer */
    char *err_msg;        /* pointer to error messages displayed */

    /* check and handle printer not ready */
    if (check_printer_status(&err_msg) < 0) {

        sprintf(message,"Printer error - %s <any key>-resume", err_msg);
        status_err_msg(message);

        return(-1);
    }

    /* printer is ready, display trace begin message containing date & time */
    p_audit_msg("======= BEGIN WSTERM AUDIT TRACE: ",34,NULL);
    get_date_time_str(message);
    p_audit_msg(message,strlen(message),NULL);
    p_audit_msg(" =======\n",9,NUL);

    return(0);
}



/*
**********************************************************************

  Routine:            END_PRINTER_AUDIT 

  Function:
      This routine terminates a printer audit session by printing a 
  terminating message containing the current date and time. 

  Parameters:         NONE 

  Returns:            0 always 

**********************************************************************/

end_printer_audit()
{
    char str[MAX_DATE_TIME_STR_LENGTH];    /* local message buffer */

    /* printer a trace end message containing the date and time */
    p_audit_msg("======= END WSTERM AUDIT TRACE: ",32,NULL);
    get_date_time_str(str);
    p_audit_msg(str,strlen(str),NULL);
    p_audit_msg(" =======\n",9,NULL);
    return(0);
}



/*
**********************************************************************

  Routine:            P_AUD_INIT 

  Function:
      This routine initializes any global variables used for printer 
  auditing 

  Parameters:         NONE 

  Returns:            NONE 

**********************************************************************/

p_aud_init()
{
    wst_p_aud_dex = 0;
}



/*
**********************************************************************

  Routine:            P_AUDIT_MSG 

  Function:
      This routine writes printer audit data to the printer buffer. 
  The buffer is flushed (by sending to the printer) when the end of a 
  line is detected or when the buffer is full. 

  Parameters:
     (input)          str - pointer to the buffer containg the 
                          printer audit data 
     (input)          len - specifies how many bytes of printer audit 
                          data to write to the printer audit buffer 
     (input)          stat_line - specifies the status line contents to
                          restore, after the status line area has
                          been overwritten with an error message;
                          if NULL, the foreground status line is used

  Returns:            NONE

**********************************************************************/

p_audit_msg(str,len,stat_line)
char *str;
int len;
char *stat_line;
{
    /* buffer the data in the printer audit buffer, flushing the buffer
       whenever a linefeed is received or when the buffer becomes full
    */
    while (len > 0) {
        if ((wst_p_aud_buff[wst_p_aud_dex++] = *str++) == '\n' ||
            wst_p_aud_dex >= P_AUDIT_BUFF_SIZE)
            p_aud_flush(stat_line);
        len--;
    }
}



/*
**********************************************************************

  Routine:            P_AUD_FLUSH 

  Function:
      This routine flushes the audit buffer by sending each buffered 
  character to the printer port. The routine handles the case where a 
  printer error occurs in the middle of sending the buffer by keeping 
  track of where the printer occurred and preserving the unprinted 
  characters. 

  Parameters:
     (input)          stat_line - specifies the status line contents to
                          restore, after the status line area has
                          been overwritten with an error message;
                          if NULL, the foreground status line is used

  Returns:            NONE

**********************************************************************/

p_aud_flush(stat_line)
char *stat_line;
{
    int code;
    int i;
    char prt_err_msg[SCREEN_COLS+1];
    char *err_type;
    char *prev_err;

    /* print each character in buffer until no more characters */
    /*   or until error occurs in printing */
    i = ZERO_INDEX_VALUE;
    while (i < wst_p_aud_dex) {
        code = lpchar(wst_p_aud_buff[i]);
        if (code) {
            prev_err = NULL;
            while (code) {
                code = check_printer_status(&err_type);
                if (code && prev_err != err_type) {
                    sprintf(prt_err_msg,"Printer error (%s), fix printer",err_type);
                    status_line(prt_err_msg);
                    prev_err = err_type;
                }
            }
            if (stat_line == NULL)
                update_status();
            else
                status_line(stat_line);
        }
        else
            i++;
    }
    wst_p_aud_dex = ZERO_INDEX_VALUE;
}



/*
**********************************************************************

  Routine:            LPCHAR 

  Function:
      This routine sends a single character to the printer port. 

  Parameters:
     (input)          ch - specifies what character to send to the 
                          printer port 

  Returns:            TRUE if timeout occurred, indicating a printer
                          error
                      FALSE if not error

**********************************************************************/

lpchar(ch)
int ch;
{
    union REGS reg, outreg;

    /* call BIOS to send character to printer port */
    reg.h.ah = BIOS_PRT_PRINT;     /* select print function */
    reg.x.dx = wst_printer_card;
    reg.h.al = ch;
    int86(BIOS_PRT,&reg,&outreg);

    /* timeout */
    return(outreg.h.ah & 01);
}



/*
**********************************************************************

  Routine:            SET_PRINTER_TIMEOUT 

  Function:
      This routine is used to set the duration of the wait period 
  before timeout occurs for a printer error. This is accomplished by 
  modifying timeout variables kept in the BIOS segment. 

  Parameters:
     (input)          timeout_val - specifies the timeout duration 

  Returns:            NONE 

**********************************************************************/

set_printer_timeout(timeout_val)
int timeout_val;
{
    char val;

    val = timeout_val;
    poke(BIOS_SEGMENT,PRT_TIMEOUT_BASE+wst_printer_card,&val,1);
}



/*
**********************************************************************

  Routine:            SAVE_PRINTER_TIMEOUT 

  Function:
      This routine saves the default duration of the wait period 
  before timeout occurs for a printer error. This is accomplished by 
  reading and saving the timeout variable kept in the BIOS segment. 

  Parameters:
     (input)          timeout_val - specifies the timeout duration 

  Returns:            NONE 

**********************************************************************/

save_printer_timeout()
{
    char val;

    /* save timeout value for printer card used */
    peek(BIOS_SEGMENT,PRT_TIMEOUT_BASE+wst_printer_card,&val,1);
    wst_prt_timeout = val;
}


/*
**********************************************************************

  Routine:            CHECK_PRINTER_STATUS 

  Function:
      This checks the printer status. If the printer is not ready, an 
  error message is passed back to try to specify the error. 

  Parameters:
     (output)         err_msg_ptr - address of a character pointer 
                          for passing back the address of a error 
                          message string if the printer is not ready; 
                          address passed back is NULL if printer is 
                          ready 

  Returns:            0 if printer is ready 
                      -1 otherwise 

**********************************************************************/

check_printer_status(err_msg_ptr)
char **err_msg_ptr;
{
    union REGS reg, outreg;
    char *error_msg;

    /* get printer status from BIOS */
    reg.h.ah = BIOS_PRT_STATUS;
    reg.x.dx = wst_printer_card;

    int86(BIOS_PRT,&reg,&outreg);

    /* check printer ready */
    if (outreg.h.ah == PRT_READY_MODE) {
        if (*err_msg_ptr != NULL)
            *err_msg_ptr = NULL;
        return(0);
    }

    /* check printer error */
    else {

        /* pass back error string if non-NULL address specified */
        if (err_msg_ptr != NULL) {
            switch(outreg.h.ah) {
                case PRT_OFF_LINE_MODE:
                    *err_msg_ptr = print_error_list[PEL_OFF_LINE];
                    break;
                case PRT_NO_PAPER_MODE:
                    *err_msg_ptr = print_error_list[PEL_NO_PAPER];
                    break;
                default:
                    *err_msg_ptr = print_error_list[PEL_NOT_READY];
             }
        }
    }

    /* indicate printer error occurred */
    return(-1);
}
