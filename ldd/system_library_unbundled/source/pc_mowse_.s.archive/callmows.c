/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-05-09,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
  2) change(86-05-20,Westcott), approve(87-07-13,MCR7580),
     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
     Provided a return code mechanism.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (call_mowse_int)

Calls one of the MOWSE's interrupt handlers, passing it a parameter
structure containing all the required data; all data in the parameter
structure is input data as this structure is copied into MOWSE's space 
(i.e. passing the structure by value).  It is up to the caller to ensure 
that the structure passed is the same as the structure expected by the 
interrupt handler.  This provides a general parameter passing mechanism
between applications and MOWSE interrupt handlers.
*/

/* : NOTES

This is the first inklink of MOWSE that the application sees.
*/

#include <dos.h>
#include <ws.h>
#include <ws_error.h>

#define INTRPT_VECTOR_TABLE   1023     /* Where DOS interrupt vectors live */
#define INTRPT_ADDRESS_SIZE   4        /* Number of bytes to the transfer vector */
#define SIGNATURE_SIZE        8        /* Size of signature */
#define SIGNATURE_ID_OFFSET   4        /* Offset into signature of MOWSE id */

int USER_INTERRUPT = 0;                /* user interrupt number (/I to mowse) */
                                       /*    must be set here at "process" init */
int local_system;                      /* system id for local system */

call_mowse_int (p_mowse_minor, p_param_addr, p_param_size)

int  p_mowse_minor;                    /* Internal function to execute */
char *p_param_addr;                    /* Parameter structure for the function */
int  p_param_size;                     /* Length of parameter structure */
{
int segment;                           /* MOWSE code segment */
int intoff;                            /* Interrupt address offset */
int offset;                            /* Interrupt vectors */
union sigun
{  int  signature[SIGNATURE_ID_OFFSET];/* MOWSE signature */
   char csig[SIGNATURE_SIZE];          /* Signature information */
} sig;
union REGS inregs, outregs;            /* Register information */

/* : if USER_INTERRUPT not set (=0 )
     - scan low memory for user_interrupt_handler signature which signifies
       where MOWSE resides as a user_interrupt, or does not reside at all */

   if (USER_INTERRUPT == 0)
   {  for (offset = 0; offset < INTRPT_VECTOR_TABLE; offset += INTRPT_ADDRESS_SIZE)
      {  peek (0, offset, &sig.csig[0], SIGNATURE_SIZE);
         segment = sig.signature[1];
         intoff  = sig.signature[0] + SIGNATURE_ID_OFFSET;
         peek (segment, intoff, &sig.csig[0], SIGNATURE_SIZE);
         if (strcmp (&sig.csig[1],"MOWSE") == 0) break;
      }

/* : If passed the whole table, then MOWSE not found */

      if (offset > INTRPT_VECTOR_TABLE)
         return (WSNOTRES);

/* : Otherwise convert the address of the offset to an iterrupt number */

      USER_INTERRUPT = offset >> 2;
      local_system = (int) sig.csig[0];
   }

/* : Generate the interrupt */

   inregs.h.ah = p_mowse_minor;                  /* ah = MOWSE minor */
   inregs.x.si = (short)p_param_addr;            /* si = parameter structure address */
   inregs.x.cx = p_param_size;                   /* cx = size of parameter */
   int86(USER_INTERRUPT, &inregs, &outregs);
   return (outregs.x.ax);                        /* ax = return code */
}
