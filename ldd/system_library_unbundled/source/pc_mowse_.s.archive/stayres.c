/* ***********************************************************
   *                                                         *
   * Copyright, (C) Honeywell Bull Inc., 1987                *
   *                                                         *
   * Copyright, (C) Honeywell Information Systems Inc., 1986 *
   *                                                         *
   *********************************************************** */

/* HISTORY COMMENTS:
  1) change(86-06-06,Rohs), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
     install(87-08-07,MR12.1-1072):
     Created.
                                                   END HISTORY COMMENTS */

/* : PROCEDURE FUNCTION (stayres)

Load the calling application into resident memory.
*/

#include <dos.h>
#include <cat.h>

#define FUNCTION_NUMBER   0x31
#define STAYRES_FUNCTION  33

extern int _TSIZE;

stayres(mcbptr)

mcb *mcbptr;                           /* Caller's MCB */
{
struct SREGS segregs;
union  REGS  inreg;

/* : Generate DOS interrupt 0x31 */

   segread(&segregs);
   inreg.x.dx = _TSIZE;
   inreg.h.ah = FUNCTION_NUMBER;
   int86(STAYRES_FUNCTION,&inreg,0);
}
