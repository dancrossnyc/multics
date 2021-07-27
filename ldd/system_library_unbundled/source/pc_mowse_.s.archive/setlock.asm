; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

page 55,132
; HISTORY COMMENTS:
;  1) change(86-08-30,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (setlock)

;This procedure provides a means of setting locks in order to provide
;a means of setting locks on critical sections of instructions (code).
;The lock byte specified is exchanged with the value passed and the older
;value is returned in ax (typical return register both for 'C' routines and
;assembler.
;
;     char lockbyte;               lockbyte  db 0
;     setlock (&lockbyte);         mov    bx,offset lockbyte
;                                  push   bx
;                                  call   setlock
;
;Where lockbyte MUST be a static variable (not from the stack).
;
;Thus register ax is NOT preserved.
;*/

;/* : NOTES
;
;This implementation is simple as it allows only true/false settings on the
;lockbyte.
;*/

;/* : RETURNS

;ax = old value of lock byte.
;*/

include dos.mac                 ;Lattice include file

;**************************************************************
;                                       DATA
;**************************************************************
dseg

;--------- External Declarations

;--------- Public Declarations
public  setlock

;--------- Local Memory Declarations

endds

;**************************************************************
;                                       MAIN
;**************************************************************
pseg

;-------- External Procedures

setlock proc near
        push    bp
        mov     bp,sp

        push    bx
        mov     bx,[bp+4]       ; address of lock in bx

        xor     ax,ax
        mov     al,1            ; set test register
lock    xchg    [bx],al         ; exchange register with lock

        pop     bx
        pop     bp
        ret

setlock endp

        endps
        end
