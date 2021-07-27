; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-04-29,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (smdmstr)
;
;Send a string of characters directly to the communications port.
;*/

;/* : NOTES
;
;Syntax:
;      call smdmstr (buffer, length)
;
;Arguments:
;      char *buffer;   pointer to string
;      int  length;    length of string
;*/

include dos.mac                 ;Lattice include file
include mowsdefs.mac

page
;**************************************************************
;                                       DATA
;**************************************************************
dseg

;--------- External Declarations

;--------- Local Memory Declarations

endds

page
;**************************************************************
;                                       MAIN
;**************************************************************
pseg

;-------- External Procedures
extrn   send_terminal_data:near

;--------- Public Declarations
public  smdmstr

smdmstr  proc near
        pop     ax                     ; bx = return address
        pop     si                     ; si = address of string
        pop     cx                     ; cx = length of string
        push    ax

send_loop:
        push    bp
        mov     bp,sp

;/* : call send_terminal_data() */

        call    send_terminal_data
        mov     sp,bp
        pop     bp
        or      ax,ax
        jnz     send_loop              ; if send buffer full try again
        ret
smdmstr endp

        endps
        end
