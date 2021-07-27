; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

page 55,132

; HISTORY COMMENTS:
;  1) change(86-02-16,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-09-04,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Installed mowse_timer to count seconds
;     rather than use timer_count to count ticks.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (int1cep):
;
;This is the NARC interrupt handler.  It is designed to receive control when 
;ever any of the three interrupts defined below occurs. If the interrupts are
;not nested, and if the interrupt did not occur in DOS, then NARC is invoked 
;by a call to rcvdatas.
;
; Entries: int 1Ch  (from timer)
;          int 28h  (from console I/O wait routine)
;          int 13h  (from diskette services)
;*/

;/* : NOTES:
;
;The implementation is a duplicate of the one used by the Print spooler in 
;Dos 2.0.
;*/

include dos.mac
include asmdefs.mac

NON_EOI    = 20h
EOI_PORT   = 20h

page
;**************************************************************
;                                       MAIN
;**************************************************************
pseg

;--------- Public Declarations

public int13ep
public int1cep
public int28ep
public wstime

;--------- External Declarations

extrn   mowse_time:word                ; Current time in MOWSE
extrn   ticpersec:word                 ; Clock tics per second
extrn   calladvtmr:word                ; Time to call advtmr routine
extrn   advance_count:word             ; Time to advance count
extrn   time_count:dword               ; Number of tics since
extrn   INT13:word                     ; Interrupt vector
extrn   INT1C:word                     ; ...
extrn   INT28:word                     ; ...
extrn   IN_DOS:word                    ; Are we in DOS flag
extrn   IN_INT28:word                  ; Are we in INT28 flag
extrn   IN_NARC:word                   ; are we in MOWSE flag
extrn   IN_SOFT:word                   ; are we in soft interrupt flag

;--------- External Procedures

extrn   rcvdata:near
extrn   receive_byte:near
extrn   setup_stack:near
extrn   reset_stack:near
extrn   advtmr:near

int1cep proc near

; /* : increment advance_count, increment time_count */

        inc     CS:advance_count
        inc     word ptr CS:time_count+2
        jnz     shortt
        inc     word ptr CS:time_count

shortt:
        inc     CS:calladvtmr          ; Increment the protocol timer
        push    cx
        push    ax
        xor     cx,cx
        call    setup_stack
        push    bp

; /* : issue non-specific EOI to allow interrupts */

        mov     al,NON_EOI
        out     EOI_PORT,al

; /* : If 1 second has elapsed
;      - reset counter
;      - increment mowsetime
;      - set flag to call advtmr */

        push    ax
        mov     ax,CS:advance_count
        cmp     ax,CS:ticpersec
        pop     ax
        jb      check_dos
        inc     CS:mowse_time
        mov     CS:advance_count,0

; /* : if in DOS, return */

check_dos:
        push    ds
        push    si
        lds     si,dword ptr cs:[IN_DOS]
        cmp     byte ptr [si],0
        pop     si
        pop     ds
        jne     ret1c

; /* : If calladvtmr flag set then reset flag and call advtmr */

        push    ax
        mov     ax,CS:calladvtmr
        shl     ax,1                   ; Multiply the current timer by two
        cmp     ax,ticpersec           ; same as comparing time to 1/2 second
        pop     ax
        jl      dont_advance           ; if equal to 1 second, increment

        mov     CS:calladvtmr,0
        push    ax
        mov     bp,sp
        call    advtmr                 ; advance protocol timeout timer
        mov     sp,bp
        pop     ax

dont_advance:

; /* : Call rcvbyte to empty rs232 buffer */

        mov     bp,sp
        call    receive_byte
        mov     sp,bp

; /* : if in narc already, return */

        cmp     CS:IN_NARC,0
        jne     ret1c

; /* : if in software interrupt, return */

        cmp     CS:IN_SOFT,0
        jne     ret1c

; /* : Call rcvdata */

        inc     cs:IN_NARC             ; Notify that we are in MOWSE
        push    ax
        mov     bp, sp
        xor     ax,ax
        push    ax
        call    rcvdata                ; call narc routine
        mov     sp, bp
        pop     ax
        mov     CS:IN_NARC,0

ret1c:
        pop     bp
        call    reset_stack
        pop     ax
        pop     cx

; /* : return from interrupt */

        jmp     dword ptr cs:[INT1C]

; /* : int28ep - interrupt 28 handler (console i/o)
;      if in narc already, return */

int28ep:
        cmp     CS:IN_NARC,0
        jne     ret28

; /* : if in software interrupt, return */

        cmp     CS:IN_SOFT,0
        jne     ret28

; /* : if in software interrupt, return */

        cmp     CS:IN_INT28,0
        jne     ret28

; /* : set in_narc and in_int28 flags */

        inc     CS:IN_NARC
        inc     CS:IN_INT28
        sti

; /* : call rcvdata */

        push    cx
        push    ax
        xor     cx,cx
        call    setup_stack            ; Swap in MOWSE stack
        push    bp
        mov     bp, sp
        mov     ax,1                   ; set dosflag to indicate that we are in DOS
        push    ax
        call    rcvdata                ; Handle messages
        mov     sp, bp
        pop     bp
        cli
        call    reset_stack            ; Swap out MOWSE stack
        pop     ax
        pop     cx
        sti

; /* : return from int28 */

        mov     CS:IN_NARC,0
        mov     CS:IN_INT28,0

ret28:
        jmp     dword ptr cs:[INT28]

int1cep endp

; /* : int13ep - interrupt 13 handler (diskette services) */

int13ep proc far
        pushf

; /* : set in_narc flag */

        inc     cs:IN_NARC
        push    cs
        push    word ptr cs:INT13RET   ; push return address

; /* : simulate interrupt */

        push    word ptr cs:INT13+2    ; push real interrupt segment address
        push    word ptr cs:INT13      ; push real interrupt offset address
        ret                            ; simulate interrupt

; /* : decrement in_narc flag */

ret13:  pushf
        dec     cs:IN_NARC
        popf
        ret     2                      ; We'll let you guess what the 2 means

INT13RET dw     offset ret13           ; return address from interrupt

int13ep endp

;/* PROCEDURE FUNCTION (wstime):
;
;returns current value of mowse_time to caller.
;*/

wstime proc near
     mov   ax,word ptr CS:mowse_time
     ret
wstime endp
endps
        end
