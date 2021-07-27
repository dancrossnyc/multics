; ********************************************
; *                                          *
; * Copyright, (C) Honeywell Bull Inc., 1988 *
; *                                          *
; ********************************************
;
; HISTORY COMMENTS:
;  1) change(88-01-23,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
;     Created.
;                                                      END HISTORY COMMENTS
;
;******************************************************************************
;
;   Module:   CTRLBRK.ASM
;
;   Function: These are a set of routines which are used to manipulate the 
;             keyboard break handler interrupt in order that the 
;             <CONTROL><BREAK> key may be handled by setting a flag
;             called "break_flag" other than being handled by the default 
;             handler.
;
;             The routine "ctrlbrk" is the handler.  What MUST be remembered 
;             is that this is an interrupt routine and MUST save and restore 
;             all registers.
;
;   Date:      January 23, 1988
;
;   Author:    Michael S. Flegel
;              908-1540-29 Street N.W.
;              Calgary, Alberta
;              Canada
;              T2N-4M1
;              (403) 289-3426
;
;   Resources: Advanced Computing Technology Centre
;              301-1620-29 Street N.W.
;              Calgary, Alberta
;              Canada
;              T2N-4L7
;              (403) 284-6400
;
;   Equipment: IBM-PC AT
;              IBM-PC DOS 3.10
;              Microsoft (R) Overlay Linker, Version 3.51
;              IBM Macro Assembler, Version 2.00
;
;******************************************************************************


include dos.mac

BREAK_PENDING  = 800h

;******************************************************************************
;                             DATA SEGMENT
;******************************************************************************

dseg

old_break_cs    dw  (?)                ; old CS of keyboard break routine
old_break_ip    dw  (?)                ; old IP of keyboard break routine

extrn  break_flag:word                 ; term's control flags

endds

;******************************************************************************
;                             PROGRAM SEGMENT
;******************************************************************************

pseg

public  ctrlbrk
public  set_break
public  reset_break

data_segment    dw  (?)                ; term's data segment register

;
;******************************************************************************
; This routine is activated whenever the <CTL><BREAK> keys are simultaneously
; struck.
;
; All that happens is that the global flag "break_flag" is set.
;******************************************************************************

ctrlbrk proc    near

        push    DS                     ; save used registers
        pushf

        push    CS:data_segment        ; set up data addressability
        pop     DS

        or      break_flag,BREAK_PENDING    ; set the break flag

        popf                           ; restore used registers
        pop     DS

        iret                           ; return from interrupt

ctrlbrk endp
;
;******************************************************************************
; This procedure sets the interrupt 1b routine (keyboard break address)
; to transfer to our "ctrlbrk" routine so that we can handle it ourselves
;******************************************************************************

set_break proc  near

        push    ax                     ; save registers
        push    bx
        push    dx
        push    ds
        push    es

; save the current DS for use by the handler

        push    ds
        pop     ax
        mov     CS:data_segment,ax

; save the control-break vector (1Bh)

        mov     ah,35h                 ; get the old vector
        mov     al,1bh
        int     21h

        mov     old_break_cs,es        ; save the old CS and IP
        mov     old_break_ip,bx

; set the new control-break vector

        mov     dx,offset ctrlbrk      ; set to our ctrlbrk routine
        push    cs
        pop     ds

        mov     ah,25h                 ; signal interrupt to set new vector
        mov     al,1bh
        int     21h

        pop     es                     ; restore registers
        pop     ds
        pop     dx
        pop     bx
        pop     ax
        ret

set_break endp
;
;******************************************************************************
; This routine restores the old interrupt 1b (keyboard break address) routine
; to the one which existed before set_break was invoked.
;******************************************************************************

reset_break proc near

        push    ax                     ; save registers
        push    dx
        push    ds

; restore saved control-break vector

        mov     dx,old_break_ip        ; restore the old CTL-BRK vector
        push    old_break_cs
        pop     ds

        mov     ah,25h                 ; signal the interrupt to restore vector
        mov     al,1bh
        int     21h

        pop     ds                     ; restore registers
        pop     dx
        pop     ax
        ret

reset_break endp

endps
        end
