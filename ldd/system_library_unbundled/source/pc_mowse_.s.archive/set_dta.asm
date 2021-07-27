; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-06-16,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (set_dta)
;
;This routine is used to provide a DTA address so that the called MOWSE 
;application does not use the foreground applications's DTA. It does this 
;by saving the old DTA address and then providing a new DTA address
;*/

;/* : NOTES
;
;Syntax:
;      call set_dta()
;*/

include dos.mac
dseg

;-------------- DATA

svdtaoff        dd      0       ; save address for old DTA offset address
trap22          dd      0       ; old int22 vector address
trap23          dd      0       ; old int23 vector address
trap24          dd      0       ; old int24 vector address
newdta          db      128 dup (0);  new dta
endds

;*******************************************************************
;                          MAIN
;*******************************************************************
pseg

;-------------- PUBLICS

public  set_dta
public  rst_dta
public  set_trap
public  rst_trap
public  wstrap23
public  wstrap24

set_dta proc    near
        push    bp
        push    es
        mov     ah,2Fh
        int     21h                    ; get old dta address into es:bx
        mov     word ptr svdtaoff+2,es ; save addresses
        mov     word ptr svdtaoff,bx
        mov     dx,offset newdta
        mov     ah,1Ah
        int     21h                    ; set new dta address
        pop     es
        pop     bp
        ret
set_dta endp

;/* : PROCEDURE FUNCTION (rst_dta)
;
;This routine is used to restore the DTA address previously saved by set_dta.
;*/

;/* : NOTES
;
;Syntax:
;     call rst_dta()
;*/

rst_dta proc    near
        push    bp
        push    ds
        lds     dx,svdtaoff            ; set ds:dx old dta address
        mov     ah,1aH
        int     21h
        pop     ds
        pop     bp
        ret
rst_dta endp

;/* : PROCEDURE FUNCTION (set_trap)
;
;This routine is used to set the trap vectors for DOS Interrupts 23,24,25 to 
;a MOWSE controlled routine.
;*/

;/* : NOTES
;
;Syntax:
;     call set_trap()
;*/

set_trap proc near
        push    bp
        push    es
        mov     al,23h
        mov     ah,35h
        int     21h                    ; get old trap vector
        mov     word ptr trap23+2,es
        mov     word ptr trap23,bx
        mov     dx, offset wstrap23
        mov     al,23h
        mov     ah,25h
        int     21h                    ; set new vector
        mov     al,24h
        mov     ah,35h
        int     21h                    ; get old trap vector
        mov     word ptr trap24+2,es
        mov     word ptr trap24,bx
        mov     dx, offset wstrap24
        mov     al,24h
        mov     ah,25h
        int     21h                    ; set new vector
        pop     es
        pop     bp
        ret
set_trap endp

;/* : PROCEDURE FUNCTION (rst_trap)
;
;This routine is used to restore the trap vectors for DOS Interrupts 23,24,25 
;to the previous values so that MOWSE knows when these interrupts occur.
;*/

;/* : NOTES
;
;Syntax:
;     call rst_trap()
;*/

rst_trap proc near
        push    bp
        push    ds
        lds     dx,trap23
        mov     al,23h
        mov     ah,25h
        int     21h                    ; restore interrupt 23 vector
        pop     ds
        push    ds
        lds     dx,trap24
        mov     al,24h
        mov     ah,25h
        int     21h                    ; restore interrupt 24 vector
        pop     ds
        pop     bp
        ret
rst_trap endp

;/* : PROCEDURE FUNCTION (wstrap23)
;
;This routine will get control whenever an Interrupt 23 occurs while MOWSE 
;is active.
;*/

;/* : NOTES
;Syntax:
;     wstrap23()
;*/

INTRAP23 dw 0

wstrap23  proc near
        sti
        cmp     cs:INTRAP23,0
        jne     notrap23
        inc     cs:INTRAP23
        or      al,al
        dec     cs:INTRAP23

notrap23:
        iret

wstrap23  endp

;/* : PROCEDURE FUNCTION (wstrap24)
;
;This routine will get control whenever an Interrupt 24 occurs while MOWSE 
;is active.
;*/

;/* : NOTES
;
;Syntax:
;     wstrap24()
;*/

INTRAP24 dw 0

wstrap24  proc near
        sti
        cmp     cs:INTRAP24,0
        jne     notrap24
        inc     cs:INTRAP24
        or      al,al
        dec     cs:INTRAP24

notrap24:
        xor     al,al                  ;tell DOS to ignore the trap
        iret

wstrap24  endp
endps
        end
