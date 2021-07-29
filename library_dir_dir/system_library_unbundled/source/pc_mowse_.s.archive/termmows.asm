; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-08-14,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-09-11,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Resetting the MCR to not drop DTR.
;  3) change(86-09-11,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Resetting the MCR to not drop RTS as well.
;  4) change(87-04-08,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Erase the entire data area (security measures).
;                                                      END HISTORY COMMENTS

page 55,132
;/* : PROCEDURE FUNCTION (terminate_mowse):
;
;Remove MOWSE from the PC. This involves:
;     - disabling interrupts from rs232 hardware
;     - restoring interrupt vectors to pre-mowse values
;     - releasing memory used by mowse
;*/

include dos.mac          ;Lattice include file
include xoscall.mac      ;Bios and Dos Call macros
include mowsdefs.mac
include ws.mac
include ws_buf.mac

env_addr equ 2Ch         ; location of address of environment string on PSP

page
;*****************************************************************************
;                                       DATA
;*****************************************************************************
dseg

;------- External Declarations -----------------------------------------------

extrn   _PSP:word
extrn   MASK8259:byte
extrn   SOFTNO:word
extrn   COM_PORT:word
extrn   HARDINTRPT:word
extrn    _TOP:word
extrn    _STACK:word

;------- Public Declarations -------------------------------------------------

public  terminate_mowse

endds

page
;*****************************************************************************
;                               Program mainline
;*****************************************************************************
pseg

extrn INTHARD:near
extrn INTSOFT:near
extrn INT13:near
extrn INT1C:near
extrn INT28:near
extrn free_program:near

terminate_mowse proc near

        push    bp

;/* : disable hardware interrupt on 8259 interrupt controller */

        cli
        in      AL,IMR8259             ;Get current masks
        mov     cl,MASK8259
        not     cl
        or      al,cl                  ;Reset interrupt mask
        out     IMR8259,AL             ;And restore to Interrupt Mask
                                       ;Register

;/* : disable 8250 interrupts */

        mov     DX,LCR                  ;DX ==> LCR
        add     DX,COM_PORT
        in      AL,DX                   ;Reset DLAB for IER access
        and     AL,MASKLCR
        out     DX,AL
        mov     DX,IER                  ;Address IER
        add     DX,COM_PORT
        xor     ax,ax
        out     DX,AL                   ; disable all rs232 interrupts

;/* : disable DTR,RTS,OUT1,OUT2 on 8250 */

        mov     DX,MCR                  ;Address MCR
        add     DX,COM_PORT
        mov     al,MASKMCR              ;Disable OUT2, Leave DTR, RTS active low
        out     DX,AL
        sti

;/* : Restore interrupt vectors to pre-mowse values */

        push    ds
        mov     ax,HARDINTRPT
        push    cs                          ; Set up addressability into CS
        pop     ds
        mov     dx,word ptr INTHARD
        mov     ds,word ptr INTHARD+2
        mov     ah,25h
        int     21h
        pop     ds

        mov     ax,SOFTNO
        push    ds
        push    cs                          ; Set up addressability into CS
        pop     ds

;/* : restore sofware interrupt vector */

        mov     dx,word ptr INTSOFT
        mov     ds,word ptr INTSOFT+2
        mov     ah,25h
        int     21h

;/* : restore int 13 vector */

        mov     dx,word ptr INT13
        mov     ds,word ptr INT13+2
        mov     al,13h
        mov     ah,25h
        int     21h

;/* : restore int 1C vector */

        mov     dx,word ptr INT1C
        mov     ds,word ptr INT1C+2
        mov     al,1Ch
        mov     ah,25h
        int     21h

;/* : restore int 28 vector */

        mov     dx,word ptr INT28
        mov     ds,word ptr INT28+2
        mov     al,28h
        mov     ah,25h
        int     21h
        pop     ds

;/* : Erase all static data area */

        xor     al,al
        mov     cx,_TOP
        sub     cx,_STACK
        xor     si,si
        push    DS
        pop     ES

erase_data:
        mov     ES:[si],al
        inc     si
        loopne  erase_data

;/* : Free memory occupied by environment block */

        push    ES
        push    CS
        pop     ES                       ; ES = CS of program to free
        call    free_program             ; free program
        pop     ES

        pop     bp
        ret

terminate_mowse endp

endps
      end
