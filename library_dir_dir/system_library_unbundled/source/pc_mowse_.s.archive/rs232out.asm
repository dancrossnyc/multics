; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

PAGE 55,132

; HISTORY COMMENTS:
;  1) change(85-12-15,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (RS232_out):
;
;Send the byte in al to the RS232 port only when it is safe to do so.
;*/

;/* : ARGUMENTS
;
;     al - byte to be outputted to RS232 port
;*/

include dos.mac                 ;Lattice include file
include mowsdefs.mac            ;Constant values
include rs232err.mac            ; Hardware interrupt errors
include ws_buf.mac              ; circular buffer macros

;*******************************************************************
;                            DATA
;*******************************************************************
dseg

;-------- External declarations

extrn  COM_PORT:word

;-------- Public Declarations
public RS232_out

endds

page
;*******************************************************************
;                             MAIN
;*******************************************************************
pseg

;--------- External Procedures

extrn put_inbuff:near

RS232_out proc near

        push ax
        push bx
        push cx
        push dx

;/* : Set up RS232 */

        mov     bl,al                  ;Save char to bl temporarily
        mov     dx,MCR                 ;Modem control Register
        add     dx,COM_PORT
        mov     al,MCRREAD             ;Out2, DTR, RTS
        out     dx,al
        sub     cx,cx                  ;Initialize timeout count
        mov     dx,MSR                 ;Modem status Register
        add     dx,COM_PORT

;/* : Wait for CTS (Clear to send) */

RS150:  
        sub     cx,cx                  ;Another timeout count

TIME_LOOP:  
        in      al,dx
        test    al,MSRCTS              ;Clear to send?
        jnz     SEND_CLEAR             ;yes
        loop    TIME_LOOP              ;No, loop til timeout

;/* : Too long, exit */

        mov     ah,INTERRUPT_STATUS
        mov     al, ISCTSTO            ; record CTS timeout
        call    put_inbuff
        jmp     short RSXIT            ;And QUIT

;/* : Wait for THRE (Transmit Holding Register Empty) */

SEND_CLEAR:
        mov     dx,LSR                 ;Line Status register
        add     dx,COM_PORT
        sub     cx,cx                  ;Another time out

STATUS_TIMING:
        in      al,dx                  ;LSR status
        test    al,LSRTHRE             ;Transmit holding reg empty
        jnz     SEND_CHAR              ;Yes
        loop    STATUS_TIMING          ;No, loop til timeout

;/* : Too long, exit */

        mov     ah,INTERRUPT_STATUS
        mov     al,ISTHRETO
        call    put_inbuff             ;record error
        jmp     short RSXIT            ;And QUIT

;/* : Get line status , send char */

SEND_CHAR:
        mov     ah,al                  ;Get line status for return
        and     ah,MASK7               ;mask bit 7
        mov     al,bl                  ;restore char to al
        mov     dx,THR                 ;transmit holding register
        add     dx,COM_PORT
        out     dx,al                  ;Output it to RS232

RSXIT:  
        pop     dx                     ;Restore Registers
        pop     cx
        pop     bx
        pop     ax
        ret

RS232_out endp
        endps
        end
