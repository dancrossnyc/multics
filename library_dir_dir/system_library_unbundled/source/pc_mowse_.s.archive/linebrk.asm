; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(87-07-04,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(88-02-11,Lee), approve(88-02-29,MCR7853),
;     audit(88-03-10,Nakaska):
;     BREAK period made to be based on calculated number of characters
;     sent to the RS232 port while the break bit is set
;                                                      END HISTORY COMMENTS
;
;FUNCTION (line_break)
;
; Send a hard break out the rs232port.  This is used when Multics MOWSE has
; not been attached.

;*/


include dos.mac                   ;Lattice include file
include mowsdefs.mac              ;Constant values
include ws_buf.mac                ; circular buffer macros

;*******************************************************************
;                            DATA
;*******************************************************************
dseg

; ----- Constants ------------------------------------------------------------

MCROUT2_MASK = 11110111b          ; Mask for the out 2 bit in MCR
BRKVAL  = 01000000b               ; Break bit in Line control register
DATA_BITS = 8
START_BITS = 1

; ----- Data Segment ---------------------------------------------------------

frame_bits  dw  11                ; holds number of bits in frame per character

;-------- External declarations

extrn  COM_PORT:word
extrn  lineparm:byte              ; line parameter values from initialize_mowse

;-------- Public Declarations

public frame_bits

endds

page
;*******************************************************************
;                             MAIN
;*******************************************************************
pseg

;--------- External Procedures

public line_break

line_break proc near
        push    ax                ; save registers
        push    bx
        push    cx
        push    dx

; /* determine the baud rate by extracting from the lineparm variable */

        mov     al,lineparm       ; get line parameter values
        mov     cl,5              ; extract highest 3 bits of line parameter
        ror     al,cl             ;   (which specifies the baud)
        and     al,7              ; clear all but lowest 3 bits and see if zero
        jnz     calc_baud         ; 0=B110, 1=B150, 2=B300, ... 7=B9600
   
        mov     ax,110            ; baud is 110, set ax to baud 110
        jmp     short line_break_next

calc_baud:
        mov     cl,al             ; baud is calculated = 75 * (2 ** cl)
        mov     ax,75
        shl     ax,cl

; /* now ax contains the baud number */
; /* determine bytes per quarter second from bits per second (baud) */
; /* determine number of bits per byte assuming: */
; /* 1 start bit, 8 data (or 7 data and 1 parity) bits, N stop */
; /* bits where N is taken from the line parameter value */

line_break_next:
        push    ax                ; save baud
                                  ; bit 2 specifies number of stop bits:
                                  ;   clear is 1 stop bit, set is 2 stop bits
        mov     al,lineparm       ; get line parameter values
        ror     al,1              ; shift til stop bit is least significant bit
        ror     al,1
        and     al,1              ; mask out all other bits
        inc     al                ; convert to actual number of stop bits

        mov     bx,DATA_BITS+START_BITS ; calculate bits per frame
        mov     ah,0
        add     bx,ax             ; add number of stop bits
        shl     bx,1            ; multiply divisor by 4 to get bits per 1/4 sec
        shl     bx,1 
        mov     frame_bits,bx     ; save in divisor location

        pop     ax                ; restore baud rate in ax
        cwd                       ; sign extend ax into dx for division
        div     frame_bits        ; divide to get characters per 1/4 second

        mov     cx,ax             ; load character count into loop counter

; disable modem interrupts which are normally generated when a character
; is sent; the interrupts are handled by MOWSE which looks at the
; output buffer for more characters to send

        mov     dx,COM_PORT       ; get modem control register to turn
        add     dx,MCR            ;     modem interrupts off while doing
        in      al,dx             ;     break

        push    ax                ; save current modem control setting
        and     al,MCROUT2_MASK   ; clear the OUT 2 bit to disable ints.
        out     dx,al             ; disable modem interrupts

        mov     dx,COM_PORT       ; port address in DX
        add     dx,LCR
        in      al,dx             ; get current setting
        or      al,BRKVAL         ; set the send-break bits
        out     dx,al             ; start the break

        push    ax                ; save LCR flags in al

        mov     al,0              ; set arbitrary character to send

line_break_loop:
        call    out_a_byte        ; send until specified number of
        loop    line_break_loop   ;    characters reached

        pop     ax                ; restore LCR flags in al
        xor     al,BRKVAL         ; clear the send-break bits
        out     dx,al             ; stop the break

        pop     ax                ; restore modem control settings
        mov     dx,COM_PORT       ; get address of MCR for
        add     dx,MCR            ;    current COMM port
        out     dx,al             ; restore the MCR settings

        pop     dx                ; restore registers
        pop     cx
        pop     bx
        pop     ax
        ret
line_break endp


;FUNCTION (out_a_byte)
;
; Send a character out to the rs232port.  This routine simply returns 
; on a time out of the send.
; 

out_a_byte proc near

        push ax
        push bx
        push cx
        push dx


;/* : Set up RS232 */

        mov     bl,al             ;Save char to bl temporarily
        mov     dx,MCR            ;Modem control Register
        add     dx,COM_PORT
        mov     al,MCRREAD        ;Out2, DTR, RTS
        out     dx,al
        mov     dx,MSR            ;Modem status Register
        add     dx,COM_PORT

;/* : Wait for CTS (Clear to send) */

OB150:  
        sub     cx,cx             ;timeout count

TIME_LOOP:  
        in      al,dx
        test    al,MSRCTS         ;Clear to send?
        jnz     SEND_CLEAR        ;yes
        loop    TIME_LOOP         ;No, loop til timeout

;/* : Too long, exit */
        jmp     short OBEXIT      ;And QUIT

;/* : Wait for THRE (Transmit Holding Register Empty) */

SEND_CLEAR:
        mov     dx,LSR            ;Line Status register
        add     dx,COM_PORT
        sub     cx,cx             ;Another time out

STATUS_TIMING:
        in      al,dx             ;LSR status
        test    al,LSRTHRE        ;Transmit holding reg empty
        jnz     SEND_CHAR         ;Yes
        loop    STATUS_TIMING     ;No, loop til timeout

;/* : Too long, exit */
        jmp     short OBEXIT      ;And QUIT

;/* : Get line status , send char */

SEND_CHAR:
        mov     ah,al             ;Get line status for return
        and     ah,MASK7          ;mask bit 7
        mov     al,bl             ;restore char to al
        mov     dx,THR            ;transmit holding register
        add     dx,COM_PORT
        out     dx,al             ;Output it to RS232

OBEXIT:  
        pop     dx                ;Restore Registers
        pop     cx
        pop     bx
        pop     ax
        ret

out_a_byte endp

        endps
        end

