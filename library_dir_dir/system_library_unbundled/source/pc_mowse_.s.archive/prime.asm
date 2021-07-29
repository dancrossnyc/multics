; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-01-01,McGinn), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created
;                                                      END HISTORY COMMENTS

;/* PROCEDURE FUNCTION (prime_rs232_out)
;
; Send a BYTE string of specified length to the RS232 port.  This has the effect
; of forcing transmit holding register empty interrupts when the character has 
; been sent where the hard_int can send out the next character to be transmitted.
;*/

;/* : NOTES
;
;       mov  cx,length
;       mov  ES,DS
;       mov  ax,offset string
;       mov  si,ax
;       call prime_RS232_out
;
; Arguments:
;       cx    - length of byte string
;       ES    - Data segment of byte string pointer
;       si    - Address of byte string into the Data segment
;*/

include dos.mac

page
;******************************************************************************
;                        DATA
;******************************************************************************
dseg

;--------- External Declarations

extrn   send_buffer:byte
extrn   send_buffer_end:word
extrn   send_buffer_head:word
extrn   send_buffer_tail:word
extrn   send_buffer_char_count:word
extrn   send_buffer_flag:word

;--------- Public Declarations

public    prime_RS232_out

endds

page
;******************************************************************************
;                        MAIN
;******************************************************************************
pseg

;--------- External Procedures
extrn     RS232_out:near

prime_RS232_out proc near

;/* pc:: Send a character in the send_buffer to RS232 port */

        push ax                       ;save used registers
        push cx
        push si
        mov  si, send_buffer_head     ;get buffer position of character to send
        mov  cx, send_buffer_char_count
                                      ;get number of characters in buffer
        cmp  cx, 0                    ;if buffer empty, do nothing
        je   empty

;   /* pc:: Call RS232_out to send the character to the RS232 port */

        mov  al, byte ptr ds:[si]     ;get next character to send from
                                      ;byte string
        push si                       ;save used registers
        push cx
        call RS232_out                ;RS232_out(al)
        pop  cx                       ;restore registers used
        pop  si

        call incptr                   ;after sending character,
                                      ;set up for next character
        mov  send_buffer_head, si     ;update pointer to next character to send
        mov  send_buffer_char_count, cx
                                      ;update number of characters to send

;   /* pc:: After character has been sent, exit routine */

empty:
        pop  si                       ;restore registers and return
        pop  cx
        pop  ax
        ret

prime_RS232_out endp

;/* : PROCEDURE FUNCTION (incptr)
;
;Increment the buffer pointer for the transmit characters.
;*/

incptr  proc near
        dec  cx                    ;decrement count of characters in buffer
        inc  si                    ;increment pointer to next character to send
        cmp  si, offset send_buffer_end
                                   ;if not at end of buffer, return
        jne  IP100
        mov  si, offset send_buffer
                                   ;end of buffer, set pointer to buffer start
IP100:  ret

incptr  endp
        endps
        end
