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
;     Created.
;  2) change(86-04-29,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support for circular buffers.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (send_terminal_data)
;
;Send a BYTE string of specified length to the RS232 port while ignoring 
;protocol procedures.
;*/

;/* : NOTES
;
;Syntax:
;          mov  cx,length
;          mov  ES,DS
;          mov  ax,offset string
;          mov  si,ax
;          call send_terminal_data
;
;Arguments:
;          cx    - length of byte string
;          ES    - Data segment of byte string pointer
;          si    - Address of byte string into the Data segment
;
; Returns: ax = 0, if successful
;          ax = 1, if not successful (buffer full)
;*/

include dos.mac
include mowsdefs.mac
include ws_buf.mac
page
;******************************************************************************
;                                       DATA
;******************************************************************************
dseg

;--------- External Declarations
extrn   send_buffer:word
extrn   transmit_active:word

;--------- Public Declarations
public    send_terminal_data

endds

page
;******************************************************************************
;                                       MAIN
;******************************************************************************
pseg

;--------- External Procedures
extrn     RS232_OUT:near

send_terminal_data proc near

;/* : return if not enough room for message in send buffer

        len_buf send_buffer
        cmp     ax,cx                  ; ax = room left, cx = amount to send
        ja      next_out               ; if room
        mov     ax,1                   ; if no room in buffer
        ret                            ; if no room in send buffer

;/* : Stuff characters to send to RS232 port in a buffer */

next_out:
        mov     al, byte ptr es:[si]    ;get next character to send from
                                        ;byte string
        put_buf send_buffer             ;RS232_out(al)
        inc     si


        loopne  next_out                ;loop until character count = 0

;/* : If (transmit not active) call prime_RS232_out() */

        cmp     transmit_active,0
        jnz     noprime                 ;if transmit interrupt will pick up new data
        inc     transmit_active

;/* : Call RS232_out to send the character to the RS232 port */

        get_buf send_buffer
        jnc     noprime                 ; if send buffer empty
        call    RS232_out               ;RS232_out(al)

noprime:
        xor     ax,ax                   ;show send successful
        ret

send_terminal_data endp
        endps
        end
