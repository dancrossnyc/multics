; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

page 55,132
; HISTORY COMMENTS:
;  1) change(86-07-26,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Installed support for /M option in MOWSE.
;  3) change(86-08-29,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Test and set installed to prevent recursive
;     calls to recevie_byte, thus preventing race condition.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (rcvbyte)

;Transfer received data from the RS232 input circular buffer to the protocol
;analyzer (rcvchr.c) if MOWSE is running, and into fg_sav_buf if MOWSE is
;not running.

;This routine also detects hardware error and status flags embedded in the
;data stream in a two byte
;*/

;/* : NOTES

;format:
;      Byte 0 = 3, following byte contains line status      */
;      Byte 0 = 4, following byte contains modem status     */
;      Byte 0 = 5, following byte contains buffer status    */
;      Byte 0 = 6, following byte contains input data       */

;If an error or status message is detected, then a message will be written to
;the PC screen.

;This routine calls get_inbuff, which also detects whether the MOWSE protocol
;is running and sets the the packet_mode flag if it is.                          */
;*/

;/* : RETURNS

;number of characters transferred.
;*/

include dos.mac                 ;Lattice include file
include mowsdefs.mac
include rs232err.mac            ; Error definitions
include wsmincap.mac            ; Predefined mincaps
include ws_buf.mac              ; Circular buffer definitions

;**************************************************************
;                                       DATA
;**************************************************************
dseg

;--------- External Declarations

extrn packet_mode:word
extrn in_protocol:word
extrn fg_buf:word
extrn error_mode:word

;--------- Public Declarations
public  receive_byte

;--------- Local Memory Declarations
merr      db      "***** ERROR  Modem status :  "
merrval   db      0,0
rerr      db      "***** ERROR  Receive Error:  "
rerrval   db      0,0
berr      db      "***** ERROR Buffer overflow ",0

proc_lock db      0       ; procedure lock
endds

;**************************************************************
;                                       MAIN
;**************************************************************
pseg

;-------- External Procedures
extrn   get_inbuff:near
extrn   sstmsg:near
extrn   rcvchr:near
extrn   setlock:near

receive_byte proc near
        push    bp

; /* : If currently executing, return */
        mov     bp,sp
        mov     bx,offset proc_lock
        push    bx
        call    setlock
        mov     sp,bp
        test    ax,1
        je      rcv_ok                   ; if lock not set then OK
        jmp     rcv_active               ; else do not process

; /* : Pass characters in inbuff to protocol */

rcv_ok:
        test    packet_mode,1            ; if in packet mode
        jnz     rcv_loop                 ;    jump to packet mode handler
        push    ax                       ; else dump chars to fg buffer
        push    bx
        mov     bx,offset fg_buf
        mov     ax,bout[bx]
        sub     ax,bin[bx]
        ja      skip1                    ; if no wrap
        add     ax,bsize[bx]

skip1:
        cmp     ax,2
        pop     bx
        pop     ax
        jb      rcv_nodat               ; if fg_buf is full
        mov     byte ptr fg_buf+bminor,FG_TERMINAL_DATA
        jmp     rcv_loop

rcv_nodat:
        jmp     no_rcvdata

rcv_loop:
        call    get_inbuff
        jnc     rcv_nodat               ; if input buffer empty
        cmp     al,ESCAPE_STATUS
        ja      call_rcv                ; if valid character
        je      esc_rcv                 ; if escape character
        cmp     al,LINE_STATUS
        jb      call_rcv                ; if valid character
        je      lsrerr                  ; receive error
        cmp     al,INTERRUPT_STATUS
        je      inbuff_ovf              ; if buffer overflow occurred
        call    get_inbuff              ; get error character

;/* : display error message */

        mov     bx, offset merr
        mov     merrval,al
        jmp     display_err

lsrerr:
        call    get_inbuff              ; get error character
        mov     bx, offset rerr
        mov     rerrval,al

display_err:
        test    error_mode, 1           ; if in error mode, display modem error
        je      rcv_loop
        mov     bp,sp
        push    bx
        call    sstmsg
        mov     sp,bp
        jmp     rcv_loop

inbuff_ovf:
        mov     bx,offset berr
        jmp     display_err

esc_rcv:
        call    get_inbuff              ; get valid character

;/* : call rcvchr to pass character to PAD */

call_rcv:
        mov     bx, packet_mode
        cmp     bx, 1
        jne     receive_data
        mov     bp, sp
        push    ax
        call    rcvchr
        mov     sp, bp
        jmp     rcv_cont

;/* : Else move data to foreground save buffer */

receive_data:
        put_buf fg_buf
        push    bx
        mov     bx,offset fg_buf
        mov     ax,bout[bx]
        sub     ax,bin[bx]
        ja      skip2                   ; if no wrap
        add     ax,bsize[bx]

skip2:
        cmp     ax,2
        pop     bx
        jb      no_rcvdata              ; if fg_buf is full

rcv_cont:
        jmp     rcv_loop

no_rcvdata:
lock    mov     proc_lock,0             ; clear procedure lock

rcv_active:                             ; jump label around clearing of
        pop     bp                      ;    procedure lock
        ret

;/* : END receive_byte */
receive_byte endp

        endps
        end
