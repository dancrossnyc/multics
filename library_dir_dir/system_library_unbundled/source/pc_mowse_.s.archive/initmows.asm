; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************
 page    55,132

; HISTORY COMMENTS:
;  1) change(85-12-22,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-02-15,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support of software interrupts.
;  3) change(88-01-26,Flegel), approve(88-02-29,MCR7853),
;     audit(88-03-10,Nakaska):
;     MOWSE made to drop DTR (disconnect) by default unless /H option
;     requested by user at command line.
;  4) change(88-02-10,Lee), approve(88-02-29,MCR7853),
;     audit(88-03-10,Nakaska):
;     Made parameter "lineparm" public so routine line_break() could
;     access it
;  5) change(88-06-17,Lee), approve(88-07-18,MCR7936), audit(88-08-10,Flegel):
;     Added support for Mark and Space parity
;                                                      END HISTORY COMMENTS

; PROCEDURE FUNCTION (initialize_mowse):
;
;Initialize the appropriate hardware and software interrupt vectors to enable 
;the MOWSE environment on the PC.  This also includes enabling the RS232 port 
;in preparation for PC<=>multics communications.

; NOTES
;
;Arguments:
;       lineparm - rs232 line parameters. This parameter is identical to
;                  the parameter required for the ROM-BIOS interrupt 14H
;                  except when parity is Mark or Space. 
;
;   7       6       5        4       3        2        1       0
;   ----- Baud Rate --       -Parity--      Stopbit    --Word Length
;   000 - 110                X0 - NONE        0 - 1     10 - 7 Bits
;   001 - 150                01 - ODD         1 - 2     11 - 8 Bits
;   010 - 300                11 - EVEN
;   011 - 600
;   100 - 1200
;   101 - 2400
;   110 - 4800
;   111 - 9600
;
; (*) - for Mark or Space, bits 8 to 10 of lineparm are used as follows:
;
;  10       9        8
; - Mark/Space parity -
;    101 - Mark parity
;    111 - Space parity
;
;
;Mowse is a '.com' file which will terminate and stay resident.  All interrupt 
;handlers and their associated modules also remain resident.
;
;        Interrupt vectors:
;                0Ch     hardware (RS232)
;                0Bh     software (MOWSE functions calls)
;                1Ch     timer interrupt
;                28h     console I/O interrupt


include dos.mac                 ;Lattice include file
include xoscall.mac             ;Bios and Dos Call macros
include mowsdefs.mac
include ws.mac
include ws_buf.mac

page
;*******************************************************************
;                                       DATA
;*******************************************************************
dseg

;-------- External Declarations

extrn   _TOP:word
extrn   _BASE:word
extrn   data_seg:word
extrn   stackseg:word
extrn   sp_base:word
extrn   SOFTNO:word
extrn   COM_NO:word
extrn   COM_PORT:word
extrn   HARDINTRPT:word                      ; hardware interrupt number
extrn   MASK8259:byte
extrn   startup_flags:word                   ; startup control options

;--------- Public Declarations
public  terminal_over
public  initialize_mowse
public  send_buffer
public  transmit_active
public  packet_mode
public  bg_in_use
public  mysystem;
public  fg_buf
public  bg_sav_buf
public  inbuff
public  in_protocol
public  sleepq
public  tikpermin
public  lineparm

;----------- Messages ---------------

divword     dw   60

lineparm    commparm <B9600,even_parity,stop1,data7>


;------------ Received data buffer and associated pointers
;------------ Used for debugging

        def_buf inbuff,1000

;------------ Transmitted data buffer and associated pointers
;------------ Used for debugging

        def_buf fg_buf,WSPAKSIZ+10

;------------ Transmit buffer
;------------ Contains all data to be transmitted over rs232 link

        def_buf send_buffer,BUFSIZE

;------------ Foreground buffer

terminal_over    db      0                   ;terminal buffer overflow flag

transmit_active  dw      0                   ; =1, if transmit interrupt active
send_buffer_over db      0                   ;send buffer overflow flag
mysystem         db      0                   ; system ID fo this mowse
packet_mode      dw      0                   ; =1, if in protocol running
in_protocol      dw      0                   ; set if protocol module is active
sleepq           dw      0                   ; pointer to first CAT entry on sleep queue
tikpermin        dw      0                   ; number of clock ticks per minute
bg_in_use        dw      0                   ; =1, if bg_sav_buf full
bg_sav_buf       db      WSPAKSIZ+10 dup (?)
ms_parity        db      0

endds

page
;**************************************************************************
;                               Program mainline
;**************************************************************************

pseg

;-------- External Procedures

extrn   user_interrupt_handler:near
extrn   hardware_interrupt_handler:near
extrn   int13ep:near
extrn   int1cep:near
extrn   int28ep:near
extrn   init_inbuff:near
extrn   uisystem:word
public  INTSOFT,INT13,INT1C,INT28,IN_DOS, INTHARD
public  IN_SOFT,IN_NARC,IN_INT28
public  advance_count
public  time_count
public  mowse_time
public  ticpersec
public  calladvtmr

initialize_mowse proc near

        push    bp
        push    es

;get parameters to control the com port into variable lineparm

        mov     bp,sp
        mov     ax,[bp+6]
        mov     lineparm,al
        mov     ms_parity,ah

;Test the interrupt vectors to see if they are already initialized

        mov     ax, SOFTNO
        mov     ah, GETVECTOR
        int     DOSFUNCTION
        cmp     bx, offset user_interrupt_handler
        jne     nonres

        mov     ax, HARDINTRPT
        mov     ah, GETVECTOR
        int     DOSFUNCTION
        cmp     bx, offset hardware_interrupt_handler
        jne     nonres

;If address of interrupt service routines are already in place then
;- return with a 0 return value

        pop     es
        pop     bp
        xor     ax,ax                        ; indicate MOWSE already resident
        ret

nonres:

;complete initialization, initialize circular buffers

        init_buf send_buffer
        init_buf inbuff
        init_buf fg_buf

;save the pointers to the stack to be used when servicing an interrupt

        cli
        push     DS                          ;save DS
        push     SS                          ;save SS

;save parameters which define stack to be used during interrupts

        mov     ax,sp
        sub     ax,FRAMESIZE
        mov     CS:sp_base,ax                ;bottom of MOWSE stack
        pop     CS:stackseg                  ;SS of MOWSE
        pop     CS:data_seg                  ;DS of MOWSE

        mov     byte ptr mysystem,WSIBMPC
        mov     byte ptr cs:uisystem,WSIBMPC

;set up Interrupt Vector for RS232 Interrupt

        push    DS
        mov     ax,HARDINTRPT
        mov     ah,35h
        int     DOSFUNCTION
        mov     word ptr CS:INTHARD+2,es
        mov     word ptr CS:INTHARD,bx       ;save old vector address
        mov     DX, offset hardware_interrupt_handler
        mov     ax,HARDINTRPT                ;Set interrupt vector RS232
        push    CS
        pop     DS                           ;Point to RS232 ISR in DS:DX
        mov     ah,25h
        int     DOSFUNCTION
        pop     DS

;initialize interrupt flags

        mov     ax, 1
        mov     CS:IN_INT28,ax
        mov     CS:IN_NARC,ax
        mov     CS:IN_SOFT,ax
        mov     transmit_active,ax
        mov     in_protocol,0
        mov     sleepq,0

;setup software interrupt vector

        mov     ax,SOFTNO
        mov     ah,35h
        int     DOSFUNCTION
        mov     word ptr CS:INTSOFT+2,es
        mov     word ptr CS:INTSOFT,bx       ;save old vector address
        mov     dx, offset user_interrupt_handler
        mov     ax,SOFTNO
        push    ds
        push    cs
        pop     ds
        mov     ah,25h
        int     DOSFUNCTION

;trap INT13 interrupt

        mov     al,13h
        mov     ah,35h
        int     DOSFUNCTION
        mov     word ptr CS:INT13+2,es
        mov     word ptr CS:INT13,bx         ;save old vector address
        mov     dx, offset int13ep
        mov     al,13h
        mov     ah,25h
        int     DOSFUNCTION

;trap INT1C interrupt *

        mov     al,1Ch
        mov     ah,35h
        int     DOSFUNCTION
        mov     word ptr CS:INT1C+2,es
        mov     word ptr CS:INT1C,bx         ;save old vector address
        mov     dx, offset int1Cep
        mov     al,1Ch
        mov     ah,25h
        int     DOSFUNCTION

;trap INT28 interrupt

        mov     al,28h
        mov     ah,35h
        int     DOSFUNCTION
        mov     word ptr CS:INT28+2,es
        mov     word ptr CS:INT28,bx         ;save old vector address
        mov     dx, offset int28ep
        mov     al,28h
        mov     ah,25h
        int     DOSFUNCTION
        pop     ds

;get address of in-dos flag

        mov     ah,34h
        int     DOSFUNCTION
        mov     word ptr CS:IN_DOS+2,es
        mov     word ptr CS:IN_DOS,bx

;initialize interrupt flags

        xor     ax,ax
        mov     CS:IN_INT28,ax
        mov     CS:IN_NARC,ax
        mov     CS:IN_SOFT,ax
        mov     transmit_active,ax
        mov     word ptr CS:time_count,ax
        mov     word ptr CS:time_count+2,ax
        mov     CS:advance_count,ax

;If hold_dtr (/H) option requested, then skip.  Otherwise default to dropping 
;DTR for disconnection

        mov     ax,startup_flags
        and     ax,OPTION_H                  ; Hold DTR ?
        jne     dtr_raise                    ;  YES, don't drop it

        mov     al,MASKMCR_DROP              ; Disable OUT2, drop DTR, RTS active low
        mov     DX,MCR                       ; Address MCR
        add     DX,COM_PORT
        out     DX,AL
dtr_raise:

;Determine the number of clock ticks in every second, minute, and hour.
;Count the number that occur in a 10 second interval and calculate the
;rest.

        sti
        mov     ah,2ch
        int     DOSFUNCTION                  ; get time
        push    word ptr CS:time_count+2     ; save current tick count
        add     dh,10
        cmp     dh,60
        jl      tick1                        ; if more than 60 seconds
        sub     dh,60
        inc     cl
        cmp     cl,60                        ; if more than 60 minutes
        jl      tick1
        xor     cl,cl
        inc     ch
        cmp     ch,24                        ; if more than 23 hours
        jl      tick1
        xor     ch,ch

tick1:
        mov     bx,cx                        ; save hours,minutes
        mov     si,dx                        ; save seconds

tick2:
        mov     ah,2ch
        int     DOSFUNCTION                  ; get time
        cmp     ch,bh
        jne     tick2                        ; if hours are not the same
        cmp     cl,bl
        ja      tick3
        jb      tick2
        cmp     dx,si
        jl      tick2                        ; if 10 seconds hasn't elapsed

tick3:
        mov     ax,word ptr CS:time_count+2
        pop     cx
        sub     ax,cx                        ; ax = number of ticks in 10 seconds
        shl     ax,1                         ; ax = number of ticks in 20 seconds
        mov     dx,ax
        shl     dx,1                         ; dx = number of ticks in 40 seconds
        add     ax,dx                        ; dx = number of ticks in 60 seconds
        mov     tikpermin,ax

;Calculate ticpersec = tikpermin / 60

        mov     ax,tikpermin
        cwd
        div     divword
        mov     CS:ticpersec,ax

;set up RS232 port to paramters in lineparam

        mov     dx, COM_NO                   ;COMM port 1
        mov     al, lineparm
        @bioscall RS232,0

;check for handling SPACE and MARK parity

        mov     ah,ms_parity   ; al = 101b for MARK, 111b for SPACE
        sal     ah,1           ; create parity bits mask for LCR
        jz      not_ms_parity  ; ms_parity is 0, parity not mark or space

        sal     ah,1           ; complete creating LCR parity mask, 
        sal     ah,1           ;  00101000b for MARK, 00111000b for SPACE

        mov     dx,LCR         ; get offset into LCR
        add     dx,COM_PORT    ; add base address of comm port
        in      al,dx          ; get current LCR status
        and     al,MASKLCR     ; reset DLAB
        or      al,ah          ; set appropriate LCR parity control bits
        out     dx,al          ; send to LCR

not_ms_parity:
        cli

;Enable DTR,RTS,OUT1,OUT2 on 8250 to get it into a known state

        mov     al,MASKMCR                   ;Disable OUT2, leave DTR, RTS active low
        mov     DX,MCR                       ;Address MCR
        add     DX,COM_PORT
        out     DX,AL

;enable IRQ(3 4) on 8259 interrupt controller

        in      al,IMR8259                   ;Get current masks
        and     al,MASK8259                  ;Reset IRQ(3 4) mask
        out     IMR8259,al                   ;And restore to Interrupt Mask
                                             ;Register


;enable 8250 data ready interrupt

        cli
        mov     dx,LCR                       ;Address LCR
        add     dx,COM_PORT

        in      al,dx                        ;Reset DLAB for IER access
        and     al,MASKLCR
        out     dx,al

        mov     dx,IER                       ;Address IER
        add     dx,COM_PORT

        xor     al,al                        ;Clear the IER
        out     dx,al

        mov     al,MASKIER                   ;Enable 'Data Ready' Interrupt and
        out     dx,al                        ;   Transmit holding register empty and
                                             ;   Break Characters and
                                             ;   Change in modem status

;enable Out2 on 8250

        mov     dx,MCR                       ;Address MCR
        add     dx,COM_PORT

        mov     al,MASKMCR                   ;Enable OUT2
        out     dx,al

;Allow interrupts

        mov     dx,MCR
        add     dx,COM_PORT                  ; dx = COM_PORT
        mov     al,MASKMCR                   ; al = enable OUT2
        out     dx,al                        ; enable MCR
        sti

;Return

        mov     ax, 1                        ; mowse initialized successfully
        pop     es
        pop     bp
        ret

initialize_mowse endp

page
;***********************************************************************
;       Static storage for interrupt vectors
;***********************************************************************
INT13         dd 0       ; Diskette services vector
INT1C         dd 0       ; Timer interrupt vector
INT28         dd 0       ; Console I/O vector
IN_DOS        dd 0       ; Address of in-dos flag
INTSOFT       dd 0       ; Old software Interrupt vector
INTHARD       dd 0       ; Old hardware interrupt vector
;***********************************************************************
;       Static storage for flags
;***********************************************************************
IN_SOFT       dw 0       ; =1, if in software interrupt
IN_NARC       dw 0       ; =1, if in MOWSE
IN_INT28      dw 0       ; =1, if in Console I/O
advance_count dw 0       ;counter to tell if one second has passed
time_count    dd 0       ; tick counter for MOWSE timing
                         ;so that Hal's "advtmr()" can be called
mowse_time    dw 0       ;seconds elapsed since startup
ticpersec     dw 0       ; number of clock ticks per second
calladvtmr    dw 0       ; flag indicating to call advtmr routine

endps
      end
