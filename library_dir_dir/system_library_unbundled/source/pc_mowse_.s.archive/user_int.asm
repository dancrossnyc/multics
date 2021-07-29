; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-01-03,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-04-13,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Extracted send_message module.
;  3) change(86-05-07,Lee), approve(87-07-13,MCR7580), audit(87-07-13,Leskiw),
;     install(87-08-07,MR12.1-1072):
;     Added CREATE_INST, DESTROY_INST, FINDCAPNUM,
;     and FINDCAPNAME handlers.
;  4) change(86-05-21,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support parameter copying.
;  5) change(86-08-12,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Installed FG_BREAK and DISCONNECT.
;  6) change(86-08-27,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Modifications to DISCONNECT.
;  7) change(86-09-02,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Installed checks for packet mode mincaps.
;     8) chnage(86-09-27,ASmith): Installed SUSPEND handler.
;  9) change(86-10-18,ASmith), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Installed connect by name handler.
; 10) change(87-02-10,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Changed tests for MOWSE active to check
;     packetize_flag instead of packet_mode
;                                                      END HISTORY COMMENTS

page 55,132
        .xlist
; /* : PROCEDURE FUNCTION (user_interrupt_handler):
;
; This is the MOWSE user interrupt handler.  This deals with MOWSE requested 
; functions.
; */

; /* : NOTES:
;
; Syntax:
;         #include <dos.h>                mov ah,<MOWSE FUNCTION>
;         union REGS *inregs,outregs;     int USERINTRPT
;         inregs->h.al = <MOWSE FUNCTION>;
;         int86(USERINTRPT,in,out);
;
;  Input Arguments:
;         ah   - register determines MOWSE FUNCTION (input)
;         si   - address of parameter structure
;         cx   - length of parameter structure
;
; */
;
;/* : RETURNS:
;         ax - return code appropriate to command called
;
; If data is required to be passed back, it would be preferrable for the 
; generating interrupt to pass addresses as determined by the interrupt handler
; requested and to return only an error code in the AX register.
; */

; /* : NOTES
;
; Each case in this massive switch is primarily concerned with setting up
; the argument list to each of the handlers for that specific case.  This may
; include moving data into and out of parameter structures.
; */

include dos.mac
include ws.mac
include ws_stack.mac
include ws_dcls.mac
include ws_error.mac
include wsmincap.mac

page
;**************************************************************
;                                       MAIN
;**************************************************************
dseg

;          Public Declarations

public user_interrupt_handler

;          Data Declarations

endds

page
;**************************************************************
;                                       MAIN
;**************************************************************

PGROUP  GROUP   PROG
PROG    SEGMENT WORD PUBLIC 'PROG'
        ASSUME  CS:PGROUP

;--------- External Procedures -----------------------------------------------

extrn   packetize_flag:word
extrn   bgcount:word
extrn   IN_SOFT:word
extrn   setup_stack:near
extrn   reset_stack:near
extrn   send_terminal_data:near
extrn   terminal_buffer_check:near
extrn   snddat:near
extrn   i_execom:near
extrn   i_creins:near
extrn   i_desins:near
extrn   i_fndcnu:near
extrn   i_fndcna:near
extrn   i_execap:near
extrn   i_getbgm:near
extrn   i_putbgm:near
extrn   i_sendbg:near
extrn   i_reset:near
extrn   i_sleep:near
extrn   i_suspnd:near
extrn   i_connect:near
extrn   terminate_mowse:near
extrn   toggle_debug_switches:near
extrn   snddis:near
extrn   sndbrk:near
extrn   line_break:near

;------ publics --------------------------------------------------------------

public uisystem

user_interrupt_handler proc near

         jmp     arounds               ; provide a signiture for MOWSE

         even
uisystem db     0
         db     'MOWSE',0

; /* : call setup_stack () */

arounds:
        inc     CS:IN_SOFT
        call    setup_stack            ;Set up user interrupt stack frame
        push    bp

; /* : copy parameters from caller's address space into MOWSE address space */

        or      cx,cx
        jbe     nocopy                 ; if no parameter to copy
        push    ds
        pop     es
        mov     si,sireg[bp]
        lea     di,wsparm[bp]
        mov     ds,dsreg[bp]
        rep     movsb                  ; copy string into this address space
        push    es
        pop     ds

; /* : switch (user call type) */

nocopy:
        xor    cx,cx                   ; requested MOWSE function:
        mov    cl,ah                   ; switch (ah)
        sal    cx,1
        mov    di,cx
        cmp    di,MAXJMP               ; ensure request is valid
        jbe    do_jump                 ; if request not valid
        jmp    end_int

do_jump:
        mov    cx,CS:JMPTBL[di]
        jmp    cx

; /* : -- case SENDMESSAGE: not used */

SENDMESS:
        jmp    end_int

; /* : SENDTERM: Not used */

SENDTERM:
        jmp    end_int

; /* : -- case EXECUTCOM: */

EXECUTCOM:
        push    bp                     ; save bp
        lea     di,wsparm[bp]          ; di -> the parameter structure
        mov     bp,sp                  ; save SP so we can put it back after the C
        push    di                     ; put the parameter pointer on the stack (arg)
        call    i_execom               ; call the procedure
        mov     sp,bp                  ; restore the stack position
        pop     bp                     ; restore BP
        jmp     copy_back              ; copy resutls back into callers space

; /* : -- case 3: EXECAP */

EXECAP:
        push    bp
        lea     di,wsparm[bp]
        mov     cx,cxreg[bp]
        mov     bp,sp
        push    cx
        push    di
        call    i_execap
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case 4: CREATE_INSTANCE */

CREATE_INST:
        push    bp
        lea     di,wsparm[bp]
        mov     ax,bp
        mov     bp,sp
        push    di                     ; push address of copy of caller's mcb
        push    ax                     ; push address of stack
        call    i_creins
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case 5: DESTROY_INSTANCE */

DESTROY_INST:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_desins
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case 6: FINDCAPNAME */

FINDCAPNAME:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_fndcna
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case 7: FINDCAPNUM */

FINDCAPNUM:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_fndcnu
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case 8: Debug packet switches - undocumented */

L08:
        mov     ax,wsparm[bp]
        push    bp
        push    ax
        call    toggle_debug_switches
        pop     bp
        jmp     copy_back

; /* : -- case GETTDATA: Get data from terminal buffer */

GETTDATA:
        lea     bx,wsparm[bp]
        mov     cx,bgcount             ; see if background messages are pending
        mov     gbpflag[bx],cx

        mov     di,getlbp[bx]          ;di = address of caller's buffer
        mov     cx,getlbs[bx]          ;cx = size of caller's buffer
        push    es
        mov     es,dsreg[bp]
        push    bp                     ;check for foreground data
        call    terminal_buffer_check
        pop     bp
        pop     es

        cmp     cx,0                   ;if no data then check the mode flag
        jne     data_ready

        test    packetize_flag,1       ;if clear, then set minor cap to mowse_detached
        jne     set_attached           ;else set minor to mowse_attached
        mov     ax,MOWSE_DETACHED
        jmp     data_ready

set_attached:
        mov     ax,MOWSE_ATTACHED

data_ready:
        mov     gmincap[bx],ax
        mov     ax,cx                  ;return number of chars obtained to caller
        mov     cx,gettlen
        jmp     copy_back

; /* : -- case PUTTDATA   : Send data over foreground channel */

PUTDATA:
        lea     bx,wsparm[bp]
        test    packetize_flag,1
        jz      nopkt

; /* : --- if in packet mode create send packet structure for snddat */

        mov     ax,minor_cap[bx]
        lea     si,pkthdr[bp]
        mov     [si],al                ; insert minor cap in packet header
        mov     datap[bp],si           ; insert pointer to packet header
        mov     datal[bp],1            ; insert length of packet header
        lea     si,puttstr[bx]
        mov     datap+2[bp],si         ; insert pointer to data string
        mov     ax,putstrl[bx]
        mov     datal+2[bp],ax         ; insert length of data string
        mov     datac[bp],2            ; insert count of packet pieces
        mov     chan[bp],FG            ; insert channel id (foreground)
        lea     bx,chan[bp]

; /* : --- give the packet to the protocol to send */

        jmp     send_pkt               ; give data to protocol

; /* : -- else, just transmit the data */

nopkt:
        push    bp
        mov     cx,putstrl[bx]
        lea     si,puttstr[bx]
        mov     bp,sp
        call    send_terminal_data
        mov     sp,bp
        pop     bp
        jmp     end_int

; /* : -- case GETBGMES: */

GETBGMES:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di                     ; push address of parameter structure
        call    i_getbgm               ; get background message
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case PUTBGMES: */

PUTBGMES:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di                     ; push address of parameter structure
        call    i_putbgm               ; put background message
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case SENDBG: */

SENDBG:
        push    bp
        lea     di,wsparm[bp]
        mov     cx,cxreg[bp]
        mov     bp,sp
        push    cx                     ; push length of message
        push    di                     ; push address of message
        call    i_sendbg
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case L14: Nothing */

L14:
        mov     bx,wsparm[bp]
        mov     ax,[bx]
        jmp     end_int

; /* : -- case RESET: */

RESET:
        push    bp
        lea     di,wsparm[bp]
        mov     cx,cxreg[bp]
        mov     bp,sp
        push    cx                     ; push length of message
        push    di
        call    i_reset
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case SLEEP */

SLEEP:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_sleep
        mov     sp,bp
        pop     bp
        jmp     copy_back

SUSPEND:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_suspnd
        mov     sp,bp
        pop     bp
        jmp     copy_back

CONNECT:
        push    bp
        lea     di,wsparm[bp]
        mov     bp,sp
        push    di
        call    i_connect
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case DISCONNECT */

DISCONNECT:
        test    wsparm[bp],1           ; but if force is requested, continue
        jne     discon_ok

        test    packetize_flag,1       ; if in packet mode then fail
        je      discon_ok

        push    bp
        mov     bp,sp
        call    snddis                 ; establish disconnection attempt
        mov     sp,bp
        pop     bp
        jmp     copy_back

discon_ok:
        push    bp
        mov     bp,sp
        call    terminate_mowse        ; remove mowse from the PC
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : -- case FOREBREAK: */

FOREBREAK:
        xor     ax,ax
        test    packetize_flag,1       ; Packet mode ?
        jne     soft_brk               ;  YES - Protocol break
        call    line_break             ;   NO - Line break
        jmp     copy_back

soft_brk:
        push    bp
        mov     bp,sp
        call    sndbrk                 ; call to send break to Multics
        mov     sp,bp
        pop     bp
        jmp     copy_back

; /* : send_pkt : give a packet structure to the mowse protocol for packetizing
;      and subsequent trasmission */

send_pkt:
        push    bp
        lea     si,datap[bp]
        lea     di,datal[bp]
        mov     ax,chan[bp]
        mov     cx,datac[bp]
        mov     bp,sp
        push    di
        push    si
        push    cx
        push    ax
        call    snddat                 ; send the packet to the protocol
        mov     sp,bp
        pop     bp
        cmp     ax,1
        je      send_pkt               ; if window full, try again
        mov     axreg[bp],ax           ; save return code
        jmp     end_int


; /* : COPY_BACK: copy parameter structure back to user's address space before
;      returning from interrupt. */

copy_back:
        mov     axreg[bp],ax           ; save return code
        mov     cx,cxreg[bp]
        or      cx,cx
        jbe     end_int                ; if no parameter to copy
        mov     di,sireg[bp]
        lea     si,wsparm[bp]
        mov     es,dsreg[bp]
        rep     movsb                  ; copy string into this address space

; /* : End of interrupt: call reset_stack(), return from interrupt */

end_int:
        pop     bp                     ; Put the stack to where it was
        call    reset_stack            ;   before the interrupt
        dec     CS:IN_SOFT
        iret

;          Switching Table

JMPTBL  dw      pgroup:SENDMESS
        dw      pgroup:SENDTERM
        dw      pgroup:EXECUTCOM
        dw      pgroup:EXECAP
        dw      pgroup:CREATE_INST
        dw      pgroup:DESTROY_INST
        dw      pgroup:FINDCAPNAME
        dw      pgroup:FINDCAPNUM
        dw      pgroup:L08
        dw      pgroup:GETTDATA
        dw      pgroup:PUTDATA
        dw      pgroup:GETBGMES
        dw      pgroup:PUTBGMES
        dw      pgroup:SENDBG
        dw      pgroup:L14
        dw      pgroup:RESET
        dw      pgroup:SLEEP
        dw      pgroup:DISCONNECT
        dw      pgroup:FOREBREAK
        dw      pgroup:SUSPEND
        dw      pgroup:CONNECT
JMP_END dw      0
MAXJMP  = JMP_END - JMPTBL

; /* : END user_interrupt_handler */
user_interrupt_handler endp

        endps
        end

