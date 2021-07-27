; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-07-06,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (putbgmes)
;
;Sends a message from a background application to the foreground message 
;handler. These messages will be queued by MOWSE until they are retrieved 
;via a getbgmes call from a foreground program.
;*/

;/* : NOTES
;
;Syntax:
;    code = putbgmes(mcb_ptr,code,caller,control,arg1,..argN)
;    mcb *mcb_ptr;
;    int code;
;    char *caller;
;    char *control;
;
;Arguments:
;    mcb_ptr - the pointer to the application's MOWSE control block.
;    code    - determines the type of message if 0, the message will 
;              consist only of the contents of the control string if WSQUERY, 
;              the control string will be used to prompt the user for a 
;              response.
;    caller  - the name of the background machine that is making this call.
;    control - a "printf" control string
;    Arg1..ArgN
;            - printf arguments to be substituted into the control argument. 
;              These arguments are optional. They are only used if required 
;              by the control argument.
;
;Returns:
;    WSINVBUF, if the resulting message is longer than WSPAKSIZ
;*/

include dos.mac
include ws.mac
include ws_dcls.mac
include ws_mcb.mac
include ws_func.mac

;----------- WORKING STORAGE FOR PUTBGMES

dseg

sv_bp   dw      ?       ; caller's BP save space
sv_retn dw      ?       ; return address from call
sv_mcb  dw      ?       ; pointer to caller's mcb
sv_code dw      ?       ; code
work    putbgstr <0,0,0>
string  db      "%d",0  ; control string to sprintf for code expansion

endds

pseg
;----------- PUBLICS

public putbgmes

;----------- EXTERNALS

extrn  call_mowse_int:near
extrn  sprintf:near

;----------- DYNAMIC STORAGE FOR PUTBGMES

dyns    struc
old_bp  dw      ?       ; caller's BP save
retn    dw      ?       ; return address from call
mcb_ptr dw      ?       ; pointer to caller's mcb
code    dw      ?       ; code
caller  dw      ?       ; address of caller string
control dw      ?       ; address of control string
dyns    ends

putbgmes proc near
        push    bp
        mov     bp,sp

;/* : cleanup stack so that a call to sprintf can be made */

        pop     ax
        mov     sv_bp,ax
        pop     ax
        mov     sv_retn,ax
        pop     ax
        mov     sv_mcb,ax
        pop     ax
        mov     sv_code,ax

;/* : insert the caller's name at the beginning of the string */

        pop     si
        lea     di,work.pbgmsg
        xor     cx,cx

pl1:
        lodsb
        or      al,al
        jz      pl2    ; if end of caller's name
        stosb
        inc     cx
        jmp     pl1

pl2:
        mov     al,':'
        stosb
        mov     al,' '
        stosb
        add     cx,2
        mov     work.plength,cx

;/* : Copy in the code value if it is not 0 and not WSQUERY */

        mov     ax, sv_code
        cmp     ax, WSQUERY
        jne     no_query
        mov     work.ptype, ax
        jmp     no_code

no_query:
        mov     work.ptype, WSINFO     ; Info type
        or      ax, ax                 ; Is the code 0?
        je      no_code

        push    di                     ; save message position
        mov     bp, sp
        push    ax                     ; ax is code
        lea     si, string             ; string is control string
        push    si
        push    di                     ; di is current position in message
        call    sprintf
        mov     sp, bp
        add     work.plength, ax       ; save the length of the string
        pop     di
        add     di, ax                 ; increment position in message

        mov     al,'.'                 ; terminate the code with some chars
        stosb
        mov     al,' '
        stosb
        add     work.plength,2

;/* : since control string and arguments are already in the stack, all we 
;     have to do is push the address of the buffer and set the stack pointer. */

no_code:
        mov     bp,sp
        push    di
        call    sprintf
        mov     sp,bp

        add     work.plength,ax        ; save length of string

        mov     bx,sv_mcb
        mov     ah,system_id[bx]
        mov     al,major_capability[bx]
        mov     work.psender_major,ax

        mov     ax,work.plength        ; make sure message is less than WSPAKSIZ
        cmp     ax,WSPAKSIZ
        jle     no_overflow
        mov     work.plength,WSPAKSIZ

no_overflow:
        mov     bp,sp
        mov     cx,type work
        push    cx
        lea     bx,work
        push    bx
        mov     ax,I$PUTBGMES
        push    ax
        call    call_mowse_int
        mov     sp,bp
        mov     bp,sv_bp
        push    sv_retn
        ret

putbgmes endp
endps
        end

