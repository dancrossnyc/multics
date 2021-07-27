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
;     Created
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (wsexecap)
;
;Pass a message received from a remote MOWSE, to an application by calling 
;it's entry point stored in the local CAT. The calling sequence for the 
;application is:
;
;        p_event(mcb_ptr,minor_cap,bufseg,bufoff,buflen)
;
;        where  mcb_ptr = pointer to application's mcb
;               minor_cap = minor capability number in the message
;               bufseg = segment address of the message
;               bufoff = offset address of the message
;               buflen = length of the message
;*/

;/* : NOTES
;
;Syntax: wsexecap(catptr,capmsgp,capmsgl)
;        struct local_cat *catptr;
;        struct execap_msg *capmsgp;
;        int capmsgl;
;
;Arguments:
;        *catptr  - pointer to cat entry for application
;        *capmsgp - pointer to the structure that contains the message
;        capmsgl  - length of the message
;*/

include dos.mac
include ws.mac
include ws_msg.mac
include cat.mac

;------------- DATA

dseg

capstr  struc
oldbp   dw      ?
retn    dw      ?
catptr  dw      ?
catmsgp dw      ?
catmsgl dw      ?
capstr  ends
endds

;*******************************************************************
;                           MAIN
;*******************************************************************
pseg

       public wsexecap
wsexecap proc near

        push    bp
        mov     bp,sp
        mov     bx,[bp].catptr

;/* : load relevant information into registers since we must switch
;     to user's address space in order to set up a call to p_event */

        mov     si,[bp].catmsgp
        mov     cx,[bp].catmsgl
        mov     al,[si].ep_minor
        xor     ah,ah
        mov     di,[bx].mcb_ptr

;/* : Switch to the user's stack. */

        mov     dx,ss
        mov     bp,sp
        cli
        mov     ss,ssreg[bx]
        mov     sp,spreg[bx]
        sti

;/* : The call to the user's application is accomplished by pushing a small
;     program into the user's stack that, when called, will do a far call
;     to the application. */

        push    dx
        push    bp
        push    es
        push    ds
        sub     sp,0ch
        mov     bp,sp
        lea     dx,[bp+4]
        mov     [bp],dx
        mov     [bp+2],ss
        mov     byte ptr [bp+05h],09ah
        mov     byte ptr [bp+0ah],0cbh
        mov     byte ptr [bp+04h],090h
        mov     dx,csreg[bx]
        mov     [bp+08h],dx
        mov     dx,ws_entry[bx]
        mov     [bp+06h],dx

;/* : Set user's DS and ES registers. */

        mov     dx,ds
        push    esreg[bx]
        push    dsreg[bx]
        pop     ds
        pop     es
        mov     bp,sp
        push    cx              ; push message length
        push    si              ; push message offset
        push    dx              ; push message segment
        push    ax              ; push minor capability number
        push    di              ; push mcb pointer

;/* : Call the user application */

        call    dword ptr [bp]     ; call user application
        mov     sp,bp
        add     sp,0ch
        pop     ds
        pop     es
        pop     bx
        pop     dx

;/* : restore mowse's segment registers and stack */

        cli
        mov     ss,dx
        mov     sp,bx
        sti
        pop     bp
        ret

wsexecap endp
endps
        end
