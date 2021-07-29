; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-01-09,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-05-14,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support local data.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (setup_stack, reset_stack)
;
;setup_stack - Manipulate the stack such that the NARC uses its own stack 
;              segment (which is in the DS) and adjust the SP appropriately.
;              ALL relevant registers are preserved here.
;              Data addressability is established and ES will contain the old 
;              DS for addressing into the other data.
;reset_stack - Adjust the stack segment and stack pointer to be what they were 
;              BEFORE the LATEST interrupt occurred.
;              ALL preserved registers are restored here. 
;              Data addressability is restored.
;*/

;/* : NOTES
;
;Syntax:
;     extrn setup_stack:near,reset_stack:near
;     push cx
;     mov  cx, size_of_local_data
;     call setup_stack
;        ...
;     call reset_stack
;
;Arguments:
;     setup_stack: cx = size of local data area
;                  current stack contains return
;*/

PAGE 55,132

include dos.mac
include util.mac
include mowsdefs.mac
include ws_stack.mac


page
;**************************************************************
;                                       DATA
;**************************************************************
dseg

;-------- External Declarations
extrn   _BASE:word

;--------- Public Declarations
public  setup_stack
public  reset_stack
public  data_seg
public  stackseg
public  sp_base

endds

page
;**************************************************************
;                                       SETUP STACK
;**************************************************************
pseg

setup_stack proc near
        cli                            ;turn interrupts off for safety
        push    ax
        push    bx
        push    bp
        push    ds
        mov     ds,CS:data_seg         ;set up addressability to local data
        mov     ax,cs:sp_base
        mov     bx,ax
        sub     bx,cx
        sub     bx,ws_stack.wsparm - ws_stack.ipreg + 4
        inc     bx
        and     bl,0feh                ;ensure stack address is even
        push    bx
        sub     bx,FRAMESIZE           ;allow space for stack operations
        mov     cs:sp_base,bx          ;set stack frame
        cmp     bx,_BASE
        ja      no_change              ;lots of stack space left

;/* : Handle STACK OVERFLOW here */

        sti
        push    ax
        mov     al,20h
        out     20h,al
        pop     ax

;/* : save registers on new stack
;     save address of old stackframe in new stack frame   */

no_change:
        mov     [bx],ax

;/* : clean up old stack */

        pop     bx                     ;bx = base address of ws_stack
        pop     dsreg[bx]
        pop     bpreg[bx]
        pop     bxreg[bx]
        pop     axreg[bx]
        mov     cxreg[bx],cx
        pop     cx                     ; cx = return address from setup_stack
        mov     dxreg[bx],dx           ; save users registers
        mov     sireg[bx],si
        mov     direg[bx],di
        mov     spreg[bx],sp
        mov     esreg[bx],es
        mov     ssreg[bx],ss
        mov     bpsave[bx],bx

;/* : now set up new stack pointers */

        mov     ss,cs:stackseg
        mov     sp,bx

;/* :  set bp = address of local data area */

        mov     bp,bx
        push    ds
        pop     es                     ; for lattice c es=ds
        mov     ax,axreg[bp]
        mov     bx,bxreg[bp]

;/* : return */

        push    cx                     ;push return address
        mov     cx,cxreg[bp]
        sti                            ; turn interrupts back on
        ret

setup_stack endp

page
;**************************************************************
;                         RESET STACK
;**************************************************************

reset_stack proc near

        cli                            ; turn interrupts off for safety

;/* : save return address */

        pop     dx                     ;get the return address of the NARC
                                       ;   function which made the call

;/* : restore user's stack registers */

        mov     bx,bp                  ; bx = address of ws_stack

        mov     ax,ssreg[bx]           ; swap in users stack stuff
        mov     cx,spreg[bx]
        mov     ss,ax
        mov     sp,cx

;/* : restore registers */

        push    dx                     ;push return address
        push    bxreg[bx]
        push    dsreg[bx]
        mov     ax,axreg[bx]           ;swap in saved registers
        mov     cx,cxreg[bx]
        mov     dx,dxreg[bx]
        mov     si,sireg[bx]
        mov     di,direg[bx]
        mov     es,esreg[bx]
        mov     bp,bpreg[bx]

;/* : restore previous stack frame  */

        mov     bx,cs:sp_base
        mov     bx,[bx]                ; bx = address of previous stack frame
        mov     cs:sp_base,bx
        pop     ds
        pop     bx
        sti
        ret

reset_stack endp

page
;**************************************************************
;                       LOCAL DATA (must be in CS)
;**************************************************************
;  These are in the CS so that they are addressible by interrupt
;  handlers which do not know the DS yet

;---------- NARC segment and stack --------
data_seg        dw      ?               ;NARC DS
stackseg        dw      ?               ;NARC SS
sp_base         dw      ?               ;NARC sp

        endps
        end
;/* pc:: END */
