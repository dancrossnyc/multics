; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-01-10,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-05-22,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support for gettdata structure.
;  3) change(86-09-05,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Returns minor cap in AX.
;  4) change(86-12-15,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Concatenation of consequetive FG_TERMINAL_DATA
;     messages.
;                                                      END HISTORY COMMENTS

page 55,132
;/* : PROCEDURE FUNCTION (terminal_buffer_check):
;
;Check the terminal buffer to see if anything is in it, if so, copy the 
;characters into the buffer supplied in ES:DI.
;*/

;/* : ARGUMENTS:
;
;        es:di - contains a pointer (byte) to where the
;                character string is be placed.
;        cx    - max. no. of characters wanted
;*/

;/* : RETURNS:
;
;        ax - minor capability number
;        cx - numbers of characters copied to buffer
;*/

include dos.mac
include mowsdefs.mac
include ws.mac
include ws_fgb.mac
include wsmincap.mac

page
;**************************************************************
;                                       DATA
;**************************************************************
dseg

;------------ External Declarations
extrn   fgbfptr:word
extrn   fgblptr:word
extrn   fgaptr:word

;------------ Public Declarations
public  terminal_buffer_check

endds

page
;**************************************************************
;                                       MAIN
;**************************************************************
pseg

;------------ External Declarations
extrn   wsfree:near

terminal_buffer_check proc near

        push    bx                         ;save registers to be used
        push    dx
        push    di
        xor     ax,ax                      ; ax = minor capability
        xor     dx,dx                      ; dx = copied data length

        cmp     cx,0                       ; cx = length of caller's buffer
        jbe     normal_return              ; if caller's buffer full, return

;/* : Look at each pending piece of foreground data
;     - Check terminal_buf status */

copy_buffer:
        mov     bx,fgbfptr                 ; bx = data buffer pointer
        cmp     bx,0
        je      normal_return              ; return if empty (no more)

        cmp     cx,fgb_length[bx]          ; cx = space left in caller's buffer
        jb      normal_return              ; if caller's buffer not big enough

;/* : Concatenate FG_TERMINAL_DATA pieces together */

        cmp     ax,0                       ; First piece extracted ?
        je      extract_portion            ; ... get portion

        cmp     ax,FG_TERMINAL_DATA        ; Not Concatenating ?
        jne     normal_return              ; ... return

        cmp     fgb_minor[bx],FG_TERMINAL_DATA  ; Concatenating next piece ?
        je      extract_portion                 ; ... get portion

        jmp     normal_return              ; Complete message so return

;/* : If (Something in terminal_buf) */

extract_portion:
        add     dx,fgb_length[bx]          ; increment copy count
        sub     cx,fgb_length[bx]          ; decrement space left length
        push    cx                         ; save space left length

        lea     si,fgb_char[bx]            ; load address of data
        mov     cx,fgb_length[bx]          ; cx = length of copy data

        mov     al,fgb_minor[bx]           ; minor capability in ax
        xor     ah,ah
        push    ax                         ; save on stack

        or      cx,cx                      ; Empty buffer ?
        jbe     nextbuf                    ; ... free empty buffer


;/* : copy to caller's buffer */

tbcloop:
        lodsb                              ; si -> source
        stosb                              ; di -> destination (always)
        loop    tbcloop                    ; si++, di++, cx-- ... automagic

nextbuf:
        pop     ax                         ; restore minor
        pop     cx                         ; restore space left

        mov     si,fgb_next[bx]            ; fgb_next = next foreground message
        mov     fgbfptr,si                 ; set fgbfptr (first) to next message

;/* : Free the message extracted */

        cmp     si,0                       ; If more buffers
        jne     free_msg
        mov     fgblptr,si                 ; set fgblptr (last) to NULL

free_msg:
        push    ax                         ; save minor
        push    cx                         ; save space left length
        push    dx                         ; save length
        push    di                         ; save destination buffer position

        push    bp
        mov     bp,sp
        push    bx                         ; bx = message to free
        push    fgaptr
        call    wsfree                     ; free the buffer just emptied
        mov     sp,bp
        pop     bp

        pop     di                         ; restore destination buffer position
        pop     dx                         ; restore length
        pop     cx                         ; restore space left length
        pop     ax                         ; restore minor

        jmp     copy_buffer                ; Get next portion

normal_return:
        mov     cx,dx                      ; cx = length of copied data
        pop     di
        pop     dx
        pop     bx
        ret

terminal_buffer_check endp                 ;End of termbuf_check procedure

        endps
        end

