; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(87-04-30,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (free_program)
;
; Release the memory occupied by a program which is resident.  This consists
; of releasing the environment block and the program block of memory.
;
; Environment memory block paragraph:  [PSP + 2Ch]
; Program memory block paragraph:      CS
;
; Arguments:  ES - CS of program
; Returns:    ax - error code from DOS free function (49h)
;*/

include dos.mac          ;Lattice include file
include xoscall.mac      ;Bios and Dos Call macros
include ws.mac           ;MOWSE definitions
include cat.mac          ;CAT structure

env_addr equ 2Ch         ; location of address of environment string on PSP

page
;*****************************************************************************
;                                       DATA
;*****************************************************************************
dseg

;------- External Declarations -----------------------------------------------

;------- Public Declarations -------------------------------------------------

public  free_program
public  free_cat_program

endds

page
;*****************************************************************************
;                               Program mainline
;*****************************************************************************
pseg

free_program proc near

; Free environment block

        push    ES                       ; ES = CS of program

        mov     ax,ES:[env_addr]         ; ax = environment block segment
        mov     ES,ax                    ; argument in ES

        mov     ah,49h
        xor     al,al
        int     21h                      ; free environment block

        cmp     ax,7                     ; error on free (destroyed block) ?
        je      error_return             ;  YES - return
        cmp     ax,9                     ; error on free (invalid block) ?
        je      error_return             ;  YES - return

; Free program block

        pop     ES                       ; Get ES = CS again
        push    ES

        mov     ah,49h
        xor     al,al
        int     21h                      ; free program memory

        cmp     ax,7                     ; error on free (destroyed block) ?
        je      error_return             ;  YES - return
        cmp     ax,9                     ; error on free (invalid block) ?
        je      error_return             ;  YES - return

        xor     ax,ax                    ; All worked, no error

error_return:
        pop     ES
        ret

free_program endp

;/* : PROCEDURE FUNCTION (free_cat_program)
;
; Release the memory occupied by a program which is resident.  This consists
; of releasing the environment block and the program block of memory.
;
; Arguments:  CAT pointer of program to be terminated
; Returns:    ax - error code from DOS free function (49h)
;*/

free_cat_program proc near

        push    bp                       ; save registers
        mov     bp,sp
        push    ES
        push    bx

        mov     bx,[bp+4]                ; bx = CAT pointer
        mov     ax,csreg[bx]             ; ax = CS of program
        mov     ES,ax                    ; argument in ES
        call    free_program             ; free application memory

        pop     bx
        pop     ES                       ; restore registers
        pop     bp
        ret

free_cat_program endp

endps
        end
