; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(88-03-22,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
;     Created.
;  2) change(88-09-01,Lee), approve(88-05-16,MCR7897), audit(88-09-19,Flegel):
;     Modified to get active page from BIOS segment rather
;     than assuming page 0 is the active page.
;                                                      END HISTORY COMMENTS
;
;FUNCTION (save_screen,restore_screen)
;
; Save the contents of the display to a user-specified buffer.
; Restore the contents of the display from a user-specified buffer.
; Only the first 24 lines of the screen will be saved.
;
;*/

include dos.mac                 ;Lattice include file

;*******************************************************************
;                                       DATA
;*******************************************************************

;define variables which are maintained by BIOS in the BIOS segment

BIOS_SEG	SEGMENT AT 0040H
	org	0050H
cursor_posn	dw ?        ; holds cursor coordinates kept by BIOS
     org  0062H
active_page    db ?        ; contains current active page

BIOS_SEG	ends


dseg
; constants

BIOS_VIDEO = 10h           ; BIOS video interrupt call
BIOS_SET_CURSOR = 2        ; BIOS video set cursor position function number
BIOS_GET_CURSOR = 3        ; BIOS video get cursor position function number
BIOS_READ_CHR_ATTR = 8     ; BIOS video get character and attributes number
BIOS_WRITE_CHR_ATTR = 9    ; BIOS video write character and attributes number
DISPLAY_WIDTH = 80         ; number of columns across displayed
DISPLAY_HEIGHT = 24        ; number of lines displayed

endds

page
;**************************************************************************
;                               Program mainline
;**************************************************************************
;

pseg

public	save_screen
public	restore_screen
public    get_active_page

; save_screen(buffer)

save_screen	proc	near
	push		bp		           ; save registers used
     push      di
     push      dx
	push      cx
     push      bx
     push      es
	push		si
	mov		bp,sp	           ; set up pointer to arguments
	mov		di,[bp+16]           ; fetch argument (pointer to user-buffer)

	mov		ax,BIOS_SEG          ; BIOS segment
     mov       es,ax                ; use es to reference into BIOS segment
     call		get_active_page	 ; get active page in al
	mov		si,ax                ; save active page in si
     sal       si,1                 ; times 2 to index word offset
	mov		bh,al                ; save current active page
	mov		ah,BIOS_GET_CURSOR   ; get cursor position
	int		BIOS_VIDEO           ; BIOS video interrupt
	push		dx	                ; save cursor coordinates
	xor		dx,dx                ; set cursor position to 0,0

save_loop:
	mov		es:cursor_posn[si],dx    ; get cursor coordinates
	mov		ah,BIOS_READ_CHR_ATTR  ; get character and attribute at position
	int		BIOS_VIDEO
	mov		[di],ax              ; save into buffer
	inc		di	                ; increment buffer pointer
	inc		di
	inc		dl                   ; increment column position
	cmp		dl,DISPLAY_WIDTH	 ; past 80th column?
	jl		save_loop
	mov		dl,0		           ; reset column to 0
	inc		dh
	cmp		dh,DISPLAY_HEIGHT	 ; past 24th line?
	jl		save_loop            ; repeat for next line if not

	pop		dx		           ; restore previous cursor coordinates
	mov		ah,BIOS_SET_CURSOR
	int		BIOS_VIDEO

	pop		si
     pop       es                   ; restore registers
     pop       bx
	pop		cx
     pop       dx
     pop       di
	pop		bp

	ret                            ; return to caller

save_screen	endp


; restore_screen(buffer)

restore_screen	proc	near
	push		bp                   ; save registers used
     push      di
     push      bx
	push		cx
     push      dx
     push      es
     push		si
	mov		bp,sp                ; set up pointer to arguments
	mov		di,[bp+16]           ; fetch argument (pointer to user buffer)

	mov		ax,BIOS_SEG          ; BIOS segment
     mov       es,ax                ; use es to reference into BIOS segment

     call      get_active_page      ; get current active page in al
     mov       si,ax			 ; save active page in si
     sal       si,1                 ; times 2 to index word offset
	mov		bh,al	           ; get from current active page
	mov		ah,BIOS_GET_CURSOR   ; get cursor position
	int		BIOS_VIDEO		 ; BIOS video routine
	push		dx		           ; save cursor coordinates

	xor		dx,dx	           ; set cursor position to 0,0
	mov		cx,1		           ; number of characters for writing character
restore_loop:
	mov		es:cursor_posn[si],dx    ; get cursor coordinates
	mov		ax,[di]	           ; get character from buffer
	mov		bl,ah
	mov		ah,BIOS_WRITE_CHR_ATTR ; write character & attributes
	int		BIOS_VIDEO
	inc		di		           ; increment buffer pointer
	inc		di
	inc		dl		           ; increment column position
	cmp		dl,DISPLAY_WIDTH	 ; past 80th column?
	jl		restore_loop
	mov		dl,0		           ; reset column to 0
	inc		dh
	cmp		dh,DISPLAY_HEIGHT	 ; past 24th line?
	jl		restore_loop         ; repeat for next line if not

	pop		dx		           ; restore previous cursor coordinates
	mov		ah,BIOS_SET_CURSOR 
	int		BIOS_VIDEO

     pop		si
     pop       es                   ; restore registers
     pop       dx
     pop       cx
     pop       bx
     pop       di
	pop		bp

	ret                            ; return to caller
restore_screen	endp


; get_active_page() - this routine returns the active page in register
;    al; the active page is obtained from a variable in the BIOS segment.

get_active_page proc near
	push		es				; save segment register
	mov		ax,BIOS_SEG		; get address of BIOS segment
	mov       es,ax			; reference BIOS segment with es
     mov		al,es:active_page	; fetch active page from BIOS segment
     pop		es				; restore segment register
	ret
get_active_page endp

endps
      end
