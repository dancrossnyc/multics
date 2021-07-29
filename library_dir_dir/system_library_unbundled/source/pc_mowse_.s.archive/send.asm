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
;  2) change(86-04-13,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Major/minor capability number in dh/dl.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (send_message)
;
;Send the message to multics through protocol channels with major/minor 
;numbers prepended to the message.
;*/

;/* : NOTES
;
;Syntax:
;        mov  cx,length
;        mov  dh,major_cap
;        mov  dl,major_cap
;        mov  si,offset data_buff
;
;Arguments:
;        cx - length of data
;        dx - major/minor capability number
;        si - address of data
;
;
;Currently, messages greater than 1 packet length will lose the data after 
;one packetlength.
;*/

page 55,132
include dos.mac
include mowsdefs.mac

dseg

;---------- Public Declarations

public send_message
hal_buff        db 255
endds
page
;**************************************************************************
;                               MAIN
;**************************************************************************
pseg

;----------- Externals

extrn snddat:near

send_message proc near

        push ax
        push cx
        push dx
        push di
        push si

;/* : place major and minor into packet buffer */

        mov  ax,offset hal_buff
        mov  di,ax

        add  dh,ADD_CONTROL            ; Make sure major printable
        mov  ds:[di],dh
        inc  di
        add  dl,ADD_CONTROL            ; Make sure minor printable
        mov  ds:[di],dl
        inc  di

;/* : Copy message into sending packet buffer */

        push cx

copyloop:
        mov  ax, es:[si]               ; si is indexing source data
        mov  ds:[di], ax               ; di is indexing destination data
        inc  si
        inc  di
        loopne copyloop

        pop  cx
        add  cx,2                      ; add character count plus minr/major count

        mov  si, offset hal_buff       ; prepare to call into snddat
        mov  ax, 1
        mov  bp,sp
        push cx                        ; length
        push si                        ; Message address
        push ax                        ; channel

;/* : call snddat(channel, data, length) */

        call snddat                     ;Hal's routine to build packet &
                                        ;send it to multics
        mov  sp,bp

        pop  si
        pop  di
        pop  dx
        pop  cx
        pop  ax
        ret

send_message endp
        endps
        end
