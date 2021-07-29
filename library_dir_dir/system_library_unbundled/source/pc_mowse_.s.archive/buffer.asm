; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

page 55,132

; HISTORY COMMENTS:
;  1) change(86-04-01,McGinn), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;  2) change(86-04-27,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support macros.
;  3) change(86-09-09,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Support for checking for length field in test
;     for protocol mode.
;  4) change(86-09-15,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Change check for checksum to look for CRC char
;     of reset request packet.
;  5) change(86-12-09,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Removed packetize flag setting so that it can
;     be left to mio to set when all is peachy.
;  6) change(87-03-24,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Changed to look for r_EOP character as the
;     EOP rather than hardcoding a <LF>
;  7) change(87-04-03,Flegel), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Corrected reset packet testing in get_inbuff
;     to account for error characters in inbuff
;                                                      END HISTORY COMMENTS

;/* : FUNCTION
;
;This is a collection of routines which are used for buffer manipulations.
;*/

include dos.mac
include mowsdefs.mac
include ws_buf.mac              ; circular buffer macros
include rs232err.mac

SOP          = 1                ; definition of a reset packet
RSTREQ       = 32               ;    type field
LENCHR       = 37               ;    length field
CRCCHAR      = 58               ;    CRC field
dseg

;---------- Public Declarations

public put_inbuff
public get_inbuff
public len_inbuff
public init_inbuff

extrn inbuff:word
extrn packet_mode:word
extrn r_EOP:byte
extrn datab:word
endds

page

pseg

;/* : PROCEDURE FUNCTION (init_inbuff):
;
;Initialize the input buffer, and place/remove an initial character to "get
;the buffer going".
;*/

init_inbuff proc near

        init_buf inbuff                     ; Initialization macro
        mov     al,'a'
        xor     ah,ah
        call    put_inbuff                  ; Put a test character in
        call    get_inbuff                  ; Get it out

init_inbuff endp


;/* : PROCEDURE FUNCTION (put_inbuff):
;
;Puts the character in AL in the input buffer.
;*/

;/* RETURNS:
;
;    ax = 0 for no error
;    ax = 1 for buffer overflow
;*/

put_inbuff proc near

        push    si                          ; save registers
        push    bx

        mov     bx,offset inbuff            ; bx = inbuff
        mov     si, bin[bx]                 ; si = buffer_in
        or      ah,ah                       ; error prefix?
        jz      nopfx                       ;  YES - abort
        mov     byte ptr ds:[si],ah         ;   NO - process character

;/* : inbuff = inbuff + 1
;     if (inbuff is at end of buffer)
;     - inbuff = start of buffer */

        inc     si                          ; inc counter
        cmp     si,blast[bx]                ; buffer wrapping ?
        jbe     short nae1                  ;   NO - continue
        mov     si,bfirst[bx]               ;  YES - wrap counter

;/* : if (not overflow) store inbuff, clear error code, and return */

nae1:
        cmp     si, bout[bx]
        je      ovf1                        ; if buffer full

; /* : inbuff = inbuff + 1
;      if (inbuff is at end of buffer)
;      - inbuff = start of buffer */

nopfx:
        cmp     datab,DATA7                 ; if data7 then mask bit 8
        jne     no_mask
        and     al,7Fh                      ; mask data8 bit

no_mask:
        mov     byte ptr ds:[si],al

        inc     si                          ; inc counter
        cmp     si,blast[bx]                ; wrapped buffer ?
        jbe     short nae2                  ;   NO - continue
        mov     si,bfirst[bx]               ;  YES - wrap counter

;/* : if (not overflow) store inbuff, and return */

nae2:
        cmp     si, bout[bx]                ; buffer full ?
        je      ovf1                        ;  YES - overflow error

        mov     bin[bx],si                  ; reset the in counter

        pop     bx                          ; restore registers
        pop     si
        ret

;/* : else store overflow flag in buffer */

ovf1:
        mov     si,bin[bx]                  ; restore original in pointer
        dec     si
        cmp     si,bfirst[bx]
        ja      ovf2
        mov     si,blast[bx]

ovf2:
        mov     byte ptr ds:[si],5          ; overflow flag = 5
        pop     bx
        pop     si
        ret

put_inbuff endp

; : END put_buffer

;/* : PROCEDURE FUNCTION (get_inbuff):
;
;Get the next character from the rsr232 input circular buffer if MOWSE is not 
;in packet mode and a SOP character is received, return with no data to the 
;user until 4 characters have been accumulated in the buffer. If these 4 
;characters constitute a valid MOWSE RST packet, enter packet mode and give 
;the characters to the caller. If no SOP is found or if the SOP is not the 
;start of a RST packet, then just give the current charcter to the caller.
;*/

;/* RETURNS:
;
;   Carry flag SET, if data available
;   Carry flag CLEAR, if no data available
;*/

get_inbuff proc near

;/* : check for empty buffer */

        push    si
        push    bx
        mov     bx, offset inbuff
        mov     si,bout[bx]
        cmp     si,bin[bx]
        jne     bnemp1
        jmp     bemp2

;/* : if (buffer not empty) get character into AX */

bnemp1:
        mov     al,byte ptr ds:[si]

;/* : outbuff = outbuff + 1
;     - if (outbuff = end of buffer) outbuff = start of buffer
;     - if not in packet mode, look for reset packet */

        inc     si
        cmp     packet_mode,0
        jne     in_packet_mode

        cmp     al,SOP
        jne     in_packet_mode              ; if not a start of (reset) packet

        mov     ax,bin[bx]
        sub     ax,bout[bx]
        jge     rst99
        add     ax,bsize[bx]

rst99:  cmp     ax,5                        ; must be at least 5 characters in reset packet
        jl      bemp2                       ; return nothing to caller until 4 chars available

        cmp     si,blast[bx]
        jbe     rst1
        mov     si,bfirst[bx]

rst1:
        call    next_char                   ; get next valid char
        call    chk_index                   ; make sure valid character
        jnc     bemp2
        cmp     al,RSTREQ
        jne     givechar                    ; not a reset packet
        inc     si
        cmp     si,blast[bx]
        jbe     rst2
        mov     si,bfirst[bx]

rst2:
        call    next_char                   ; get next valid char
        call    chk_index                   ; make sure valid character
        jnc     bemp2
        cmp     al,LENCHR
        jne     givechar                    ; not a reset packet
        inc     si
        cmp     si,blast[bx]
        jbe     rst3
        mov     si,bfirst[bx]

rst3:
        call    next_char                   ; get next valid char
        call    chk_index                   ; make sure valid character
        jnc     bemp2
        cmp     al,CRCCHAR
        jne     givechar                    ; not a reset packet
        inc     si
        cmp     si,blast[bx]
        jbe     rst4
        mov     si,bfirst[bx]

rst4:
        call    next_char                   ; get next valid char
        call    chk_index                   ; make sure valid character
        jnc     bemp2
        cmp     al,r_EOP
        jne     givechar                    ; not a reset packet
        mov     packet_mode,1               ; reset received, enter packet mode

givechar:
        mov     si,bout[bx]
        mov     al,byte ptr ds:[si]
        inc     si

in_packet_mode:
        cmp     si,blast[bx]
        jbe     nae4
        mov     si,bfirst[bx]

nae4:
        mov     bout[bx],si
        jmp     bnemp2

;/* : if (buffer empty) clear AX, and return */

bemp2:
        pop     bx
        pop     si
        clc                                 ; CLEAR carry flag to indicate no data
        ret

bnemp2:
        pop     bx
        pop     si
        stc                                 ; SET carry flag to indicate data available
        ret

get_inbuff endp

;/* : END get_buffer */

;/* : PROCEDURE FUNCTION (len_inbuff):
;
; Returns the number of characters in the buffer in AX.
;*/

len_inbuff proc near

        len_buf inbuff
        ret

len_inbuff endp

;/* : END get_length */

;/* : PROCEDURE FUNCTION (chk_index):
;
; Test if the current value of si is within the current inbuffer.
;
; carry flag = 1: valid index
;            = 0: invalid index
;*/

chk_index proc near

        push    ax
        mov     ax,bin[bx]
        cmp     ax,bout[bx]                 ; in < out ?
        jb      reverse_test                ;  YES - reverse test

        cmp     si,bout[bx]                 ; ?????O----Ixxxxx
        jb      invalid_test                ; ^^^^^

        cmp     si,bin[bx]                  ; xxxxxO----I?????
        jb      valid_test                  ; ^^^^^^^^^^
        jmp     invalid_test

reverse_test:

        cmp     si,bin[bx]                  ; -----I????O-----
        jb      valid_test                  ; ^^^^^

        cmp     si,bout[bx]                 ; -----I????O-----
        jb      invalid_test                ; ^^^^^^^^^^

valid_test:
        stc                                 ; Carry = 1, passed
        pop     ax
        ret

invalid_test:
        clc                                 ; Carry = 0, failed
        pop     ax
        ret

chk_index endp

;/* : END get_length */

;/* : PROCEDURE FUNCTION (next_char)
;
; Returns the next valid character in the inbuff thus skipping over error escape
; characters.
;*/

next_char proc near

try_char:
        mov     al,byte ptr ds:[si]         ; get current char

        cmp     al,ESCAPE_STATUS            ; escape char ?
        ja      valid_char                  ;   NO - then current is OK
        je      esc_chr                     ;  YES - then next char is valid

        cmp     al,LINE_STATUS              ; line_status char ?
        jb      valid_char                  ;   NO - then current is OK

skip_2:
        inc     si                          ; increment index
        cmp     si,blast[bx]
        jbe     skip_1
        mov     si,bfirst[bx]

skip_1:
        inc     si                          ; increment index
        cmp     si,blast[bx]
;       jbe     next_char
        jbe     try_char
        mov     si,bfirst[bx]

        jmp     try_char                    ; repeat as this may be another escape
;       jmp     next_char                   ; repeat as this may be another escape

esc_chr:
        inc     si                          ; increment index
        cmp     si,blast[bx]
        jbe     no_wrap
        mov     si,bfirst[bx]
no_wrap:
        mov     al,byte ptr ds:[si]         ; get current char


valid_char:

        ret

next_char endp

;/* : END next_char */

        endps
        end
