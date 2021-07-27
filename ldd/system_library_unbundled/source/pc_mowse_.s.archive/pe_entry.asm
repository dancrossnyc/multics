; ***********************************************************
; *                                                         *
; * Copyright, (C) Honeywell Bull Inc., 1987                *
; *                                                         *
; * Copyright, (C) Honeywell Information Systems Inc., 1986 *
; *                                                         *
; ***********************************************************

; HISTORY COMMENTS:
;  1) change(86-07-04,Westcott), approve(87-07-13,MCR7580),
;     audit(87-07-13,Leskiw), install(87-08-07,MR12.1-1072):
;     Created.
;                                                      END HISTORY COMMENTS

;/* : PROCEDURE FUNCTION (pe_antry)
;
;Provides a "far" entry point to be used by MOWSE for calling pe.c when an 
;application message is received. The use of this routine allows us to use
;the small memory model for the Lattice compiler.
;*/

include  dos.mac

;-------------- External procedures

extrn    pe:near

;-------------- Data segment

dseg

pe_str  struc
old_bp  dw      ?       ; save area for old bp
off_r1  dw      ?       ; return offset from stack call
seg_r1  dw      ?       ; return segment from stack call
off_r2  dw      ?       ; return offset from wsexecap
seg_r2  dw      ?       ; return segment from execap
mcb_ptr dw      ?       ; param for pe (mcb_ptr)
min_cap dw      ?       ; param for pe (minor_capability)
bufseg  dw      ?       ; param for pe (buffer segment address)
bufoff  dw      ?       ; param for pe (buffer offset address)
buflen  dw      ?       ; param for pe (buffer length)
pe_str  ends
endds

;*************************************************************************
;                                  MAIN
;*************************************************************************
pseg

public pe_entry

pe_entry proc far

        push    bp                     ; save BP
        mov     bp,sp                  ; Get parameter off stack
        push    buflen[bp]             ; ... bp -> pe_str.buf_lend (backwords addressing)
        push    bufoff[bp]             ; ... and put them onto stack individually
        push    bufseg[bp]
        push    min_cap[bp]
        push    mcb_ptr[bp]
        call    pe                     ; call pre_entry routine to application
        mov     sp,bp                  ; put sp back
        pop     bp                     ; restore original bp
        ret

pe_entry endp
endps
       end
