; $Id: $
; INLIN/PINLIN/QINLIN routines for C-BIOS
;
; Copyright (c) 2007 Eric Boon.  All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;--------------------------------
; $00AE PINLIN
; Function : Stores in the specified buffer the character codes input
;           until the return key or STOP key is pressed
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
; TODO: call H_PINL
pinlin:
                call    H_PINL
		jp	inlin

;--------------------------------
; $00B4 QINLIN
; Function : Prints a questionmark and one space and then calls INLIN
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
qinlin_prompt:
		db	"? ",0
qinlin:
                call    H_QINL
                ld      hl,qinlin_prompt
                call    prn_text
		; continue with inlin
;--------------------------------
; $00B1 INLIN
; Function : Same as PINLIN except that AUGFLG (#F6AA) is set
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All

inlin:
                call    chget                   ; get a character from the kbd

                ld      b,11                    ; check control characters
                ld      hl,inlin_table
                call    search_table

                cp      $20                     ; Control character?
                jr      nc,inlin_printable

inlin_nonprintable:                             ; then...
                rst     $18                     ; OUTDO
                xor     a                       ; we just put out a ctrl char
                ld      (INSFLG),a              ; switch insert mode off
                ld      (CSTYLE),a
                jr      inlin

inlin_printable:                                ; else...
                push    af
                ld      a,(INSFLG)
                and     a
                call    nz,inlin_insert
                pop     af
                rst     $18
                jr      inlin

inlin_insert:
                ret

inlin_wback:
                ret

inlin_break:
                ret

inlin_clear:
                ret

inlin_wfwd:
                ret

inlin_bs:
                ret

inlin_cr:
                ret

inlin_end:
                ret

inlin_ins:
                ret

inlin_clrlin:
                ret

inlin_esc:
                ret

inlin_del:
                ret

inlin_table:
                db      $02
                dw      inlin_wback             ; CTRL-B: word back
                db      $03
                dw      inlin_break             ; CTRL-C: stop, abort, quit
                db      $05
                dw      inlin_clear             ; CTRL-E: clear to end of line
                db      $06
                dw      inlin_wfwd              ; CTRL-F: word fwd
                db      $08
                dw      inlin_bs                ; BACKSP: erase char left
                db      $0D
                dw      inlin_cr                ; ENTER : confirm, yes, ok
                db      $0E
                dw      inlin_end               ; CTRL-N: to end of line
                db      $12
                dw      inlin_ins               ; INSERT: toggle insert mode
                db      $15
                dw      inlin_clrlin            ; CTRL-U: clear line
                db      $1B
                dw      inlin_esc               ; ESCAPE: ignore
                db      $7F
                dw      inlin_del               ; DELETE: erase char under csr
; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
