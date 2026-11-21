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
; Function : Reads a line typed with the screen editor, from column 1
; Output   : BUF - the line, ended by a 0; HL - BUF - 1
;            C-flag set when it ends with the STOP key
; Registers: All
pinlin:
                call    H_PINL
                ld      a,1             ; the whole line, from column 1
                jr      inlin_start

;--------------------------------
; $00B4 QINLIN
; Function : Prints "? " and reads a line, as INLIN
; Output   : See PINLIN
; Registers: All
qinlin_prompt:
                db      "? ",0
qinlin:
                call    H_QINL
                ld      hl,qinlin_prompt
                call    prn_text
                ; continue with inlin

;--------------------------------
; $00B1 INLIN
; Function : Reads a line typed with the screen editor, from the column
;            the cursor is at (column 1 when AUTFLG is set)
; Output   : See PINLIN
; Registers: All
inlin:
                ld      a,(CSRY)
                dec     a
                jr      z,inlin_top
                call    inlin_linttb    ; the row above: a new line starts here
                ld      (hl),$AF
inlin_top:
                ld      a,(AUTFLG)
                or      a
                ld      a,1
                jr      nz,inlin_start
                ld      a,(CSRX)
inlin_start:
                ld      hl,(CSRY)       ; L = the row
                ld      h,a             ; H = the column the line starts at
                ld      (FSTPOS),hl
inlin_loop:
                call    chget
                cp      13
                jr      z,inlin_enter
                call    chput
                jr      inlin_loop

; Enter: the logical line under the cursor, rows D to E, goes into BUF.
inlin_enter:
                call    chput_remove_cursor
                ld      a,','
                ld      (BUF-1),a       ; stops the trim below
                ld      a,(CSRY)
inlin_first:                            ; D := the line's first row
                ld      d,a
                dec     a
                jr      z,inlin_last
                ld      e,a
                call    inlin_linttb
                ld      a,e
                jr      z,inlin_first   ; the row above continues on this one
inlin_last:                             ; E := the line's last row
                ld      a,(CSRY)
inlin_last_lp:
                ld      e,a
                ld      hl,CRTCNT
                cp      (hl)
                jr      nc,inlin_read   ; the bottom row
                call    inlin_linttb
                ld      a,e
                jr      nz,inlin_read   ; the line ends on this row
                inc     a
                jr      inlin_last_lp
inlin_read:
                ld      hl,(FSTPOS)     ; the start column, if the line starts
                ld      a,l             ; on the row input started on
                cp      d
                ld      a,h
                jr      z,inlin_col
                ld      a,1
inlin_col:
                ld      (CSRX),a
                ld      a,d
                ld      (CSRY),a
                push    de
                ld      de,BUF
                ld      c,255           ; at most 255 characters
inlin_row:
                ld      a,(LINLEN)
                ld      hl,CSRX
                sub     (hl)
                inc     a
                ld      b,a             ; the cells from CSRX to the row's end
                call    curs2hl
inlin_cell:
                call    rdvrm
                ld      (de),a
                inc     de
                inc     hl
                dec     c
                jr      z,inlin_full
                djnz    inlin_cell
                ld      a,1
                ld      (CSRX),a
                pop     hl              ; L = the last row
                push    hl
                ld      a,(CSRY)
                cp      l
                jr      z,inlin_full
                inc     a
                ld      (CSRY),a
                jr      inlin_row
inlin_full:
inlin_trim:                             ; drop the trailing spaces
                dec     de
                ld      a,(de)
                cp      ' '
                jr      z,inlin_trim
                inc     de
                xor     a
                ld      (de),a
                ld      (INSFLG),a
                pop     de
                ld      a,e
                ld      (CSRY),a
                call    inlin_linttb
                ld      (hl),$AF        ; the line ends on its last row
                ld      a,1
                ld      (CSRX),a
                call    chput_ctrl_lf   ; to the next row; on row 24 it scrolls
                call    chput_restore_cursor
                ld      hl,BUF-1
                or      a               ; NC: ended with Enter
                ret

; A = a row (1-24) -> HL = its LINTTB entry, A = the entry, Z if 0
; (the row continues on the next). Changes: AF
inlin_linttb:
                add     a,(LINTTB-1) & $FF
                ld      l,a
                ld      h,LINTTB >> 8   ; LINTTB does not cross a page
                ld      a,(hl)
                or      a
                ret

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
