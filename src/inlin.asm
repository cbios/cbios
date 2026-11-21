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
                push    af
                call    chput_remove_cursor ; the screen as it is, for BS/DEL/Enter
                pop     af
                cp      13
                jr      z,inlin_enter
                cp      8
                jr      z,inlin_bs
                cp      $7F
                jr      z,inlin_del
                call    chput
                jr      inlin_loop

; BS: one cell left within the logical line, then as DEL. At column 1 the
; cursor goes to the row above only if that row continues onto this one.
inlin_bs:
                ld      hl,CSRX
                dec     (hl)
                jr      nz,inlin_del
                inc     (hl)            ; column 1
                ld      a,(CSRY)
                dec     a
                jr      z,inlin_del
                ld      e,a
                call    inlin_linttb
                jr      nz,inlin_del    ; a line starts here: stay
                ld      a,(LINLEN)
                ld      h,a
                ld      l,e
                ld      (CSRY),hl       ; the row above, its last column
; DEL: delete the character under the cursor and pull the rest of the
; logical line one cell left; its last cell becomes a space.
inlin_del:
                ld      hl,(CSRY)
                push    hl              ; the cursor, put back at the end
                ld      a,l
                call    inlin_lastrow   ; E := the line's last row
                call    curs2hl
inlin_del_cell:
                push    hl              ; this cell
                call    inlin_next
                jr      z,inlin_del_done
                call    curs2hl
                call    rdvrm           ; the next cell's character
                ex      (sp),hl
                call    wrtvrm          ; into this cell
                pop     hl
                jr      inlin_del_cell
inlin_del_done:
                pop     hl              ; the line's last cell
                ld      a,' '
                call    wrtvrm
                pop     hl
                ld      (CSRY),hl
                call    chput_restore_cursor
                jr      inlin_loop

; Enter: the logical line under the cursor, rows D to E, goes into BUF.
inlin_enter:
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
inlin_last:
                ld      a,(CSRY)
                call    inlin_lastrow   ; E := the line's last row
                ld      hl,(FSTPOS)     ; the start column, if the line starts
                ld      a,l             ; on the row input started on
                cp      d
                ld      l,d
                jr      z,inlin_col
                ld      h,1
inlin_col:
                ld      (CSRY),hl
                ld      bc,BUF
                ld      d,255           ; at most 255 characters
inlin_cell:
                call    curs2hl
                call    rdvrm
                ld      (bc),a
                inc     bc
                dec     d
                jr      z,inlin_trim
                call    inlin_next
                jr      nz,inlin_cell
inlin_trim:                             ; drop the trailing spaces
                dec     bc
                ld      a,(bc)
                cp      ' '
                jr      z,inlin_trim
                inc     bc
                xor     a
                ld      (bc),a
                ld      (INSFLG),a
                ld      a,e
                call    inlin_linttb
                ld      (hl),$AF        ; the line ends on its last row
                ld      l,e
                ld      h,1
                ld      (CSRY),hl
                call    chput_ctrl_lf   ; to the next row; on row 24 it scrolls
                call    chput_restore_cursor
                ld      hl,BUF-1
                or      a               ; NC: ended with Enter
                ret

; The cursor to the next cell of the logical line that ends on row E.
; Output : Z if it was on the line's last cell (and stays there)
; Changes: AF, HL
inlin_next:
                ld      hl,(CSRY)
                ld      a,(LINLEN)
                cp      h
                jr      nz,inlin_next_col
                ld      a,l
                cp      e
                ret     z
                ld      h,0
                inc     l
inlin_next_col:
                inc     h               ; NZ
                ld      (CSRY),hl
                ret

; A = a row (1-24) -> E = the last row of the logical line through it.
; Changes: AF, HL
inlin_lastrow:
                ld      e,a
                ld      hl,CRTCNT
                cp      (hl)
                ret     nc              ; the bottom row
                call    inlin_linttb
                ld      a,e
                ret     nz              ; the line ends on this row
                inc     a
                jr      inlin_lastrow

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
