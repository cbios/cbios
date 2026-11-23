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
                call    inlin_lastrow_cur ; E := the line's last row
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
inlin_setcur:
                pop     hl
                ld      (CSRY),hl
; ESC does nothing in the editor. BS, DEL and INS also end here.
inlin_esc:
                call    chput_restore_cursor
inlin_loop:
                call    chget
                push    af
                call    chput_remove_cursor ; the screen as it is, for BS/DEL/Enter
                pop     af
                ld      hl,INSFLG
                cp      8
                jr      z,inlin_bs
                cp      $7F
                jr      z,inlin_del
                cp      $12
                jr      z,inlin_ins
                cp      9
                jr      z,inlin_tab
                cp      $1B
                jr      z,inlin_esc
                cp      ' '
                jr      nc,inlin_char
                ld      (hl),0          ; another control code ends insert mode
                inc     l
                inc     l
                ld      (hl),0          ; CSTYLE, two bytes after INSFLG
                cp      13
                jp      z,inlin_enter
                cp      3
                jp      z,inlin_break   ; CTRL+C, also what CTRL+STOP queues
                cp      2
                jp      z,inlin_wordl   ; CTRL+B
                cp      6
                jp      z,inlin_wordr   ; CTRL+F
                cp      5
                jr      z,inlin_clreol  ; CTRL+E
                cp      $0E
                jr      z,inlin_eol     ; CTRL+N
                cp      $15
                jr      z,inlin_clrln   ; CTRL+U
inlin_put:
                call    chput
                jr      inlin_loop
inlin_char:
                inc     (hl)
                dec     (hl)
                jr      z,inlin_put     ; overwrite mode
                call    inlin_insert
                jr      inlin_loop

; INS: insert mode on or off; the insert cursor shows while it is on.
inlin_ins:
                ld      a,(hl)
                cpl
                ld      (hl),a
                and     $AF
                inc     l
                inc     l
                ld      (hl),a          ; CSTYLE
                jr      inlin_esc

; TAB: in insert mode spaces are inserted up to the next tab stop.
inlin_tab:
                inc     (hl)
                dec     (hl)
                jr      z,inlin_put     ; overwrite mode: CHPUT's TAB
                ld      a,(SCRMOD)
                cp      2
                ld      a,9
                jr      nc,inlin_put    ; no text screen: CHPUT ignores it
inlin_tab_lp:
                call    chput_remove_cursor ; CHPUT put it back
                ld      a,' '
                call    inlin_insert
                ld      a,(CSRX)
                dec     a
                and     7
                jr      nz,inlin_tab_lp
                jr      inlin_loop

; CTRL+U: to where the line is read from, then clear it as CTRL+E.
inlin_clrln:
                ld      a,(CSRY)
                call    inlin_firstrow
                call    inlin_startpos
; CTRL+E: clear from the cursor to the end of the logical line; the line
; now ends on the cursor's row.
inlin_clreol:
                ld      hl,(CSRY)
                push    hl
                call    inlin_lastrow_cur   ; E := the line's last row
inlin_clreol_lp:
                push    de
                call    chput_esc_k     ; this row, from CSRX; LINTTB $AF
                pop     de
                ld      hl,(CSRY)
                ld      a,l
                cp      e
                jp      z,inlin_setcur  ; the cursor goes back where it was
                inc     l
                ld      h,1
                ld      (CSRY),hl
                jr      inlin_clreol_lp

; CTRL+N: to the cell after the logical line's last character.
inlin_eol:
                call    inlin_lastrow_cur   ; E := the line's last row (also in A)
                call    inlin_firstrow  ; D := its first row
                ld      l,d
                ld      h,1
                ld      (CSRY),hl
                ld      b,h
                ld      c,l             ; C, B: the row and column to go to
inlin_eol_lp:
                call    curs2hl
                call    rdvrm
                cp      ' '
                jr      z,inlin_eol_next
                ld      hl,(CSRY)       ; a character: the cell after it
                ld      a,(LINLEN)
                cp      h
                jr      nz,inlin_eol_col
                ld      h,0             ; column 1 of the next row, but no
                call    text_bottom     ; further than the bottom row
                dec     a
                cp      l
                jr      c,inlin_eol_col
                inc     l
inlin_eol_col:
                inc     h
                ld      b,h
                ld      c,l
inlin_eol_next:
                call    inlin_next
                jr      nz,inlin_eol_lp
                ld      (CSRY),bc
                jr      inlin_cursor

; CTRL+F: to the start of the next word, over the whole screen. Words are
; runs of letters and digits.
inlin_wordr:
                call    text_bottom
                ld      e,a             ; the screen's last row
inlin_wordr_in:
                call    inlin_wordc
                jr      c,inlin_wordr_out
                call    inlin_next
                jr      nz,inlin_wordr_in
inlin_wordr_out:
                call    inlin_wordc
                jr      nc,inlin_cursor
                call    inlin_next
                jr      nz,inlin_wordr_out
                jr      inlin_cursor

; CTRL+B: to the start of the previous word, over the whole screen.
inlin_wordl:
                call    text_bottom
                ld      e,a
inlin_wordl_out:
                call    inlin_prev
                jr      z,inlin_cursor
                call    inlin_wordc
                jr      c,inlin_wordl_out
inlin_wordl_in:
                call    inlin_prev
                jr      z,inlin_cursor
                call    inlin_wordc
                jr      nc,inlin_wordl_in
                call    inlin_next      ; back onto the word's first letter
inlin_cursor:
                jp      inlin_esc

; Enter: the logical line under the cursor, rows D to E, goes into BUF.
inlin_enter:
                ld      a,','
                ld      (BUF-1),a       ; stops the trim below
                call    inlin_lastrow_cur   ; E := the line's last row (also in A)
                call    inlin_firstrow  ; D := its first row
                call    inlin_startpos
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
                xor     a               ; NC: ended with Enter
                ld      (bc),a
                ld      a,13            ; A = the Enter, flags kept
inlin_end:
                push    af
                ld      a,e
                call    inlin_linttb
                ld      (hl),$AF        ; the line ends on its last row
                ld      l,e
                ld      h,1
                ld      (CSRY),hl
                call    chput_ctrl_lf   ; to the next row; on row 24 it scrolls
                call    chput_restore_cursor
                ld      hl,BUF-1
                pop     af
                ret

; CTRL+STOP or CTRL+C: an empty line in BUF, C set; the screen stays as
; it is and the cursor goes to the row after the line, as with Enter.
inlin_break:
                call    inlin_lastrow_cur   ; E := the line's last row
                xor     a
                ld      (BUF),a
                scf
                jr      inlin_end

; Insert A at the cursor: the rest of the logical line moves one cell
; right. When the line's last cell is not free, the line first gets a new
; row: inserted below it, or on row 24 the screen scrolls up.
inlin_insert:
                push    af
                ld      hl,(CSRY)
                push    hl              ; the cursor
                call    inlin_lastrow_cur ; E := the line's last row
                ld      a,(LINLEN)
                ld      h,a
                ld      l,e
                ld      (CSRY),hl
                call    curs2hl
                call    rdvrm           ; the line's last cell
                cp      ' '
                jr      z,inlin_insert_shift
                call    text_bottom
                dec     a
                cp      e
                jr      c,inlin_insert_scroll
                inc     e               ; the line's new last row
                push    de
                ld      hl,CSRY
                inc     (hl)
                call    chput_esc_ll    ; a new row below the line
                jr      inlin_insert_link
inlin_insert_scroll:
                pop     hl              ; on the bottom row the screen
                dec     l               ; scrolls up, the cursor with it
                push    hl
                push    de
                call    chput_ctrl_home
                call    chput_esc_m
inlin_insert_link:
                pop     de
                ld      a,e
                dec     a
                call    inlin_linttb
                ld      (hl),0          ; the line continues on its new row
inlin_insert_shift:
                pop     hl
                ld      (CSRY),hl
                push    hl
                ld      d,' '           ; the cell at the cursor becomes free
inlin_insert_lp:
                call    curs2hl
                call    rdvrm
                ld      c,a
                ld      a,d
                call    wrtvrm
                ld      d,c
                call    inlin_next
                jr      nz,inlin_insert_lp
                pop     hl
                ld      (CSRY),hl
                pop     af
                jp      chput           ; the character, as typed

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

; A = a row (1-24) -> D = the first row of the logical line through it.
; Changes: AF, C, HL
inlin_firstrow:
                ld      d,a
                dec     a
                ret     z
                ld      c,a
                call    inlin_linttb
                ld      a,c
                jr      z,inlin_firstrow ; the row above continues on this one
                ret

; D = a line's first row -> CSRY, CSRX := where the line is read from:
; FSTPOS's column if input started on that row, else column 1.
; Changes: AF, HL
inlin_startpos:
                ld      hl,(FSTPOS)
                ld      a,l
                cp      d
                ld      l,d
                jr      z,inlin_startpos_col
                ld      h,1
inlin_startpos_col:
                ld      (CSRY),hl
                ret

; The cursor one cell back, over the whole screen.
; Output : Z if it was at row 1, column 1 (and stays there)
; Changes: AF, HL
inlin_prev:
                ld      hl,(CSRY)
                dec     h
                jr      nz,inlin_prev_set
                ld      a,l
                dec     a
                ret     z
                ld      l,a
                ld      a,(LINLEN)
                ld      h,a             ; NZ from the DEC A
inlin_prev_set:
                ld      (CSRY),hl
                ret

; C if the character at the cursor is not a letter or a digit.
; Changes: AF, HL
inlin_wordc:
                call    curs2hl
                call    rdvrm
                cp      '0'
                ret     c
                cp      '9'+1
                ccf
                ret     nc              ; a digit
                and     $DF             ; lower case to upper case
                cp      'A'
                ret     c
                cp      'Z'+1
                ccf
                ret

; A = a row (1-24), or the cursor's row from inlin_lastrow_cur -> E = the
; last row of the logical line through it, also in A.
; Changes: AF, HL
inlin_lastrow_cur:
                ld      a,(CSRY)        ; the cursor's row
inlin_lastrow:
                ld      e,a
                call    text_bottom
                dec     a
                cp      e
                ld      a,e
                ret     c               ; the bottom row
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
