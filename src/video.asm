; $Id: video.asm,v 1.29 2004/12/30 13:09:12 andete Exp $
; C-BIOS video routines
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004 Joost Yervante Damad.  All rights reserved.
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

                include "font.asm"

;--------------------------------
; $0041 DISSCR
; Function : inhibits the screen display
; Registers: AF, BC
disscr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                and     $BF
                ld      b,a
                ld      c,1
                call    wrtvdp
                ret

;--------------------------------
; $0044 ENASCR
; Function : displays the screen
; Registers: AF, BC
enascr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                or      $40
                ld      b,a
                ld      c,1
                call    wrtvdp
                ret

;--------------------------------
; 0047$ WRTVDP
; Function : write data in the VDP-register
; Input    : B  - data to write
;            C  - number of the register
; Output   : RG0SAV(F3DF)-RG7SAV(F3E6)
; Registers: AF, BC
wrtvdp:
                push    hl
                di
                ld      a,b
                out     (VDP_ADDR),a
                ld      a,c
                or      $80
                out     (VDP_ADDR),a
                ei
                ld      a,c
                and     $3F
                sub     8
                jr      nc,rg8_sav
                ld      a,b
                ld      b,0
                ld      hl,RG0SAV
                add     hl,bc
                ld      (hl),a
                pop     hl
                ret

rg8_sav:
                push    bc
                ld      c,a

                ld      a,b
                ld      b,0
                ld      hl,RG8SAV
                add     hl,bc
                ld      (hl),a
                pop     bc
                pop     hl
                ret

;--------------------------------
; $004A RDVRM
; Function : Reads the content of VRAM
; Input    : HL - address read
; Output   : A  - value which was read
; Registers: AF
rdvrm:
                call    setrd
                in      a,(VDP_DATA)
                ret


;--------------------------------
; $004D WRTVRM
; Function : Writes data in VRAM
; Input    : HL - address write
;            A  - value write
; Registers: AF
wrtvrm:
                push    af
                call    setwrt
                pop     af
                out     (VDP_DATA),a
                ret

;--------------------------------
; $0050 SETRD
; Function : Enable VDP to read
; Input    : HL - for VRAM-address
; Registers: AF
setrd:
                di
                ld      a,l
                out     (VDP_ADDR),a
                ld      a,h
                and     $3F
                out     (VDP_ADDR),a
                ei
                ret

;--------------------------------
; $0053 SETWRT
; Function : Enable VDP to write
; Input    : HL - Address
; Registers: AF
setwrt:
                di
                ld      a,l
                out     (VDP_ADDR),a
                ld      a,h
                and     $3F
                or      $40
                out     (VDP_ADDR),a
                ei
                ret

;--------------------------------
; $0056 FILVRM
; Function : fill VRAM with value
; Input    : A  - data byte
;            BC - length of the area to be written
;            HL - start address
; Registers: AF, BC
filvrm:
                push    af
                call    setwrt
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
filvrm_lp:
                out     (VDP_DATA),a
                djnz    filvrm_lp
                dec     c
                jr      nz,filvrm_lp
                ret

;--------------------------------
; $0059 LDIRMV
; Function : Block transfer from VRAM to memory
; Input    : BC - blocklength
;            DE - Start address of memory
;            HL - Start address of VRAM
; Registers: All
ldirmv:
                call    setrd
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
ldirmv_lp:
                inir
                dec     a
                jr      nz,ldirmv_lp
                ret

;--------------------------------
; $005C LDIRVM
; Function : Block transfer from memory to VRAM
; Input    : BC - blocklength
;            DE - Start address of memory
;            HL - Start address of VRAM
; Registers: All
ldirvm:
                ex      de,hl
                ld      a,(SCRMOD)
                cp      4
                jr      nc,ldirvm_new
                call    setwrt
                jr      ldirvm_cont
ldirvm_new:
                call    nsetwr
ldirvm_cont:
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
ldirvm_lp:
                otir
                dec     a
                jr      nz,ldirvm_lp
                ; Note: Without this, Quinpl shows glitches.
                ; TODO: Investigate why.
                ex      de,hl
                ret

;----------------------------------
; $005F CHGMOD   画面モードの変更
; Function : Switches to given screenmode
; Input    : A  - screen mode
; Registers: All
chgmod:
                ; Guard against non-existing screen mode.
                cp      9
                ret     nc
                ; Redirect to initialisation routine.
                ld      hl,chgmod_tbl
                jp      jump_table
chgmod_tbl:
                dw      initxt          ; SCREEN0
                dw      init32          ; SCREEN1
                dw      inigrp          ; SCREEN2
                dw      inimlt          ; SCREEN3
                dw      init_sc4        ; SCREEN4
                dw      init_sc5        ; SCREEN5
                dw      init_sc6        ; SCREEN6
                dw      init_sc7        ; SCREEN7
                dw      init_sc8        ; SCREEN8

;--------------------------------
; $0062 CHGCLR
; Function : Changes the screencolors
; Input    : Foregroundcolor in FORCLR
;            Backgroundcolor in BAKCLR
;            Bordercolor in BDRCLR
; Registers: All
chgclr:
                ld      a,(SCRMOD)
                cp      8
                jr      z,chgclr_sc8
                dec     a
                push    af
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
                and     $F0
                ld      l,a
                ld      a,(BDRCLR)
                or      l

                ld      b,a
                ld      c,7
                call    wrtvdp
                pop     af
                ret     nz

                ; SCREEN1
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
                and     $F0
                ld      hl,BAKCLR
                or      (hl)
                ld      hl,(T32COL)
                ld      bc,$0020
                push    af
                call    setwrt
cclr_lp:
                pop     af
                out     (VDP_DATA),a
                push    af
                dec     bc
                ld      a,b
                or      c
                jr      nz,cclr_lp
                pop     af
                ret

chgclr_sc8:
                ; SCREEN8
                ld      a,(BDRCLR)
                ld      b,a
                ld      c,7
                jp      wrtvdp

;--------------------------------
; $0069 CLRSPR
; Function : Initialises all sprites
; Input    : SCRMOD
; Registers: All
clrspr:
; Check screen mode.
                ld      a,(SCRMOD)
                or      a
                ret     z               ; no sprites in SCREEN0

; Clear sprite attribute table.
                call    clrspr_attr

; Clear sprite colour table.
                ld      a,c
                cp      209             ; sprite mode 1?
                jr      z,clrspr_col_skip
                ld      hl,(PATBAS)
                dec     h
                dec     h               ; HL = (PATBAS) - 512
                ld      bc,32 * 16
                ld      a,(FORCLR)
                and     $0F
                call    bigfil
clrspr_col_skip:

; Clear sprite pattern generator table.
                ld      hl,(PATBAS)
                ld      bc,256 * 8
                xor     a
                call    bigfil

                ret

;--------------------------------
; Clear sprite attribute table.
clrspr_attr:
                ld      a,(SCRMOD)
                cp      4
                jr      c,clrspr_attr_spritemode1

; Note: This label is called directly by external routines.
clrspr_attr_spritemode2:
                ld      e,217           ; Y coordinate
                jr      clrspr_attr_spritemode_start

; Note: This label is called directly by external routines.
clrspr_attr_spritemode1:
                ld      e,209           ; Y coordinate

clrspr_attr_spritemode_start:
                ld      hl,(ATRBAS)
                ; TODO: On MSX2, call nsetwr instead.
                ;       Before that is possible, we should split MSX1 and MSX2
                ;       into separate ROMs, because nsetwr writes to R#14,
                ;       which does not exist and therefore wraps to R#6.
                call    setwrt
                ld      a,(FORCLR)
                ld      d,a
                ld      bc,$2000        ; B = 32 = counter, C = pattern index
clrspr_attr_lp:
                ld      a,e
                out     (VDP_DATA),a    ; Y coordinate
                ld      a,0
                out     (VDP_DATA),a    ; X coordinate
                ld      a,c
                out     (VDP_DATA),a    ; pattern number
                inc     c
                call    gspsiz
                jr      nc,clrspr_attr_8
                inc     c
                inc     c
                inc     c
clrspr_attr_8:
                ld      a,d
                out     (VDP_DATA),a    ; color
                djnz    clrspr_attr_lp
                ret

;--------------------------------
; $006C INITXT
; Function : Switch to SCREEN 0
; Input    : TXTNAM, TXTCGP
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
initxt:
                ld      a,$00
                ld      (SCRMOD),a
                ld      (OLDSCR),a

                call    clr_text40
                call    chgclr

                ld      hl,B_Font
                ld      de,(TXTCGP)
                ld      bc,$0800
                call    ldirvm          ; init Font

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$0800
                ld      (CGPBAS),hl

                ld      a,(LINL40)
                ld      (LINLEN),a

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                call    settxt
                call    cls_screen0
                jp      enascr

;--------------------------------
; $006F INIT32
; Function : Switches to SCREEN 1 (text screen with 32*24 characters)
; Input    : T32NAM, T32CGP, T32COL, T32ATR, T32PAT
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
init32:
                ld      a,$01           ; SCREEN1
                ld      (SCRMOD),a
                ld      (OLDSCR),a

                call    clr_text32
                call    chgclr

                ld      hl,(T32NAM)
                ld      (NAMBAS),hl
                ld      hl,(T32CGP)
                ld      (CGPBAS),hl
                ld      hl,(T32PAT)
                ld      (PATBAS),hl
                ld      hl,(T32ATR)
                ld      (ATRBAS),hl

                ld      hl,B_Font
                ld      de,(T32CGP)
                ld      bc,$0800
                call    ldirvm          ; init Font

                ld      a,(LINL32)
                ld      (LINLEN),a

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                call    sett32
                call    clrspr_attr_spritemode1
                call    cls_screen1
                jp      enascr

;--------------------------------
; $0072 INIGRP
; Function : Switches to SCREEN 2 (high resolution screen with 256*192 pixels)
; Input    : GRPNAM, GRPCGP, GRPCOL, GRPATR, GRPPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inigrp:
                ld      a,$02
                ld      (SCRMOD),a

                call    chgclr

                ld      hl,(GRPNAM)
                ld      (NAMBAS),hl
                call    setwrt
                ld      b,3
                xor     a
inigrp_lp:
                out     (VDP_DATA),a
                inc     a
                jr      nz,inigrp_lp
                djnz    inigrp_lp

                ld      hl,(GRPCGP)
                ld      (CGPBAS),hl

                ld      hl,(GRPATR)
                ld      (ATRBAS),hl

                ld      hl,(GRPPAT)
                ld      (PATBAS),hl

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                call    setgrp
                call    clrspr_attr_spritemode1
                call    cls_screen2
                jp      enascr

;------------------------------
; $0075 INIMLT
; Function : Switches to SCREEN 3 (multi-color screen 64*48 pixels)
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inimlt:
                ld      hl,inimlt_text
                jp      print_debug
inimlt_text:    db      "SCREEN3",0

;------------------------------
; $0078 SETTXT
; Function : Switches to VDP in SCREEN 0 mode
; Input    : TXTNAM, TXTCGP
; Registers: All
; TODO:      Make use of the inputs; see setgrp.
settxt:
                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                or      $10
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ld      bc,$0104        ; R#4 PatGenTBLaddr=$0800
                call    wrtvdp          ; write VDP R#4

                ld      bc,$0002        ; R#2 PatNamTBLaddr=$0000
                call    wrtvdp          ; write VDP R#2
                ret

;------------------------------
; $007B SETT32
; Function : Schakelt VDP in SCREEN 1 modus
; Input    : T32NAM, T32CGP, T32COL, T32ATR, T32PAT
; Registers: All
; TODO:      Try to reduce code duplication; see setgrp.
sett32:
                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ld      hl,(T32NAM)
                ld      a,h
                rrca
                rrca
                and     $3F
                ld      b,a
                ld      c,2
                call    wrtvdp          ; write VDP R#2

                ld      hl,(T32COL)
                ld      b,2
tcol_lp:
                xor     a
                rl      l
                rl      h
                djnz    tcol_lp
                ld      b,h
                ld      c,3
                call    wrtvdp          ; write VDP R#3

                ld      hl,(T32CGP)
                ld      a,h
                rrca
                rrca
                rrca
                and     $1F
                ld      b,a
                ld      c,4
                call    wrtvdp          ; write VDP R#4

                ld      hl,(T32ATR)
                rl      l
                rl      h
                ld      b,h
                ld      c,5
                call    wrtvdp          ; write VDP R#5

                ld      hl,(T32PAT)
                ld      a,h
                rrca
                rrca
                rrca
                and     $1F
                ld      b,a
                ld      c,6
                call    wrtvdp          ; write VDP R#6
                ret

;------------------------------
; $007E SETGRP
; Function : Switches VDP to SCREEN 2 mode
; Input:     GRPNAM, GRPCGP, GRPCOL, GRPATR, GRPPAT
; Registers: All
setgrp:
                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $02             ; M3 = 1
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ld      hl,GRPNAM
                ld      de,$7F03
                push    ix
                ld      ix,shift_tbl
                ld      c,2             ; C = VDP R#

                xor     a               ; data=0
                call    adr_sft         ; R#2: GRPNAM
                ld      a,d             ; data=D
                call    adr_sft         ; R#3: GRPCOL
                ld      a,e             ; data=E
                call    adr_sft         ; R#4: GRPCGP
                xor     a               ; data=0
                call    adr_sft         ; R#5: GRPATR
                xor     a               ; data=0
                call    adr_sft         ; R#6: GRPPAT
                pop     ix
                ret

shift_tbl:
                db      $06,$0A,$05,$09,$05

; HL = table address
adr_sft:
                push    hl
                push    af
                ld      b,(ix+0)
                inc     ix

                ; HL <- (HL)
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a
sft_lp:
                add     hl,hl
                adc     a,a
                djnz    sft_lp
                ld      h,a             ; H = シフトしたHLの上位。
                pop     af

                or      h
                ld      b,a

                call    wrtvdp
                pop     hl
                inc     hl
                inc     hl
                inc     c
                ret

;------------------------------
; $0081 SETMLT
; Function : Switches VDP to SCREEN 3 mode
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Registers: All
setmlt:
                ld      hl,setmlt_text
                jp      print_debug
setmlt_text:    db      "SETMLT",0

;------------------------------
; $0084 CALPAT
; Returns the address of a sprite pattern in the sprite pattern table.
; Input:     A  = pattern number
; Output:    HL = address
; Changes:   AF, DE, HL
calpat:
                ld      h,0
                ld      l,a
                add     hl,hl
                add     hl,hl
                add     hl,hl
                call    gspsiz
                jr      nc,calpat_8
                add     hl,hl
                add     hl,hl
calpat_8:       ld      de,(PATBAS)
                add     hl,de
                ret

;------------------------------
; $0087 CALATR
; Returns the address of a sprite in the sprite attribute table.
; Input:     A  = sprite number
; Output:    HL = address
; Changes:   AF, DE, HL
calatr:
                add     a,a
                add     a,a
                ld      hl,(ATRBAS)
                ld      d,0
                ld      e,a
                add     hl,de
                ret

;------------------------------
; $008A GSPSIZ
; Returns the current sprite-size in bytes.
; Output:    A  = sprite-size in bytes
;            CF = set when size is 16x16, otherwise reset
; Changes:   AF
gspsiz:
                ld      a,(RG1SAV)
                rrca
                rrca
                ld      a,8
                ret     nc
                ld      a,32
                ret

;------------------------------
; $008D GRPPRT
; Function:  Places a character on graphic screen
; Input:     A  - Character
;            ATRBYT for attribute
;            LOGOPR for logical operator
; NOTE: this implementation is still a stub!
grpprt:
                push    hl
                push    af
                ld      hl,grpprt_text
                call    print_debug
                pop     af
                pop     hl
                ret
grpprt_text:    db      "GRPPRT",0

;--------------------------------
; 0165h CHKNEW
; Is the current screen mode a bitmap mode?
; Output:  Carry flag set if current screen mode is SCREEN 5 or higher.
; Changes: AF
chknew:
                ld      a,(SCRMOD)
                cp      5
                ret

;--------------------------------
; 016Bh BIGFIL
; Fills VRAM with a fixed value.
; Like FILVRM, but supports 128K of VRAM.
; Input:   HL = VRAM start address
;    (ACPAGE) = active VRAM page
;          BC = number of bytes to fill
;          A  = value to fill VRAM with
; Changes: AF, BC
bigfil:
                push    af
                call    nsetwr
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
bigfil_lp:
                out     (VDP_DATA),a
                djnz    bigfil_lp
                dec     c
                jr      nz,bigfil_lp
                ret

;--------------------------------
; 016Eh NSETRD
; Set VRAM address and read mode.
; Like SETRD, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Changes: AF
; Note: If an odd-numbered 32K page is active and HL >= $8000,
;       16-bit wrap around occurs.
nsetrd:
                ld      a,(ACPAGE)
                or      a
                jr      z,nsetrd_32k

                ld      a,(SCRMOD)
                cp      5
                jp      c,setrd
                cp      7
                ld      a,(ACPAGE)
                jr      c,nsetrd_32k    ; SCREEN5/6 -> 32K pages
                add     a,a             ; SCREEN7/8 -> 64K pages
nsetrd_32k:     push    hl
                and     $03             ; A  =  0   0   0   0   0   0   P1  P0
                rrca
                ld      l,a             ; L  =  P0  0   0   0   0   0   0   P1
                and     $80             ; A  =  P0  0   0   0   0   0   0   0
                xor     h               ; A  = A15 A14 A13 A12 A11 A10  A9  A8
                rla                     ; CF = A15
                rl      l               ; L  =  0   0   0   0   0   0   P1 A15
                rla                     ; CF = A14
                ld      a,l
                rla                     ; A  =  0   0   0   0   0   P1 A15 A14
                di
                out     (VDP_ADDR),a    ; A16..A14
                ld      a,$8E
                out     (VDP_ADDR),a    ; R#14
                pop     hl
                ld      a,l
                out     (VDP_ADDR),a    ; A7..A0
                ld      a,h
                and     $3F
                out     (VDP_ADDR),a    ; A13..A8
                ei
                ret

;--------------------------------
; 0171h NSETWR
; Set VRAM address and write mode.
; Like SETWRT, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Changes: AF
; Note: If an odd-numbered 32K page is active and HL >= $8000,
;       16-bit wrap around occurs.
nsetwr:
                ld      a,(ACPAGE)
                or      a
                jr      z,nsetwr_32k

                ld      a,(SCRMOD)
                cp      5
                jp      c,setwrt
                cp      7
                ld      a,(ACPAGE)
                jr      c,nsetwr_32k    ; SCREEN5/6 -> 32K pages
                add     a,a             ; SCREEN7/8 -> 64K pages
nsetwr_32k:     push    hl
                and     $03             ; A  =  0   0   0   0   0   0   P1  P0
                rrca
                ld      l,a             ; L  =  P0  0   0   0   0   0   0   P1
                and     $80             ; A  =  P0  0   0   0   0   0   0   0
                xor     h               ; A  = A15 A14 A13 A12 A11 A10  A9  A8
                rla                     ; CF = A15
                rl      l               ; L  =  0   0   0   0   0   0   P1 A15
                rla                     ; CF = A14
                ld      a,l
                rla                     ; A  =  0   0   0   0   0   P1 A15 A14
                di
                out     (VDP_ADDR),a    ; A16..A14
                ld      a,$8E
                out     (VDP_ADDR),a    ; R#14
                pop     hl
                ld      a,l
                out     (VDP_ADDR),a    ; A7..A0
                ld      a,h
                and     $3F
                or      $40
                out     (VDP_ADDR),a    ; A13..A8
                ei
                ret

;--------------------------------
; 0174h NRDVRM
; Read a byte from VRAM.
; Leaves the VRAM in read mode at the byte after the one read.
; Like RDVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Output:   A = the byte read
nrdvrm:
                call    nsetrd
                in      a,(VDP_DATA)
                ret

;--------------------------------
; 0177h NWRVRM
; Write a byte to VRAM.
; Leaves the VRAM in write mode at the byte after the one written.
; Like WRTVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
;           A = the byte to write
nwrvrm:
                push    af
                call    nsetwr
                pop     af
                out     (VDP_DATA),a
                ret


; VDP routines which only exist in sub rom, but are useful for C-BIOS internal
; use as well:

;-------------------------------------
; $0131(sub) VDPSTA
; Read VDP status register.
; Input:   A = number of status register
; Output:  A = value read
; Changes: F
vdpsta:
                di
                ; Select desired status register.
                out     (VDP_ADDR),a
                ld      a,$80 + 15
                out     (VDP_ADDR),a
                ; Read status register.
                in      a,(VDP_STAT)
                push    af
                ; Restore status register 0.
                xor     a
                out     (VDP_ADDR),a
                ld      a,$80 + 15
                out     (VDP_ADDR),a
                ei
                pop     af
                ret


;--------------------
;VDPルーチンの初期化
;--------------------

init_vdp:
                in      a,(VDP_STAT)    ; ラッチの初期化

                ld      bc,$0000        ; R#0
                call    wrtvdp
                ld      bc,$6001        ; R#1
                call    wrtvdp
                ld      bc,$0002        ; R#2
                call    wrtvdp
                ld      bc,$8003        ; R#3
                call    wrtvdp
                ld      bc,$0104        ; R#4
                call    wrtvdp

                call    clr_text32

                ld      a ,$00
                ld      hl,$0800
                ld      bc,$0800
                call    filvrm

                ; for screen 1 color table
                ld      a ,$F5
                ld      hl,$2000
                ld      bc,$0020
                call    filvrm


; PatGenTbl
;        76543210 76543210
;        00000100 00000000
;             04h      00h

                ld      bc,$F507        ; R#7
                call    wrtvdp

                in      a,(VDP_STAT)    ;　ラッチの初期化

                ld      hl,B_Font

                ld      de,$0800
                ld      bc,$0800
                call    ldirvm

                ret

; TODO: Is it safe to enable this on MSX1 machines?
;       Or can we autodetect the VDP?
                ; Write colour burst settings.
                ld      bc,$0014        ; B = $00, C = 20
                call    wrtvdp          ; VDP R#20
                ld      bc,$3B15        ; B = $3B, C = 21
                call    wrtvdp          ; VDP R#21
                ld      bc,$0516        ; B = $05, C = 22
                call    wrtvdp          ; VDP R#22

                ret

;------------------------
; 画面表示されているテキストをクリアする。
clr_text40:
                xor     a
                ld      bc,$0400
                ld      hl,(TXTNAM)
                call    filvrm
                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a
                ret

clr_text32:
                xor     a
                ld      bc,$0300
                ld      hl,(T32NAM)
                call    filvrm
                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a
                ret

;------------------------------
; Initialise SCREEN4 (graphic 3).
init_sc4:
; TODO: Try to reduce code duplication from inimlt.
                ld      a,$04
                ld      (SCRMOD),a

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $04             ; M4 = 1
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ; TODO: This should be done for SCREEN2 as well,
                ;       but on MSX1 this reg doesn't exist.
                ld      bc,$000E        ; B = $00, C = 14
                call    wrtvdp          ; VDP R#14

                ld      hl,(GRPNAM)
                ld      (NAMBAS),hl
                call    setwrt
                ld      b,3
                xor     a
init_sc4_lp:
                out     (VDP_DATA),a
                inc     a
                jr      nz,init_sc4_lp
                djnz    init_sc4_lp

                ld      hl,(GRPCGP)
                ld      (CGPBAS),hl

                ld      hl,(GRPATR)
                ld      (ATRBAS),hl

                ld      hl,(GRPPAT)
                ld      (PATBAS),hl

                ld      hl,GRPNAM
                ld      de,$7F03
                push    ix
                ld      ix,shift_tbl
                ld      c,2             ; C = VDP R#

                xor     a               ; data=0
                call    adr_sft         ; R#2: GRPNAM
                ld      a,d             ; data=D
                call    adr_sft         ; R#3: GRPCOL
                ld      a,e             ; data=E
                call    adr_sft         ; R#4: GRPCGP
                ld      hl,sc4atr
                ld      a,$03           ; data=xxxxxx11
                call    adr_sft         ; R#5: Sprite attribute table.
                ld      hl,GRPPAT
                xor     a               ; data=0
                call    adr_sft         ; R#6: GRPPAT
                pop     ix

                ld      a,(RG8SAV+9-8)
                and     $7F             ; 192 lines mode
                ld      b,a
                ld      c,9
                call    wrtvdp          ; VDP R#9

                ; TODO: This should be done for SCREEN2 as well,
                ;       but on MSX1 these regs don't exist.
                ld      bc,$000A        ; B = $00, C = 10
                call    wrtvdp          ; VDP R#10
                ld      bc,$000B        ; B = $00, C = 11
                call    wrtvdp          ; VDP R#11

                call    clrspr_attr_spritemode1
                ld      bc,$000E        ; B = $00, C = 14
                call    wrtvdp          ; VDP R#14

                call    cls_screen2
                jp      enascr

sc4atr:         dw      $1E00

;------------------------------
;screen 5の初期化.
init_sc5:
                ld      a,$05
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; latch reset

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $06             ; M4,M3 = 1
                ld      b,a             ; B = R#0 data
                ld      c,0
                call    wrtvdp          ; VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$7800
                ld      (PATBAS),hl
                ld      hl,$7600
                ld      (ATRBAS),hl

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                ld      bc,$1F02        ; B = $1F, C = 2
                call    wrtvdp          ; VDP R#2

                ; Set sprite attribute table base.
                ld      bc,$EF05        ; B = $EF, C = 5
                call    wrtvdp          ; VDP R#5
                ld      bc,$000B        ; B = $00, C = 11
                call    wrtvdp          ; VDP R#11

                ; Set sprite pattern table base.
                ld      bc,$0F06        ; B = $0F, C = 6
                call    wrtvdp          ; VDP R#6

                ld      a,(RG8SAV+9-8)
                or      $80             ; 212 lines mode
                ld      b,a
                ld      c,9
                call    wrtvdp          ; VDP R#9

                ; Turn off page blinking.
                ld      bc,$000D
                call    wrtvdp          ; VDP R#13

                call    clrspr_attr_spritemode2
                call    cls_screen5
                jp      enascr

;------------------------------
; Initialise SCREEN6 (graphic 5).
init_sc6:
                ld      a,$06
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; latch reset

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $08             ; M5 = 1
                ld      b,a             ; B = R#0 data
                ld      c,0
                call    wrtvdp          ; VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$7800
                ld      (PATBAS),hl
                ld      hl,$7600
                ld      (ATRBAS),hl

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                ld      bc,$1F02        ; B = $1F, C = 2
                call    wrtvdp          ; VDP R#2

                ; Set sprite attribute table base.
                ld      bc,$EF05        ; B = $EF, C = 5
                call    wrtvdp          ; VDP R#5
                ld      bc,$000B        ; B = $00, C = 11
                call    wrtvdp          ; VDP R#11

                ; Set sprite pattern table base.
                ld      bc,$0F06        ; B = $0F, C = 6
                call    wrtvdp          ; VDP R#6

                ld      a,(RG8SAV+9-8)
                or      $80             ; 212 lines mode
                ld      b,a
                ld      c,9
                call    wrtvdp          ; VDP R#9

                ; Turn off page blinking.
                ld      bc,$000D
                call    wrtvdp          ; VDP R#13

                call    clrspr_attr_spritemode2
                call    cls_screen6
                jp      enascr

;------------------------------
; Initialise SCREEN7 (graphic 6).
init_sc7:
                ld      a,$07
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; reset latch 

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $0A             ; M5,M3 = 1
                ld      b,a             ; B = R#0 data
                ld      c,0
                call    wrtvdp          ; VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a             ; B = R#1 data
                inc     c
                call    wrtvdp          ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$F000
                ld      (PATBAS),hl
                ld      hl,$FA00
                ld      (ATRBAS),hl

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                ld      bc,$1F02        ; B = $1F, C = 2
                call    wrtvdp          ; write VDP R#2

                ; Set sprite attribute table base.
                ld      bc,$EF05        ; B = $EF, C = 5
                call    wrtvdp          ; write VDP R#5
                ld      bc,$000B        ; B = $00, C = 11
                call    wrtvdp          ; write VDP R#11

                ; Set sprite pattern table base.
                ld      bc,$0F06        ; B = $0F, C = 6
                call    wrtvdp          ; write VDP R#6

                ld      a,(RG8SAV+9-8)
                or      $80             ; 212 lines mode
                ld      b,a
                ld      c,9
                call    wrtvdp          ; VDP R#9

                ; Turn off page blinking.
                ld      bc,$000D
                call    wrtvdp          ; VDP R#13

                call    clrspr_attr_spritemode2
                call    cls_screen7
                jp      enascr

;------------------------------
; Initialise SCREEN8 (graphic 7).
init_sc8:
                ld      a,$08
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; reset latch 

                call    chgclr

                ld      a,(RG0SAV)
                or      $0E             ; M5,M4,M3 = 1
                ld      b,a             ; B = R#0 data
                ld      c,0
                call    wrtvdp          ; VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a             ; B = R#1 data
                inc     c
                call    wrtvdp          ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$F000
                ld      (PATBAS),hl
                ld      hl,$FA00
                ld      (ATRBAS),hl

                xor     a
                ld      (DPPAGE),a
                ld      (ACPAGE),a
                ld      bc,$1F02        ; B = $1F, C = 2
                call    wrtvdp          ; write VDP R#2

                ; Set sprite attribute table base.
                ld      bc,$EF05        ; B = $EF, C = 5
                call    wrtvdp          ; write VDP R#5
                ld      bc,$000B        ; B = $00, C = 11
                call    wrtvdp          ; write VDP R#11

                ; Set sprite pattern table base.
                ld      bc,$0F06        ; B = $0F, C = 6
                call    wrtvdp          ; write VDP R#6

                ld      a,(RG8SAV+9-8)
                or      $80             ; 212 lines mode
                ld      b,a
                ld      c,9
                call    wrtvdp          ; VDP R#9

                ; Turn off page blinking.
                ld      bc,$000D
                call    wrtvdp          ; VDP R#13

                call    clrspr_attr_spritemode2
                call    cls_screen8
                jp      enascr

;--------------------------------
; $00C3 CLS
; Function : clear the screen
; Input: BAKCLR, Z-Flag has to be low
; Registers: AF, BC, DE
;TODO: add SCREEN 3 CLS
;TODO: add optional borders to text based screens
cls:
                ret     nz
                push    hl
                ld      a,(SCRMOD)
                ld      c,a
                ld      b,0
                ld      hl,cls_table
                add     hl,bc
                add     hl,bc
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a
                jp      (hl)
cls_table:
                dw      cls_screen0
                dw      cls_screen1
                dw      cls_screen2
                dw      cls_screen3
                dw      cls_screen2             ; SCREEN 4 = SCREEN 2
                dw      cls_screen5
                dw      cls_screen6
                dw      cls_screen7
                dw      cls_screen8

cls_screen0:
                ld      a,(LINLEN)
                cp      40
                ld      bc,40*24
                jr      c,cls_text
                ld      bc,80*24
                jr      cls_text

cls_screen1:
                ld      bc,32*24

cls_text:
                ld      hl,(NAMBAS)
                ld      a,$20
                call    filvrm
                pop     hl
                ret

cls_screen2:
                xor     a
                ld      bc,$1800
                ld      hl,(CGPBAS)
                ld      l,a
                push    bc
                call    filvrm
                pop     bc

                ld      a,(BAKCLR)
                ld      hl,$2000
                call    filvrm
                pop     hl
                ret

cls_screen3:
                pop     hl
                ret

cls_screen5:
                ld      a,(BAKCLR)
                and     15
                ld      b,a
                rlca
                rlca
                rlca
                rlca
                or      b

                ld      hl,256
                jr      cls_bitmap

cls_screen6:
                ld      a,(BAKCLR)
                and     3
                ld      b,a
                rlca
                rlca
                or      b
                rlca
                rlca
                rlca
                rlca
                or      b

                ld      hl,512
                jr      cls_bitmap

cls_screen7:
                ld      a,(BAKCLR)
                and     15
                ld      b,a
                rlca
                rlca
                rlca
                rlca
                or      b

                ld      hl,512
                jr      cls_bitmap

cls_screen8:
                ld      a,(BAKCLR)
                ld      hl,256

cls_bitmap:
                ld      (CDUMMY),a
                ld      (NX),hl
                ld      hl,212
                ld      (NY),hl
                ld      hl,0
                ld      (DX),hl
                ld      a,(ACPAGE)
                ld      h,a
                ld      l,0
                ld      (DY),hl
                ld      a,$C0
                ld      (L_OP),a
cls_bitmap_ce:
                ld      a,2
                call    vdpsta
                bit     0,a
                jr      nz,cls_bitmap_ce

                di
                ld      a,32
                out     (VDP_ADDR),a
                ld      a,128+ 17
                out     (VDP_ADDR),a

                ld      bc,15 *256+ VDP_REGS
                ld      hl,SX
                otir
                ei
                pop     hl
                ret

; $0105 GETPAT
; Function : Returns current pattern of a character
; Input    : A  - ASCII code of character
; Output   : Pattern in PATWRK starting from address #FC40
; Registers: All
; Remark   : Same as routine in MSX1-BIOS, but there it doesn't exist as
;            a BIOS-call
;NOTE: this implementation is still a stub!
getpat:
                push    hl
                push    af
                ld      hl,getpat_text
                call    print_debug
                pop     af
                pop     hl
                ret
getpat_text:    db      "GETPAT",0


; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
