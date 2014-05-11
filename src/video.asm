; $Id: video.asm,v 1.7 2004/12/18 18:50:31 mthuurne Exp $
; C-BIOS video routines
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Albert Beevendorp.  All rights reserved.
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
;0041h DISSCR
disscr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                and     $BF
                ld      b,a
                ld      c,1
                call    wrt_vdp
                ret

;--------------------------------
;0044h ENASCR
enascr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                or      $40
                ld      b,a
                ld      c,1
                call    wrt_vdp
                ret

;--------------------------------
wrt_vdp:
; 0047h WRTVDP
;in:B = VDP �f�[�^ , C = ���W�X�^�ԍ�
; dest af,b
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
;004Ah VRAM�ǂݏo��
rd_vrm:
                call    vdp_setrd
                in      a,(VDP_DATA)
                ret


;--------------------------------
;004Dh VRAM��������
wrt_vrm:
                push    af
                call    vdp_setwrt
                pop     af
                out     (VDP_DATA),a
                ret

;--------------------------------
; 0050h SETRD
vdp_setrd:
                di
                ld      a,l
                out     (VDP_ADDR),a
                ld      a,h
                and     $3F
                out     (VDP_ADDR),a
                ei
                ret

;--------------------------------
; 0053h SETWRT
; VRAM�A�h���X�̐ݒ�
vdp_setwrt:
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
;0056h fill VRAM
;HL = VRAM �A�h���X, BC = ���� , A = �f�[�^
vdp_fillmem:
                push    af
                call    vdp_setwrt
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
vdp_fillmem_lp:
                out     (VDP_DATA),a
                djnz    vdp_fillmem_lp
                dec     c
                jr      nz,vdp_fillmem_lp
                ret

;--------------------------------
;0059h VRAM -> Memory
;in de bc
;dest. af de bc
vdp_ldirmv:
                call    vdp_setrd
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
vdp_ldirmv_lp:
                inir
                dec     a
                jr      nz,vdp_ldirmv_lp
                ret

;--------------------------------
;005Ch Memory -> VRAM
;in hl de bc
;dest. af de bc
vdp_data_rep:
                ex      de,hl
                ld      a,(SCRMOD)
                cp      4
                jr      nc,vdp_data_rep_new
                call    vdp_setwrt
                jr      vdp_data_rep_cont
vdp_data_rep_new:
                call    nsetwr
vdp_data_rep_cont:
                ex      de,hl
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                inc     a
                ld      c,VDP_DATA
vdp_data_rep_lp:
                otir
                dec     a
                jr      nz,vdp_data_rep_lp
                ; Note: Without this, Quinpl shows glitches.
                ; TODO: Investigate why.
                ex      de,hl
                ret

;----------------------------------
;005Fh CHGMOD   ��ʃ��[�h�̕ύX
chgmod:
                or      a
                jp      z,init_txt      ; screen 0
                dec     a
                jp      z,init_txt32    ; screen 1
                dec     a
                jp      z,init_grp      ; screen 2
                dec     a
;                jp     z,init_mlt       ; screen 3
                dec     a
;                jp     z,init_sc4       ; screen 4
                dec     a
                jp      z,init_sc5      ; screen 5
                dec     a
;                jp      z,init_sc6      ; screen 6
                dec     a
                jp      z,init_sc7      ; screen 7
                dec     a
;                jp      z,init_sc8      ; screen 8
                ret

;--------------------------------
;0062h CHGCLR
;in = ����
chgclr:
                ld      a,(SCRMOD)
                dec     a
                push    af
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
		and	$F0
                ld      l,a
                ld      a,(BDRCLR)
                or      l

                ld      b,a
                ld      c,7
                call    wrt_vdp
                pop     af
                ret     nz

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
                call    vdp_setwrt
cclr_lp: pop     af
                out     (VDP_DATA),a
                push    af
                dec     bc
                ld      a,b
                or      c
                jr      nz,cclr_lp
                pop     af
                ret

;--------------------------------
;0069h CLRSPR
clrspr:
; Check screen mode.
                ld      a,(SCRMOD)
                or      a
                ret     z               ; no sprites in SCREEN0
                cp      4
                jr      c,clrspr_spritemode1

; Note: This label is called directly by external routines.
clrspr_spritemode2:                     
                ld      c,217           ; Y coordinate
                jr      clrspr_spritemode_start

; Note: This label is called directly by external routines.
clrspr_spritemode1:
                ld      c,209           ; Y coordinate

clrspr_spritemode_start:
; Initialise sprite attribute table.
                ld      hl,(ATRBAS)
                call    nsetwr
                ld      b,32
clrspr_attr_lp:
                ld      a,c             ; Y coordinate
                out     (VDP_DATA),a
                ld      a,0
                out     (VDP_DATA),a    ; X coordinate
                ld      a,32
                sub     b
                out     (VDP_DATA),a    ; pattern number
                ld      a,(FORCLR)
                and     $0F
                out     (VDP_DATA),a    ; colour
                djnz    clrspr_attr_lp

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
;006Ch INITXT
init_txt:
                ld      a,$00
                ld      (SCRMOD),a

                call    clr_text40

                in      a,(VDP_STAT)    ; reset Latch
                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001

                ld      b,a             ; B = R#0 data
                ld      c,0

                call    wrt_vdp         ; write VDP R#0


                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                or      $10

                ld      b,a             ; B = R#1 data
                inc     c

                call    wrt_vdp         ; write VDP R#1

                ld      bc,$0104        ; R#4 PatGenTBLaddr=$0800
                call    wrt_vdp         ; write VDP R#4

                ld      bc,$0002        ; R#2 PatNamTBLaddr=$0000
                call    wrt_vdp         ; write VDP R#2

                in      a,(VDP_STAT)    ; reset Latch

                ld      hl,B_Font
                ld      de,(TXTCGP)
                ld      bc,$0800
                call    vdp_data_rep    ; init Font

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      hl,$0800
                ld      (CGPBAS),hl

                ld      a,(LINL40)
                ld      (LINLEN),a

                ret

;--------------------------------
;006Fh INIT32
init_txt32:
                ld      a,$01           ; SCREEN1
                ld      (SCRMOD),a

                call    clr_text32

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001

                ld      b,a             ; B = R#0 data
                ld      c,0
                call    wrt_vdp         ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111

                ld      b,a             ; B = R#1 data
                inc     c
                call    wrt_vdp         ; write VDP R#1

                ld      hl,(T32NAM)
                ld      (NAMBAS),hl
                ld      hl,(T32CGP)
                ld      (CGPBAS),hl
                ld      hl,(T32PAT)
                ld      (PATBAS),hl
                ld      hl,(T32ATR)
                ld      (ATRBAS),hl

                ld      hl,(T32NAM)
                ld      a,h
                rrca
                rrca
                and     $3F

                ld      b,a
                ld      c,2
                call    wrt_vdp         ; write VDP R#2


                ld      hl,(T32COL)
                ld      b,2
tcol_lp:
                xor     a
                rl      l
                rl      h
                djnz    tcol_lp
                ld      a,h

                ld      b,a
                ld      c,3
                call    wrt_vdp         ; write VDP R#3

                ld      hl,(T32CGP)
                ld      a,h
                rrca
                rrca
                rrca
                and     $1F

                ld      b,a
                ld      c,4
                call    wrt_vdp         ; write VDP R#4

                ld      hl,(T32ATR)
                rl      l
                rl      h
                ld      a,h

                ld      b,a
                ld      c,5
                call    wrt_vdp         ; write VDP R#5

                ld      hl,(T32PAT)
                ld      a,h
                rrca
                rrca
                rrca
                and     $1F
                ld      b,a
                ld      c,6
                call    wrt_vdp         ; write VDP R#6

                in      a,(VDP_STAT)    ; reset Latch

                ld      hl,B_Font
                ld      de,(T32CGP)
                ld      bc,$0800
                call    vdp_data_rep    ; init Font

                call    clrspr_spritemode1

                ld      a,(LINL32)
                ld      (LINLEN),a

                ret

;--------------------------------
;0072h INIGRP
init_grp:
                ld      a,$02
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; reset Latch

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      2               ; M3 = 1

                ld      b,a             ; B = R#0 data
                ld      c,0

                call    wrt_vdp         ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111

                ld      b,a             ; B = R#1 data
                inc     c

                call    wrt_vdp         ; write VDP R#1

                ld      hl,(GRPNAM)
                call    vdp_setwrt
                ld      b,3
                xor     a
ig_loop:
                out     (VDP_DATA),a
                inc     a
                jr      nz,ig_loop
                djnz    ig_loop

                ld      hl,(GRPATR)
                ld      (ATRBAS),hl

                ld      hl,(GRPPAT)
                ld      (PATBAS),hl

                ld      hl,GRPNAM
                ld      de,$7F03
                call    set_grp

                call    clrspr_spritemode1
                call    enascr
                ret

; HL = �e�[�u���A�h���X
; B  = DATA , C = VDP R#
; DE = VDPDATA
set_grp:
                push    ix
                ld      ix,shift_tbl
                ld      c,2             ; C = VDP R#

                xor     a               ; data=0
                call    adr_sft         ; R#2
                ld      a,d             ; data=D
                call    adr_sft         ; R#3
                ld      a,e             ; data=E
                call    adr_sft         ; R#4
                xor     a               ; data=0
                call    adr_sft         ; R#5
                xor     a               ; data=0
                call    adr_sft
                pop     ix
                ret

shift_tbl:
                db      $06,$0A,$05,$09,$05

;
;HL = �e�[�u���A�h���X
;
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
                ld      h,a             ; H = �V�t�g����HL�̏�ʁB
                pop     af

                or      h
                ld      b,a

                call    wrt_vdp
                pop     hl
                inc     hl
                inc     hl
                inc     c
                ret

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
                ld      a,(SCRMOD)
                cp      5
                jp      c,vdp_setrd
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
                ld      a,(SCRMOD)
                cp      5
                jp      c,vdp_setwrt
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

;--------------------
;VDP���[�`���̏�����
;--------------------

init_vdp:
                in      a,(VDP_STAT)    ; ���b�`�̏�����

                ld      bc,$0000        ; R#0
                call    wrt_vdp
                ld      bc,$6001        ; R#1
                call    wrt_vdp
                ld      bc,$0002        ; R#2
                call    wrt_vdp
                ld      bc,$8003        ; R#3
                call    wrt_vdp
                ld      bc,$0104        ; R#4
                call    wrt_vdp

                call    clr_text32

                ld      a ,$00
                ld      hl,$0800
                ld      bc,$0800
                call    vdp_fillmem

                ; for screen 1 color table
                ld      a ,$F5
                ld      hl,$2000
                ld      bc,$0020
                call    vdp_fillmem


; PatGenTbl
;        76543210 76543210
;        00000100 00000000
;             04h      00h

                ld      bc,$F507        ; R#7
                call    wrt_vdp

                in      a,(VDP_STAT)    ;�@���b�`�̏�����

                ld      hl,B_Font

                ld      de,$0800
                ld      bc,$0800
                call    vdp_data_rep

                ret

;------------------------
; ��ʕ\������Ă���e�L�X�g���N���A����B
clr_text40:
                xor     a
                ld      bc,$0400
                ld      hl,(TXTNAM)
                call    vdp_fillmem
                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a
                ret

clr_text32:
                xor     a
                ld      bc,$0300
                ld      hl,(T32NAM)
                call    vdp_fillmem
                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a
                ret

;------------------------------
;screen 5�̏�����.
init_sc5:
                ld      a,$05
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; ���b�`���Z�b�g

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $06             ; M4,M3 = 1

                ld      b,a             ; B = R#0 data
                ld      c,0

                call    wrt_vdp         ; VDP R#0

                ld      a,(RG0SAV+1)
                and     $E7             ; MASK 11100111

                ld      b,a             ; B = R#1 data
                inc     c

                call    wrt_vdp         ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      (CGPBAS),hl
                ld      hl,$7600
                ld      (ATRBAS),hl
                ld      hl,$7800
                ld      (PATBAS),hl

                ld      b,$1F           ; B = data
                ld      c,2
                call    wrt_vdp         ; VDP R#2

                ld      a,(RG0SAV+3)
                ld      b,a             ; B = data
                ld      c,3
                call    wrt_vdp         ; VDP R#3

                ld      a,(RG0SAV+4)
                ld      b,a             ; B = data
                ld      c,4
                call    wrt_vdp         ; VDP R#4

                ld      b,$EF           ; B = data
                ld      c,5
                call    wrt_vdp         ; VDP R#5

                ld      b,$0F           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#6

                ld      b,$08           ; B = data
                ld      c,8
                call    wrt_vdp         ; VDP R#8

                ld      b,$80           ; B = data
                ld      c,9
                call    wrt_vdp         ; VDP R#9

                ld      b,0             ; B = data
                ld      c,10
                call    wrt_vdp         ; VDP R#10
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#11
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#12
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#13
                ld      b,1             ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#14
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#15
                ld      b,$0F           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#16
                ld      b,$2C           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#17
                ld      b,$00           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#18
                ld      b,$00           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#19
                ld      b,$00           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#20

                ld      b,$3B           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#21
                ld      b,$05           ; B = data
                inc     c
                call    wrt_vdp         ; VDP R#22

                call    clrspr_spritemode2
                call    enascr
                ret

;------------------------------
;VDP���X�N���[��7�ŏ���������B
init_sc7:
                ld      a,$07
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; ���b�`�̃��Z�b�g

                call    chgclr

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $0A             ; M4,M3 = 1

                ld      b,a             ; B = R#0 data
                ld      c,0

                call    wrt_vdp         ; VDP R#0


                ld      a,(RG0SAV+1)
                and     $E7             ; MASK 11100111

                ld      b,a             ; B = R#1 data
                inc     c

                call    wrt_vdp         ; VDP R#1

                ld      hl,$0000
                ld      (NAMBAS),hl
                ld      (CGPBAS),hl
                ld      hl,$7600
                ld      (ATRBAS),hl
                ld      hl,$7800
                ld      (PATBAS),hl

                ld      b,$1F           ; B = data
                ld      c,2
                call    wrt_vdp         ; write VDP R#2

                ld      a,(RG0SAV+4)
                ld      b,a             ; B = data
                ld      c,4
                call    wrt_vdp         ; write VDP R#4

                ld      a,(RG0SAV+3)
                ld      b,a             ; B = data
                ld      c,3
                call    wrt_vdp         ; write VDP R#3


                ld      b,0             ; B = data
                ld      c,10
                call    wrt_vdp         ; write VDP R#10
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; write VDP R#11
                ld      b,0             ; B = data
                inc     c
                call    wrt_vdp         ; write VDP R#12

                ld      b,$00           ; B = data
                ld      c,19
                call    wrt_vdp         ; write VDP R#19

                ld      b,$01           ; B = data
                ld      c,15
                call    wrt_vdp         ; write VDP R#15

                in      a,(VDP_STAT)    ; reset Latch

                ld      b,$00           ; B = data
                ld      c,15
                call    wrt_vdp         ; write VDP R#15

                ld      b,$EF           ; B = data
                ld      c,5
                call    wrt_vdp         ; write VDP R#5
                ld      b,$0F           ; B = data
                inc     c
                call    wrt_vdp         ; write VDP R#6

                call    clrspr_spritemode2
                call    enascr
                ret

