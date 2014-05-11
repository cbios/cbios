; $Id: systemvars.asm,v 1.6 2004/12/22 07:55:57 andete Exp $
;
; C-BIOS system variable declarations
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
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


;-------------------
; help function area

; F380-F384: interslot read
RDPRIM:         equ     $F380

; F385-F38B: interslot read
WRPRIM:         equ     $F385

; F38C-F399: interslot call
CLPRIM:         equ     $F38C

; F39A-F3AD: workarea for the DEF USR statement
; this area is initialized with the 10 times the value $475A, which gives
; the error 'Syntax Error'
USRTAB:         equ     $F39A

;----------------------
; screen parameter area

; F3AE: # of positions on a line in SCREEN 0 (ini:39)
LINL40:         equ     $F3AE

; F3AF: # of positions on a line in SCREEN 1 (ini:29)
LINL32:         equ     $F3AF

; F3B0: # of actually used positions in the current screenmodus (ini:39)
LINLEN:         equ     $F3B0

; F3B1: # of used lines on screen (ini:24)
CRTCNT:         equ     $F3B1

; F3B2: # of positions within a tabulator-column (ini:14)
CLMLST:         equ     $F3B2

; F3B3-F3B4: BASE(0): name table of SCREEN 0 (ini:$0000)
; used to initialize NAMBAS when SCREEN 0 is activated
TXTNAM:         equ     $F3B3

; F3B5-F3B6: BASE(1): color table of SCREEN 0, unused? (ini:$0000)
TXTCOL:         equ     $F3B5

; F3B7-F3B8: BASE(2): pattern table of SCREEN 0 (ini:$0800)
; used to initialize CGPBAS when SCREEN 0 is activated
TXTCGP:         equ     $F3B7

; F3B9-F3BA: BASE(3): sprite attribute table of SCREEN 0, unused (ini:$0000)
; used to initialize ATRBAS when SCREEN 0 is activated
TXTATR:         equ     $F3B9

; F3BB-F3BC: BASE(4): sprite pattern table of SCREEN 0, unused (ini:$0000)
; used to initialize PATBAS when SCREEN 0 is activated
TXTPAT:         equ     $F3BB

; F3BD-F3BE: BASE(5): nametable of SCREEN 1 (ini:$1800)
; used to initialize NAMBAS when SCREEN 1 is activated
T32NAM:         equ     $F3BD

; F3BF-F3C0: BASE(6): color table of SCREEN 1 (ini:$2000)
T32COL:         equ     $F3BF

; F3C1-F3C2: BASE(7): pattern table of SCREEN 1 (ini:$0000)
; used to initialize CGPBAS when SCREEN 1 is activated
T32CGP:         equ     $F3C1

; F3C3-F3C4: BASE(8): sprite attribute table of SCREEN 1 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 1 is activated
T32ATR:         equ     $F3C3

; F3C5-F3C6: BASE(9)
T32PAT:         equ     $F3C5

; F3C7-F3C8: BASE(10)
GRPNAM:         equ     $F3C7           ; Screen2 Name

; F3C9-F3CA: BASE(11)
GRPCOL:         equ     $F3C9           ; Screen2 Color

; F3CB-F3CC: BASE(12)
GRPCGP:         equ     $F3CB           ; Screen2 CG pattern

; F3CD-F3CE: BASE(13)
GRPATR:         equ     $F3CD           ; Screen2 Attribute

; F3CF-F3D0: BASE(14)
GRPPAT:         equ     $F3CF           ; Screen2 Sprite pattern

; F3D1-F3D2: BASE(15)
MLTNAM:         equ     $F3D1           ; Screen3 Name

; F3D3-F3D4: BASE(16)
MLTCOL:         equ     $F3D3           ; Screen3 Color

; F3D5-F3D6: BASE(17)
MLTCGP:         equ     $F3D5           ; Screen3 CG pattern

; F3D7-F3D8: BASE(18)
MLTATR:         equ     $F3D7           ; Screen3 Attribute

; F3D9-F3DA: BASE(19)
MLTPAT:         equ     $F3D9           ; Screen3 Sprite pattern

CLIKSW:         equ     $F3DB           ; Key Click.
CSRY:           equ     $F3DC           ; �J�[�\�[���ʒu(Y���W)
CSRX:           equ     $F3DD           ; �J�[�\�[���ʒu(X���W)

RG0SAV:         equ     $F3DF
RG1SAV:         equ     $F3E0
RG2SAV:         equ     $F3E1
RG3SAV:         equ     $F3E2
RG4SAV:         equ     $F3E3
RG5SAV:         equ     $F3E4
RG6SAV:         equ     $F3E5
RG7SAV:         equ     $F3E6
STATFL:         equ     $F3E7

PUTPNT:         equ     $F3F8           ; �L�[�o�b�t�@�ւ̃|�C���^
GETPNT:         equ     $F3FA           ; �L�[�o�b�t�@�ւ̃|�C���^

STKTOP:         equ     $F674

NAMBAS:         equ     $F922
CGPBAS:         equ     $F924
PATBAS:         equ     $F926
ATRBAS:         equ     $F928

FORCLR:         equ     $F3E9
BAKCLR:         equ     $F3EA
BDRCLR:         equ     $F3EB

PSG_DBG:        equ     $F3EC           ; �f�o�b�O�p�t���O

DPPAGE:         equ     $FAF5           ; Display page (SCR5+)
ACPAGE:         equ     $FAF6           ; Active page (SCR5+)

EXBRSA:         equ     $FAF8           ; �T�u�����ʒu

CLIKFL:         equ     $FBD9
OLDKEY:         equ     $FBDA
NEWKEY:         equ     $FBE5
KEYBUF:         equ     $FBF0

LIMPNT:         equ     $FC17           ; �L�[�o�b�t�@�ւ̃|�C���^
LINWRK:         equ     $FC18           ; 40�����̃o�b�t�@

BOTTOM:         equ     $FC48
HIMEM:          equ     $FC4A

ESCCNT:         equ     $FCA7           ; ESC�p�J�E���^.

EXP_TBL:        equ     $FCC1           ; �X���b�g���e�[�u��
SLT_TBL:        equ     $FCC5           ; �X���b�g���

JIFFY:          equ     $FC9E           ; timer counter
SCRMOD:         equ     $FCAF

RG8SAV:         equ     $FFE7





; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
