; $Id: systemvars.asm,v 1.7 2004/12/22 08:19:36 andete Exp $
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

; F3B3-F3B4: BASE(0): name table address for SCREEN 0 (ini:$0000)
; used to initialize NAMBAS when SCREEN 0 is activated
TXTNAM:         equ     $F3B3

; F3B5-F3B6: BASE(1): color table address for SCREEN 0, unused? (ini:$0000)
TXTCOL:         equ     $F3B5

; F3B7-F3B8: BASE(2): pattern table address for SCREEN 0 (ini:$0800)
; used to initialize CGPBAS when SCREEN 0 is activated
TXTCGP:         equ     $F3B7

; F3B9-F3BA: BASE(3): sprite attribute table address for SCREEN 0, unused (ini:$0000)
; used to initialize ATRBAS when SCREEN 0 is activated
TXTATR:         equ     $F3B9

; F3BB-F3BC: BASE(4): sprite pattern table address for SCREEN 0, unused (ini:$0000)
; used to initialize PATBAS when SCREEN 0 is activated
TXTPAT:         equ     $F3BB

; F3BD-F3BE: BASE(5): nametable address for SCREEN 1 (ini:$1800)
; used to initialize NAMBAS when SCREEN 1 is activated
T32NAM:         equ     $F3BD

; F3BF-F3C0: BASE(6): color table address for SCREEN 1 (ini:$2000)
T32COL:         equ     $F3BF

; F3C1-F3C2: BASE(7): pattern table address for SCREEN 1 (ini:$0000)
; used to initialize CGPBAS when SCREEN 1 is activated
T32CGP:         equ     $F3C1

; F3C3-F3C4: BASE(8): sprite attribute table address for SCREEN 1 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 1 is activated
T32ATR:         equ     $F3C3

; F3C5-F3C6: BASE(9): sprite pattern table address for SCREEN 1 (ini:$0800)
; used to initialize PATBAS when SCREEN 1 is activated
T32PAT:         equ     $F3C5

; F3C7-F3C8: BASE(10): name table address for SCREEN 2 (ini:$1800)
; used to initialize NAMBAS when SCREEN 2 is activated
GRPNAM:         equ     $F3C7

; F3C9-F3CA: BASE(11): color table address for SCREEN 2 (ini:$2000)
GRPCOL:         equ     $F3C9           ; Screen2 Color

; F3CB-F3CC: BASE(12): pattern table address for SCREEN 2 (ini:$0000)
; used to initialize CGPBAS when SCREEN 2 is activated
GRPCGP:         equ     $F3CB

; F3CD-F3CE: BASE(13): sprite attribute table address for SCREEN 2 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 2 is activated
GRPATR:         equ     $F3CD

; F3CF-F3D0: BASE(14): sprite pattern table address for SCREEN 2 (ini:$3800)
; used to initialize PATBAS when SCREEN 2 is activated
GRPPAT:         equ     $F3CF

; F3D1-F3D2: BASE(15): name table address for SCREEN 3 (ini:$0800)
; used to initialize NAMBAS when SCREEN 3 is activated
MLTNAM:         equ     $F3D1

; F3D3-F3D4: BASE(16): color table address for SCREEN 3 (ini:$0000)
; the color table is unused in SCREEN 3
MLTCOL:         equ     $F3D3

; F3D5-F3D6: BASE(17): pattern table address for SCREEN 3 (ini:$0000)
; used to initialize CGPBAS when SCREEN 3 is activated
MLTCGP:         equ     $F3D5

; F3D7-F3D8: BASE(18): sprite attribute table address for SCREEN 3 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 3 is activated
MLTATR:         equ     $F3D7

; F3D9-F3DA: BASE(19): sprite pattern table address for SCREEN 3 (ini:$3800)
; used to initialize PATBAS when SCREEN 3 is activated
MLTPAT:         equ     $F3D9

; F3DB: keyclick when a key is pressed: 0: no, 1: yes
CLIKSW:         equ     $F3DB

; F3DC: line where the cursor is located
; starts to count at 1 for the topmost line
CSRY:           equ     $F3DC

; F3DD: column where the cursor is located
; starts to count at 1 for the leftmost column
CSRX:           equ     $F3DD

RG0SAV:         equ     $F3DF
RG1SAV:         equ     $F3E0
RG2SAV:         equ     $F3E1
RG3SAV:         equ     $F3E2
RG4SAV:         equ     $F3E3
RG5SAV:         equ     $F3E4
RG6SAV:         equ     $F3E5
RG7SAV:         equ     $F3E6
STATFL:         equ     $F3E7

PUTPNT:         equ     $F3F8           ; キーバッファへのポインタ
GETPNT:         equ     $F3FA           ; キーバッファへのポインタ

; ---------------------------
; basic interpreter work area

STKTOP:         equ     $F674

NAMBAS:         equ     $F922
CGPBAS:         equ     $F924
PATBAS:         equ     $F926
ATRBAS:         equ     $F928

FORCLR:         equ     $F3E9
BAKCLR:         equ     $F3EA
BDRCLR:         equ     $F3EB

PSG_DBG:        equ     $F3EC           ; デバッグ用フラグ

; --------------------
; filesystem work area

; ------------------------
; screen routine work area

; --------------------------------------
; work area for sound and queueing and RS232
; in the MSX2, the RS232 addresses are used for other purposes

DPPAGE:         equ     $FAF5           ; Display page (SCR5+)
ACPAGE:         equ     $FAF6           ; Active page (SCR5+)

EXBRSA:         equ     $FAF8           ; サブロム位置

; -----------------------------------------------
; settings for screen editor and interrupt system

CLIKFL:         equ     $FBD9
OLDKEY:         equ     $FBDA
NEWKEY:         equ     $FBE5
KEYBUF:         equ     $FBF0

LIMPNT:         equ     $FC17           ; キーバッファへのポインタ
LINWRK:         equ     $FC18           ; 40桁分のバッファ

BOTTOM:         equ     $FC48
HIMEM:          equ     $FC4A

ESCCNT:         equ     $FCA7           ; ESC用カウンタ.

EXP_TBL:        equ     $FCC1           ; スロット情報テーブル
SLT_TBL:        equ     $FCC5           ; スロット情報

JIFFY:          equ     $FC9E           ; timer counter
SCRMOD:         equ     $FCAF

; ---------------------------
; storage of slot information

; ------------------------------
; storage of ROM-page parameters

; ------------
; system hooks

; system hooks are defined in hooks.asm

; ------------------
; storage of VDP8-23

RG8SAV:         equ     $FFE7

; ----------------------
; extra slot information

; ---------------------------
; subslot switching addresses

; -------
; the end

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
