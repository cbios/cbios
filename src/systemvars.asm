; $Id$
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

STKTOP:         equ     $F674

EXP_TBL:        equ     $FCC1           ; スロット情報テーブル
SLT_TBL:        equ     $FCC5           ; スロット情報

EXBRSA:         equ     $FAF8           ; サブロム位置

;----------------------------------------------
; ワークエリア

GRPNAM:         equ     $F3C7           ; Screen2 Name
GRPCOL:         equ     $F3C9           ; Screen2 Color
GRPCGP:         equ     $F3CB           ; Screen2 CG pattern
GRPATR:         equ     $F3CD           ; Screen2 Attribute
GRPPAT:         equ     $F3CF           ; Screen2 Sprite pattern

TXTNAM:         equ     $F3B3
TXTCOL:         equ     $F3B5
TXTCGP:         equ     $F3B7
TXTATR:         equ     $F3B9
TXTPAT:         equ     $F3BB

T32NAM:         equ     $F3BD
T32COL:         equ     $F3BF
T32CGP:         equ     $F3C1
T32ATR:         equ     $F3C3
T32PAT:         equ     $F3C5

NAMBAS:         equ     $F922
CGPBAS:         equ     $F924
PATBAS:         equ     $F926
ATRBAS:         equ     $F928

MLTNAM:         equ     $F3D2           ; Screen3 Name
MLTCOL:         equ     $F3D4           ; Screen3 Color
MLTCGP:         equ     $F3D6           ; Screen3 CG pattern
MLTATR:         equ     $F3D8           ; Screen3 Attribute
MLTPAT:         equ     $F3DA           ; Screen3 Sprite pattern

CLIKSW:         equ     $F3DB           ; Key Click.

STATFL:         equ     $F3E7
RG0SAV:         equ     $F3DF
RG1SAV:         equ     $F3E0

RG4SAV:         equ     $F3E3

RG8SAV:         equ     $FFE7

CLIKFL:         equ     $FBD9
OLDKEY:         equ     $FBDA
NEWKEY:         equ     $FBE5
KEYBUF:         equ     $FBF0

PUTPNT:         equ     $F3F8           ; キーバッファへのポインタ
GETPNT:         equ     $F3FA           ; キーバッファへのポインタ
LIMPNT:         equ     $FC17           ; キーバッファへのポインタ

BOTTOM:         equ     $FC48
HIMEM:          equ     $FC4A

JIFFY:          equ     $FC9E           ; タイマーカウンタ
SCRMOD:         equ     $FCAF

FORCLR:         equ     $F3E9
BAKCLR:         equ     $F3EA
BDRCLR:         equ     $F3EB

PSG_DBG:        equ     $F3EC           ; デバッグ用フラグ

CSRY:           equ     $F3DC           ; カーソール位置(Y座標)
CSRX:           equ     $F3DD           ; カーソール位置(X座標)

LINL40:         equ     $F3AE
LINL32:         equ     $F3AF
LINLEN:         equ     $F3B0           ; 行数.
CRTCNT:         equ     $F3B1           ; 画面の桁数

DPPAGE:         equ     $FAF5           ; Display page (SCR5+)
ACPAGE:         equ     $FAF6           ; Active page (SCR5+)

LINWRK:         equ     $FC18           ; 40桁分のバッファ

ESCCNT:         equ     $FCA7           ; ESC用カウンタ.

