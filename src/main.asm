; $Id: main.asm,v 1.2 2004/11/27 02:34:03 mthuurne Exp $
; C-BIOS ver 0.17
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

;---------------------------------------------------
; I/Oとフックの定義

PRN_STAT:       equ     $90             ; プリンタのステータス
VDP_RP:         equ     $98             ; VDPポート読み出し
VDP_STAT:       equ     $99             ; VDPステータス、ラッチリセット。
PSL_STAT:       equ     $A8             ; スロットステータス
KBD_STAT:       equ     $A9             ; キーボードステータス
GIO_REGS:       equ     $AA             ; 総合IOレジスタ
PPI_REGS:       equ     $AB             ; PPIレジスタ

PSG_REGS:       equ     $A0             ; PSGレジスタ番号
PSG_DATA:       equ     $A1             ; PSGデータ
PSG_STAT:       equ     $A2             ; PSGステータス

MAP_REG1:       equ     $FC             ; RAMマッパー 0000h-3FFFh
MAP_REG2:       equ     $FD             ; RAMマッパー 4000h-7FFFh
MAP_REG3:       equ     $FE             ; RAMマッパー 8000h-BFFFh
MAP_REG4:       equ     $FF             ; RAMマッパー C000h-FFFFh

VDP_DATA:       equ     $98             ; VDPデータ書き込み
VDP_ADDR:       equ     $99             ; VDPアドレス
VDP_PALT:       equ     $9A             ; VDPパレットラッチ
VDP_REGS:       equ     $9B             ; VDPレジスタアクセス

H_KEYI:         equ     $FD9A           ; キーボード割り込みフック
H_TIMI:         equ     $FD9F           ; タイマー割り込みフック
H_NMI:          equ     $FDD6           ; NMIフック
H_STKE:         equ     $FEDA           ; スタックエラーフック

STKTOP:         equ     $F674

EXP_TBL:        equ     $FCC1           ; スロット情報テーブル
SLT_TBL:        equ     $FCC5           ; スロット情報

EXBRSA:         equ     $FAF8           ; サブロム位置
SSL_REGS:       equ     $FFFF           ; 拡張スロット選択レジスタ

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

LINWRK:         equ     $FC18           ; 40桁分のバッファ

ESCCNT:         equ     $FCA7           ; ESC用カウンタ.

;-----------------
; デバッグルーチン用メモリ
;-----------------

DISPADDR:       equ     $E010           ; ダンプアドレス用メモリ
LASTSTAC:       equ     $E000
SP_REGS:        equ     $E002

;---------------------
; ジャンプテーブル
;---------------------

;0000h CHKRAM
chkram:
                org     $0000
                di
                jp      soft_reset

;フォントへのポインタ

                ds      $0004 - $
                dw      B_Font

                ds      $0006 - $

rdvdpa:         db      VDP_DATA        ; VDP読み出しポート
wrvdpa:         db      VDP_DATA        ; VDP書き込みポート

;0008h SYNCHR
synchr:
                ds      $0008 - $
                ret

;000Ch RDSLT    任意スロットからのメモリ読み込み

                ds      $000C - $
                jp      rdslt

chrgtb:
                ds      $0010 - $
                ret

;0014h WRSLT    任意スロットへのメモリ書き込み
                ds      $0014 - $
                jp      wrslt
                ret
;0018h OUTDO
                ds      $0018 - $
                jp      ch_put
;001Ch CALSLT   インタースロットコールルーチン
calslt:
                ds      $001C - $
                jp      cal_slt
                ret

;0020h DCOMPR   HLとDEの比較
dcompr:
                ds      $0020 - $
                jp      wordcomp
                ret

;0024h ENASLT   スロットの変更
                ds      $0024 - $
                jp      enaslt

;0028h MATH-PACK
                ds      $0028 - $
                ret

;002D バージョンID等
romid:
                ds      $002D - $
; version ID
                db      0 ; 0 .. msx1 ,1 .. msx2
                db      0 ; ??
                db      0 ; ??

;0030h CALLF    インタースロット呼び出し(RST30h版)
                ds      $0030 - $
                jp      call_lf

;0038h INT_38   割り込みルーチン(RST38,VBlank,Timer...)
                ds      $0038 - $
                jp      int_start

;0038h INITIO   I/Oの初期化
                ds      $003B - $
                ret

;0041h DISSCR   スクリーンを表示させない。
                ds      $0041 - $
                jp      disscr

;0044h ENASCR   スクリーンを表示させる。
                ds      $0044 - $
                jp      enascr

;---------------
;VDPルーチン
;---------------

;0047h WRTVDP
                ds      $0047 - $
                jp      wrt_vdp

;004Ah RDVRM
                ds      $004A - $
                jp      rd_vrm

;004Dh WRTVRM
                ds      $004D - $
                jp      wrt_vrm

;0050h SETRD
                ds      $0050 - $
                jp      vdp_setrd

;0053h SETWRT  .. VRAM書き込みアドレスの設定
                ds      $0053 - $
                jp      vdp_setwrt

                ds      $0056 - $
                jp      vdp_fillmem

                ds      $0059 - $
                jp      vdp_ldirmv      ; VRAM -> Memory

                ds      $005C - $
                jp      vdp_data_rep    ; Memory -> VRAM

;005Fh VDPスクリーンモードの変更
                ds      $005F - $
                jp      chgmod

;0062h CHGCLR
                ds      $0062 - $
                jp      chgclr

;0066h INT_NMI .. NMI割り込み
                ds      $0066 - $
                jp      nmi_int

;0069h CLRSPR  .. スプライトを消去。
                ds      $0069 - $
                ret

;006Ch INITXT   画面をTEXT1モードに初期化。
                ds      $006C - $
                jp      init_txt

;006Fh INIT32   画面をGRAPHIC1モードに初期化。
                ds      $006F - $
                jp      init_txt32

;0072h INITGRP  画面をGRAPHIC2モードに初期化。

                ds      $0072 - $
                jp      init_grp

;007Eh  SETGRP
                ds      $007E - $
                jp      set_grp

;0085h DOLINE   ライン描画
doline:
                ds      $0085 - $
                ret

;0089h GRPPRT   basic?
grpprt:
                ds      $0089 - $
                ret

                ds      $008A - $
                ret

;0090h GICINI   音源ICの初期化
                ds      $0090 - $
                jp      sound_init

                ds      $0093 - $
                jp      sound_out

                ds      $0096 - $
                jp      sound_stat

;009Ch CHSNS  .. キーバッファのチェック
                ds      $009C - $
                jp      ch_sns

;009Fh CHGET .. キーバッファからデータを得る
                ds      $009F - $
                jp      ch_get

;00A2h  CHPUT .. ディスプレイのキャラクタを出力する。
                ds      $00A2 - $
                jp      ch_put

;00C6h  POSIT .. カーソル移動。
                ds      $00C6 - $
                jp      curxy

;00D5h GTSTCK .. ジョイスティック情報を得る。
                ds      $00D5 - $
                jp      in_joy

;00D8h GTTRIG .. トリガー情報を得る。
                ds      $00D8 - $
                jp      in_trig

;012Dh WRTVDP .. VDPレジスタの値を変更する
                ds      $012D - $
                ret

;0131h VDPSTA .. VDPステータスを読み出す
;vdpsta
                ds      $0131 - $
                ret

;chgsnd
                ds      $0135 - $
                ret

;0138h RDSLTREG プライマリスロットの情報を読み出す
;g_slotreg
                ds      $0138 - $
                jp      get_slotreg

;013Bh WRSLTREG プライマリスロットに情報を書き込む。
;s_slotreg
                ds      $013B - $
                jp      set_slotreg

;013Eh RDVDP    VDPステータスの読み出し
                ds      $013E - $
                jp      vdp_stat_in

;setpag          ds   $013D - $
;                ret

;0141h SNSMAT   キーマトリクスを得る
;snsmat
                ds      $0141 - $
                jp      in_keyboard

;phydio
                ds      $0144 - $
                ret

;0145h RSTPLT
;rstplt
                ds      $0145 - $
                ret

;0149h RSTPLT?
;rstplt
                ds      $0149 - $
                ret

;getplt
                ds      $014D - $
                ret

;0156h KILBUF   キーボードバッファをクリアする
                ds      $0156 - $
                jp      kilbuf

;0159h CALBAS   ベーシックインタプリタを呼び出す。
                ds      $0159 - $
                jp      call_basic_intr

;beep
                ds      $017D - $
                ret

;prompt
                ds      $0181 - $
                ret

;newpad
                ds      $01AD - $
                ret

;chgmdp
                ds      $01B5 - $
                ret

;knjprt
                ds      $01BD - $
                ret

;redclk
                ds      $01F5 - $
                ret

;wrtclk
                ds      $01F9 - $
                ret

; -------------------
; スタートアップコード（リセット時に呼び出される）
; -------------------

                ds      $0200 - $
soft_reset:
;デバッグ用
;                ex      (sp),hl
;                ld      (LASTSTAC),hl
;
;                ld      hl,$0000
;                add     hl,sp
;                ld      (SP_REGS),hl
;
;                ld      hl,$F300

; インターフェースを初期化する。
                ld      a,$82
                out     (PPI_REGS),a
                ld      a,$50
                out     (GIO_REGS),a

;メモリバンクを初期化する。
                xor     a
;                out     (MAP_REG1),a
                inc     a
                out     (MAP_REG2),a
                inc     a
                out     (MAP_REG3),a
                inc     a
                out     (MAP_REG4),a

;メモリチェック、選択スロットをメモリに書き込む。
; C = プライマリ,B = セカンダリ。
                ld      bc,$0303
chk_wrt_ram:               ; ページ３のRAMをチェックする。
                in      a,(PSL_STAT)
                and     $3F
                ld      e,a

                ld      a,c
                and     $03
                rrca
                rrca
                or      e
                out     (PSL_STAT),a    ; A = BBxxxxxx

                ld      a,(SSL_REGS)
                cpl
                and     $3F
                ld      e,a

                ld      a,b
                and     $03
                rrca
                rrca
                or      e
                ld      (SSL_REGS),a    ; A = EExxxxxx
                ld      e,a

                ld      a,$12
                ld      ($C010),a
                ld      a,($C010)
                cp      $12
                jr      nz,cant_wrt
                jp      ram_ok
cant_wrt:
                dec     b
                jp      p,chk_wrt_ram
                ld      b,$03
                dec     c
                jp      p,chk_wrt_ram
                ld      de,str_memory_err
                jp      print_error

ram_ok:
                ld      hl,SLT_TBL
                ld      b,0
                ld      c,a
                add     hl,bc

                ld      a,e
                ld      (hl),a          ; 拡張スロット

                and     $0C

                in      a,(PSL_STAT)
                and     $03
                or      $80
                ld      (EXP_TBL),a     ; MAIN-ROMの位置
                and     $80
                ld      (EXP_TBL+1),a
                ld      (EXP_TBL+2),a
                ld      (EXP_TBL+3),a

; you can write the memory.

;----------------------
;ユーザーインターフェース
;----------------------

                ld      hl,$F300
                ld      sp,hl           ; スタックを$F300に。

                call    init_ram

                call    check_rom

;                in      a,(PSL_STAT)
;                ld      ($F000),a
;                call    p3_chk

                call    init_vdp

                xor     a
                ld      (PSG_DBG),a

                call    sound_init

                ei

                call    disp_info
                call    start_cartprog

;----------------------
;カートリッジを実行する
;----------------------

start_game:
                in      a,(PSL_STAT)
                rrca
                rrca
                and     $03
                cp      $03
                jr      z,dont_chgscr
                ld      a,$01
                call    chgmod
dont_chgscr:
                ld      hl,stack_error
                push    hl
                ld      a,($4000)
                cp      'A'
                jr      nz,p3_run
                ld      hl,($4002)      ; カートリッジ開始アドレス。
                jr      prg_jumper
p3_run:
                ld      hl,($8002)      ; カートリッジ開始アドレス。
prg_jumper:
                jp      (hl)            ; 実行...


;-------------------------------
; 情報表示
;-------------------------------
disp_info:

; シフトキーが押されていればdebug_modeへ。
;
                ld      a,$06
                call    in_keyboard
                bit     0,a
                jp      z,debug_mode

; プログラム情報の表示

                ld      hl,str_proginfo ; HL = 文字列アドレス
                call    prn_text

                ld      a,($4000)
                cp      'A'
                ret     z
                ld      a,($8000)
                cp      'A'
                ret     z

                ld      hl,str_nocart
                call    prn_text

                jp      hang_up_mode


;----------------------------
start_cartprog:
; スロット上にカートリッジが存在したなら、
; そのスロットのプログラムを実行する。

                ld      hl,str_slot
                call    prn_text

                call    get_slotreg
                rrca
                rrca
                and     $03

                ld      d,$00
                ld      e,a
                push    de

                call    vout_hex8
                call    vout_hyphen

                pop     de
                ld      hl,SLT_TBL
                add     hl,de
                ld      a,(hl)
                rrca
                rrca
                and     $03
                call    vout_hex8

;デバッグと動作チェック

;                call    check_sum

;                ld      a,(SMOD_TSTSND)
;                or      a
;                jr      z,stst_skip

;サウンドモード用
;                ld      hl,str_canstst
;                call    prn_text

stst_skip:
                ld      b,120           ; 2sec wait (1 = 1/60sec)
                call    wait_key07      ; wait routine

;サウンドモード用
;                bit     7,a
;                call    z,sound_mode

                bit     5,a
                jp      z,sh_keyboard
                bit     0,a
                jp      z,debug_mode

                ld      hl,str_crlf
                call    prn_text

                ld      hl,str_run
                call    prn_text

                call    dbg_reg

                ld      b,60            ; 1sec
                call    wait_b

                ret

; ------
; BIOSデバッグルーチン
; ------
; HL ... NEWKEYのアドレス .
;
sh_keyboard:
                call    init_vdp
kbd_lp:
                ld      a,1
                ld      (CSRX),a
                ld      (CSRY),a
                ld      hl,NEWKEY

                ld      b,10
kbd_byteread:
                push    bc
                ld      a,(hl)
                inc     hl
                ld      b,8
kbd_shift:
                rlca
                push    af
                jr      c,kbd_on
;Bit[n] = 0
                ld      a,'0'
                call    ch_put
                pop     af
                jr      kbd_lpchk
;Bit[n] = 1
kbd_on:
                ld      a,'1'
                call    ch_put
                pop     af
kbd_lpchk:
                djnz    kbd_shift
                pop     bc
                ld      a,$0D
                call    ch_put
                ld      a,$0A
                call    ch_put

                djnz    kbd_byteread

                jr      kbd_lp


sh_debug:
                ex      (sp),hl
                ld      (LASTSTAC),hl

debug_mode:
                ; リターンコードをフックに埋め込む。
                ld      a,$C9
                ld      (H_KEYI),a
                ld      (H_TIMI),a

                call    init_vdp

                ld      a,1
                ld      (CSRX),a
                ld      a,1
                ld      (CSRY),a

                ld      hl,$4000        ; ページ1から表示する。
                ld      (DISPADDR),hl

                ld      ix,(LASTSTAC)
                call    vout_hex16

                ld      a,' '
                call    ch_put

                ld      ix,(SP_REGS)
                call    vout_hex16
loop_dump:
                call    disp_dump
                call    dump_keywait
                jr      loop_dump


;-----------------
; ダンプ用ルーチン
;-----------------

disp_dump:

                ld      a,1
                ld      (CSRX),a
                ld      a,3
                ld      (CSRY),a

                ld      ix,(DISPADDR)
                ld      c,$10           ; RegC = rows

dump_lp:
                push    bc
                call    vout_hex16
                pop     bc
                ld      a,':'
                call    ch_put
                ld      b,$10           ; RegB = cols
d16_lp:
                ld      a,(ix+0)
                push    bc
                call    vout_hex8
                pop     bc
                inc     ix
                djnz    d16_lp

                dec     c
                jr      nz,dump_lp

                ret

;-------------------------
; キーループ用ウェイト
dump_keywait:

                ld      e,$02

                ld      a,($E008)
                ld      d,a

                ld      a,$08
                call    in_keyboard

                cp      d
                jr      z,dumpkey_loop
                ; case of A != E008
                ld      ($E008),a
                ld      e,$10

dumpkey_loop:
                halt
                ld      a,($E008)
                ld      d,a

                ld      a,$08
                call    in_keyboard

                dec     e
                jr      z,skip_kchk
                cp      d
                jr      z,dumpkey_loop

skip_kchk:
                push    af
                ld      a,$06
                call    in_keyboard
                bit     0,a
                jr      nz,norm
                ld      iy,$1000
                jr      bit_chk
norm:
                ld      iy,$0100
bit_chk:
                pop     af

                bit     7,a
                jr      z,on_pagedown
                bit     6,a
                jr      z,on_down
                bit     5,a
                jr      z,on_up
                bit     4,a
                jr      z,on_pageup

                ld      ($E008),a

                ld      a,$07
                call    in_keyboard

                bit     1,a
                jr      z,on_start

                jr      dumpkey_loop

on_pagedown:
                push    iy
                pop     bc
                jr      up_addr
on_pageup:
                push    iy
                pop     bc
                jr      down_addr
on_down:
                ld      bc,$10
                jr      up_addr
on_up:
                ld      bc,$10
                jr      down_addr

up_addr:
                ld      hl,($E010)
                add     hl,bc
                ld      ($E010),hl
                ret
down_addr:
                ld      hl,($E010)
                and     a
                sbc     hl,bc
                ld      ($E010),hl
                ret

on_start:
                jp      start_game


;--------------------
;VDPルーチンの初期化
;--------------------

init_vdp:
                in      a,(VDP_STAT)    ; ラッチの初期化

                ld      bc,$0000        ; R#0
                call    wrt_vdp
                ld      bc,$7001        ; R#1
                call    wrt_vdp
                ld      bc,$0002        ; R#2
                call    wrt_vdp
                ld      bc,$8003        ; R#3
                call    wrt_vdp
                ld      bc,$0104        ; R#4
                call    wrt_vdp

                call    clr_text40

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

                in      a,(VDP_STAT)    ;　ラッチの初期化

                ld      hl,B_Font

                ld      de,$0800
                ld      bc,$0800
                call    vdp_data_rep

                ret

;------------------------
; 画面表示されているテキストをクリアする。
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

;------------------------
; RAMの初期化

init_ram:

; ワークエリア初期化。
                ld      a,$00
                ld      hl,$F380
                ld      (hl),a
                ld      de,$F381
                ld      bc,$0C7D
                ldir

;リターンコード埋め込み
                ld      a,$C9           ; ret code
                ld      hl,H_KEYI
                ld      (hl),a
                ld      de,H_KEYI+1
                ld      bc,$024D
                ldir

;キーマトリクスの初期化。
                ld      a,$FF
                ld      hl,OLDKEY
                ld      (hl),a
                ld      de,OLDKEY+1
                ld      bc,21
                ldir

;キーバッファの初期化。
                ld      a,$00
                ld      hl,KEYBUF
                ld      (hl),a
                ld      de,KEYBUF+1
                ld      bc,39
                ldir

                ld      a,$00
                ld      hl,$FFE7 ; RG8SAV
                ld      (hl),a
                ld      de,$FFE7+1
                ld      bc,15
                ldir

;アドレスポインタを設定。
                ld      hl,KEYBUF
                ld      (PUTPNT),hl
                ld      (GETPNT),hl

                ld      hl,$8000
                ld      (BOTTOM),hl     ; ページ1,2はROM。ページ3,4はRAM.

                ld      hl,$F380
                ld      (HIMEM),hl      ; 使用可能メモリの上限

                ld      (STKTOP),hl     ; BASICスタックの位置

                ld      hl,disk_intr
                ld      a,$C3
                ld      (H_STKE),a
                ld      (H_STKE+1),hl

;RDPRIMをRAMに転送する.

                ld      hl,m_rdprim
                ld      de,$F380

                ld      bc,m_prim_end-m_rdprim
                ldir

; screen 0 テーブルの初期化。
                ld      hl,$0000
                ld      (TXTNAM),hl
                ld      hl,$0800
                ld      (TXTCGP),hl

; screen 1 テーブルの初期化。
                ld      hl,$1800
                ld      (T32NAM),hl
                ld      hl,$2000
                ld      (T32COL),hl
                ld      hl,$0000
                ld      (T32CGP),hl
                ld      hl,$1B00
                ld      (T32ATR),hl
                ld      hl,$3800
                ld      (T32PAT),hl

; screen 2 テーブルの初期化
                ld      hl,$1800
                ld      (GRPNAM),hl
                ld      hl,$2000
                ld      (GRPCOL),hl
                ld      hl,$0000
                ld      (GRPCGP),hl
                ld      hl,$1B00
                ld      (GRPATR),hl
                ld      hl,$3800
                ld      (GRPPAT),hl

; screen 3 テーブルの初期化
                ld      hl,$0800
                ld      (MLTNAM),hl
                ld      hl,$0000
                ld      (MLTCGP),hl
                ld      hl,$1B00
                ld      (MLTATR),hl
                ld      hl,$3800
                ld      (MLTPAT),hl

; その他の設定。
                ld      a,39
                ld      (LINL40),a
                ld      a,29
                ld      (LINL32),a
;                ld      a,29
                ld      a,39
                ld      (LINLEN),a
                ld      a,24
                ld      (CRTCNT),a

                ld      a,$04
                ld      (BDRCLR),a
                ld      (BAKCLR),a
                ld      a,$0F
                ld      (FORCLR),a

                ld      a,$20
                ld      (RG1SAV),a

                ld      a,$08
                ld      (RG8SAV),a


                ret

;----------------------
;サブロム位置の検出
chksubpos:
                ld      b,$03
                ld      hl,$0000
loop_subpos:
                ld      a,b
                push    bc
                call    rdslt
                pop     bc
                cp      'C'
                jr      z,chked_c
slt_wrong:
                djnz    loop_subpos
                xor     a
                ld      (EXBRSA),a
                ret
chked_c:
                inc     hl
                ld      a,b
                push    bc
                call    rdslt
                pop     bc
                cp      'D'
                jr      z,chked_d
                dec     hl
                jr      slt_wrong
chked_d:
                ld      a,b
                ld      (EXBRSA),a
                ret

;------------------------
;
;レジスタの表示
;
dbg_reg:
                push    ix
                push    iy
                push    hl
                push    de
                push    bc
                push    af

                ld      iy,6

                ld      a,1
                ld      (CSRX),a
                ld      a,14
                ld      (CSRY),a

dbg_loop:
                pop     ix
                call    prn_hex

                dec     iy

                push    iy
                pop     bc

                xor     a
                or      c

                jr      nz,dbg_loop
                pop     ix
                push    ix
                jp      prn_hex ; レジスタPCの表示


;------------------------
prn_hex:
;BC = Reg
                call    prn_reg
                call    vout_hex16

                ld      a,' '
                call    ch_put

                ret

;------------------------
prn_reg:
;
                push    hl
                push    bc
                push    iy
                pop     bc

                ld      a,c
                add     a,a
                add     a,a
                ld      c,a

                ld      hl,reg_tbl
                add     hl,bc

                ld      b,3
reg_lp:
                ld      a,(hl)
                inc     hl
                call    ch_put
                djnz    reg_lp

                pop     bc
                pop     hl
                ret

;------------------------
vout_hex16:
;16bit幅の16進数表示。
;IX = number
;dest = BC,HL,AF
                ; RegB上位4bit
                ld      hl,hex_tbl
                push    ix
                pop     bc

                ld      a,b
                rlca
                rlca
                rlca
                rlca
                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put


                ; RegB下位4bit
                ld      hl,hex_tbl
                push    ix
                pop     bc

                ld      a,b
                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put

                ; RegC上位4bit
                ld      hl,hex_tbl
                push    ix
                pop     bc

                ld      a,c
                rlca
                rlca
                rlca
                rlca
                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put

                ; RegC 下位 4bit
                ld      hl,hex_tbl
                push    ix
                pop     bc

                ld      a,c
                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put

                ret

;------------------------
vout_hex8:
;8bit幅の16進数表示。
;A = 数値
;dest = BC,HL,AF
                push    af ; このスタックは下位4bitのために使う。

                ; RegA 上位 4bit

                ld      hl,hex_tbl
                rlca
                rlca
                rlca
                rlca
                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put

                pop     af

                ; RegA 下位 4bit
                ld      hl,hex_tbl

                and     $0F

                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                call    ch_put

                ret

;------------------------------
vout_hyphen:
                ld      a,'-'
                call    ch_put
                ret


;------------------------
; ウェイトルーチン
;注意、このルーチン呼び出しの際は常にEIであること。
; B = ループ回数
wait_b:
                halt
                djnz    wait_b
                ret

;------------------------
; ウェイトルーチン
;注意、このルーチン呼び出しの際は常にEIであること。
; in .... B = ループ回数
; out ... A = キーマトリクスの７番目。
; dest .. RegC
wait_key07:
                ld      c,$FF
wk07_lp:
                halt
                ld      a,($FBEB)
                and     c
                ld      c,a
                djnz    wk07_lp
                ld      a,c
                ret

;------------------------
;prn_text
; HL = 文字列

prn_text:
prn_str_disp:
                ld      a,(hl)
                or      a
                jp      z,nul_term
                call    ch_put
                inc     hl
                jr      prn_str_disp
nul_term:
                ret

;-------------------------
check_sum:
                xor     a
                ld      bc,$4000
                ld      hl,$4000
                ld      de,$0000
csum_lp:
                ld      a,(hl)
                add     a,e
                ld      e,a
                jr      nc,csum_nc
                inc     d
csum_nc:
                inc     hl
                dec     bc

                ld      a,b
                or      c
                jr      nz,csum_lp
                ret

;---------------------------------------------
check_rom:

                ld      b,$80

chk_rom_loop:
                ld      h,$40
                ld      a,b
                push    bc
                call    enaslt
                pop     bc

                ld      a,($4000)
                cp      'A'
                jr      nz,chk_p3

                ld      a,($4003)
                cp      $40
                jr      c,chk_p3        ; A-$40 < 0
                cp      $80
                jr      nc,chk_p3       ; A-$80 >= 0
                jp      chk_rom_ok
;
chk_p3:
                ld      h,$80
                ld      a,b
                push    bc
                call    enaslt
                pop     bc

                ld      a,($8003)
                cp      $80
                jr      c,no_cart       ; A-$80 < 0
                cp      $C0
                jr      nc,no_cart      ; A-$C0 >= 0
                jp      chk_rom_ok
;
no_cart:
                inc     b
                ld      a,b
                and     $7F
                cp      $10
                jr      nz,chk_rom_loop

chk_rom_ng:
                ld      h,$40
                ld      a,$80

                push    bc
                call    enaslt
                pop     bc

chk_rom_ok:
                ret

;----------------------------------
p3_chk:
                ld      a,($8000)
                cp      'A'
                jp      z,page_set0

                ret

page_set0:
                call    get_slotreg
                ld      c,a
                and     $3F             ; 00111111
                ld      b,a
                and     $03             ; 000000AA
                rrca
                rrca
                or      b               ; AABBBBAA
                out     (PSL_STAT),a

                ld      a,(SSL_REGS)
                cpl
                and     $F3             ; 11110011
                ld      d,a
                and     $03             ; 00000011
                rlca
                rlca
                or      d
                ld      (SSL_REGS),a

                ld      a,c
                and     $F3
                ld      c,a
                and     $03
                rlca
                rlca
                or      c
                out     (PSL_STAT),a
                ret


;-------------------------------------
;カーソルをRegDEに変換する。
;-------------------------------------
;out.. DE = VRAMアドレス
curs2de:
                push    af
                push    bc
                push    hl

                xor     a
                ld      hl,SCRMOD
                cp      (hl)
                jr      nz,c2d_scr1
                ld      hl,(TXTNAM)
                ld      de,40           ; text40
                jr      c2d_do
c2d_scr1:
                ld      hl,(T32NAM)
                ld      de,32           ; text32
;                jr      c2d_do
c2d_do:

                ld      a,(CSRY)
                cp      2
                jr      c,c2d_add_skip
                dec     a
                ld      b,a
c2d_add_lp:
                add     hl,de
                djnz    c2d_add_lp
c2d_add_skip:
                ld      a,(CSRX)
                cp      2
                jr      c,c2d_add_skip2
                dec     a
                ld      e,a
                add     hl,de

c2d_add_skip2:

                ld      d,h
                ld      e,l

                pop     hl
                pop     bc
                pop     af
                ret


;-----------------------------------
; RegDEからカーソル位置へ (CSRX,CSRY)
de2curs:
                push    af
                push    bc
                push    de
                push    hl

                ex      de,hl

                xor     a
                ld      hl,SCRMOD
                cp      (hl)
                jr      nz,d2c_scr1
                ld      de,(TXTNAM)
                ld      bc,40
                jr      d2c_do
d2c_scr1:
                ld      de,(T32NAM)
                ld      bc,32           ; text32
;                jr      d2c_do
d2c_do:
                inc     a
                ld      c,a
                jr      under_chk

d2c_sub_lp:
                and     a
                sbc     hl,bc
                inc     e
under_chk:
                xor     a
                cp      h
                jr      nz,d2c_sub_lp
                ld      a,l
                cp      40
                jr      nc,d2c_sub_lp

                ld      a,e
                inc     a
                ld      (CSRY),a
                ld      a,l
                inc     a
                ld      (CSRX),a

                pop     hl
                pop     de
                pop     bc
                pop     af
                ret


;----------------------------------

m_rdprim:
                out     (PSL_STAT),a
                ld      e,(hl)
                jr      m_wrprm1
m_wrprim:
                out     (PSL_STAT),a
                ld      (hl),e
m_wrprm1:
                ld      a,d
                out     (PSL_STAT),a
                ret
m_clprim:
                out     (PSL_STAT),a
                ex      af,af'
                call    cl_jp
                ex      af,af'
                pop     af
                out     (PSL_STAT),a
                ex      af,af'
                ret
m_cl_jp:
                jp      (ix)
m_prim_end:
                nop

rdprim:         equ     $F380
wrprim:         equ     rdprim+(m_wrprim-m_rdprim)
clprim:         equ     rdprim+(m_clprim-m_rdprim)
cl_jp:          equ     rdprim+(m_cl_jp-m_rdprim)


;---------------------------
; サブルーチン
;---------------------------
; 000Ch RDSLT
; in ..  A = スロットID , HL = アドレス
rdslt:
                push    hl
                push    af

                ld      a,h
                rlca
                rlca
                and     $03             ; アドレス上位2bit

                ld      l,a             ; L=シフトナンバー
                ld      b,a

                ld      a,$03
                call    rdsft
                cpl
                ld      e,a             ; E= マスク
                ld      a,l             ; A=シフトナンバー
                ld      b,a             ; B=シフトナンバー
                pop     af
                and     $03
                call    rdsft
                ld      b,a             ; B=シフトしたスロット番号
                in      a,(PSL_STAT)
                ld      d,a             ; D=前のプライマリスロット
                and     e
                or      b               ; スロットを変更する
                pop     hl

                call    rdprim
                ld      a,e
                ret

rdsft:
                inc     b
                dec     b
                ret     z
rdsft_lp:
                rlca
                rlca
                djnz    rdsft_lp
                ret

; 0014h WRSLT
; in ..  A = スロットID , HL = アドレス
wrslt:
                push    hl
                push    af

                ld      a,h
                rlca
                rlca
                and     $03             ; アドレス上位 2bit

                ld      l,a             ; L=シフト番号
                ld      b,a

                ld      a,$03
                call    rdsft
                cpl
                ld      e,a             ; E=マスク
                ld      a,l             ; A=シフトナンバー
                ld      b,a             ; B=シフトナンバー
                pop     af
                and     $03
                call    rdsft
                ld      b,a             ; B=シフトされたスロット
                in      a,(PSL_STAT)
                ld      d,a             ; D=前のプライマリスロット
                and     e
                or      b               ; スロットを変更する
                pop     hl

                call    wrprim
                ret


;-------------------------------------
; 001Ch CALSLT(暫定的な関数)
; in .. IYh(スロット番号),(IX)
cal_slt:
                jp      (ix)

;--------------------------------
; 0020h DCOMPR　16ビット比較
; in .. hl,de= 数値
wordcomp:
                ld      a,h
                cp      d
                ret     nz
                ld      a,l
                cp      e
                ret


;--------------------------------
; 0024h ENASLT
; in .. hl=アドレス,a=スロット番号
; A = FxxxEESS
; RegA 詳細
; F = 拡張スロットのフラグ
; E = 拡張スロット番号
; S = スロット番号
; Dest. AF,BC,DE,DI

enaslt:

; A=(A >> 6)&0x3
                di
                push    hl

                ld      l,a             ; L = FxxxEEPP

                and     $03             ; A = 000000PP
                ld      b,a
                ld      a,$AB
psl_dup_lp:
                add     a,$55
                dec     b
                jp      p,psl_dup_lp

                ld      d,a             ; D = PP PP PP PP

                ld      a,h
                rlca
                rlca
                and     $03

                ld      h,a             ; H = アドレス上位 2bit

                ld      b,a

                ld      a,$C0
page_msk_lp:
                rlca
                rlca
                dec     b
                jp      p,page_msk_lp

                ld      e,a             ; E = 00 00 11 00(ページマスク)
                cpl
                ld      c,a             ; C = 11 11 00 11(AND MASK)

                ld      a,d
                and     e
                ld      b,a             ; B = 00 00 PP 00

                ld      a,l
                and     a
                jp      p,chg_psl

;SSL-Change
                rrca
                rrca
                and     $03             ; A = 000000SS

                push    hl
                push    bc

                ld      b,a
                ld      a,$AB
ssl_dup_lp:
                add     a,$55
                dec     b
                jp      p,ssl_dup_lp

                and     e
                ld      b,a             ; B = 00 00 SS 00

                ld      a,d
                and     $C0
                ld      h,a

                in      a,(PSL_STAT)
                ld      l,a
                and     $C0
                or      h
                out     (PSL_STAT),a

                ld      a,(SSL_REGS)
                cpl
                and     c
                or      b               ; A = xx xx SS xx ( x = 前の値 )
                ld      c,a
                ld      (SSL_REGS),a

                ld      a,l
                out     (PSL_STAT),a

                ; (SLTTBL + PP) <- RegC

                ld      hl,SLT_TBL

                ld      a,d
                and     $03             ; A = 000000PP

                add     a,l
                ld      l,a             ; L = L + A

                ld      a,h
                adc     a,0
                ld      h,a             ; H = H + Cy

                ld      a,c
                ld      (hl),a

                pop     bc
                pop     hl

chg_psl:
                in      a,(PSL_STAT)
                and     c
                or      b
                out     (PSL_STAT),a

                pop     hl
                ret


;--------------------------------
; 0030h CALLLF
call_lf:
                pop     hl ; スロット情報のアドレス
                ld      a,(hl)
                inc     hl
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                inc     hl
                ex      de,hl
                push    de
                push    hl
                push    af
                call    enaslt
                pop     af
                pop     hl
                jp      (hl)


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
; VRAMアドレスの設定
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
;HL = VRAM アドレス, BC = 長さ , A = データ
vdp_fillmem:
                push    af
                call    vdp_setwrt
lp_u001:
                pop     af
                out     (VDP_DATA),a
                push    af

                dec     bc
                ld      a,c
                or      b
                jr      nz,lp_u001
                pop     af
                ret

;--------------------------------
wrt_vdp:
; 0047h WRTVDP
;in:B = VDP データ , C = レジスタ番号
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
;004Ah VRAM読み出し
rd_vrm:
                call    vdp_setrd
                ex      (sp),ix
                ex      (sp),ix
                in      a,(VDP_DATA)
                ret


;--------------------------------
;004Dh VRAM書き込み
wrt_vrm:
                push    af
                call    vdp_setwrt
                ex      (sp),ix
                ex      (sp),ix
                pop     af
                out     (VDP_DATA),a
                ret

;--------------------------------
;in de bc
;dest. af de bc
vdp_ldirmv:
                call    vdp_setrd
                ex      (sp),ix
                ex      (sp),ix
ldir_lp1:
                in      a,(VDP_DATA)
                ld      (de),a
                inc     de
                dec     bc
                ld      a,c
                or      b
                jr      nz,ldir_lp1
                ret

;--------------------------------
vdp_reg1chk:
                ld      a,(RG1SAV)
                rrca
                rrca
                ld      a,$08
                ret     nc
                ld      a,$20
                ret

;--------------------------------
;in hl de bc
;dest. af de bc
vdp_data_rep:
                ex      de,hl
                call    vdp_setwrt
lp_vd_rep:
                ld      a,(de)
                out     (VDP_DATA),a
                inc     de
                dec     bc
                ld      a,c
                or      b
                jr      nz,lp_vd_rep
                ret
;------------------------------
;screen 5の初期化.
init_sc5:
                ld      a,$05
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; ラッチリセット

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

                call    enascr
                ret

;------------------------------
;VDPをスクリーン7で初期化する。
init_sc7:
                ld      a,$07
                ld      (SCRMOD),a

                in      a,(VDP_STAT)    ; ラッチのリセット

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

                call    enascr
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
                call    enascr
                ret

; HL = テーブルアドレス
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
;HL = テーブルアドレス
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
                ld      h,a             ; H = シフトしたHLの上位。
                pop     af

                or      h
                ld      b,a

                call    wrt_vdp
                pop     hl
                inc     hl
                inc     hl
                inc     c
                ret

;----------------------------------
;005Fh CHGMOD   画面モードの変更
chgmod:
                or      a
                jp      z,init_txt      ; screen 0
                dec     a
                jp      z,init_txt32    ; screen 1
                dec     a
                jp      z,init_grp      ; screen 2
                dec     a
;
;        jp     z,init_mlt   ; screen 3
;
                dec     a
;
;        jp     z,init_sc4   ; screen 4
;
                dec     a
                jp      z,init_sc5   ; screen 5
                dec     a
;        jp      z,init_sc6  ; screen 6
                dec     a
                jp      z,init_sc7   ; screen 7

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

                ld      a,39
                ld      (LINLEN),a

                ret

;--------------------------------
;006Fh INIT32
init_txt32:
                ; screen 1
                ld      a,$01
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


;                ld      a,1
;                ld      (CSRY),a
;                ld      (CSRX),a

                ld      a,29
                ld      (LINLEN),a

                ret

;0062h CHGCLR
;in = 無し
chgclr:
                ld      a,(SCRMOD)
                dec     a
                push    af
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
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

;0044h ENASCR
enascr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                or      $40
                ld      b,a
                ld      c,1
                call    wrt_vdp
                ret

;00?? DISSCR
disscr:
                in      a,(VDP_STAT)    ; reset Latch

                ld      a,(RG1SAV)
                and     $BF
                ld      b,a
                ld      c,1
                call    wrt_vdp
                ret


;--------------------------------
;009Ch  CHSNS
ch_sns:
                push    hl
                push    de
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                call    wordcomp
                jr      z,no_chr
                ld      a,(hl)
                and     a
                pop     de
                pop     hl
                ret
no_chr:
                xor     a
                and     a
                pop     de
                pop     hl
                ret

;---------------------------------
;009Fh  CHGET
ch_get:
                ld      a,$00
                push    hl
                push    de
                push    bc
loop_chget:
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                call    wordcomp
                jr      nz,get_ch
                ei
                halt
                jr      loop_chget
get_ch:
                ld      hl,(PUTPNT)
                ld      bc,(GETPNT)
                ld      a,(bc)
                and     a
                push    af
                ld      bc,39
                ld      hl,(GETPNT)
                inc     hl
                ld      de,(GETPNT)
                ldir
                ld      de,(PUTPNT)
                dec     de
                ld      (PUTPNT),de
                pop     af
                pop     bc
                pop     de
                pop     hl
                ret
;-----------------------------------
;00A2h  CHPUT
;in ... A = キャラクタコード
ch_put:
                push    de
                push    af
                ld      a,(SCRMOD)
                ld      e,a
                ld      a,1
                cp      e
                jr      nc,scr_txt_mode
                pop     af
                pop     de
                ret

scr_txt_mode:
                pop     af
                push    af

                ; CTRL code
                cp      $08
                jp      z,back_spc
                cp      $0D
                jp      z,ctrl_cr
                cp      $0A
                jp      z,ctrl_lf

                ; Charactor code

                call    set_curs
                pop     af
                push    af
                out     (VDP_DATA),a
                ld      a,(LINLEN)
                ld      e,a
                ld      a,(CSRX)
                cp      e
                jr      nc,chput_nx

                ; next point
                inc     a
chput_ret:
                ld      (CSRX),a
                pop     af
                pop     de
                ret
chput_nx:
                ld      a,(CRTCNT)
                ld      e,a
                ld      a,(CSRY)
                cp      e
                jr      nc,chput_scrll
                ld      a,(CSRY)
                inc     a
                ld      (CSRY),a
                ld      a,1
                jr      chput_ret
chput_scrll:
                call    scroll_txt
                ld      a,1
                jr      chput_ret

set_curs:
                call    curs2de
                ex      de,hl
                call    vdp_setwrt
                ex      de,hl
                ret

back_spc:
                ld      a,(CSRX)
                cp      2
                jr      c,skip_bs
                dec     a
                ld      (CSRX),a
                call    set_curs
                xor     a
                out     (VDP_DATA),a
skip_bs:
                pop     af
                pop     de
                ret
; 0Dh CR
ctrl_cr:
                ld      a,1
                ld      (CSRX),a
                pop     af
                pop     de
                ret

; 0Ah LF  ラインフィード
ctrl_lf:
                ld      a,(CRTCNT)
                ld      e,a
                ld      a,(CSRY)
                cp      e
                jr      nc,lf_scroll
                inc     a
                ld      (CSRY),a
                pop     af
                pop     de
                ret
lf_scroll:
                call    scroll_txt
                pop     af
                pop     de
                ret

; スクロールルーチン
scroll_txt:
                push    af
                push    bc
                push    hl
                push    de
                ld      hl,(NAMBAS)
                ld      de,LINWRK
                ld      a,(CRTCNT)
;                ld      (CSRY),a
;                dec     a
                ld      b,a
                ld      a,(SCRMOD)
                and     a
                jr      nz,scroll_n0
                ld      c,40
                jr      scr_loop
scroll_n0:
                dec     a
                jr      nz,scroll_n1
                ld      c,32

scr_loop:
                push    bc
                xor     a
                ld      b,a

                add     hl,bc
                push    hl
                push    de
                push    bc
                call    vdp_ldirmv      ; HL=VRAM,DE=RAM,BC=LENGTH
                pop     bc
                pop     de
                pop     hl

                sbc     hl,bc

                ex      de,hl           ; DE = VRAM,HL = RAM

                push    hl
                push    de
                push    bc
                call    vdp_data_rep
                pop     bc
                pop     de
                pop     hl

                ex      de,hl           ; DE = RAM,HL = VRAM

                add     hl,bc

                pop     bc
                djnz    scr_loop

                ld      a,0
                call    vdp_fillmem

scroll_n1:
                pop     de
                pop     hl
                pop     bc
                pop     af
                ret



;-----------------------
;00C6h  カーソルを設定する
;in ... RegH = X, RegL = Y
curxy:
                ld      a,h
                ld      (CSRY),a
                ld      a,l
                ld      (CSRX),a
                ret

;-----------------------



;--------------------------------------
;0156h  KILBUF  キーバッファをクリア。
kilbuf:
                push    de
                push    bc
                push    af
                ld      hl,KEYBUF
                ld      (PUTPNT),hl

                ld      a,$FF
                ld      hl,OLDKEY
                ld      (hl),a
                ld      de,OLDKEY+1
                ld      bc,21
                ldir
                pop     af
                pop     bc
                pop     de
                ret


;--------------------------------
;0090h GICINIT  音源IC初期化
sound_init:
                ld      e,$00
                ld      a,$08
                call    sound_out
                inc     a
                call    sound_out
                inc     a
                call    sound_out
                inc     a

                ld      e,$B8
                ld      a,$07
                call    sound_out
                ret

;--------------------------------
;0093h in a=reg#,e=data
sound_out:
                di
                out     (PSG_REGS),a
                push    af
                ld      a,e
                out     (PSG_DATA),a
                ei
                pop     af

;サウンドモード用
;                push    af
;                push    de
;                ld      d,a
;                ld      a,(PSG_DBG)
;                or      a
;                call    nz,disp_psg
;                pop     de
;                pop     af

                ret

;--------------------------------
sound_stat:
                out     (PSG_REGS),a
                in      a,(PSG_STAT)
                ret

;--------------------------------
get_slotreg:
                in      a,(PSL_STAT)
                ret

;--------------------------------
set_slotreg:
                out     (PSL_STAT),a
                ret

;--------------------------------
; 013Eh
vdp_stat_in:
                in      a,(VDP_STAT)
                ret

;--------------------------------
;0141h SNSMAT
; in a = キーボードマトリクス行数
;dest AF,C,EI

in_keyboard:
                di
                ld      c,a
                in      a,(GIO_REGS)
                and     $F0
                or      c
                out     (GIO_REGS),a
                in      a,(KBD_STAT)
                ei
                ret

;--------------------------------
;00D5h GTSTCK
; a = InID...
; dest AF,BC,EI
in_joy:
                push    bc
                cp      $00
                jr      nz,joy_stc1

                ld      a,$08
                call    in_keyboard
                rrca
                rrca
                rrca
                rrca
                cpl
                and     $0F             ; 0000RDUL

                push    hl
                ld      hl,joypos_kbd_tbl
                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                pop     hl
                pop     bc
                and     a
                ret
joy_stc1:
;PSG reg 15h
;0J001111
;PSG reg 14h
;00BARLDU
                push    hl
                push    de

                ld      e,0
                dec     a
                jr      z,sel_stc1
                ld      a,$40
                ld      e,a
sel_stc1:
                ld      a,$0F
                di
                call    sound_stat
                ei
                and     $BF
                or      e
                ld      e,a
                ld      a,$0F
                call    sound_out
                ld      a,$0E
                di
                call    sound_stat
                ei
                cpl
                and     $0F             ; 0000RLDU

                ld      hl,joypos_joy_tbl
                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                pop     de
                pop     hl

                pop     bc
                and     a
                ret

joy_end:
                ld      a,$00
                pop     bc
                and     a
                ret

joypos_joy_tbl:
                ;         0   1   2   3   4   5   6   7
                db      $00,$01,$05,$00,$07,$08,$06,$00
                ;         8   9   A   B   C   D   E   F
                db      $03,$02,$04,$00,$00,$00,$00,$00

joypos_kbd_tbl:
                ;         0   1   2   3   4   5   6   7
                db      $00,$07,$01,$08,$05,$06,$00,$00
                ;         8   9   A   B   C   D   E   F
                db      $03,$00,$02,$00,$04,$00,$00,$00


;--------------------------------
;00D8h GTTRIG
;dest AF,BC,EI
;

in_trig:
                cp      $00
                jr      z,kbd_spc
                jr      joy_trig
kbd_spc:
                ld      a,$08
                call    in_keyboard
                and     $01
                jr      z,spc_on
                jr      spc_off
spc_on:
                ld      a,$FF
                ret
spc_off:
                xor     a
                ret

joy_trig:
                di
                dec     a
                push    de
                ld      e,$00
                ld      b,a
                and     $01
                jr      z,sel_trig1
                ld      e,$40
sel_trig1:
                ld      a,$0F
                call    sound_stat
                and     $BF
                or      e
                ld      e,a
                ld      a,$0F
                call    sound_out

                ld      a,b
                ld      b,$10
                and     $02
                jr      z,istrg_a
                ld      b,$20
istrg_a:
                ld      a,$0E
                di
                call    sound_stat
                ei
                pop     de
                and     b
                jr      z,trig_on
                jr      trig_off
trig_on:
                ld      a,$FF
                ret
trig_off:
                xor     a
                ret


;------------------
; interrupt routine code
;------------------

int_start:
;デバッグ用
;                push    hl
;                ld  hl,$3232
;                ex (sp),hl

                push    hl
                push    de
                push    bc
                push    af
                exx
                ex      af,af'
                push    hl
                push    de
                push    bc
                push    af
                push    iy
                push    ix

                call    H_KEYI
                in      a,(VDP_STAT)
                or      a
                jp      p,int_end

                call    H_TIMI

                ld      hl,(JIFFY)
                inc     hl
                ld      (JIFFY),hl

                ei
                ld      (STATFL),a      ; ステータス保存

                xor     a
                ld      (CLIKFL),a
                call    in_trig
                cpl
                and     $01
                ld      ($F3E8),a
                call    old_key
                call    key_in

int_end:
                pop     ix
                pop     iy
                pop     af
                pop     bc
                pop     de
                pop     hl
                exx
                ex      af,af'
                pop     af
                pop     bc
                pop     de
                pop     hl
                ei
                ret

;--------------------------------
; 0066h NMI割り込み
nmi_int:
                call    H_NMI
                retn

;--------------------------------
old_key:
                ld      de,NEWKEY
                ld      hl,OLDKEY
                ld      b,$0B
oldkey_lp:
                ld      a,(de)
                ld      (hl),a
                inc     de
                inc     hl
                djnz    oldkey_lp
                ret

;--------------------------------
; キーボード入力をバッファに取り込む
key_in:
                in      a,(GIO_REGS)
                and     $F0
                ld      c,a
                ld      b,$0B
                ld      hl,NEWKEY
key_in_lp:
                ld      a,c
                out     (GIO_REGS),a
                in      a,(KBD_STAT)
                ld      (hl),a
                inc     hl
                inc     c
                djnz    key_in_lp
                call    key_chk
                ret

;--------------------------------
; キーコードチェックルーチン
; 割り込みから呼び出される。
;
key_chk:
                ld      ix,OLDKEY
                ld      de,NEWKEY
                ld      a,($FBEB)
                rrca
                jr      nc,code_shift
                ld      hl,scode_tbl
                jr      scan_start
code_shift:
                ld      hl,scode_tbl_shift
scan_start:
                ld      b,$0B
key_chk_lp:
                ld      a,(de)
                cpl
                and     (ix+0)
                ld      c,$08
key_bit_lp:
                rrca
                jr      c,push_pnt
                inc     hl
                dec     c
                jr      nz,key_bit_lp
                inc     ix
                inc     de
                djnz    key_chk_lp
                ret
push_pnt:
                push    hl
                push    de
                push    bc
                ld      a,(hl)
                and     a

                ; RegAが表示可能キャラクタでなければストックしない。

                jr      z,pnt_flow
                ld      c,a
                ld      de,LIMPNT
                ld      hl,(PUTPNT)
                call    wordcomp
                jr      nc,pnt_flow
                ld      a,c
                ld      (hl),a
                inc     hl
                ld      (PUTPNT),hl
pnt_flow:
                pop     bc
                pop     de
                pop     hl
                ret

key_int:
                ld      hl,NEWKEY
                ld      bc,$0006
                add     hl,bc
                ld      a,(hl)
                and     $01
                jr      nz,ki_end
                call    dbg_reg
ki_end:
                ret

;-------------------
                ds      $1000 - $

                ld      a,$82
                out     (PPI_REGS),a

                ld      a,$C9
                ld      (H_KEYI),a
                ld      (H_TIMI),a

                ei

hang_up_mode:
                ld      a,$06
                call    in_keyboard
                bit     0,a
                jp      z,debug_mode

                halt

                jr      hang_up_mode

                ds      $1020 - $
stack_error:
                call    H_STKE
                ld      de,str_stack_error
                jp      print_error

;------------------------------------
;ベーシック呼び出し
call_basic_intr:
                ld      de,str_no_basic_intr
                jp      print_error

;------------------------------------
;エラー表示
;in DE=メッセージのアドレス。

print_error:
                in      a,(VDP_STAT) ; reset Latch
                ld      hl,vdp_bios
                ld      b,$0C
                ld      c,VDP_ADDR
                otir

                ld      bc,$0800
lp_clearmem:
                xor     a
                out     (VDP_DATA),a
                dec     bc
                ld      a,b
                or      c
                jr      nz,lp_clearmem

                ld      hl,B_Font
                ld      bc,$0800
lp_fontset:
                ld      a,(hl)
                out     (VDP_DATA),a
                inc     hl
                dec     bc
                ld      a,b
                or      c
                jr      nz,lp_fontset

;set cursor to (0,0)
                ld      a,$00
                out     (VDP_ADDR),a
                ld      a,$40
                out     (VDP_ADDR),a

                ld      hl,str_error_prompt

                ld      a,(hl)
lp_errprn:
                out     (VDP_DATA),a
                inc     hl
                ld      a,(hl)
                and     a
                jr      nz,lp_errprn

                ld      a,(de)
lp_strprn:
                out     (VDP_DATA),a
                inc     de
                ld      a,(de)
                and     a
                jr      nz,lp_strprn

                jr      hang_up_mode


;------------------------------------
;ディスクルーチン
;------------------------------------

DISKIO:  equ     $4010

disk_intr:
                ld      hl,str_flist
                call    prn_text

                xor     a
                ld      bc,$01F9
                ld      de,$0000
                ld      hl,$C000
                and     a

                call    DISKIO
                jp      c,disk_error
                ld      hl,($C00B)

                ; セクターサイズ / 0x20(ファイル構造体)

                ld      b,5
shift_adr:
                and     a
                rr      h
                rr      l
                djnz    shift_adr

                ld      ($E100),hl

                ld      a,($C010)
                ld      b,a
                ld      hl,$0001
                ld      de,($C016)
dsk_lp:
                add     hl,de
                djnz    dsk_lp

                ex      de,hl
                xor     a
                ld      bc,$01F9
                ld      hl,$C000
                and     a
                call    DISKIO
                jp      c,disk_error

                ld      ix,$C000

                ld      a,($E100)
                ld      c,a

file_lp:
                ld      a,(ix+0)
                and     a
                jp      z,end_lp
                ld      b,8
name_lp:
                ld      a,(ix+0)
                inc     ix
                call    ch_put
                djnz    name_lp

                ld      a,'.'
                call    ch_put

                ld      b,3
ext_lp:
                ld      a,(ix+0)
                inc     ix
                call    ch_put
                djnz    ext_lp

                ld      de,$0015
                add     ix,de

                ld      hl,str_crlf
                call    prn_text

                dec     c
                jr      nz,file_lp

end_lp:
                jp      hang_up_mode

;        ld      de,str_disk
;        jp      print_error

disk_error:
                push    af
                ld      hl,str_diskerr
                call    prn_text
                pop     af
                ld      b,0
                ld      c,a
                ld      hl,str_de_addr
                add     hl,bc

                ld      e,(hl)
                inc     hl
                ld      d,(hl)

                ex      de,hl
                call    prn_text

                jp      hang_up_mode


;---------------------------------
;システムメッセージ
;---------------------------------

str_proginfo:
                ;       [0123456789012345678901234567890123456789]
                db      "C-BIOS ver 0.17",$0D,$0A
                db      "Copyright (C) 2002-2003 BouKiCHi",$0D,$0A
                db      "Copyright (C) 2003 Reikan",$0D,$0A
                db      $00

str_slot:
                db      $0D,$0A,"Slot:",$00
str_chksum:
                db      $0D,$0A,"CheckSum:",$00
str_isgame:
                db      " Game:",$00

;-------------------------------------
;エラーメッセージ
str_error_prompt:
;
                db      "ERROR:",$00
str_memory_err:
;
                db      "MEMORY NOT FOUND.",$00

str_no_basic_intr:
;
                db      "CALLED NO EXISTANCE BASIC.",$00

str_disk:
;
                db      "CALLED DISK ROUTINE.",$00

str_stack_error:
;
                db      "STACK ERROR.",$00

str_crlf:
                db      $0D,$0A,$00

str_flist:
                db      $0D,$0A,"--- display disk file list ---",$0D,$0A,$00

str_diskerr:
                db      "Disk Error:",$00

str_de_addr:
                dw      str_de_wp
                dw      str_de_nr
                dw      str_de_de
                dw      str_de_se
                dw      str_de_rn
                dw      str_de_wf
                dw      str_de_oe

str_de_wp:
                db      "Write protected",$0D,$0A,$00
str_de_nr:
                db      "Not ready",$0D,$0A,$00
str_de_de:
                db      "Data (CRC) error",$0D,$0A,$00
str_de_se:
                db      "Seek error",$0D,$0A,$00
str_de_rn:
                db      "Record not found",$0D,$0A,$00
str_de_wf:
                db      "Write fault",$0D,$0A,$00
str_de_oe:
                db      "Other Error",$0D,$0A,$00

str_nocart:
                ;       [0123456789012345678901234567890123456789]
                db      $0D,$0A
                db      "        Cartridge not found.",$00

str_run:
                ;       [0123456789012345678901234567890123456789]
                db      $0D,$0A,$0D,$0A
                db      "           Starting...",$00

str_s_test:
                ;       [0123456789012345678901234567890123456789]
                db      $0D,$0A,$0D,$0A
                db      "           SOUND TEST MODE",$00


hex_tbl:
                db      "0123456789ABCDEF",$00
reg_tbl:
                db      "PC: IY: IX: HL: DE: BC: AF: ",$00

;スキャンコードテーブル
scode_tbl:
                db      "01234567"                      ;00
                db      "89-^",$5C,"@[;"                ;01 ($5C = backslash)
                db      ":],./_ab"                      ;02
                db      "cdefghij"                      ;03
                db      "klmnopqr"                      ;04
                db      "stuvwxyz"                      ;05
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;06
                db      $00,$00,$1B,$09,$00,$08,$00,$0D ;07
                db      $20,$00,$00,$00,$1D,$1E,$1F,$1C ;08
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;09
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;0a

scode_tbl_shift:
                db      "0!",$22,"#$%&'"                ;00 ($22 = quote)
                db      "()=~|`{+"                      ;01
                db      "*}<>?_AB"                      ;02
                db      "CDEFGHIJ"                      ;03
                db      "KLMNOPQR"                      ;04
                db      "STUVWXYZ"                      ;05
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;06
                db      $00,$00,$1B,$09,$00,$08,$00,$0D ;07
                db      $20,$00,$00,$00,$1D,$1E,$1F,$1C ;08
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;09
                db      $00,$00,$00,$00,$00,$00,$00,$00 ;0a

vdp_bios:
                db      $00,$80,$70,$81,$00,$82,$01,$84
                db      $F5,$87,$00,$40


;------------
;デバッグ用フックルーチン。

                ds      $3232 - $
debug_test:
                ret

                include "font.asm"

; ????
                ds      $77CD - $
                ret

                ds      $8000 - $
