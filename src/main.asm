; $Id: main.asm,v 1.21 2004/12/19 11:23:50 manuelbi Exp $
; C-BIOS main ROM
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

                include "hardware.asm"
                include "systemvars.asm"
                include "hooks.asm"

;-----------------
; デバッグルーチン用メモリ
;-----------------

DISPADDR:       equ     $E010           ; ダンプアドレス用メモリ
LASTSTAC:       equ     $E000
SP_REGS:        equ     $E002

;---------------------
; jump table
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
                ds      $0008 - $
                jp      synchr

;000Ch RDSLT    任意スロットからのメモリ読み込み

                ds      $000C - $
                jp      rdslt

;0010h CHRGTR
                ds      $0010 - $
                jp      chrgtr

;0014h WRSLT    任意スロットへのメモリ書き込み
                ds      $0014 - $
                jp      wrslt
                ret

;0018h OUTDO
                ds      $0018 - $
                jp      ch_put

;001Ch CALSLT   inter slot call routine
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

;0028h GETYPR
                ds      $0028 - $
                jp      getypr

;002D version ID等
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
                jp      initio 

;0041h DISSCR   スクリーンを表示させない。
                ds      $0041 - $
                jp      disscr

;0044h ENASCR   スクリーンを表示させる。
                ds      $0044 - $
                jp      enascr

;---------------
;VDP routines
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
                jp      clrspr

;006Ch INITXT   画面をTEXT1モードに初期化。
                ds      $006C - $
                jp      init_txt

;006Fh INIT32   画面をGRAPHIC1モードに初期化。
                ds      $006F - $
                jp      init_txt32

;0072h INITGRP  画面をGRAPHIC2モードに初期化。

                ds      $0072 - $
                jp      init_grp

;007Eh SETGRP
                ds      $007E - $
                jp      set_grp

;0090h GICINI   音源ICの初期化
                ds      $0090 - $
                jp      sound_init

                ds      $0093 - $
                jp      sound_out

                ds      $0096 - $
                jp      sound_stat

;009Ch CHSNS  .. check key buffer
                ds      $009C - $
                jp      ch_sns

;009Fh CHGET .. キーバッファからデータを得る
                ds      $009F - $
                jp      ch_get

;00A2h CHPUT .. ディスプレイのキャラクタを出力する。
                ds      $00A2 - $
                jp      ch_put

;00C6h POSIT .. カーソル移動。
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

;0135h CHGSND
                ds      $0135 - $
                jp      chgsnd

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

;0141h SNSMAT   キーマトリクスを得る
;snsmat
                ds      $0141 - $
                jp      in_keyboard

;0144h PHYDIO
                ds      $0144 - $
                jp      phydio

;0156h KILBUF   キーボードバッファをクリアする
                ds      $0156 - $
                jp      kilbuf

;0159h CALBAS   ベーシックインタプリタを呼び出す。
                ds      $0159 - $
                jp      call_basic_intr

;015Ch SUBROM   Calls a routine in the subrom.
                ds      $015C - $
                jp      subrom

;015Fh EXTROM   Calls a routine in the subrom.
                ds      $015F - $
                jp      extrom

;0165h CHKNEW   Is the current screen mode a bitmap mode?
                ds      $0165 - $
                jp      chknew

;016Bh BIGFIL   Like FILVRM, but supports 128K of VRAM.
                ds      $016B - $
                jp      bigfil

;016Eh NSETRD   Like SETRD, but supports 128K of VRAM.
                ds      $016E - $
                jp      nsetrd

;0171h NSETWR   Like SETWRT, but supports 128K of VRAM.
                ds      $0171 - $
                jp      nsetwr

;0174h NRDVRM   Like RDVRM, but supports 128K of VRAM.
                ds      $0174 - $
                jp      nrdvrm

;0177h NWRVRM   Like WRTVRM, but supports 128K of VRAM.
                ds      $0177 - $
                jp      nwrvrm

; -------------------
; start up code（リセット時に呼び出される）
; -------------------

                ds      $0200 - $

                include "util.asm"
                include "debug.asm"
                include "video.asm"
                include "logo.asm"

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
                out     (MAP_REG4),a
                inc     a
                out     (MAP_REG3),a
                inc     a
                out     (MAP_REG2),a
                inc     a
                out     (MAP_REG1),a

; memory check, 選択スロットをメモリに書き込む。
; C = primary, B = secondary.
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

; you can write the memory.

;----------------------
; user interface
;----------------------

                ld      hl,$F300
                ld      sp,hl           ; スタックを$F300に。

                call    init_ram

                call    check_expanded                                
                call    chksubpos
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
                ld      a,29
                ld      (LINL32),a
                ld      a,$01
                call    chgmod

                ld      hl,stack_error
                push    hl

                ld      a,($4000)
                cp      'A'
                jr      nz,p3_run
                ld      hl,($4002)      ; カートリッジ開始アドレス。
                jp      (hl)            ; 実行...
p3_run:
                ld      hl,($8002)      ; カートリッジ開始アドレス。
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

                call    init_txt32

                ; Print program info.
                ld      hl,$0101
                call    curxy
                ld      hl,str_proginfo
                call    prn_text

                ; Upload pattern table.
                ld      hl,(CGPBAS)
                ld      bc,8 * logo_patoffset
                add     hl,bc
                ex      de,hl
                ld      hl,logo_patterns
                ld      bc,8 * logo_npatterns
                call    vdp_data_rep

                ; Upload colour table.
                ld      hl,(T32COL)
                ld      bc,logo_patoffset / 8
                add     hl,bc
                call    vdp_setwrt
                ld      b,10
                ld      a,$F1
plot_logo_col:
                out     (VDP_DATA),a
                djnz    plot_logo_col
                ld      a,$E1
                out     (VDP_DATA),a
                ld      a,$E1
                out     (VDP_DATA),a

                ; Upload name table.
                ld      hl,(NAMBAS)
                ld      bc,logo_namoffset
                add     hl,bc
                ex      de,hl
                ld      hl,logo_names
                ld      b,logo_namheight
plot_logo_nam:
                push    bc
                push    hl
                push    de
                ld      bc,logo_namwidth
                call    vdp_data_rep
                pop     hl              ; value of DE
                ld      bc,32
                add     hl,bc
                ex      de,hl
                pop     hl              ; value of HL
                ld      bc,logo_namwidth
                add     hl,bc
                pop     bc
                djnz    plot_logo_nam

                ; Select 16x16 sprites.
                ld      a,(RG1SAV)
                or      $02
                ld      b,a
                ld      c,$01
                call    wrt_vdp

                ; Upload sprite pattern table.
                ld      hl,logo_spritepat
                ld      de,(PATBAS)
                ld      bc,32
                call    vdp_data_rep

                ; Upload sprite attribute table.
                ld      hl,logo_spriteattr
                ld      de,(ATRBAS)
                ld      bc,8
                call    vdp_data_rep

                ret


;----------------------------
start_cartprog:
; スロット上にカートリッジが存在したなら、
; そのスロットのプログラムを実行する。

                ld      hl,$1201
                call    curxy

                ld      a,($4000)
                cp      'A'
                jr      z,start_cartprog_found
                ld      a,($8000)
                cp      'A'
                jr      z,start_cartprog_found

                ld      hl,str_nocart
                call    prn_text

                jp      hang_up_mode

start_cartprog_found:
                ld      hl,str_slot
                call    prn_text

                call    get_slotreg
                rrca
                rrca
                and     $03
                ld      d,$00
                ld      e,a
                add     a,'0'
                call    ch_put
                ld      hl,EXP_TBL
                add     hl,de
                bit     7,(hl)
                jr      z,start_cartprog_notexp
                ld      a,'.'
                call    ch_put
                ld      hl,SLT_TBL
                add     hl,de
                ld      a,(hl)
                rrca
                rrca
                and     $03
                add     a,'0'
                call    ch_put
start_cartprog_notexp:

                ld      b,120           ; 2sec wait (1 = 1/60sec)
                call    wait_key07      ; wait routine

                bit     5,a
                jp      z,sh_keyboard
                bit     0,a
                jp      z,debug_mode

                ld      hl,str_crlf
                call    prn_text

                ld      hl,str_run
                call    prn_text

                ret

; ------
; BIOS debug routine
; ------
; HL ... address of NEWKEY.
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
                ld      bc,$0010
                jr      up_addr
on_up:
                ld      bc,$0010
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
                ld      a,32            ; Set to 29 after splash screen.
                ld      (LINL32),a
                ;TODO: Rely on call to INIT32 instead.
                ld      a,(LINL32)
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
; Check which slots are expanded.
; Initialises EXP_TBL for all 4 slots.
check_expanded:
                ; Prepare to iterate over slots [0..3].
                di
                ld      hl,EXP_TBL
                in      a,(PSL_STAT)
                ld      d,a             ; D = saved value from port $A8
                and     $3F
                ld      c,a
check_expanded_lp:
                out     (PSL_STAT),a
                ld      a,(SSL_REGS)
                cpl
                ld      e,a             ; E = saved SSL value

                ; Test whether $0x is read back as complement.
                and     $0F
                ld      (SSL_REGS),a
                ld      b,a
                ld      a,(SSL_REGS)
                cpl
                cp      b
                jr      nz,check_expanded_not

                ; Test whether $5x is read back as complement.
                ld      a,e
                and     $0F
                or      $50
                ld      (SSL_REGS),a
                ld      b,a
                ld      a,(SSL_REGS)
                cpl
                cp      b
                jr      nz,check_expanded_not

                ; SSL register present -> slot expanded.
                ld      b,$80
                ld      a,e             
                jr      check_expanded_next
check_expanded_not:
                ; SSL register present -> slot expanded.
                ld      b,$00
                ld      a,e             ; E = saved SSL value
                cpl                     ; not SSL -> back to original
check_expanded_next:
                ld      (SSL_REGS),a
                ld      a,d             ; D = saved value from port $A8
                out     (PSL_STAT),a
                ld      (hl),b
                inc     hl
                ; Next slot.
                ld      a,c
                add     a,$40
                ld      c,a
                jr      nc,check_expanded_lp

                ei
                ret

;----------------------
;サブロム位置の検出
chksubpos:
                
                ld      bc,$0400
                ;ld     de,$fa00        ; debug
                ld      hl,EXP_TBL
pri_subpos_loop:
                push    bc
                push    hl
                ld      a,c
                or      (hl)
                bit     7,a
                jr      nz,pri_subpos_call

                call    chk_subpos
                jr      pri_subpos_next
pri_subpos_call:
                call    sub_subpos
pri_subpos_next:
                pop     hl
                pop     bc
                ret     c
                inc     hl
                inc     c
                djnz    pri_subpos_loop

                xor     a
                ld      (EXBRSA),a
                ret

sub_subpos:
                ld      b,4
sub_subpos_loop:
                push    bc
                call    chk_subpos
                pop     bc
                ret     c

                add     a,4
                djnz    sub_subpos_loop
                ret

chk_subpos:
                ld      c,a
                ld      (EXBRSA),a
                ;ld     (de),a          ; debug
                ;inc    de              ; debug

                ld      hl,0
                call    rd_subpos
                ;ld     (de),a          ; debug
                ;inc    de              ; debug
                cp      "C"
                jr      nz,chk_subpos_notfound

                inc     hl
                call    rd_subpos
                ;ld     (de),a          ; debug
                ;inc    de              ; debug
                cp      "D"

chk_subpos_notfound:
                ld      a,c
                scf
                ret     z
                or      a
                ret

rd_subpos:
                ld      a,c
                push    bc
                ;push   de              ; debug
                push    hl
                call    rdslt
                pop     hl
                ;pop    de              ; debug
                pop     bc
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

;------------------------
; wait routine
;注意、このルーチン呼び出しの際は常にEIであること。
; B = ループ回数
wait_b:
                halt
                djnz    wait_b
                ret

;------------------------
; wait routine
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
;out.. DE = VRAM address
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

; the extensive descriptions were taken with permission from http://map.tni.nl/

;0008h SYNCHR
;Function:  tests whether the character of [HL] is the specified character
;           if not, it generates SYNTAX ERROR, otherwise it goes to CHRGTR
;           (#0010)
;Input:     set the character to be tested in [HL] and the character to be
;           compared next to RST instruction which calls this routine (inline 
;           parameter)
;Output:    HL is increased by one and A receives [HL], When the tested character
;           is numerical, the CY flag is set the end of the statement (00h or
;           3Ah) causes the Z flag to be set
;Registers: AF, HL
;NOTE: this implementation is still a stub!
synchr:
                push    hl
                push    af
                ld      hl,synchr_text
                call    print_debug
                pop     af
                pop     hl
                ret
synchr_text:    db      "SYNCHR",0

; 000Ch RDSLT
; in ..  A = slot ID , HL = address
rdslt:
                push    hl
                push    af
                ld      d,a             ; init D in case call is not made
                and     a               ; expanded slot?
                call    m,select_subslot
                pop     af
                pop     hl
                push    de              ; D = slot ID, E = saved SSL

                push    hl
                push    af
                ld      a,h
                rlca
                rlca
                and     $03             ; アドレス上位2bit

                ld      l,a             ; L=シフトナンバー
                ld      b,a

                ld      a,$FC
                call    rdsft
                ld      e,a             ; E= mask
                ld      b,l             ; B=シフトナンバー
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
                pop     de              ; D = slot ID, E = saved SSL
                push    af
                bit     7,d             ; expanded slot?
                call    nz,restore_subslot
                pop     af
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

; 0010h CHRGTR
;Function:  Gets the next character (or token) of the Basic-text
;Input:     HL - Address last character
;Output:    HL - points to the next character
;           A  - contains the character
;           C  - flag set if it's a number
;           Z  - flag set if it's the end of the statement
;Registers: AF, HL
;NOTE: this implementation is still a stub!
chrgtr:
                push    hl
                push    af
                ld      hl,chrgtr_text
                call    print_debug
                pop     af
                pop     hl
                ret
chrgtr_text:    db      "CHRGTR",0

; 0014h WRSLT
; in ..  A = slot ID , HL = address
wrslt:
                push    hl
                ld      d,a             ; D = slot ID
                push    de
                and     a               ; expanded slot?
                call    m,select_subslot
                pop     bc              ; B = slot ID, C = data
                pop     hl
                push    de              ; D = slot ID, E = saved SSL

                push    hl
                ld      a,h
                rlca
                rlca
                and     $03             ; アドレス上位 2bit

                ld      l,a             ; L=シフト番号
                ld      b,a

                ld      a,$FC
                call    rdsft
                ld      e,a             ; E=mask
                ld      b,l             ; B=シフトナンバー
                ld      a,d
                and     $03             ; A = 000000PP
                call    rdsft
                ld      b,a             ; B=シフトされたスロット
                in      a,(PSL_STAT)
                ld      d,a             ; D=前のプライマリスロット
                and     e
                or      b               ; スロットを変更する
                pop     hl
                ld      e,c             ; E = data
                call    wrprim

                pop     de              ; D = slot ID, E = saved SSL
                push    af
                bit     7,d             ; expanded slot?
                call    nz,restore_subslot
                pop     af
                ret


;-------------------------------------
; 001Ch CALSLT(暫定的な関数)
; in .. IYh(スロット番号),(IX)
cal_slt:
                ex      af,af'
                exx

; Select secondary slot of target:
; Note: This approach fails if target is in page 0 of slot 0.1, 0.2 or 0.3.
; TODO: Put slot 0 specific routine in page 3, on the stack if necessary.
                di
                push    iy
                pop     af              ; A = slot ID: E000SSPP
                push    ix
                pop     hl              ; HL = address to call
                ld      d,a             ; init D in case call is not made
                and     a               ; expanded slot?
                call    m,select_subslot
                push    de              ; D = slot ID, E = saved SSL

; Calculate primary slot select value:
                ld      a,d             ; A = slot ID: E000SSPP
                and     $03
                ld      b,a             ; B = primary slot
                ld      c,$FC           ; C = mask
                ; Calculate page that contains call address.
                push    ix
                pop     af              ; A = high byte call address
                rlca
                rlca
                and     $03             ; A = page
                ; Shift B and C page*2 positions to the left.
                add     a,a
                jr      z,cal_slt_sh2
cal_slt_sh1:
                rlc     b
                rlc     c
                dec     a
                jr      nz,cal_slt_sh1
cal_slt_sh2:

; Select primary slot of target and perform call:
                ld      hl,cal_slt_restore
                push    hl
                in      a,(PSL_STAT)
                push    af
                and     c               ; C = mask (shifted)
                or      b               ; B = primary slot (shifted)
                exx
                jp      clprim

cal_slt_restore:
                ex      af,af'
                exx

; Restore secondary slot:
                di
                pop     de              ; D = slot ID, E = saved SSL
                bit     7,d             ; expanded slot?
                call    nz,restore_subslot

; Done:
                ex      af,af'
                exx
                ret


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
; in .. hl=address, a=slot番号
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
; 0028h GETYPR
;NOTE: this implementation is still a stub!
getypr:
                push    hl
                push    af
                ld      hl,getypr_text
                call    print_debug
                pop     af
                pop     hl
                ret
getypr_text:    db      "GETYPR",0

;--------------------------------
; 0030h CALLLF
call_lf:
                ex      af,af'
                exx
                pop     hl              ; Get data from return address.
                ld      a,(hl)
                inc     hl
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                push    de              ; IX = call address
                pop     ix
                push    af              ; IY = slot
                pop     iy
                push    hl              ; Update return address.
                ex      af,af'
                exx
                jp      cal_slt         ; Perform inter-slot call.

; 003Bh INITIO
;Function:  Initialises the device
;Registers: All
;NOTE: this implementation is still a stub!
initio:
                push    hl
                push    af
                ld      hl,initio_text
                call    print_debug
                pop     af
                pop     hl
                ret
initio_text:    db      "INITIO",0

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
;in ... A = character code
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

; 0Ah LF  line feed
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

; scroll routine
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
                
                ld      e,$80           ; TODO: What about strobe and trigger?
                ld      a,$0F
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

;0135h CHGSND
;Function:  Alternates the 1-bit sound port status
;Input:     A  - #00 to turn off
;            not #00 to turn on
;Registers: AF
;NOTE: this implementation is still a stub!
chgsnd:
                push    hl
                push    af
                ld      hl,chgsnd_text
                call    print_debug
                pop     af
                pop     hl
                ret
chgsnd_text:    db      "CHGSND",0

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

; 0144h PHYDIO
;Function:  Executes I/O for mass-storage media like diskettes
;Input:     B  - Number of sectors
;           C  - Media ID of the disk
;           DE - Begin sector
;           HL - Begin address in memory
;Registers: All
;Remark:    Before the call is called, the Z-flag must be reset, and the
;           execution address which was in HL must be at the last stack address
;           By the way: In minimum configuration only a HOOK is available
;NOTE: this implementation is still a stub!
phydio:
                push    hl
                push    af
                ld      hl,phydio_text
                call    print_debug
                pop     af
                pop     hl
                ret
phydio_text:    db      "PHYDIO",0

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
                ld      e,$80           ; TODO: What about strobe and trigger?
                ld      b,a
                and     $01
                jr      z,sel_trig1
                set     6,e
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
; key code check routine
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


;--------------------------------
; 015Ch Calls a routine in the subrom.
; Input:   IX = call adress, must also be pushed on top of the stack
; Changes: IY, shadow registers
subrom:
                ; TODO: What is that "IX on top of the stack" thing?
                ret


;--------------------------------
; 015Fh Calls a routine in the subrom.
; Input:   IX = call adress
; Changes: IY, shadow registers
extrom:
                ex      af,af'
                ld      a,(EXBRSA)
                push    af
                pop     iy              ; IYH = slot ID
                ex      af,af'
                jp      cal_slt         ; Perform inter-slot call.


;--------------------------------
; Select subslot.
; Input:   A  = slot ID: E000SSPP
;          HL = address which specifies page to select
;               (actually, only the highest 2 bits of H are relevant)
; Output:  D  = slot ID (same as input)
;          E  = original value of secondary slot select register
;          SLTTBL[slot] = new value of secondary slot select register
; Changes: AF, HL, BC
; Note:    Interrupts must be disabled before calling this routine.
select_subslot:
                ; Select primary slot of target in page 3.
                ; Note: Stack is unavailable until primary slot is restored.
                ld      d,a             ; D = E000SSPP
                rrca
                rrca
                ld      e,a             ; E = PPE000SS
                and     $C0
                ld      l,a             ; L = PP000000
                in      a,(PSL_STAT)
                ld      c,a             ; C = saved PSL
                and     $3F
                or      l
                out     (PSL_STAT),a
                ; Shift mask and subslot according to page.
                ld      a,e             ; A = PPE000SS
                and     $03
                ld      l,a             ; L = subslot
                ld      a,h             ; A = high byte of address
                ld      h,$03           ; H = mask
                jr      select_subslot_next
select_subslot_lp:
                add     hl,hl           ; Shift 2 bits to the left.
                add     hl,hl
select_subslot_next:
                sub     $40             ; Subtract 1 page.
                jr      nc,select_subslot_lp
                ld      a,h
                cpl
                ld      h,a
                ; Select secondary slot of target.
                ld      a,(SSL_REGS)
                cpl
                ld      e,a             ; E = saved SSL
                and     h               ; H = mask (shifted)
                or      l               ; L = subslot (shifted)
                ld      (SSL_REGS),a
                ld      l,a             ; L = value written to SSL_REGS
                ; Restore original primary slot in page 3.
                ld      a,c
                out     (PSL_STAT),a
                ; Update SLTTBL.
                ld      a,d
                and     $03             ; A = 000000SS
                ld      c,a
                ld      b,0
                ld      a,l             ; A = value written to SSL_REGS
                ld      hl,SLT_TBL
                add     hl,bc
                ld      (hl),a
                ret


;--------------------------------
; Restore subslot, companion routine to select_subslot.
; Input:   D  = slot ID: E000SSPP
;          E  = original value of secondary slot select register
; Output:  SLTTBL[slot] = original value of secondary slot select register
; Changes: AF, HL, BC
; Note:    Interrupts must be disabled before calling this routine.
restore_subslot:
                ; Select primary slot of target in page 3.
                ; Note: Stack is unavailable until primary slot is restored.
                ld      a,d
                rrca
                rrca
                and     $C0
                ld      b,a             ; B = PP000000
                in      a,(PSL_STAT)
                ld      c,a             ; C = saved PSL
                and     $3F
                or      b
                out     (PSL_STAT),a
                ; Restore secondary slot.
                ld      a,e
                ld      (SSL_REGS),a
                ; Restore original primary slot in page 3.
                ld      a,c
                out     (PSL_STAT),a
                ; Update SLTTBL.
                ld      a,d
                and     $03             ; A = 000000SS
                ld      c,a
                ld      b,0
                ld      hl,SLT_TBL
                add     hl,bc
                ld      (hl),e
                ret


;-------------------
                ds      $2000 - $

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

                ds      $2020 - $
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
;in DE= message address

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
; disk routine
;------------------------------------

DISKIO:         equ     $4010

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

                ; sector size / 0x20(ファイル構造体)

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
; system messages
;---------------------------------

str_proginfo:
                ;       [01234567890123456789012345678901]
                db      "C-BIOS 0.18         cbios.sf.net",$00

str_slot:
                ;       [01234567890123456789012345678901]
                db      "Cartridge found in slot: ",$00

;-------------------------------------
; error messages
str_error_prompt:
;
                db      "ERROR:",$00
str_memory_err:
;
                db      "MEMORY NOT FOUND.",$00

str_no_basic_intr:
;
                db      "CALLED NON EXISTING BASIC.",$00

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
                db      "No cartridge found.",$00

str_run:
                ;       [0123456789012345678901234567890123456789]
                db      $0D,$0A,$0D,$0A
                db      "Starting...",$00

hex_tbl:
                db      "0123456789ABCDEF",$00
reg_tbl:
                db      "PC: IY: IX: HL: DE: BC: AF: ",$00

; scan code table
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

; ????
                ds      $77CD - $
                ret

                ds      $8000 - $
