; $Id: sub.asm,v 1.26 2004/12/30 09:15:30 andete Exp $
; C-BIOS subrom file...
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
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

                include "hardware.asm"
                include "systemvars.asm"
                include "hooks.asm"

                org     $0000
                db      "CD"
                dw      0               ; Initialisation routine.
                dw      0               ; Statement handler.
                dw      0               ; Device handler.

; $0038 Handler for maskable interrupt.
                ds      $0038 - $,$C9
                ; TODO: Write a real handler.
                push    af
                in      a,($99)
                pop     af
                ei
                ret

; $0066 Handler for non-maskable interrupt (not used on MSX).
                ds      $0066 - $,$C9
                call    H_NMI
                retn

; $0069 PAINT   (BASIC)
; $006D PSET    (BASIC)
; $0071 ATRSCN  (BASIC)
; $0075 GLINE   (BASIC)
; $0079 DOBOXF  (BASIC)
; $0081 BOXLIN  (BASIC)

; $0085 DOGRPH  ƒ‰ƒCƒ“•`‰æ
                ds      $0085 - $,$C9
                ei
                jp      dogrph

; $0089 GRPPRT
                ds      $0089 - $,$C9
                ei
                jp      grpprt

; $008D SCALXY
; $0091 MAPXYC
; $0095 READC
; $0099 SETATR
; $009D SETC
; $00A1 TRIGHT
; $00A5 RIGHTC
; $00A9 TLEFTC
; $00AD LEFTC
; $00B1 TDOWNC
; $00B5 DOWNC
; $00B9 TUPC
; $00BD UPC
; $00C1 SCANR
; $00C5 SCANL
; $00C9 NVBXLN
; $00CD NVBXFL

; $00D1 CHGMOD Set screen mode.
                ds      $00D1 - $,$C9
                ei
                jp      chgmod

; $00D5 INITXT Set VDP for 40x24 text mode (SCREEN0).
                ds      $00D5 - $,$C9
                ei
                jp      initxt

; $00D9 INIT32 Set VDP for 32x24 text mode (SCREEN1).
                ds      $00D9 - $,$C9
                ei
                jp      init32

; $00DD INIGRP Set VDP for graphics mode (SCREEN2).
                ds      $00DD - $,$C9
                ei
                jp      inigrp

; $00E1 INIMLT Set VDP for multicolour mode (SCREEN3).
                ds      $00E1 - $,$C9
                ei
                jp      inimlt

; $00E5 SETTXT Set VDP for 40x24 text mode (SCREEN0).
                ds      $00E5 - $,$C9
                ei
                jp      settxt

; $00E9 SETT32 Set VDP for 32x24 text mode (SCREEN1).
                ds      $00E9 - $,$C9
                ei
                jp      sett32

; $00ED SETGRP Set VDP for graphics mode (SCREEN2).
                ds      $00ED - $,$C9
                ei
                jp      setgrp

; $00F1 SETMLT Set VDP for multicolour mode (SCREEN3).
                ds      $00F1 - $,$C9
                ei
                jp      setmlt

; $00F5 CLRSPR Clear sprites.
                ds      $00F5 - $,$C9
                ei
                jp      clrspr

; $00F9 CALPAT Returns address of sprite pattern-table.
                ds      $00F9 - $,$C9
                ei
                jp      calpat

; $00FD CALATR Returns address of sprite attribute-table.
                ds      $00FD - $,$C9
                ei
                jp      calatr

; $0101 GSPSIZ Returns current sprite-size.
                ds      $0101 - $,$C9
                ei
                jp      gspsiz

; $0105 GETPAT
                ds      $0105 - $,$C9
                ei
                jp      getpat

; $0109 WRTVRM
                ds      $0109 - $,$C9
                ei
                jp      nwrvrm                  ; call 16 bit version

; $010D RDVRM
                ds      $010D - $,$C9
                ei
                jp      nrdvrm                  ; call 16 bit version

; $0111 CHGCLR Change colours.
                ds      $0111 - $,$C9
                ei
                jp      chgclr

; $0115 CLS
                ds      $0115 - $,$C9
                ei
                jp      cls

; $0119 CLRTXT
                ds      $0119 - $,$C9
                ei
                jp      clrtxt

; $011D DSPFNK
; $0121 DELLNO
; $0125 INSLNO
; $0129 PUTVRM

; $012D WRTVDP Write to VDP register.
                ds      $012D - $,$C9
                ei
                jp      wrtvdp

; $0131 VDPSTA Read VDP status register.
                ds      $0131 - $,$C9
                ei
                jp      vdpsta

; $0135 KYKLOK
; $0139 PUTCHR

; $013D SETPAG Switches display page.
                ds      $013D - $,$C9
                ei
                jp      setpag

; $0141 INIPLT
                ds      $0141 - $,$C9
                ei
                jp      iniplt

; $0145 RSTPLT
                ds      $0145 - $,$C9
                ei
                jp      rstplt

; $0149 GETPLT
                ds      $0149 - $,$C9
                ei
                jp      getplt

; $014D SETPLT
                ds      $014D - $,$C9
                ei
                jp      setplt

; $0151 PUTSPRT (BASIC)
; $0155 COLOR   (BASIC)
; $0159 SCREEN  (BASIC)
; $015D WIDTHS  (BASIC)
; $0161 VDP     (BASIC)
; $0165 VDPF    (BASIC)
; $0169 BASE    (BASIC)
; $016D BASEF   (BASIC)
; $0171 VPOKE   (BASIC)
; $0175 VPEEK   (BASIC)
; $0179 SETS    (BASIC)

; $017D BEEP
                ds      $017D - $,$C9
                ei
                jp      beep

; $0181 PROMPT
                ds      $0181 - $,$C9
                ei
                jp      prompt

; $0185 SDFSCR Restore screen parameters from RTC
                ds      $0185 - $,$C9
                ei
                jp      sdfscr

; $0189 SETSCR Restore screen parameters from RTC and print welcome message
                ds      $0189 - $,$C9
                ei
                jp      setscr

; $018D SCOPY   (BASIC)

; $0191 BLTVV Copy from VRAM to VRAM
                ds      $0191 - $,$C9
                ei
                jp      bltvv

; $0195 BLTVM Copy from Memory to VRAM
                ds      $0195 - $,$C9
                ei
                jp      bltvm

; $0199 BLTMV Copy from Memory to VRAM
                ds      $0199 - $,$C9
                ei
                jp      bltmv

; $019D BLTVD Copy from Memory to VRAM
                ds      $019D - $,$C9
                ei
                jp      bltvd

; $01A1 BLTDV Copy from Memory to VRAM
                ds      $01A1 - $,$C9
                ei
                jp      bltdv

; $01A5 BLTMD Copy from Memory to VRAM
                ds      $01A5 - $,$C9
                ei
                jp      bltmd

; $01A9 BLTDM Copy from Memory to VRAM
                ds      $01A9 - $,$C9
                ei
                jp      bltdm

; $01AD NEWPAD
                ds      $01AD - $,$C9
                ei
                jp      newpad

; $01B1 GETPUT  (BASIC)

; $01B5 CHGMDP Set screen mode, initialise palette.
                ds      $01B5 - $,$C9
                ei
                jp      chgmdp

; $01BD KNJPRT
                ds      $01BD - $,$C9
                ei
                jp      knjprt

; $01F5 REDCLK
                ds      $01F5 - $,$C9
                ei
                jp      redclk

; $01F9 WRTCLK
                ds      $01F9 - $,$C9
                ei
                jp      wrtclk

; End of entry points, catch non-implemented calls.
                ds      $0200 - $,$C9

                include "util.asm"
                include "debug.asm"
                include "video.asm"

;-------------------------------------
; $0085h DOGRPH
; Function:  Draws a line
; Input:     BC, HL are start coordinates
;            GXPOS, GYPOS are end-coordinates
;            ATRBYT for attribute
;            LOGOPR for logical operator
; Registers: AF
; NOTE: this implementation is still a stub!
dogrph:
                push    hl
                push    af
                ld      hl,dogrph_text
                call    print_debug
                pop     af
                pop     hl
                ret
dogrph_text:    db      "DOGRPH",0

;-------------------------------------
; $0119 CLRTXT
; Function : Clear Text-screen
; Registers: All
; NOTE: this implementation is still a stub!
clrtxt:
                push    hl
                push    af
                ld      hl,clrtxt_text
                call    print_debug
                pop     af
                pop     hl
                jp      cls
clrtxt_text:    db      "CLRTXT",0


;-------------------------------------
; $013D SETPAG
; Input:   DPPAGE
; Changes: AF
; TODO: Does it do more? Maybe something involving ACPAGE?
setpag:
; TODO: This is valid for SCREEN5, but what about other modes?
                ld      a,(DPPAGE)
                rrca
                rrca
                rrca
                or      $1F
                push    bc
                ld      b,a             ; B = R#2 data
                ld      c,2
                call    wrtvdp          ; write VDP R#2
                pop     bc
                ret

;-------------------------------------
; $0141 INIPLT
; Function:  Initialises the palette (current palette is saved in VRAM)
; Registers: AF, BC, DE
iniplt:
                push    hl
                call    palette_vram
                call    nsetwr
                ld      bc,16
                call    wrtvdp          ; set palette index
                ld      b,32
                ld      hl,palette_vram_init
iniplt_loop:    ld      a,(hl)
                out     (VDP_DATA),a
                inc     hl
                out     (VDP_PALT),a
                djnz    iniplt_loop
                pop     hl
                ret

;-------------------------------------
; $0145 RSTPLT
; Function:  Restore palette from VRAM
; Registers: AF, BC, DE
rstplt:
                push    hl
                call    palette_vram
                call    nsetrd
                pop     hl
                ld      bc,16
                call    wrtvdp          ; set palette index
                ld      b,32
rstplt_loop:    in      a,(VDP_DATA)
                out     (VDP_PALT),a
                djnz    rstplt_loop
                ret

;-------------------------------------
; $0149 GETPLT
; Function:  Obtains the colorcodes from the palette
; Input:     A  - Colorcode
; Output:    B  - RRRRBBBB
;            C  - xxxxGGGG
getplt:
                push    af
                call    palette_vram
                pop     af
                add     a,a
                ld      c,a
                ld      b,0
                add     hl,bc
                call    nsetrd
                in      a,(VDP_DATA)
                ld      b,a
                in      a,(VDP_DATA)
                ld      c,a
                ret

;-------------------------------------
; $014D SETPLT
; Sets a palette index to a given RGB value.
; Input:   D = palette index
;          E = xxxxxGGG
;          A = xRRRxBBB
; Changes: AF
setplt:
                push    af
                push    bc
                push    hl
                call    palette_vram
                ld      a,d
                add     a,a
                ld      c,a
                ld      b,0
                add     hl,bc
                call    nsetwr
                pop     hl
                ld      b,d
                ld      c,16
                call    wrtvdp          ; set palette index
                pop     bc
                pop     af
                out     (VDP_PALT),a    ; set red and blue
                out     (VDP_DATA),a
                ld      a,e
                out     (VDP_PALT),a    ; set green
                out     (VDP_DATA),a
                ret
;
; internal - get palette base address from screen mode
; in : SCRMOD
; out: HL = base address
;
palette_vram:
                ld      a,(SCRMOD)
                or      a
                call    z,palette_width
                inc     a
                ld      hl,palette_vram_table
                add     a,a
                add     a,l
                ld      l,a
                ld      a,h
                adc     a,0
                ld      h,a
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a
                ret

palette_width:
                ld      h,a
                ld      a,(LINLEN)
                cp      40
                ld      a,h
                ret     nc
                dec     a
                ret

palette_vram_table:
                dw      $0400           ; SCREEN 0 - WIDTH 40
                dw      $0f00           ; SCREEN 0 - WIDTH 80
                dw      $2020           ; SCREEN 1
                dw      $1b80           ; SCREEN 2
                dw      $2020           ; SCREEN 3
                dw      $1b80           ; SCREEN 4
                dw      $7680           ; SCREEN 5
                dw      $7680           ; SCREEN 6
                dw      $fa80           ; SCREEN 7
                dw      $fa80           ; SCREEN 8

palette_vram_init:
                dw      $000,$000,$611,$733,$117,$327,$115,$627
                dw      $171,$373,$661,$664,$411,$265,$555,$777

;-------------------------------------
; $017D BEEP
; Function : Generates beep
; Registers: All
; NOTE: this implementation is still a stub!
beep:
                push    hl
                push    af
                ld      hl,beep_text
                call    print_debug
                pop     af
                pop     hl
                ret
beep_text:      db      "BEEP",0

;-------------------------------------
; $0181 PROMPT
; Function:  Shows prompt (normally "Ok")
; Registers: All
; NOTE: this implementation is still a stub!
prompt:
                push    hl
                push    af
                ld      hl,prompt_text
                call    print_debug
                pop     af
                pop     hl
                ret
prompt_text:    db      "PROMPT",0

;-------------------------------------
; $0185 SDFSCR
; Function:  Recovers screen-parameters from RTC.
; Input:     F = NC when called from MSX-DOS
;                 C to display function keys
; Registers: All
; NOTE: this implementation is still a stub!
sdfscr:
                push    hl
                push    af
                ld      hl,sdfscr_text
                call    print_debug
                pop     af
                pop     hl
                ret
sdfscr_text:    db      "SDFSCR",0

;-------------------------------------
; $0189 SETSCR
; Function:  Recovers screen parameters from RTC and display welcome message
; Registers: All
; NOTE: this implementation is still a stub!
setscr:
                push    hl
                push    af
                ld      hl,setscr_text
                call    print_debug
                pop     af
                pop     hl
                ret
setscr_text:    db      "SETSCR",0

;
; internal, wait for VDP command to end and execute the next one
;
exec_cmd:
                ld      a,2
                call    vdpsta
                bit     0,a
                jr      nz,exec_cmd

                ld      bc,32 *256+ 17
                call    wrtvdp

                di
                ld      bc,14 *256+ VDP_REGS
                ld      hl,SX
                otir
                ret

blt_clip:
                ld      a,b
                or      c
                scf
                ret     z

                ld      a,d
                or      e
                scf
                ret     z

                push    hl
                ld      hl,256
                ld      a,(SCRMOD)
                and     6                       ; SCREEN 6 or 7
                cp      6
                jr      nz,blt_clip_x           ; SCREEN 5 or 8 => X-max = 256
                inc     h                       ; x-max = 512
blt_clip_x:
                or      a
                sbc     hl,bc
                pop     hl
                ret     c

                push    hl
                ld      hl,212
                or      a
                sbc     hl,de
                pop     hl
                ret

;-------------------------------------
; $0191 BLTVV
; Function:  Copy from VRAM to VRAM
; Input:     SX, SY, DX, DY, NX, NY, ARG_, L_OP
; Registers: All
bltvv:
                ld      bc,(NX)
                ld      de,(NY)
                call    blt_clip
                ret     c

                call    exec_cmd

                ld      a,(hl)
                and     15
                or      $90                     ; LMMM
                out     (c),a
                ei
                or      a
                ret

;-------------------------------------
; $0195 BLTVM
; Function:  Copy from RAM to VRAM
; Input:     SX = address of screen data in RAM
;            DX, DY, NX, NY, ARG_, L_OP
;            NX and NY are required in screen data in RAM
; Registers: All
bltvm:
                ld      hl,(SX)
                ld      c,(hl)                  ; read NX from screen data to BC
                inc     hl
                ld      b,(hl)
                inc     hl
                ld      (NX),bc                 ; store NX

                ld      e,(hl)                  ; read NY from screen data to BC
                inc     hl
                ld      d,(hl)
                inc     hl
                ld      (NY),de                 ; store NY

                call    blt_clip
                ret     c

                cp      8
                ld      de,1 *256+ 0            ; D = number of pixels in a byte
                jr      z,bltvm_cont            ; E = number of bits per pixel (shift)
                cp      6
                ld      de,4 *256+ 2
                jr      z,bltvm_cont
                ld      de,2 *256+ 4

bltvm_cont:
                ld      a,(hl)                  ; read first value to write
                inc     hl
                ld      (CDUMMY),a              ; store first byte

                push    af
                push    hl
                call    exec_cmd

                ld      a,(hl)
                and     15
                or      $b0                     ; LMMC
                out     (c),a
                ei
                pop     hl

                ld      bc,$AD +256* 17
                call    wrtvdp
                pop     af
                ld      c,a

bltvm_loop:
                ld      a,2
                call    vdpsta
                bit     0,a                     ; end of command?
                ret     z
                bit     7,a                     ; transmit ready?
                jr      z,bltvm_loop

bltvm_byte:
                ld      b,d                     ; number of pixels
                push    bc
                xor     a
                ld      b,e                     ; number of bits per pixel
bltvm_pixel:
                rl      c                       ; shift bits into A
                rla
                djnz    bltvm_pixel
                out     (VDP_REGS),a            ; write pixel color
                pop     bc
                djnz    bltvm_byte              ; until the complete byte is done

                ld      c,(hl)                  ; get the next byte
                inc     hl
                jr      bltvm_loop

;-------------------------------------
; $0199 BLTMV
; Function:  Copy from VRAM to RAM
; Input:     SX, SY, NX, NY, ARG_, L_OP
;            NX and NY are written to screen data in RAM
; Registers: All
; NOTE: this implementation is still a stub!
bltmv:
                push    hl
                push    af
                ld      hl,bltmv_text
                call    print_debug
                pop     af
                pop     hl
                ret
bltmv_text:     db      "BLTMV",0

;-------------------------------------
; $019D BLTVD
; Function:  Copy from diskfile to VRAM
; Input:     SX = address to diskfile name
;            DX, DY, NX, NY, ARG_, L_OP
;            NX and NY are required in diskfile
; Registers: All
; NOTE: this implementation is still a stub!
bltvd:
                push    hl
                push    af
                ld      hl,bltvd_text
                call    print_debug
                pop     af
                pop     hl
                ret
bltvd_text:     db      "BLTVD",0

;-------------------------------------
; $01A1 BLTDV
; Function:  Copy from VRAM to diskfile
; Input:     SX, SY, DX, NX, NY, ARG_, L_OP
;            DX = address to diskfile name
;            NX and NY are written to diskfile
; Registers: All
; NOTE: this implementation is still a stub!
bltdv:
                push    hl
                push    af
                ld      hl,bltdv_text
                call    print_debug
                pop     af
                pop     hl
                ret
bltdv_text:     db      "BLTDV",0

;-------------------------------------
; $01A5 BLTMD
; Function:  Copy from diskfile to RAM
; Input:     SX = address to diskfile name
;            DX = base address in RAM
;            DY = end address in RAM
; Registers: All
; NOTE: this implementation is still a stub!
bltmd:
                push    hl
                push    af
                ld      hl,bltmd_text
                call    print_debug
                pop     af
                pop     hl
                ret
bltmd_text:     db      "BLTMD",0

;-------------------------------------
; $01A9 BLTDM
; Function:  Copy from RAM to diskfile
; Input:     SX = base address in RAM
;            SY = end address in RAM
;            DX = address to diskfile name
; Registers: All
; NOTE: this implementation is still a stub!
bltdm:
                push    hl
                push    af
                ld      hl,bltdm_text
                call    print_debug
                pop     af
                pop     hl
                ret
bltdm_text:     db      "BLTDM",0

;-------------------------------------
; $01AD NEWPAD
; Function:  Read lightpen, mouse and trackball
; Input:     Access via GETPAD in MSX1BIOS, will be linked to this call
;            Procedure: read device, after that X and Y.
;            A  - 8 tot 19
;            [ 8]   Read lightpen (#FF if available)
;            [ 9]   Read X-position
;            [10]   Read Y-position
;            [11]   Read lightpen-status (#FF if pressed)
; 
;            [12]   Read mouse/trackball in port 1
;            [13]   Read X-offset
;            [14]   Read Y-offset
;            [15]   No function (always #00)
; 
;            [16]   Read mouse/trackball in port 2
;            [17]   Read X-offset
;            [18]   Read Y-offset
;            [19]   No function (always #00)
; 
; Output:    A  - Read value
; Registers: All
; NOTE: this implementation is still a stub!
newpad:
                push    hl
                push    af
                ld      hl,newpad_text
                call    print_debug
                pop     af
                pop     hl
                ret
newpad_text:    db      "NEWPAD",0

;-------------------------------------
; $01B5 CHGMDP
; Input:   A = screen mode
; Changes: all
chgmdp:
                call    chgmod
                call    iniplt
                ret

;-------------------------------------
; $01BD KNJPRT
; Function:  Puts Kanji-character on graphical screen (5-8)
; Input:     BC - JIS Kanji-character code
;            A  - Display-mode (0=full, 1=even, 2=odd)
; Registers: AF
; NOTE: this implementation is still a stub!
knjprt:
                push    hl
                push    af
                ld      hl,knjprt_text
                call    print_debug
                pop     af
                pop     hl
                ret
knjprt_text:    db      "KNJPRT",0

;-------------------------------------
; $01F5 REDCLK
; Function:  Read clock-RAM
; Input:     C  - clock-RAM address
;                 xxBBAAAA
;                   ||++++-- address
;                   ++------ Block-number
; Output:    A  - Read value in lowest four bits
; Registers: F
redclk:
                ld      a,13
                out     (RTC_ADDR),a
                ld      a,c
                rrca
                rrca
                rrca
                rrca
                and     3
                or      8
                out     (RTC_DATA),a
                ld      a,c
                and     15
                out     (RTC_ADDR),a
                in      a,(RTC_DATA)
                ret

;-------------------------------------
; $01F9 WRTCLK
; Function:  Write clock-RAM
; Input:     C  - clock-RAM address
;                 xxBBAAAA
;                   ||++++-- address
;                   ++------ Block-number
;            A  - Value to write
; Registers: F
wrtclk:
                push    bc
                ld      b,a
                ld      a,13
                out     (RTC_ADDR),a
                ld      a,c
                rrca
                rrca
                rrca
                rrca
                and     3
                or      8
                out     (RTC_DATA),a
                ld      a,c
                and     15
                out     (RTC_ADDR),a
                ld      a,b
                out     (RTC_DATA),a
                pop     bc
                ret

; Empty space until end of page.                
                ds      $4000 - $

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
