; $Id: sub.asm,v 1.8 2004/12/19 03:44:38 mthuurne Exp $
; C-BIOS subrom file...
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
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

; 0085h DOGRPH  ƒ‰ƒCƒ“•`‰æ
                ds      $0085 - $,$C9
		jp      dogrph

; 0089h GRPPRT
                ds      $0089 - $,$C9
                jp      grpprt

; $00D1 CHGMOD Set screen mode.
                ds      $00D1 - $,$C9
                ei
                jp      chgmod

; $00F5 CLRSPR Clear sprites.
                ds      $00F5 - $,$C9
                ei
                jp      clrspr

; $013D SETPAG Switches display page.
; Input:   DPPAGE
; Changes: AF
; TODO: Does it do more? Maybe something involving ACPAGE?
                ds      $013D - $,$C9
                ei
                jp      setpag

; 0141h INIPLT
                ds      $0141 - $,$C9
                jp      iniplt

; 0145h RSTPLT
                ds      $0145 - $,$C9
                jp      rstplt

; 0149h GETPLT
                ds      $0149 - $,$C9
                jp      getplt

; 0149D SETPLT
                ds      $014D - $,$C9
                jp      setplt

; 017Dh BEEP
                ds      $017D - $,$C9
                jp      beep

; 0181h PROMPT
                ds      $0181 - $,$C9
                jp      prompt

; 01ADh NEWPAD
                ds      $01AD - $,$C9
                jp      newpad

; $01B5 CHGMDP Set screen mode, initialise palette.
; Input:   A = screen mode
; Changes: all
                ds      $01B5 - $,$C9
                ei
                jp      chgmdp

; 01BDh KNJPRT
                ds      $01BD - $,$C9
                jp      knjprt

; 01F5h REDCLK
                ds      $01F5 - $,$C9
                jp      redclk

; 01F9h WRTCLK
                ds      $01F9 - $,$C9
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
; $0089h GRPPRT
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

;-------------------------------------
; $013D SETPAG
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
                call    wrt_vdp         ; write VDP R#2
                pop     bc
                ret

;-------------------------------------
; $0141 INIPLT
; Function:  Initialises the palette (current palette is saved in VRAM)
; Registers: AF, BC, DE
; NOTE: this implementation is still a stub!
iniplt:
                push    hl
                push    af
                ld      hl,iniplt_text
                call    print_debug
                pop     af
                pop     hl
                ret
iniplt_text:    db      "INIPLT",0

;-------------------------------------
; $0145 RSTPLT
; Function:  Restore palette from VRAM
; Registers: AF, BC, DE
; NOTE: this implementation is still a stub!
rstplt:
                push    hl
                push    af
                ld      hl,rstplt_text
                call    print_debug
                pop     af
                pop     hl
                ret
rstplt_text:    db      "RSTPLT",0

;-------------------------------------
; $0149 GETPLT
; Function:  Obtains the colorcodes from the palette
; Input:     A  - Colorcode
; Output:    B  - RRRRBBBB
;            C  - xxxxGGGG
; NOTE: this implementation is still a stub!
getplt:
                push    hl
                push    af
                ld      hl,getplt_text
                call    print_debug
                pop     af
                pop     hl
                ret
getplt_text:    db      "GETPLT",0

;-------------------------------------
; $014D SETPLT
; Function:  Sets the color code to the palette
; Input:     D  - Colorcode
;            E  - xxxxGGGG
;            A  - RRRRBBBB
; Registers: AF
; NOTE: this implementation is still a stub!
setplt:
                push    hl
                push    af
                ld      hl,setplt_text
                call    print_debug
                pop     af
                pop     hl
                ret
setplt_text:    db      "SETPLT",0

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
chgmdp:
                call    chgmod
                ; TODO: Initialise palette.
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
; NOTE: this implementation is still a stub!
redclk:
                push    hl
                push    af
                ld      hl,redclk_text
                call    print_debug
                pop     af
                pop     hl
                ret
redclk_text:    db      "REDCLK",0

;-------------------------------------
; $01F9 WRTCLK
; Function:  Write clock-RAM
; Input:     C  - clock-RAM address
;                 xxBBAAAA
;                   ||++++-- address
;                   ++------ Block-number
;            A  - Value to write
; Registers: F
; NOTE: this implementation is still a stub!
wrtclk:
                push    hl
                push    af
                ld      hl,wrtclk_text
                call    print_debug
                pop     af
                pop     hl
                ret
wrtclk_text:    db      "WRTCLK",0

; Empty space until end of page.                
                ds      $4000 - $
