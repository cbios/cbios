; $Id: sub.asm,v 1.2 2004/11/30 01:06:03 mthuurne Exp $
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

; $013D SETPAG Switches display page.
; Input:   DPPAGE
; Changes: AF
; TODO: Does it do more? Maybe something involving ACPAGE?
                ds      $013D - $,$C9
                ei
                jp      setpag

; $01B5 CHGMDP Set screen mode, initialise palette.
; Input:   A = screen mode
; Changes: all
                ds      $01B5 - $,$C9
                ei
                jp      chgmdp

; End of entry points, catch not-implemented calls.
                ds      $0200 - $,$C9

                include "video.asm"

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
; $01B5 CHGMDP
chgmdp:
                call    chgmod
                ; TODO: Initialise palette.
                ret
                
; Empty space until end of page.                
                ds      $4000 - $
