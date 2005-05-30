; $Id: main.asm,v 1.108 2005/05/29 02:05:17 mthuurne Exp $
; C-BIOS main ROM
;
; Copyright (c) 2002-2005 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004-2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004-2005 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004-2005 Joost Yervante Damad.  All rights reserved.
; Copyright (c) 2004-2005 Jussi Pitk‰nen.  All rights reserved.
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

                include "systemvars.asm"
                include "hooks.asm"

;-----------------
; A memory address for debug
;-----------------

DISPADDR:       equ     $E010           ; Memory for dump routine
LASTSTAC:       equ     $E000
SP_REGS:        equ     $E002

COMPILE_FONT:   equ     YES

;---------------------
; Jump table
;---------------------

; $0000 CHKRAM
                org     $0000
                di
                jp      chkram

; Pointer to font
; $0004 CGTABL  Base address of the MSX character set in ROM
                ds      $0004 - $
                dw      B_Font

                ds      $0006 - $

; $0006 VDP.DR  Base port address for VDP data read
vdp_dr:         db      VDP_DATA        ; VDP read port
; $0007 VDP.WR  Base port address for VDP data write
vdp_dw:         db      VDP_DATA        ; VDP write port

; $0008 SYNCHR
                ds      $0008 - $
                jp      synchr

; $000C RDSLT   Read memory from an optional slot
                ds      $000C - $
                jp      rdslt

; $0010 CHRGTR
                ds      $0010 - $
                jp      chrgtr

; $0014 WRSLT   Write memory to an optional slot
                ds      $0014 - $
                jp      wrslt

; $0018 OUTDO
                ds      $0018 - $
                jp      outdo

; $001C CALSLT   inter slot call routine
                ds      $001C - $
                jp      calslt

; $0020 DCOMPR  Compare HL to DE
                ds      $0020 - $
                jp      dcompr

; $0024 ENASLT  Change slot
                ds      $0024 - $
                jp      enaslt

; $0028 GETYPR
                ds      $0028 - $
                jp      getypr

; $002B IDBYT1
                ds      $002B - $
idbyt1:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Character set
; | | | |           0 = Japanese, 1 = International (ASCII), 2=Korean
; | +-+-+---------- Date format
; |                 0 = Y-M-D, 1 = M-D-Y, 2 = D-M-Y
; +---------------- Default interrupt frequency
;                   0 = 60Hz, 1 = 50Hz
                db      $21 ; ?? TODO Dutch MSX value: $91
; $002C IDBYT2
idbyt2:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Keyboard type
; | | | |           0 = Japanese, 1 = International (QWERTY)
; | | | |           2 = French (AZERTY), 3 = UK, 4 = German (DIN)
; +-+-+-+---------- Basic version
;                   0 = Japanese, 1 = International
                db      $11 ; ?? TODO Dutch MSX value: $11

; $002D Version ID
romid:
                ds      $002D - $
; version ID
; MSX version number
;  0 = MSX 1
;  1 = MSX 2
;  2 = MSX 2+
;  3 = MSX turbo R
        IF MODEL_MSX = MODEL_MSX1
                db      0
                db      0
        ENDIF
        IF MODEL_MSX = MODEL_MSX2
                db      1
                db      0
        ENDIF
        IF MODEL_MSX = MODEL_MSX2P
                db      2
                db      0
        ENDIF
        IF MODEL_MSX = MODEL_MSXTR
                db      3
; Bit 0: if 1 then MSX-MIDI is present internally (MSX turbo R only)
                db      1
        ENDIF
; Reserved
                db      0

; $0030 CALLF    Call interslot routine(RST30h version)
                ds      $0030 - $
                jp      callf

; $0038 KEYINT   Interrupt routines(RST38,VBlank,Timer...)
                ds      $0038 - $
                jp      keyint

; $003B INITIO  Initialize I/O
                ds      $003B - $
                jp      initio

; $003E INIFNK
                ds      $003E - $
                jp      inifnk

; $0041 DISSCR  Don't make to display screen
                ds      $0041 - $
                jp      disscr

; $0044 ENASCR  Make to display screen
                ds      $0044 - $
                jp      enascr

;---------------
;VDP routines
;---------------

; $0047 WRTVDP
                ds      $0047 - $
                jp      wrtvdp

; $004A RDVRM
                ds      $004A - $
                jp      rdvrm

; $004D WRTVRM
                ds      $004D - $
                jp      wrtvrm

; $0050 SETRD
                ds      $0050 - $
                jp      setrd

; $0053 SETWRT  .. set an address of writting VRAM
                ds      $0053 - $
                jp      setwrt
; $0056 FILVRM
                ds      $0056 - $
                jp      filvrm
; $0059 LDIRMV
                ds      $0059 - $
                jp      ldirmv          ; VRAM -> Memory
; $005C LDIRVM
                ds      $005C - $
                jp      ldirvm          ; Memory -> VRAM

; $005F CHGMOD Change VDP screen mode
                ds      $005F - $
                jp      chgmod

; $0062 CHGCLR
                ds      $0062 - $
                jp      chgclr

; $0066 NMI .. NMI interrupt
                ds      $0066 - $
                jp      nmi

; $0069 CLRSPR  .. clear sprites
                ds      $0069 - $
                jp      clrspr

; $006C INITXT  Initialize display to mode TEXT1
                ds      $006C - $
                jp      initxt

; $006F INIT32  Initialize display to mode GRAPHIC1
                ds      $006F - $
                jp      init32

; $0072 INITGRP Initialize display to mode GRAPHIC2
                ds      $0072 - $
                jp      inigrp

; $0075 INIMLT
                ds      $0075 - $
                jp      inimlt

; $0078 SETTXT
                ds      $0078 - $
                jp      settxt

; $007B SETT32
                ds      $007B - $
                jp      sett32

; $007E SETGRP
                ds      $007E - $
                jp      setgrp

; $0081 SETMLT
                ds      $0081 - $
                jp      setmlt

; $0084 CALPAT
                ds      $0084 - $
                jp      calpat

; $0087 CALATR
                ds      $0087 - $
                jp      calatr

; $008A GSPSIZ
                ds      $008A - $
                jp      gspsiz

; $008D GRPPRT
                ds      $008D - $
                jp      grpprt

; $0090 GICINI  initialize sound IC
                ds      $0090 - $
                jp      gicini
; $0093 WRTPSG
                ds      $0093 - $
                jp      wrtpsg
; $0096 RDPSG
                ds      $0096 - $
                jp      rdpsg

; $0099 STRTMS
                ds      $0099 - $
                jp      strtms

; $009C CHSNS  .. check key buffer
                ds      $009C - $
                jp      chsns

; $009F CHGET .. Get data from keyboard buffer
                ds      $009F - $
                jp      chget

; $00A2 CHPUT .. Output charactor to display
                ds      $00A2 - $
                jp      chput

; $00A5 LPTOUT
                ds      $00A5 - $
                jp      lptout

; $00A8 LPTSTT
                ds      $00A8 - $
                jp      lptstt

; $00AB CNVCHR
                ds      $00AB - $
                jp      cnvchr

; $00AE PINLIN
                ds      $00AE - $
                jp      pinlin

; $00B1 INLIN
                ds      $00B1 - $
                jp      inlin

; $00B4 QINLIN
                ds      $00B4 - $
                jp      qinlin

; $00B7 BREAKX
                ds      $00B7 - $
                jp      breakx

; $00BA ISCNTC
                ds      $00BA - $
                jp      iscntc

; $00BD CKCNTC
                ds      $00BD - $
                jp      ckcntc

; $00C0 BEEP
                ds      $00C0 - $
                jp      beep

; $00C3 CLS
                ds      $00C3 - $
                jp      cls_z

; $00C6 POSIT
                ds      $00C6 - $
                jp      posit

; $00C9 FNKSB
                ds      $00C9 - $
                jp      fnksb

; $00CC ERAFNK
                ds      $00CC - $
                jp      erafnk

; $00CF DSPFNK
                ds      $00CF - $
                jp      dspfnk

; $00D2 TOTEXT
                ds      $00D2 - $
                jp      totext

; $00D5 GTSTCK .. Get joystick infomation
                ds      $00D5 - $
                jp      gtstck

; $00D8 GTTRIG .. Get trigger infomation
                ds      $00D8 - $
                jp      gttrig

; $00DB GTPAD
                ds      $00DB - $
                jp      gtpad

; $00DE GTPDL
                ds      $00DE - $
                jp      gtpdl

; $00E1 TAPION
                ds      $00E1 - $
                jp      tapion

; $00E4 TAPIN
                ds      $00E4 - $
                jp      tapin

; $00E7 TAPIOF
                ds      $00E7 - $
                jp      tapiof

; $00EA TAPOON
                ds      $00EA - $
                jp      tapoon

; $00ED TAPOUT
                ds      $00ED - $
                jp      tapout

; $00F0 TAPOOF
                ds      $00F0 - $
                jp      tapoof

; $00F3 STMOTR
                ds      $00F3 - $
                jp      stmotr

; $00F6 LFTQ
                ds      $00F6 - $
                jp      lftq

; $00F9 PUTQ
                ds      $00F9 - $
                jp      putq

; $00FC RIGHTC
                ds      $00FC - $
                jp      rightc

; $00FF LEFTC
                ds      $00FF - $
                jp      leftc

; $0102 UPC
                ds      $0102 - $
                jp      upc

; $0105 TUPC
                ds      $0105 - $
                jp      tupc

; $0108 DOWNC
                ds      $0108 - $
                jp      downc

; $010B TDOWNC
                ds      $010B - $
                jp      tdownc

; $010E SCALXY
                ds      $010E - $
                jp      scalxy

; $0111 MAPXY
                ds      $0111 - $
                jp      mapxy

; $0114 FETCHC
                ds      $0114 - $
                jp      fetchc

; $0117 STOREC
                ds      $0117 - $
                jp      storec

; $011A SETATR
                ds      $011A - $
                jp      setatr

; $011D READC
                ds      $011D - $
                jp      readc

; $0120 SETC
                ds      $0120 - $
                jp      setc

; $0123 NSETCX
                ds      $0123 - $
                jp      nsetcx

; $0126 GTASPC
                ds      $0126 - $
                jp      gtaspc

; $0129 PNTINI
                ds      $0129 - $
                jp      pntini

; $012C SCANR
                ds      $012C - $
                jp      scanr

; $012F SCANL
                ds      $012F - $
                jp      scanl

; $0132 CHGCAP
                ds      $0132 - $
                jp      chgcap

; $0135 CHGSND
                ds      $0135 - $
                jp      chgsnd

; $0138 RSLREG  Read infomation of primary slot
                ds      $0138 - $
                jp      rslreg

; $013B WSLREG  Write infomation to primary slot
                ds      $013B - $
                jp      wslreg

; $013E RDVDP   Read VDP status
                ds      $013E - $
                jp      rdvdp

; $0141 SNSMAT  Get key matrix
                ds      $0141 - $
                jp      snsmat

; $0144 PHYDIO
                ds      $0144 - $
                jp      phydio

; $0147 FORMAT
                ds      $0147 - $
                jp      format

; $014A ISFLIO
                ds      $014A - $
                jp      isflio

; $014D OUTDLP
                ds      $014D - $
                jp      outdlp

; $0150 GETVCP
                ds      $0150 - $
                jp      getvcp

; $0153 GETVC2
                ds      $0153 - $
                jp      getvc2

; $0156 KILBUF  Clear keyboard buffer
                ds      $0156 - $
                jp      kilbuf

; $0159 CALBAS  Call BASIC interpreter
                ds      $0159 - $
                jp      calbas

; ---------------
; MSX2 BIOS calls
; ---------------

; $015C SUBROM   Calls a routine in the subrom.
                ds      $015C - $
                jp      subrom

; $015F EXTROM   Calls a routine in the subrom.
                ds      $015F - $
                jp      extrom

; $0165 CHKNEW   Is the current screen mode a bitmap mode?
                ds      $0165 - $
                jp      chknew

; $016B BIGFIL   Like FILVRM, but supports 128K of VRAM.
                ds      $016B - $
                jp      bigfil

; $016E NSETRD   Like SETRD, but supports 128K of VRAM.
                ds      $016E - $
                jp      nsetrd

; $0171 NSETWR   Like SETWRT, but supports 128K of VRAM.
                ds      $0171 - $
                jp      nsetwr

; $0174 NRDVRM   Like RDVRM, but supports 128K of VRAM.
                ds      $0174 - $
                jp      nrdvrm

; $0177 NWRVRM   Like WRTVRM, but supports 128K of VRAM.
                ds      $0177 - $
                jp      nwrvrm

; ----------------
; MSX2+ BIOS calls
; ----------------

; $017A RDBTST

; $017D WRBTST

; ---------------------
; MSX TurboR BIOS calls
; ---------------------

; $0180 CHGCPU

; $0183 GETCPU

; $0186 PCMPLY

; $0189 PCMREC

; -------------------
; start up codeÅicall this one when reset)
; -------------------

                ds      $0200 - $

                include "util.asm"
                include "debug.asm"
                include "video.asm"
                include "slot.asm"

; $0000 CHKRAM
; Function : Tests RAM and sets RAM slot for the system
; Registers: All
; Remark   : After this, a jump must be made to INIT, for further initialisation.
chkram:
;for debug
;                ex      (sp),hl
;                ld      (LASTSTAC),hl
;
;                ld      hl,$0000
;                add     hl,sp
;                ld      (SP_REGS),hl
;
;                ld      hl,$F300

;Initialize interface

                ld      a,$82
                out     (PPI_REGS),a
                ld      a,$50
                out     (GIO_REGS),a

;Initialize memory bank
                xor     a
                out     (MAP_REG4),a
                inc     a
                out     (MAP_REG3),a
                inc     a
                out     (MAP_REG2),a
                inc     a
                out     (MAP_REG1),a

; memory check, Write selected slot to memory
; C = primary, B = secondary.
                ld      bc,$0303
chk_wrt_ram:               ; Check page3 RAM
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
                ld      hl,SLTTBL + 3
                ld      (hl),e          ; Expanded slot

; Yes,You can write the memory after the routine.

                ; Select RAM in page 2.
                ; This assumes the same slot used for page 3 also has RAM in
                ; slot 2.
                in      a,(PSL_STAT)
                and     $CF
                ld      c,a
                rrca
                rrca
                and     $30
                or      c
                out     (PSL_STAT),a
                ld      a,(SSL_REGS)
                cpl
                and     $CF
                ld      c,a
                rrca
                rrca
                and     $30
                or      c
                ld      (SSL_REGS),a

;----------------------
; User interface
;----------------------

                ld      hl,$F300
                ld      sp,hl           ; set $F300 to stack pointer

                call    init_ram

                call    check_expanded
        IF VDP != TMS99X8
                call    chksubpos
        ENDIF

                call    init_vdp

                xor     a
                ld      (PSG_DBG),a

                call    gicini

                ei

                call    logo_show
                ld      b,120
                call    wait_b
                ld      a,5
                ld      (BAKCLR),a
                ld      (BDRCLR),a
                ld      a,29
                ld      (LINL32),a
                call    init32
        IF VDP != TMS99X8
                ld      ix,$0141 ; call INIPLT
                call    extrom
        ENDIF

                ld      hl,str_proginfo
                call    prn_text

                call    search_roms
                call    H_STKE
                call    run_basic_roms

                ld      hl,str_nocart
                call    prn_text

                ei
hang:
                halt
                jr      hang

;----------------------
; Search for any extension ROMs and initialize them.
search_roms:
                ; Clear SLTATR.
                ld      hl,SLTATR
                ld      de,SLTATR+1
                ld      bc,4*4*4-1
                ld      (hl),0
                ldir

                ; Search for ROMs.
                ld      hl,EXPTBL
                xor     a               ; A = input for RDSLT
search_roms_lp:
                push    hl
                or      (hl)
search_roms_lp_sub:
                ld      hl,$4000
                call    search_roms_check
                call    z,search_roms_init
                ld      hl,$8000
                call    search_roms_check
                call    z,search_roms_init
                bit     7,a
                jr      z,search_roms_next_slot
                add     a,4             ; Select next subslot.
                bit     4,a
                jr      z,search_roms_lp_sub
search_roms_next_slot:
                pop     hl              ; Select next slot.
                inc     hl
                inc     a
                and     $03
                jr      nz,search_roms_lp
                ret

                ; Helper routine to read two bytes from a given slot.
search_roms_read:
                ld      b,a             ; Save the input for RDSLT in B.
                call    rdslt
                inc     hl
                push    af
                ld      a,b
                call    rdslt
                inc     hl
                ld      d,a
                pop     af
                ld      e,a
                ld      a,b
                ret

                ; Check whether the ROM is present or not.
search_roms_check:
                push    hl
                call    search_roms_read
                ld      hl,$4241        ; "AB"
                call    dcompr          ; ZF is set if the ROM is present.
                ld      a,b
                pop     hl
                ret

                ; Initialize the ROM and set up the related system variables.
search_roms_init:
                ; Output a message to show that a ROM is found.
                push    hl
                push    af
                ld      hl,str_slot
                call    prn_text
                pop     af
                push    af
                ld      b,a
                and     $03
                add     a,'0'
                call    chput
                ld      a,b
                bit     7,b
                jr      z,search_roms_init_skip
                ld      a,'.'
                call    chput
                ld      a,b
                rrca
                rrca
                and     $03
                add     a,'0'
                call    chput
search_roms_init_skip:
                ld      a,$0D
                call    chput
                ld      a,$0A
                call    chput

                pop     af
                pop     hl

                ; Read the initialization address and initialize the ROM.
                inc     hl
                inc     hl
                call    search_roms_address
                jr      z,search_roms_init_statement
                push    de
                pop     ix
                push    af
                pop     iy
                ; Some cartridges have buggy initialisation code.
                ; By postponing the interrupt as long as possible,
                ; there is a better chance they will boot correctly.
                ; For example the game "Koronis Rift" depends on this.
                ei
                halt
                push    af
                push    hl
                call    calslt
                di
                pop     hl
                pop     af

                ; Check if the addresses are valid.
search_roms_init_statement:
                ld      c,0
                call    search_roms_address
                jr      z,search_roms_init_device
                set     5,c
search_roms_init_device:
                call    search_roms_address
                jr      z,search_roms_init_basic
                set     6,c
search_roms_init_basic:
                call    search_roms_address
                jr      z,search_roms_init_variables
                set     7,c

                ; Set up the related system variables.
search_roms_init_variables:
                ld      b,a             ; A = x000sspp
                and     $0C
                ld      e,a
                ld      a,b
                rlca
                rlca
                rlca
                rlca
                and     $30
                or      e               ; A = 00ppss00
                ld      e,a
                ld      a,h
                rlca
                rlca
                and     $03
                or      e               ; A = 00ppssPP
                ld      hl,SLTATR
                ld      d,0
                ld      e,a
                add     hl,de
                ld      (hl),c
                ld      a,b
                ret

                ; Read an address and check whether it is valid or not.
search_roms_address:
                push    bc
                call    search_roms_read
                ld      a,d
                or      e               ; ZF is not set if the address is
                ld      a,b             ; correct.
                pop     bc
                ret

;----------------------
; Run any BASIC roms found.
run_basic_roms:
                ld      hl,SLTATR
                ld      b,64
run_basic_roms_lp:
                ld      a,(hl)
                bit     7,a
                jr      z,run_basic_roms_next
                push    hl
                ld      hl,str_basic
                call    prn_text
                pop     hl
run_basic_roms_next:
                inc     hl
                djnz    run_basic_roms_lp
                ret

;boot_stage2:
;                ; Set up hooks and system vars so NMS8250 disk ROM will try
;                ; to load and execute the boot sector.
;                ld      a,1
;                ld      (DEVICE),a
;                xor     a
;                ; TODO: Find out or invent name for $FB29.
;                ld      ($FB29),a
;
;                ; This is the hook the disk ROM uses for booting.
;                call    H_RUNC
;
; We couldn't boot anything, instead show disk contents.
; TODO: This breaks boot of MG2, so disabled for now.
;                jp      disk_intr
;                ret                     ; goto stack_error


;------------------------
; Initialize RAM

init_ram:

; Initialize workarea
                ld      a,$00
                ld      hl,$F380
                ld      (hl),a
                ld      de,$F381
                ld      bc,$0C7D
                ldir

; initialize hook area with $C9 (assembler code for ret)
                ld      a,$C9           ; ret code
                ld      hl,H_KEYI
                ld      (hl),a
                ld      de,H_KEYI+1
                ld      bc,$024D        ; shouldn't this be $0235 ?
                ldir

; Initialize key matrix
                ld      a,$FF
                ld      hl,OLDKEY
                ld      (hl),a
                ld      de,OLDKEY+1
                ld      bc,21
                ldir

; Initialize Key buffer
                ld      a,$00
                ld      hl,KEYBUF
                ld      (hl),a
                ld      de,KEYBUF+1
                ld      bc,39
                ldir

        IF VDP != TMS99X8
                ld      a,$00
                ld      hl,RG8SAV
                ld      (hl),a
                ld      de,RG8SAV + 1
                ld      bc,15
                ldir
        ENDIF

; Set address pointer
                ld      hl,KEYBUF
                ld      (PUTPNT),hl
                ld      (GETPNT),hl

                ld      hl,$8000
                ld      (BOTTOM),hl     ; Page1 and 2 is ROM,Page3 and 4 is RAM.

                ; I don't know exactly what is stored between $F168 and $F380,
                ; but the disk ROM needs some space there, so I'll just
                ; reserve all of it.
                ld      hl,$F380        ; was $F168, but needs to be changed by disk ROM
                ld      (HIMEM),hl      ; limit of usable memory
                ld      (STKTOP),hl     ; position of BASIC stack

;Transmit RDPRIM to RAM.

                ld      hl,m_rdprim
                ld      de,$F380
                ld      bc,m_prim_end-m_rdprim
                ldir

; Initialize table of screen 0
                ld      hl,$0000
                ld      (TXTNAM),hl
                ld      hl,$0800
                ld      (TXTCGP),hl

; Initialize table of screen 1
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

; Initialize table of screen 2

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

; Initialize table fo screen 3
                ld      hl,$0800
                ld      (MLTNAM),hl
                ld      hl,$0000
                ld      (MLTCGP),hl
                ld      hl,$1B00
                ld      (MLTATR),hl
                ld      hl,$3800
                ld      (MLTPAT),hl

; other settings
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

                ld      a,(SLTTBL)
                ld      (CGPNT),a
                ld      hl,(4)
                ld      (CGPNT+1),hl

                ret

;----------------------
; Check which slots are expanded.
; Initialises EXPTBL for all 4 slots.
check_expanded:
                ; Prepare to iterate over slots [0..3].
                di
                ld      hl,EXPTBL
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

        IF VDP != TMS99X8
;----------------------
;Detect position of subrom
chksubpos:

                ld      bc,$0400
                ld      hl,EXPTBL
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

                ld      hl,0
                call    rd_subpos
                cp      "C"
                jr      nz,chk_subpos_notfound

                inc     hl
                call    rd_subpos
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
                push    hl
                call    rdslt
                pop     hl
                pop     bc
                ret
        ENDIF

;------------------------
; wait routine
; caution,already EI when call the rouine
; B = frequency of loop
wait_b:
                halt
                djnz    wait_b
                ret

;------------------------
;prn_text
; HL = string

prn_text:
                ld      a,(SCRMOD)
                cp      5
                jr      nc,prn_text_graph
prn_text_char:
                ld      a,(hl)
                or      a
                ret     z
                call    chput
                inc     hl
                jr      prn_text_char
prn_text_graph:
                ld      a,(hl)
                or      a
                ret     z
                ld      ix,$0089
                call    extrom
                inc     hl
                jr      prn_text_graph

;--------------------------------
; Determine bytes per line in the current text mode.
; Input:   SCRMOD, LINLEN
; Output:  C = number of bytes per line
; Changes: AF
text_bytes_per_line:
                ld      c,32            ; text32
                ld      a,(SCRMOD)
                or      a
                ret     nz
                ld      c,40            ; text40
                ld      a,(LINLEN)
                cp      41
                ret     c
                ld      c,80            ; text80
                ret

;--------------------------------
; Calculate the VRAM address that corresponds to the current cursor position.
; Input:   CSRX, CSRY
; Output:  HL = VRAM address
; Changes: none
curs2hl:
                push    bc
                push    af

                call    text_bytes_per_line

                ; Calculate left border.
                ld      a,(LINLEN)
                neg
                add     a,c             ; A = bytes_per_line - LINLEN
                inc     a               ; round up
                srl     a               ; A = A / 2
                ld      l,a             ; L = size of left border

                ; Add X coordinate.
                ld      a,(CSRX)
                dec     a               ; from 1-based to 0-based
                add     a,l             ; add border size
                ld      l,a

                ; Convert to 16-bits counters.
                ld      h,0
                ld      b,h

                ; Add Y * bytes_per_line.
                ld      a,(CSRY)
                dec     a               ; from 1-based to 0-based
curs2hl_mult_loop:
                srl     a
                jr      nc,curs2hl_mult_skip
                add     hl,bc
curs2hl_mult_skip:
                sla     c               ; BC = BC * 2
                rl      b
                or      a
                jr      nz,curs2hl_mult_loop

                ; Add base address.
                ld      bc,(NAMBAS)
                add     hl,bc

                pop     af
                pop     bc
                ret


;---------------------------
; Subroutines
;---------------------------

; the extensive descriptions were taken with permission from http://map.tni.nl/

;-------------------------------------
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

;-------------------------------------
; 0010h CHRGTR
;Function:  Gets the next character (or token) of the Basic-text
;Input:     HL - Address last character
;Output:    HL - points to the next character
;           A  - contains the character
;           C  - flag set if it's a number
;           Z  - flag set if it's the end of the statement
;Registers: AF, HL
;NOTE: this implementation is still a stub!
;TODO: call H_CHRG
chrgtr:
;               call    H_CHRG
                push    hl
                push    af
                ld      hl,chrgtr_text
                call    print_debug
                pop     af
                pop     hl
                ret
chrgtr_text:    db      "CHRGTR",0

;-------------------------------------
; $0018 OUTDO
; Function : Output to current outputchannel (printer, diskfile, etc.)
; Input    : A  - PRTFIL, PRTFLG
; Remark   : Used in basic, in ML it's pretty difficult
; TODO     : call H_OUTD
outdo:
;                call    H_OUTD
                jp      chput

;--------------------------------
; $001A ISFLIO
; Function : Tests if I/O to device is taking place
; Output   : A  - #00 if not taking place
;             not #00 if taking place
; Registers: AF
; TODO: call H_ISFL
isflio:
;                call    H_ISFL
                ld      a,(PTRFIL)
                and     a               ; adjust flags
                ret


;--------------------------------
; 0020h DCOMPRÅ@Comparison 16bit
; in .. hl,de= the number
dcompr:
                ld      a,h
                cp      d
                ret     nz
                ld      a,l
                cp      e
                ret

;--------------------------------
; $0028 GETYPR
; Function : Returns Type of DAC
; Input    : VALTYP(F663)
; Output   : C, Z, S
;       C       Z       S       Type    VALTYP
;       low     -       -       double  8
;       high    high    low     string  3
;       high    low     high    integer 2
;       high    low     low     float   4
; Registers: AF
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
; $0030 CALLLF
callf:
                ex      af,af'
                exx
                pop     hl              ; Get data from return address.
                ld      a,(hl)
                inc     hl
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                inc     hl
                push    de              ; IX = call address
                pop     ix
                push    af              ; IY = slot
                pop     iy
                push    hl              ; Update return address.
                ex      af,af'
                exx
                jp      calslt          ; Perform inter-slot call.

;--------------------------------
; $003B INITIO
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
; $003E INIFNK
; Function : Initialises the contents of the function keys
; Registers: All
;NOTE: this implementation is still a stub!
inifnk:
                push    hl
                push    af
                ld      hl,inifnk_text
                call    print_debug
                pop     af
                pop     hl
                ret
inifnk_text:    db      "INIFNK",0

;--------------------------------
; $0099 STRTMS
; Function : Tests whether the PLAY statement is being executed as a background
;            task. If not, begins to execute the PLAY statement
; Registers: All
;NOTE: this implementation is still a stub!
strtms:
                push    hl
                push    af
                ld      hl,strtms_text
                call    print_debug
                pop     af
                pop     hl
                ret
strtms_text:    db      "STRTMS",0


;--------------------------------
; $009C CHSNS
; Function : Tests the status of the keyboard buffer
; Output   : Z-flag set if buffer is filled
; Registers: AF
chsns:
                push    hl
                push    de
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                rst     $20
                pop     de
                pop     hl
                ret

;--------------------------------
; $009F CHGET
; Function : One character input (waiting)
; Output   : A  - ASCII-code of the input character
; Registers: AF
; TODO:    : call H_CHGE
chget:
;               call H_CHGE
                push    hl
                push    de
chget_wait:
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                rst     $20
                jr      nz,chget_char
                ei
                halt
                jr      chget_wait
chget_char:
                ld      a,(hl)          ; HL = (GETPNT)
                push    af
                inc     hl
                ; See comment in keyint (below label key_store).
                ld      a,l
                ; Currently, tniASM doesn't support "&" and SjASM doesn't
                ; support "AND", so we have to hardcode the result.
;                cp      $00FF & (KEYBUF + 40)
                cp      $18
                jr      nz,chget_nowrap
                ld      hl,KEYBUF
chget_nowrap:
                ld      (GETPNT),hl
                pop     af
                pop     de
                pop     hl
                ret

;--------------------------------
; $00A2 CHPUT
; Input:   A = character code
; Changes: none
chput:
                push    af
                ld      a,(SCRMOD)
                cp      2
                jr      nc,chput_exit_af
                pop     af
                push    af
                push    hl
                push    de

                push    af
                ld      a,(ESCCNT)
                or      a
                jp      nz,chput_escape
                pop     af

                ; CTRL code
                cp      $00
                jp      z,chput_exit
                cp      $07
                jp      z,chput_ctrl_beep
                cp      $08
                jp      z,chput_ctrl_left
                cp      $09
                jp      z,chput_ctrl_tab
                cp      $0A
                jp      z,chput_ctrl_lf
                cp      $0B
                jp      z,chput_ctrl_home
                cp      $0C
                jp      z,chput_ctrl_clear
                cp      $0D
                jp      z,chput_ctrl_cr
                cp      $1B
                jp      z,chput_start_escape
                cp      $1C
                jp      z,chput_ctrl_right
                cp      $1D
                jp      z,chput_ctrl_left
                cp      $1E
                jp      z,chput_ctrl_up
                cp      $1F
                jp      z,chput_ctrl_down

                ; Charactor code
                call    chput_character

chput_exit:
                pop     de
                pop     hl
chput_exit_af:
                pop     af
                ret

; Output a character.
chput_character:
                ; Output the character.
                push    af
                call    curs2hl
                call    setwrt
                pop     af
                out     (VDP_DATA),a

                ; Move cursor.
                ld      hl,(CSRY)
                ld      a,(LINLEN)
                inc     h
                cp      h
                jr      nc,chput_character_posit
                ld      h,1
                ld      a,(CRTCNT)
                inc     l
                cp      l
                jr      nc,chput_character_posit
                ld      l,a
                call    posit
                call    scroll_txt
                ret
chput_character_posit:
                call    posit
                ret

chput_start_escape:
                ld      a,$FF
                ld      (ESCCNT),a
                jr      chput_exit

; Generate a beep.
chput_ctrl_beep:
                push    bc
                call    beep
                pop     bc
                jr      chput_exit

; Cursor left.
chput_ctrl_left:
                ld      hl,(CSRY)
                dec     h
                jr      nz,chput_ctrl_left_posit
                ld      a,(LINLEN)
                ld      h,a
                dec     l
                jr      nz,chput_ctrl_left_posit
                inc     l
                ld      h,l
chput_ctrl_left_posit:
                call    posit
                jr      chput_exit

; Fill with space to next tab position.
chput_ctrl_tab:
                ld      a,$20
                call    chput_character
                ld      a,(CSRX)
                and     $07
                cp      $01
                jr      nz,chput_ctrl_tab
                jr      chput_exit

; Line feed.
chput_ctrl_lf:
                ld      hl,(CSRY)
                inc     l
                ld      a,(CRTCNT)
                cp      l
                jr      c,chput_ctrl_lf_scroll
                call    posit
                jp      chput_exit
chput_ctrl_lf_scroll:
                call    scroll_txt
                jp      chput_exit

; Set cursor to home.
chput_ctrl_home:
                ld      hl,$0101
                call    posit
                jp      chput_exit

; Clear screen and set cursor to home.
chput_ctrl_clear:
                push    bc
                call    cls
                pop     bc
                jr      chput_ctrl_home

; Carriage return.
chput_ctrl_cr:
                ld      a,1
                ld      (CSRX),a
                jp      chput_exit

; Cursor right.
chput_ctrl_right:
                ld      hl,(CSRY)
                inc     h
                ld      a,(LINLEN)
                cp      h
                jr      nc,chput_ctrl_right_posit
                ld      h,1
                inc     l
                ld      a,(CRTCNT)
                cp      l
                jr      nc,chput_ctrl_right_posit
                ld      l,a
                ld      a,(LINLEN)
                ld      h,a
chput_ctrl_right_posit:
                call    posit
                jp      chput_exit

; Cursor up.
chput_ctrl_up:
                ld      hl,(CSRY)
                dec     l
                call    nz,posit
                jp      chput_exit

; Cursor down.
chput_ctrl_down:
                ld      hl,(CSRY)
                inc     l
                ld      a,(CRTCNT)
                cp      l
                call    nc,posit
                jp      chput_exit

; Handle escape sequences.
chput_escape:                           ; A = escape state (ESCCNT)
                ld      d,0             ; D = next state
                dec     a
                jr      z,chput_escape_curshapex        ; 1
                dec     a
                jr      z,chput_escape_curshapey        ; 2
                dec     a
                jr      z,chput_escape_locate2          ; 3
                dec     a
                jr      z,chput_escape_locate1          ; 4

                ; Seen: <ESC>
                pop     af
                inc     d               ; D = 1
                cp      'x'
                jr      z,chput_escape_exit
                inc     d               ; D = 2
                cp      'y'
                jr      z,chput_escape_exit
                ld      d,4
                cp      'Y'
                jr      z,chput_escape_exit
                ld      d,0
                cp      'A'
                jr      z,chput_escape_up
                cp      'B'
                jr      z,chput_escape_down
                cp      'C'
                jr      z,chput_escape_right
                cp      'D'
                jr      z,chput_escape_left
                cp      'E'
                jr      z,chput_escape_cls
                cp      'H'
                jr      z,chput_escape_home
                cp      'J'
                jr      z,chput_escape_erase_to_eos
                cp      'j'
                jr      z,chput_escape_cls
                cp      'K'
                jr      z,chput_escape_erase_to_eol
                cp      'L'
                jr      z,chput_escape_insert
                cp      'l'
                jr      z,chput_escape_erase_line
                cp      'M'
                jp      z,chput_escape_delete_line

chput_escape_exit:
                ld      a,d
                ld      (ESCCNT),a
                jp      chput_exit

; Change shape of cursor.
chput_escape_curshapex:
                ; Seen: <ESC>x
                pop     af
                ; TODO: Implement.
                jr      chput_escape_exit

chput_escape_curshapey:
                ; Seen: <ESC>y
                pop     af
                ; TODO: Implement.
                jr      chput_escape_exit

; Locate cursor.
chput_escape_locate1:
                ; Seen: <ESC>Y
                pop     af
                ; TODO: Store this value somewhere.
                ld      d,3
                jr      chput_escape_exit

chput_escape_locate2:
                ; Seen: <ESC>Y<row>
                pop     af
                ; TODO: Implement.
                jr      chput_escape_exit

; Cursor up.
chput_escape_up:
                ld      hl,(CSRY)
                dec     l
                call    nz,posit
                jr      chput_escape_exit

; Cursor down.
chput_escape_down:
                ld      hl,(CSRY)
                inc     l
                ld      a,(CRTCNT)
                cp      l
                call    nc,posit
                jr      chput_escape_exit

; Cursor right.
chput_escape_right:
                ld      hl,(CSRY)
                inc     h
                ld      a,(LINLEN)
                cp      h
                call    nc,posit
                jr      chput_escape_exit

; Cursor left.
chput_escape_left:
                ld      hl,(CSRY)
                dec     h
                call    nz,posit
                jr      chput_escape_exit

; Clear screen and set cursor to home.
chput_escape_cls:
                push    bc
                call    cls
                pop     bc
                jr      chput_escape_home

; Set cursor to home (top left).
chput_escape_home:
                ld      hl,$0101
                call    posit
                jr      chput_escape_exit

; Erase to end of screen.
chput_escape_erase_to_eos:
                ld      hl,(CSRY)
                push    hl
chput_escape_erase_to_eos_loop:
                call    chput_erase
                ld      hl,(CSRY)
                ld      h,1
                inc     l
                ld      (CSRY),hl
                ld      a,(CRTCNT)
                cp      l
                jr      nc,chput_escape_erase_to_eos_loop
                pop     hl
                ld      (CSRY),hl
                jr      chput_escape_exit

; Erase to end of line.
chput_escape_erase_to_eol:
                call    chput_erase
                jr      chput_escape_exit

; Insert a line and scroll the rest of the screen down.
chput_escape_insert:
                ; TODO: Implement.
                jr      chput_escape_exit

; Erase entire line.
chput_escape_erase_line:
                ld      a,(CSRX)
                push    af
                ld      a,1
                ld      (CSRX),a
                call    chput_erase
                pop     af
                ld      (CSRX),a
                jp      chput_escape_exit

; Delete a line and scroll the rest of the screen up.
chput_escape_delete_line:
                ; TODO: Implement.
                jp      chput_escape_exit

; Erase to end of line.
chput_erase:
                push    bc

                ; Calculate the number of bytes to erase.
                ld      a,(CSRX)
                dec     a
                ld      b,a
                ld      a,(LINLEN)
                sub     b
                ld      c,a
                ld      b,0

                ; Calculate the VRAM position.
                call    curs2hl

                ; Fill with space.
                ld      a,$20
                call    filvrm

                pop     bc
                ret

; scroll routine
scroll_txt:
                push    bc
                call    text_bytes_per_line
                ld      b,0             ; BC = bytes_per_line
                ld      hl,(NAMBAS)
                ld      e,l
                ld      d,h             ; DE = dest (VRAM addr)
                add     hl,bc           ; HL = source (VRAM addr)

                ld      a,(CRTCNT)
                dec     a
scr_loop:
                push    bc
                push    af
                ld      a,c
                cp      41
                jr      c,scr_copy
                ; The LINWRK buffer is 40 bytes long, so for 80 bytes per
                ; line we have to copy in 2 steps.
                ld      c,40
                call    copy_line
scr_copy:
                call    copy_line       ; HL = source, DE = dest, BC = length

                pop     af
                pop     bc
                dec     a
                jr      nz,scr_loop

                ex      de,hl
                xor     a
                call    filvrm

                pop     bc
                ret

copy_line:
                push    hl              ; HL = source
                push    de
                push    bc
                ld      de,LINWRK
                call    ldirmv          ; HL = VRAM, DE = RAM, BC = length
                pop     bc
                pop     de
                push    de              ; DE = dest
                push    bc
                ld      hl,LINWRK
                call    ldirvm          ; HL = RAM, DE = VRAM, BC = length
                pop     bc
                pop     hl
                add     hl,bc
                ex      de,hl           ; DE = updated dest
                pop     hl
                add     hl,bc           ; HL = updated source
                ret

;--------------------------------
; $00A5 LPTOUT
; Function : Sends one character to printer
; Input    : A  - ASCII-code of character to send
; Output   : C-flag set if failed
; Registers: F
; NOTE: this implementation is still a stub!
;       currently it always claims success
; TODO: call H_LPTO
lptout:
;               call    H_LPTO
                push    hl
                push    af
                ld      hl,lptout_text
                call    print_debug
                pop     af
                pop     hl
                and     a       ; always state success
                ret
lptout_text:    db      "LPTOUT",0

;--------------------------------
; $00A8 LPTSTT
; Function : Tests printer status
; Output   : A  - #FF and Z-flag reset if printer is ready
;                 #00 and Z-flag set if not ready
; Registers: AF
; NOTE: this implementation is still a stub!
;       currently printer is always ready
; TODO: call H_LPTS
lptstt:
;               call    H_LPTS
                push    hl
                ld      hl,lptstt_text
                call    print_debug
                pop     hl
                ld      a,1     ; just always state
                and     a       ; printer is ready
                ret
lptstt_text:    db      "LPTSTT",0

;--------------------------------
; $00AB CNVCHR
; Function : tests for the graphic header and transforms the code
; Input    : A  - charactercode
;            GRPHED(FCA6): indicates if previous char was an extension code
; Output:                               C-flag  Z-flag  A
;       if byte is extension byte       low     -       1
;       if byte is normal ASCII         high    low     ASCII code
;       if byte is graphical extension  high    high    extension code
;       GRPHED is updated
; Registers: AF
; Note: this implementation is untested!
cnvchr:
                push bc
                ld      b,a
                                        ; was the previous byte the extension byte?
                ld      a,(GRPHED)
                and     a
                ld      a,b
                jr      nz, cnvchr_ext
                                        ; no
                                        ; is the current one an extension byte?
                dec     a
                jr      nz, cnvchr_noext
cnvchr_curext:                          ; yes
                ld      a,b
                and     a               ; reset C-flag
                ld      (GRPHED), A
                pop     bc
                ret
cnvchr_noext:                           ; current code is no extension code,
                                        ; and there was no extension byte
                xor     a               ; resets Z-flag also
                ld      (GRPHED), A
cnvchr_noext2:
                ld      a,b
                scf                     ; set C-flag
                pop     bc
                ret
cnvchr_ext:                             ; previous code was extension byte
                                        ; is the current one an extension byte?
                dec     a
                jr      z, cnvchr_curext
                                        ; previous was extension, current one is not
                xor     a
                ld      (GRPHED),a
                ld      a,b
                                        ; is byte between $40 and $5f ?
                sub     $40
                jr      c, cnvchr_noext2
                cp      $20
                jr      nc, cnvchr_noext2
                                        ; yes, then correct value is in A
                cp      a               ; set Z-flag
                scf                     ; set C-flag
                pop     bc
                ret

;--------------------------------
; $00AE PINLIN
; Function : Stores in the specified buffer the character codes input until the return
;           key or STOP key is pressed
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
; NOTE: this implementation is still a stub!
; TODO: call H_PINL
pinlin:
;               call    H_PINL
                push    hl
                push    af
                ld      hl,pinlin_text
                call    print_debug
                pop     af
                pop     hl
                ret
pinlin_text:    db      "PINLIN",0

;--------------------------------
; $00B1 INLIN
; Function : Same as PINLIN except that AUGFLG (#F6AA) is set
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
; NOTE: this implementation is still a stub!
; TODO: call H_INLI
inlin:
;               call    H_INLI
                push    hl
                push    af
                ld      hl,inlin_text
                call    print_debug
                pop     af
                pop     hl
                ret
inlin_text:     db      "INLIN",0

;--------------------------------
; $00B4 QINLIN
; Function : Prints a questionmark and one space and then calls INLIN
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
; NOTE: this implementation is still a stub!
; TODO: call H_QINL
qinlin:
;               call    H_QINL
                push    hl
                push    af
                ld      hl,qinlin_text
                call    print_debug
                pop     af
                pop     hl
                ret
qinlin_text:    db      "QINLIN",0

;--------------------------------
; $00B7 BREAKX
; Tests status of CTRL-STOP.
; This routine reads the keyboard status from the hardware, so its result
; will be accurate even if interrupts have been disabled for a while.
; Output:  CF set if CTRL-STOP is pressed
; Changes: AF
breakx:
                in      a,(GIO_REGS)
                and     $F0
                or      $06
                out     (GIO_REGS),a
                in      a,(KBD_STAT)
                and     $02             ; check CTRL, also resets CF
                ret     nz
                in      a,(GIO_REGS)
                and     $F0
                or      $07
                out     (GIO_REGS),a
                in      a,(KBD_STAT)
                and     $10             ; check STOP, also resets CF
                ret     nz
                scf
                ret

;--------------------------------
; $00BA ISCNTC
; Function: Test status of STOP or CTRL-STOP; if BASIC is in a ROM (see BASROM),
;           then check for STOP or CTRL-STOP is not done. Otherways:
;       INTLFLG: 0 => no action
;       INTLFLG: 3 => CTRL-STOP pressed => break program, if "STOP-interrupts not on"??
;       INTLFLG: 4 => STOP pressed => wait in ISCNTC till stop pressed again
; Input: INTFLG, BASROM
; Registers: AF
; NOTE: this implementation is still a stub!
iscntc:
                push    hl
                push    af
                ld      hl,iscntc_text
                call    print_debug
                pop     af
                pop     hl
                ret
iscntc_text:    db      "ISCNTC",0

;--------------------------------
; $00BD CKCNTC
; Function : Same as ISCNTC. used in Basic
ckcntc:
                jp      iscntc

;--------------------------------
; $00C0 BEEP
; Function : play a short beep, and reset sound system via GICINI
; Registers: All
; NOTE: this implementation is still a stub!
beep:
; Note: Called by CHPUT; if you need to change more regs than AF, HL, DE, BC
;       then update CHPUT.
                push    hl
                push    af
                ld      hl,beep_text
                call    print_debug
                pop     af
                pop     hl
                ret
beep_text:      db      "BEEP",0

;--------------------------------
; $00C6 POSIT
; Sets cursor position.
; Input:   H = column
;          L = row
; Changes: AF
posit:
                ; Note: this works because CSRX == CSRY + 1
                ld      (CSRY),hl
                ret

;--------------------------------
; $00C9 FNKSB
; Tests whether the function key display is active (FNKFLG),
; if so, displays them, otherwise erases them.
; Input:   FNKFLG (#FBCE)
; Changes: all
; NOTE: This implementation is still a stub!
fnksb:
                push    hl
                push    af
                ld      hl,fnksb_text
                call    print_debug
                pop     af
                pop     hl
                ret
fnksb_text:     db      "FNKSB",0

;--------------------------------
; $00CC ERAFNK
; Erase function key display.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_ERAF
erafnk:
;               call    H_ERAF
                push    hl
                push    af
                ld      hl,erafnk_text
                call    print_debug
                pop     af
                pop     hl
                ret
erafnk_text:    db      "ERAFNK",0

;--------------------------------
; $00CF DSPFNK
; Display function keys.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_DSPF
dspfnk:
;               call    H_DSPF
                push    hl
                push    af
                ld      hl,dspfnk_text
                call    print_debug
                pop     af
                pop     hl
                ret
dspfnk_text:    db      "DSPFNK",0

;--------------------------------
; $00D2 TOTEXT
; Forces the screen to be in the text mode.
; Input: SCRMOD, OLDSCR
; Changes: all
totext:
                ld      a,(SCRMOD)
                cp      2
                ret     c
                ld      a,(OLDSCR)
                call    H_TOTE
                or      a
                jp      z,initxt
                jp      init32

;--------------------------------
; $00E1 TAPION
; Reads the header block after turning the cassette motor on.
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapion:
                push    hl
                push    af
                ld      hl,tapion_text
                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapion_text:    db      "TAPION",0

;--------------------------------
; $00E4 TAPIN
; Read data from the tape.
; Output:  A = data read
; Changes: all
; NOTE: This implementation is still a stub!
tapin:
                push    hl
                push    af
                ld      hl,tapin_text
                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapin_text:     db      "TAPIN",0

;--------------------------------
; $00E7 TAPIOF
; Stops reading from the tape.
; NOTE: This implementation is still a stub!
tapiof:
                push    hl
                push    af
                ld      hl,tapiof_text
                call    print_debug
                pop     af
                pop     hl
                ret
tapiof_text:    db      "TAPIOF",0

;--------------------------------
; $00EA TAPOON
; Turns on the cassette motor and writes the header.
; Input:   A  = zero for short header, non-zero for long header
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapoon:
                push    hl
                push    af
                ld      hl,tapoon_text
                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapoon_text:    db      "TAPOON",0

;--------------------------------
; $00ED TAPOUT
; Writes data to the tape.
; Input:   A  = data to write
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapout:
                push    hl
                push    af
                ld      hl,tapout_text
                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapout_text:    db      "TAPOUT",0

;--------------------------------
; $00F0 TAPOOF
; Stops writing on the tape.
; NOTE: This implementation is still a stub!
tapoof:
                push    hl
                push    af
                ld      hl,tapoof_text
                call    print_debug
                pop     af
                pop     hl
                ret
tapoof_text:    db      "TAPOOF",0

;--------------------------------
; $00F3 STMOTR
; Changes the cassette motor state.
; Input:   A = action: #00 stops motor, #01 starts motor,
;                      #FF inverts current state
; Changes: AF
stmotr:
                push    bc
                ld      b,a
                in      a,(GIO_REGS)
                inc     b
                jr      z,stmotr_inv
                set     4,a
                dec     b
                jr      z,stmotr_set
                res     4,a
                dec     b
                jr      z,stmotr_set
                pop     bc
                ret

stmotr_inv:     xor     16
stmotr_set:     out     (GIO_REGS),a
                pop     bc
                ret

;--------------------------------
; $0090 GICINI  Initialize Sound IC
; Function : Initialises PSG and sets initial value for the PLAY statement
; Registers: All
gicini:
                ld      e,$00
                ld      a,$08
                call    wrtpsg
                inc     a
                call    wrtpsg
                inc     a
                call    wrtpsg
                inc     a

                ld      e,$B8
                ld      a,$07
                call    wrtpsg

                ld      e,$80           ; TODO: What about strobe and trigger?
                ld      a,$0F
                call    wrtpsg

                ret

;--------------------------------
; $0093 WRTPSG
; Function : Writes data to PSG-register
; Input    : A  - PSG register number
;            E  - data write
wrtpsg:
                di
                out     (PSG_REGS),a
                push    af
                ld      a,e
                out     (PSG_DATA),a
                ei
                pop     af

;for sound player mode
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
; $0096 RDPSG
; Function : Reads value from PSG-register
; Input    : A  - PSG-register read
; Output   : A  - value read
rdpsg:
                out     (PSG_REGS),a
                in      a,(PSG_STAT)
                ret

;--------------------------------
; $0135 CHGSND
; Write to the 1-bit sound port.
; Input:   A = zero to set sound state to 0, non-zero to set sound state to 1
; Changes: AF
chgsnd:
                or      a
                ld      a,$0E           ; $0E = command to reset bit 7
                jr      z,chgsnd_write
                inc     a               ; $0F = command to set bit 7
chgsnd_write:
                out     (PPI_REGS),a    ; set/reset bit of port C
                ret

;--------------------------------
; $0138 RSLREG
; Function : Reads the primary slot register
; Output   : A  - for the value which was read
;            33221100
;            ||||||- Pagina 0 (#0000-#3FFF)
;            ||||--- Pagina 1 (#4000-#7FFF)
;            ||----- Pagina 2 (#8000-#BFFF)
;            ------- Pagina 3 (#C000-#FFFF)
; Registers: A
rslreg:
                in      a,(PSL_STAT)
                ret

;--------------------------------
; $013B WSLREG
; Function : Writes value to the primary slot register
; Input    : A  - value value to (see RSLREG)
wslreg:
                out     (PSL_STAT),a
                ret

;--------------------------------
; $013E RDVDP
; Function : Reads VDP status register
; Output   : A  - Value which was read
; Registers: A
rdvdp:
                in      a,(VDP_STAT)
                ret

;--------------------------------
;0141h SNSMAT
; Function : Returns the value of the specified line from the keyboard matrix
; Input    : A  - for the specified line
; Output   : A  - for data (the bit corresponding to the pressed key will be 0)
; Registers: AF
snsmat:
                di
                push bc
                ld      c,a
                in      a,(GIO_REGS)
                and     $F0
                or      c
                out     (GIO_REGS),a
                in      a,(KBD_STAT)
                pop bc
                ei
                ret

;--------------------------------
; $0144 PHYDIO
; Executes I/O for mass-storage media like diskettes.
; All this routine does is call H_PHYD, which should be installed by the main
; disk ROM.
; Input:     B  = number of sectors to save/load
;            C  = media ID of the disk
;            DE = begin sector
;            HL = begin address in memory
; Changes:   all
; Remark:    Before the call is called, the Z-flag must be reset, and the
;            execution address which was in HL must be at the last stack address
phydio:
                call    H_PHYD
                ret

;--------------------------------
; $014A FORMAT
; Initialises mass-storage media like formatting of diskettes.
; All this routine does is call H_FORM, which should be installed by the main
; disk ROM.
; Changes:   all
format:
                call    H_FORM
                ret

;--------------------------------
; $00D5 GTSTCK
; Function : Returns the joystick status
; Input    : A  - Joystick number to test (0 = cursors, 1 = port 1, 2 = port 2)
; Output   : A  - Direction
; Registers: All
gtstck:
                push    bc
                cp      $00
                jr      nz,joy_stc1

                ld      a,$08
                call    snsmat
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
                call    rdpsg
                ei
                and     $BF
                or      e
                ld      e,a
                ld      a,$0F
                call    wrtpsg
                ld      a,$0E
                di
                call    rdpsg
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
                db      $00,$01,$05,$00,$07,$08,$06,$07
                ;         8   9   A   B   C   D   E   F
                db      $03,$02,$04,$03,$00,$01,$05,$00

joypos_kbd_tbl:
                ;         0   1   2   3   4   5   6   7
                db      $00,$07,$01,$08,$05,$06,$00,$07
                ;         8   9   A   B   C   D   E   F
                db      $03,$00,$02,$01,$04,$05,$03,$00


;--------------------------------
; $00D8 GTTRIG
; Function : Returns current trigger status
; Input    : A  - trigger button to test
;            0 = spacebar
;            1 = port 1, button A
;            2 = port 2, button A
;            3 = port 1, button B
;            4 = port 2, button B
; Output   : A  - #00 trigger button not pressed
;                 #FF trigger button pressed
; Registers: All
gttrig:
                cp      $00
                jr      z,kbd_spc
                jr      joy_trig
kbd_spc:
                ld      a,$08
                call    snsmat
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
                call    rdpsg
                and     $BF
                or      e
                ld      e,a
                ld      a,$0F
                call    wrtpsg

                ld      a,b
                ld      b,$10
                and     $02
                jr      z,istrg_a
                ld      b,$20
istrg_a:
                ld      a,$0E
                di
                call    rdpsg
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

;--------------------------------
; $00DB GTPAD
; Function : Returns current touch pad status
; Input    : A  - Touchpad number to test
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpad:
                push    hl
                push    af
                ld      hl,gtpad_text
                call    print_debug
                pop     af
                pop     hl
                ret
gtpad_text:     db      "GTPAD",0

;--------------------------------
; $00DE GTPDL
; Function : Returns currenct value of paddle
; Input    : A  - Paddle number
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpdl:
                push    hl
                push    af
                ld      hl,gtpdl_text
                call    print_debug
                pop     af
                pop     hl
                ret
gtpdl_text:     db      "GTPDL",0

;--------------------------------
; $00F6 LFTQ
; Function : Gives number of bytes in queue
; Output   : A  - length of queue in bytes
; Remark   : Internal use
; NOTE     : This implementation is still a stub!
lftq:
                push    hl
                push    af
                ld      hl,lftq_text
                call    print_debug
                pop     af
                pop     hl
                ret
lftq_text:      db      "LFTQ",0

;--------------------------------
; $00F9 PUTQ
; Function : Put byte in queue
; Remark   : Internal use
; NOTE     : This implementation is still a stub!
putq:
                push    hl
                push    af
                ld      hl,putq_text
                call    print_debug
                pop     af
                pop     hl
                ret
putq_text:      db      "PUTQ",0

;--------------------------------
; $0132 CHGCAP
; Function : Alternates the CAP lamp status
; Input    : A  - #00 is lamp on
;             not #00 is lamp off
; Registers: AF
chgcap:
                or      a
                in      a,(GIO_REGS)
                res     6,a
                jr      nz,chgcap_on
                set     6,a
chgcap_on:      out     (GIO_REGS),a
                ret

;--------------------------------
; $014D OUTDLP
; Function : Printer output
; Input    : A  - code to print
; Registers: F
; Remark   : Differences with LPTOUT:
;            1. TAB is expanded to spaces
;            2. For non-MSX printers, Hiragana is transformed to katakana
;               and graphic characters are transformed to 1-byte characters
;            3. If failed, device I/O error occurs
; TODO     : This implementation is still a stub!
outdlp:
                push    hl
                push    af
                ld      hl,outdlp_text
                call    print_debug
                pop     af
                pop     hl
                ret
outdlp_text:    db      "OUTDLP",0

;--------------------------------
; $0150 GETVCP
; Returns pointer to a variable at offset 2 in a voice structure.
; TODO: find out the purpose of this variable.
; Address  : #0150
; Function : Returns pointer to play queue
; Input    : A  - Channel number
; Output   : HL - Pointer
; Registers: AF
; Remark   : Only used to play music in background
getvcp:
                ld      l,2
                jr      getvc2_a

;--------------------------------
; $0153 GETVC2
; Returns pointer to a given variable in a voice structure.
; Input    : L        - Pointer in play buffer
;            (VOICEN) - Voice structure number
; Output   : HL - Pointer
; Registers: AF
getvc2:
                ld      a,(VOICEN)
getvc2_a:
                push    de
                ld      d,0
                ld      e,l
                ld      hl,VCBA
                add     hl,de
                ld      e,37            ; Size of a structure
getvc2_loop:
                or      a
                jr      z,getvc2_exit
                add     hl,de
                dec     a
                jr      getvc2_loop
getvc2_exit:
                pop     de
                ret

;--------------------------------
; $0156 KILBUF
; Empties the keyboard buffer.
; Changes: HL
kilbuf:
                ld      hl,(GETPNT)
                ld      (PUTPNT),hl
                ret

;------------------
; interrupt routine code
;------------------

keyint:
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
                ld      (STATFL),a      ; save status

                call    H_TIMI

                ld      hl,(JIFFY)
                inc     hl
                ld      (JIFFY),hl

                ; TODO: It seems unsafe to me to already allow interrupts
                ;       while this one is still busy: possible interference
                ;       between two interrupts and also the amount of stack
                ;       space claimed is a lot.
                ;ei

                xor     a
                ld      (CLIKFL),a
                call    gttrig
                cpl
                and     $01
                ld      (TRGFLG),a

                ; TODO: MSX BIOS doesn't scan the full keyboard on every
                ;       interrupt, see SCNCNT (F3F6).
                ; TODO: This can probably be done cheaper as part of the
                ;       key_in routine.
                ld      hl,NEWKEY
                ld      de,OLDKEY
                ld      bc,$0B
                ldir
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
; 0066h NMI interrupt
nmi:
                call    H_NMI
                retn

;--------------------------------
; Get buffer from keyboard input
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

                ld      ix,OLDKEY
                ld      de,NEWKEY
                ld      a,(NEWKEY + 6)
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
                ; TODO: Optimise scanning if no keys are pressed.
                ;       That's the most common case by far.
                ld      c,$08
key_bit_lp:
                rrca
                jr      c,key_store
key_bit_next:
                inc     hl
                dec     c
                jr      nz,key_bit_lp
                inc     ix
                inc     de
                djnz    key_chk_lp
                ret
key_store:
                push    af
                ld      a,(hl)          ; get ASCII value
                and     a               ; dead key?
                jr      z,key_store_end2
                ; Store ASCII value in key buffer.
                ; Since a full buffer is indicated by PUTPNT == GETPNT - 1,
                ; it is always safe to store a character, but if the buffer
                ; is full, PUTPNT cannot be increased.
                push    hl
                ld      hl,(PUTPNT)
                ld      (hl),a
                ; Note: Ashguine 2 has a bug: it puts KEYBUF at FDF0 iso FBF0
                ;       in the name input routine. This writes keys in memory
                ;       reserved for hooks, but since those hooks are only used
                ;       by BASIC, the game doesn't suffer. When PUTPNT reaches
                ;       FE18, it wraps back to FBF0.
                inc     hl
                ld      a,l
;                cp      $00FF & (KEYBUF + 40)
                cp      $18
                jr      nz,key_store_nowrap
                ld      hl,KEYBUF
key_store_nowrap:
                ; Check whether the buffer is full.
                push    de
                ld      de,(GETPNT)
                rst     $20
                pop     de
                jr      z,key_store_end
                ld      (PUTPNT),hl
key_store_end:
                pop     hl
key_store_end2:
                pop     af
                jr      key_bit_next


;--------------------------------
; $015C SUBROM
; Function : Calls a routine in SUB-ROM
; Input    : IX - Address of routine in SUB-ROM
; Output   : Depends on the routine
; Registers: Alternative registers, IY
; Remark   : Use of EXTROM or CALSLT is more convenient.
;            You have to use this routine like this:
;               push    ix
;               jp      subrom
;            The purpose is unclear
subrom:
                call    extrom
                pop     ix
                ret


;--------------------------------
; $015F EXTROM
; Function : Calls a routine in SUB-ROM. Most common way
; Input    : IX - Address of routine in SUB-ROM
; Output   : Depends on the routine
; Registers: Alternative registers, IY
; Remark   : Use: LD IX,address
;                 CALL EXTROM
extrom:
                ex      af,af'
                ld      a,(EXBRSA)
                push    af
                pop     iy              ; IYH = slot ID
                ex      af,af'
                jp      calslt          ; Perform inter-slot call.

;------------------------------------
hang_up_mode:
                di
                halt

;------------------------------------
; Called if the stack underflows.
stack_error:
                call    H_STKE
                ld      de,str_stack_error
                jp      print_error

;------------------------------------
; $0159 CALBAS
; Function : Executes inter-slot call to the routine in BASIC interpreter
; Input    : IX - for the calling address
; Output   : Depends on the called routine
; Registers: Depends on the called routine
calbas:
                push    hl
                push    af
                ld      hl,calbas_text
                call    print_debug
                pop     af
                pop     hl
                ld      de,str_no_basic_intr
                jp      print_error
calbas_text:    db      "CALBAS",0

;------------------------------------
;Display error
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

                jp      hang_up_mode


                ds      $1bbf - $
                include "font.asm"

;------------------------------------
; disk routine
;------------------------------------

DISKIO:         equ     phydio ;$4010

; TODO: It seems this routine is currently broken, but it worked before.
;       It is currently disabled as well, so there is no harm.
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

                ; sector size / 0x20(file structure)

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
                call    chput
                djnz    name_lp

                ld      a,'.'
                call    chput

                ld      b,3
ext_lp:
                ld      a,(ix+0)
                inc     ix
                call    chput
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
                ;       [01234567890123456789012345678]
                db      "C-BIOS 0.21      cbios.sf.net"
                db      $0D,$0A,$0D,$0A,$0D,$0A,$00

str_slot:
                ;       [01234567890123456789012345678]
                db      "Init ROM in slot: ",$00

str_basic:
                ;       [01234567890123456789012345678]
                db      "Cannot execute a BASIC ROM.",$0D,$0A,$00

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
                ;       [01234567890123456789012345678]
                db      $0D,$0A,$0D,$0A
                db      "No cartridge found.",$0D,$0A
                db      $0D,$0A
                db      "This version of C-BIOS can",$0D,$0A
                db      "only start cartridges.",$0D,$0A
                db      "Please restart your MSX",$0D,$0A
                db      "(emulator) with a cartridge",$0D,$0A
                db      "inserted.",$00

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


                include "logo.asm"
; ????
                ds      $77CD - $
                ret

; Note: Below are a bunch of routines which do not adhere to any API that
;       I know of. They are called directly by the NMS8250 disk ROM.
;       For comparing the behaviour of the C-BIOS disk ROM which is under
;       development with the known working NMS8250 disk ROM it is useful
;       to be able to run either disk ROM in a C-BIOS machine. Therefore we
;       have stubs for the routines that the NMS8250 disk ROM may call.

; NMS8250 disk ROM can call to this address.
                ds      $7D17 - $
                push    hl
                push    af
                ld      hl,unk7D17_text
                call    print_debug
                pop     af
                pop     hl
                ret
unk7D17_text:   db      "unknown@7D17",0

; NMS8250 disk ROM retrieves a pointer from this address.
                ds      $7D2F - $
                dw      call_sdfscr

; NMS8250 disk ROM can call to this address.
                ds      $7D31 - $
; Restore screen parameters from RTC and print welcome message.
                ld      ix,$0189        ; SETSCR
                call    extrom
                ; Print BASIC copyright message.
                ret

; NMS8250 disk ROM calls to this address.
; Restore screen parameters from RTC.
call_sdfscr:
                ld      ix,$0185        ; SDFSCR
                jp      extrom

; NMS8250 disk ROM can call to this address.
                ds      $7E14 - $
                push    hl
                push    af
                ld      hl,unk7E14_text
                call    print_debug
                pop     af
                pop     hl
                ret
unk7E14_text:   db      "unknown@7E14",0

                ds      $8000 - $

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
