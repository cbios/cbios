; $Id: hardware.asm,v 1.4 2004/12/22 21:11:13 manuelbi Exp $
; C-BIOS hardware related declarations
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
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

;---------------------------------------------------
; I/O ports

DBG_CTRL:       equ     $2E             ; openMSX debugdevice control (mode)
DBG_DATA:       equ     $2F             ; openMSX debugdevice data

PRN_STAT:       equ     $90             ; printer status

VDP_DATA:       equ     $98             ; VDP data port (VRAM read/write)
VDP_ADDR:       equ     $99             ; VDP address (write only)
VDP_STAT:       equ     $99             ; VDP status (read only)
VDP_PALT:       equ     $9A             ; VDP palette latch (write only)
VDP_REGS:       equ     $9B             ; VDP register access (write only)

PSG_REGS:       equ     $A0             ; PSGレジスタ番号
PSG_DATA:       equ     $A1             ; PSG data
PSG_STAT:       equ     $A2             ; PSG status

PSL_STAT:       equ     $A8             ; slot status
KBD_STAT:       equ     $A9             ; keyboard status
GIO_REGS:       equ     $AA             ; 総合IOレジスタ
PPI_REGS:       equ     $AB             ; PPI register

RTC_ADDR:       equ     $B4             ; RTC address
RTC_DATA:       equ     $B5             ; RTC data

MAP_REG1:       equ     $FC             ; memory mapper: bank in $0000-$3FFF
MAP_REG2:       equ     $FD             ; memory mapper: bank in $4000-$7FFF
MAP_REG3:       equ     $FE             ; memory mapper: bank in $8000-$BFFF
MAP_REG4:       equ     $FF             ; memory mapper: bank in $C000-$FFFF

;---------------------------------------------------
; memory mapped I/O

SSL_REGS:       equ     $FFFF           ; secondary slot select registers

