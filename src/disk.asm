; C-BIOS Disk ROM file - based on WD2793 FDC
;
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
                org     $4000
                db      "AB"
                dw      init            ; init
                dw      0               ; statement
                dw      0               ; device
                dw      0               ; basic text
                dw      0,0,0

; $4010 DSKIO
                ds      $4010-$,$FF
                jp      dskIO
; $4013 DSKCHG
                ds      $4013-$,$C9
                jp      dskChg
; $4016 GETDPB
                ds      $4016-$,$C9
                jp      getDpb
; $4019 CHOICE
                ds      $4019-$,$C9
                jp      choice
; $401C DSKFMT
                ds      $401C-$,$C9
                jp      dskFmt
; $401F LOC_DS - stop motor of drives connected to this interface
                ds      $401F-$,$C9
                jp      loc_ds
; $4022 BASIC
                ds      $4022-$,$C9
                jp      basic

                scf
; $4026 FORMAT
                ds      $4026-$,$C9
                jp      format
; $4029 DSKSTP - stop motor of drives on all interfaces
                ds      $4029-$,$C9
                jp      dskStp
; $402D DSKSLT
                ds      $402D-$,$00
                jp      dskslt
;
init:
                ld      hl,tInit
                jp      print_debug
;
; DSKIO
; Input:   F  = NC to read, C to write
;          A  = Drive number (0=A:)
;          B  = Number of sectors to transfer
;          C  = Media descriptor
;          DE = Logical sector number
;          HL = Transfer address
;
; Output:  F  = NC if successful, C if error
;          A  = Error code if error
;               0 = Write protected
;               2 = Not ready
;               4 = Data (CRC) error
;               6 = Seek error
;               8 = Record not found
;              10 = Write fault
;              12 = Other errors
;          B  = Always the number of sectors transferred
; NOTE: This routine is still stubbed
dskIO:
                push    hl
                push    af
                ld      hl,tDskIO
                call    print_debug
                pop     af
                pop     hl
                ret
;
; DSKCHG
; Input:   A  = Drive number (0=A:)
;          B  = Media Descriptor
;          C  = Media Descriptor
;          HL = Base address of DPB
;
; Output:  F  = NC if successful, C if error
;          A  = Error code if error
;               0 = Write protected
;               2 = Not ready
;               4 = Data (CRC) error
;               6 = Seek error
;               8 = Record not found
;              10 = Write fault
;              12 = Other errors
;          B  = Disk Change state if successful
;              -1 = Disk changed
;               0 = Unknown
;               1 = Disk unchanged
; Note:    If the disk has been changed or may have been changed (unknown)
;          read the bootsector or the first FAT sectoe for a disk media
;          descriptor and transfer a new DPB as with GETDPB.
; NOTE: This routine is still stubbed
dskChg:
                push    hl
                push    af
                ld      hl,tDskChg
                call    print_debug
                pop     af
                pop     hl
                ret
;
; GETDPB
; Input:   A  = Drive number (0=A:)
;          B  = First byte of FAT (media descriptor)
;          C  = Media descriptor
;          HL = Base address of DPB
;
; Output:  HL = DPB filled in
; Note:    DPB consists of:
;          Name    Offset    Size    Description
;          --------------------------------------------------
;          MEDIA      $00       1    Media type ($F8 - $FF)
;          SECSIZE    $01       2    Sector size (must be 2^n)
;          DIRMSK     $03       1    (SECSIZ / 32 - 1)
;          DIRSHFT    $04       1    Number of one bits in DIRMSK
;          CLUSMSK    $05       1    (Sectors per cluster - 1)
;          CLUSSHFT   $06       1    (Number of one bits in CLUSMSK) - 1
;          FIRFAT     $07       2    Logical sector number of first FAT
;          FATCNT     $09       1    Number of FATs
;          MAXENT     $0A       1    Number of root directory entries
;          FIRREC     $0B       2    Logical sector number of first data
;          MAXCLUS    $0D       2    (Number of clusters) + 1
;                                    This excludes the number of reserved,
;                                    FAT and root directory sectors.
;          FATSIZ     $0F       1    Number of sectors used for FAT
;          FIRDIR     $10       2    Logical sector number of first directory
; NOTE: This routine is still stubbed
getDpb:
                push    hl
                push    af
                ld      hl,tGetDpb
                call    print_debug
                pop     af
                pop     hl
                ret
;
; CHOICE
; Output:  HL = Address of ASCIIz string containing the text with choices
;               for DSKFMT. If there are no choices (only one format sup-
;               ported) HL=0
choice:
                ld      hl,tChoice
                ret
;
; DSKFMT
; Input:   A  = Choice specified by user: 1-9. See CHOICE
;          D  = Drive number (0=A:)
;          BC = Length of work area
;          HL = Base address of work area
;
; Output:  F  = NC if successful, C if error
;          A  = Error code if error
;               0 = Write protected
;               2 = Not ready
;               4 = Data (CRC) error
;               6 = Seek error
;               8 = Record not found
;              10 = Write fault
;              12 = Bad parameter
;              14 = Insufficient memory
;              16 = Other errors
; Note:    Also write MSX bootsector at sector 0, clears all FATs (media
;          descriptor ar first byte, $FF at the second/third byte and
;          rest filled with $00) and clears the root directory (full $00).
; NOTE: This routine is still stubbed
dskFmt:
                push    hl
                push    af
                ld      hl,tDskFmt
                call    print_debug
                pop     af
                pop     hl
                ret
;
; LOC_DS
; Note:    Stop motor for all drives on THIS interface.
loc_ds:
                push    hl
                push    af
                ld      hl,tLoc_ds
                call    print_debug
                pop     af
                pop     hl
                ret
;
; BASIC
; Note:    Warmboots to BASIC.
; NOTE: This routine is still stubbed
basic:
                push    hl
                push    af
                ld      hl,tBasic
                call    print_debug
                pop     af
                pop     hl
                ret
;
; FORMAT
; Note:    Like CALL FORMAT, FORMAT (DOS) and BIOS routine $0147.
;          Display CHOICE, wait for input and do DSKFMT.
; NOTE: This routine is still stubbed
format:
                push    hl
                push    af
                ld      hl,tFormat
                call    print_debug
                pop     af
                pop     hl
                ret
;
; DSKSTP
; Note:    Stop motor for all drives on all interfaces. Interslot-calls
;          LOC_DS for all detected interfaces.
; NOTE: This routine is still stubbed
dskStp:
                push    hl
                push    af
                ld      hl,tDskStp
                call    print_debug
                pop     af
                pop     hl
                ret
;
; DSKSLT
; Output:  Address $F348 keeps the slot where the DISK-ROM is found.
; NOTE: This routine is still stubbed
dskSlt:
                push    hl
                push    af
                ld      hl,tDskSlt
                call    print_debug
                pop     af
                pop     hl
                ret
;
                include "debug.asm"
;
tInit:          db      "C-DISK is initializing",0
;
tDskIO:         db      "Address $4010 called (DSKIO)",0
tDskChg:        db      "Address $4013 called (DSKCHG)",0
tGetDpb:        db      "Address $4016 called (GETDPB)",0
tDskFmt:        db      "Address $401C called (DSKFMT)",0
tLoc_ds:        db      "Address $401F called (LOC_DS)",0
tBasic:         db      "Address $4022 called (BASIC)",0
tFormat:        db      "Address $4026 called (FORMAT)",0
tDskStp:        db      "Address $4029 called (DSKSTP)",0
tDskSlt:        db      "Address $402D called (DSKSLT)",0
;
tChoice:        db      13,10,"1 - Single sided, 80 tracks"
                db      13,10,"2 - Double sided, 80 tracks"
                db      13,10,0
;
                ds      $4000-($-$4000),$FF
