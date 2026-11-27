; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; sv-bsvdisk.inc -- BSAVE's DISK write engine (bsv_open .. bsv_data, disk_putword).
; ===========================================================================
; 🏗️ D-ROMSIZE step 1 (2026-10-19): INCLUDED BY disk.rom ALONE (disk/kernel.asm,
; in the hole after the pinned $50E0 CONIN entry), reached through H_FOPEN with
; FOPEN_SEL_BSAVE -> hk_dpload -> hk_dpbsave, which shares hk_dpsave's prologue
; and unwind. The CF-3300's BSAVE enters $FE5D exactly once, never $FE71, and no
; claimed cell per byte (scratchpad/bsave_hook_run.out), so the whole write --
; create, the fit verdict, the 7-byte header, the data, the close -- is one
; crossing. Main keeps the parse (basic/save.asm do_bsave: the name, start, end,
; exec / `,S`, and the end of the statement BEFORE the open) and the diskless /
; device dispatch; every value crosses in its RAM home (DISK_FCB_NAME, CURPTR,
; DSV_END, EXECPTR, VRAM_FLAG -- published to this ROM by tools/gen_resident_abi.py).
;
; HISTORY. One of the five SAVE-family WRITE ENGINES evicted from main page 1 into
; the sub-ROM PAGE-1 tenant sub/save.asm (SUBROM_IDX_SAVE) to fund `TIME` and
; interrupt-traps T5 (docs/decision-fund-time-and-t5.md, D-FUND-1), over the
; sub-ROM's own copy of the FAT engine (sub/fatprim.asm). Tokenised SAVE left that
; tenant for disk.rom in step 12 (D-SAVEPORT); this body followed on 2026-10-19,
; VERBATIM, over disk.rom's own fat_io_create / fat_io_putbyte / fat_io_close and
; fat_fits -- the same four bodies hk_dpsave already uses -- so the file on the
; disk is byte-for-byte what the tenant wrote.
;
; `sv_load_error` is disk.rom's: DISKOP_STATUS := 3 and an unwind to hk_dpsave's
; frame from any depth (D-DISKFULL / D-DISKFULLRETRY), so the FIRST failure ends
; the write and main's disk_error raises the mapped code once. The tenant's copy
; also called TAPIOF and set ERRMARK; a disk write never armed the tape.
;
; CLEAN-ROOM: our own code, moved. Cassette/disk format provenance unchanged
; (basic/PROVENANCE.md).
; ===========================================================================

; disk_putword -- write the 16-bit little-endian word stored at RAM address HL out
; through disk_putbyte (low byte then high byte). disk_putbyte clobbers HL (and
; everything else), so the source address is kept in a RAM slot (DSV_PTR, which is
; otherwise only used by tokenised SAVE's own loop and is free during the BSAVE
; header) and reloaded for the high byte rather than trusted in a register.
;   in:  HL = address of a 2-byte LE word in page-3 RAM.
;   out: both bytes appended to the write stream. Clobbers all (via disk_putbyte).
; (It lived in basic/sv-diskwr.inc under `IF SUB_BUILD` until 2026-10-19 -- see
; the note there; this file is its only caller and disk.rom its only home.)
disk_putword:
                ld      (DSV_PTR),hl        ; remember the word's address
                ld      a,(hl)              ; low byte
                call    disk_putbyte
                ld      hl,(DSV_PTR)        ; reload (disk_putbyte clobbered HL)
                inc     hl
                ld      a,(hl)              ; high byte
                jp      disk_putbyte        ; tail call (D-DISKREREAD 2026-11-26, 1 B:
                                            ; its error path resets SP, sv_load_error)

bsv_open:
                ; --- create the file and set the DTA -----------------------
                call    disk_write_begin    ; require DISKSLOT_OK; Set-DTA; Create
                ; --- D-DISKFULLSTAMP: does the whole image FIT? ------------
                ; MEASURED on the CF-3300 (scratchpad/bsvfull_stamp.out): when it
                ; does not, the header is written, the data block is refused
                ; WHOLE (no partial data, no second cluster even with two free),
                ; and the file is CLOSED -- a Ctrl-Z after the header, size 8,
                ; one cluster, then 66. Streaming on until the disk filled left
                ; ours a LOST cluster (an allocated chain no entry points at).
                ; So count first (fat_fits, shared with tokenised SAVE).
                ld      hl,(DSV_END)
                ld      de,(CURPTR)
                or      a
                sbc     hl,de               ; HL = length - 1
                ld      de,7
                add     hl,de               ; HL = header + length - 1, CF = bit 16
                call    fat_fits            ; A = 0 fits, $FF does not
                push    af                  ; the verdict, across the header writes
                ; --- 7-byte header: $FE start(LE) end(LE) exec(LE) ----------
                ; disk_putbyte clobbers ALL registers (its CALSLT may flush a
                ; record), so each 16-bit field is emitted via disk_putword, which
                ; reloads the word from its RAM home for the high byte rather than
                ; trusting a register across the call.
                ld      a,BSAVE_DISK_ID     ; $FE
                call    disk_putbyte
                ld      hl,CURPTR           ; start (LE)
                call    disk_putword
                ld      hl,DSV_END          ; end (LE)
                call    disk_putword
                ld      hl,EXECPTR          ; exec (LE)
                call    disk_putword
                pop     af
                or      a
                jr      nz,bsv_full         ; D-DISKFULLSTAMP: the block cannot fit
                ; --- data bytes [start..end] inclusive, RAM or VRAM --------
                ; With ",S" (VRAM_FLAG) the start/end are VRAM addresses and each
                ; byte is fetched via RDVRM ($004A) instead of a RAM read; the
                ; loop shape is otherwise identical.
bsv_data:
                ld      hl,(CURPTR)
                ld      a,(VRAM_FLAG)
                or      a
                jr      z,bsv_data_ram
                call    RDVRM               ; HL=CURPTR (VRAM addr) -> A
                jr      bsv_data_put
bsv_data_ram:
                ld      a,(hl)              ; HL=CURPTR (RAM addr) -> A
bsv_data_put:
                call    disk_putbyte
                ld      hl,(CURPTR)
                ld      de,(DSV_END)
                or      a                   ; D-DISKREREAD (2026-11-26): one 16-bit
                sbc     hl,de               ; compare, and HL kept for the advance
                add     hl,de               ; (16-bit add: Z stays the sbc's) -- 7 B
                jp      z,disk_write_end    ; cur == end -> last byte written: flush +
                                            ; Close + back to the prompt
                inc     hl                  ; HL = CURPTR still
                ld      (CURPTR),hl
                jr      bsv_data
; bsv_full -- the CF-3300's refusal: close the file as it stands (its CLOSE adds
; the Ctrl-Z; the successful path does not -- a 263 B BSAVE is 263 B there), then
; 66. A close that cannot even place the header (no free cluster at all) fails
; with the allocator's own 66 pending.
bsv_full:
                ld      a,$1A
                call    disk_putbyte
; sav_full -- tokenised SAVE's Disk full (basic/sv-savdisk.inc, D-SAVEFULLSTAMP): the
; marker alone closed, stamped, then 66 -- byte for byte this tail, so it IS this tail
; (D-CALLSTMT carve, 2026-11-13: its 14 B copy in front of $5FE5 went).
sav_full:
                call    fat_io_close        ; header + Ctrl-Z, stamped
                jr      c,sf_x              ; its code is already pending (D-DISKREREAD
                                            ; 2026-11-26: the jp below, 1 B)
                ld      a,66                ; Disk full
                ld      (DISKOP_ERR),a
sf_x:
                jp      sv_load_error
