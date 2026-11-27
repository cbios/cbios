; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fatiow-body.inc -- fat_io_putbyte + fwr_bytes_inc + fat_io_close.
; ===========================================================================
; 🏗️ 2026-10-19 (D-ROMSIZE step 1): DISK.ROM IS THE ONLY BUILD THAT ASSEMBLES THIS
; BODY. The sub-ROM save tenant stopped including it when BSAVE's disk engine --
; its last writer -- moved to disk.rom (hk_dpbsave); main includes it at its
; original position and assembles nothing (the gate below). The history that
; follows describes the sub-ROM copy as it was.
; Extracted VERBATIM from basic/fat.asm so the sub-ROM SAVE tenant
; (sub/save.asm, SUBROM_IDX_SAVE) gets a byte-identical SUB-LOCAL copy instead
; of a second hand-maintained one. This is the WRITE mirror of fatio-body.inc,
; which did the same for the read cursor and sub/bload.asm (until 2026-10-19:
; BLOAD's disk load is disk.rom's, and so is fatio-body.inc's one home).
;
; ~~fat_io_putbyte and fwr_bytes_inc STAY resident -- the per-byte hot path of
; PRINT # and the ASCII SAVE.~~ 🏗️ S10 increment 3 (D-ASAVEDOUT, 2026-10-08):
; NOTHING HERE IS RESIDENT ANY MORE. PRINT# went to disk.rom's record writer in
; S10.B, and SAVE ,A to disk.rom's copy of THIS file; main assembles none of
; it. In the tenant and in disk.rom fat_flush_data_sector binds to the REAL
; implementation beside it -- a plain in-page call.
; 🔴 SBH-3 (2026-10-03): fat_io_close's resident copy IS GONE. Its body
; assembles only under SUB_BUILD / DISK_BUILD; in main it is a stub in
; basic/fat.asm's shim block (DISKOP_SEL_FAT_IO_CLOSE, fatprim row 24, served
; by sub/save.asm's t_fat_io_close). One crossing per close instead of two.
;
; fat.asm still includes this at its ORIGINAL POSITION, so the three homes
; share one source text.
;
; CLEAN-ROOM: our own code, moved. FAT12 provenance unchanged
; (basic/PROVENANCE.md §disk DSKIO host engine).
; ===========================================================================

; fat_io_putbyte — append the byte in A to the open-for-write file.
;   in:  A = byte to write.
;   out: Cy = 0 ok; Cy = 1 = disk full / write error.
; Buffers the byte into FAT_DBUF at offset FWR_BUFLEN; when the buffer fills
; (512 bytes) it is flushed to the file's current data sector (allocating/extending
; the cluster chain as needed) and the buffer resets. FWR_BYTES accumulates the
; true byte count for fat_io_close to stamp into the directory entry. The flush's
; DSKIO clobbers all registers, so state is kept in RAM.
; 🏗️ S10 increment 3 (D-ASAVEDOUT): MAIN NO LONGER ASSEMBLES THIS FILE'S
; BODIES. Its last two callers -- pchar's SAVE ,A arm and sv-diskwr.inc's
; disk_putbyte -- went to disk.rom (H_CHOUT with no channel live), so all
; three are SUB_BUILD / DISK_BUILD only, like fat_io_close before them.
    IF SUB_BUILD + DISK_BUILD
fat_io_putbyte:
                ld      c, a                ; C = byte to store
                ; 🔴 D-DISKFULL (2026-10-02): THE FLUSH COMES FIRST, when the
                ; buffer is already full -- it used to come AFTER the 512th store,
                ; and a failed flush (a full disk) left FWR_BUFLEN at 512, so the
                ; NEXT byte landed past the buffer. That was PRINT#'s hang on a
                ; full disk. Now a failed flush returns before storing, the
                ; buffer stays full, and every later call retries the flush and
                ; fails the same way. The file's bytes are unchanged: the 512th
                ; byte's flush simply waits for the 513th byte, or CLOSE.
                ld      hl, (FWR_BUFLEN)
                ld      a, h
                cp      2                   ; FWR_BUFLEN = 512 ($0200): full
                jr      c, fip_store
                push    bc                  ; the flush's DSKIO clobbers everything
                call    fat_flush_data_sector
                pop     bc
                ret     c                   ; disk full / write error: NOTHING stored
                ld      hl, 0
                ld      (FWR_BUFLEN), hl    ; buffer drained
fip_store:
                ; 🔴 `FAT_DBUF`, NEVER `FSECTOR_BUF` -- the SAME rule
                ; fatio-body.inc states for the read side, and this file was
                ; MISSED when D-FATENG applied it (D-SAVEPORT, 2026-09-20). In
                ; main and sub the two names are one address, so the mistake was
                ; invisible; in disk.rom `FSECTOR_BUF` resolves through
                ; basic-resident-abi.inc to MAIN's $E5C0 while FAT_DBUF is
                ; SECTOR_BUF $E2A0 -- so every byte landed in main's buffer and
                ; the flush wrote the untouched one, which still held the boot
                ; sector fat_mount had just read. The saved file was the right
                ; SIZE and contained `EB FE 90 "ZEROBAS "`.
                ld      de, FAT_DBUF        ; per-ROM; see the note above
                add     hl, de              ; HL = FAT_DBUF + FWR_BUFLEN
                ld      (hl), c             ; store the byte
                ; advance buffered length and total byte count by 1.
                ld      hl, (FWR_BUFLEN)
                inc     hl
                ld      (FWR_BUFLEN), hl
                ; FWR_BYTES += 1, 4-byte little-endian (fwr_bytes_inc, its only call, inline
                ; since D-DISKROMHOST step 2b: 4 B for the corridor)
                ld      hl, FWR_BYTES
                ld      a, (hl)
                add     a, 1
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                adc     a, 0
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                adc     a, 0
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                adc     a, 0
                ld      (hl), a
                or      a                   ; Cy = 0 success
                ret

; fat_io_close — finish a sequential-write file durably.
;   out: Cy = 0 ok; Cy = 1 = I/O error.
; Flushes a buffered partial final sector (FWR_BUFLEN > 0), then rewrites the
; directory entry with the true byte count and first cluster.
fat_io_close:
                ld      hl, (FWR_BUFLEN)
                ld      a, h
                or      l
                jr      z, fic_dir          ; nothing buffered
                call    fat_flush_data_sector
                ret     c
fic_dir:
                jp      fat_dir_update      ; tail: write true size + first cluster
    ENDIF
