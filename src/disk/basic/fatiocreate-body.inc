; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fatiocreate-body.inc -- fat_io_create (the sequential FAT12 WRITE open).
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
; 🔴 SBH-3 (2026-10-03): THE RESIDENT COPY IS GONE. The body below assembles
; only under SUB_BUILD / DISK_BUILD; in main, fat_io_create is a stub in
; basic/fat.asm's shim block (DISKOP_SEL_FAT_IO_CREATE, fatprim row 23, served
; by sub/save.asm's t_fat_io_create). It used to stay resident, with
; fat_mount / fat_dir_create as marshalling STUBS over the fatprim tenant on
; that side; inside the tenant (and in disk.rom) the same source text binds to
; the REAL implementations sitting beside it in sub/fatprim.asm -- plain
; in-page calls. One crossing per create now instead of two.
;
; fat.asm still includes this at its ORIGINAL POSITION, so the stub's callers
; and the sub/disk bodies share one source text.
;
; CLEAN-ROOM: our own code, moved. FAT12 provenance unchanged
; (basic/PROVENANCE.md §disk DSKIO host engine).
; ===========================================================================

; fat_io_create — create (truncate-or-make) the file named in DISK_FCB_NAME for
; sequential WRITE from offset 0.
;   out: Cy = 0 created (stream ready); Cy = 1 = mount / dir-full / I/O error.
; Mounts, makes/reuses a root-dir slot (fat_dir_create), and resets the write
; iterator: no cluster yet, empty buffer, zero bytes. The first cluster is
; allocated lazily on the first flush.
    IF SUB_BUILD + DISK_BUILD
fat_io_create:
                ; 🔴 D-DISKFULL (2026-10-02): THE WRITE STATE IS RESET FIRST, before
                ; the mount and the directory entry, not after them. A failed
                ; create (an empty drive, a write-protected disk) used to return
                ; with the cells still holding whatever the last stream left --
                ; disk.rom's BDOS_WRBUFLEN read $FFFF -- and a caller whose error
                ; path resumes (the sub-ROM tenant's sv_load_error `ret`s into its
                ; loop) then wrote on from that. Neither fat_mount nor
                ; fat_dir_create reads or writes any of these cells.
                call    wr_reset            ; D-WRRESET (disk/kernel.asm; the disk build's FWR_* = BDOS_WR*)
                ; D-LOF (docs/spec-basic-lof-size-field.md §4a): the LOF field
                ; too. FAT_FILESIZE is written ONLY by fat_find, i.e. only when a
                ; file is FOUND, so before this a created channel reported
                ; whatever the previous tenant of the FCH_STATE0 span left --
                ; MEASURED at 26 and at 2048 (docs/lof-cf3300-characterization.md
                ; §2). The famous -1 was just the cold-boot content of the cell;
                ; nothing ever stored $FFFF. A created file is 0 bytes until
                ; CLOSE stamps the directory, and the reference agrees (0, and
                ; still 0 after a PRINT #). All four bytes: it is a size field,
                ; and a half-initialised one is the same defect one reader away.
                ld      (FAT_FILESIZE), hl
                ld      (FAT_FILESIZE + 2), hl
                call    fat_mount
                ret     c
                ld      hl, DISK_FCB_NAME
                jp      fat_dir_create      ; tail: Cy = 0 created, 1 = dir full / I-O
    ENDIF
