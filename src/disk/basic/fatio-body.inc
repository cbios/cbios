; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fatio-body.inc — the sequential FAT12 READ layer (fat_io_open + fat_io_getbyte).
; ===========================================================================
; ONE home since 2026-10-19: disk.rom (disk/kernel.asm -- LOAD, MERGE and, since
; that date, BLOAD's hk_dpbload), where fat_mount/fat_find/fat_open/
; fat_read_file_sector bind to its own FAT engine. It was shared, byte-identically,
; with the sub-ROM PAGE-1 BLOAD tenant (sub/bload.asm, docs/spec-traps-t3-key.md
; §7.6), whose copy bound them to sub/fatprim.asm's; that disk path moved to
; disk.rom (D-ROMSIZE step 1) and the sub-ROM include went with it.
; 🗑️ S10.C (2026-10-15): THE RESIDENT MAIN COPY IS GONE. This header said "The
; resident copy must stay — do_load, ex_merge and the file verbs all use it";
; do_load and ex_merge moved to disk.rom with steps 9 / 11, and OPEN FOR INPUT,
; the last file verb on it, with S10.C (hk_finp / hk_chin read the channel's FCB
; row, not this cursor). basic/fat.asm no longer includes it.
;
; CLEAN-ROOM: extracted verbatim from our own basic/fat.asm. FAT12 provenance is
; unchanged (basic/PROVENANCE.md §disk DSKIO host engine).
; ===========================================================================

; 🗑️ fat_io_open -- `call fat_mount / ret c` falling into fat_io_find, the
; combined open -- IS GONE (S10.C, 2026-10-15). Main was its only caller (OPEN FOR
; INPUT); both ROMs that then included this body mount SEPARATELY and enter
; below, which is D-BLNF's whole point, so its 4 B were dead in each and
; allowlisted as "load-bearing in main" (tools/deadcode-allow.txt). That reason
; left with main's copy, and so did the two allowlist lines.
; fat_io_find — open the file named in DISK_FCB_NAME for sequential READ, for a
; caller that has already mounted and wants the find's carry BY ITSELF. Cy out
; of here means precisely "the volume mounted and the 8.3 name is NOT in the
; directory" — the tail below cannot set it (it ends `or a`). D-BLNF: that is how
; disk.rom's hk_dpload / hk_aopen / hk_dpbload tell NOT FOUND from a mount/I-O
; fault (disk/kernel.asm) -- hk_dpbload since 2026-10-19, when it took the BLOAD
; tenant's disk path, which told them apart the same way. Primes the read iterator, seeds the
; bytes-remaining counter from the true file size, and marks the sector buffer
; empty so the first fat_io_getbyte refills it.
fat_io_find:
                call    ff_name
                ret     c
                call    fat_open
                ; seed FREAD_LEFT = FAT_FILESIZE (4-byte LE).
                ld      hl, FAT_FILESIZE
                ld      de, FREAD_LEFT
                ld      bc, 4
                ldir
                ; mark the in-RAM sector buffer exhausted (OFF==512 -> first
                ; getbyte triggers fat_read_file_sector).
                ld      hl, 512
                ld      (FREAD_OFF), hl
                or      a                   ; Cy = 0 success
                ret

; fat_io_getbyte — return the next file byte in A; Cy set on EOF (no more data).
; State: FREAD_OFF = next byte index within FAT_DBUF (0..512); FREAD_LEFT =
; bytes of the file still undelivered (bounds the true EOF). Refills FAT_DBUF
; from the cluster chain when the in-RAM sector is exhausted.
; 🔴 THE BUFFER IS NAMED `FAT_DBUF`, NEVER `FSECTOR_BUF`, AND THAT IS
; LOAD-BEARING FOR A ROM THIS FILE IS NOT IN YET. `FAT_DBUF` is the per-ROM alias
; of whatever buffer THIS build's `fat_read_file_sector` actually fills --
; $E5C0 in main/sub, $E2A0 in disk.rom (Option 2's parametrisation). In the main
; build the two names are the same cell, so this rename is byte-identical; in a
; disk build they are 800 bytes apart. `disk/basic-resident-abi.inc` ALSO binds
; `FSECTOR_BUF equ 0E5C0H`, so spelling it the old way would assemble WITHOUT ONE
; ERROR and read a buffer nobody filled (spec-diskcode-eviction.md §6.6c).
; Step 9 ports this file into `disk.rom`; the name must already be right then.
;
; CALSLT (inside fat_read_file_sector's DSKIO) clobbers every register, so all
; loop state lives in RAM and is reloaded here each call.
fat_io_getbyte:
                ; EOF once every file byte has been delivered (FREAD_LEFT == 0).
                ld      hl, (FREAD_LEFT)
                ld      de, (FREAD_LEFT + 2)
                ld      a, h
                or      l
                or      d
                or      e
                jr      z, fig_eof
                ; refill the sector buffer if exhausted (OFF >= 512).
                ld      hl, (FREAD_OFF)
                ld      de, 512
                or      a
                sbc     hl, de
                jr      c, fig_have         ; OFF < 512 -> bytes left in FAT_DBUF
                call    fat_read_file_sector
                jr      c, fig_eof          ; chain ended early -> EOF (size bounded)
                ld      hl, 0
                ld      (FREAD_OFF), hl     ; back to byte 0 of the fresh sector
fig_have:
                ; A = FAT_DBUF[OFF]; advance OFF and decrement LEFT.
                ld      hl, (FREAD_OFF)
                ld      de, FAT_DBUF        ; per-ROM; see the header
                add     hl, de
                ld      a, (hl)
                ld      b, a                ; B = the byte (survives the bookkeeping)
                ld      hl, (FREAD_OFF)
                inc     hl
                ld      (FREAD_OFF), hl
                ; FREAD_LEFT -= 1 (4-byte LE decrement).
                ld      hl, FREAD_LEFT
                ld      a, (hl)
                sub     1
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                sbc     a, 0
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                sbc     a, 0
                ld      (hl), a
                inc     hl
                ld      a, (hl)
                sbc     a, 0
                ld      (hl), a
                ld      a, b                ; A = the byte
                or      a                   ; Cy = 0 (A may be anything; OR clears Cy)
                ret
fig_eof:
                scf
                ret
