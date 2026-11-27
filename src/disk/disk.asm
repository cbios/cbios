; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; zerobas-disk — disk.asm
; ===========================================================================
; A clean-room MSX1 disk-interface ROM. Standalone 16 KB ROM that lives in an
; internal expansion slot (slot 3-1, page 1, $4000-$7FFF) on a built-in-disk
; MSX1. See README.md and disk/PROVENANCE.md.
;
; CLEAN-ROOM DISCIPLINE: every constant, address, and algorithm here traces to
; an allowed source (MSX2 Technical Handbook, the FDC datasheet, Microsoft FAT
; spec, ECMA-107, openMSX/C-BIOS sources, or this project's own black-box
; oracle observations). Nothing is derived from any disk-ROM or MSX-BASIC
; disassembly. See disk/PROVENANCE.md.
;
; FDC NOTE: the CF-3300's actual FDC is the Fujitsu MB8877A; it is WD179x-family
; compatible, so the WD2793 datasheet is a faithful compatible-family reference
; for the identical command/status register interface (oracle-validated
; byte-identical to the real CF-3300). openMSX models this FDC as a WD2793.
; "WD2793" below refers to that compatible interface, not a different chip.
;
; Runtime model: this is an "AB" disk-interface ROM. At boot the BIOS finds the
; header at $4000 and calls INIT, which (1) installs the standard HPHYD ($FFA7)
; -> DSKIO ($4010) inter-slot hook so a real MSX-BASIC / MSX-DOS host can drive
; us via PHYDIO (the Phase-1.5 PROVIDER surface), and (2) publishes the BDOS
; entry point via the SYSTEM ($F37D) sysvar (and seeds the default DTA), then
; returns. (Since D-DISKROMHOST INIT only claims HIMEM and the hooks -- (1), and
; H.RUNC; (2), the rest of the set-up and the DOS boot run from the host's first
; H.RUNC call, as on a stock disk ROM -- disk/init.asm.) zerobas-BASIC itself reaches the file layer
; through that SYSTEM-vector BDOS entry across slots with CALSLT (the internal
; path) — NOT through the H.* / HPHYD chain; HPHYD exists for FOREIGN hosts. The
; disk subsystem reaches the physical driver through the six fixed-offset entry
; points at $4010..$401F.
;
; STATUS: INIT installs the HPHYD->DSKIO hook (provider direction) and publishes
; the BDOS entry + DTA default (internal direction). The FDC driver is
; implemented (WD2793, National memory-mapped register map, polled sector read
; AND polled sector write — physical write primitive). The FAT12 read layer is
; implemented (BPB parse, cluster-
; chain walk, root-directory 8.3 search, sequential file-sector read) as
; internal helpers. The BDOS/FCB layer is implemented (bdos_entry dispatches
; Open $0F / Sequential Read $14 / Close $10 on top of the FAT12 helpers); it is
; reached through the SYSTEM vector and is the bridge zerobas's BLOAD path
; calls. GETDPB ($4016) is REAL: it builds a Drive Parameter Block from the
; on-disk BPB for a foreign host (provider direction), field-for-field confirmed
; against a black-box CF-3300 GETDPB trace (see getdpb / disk/PROVENANCE.md §DPB).
; End-to-end disk I/O is implemented and the read + BDOS paths are differentially
; oracle-confirmed (probes/disk/disk_probe_dskio.py / disk_probe_bdos.py).
; ===========================================================================

; ===========================================================================
; Source split into logical parts (§8.71). Assembled with `pasmo -I disk`; each
; part is included verbatim below IN LAYOUT ORDER -- the build is position-
; dependent (ds-anchored canonical kernel addresses + the 16 KB page pad), so the
; include order reproduces the exact byte sequence. Do not reorder.
; ===========================================================================
DISK_BUILD      equ     1   ; D-FATENG Option 2: the shared FAT body
                            ; binds its sector primitives to our LOCAL
                            ; dskio in this ROM.
                include "equates.inc"
                ; The main ROM's addresses this ROM calls into (D-DISKABI).
                ; GENERATED per build from build/basic-reloc.sym, so a low-region
                ; shift cannot leave us calling stale addresses; `make
                ; diskrom-abi-check` is the standing assert.
                include "basic-resident-abi.inc"
                ; D-BUFMERGE: ONE shared buffer pair (Joost, 2026-09-24). The two
                ; maps are separate assembly units, so the equality is asserted
                ; here, where both names are in scope; the cell block moved out of
                ; the buffers must also stay clear of them. D-FATFLOOR put the
                ; buffers at the diskless floor and the cells ABOVE them, so the
                ; old `MBUF_PTR + 2 > SECTOR_BUF` (cells below) became the wrong
                ; test: the block runs FAT_SECPERCLUS (its lowest cell) .. MBUF_PTR.
    IF SECTOR_BUF - FSECTOR_BUF
                db      DISK_SECTOR_BUF_IS_NOT_MAIN_FSECTOR_BUF
    ENDIF
    IF WBUF - FWBUF
                db      DISK_WBUF_IS_NOT_MAIN_FWBUF
    ENDIF
    IF FAT_SECPERCLUS < WBUF + 512
                db      DISK_CELL_BLOCK_OVERRAN_INTO_THE_SHARED_BUFFER
    ENDIF
    IF SECTOR_BUF + 512 - WBUF
                db      DISK_SHARED_BUFFERS_ARE_NOT_ADJACENT
    ENDIF
                ; step 2b (2026-10-13): the cell block lives on the diskless FLOOR,
                ; under main's TXTMAX_NODISK -- program RAM on a diskless machine,
                ; where no disk ROM runs -- and so under EVERY basic-core cell. It
                ; overlapped the persistent DRAW cells until then (D-DSCALEDISK).
    IF FAT_DEFER + 1 > TXTMAX_NODISK
                db      DISK_CELL_BLOCK_ABOVE_THE_DISKLESS_CEILING
    ENDIF
    IF FAT_FCACHE + 1 > TXTMAX_NODISK       ; D-LOADFAT: the cache cell is in the block too
                db      DISK_CELL_BLOCK_ABOVE_THE_DISKLESS_CEILING
    ENDIF
                include "init.asm"
                include "pageenv.asm"
                include "driver.asm"
                include "fat.asm"
                include "kernel.asm"
                include "runtime.asm"
                ; D-IFFREAD2 (2026-11-14): the two RAM templates that grew by a second
                ; `ld a,i` -- here, below every label they use (D-PASMO053).
    IF P1_BLIT + (p1_blit_end - p1_blit_tmpl) > FLD_END ; $EFFE..$F018: FLD_END is at $F019
                db      DISK_P1_BLIT_OVERRUNS_ITS_RAM_RUN
    ENDIF
    IF BR6_RAM + (br6_end - br6_tmpl) > DOS_RESV_TOP    ; the page-3 bridges' reservation
                db      DISK_SLOT_BRIDGES_OVERRUN_DOS_RESV_TOP
    ENDIF
