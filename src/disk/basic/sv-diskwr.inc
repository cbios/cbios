; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; sv-diskwr.inc -- the sequential DISK write helpers (disk_write_begin/putword/putbyte/write_end).
; ===========================================================================
; Clean-room: zerobas's own code, built on this project's own FAT12 engine over
; the STANDARD $4010 DSKIO entry -- the public FAT12 format, no disk-ROM
; internals. Nothing here is derived from a disassembly or byte-copy of any
; reference ROM. Basis: basic/PROVENANCE.md.
; ===========================================================================

; Shared disk-write helper (the WRITE analogue of the read side's fat_io_open /
; fat_io_getbyte). Built on the loader-side FAT12 engine over the STANDARD $4010
; DSKIO entry (basic/fat.asm) — disk-ROM-independent, NOT the private bdos_entry.
; disk_write_begin creates a fresh file; disk_putbyte appends one byte (the engine
; buffers it into a 512-byte sector and flushes whole sectors as they fill);
; disk_write_end flushes the partial final sector and stamps the directory size.
; The engine tracks exact byte counts, so the on-disk file size is its true length
; (no 128-byte record zero-padding) — SAVE/BSAVE produce byte-exact images.

; disk_write_begin — require a recorded disk-ROM slot, then Create (truncate-or-
; make) the file named in DISK_FCB_NAME and arm the sequential-write iterator.
; On any failure jumps to sv_load_error (does not return).
;
; DOCUMENTED DIVERGENCE (no write rollback): fat_io_create stamps the directory
; entry up front. If a later disk_putbyte/disk_write_end fails mid-stream (disk
; full / write error), the error path jumps to sv_load_error WITHOUT fat_io_close, so
; the entry is left with an un-stamped size/first-cluster (a zero-length or partial
; file). Acceptable for a game loader — a failed SAVE simply needs re-issuing — and
; no caller depends on atomic write; surfaced here so it is not mistaken for a bug.
disk_write_begin:
                ld      a,(DISKSLOT_OK)
                or      a
                jr      z,sv_load_error
                call    fat_io_create       ; mount + dir-create + reset write state
                jr      c,sv_load_error        ; disk full / dir full / write protect / I-O
                ret

; (disk_putword -- the BSAVE header's word writer -- lives in basic/sv-bsvdisk.inc
; since 2026-10-19, beside its only caller. It sat here under `IF SUB_BUILD`
; because this file was shared by main, sub and disk while its one caller was the
; sub-ROM's alone (the R1 carve, docs/spec-rom-region-rebalance-r1.md A3: 16 B of
; main page 1, and "dead is per-BUILD" -- docs/rom-region-structure-review.md
; §0.1, §4). BSAVE's disk engine runs in disk.rom now, and only disk.rom includes
; sv-bsvdisk.inc, so the routine went with it and the gate has nothing to gate.)

; disk_putbyte — append the byte in A to the open-for-write file via the FAT12
; engine (which buffers into a 512-byte sector and flushes whole sectors as they
; fill). On a write error jumps to sv_load_error.
;   in:  A = byte to write.
;   out: byte appended. Clobbers all (CALSLT clobbers everything across a sector
;        flush's DSKIO; the engine keeps its state in RAM).
; (disk_putbyte -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $5058, disk/kernel.asm.)

; disk_write_end — flush the buffered partial final sector and stamp the directory
; entry with the true byte count + first cluster (fat_io_close). On a write error
; jumps to sv_load_error. Returns to the caller's caller (the REPL) on success.
disk_write_end:
                call    sdw_end             ; D-SAVEFAT: the held FAT out, then fat_io_close
                jr      c,sv_load_error
                ret                         ; back to the prompt
