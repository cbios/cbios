; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fat-delete-body.inc — fat_delete (shared body), the LAST primitive-layer
; routine (docs/spec-evict-diskfile-cluster.md §11).
;
; SEPARATE from basic/fat-prim-body.inc only because fat_delete sits AFTER the
; resident fat_io_* byte cursor in the original basic/fat.asm (mount/find/
; next-cluster/alloc/dir-create/dir-update come BEFORE the cursor; fat_delete
; comes AFTER it) — so the original byte order (primitives, then cursor,
; then fat_delete) can only be preserved by including this
; routine at its ORIGINAL position, separately from the earlier bulk. Same
; two-home inclusion pattern as fat-prim-body.inc (see that file's header):
; retired lean 16 KB cart inlined it verbatim at
; its original position; the sub-ROM PAGE-1 tenant (sub/fatprim.asm) includes
; it alongside fat-prim-body.inc (order doesn't matter there — no byte-
; identity constraint on the tenant image).
;
; Clean-room: our own code, extracted verbatim from basic/fat.asm (see that
; file's header for the full FAT12/DSKIO provenance / oracle citations).

; fat_delete — delete the file named in DISK_FCB_NAME: free its FAT cluster chain
; and mark its directory entry deleted ($E5). Mounts first.
;   out: Cy = 0 deleted; Cy = 1 not found / mount / I-O error.
; Composed from fat_find (which now records the entry location in FWR_DIRSEC /
; FWR_DIROFF), fat_next_cluster, and fat_write_fat_entry. Sources: Microsoft FAT
; spec — a deleted directory entry's first byte is $E5; freeing a chain writes 0
; to each of its FAT entries (in every FAT copy, done by fat_write_fat_entry).
fat_delete:
                call    fat_mount
                ret     c
                call    ff_name            ; FAT_FIRSTCLUS + FWR_DIRSEC/FWR_DIROFF
                ret     c                   ; not found
                ; --- free the cluster chain from FAT_FIRSTCLUS ---
                ld      hl, (FAT_FIRSTCLUS)
                call    fat_free_chain
                ret     c
                ; D-DISKREREAD (2026-11-26): fdl_marked FOLLOWS here (it ends in a `jp`, so
                ; nothing falls out of it into fat_free_chain), not a `jr` to it below, 2 B
fdl_marked:
                ; --- mark the directory entry deleted ($E5) and write it back ---
                ld      de, (FWR_DIRSEC)
                push    de                  ; D-DISKREREAD (2026-11-26): the sector kept on the
                call    rs_dbuf             ; stack, not reloaded, and the offset added through BC
                pop     de                  ; (fatprim_write_sector sets B / C itself), 2 B
                ret     c
                ld      hl, (FWR_DIROFF)
                ld      bc, FAT_DBUF
                add     hl, bc
                ld      (hl), $E5           ; deleted-entry marker
                ld      hl, FAT_DBUF
                jp      fatprim_write_sector ; tail: returns the write's CY (renamed,
                                            ; see fat-prim-body.inc's write_sector
                                            ; header for why)

; fat_free_chain -- write 0 to every FAT entry of the chain from HL (in every
; FAT copy), stopping at a non-data cluster. Cy = 1 on an I/O error. Split out
; of fat_delete for D-PRINTFULLSTAMP (2026-10-06): disk.rom releases an OUTPUT
; channel's chain when its CLOSE fails, as the CF-3300 never commits it.
fat_free_chain:
fdl_free:
                ; stop on a non-data cluster (clus < 2, or >= $FF8 end-of-chain).
                ld      a, h
                or      a
                jr      nz, fdl_hi
                ld      a, l
                cp      2
                jr      c, fdl_done         ; cluster 0/1 -> nothing more to free
                jr      fdl_isdata          ; 2..255 -> data cluster
fdl_hi:
                cp      $0F
                jr      c, fdl_isdata       ; high < $0F -> data (clus < $F00)
                ld      a, l
                cp      $F8
                jr      nc, fdl_done        ; clus >= $FF8 -> end-of-chain, stop
fdl_isdata:
                push    hl                  ; save the current cluster
                call    fat_next_cluster    ; HL = next cluster (uses FSECTOR_BUF)
                ex      de, hl              ; DE = next
                pop     hl                  ; HL = current cluster
                push    de                  ; save next across the FAT write
                call    fwfe_zero ; FAT[current] = 0 in every FAT copy  ; free marker
                pop     hl                  ; HL = next cluster
                ret     c                   ; I/O error, CF = 1 (stack balanced) -- D-DISKREREAD
                                            ; (2026-11-26): was `jr c, fdl_err` (`scf / ret`), 3 B
                jr      fdl_free
fdl_done:
                or      a                   ; Cy = 0: the chain is free
                ret
