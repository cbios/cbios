; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fat-prim-body.inc — the FAT12 primitive/sector layer (shared body).
;
; docs/spec-evict-diskfile-cluster.md §11 (Phase 1). This is the CALSLT-pure,
; coarse (per-sector/cluster/directory) FAT12 engine BELOW the byte-stream
; cursor: physical sector I/O (dskio_calslt/read_sector/write_sector), mount,
; directory search (fat_find/name_cmp), cluster-chain walk (fat_open/
; fat_advance/fat_next_cluster/fat_read_file_sector), free-cluster allocation
; and FAT12 write-back (fat_alloc_cluster/fat_write_fat_entry/
; fat_write_buf_allfats/fat_total_clusters/fat_count_free/
; fat_flush_data_sector), and directory create/update (fat_dir_create/
; fat_dir_update). The BYTE-GRANULAR fat_io_* cursor (fat_io_open..
; fat_io_close) is NOT part of this file — it stays main-ROM-resident in both
; builds (spec §11's "why the cursor must not move now": the ARL_GETBYTE
; polymorphic RAM vector points at it, and that vector is only valid while the
; owning code is mapped in). `fat_delete` is ALSO primitive-layer (spec §11's
; table), but it sits AFTER the cursor in the original file, so it lives in
; the separate basic/fat-delete-body.inc (kept out of THIS file to preserve the
; original byte order — primitives, then cursor, then fat_delete; see that file's
; own header).
;
; Home (the format-body.inc / pu-render.inc pattern,
; docs/spec-evict-call-format.md, docs/spec-evict-printusing.md):
;   * (retired) lean 16 KB cart, inline at this
;     file's original position (right after the file header, before the
;     fat_io_* cursor) — byte-identical to the pre-eviction build (this file
;     is a pure relocation of that exact code, nothing added/removed/
;     reordered).
;   * sub-ROM PAGE-1 tenant (sub/fatprim.asm, repack): fatprim_tenant reads
;     a DISKOP_OP selector byte (RAM) and jumps to the requested primitive
;     below, with HL/DE intact (the primitive's own real inputs). Every
;     primitive's Cy/Z/register outputs are marshalled back through the
;     DISKOP result block (RAM) by a small tenant-side wrapper in
;     sub/fatprim.asm, because Cy cannot ride back through subrom_call/
;     CALSLT (spec §4/§11, the `sub/format.asm` "Cy cannot ride back" rule).
;     `write_sector`/`dskio_calslt` below ARE the tenant's sub-local CALSLT
;     copy (this file is included there wholesale, so no separate
;     duplication is needed the way `sub/format.asm` needed one for
;     write_sector alone — here the primitive/sector layer BEING MOVED
;     already starts at dskio_calslt).
;
; 🗑️ 2026-10-19 (D-ROMSIZE step 1): THE SUB-ROM HALF OF THAT STORY IS OVER. Every
; fatprim row that reached a primitive is retired (sub/fatprim.asm); BSAVE's disk
; write, then BLOAD's disk load -- the last two callers -- moved to disk.rom, and
; sub/fatprim.asm no longer includes this file. Its ONE home is disk.rom
; (disk/kernel.asm, D-FATENG Option 2), so the `IF DISK_BUILD` / `IF SUB_BUILD`
; gates it carried are gone and every line below is disk.rom's.
;
; Clean-room: our own code, extracted verbatim from basic/fat.asm (see that
; file's header for the full FAT12/DSKIO provenance / oracle citations).

; ===========================================================================
; Physical-sector primitives — the ONLY routines that differ from disk.asm.
; They CALSLT the disk ROM's standard DSKIO entry ($4010) across slots, with the
; MSX2-TH register convention (A=drive, B=#sectors, C=media, DE=first logical
; sector, HL=buffer, CY=direction). DISKSLOT (the INIT-scan capture) is the slot;
; the target address is the FIXED $4010 offset (NOT the SYSTEM-vector bdos_entry).
; ===========================================================================

; dskio_calslt — issue one DSKIO call across slots to the in-slot disk ROM.
;   in:  A=drive, B=#sectors, C=media descriptor, DE=first logical sector,
;        HL=transfer buffer, CY=direction (clear=read / set=write).
;   out: CY=0 ok; CY=1 error (A=DSKIO error code, B=sectors not transferred).
; CALSLT clobbers IX/IY and (per its contract) the register file, but it preserves
; the direction CY into DSKIO and returns DSKIO's result CY (see file header).
; The slot word is built in RAM (CALSLT reads the slot from IYh): we reuse the
; INIT-scan CALSLT word SCAN_IY ($E0D9), dead once the REPL runs, exactly as
; bdos_call did. DSKIO_ENTRY = $4010 is the disk-ROM interface table's DSKIO offset
; (MSX2 TH §5; header is 16 bytes so the table starts at +$10).
; 🏗️ D-FATENG Option 2: the CROSS-SLOT half is omitted from disk.rom, which
; reaches the drive LOCALLY. Gating it out keeps DISKSLOT / SCAN_IY -- basic-core
; cells the disk ROM does not define -- out of that assembly.
; 🔴 BUT THE ERROR MAPPING BELOW IS **NOT** GATED OUT ANY MORE, AND THAT WAS A
; REAL DEFECT, NOT A TIDY-UP (D-DPLMOVE, spec-diskcode-eviction.md §6.6o). It
; used to sit inside the ELSE arm, so `disk.rom` mapped no DSKIO failure to an
; MSX ERR code and wrote no DISKOP_ERR. It did not matter while every disk-ROM
; verb reached the drive by calling BACK into main's engine -- `hk_dpload` is the
; first that does not, and at an EMPTY DRIVE `LOAD"X"` printed `load error`
; instead of raising ERR 70, because main's `disk_error` raises what is in
; DISKOP_ERR and nothing had put anything there. One `l.load` row went red
; against twelve green siblings.
; 🗑️ 2026-10-19 (D-ROMSIZE step 1): disk.rom is this file's ONLY home now, so the
; `IF DISK_BUILD` / `ELSE` around the next routine is gone with its ELSE arm --
; the CROSS-SLOT dskio_calslt (CALSLT to DISKSLOT's $4010 through SCAN_IY, with
; D-BLDI's "give a caller that ran with interrupts on its EI back after each
; sector": scratchpad/verbclock_run.out). Its last callers were the sub-ROM BLOAD
; tenant's disk reads, which disk.rom's hk_dpbload does now -- with `ei` at its
; entry and the driver keeping the caller's state per sector (fdc_di_save), the
; same TIME behaviour by another road (D-LOADDI).
; read_sector / fatprim_write_sector and the two buffers' entries -- HERE, in front of
; dskio_calslt, so the read path FALLS INTO it (the comments further down say what each
; entry promises). 💰 D-DISKREREAD (2026-11-26): both directions share `ld bc,$0100`
; (B = 1 sector, C = 0 = not a media ID, D-SIDES / D-SIDEWRITE) and fall into the DSKIO
; call instead of each loading B/C/A and `jr dskio_calslt`, 8 B.
fws_mbuf:                                    ; D-DISKROMHOST step 2b: the 3 sites' `ld      hl, FAT_MBUF`, in front
                ld      hl, FAT_MBUF
fatprim_write_sector:
                xor     a               ; drive 0 (ignored)
                scf                     ; Cy = 1 = WRITE direction (MSX2 TH DSKIO)
                jr      rw_go           ; (ld bc leaves Cy alone)
; 💰 D-DISKROMHOST step 2b (2026-11-24): the two buffers' own entries, in front -- `ld hl,<buf>`
; + `call read_sector` stood at 15 sites (8 FAT_DBUF, 7 FAT_MBUF); they call / jump here now.
rs_mbuf:
                ld      hl, FAT_MBUF    ; the metadata buffer
                jr      read_sector
rs_dbuf:
                ld      hl, FAT_DBUF    ; the data buffer; falls into read_sector
read_sector:
                call    rh_check        ; D-PUTCACHE: a held PUT sector -- served or
                                        ; written out first (disk/driver.asm)
read_sector_nh:                         ; D-LOADHOLD: past the check (frs_to's entry)
                xor     a               ; drive 0 (ignored); also Cy = 0 = read
rw_go:
                ld      bc, $0100       ; B = one sector; C = 0: not a media ID -> DSKIO keeps the
                                        ; mounted disk's (D-SIDES; fat_geom_read records it). 🔴
                                        ; D-SIDEWRITE (2026-11-10): the write's C was $F9 once, which
                                        ; DSKIO seats by -- a write to a SINGLE-SIDED disk landed on
                                        ; another sector. Falls into dskio_calslt: returns to our
                                        ; caller with DSKIO's CY (and the DISKOP_ERR mapping, §6.6o)
dskio_calslt:
                call    bdos_dskio          ; LOCAL (D-DISKVE: through DISKVE's handler); no slot to cross, and the
                                            ; inter-slot call disappears with it
                                            ; (the `jr dc_result` to the next line went
                                            ; to D-DISKCARVE6, 2026-11-17: 2 B)
dc_result:
                ; D-DISKERR (docs/spec-basic-diskerr.md): DSKIO's error byte (published:
                ; 0 write protected, 2 not ready, 4 CRC, 6 seek, 8 record not found,
                ; 10 write fault, 12 other) becomes the MSX ERR code the verb raises --
                ; 70 `Disk offline`, 68 `Disk write protected`, else 69 `Disk I/O
                ; error` -- and a successful transfer clears it, so a later non-DSKIO
                ; failure in the same verb keeps its own face.
                jr      c,dc_fail
                xor     a
                ld      (DISKOP_ERR),a      ; CY clear: no code pending
                ret
dc_fail:
                push    af
                ld      c,70                ; not ready (2) -> Disk offline
                cp      2
                jr      z,dc_map
                ld      c,68                ; write protected (0) -> Disk write protected
                or      a
                jr      z,dc_map
                ld      c,69                ; anything else -> Disk I/O error
dc_map:
                ld      a,c
                ld      (DISKOP_ERR),a
                pop     af
                scf
                ret

; read_sector — read one logical sector into a buffer via cross-slot DSKIO.
;   in:  DE = logical sector number, HL = buffer
;   out: Cy = 0 ok, Cy = 1 error (A = DSKIO error code)
; Ported from disk.asm read_sector; the body is identical except the tail call now
; goes through dskio_calslt (cross-slot $4010) instead of the local dskio.

; 🏗️ D-FATENG Option 2: THE ONLY PLACE THE TWO HOMES DIFFER. Assembled into
; disk.rom the primitive reaches its OWN DSKIO by a plain local call; assembled
; into sub.rom (or main) it must CALSLT across to whatever disk ROM holds the
; slot. Everything else in this file is identical on both sides, which is what
; makes one source possible at all. (2026-10-19: there is one home left, disk.rom
; -- BLOAD's disk load followed BSAVE's write there and the sub-ROM's copy went --
; so the difference, and dskio_calslt's gate, are gone.)
; ⚠️ ONE LABEL, CONDITIONAL ONLY ON THE DIFFERING INSTRUCTION. The first cut
; put a whole `read_sector:` in each arm, and `check_dead_code` does not read
; `IF/ELSE` -- it kept one arm's span, lost `dskio_calslt`'s only reacher, and
; reported it plus dc_fail/dc_map as 47 B of dead code in [sub]. With the label
; outside, the span carries BOTH `jp` edges and the graph stays honest on both
; sides.
; 💰 D-DISKROMHOST step 2b (2026-11-24): the two buffers' own entries, in front -- `ld hl,<buf>`
; + `call read_sector` stood at 15 sites (8 FAT_DBUF, 7 FAT_MBUF); they call / jump here now.
; (rs_mbuf / rs_dbuf / read_sector / read_sector_nh: moved in front of dskio_calslt,
; D-DISKREREAD 2026-11-26 -- they fall into it now.)

; write_sector — write one logical sector from a buffer via cross-slot DSKIO.
;   in:  DE = logical sector number, HL = buffer (512 bytes)
;   out: Cy = 0 ok, Cy = 1 error (A = DSKIO error code)
; Ported from disk.asm write_sector; identical body, cross-slot tail call.
; NAMED fatprim_write_sector (not write_sector): sub/format.asm (retired
; 2026-11-11) DID define its own private write_sector (FORMAT's sub-local CALSLT write
; path, with its own FMT_RESULT stash) in the SAME sub.rom assembly as
; this tenant (sub/fatprim.asm) -- pasmo has one flat global namespace, so
; the two cannot share the name. (The retired lean cart aliased `write_sector equ
; fatprim_write_sector` right after this include, in basic/fat.asm, so every
; external call site — field.asm/files.asm's `call write_sector` —
; is unaffected; the repack build's resident shim (basic/fat.asm) is
; independently named `write_sector` already (a different code path, no
; collision there). Zero-byte rename either way (EQU never emits bytes).
; (fatprim_write_sector: disk.rom only since 2026-10-19, and the name still
; matters there -- disk/kernel.asm has a `write_sector` of its own.)
; (fws_mbuf / fatprim_write_sector: in front of dskio_calslt with the read entries,
; D-DISKREREAD 2026-11-26.)

; ===========================================================================
; FAT12 READ layer (ported verbatim from disk.asm §FAT12 layer).
; Sources: Microsoft FAT filesystem specification (BPB, FAT, directory) +
; ECMA-107 (geometry). All algorithms unchanged from disk.asm.
; ===========================================================================

; fat_mount — derive FAT12 geometry into scratch: from the media ID in the FAT's
; first byte (D-MEDIAID, disk/fat.asm fat_media -- what both references do), or,
; with no media ID there, from the boot sector's BPB.
;   out: Cy = 0 ok (geometry valid), Cy = 1 error (FDC error / not 512 B per sec)
fat_mount:
                call    fat_geom_read   ; FAT_DBUF = BPB fields (disk/fat.asm)
                ret     c
                ; require 512 bytes per sector ($0200 LE) — matches FAT_DBUF
                ; D-DISKREREAD (2026-11-26): one word load and `xor 2 / or l`, 4 B
                ld      hl, (FAT_DBUF + BPB_BYTSPERSEC)
                ld      a, h
                xor     2
                or      l
                jr      nz, fat_mount_bad   ; must be $0200 = 512
                ld      a, (FAT_DBUF + BPB_SECPERCLUS)
                ld      (FAT_SECPERCLUS), a
                ld      c, a                ; D-DISKREREAD (2026-11-26): C = SPC for fm_div, kept from
                                            ; here (nothing below touches C, hl_x32 included), 3 B
                ld      hl, (FAT_DBUF + BPB_RSVDSECCNT)
                ld      (FAT_FATSTART), hl  ; first FAT sector = reserved sectors
                ; first root sector = reserved + numFATs * secPerFAT
                ld      a, (FAT_DBUF + BPB_NUMFATS)
                ld      (FAT_NUMFATS), a    ; cache for the write path's FAT sync
                ld      b, a
                ld      de, (FAT_DBUF + BPB_FATSZ16)
                ld      (FAT_SECPERFAT), de ; cache for per-copy sector stride
                ; D-DISKREREAD (2026-11-26): HL still holds RSVDSECCNT (= FAT_FATSTART), so
                ; accumulate onto it instead of from 0 and re-adding FATSTART, 8 B
fm_fatacc:
                add     hl, de
                djnz    fm_fatacc           ; HL = reserved + numFATs * secPerFAT
                ld      (FAT_FIRSTROOT), hl
                push    hl                  ; D-DISKREREAD: kept for FIRSTDATA below
                ; root sectors = (rootEnts*32 + 511) / 512  (512 B per sector)
                ld      hl, (FAT_DBUF + BPB_ROOTENTCNT)
                call    hl_x32              ; HL = rootEnts * 32 -- D-DISKREREAD (2026-11-26):
                                            ; the shared x32 (disk/driver.asm: HL and flags only), 2 B
                ld      de, 511
                add     hl, de
                ld      e, h
                srl     e                   ; DE = HL >> 9 (== H >> 1, < 256) = root sectors
                ld      d, 0                ; (not kept: fat_rootsecs)
                ; first data sector = firstRoot + rootSecs
                pop     hl                  ; D-DISKREREAD (2026-11-26): FIRSTROOT from the stack, 3 B
                add     hl, de
                ld      (FAT_FIRSTDATA), hl
                ; 🔴 D-WCACHE W1 (docs/spec-diskwcache.md): THE CLUSTER COUNT, ONCE, HERE.
                ; fat_total_clusters used to re-read the boot sector for it on every
                ; call -- one READ per cluster a sequential write allocates
                ; (scratchpad/fdcseq_run.out: `R0/1` before every allocation). The boot
                ; sector is in FAT_DBUF now, and only this routine writes the geometry,
                ; so the count is as trustworthy across calls as FAT_FIRSTDATA. It
                ; lives where FAT_ROOTSECS did: that one is FIRSTDATA - FIRSTROOT.
                ex      de, hl              ; DE = first data sector
                ld      hl, (FAT_DBUF + 19) ; total sectors 16-bit (BPB +19)
                or      a
                sbc     hl, de              ; HL = data sectors
                ld      de, 2               ; DE accumulates 2 + dataSectors / secPerClus (D-DISKREREAD
                                            ; 2026-11-26: the +2 up front, not two `inc de` after, 2 B;
                                            ; C = SPC since the top, 3 B)
                ; 🔴 D-FATODDGEO (2026-11-26): FLOOR(data sectors / SPC), as the FAT spec counts clusters. The
                ; loop stopped only when the remainder hit EXACTLY 0: a count that is not a multiple of SPC
                ; (5 with SPC 2: 5, 3, 1, $FFFF, ...) wrapped and, with an even SPC, never reached 0 -- the mount
                ; HUNG. 3 B shorter. B = 0 on exit, as the djnz above left it.
                ld      b, 0                ; BC = SPC
                or      a                   ; Cy = 0; it stays 0 on every pass that loops
fm_div:
                sbc     hl, bc
                jr      c, fm_divdone       ; fewer than SPC left: a partial cluster is no cluster
                inc     de
                jr      fm_div
fm_divdone:                                 ; DE = + 2 (first data cluster is 2) already
                ld      (FAT_TOTCLUS), de
                xor     a
                ld      (FAT_DEFER), a      ; D-WCACHE W3a: no FAT write deferred
                ; 🔴 M30's hint RESET, and it must live here once the swap lands.
                ; It currently sits in fat_mount_tail (disk/kernel.asm) -- which a
                ; call-graph walk shows the swap ORPHANS, because the body's
                ; fat_mount replaces the routine that reaches it. Porting the READ
                ; and the ADVANCE without this would leave the hint climbing
                ; monotonically and never restarting: allocation would walk off the
                ; end of the volume and report the disk full while free clusters
                ; remained. fat_mount runs exactly once at the start of every BDOS
                ; file operation and always precedes any fat_alloc_cluster, so
                ; resetting on the SUCCESS path gives the hint per-operation
                ; lifetime with no free-invalidation surface
                ; (tier2-m30-alloc-hint-spec.md §2/§3.2).
                ld      hl, 2
                ld      (FAT_ALLOCHINT), hl
                ret                         ; Cy = 0 success: the `xor a` above (no flag-setting op
                                            ; since) -- D-DISKREREAD (2026-11-26): its `or a` went, 1 B
fat_mount_bad:
                scf
                ret

; fat_find — search the root directory for an 8.3 file name.
;   in:  HL = pointer to an 11-byte name field (8 name + 3 ext, space-padded)
;   out: Cy = 0 found  -> FAT_FIRSTCLUS, FAT_FILESIZE set; Cy = 1 not found / error
; Compare is case-insensitive; FAT directories store upper-case 8.3 names.
ff_name:                                    ; D-DISKROMHOST step 2b: the 5 sites' `ld      hl, DISK_FCB_NAME`, in front
                ld      hl, DISK_FCB_NAME
fat_find:
                ld      (FAT_NAMEPTR), hl
                call    fat_dir_start       ; DIRSEC / DIRREM: the whole root
ff_secloop:
                call    dirrem_z
                jr      z, ff_notfound      ; scanned every root sector
                ld      de, (FAT_DIRSEC)
                call    rs_dbuf
                ret     c                   ; propagate FDC error
                ld      hl, FAT_DBUF
                ld      b, 16               ; 512 / 32 entries per sector
ff_entloop:
                push    bc
                push    hl
                ld      a, (hl)
                or      a
                jr      z, ff_endmark       ; $00 = end of directory
                cp      $E5
                jr      z, ff_skip          ; deleted entry
                push    hl
                ld      de, 11
                add     hl, de
                ld      a, (hl)             ; attribute byte (+11)
                pop     hl
                and     $18                 ; volume-label | directory -> skip
                jr      nz, ff_skip
                call    nc_ptr
                jr      z, ff_found
ff_skip:
                pop     hl
                ld      de, 32
                add     hl, de              ; next 32-byte directory entry
                pop     bc
                djnz    ff_entloop
                call    dir_nextsec         ; 💰 D-DISKCARVE6: FAT_DIRSEC += 1, FAT_DIRREM -= 1
                jr      ff_secloop
ff_endmark:
                pop     hl
                pop     bc
ff_notfound:
                scf
                ret
ff_found:
                pop     hl                  ; HL = directory entry
                pop     bc
                ; record the entry's location (sector + byte offset) for fat_delete.
                ; FWR_DIRSEC/FWR_DIROFF are write-side scratch the read path ignores
                ; and the write path re-sets, so reusing them here is safe.
                ld      de, FAT_DBUF
                call    slot_rec            ; D-DISKREREAD (2026-11-26): shared with fdcr_useslot, 9 B net
                push    hl
                ld      de, 26
                add     hl, de
                ld      a, (hl)             ; first cluster low (+26) -- kept in A: the `or a`
                ld      e, a                ; below sets fat_find's exit Z from it, as before
                inc     hl
                ld      d, (hl)             ; first cluster high (+27)
                inc     hl                  ; D-DISKREREAD (2026-11-26): walk on to +28, not a
                ld      (FAT_FIRSTCLUS), de ; second pop / push / `add` from the entry, 4 B
                ld      de, FAT_FILESIZE
                call    copy4               ; file size (+28..31, LE) -- D-DISKREREAD: copy4, 2 B
                pop     hl
                or      a                   ; Cy = 0 found
                ret
; slot_rec -- D-DISKREREAD (2026-11-26): record a directory slot's place for fat_delete /
; fat_dir_update: FWR_DIRSEC := FAT_DIRSEC, FWR_DIROFF := HL - DE (the slot's offset in the
; sector buffer DE). The two writers (ff_found, fdcr_useslot) each carried it inline.
;   in HL = the slot, DE = its sector buffer; out HL kept, DE = FAT_DIRSEC, A kept
slot_rec:
                push    hl
                or      a
                sbc     hl, de              ; HL = the offset within the sector
                ld      (FWR_DIROFF), hl
                ld      de, (FAT_DIRSEC)
                ld      (FWR_DIRSEC), de
                pop     hl
                ret

; name_cmp — compare two 11-byte 8.3 name fields, case-insensitive.
;   in:  HL = directory entry name, DE = search name
;   out: Z set if equal; trashes A, BC, DE, HL
nc_ptr:                                    ; D-DISKROMHOST step 2b: the 2 sites' `ld      de, (FAT_NAMEPTR)`, in front
                ld      de, (FAT_NAMEPTR)
name_cmp:
                ld      b, 11
nc_loop:
                ld      a, (de)
                cp      '?'                 ; '?' in the pattern (DE) matches ANY entry
                jr      z, nc_wild          ; char (FILES/KILL wildcard; MSX FCB '?')
                call    toupper_cmp
                ret     nz
nc_wild:
                inc     hl                  ; 16-bit inc: leaves flags intact
                inc     de
                djnz    nc_loop             ; djnz leaves flags intact
                ret                         ; Z set (final cp c on a match, or the
                                            ; cp '?' when the last char is a wildcard)

; toupper — fold a..z to A..Z; all other bytes unchanged.
;   in: A, out: A
toupper:
                cp      $61                 ; 'a'
                ret     c
                cp      $7B                 ; 'z' + 1
                ret     nc
                sub     $20
                ret

; cpy_up11 -- copy the 11-byte 8.3 name (HL) to DE, upper-cased: what reaches the
; DISK is upper case on the CF-3300 (OPEN twice, 2026-10-28). Leaves HL / DE past
; the 11 bytes, as the `ld bc,11 / ldir` it replaces did. Clobbers A, B.
cpy_up11:
                ld      b, 11
cu11_lp:
                ld      a, (hl)
                call    toupper
                ld      (de), a
                inc     hl
                inc     de
                djnz    cu11_lp
                ret

; hoc_cmp -- the open check's compare (OPEN, KILL, NAME, COPY): HL = an open row's
; name, DE = DISK_FCB_NAME. Z = equal, byte for byte, with `?` in DE matching any
; character as name_cmp does -- but CASE-SENSITIVE: on the CF-3300 a second OPEN of
; `ts.dat`, and KILL / NAME / COPY of it, all go through while `TS.DAT` is open; only
; the same spelling is refused (scratchpad/fcbcase2_run.out, dupopen s.case). The
; names keep the typed case since 2026-10-28. Clobbers A, B, DE, HL.
hoc_cmp:
                ld      b, 11
hcm_lp:
                ld      a, (de)
                cp      '?'
                jr      z, hcm_next
                cp      (hl)
                ret     nz
hcm_next:
                inc     hl
                inc     de
                djnz    hcm_lp
                ret                         ; Z: the last test was equal (or '?')

; fat_open — start sequential reading of the file found by fat_find.
fat_open:
                ld      hl, (FAT_FIRSTCLUS)
fat_open_at:                                ; (fat_advance's tail too: carve step 4)
                ld      (FAT_CURCLUS), hl
                xor     a
                ld      (FAT_CLUSSEC), a
                ret

; fat_advance — step the iterator to the next cluster in the chain.
fat_advance:
                ld      hl, (FAT_CURCLUS)
                call    fat_next_cluster
                jr      fat_open_at         ; FAT_CURCLUS := HL, the sector index 0

; fat_next_cluster — follow the FAT12 chain one link.
;   in:  HL = current cluster
;   out: HL = next cluster (12-bit; >= $0FF8 means end-of-chain)
; FAT12 packs 1.5 bytes per entry, so an entry can straddle a 512-byte sector
; boundary; the high byte is then read from the following FAT sector.
; (Microsoft FAT spec §3.2.)
fat_next_cluster:
                call    fat_locate          ; 💰 D-DISKCARVE4: HL = the entry's FAT sector,
                                            ; PARITY / BYTEIDX set (disk/kernel.asm; the
                                            ; sub ROM no longer assembles this body)
                ld      (FAT_FATSEC), hl
                ex      de, hl              ; DE = the FAT sector
                ; 🐌 D-LOADFAT (2026-11-06): LOAD read this FAT sector into FAT_DBUF
                ; before EVERY cluster -- a seek to track 0 and a rotational wait
                ; each time (scratchpad/fdccmd_run.out). While hk_dpload's
                ; tokenised read has FAT_DEFER's bit 7 set, the sector goes into
                ; FAT_MBUF instead and is read only when FAT_FCACHE's number
                ; changes. Off everywhere else, so every other reader is unchanged.
                ; 🔴 THE SWITCH IS A BIT OF FAT_DEFER, NOT FAT_FCACHE ITSELF: the first
                ; cut tested FAT_FCACHE (0 = off) and cleared it in fat_mount -- under
                ; MSX-DOS that byte is not ours, and p1dta-acceptance's BDOS SEQRD
                ; came back $C9 with the record unread. FAT_DEFER is measured free
                ; there and already zeroed by every mount; FAT_FCACHE is now written
                ; only inside a BASIC LOAD.
                ld      bc, FAT_DBUF
                ld      a, (FAT_DEFER)
                rla                         ; bit 7: LOAD's cache is on
                jr      nc, fnc_read        ; off: as before
                inc     d
                dec     d
                jr      nz, fnc_read        ; sector >= 256: never a FAT one, uncached
                ld      bc, FAT_MBUF
                ld      a, (FAT_FCACHE)
                cp      e
                jr      z, fnc_have         ; the sector FAT_MBUF already holds
                ld      a, e
                ld      (FAT_FCACHE), a     ; (sector 0 is the boot sector: never here)
fnc_read:       ld      h, b
                ld      l, c
                push    bc
                call    read_sector
                pop     bc
fnc_have:       ; byte0 = buf[byteidx]
                call    fat_b01             ; B0 stored, A = byte1 -- unless the entry straddles (Z)
                jr      nz, fnc_combine     ; D-DISKREREAD (2026-11-26): fat_b01 shared with fac, 16 B here
fnc_straddle:
                ld      hl, (FAT_FATSEC)
                inc     hl
                ex      de, hl
                call    rs_dbuf
                ld      a, (FAT_DBUF)
fnc_combine:
                ; D-DISKREREAD (2026-11-26): FAT_B0 / FAT_B1 are ADJACENT cells (disk/equates.inc), so the
                ; odd case nibble-rotates them in place with two `rrd` and both cases end in ONE word
                ; load -- 20 B. Out as before: HL = the entry, Cy = 0 (`and`); A = H now, not B0.
                ; The rotation leaves B0/B1 changed: no reader after this (fcf writes its own B0).
                ld      hl, FAT_B1
                ld      (hl), a
                ld      a, (FAT_PARITY)
                or      a
                jr      z, fnc_even
                ; odd cluster: next = (B1 << 4) | (B0 >> 4)
                xor     a
                rrd                         ; (B1) := B1 >> 4; A low = B1 low
                dec     hl
                rrd                         ; (B0) := B1 low << 4 | B0 >> 4
fnc_even:
                ; even cluster: next = B0 | ((B1 & $0F) << 8)
                ld      hl, (FAT_B0)        ; L = B0, H = B1
                ld      a, h
                and     $0F
                ld      h, a
                ret

; fat_b01 -- D-DISKREREAD (2026-11-26): the "B0 / byte1 / straddle?" fetch fnc_have and
; fac_entry_from_wbuf each carried inline (22 B net). in BC = the FAT sector's buffer,
; FAT_BYTEIDX = the entry's low byte (0..511). out FAT_B0 = buf[idx], A = buf[idx+1] (a
; speculative read of buf[512] when idx = 511), HL = &buf[idx+1], Z = the entry straddles
; (idx == 511); DE clobbered, BC kept.
fat_b01:
                ld      hl, (FAT_BYTEIDX)
                add     hl, bc
                ld      a, (hl)
                ld      (FAT_B0), a
                inc     hl
                ld      a, (hl)
                ld      de, (FAT_BYTEIDX)
                inc     de
                dec     d
                dec     d                   ; idx + 1 == $0200 <=> idx == 511
                ret

; fat_read_file_sector — read the open file's next data sector into FAT_DBUF.
;   out: Cy = 0 ok (FAT_DBUF holds 512 bytes), Cy = 1 end-of-file / error
; The caller bounds the true end of file with FAT_FILESIZE; this returns Cy = 1
; once the cluster chain reaches an end-of-chain marker.
; frs_to -- the same, into the 512 bytes at HL (D-LOADDIRECT: LOAD reads its full
; sectors straight into the program area, as the CF-3300 does,
; scratchpad/fdccmd_cf1107.out).
fat_read_file_sector:
                ld      hl, FAT_DBUF
frs_to:
                push    hl                  ; [destination]
                call    frs_head
                pop     hl                  ; the destination (flags kept)
                ret     c                   ; no data cluster: end of file
                ; 🐌 D-LOADHOLD (2026-11-11): a file's data sector is read PAST rh_check.
                ; The check's fast path (~85 T a sector, D-PUTCACHE) was enough to lose
                ; disk revolutions in LOAD's per-sector loop: 7 KB 1.65 -> 2.65 s, 14 KB
                ; 3.07 -> 3.47 s (scratchpad/loadhold_bisect.out, loadhold_after.out; bisected to 3c2709ad and
                ; a6c09b83, which only moved where the misses fell). Safe here: the
                ; held sector lives in FAT_MBUF, and this reads into FAT_DBUF or LOAD's
                ; program area; FAT_MBUF readers (the FAT walk, GET, the directory)
                ; still pass the check, so the hold is flushed before FAT_MBUF is
                ; reused. The one read it skips -- the HELD sector itself into another
                ; buffer -- needs one file open twice, which BASIC refuses ("File
                ; already open").
                call    read_sector_nh
                ret     c
                ld      hl, FAT_CLUSSEC
                inc     (hl)
                or      a                   ; Cy = 0 success
                ret
; frs_head -- the next sector's number, WITHOUT reading it. Moves to the next cluster
; when this one is used up; out DE = the absolute sector, Cy = 1 when FAT_CURCLUS is
; no data cluster (free, end of chain). Split out for D-LOADRUNS's multi-sector runs
; (measured slower and removed, disk/runtime.asm hd_full); kept, as it is 6 B
; shorter than the inline form (one unsigned compare for both cluster bounds).
frs_head:
                ld      a, (FAT_CLUSSEC)
                ld      hl, FAT_SECPERCLUS
                cp      (hl)
                call    nc, fat_advance     ; current cluster exhausted -> next
                ; data clusters are 2..$0FF7: cluster - 2 < $0FF6, unsigned
                ld      hl, (FAT_CURCLUS)
                dec     hl
                dec     hl
                ld      de, $0FF6
                or      a
                sbc     hl, de
                ccf
                ret     c                   ; free (< 2) or end of chain (>= $0FF8)
                ; sector = firstData + (cluster-2)*secPerClus + clussec: clus_sec's
                ; arithmetic (disk/kernel.asm), which stood here a second time (D-DSKFMT
                ; defrag, 2026-11-09). The caller pops HL and reads DE / Cy only.
                ld      hl, (FAT_CURCLUS)
                ld      a, (FAT_CLUSSEC)
                jp      clus_sec            ; DE = the absolute sector (Cy = 0)

; (D-FATENG Option 2 assembled the next routine in the sub-ROM only -- `IF
; DISK_BUILD / ELSE` -- because it touched basic-core's FCH_ACTIVE / FCH_MODES.)
; 🗑️ fat_restage_channel IS GONE (S10.C, 2026-10-15). It re-read into FAT_DBUF
; whatever the LIVE channel had staged there before a switch (D-FCH S-FCH-1,
; docs/spec-basic-filechan-alloc.md §3.1: the shared buffer as a CACHE, as the
; reference treats it), and only an INPUT channel ever staged a sector (mode 2,
; the other, left with D-MODE2DEAD). An INPUT channel is disk.rom's FCB now and
; its read-ahead is the block's own record, so there is nothing to re-stage.

; ===========================================================================
; FAT12 WRITE-BACK substrate (ported verbatim from disk.asm §FAT12 write-back).
; All structures (free-cluster scan, 12-bit entry pack, multi-FAT sync, directory
; create/update) realise the Microsoft FAT specification; the physical sector
; write goes through write_sector (cross-slot DSKIO with CY=1). Buffer discipline
; unchanged: the file DATA being accumulated lives in FAT_DBUF
; (fat_flush_data_sector writes it out); the FAT/dir METADATA helpers use the
; independent FAT_MBUF, so a cluster scan or dir stamp never disturbs the in-flight
; data sector.
; ===========================================================================

; 🗑️ 2026-10-19 (D-ROMSIZE step 1): THIS WRITE HALF WAS `IF DISK_BUILD` FOR A FEW
; HOURS, and is plain again because disk.rom is the file's only home. The sub-ROM's
; only writer through it was BSAVE's disk engine (sub/save.asm), which left for
; disk.rom that day, and its only DSKF / KILL users went earlier (D-DSKFLOCAL,
; D-KILLLOCAL): `make basic-reloc`'s dead-code gate found 888 B of it unreachable
; in the sub build. The READ half followed with BLOAD's disk load (disk.rom
; hk_dpbload), and sub/fatprim.asm's include went. The `IF SUB_BUILD` arms this
; half carried (the sub's own fat_write_fat_entry_hot and fat_write_buf_allfats --
; disk.rom's are in disk/fat.asm's $47C1 fill -- ffds_full's D-ASAVEFULLSTAMP
; stamp, the fixed 0821h date) went with the write half; so did the sub's own
; fat_dir_start / fat_rootsecs (below) with the read half.
; fat_read_fat_sector — read FAT-copy-0 sector that holds cluster N's entry.
;   in:  HL = cluster number
;   out: FAT_MBUF holds that FAT sector; (FAT_FATSEC) = its absolute sector;
;        (FAT_BYTEIDX) = byte index of the entry's low byte within the sector;
;        (FAT_PARITY) = cluster & 1; Cy reflects the read.
fat_read_fat_sector:
                call    fat_locate          ; 💰 D-DISKCARVE4: HL = the entry's FAT sector,
                                            ; PARITY / BYTEIDX set (disk/kernel.asm; the
                                            ; sub ROM no longer assembles this body)
                jp      rd_fatsec           ; D-DISKREREAD (2026-11-26): FAT_FATSEC := HL, read into
                                            ; FAT_MBUF -- disk/kernel.asm's copy of these 3 lines, 4 B

; fat_alloc_cluster — find a free ($000) cluster, mark it EOC, sync all FATs.
;   out: Cy = 0 ok, HL = the allocated cluster number; Cy = 1 = disk full / error
; (Microsoft FAT spec §3.2: $000 = free, $FF8-$FFF = end-of-chain.)
fat_alloc_cluster:
                call    fat_total_clusters  ; DE = total clusters (reads boot sector)
                ret     c                   ; D-DISKFULL: a failed boot-sector read is
                                            ; ITS code (70 at an empty drive), not 66 --
                                            ; DE = 0 would scan nothing and say "full"
                ld      (FAT_WRTMP), de
                ; FAT_FATSEC tracks which FAT sector is in FAT_MBUF; -1 = none loaded.
                call    fac_cacheinit       ; 🐌 D-RECSLOW: HL = $FFFF, or (FAT_FATSEC)
                ld      (FAT_WRTMP2), hl    ; when FAT_MBUF is dirty (disk/fat.asm)
                ; 🏗️ D-FATENG: M30's per-operation allocation hint, DISK-SIDE ONLY.
                ; disk/fat.asm scans from FAT_ALLOCHINT instead of restarting at 2
                ; every time; the hint is reset to 2 by fat_mount_tail
                ; (disk/kernel.asm) at the start of each BDOS operation and
                ; advanced past each claimed cluster by fac_found below, so it
                ; returns the SAME cluster sequence as a from-2 scan, faster
                ; (tier2-m30-alloc-hint-spec.md §4). Byte-size-neutral: `ld hl,2`
                ; and `ld hl,(nn)` are both 3 bytes.
                ; ⚠️ GATED, NOT ADOPTED WHOLESALE: basic-core has no FAT_ALLOCHINT
                ; cell and giving it one is a RAM allocation, not a port. Each side
                ; keeps exactly what it has today.
                ; 🔴 AND WITHOUT THIS PORT THE SWAP WOULD HAVE LEFT DEAD STATE THE
                ; GATES CANNOT SEE: fat_mount_tail keeps RESETTING FAT_ALLOCHINT in
                ; disk/kernel.asm, which survives the swap -- a RAM write nothing
                ; reads, which no dead-code sweep reports because it is not an
                ; unreachable routine.
                ld      hl, (FAT_ALLOCHINT) ; first candidate cluster
fac_loop:
                ld      de, (FAT_WRTMP)
                push    hl
                or      a
                sbc     hl, de
                pop     hl
                jr      nc, fac_full        ; cluster >= total -> disk full
                ; which FAT sector + byte index holds cluster HL's entry?
                push    hl
                call    fat_locate          ; 💰 D-DISKCARVE4: HL = the entry's FAT sector,
                                            ; PARITY / BYTEIDX set (disk/kernel.asm; the
                                            ; sub ROM no longer assembles this body)
                ; is this sector already in FAT_MBUF? (cached-sector compare)
                ld      de, (FAT_WRTMP2)
                push    hl
                or      a
                sbc     hl, de
                pop     hl
                jr      z, fac_have_sec     ; already loaded -> no re-read
                push    hl
                call    fac_flushdirty      ; 🐌 D-RECSLOW: a dirty buffer goes out
                pop     hl                  ; before another sector replaces it
                jr      c, fac_rderr
                ld      (FAT_WRTMP2), hl    ; remember the new cached sector
                call    rd_fatsec           ; FAT_FATSEC := HL, read it into FAT_MBUF (Cy)
                jr      c, fac_rderr
fac_have_sec:
                ; read the 12-bit entry from FAT_MBUF (handles straddle into next sec).
                pop     hl                  ; HL = cluster
                push    hl
                call    fac_entry_from_wbuf ; DE = entry value
                ld      a, d
                or      e
                pop     hl
                jr      z, fac_found        ; $000 -> free
                inc     hl
                jr      fac_loop
fac_rderr:
                pop     hl
                ret                         ; Cy set from read_sector
fac_found:
                ; claim it: write EOC into every FAT copy, return the cluster.
                push    hl
                ld      de, EOC
                call    fat_write_fat_entry_hot ; HL = cluster, DE = value (W2: the
                                            ; scan just loaded its sector)
                pop     hl
                ret     c                   ; write error propagates (Cy set)
                ; M30: advance the scan hint past this claimed cluster WITHOUT
                ; disturbing HL, which must return as the allocated cluster.
                inc     hl
                ld      (FAT_ALLOCHINT), hl
                dec     hl                  ; D-DISKREREAD (2026-11-26): not a push / pop, 1 B
                or      a                   ; Cy = 0 success, HL = cluster
                ret
fac_full:
                ; D-DISKFULL (2026-10-02): leave ERR 66 `Disk full` PENDING, the
                ; way dc_fail leaves a DSKIO code, so `disk_error` raises it.
                ; Nothing wrote one before: a full disk was not a DSKIO failure,
                ; so SAVE said `load error` and CLOSE/PRINT# said nothing.
                ld      a,66
                ld      (DISKOP_ERR),a
                scf                         ; disk full
                ret

; fac_entry_from_wbuf — unpack cluster (FAT_BYTEIDX/FAT_PARITY already set) from
; the FAT sector currently in FAT_MBUF; if the entry straddles the 512-byte boundary
; (byteidx == 511) read the FOLLOWING FAT sector for the high byte.
;   out: DE = 12-bit entry value; preserves nothing but DE
fac_entry_from_wbuf:
                ld      bc, FAT_MBUF        ; D-DISKREREAD (2026-11-26): fat_b01 (above), 24 B here;
                call    fat_b01             ; BC is clobbered now on this path too (the straddle's
                jr      nz, fac_comb        ; read always did) -- HL = &MBUF[idx+1] at fac_comb, as before
fac_straddle:
                ; read the next FAT sector into FAT_MBUF and cache it -- a dirty
                ; buffer written out first (🐌 D-RECSLOW; B0 is already taken).
                call    fac_flushdirty
                ld      hl, (FAT_FATSEC)
                inc     hl
                ld      (FAT_WRTMP2), hl
                call    rd_fatsec           ; D-DISKREREAD (2026-11-26): FAT_FATSEC := HL + the read, 4 B
                ld      a, (FAT_MBUF)
fac_comb:
                ; the 12-bit unpack is fnc_combine's (above), with the result in DE --
                ; this carried a byte-identical copy (D-WRZER's carve, 2026-11-09).
                ; 🔴 HL IS KEPT, as the old copy's even path kept it: without the
                ; push / pop, L5_journal's BSAVE ran 1.4 s of FDC work under DI
                ; (programs-acceptance's gap bound; scratchpad/wz_bis5.out / wz_bis6.out
                ; bisected it to HL) although no reader of HL was found -- filed.
                push    hl
                call    fnc_combine         ; HL = the entry
                ex      de, hl
                pop     hl
                ret

; fat_write_fat_entry — set a cluster's 12-bit value in EVERY FAT copy on disk.
;   in:  HL = cluster, DE = 12-bit value to store
;   out: Cy = 0 ok, Cy = 1 error
; (Microsoft FAT spec §3.2 packing; multi-FAT sync per BPB_NUMFATS.)
; 🔴 D-WCACHE W2 (docs/spec-diskwcache.md): fat_write_fat_entry_hot -- the same,
; for a caller whose LAST FAT operation loaded FAT_MBUF, so (FAT_FATSEC) names what
; it holds: the read is skipped when the entry lives in that sector. Every FAT
; routine keeps that pair together (fac's scan and straddle, this routine's own
; straddle, fat_write_buf_allfats leaves both as they were); a caller with ANY
; other FAT_MBUF user since -- a directory scan, a mount, another channel --
; must take the plain entry. Two such callers: fac_found (straight after its
; scan) and ffds_alloc's link (straight after the allocation). It was 3 reads of
; one FAT sector per allocated cluster (scratchpad/fdcseq_w1.out).
                ; (disk.rom's copy sits in disk/fat.asm's $47C1 fill: this region,
                ; pinned at $75A5, had no room for it -- the first build assembled EMPTY.)
fwfe_zero:                                    ; D-DISKROMHOST step 2b: the 3 sites' `ld      de, 0`, in front
                ld      de, 0
fat_write_fat_entry:
                ld      (FAT_WRTMP), de     ; save the value
                push    hl                  ; save cluster
                call    fat_read_fat_sector ; FAT_MBUF = FAT sector 0; BYTEIDX/PARITY set
fwe_have:
                ; --- pack the low byte / shared nibble into FAT_MBUF[byteidx]
                ld      hl, (FAT_BYTEIDX)
                ld      de, FAT_MBUF
                add     hl, de
                push    hl                  ; HL = &buf[byteidx]
                ld      de, (FAT_WRTMP)     ; DE = value
                ld      a, (FAT_PARITY)
                or      a
                jr      nz, fwe_odd0
                ; even: buf[byteidx] = value & $FF
                ld      (hl), e             ; D-DISKREREAD (2026-11-26): not via A, 1 B
                jr      fwe_byte1
fwe_odd0:
                ; odd: buf[byteidx] = (buf[byteidx] & $0F) | ((value & $0F) << 4)
                ; D-DISKREREAD (2026-11-26): a masked merge (x ^ ((x ^ y) & m)), no B, 3 B
                ld      a, e
                rlca
                rlca
                rlca
                rlca                        ; high nibble = value & $0F
                xor     (hl)
                and     $F0
                xor     (hl)                ; low nibble from buf[byteidx]
                ld      (hl), a
fwe_byte1:
                pop     hl                  ; HL = &buf[byteidx]
                ; the second byte may live in the next FAT sector.
                ; D-DISKREREAD (2026-11-26): byteidx + 1 == $0200 <=> byteidx == 511, one
                ; test instead of two (4 B) -- still the 9-bit test D-FATSTRAD demands:
                ld      bc, (FAT_BYTEIDX)
                inc     bc
                dec     b
                dec     b
                jr      nz, fwe_b1_same     ; 🔴 D-FATSTRAD: byteidx is 9-bit (0..511),
                                            ; so the straddle case 511 = $01FF has HIGH
                                            ; BYTE 1 -- straddle iff (c==$FF AND b==1).
                                            ; This said `or a` (b==0) until 2026-09-18,
                                            ; which mis-read byteidx 255 as a straddle
                                            ; and byteidx 511 as same-sector, packing
                                            ; the entry into the WRONG SECTOR and
                                            ; corrupting FAT entries at clusters
                                            ; 170/341/682 on a 720 KB disk.
                                            ; ⚠️ THE FIX LANDED IN disk/fat.asm ON
                                            ; 2026-07-04, oracle-confirmed against stock
                                            ; ("101 real disks straddle at 511"), AND
                                            ; NEVER REACHED THIS COPY. tests/
                                            ; test_fat_write_fat_entry.py found the
                                            ; original, documents it, sets
                                            ; STRADDLE_FIXED = True -- and drives the
                                            ; DISK build, so it has been proving the
                                            ; fixed half while this half shipped the
                                            ; defect to every BASIC disk write.
                                            ; Found by D-FATDIFF's engine differential
                                            ; (scratchpad/fateng_diff.py), run before
                                            ; Option 2's swap precisely to catch a
                                            ; routine where disk's copy knew more.
                ; straddle: byte1 is buf[0] of the NEXT FAT sector. First persist
                ; this sector to all FATs, then load + patch the next sector.
                call    fat_write_buf_allfats
                jr      c, fwe_err
                ld      hl, (FAT_FATSEC)
                inc     hl
                call    rd_fatsec           ; FAT_FATSEC := HL, read it into FAT_MBUF (Cy)
                jr      c, fwe_err
                ld      hl, FAT_MBUF           ; patch byte 0 of the next sector
                ; D-FWESHARE (2026-11-11): the same even / odd store as the same-sector
                ; path below -- it stood here a second time (~41 B), HL being the only
                ; difference (FAT_MBUF here, &buf[byteidx+1] there).
                jr      fwe_b1_store
fwe_b1_same:
                ; second byte is buf[byteidx+1] in the same sector.
                inc     hl                  ; HL = &buf[byteidx+1]
fwe_b1_store:                               ; (the straddle path joins with HL = FAT_MBUF)
                ld      de, (FAT_WRTMP)
                ld      a, (FAT_PARITY)
                or      a
                jr      nz, fwe_same_odd
                ; even: buf[byteidx+1] = (buf[byteidx+1] & $F0) | ((value>>8)&$0F)
                ; D-DISKREREAD (2026-11-26): the masked merge, no B, 3 B
                ld      a, (hl)
                xor     d
                and     $F0
                xor     d
                ld      (hl), a
                jr      fwe_finish
fwe_same_odd:
                ; odd: buf[byteidx+1] = (value >> 4) & $FF
                ; D-DISKREREAD (2026-11-26): merge d.lo / e.hi, then swap nibbles, 7 B
                ld      a, d
                xor     e
                and     $0F
                xor     e                   ; A = e.hi : d.lo
                rrca
                rrca
                rrca
                rrca                        ; A = d.lo : e.hi
                ld      (hl), a
fwe_finish:
                pop     hl                  ; discard saved cluster
                ; persist the (current) FAT sector to all FAT copies -- unless the
                ; caller ASKED for this write to wait (D-WCACHE W3a, FAT_DEFER = 1:
                ; ffds_alloc, whose link to the previous cluster follows at once and
                ; writes this sector with both entries). Then it is DIRTY (2), and
                ; the next write of this buffer -- or fat_write_fat_entry_hot, before
                ; it loads another sector -- takes it out.
                ; 🐌 D-RECSLOW (2026-11-05): FAT_DEFER is two bits now -- bit 0 "the
                ; caller asks this write to wait", bit 1 "FAT_MBUF is dirty" -- so a
                ; held sector can take a second deferred patch (frnd_locate's
                ; extension: 3 -> 2); W3a's 1 -> 2 and 2 -> write are unchanged.
                ld      a, (FAT_DEFER)
                rrca
                jp      nc, fat_write_buf_allfats   ; no request: write now (it clears)
                rlca                        ; A = FAT_DEFER again (bit 0 was 1)
                and     $FE
                or      2                   ; request -> dirty; D-SAVEFAT: bit 2 (a
                ld      (FAT_DEFER), a      ; SAVE's hold) survives -- 1 / 3 -> 2 as before
                ret                         ; Cy = 0 and NZ from the `or 2` -- D-DISKREREAD (2026-11-26):
                                            ; the `or a` of the same A set the same flags, 1 B
fwe_err:                                    ; (both entries are `jr c`: Cy = 1 already --
                pop     hl                  ; discard saved cluster (keep stack sane)
                ret                         ; D-DISKREREAD 2026-11-26: its `scf` went, 1 B)

; (fat_write_buf_allfats -- FAT_MBUF back to (FAT_FATSEC) in every FAT copy -- and
; the fat_write_fat_entry_hot entry above are disk.rom's in disk/fat.asm's $47C1
; fill. The sub build's own copies stood here under `IF SUB_BUILD` and went on
; 2026-10-19 with the sub-ROM's last writer, BSAVE's disk engine.)

; fat_total_clusters — total cluster count of the volume (2 + data clusters).
;   out: DE = total clusters; preserves HL (Microsoft FAT spec §3.3).
; (the body -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $544E, disk/kernel.asm.)

; (fat_dir_start / fat_rootsecs: disk.rom's are in disk/fat.asm's $47C1 fill. The
; sub build's own copies stood here under `IF SUB_BUILD` and went on 2026-10-19
; with the sub-ROM's last reader, BLOAD's disk load.)

; fat_count_free — count free clusters (FAT entries == 0) over the data range
; [2, total). Mounts first. Used by DSKF.
;   out: DE = free cluster count (0 on mount error).
; Reads each FAT sector ONCE (caching it in FAT_DBUF, keyed by FWR_FIRST) and
; decodes the 12-bit entry for each cluster inline, counting the zeros. A naive
; one-read-per-cluster scan is correct but ~700 cross-slot DSKIO reads — far too
; slow on a real FDC; caching collapses it to a handful. Loop state lives in RAM
; (the register file is clobbered by read_sector's CALSLT): FAT_WRTMP = total,
; FAT_WRTMP2 = running count, FWR_CLUS = current cluster, FWR_FIRST = cached FAT
; sector (all write-side scratch, dead during this read-only query); FAT_B0/FAT_B1
; hold the entry's two bytes. Sources: Microsoft FAT spec (a 0 entry is free; the
; 12-bit even/odd nibble packing).
; 🏗️ D-FATENG Option 2: OMITTED FROM disk.rom, and not because it would not work.
; disk/basic-resident-abi.inc already binds `fat_count_free` as a CALLBACK:
; hk_dskf reaches MAIN's copy across the slot for DSKF, and main's is itself a
; stub onto the sub-ROM tenant. Defining it here as well is a redefinition, which
; the assembler refuses -- and it was the ONLY such clash between this body and
; the disk ABI (checked: 1 of 29 ABI names).
;  ✅ D-DSKFLOCAL (2026-09-29): THAT SEPARATE SLICE IS DONE -- the body is in
; BOTH builds now, the callback is gone from the disk ABI, and hk_dskf counts
; without leaving the slot. Main's 13 B stub onto the sub-ROM tenant had no
; other caller and is deleted.
fat_count_free:
                ld      de, $FFFF           ; no limit: DSKF counts them all
; fat_count_upto -- the same count, STOPPING once DE free clusters are found
; (D-SAVEFULLSTAMP: fat_fits only asks "at least N?", and a full count before
; every SAVE/BSAVE made both slower -- 0.42x -> 0.70x the reference's time, and
; namspc's SAVE + FILES rows overran their capture window). The limit rides in
; FAT_BYTEIDX, the allocator's per-operation scratch, dead during a count.
fat_count_upto:
                ld      (FAT_BYTEIDX), de
                call    fat_mount
                jp      c, fcf_zero
                call    fat_total_clusters  ; DE = total clusters (2 + data)
                ld      (FAT_WRTMP), de
                ld      hl, 0
                ld      (FAT_WRTMP2), hl    ; free count = 0
                ld      hl, $FFFF
                ld      (FWR_FIRST), hl     ; cached FAT sector = none
                ld      hl, 2
                ld      (FWR_CLUS), hl
fcf_loop:
                ld      hl, (FWR_CLUS)
                ld      de, (FAT_WRTMP)
                or      a
                sbc     hl, de              ; cluster - total
                jp      nc, fcf_done        ; cluster >= total -> done
                ; offset = cluster + cluster>>1 (= cluster * 3/2)
                call    fat_wclus_x15
                ; FAT sector = FAT_FATSTART + (offset>>9)
                srl     a                   ; A = offset>>9
                ld      e, a
                ld      d, 0
                ld      hl, (FAT_FATSTART)
                add     hl, de              ; HL = absolute FAT sector
                ld      de, (FWR_FIRST)
                or      a
                push    hl
                sbc     hl, de
                pop     hl
                jr      z, fcf_have         ; already cached
                ld      (FWR_FIRST), hl
                ex      de, hl
                call    rs_dbuf              ; cache this FAT sector
fcf_have:
                ; byteidx = offset & 511
                call    fat_wclus_x15
                and     1
                ld      h, a                ; HL = byteidx (0..511)
                ; D-DISKREREAD (2026-11-26): the straddle test FIRST, on a copy of the index, and
                ; byte0 / byte1 fetched under its Z (`ld` / `add hl` / 16-bit `inc` leave Z), so
                ; the same-sector case falls into fcf_haveb1 -- 11 B (was a push / pop of the
                ; index, a two-byte compare and a `jr` round the same-sector fetch)
                ld      d, h
                ld      e, l
                inc     de
                dec     d
                dec     d                   ; Z: byteidx + 1 == $0200, i.e. byteidx == 511
                ld      de, FAT_DBUF
                add     hl, de
                ld      a, (hl)
                ld      (FAT_B0), a         ; byte0 = FAT_DBUF[byteidx]
                inc     hl
                ld      a, (hl)             ; byte1 = FAT_DBUF[byteidx+1] (a speculative read at 511)
                jr      nz, fcf_haveb1
                ; straddle: byte1 lives in the next FAT sector (read into FAT_MBUF).
                ld      hl, (FWR_FIRST)
                inc     hl
                ex      de, hl
                call    rs_mbuf
                ld      a, (FAT_MBUF)
fcf_haveb1:                                 ; A = byte1
                ; the 12-bit entry is zero iff the relevant nibbles are all zero.
                ; D-DISKREREAD (2026-11-26): byte1 kept in C (B / C are dead in this loop,
                ; which reloads everything from RAM) instead of FAT_B1, one OR per case, 16 B
                ld      c, a
                ld      a, (FWR_CLUS)
                rra                         ; Cy = odd cluster
                ld      a, (FAT_B0)
                jr      nc, fcf_even
                and     $F0                 ; odd: zero iff (byte0 & $F0) | byte1 == 0
                or      c
                jr      fcf_test
fcf_even:
                ld      b, a                ; even: zero iff byte0 | (byte1 & $0F) == 0
                ld      a, c
                and     $0F
                or      b
fcf_test:
                jr      nz, fcf_used
fcf_free:
                ld      hl, (FAT_WRTMP2)
                inc     hl
                ld      (FAT_WRTMP2), hl    ; free++
                ld      de, (FAT_BYTEIDX)
                or      a
                sbc     hl, de
                jr      z, fcf_done         ; the caller's limit is reached
fcf_used:
                ld      hl, (FWR_CLUS)
                inc     hl
                ld      (FWR_CLUS), hl
                jp      fcf_loop
fcf_done:
                ld      de, (FAT_WRTMP2)
                ret
; (fcf_zero -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $43A4, disk/driver.asm.)

; fat_flush_data_sector — write FAT_DBUF (the current 512-byte data buffer) to
; the file's current data sector, allocating/extending the cluster chain first.
;   out: Cy = 0 ok, Cy = 1 = disk full / write error
; (Microsoft FAT spec §3.3 data-sector math.)

fat_flush_data_sector:
                ; zero-pad the unused tail of the buffer (bytes WRBUFLEN..511) so a
                ; partial final sector writes 512 well-defined bytes.
                ld      hl, 512
                ld      de, (FWR_BUFLEN)
                or      a
                sbc     hl, de              ; HL = 512 - WRBUFLEN (pad count, 0..512)
                jr      z, ffds_nopad       ; buffer already full -> no pad
                jr      c, ffds_nopad       ; (defensive: WRBUFLEN > 512 never happens)
                ld      b, h
                ld      c, l                ; BC = pad count
                ld      hl, FAT_DBUF
                add     hl, de              ; HL = FAT_DBUF + WRBUFLEN = first pad byte
ffds_padloop:                               ; D-DISKREREAD (2026-11-26): a do-while (the
                ld      (hl), 0             ; count is >= 1 here: both 0 and "negative"
                inc     hl                  ; left above), 2 B
                dec     bc
                ld      a, b
                or      c
                jr      nz, ffds_padloop
ffds_nopad:
                ; ensure we have a data cluster to write into.
                ld      hl, (FWR_CLUS)
                ld      a, h
                or      l
                jr      z, ffds_alloc       ; no cluster yet -> allocate the first
                ld      a, (FWR_SECIDX)
                ld      hl, FAT_SECPERCLUS
                cp      (hl)
                jr      c, ffds_haveclus    ; room in the current cluster
                call    frl_defer           ; D-WCACHE W3a: a chain EXTENSION -- the
                                            ; link below writes the mark's sector too
                                            ; (frl_defer ORs bit 0: a SAVE's hold survives)
ffds_alloc:
                ; allocate a new cluster (first one, or chain extension).
                call    fdh_rehold          ; 🐌 D-SAVEFAT: a held SAVE defers this mark
                call    fat_alloc_cluster
                jr      c, ffds_undefer     ; disk full / I/O: nothing may stay deferred
                ; HL = new cluster. Link it: if a previous cluster exists, point it
                ; at HL; else record HL as the file's first cluster.
                ld      de, (FWR_CLUS)
                ld      a, d
                or      e
                jr      z, ffds_first       ; no previous cluster -> this is first
                ex      de, hl              ; HL = previous cluster, DE = new cluster (value to
                push    de                  ; link) -- D-DISKREREAD (2026-11-26): one `ex`, 2 B
                call    fdh_rehold          ; D-SAVEFAT: ... and this link
                call    fat_write_fat_entry_hot ; previous -> new (12-bit link; W2:
                                            ; the allocation just left FAT_MBUF/FATSEC)
                pop     hl                  ; HL = new cluster
                ret     c
                jr      ffds_setclus
ffds_first:
                ld      (FWR_FIRST), hl     ; remember the file's first cluster
ffds_setclus:
                ld      (FWR_CLUS), hl
                xor     a
                ld      (FWR_SECIDX), a     ; start at sector 0 of the new cluster
ffds_haveclus:
                ; absolute sector = firstData + (cluster-2)*secPerClus + WRSECIDX: clus_sec
                ; (disk/kernel.asm) -- the same arithmetic stood here (D-DSKFMT defrag)
                ld      hl, (FWR_CLUS)
                ld      a, (FWR_SECIDX)
                call    clus_sec            ; DE = sector
                ld      hl, FAT_DBUF
                call    fatprim_write_sector
                ret     c
                ld      hl, FWR_SECIDX      ; D-DISKREREAD (2026-11-26): `inc (hl)`, 4 B -- Cy = 0
                inc     (hl)                ; still (the `ret c` above; `inc` keeps it); A / HL
                ret                         ; differ, and all 6 callers test Cy only
ffds_undefer:
                jp      frl_xfail           ; D-SAVEFAT: a HELD FAT sector reaches the
                                            ; disk first (what was linked, as before);
                                            ; a bare W3a request just dies (disk/kernel.asm)

; fat_dir_create — find/make a root-directory slot for an 8.3 name and write a
; fresh entry; record the slot's sector + offset in FWR_DIRSEC/FWR_DIROFF.
;   in:  HL = 11-byte 8.3 name field
;   out: Cy = 0 ok, Cy = 1 = directory full / I/O error
; (Microsoft FAT spec §3.4. Date 0821h, time 0: as the CF-3300, Joost 2026-10-01.)
fat_dir_create:
                ld      (FAT_NAMEPTR), hl
                call    fat_dir_start       ; DIRSEC / DIRREM: the whole root
fdcr_secloop:
                call    dirrem_z
                jr      z, fdcr_full        ; no slot in any root sector
                ld      de, (FAT_DIRSEC)
                call    rs_mbuf
                ret     c
                ld      hl, FAT_MBUF
                ld      b, 16               ; 16 entries per 512-byte sector
fdcr_entloop:
                push    bc
                push    hl
                ld      a, (hl)
                or      a
                jr      z, fdcr_useslot     ; $00 end-marker -> free slot here
                cp      $E5
                jr      z, fdcr_useslot     ; $E5 deleted -> reusable slot
                ; same-name existing entry? (truncate-in-place)
                call    nc_ptr
                jr      z, fdcr_useslot
                pop     hl
                ld      de, 32
                add     hl, de
                pop     bc
                djnz    fdcr_entloop
                call    dir_nextsec         ; 💰 D-DISKCARVE6: FAT_DIRSEC += 1, FAT_DIRREM -= 1
                jr      fdcr_secloop
fdcr_useslot:
                pop     hl                  ; HL = dir entry slot in FAT_MBUF
                pop     bc
                ; record the slot's sector + byte offset for fat_dir_update.
                ld      de, FAT_MBUF
                call    slot_rec            ; D-DISKREREAD (2026-11-26): ff_found's too (above), 9 B net
                ; write the 11-byte name, UPPER-CASED here: the caller's name keeps
                ; the typed case since 2026-10-28 (OPEN twice), and the CF-3300
                ; writes an upper-case entry from any of them -- SAVE"ts.dat", an
                ; OPEN, BDOS _FMAKE (scratchpad/fcbcase_run.out, fcbcase2_bdos.out).
                push    hl
                ex      de, hl              ; DE = dest slot
                ld      hl, (FAT_NAMEPTR)
                call    cpy_up11            ; name -> entry +0..10, upper-cased
                ; +11 = attribute $00 (normal file; ORACLE: matches MSX-DOS Create).
                ; zero +11 too, then +12..+31 (S1/S2, rec-count, alloc-map, date/time,
                ; first cluster, size), then stamp the date. D-DISKREREAD (2026-11-26):
                ; one loop of 21 with A = 0 outside it, 3 B
                xor     a
                ld      b, 21               ; +11..+31 is 21 bytes
fdcr_zero:
                ld      (de), a
                inc     de
                djnz    fdcr_zero
                pop     hl                  ; HL = the slot
                call    dir_stamp_date      ; +22..25 := time 0, date 0821h (as the CF-3300)
                ; write the dir sector back.
fdir_wrback:                                ; (fat_dir_update's tail too: carve step 4)
                ld      de, (FWR_DIRSEC)
                jp      fws_mbuf            ; D-DISKREREAD (2026-11-26): a tail jump, 3 B -- dc_result's
                                            ; success exit is `xor a` (A = 0, Z, Cy = 0), exactly what
                                            ; the `or a` made; its error exit is the same Cy = 1
fdcr_full:
                scf
                ret

; dir_stamp_date — a directory entry's +22..25 := time 0, date 0821h. 🔴 Joost
; 2026-10-01, "Stamp as 3300": 0821h is 1984-01-01, MSX-DOS's default date -- the
; CF-3300 has no clock either. It stamps a file when it is CREATED (SAVE, SAVE ,A,
; OPEN FOR OUTPUT, DOS FMAKE: probes/disk/disk_probe_savedate.py, --wseek's FCB
; dump) and when it is WRITTEN (BDOSX/BDOSX3/BDOSX6 read 0821h after WRBLK/WRRND
; on a file dated 0). It was $0000, "no fabricated clock" -- that premise fell.
; D-DOSDATE: in disk.rom the date is the DOS date SDATE keeps (DATE_DAYS);
; sub.rom (disk BASIC's copy) keeps 0821h until it is measured whether a date
; set in DOS reaches BASIC's SAVE on the CF-3300.
;   in: HL = the entry; out: HL kept; trashes A, BC, DE
dir_stamp_date:
                push    hl
                call    date_fat_word       ; DE = the DOS date (trashes AF, BC, HL)
                pop     hl
                push    hl
                ld      bc, 22
                add     hl, bc
                xor     a
                ld      (hl), a
                inc     hl
                ld      (hl), a             ; +22..23 time = 0
                inc     hl
                ld      (hl), e
                inc     hl
                ld      (hl), d             ; +24..25 date
                pop     hl
                ret

; fat_dir_update — rewrite the open-for-write file's directory entry at Close with
; its true byte count (DIRENT_FILESIZE) and first cluster (DIRENT_FIRSTCLUS), and
; date it as the CF-3300 dates a written file (dir_stamp_date).
;   out: Cy = 0 ok, Cy = 1 = I/O error
; (Microsoft FAT spec §3.4.)
fat_dir_update:
                ld      de, (FWR_DIRSEC)
                call    rs_mbuf
                ret     c
                ; HL = &entry = FAT_MBUF + DIROFF
                ld      hl, (FWR_DIROFF)
                ld      de, FAT_MBUF
                add     hl, de
                call    dir_stamp_date      ; a written file is dated as the CF-3300's
                ; +26 first cluster (LE) = FWR_FIRST
                ld      de, DIRENT_FIRSTCLUS
                add     hl, de
                ld      de, (FWR_FIRST)
                ld      (hl), e
                inc     hl
                ld      (hl), d
                ; +28 file size (4-byte LE) = FWR_BYTES -- D-DISKREREAD (2026-11-26): HL is at
                ; +27, so one `inc hl` instead of push / pop and `add 28`, 5 B
                inc     hl
                ex      de, hl              ; DE = &entry+28
                ld      hl, FWR_BYTES
                call    copy4               ; D-DISKREREAD (2026-11-26): copy4, 2 B
                jr      fdir_wrback         ; write the dir sector back (carve step 4)
