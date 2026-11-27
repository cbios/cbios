; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; FAT12 + BDOS/FCB file layer (read + write-back substrate)
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- BDOS WRITE calls (disk/PROVENANCE.md §BDOS interface, §FAT12 write-back) -

; bdos_create ($16) — create (or truncate-if-exists) the file named in the FCB,
; ready for sequential writing from offset 0.
;   in:  DE = FCB pointer (FCB+1 = 11-byte 8.3 name)
;   out: A = $00 created / $FF error (disk full / write protect / I/O)
; Mounts the volume, then finds or makes a root-directory slot for the name and
; writes a fresh dir entry (name, attribute $00, first cluster = 0, size = 0,
; date 0821h, time 0 -- as the CF-3300, Joost 2026-10-01). No data
; cluster is allocated yet: the first cluster is allocated lazily on the first
; Sequential Write, so a zero-byte file occupies no clusters (matching MSX-DOS).
; (bdos_create -- a `jp bdos_create_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
; dsl_bcheck: drv_select's B: arm (D-BOOTKEYS CTRL, 2026-10-27; disk/kernel.asm):
; A := 1 when there are two logical drives, else `Bad drive name` -- one drive
; after a CTRL boot, as on the CF-3300 (scratchpad/bootctrl_run.out). Sited in
; this fill for room.
dsl_bcheck:
                ld      a, (DRVCNT)
                cp      2
                ld      a, 1
                ret     nc
                ld      a, 62               ; Bad drive name
                ld      ix, raise_error
                jp      calbak              ; raises; never returns
; dst_body -- din_str's unquoted field (disk/kernel.asm din_str). A comma ends it
; (taken), a CR ends it with its LF (din_num's dnm_delim), an LF is dropped.
dst_body:
                call    din_peek
                ret     c
                cp      ','
                jp      z, dnm_eat          ; take it: the field is done
                cp      $0D
                jp      z, dnm_delim        ; the CR, and an LF after it
                call    din_take
                cp      $0A
                call    nz, dst_put
                jr      dst_body
; dst_put -- A = a data byte -> STRSCR (a full one is read and dropped).
dst_put:
                ld      c, a
                ld      hl, STRSCR
                ld      a, (hl)
                cp      STRMAX
                ret     nc
                inc     (hl)
                ld      e, (hl)
                ld      d, 0
                add     hl, de
                ld      (hl), c
                ret
; hk_openfind -- the three file hooks' common start (hk_finp, hk_fapp, hk_frnd):
; interrupts on (D-HOOKDI), dout_open (IX = the row, mounted), then fat_find of
; DISK_FCB_NAME -- CF = not found, FAT_FIRSTCLUS / FAT_FILESIZE / the entry set. A
; failed dout_open leaves by hfm_fail on the HOOK's stack (our return dropped), as
; the inline copies did. D-DSKFMT carve 13, 2026-11-10 (-14 B).
hk_openfind:
                ei
                call    dout_open
                jr      c, hof_fail
                jp      ff_name
hof_fail:
                pop     hl                  ; our return address
                jp      hfm_fail
; (moved here from driver.asm by the D-DSKFMT defrag)
wrseq_body:                                 ; $477D: BDOS $15 SEQWRITE
                call    seq_load            ; D-SEQMIX2: this file's own state (driver.asm)
                call    dta_from_dos
                ; Item 5 (spec-diskbasic-option-closure.md): the write-side twin of
                ; the M33 read-branch FCB-position write-back. M33 advanced the
                ; caller-visible FCB position (+32 CR / +12 EX / +28,29 cluster /
                ; +30 rec-in-cluster) after each DELIVERED read record but wired it
                ; to the read branch only; stock MSX-DOS-1 advances the FCB on
                ; sequential WRITES too. Our file bytes were already correct (the
                ; write-iterator tracks position internally); only the caller-visible
                ; FCB position went stale, observable via a WRSEQ-then-$24-SETRND
                ; sequence (SETRND reads +12/+32). Mirror the read branch: advance
                ; only on a WRITTEN record (A=$00; on A=$01 disk-full / A=$FF
                ; not-open, do NOT advance -- no record was stored). The twin is
                ; fully register-transparent (preserves AF/BC/DE/HL), so the write
                ; branch still returns bdos_seqwrite's exact registers -- BDOSX3/
                ; BDOSX4 register snaps are unchanged; only the FCB position moves.
                ; (wrseq_body_write, which held this, folded in here: D-BDOSFCB B)
                push    de
                call    bdos_seqwrite_body
                pop     ix
                or      a
                call    z, wrseq_wr_writeback   ; advance FCB position on a written record
                ret
                ds      $46BA - $, $00      ; pure $00 pad (M26, tier2-m26-spec.md
                                            ; §2.4: trace --resync confirmed $46BA is
                                            ; the kernel's real fixed $2F RDABS dispatch
                                            ; address, landing in dead pad -- NOT 61
                                            ; bytes into bdos_create's body as first
                                            ; guessed; no relocation needed)
k_46BA:         jp      rdabs_body          ; $46BA: BDOS $2F RDABS canonical entry (M26)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; dsm_bump — BDOS_SRCHIDX += 1 (advance the persistent scan cursor). Preserves all
; registers (HL is the live entry pointer at both call sites).
dsm_bump:
                push    hl
                ld      hl, (BDOS_SRCHIDX)
                inc     hl
                ld      (BDOS_SRCHIDX), hl
                pop     hl
                ret
                ds      $46C8 - $, $00      ; anchor the canonical address
                jp      k_46C8              ; $46C8: COMMAND.COM-load kernel veneer
; 📍 D-RECSLOW: from disk/kernel.asm's $75A5 region (no room there).
; frl_alloc -- GET (GP_FLAGS 0): CF = 1, past the end. PUT: a new cluster in HL
; (marked end-of-chain), CF = 1 disk full (66 pending) / I/O.
frl_alloc:
                ld      a, (GP_FLAGS)
                or      a
                scf
                ret     z
                jp      fat_alloc_cluster
; frnd_next -- HL = the cluster after HL. The FAT sector goes into FAT_MBUF, never
; FAT_DBUF, which held the record until D-FLDDESC (it is the block's +9 now; the
; choice stands): fat_read_fat_sector + fac_entry_from_wbuf,
; the two halves this ROM's allocator already uses (the sub-ROM carried a private
; 75 B copy of them). CF = 1 the read failed.
frnd_next:
                call    fat_read_fat_sector
                ret     c
                call    fac_entry_from_wbuf ; DE = the 12-bit entry
                ex      de, hl
                or      a
                ret

; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
wrblk_full:
                ld      a, 1                ; disk-full path ([C]-only, not black-box-provoked)
                ld      hl, (WRBLK_REQ)
                jp      wrblk_finish        ; (jp: out of jr reach since the move)
                ds      $46E6 - $, $00      ; pad to bdos_create_failpop (net-zero)
bdos_create_failpop:
                pop     hl
bdos_create_err:
                xor     a
                ld      (BDOS_WRMODE), a
                ld      a, $FF
                ret

; bdos_seqwrite ($15) — write the next 128-byte record FROM the DTA into the file.
;   out: A = $00 ok / $01 disk full / $FF error (MSX2 TH, BDOS conventions)
; Buffers RECSIZE (128) bytes from BDOS_DTA into SECTOR_BUF at offset BDOS_WRBUFLEN;
; whenever the buffer fills (512 bytes) it is flushed to the file's current data
; sector (allocating/extending the cluster chain as needed) and the buffer resets.
; BDOS_WRBYTES accumulates the true byte count for Close to stamp into the dir
; entry. (CP/M / MSX-DOS sequential write delivers a fixed 128-byte record from
; the DTA — MSX2 TH FCB sequential I/O; record framing as the read side.)
; (bdos_seqwrite -- a `jp bdos_seqwrite_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
                                            ; the old span here collided mid-instruction
                                            ; with the real $30 WRABS dispatch address,
                                            ; $4720 (trace --resync confirmed: it's the
                                            ; displacement byte of the `jr c, bsw_full`
                                            ; that used to live at this offset). Real
                                            ; body now bdos_seqwrite_body (disk/kernel.asm
                                            ; free tail); only symbolic callers
                                            ; (driver.asm/kernel.asm `jp bdos_seqwrite`)
                                            ; keep working unchanged.

; rnd_parse -- the record number's domain (D-RECBIG, 2026-11-04), measured on the
; CF-3300 for PUT and GET alike (TODO D-RECBIG): the value is TRUNCATED toward
; zero (1.99 -> 1, 65536.5 -> 65536); 1 .. 2^32-1 is that record (4294967296 is
; Overflow; 1E9 is a record, and Disk full); -32767..-1 WRAP to n + 65536 (-1 ->
; 65535, -32767 -> 32769); -32768 and below are Overflow; 0 is hk_dget's 5.
;   in   the evaluator's result: FAC / FACTYP, DE = an int's value
;   out  GP_RECNO (4 B) = the record 0 .. 2^32-1
;   raises 13 (a string) and a pending evaluator error through check_expr_errors,
;        6 out of the domain. Clobbers A, BC, DE, HL, IX and the alternate set.
; 📍 Sited in the pad in front of the $4720 pin: the $5602..$5E10 fill that holds
; hk_dget (disk/kernel.asm) is full. DE crosses calbak intact (D-XSLOTABI).
rnd_parse:
                ld      ix, check_expr_errors
                call    calbak              ; a string's 13, a deferred 1/0 ...
                ld      a, (FACTYP)
                cp      2
                jr      nz, rnp_flt
                ld      a, d                ; an int: -32767..-1 IS n + 65536
                xor     $80
                or      e
                jr      z, rnp_ovf          ; -32768 falls into Overflow
                ex      de, hl
                ld      de, 0
rnp_store:
                ld      (GP_RECNO), hl
                ld      (GP_RECNO+2), de
                ret
rnp_ovf:
                ld      a, 6
                jp      hdg_raise
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; fill_rec -- the record (rnd_rec, FCH_RECMAX bytes) := spaces, CF = 0 (fill_sp).
; IX = the row. Clobbers BC, DE, HL. GET's clean end (hk_frnd's OPEN fill went
; with D-FLDDESC: the reference never clears the record).
fill_rec:
                call    rnd_rec
                ld      bc, FCH_RECMAX-1
                jp      fill_sp
                ds      $4720 - $, $00      ; pad up to the pinned canonical entry
k_4720:         jp      wrabs_body          ; $4720: BDOS $30 WRABS canonical entry (M26)
; rnd_parse's float half, past the pin: the pad in front of $4720 holds the rest.
rnp_flt:
                call    fac_mag32           ; DE:HL = |x| truncated, CF = past 2^32-1
                jr      c, rnp_ovf
                ld      a, (FAC)
                rla
                jr      nc, rnp_store       ; positive: as is
                ld      a, d
                or      e
                jr      nz, rnp_ovf
                bit     7, h
                jr      nz, rnp_ovf         ; -32768 and below
                xor     a
                sub     l
                ld      l, a
                sbc     a, a
                sub     h
                ld      h, a                ; HL = 65536 - |x| (DE = 0)
                jr      rnp_store

; --- FAT12 layer (disk/PROVENANCE.md §FAT12 layer) -------------------------
; Read-only FAT12 on top of the DSKIO sector reader. Sources: Microsoft FAT
; filesystem specification (BPB, FAT, directory) and ECMA-107 (geometry).
; Assumes 512-byte sectors (validated at mount); the on-disk BPB supplies every
; other geometry value, so the same code serves any FAT12 image the BPB
; describes. These routines are internal helpers; the BDOS/FCB layer (next TODO
; item) wires them to zerobas's BLOAD path. End-to-end validation waits on the
; openMSX machine config + oracle probe 2 (DSKIO boot-sector read).

; read_sector — read one logical sector into a buffer via the DSKIO core.
;   in:  DE = logical sector number, HL = buffer
;   out: Cy = 0 ok, Cy = 1 error (A = DSKIO error code)
; --- MSX-DOS-1 kernel WRSEQ-worker entry: $477D (M24 slice B;
; tier2-m24-fclose-multicluster-spec.md) --------------------------------------
; The RAM kernel's BDOS $15 WRSEQ handling CALLs this page-1 entry once per
; 128-byte record (pinned black-box, ret=$D88A dispatcher class, DE=IY=$DA40
; kernel FCB pointer). Pre-M24 this fell on the high byte of fat_mount's own
; `ld hl,0` (the numFATs*secPerFAT multiply preamble), NOP'd through, then
; spun the multiply loop 256x on garbage B and clobbered our FAT geometry
; cells from stale SECTOR_BUF bytes -- silently discarding every WRSEQ
; record's data while reporting success. fat_mount's own multiply+remainder
; (fm_fatacc onward, unchanged) is relocated below to make room -- position-
; free, reached only by label from fat_mount's own `jp fat_mount_tail`
; fall-through above.
; dsm_rc -- D-SRCHFCB: the found entry's +15 = its record count, ceil(size / 128)
; (the CF-3300: 512 bytes -> 4, 300 -> 3; scratchpad/srchfcb_ref.out), capped at
; 128 -- one extent's worth (own reading of the CP/M field, unmeasured).
;   in: HL = the 33-byte FCB form (size at +29..+32)   keeps HL, DE
; Sited in front of the $477D pin: the $53A7 hole could not take it (EMPTY disk.rom).
dsm_rc:
                push    de
                push    hl
                ld      bc, 29
                add     hl, bc
                ld      e, (hl)
                inc     hl
                ld      d, (hl)             ; DE = size, low word
                inc     hl
                ld      a, (hl)
                inc     hl
                or      (hl)                ; size >= 64K?
                jr      nz, drc_max
                ld      hl, 127
                add     hl, de              ; HL = size + 127
                jr      c, drc_max
                ld      a, h
                cp      $40
                jr      nc, drc_max         ; 16K or more: one extent's 128
                ld      a, l
                rla                         ; CF = bit 7 of L
                ld      a, h
                rla                         ; A = (size + 127) >> 7
                jr      drc_set
drc_max:
                ld      a, 128
drc_set:
                pop     hl
                push    hl
                ld      bc, 15
                add     hl, bc
                ld      (hl), a
                pop     hl
                pop     de
                ret
                ; 🔴 D-LOADFAT (2026-11-06): $4775 IS AN ENTRY, AND THE ZERO PAD WAS ITS CODE.
                ; MSX-DOS calls the disk ROM at $4775 (return address $D88A, its RAM kernel;
                ; scratchpad/hole477_probe.py -> hole477_probe.out, our ROM's PC logged for
                ; every instruction run in $476A..$477D) and the $00 pad ran as NOPs into
                ; k_477D below. Filling the pad with install_hook broke p1dta-acceptance
                ; (BDOS SEQRD = $C9, the record unread). Pinned now, with the slide's own
                ; effect; the bytes below $4775 and between the pins stay EMPTY -- another
                ; unmeasured entry may land in them.
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; pg0_mainrom_out - restore page 0 from PG_SV_A8 (the RAM slot's own secondary is never
; touched, so its page-0 subslot is intact). Clobbers A. (Moved here from
; disk/runtime.asm by D-WRRNDEXT, whose wrapper needed that region's bytes.)
pg0_mainrom_out:
                ld      a, (PG_SV_A8)
                out     ($A8), a
                ret
                ds      $4775 - $, $00
k_4775:         jp      wrseq_rd            ; $4775: BDOS $14 SEQREAD (measured; D-SEQTWO)
                ds      $477D - $, $00      ; pad up to the pinned $477D WRSEQ-worker entry
k_477D:
                jp      wrseq_body          ; $477D: BDOS $15 WRSEQ canonical entry (M24 slice B)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; wrblk_set_wrfirst — BDOS_WRFIRST := FAT_FIRSTCLUS (may have just been updated by
; wrblk_read_or_extend_sector's very-first-cluster case, or zeroed by wrblk_shrink's
; empty-file case) -- the field fat_dir_update stamps into the dirent's first-
; cluster word.
wrblk_set_wrfirst:
                ld      hl, (FAT_FIRSTCLUS)
                ld      (BDOS_WRFIRST), hl
                ret
                ds      $4788 - $, $00      ; pad remainder (M26, tier2-m26-spec.md §2.3:
                                            ; trace --resync confirmed $4788 is the
                                            ; kernel's real fixed $21 RDRND dispatch
                                            ; address, dead pad -- the original
                                            ; "$21/$22 share one dispatch" callwatch
                                            ; read was a dedup artifact; no relocation
                                            ; needed, RDABS-shape)
k_4788:         jp      rdrnd_body          ; $4788: BDOS $21 RDRND canonical entry (M26)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; rnd_rec -- D-FLDDESC (2026-10-17): HL = the channel's RECORD, its block's +9
; (FCH_HDRSZ) -- where the CF-3300 keeps it and where FIELD points a variable's
; descriptor. IX = the row (its cached DOUT_BLK). Clobbers nothing else.
; Sited here, in the bytes hk_lrset's field-entry walk left, because the record
; engine's own fills (before the $607B and $77B8 veneers) have no room.
rnd_rec:
                call    dout_pos            ; HL = block+6
                inc     hl
                inc     hl
                inc     hl                  ; HL = block+9
                ret
                ds      $4793 - $, $00      ; pad remainder (M26, tier2-m26-spec.md §2.3:
                                            ; $4793 is $22 WRRND's own separate dispatch,
                                            ; same pad corridor, RDABS-shape)
k_4793:         jp      wrrnd_body          ; $4793: BDOS $22 WRRND canonical entry (M26)
; 🐌 D-RECSLOW (2026-11-05): fat_alloc_cluster's two dirty-buffer pieces
; (basic/fat-prim-body.inc), here because the body's region has no room. FAT_DEFER
; bit 1 = FAT_MBUF holds a patched sector not yet on disk (frnd_locate's extension).
fac_cacheinit:                              ; HL = the scan's cached sector at entry
                ld      hl, $FFFF
                ld      a, (FAT_DEFER)
                and     2
                ret     z                   ; clean: none cached, read on first use
                ld      hl, (FAT_FATSEC)    ; dirty: it IS the newest copy
                ret
fac_flushdirty:                             ; a dirty buffer to both FATs (CF = I/O)
                ld      a, (FAT_DEFER)
                and     2
                ret     z
                jr      fat_write_buf_allfats
; skip_sp -- HL past the blanks at it; A = the first other character. 💰 D-DSKFMT carve 34
; (2026-11-10): NAME's `AS`, COPY's `TO`, FILES. MOVED here by D-DISKCARVE6 step 3 (2026-11-17).
skip_sp:
                ld      a,(hl)
                cp      ' '
                ret     nz
                inc     hl
                jr      skip_sp
                ds      $47B2 - $, $00      ; pad remainder (net-zero: k_47B2 unaffected)
                jp      k_47B2              ; $47B2: COMMAND.COM-load kernel veneer
; bb_dirio -- $F37D's DIRIO ($06): E = $FF reads a key (or 0), else it writes E as CONOUT
; does. (Moved here from in front of $543C by D-F37DCONIN, with a `jp` to bb_conout: the
; two were one fall-through. This pad held $47B9's old `ds`; $47BE's below anchors it.)
bb_dirio:
                ld      a, e
                inc     a
                jp      z, dirio_in_body    ; E = $FF: a key, or 0
                jp      bb_conout
; --- M28: $26 WRBLK canonical entry $47BE -- 3b relocation (tier2-m28-blockrandom-
; spec.md §3.1/§4). $47BE used to be 5 bytes INSIDE ff_secloop (the old `jr z,
; ff_notfound` at old-$47BE, [LANDED-B]) -- a latent accidental target, harmless only
; because WRBLK was never exercised during boot. ff_secloop..ff_found (old $47B9-
; $4828, verbatim, position-free -- reached only by fat_find_body's `jp ff_secloop`
; and its own internal back-edge; every internal ff_* label is local to the block)
; is relocated OUT to kernel.asm's free tail (same technique as fdc_entloop_body /
; the FDC-window fix): we do NOT repoint the kernel's $D8BE RAM dispatch table (the
; fixed shared-kernel ABI, not ours) -- we free the canonical address by moving OUR
; colliding code, then expose the real `jp wrblk_body` veneer at $47BE.
                ds      $47BE - $, $00      ; pad the vacated 5 bytes ($47B9-$47BD)
k_47BE:         jp      wrblk_body          ; $47BE: BDOS $26 WRBLK canonical entry (M28)

; fat_write_fat_entry_hot -- disk.rom's copy of the shared body's routine
; (basic/fat-prim-body.inc, D-WCACHE W2, which says when a caller may use it). It
; lives in this fill ($47C1.., free up to $4C29 -- the old "$4919 anchor" was a dead assert,
; deleted 2026-10-15) because the body's region, pinned at $75A5, had no room.
fat_write_fat_entry_hot:
                ld      (FAT_WRTMP), de     ; save the value
                push    hl                  ; save cluster
                call    fat_locate          ; 💰 D-DISKCARVE: HL = the entry's FAT sector, PARITY / BYTEIDX set
                push    hl                  ; the entry's sector
                ld      de, (FAT_FATSEC)    ; the sector FAT_MBUF holds now
                or      a
                jr      fwe_cmp             ; over WRZER's entry (Cy = 0 kept: jr leaves flags)
; 🔴 D-WRZER (2026-11-08): $47D1 IS BDOS $28 WRZER's CANONICAL ENTRY. MSX-DOS's
; kernel CALLs it (disk_probe_diff.py callwatch --in-func 0x28, our ROM's PCs:
; scratchpad/wrzer_callwatch.out), and it fell 16 bytes into this routine -- the
; FAT-entry write ran with WRZER's registers and could damage the user's FAT (the
; M26 / M28 collision class: $436C FDEL, $4392 FREN, $47BE WRBLK). A veneer now
; sits at the pinned address and this routine steps over it.
                ds      $47D1 - $, $00      ; (zero-size when exact; an overrun empties the ROM)
k_47D1:         jp      wrzer_body          ; $47D1: BDOS $28 WRZER -- zero-fills the
                                            ; records an extension adds (disk/kernel.asm)
fwe_cmp:
                sbc     hl, de
                pop     hl
                jp      z, fwe_have         ; already loaded: no read
                push    hl
                call    fac_flushdirty      ; D-WCACHE W3a: a deferred write of the
                                            ; sector we are about to replace goes
                                            ; out FIRST (it clears the flag;
                                            ; bit 1 = dirty since D-RECSLOW)
                                            ; D-DISKREREAD (2026-11-26): the same test
                                            ; and call, fac_flushdirty's -- 5 B
                pop     hl
                call    rd_fatsec           ; FAT_FATSEC := HL, read it into FAT_MBUF (Cy)
                jp      fwe_have

; --- disk.rom's copies of three shared-body routines (basic/fat-prim-body.inc),
; moved here by D-WCACHE W3a: the body's region, pinned at $75A5, had no room for
; the deferral logic otherwise. Same code, same contracts.
fat_write_buf_allfats:
                xor     a
                ld      (FAT_DEFER), a      ; D-WCACHE W3a: the buffer is clean after this
                ld      a, (FAT_NUMFATS)
                ld      b, a                ; B = copies to write
                ld      hl, (FAT_FATSEC)    ; copy-0 target sector
fwba_loop:
                push    bc
                push    hl
                ex      de, hl              ; DE = target sector
                call    fws_mbuf
                pop     hl
                pop     bc
                ret     c                   ; write error
                ; advance to the same sector in the next FAT copy.
                ld      de, (FAT_SECPERFAT)
                add     hl, de
                djnz    fwba_loop
                or      a                   ; Cy = 0 success
                ret

; fat_total_clusters — total cluster count of the volume (2 + data clusters).
;   out: DE = total clusters; preserves HL
; (Microsoft FAT spec §3.3.) Re-reads the boot sector into FAT_MBUF (NOT FAT_DBUF,
; which may hold in-flight write data).

; fat_dir_start -- FAT_DIRSEC := the first root sector, FAT_DIRREM := how many
; there are: the start of every root-directory scan. Clobbers DE, F.
fat_dir_start:
                ld      hl, (FAT_FIRSTROOT)
                ld      (FAT_DIRSEC), hl
                call    fat_rootsecs
                ld      (FAT_DIRREM), hl
                ret

; fat_rootsecs -- HL := root-directory sectors = FAT_FIRSTDATA - FAT_FIRSTROOT
; (fat_mount's own sum, undone; the cell that held it holds FAT_TOTCLUS now).
; Clobbers DE, F.
fat_rootsecs:
                ld      hl, (FAT_FIRSTDATA)
                ld      de, (FAT_FIRSTROOT)
                or      a
                sbc     hl, de
                ret

; name_cmp — compare two 11-byte 8.3 name fields, case-insensitive.
;   in:  HL = directory entry name, DE = search name
;   out: Z set if equal; trashes A, BC, DE, HL

; --- D-FDCDI: Type II transfers wait for their FIRST DRQ with interrupt windows ---
; (WD2793 DS: Type II commands -- DRQ per byte, the Lost Data status bit when a
; byte is not taken in time; disk/PROVENANCE.md §FDC. The windowing is our own.)
; The rotational wait before a sector's first byte is most of a disk command's
; time, and the driver used to spend it masked: keys typed during long disk work
; were lost (scratchpad/fdcdi_iff.out: masked 62 % of a PRINT# + CLOSE, nearly
; all inside these waits). Now, when the CALLER had interrupts on (FDC_IFF != 0)
; and windowed tries remain, the wait opens a one-instruction interrupt window
; per poll. An interrupt that lands as the first DRQ arrives can cost a byte --
; LOST DATA -- and that attempt is simply re-issued (fdc_lostwin), still windowed,
; up to FDC_WINMAX times; the attempt after them waits masked, as every one did.
; A masked caller (the MSX-DOS boot) never opens a window, and NEITHER DOES
; MSX-DOS itself (page 0 is RAM there, not the BIOS): its COMMAND.COM runs with
; interrupts on, and with windows open wrblkalt-acceptance's 32 KB WSEEK write --
; already the slowest phase here -- outran its window and BIGW.BIN came back
; 29184 B (scratchpad/fdcdi_full.out). The subject is BASIC's typing.
; 2026-10-06's try at this, reverted, retried a lost first byte MASKED for a
; whole rotation.

; fdc_fdrq -- after a Type II command: wait for its first DRQ (or its end), with
; windows when allowed. Returns masked; A = status. Keeps BC, DE, HL.
fdc_fdrq:
                ld      a, (FDC_IFF)
                or      a
                ret     z               ; a masked caller: the loop waits masked
                cp      1 + 2 * FDC_WINMAX
                ret     nc              ; windows spent: the last try waits masked
                in      a, ($A8)        ; primary slot register: page 0 in bits 1-0
                and     3
                ret     nz              ; page 0 not the BIOS -- MSX-DOS: masked (below)
fdq_win:
                ei
                nop                     ; a pending interrupt is taken here
                di
                ld      a, (FDC_STATUS)
                bit     1, a
                ret     nz              ; DRQ: the transfer runs masked
                bit     0, a
                jr      nz, fdq_win
                ret                     ; ended without a DRQ: the status decides

; fdc_lostwin -- B = a failed transfer's error bits. Z (and one windowed try
; spent, HL = the buffer again) iff it was LOST DATA under a window; NZ otherwise.
fdc_lostwin:
                ld      a, b
                and     ST_LOST
                jr      z, flw_no
                ld      a, (FDC_IFF)
                or      a
                jr      z, flw_no
                cp      1 + 2 * FDC_WINMAX
                jr      nc, flw_no
                add     a, 2
                ld      (FDC_IFF), a
                ld      hl, (FDC_DEST)
                xor     a               ; Z: re-issue
                ret
flw_no:
                or      1               ; NZ: decode the error as before
                ret

; fdc_write_data_body -- fdc_write_data's body (disk/driver.asm), moved here
fdc_write_data_body:
                call    fdc_fdrq        ; D-FDCDI: the rotational wait with interrupt windows
                ld      e, 2            ; two 256-byte halves
fwd_blk:
                ld      b, 0            ; djnz 0 -> 256 iterations
fwd_wait:
                ld      a, (FDC_STATUS)
                bit     1, a            ; DRQ?
                jr      nz, fwd_byte
                bit     0, a            ; BUSY?
                jr      nz, fwd_wait
                jr      fwd_status   ; finished with no DRQ -> short/error
fwd_byte:
                ld      a, (hl)
                ld      (FDC_DATA), a
                inc     hl
                djnz    fwd_wait
                dec     e
                jr      nz, fwd_blk
fwd_drain:
                ld      a, (FDC_STATUS) ; all 512 written; wait for command end
                bit     0, a
                jr      nz, fwd_drain
fwd_status:
                ld      a, (FDC_STATUS)
                ; write-protect / write-fault (bit 6, $40) -> DSKIO code 0 first,
                ; so a protected disk reports write-protected rather than retrying.
                and     ST_WP
                jr      z, fwd_chk
                xor     a               ; A = 0 = write protected
                scf
                ret
fwd_chk:
                ld      a, (FDC_STATUS)
                and     ST_NOTRDY + ST_RNF + ST_CRC + ST_LOST
                jr      z, fwd_ok
                ld      b, a            ; keep the error bits
                call    fdc_lostwin     ; D-FDCDI: lost data under a window? try again
                jr      nz, fwd_nowin
                ld      a, CMD_WRITE
                ld      (FDC_STATUS), a
                jr      fdc_write_data_body
; D-DISKREREAD (2026-11-26): `bit` tests and one shared `scf / ret` -- 9 B. Same
; priority, same codes, B kept; `ld a,n` leaves the `bit` flags, which for these
; one-bit masks match the old `and`'s S/Z/H/PV/N/C on every arm.
fwd_nowin:
                ld      a, 2            ; not ready
                bit     7, b            ; ST_NOTRDY
                jr      nz, fwn_e
                ld      a, 8            ; record not found
                bit     4, b            ; ST_RNF
                jr      nz, fwn_e
                ld      a, 4            ; CRC / data error
                bit     3, b            ; ST_CRC
                jr      nz, fwn_e
                ld      a, 12           ; lost data / other
fwn_e:
                scf
                ret
fwd_ok:
                or      a               ; A = 0, Cy = 0
                ret

; --- S10.D: the RANDOM record reader (disk/kernel.asm hk_dget), in this fill
; below $4C29 (no pin in between since 2026-10-15). IX = the channel's FCB row.

; fat_rand_get -- read record GP_RECNO into the record (block+9 [0..reclen):
; D-FLDDESC, 2026-10-17 -- it was FAT_DBUF[0..reclen) while the record travelled).
; 🔴 D-GETEOF: a record that STARTS at or past EOF is `Input past end` (55),
; raised; one that starts inside and ends past is accepted (D-GETSTRADDLE).
; The bound is the whole 24-bit offset against the 32-bit size: main's resident
; test skipped files over 64 KB ("recno <= 255", false since D-PUTU32).
; 🎯 D-STRADDLE: two passes, as fat_rand_put.
;   in:  GP_RECNO, GP_FLAGS = 0 (hk_dget), IX = the row
;   out: CF = 0 read (a second sector past the chain: the record is spaces);
;        CF = 1 a read failed (D-GETEJECT: the code pending, hk_dget raises it)
fat_rand_get:
                call    frnd_calc           ; A:HL = the offset; the pass cursor
                jr      c, frg_past         ; past any disk
                call    rnd_cmp
                jr      c, frg_pass         ; starts before EOF
frg_past:
                ld      a, 55
                jp      hdg_raise           ; starts at or past EOF
frg_pass:
                call    frnd_locate
                jp      c, frg_eofchk       ; past the chain -- or a FAILED READ
                call    rnd_rd
                ret     c
                call    frnd_pass_len       ; BC = this sector's share
                ld      hl, (GP_WITHIN)
                ld      de, FAT_MBUF
                add     hl, de              ; HL = FAT_MBUF + within
                ld      de, (GP_SRC)        ; the record, advanced per pass
                ldir
                ld      (GP_SRC), de
                call    rnd_step
                jr      nz, frg_pass
                ret                         ; CF = 0

; ===== S10.C (2026-10-15): an INPUT channel is the same FCB row, read HERE =====
; D-ROMSIZE / spec-diskcode-eviction.md S10.C. Until now OPEN FOR INPUT was
; main's resident fat_io_open (basic/fatio-body.inc) over main's engine globals --
; FREAD_OFF / FREAD_LEFT and the staged sector in FSECTOR_BUF, copied per channel
; by the sub-ROM's t_fch_save / t_fch_load and re-staged after every crossing --
; and the bytes came from the sub-ROM's seqio tenant (D-SEQEOF), one subrom_call
; per byte. S10.B / S10.D's lesson again: the reader can only move if the state is
; disk-side FROM OPEN. So an INPUT channel's row is the S10.B FCB too, its record
; (the block's +9) is its read-ahead, and the CF-3300's measured cells set the
; split (scratchpad/s10c_hook_run.out): OPEN FOR INPUT reaches $FE5D and a
; missing file is 53 AFTER it; every byte read enters $FE8A once.
;   row: +1..11 the name (hkk_open_check), +14 RS = 1, +16..19 the size (LOF,
;        the end), +26 the first cluster, +28..31 RDBLK's running cluster and its
;        index, +33..35 the random record = the file offset just past the bytes
;        loaded into the record, DIN_LEFT the record's bytes not yet read.
;   block+6 = the position in the record (the reference's header +6), +9.. the
;        record.
; The refill is RDBLK's own positioning and byte source (k47b2_seek /
; k47b2_nextbyte, D-RDBLKSEEK): no re-mount, no re-find, the chain walked from
; the FCB's running cluster -- 256 B a refill, the CF-3300's block size.
DIN_CRPEND      equ     2               ; DOUT_FLG bit (an INPUT row): INPUT$ handed over a CR
                                        ; and its LF is still unread (D-INPLF (A), din_num)
DIN_LEFT        equ     DOUT_FLG+1      ; word: the record's unread bytes (0..256).
                                        ; DOUT_FIRST's place -- an INPUT row has no
                                        ; write cursor
    IF DIN_LEFT+2 > FCH_STATESZ
                db      DIN_ROW_FIELDS_OVERRUN_FCH_STATESZ
    ENDIF

; hk_finp -- OPEN FOR INPUT (H_FOPEN, FOPEN_SEL_INP). FCH_ACTIVE is the channel
; being opened (OPEN stores it), DISK_FCB_NAME the parsed 8.3 name. The FCB is
; dout_open's (name, RS = 1, everything else 0: random record 0, nothing loaded,
; position 0) plus the file's first cluster (+26, where k47b2_seek walks from)
; and its 32-bit size (+16). A missing file is refused with 53 pending -- hk_fapp's
; own tail -- which main raises after the crossing, as the CF-3300 raises it after
; $FE5D. Status 0 open (hfm_ok); 1 with the code pending. LOF / LOC read the
; row (H_CHIN ops 6 / 8, din_lofloc -- S10.E; hfm_ok mirrored them until then).
hk_finp:
                call    hk_openfind         ; IX = the row; FAT_FIRSTCLUS / FAT_FILESIZE
                jp      c, hfa_nf           ; 53, pending
                ld      hl, (FAT_FIRSTCLUS)
                ld      (ix+26), l
                ld      (ix+27), h
                jp      hfr_size            ; the size into +16..19, status 0 (hk_frnd's
                                            ; tail, kernel.asm: D-DSKFMT carve 14)

; din_lofloc -- A = C = 6 LOF / 8 LOC (S10.E, 2026-10-19): the live channel's
; value straight from its FCB row, for a DISK channel in any mode -- INPUT,
; DOUT_MODE (OUTPUT / APPEND) and RANDOM all keep the S10.B FCB there.
;   out  DE:HL = the value, unsigned 32-bit (main's ev_ff_field turns it into
;        LOF's double, or its int 0): op 6 the size, +16..19 -- the bytes committed
;        in whole records, which dout_write / frnd_update_size move as they write
;        (S10.B DESIGN INPUT 2; D-LOF §4c: a RANDOM PUT moves LOF before the
;        directory knows); op 8 the random record, +33..35, high byte 0.
; Main sends op 8 for a DOUT_MODE or a RANDOM channel: an INPUT channel's LOC is
; its LOF (D-LOCSEM's 26,26,26 -- its +33 is the read-ahead's offset, not a LOC).
; A RANDOM row's +33..34 is its last record (disk/kernel.asm rnd_loc, D-RECBIG
; 2026-11-04: main read FCH_RECNOS[ch] as a SIGNED int until then).
; 🏗️ UNTIL S10.E THIS WAS A MIRROR: dout_mirror (disk/kernel.asm) copied +16 and
; +33 into main's FAT_FILESIZE / FWR_BYTES after every OPEN and record write, and
; the sub-ROM's t_fch_load copied them again on every channel switch, so main's
; channel-switch machinery (fch_save_active / fch_load_ctx, chan_gate's restore)
; existed to keep two cells current that only LOF and LOC read. The CF-3300
; reaches $FE9E (LOF) / $FE99 (LOC) here; ours ride H_CHIN with an op, as S10.C's
; EOF does (the kept one-cell divergence, basic/sysvars.inc H_CHIN).
; Neither op reads, refills or clears DISKOP_ERR. Clobbers A, BC, IX. Sited
; BEFORE hk_chin: din_num's `jr`s back to din_ok leave no room between them.
din_lofloc:
                call    dout_live           ; IX = the row (C survives)
                bit     1, c                ; 6 = %0110 LOF, 8 = %1000 LOC
                jr      z, dll_loc
                ld      l, (ix+16)
                ld      h, (ix+17)
                ld      e, (ix+18)
                ld      d, (ix+19)          ; DE:HL = the size
                ret
dll_loc:
                ld      l, (ix+33)
                ld      h, (ix+34)
                ld      e, (ix+35)
                ld      d, (ix+36)          ; DE:HL = the random record (32 bits,
                                            ; D-RECBIG; dout_open zeroes +36)
                ret

; hk_chin -- H_CHIN ($FE8A): the live INPUT channel's text, the reference's way.
;   in   A = 0 the next byte -- FCH_RDMODE 2 (INPUT$) raw, else INPUT# / LINE
;            INPUT#'s line rule: a CR swallows the LF after it (D-SEQEOF R2);
;        A = 2 EOF()'s test, nothing consumed;
;        A = 4 one numeric INPUT # item into STRSCR (D-CHANSWITCH);
;        A = 6 LOF() / 8 LOC() of a DISK channel in ANY mode (S10.E, din_lofloc
;            below: DE:HL = the value, nothing else read or moved);
;        the live channel (FCH_ACTIVE) is the one main selected.
;   out  A = the byte, CF clear; CF set = the end: nothing left, a Ctrl-Z next (the
;        soft end, never consumed -- R1), or a failed read, whose code is pending
;        in DISKOP_ERR (D-LINEJECT: main's seq_end raises it, else 55).
; The rules are the sub-ROM seqio tenant's, moved (sub/bload.asm; measured on the
; CF-3300: scratchpad/ipe_run*.out, inpnum_run2.out, inpnum_run3.out).
; ⚠️ A LIVE CHANNEL THAT IS NOT OPEN FOR INPUT ANSWERS "THE END". Main selects the
; channel once per statement, and an expression inside the statement can select
; another -- `INPUT #1,A$(LOF(2))` -- before the read (the brief's unmeasured
; latent case). Reading that row as an INPUT one would fill FSECTOR_BUF -- a
; RANDOM channel's live record -- with file bytes, so the guard refuses instead:
; 55, where the old reader served the other channel's bytes. Neither is the
; reference's answer, which is unmeasured.
hk_chin:
                ei                          ; D-HOOKDI: a refill is sector I/O
                ld      c, a                ; C = the operation
                cp      6
                jr      nc, din_lofloc      ; S10.E: 6 LOF / 8 LOC, any disk mode
                xor     a
                ld      (DISKOP_ERR), a     ; only THIS read's failure is pending
                ld      a, (FCH_ACTIVE)
                ld      e, a
                ld      d, 0
                ld      hl, FCH_MODES
                add     hl, de
                ld      a, (hl)
                dec     a                   ; open FOR INPUT?
                scf
                ret     nz                  ; no: the end (see the header)
                push    bc
                call    dout_live           ; IX = the row
                pop     bc
                ld      a, c
                cp      4
                jr      z, din_num
                cp      5
                jp      z, din_str          ; D-INPQUOTE2: a string field
                or      a
                jp      nz, din_peek        ; 2: EOF() is the peek's CF (`jp`: din_num sits between)
                ; 0: the next byte
                call    din_peek
                ret     c                   ; the end: nothing consumed
                call    din_take
                cp      $0D
                jr      nz, din_ok
                ld      a, (FCH_RDMODE)
                cp      2                   ; INPUT$ reads the CR-LF pair as data
                jr      nz, din_inp
                set     DIN_CRPEND, (ix+DOUT_FLG)   ; D-INPLF (A): its LF is still to come
                jr      din_cr
din_inp:
                call    din_peek            ; INPUT#: what follows the CR?
                jr      c, din_cr
                cp      $0A
                call    z, din_take         ; an LF: swallow it (R2)
din_cr:
                ld      a, $0D
din_ok:
                or      a                   ; CF clear: a byte
                ret

; din_num -- A = 4: ONE NUMERIC INPUT # item into STRSCR = [len][bytes], the
; length byte the counter (D-CHANSWITCH, the seqio tenant's sq_numitem, moved).
; Measured on the CF-3300 (inpnum_run2.out, inpnum_run3.out): leading blanks, CRs
; and LFs are skipped; the item ends at a blank, a comma or a CR; then the blanks
; after it are eaten, and ONE comma, or ONE CR with its LF, with them -- any
; other byte belongs to the NEXT item and stays: `" 1  2 "` read with INPUT #1,X
; leaves "2 " for a LINE INPUT, `" 1 \r\n"` leaves nothing (EOF is -1 after the
; last number), `"1   ,2"` leaves "2". Leaving a byte needs the PEEK below.
; out: CF set only when the end came before any byte (length 0 then).
din_num:
                xor     a
                ld      (STRSCR), a
                ; D-INPLF (A), measured on the CF-3300 (scratchpad/inplf_measure.out): after
                ; INPUT$ took a CR, the LF that follows ENDS AN EMPTY ITEM -- INPUT # reads 0
                ; there -- where the lead-in below would skip it. The CR is still pending.
                bit     DIN_CRPEND, (ix+DOUT_FLG)
                jr      z, dnm_lead
                call    din_peek
                jr      c, dnm_lead
                cp      $0A
                jr      nz, dnm_lead
                call    din_take            ; the LF (and the pending CR with it)
                or      a                   ; an empty item, CF clear: 0
                ret
dnm_lead:
                call    din_peek
                ret     c                   ; the end before any byte
                cp      ' '
                jr      z, dnm_skip
                cp      $0D
                jr      z, dnm_skip
                cp      $0A
                jr      nz, dnm_body
dnm_skip:
                call    din_take
                jr      dnm_lead
dnm_body:
                call    din_peek
                jr      c, din_ok           ; the end (or Ctrl-Z) closes the item
                cp      ' '
                jr      z, dnm_tail
                cp      ','
                jr      z, dnm_tail
                cp      $0D
                jr      z, dnm_tail
                call    din_take            ; A = the byte
                cp      $0A                 ; 🔴 D-INPLF (2026-11-06): an LF INSIDE a
                call    nz, dst_put         ; number is skipped -- `5<LF>6` is 56,
                                            ; `-<LF>5` -5, `5.<LF>5` 5.5 on the CF-3300
                                            ; (scratchpad/inplf_grid2.out 3 / 7 / 8);
                                            ; it was stored and ended the number
                                            ; D-DISKREREAD (2026-11-26): dst_put, not
                                            ; an inline copy of it (a full STRSCR is
                                            ; read and dropped there too) -- 14 B
                jr      dnm_body
dnm_tail:
                call    din_peek
                jr      c, din_ok
                cp      ' '
                jr      nz, dnm_delim
                call    din_take            ; a blank after the number
                jr      dnm_tail
dnm_delim:
                cp      ','
                jr      z, dnm_eat
                cp      $0D
                jr      nz, din_ok          ; the NEXT item's byte: leave it
                call    din_take            ; the CR ...
                call    din_peek
                jr      c, din_ok
                cp      $0A
                jr      nz, din_ok
dnm_eat:
                call    din_take            ; ... and its LF, or the comma
                jr      din_ok

; din_peek -- A = the row's next byte, NOT consumed, CF clear; CF set when there
; is none (the end, a failed read) or it is a Ctrl-Z. A spent record is refilled
; first, so a peek at a record boundary reads the next 256 B. IX = the row (kept).
; Clobbers BC, DE, HL.
din_peek:
                ld      a, (ix+DIN_LEFT)
                or      (ix+DIN_LEFT+1)
                call    z, din_fill         ; spent: the next block (CF = none)
                ret     c
                call    dout_pos            ; HL = &block+6, the position
                ld      e, (hl)
                ld      d, 0
                inc     hl
                inc     hl
                inc     hl                  ; HL = the record
                add     hl, de
                ld      a, (hl)
                cp      $1A
                scf
                ret     z                   ; Ctrl-Z: the soft end (R1)
                or      a
                ret

; din_take -- consume the byte din_peek saw: position + 1, DIN_LEFT - 1. A and
; the flags are kept; BC, DE, HL clobbered. IX = the row.
din_take:
                res     DIN_CRPEND, (ix+DOUT_FLG)   ; any byte taken: no CR pending
                push    af
                call    dout_pos
                inc     (hl)                ; the position (wraps with the record)
                ld      l, (ix+DIN_LEFT)
                ld      h, (ix+DIN_LEFT+1)
                dec     hl
                ld      (ix+DIN_LEFT), l
                ld      (ix+DIN_LEFT+1), h
                pop     af
                ret

; din_fill -- the record := the file's next (up to) 256 bytes, read at the FCB's
; random record (RS = 1, so it is the byte offset) through k47b2_seek, which
; bounds it by the size and walks the chain from the FCB's running cluster
; (+28/+30), and k47b2_nextbyte. The random record moves past them, the position
; goes to 0, DIN_LEFT = the count. out: CF = nothing read -- the end, or a failed
; read with its code pending (DISKOP_ERR). IX = the row (kept: the FAT walk and the
; driver do not touch it, which k47b2_seek itself relies on).
din_fill:
                ; 🔴 THE END IS ANSWERED WITHOUT A SEEK. The random record never
                ; passes the size, so "equal" is the end -- and k47b2_seek at an
                ; offset inside a sector READS that sector even when nothing is
                ; left to deliver. Without this an INPUT# at the end of a file in
                ; an ejected drive raised 70 where main's old reader (FREAD_LEFT = 0,
                ; no I/O) raised 55, and every EOF() at the end cost a sector read.
                ; 24 bits: a floppy file's size never reaches +19.
                ld      a, (ix+33)
                cp      (ix+16)
                jr      nz, dfl_go
                ld      a, (ix+34)
                cp      (ix+17)
                jr      nz, dfl_go
                ld      a, (ix+35)
                cp      (ix+18)
                scf
                ret     z                   ; RR = the size: the end, DIN_LEFT 0
dfl_go:
                ld      hl, 1
                ld      (RDBLK_RECSIZE), hl ; RS = 1
                call    k47b2_seek          ; Cy: unreadable, or past the size
                ld      hl, 0
                jr      c, dfl_set
                call    dout_pos
                ld      (hl), 0             ; the position
                inc     hl
                inc     hl
                inc     hl                  ; HL = the record
                ld      de, 0               ; DE = the count
dfl_lp:
                push    hl
                push    de
                call    k47b2_nextbyte      ; A = the byte; Cy = no more (or a fault)
                pop     de
                pop     hl
                jr      c, dfl_done
                ld      (hl), a
                inc     hl
                inc     de
                bit     0, d                ; 256 read: the record is full
                jr      z, dfl_lp
dfl_done:
                ex      de, hl              ; HL = the count
                ld      a, (ix+33)
                add     a, l
                ld      (ix+33), a
                ld      a, (ix+34)
                adc     a, h
                ld      (ix+34), a
                jr      nc, dfl_set
                inc     (ix+35)             ; the random record (24 bits)
dfl_set:
                ld      (ix+DIN_LEFT), l
                ld      (ix+DIN_LEFT+1), h
                ld      a, h
                or      l
                ret     nz                  ; CF clear: bytes to read
                scf
                ret

; --- D-BOOTDATE: the date question after a boot loader's fallback (oracle) -----
; Both disk references, booting a disk whose boot code cannot open MSXDOS.SYS (it
; then jumps to $4022), ask the date before BASIC starts; MSX-DOS's own `BASIC`
; (also $4022) does not. Measured black box, keys in and the screen and the SAVEd
; file's directory stamp out (scratchpad/bootdate_probe.py, bootdate_run.out; the
; full table is TODO.md D-BOOTDATE). The European VG-8020 + NMS 1200's form is the
; one followed (a style split goes to the VG-8020, Joost 2026-09-04): a cleared
; SCREEN 0, `Enter date (M-D-Y):`, the typed text right after the colon; RETURN
; alone keeps the date; a date is set and BASIC starts; anything else prints
; `Invalid date` on its own line and asks again. MONTH-DAY[-YEAR], separated by
; `-`, `/` or `.`; a two-digit year is 1980..2099 (80-99 = 19xx, 00-79 = 20xx); no
; year is this year; validity (month lengths, leap years) is sdate_body's.
; Rejected as measured: a field 0 or past its range, an empty field (`1--90`),
; junk (`XYZ`), a leading blank. Trailing junk after the year is own choice
; (rejected). Own code throughout, over the published BIOS calls (MSX2 TH: CHGET,
; CHPUT, INITXT): it runs in disk INIT, inside main's slot scan, before the sub
; ROM is found -- so zerobas's own line editor is not there to use.
; 📌 SITED IN THE $4C29 FILL: the largest hole in this ROM (check_disk_walls).
BIOS_CHGET      equ     $009F   ; MSX2 TH BIOS: wait for a key, A = its code
BIOS_INITXT     equ     $006C   ; MSX2 TH BIOS: SCREEN 0 (TEXT1), cleared, cursor home
BD_LINL40       equ     $F3AE   ; MSX2 TH work area: LINL40, SCREEN 0's width (1 B)
BD_SCNCNT       equ     $F3F6   ; MSX work area: SCNCNT, the key-scan countdown (1 B)
BD_LINE         equ     SECTOR_BUF          ; the typed line (own choice: scratch --
                                            ; the BASENT path skips the boot read)
BD_MAX          equ     24                  ; own choice: the longest line kept
BD_DAY          equ     BD_LINE + BD_MAX + 1 ; the day, while the rest is parsed (1 B)
BD_MON          equ     BD_DAY + 1          ; the month (1 B) -- MUST follow BD_DAY:
                                            ; bp_set loads both as one word (E, D)

; basent_resume -- init's BASENT branch (init.asm, just before boot_disk). In: HL = the mark (basent_pending).
; 'B','A' (from MSX-DOS): the mark becomes DOS_SIG, as before (D-CALLSYSTEM).
; 'B','D' (a boot loader's fallback): MSX-DOS never ran, so the mark is cleared --
; first, so a reset during the question boots normally -- and the date is asked,
; unless the disk has an AUTOEXEC.BAS.
; 'B','B' (a running program, D-BASENTRUN): no date; the mark stays until hk_auto
; skips AUTOEXEC.BAS and clears it.
basent_resume:
                ld      a, h
                ld      hl, DOS_SIG
                cp      'A'
                jr      z, brs_mark
                cp      'B'                 ; D-BASENTRUN: from a running program --
                ret     z                   ; the mark stays for hk_auto; no date
                ld      hl, 0
brs_mark:
                ld      (BASENT_REQ), hl
                ret     z                   ; from MSX-DOS: on to BASIC
                ; --- intentional fall-through to boot_date ---
boot_date:
                ; Not when the disk has an AUTOEXEC.BAS -- of ANY size, a 0-byte one too:
                ; both references then start BASIC without asking (and run it), measured
                ; with scratchpad/disklook.py (D-BOOTDATE, the UR-1 row).
                call    auto_find           ; CF = 0: found
                ret     nc
                ; The VG-8020 + NMS 1200 asks at WIDTH 37 (LINL40 = 37, measured: the
                ; prompt starts one column further in than at C-BIOS's default 39; the
                ; CF-3300 asks at 39 -- a style split, the VG-8020's wins, Joost
                ; 2026-09-04). zerobas BASIC sets its own width when it starts, after.
                ld      a, 37
                ld      (BD_LINL40), a
                ; The keyboard answers the prompt at once: C-BIOS leaves the key-scan
                ; countdown SCNCNT at its power-on $FF, so the interrupt took no key for
                ; ~5 s after power-on; main INIT's D-BOOTSCAN store comes after this
                ; question. The VG-8020's start-up clears the cell within 1 s
                ; (scratchpad/keycells_vg.out); measured here in scratchpad/step7b_scncnt.out.
                ld      a, 1                ; 1 = scan at the next interrupt
                ld      (BD_SCNCNT), a
                call    BIOS_INITXT         ; a cleared SCREEN 0, the cursor home
                ld      hl, bd_prompt
bd_ask:
                call    bd_puts
                ei                          ; CHGET waits on the keyboard interrupt
                call    bd_line             ; B = its length
                ld      a, b
                or      a
                jr      z, bd_done          ; RETURN alone: the date stays
                call    bd_parse
                or      a
                jr      z, bd_done          ; a date: stored
                ld      hl, bd_invalid      ; runs on into the prompt
                jr      bd_ask
bd_done:
                di                          ; INIT runs masked (the slot scan's DI)
                ret

; bd_puts -- CHPUT the 0-terminated text at HL. Trashes AF, HL.
bd_puts:
                ld      a, (hl)
                or      a
                ret     z
                call    bd_out
                inc     hl
                jr      bd_puts

; bd_out -- CHPUT A, keeping BC/DE/HL whatever the BIOS does with them.
bd_out:
                push    bc
                push    de
                push    hl
                call    CHPUT
                pop     hl
                pop     de
                pop     bc
                ret

; bd_line -- read a line into BD_LINE, echoed: printable keys are kept (up to
; BD_MAX), BS takes the last one back off the screen (BS, space, BS), other
; control keys are ignored, RETURN ends it. Out: B = length, BD_LINE 0-terminated.
bd_line:
                ld      hl, BD_LINE
                ld      b, 0
bl_key:
                push    hl
                push    bc
                call    BIOS_CHGET
                pop     bc
                pop     hl
                cp      13
                jr      z, bl_end
                cp      8
                jr      z, bl_bs
                cp      ' '
                jr      c, bl_key           ; another control key
                ld      c, a
                ld      a, b
                cp      BD_MAX
                jr      nc, bl_key          ; full: dropped
                ld      (hl), c
                inc     hl
                inc     b
                ld      a, c
                call    bd_out
                jr      bl_key
bl_bs:
                ld      a, b
                or      a
                jr      z, bl_key           ; nothing to take back
                dec     hl
                dec     b
                push    hl                  ; D-DISKREREAD (2026-11-26): back, blank it,
                ld      hl, cinl_bs         ; back again -- conin_line_body's own text
                call    bd_puts             ; (kernel.asm), not three bd_out calls -- 7 B
                pop     hl
                jr      bl_key
bl_end:
                ld      (hl), 0
                ret

; bd_parse -- BD_LINE (0-terminated) as MONTH-DAY[-YEAR]; a date is stored by
; sdate_body. Out: A = 0 stored, else not a date (DATE_DAYS untouched).
bd_parse:
                ld      de, BD_LINE
                call    bd_small            ; the month
                jr      c, bp_bad
                ld      (BD_MON), a
                call    bd_sep
                jr      c, bp_bad
                call    bd_small            ; the day
                jr      c, bp_bad
                ld      (BD_DAY), a
                ld      a, (de)
                or      a
                jr      z, bp_noyear        ; M-D: this year
                call    bd_sep
                jr      c, bp_bad
                call    bd_field            ; the year
                jr      c, bp_bad
                ld      a, (de)
                or      a
                jr      nz, bp_bad          ; anything after the year
                ld      a, h
                or      a
                jr      nz, bp_set          ; 256 and up: as typed (sdate_body judges)
                ld      a, l
                cp      100
                jr      nc, bp_set          ; 100..255: as typed (refused)
                ld      bc, 1900
                cp      80
                jr      nc, bp_add          ; 80..99: 19xx
                ld      bc, 2000            ; 00..79: 20xx
bp_add:
                add     hl, bc
                jr      bp_set
bp_noyear:
                call    gdate_core          ; HL = the year of the date kept so far
bp_set:
                ld      de, (BD_DAY)        ; E = day, D = month -- D-DISKREREAD (2026-11-26):
                                            ; one word load, the cells swapped for it: 4 B
                jp      sdate_body          ; A = 0 stored / $FF not a date
bp_bad:
                ld      a, $FF
                ret

; bd_sep -- step past a `-`, `/` or `.` at DE (CF clear); anything else: CF.
bd_sep:
                ld      a, (de)
                inc     de
                cp      '-'
                ret     z
                cp      '/'
                ret     z
                cp      '.'
                ret     z
                scf
                ret

; bd_small -- bd_field, and the value must fit a byte. Out: CF = no; else A = it.
bd_small:
                call    bd_field
                ret     c
                ld      a, h
                or      a                   ; CF clear
                ld      a, l
                ret     z
                scf
                ret

; bd_field -- a decimal field at DE. Out: CF = no digit there; else HL = its value
; (held once it reaches 2560 -- past every range), DE past the digits. Trashes A, BC.
bd_field:
                ld      hl, 0
                ld      b, h                ; B = digits seen
bf_lp:
                ld      a, (de)
                sub     '0'
                cp      10
                jr      nc, bf_end          ; not a digit
                inc     de
                inc     b
                ld      c, a
                ld      a, h
                cp      10
                jr      nc, bf_lp           ; 2560 or more: held
                push    de
                ld      d, h
                ld      e, l
                add     hl, hl
                add     hl, hl
                add     hl, de
                add     hl, hl              ; x 10
                ld      e, c
                ld      d, 0
                add     hl, de              ; + the digit
                pop     de
                jr      bf_lp
bf_end:
                ld      a, b
                cp      1                   ; CF: no digit
                ret

bd_invalid:     db      13, 10, "Invalid date", 13, 10  ; runs on into bd_prompt
bd_prompt:      db      "Enter date (M-D-Y):", 0

; br2 (D-TPAP1) -- the third piece of the page-3 slot bridges (see br1_tmpl,
; pageenv.asm). br_callf: RST 30h's inline operand is copied to BR_CFOP and the
; BIOS's own CALLF reads it there; it returns to BR_CFOP + 3, a `jp BR_BACK`, with
; the caller's return (past the operand) underneath. br_go / br_frame: br5 (kernel.asm).
br2_tmpl:
br_callf_tmpl:
                ex      (sp), hl            ; HL -> slot, lo, hi
                push    af
                ld      a, (hl)
                ld      (BR_CFOP), a
                inc     hl
                ld      a, (hl)
                ld      (BR_CFOP + 1), a
                inc     hl
                ld      a, (hl)
                ld      (BR_CFOP + 2), a
                inc     hl                  ; the caller's return: past the operand
                ; D-BOOTREGS: an "expanded" slot id whose primary is NOT expanded ($80
                ; for slot 0 here) is called where it is, no slot switch -- the CF-3300
                ; before MSXDOS.SYS (scratchpad/bootcallf_run.out); frayd1.dsk's boot
                ; code asks for $80:$0141 and gets its own program's RAM there.
                push    hl
                ld      hl, (BR_CFOP)       ; L = the slot id
                bit     7, l
                jr      z, br_cf_bios_tmpl
                ld      a, l
                and     $03
                ld      hl, EXPTBL
                add     a, l                ; EXPTBL aligned: no page crossing
                ld      l, a
                bit     7, (hl)
                jr      nz, br_cf_bios_tmpl ; really expanded: the BIOS's CALLF
                pop     hl                  ; HL = past the operand
                pop     af
                ex      (sp), hl            ; [past the operand], HL = the caller's
                push    hl
                ld      hl, (BR_CFOP + 1)
                ex      (sp), hl            ; [target][past the operand]
                ret                         ; called where it is
br_cf_bios_tmpl:
                pop     hl
                pop     af
                ex      (sp), hl
                push    hl
                ld      hl, $0030           ; CALLF
                ld      (BR_TGT), hl
                ld      hl, BR_CFOP
                jp      BR_FRAME
br2_end:

                ; 🔴 NO PINS BETWEEN HERE AND $4C29 (2026-10-15). Thirteen `ds $49xx..$4C25
                ; - $` lines stood here, commented "anchor canonical address", with NO
                ; code between them: the `jp k_49xx` veneers left with D-FATENG
                ; (06c7574c, "position assertions survive, code goes"), each k_49xx is
                ; a lone `ret` in kernel.asm that nothing references, and the stock
                ; kernel's only route to these addresses -- the $F27C.. hiRAM
                ; trampolines (disk/docs/provider-oracle-scope.md) -- init.asm fills
                ; with RET. They were deleted because the comment MISLED: S10.D's
                ; drafter read them as pins and priced this 793 B run at ~59 B.
                ; tools/check_disk_walls.py was right all along (room-before-pin 793 B).
; (rh_flush moved to disk/kernel.asm, in front of the $75A5 pin, for room.)
; rh_init -- INIT's half of D-PUTCACHE (disk/init.asm calls it): no sector held.
; Keeps A (INIT's 0 is still needed after the call).
; (rh_init moved to disk/kernel.asm beside rh_flush, D-WRZER: this fill gave its
; 7 bytes to WRZER's $47D1 veneer.)
; fw_refuse -- fren_wild's refusal (disk/kernel.asm): a built name already exists.
; [idx][cnt] and the 12-byte frame dropped, A = $FF. (Here, in the $4C29 hole, for
; the room in front of $75A5: D-FRENWILD.)
fw_refuse:
                ld      hl, 16
                add     hl, sp
                ld      sp, hl
                jp      fren_miss
; (moved here from kernel.asm by the D-DSKFMT defrag)
dir_nextsec:
                ld      hl, (FAT_DIRSEC)
                inc     hl
                ld      (FAT_DIRSEC), hl
                ld      hl, (FAT_DIRREM)
                dec     hl
                ld      (FAT_DIRREM), hl
                ret
; (moved here from driver.asm by the D-DSKFMT defrag)
; (moved here from kernel.asm by the D-DSKFMT defrag)
seq_k30:
                dec     hl
                ld      a, (FAT_SECPERCLUS)
                call    recperclus_div
                ld      a, e
                ld      (ix+30), a
                ret
                ds      $4C29 - $, $00      ; pad to fdc_zero (net-zero)
; fdc_di_save / fdc_io_done — bracket a sector op with a DI..(EI) guard.
; The WD2793 data transfer is a tight DRQ poll: a foreign interrupt (the 50 Hz
; VDP IRQ, live once MSX-DOS / COMMAND.COM run with EI) preempting the loop drops
; an FDC byte -> LOST DATA, an endless restore-retry livelock (a3 §8.34). MSX-DOS
; boot ran with interrupts masked, so this only bit in the DOS context. fdc_di_save
; records the caller's IFF2 then masks; fdc_io_done restores it on every exit,
; leaving the result (A = error code, Cy, HL) untouched. (`ld a,i` puts IFF2 in
; P/V; the entry clobbers A, but fdc_read_phys/fdc_write_phys take no A input.)
; 🔴 D-IFFREAD (2026-11-13): ASKED TWICE. On an NMOS Z80 (and openMSX) an interrupt
; ACCEPTED during `ld a,i` makes P/V read 0 although interrupts were on -- one such read
; recorded "masked", fdc_io_done then left the CPU masked, and every later sector of the
; save inherited it: a 0.528 s frame-interrupt gap in L5_journal against the CF-3300's
; 0.223 (scratchpad/fdcgap_trace4.out: FDC_IFF 1 on one sector, 0 on the next, IFF 0 from
; there). A wrong read is only ever a false 0, and by the second read the handler has
; returned with EI -- so a 1 from either is the truth. The last read's flags survive the
; `di` and the `ld a,0`.
fdc_di_save:
                ld      a, i            ; P/V = IFF2 (interrupts enabled?)
                jp      pe, fdc_dis_x   ; PE -> IFF2 was set
                ld      a, i            ; a false 0 is possible: ask again
fdc_dis_x:
                di
                ld      a, 0            ; (no flags touched)
                jp      po, fdc_dis_st  ; PO -> masked: remember 0
                inc     a               ; enabled: remember 1
fdc_dis_st:
                ld      (FDC_IFF), a
                ret

fdc_io_done:
                push    af              ; preserve result (A error code + Cy)
                ld      a, (FDC_IFF)
                or      a
                jr      z, fdc_iod_x    ; caller had interrupts masked: leave masked
                ei                      ; restore the caller's enabled interrupts
fdc_iod_x:
                pop     af
                ret

; div9 — divide HL by 9 (logical sector -> track*2+head, sector-1).
;   in:  HL = logical sector (0..1439)
;   out: B = quotient (HL / 9), L = remainder (0..8), H = 0; A trashed
lsec_chs:
                ; D-SIDES: logical sector HL -> C = sector (1..), D = track, E = side,
                ; seated by DSK_MEDIA as the references seat it: bit 1 set = 8 sectors
                ; a track (else 9), bit 0 set = two sides (track = q / 2, side = q & 1),
                ; clear = one side (track = q, side 0). Was div9: always 2 sides x 9.
                ; D-DISKREREAD (2026-11-26): a 16-bit `sbc hl,de` loop with the divisor in
                ; DE (free on entry: the tail below rewrites D and E) -- 9 B, and faster.
                ; `inc b` keeps the carry, and after a looping `sbc` it is 0, so the one
                ; `or a` serves every pass; the overshoot is added back.
                ld      a, (DSK_MEDIA)
                ld      de, 9
                bit     1, a
                jr      z, lc_div
                dec     e                   ; 8 sectors a track
lc_div:
                ld      b, $FF
                or      a
fdc_div_loop:
                inc     b
                sbc     hl, de
                jr      nc, fdc_div_loop
                add     hl, de              ; L = remainder, H = 0
fdc_div_done:                               ; B = q, L = sector - 1
                inc     l
                ld      c, l                ; C = sector
                ld      e, 0
                ld      a, (DSK_MEDIA)
                rra                         ; bit 0: two sides?
                jr      nc, lc_one
                srl     b                   ; track = q / 2 ...
                rl      e                   ; ... side = q & 1
lc_one:
                ld      d, b                ; D = track
                ret

; dskchg — disk-change status inquiry ($4013).
; Returns CF set: "cannot tell".
; 🔴 THIS COMMENT READ "Fail with carry set until FDC driver lands" UNTIL
; 2026-09-09 — and the FDC driver is RIGHT ABOVE IT (`fdc_div_*`, relocated here
; from disk/driver.asm). The prerequisite it named landed long ago; the stub did
; not move, so the reasoning outlived the reason and a reader would go looking for
; a driver that is already there [[a-fix-falsifies-the-justification-beside-it]].
; ✅ MEASURED 2026-11-08 (probes/disk/dskchg.asm; scratchpad/dskchg_stock.out,
; dskchg_stock2.out): the CF-3300 answers B = 1 "unchanged" right after an access and
; 1 s later; B = 0 "unknown" at 3 s and 10 s WITH THE DPB AT HL REFRESHED; Cy = 0. The
; stub (Cy = 1 always) went: dskchg_body (disk/kernel.asm) is that answer. The old note:
; ⚠️ WHETHER THE STUB SHOULD STAY IS UNMEASURED, and this note does not claim it
; should. "Cannot tell" is a legitimate answer for a driver that does not latch the
; disk-change line, `diskbasic-acceptance` is 34/34 with it, and nothing in the
; tree has yet asked what the reference answers here. That is a row nobody has
; written, not a defect anybody has found.
; (dskchg -- a `jp dskchg_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)

; getdpb — build the Drive Parameter Block from the BPB ($4016 disk-ROM entry).
;
; Builds a real DPB from the on-disk BPB. This is the PROVIDER-direction surface
; (disk/docs/expansion-protocol.md §4a): a real MSX-BASIC / MSX-DOS host calls
; $4016 to obtain the mounted volume's geometry as a DPB, so the provider must
; answer it (a black-box trace of BLOAD/SAVE on the CF-3300 showed Disk BASIC
; DOES call GETDPB). zerobas's own loader path never calls $4016 — it derives
; geometry straight from the BPB (fat_mount) — but a foreign host driving us does.
;
; Calling convention (MSX disk-ROM interface; Nextor 2.1 Driver Development Guide
; §4.5.3, which restates the standard GETDPB contract):
;   in:  A  = drive (unit) number (0 = A:)  — ignored, single-drive machine
;        B  = C = media descriptor byte
;        HL = DPB base address MINUS ONE (the byte at base+0 is the drive number,
;             which GETDPB does NOT fill; GETDPB fills base+1 = media onward)
;   out: Cy = 0 ok (DPB filled), Cy = 1 = error (could not read the boot sector)
;
; DPB field layout + encodings: MSX2 Technical Handbook §3, Figure 3.11 (DPB
; structure), and the Nextor 2.1 Driver Development Guide §4.5.3 field formulas.
; Each field below cites its source + derivation. The whole layout was confirmed
; FIELD-FOR-FIELD against a black-box GETDPB trace of the National CF-3300
; reference on this same 720 KB image (carry=0, DPB bytes read out of RAM — the
; reference ROM's code was never read). See disk/PROVENANCE.md §DPB.
;
; fat_mount already parses the BPB into scratch (FAT_FATSTART / FAT_FIRSTROOT /
; FAT_FIRSTDATA / FAT_SECPERCLUS / FAT_NUMFATS / FAT_SECPERFAT) and leaves the
; boot sector in SECTOR_BUF, so GETDPB reuses those (our own code) and reads the
; remaining raw BPB fields (media, sector size, root-entry count) from SECTOR_BUF.
;
; 🔴 D-FATENG: THE FOUR `SECTOR_BUF + n` READS BELOW ARE THE ONLY LITERAL BUFFER
; REFERENCES LEFT IN THIS FILE, AND THEY ARE DELIBERATE. Everything else now
; loads its base from (DBUF_PTR)/(MBUF_PTR) so one engine can serve two callers.
; GETDPB does not need to: it is the disk DRIVER's $4016 entry (disk/init.asm:31),
; reached by MSX-DOS and by our own init -- never by BASIC, whose loader drives
; DSKIO $4010 instead (basic/fat.asm:10). Its buffer is therefore always the BDOS
; one. Converting them would cost a register getdpb does not have spare: HL walks
; the DPB destination across all four, and IX is not free in this file (22 uses;
; BDOS callers hold the FCB there).
; ⚠️ THE LATENT COUPLING, NAMED SO IT IS NOT REDISCOVERED: `fat_mount` above
; reads the boot sector through (DBUF_PTR). If GETDPB were ever reached while
; that pointer addressed BASIC's buffer, fat_mount would fill THAT buffer and the
; reads below would return stale bytes from SECTOR_BUF. Nothing does this today.
; A slice that makes BASIC reach GETDPB must convert these four first.
getdpb:
                ; HL = DPB base - 1 (Nextor §4.5.3). GETDPB fills from base+1 (media)
                ; onward; base+0 (drive number) is the caller's, not ours. So the
                ; first byte WE write (media) lands at HL+1. (CONFIRMED by the
                ; CF-3300 black-box trace: with HL = $C0FF the reference wrote the
                ; media byte at $C100 = HL+1; see disk/PROVENANCE.md §DPB.)
                inc     hl                  ; HL -> DPB +1 (media ID), = caller HL + 1
                push    hl                  ; keep DPB+1 pointer across fat_mount
                call    fat_mount           ; parse BPB; leaves boot sector in SECTOR_BUF
                pop     hl
                ret     c                   ; boot-sector read failed -> Cy = 1

                ; +1 media ID = BPB media descriptor (boot sector +21). TH Fig 3.11
                ; "media ID"; ECMA-107 / MS FAT spec media byte. (= $F9 on 720 KB.)
                ld      a, (SECTOR_BUF + 21)
                ld      (hl), a
                inc     hl                  ; HL -> DPB +2

                ; +2..3 sector size (LE) = BPB bytes-per-sector. TH Fig 3.11; we
                ; validated $0200 (512) at mount, so copy the BPB word verbatim.
                ld      a, (SECTOR_BUF + BPB_BYTSPERSEC)
                ld      (hl), a
                inc     hl
                ld      a, (SECTOR_BUF + BPB_BYTSPERSEC + 1)
                ld      (hl), a
                inc     hl                  ; HL -> DPB +4

                ; +4 directory mask = (sector size / 32) - 1  (Nextor §4.5.3:
                ; "directory mask = (sector size/32) - 1"). 512/32 - 1 = 15 = $0F.
                ; +5 directory shift = number of one-bits in the directory mask
                ;    (Nextor §4.5.3) = log2(entries per sector) = 4 for 512-byte
                ;    sectors. Sector size is validated = 512 at mount, so these are
                ;    the fixed 512-byte values $0F / $04 (CF-3300 oracle: 0f 04).
                ld      (hl), $0F           ; +4 directory mask
                inc     hl
                ld      (hl), 4             ; +5 directory shift
                inc     hl                  ; HL -> DPB +6

                ; +6 cluster mask = (sectors per cluster) - 1   (Nextor §4.5.3).
                ; +7 cluster shift = (one-bits in cluster mask) + 1 (Nextor §4.5.3);
                ;    for a power-of-two secPerClus this equals log2(secPerClus)+1.
                ;    720 KB: secPerClus = 2 -> mask = 1, shift = 2 (CF-3300: 01 02).
                ld      a, (FAT_SECPERCLUS) ; our own BPB-derived value (fat_mount)
                dec     a
                ld      (hl), a             ; +6 cluster mask = secPerClus - 1
                ld      c, a                ; C = cluster mask (count its one-bits)
                inc     hl                  ; HL -> DPB +7
                ld      b, 1                ; shift starts at 1 (Nextor: +1)
                ld      a, c
                or      a
                jr      z, gdpb_popdone
gdpb_pc_loop:
                srl     c
                jr      nc, gdpb_pc_next
                inc     b                   ; one more set bit -> +1 to the shift
gdpb_pc_next:
                ld      a, c
                or      a
                jr      nz, gdpb_pc_loop
gdpb_popdone:
                ld      (hl), b             ; +7 cluster shift
                inc     hl                  ; HL -> DPB +8

                ; +8..9 top sector of FAT (LE) = first FAT sector = reserved sectors.
                ; TH Fig 3.11 "top sector of FAT"; our FAT_FATSTART (fat_mount).
                ld      de, (FAT_FATSTART)
                ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl                  ; HL -> DPB +10

                ; +10 number of FATs. TH Fig 3.11; our FAT_NUMFATS (BPB +16).
                ld      a, (FAT_NUMFATS)
                ld      (hl), a
                inc     hl                  ; HL -> DPB +11

                ; +11 number of directory entries (max 254). TH Fig 3.11; BPB +17
                ; root-entry count low byte (112 on 720 KB -> $70; high byte is 0).
                ld      a, (SECTOR_BUF + BPB_ROOTENTCNT)
                ld      (hl), a
                inc     hl                  ; HL -> DPB +12

                ; +12..13 top sector of data area (LE) = first data sector.
                ; TH Fig 3.11 "top sector of data area"; our FAT_FIRSTDATA.
                ld      de, (FAT_FIRSTDATA)
                ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl                  ; HL -> DPB +14

                ; +14..15 amount of cluster + 1 (LE). TH Fig 3.11 "amount of cluster
                ; + 1"; = data-cluster count + 1. fat_total_clusters returns
                ; dataClusters + 2 (its highest-cluster-plus-one chain bound), so
                ; the DPB field is that value - 1. (CF-3300 oracle: $02CA = 714 =
                ; dataClusters(713) + 1 on this 720 KB image; fat_total_clusters
                ; returns 715, minus 1 = 714.) Microsoft FAT spec §3.3 cluster count.
                push    hl
                call    fat_total_clusters  ; DE = dataClusters + 2 (fat_mount's, D-WCACHE W1)
                dec     de                  ; DE = dataClusters + 1 (the DPB encoding)
                pop     hl
                ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl                  ; HL -> DPB +16

                ; +16 number of sectors per FAT. TH Fig 3.11; our FAT_SECPERFAT
                ; (BPB +22). 720 KB = 3. Single byte (FAT fits < 256 sectors).
                ld      a, (FAT_SECPERFAT)
                ld      (hl), a
                inc     hl                  ; HL -> DPB +17

                ; +17..18 top sector of directory area (LE) = first root-dir sector.
                ; TH Fig 3.11 "top sector of directory area"; our FAT_FIRSTROOT.
                ld      de, (FAT_FIRSTROOT)
                ld      (hl), e
                inc     hl
                ld      (hl), d
                ; +19..20 (FAT address in memory) is filled by the OS, not GETDPB
                ; (TH Fig 3.11) — left untouched, matching the CF-3300 oracle which
                ; left those two bytes unwritten.
                or      a                   ; Cy = 0 success
                ret

; fopen_fill_body — M21a: the real page-1 FOPEN dir-fill (tier2-m21-spec.md
; §5.2/§0.1). Reached as a page-1 TAIL-CALL from the kernel's FOPEN handler via
; the $4462 veneer (disk/driver.asm) — the return address already on the stack
; is the kernel's own BDOS-exit trampoline (§0.1 Fact 4), so this body's exit
; A/F ARE the BDOS FOPEN result, not an internal call's return value. The
; kernel has already filled FCB +0..+13 (search name/ext/extent/S1) via its own
; SFIRST/SNEXT-sourced LDIR before calling here (§5.2's division-of-labor
; finding) — this body owns ONLY +14(high)..+31.
;   in:  DE = FCB pointer (kernel work buffer, e.g. $DA40 runtime / $DC5B boot)
;   out: found:     A = $00, HL = $0000 (§6/§0.1 pinned contract at $C4A1)
;        not found: A = $FF                (§0.1 Fact 5, matches stock's $D88A)
;        $F306 cleared either way (Tier-2 dispatcher-flag rule, M20)
fopen_fill_body:
                push    de
                pop     ix                  ; IX = FCB pointer throughout
                xor     a
                ld      (ix+14), a          ; +14 unconditional pre-search clear (§0.1 facts 2/3)
                call    mnt_find            ; HL = &matched dirent (a miss -> fren_miss)
                call    fcb_dirloc          ; +25 := the entry's index (D-WRBLKSEEK (b))
                ; date/time (§0.1 addendum, confirmed against the FAT12 dir-entry
                ; layout on our own test disk): FCB+20/21 := dirent+24/25 (date),
                ; FCB+22/23 := dirent+22/23 (time) — a word-swap vs. the dirent's
                ; own time-then-date order, not a straight 4-byte copy.
                ld      de, 22
                add     hl, de              ; HL -> dirent+22 (time word)
                ld      a, (hl)
                ld      (ix+22), a
                inc     hl
                ld      a, (hl)
                ld      (ix+23), a
                inc     hl                  ; HL -> dirent+24 (date word)
                ld      a, (hl)
                ld      (ix+20), a
                inc     hl
                ld      a, (hl)
                ld      (ix+21), a
                ; +15 record count = ceil(size/128), capped at 128 ($80)
                ld      hl, (FAT_FILESIZE)
                ld      a, l
                and     $7F
                ld      b, a
                add     hl, hl              ; D-DISKREREAD (2026-11-26): HL >> 7 as
                ld      l, h                ; (HL << 1) >> 8, bit 15 back in through
                ld      h, 0                ; the carry -- not a 7-pass loop: 3 B
                rl      h
                ld      a, b
                or      a
                jr      z, ffb_noround
                inc     hl
ffb_noround:
                ; 🔴 D-FOPENRC (2026-10-01): the cap tested L alone, so a count of
                ; 256 ($0100, a 32 KB file) stored $00 where the CF-3300 stores $80
                ; (disk_probe_wrblk_alt.py --seek's FCB dump), and a size >= 64 KB
                ; lost its high word entirely. ANY high bit caps.
                ld      a, (FAT_FILESIZE + 2)
                ld      b, a
                ld      a, (FAT_FILESIZE + 3)
                or      b
                or      h
                ld      a, $80
                jr      nz, ffb_rcok
                ld      a, l
                cp      $81
                jr      c, ffb_rcok
                ld      a, $80
ffb_rcok:
                ld      (ix+15), a
                ; +16..19 size mirror (4-byte LE)
                push    ix
                pop     hl
                ld      bc, 16
                add     hl, bc
                ex      de, hl
                ld      hl, FAT_FILESIZE
                call    copy4
                ; +24 devid: oracle-observed constant (§3 RC-1). +25 dirloc was left
                ; at the kernel's pre-zero ("cosmetic-only") until D-WRBLKSEEK (b):
                ; fcb_dirloc above now fills it, as the CF-3300 does.
                ld      (ix+24), $40
                ; +26/27 top cluster, +28/29 last cluster (fresh open: both = first)
                ld      hl, (FAT_FIRSTCLUS)
                ld      (ix+26), l
                ld      (ix+27), h
                ld      (ix+28), l
                ld      (ix+29), h
                ; +30/31 relloc = 0 (D-DISKREREAD: through A, 1 B; fat_open sets its own)
                xor     a
                ld      (ix+30), a
                ld      (ix+31), a
                ; Prime the READ-side iterator (M25, tier2-m24-fclose-
                ; multicluster-spec.md "UPDATE 2"): this body previously only
                ; populated FCB display fields, never the internal read state
                ; $14 RDSEQ depends on -- FAT_CURCLUS/FAT_CLUSSEC (fat_open),
                ; BDOS_RECIDX, BDOS_BYTESLEFT. Those were only ever seeded by
                ; our OWN bdos_open/bdos_rdblk, neither of which runs for a
                ; real kernel-driven $0F FOPEN -- so a subsequent $477D-routed
                ; RDSEQ (see wrseq_body below) found BDOS_BYTESLEFT still $0
                ; and returned a false EOF. Exact mirror of bdos_open's own
                ; read-state seeding (disk/driver.asm); reuses the
                ; FAT_FIRSTCLUS/FAT_FILESIZE this body already read above --
                ; no extra mount/find work.
                call    fat_open
                ld      a, RECPERSEC
                ld      (BDOS_RECIDX), a
                ld      hl, FAT_FILESIZE
                ld      de, BDOS_BYTESLEFT
                call    copy4
                ; M33 (tier2-m33-m32-fcb-position-spec.md §2.1): reset the
                ; per-open RDSEQ records-delivered counter alongside the other
                ; read-iterator seeding above (wrseq_writeback increments it).
                ld      hl, 0
                ld      (BDOS_SEQREC), hl
                jp      clr_f306    ; M20 dispatcher-flag rule; HL = 0 out (clr_f306
                                    ; keeps it) -- D-DISKREREAD (2026-11-26): 4 B
; (ffb_miss -- byte-identical to fren_miss below -- removed by D-DISKCARVE, 2026-11-07:
; its two callers jump to fren_miss)

; fren_body — MSX-DOS-1 kernel FREN ($17) canonical entry $4392's real body
; (M26, tier2-m26-spec.md; M35 tier2-m35-m36-tierc-fixes-spec.md §M35 adds the
; collision pre-check below). CLEAN-ROOM: reuses fat_mount/fat_find exactly as
; fopen_fill_body does above (M21a) -- searches the root directory for the
; FCB's OLD 8.3 name (+1..11, CP/M rename convention), then overwrites the
; matched directory entry's name field in place with the NEW 8.3 name
; (+17..27, the "second half" of the rename FCB) and persists it via
; write_sector. fat_find leaves the matched entry inside SECTOR_BUF with
; FAT_DIRSEC holding that entry's own sector number -- exactly the state
; needed to write it back in place; no separate directory-entry-address
; bookkeeping is needed beyond what fat_find already provides.
;
; M35 collision pre-check (bdosx7 differential, Tier-C case 5): before the
; OLD-name find + rename, run a pure existence test on the NEW name
; (FCB+17..27) -- if it is already present, stock refuses the rename (A=$FF,
; mirroring fren_miss's exit) rather than silently producing a duplicate
; directory entry. fat_find takes no IX input and touches no IX-addressed
; state (confirmed: fat_find_body/ff_secloop/ff_found only use AF/BC/DE/HL),
; so IX (the FCB pointer, loaded once at entry) survives this extra call
; unclobbered and is simply re-derived into DE for the real old-name find
; below, byte-unchanged from the pre-M35 body.
; fr_entry -- D-FRENWILD: HL = a root-directory entry index -> HL = that entry in
; FAT_DBUF (its sector read, FAT_DIRSEC = it; the read SKIPPED when FAT_DIRSEC already
; names it -- fat_find and this keep the two together); Cy = past the root, or the
; read failed. Clobbers A, BC, DE.
fr_entry:
                push    hl
                ld      b, 4
fre_shr:
                srl     h
                rr      l
                djnz    fre_shr             ; HL = the sector within the root
                push    hl
                call    fat_rootsecs
                pop     de
                scf
                sbc     hl, de              ; Cy: past the root (sector >= rootsecs)
                jr      c, fre_out
                ld      hl, (FAT_FIRSTROOT)
                add     hl, de
                ex      de, hl              ; DE = the sector
                ld      hl, (FAT_DIRSEC)
                or      a
                sbc     hl, de
                jr      z, fre_have         ; already in FAT_DBUF
                ld      (FAT_DIRSEC), de
                call    rd_dbuf
                jr      c, fre_out
fre_have:
                pop     hl
                ld      a, l
                and     15
                ld      l, a
                ld      h, 0
                call    hl_x32            ; (index AND 15) * 32
                ld      de, FAT_DBUF
                add     hl, de              ; Cy = 0
                ret
fre_out:
                pop     hl
                scf
                ret

fren_miss:
                call     clr_f306   ; M20 dispatcher-flag rule
                scf
                ld      a, $FF
                ret
fren_ioerr:
                call     clr_f306   ; M20 dispatcher-flag rule
                scf
                ld      a, 2                ; generic FDC I/O error (fdc_read_data convention)
                ret

; rdabs_body — MSX-DOS-1 kernel RDABS ($2F) canonical entry $46BA's real body
; (M26, tier2-m26-spec.md §2.4). CLEAN-ROOM, from the published contract
; (map.grauw.nl BDOS function reference, disk/docs/tier2-bdos-remaining-spec.md
; §2 table): in DE = start logical sector, H = sector count, L = drive
; (ignored -- single-drive ROM, same convention as read_sector/write_sector
; above). Target buffer = the runtime DTA (DOS_DTAPTR, $F23D -- M19/M21
; precedent, NOT our own internal BDOS_DTA cell). Calls dskio directly
; (disk/driver.asm) rather than read_sector, since RDABS must honour an
; arbitrary sector count, not just one.
; Exit contract empirically pinned via `capture` (BDOSX3 record 22, H=1):
; stock's own A=$00 B=$00 C=$01 D=$00 E=$00 H=$00 L=$00 on success -- C
; carries the original sector count (not officially documented; matched here
; since BDOSX3's zero-diff bar covers full register state, same discipline
; as M22a's VERIFY/CPMVER register-parity work). dskio clobbers B/C
; internally for its own CHS bookkeeping, so the original count is saved
; across the call on the stack, not left in a register.
rdabs_body:
                ; 🔴 D-SDSBOOT (2026-11-07): through $F37D the CF-3300 hands DE (the
                ; sector) back unchanged and leaves F = $42 (Z, N -- a `sub`), where this
                ; returned dskio's DE and F = $44 (`xor`). SDSNAT1C.DSK's boot code reads
                ; DE after RDABS and took another road here
                ; (scratchpad/bdosregs_probe.py -> bdosregs_run2.out).
                ; 🎯 D-DISKVE (2026-11-17): RDABS and WRABS are ONE body now (the write
                ; was its mirror), through bdos_dskio -- DISKVE's handler on an error.
                xor     a                   ; drive ignored (single-drive); Cy=0 = read
                jr      abs_go
; wrabs_body -- MSX-DOS-1 WRABS ($30), canonical entry $4720 (M26, tier2-m26-spec.md §2.5/§7):
; RDABS's mirror, the DTA as the source; pinned by BDOSX3 record 23 (H=1) -- the same shape as
; RDABS's: A=$00 B=$00 C=$01 D=$00 E=$00 H=$00 L=$00 on success.
wrabs_body:
                xor     a                   ; drive 0 (ignored, single-drive)
                scf                         ; Cy=1 = write direction (MSX2 TH DSKIO)
abs_go:
                push    de                  ; the sector, handed back as on the CF-3300
                ld      b, h                ; B := sector count (dskio's own count param)
                push    bc                  ; preserve original count across the dskio call
                ld      hl, (DOS_DTAPTR)    ; the runtime DTA
                call    bdos_dskio
                jr      c, abs_err
                inc     a                   ; A = $FF: DISKVE's handler answered Ignore
                jr      z, abs_ign
                pop     bc                  ; B := original count again (dskio clobbers B/C)
                pop     de
                ld      c, b                ; C := count (pinned exit value; D-DISKREREAD:
                                            ; not through A, 1 B -- `sub a` needs no input)
                sub     a                   ; A = 0, F = $42 as on the CF-3300 (not xor's $44)
                ld      b, a                ; B := $00 (pinned exit value)
                ld      ($F306), a          ; M20 dispatcher-flag rule
                ld      h, a
                ld      l, a                ; H=L=$00 (pinned exit value)
                ret
abs_ign:
                ; After Ignore the CF-3300 returns A = $FF, B = 1, C = the code the handler
                ; was given (bdos_dskio's B / C), DE = the sector, H = 1, L = $FF
                ; (scratchpad/XE2_ign_stock.out, DVE_run1.out).
                pop     hl                  ; (the count)
                pop     de                  ; the sector
                ld      hl, $01FF
                ld      ($F306), a          ; M20 dispatcher-flag rule (A = 0 here: the
                                            ; `inc a` above gave Z) -- D-DISKREREAD
                                            ; (2026-11-26): clr_f306 inlined, 2 B
                dec     a                   ; A = $FF
                ret
abs_err:
                pop     bc
                pop     de                  ; the sector, handed back
rdabs_ioerr:
                ld      b, a                ; preserve dskio's error code
                call     clr_f306   ; M20 dispatcher-flag rule
                ld      a, b                ; restore error code
                scf
                ret
