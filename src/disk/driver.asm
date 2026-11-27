; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; the disk driver: DSKIO/DSKCHG/GETDPB/DSKFMT + WD2793 FDC primitives
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- Disk entry-point handlers ---------------------------------------------

; dskio — physical sector read/write (disk-ROM entry +$10; also the H.DSKIO
; hook target). MSX2 TH disk ROM interface:
;   in:  Cy = 0 read / 1 write; A = drive; B = sector count; C = media byte;
;        DE = start logical sector; HL = transfer address
;   out: Cy = 0 ok; Cy = 1 error with A = error code, B = sectors not done
; Single-drive machine: the drive number is ignored (always drive A); the media
; byte sets the seating (D-SIDES, below). Both directions are implemented: read via fdc_read_phys, write via
; fdc_write_phys. See PROVENANCE.md §FDC / §DSKIO interface.
dskio:
                ; D-SIDES (2026-10-21): the MEDIA BYTE (C) decides the seating -- bit 0
                ; two sides, bit 1 eight sectors a track -- as on both references, which
                ; read a single-sided disk past its first tracks (bpbrule_p8f.out). A
                ; C below $F8 is not a media ID: our own FAT layer passes 0 for "the
                ; mounted disk's" (fat_geom_read records it), so the last one stays.
                push    af                  ; Cy = the direction
                ld      a, c
                cp      $F8
                jr      c, dskio_keep
                ld      (DSK_MEDIA), a
dskio_keep:
                ; 💰 D-DSKFMT carve 16: one setup for both directions (no `ld` touches
                ; the flags); a zero count leaves by the loop head's own test.
                ld      a, b
                ld      (FDC_CNT), a
                ld      (FDC_LSEC), de
                ld      (FDC_DEST), hl
                pop     af              ; Cy = the direction
                jr      c, dskio_write
                ; --- read path ---
dskio_next:
                call    dskio_head      ; count 0: leaves DSKIO; else C/D/E = CHS, HL = dest
                ; page-1 bounce: if destination in $4000-$7FFF (page 1 = our disk
                ; ROM under DOS) read into SECTOR_BUF then blit to the real target
                ; with page 1 remapped to RAM (a3 §8.35 / p1_blit_tmpl below).
                ld      a, h
                and     $C0
                cp      $40             ; $4000-$7FFF: bit7=0, bit6=1
                jr      nz, dskio_rd    ; not page 1 -> direct path
                ld      (P1_DEST), hl   ; save real page-1 destination
                ld      hl, SECTOR_BUF
                ld      (FDC_DEST), hl  ; redirect FDC to bounce buffer
                call    fdc_read_phys
                jr      c, dskio_err
                call    fdc_di_save     ; a3 §8.36: mask across the page-1 blit too
                ld      hl, SECTOR_BUF  ; D-P1RDBLK: P1_BLIT takes HL / DE / BC now
                ld      de, (P1_DEST)
                ld      bc, 512
                call    P1_BLIT         ; copy SECTOR_BUF -> P1_DEST with page1=RAM
                call    fdc_io_done     ; restore caller IFF after the blit
                ld      hl, (P1_DEST)
                ld      de, 512
                add     hl, de
                call    dskio_bump      ; the real destination + 512, then the count
                                        ; 🔴 D-ROBOREG: carve 16 had `call dskio_adv`
                                        ; here, which skips the FDC_DEST store -- every
                                        ; sector after a page-1 one landed at
                                        ; SECTOR_BUF + 512 * n, SDSNAT1C's 36-sector read
                                        ; into $2000 running up over the hooks ($FCAD..)
                jr      dskio_next
dskio_rd:
                call    fdc_read_phys
                jr      c, dskio_err
                call    dskio_bump      ; HL = buf+512, advance for next sector
                jr      dskio_next
; 💰 D-DSKFMT carve 16: the per-sector advance, once for both directions.
; dskio_bump: FDC_DEST := HL; FDC_LSEC += 1; FDC_CNT -= 1. Both read arms MUST store the
; destination: the page-1 arm's FDC_DEST is SECTOR_BUF until this puts the real one back.
dskio_bump:
                ld      (FDC_DEST), hl
                ld      hl, (FDC_LSEC)
                inc     hl
                ld      (FDC_LSEC), hl
                ld      a, (FDC_CNT)
                dec     a
                ld      (FDC_CNT), a
                ret
; dskio_head -- 💰 D-DISKREREAD (2026-11-26): both loop heads, once (6 B), and the
; old dskio_ok tail folded into dskio_err (7 B). Count left: C = sector, D = track,
; E = side (lsec_chs), HL = FDC_DEST, return. Count 0: DROP the loop's return and
; leave DSKIO through dskio_err with A = 0 and F from `or a` on 0 (Z, NC, PE) --
; there FDC_CNT = 0 gives B = 0, which is exactly what the old dskio_ok returned
; (mtoff's `xor a`, `ld b,0`, `or a`).
dskio_head:
                ld      a, (FDC_CNT)
                or      a
                jr      z, dh_none
                ld      hl, (FDC_LSEC)
                call    lsec_chs        ; C = sector, D = track, E = side (D-SIDES)
                ld      a, d            ; D-DSKIOCYL (2026-11-26): a track past the drive's 80 answers
                cp      80              ; 2 (NOT READY) at once, as the CF-3300 -- sectors 1440 / 1500 /
                jr      nc, dh_far      ; 2000 of a 720 KB disk; $FFFF wraps in range on both (8-bit q)
                ld      hl, (FDC_DEST)
                ret
dh_far:
                ld      a, 2            ; NOT READY, Cy: out through dskio_err like a failed transfer
                scf
dh_none:
                inc     sp
                inc     sp              ; drop the loop's return (flags kept): DSKIO ends
dskio_err:
                push    af              ; save error code + carry
                call    mtoff
                ld      a, (FDC_CNT)
                ld      b, a            ; B = sectors not transferred
                pop     af
                ret                     ; Cy still set, A = error code (dh_none: A = 0, NC)
; --- write path -------------------------------------------------------------
; Mirrors the read loop's structure exactly: loop the sector count, convert each
; logical sector -> CHS via lsec_chs, and call fdc_write_phys (the write twin of
; fdc_read_phys). Entered from dskio's shared setup (carve 16), Cy = 1.
dskio_write:
                call    dskio_head      ; count 0: leaves DSKIO; else C/D/E = CHS, HL = dest
                call    fdc_write_phys
                jr      c, dskio_err
                call    dskio_verify    ; D-VERIFYREAD: VERIFY on -> the sector read back; Cy its error
                jr      c, dskio_err
                call    dskio_bump      ; HL advanced by 512 on success
                jr      dskio_write

; fdc_read_phys — read one physical sector into (HL).
;   in:  D = track, E = side (0/1), C = sector (1..9), HL = buffer
;   out: Cy = 0 ok, HL advanced 512; Cy = 1 error, A = DSKIO error code
; Selects drive A + side + motor, then seeks and reads, retrying once via a
; restore if the first attempt fails (recovers a stale Track register).
fdc_read_phys:
                call    fdc_prep        ; DI, select, settle, two attempts (D-DISKCARVE2)
fdc_rp_attempt:
                call    fdc_seek        ; seek to target track
                ; issue read-sector and transfer the data
                ld      a, c
                ld      (FDC_SECTOR), a
                ld      hl, (FDC_DEST)  ; reload buffer start each attempt
                ld      a, CMD_READ
                ld      (FDC_STATUS), a
                call    fdc_read_data_body
                jp      nc, fdc_io_done ; success: Cy = 0, HL = buffer + 512
                ; failure: A = error code, Cy = 1
                call    fdc_again       ; last try: ends at fdc_io_done; else restored
                jr      fdc_rp_attempt

; 💰 D-DISKCARVE2 (2026-11-08): the three pieces fdc_read_phys and fdc_write_phys
; both carried, once. fdc_prep: interrupts masked (fdc_di_save), drive A + side
; (E bit 0) + motor, settle, FDC_TRY = 2. fdc_seek: seek to track D, wait.
; fdc_again: A / Cy = a failed attempt's result; with a try left, recalibrate and
; return (the caller re-attempts); on the last, DROP the caller's return and end
; the transfer through fdc_io_done with that result -- what the two tails did.
fdc_prep:
                call    fdc_di_save     ; mask interrupts across the transfer
                ld      a, CTRL_DRIVE_A + CTRL_MOTOR
                bit     0, e
                jr      z, fdc_pp_nos
                or      CTRL_SIDE
fdc_pp_nos:
                ld      (FDC_CTRL), a
                call    fdc_settle      ; motor spin-up / head settle
                ld      a, 2
                ld      (FDC_TRY), a    ; up to two attempts
                ld      hl, (JIFFY)     ; 🔴 D-DSKCHG: the clock DSKCHG reads -- every sector
                ld      (DSK_STAMP), hl ; command starts here (HL is free: both callers
                ret                     ; reload it from FDC_DEST)
fdc_again:
                push    af
                ld      a, (FDC_TRY)
                dec     a
                ld      (FDC_TRY), a
                jr      z, fdc_ag_last
                pop     af
                jp      fdc_restore     ; recalibrate; the caller retries
fdc_ag_last:
                pop     af              ; the error code + carry
                inc     sp
                inc     sp              ; drop the caller's return: the transfer ends
                jp      fdc_io_done

; fdc_read_data — transfer 512 bytes of a read-sector command into (HL).
;   in:  HL = buffer; a READ command has just been written
;   out: Cy = 0 ok, HL += 512; Cy = 1 error with A = DSKIO error code
; Polled transfer: 512 = 2 x 256 (E = block counter, B = byte counter), so the
; sector (C) and track (D) registers survive for a possible retry.
; RELOCATED (M26, tier2-m26-spec.md): this 74-byte body used to live here
; in full, but 56 bytes in (the fdc_rd_n1/fdc_rd_n2 status-decode tail) it
; collided with the real kernel's canonical $17 FREN dispatch entry, $4392
; (confirmed via trace --resync: stock's real disk ROM jumps to its own
; FREN body from that exact address; ours fell through into this unrelated
; FDC code instead -- the same address-collision shape as M21's $4462 and
; M24's $456F). Only ONE caller (`call fdc_read_data` in fdc_rp_attempt
; above), self-contained, no external refs into its middle -- so it
; relocates cleanly. Real body now fdc_read_data_body (kernel.asm free
; tail); this span is net-zero pad with the fren_body veneer planted at
; the pinned offset so fdc_write_phys below stays at its exact address.
; (fdc_read_data -- a `jp fdc_read_data_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; --- calbak: ONE inter-slot call back into main page 1 (MSX2 TH) ------------
; D-DISKVERB. IX = the main page-1 target (an address from the GENERATED
; basic-resident-abi.inc, so a page-1 shift cannot leave us calling a stale one).
; A/HL/DE/BC pass through in both directions -- MEASURED, not assumed
; (D-XSLOTABI, scratchpad/xslot_abi.py); IY is ours and IX is consumed.
; 🔴 D-HOOKDI: and it RETURNS WITH INTERRUPTS ON. CALSLT returns DI, and the FDC
; driver keeps the caller's state per sector (fdc_di_save), so every disk verb
; whose work followed a calbak ran masked to its end: TIME stood still for a
; whole SAVE / KILL / COPY / NAME, keys typed meanwhile were lost
; (scratchpad/verbclock_run.out, against the CF-3300's 30-75 %). Every caller is
; a BASIC-facing hook handler; the one that hands over to MSX-DOS masks again
; itself (hsy_ok). It was `jp CALSLT`, CALSLT's `ret` landing on our caller.
calbak:
                ld      iy,(EXPTBL-1)   ; IYh = main-ROM slot id (MSX2 TH idiom)
                call    CALSLT
                ei
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; new_taken -- is DISK_FCB_NAME (the NEW name) already in the directory? fat_find,
; with FWR_DIRSEC / FWR_DIROFF (the OLD entry, which a rename then rewrites) kept.
; Out: CF clear = taken; HL clobbered (POP and LD (nn),HL touch no flag). It stood
; twice, inline, in bb_rename and hkn_a (D-DSKFMT's carve, 2026-11-10).
new_taken:
                ld      hl, (FWR_DIRSEC)
                push    hl
                ld      hl, (FWR_DIROFF)
                push    hl
                call    ff_name
                pop     hl
                ld      (FWR_DIROFF), hl
                pop     hl
                ld      (FWR_DIRSEC), hl
                ret
; fname_ev -- the file-name expression through calbak (MOVED here by D-DISKCARVE6 step 2).
fname_ev:
                ld      ix,fname_expr
                jr      calbak              ; D-DISKROOM: jp -> jr
                ds      $436C - $, $00      ; pad remainder up to the pinned canonical entry
k_436C:
                jp      fdel_body           ; $436C: BDOS $13 FDEL canonical entry (M26)
                                            ; (re-characterised 2026-07-03: this address used
                                            ; to sit mid-body inside fdc_read_data's OLD span;
                                            ; landing FREN's relocation above turned it into
                                            ; dead pad -- but before this fix, the un-wired
                                            ; FDEL call NOP-slid straight through into
                                            ; fren_body with FDEL's own FCB, corrupting the
                                            ; target directory entry. See tier2-m26-spec.md
                                            ; sec 2.2.)
; dst_quoted -- din_str inside the quotes: every byte is data, CR and LF too,
; until the closing `"`; then din_num's dnm_tail eats what may follow it.
dst_quoted:
                call    din_peek
                ret     c
                call    din_take
                cp      '"'
                jp      z, dnm_tail
                call    dst_put
                jr      dst_quoted
; 📍 D-LOADFAT (2026-11-06): MOVED HERE from the $607B..$75A5 region, where LOAD's
; FAT cache needed the room; entered only by call / jp, so it can live in any hole.
install_hook:                               ; (D-DISKREREAD 2026-11-26: `ld (hl),n`
                                            ; for +0 / +4, 2 B; A out = the slot byte,
                                            ; which ibh_lp reloads)
                ld      (hl), $F7           ; +0: RST 30h (CALLF)
                inc     hl
                ld      a, (HOOK_SLOT)      ; +1: this ROM's slot byte
                ld      (hl), a
                inc     hl
                ld      (hl), e             ; +2: target lo
                inc     hl
                ld      (hl), d             ; +3: target hi
                inc     hl
                ld      (hl), $C9           ; +4: RET
                ret
                ds      $4392 - $, $00      ; pad remainder up to the next canonical entry
k_4392:
                jp      fren_wild           ; $4392: BDOS $17 FREN canonical entry (M26; D-FRENWILD)
; fdc_seek -- D-DISKCARVE2's helper (seek to track D, wait); here since D-DSKCHG,
; whose stamp needed the bytes beside fdc_prep. Entered only by `call`.
fdc_seek:
                ld      a, d            ; track number via the Data register
                ld      (FDC_DATA), a
                ld      a, CMD_SEEK
                jr      fdc_cmd         ; D-DISKREREAD (2026-11-26): fdc_restore's store -- 3 B
; fcf_zero -- fat_count_free's DE := 0 exit (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
fcf_zero:
                ld      de, 0
                ret
                ds      $43A4 - $, $00      ; net-zero: fdc_write_phys stays at $43A4

; fdc_write_phys — write one physical sector from (HL).
;   in:  D = track, E = side (0/1), C = sector (1..9), HL = buffer
;   out: Cy = 0 ok, HL advanced 512; Cy = 1 error, A = DSKIO error code
; The write twin of fdc_read_phys: selects drive A + side + motor, seeks to the
; target track, then issues the WD2793 Write Sector command (CMD_WRITE = $A0,
; single record) and feeds the 512-byte payload to the data register on DRQ.
; Retries once via a restore if the first attempt fails (recovers a stale Track
; register after a reset), exactly like the read path. A genuine write-protected
; disk is reported up front (no retry) by fdc_write_data's status check.
fdc_write_phys:
                call    fdc_prep        ; DI, select, settle, two attempts (D-DISKCARVE2)
fdc_wp_attempt:
                call    fdc_seek        ; seek to target track
                ; D-DISKERR (docs/spec-basic-diskerr.md): an EMPTY drive must fail a
                ; write the way it fails a read -- the seek's type-I status carries
                ; NOT READY; the body is in the kernel's corridor (this span has 7 B
                ; before the $4462 pin).
                call    fdc_wp_chkrdy   ; CY, A=2 when the drive is not ready
                jp      c, fdc_io_done
                ; issue write-sector and transfer the data
                ld      a, c
                ld      (FDC_SECTOR), a
                ld      hl, (FDC_DEST)  ; reload buffer start each attempt
                ld      a, CMD_WRITE
                ld      (FDC_STATUS), a
                call    fdc_write_data_body
                jp      nc, fdc_io_done ; success: Cy = 0, HL = buffer + 512
                ; failure: A = error code, Cy = 1
                ; 🔴 D-WPROTECT (2026-09-28): THIS WAS `cp 0`, AND IT CLEARED THE CARRY
                ; ON EVERY FAILED WRITE -- nothing is below 0, so `cp 0` always leaves
                ; Cy = 0. Write-protect (A = 0) left here "successful"; every other
                ; error was pushed below with that cleared carry and popped back at
                ; fdc_wp_fail2 as a success too. So no disk WRITE error ever reached
                ; BASIC: SAVE / BSAVE / OPEN FOR OUTPUT on a protected disk answered
                ; OK where the CF-3300 raises 68 (scratchpad/errkeep2_run3.out), and
                ; scratchpad/wptrace_probe.py saw ST_WP set after all 5 writes with
                ; dc_result reached at Cy = 0 every time. `inc a / dec a` tests
                ; A = 0 in the same 2 bytes and leaves Cy alone.
                inc     a
                dec     a               ; Z iff A = 0 (write protected); Cy KEPT
                jp      z, fdc_io_done
                call    fdc_again       ; last try: ends at fdc_io_done; else restored
                jr      fdc_wp_attempt
; (fdc_wp_fail -- a `jp fdc_io_done` veneer -- retired by D-DSKFMT carve 32: its callers jump there)

; fdc_write_data — transfer 512 bytes from (HL) to a write-sector command.
;   in:  HL = buffer; a WRITE command has just been written
;   out: Cy = 0 ok, HL += 512; Cy = 1 error with A = DSKIO error code
; Polled transfer, the exact write twin of fdc_read_data: 512 = 2 x 256
; (E = block counter, B = byte counter), so the sector (C) and track (D)
; registers survive for a possible retry. The WD2793 asserts DRQ when it is ready
; for the next data byte; the driver polls the status register at $7FB8 and writes
; each byte to the data register. After the last byte it polls for BUSY-clear and
; reads the result-phase status: bit 6 ($40) on a Type-II write is WRITE FAULT /
; write-protect -> DSKIO write-protected (code 0); RNF/CRC/LOST/NOTRDY map exactly
; as the read path does.  (WD2793 DS: Write Sector, status register Type II.)
; (fdc_write_data -- a `jp fdc_write_data_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
                                            ; it waits for its first DRQ with interrupt
                                            ; windows (fdc_fdrq)

; fdc_restore — recalibrate the head to track 0 (Type I restore).
fdc_restore:
                ld      a, CMD_RESTORE
fdc_cmd:                                ; (fdc_seek's command store too)
                ld      (FDC_STATUS), a
                ; fall through to fdc_wait_ready

; fdc_wait_ready — wait for the WD2793 to finish (BUSY clear).
fdc_wait_ready:
                call    fdc_settle      ; let BUSY assert before polling
fdc_wr_loop:
                ld      a, (FDC_STATUS)
                bit     0, a            ; BUSY?
                jr      nz, fdc_wr_loop
                ret

; fdc_settle — short busy-wait (~0.9 ms) for command latency / motor spin-up.
fdc_settle:
                push    bc
                ld      b, 0            ; 256 iterations
fdc_st_loop:
                djnz    fdc_st_loop
                pop     bc
                ret

; fdc_di_save/fdc_io_done/div9/dskchg/getdpb relocated to disk/fat.asm's free
; tail (M21a, disk/docs/tier2-m21-spec.md §5.3): $4462 — a byte inside the old
; fdc_di_save body — collided with the kernel's FOPEN dir-fill entry point
; (RC-1). Vacating this span lets a real veneer sit exactly at $4462; every
; caller of the moved routines resolves by symbol (grep-confirmed, §5.3), so
; no repoint is needed anywhere but here.
; drv_select's text (D-DSKIB, disk/kernel.asm): sited in this fill for room.
dsl_text_1:     db      "Insert diskette for drive ",0
dsl_text_2:     db      ":",13,10,"and strike a key when ready"
dsl_text_nl:    db      13,10,0
; 📍 D-COPYALL (2026-11-06): MOVED HERE from the $5602..$5E10 fill, which COPY's
; every-match walk outgrew by 56 B; entered only by call / jp, so it can live
; in any hole.
; hoc_dout -- hkk_open_check's arm for a DOUT_MODE channel B -- or, since S10.D,
; a RANDOM one, and since S10.C an INPUT one (the same FCB row): its name is the
; FCB's +1..11, no sector to read. CF=1 a match.
hoc_dout:
                push    bc
                ld      a, b
                call    dout_row
                inc     hl                  ; the FCB's name
                ld      de, DISK_FCB_NAME
                call    hoc_cmp             ; Z = it matches the pattern, CASE KEPT
                pop     bc
                scf
                ret     z
                jp      hoc_next
; dc_unk -- dskchg_body's "unknown" path (D-DSKCHG; here for room): as the CF-3300 does,
; bring the DPB at HL up to date (getdpb reads the boot sector), then B = 0. Cy / A are
; getdpb's (an unreadable disk answers its error).
dc_unk:
                call    getdpb
                ld      b, 0
                ret
; 🐌 D-WRBLKFAT (2026-11-08): WRBLK's chain extension (wroe_need_extend, disk/kernel.asm)
; in frl_ext's shape (D-RECSLOW): the new cluster's EOC mark and the link from the
; previous cluster both DEFERRED in FAT_MBUF (FAT_DEFER bit 0 -> dirty), then the
; sector written to both FATs once -- it was written after the mark AND after the
; link (scratchpad/fdclog7_1108.out: 4 FAT writes a cluster). Nothing outlives the
; call. A failure ends through frl_xfail: what was claimed reaches the disk, as before.
;   out: HL = the new cluster; Cy = 1 disk full / I/O (A = the code)
wroe_ext:
                call    frl_defer
                call    fat_alloc_cluster   ; HL = the new cluster, its EOC deferred
                jp      c, frl_xfail
                ld      de, (WRBLK_PREVCLUS)
                ld      a, d
                or      e
                jr      z, wx_out           ; the file's first cluster: nothing to link
                push    hl
                ex      de, hl              ; HL = the previous cluster, DE = the new one
                call    frl_defer
                call    fat_write_fat_entry_hot ; link it (the scan left FAT_MBUF/FATSEC)
                pop     hl
                jp      c, frl_xfail
wx_out:
                push    hl
                call    frl_flush           ; the held sector out, both copies (Cy = I/O)
                pop     hl
                ret
; hl_x128 / hl_x32 -- HL := HL x 128 / x 32 (MOVED here by D-DISKCARVE6, 2026-11-17).
hl_x128:
                add     hl, hl
                add     hl, hl
hl_x32:
                add     hl, hl
                add     hl, hl
                add     hl, hl
                add     hl, hl
                add     hl, hl
                ret
                ds      $4462 - $, $00      ; pad to the collision point
                jp      fopen_fill_body     ; M21a: real FOPEN dir-fill (disk/fat.asm)
; dta_put -- store A at (RDBLK_DST) and advance it: the block reads' one store.
; 🔴 D-P1RDBLK / D-P1DOS (2026-10-22): a DTA in page 1 is RAM UNDER this ROM, and
; `ld (hl),a` wrote into the disk ROM -- the data was lost. starsh2's loader (through
; $F37D, scratchpad/starsh2_p1.out) and an MSX-DOS program of our own (through
; $0005, scratchpad/p1dta_run.out) both read to $4000: the CF-3300 has the record
; there, zerobas had $FF. A page-1 byte goes through P1_BLIT with page 1 = RAM.
; P1_DEST is dskio's bounce word, idle while a block read copies out of the buffer.
;   in: A = the byte   out: RDBLK_DST advanced   clobbers A, BC, DE, HL
dta_put:
                ld      hl, (RDBLK_DST)
                ld      c, a
                ld      a, h
                and     $C0
                cp      $40
                ld      a, c
                jr      nz, dp_plain        ; not page 1: a plain store
                ex      de, hl              ; DE = the destination
                call    blit1               ; D-DISKREREAD (2026-11-26): the shared
                                            ; one-byte P1_BLIT (bsr_have's) -- 9 B
                ex      de, hl              ; HL = past the destination
                jr      dp_done
dp_plain:
                ld      (hl), a
                inc     hl
dp_done:
                ld      (RDBLK_DST), hl
                ret
; install_p1blit -- D-P1RDBLK (2026-10-22): put P1_BLIT in page-3 RAM on EVERY host.
; It was installed only by install_resident, inside the $FF-gated block, so on the
; C-BIOS target any DSKIO into page 1 called whatever RAM held at P1_BLIT.
install_p1blit:
                ld      hl, p1_blit_tmpl
                ld      de, P1_BLIT
                ld      bc, p1_blit_end - p1_blit_tmpl
                ldir
                jp      p1_patch            ; the flip's operand (D-DISKMSX2HOST)
; (boot_call0 moved to the kernel's old CALLF-handler hole, D-BOOTREGS.)
; (hdl_scan, D-LOADBASE's first-line scan, is D-LOADDIRECT's hdl_walk now.)
                include "basic/tokpay-body.inc"   ; tok_payload (D-LOADBASE)
; 📍 D-LOADRUN (2026-11-06): MOVED HERE from the $782B..$7F7C region, which LOAD's
; hdl_run outgrew by 48 B; entered only by call / jp, so it can live in any hole.
; fat_mount_tail — verbatim relocation of disk/fat.asm's fat_mount continuation
; (from the numFATs*secPerFAT multiply setup through the final `ret`,
; unchanged). Moved here only because the multiply's `ld hl,0` tail byte
; collided with the new $477D WRSEQ-worker canonical entry; reached solely by
; fat_mount's own `jp fat_mount_tail` fall-through (position-free, no
; external caller). The diversion point had to move one instruction earlier
; than the collision itself (to `ld b,a`) to leave enough room for the
; 3-byte veneer to land exactly at $477D (a 3-byte-for-3-byte swap at the
; collision point alone leaves zero slack).
; wrseq_body — MSX-DOS-1 kernel per-record sequential-I/O worker entry
; $477D's body. CORRECTED (M25, tier2-m24-fclose-multicluster-spec.md
; "UPDATE 2"): the RAM kernel CALLs $477D for EVERY sequential record
; access -- BOTH BDOS $15 WRSEQ *and* BDOS $14 RDSEQ share this ONE page-1
; entry (confirmed black-box via `callwatch --in-func 0x14`; no register at
; entry distinguishes direction -- BC/DE/HL/AF identical in shape between a
; genuine WRSEQ hit and a RDSEQ hit), so direction is read from OUR OWN
; BDOS_WRMODE cell instead: nonzero forwards to bdos_seqwrite (unchanged);
; zero forwards to bdos_seqread. The companion fix in fopen_fill_body
; (disk/fat.asm) seeds the read-iterator state (fat_open, BDOS_RECIDX,
; BDOS_BYTESLEFT) that bdos_seqread depends on and that a real kernel-driven
; FOPEN never populated before -- without it bdos_seqread ran but always
; reported false EOF.
; DTA staleness (M21b lesson, see k_47B2 above): the kernel's real SETDTA
; only ever writes DOS_DTAPTR ($F23D), never our own mini-BDOS's BDOS_DTA
; cell, so BDOS_DTA must be reseeded from DOS_DTAPTR before EITHER
; bdos_seqread or bdos_seqwrite consumes it -- both directions need the
; reseed, not just write. DE (=IY=$DA40 kernel FCB pointer) is not consumed
; by either -- both are single-open-file global state, same as Tier-1. Exit
; A=$00/$01/$FF passes straight through from whichever real routine ran.
; $F306 left untouched (M24 spec §5 item 4: no observed load-bearing exit HL
; for this entry family -- default to the kernel's own H:=B,L:=A mirror
; unless the acceptance diff proves otherwise).
;
; M33 (tier2-m33-m32-fcb-position-spec.md §2.2) added the RDSEQ FCB position
; write-back HERE, in the read branch only -- NOT inside bdos_seqread_body,
; which is a SHARED routine also reached by RRND $21 (rdrnd_body, kernel.asm
; ~2697, which does its own M26 copy+32:=copy+33 bookkeeping) and by the boot
; mini-BDOS (driver.asm bdos_entry, which must stay byte-identical). Putting
; the write-back inside bdos_seqread_body would corrupt those other callers'
; FCBs / the boot path, so instead the read branch's tail-jump becomes a call
; + inline write-back + ret. Only on a DELIVERED record (A=$00 on return; on
; A=$01 EOF, §2.3, skip it -- stock's position stops at/after EOF). Body =
; wrseq_writeback (free tail): writes copy+32/+12/+28/29/+30 into the FCB in IX
; (it was the fixed base $DA40 until D-BDOSFCB stage B -- the same address under
; MSX-DOS, where DE = $DA40 -- and is DE at entry, popped into IX after the
; body, so a $F37D caller's own FCB gets it). A is stashed across the write-back and restored before ret so
; bdos_seqread's pass-through A/HL exit contract is unchanged.
; 🔴 D-SEQTWO (2026-11-09): THE BRANCH IS THE ENTRY, NOT BDOS_WRMODE. This picked
; read or write by BDOS_WRMODE -- "a file is open for writing" -- so once a program
; FMAKE'd a copy, every SEQREAD ($4775) of its source ran the WRITE branch and the
; copy never ended (probes/disk/dosl/d3copy.asm; the CF-3300 copies it). $4775 is
; SEQREAD's entry and $477D SEQWRITE's (callwatch, per call): each its own branch.
; D-BDOSFCB stage B (2026-11-10): the write-backs address the FCB in IX -- DE at entry,
; kept across the body -- not the fixed $DA40. Under MSX-DOS DE IS $DA40, so nothing
; changes there; from $F37D (bdos_entry now routes $14 / $15 here) they reach the
; caller's own FCB, as the CF-3300's do (scratchpad/bdosfcb_run3.out: CR and the size).
wrseq_rd:                                   ; $4775: BDOS $14 SEQREAD
                push    de
                pop     ix
                call    srd_size0           ; D-BDOSUNOP: a size of 0 -- an FCB never
                ld      a, 1                ; opened, or an empty file -- is end of
                ret     z                   ; file, as on the CF-3300 (kernel.asm)
                push    de
                call    srd_pos             ; DTA, and the record EX * 128 + CR (B2)
                call    bdos_seqread_body
                pop     ix                  ; IX = the FCB (POP keeps the flags)
                or      a
                call    z, wrseq_writeback  ; only on a delivered record (A=$00)
                ret
; rh_check -- D-PUTCACHE, read_sector's first act (basic/fat-prim-body.inc). The
; sector a PUT left unwritten in FAT_MBUF (RND_HSEC) is either the one being read
; INTO FAT_MBUF -- then the buffer already holds it: read_sector returns at once,
; CF = 0 -- or it is written out first (rh_flush, disk/fat.asm) and the read goes on.
; A failed write returns read_sector's CF / A to its caller.
;   in DE = the sector, HL = the destination. Keeps DE, HL.
rh_check:
                push    hl
                ld      hl, (RND_HSEC)
                ld      a, h
                or      l
                jr      z, rhc_go           ; nothing held
                sbc     hl, de              ; (CF = 0)
                jr      nz, rhc_flush       ; another sector
                pop     hl
                push    hl
                ld      a, h
                cp      high FAT_MBUF
                jr      nz, rhc_flush
                ld      a, l
                cp      low FAT_MBUF
                jr      nz, rhc_flush
                pop     hl                  ; a HIT: FAT_MBUF holds it already
                inc     sp
                inc     sp                  ; past read_sector itself
                xor     a                   ; CF = 0, A = 0 (DSKIO's success)
                ret
rhc_flush:
                call    rh_flush
rhc_go:
                pop     hl
                ret     nc                  ; on to the read
                inc     sp
                inc     sp                  ; a failed write: read_sector's caller
                ret                         ; gets its CF and A
                ds      $44F4 - $, $00      ; pad the rest of the vacated span (net-zero)

; choice — format-choice string (CHOICE entry point): HL -> the menu FORMAT prints,
; $00-terminated. D-DSKFMT (2026-11-10): the CF-3300's four choices, measured as the
; text a program is handed (scratchpad/dskfmt_stock2.out, _c2 .. _c4: 82 bytes, CR LF
; first, ",double track" with no space, CR LF CR LF last). The text is in kernel.asm's
; hole before $55DB (choice_txt). DSKFMT itself is still the stub below.
choice:
                ld      hl, choice_txt
                ret

; dskfmt — format disk: the CF-3300's four choices (D-DSKFMT, kernel.asm dskfmt_body).
; (dskfmt -- a `jp dskfmt_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)

; mtoff — turn off disk motor(s).
; Clear the National control latch: deselect the drive and drop the motor line.
; MSX2 TH: MTOFF has no error-return convention; the BIOS ignores the return state.
mtoff:
                xor     a
                ld      (FDC_CTRL), a
                ret

; bdos_entry — BDOS dispatcher (written into the SYSTEM sysvar by INIT).
; CP/M-compatible calling convention (MSX2 TH, MSX-DOS BDOS conventions):
;   in:  C = call number, DE = FCB pointer (for FCB calls)
;   out: A = result
; Every function goes through ONE table, bdos_tab (disk/kernel.asm); one not in it
; answers A = $FF. 💰 D-DSKFMT carve 19 (2026-11-10): the ten functions this entry and
; bdos_more tested one `cp` / `jp` at a time are rows of that table now (3 B a row
; against 5). The table scan hands the handler DE, C and the caller's HL; A and B
; are the scan's -- every handler loads them before use.
; The file state lives in the caller's FCB, as MSX-DOS keeps it (D-BDOSFCB stages A-C:
; FOPEN fills +12..31, SEQREAD / SEQWRITE keep CR / EX and the size there).
; See disk/PROVENANCE.md §BDOS interface.
bdos_entry:
                ld      a, c
                jp      bdos_more2          ; bdos_tab's scan (disk/kernel.asm)

; (SETDTA $1A is bdos_tab's row for bdos_setdta_body, disk/kernel.asm: DE -> BDOS_DTA
; and DOS_DTAPTR.)
; dta_init -- INIT: the DTA defaults to $0080 in BOTH cells: BDOS_DTA, which this
; ROM's sequential bodies read, and DOS_DTAPTR, which the kernel's random and block
; bodies read (they reseed BDOS_DTA from it). D-BDOSFCB, 2026-11-10: the $F37D BDOS
; reaches those bodies now, and DOS_DTAPTR was never set before a program's SETDTA
; ($FFFF on a cold boot here). The CF-3300's own default is $DC6E, a buffer in its
; disk work area (scratchpad/bdosfcb_run1.out era reading, TODO D-BDOSFCB); ours
; keeps the documented $0080. Here and not in INIT: INIT's region had no room.
dta_init:
                ld      hl, DTA_DEFAULT
                ld      (BDOS_DTA), hl
                ld      (DOS_DTAPTR), hl
                ret
; bdos_open ($0F) -- open the file named in the FCB (bdos_tab's row).
;   in:  DE = FCB pointer
;   out: A = $00 opened / $FF not found or I/O error (MSX2 TH, BDOS conventions)
bdos_open:
                ; D-BDOSFCB (2026-11-10): the FCB is filled as the kernel's FOPEN fills
                ; it -- +14..31, size, first cluster, the cluster cache, +24 / +25 --
                ; so the kernel's random and block bodies work on a file opened from
                ; BASIC, as on the CF-3300 (scratchpad/bdosfcb_run1.out). fopen_fill_body
                ; also seeds this ROM's sequential state (fat_open, BDOS_RECIDX,
                ; BDOS_BYTESLEFT, BDOS_SEQREC), all this body did; IX is clobbered.
                xor     a
                ld      (BDOS_WRMODE), a    ; an Open is a READ open; clear write state
                jp      fopen_fill_body

; bdos_seqread ($14) — read the next 128-byte record into the DTA (BDOS_DTA).
; The record stream is bounded by the true file size in BDOS_BYTESLEFT (seeded
; from FAT_FILESIZE by Open, decremented per record): once it reaches 0 every
; further read returns EOF, and the final partial record is delivered with only
; n = min(RECSIZE, BYTESLEFT) real bytes followed by RECSIZE-n zero bytes, the
; whole record returned with code $00; EOF ($01) comes on the NEXT read.
;   out: A = $00 record delivered / $01 end-of-file (MSX2 TH, BDOS conventions)
; Partial-record zero-fill provenance: ORACLE OBSERVATION of real MSX-DOS 1.03 on
; a 1500-byte file (probes/disk/disk_probe_bdos.py) — the last record is
; 92 real bytes + 36 bytes of $00 (confirmed by pre-filling the DTA with $FF: the
; tail still returns $00, so MSX-DOS actively zero-fills, not Ctrl-Z/stale data),
; code $00; the following read returns $01. CP/M FCB sequential-I/O record model
; (MSX2 TH, FCB sequential I/O) supplies the record framing.
; (bdos_seqread -- a `jp bdos_seqread_body` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
; (moved here from kernel.asm by the D-DSKFMT defrag)
; --- hkn_stamp: overwrite the located dir entry's 8.3 name, IN THIS ROM ------
; D-NAMESTAMP (disk/docs/spec-diskcode-eviction.md §6.6aw). This was
; `ld a,DISKOP_SEL_NAME_STAMP / ld ix,dirverb_op / call calbak` -- a call-back
; into MAIN that marshalled on to `sub.rom`'s dirverb tenant, so a NAME crossed
; main -> disk.rom -> main -> sub.rom. Steps 9/11/12 gave this ROM its own
; read_sector / write_sector / FAT_DBUF, so the stamp is thirteen instructions
; here and the round trip is gone.
; Sources: the dir-entry layout is FAT12's published one (the 8.3 name is the
; entry's first 11 bytes -- disk/docs/spec-diskbasic-verbs.md, and the same
; DIRENT_* constants disk/equates.inc already declares); read_sector /
; write_sector take DE = logical sector, HL = buffer (basic/fat-prim-body.inc).
; Clean-room: our own FAT12 engine throughout; no reference ROM byte is read.
;
;   in : FWR_DIRSEC / FWR_DIROFF -- where THIS ROM's fat_find found the OLD
;        entry (hk_name); DISK_FCB_NAME -- the NEW 8.3 name
;   out: CF = 0 stamped, CF = 1 a DSKIO failure (DISKOP_ERR carries the code)
; 🔴 THE BARE NAMES ARE RIGHT NOW, AND UNTIL 2026-10-19 THEY WOULD HAVE BEEN THE
; BUG. disk.rom declares FWR_DIRSEC/FWR_DIROFF itself (BDOS_DIRSEC / BDOS_DIROFF,
; disk/equates.inc) -- its own FAT's scratch, a DIFFERENT address from main's.
; While hk_name found the old entry with MAIN's fat_find, this read the aliased
; import main_FWR_DIRSEC / main_FWR_DIROFF, and the bare spelling would have
; assembled cleanly and written a valid sector at the WRONG OFFSET -- the class
; that cost step 12 a day (§6.6ao). hk_name's lookups are this ROM's own now, so
; the position is in this ROM's cells, and the import is gone from the ABI
; (tools/gen_resident_abi.py).
; 🔴 AND `FAT_DBUF`, NEVER `FSECTOR_BUF`: the sub-ROM body this replaces spells
; `FSECTOR_BUF`, which in THIS ROM is main's buffer at $E5C0 rather than ours at
; $E2A0. Copying it verbatim is the bug it would have been. (D-BUFMERGE made
; them one address on 2026-09-24; the neutral spelling stays right either way.)
hkn_stamp:
                ld      de,(FWR_DIRSEC)
                call    rd_dbuf
                ret     c
                ld      hl,FAT_DBUF
                ld      de,(FWR_DIROFF)
                add     hl,de               ; HL -> the entry in the buffer
                ex      de,hl               ; DE -> its 11-byte name field
                ld      hl,DISK_FCB_NAME    ; source = the NEW 8.3 name, typed case:
                call    cpy_up11            ; the entry is upper case (OPEN twice)
                ld      de,(FWR_DIRSEC)
                ld      hl,FAT_DBUF
                jp      write_sector        ; tail-call: its CF is our answer
; D-DSKFMT: one sector of the track -- sync, the ID mark ($F5 writes A1), C H R
; (set per track by fmt_track) N = 2, the CRC ($F7), gap 2, sync, the data mark,
; 512 x $E5, the CRC, gap 3. 14 runs; read by address (kernel.asm dskfmt_body).
fmt_sec:
                db      12, $00, 3, $F5, 1, $FE, 3, $00, 1, $02, 1, $F7, 22, $4E
                db      12, $00, 3, $F5, 1, $FB, 0, $40, 0, $40, 1, $F7, 80, $4E  ; data $40: the CF-3300's (D-FMTFILL)
; (moved here from kernel.asm by the D-DSKFMT defrag)
set_rr:
                ld      (ix+33), l
                ld      (ix+34), h
                ret
                ds      $4558 - $, $00      ; anchor the canonical address
                jp      k_4558              ; $4558: COMMAND.COM-load kernel veneer
; --- MSX-DOS-1 kernel FCLOSE-finaliser entry: $456F (M24 slice A;
; tier2-m24-fclose-multicluster-spec.md) --------------------------------------
; The RAM kernel implements the FCB write tier (FMAKE/WRSEQ/FCLOSE) itself and
; CALLs three page-1 canonical worker entries for it; pre-M24 all three were
; un-wired. This one ($456F, the FCLOSE finaliser) fell on a lone $00 pad byte
; that NOP-slid into bsr_have (the Sequential-Read record copier): with the
; ambient BDOS_BYTESLEFT left at 0 after a completed .COM load, bsr_have's
; n := min(RECSIZE, BYTESLEFT) computed 0, and its LDIR ran with BC=0 -- a
; 65536-byte block move that sprayed the whole RAM map, crashing every write-mode
; Close of a file needing real flush/FAT/dir work (BDOSX3 record 10 exposed
; it; earlier FCLOSE calls survived only by luck, hitting this fall-in with a
; nonzero ambient BYTESLEFT that made the same LDIR a harmless copy).
; Reproduces stock's finaliser contract with OUR already-proven bdos_close
; (BDOS_WRMODE-gated: cheap no-op for a read-close, flush + fat_dir_update for
; a dirty write-close) -- no stock algorithm decoded, only the call-target
; address pinned black-box (ret=$D88A dispatcher class, DE=IY=$DA40 kernel FCB
; pointer). bsr_have is NOT itself a canonical address (only $4558/$456F are);
; shifting it by 2 bytes here is safe (M15 SS7.3 free-fall convention).
; 📍 D-COPYALL (2026-11-06): MOVED HERE from the $5602..$5E10 fill, which COPY's
; every-match walk outgrew by 56 B; entered only by call / jp, so it can live
; in any hole.
; --- hk_ochk: OPEN's same-file check (D-OPENSAME, FOPEN_SEL_OCHK) ----------
; The CF-3300 refuses a second OPEN of one file with `File already open` (54)
; in every mode -- OUTPUT, INPUT, RANDOM, APPEND-then-INPUT -- and the refused
; OPEN leaves the first channel's data intact (scratchpad/opensame_before.out,
; oracle). Main asks before it claims the channel: DISKOP_STATUS non-zero back
; means "open elsewhere". The walk is hkk_open_check, the one KILL / NAME /
; COPY already use (Joost's rule 3: no private copy). A mount failure answers
; 0 -- say nothing -- so OPEN's own path meets and reports the same drive.
hk_ochk:
                ei                          ; D-FWDWIN (2026-11-12): a hook arrives through an
                                            ; inter-slot call, interrupts OFF -- as hk_fmake
                                            ; does, give the mount's reads their windows
                                            ; (scratchpad/FW5_stack.out: masked here)
                call    fat_mount           ; LOCAL, for hkk_open_check's reads
                ld      a,0                 ; (flags kept)
                jp      c,hdl_answer        ; no mount: nothing to say
                call    hkk_open_check      ; CF=1: an open file matches
                sbc     a,a                 ; $FF open / 0 not
                jp      hdl_answer          ; stores it, disarms, claims
                ds      $456F - $, $00      ; pad up to the pinned $456F FCLOSE-finaliser entry
k_456F:
                jp      bdos_close          ; $456F: BDOS $10 FCLOSE canonical entry (M24 slice A)
bsr_have:
                ; n = real bytes this record = min(RECSIZE, BYTESLEFT).
                ; BYTESLEFT is nonzero here; if the high word is set or low word
                ; >= RECSIZE then a full RECSIZE record; otherwise n = low byte.
                ; D-DISKREREAD (2026-11-26): the three high bytes OR'd through HL
                ; (3 B), and BC = n set before the source is computed -- hl_x128
                ; touches only HL -- so n needs no pop / push there (3 B).
                ld      hl, BDOS_BYTESLEFT + 3
                ld      a, (hl)
                dec     hl
                or      (hl)
                dec     hl
                or      (hl)
                jr      nz, bsr_full                ; >= 256 left -> full record
                dec     hl
                ld      a, (hl)                     ; < 256 bytes left
                cp      RECSIZE
                jr      c, bsr_partial              ; < 128 -> partial record
bsr_full:
                ld      a, RECSIZE                  ; full 128-byte record
bsr_partial:
                ; A = n (real bytes, 1..128). Compute source in SECTOR_BUF.
                push    af                          ; save n (for the pad and the decrement)
                ld      c, a
                ld      b, 0            ; BC = n real bytes
                ld      a, (BDOS_RECIDX)
                ld      h, b
                ld      l, a
                call    hl_x128            ; HL = RECIDX * 128
                ld      de, SECTOR_BUF
                add     hl, de          ; HL = source record in SECTOR_BUF
                ld      de, (BDOS_DTA)  ; settable DTA (BDOS $1A); default $0080
                call    P1_BLIT         ; copy n real bytes to the DTA -- D-P1DOS: with
                                        ; page 1 = RAM, so a page-1 DTA gets them too
                                        ; (an LDIR there wrote into this ROM)
                ; zero-fill the remaining RECSIZE - n bytes of the record (DE now
                ; points just past the real bytes in the DTA).
                pop     af              ; A = n
                push    af              ; keep n for the decrement
                neg
                add     a, RECSIZE      ; A = RECSIZE - n (pad count, 0..127)
                jr      z, bsr_nopad
                ld      b, a            ; B = pad byte count
bsr_padloop:
                push    bc              ; D-P1DOS: each pad byte through P1_BLIT too
                xor     a               ; a $00 source byte (oracle: MSX-DOS pads with $00)
                call    blit1           ; DE advances (D-DISKREREAD: shared, 9 B)
                pop     bc
                djnz    bsr_padloop
bsr_nopad:
                ; BYTESLEFT -= n (n in A on stack); 4-byte LE subtract.
                pop     af              ; A = n
                ld      hl, BDOS_BYTESLEFT
                ld      c, a
                ld      a, (hl)
                sub     c
                ld      (hl), a
                inc     hl
                ld      b, 3            ; propagate borrow through the upper 3 bytes
bsr_borrow:
                ld      a, (hl)
                sbc     a, 0
                ld      (hl), a
                inc     hl
                djnz    bsr_borrow
                ld      a, (BDOS_RECIDX)
                inc     a
                ld      (BDOS_RECIDX), a
                xor     a               ; A = $00 success
                ret
bsr_eof:
                ld      a, $01          ; end-of-file
                ret
; blit1 -- D-DISKREREAD (2026-11-26): ONE byte, A, to (DE) through P1_BLIT (page 1 =
; RAM), DE advanced. P1_DEST is dskio's bounce word, idle here. Clobbers A, BC, HL.
; dta_put and bsr_padloop each had it inline (net 6 B, one extra stack level).
blit1:
                ld      (P1_DEST), a
                ld      hl, P1_DEST
                ld      bc, 1
                jp      P1_BLIT

; bdos_rdblk ($27) is gone (D-BDOSFCB, 2026-11-10): the $F37D RDBLK is the kernel's
; k47b2_body -- RR-positioned, the zero-padded last record, RR written back -- on
; the FCB bdos_open now fills, as the CF-3300 answers it from BASIC
; (scratchpad/bdosfcb_run1.out). The boot loader's RR 0 / RS 1 / huge-count call is
; that same contract (k47b2_body's header). It re-primed from record 0 every call.
; rdblk_rs -- RDBLK_REQ := HL (the requested record count); RDBLK_RECSIZE := the
; FCB's (DE) record size at +14..15, 0 -> 128 (MSX-DOS block I/O). Shared with
; k47b2_body, which carried the same 29 B (D-WRZER's carve, 2026-11-09).
rdblk_rs:
                ld      (RDBLK_REQ), hl
                ld      hl, 14
                add     hl, de
                ld      a, (hl)
                inc     hl
                ld      h, (hl)
                ld      l, a                ; HL = the FCB's record size
                ld      a, h
                or      l
                jr      nz, rrs_st
                ld      hl, RECSIZE         ; 0 -> 128
rrs_st:
                ld      (RDBLK_RECSIZE), hl
                ret
; --- MSX-DOS-1 kernel FMAKE-worker entry: $461D (M24 slice B;
; tier2-m24-fclose-multicluster-spec.md) --------------------------------------
; The RAM kernel's BDOS $16 FMAKE handling CALLs this page-1 entry with the
; same convention our own bdos_create already expects (DE = FCB pointer,
; FCB+1 = 11-byte 8.3 name; pinned black-box, ret=$D88A dispatcher class,
; DE=IY=$DA40 kernel FCB pointer) -- so the body is a direct tail-call, no
; wrapper needed. Pre-M24 this fell on a byte inside rdb_byteloop's
; predecessor instruction (the M21b RDBLK loop), producing the harmless-but-
; wrong A=$01 exit (the "n=37" register oddity) instead of ever creating a
; file. rdb_recloop..rdb_eof (unchanged, M21b) are relocated below to make
; room -- position-free, reached only by label from bdos_rdblk's own
; `jp rdb_recloop_body` fall-through above.
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; 📍 D-RECBIG (2026-11-04): rnd_loc and fac_mag moved here from the $5602..$5E10 fill, which the 32-bit record outgrew.
; rnd_loc -- D-LOC: the row's random record +33..36 := GP_RECNO (32 bits; out: DE
; = row+33). 🏗️ D-RECBIG (2026-11-04): LOC is the ROW's random record, where the
; CF-3300's FCB keeps it (+33 = 2 after `GET#1,2`), read UNSIGNED by main's LOC
; through H_CHIN op 8 (din_lofloc) and by D-RECAUTO's bare form (hdg_next). hk_dget
; sets it to n - 1 before the I/O and to n after it succeeds -- the CF-3300's rule
; (PUT #1,1E9: Disk full, LOC 999999999; PUT 5 then GET 9: Input past end, LOC 8).
; The record's own k = n - 1 is therefore what frnd_off reads during the I/O.
; IX = the row. Clobbers BC, HL.
rnd_loc:
                push    ix
                pop     hl
                ld      bc, 33
                add     hl, bc
                push    hl
                ex      de, hl
                ld      hl, GP_RECNO
                ld      c, 4
                ldir
                pop     de                  ; DE = row+33 (CF = 0: add's)
                ret
; (seq_flush -- the buffered SEQWRITE records out before a random positioning -- is gone:
; D-SEQMIX, 2026-11-14, SEQWRITE writes through and nothing stays buffered.)
; seq_load -- 🔴 D-SEQMIX2 (2026-11-14): the writer's first cluster and byte count come
; from ITS FCB at every SEQWRITE (+26, +16..19 -- kept there each record by
; wrseq_wr_writeback), not from globals another file's FCLOSE / WRBLK rewrites (cr_copy1 /
; cr_copy2, wrblk_set_wrfirst). In: DE = the FCB (kept). A, F, BC, HL clobbered.
seq_load:
                push    de
                ld      hl, 26
                add     hl, de
                ld      a, (hl)
                inc     hl
                ld      h, (hl)
                ld      l, a
                ld      (BDOS_WRFIRST), hl
                ld      hl, 16              ; D-DISKREREAD (2026-11-26): DE is still the
                add     hl, de              ; FCB -- no pop / push, no BC: 2 B
                call    to_wrbytes          ; BDOS_WRBYTES := FCB +16..19
                pop     de
                ret
; bb_dskrst -- $F37D's DSKRST ($0D): drive A, nothing else (the MSX-DOS one marks
; MSX-DOS booted, D-BOOTDATE). (Moved here from in front of $543C by D-F37DCONIN.)
bb_dskrst:
                xor     a
                ld      (CURDRV_CELL), a    ; drive A
                ret
                ds      $461D - $, $00      ; pad up to the pinned $461D FMAKE-worker entry
k_461D:
                jp      bdos_create_body         ; $461D: BDOS $16 FMAKE canonical entry (M24 slice B)

; rdblk_getbyte — deliver the next byte of the open file in A, refilling
; SECTOR_BUF from the cluster chain when exhausted and decrementing BDOS_BYTESLEFT.
;   out: Cy = 0 ok (A = byte) / Cy = 1 chain ended (caller guards with BYTESLEFT).
; Uses RDBLK_BUFPOS (0..512). Clobbers A + flags only (works through HL/DE/C via the
; helper, which the caller does not rely on across the call).
; D-DISKREREAD (2026-11-26): BUFPOS < 512 is H < 2 (unsigned), and HL stays the
; position on both arms (the refill's 0 goes straight into the `inc`/store, not
; through the cell) -- 9 B.
rdblk_getbyte:
                ld      hl, (RDBLK_BUFPOS)
                ld      a, h
                cp      2
                jr      c, rgb_have         ; BUFPOS < 512 -> a byte is buffered
                call    fat_read_file_sector ; refill SECTOR_BUF with the next sector
                ret     c                   ; chain ended -> Cy = 1
                ld      hl, 0
rgb_have:
                inc     hl
                ld      (RDBLK_BUFPOS), hl  ; advance for next time
                dec     hl                  ; HL = the byte's index
                ld      de, SECTOR_BUF
                add     hl, de
                ld      c, (hl)             ; C = the file byte
                ; BDOS_BYTESLEFT -= 1 (4-byte LE): the borrow rides the carry (DEC E
                ; leaves it alone). Written out it was 19 B, the loop is 11 (D-DSKFMT's
                ; carve, 2026-11-10); every caller saves or reloads DE, which now ends
                ; with E = 0.
                ld      hl, BDOS_BYTESLEFT
                ld      e, 4
                scf
rgb_dec:
                ld      a, (hl)
                sbc     a, 0
                ld      (hl), a
                inc     hl
                dec     e
                jr      nz, rgb_dec
                ld      a, c                ; A = the byte
                or      a                   ; Cy = 0 success
                ret

; bdos_close ($10) — close the file.
;   out: A = $00 ok / $FF error (MSX2 TH, BDOS conventions)
; For a READ-opened file there is no dirty state -> success (the historical
; behaviour). For a WRITE-opened file (BDOS_WRMODE = 1) this is where the file
; is made durable: flush the partial final 512-byte sector, persist the cluster
; chain's end-of-chain marker (already linked as each cluster was allocated), and
; rewrite the directory entry with the true byte count (DIRENT_FILESIZE) and the
; first cluster (DIRENT_FIRSTCLUS). On any I/O failure return $FF.
; 🔴 D-BDOSUNOP (2026-11-11): write-open is BDOS_WRMODE = 1 EXACTLY (FMAKE stores 1,
; FOPEN 0). Nothing clears the cell at power-on, so after a cold boot it holds RAM's
; $FF, and `or a / jp nz` sent the FIRST FCLOSE through $F37D to the write-mode close,
; which wrote a directory entry from garbage globals (cluster $FFFF, size 0, over
; AUTOEXEC.BAS -- probes/disk/bdosunop_acceptance.py). A clear at INIT fixed that too
; but moved an unrelated masked FAT read in BASIC (programs-acceptance L5_house: a
; 0.5 s interrupt gap in DSKF), so the test is narrowed here instead, byte for byte.
bdos_close:
                ld      a, (BDOS_WRMODE)
                dec     a
                jp      z, bdos_close_wm    ; D-WRBLKDIR: disk/runtime.asm, for room
bdos_close_rd:
                jp      close_read      ; read close: nothing to flush, but a WRITTEN
                                        ; file is re-dated (D-CLOSESTAMP)
; (bdos_close_write -- the write-mode close from the globals -- is gone: D-SEQMIX,
; 2026-11-14; every FCLOSE closes from its FCB, kernel.asm bdos_close_wm.)

