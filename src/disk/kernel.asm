; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; Tier-2 MSX-DOS-1 kernel veneers + relocated bodies (Interface A: the de-facto DOS ABI)
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- MSX-DOS-1 kernel continuation entry: $50A9 ($4000 + $10A9; a3 §8.26/§8.27) -
; The relocated kernel CALLs $50A9 right after publishing its BDOS vector; on the
; genuine CF-3300 this disk-ROM routine (`CALL $472D` + work-area setup) reaches A>.
; Our code ends far below $50A9, so the entry is positioned with a `ds` fill (like
; $4030). We reproduce the BLACK-BOX contract (disk_probe_dosboot_50a9.py):
;   side effect : W50A9_WRKB ($F242) := $00   (the only non-stack write observed)
;   returns     : A=$00, F=$42 (Z+N) ; DE=IX=$F1AA ; HL=$F359 ; BC, IY preserved
; `sub a` yields exactly A=$00 / F=$42 ($42 = Z|N: 0-0 sets Z+N, clears S/H/PV/C); the
; following loads do not disturb the flags, so the exit AF is exact. Inputs ignored
; (the kernel passes AF=C340 BC=0000 DE=DC80 HL=D606 IX=F195 IY=C0AB; none consumed).
; 🐌 D-RECSLOW (2026-11-05): frnd_locate's chain EXTENSION, with the FAT sector HELD
; in FAT_MBUF -- every mark and link a deferred patch (FAT_DEFER bit 0), the sector
; written to both FAT copies only when the scan or a link needs another one, and
; once at the end (frl_xdone, the $4FB8 fill). It was a read + two writes per mark
; and per link, ~6 FDC commands a cluster: `PUT #1,32767` at LEN=1 issued 451
; where the CF-3300 issues none before CLOSE (scratchpad/recslow_fdc.out). No state
; outlives the call (frl_flush on every exit), so no other FAT_MBUF user sees a
; dirty buffer. No frnd_next past the first new cluster: a fresh cluster's
; successor is the chain's end, and a read would overwrite the held sector.
; in: GP_CLUS = the chain's last cluster, GP_CLIDX = clusters still to add - 1.
frl_ext:
                call    frl_defer
                call    fat_alloc_cluster   ; HL = the new cluster, its EOC deferred
                jp      c, frl_xfail
                push    hl
                ld      de, (GP_CLUS)
                ex      de, hl              ; HL = the last cluster, DE = the new one
                call    frl_defer
                call    fat_write_fat_entry_hot ; link it (the scan left FAT_MBUF/FATSEC)
                pop     hl
                jp      c, frl_xfail
                ld      (GP_CLUS), hl
                ld      hl, (GP_CLIDX)
                ld      a, h
                or      l
                dec     hl                  ; (`dec` / `ld` keep Z)
                ld      (GP_CLIDX), hl
                jr      nz, frl_ext
                jp      frl_xdone
; install_br6 -- install_dos_env's last piece (D-CALSLTP0): br6, the page-1/2 CALSLT,
; LDIR'd on from where br5 ended (DE runs on). Here for room.
install_br6:
                ld      hl, br6_tmpl
                ld      bc, br6_end - br6_tmpl
                ldir
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
dskchg_body:
                push    hl
                push    de
                ld      hl, (JIFFY)
                ld      de, (DSK_STAMP)
                or      a
                sbc     hl, de              ; HL = jiffies since the last sector command
                ld      de, -100
                add     hl, de              ; Cy = 1: 2 s or more
                pop     de
                pop     hl
                jp      c, dc_unk           ; "unknown": the DPB refreshed, B = 0
                ld      b, 1                ; "unchanged"
                xor     a                   ; A = 0, Cy = 0
                ret
; (moved here from runtime.asm by the D-DSKFMT defrag)
; --- conin_body - the $5445 CONIN canonical entry (M22a) --------------------------
; Blocking read: CHGET ($009F), then echo via conout_body (E=char, its ABI) —
; the published BDOS $01 CONIN contract (read-with-echo). BC/DE/HL preserved;
; A holds the char (bb_conout's A:=E return; the caller's E is popped back).
; CLEAN-ROOM: published CONIN contract (map.grauw.nl) + our own CHGET/conout_body
; primitives; no oracle bytes.
; 💰 D-DSKFMT carve 15: the read is innoe_body's (the same CHGET with the same paging).
conin_body:
                push    de                  ; D-DISKREREAD (2026-11-26): DE alone -- innoe_body
                                            ; keeps BC/DE/HL and bb_conout BC/HL; 4 B
                call    innoe_body          ; A := char, BIOS paged back out
                ld      e, a                ; echo via conout_body (E = char, its ABI) --
                call    bb_conout           ; through bb_conout: A := E after it. conout_body's
                                            ; TAB answers A 0 (the DOS CONOUT's, measured); CONIN
                                            ; returns the key, $09 too (D-F37DCONIN, CONIN.md R-BC-CTRL)
                pop     de
                ret
; bb_conout -- $F37D's CONOUT ($02; bb_dirio's output half): the references leave the
; character in A. (Moved here from in front of $543C by D-F37DCONIN.)
bb_conout:
                call    conout_body         ; E = the character (AF kept)
                ld      a, e                ; the references leave it in A
                ret
                ds      $4E4B - $, $00  ; pad to the $4E4B kernel veneer (was $50A9 fill)
                jp      k_4E4B          ; $4E4B: COMMAND.COM-load kernel veneer
; 📌 In the $4E4E..$4EDE fill (check_disk_walls, 2026-10-21).
; --- D-BDOSBASIC slice 2: $F37D from a BASIC program (oracle; MSX2 TH CALSLT) ---
; From a BASIC program page 1 is BASIC's own ROM, so a bare `JP bdos_entry` ran into
; BASIC (the A = 2 of scratchpad/bdos_sfirst.out). $F37D now jumps to this page-3
; stub. Page 0 is the BIOS (slot 0, the MSX standard) whenever BASIC runs: then
; the stub calls bdos_entry through the BIOS's CALSLT ($001C) with this ROM's slot
; (HOOK_SLOT), the caller's IX / IY kept, interrupts back on. Page 0 is NOT the
; BIOS in a disk's boot code (RAM there) and under MSX-DOS -- and there page 1 is
; already this ROM, so the stub jumps straight in, as $F37D always did.
f37d_tmpl:
                in      a, ($A8)
                and     3                   ; page 0's primary slot
                jp      nz, BR_BDOS         ; not the BIOS: the page-3 BDOS bridge (it pages
                                            ; this ROM into page 1 and back; D-BOOTREGS).
                                            ; 🔴 D-SDSBOOT (2026-11-07): this was `jp $0005`,
                                            ; and a boot code on all-RAM writes its OWN page-0
                                            ; RAM -- SDSNAT1C copies the BIOS's $0006 / $0007
                                            ; (the VDP ports, $98 $98) there, so $0005 read
                                            ; `jp $9898` and every BDOS call after it went
                                            ; into the weeds (scratchpad/sdsboot_0005_zb.out).
                                            ; The CF-3300's $F37D does not go through $0005
                                            ; either. Under DOS page 1 is the TPA's RAM
                                            ; (D-TPAPTAIL): the bridge restores it after.
                push    ix
                push    iy
f37d_iy:        ld      iy, 0               ; high byte := HOOK_SLOT (install_f37d)
                ld      ix, bdos_entry
                call    $001C               ; CALSLT: AF / BC / DE / HL both ways
                pop     iy
                pop     ix
                ei
                ret
f37d_end:
install_f37d:
                ld      hl, f37d_tmpl
                ld      de, F37D_STUB
                ld      bc, f37d_end - f37d_tmpl
                ldir
                ld      a, (HOOK_SLOT)
                ld      (F37D_STUB + f37d_iy - f37d_tmpl + 3), a
                jp      install_p1blit  ; D-P1RDBLK: dskio's page-1 bounce, every host
; 💰 D-DISKCARVE (2026-11-07): three BDOS bodies moved here from disk/runtime.asm's
; $782B..$7F7C region (15 B), which the int_h_hiram template's ENASLT fix needed
; (D-SDSBOOT). Entered only by `jp` from the BDOS dispatch; this pad was empty and
; never entered (scratchpad/padwatch_4ede.out, the ten DOS suites).
; (cpmver_body moved to the hole in front of $607B by D-CALSLTP0, whose install
; needed this region's bytes.)

; --- login_body - the $504E LOGIN canonical entry (M22a) -------------------------
; A := the online-drive bitmap (1 << DRVCNT) - 1, the published LOGIN contract
; (map.grauw.nl), derived from our OWN DRVCNT cell ($F347, =2 here) rather than
; a hardcoded constant. B := 0. Only B (the loop counter, no longer needed once
; the shift is done) is clobbered besides A/HL — C/D/E (§5.3) are never touched.
; (stime_body moved to the hole in front of $75A5 by D-F37DSETDATE: it checks the time now)

; --- verify_body - the $55FF VERIFY canonical entry (M22a) -----------------------
; A := E (the published VERIFY contract: echo the requested on/off flag); B
; passes through untouched (§5.3). NO-OP SETTER BY DESIGN, and this is the
; FAITHFUL choice — RESOLVED 2026-07-04 (remaining-spec §5.4, was open): stock
; MSX-DOS 1 has NO verify-after-write effect (black-box: a VERIFY-on WRABS issues
; exactly the same single $4010 DSKIO write as a VERIFY-off WRABS, zero read-back —
; probes/disk/verifyx.asm). So neither persisting E nor coupling a verify-read into
; the write path is needed; doing so would make ours LESS faithful (extra reads
; stock never does) and is pointless on the emulator target (writes cannot fail).
; D-DSKFMT: the boot sector a format writes -- the BPB (each choice's fields set by
; dskfmt_body), then OUR loader at $C01E: with Cy = 0 nothing to set up; with Cy = 1
; it loads MSXDOS.SYS at $0100 through the BDOS at $F37D (FOPEN, SETDTA, RDBLK of up
; to $3F01 bytes at a record size of 1) and jumps there, IX (the DPB pointer the disk
; ROM hands MSXDOS.SYS) kept; without MSXDOS.SYS it returns and the disk ROM goes on
; to BASIC. Written for this format; nothing of the CF-3300's boot code.
fmt_boot:
                db      $EB, $FE, $90, "MSX_01  "
                dw      512
                db      0                   ; sectors / cluster (set)
                dw      1                   ; reserved
                db      2                   ; FATs
                dw      0, 0                ; root entries, total sectors (set)
                db      0                   ; the media ID (set)
                dw      0, 9, 0, 0          ; sectors / FAT (set), / track, sides (set), hidden
                ret     nc
                push    ix
                ld      de, BOOT_LOAD + fmt_bfcb - fmt_boot
                ld      c, $0F
                call    $F37D               ; FOPEN
                inc     a
                jr      z, fmt_bnone
                ld      de, $0100
                ld      c, $1A
                call    $F37D               ; SETDTA
                ld      hl, 1
                ld      (BOOT_LOAD + fmt_bfcb - fmt_boot + 14), hl
                ld      h, $3F              ; HL = $3F01 records of 1 byte
                ld      de, BOOT_LOAD + fmt_bfcb - fmt_boot
                ld      c, $27
                call    $F37D               ; RDBLK
                pop     ix
                jp      $0100
fmt_bnone:
                pop     ix
                ret
fmt_bfcb:
                db      0, "MSXDOS  SYS"
FMT_BOOTLEN     equ     $ - fmt_boot
; rh_init -- INIT's half of D-PUTCACHE (disk/init.asm's init_p2 calls it): no sector
; held. Keeps A (INIT's 0 is still needed after the call). Here, AFTER FMT_BOOTLEN (not
; part of the boot template), since D-DISKROMHOST took its bytes in front of $75A5 for
; hrunc_h. (Before that: in front of $543C until D-F37DSETDATE, and disk/fat.asm's $47C1
; fill until D-WRZER.)
rh_init:
                ld      hl, 0
                ld      (RND_HSEC), hl
                ret
                ds      $4EDE - $, $00  ; pad to the $4EDE kernel veneer
                jp      k_4EDE          ; $4EDE: COMMAND.COM-load kernel veneer
; 📌 In the $4EE1..$4FB8 fill (check_disk_walls: the largest hole, 2026-10-21).
; --- D-BDOSBASIC slice 1: more BDOS functions for $F37D (oracle; MSX2 TH BDOS table)
; A boot loader running in the boot code's environment (page 1 = this ROM) calls
; these too: ROBOCOP calls $09, $1A, $11, $0F, $27 and runs on the CF-3300
; (scratchpad/bootpath_robocop_bdos.out). SFIRST / SNEXT are the kernel's own root
; walk, which fills the DTA through the work-area cell $F23D -- the cell the CF-3300's
; own SETDTA writes from BASIC (scratchpad/bdos_dtacell.out: $D100), so SETDTA sets
; it too. STROUT is the resident '$'-string printer. Anything else: A = $FF, as before.
; (bdos_more's three tests are bdos_tab rows since D-DSKFMT carve 19.)
bdos_setdta_body:
                ld      (BDOS_DTA), de
                ld      (DOS_DTAPTR), de
                ret
; --- D-MEDIAID: a disk's geometry comes from its MEDIA ID, not its BPB (oracle) ---
; Both references (the CF-3300; the VG-8020 + NMS 1200) read a disk whose boot
; sector BPB is damaged or ALL ZERO, and lose one whose FAT starts with another
; media ID than the layout's (scratchpad/bpbrule_probe.py). The geometry is the
; standard format of the media ID in the FAT's first byte -- logical sector 1,
; the format's single reserved sector before it -- and the BPB is not read.
; fat_geom_read -- fat_mount's read: logical sector 1 into FAT_DBUF and, when its
; first byte is a media ID, that format's BPB fields over it; otherwise the boot
; sector, BPB and all. Out: CF = 1 on a read error.
fat_geom_read:
                ld      a, (DK_FCB_DRV)     ; D-DSKIB: the drive this operation named
                call    dsel_bdos           ; -> the swap prompt when it is the other one
                                            ; (D-DOSDRVB: under MSX-DOS too)
                ld      de, 1               ; logical sector 1: the FAT, media ID first
                call    rd_dbuf
                ret     c
                call    fat_media
                ld      a, (FAT_DBUF)       ; D-SIDES: DSKIO seats by this media ID
                jr      nc, fgr_media
                ld      a, $F9              ; no media ID: the old 2 sides x 9 seating
                ld      (DSK_MEDIA), a
                ld      de, 0               ; boot sector = logical sector 0
; rd_dbuf -- read logical sector DE into FAT_DBUF. 💰 D-DSKFMT carve 24 (2026-11-10):
; `ld hl,FAT_DBUF / call read_sector` stood at seven sites; this tail is the one.
rd_dbuf:
                jp      rs_dbuf
; wr_sbuf -- write SECTOR_BUF to logical sector DE (carve 24: four sites).
wr_sbuf:
                ld      hl, SECTOR_BUF
                jp      write_sector
fgr_media:
                ld      (DSK_MEDIA), a      ; CF = 0: reached by `jr nc` alone, and fat_mount
                ret                         ; tests CF only (D-DISKREREAD 2026-11-26: no `or a`, 1 B)
; fat_media -- in: FAT_DBUF = logical sector 1. A media ID $F8..$FF there: its
; format's fields written over FAT_DBUF + 11 .. + 23 in the BPB's own layout (so
; fat_mount and getdpb read them as before), CF = 0. Otherwise CF = 1 and nothing
; written (own choice: no media ID, so the boot sector's BPB is the only clue).
; Trashes AF, BC, DE, HL.
fat_media:
                ld      a, (FAT_DBUF)
                sub     $F8
                ret     c                   ; not a media ID
                ld      c, a
                add     a, a
                add     a, a
                add     a, c                ; A = 5 * index
                ld      e, a
                ld      d, 0
                ld      hl, fmd_tab
                add     hl, de
                ex      de, hl              ; DE = the format's entry
                ld      hl, FAT_DBUF + 11
                ld      (hl), 0
                inc     hl
                ld      (hl), 2             ; +11 bytes per sector, $0200
                inc     hl
                call    fmd_copy            ; +13 sectors per cluster
                ld      (hl), 1
                inc     hl
                ld      (hl), 0             ; +14 reserved sectors, 1
                inc     hl
                ld      (hl), 2             ; +16 FATs, 2
                inc     hl
                call    fmd_copy0           ; +17 root entries
                call    fmd_copy
                call    fmd_copy            ; +19 total sectors
                ld      a, c
                add     a, $F8
                ld      (hl), a             ; +21 the media ID
                inc     hl
                ; --- intentional fall-through: +22 sectors per FAT ---
fmd_copy0:
                call    fmd_copy            ; one byte from the entry, a 0 above it
                ld      (hl), 0
                inc     hl
                or      a                   ; CF = 0
                ret
fmd_copy:
                ld      a, (de)
                ld      (hl), a
                inc     de
                inc     hl
                ret
; The standard formats: sectors per cluster, root entries, total sectors (LE),
; sectors per FAT. MEASURED on both references with the BPB all zero: where the
; root is (sectors per FAT) for all eight; where the data starts (root entries)
; for $F8, $F9, $FD and $FE. $FA / $FB / $FC / $FF's data start stayed unmeasured:
; openMSX did not seat those images' tracks where the format puts them.
fmd_tab:        db      2, 112, low 720,  high 720,  2  ; $F8 1 side, 80 tracks, 9 sectors
                db      2, 112, low 1440, high 1440, 3  ; $F9 2 sides, 80 tracks, 9 sectors
                db      2, 112, low 640,  high 640,  1  ; $FA 1 side, 80 tracks, 8 sectors
                db      2, 112, low 1280, high 1280, 2  ; $FB 2 sides, 80 tracks, 8 sectors
                db      1, 64,  low 360,  high 360,  2  ; $FC 1 side, 40 tracks, 9 sectors
                db      2, 112, low 720,  high 720,  2  ; $FD 2 sides, 40 tracks, 9 sectors
                db      1, 64,  low 320,  high 320,  1  ; $FE 1 side, 40 tracks, 8 sectors
                db      2, 112, low 640,  high 640,  1  ; $FF 2 sides, 40 tracks, 8 sectors
; --- MSX-DOS-1 kernel SFIRST/SNEXT dir-search entries: $4FB8/$5006 (M19) -------
; While processing BDOS SFIRST ($11) / SNEXT ($12) the relocated kernel CALLs these
; two page-1 disk-ROM entries to walk the root directory for entries matching the
; search FCB's 8.3 pattern. Both are our own $00 pad (inside the $4EE1..$50A9 fill,
; below the $50A9/$50C4/$50D5/$50E0 veneers), so before this the CALL NOP-slid into
; the $50A9 stub tail and returned a bogus "entry found" -> DIR hung (M19 §3). Each
; is inserted as an additional descending ds-anchor in this chain ($4FB8, $5006 slot
; in address order between $4EDE and $50A9), consuming existing pad -> net-zero.
; Black-box entry contract (M19 §2, callseq/capture, no stock CODE decoded): entry
; DE -> the kernel's search-FCB copy (drive byte + 11-byte 8.3 pattern, ?=$3F
; wildcard); the runtime DTA base lives in the fixed work-area cell $F23D (the
; kernel's current-DTA pointer, set via its own SETDTA before the search). Bodies
; (sfirst_body/snext_body/dirscan_match) live in the free tail; clean-room -- our
; own fat_mount/fat_find root-dir walk + published SFIRST/SNEXT contract. See
; docs/tier2-m19-spec.md.
; 🐌 D-RECSLOW: frl_ext's exits and helpers (it is in the $4E20 fill above).
frl_xdone:
                call    frl_flush
                ret     c
                jp      frl_done
frl_xfail:
                push    af                  ; the failure (CF, the code pending)
                call    frl_flush           ; what was claimed and linked reaches the
                pop     af                  ; disk, as before (a full disk fills first)
                ret
frl_defer:                                  ; FAT_DEFER bit 0; A only -- frl_ext
                ld      a, (FAT_DEFER)      ; holds the link's clusters in HL / DE
                or      1                   ; across it (an `ld hl` here sent the
                ld      (FAT_DEFER), a      ; link to a garbage cluster: the host
                ret                         ; rig scratchpad/recslow_host.py)
frl_flush:
                ld      a, (FAT_DEFER)
                and     2
                jp      nz, fat_write_buf_allfats   ; dirty: out (it clears the flag)
                ld      (FAT_DEFER), a      ; clean: drop a request (A = 0, CF = 0)
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; wrseq_writeback — M33 (tier2-m33-m32-fcb-position-spec.md §2.2): mirror the
; sequential-read iterator's position into the FCB in IX (the fixed base $DA40
; until D-BDOSFCB stage B, 2026-11-10; IX = DE at entry, which is $DA40 under
; MSX-DOS and the caller's own FCB from $F37D) after a DELIVERED RDSEQ record (wrseq_body's read branch calls
; this only when A=$00; see wrseq_body above). K = BDOS_SEQREC after this
; call's increment (records delivered so far, 1-based).
;   copy+32 (CR)          := K mod 128
;   copy+12 (EX)          := K div 128         (floppy -> fits a byte, §2.2)
;   copy+28/29 (word)     := FAT_CURCLUS       (cluster of the just-read record)
;   copy+30               := (K-1) div recPerClus, recPerClus = FAT_SECPERCLUS*RECPERSEC
; Does not touch copy+14/15/16/17/24/25/26/27/31 (already correct/out of scope,
; §2.2/§1). Leaf routine: preserves A (the caller's bdos_seqread exit code)
; across the whole body; trashes BC/DE/HL/IX (dead in wrseq_body at the call
; site -- bdos_seqread's own exit values are only A, per the dispatcher's
; H:=B,L:=A mirror rule already noted above). PLACEMENT: this position-free
; corridor (same class as fdc_entloop_body/p0_env_tab/ff_secloop above --
; 0 canonical entries, no FDC window, ample slack), not kernel.asm's own tail
; (which is already tight against the $7F80 FDC-window guard).
wrseq_writeback:
                push    af                  ; preserve bdos_seqread's exit A
                ; K := BDOS_SEQREC + 1
                call    seq_k_adv           ; HL = K (+32 CR, +12 EX set; D-DSKFMT defrag)
                push    hl
                ; copy+28/29 := FAT_CURCLUS (word)
                ld      hl, (FAT_CURCLUS)
                ld      (ix+28), l
                ld      (ix+29), h
                ; copy+30 := (K-1) div recPerClus, recPerClus = FAT_SECPERCLUS*RECPERSEC
                pop     hl                  ; HL = K
                call    seq_k30             ; +30 := (K-1)'s cluster index
                pop     af                  ; restore bdos_seqread's exit A
                ret
                ds      $4FB8 - $, $00  ; pad up to the pinned $4FB8 SFIRST entry
sfirst:
                jp      sfirst_body     ; -> free-tail: find FIRST matching dir entry (M19)
; br5 (D-BOOTREGS) -- br2's shared tail, moved here when br_callf grew: br_go /
; br_frame put the BIOS into page 0, then [target][frame][caller's $A8][caller's
; return], every register the caller's. BR_TGT / BR_FRM / BR_HL / BR_CFOP are read
; before the target runs; the BIOS's CALLF returns through BR_CFOP + 3.
br5_tmpl:
br_go_tmpl:
                ld      (BR_TGT), hl
                ld      hl, BR_BACK
br_frame_tmpl:                              ; HL = the frame; stack [HL][return]
                di
                ld      (BR_FRM), hl
                pop     hl
                push    af
                push    bc
                push    de
                push    hl
                call    BR_MAININ
                pop     hl
                pop     de
                pop     bc
                pop     af
                ld      (BR_HL), hl
                ld      hl, (PG_SV_A8 - 1)  ; H = the caller's $A8
                push    hl
                ld      hl, (BR_FRM)
                push    hl
                ld      hl, (BR_TGT)
                push    hl
                ld      hl, (BR_HL)
                ret                         ; -> the BIOS entry
br_tgt_tmpl:    dw      0
br_frm_tmpl:    dw      0
br_hl_tmpl:     dw      0
br_cfop_tmpl:   db      0, 0, 0
                jp      BR_BACK
br5_end:
; bd_step7 / bd_nosig (D-STEP7DATE, 2026-10-23) -- a disk in the drive that did not start
; MSX-DOS: its boot code RETURNED from step 7 (a data disk's `RET NC` / `RET` stub), or
; it carries no boot signature at all. Both references then ask the date before BASIC
; (the CF-3300 `Enter date ( - - ):`, the VG-8020 + NMS 1200 `Enter date (M-D-Y):`)
; with our own make_test_dsk.py disk (scratchpad/stepret_plain_cf.out,
; scratchpad/stepret_plain_nms.out), and the CF-3300 for an all-zero image too.
; boot_date is D-BOOTDATE's question (it skips a disk with an AUTOEXEC.BAS, as both
; references do) and returns with interrupts masked.
bd_step7:
                call    boot_date
                ei                      ; as the step-7 return always did
                ret                     ; -> the BIOS boot scan -> BASIC
; (bd_nosig -- a `jp boot_date` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
; (moved here from runtime.asm by the D-DSKFMT defrag, 2026-11-09)
; bdos_close_wm -- D-WRBLKDIR (2026-11-08): BDOS_WRMODE is ONE global (FMAKE sets
; it), so with two FMAKE'd FCBs written by WRBLK the first FCLOSE came here and
; wrote the entry from the GLOBALS -- the last-used file's -- never its own
; (wrblkalt-acceptance's ALT1). The globals path is for the SEQUENTIAL writer's
; file only: nothing written sequentially (no cluster yet) -> close from the FCB.
; 🔴 D-SEQMIX (2026-11-14): AND THE SEQUENTIAL WRITER'S FILE NOW CLOSES THE SAME WAY. Its
; globals path wrote the entry at FWR_DIRSEC / FWR_DIROFF, which any other file's
; fat_find had moved -- FCLOSE A wrote A's chain into B.DAT's entry and left A empty
; (scratchpad/seqmix_run3.out). Nothing is buffered any more (SEQWRITE writes through),
; so every FCLOSE closes from its own FCB: +26 / +16..19 at +25 (close_read, cr_dir).
bdos_close_wm:
                jp      bcw_own             ; (the ownership test is in front of $5FE5)
; wrblk_mul_rr_rs -- MOVED here by D-DISKCARVE6 (2026-11-17); its contract is at wmr_core.
wrblk_mul_rr_rs:
                call    rh_flush            ; D-PUTCACHE: WRBLK_MUL* are WBUF+500..511,
                call    wmr_core
                ld      (WRBLK_MULACC), hl
                ld      (WRBLK_MULACC + 2), de
                ret
                ds      $5006 - $, $00  ; pad up to the pinned $5006 SNEXT entry
snext:
                jp      snext_body      ; -> free-tail: find NEXT matching dir entry (M19)
; --- MSX-DOS-1 kernel FSIZE entry: $501E (M28 §3.1/§3.2, [LANDED-B]) ----------
; While processing BDOS FSIZE ($23) the relocated kernel CALLs this page-1 disk-
; ROM entry (same dispatcher class as $4788/$4793, M26: DE=$DA40 kernel FCB
; copy, A=$25/B=$00/C=$00, ret=$D88A). Sits in the dead-$00 pad between snext
; ($5006, ends $5008) and $504E LOGIN -- a 3-byte `jp fsize_body` wires directly,
; RDABS/M26-style, no relocation needed. Body = fsize_body (free tail): a real
; GET FILE SIZE (fat_mount + fat_find, ceil(size/128) -> copy+33..35); the prior
; stub returned a constant A=L=$03 for every input and never searched at all.
; 📍 D-COPYALL (2026-11-06): MOVED HERE from the $5602..$5E10 fill, which COPY's
; every-match walk outgrew by 56 B; entered only by call / jp, so it can live
; in any hole.
; hkc_body's result tails (A = hk_copy's status).
hkc_io:         ld      a,2                 ; mount / full / I-O -> load_error
                ret
hkc_ill:        ld      a,3                 ; refused -> ERR 5
                ret
hkc_nf:         xor     a                   ; 0 = source not found -> ERR 53
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; 🔴 D-DISKFULL (2026-10-02): THE FIRST FAILURE ENDS THE SAVE. sv_load_error used
; to set the status and `ret` -- to whoever had `jp`'d into it, i.e. back INTO
; sav_disk_write's loop, which wrote on. After a failed create the write state was
; never initialised (BDOS_WRBUFLEN read $FFFF): the old putbyte stored one byte at
; SECTOR_BUF+$FFFF and wrapped, and the flush-first putbyte retried a doomed flush
; on EVERY byte -- a write-protected SAVE took long enough that wprotect-acceptance
; captured before it answered (scratchpad/dfwp_trace.py). Resident, sv_load_error
; is disk_error and raises; here it now unwinds to hk_dpsave the same way.
hkds_run:
                ld      (DPSAVE_SP),sp      ; = the return address into hk_dpsave
                ; hk_dpsave loads HL = sav_disk_write, the BSAVE entry HL = bsv_open
                ; z80flow: targets sav_disk_write bsv_open
                jp      (hl)                ; sav_disk_write, or BSAVE's bsv_open
; fmt_buf -- HL := IX + FMT_BUF (MOVED here by D-DISKCARVE6, 2026-11-17).
fmt_buf:
                push    ix
                pop     hl
                ld      bc, FMT_BUF
                add     hl, bc
                ret
                ds      $501E - $, $00  ; pad up to the pinned $501E FSIZE entry (M28)
k_501E:
                jp      fsize_body      ; $501E: BDOS $23 FSIZE canonical entry (M28)
; --- MSX-DOS-1 kernel LOGIN entry: $504E (M22a; tier2-m22-cpmver-spec.md) ------
; While processing BDOS LOGIN ($18) the relocated kernel CALLs this page-1 disk-
; ROM entry for the online-drive bitmap. Was un-wired $00 pad, NOP-sliding into
; the $5058 SETDTA-cache veneer below: garbage exit AND a silent stomp of
; DOS_DTAPTR from whatever DE happened to hold (benign-by-luck only because DE
; was $0080 at the observed call site, §4.3). Body = login_body (free tail).
; CLEAN-ROOM: published LOGIN online-drive-bitmap contract (map.grauw.nl) +
; our own DRVCNT cell; no stock routine internals decoded.
; din_str -- H_CHIN op 5: ONE STRING INPUT # field into STRSCR = [len][bytes]
; (D-INPQUOTE2, 2026-11-05), the length byte the counter, with a PEEK -- what
; main's consume-only reader could not do. Measured on the CF-3300
; (scratchpad/inpquote_run.out): leading blanks and LFs are shed; an unquoted
; field ends at a comma or a CR (taken, with its LF); a `"` as the first byte
; opens a quoted field, inside which CR and LF are DATA; after the closing quote
; the blanks are eaten and ONE comma, or ONE CR with its LF, with them -- any
; other byte (`"AB"CD`) belongs to the NEXT read and stays (din_num's dnm_tail,
; the same rule). out: CF set when the file ended (the caller's seq_end makes
; that 55 only when nothing was read), as main's reader returned it.
; 📍 Three pieces in three holes (this, dst_body + dst_put, dst_quoted): no hole
; held all 70 B. IX = the row.
din_str:
                xor     a
                ld      (STRSCR), a
dst_lead:
                call    din_peek
                ret     c                   ; the end before any byte
                cp      ' '
                jr      z, dst_skip
                cp      $0A
                jr      z, dst_skip
                cp      '"'
                jp      nz, dst_body
                call    din_take            ; the opening quote
                jp      dst_quoted
dst_skip:
                call    din_take
                jr      dst_lead
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
hdl_oom:                                    ; D-LOADDIRECT: the file does not fit
                ld      a,4                 ; 4: out of memory
                jp      hdl_answer          ; (jp: out of jr reach since the move)
; fmt_key / fmt_echo -- FORMAT's key read / echo (MOVED here by D-DISKCARVE6 step 2, 2026-11-17).
fmt_key:
                ld      hl,(FDC_DEST)
jp_hl:                                      ; (D-DISKVE: bdos_dskio's call into the handler)
                jp      (hl)
fmt_echo:
                ld      e,a
                jp      conout_emit_e
                ds      $504E - $, $00  ; pad up to the pinned $504E LOGIN entry (M22a)
k_504E:
                jp      login_body      ; $504E: BDOS $18 LOGIN canonical entry (M22a)
; --- MSX-DOS-1 kernel SETDTA-time entry: $5058 (M19) --------------------------
; While processing BDOS SETDTA ($1A) the relocated kernel CALLs this page-1
; disk-ROM entry to cache the new DTA pointer into the disk work area. Black-box
; contract (M19, callseq/capture/callwatch causal pin, no stock CODE decoded):
; entry DE = the new DTA pointer (=$D403 for DIR; register-identical ours==stock,
; 51 calls each), and stock's routine stores it at the fixed work-area cell $F23D
; (proven: on stock $F23D=$D403 and SFIRST reads it via PCs $4FCC/$4FEF; on ours,
; before this veneer, $F23D was never written -> SFIRST/SNEXT wrote the found entry
; to a stale/garbage DTA -> DIR reported "File not found"). Wiring $5058 to cache
; DE at $F23D makes SFIRST/SNEXT (which read DOS_DTAPTR=$F23D) target the real
; runtime DTA. Same un-wired-$50xx-entry class as M13/M17/M18. Body =
; setdta_cache_body (free tail). CLEAN-ROOM: our own DE->$F23D store + the observed
; entry-register / write-target contract; no stock routine internals decoded.
; disk_putbyte -- A = the byte, appended; a write error -> sv_load_error (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
disk_putbyte:
                call    fat_io_putbyte
                jp      c,sv_load_error        ; disk full / write error
                ret
                ds      $5058 - $, $00  ; pad up to the pinned $5058 SETDTA-cache entry
setdta_cache:
                jp      setdta_cache_body ; -> free-tail: ($F23D) := DE (runtime DTA; M19)
; --- MSX-DOS-1 kernel GETALLOC entry: $505D (M20 REVISION 3; tier2-m20-spec.md) -----
; While processing BDOS GETALLOC ($1B) the relocated kernel CALLs this page-1 disk-ROM
; entry to compute the DIR "nn bytes free" footer. Black-box contract (M20 §2, no stock
; CODE decoded): entry regs byte-identical ours==stock (C=$1B BC=$5D1B DE=$D100
; HL=$C924 IX=$F195); the un-wired $00 pad here made the CALL NOP-slide into the $50A9
; stub tail (garbage exit -> "0 bytes free"). Exit contract (map.grauw.nl _ALLOC):
; A=sectors/cluster, BC=bytes/sector, DE=total data clusters, HL=free clusters.
; REVISION 3 (this span): Revisions 1 (register-only) and 2 (+ resident FAT buffer +
; IY + a per-drive DPB+19 field) were both BUILT and FALSIFIED -- the A/BC/DE/HL scan
; math was correct in both (host-unit-tested + confirmed at getalloc_body's own ret),
; but the DIR footer was wrong regardless, IDENTICALLY so both with and without the
; IY/DPB+19 wiring. Root cause (this span, a narrow read of 20 RAM-KERNEL bytes at the
; shared post-handler exit path $D8AA-$D8BD -- NOT stock ROM/COMMAND.COM code -- to
; answer one question, "what does the branch at $D8AE test"; same technique already
; used to pin $F23D/$F247/$F347/$F306 in prior milestones): the kernel's common BDOS-
; exit path gates HL passthrough on a flag cell, $F306 (set to $01 by the dispatcher on
; every BDOS call, per tier2-gdate-spec.md's earlier pin) -- a handler that leaves
; $F306 set has its HL SILENTLY OVERWRITTEN with H:=B,L:=A (the CP/M single-byte-result
; B:A->HL mirror) instead of passed through. Stock's $505D handler clears it; ours never
; did, so our correct HL was replaced by a mirror of our own A/BC (explaining the exact
; wrong constant observed, `$0202`=514, in both falsified revisions). Causally proven by
; a poke test (forcing $F306:=0 flips the branch and the handler's real HL survives).
; `gdate_handler` below already clears $F306 (previously assumed "harmless stock
; parity" -- it is in fact load-bearing, and the only reason GDATE's HL reaches its
; caller). Revision 3 = Revision 1's register-only body (own FAT scan via read_sector/
; WBUF, reusing fat.asm's fac_entry_from_wbuf unpack -- no new RAM cell, no IY, no
; DPB+19, both proven unnecessary) + one added store: clear $F306 before `ret`, loading
; the final A value AFTER the clear (order matters: A must hold sectors/cluster at
; `ret`; the clear only needs A=0 transiently as the store source).
; CLEAN-ROOM: our own FAT scan (fat_mount + read_sector + fat.asm's fac_entry_from_wbuf,
; all pre-existing oracle-validated primitives) + the published GETALLOC A/BC/DE/HL
; contract (map.grauw.nl) + the black-box-pinned $F306 dispatcher-flag semantics
; (register/memory VALUES + one narrow RAM-kernel byte read, no stock ROM decoded).
                ds      $505D - $, $00  ; pad up to the pinned $505D GETALLOC entry
getalloc:
                jp      getalloc_body   ; -> free-tail: FAT free-cluster scan (M20 rev3)
; --- MSX-DOS-1 kernel DSKRST entry: $509F (M22a; tier2-m22-cpmver-spec.md) -----
; While processing BDOS DSKRST ($0D) the relocated kernel CALLs this page-1
; disk-ROM entry to flush/reset the disk work area. Was un-wired $00 pad,
; NOP-sliding into our OWN $50A9 continuation stub below — which happens to
; reproduce the exact register contract by coincidence (§4.4: "pad-slide
; luck", the same class of latent bug M21a-RC-1 broke later). Body =
; dskrst_body (free tail): a wired `jp` to the same stub, replacing luck with
; an explicit entry. Real flush semantics are unverifiable here (no dirty
; buffers at these call sites); the register contract is the observable
; surface (§8.4 residual).
; drv_select: the single drive's A:/B: swap (D-DSKIB; black box: scratchpad/dskib_run.out).
; 🏗️ Joost 2026-10-09: "yeah, we need to have the prompt mechanism" -- ONE physical
; drive, two logical ones. On the CF-3300 (scratchpad/dskib_run.out, _run2, _run3)
; addressing the OTHER logical drive prints, from a fresh line,
;     Insert diskette for drive B:
;     and strike a key when ready
; waits for ONE key (consumed: the next one stays queued), goes to a new line and
; carries on with the same disk; a second access to the same drive is silent, and
; a name with no drive (DSKF(0), DSKF(1) too) is A:. The current drive is
; DSK_CURDRV. ⚠️ The WAIT goes through calbak to the BIOS's CHGET: a page-1 tenant
; must never wait itself (PLAY and the traps tick only with main in page 1).
; ⚠️ COPY "A:X" TO "B:Y" prompts four times there (per buffer); here it names one
; drive per operation -- filed in D-DSKIB, not modelled.
;   in   A = a drive code: 0 default, 1 A:, 2 B:   out  nothing; keeps IX only --
; fat_geom_read, its main caller, clobbers BC DE HL itself straight after
DK_FCB_DRV      equ     DISK_FCB_NAME - 1   ; DISK_FCB+0 (basic/sysvars.inc DISK_FCB_DRV) (1 B)
DK_CHGET        equ     $009F               ; MSX BIOS: wait for a key -> A
drv_select:
                cp      2                   ; 2 (B:) -> 1, anything else -> 0 (A:)
                ld      a,0
                jr      nz,dsl_cmp
                call    dsl_bcheck          ; B: = 1 -- unless there is no B: (fat.asm)
dsl_cmp:
                ld      hl,DSK_CURDRV
                cp      (hl)
                ret     z                   ; the drive already holds it: silent
                ld      (hl),a
                push    ix
                push    af                  ; the new drive, 0 / 1
                ld      a,(CSRX)
                dec     a
                ld      hl,dsl_text_nl      ; mid-line: a fresh line first
                call    nz,hke_pr
                ld      hl,dsl_text_1
                call    hke_pr
                pop     af
                add     a,'A'
                call    CHPUT               ; the drive letter
                ld      hl,dsl_text_2
                call    hke_pr
                ld      ix,DK_CHGET
                call    calbak              ; ONE key, with main in page 1
                ld      hl,dsl_text_nl
                call    hke_pr
                pop     ix
                ret
                ds      $509F - $, $00  ; pad up to the pinned $509F DSKRST entry (M22a)
k_509F:
                jp      w50a9_stub      ; $509F: BDOS $0D DSKRST canonical entry (M22a; was via
                                        ; the dskrst_body veneer -- D-DISKCARVE)
; hkcv_float -- hk_cv's float: the string's bytes into FAC (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
hkcv_float:
                ld      de,FAC
                ldir                        ; the string's bytes ARE the packed value
                scf                         ; FACTYP is already the width
                ret
                ds      $50A9 - $, $00  ; pad remainder up to the kernel's $50A9 target
w50a9_stub:
                ld      a, $FF          ; D-F346: MSX-DOS has been booted -- the CF-3300
                ld      (DOS_F346), a   ; reads 255 after it (scratchpad/f346_run.out)
                sub     a               ; A=$00, F=$42 (Z+N) -- the exact exit AF
                ld      (W50A9_WRKB), a ; $F242 := $00  (the only persistent write)
                ld      hl, DOS_SIG     ; D-BOOTDATE: MSXDOS.SYS is running, so the
                ld      (BASENT_REQ), hl ; mark says MSX-DOS was booted (our own
                                        ; cell, not the contract's; HL is set below --
                                        ; D-DISKREREAD 2026-11-26: HL, not DE, 1 B)
                ld      de, W50A9_RET_DE ; DE = $F1AA
                push    de              ; IX = $F1AA (= DE; push / pop, 1 B: no
                pop     ix              ; flag touched, the exit AF stays $0042)
                ld      hl, W50A9_RET_HL ; HL = $F359
                ret                     ; AF=$0042, BC/IY untouched

; --- MSX-DOS-1 kernel CONIN line-read entry: $50E0 (M12d pin; tier2-conin-spec.md v3) --
; BUFIN (BDOS func $0A) has the relocated kernel CALL a disk-ROM console-line routine;
; the M12d black-box pin (guarded callseq, no stock bytes decoded) fixed the entry as the
; CALL target $50E0 (regs byte-identical ours==stock at first entry: DE=$DA40 HL=$C924
; ret=$D88A) and the register convention as DE -> buffer, [DE]=max length (the published
; BDOS func-$0A layout: [+0]=max/[+1]=count/[+2..]=chars; confirmed by a second pin
; walking HL/D/E across 7 CHGET calls with an injected keystroke). No return register --
; the result lives in the buffer (func-$0A contract). Our `$50B7-$5453` region is our own
; $00 padding (active code ends at the $50A9 routine above), so before this veneer BUFIN
; NOP-slid straight into the $5454 CONOUT veneer below (one garbage char, no block) --
; the M12 root cause. Body = conin_line_body (free tail): reimplements the func-$0A
; buffered-line read from the published CHGET ($009F) / CHPUT ($00A2) / func-$0A buffer
; ABI, clean-room -- the stock routine's internals were never read (spec §3 Option A).
;
; --- MSX-DOS-1 kernel SELDSK-time entry: $50D5 (M17; tier2-m17-spec.md) --------
; While processing BDOS SELDSK (func $0E) the relocated kernel CALLs this page-1
; disk-ROM entry (register-identical ours==stock at entry: AF=0044 BC=D50E DE=D3FF
; HL=D349 IX=F459 IY=DC5B, ret=D88A -- a real CALL boundary, M17 §2.1). Black-box
; contract: read the logical-drive count from $F347 into A, preserve BC/DE/HL/IX/IY
; (return F irrelevant so far, §4). Our $50B8-$5453 block is our own $00 padding, so
; before this veneer the CALL NOP-slid into the $5454 CONOUT veneer and never
; returned to the trampoline -> the M17 SELDSK stall (no A> prompt). Body =
; seldsk_drv_body (free tail); clean-room -- the stock routine's internals were never
; read (only its entry/exit registers + a DATA-cell read-watch of $F347).
; --- MSX-DOS-1 kernel CURDRV-time entry: $50C4 (M18; tier2-m18-spec.md) ---------
; While processing BDOS CURDRV (func $19) the relocated kernel CALLs this page-1
; disk-ROM entry (register-identical ours==stock at entry: AF=0044 BC=C419 DE=D3FF
; HL=D502 SP=DBFE IY=DC5B, ret=D88A -- a real CALL boundary, M18 §2.1). Black-box
; contract (readwatch causal pin): read the CURRENT-drive index from $F247 into A,
; preserve BC/DE/HL/IX/IY. Our $50B8-$50D4 block is our own $00 padding, so before
; this veneer the CALL NOP-slid and never read the drive -> the kernel's later
; 'A'+drive letter math used a bogus index ($C400 + 2 = 'C>') instead of A>. Body =
; curdrv_body (free tail); clean-room -- the stock routine's internals were never
; read (only its entry/exit registers + a DATA-cell read-watch of $F247).
                ds      $50C4 - $, $00  ; pad up to the pinned $50C4 kernel entry
curdrv:
                jp      curdrv_body     ; -> free-tail: A = ($F247) current drive (M18)
; --- MSX-DOS-1 kernel SETRND entry: $50C8 (M32; tier2-m33-m32-fcb-position-
; spec.md §3) --------------------------------------------------------------
; While processing BDOS $24 SETRND the relocated kernel CALLs this page-1
; disk-ROM entry (same FCB-copy dispatcher class as M26 RDRND/WRRND: entry
; A=$25, DE=IY=$DA40 kernel FCB copy, BC=HL=$0000, SP=$DBFE, captured
; black-box on both machines). Carved from the $50C4-$50D5 pad (curdrv above
; occupies $50C4-$50C6; $50C7..$50D4 was un-wired $00 pad -- 14 bytes, ample
; for this 3-byte veneer) -- net-zero, no canonical address shifts. Was
; un-wired $00 pad -> ours' $24 NOP-slid -> no write -> RR stayed 0. Body =
; setrnd_body (free tail): computes the CORRECT CP/M func-36 rr = cr +
; ex*128 + s2*4096 from the FCB-copy position fields M33 now maintains -- an
; INTENTIONAL, DOCUMENTED divergence from stock's known-broken RR:=1 stub
; (§4, allowlisted; ours' $27 stays independently gate-verified by M31's
; dedicated round-trip probe, not by the BDOSX $24-chained bytes). CLEAN-
; ROOM: hook address + register convention from black-box capture (call-
; target + register/RAM observation, same M26 dispatcher class); rr formula
; from the published CP/M func-36 contract; no stock ROM code decoded.
                ds      $50C8 - $, $00  ; pad the single leftover pad byte ($50C7)
setrnd:
                jp      setrnd_body     ; $50C8: BDOS $24 SETRND canonical entry (M32)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; rd_fatsec -- FAT_FATSEC := HL and read that sector into FAT_MBUF (= WBUF); Cy and A
; as read_sector leaves them. The four instructions stood four times (fat_alloc's
; scan, both fat_write_fat_entry paths, fac_cached_entry): D-DSKFMT defrag, 2026-11-09.
rd_fatsec:
                ld      (FAT_FATSEC), hl
                ex      de, hl
                jp      rs_mbuf
                ds      $50D5 - $, $00  ; pad up to the pinned $50D5 kernel entry
seldsk_drv:
                jp      seldsk_drv_body ; -> free-tail: A = ($F347) drive count (M17)
; fmt_off -- the boot-sector offsets dskfmt_body sets (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
fmt_off:
                db      13, 17, 19, 20, 21, 22, 26
                ds      $50E0 - $, $00  ; pad up to the pinned CONIN line-routine entry
conin:
                jp      conin_line_body ; -> free-tail buffered-line CHGET loop (M13)

; --- hk_dpbsave: BSAVE to disk, in the disk ROM (D-ROMSIZE step 1, 2026-10-19) ---
; Reached through H_FOPEN's dispatcher (hk_dpload) with FOPEN_SEL = FOPEN_SEL_BSAVE.
; The CF-3300's BSAVE enters $FE5D exactly once, never $FE71, and no claimed cell
; per byte; a bad drive is 62 BEFORE it, no disk 70 after it; trailing junk is a
; Syntax error with no file written (scratchpad/bsave_hook_probe.py ->
; bsave_hook_run.out). Main does the 62 (parse_disk_fcb) and the junk (do_bsave's
; mxf_require_end) before it crosses; the 70 is this ROM's mount, as for SAVE.
; Until this date the engine ran in the SUB-ROM save tenant over that ROM's second
; FAT engine (sub/save.asm, SV_OP_BSV_DISK) and reached no hook at all.
; Clean-room: own code (the engine moved verbatim from our sub-ROM tenant) over
; our own FAT12 engine; the hook contract is a black-box measurement of entry
; counts and registers -- no reference ROM byte read.
; SITED HERE, IN THE $50E3 HOLE (708 B the day it moved; `make basic-reloc`
; reads it), because hk_dpsave's region before $75A5 had 45 B: only the 5 B
; dispatch arm and hk_dpsave's 1 B `ld hl` / `jp (hl)` rework went there.
;   in   FOPEN_SEL = FOPEN_SEL_BSAVE; DISK_FCB_NAME = the 8.3 name; CURPTR =
;        start, DSV_END = end (inclusive), EXECPTR = exec, VRAM_FLAG = 1 for `,S`
;   out  CF = 1 claimed; DISKOP_STATUS = 0 written / 3 mount, create, disk full
;        or I-O (DISKOP_ERR carries the mapped code: 70, 66, 68, ...)
hk_dpbsave:
                ld      hl,bsv_open         ; the engine, for hkds_run's `jp (hl)`
                jp      hkds_go             ; hk_dpsave's prologue, unwind and tail
                include "basic/sv-bsvdisk.inc"       ; disk_putword, bsv_open .. bsv_full

; --- hk_dpbload: BLOAD from disk, in the disk ROM (D-ROMSIZE step 1, 2026-10-19) ---
; Reached through H_FOPEN's dispatcher (hk_dpload) with FOPEN_SEL = FOPEN_SEL_BLOAD.
; MEASURED on the CF-3300 (disk/docs/spec-diskcode-eviction.md §6.6ab / §6.6v /
; §8.2): BLOAD enters $FE5D once with DE = $0001 (INPUT) and HL = the program-verb
; buffer, as LOAD and MERGE do; it never enters $FE76 (tokenised LOAD's) and no
; claimed cell per byte -- so the whole load is one crossing, and this is it.
; Main parses the tail first (pcr_bload: `,R` / `,S` / `,offset` -- the CF-3300
; parses it before the lookup, D-BLOADOFS); the sub-ROM BLOAD tenant decides the
; device and builds DISK_FCB_NAME (62 / 56 for a bad drive / name, before this
; hook), then main crosses. Main keeps the `,R` hand-off (basic/bload.asm
; load_handoff / lh_back, the $6CF3 pin's continuation, D-BLOADRTAIL,
; D-VARTABADOPT / D-HFOXOOM): it reads EXECPTR after this returns.
; UNTIL THIS DATE the tenant read the file itself (basic/bload-body.inc's
; do_disk_bload, over the sub-ROM's copy of basic/fat-prim-body.inc's read half and
; of basic/fatio-body.inc) -- the last disk verb on that ROM's FAT engine, which is
; gone with it. This is that loop, over THIS ROM's fat_mount / fat_io_find /
; fat_io_getbyte, with the same exits in the same order.
; Clean-room: our own code (moved) over our own FAT12 engine; the BSAVE binary
; layout is the public one (basic/PROVENANCE.md); the hook contract is a black-box
; measurement of entry counts and registers -- no reference ROM byte read.
;   in   FOPEN_SEL = FOPEN_SEL_BLOAD; DISK_FCB_NAME = the 8.3 name; BL_OFS = the
;        `,offset` (0 = none); VRAM_FLAG = 1 for `,S`
;   out  CF = 1 claimed; DISKOP_STATUS = 0 loaded / 1 no volume, I-O, or a file
;        that ends early (DISKOP_ERR carries a DSKIO failure's code, 70 for an
;        empty drive; with none main PRINTS `load error`) / 53 not found (D-BLNF) /
;        61 not a BSAVE file (D-BLMODE) -- BL_STAT's code, which main decodes;
;        CURPTR = ENDPTR, EXECPTR = the header's + the offset
hk_dpbload:
                ei                          ; D-LOADDI: the hook arrives DI (CALSLT); the
                                            ; tenant ran this load with interrupts ON, and
                                            ; the driver keeps the caller's state per sector
                xor     a
                ld      (DISKOP_ERR),a      ; D-DISKERR: no DSKIO failure pending yet
                call    fat_mount
                jr      c,hbl_err           ; no volume / I-O: 1 (70 pending if empty)
                call    fat_io_find
                ld      a,53                ; mounted, and the 8.3 name is not there
                jr      c,hbl_answer
                call    fat_io_getbyte      ; header byte 0: the BSAVE disk marker
                jr      c,hbl_err           ; an empty file: the safe default, as before
                cp      BSAVE_DISK_ID
                ld      a,61                ; the file EXISTS and is not a BSAVE binary
                jr      nz,hbl_answer       ; (PROG.BAS and a non-$FE TEST.BIN alike)
                ld      hl,CURPTR           ; header bytes 1..6 -- start, end, exec (LE)
hbl_hdr:                                    ; -- into CURPTR / ENDPTR / EXECPTR, six
                push    hl                  ; contiguous bytes, the tape path's loop
                call    fat_io_getbyte      ; (CALSLT-free here, but a sector refill
                pop     hl                  ; clobbers every register)
                jr      c,hbl_err
                ld      (hl),a
                inc     hl
                ld      a,l
                cp      low (EXECPTR + 2)
                jr      nz,hbl_hdr
                ld      de,(BL_OFS)         ; D-BLOADOFS: + the `,offset` to start, end
                ld      hl,CURPTR           ; AND exec, wrapping at 16 bits, as the
                ld      b,3                 ; CF-3300 does (the tenant's bl_apply_ofs)
hbl_ofs:
                ld      a,(hl)
                add     a,e
                ld      (hl),a
                inc     hl
                ld      a,(hl)
                adc     a,d
                ld      (hl),a
                inc     hl
                djnz    hbl_ofs
                ; the data, into [start..end] inclusive -- RAM, or with `,S` VRAM
                ; through WRTVRM (the header words are VRAM addresses then)
hbl_data:
                call    fat_io_getbyte
                jr      c,hbl_err           ; ran out before the end: 1, as before
                ld      hl,(CURPTR)
                ld      c,a
                ld      a,(VRAM_FLAG)
                or      a
                ld      a,c
                jr      z,hbl_ram
                call    WRTVRM              ; HL = the VRAM address, A = the byte
                jr      hbl_adv
hbl_ram:
                ; 🔴 PAGE 1 IS THIS ROM NOW. The tenant stored with the sub-ROM in
                ; page 1 (and main's ROM before it), so a byte aimed at $4000-$7FFF
                ; went nowhere. Here it would reach disk.rom's slot -- its FDC
                ; window included -- so it is dropped, exactly as a ROM dropped it.
                ld      a,h
                sub     $40
                cp      $40                 ; CF iff $40 <= H < $80
                jr      c,hbl_adv
                ld      (hl),c
hbl_adv:
                ld      hl,(CURPTR)
                ld      de,(ENDPTR)
                or      a
                sbc     hl,de
                jr      z,hbl_ok            ; cur == end: the last byte is stored
                add     hl,de
                inc     hl
                ld      (CURPTR),hl
                jr      hbl_data
hbl_ok:
                xor     a                   ; 0: loaded
                jr      hbl_answer
hbl_err:
                ld      a,1                 ; 1: main's disk_error -- the pending code,
hbl_answer:                                 ;    else a PRINTED `load error`
                jp      hdl_answer          ; DISKOP_STATUS := A, FOPEN_SEL disarmed, CF = 1

; --- MSX-DOS-1 kernel CONOUT-worker entry: $53A7 (M22b; tier2-m22b-conout53a7-spec.md) --
; The relocated kernel CALLs this page-1 disk-ROM entry for EVERY BDOS CONOUT
; ($02) character (ret=$D88A, B=$A7 fingerprint, C=$02, char in E — the same
; dispatcher class as the M22a eleven). It was un-wired $00 pad; pre-M22a the
; CALL NOP-slid 173 bytes into the $5454 CONOUT veneer below, which happens to
; implement the exact right observable contract (emit E via CHPUT, A:=E) — the
; accident every M8-M21 console-parity probe rode. M22a's new $543C CONST
; veneer (below) now sits earlier in that same slide and intercepts it instead,
; silently dropping every func-2 char (date/A>/echo/DIR listing). This veneer
; restores the pre-M22a behavior explicitly instead of by accident; net-zero,
; 3 bytes carved from the existing pad before $543C. CLEAN-ROOM: $53A7 is a
; de-facto page-1 kernel-ABI address (class of $4010/$5454), pinned by
; call-target + register observation only (400/400 region-entry samples);
; body = conout_body, the proven $5454 implementation (M8/M10).
; --- hk_lrset: LSET/RSET's body, RUNNING IN THE DISK ROM --------------------
; docs/spec-basic-nodisk.md §9 for the hook; disk/docs/spec-diskcode-eviction.md
; §7.3-§7.5 for the first slice. D-LRSETMOVE, Joost 2026-09-17: *"cut it anyway,
; it is the correct place for it"*. HISTORY, condensed from the header that
; stood with the old handler: at D-LRSETMOVE only the field-entry walk (~25 B of
; a 122 B body) moved here and the PARSE stayed in main behind three bundled
; call-backs; D-FLDDESC (2026-10-17) removed the walk, leaving only the decision
; to run; D-DISKREST slice 1 (below) moved the parse.
; hk_lrset -- LSET / RSET v$ = s$, PARSED HERE (D-DISKREST slice 1, 2026-10-20).
; 📌 SITED IN FRONT OF THE PINNED $53A7 ENTRY: the largest hole in this ROM; the
; region it used to sit in (up to $75A5) had no room for the parse.
; Until then this handler only called three main-side BUNDLES (lrset_tgt,
; lrset_rhs, lrset_finish), so all of LSET ran in main page 1 -- ~49 B of it --
; while Joost had ruled on 2026-09-17 that disk.rom is "the correct place for
; it". The parse now runs here and calls back one HELPER at a time, as a disk
; ROM reaches BASIC through CALBAS; main keeps the gate (a diskless machine's
; ERR 5 comes before any of this) and lrset_finish, whose store dispatches a
; sub-ROM tenant -- a disk -> sub nesting this tree has never made.
; 💰 The price is crossings: 8 where there were 3 (~0.17 ms each, D-XSLOTPRICE).
; A / HL / DE / BC and CF cross both ways (D-XSLOTABI); Z does NOT, so every test
; here re-derives its flag from A or CF after the call-back. Each helper may
; RAISE, which is safe from a call-back (main page 1 is mapped; the unwind resets
; SP and discards this frame) -- the precedent is hk_dget's record call-backs.
;   in   FN_RESUME = the cursor just past the LSET / RSET token
;   out  FN_RESUME = the resume cursor; CF = 1 (claimed)
hk_lrset:
                ld      hl,(FN_RESUME)
                dec     hl
                ld      a,(hl)              ; the verb's own token: bit 0 = RSET
                inc     hl                  ; (main asserts LSET even, RSET = LSET+1)
                ld      (LRSET_JUST),a      ; a diskless machine never gets here
                ld      ix,req_letter
                call    calbak              ; a target must be a name, else ERR 2
                ld      ix,var_str_type
                call    calbak              ; A = 1 if `$` (or DEFSTR)
                or      a
                jr      z,hklr_tm           ; a numeric target -> ERR 13
                ld      ix,tgt_parse_req
                call    calbak              ; BC = key, (TGT_ADDR); HL past it all
                push    hl
                ld      ix,tgt_desc
                call    calbak              ; HL = the descriptor / element offset
                ld      (MIDS_DEST),hl
                pop     hl
                ld      ix,skip_eq
                call    calbak              ; A = the next non-space byte
                cp      EQ_TOKEN
                jr      nz,hklr_syn         ; no `=` -> ERR 2
                inc     hl
                ld      ix,req_operand
                call    calbak              ; `LSET A$=` / `= :` -> ERR 24
                ld      ix,str_eval
                call    calbak              ; STRPTR -> [len][ptr]; HL advanced
                jr      nc,hklr_tm          ; a non-string RHS -> ERR 13
                ld      (FN_RESUME),hl
                ld      ix,lrset_finish
                call    calbak              ; destination, then the store
                scf
                ret
hklr_tm:
                ld      ix,type_mismatch_error
                jr      hklr_raise          ; D-DISKREREAD (2026-11-26): one jp calbak; 1 B
hklr_syn:
                ld      ix,stmt_error
hklr_raise:
                jp      calbak              ; raises; never returns

; --- hk_dsk: DSKI$ / DSKO$'s sector I/O in the disk ROM (MSX2 TH hooks H.DSKI / H.DSKO) ---
; D-ROMSIZE step 1 (Joost 2026-10-14/15: the disk evictions first). Main's dsk_core
; (basic/str-engine.asm) crosses this hook TWICE: first with A = 0, BEFORE the parse,
; so a diskless machine (cell unclaimed) still answers ERR 5 where the CF-3300
; answers ERR 2 -- that crossing only says "present"; then, after parsing `d,s`,
; with A = the selector and BC = the sector. The sector moves between
; (DSKBUF_PTR) and the disk with THIS ROM's own primitives (basic/fat-prim-body.inc,
; raw DSKIO through dskio_calslt: no mount, and the DISKOP_ERR mapping inside).
; It used to be the sub-ROM dirverb tenant, reached through main's dirverb_op with
; the sector poked into FWR_DIRSEC -- the open OUTPUT channel's own directory
; pointer, which made a DSKI$ with a file open lose that file (D-ALIASWCELL). The
; sector now crosses in BC, so that hazard is gone by construction.
; 🔁 D-DISKREST slice 3 (2026-10-20): AND THE `d,s` PARSE IS HERE, as on the
; CF-3300 (whose ERR 2 for `DSKO$ 0` is the parse's; the diskless VG-8020's ERR 5
; is main's unclaimed cell, before it). Main crosses ONCE now, with the cursor in
; BC; the two-crossing paragraph above is how D-DSKLOCAL left it. Each argument is
; evaluated by a call-back into main, which may RAISE -- and may itself re-enter
; this ROM (`DSKO$ 0,LOF(1)`), which is why the selector waits on the stack.
; Z does not cross a call-back; A and CF do, so each test is re-derived from them.
;   in   A = DISKOP_SEL_DSKO / DISKOP_SEL_DSKI; BC = the cursor at `d`
;   out  HL = the cursor after `s`; DISKOP_STATUS = 0 ok / $FF failed
;        (DISKOP_ERR = the DSKIO code); CF = 1
hk_dsk:
                ei                      ; entered through CALLF, masked
                push    af              ; the selector, across the parse
                ld      h,b
                ld      l,c             ; HL = the cursor at `d`
                ld      ix,eval_byte_arg
                call    calbak          ; A = E = the drive (0..255, else ERR 5)
                cp      3               ; D-DSKIB: 2 is B:, the swap prompt (dskib_run.out)
                jr      c,hkd_drv
                ld      a,62            ; Bad drive name (DSKI$(3,0), measured)
                jp      hkf_raise       ; raise_error; never returns (D-DISKREREAD
                                        ; 2026-11-26: hk_field's copy of the raise; 4 B)
hkd_drv:
                ld      (DK_FCB_DRV),a  ; D-DSKIB: this drive, and the swap prompt if it
                push    hl              ; is the other one -- DSKI$/DSKO$ read the sector
                call    drv_select      ; straight through DSKIO, past fat_mount
                pop     hl
                ld      ix,skip_comma
                call    calbak          ; A = the next non-blank byte
                cp      ','
                jr      nz,hklr_syn     ; ERR 2 the statement's way (D-STMTPEND) --
                                        ; D-DISKREREAD (2026-11-26): hk_lrset's own
                                        ; `ld ix,stmt_error / jp calbak`; 7 B
hkd_comma:
                inc     hl
                ld      ix,eval_int16_checked
                call    calbak          ; DE = the sector, HL past it
                pop     af              ; A = the selector
                push    hl              ; the cursor, across the I/O
                ld      hl,(DSKBUF_PTR) ; HL = the buffer
                cp      DISKOP_SEL_DSKO
                jr      nz,hkd_read
                call    fatprim_write_sector
                jr      hkd_st
hkd_read:
                call    read_sector
hkd_st:
                sbc     a,a             ; CF -> $FF, clear -> 0
                ld      (DISKOP_STATUS),a
                pop     hl              ; HL = the cursor after `s`
                scf                     ; claimed
                ret

; hk_field -- FIELD #f, w AS v$ [, ...]. 🔁 D-DISKREST slice 4 (2026-10-20): THE
; PROLOGUE IS HERE -- it was main's call-back 1, field_prologue. `#` and the comma
; are read locally (hkp_sp is main's skip_spaces); the channel expression,
; fch_check (5 / 59 / 52) and fch_mode_class (59 if not open; CF = disk) are
; call-backs; the record's address comes from this ROM's own RANDOM row (dout_row
; + rnd_rec: block+9, where FIELD has pointed descriptors since D-FLDDESC) where
; main asked the sub ROM (fch_ctx_addr). Then the item loop, as before.
; 📌 SITED IN FRONT OF THE PINNED $53A7 ENTRY: its old region had no room.
;   in   FN_RESUME = the statement cursor, just past the FIELD token
;   out  the statement is DONE; FN_RESUME = the resume cursor; CF = 1
hk_field:
                ei                          ; entered through CALLF, masked
                ld      hl,(FN_RESUME)
                call    hkp_sp
                cp      '#'
                jr      nz,hkf_ch
                inc     hl
hkf_ch:
                ld      ix,eval
                call    calbak              ; DE = the channel; HL advanced
                ld      ix,fch_check
                call    calbak              ; D!=0 -> 5, 0 -> 59, > MAXFILES -> 52
                push    hl
                ld      ix,fch_mode_class
                call    calbak              ; A = FCH_MODES[E]; 59 if NOT OPEN;
                pop     hl                  ; CF = a disk channel (E survives)
                jr      nc,hkf_dev          ; LPT: / CRT: -> 5
                cp      4
                jr      nz,hkf_bfm          ; disk, not RANDOM -> 61
                push    hl                  ; the cursor
                ld      a,e
                push    af                  ; the channel
                ld      de,FCH_RECLENS
                call    rnd_tab             ; HL = &FCH_RECLENS[ch]
                ld      c,(hl)
                inc     hl
                ld      b,(hl)              ; BC = the record length, 1..256
                pop     af
                push    bc
                call    dout_row            ; HL = the channel's row
                push    hl
                pop     ix
                call    rnd_rec             ; HL = its record, block+9
                ld      (FLD_CUROFF),hl     ; where the first field starts
                pop     bc
                add     hl,bc
                ld      (FLD_END),hl        ; field_item's ERR 50 bound
                pop     hl
                call    hkp_sp
                cp      ','                 ; FIELD #f , w AS v$
                jp      nz,hklr_syn         ; ERR 2, as req_comma raised it --
                                            ; D-DISKREREAD (2026-11-26): its copy
                                            ; hkf_syn went; 6 B
                inc     hl
hkf_next:                                   ; D-DISKREREAD (2026-11-26): the item loop
                ld      (FN_RESUME),hl      ; re-enters HERE -- one store, not two; 3 B
hkf_item:
                ; 🔁 D-DISKREST slice 5 (2026-10-20): field_item only PARSES now
                ; (`w AS v$`: DE = w, BC = the variable's key, TGT_ADDR, HL past
                ; it); the descriptor, the bound and the comma are here.
                ld      ix,field_item
                call    calbak              ; DE = width, BC = key, HL = cursor
                push    hl
                ld      hl,(FLD_CUROFF)     ; this field's first byte
                ld      a,e
                ld      (RVDESC),a          ; [w]
                ld      (RVDESC+1),hl       ; [ptr]
                add     hl,de               ; the byte past this field
                ld      (FLD_CUROFF),hl
                ex      de,hl
                ld      hl,(FLD_END)
                or      a
                sbc     hl,de               ; CF iff it runs past the record
                ld      a,50                ; FIELD overflow -- BEFORE the store
                jr      c,hkf_raise
                ld      ix,tgt_store_desc
                call    calbak              ; the variable := [w][ptr] (BC = key)
                pop     hl
                call    hkp_sp
                cp      ','
                jr      nz,hkf_done
                inc     hl
                jr      hkf_next
hkf_done:
                ld      (FN_RESUME),hl      ; the end of the field list
                scf
                ret
hkf_dev:
                ld      a,5                 ; illegal function call (LPT: / CRT:)
                jr      hkf_raise
hkf_bfm:
                ld      a,61                ; bad file mode
hkf_raise:
                ld      ix,raise_error
                jp      calbak              ; raises; never returns

; dsm_p1_pre / dsm_p1_post -- D-P1SRCH (2026-10-22): SFIRST / SNEXT's 33-byte entry
; reaches a DTA in page 1. The fill (disk/kernel.asm dsm_found) stores byte by byte
; and with an overlapping LDIR into (DOS_DTAPTR) -- into this ROM when the DTA is in
; page 1: the CF-3300 had the entry at $4200, zerobas $FF (scratchpad/
; p1dta_run4.out). So the fill goes to a scratch and one P1_BLIT carries it. The
; scratch is the HALF OF SECTOR_BUF the matched entry is not in -- the directory
; sector is not needed after the match (SNEXT reads it again), so no new RAM. The
; real DTA waits in P1_DEST (dskio's bounce word; no dskio runs during the fill).
; Sited in front of $53A7: the driver fill at $4465 was full (an EMPTY disk.rom).
;   pre:  in HL = the matched entry (kept)     post: clobbers A, BC, DE, HL
dsm_p1_pre:
                push    hl
                ld      de, (DOS_DTAPTR)
                ld      (P1_DEST), de       ; the real DTA
                ld      de, SECTOR_BUF + 256
                or      a
                sbc     hl, de              ; CF: the entry is in the first half
                ld      hl, SECTOR_BUF + 256
                jr      c, dpp_set
                ld      hl, SECTOR_BUF
dpp_set:
                ld      (DOS_DTAPTR), hl    ; the fill goes to the scratch
                pop     hl
                ret
dsm_p1_post:
                ld      hl, (DOS_DTAPTR)    ; the scratch
                ld      de, (P1_DEST)       ; the real DTA
                ld      (DOS_DTAPTR), de
                call    dsm_rc              ; D-SRCHFCB: +15 = the record count
                ld      a, d
                and     $C0
                cp      $40
                ld      bc, 33
                jp      nz, P1_BLIT         ; not page 1: the FCB form, 33 bytes
                ; 🔴 D-SRCHFCB: into PAGE 1 the CF-3300 writes the RAW directory entry
                ; at DTA+1 -- +0 untouched, the attribute at +12, +13..+22 zero, no
                ; record count (scratchpad/srchfcb_ref.out). From the FCB form: the
                ; attribute down one byte, +13 and +15 cleared, 32 bytes from +1.
                push    hl
                ld      bc, 13
                add     hl, bc
                ld      a, (hl)             ; the attribute (+13)
                ld      (hl), 0
                dec     hl
                ld      (hl), a             ; -> +12
                inc     hl
                inc     hl
                inc     hl
                ld      (hl), 0             ; +15: no record count in the raw form
                pop     hl
                inc     hl                  ; from +1
                inc     de                  ; to DTA+1
                ld      bc, 32
                jp      P1_BLIT


; fac_mag -- HL = |FAC| TRUNCATED toward zero, for a FLOAT FAC (FACTYP 4 / 8);
; CF = 1 when the magnitude passes 65535. hk_mki's range: fac_mag32's value with
; its high word required to be 0. Clobbers A, BC, DE, BC', DE', HL'.
fac_mag:
                call    fac_mag32
                ret     c
                ld      a,d
                or      e
                ret     z                   ; CF = 0 (or's)
                scf
                ret

; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; --- MSX-DOS-1 kernel console entries: $543C/$5445/$544E (M22a) ---------------
; While processing BDOS CONST ($0B) / CONIN ($01) / INNOE ($08) the relocated
; kernel CALLs these page-1 disk-ROM entries. All three were un-wired $00 pad,
; NOP-sliding into the $5454 CONOUT veneer below — each would have emitted a
; garbage char via CONOUT (E's leftover value) instead of polling/reading the
; keyboard, without ever consuming a queued key (tier2-m22-cpmver-spec.md §4.5).
; Bodies (const_body/conin_body/innoe_body, free tail) use the same CHSNS/CHGET
; inter-slot primitives conin_line_body/conout_body already use.
; CLEAN-ROOM: published CONST/CONIN/INNOE contracts (map.grauw.nl) + our own
; pg0_mainrom_in/out inter-slot path; no stock routine internals decoded.
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
bb_gtime:
                xor     a                   ; no clock: 00:00:00.00
                ld      b, a
                ld      c, a
                ld      d, a
                ld      e, a
                ld      h, a
                ld      l, a
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
bb_version:
                ld      hl, $0022           ; MSX-DOS 1 answers version 2.2
                ld      b, h
                ld      a, l
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; 📍 D-LOADRUN (2026-11-06): MOVED HERE from the $782B..$7F7C region, which LOAD's
; hdl_run outgrew by 48 B; entered only by call / jp, so it can live in any hole.
; --- innoe_body / dirin_body - the $544E INNOE / $5462 DIRIN canonical entries
; (M22a) -- blocking read, NO echo (the published BDOS $08/$07 contract, as
; distinct from $01 CONIN's echo). Pinned identical at this probe's granularity
; (tier2-m22-cpmver-spec.md §6.3): dirin_body tail-jumps into innoe_body.
; B/C/D/E preserved; A := char.
; CLEAN-ROOM: published INNOE/DIRIN contracts (map.grauw.nl) + CHGET; no oracle bytes.
innoe_body:
                push    bc
                push    de
                push    hl
                di
                call    br_mainin_tmpl
                jp      dirio_chget         ; 💰 carve 15: CHGET + the shared exit (runtime.asm)
                ds      $53A7 - $, $00  ; pad up to the pinned $53A7 CONOUT-worker entry (M22b)
k_53A7:
                jp      conout_body     ; $53A7: BDOS $02 CONOUT canonical entry (M22b slice 1)
; --- D-BDOSBASIC slice 3a: console / info functions at $F37D (oracle; MSX2 TH BDOS) ---
; Both references answer these from a BASIC program; zerobas returned $FF
; (scratchpad/bdosset_run.out). Most bodies already serve the same functions under
; MSX-DOS (the k_ entries); the DOS kernel finishes their registers there, so where
; the measured BASIC-side answer needs more, a small bb_ body here gives it: CONOUT
; leaves the character in A, VERSION HL = $0022, GTIME zeroes HL too, and DSKRST is
; NOT the DOS-side one (that marks MSX-DOS as booted, D-BOOTDATE).
bdos_tab:
                ; 💰 D-DSKFMT carve 19 (2026-11-10): the rows bdos_entry (disk/driver.asm)
                ; and bdos_more tested by `cp` / `jp`, the block and sequential reads first
                ; (BLOAD and the boot loaders call them most).
                db      $00                 ; D-F37DTERM (2026-11-26): TERMINATE
                dw      bb_term
                db      BDOS_F_RDBLK
                dw      k47b2_body          ; RR-positioned, as the CF-3300 (D-BDOSFCB)
                db      BDOS_F_SEQRD
                dw      wrseq_rd            ; CR / EX in the caller's FCB (D-BDOSFCB)
                db      BDOS_F_SEQWR
                dw      wrseq_body          ; CR / the size in the FCB (D-BDOSFCB)
                db      BDOS_F_OPEN
                dw      bdos_open
                db      BDOS_F_CLOSE
                dw      bdos_close
                db      BDOS_F_CREATE
                dw      bdos_create_body
                db      BDOS_F_SETDTA
                dw      bdos_setdta_body    ; + the $F23D cell
                db      BDOS_F_SFIRST
                dw      sfirst_body
                db      BDOS_F_SNEXT
                dw      snext_body
                db      BDOS_F_STROUT
                dw      RES_PRINT           ; '$'-terminated string at DE
                db      $01                 ; D-F37DCONIN (2026-11-22): the console input
                dw      conin_body          ; functions answered $FF from $F37D; both
                db      $07                 ; references take the key (CONIN.md R-BC-RAW,
                dw      innoe_body          ; R-BC-ECHO, R-BC-BUFIN) -- MSX-DOS's bodies
                db      $08
                dw      innoe_body
                db      $0A
                dw      conin_line_body
                db      $02
                dw      bb_conout
                db      $06
                dw      bb_dirio
                db      $0B
                dw      const_body
                db      $0C
                dw      bb_version
                db      $0D
                dw      bb_dskrst
                db      $0E
                dw      seldsk_drv_body
                db      $13                 ; D-BDOSFCB C: the kernel's, every match
                dw      fdel_body
                db      $17
                dw      fren_wild
                db      $21                 ; D-BDOSFCB (2026-11-10): the kernel's random
                dw      rdrnd_body          ; and block bodies, on the FCB the $F37D
                db      $22                 ; FOPEN / FMAKE fill (they answered $FF)
                dw      wrrnd_body
                db      $23
                dw      fsize_body
                db      $24
                dw      bb_setrnd           ; A = L = $12 from $F37D (D-BDOSFCB residual 2)
                db      $26
                dw      wrblk_body
                db      $28
                dw      wrzer_body
                db      $18
                dw      login_body
                db      $19
                dw      curdrv_body
                db      $1B
                dw      getalloc_body
                db      $2A
                dw      gdate_body
                db      $2B                 ; D-F37DSETDATE (2026-11-22): both references
                dw      sdate_body          ; take a valid date / time from $F37D (A 0);
                db      $2C
                dw      bb_gtime
                db      $2D                 ; this answered $FF to both (SYSTEM.md R-BS-DATE)
                dw      stime_body
                db      $2E
                dw      verify_body
                db      $2F                 ; D-BDOSABS (2026-10-22): RDABS / WRABS for a
                dw      rdabs_body          ; boot loader -- f1sp3d-1's reads 8 sectors
                db      $30                 ; into the DTA before MSX-DOS exists; the
                dw      wrabs_body          ; CF-3300 does, this answered $FF
                                            ; (scratchpad/f1sp3d_slotcalls.out)
bdos_more2:                                 ; A = C = the function
; D-DISKREREAD (2026-11-26): the scan COUNTS the rows (djnz) instead of stopping on a
; `db 0` terminator; 3 B. The handler sees what it did: B = the function, the flags of
; an equal compare (`cp (hl)` and `cp b` with the same value set the same flags).
; The table ends where this code starts (no `bdos_tab_end` label: only an equ read it,
; and check_dead_code saw an empty span no instruction reaches).
BDOS_TAB_ROWS   equ     (bdos_more2 - bdos_tab) / 3
    IF (bdos_more2 - bdos_tab) - BDOS_TAB_ROWS * 3
                db      BDOS_TAB_ROW_IS_NOT_3_BYTES
    ENDIF
    IF BDOS_TAB_ROWS > 255
                db      BDOS_TAB_HAS_MORE_ROWS_THAN_DJNZ_COUNTS
    ENDIF
                push    hl                  ; 🔴 D-BDOSABS: the CALLER'S HL rides through
                ld      hl, bdos_tab        ; to the handler -- RDABS / WRABS take the
                ld      b, BDOS_TAB_ROWS
bm_loop:
                cp      (hl)
                inc     hl
                jr      z, bm_hit
                inc     hl
                inc     hl
                djnz    bm_loop             ; past the last row: B = 0, as before
bm_none:
                pop     hl                  ; the caller's HL back
                xor     a                   ; 🔴 D-F37DHIGH (2026-11-26, D-DISKCONTRACT gap 6): a function with no
                ret                         ; row answers A = 0 (Z / P set, F $44) on the CF-3300 -- $31, $40,
                                            ; $5F, $6F (DOS 2's _DOSVER), $7F, $FF all measured; this was $FF
                                            ; (bdosf37d-acceptance's high rows, scratchpad/AE_f37d_high.out)
bm_hit:
                ld      b, a                ; B = the function, as `ld b,(hl)` left it
                ld      a, (hl)
                inc     hl
                ld      h, (hl)
                ld      l, a
                ex      (sp), hl            ; sector count in H; `jp (hl)` handed them
                ret                         ; the handler's address (77 sectors for
                                            ; f1sp3d-1's 8: scratchpad/f1sp3d_rdabs.out)
; (bb_dirio, bb_conout and bb_dskrst moved to the pads in front of $47BE, $4E4B and
; $461D by D-F37DCONIN: bdos_tab's $01 / $07 / $08 / $0A rows needed these bytes)
; (moved here from init.asm by the D-DSKFMT defrag)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; (rh_init moved to the hole in front of $75A5 by D-F37DSETDATE: bdos_tab's $2B / $2D rows)
cinl_bs:        db      8, " ", 8, 0        ; D-BUFINECHO: BackSpace's un-echo (conin_line_body,
                                            ; disk/runtime.asm; moved here by D-F37DCONIN)
                ds      $543C - $, $00  ; pad up to the pinned $543C CONST entry (M22a)
k_543C:
                jp      const_body      ; $543C: BDOS $0B CONST canonical entry (M22a)
; D-DSKFMT: the fill after the ninth sector, to the index and past it (768 x $4E).
fmt_tail:
                db      0, $4E, 0, $4E, 0, $4E
                ds      $5445 - $, $00  ; pad up to the pinned $5445 CONIN entry (M22a)
k_5445:
                jp      conin_body      ; $5445: BDOS $01 CONIN canonical entry (M22a)
; fat_total_clusters -- DE = total clusters, Cy = 0 (fat_mount's, D-WCACHE W1; MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
fat_total_clusters:
                ; D-WCACHE W1: fat_mount computed it; no boot-sector read, so no
                ; failure either (Cy = 0, which every caller's `ret c` expects).
                ld      de, (FAT_TOTCLUS)
                or      a
                ret
                ds      $544E - $, $00  ; pad up to the pinned $544E INNOE entry (M22a)
k_544E:
                jp      innoe_body      ; $544E: BDOS $08 INNOE canonical entry (M22a)

; --- MSX-DOS-1 kernel CONOUT entry: $5454 ($4000 + $1454; a3 §8.38) ---------
; The relocated kernel CALLs $5454 to emit its sign-on banner one character at a
; time (the boot's first divergence point, disk_probe_dosboot_pctrace.py). It is
; the disk ROM's CONOUT: output the char in A via the BIOS CHPUT path, preserving
; BC/DE/HL/IX/IY. Black-box call-chain on the stock: $5454 -> $408F -> $001C
; (CALSLT) -> resident kernel -> $F398 -> $00A2 (CHPUT); first call A=$0D (the
; banner's leading CR). $5454 is reached by a fixed immediate call in MSXDOS.SYS
; (the call TARGET is $5454 in the pristine just-loaded image, unchanged at call
; time -- observed via the call landing at $5454, not a relocated vector).
; CLEAN-ROOM: $5454 is a cross-vendor de-facto-standard entry,
; byte-identical across seven vendors' disk ROMs in the shared ASCII-kernel block
; ($4768-$576F) -- the same ABI class as $4010 DSKIO / $4016 GETDPB, never a byte
; copy (oracle-artifacts.md "Cross-vendor disk-ROM set"; spec-diskrom-kernel.md
; §1.3). Our code ends far below $5454, so the entry is positioned with a `ds`
; fill (like $4030/$50A9), consuming otherwise-$00 page padding -- nothing shifts.
; M8/§8.67: the first-cut no-op was PROVEN to be the COMPLETE COMMAND.COM-load
; blocker (aligned pctrace from $0100 diverges at exactly $5454; ours spins in the
; banner loop forever, 56 distinct PCs, while stock proceeds, 482). So CONOUT now
; does the real thing: emit A via the main-ROM CHPUT ($00A2) through a genuine
; inter-slot call. The body lives in the free tail (conout_body); $5454 just diverts
; to it (the 2 extra bytes shift the $5456 bodies gap, absorbed by `ds $5FE5 - $`).
                ds      $5454 - $, $00  ; pad up to the kernel's $5454 CONOUT target
conout:
                jp      conout_body     ; -> free-tail inter-slot CHPUT call (M8)

; --- MSX-DOS-1 kernel DIRIN entry: $5462 (M22a; tier2-m22-cpmver-spec.md) ------
; While processing BDOS DIRIN ($07) the relocated kernel CALLs this page-1
; disk-ROM entry. Was inside fat_find_body's span (relocated to the free tail
; below, §4.6) — fat_find_body never had a canonical-address requirement of its
; own (only reached by label from fat.asm's `fat_find` veneer), so moving it
; frees this slot outright. Body = dirin_body (free tail).
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; rrnd_sector — recover the absolute logical sector number rrnd_position's
; seek loop last landed on (see the placement note above for why this is
; safe: FAT_CLUSSEC - 1, FAT_CURCLUS as-is).
;   out: DE = absolute logical sector number; trashes AF, BC, HL
rrnd_sector:
                ld      a, (FAT_CLUSSEC)
                dec     a                   ; the seek loop's sector: FAT_CLUSSEC - 1
                ld      hl, (FAT_CURCLUS)
                jp      clus_sec            ; DE = the absolute sector -- clus_sec's
                ds      $5462 - $, $00  ; pad up to the pinned $5462 DIRIN entry (M22a)
k_5462:
                jp      innoe_body      ; $5462: BDOS $07 DIRIN canonical entry (M22a; was via
                                        ; the dirin_body veneer -- D-DISKCARVE)
k_5465:
                jp      lstout_body     ; $5465: BDOS $05 LSTOUT canonical entry
                                        ; (lands naturally at $5465 = k_5462 + 3 bytes;
                                        ;  was squatted by callf_body_body, M27 §4)

; --- lstout_body - the real $5465 LSTOUT (BDOS $05); characterised 2026-07-04 -----
; Output the char (in E) to the list device via the main-ROM LPTOUT ($00A5),
; preserving BC/DE/HL. Entry E=char is the $5465 LSTOUT contract (= the $5454 CONOUT
; contract; black-box confirmed: kernel func-5 worker entry has E=char, A=$00,
; ret=$D88A -- disk/docs/tier2-lstout-characterisation.md §3.3). Pages the main BIOS
; ROM into page 0 (pg0_mainrom_in), calls LPTOUT, restores (pg0_mainrom_out); DI spans
; the half-mapped window; EI on exit -- the same structure conout_body uses for CHPUT.
;   in:  E = char ; out: BC/DE/HL preserved (IX/IY untouched)
; BIOS-DELEGATING by design (two-interface rule, [[cbios-target-cf3300-oracle]]):
; LSTOUT does NOT poke the printer ports ($90/$91) itself -- it calls the main BIOS's
; LPTOUT and inherits whatever the host BIOS does. On real CF-3300 hardware this drives
; the printer (and, faithfully, blocks if none is attached -- matching stock, which
; tight-polls status port $90). On C-BIOS it inherits C-BIOS's own $00A5, TODAY A STUB
; -> LSTOUT safely no-ops (no port poll, no hang). *** C-BIOS LPTOUT is a LATER FIX ***
; (tier2-lstout-characterisation.md §4): once C-BIOS grows a real $00A5, LSTOUT here
; starts working with no change to this ROM. NB: the DI window means a real-hardware
; no-printer poll would block with interrupts off (no Ctrl-STOP escape) -- an authentic-
; hang edge case, not reachable on our targets (C-BIOS no-op / real-hw-with-printer).
; CLEAN-ROOM: E=char is the BDOS LSTOUT ($05) ABI; $00A5 LPTOUT is the published BIOS
; entry (same documented class as CHPUT $00A2 already called by conout_body); no oracle
; bytes decoded. Lives in the $5456-$5FE4 gap section (net-zero: absorbed by the
; `ds $5FE5 - $` pad below, so nothing downstream shifts / the FDC-window guard holds).
; (moved here from kernel.asm by the D-DSKFMT defrag)
; clr_f306 -- A := 0 (Z, CF = 0) and the dispatcher's HL-passthrough flag $F306 := 0
; (the M20 rule). 💰 D-DSKFMT carve 30 (2026-11-10): the pair stood at fifteen sites.
clr_f306:
                xor     a
                ld      ($F306), a
                ret
                ds      $546E - $, $00
; 🔴 D-AUXIN (2026-11-08): $546E IS BDOS $03 AUXIN's CANONICAL ENTRY. MSX-DOS's kernel
; CALLs it (disk_probe_diff.py callwatch --in-func 0x03, our ROM's PCs:
; scratchpad/auxin_callwatch.out) -- and it landed 6 bytes into cr_copy1 (D-AUXOUT had
; moved it into this span), which ran cr_copy2 and fat_dir_update: AUXIN went to the
; directory-entry writer. (Before that it landed in lstout_body.) No AUX device on
; either target; the CF-3300 answers A = $1A, CP/M's end of file
; (probes/disk/auxinx.asm, scratchpad/auxinx_both1.out).
k_546E:
auxin_body:
                ld      a, $1A
                ret
                ds      $5474 - $, $00
; 🔴 D-AUXOUT (2026-11-08): $5474 IS BDOS $04 AUXOUT's CANONICAL ENTRY. MSX-DOS's
; kernel CALLs it (disk_probe_diff.py callwatch --in-func 0x04, our ROM's PCs:
; scratchpad/auxout_callwatch.out), and it fell 12 bytes into lstout_body -- which
; then ran into our page-3 bridge and the program never came back (the D-WRZER
; class). No AUX device exists on either target; the CF-3300 answered E = 'A' with
; A = $41 (scratchpad/bdosx9_stock.out record 10): the char comes back in A.
k_5474:
auxout_body:
                ld      a, e
                ret
lstout_body:
                ld      a, e                ; char arrives in E ($5465 LSTOUT contract)
                ld      (CONOUT_CHAR), a    ; stash across the slot work (A needed for paging)
                push    bc
                push    de
                push    hl
                di                          ; no interrupt while the BIOS is half-mapped
                call    br_mainin_tmpl
                ld      a, (CONOUT_CHAR)
                call    $00A5               ; LPTOUT - output A to the list device
                call    pg0_mainrom_out     ; restore page 0
                ei
                pop     hl
                pop     de
                pop     bc
                ret

; ===== Tier-2 3b: relocated Disk-BASIC routine bodies ($5456-$5FE4 gap) =====
; Each colliding routine's body lives here; its low-region slot holds `entry:
; jp entry_body` + the ds-anchored veneer(s) + padding, sized to exactly fill the
; original span (net-zero — nothing downstream shifts). Each body ends with a `jp`
; back to the label that followed it, so fall-through is preserved. Callers reach
; the routine through its unchanged low-region entry label.
; (fat_find_body relocated to runtime.asm, M22a — its old span here collided
; with the $5462 DIRIN canonical entry above, tier2-m22-cpmver-spec.md §4.6.)

; (callf_body_body, the $0030 CALLF handler, was here: D-TPAP1 moved the page-0
; entries to page-3 bridges into the main BIOS -- br1_tmpl, pageenv.asm.)

; br3 (D-BOOTREGS) -- the fourth piece of the page-3 slot bridges, laid after the hook
; bodies: $0005 until MSXDOS.SYS lays its own. A boot code may put RAM into page 1
; ($F368+3) and then call the BDOS -- frayd1.dsk's does, through $F37D, and the
; CF-3300 answers it (scratchpad/fray_f37d.out). So: the disk ROM in for bdos_entry,
; and the caller's page 1 (RAM, subslot 0, or this ROM) back after.
; boot_call0 -- step 5: the boot sector's code at $C01E with Cy = 0 ("custom boot"),
; under the DOS_TRY mark. 🔴 D-BASENTRUN's other half (2026-10-22): a boot loader may
; fall back to $4022 from THIS call too, not only from dos_handoff's -- facdemo5's
; does, and the references ask the date. Without the mark, $4022 took it for a
; running program and asked nothing (scratchpad/diskcorpus_rerun2.out). The same
; window as dos_handoff's: the mark as it was comes back if the code returns.
; Sited here (the old CALLF handler's hole) by D-BOOTREGS, whose DE / HL overflowed the
; driver.asm fill it had moved to from init.asm.
boot_call0:
                ld      hl, (BASENT_REQ)
                push    hl
                ld      hl, DOS_TRY
                ld      (BASENT_REQ), hl
                ld      de, $F368       ; D-BOOTREGS: DE / HL as the CF-3300 hands them
                ld      hl, $F323       ; to the boot code at both steps (fray_c01e.out)
                or      a               ; Cy = 0 -> step-5 "custom boot" call
                ; the boot sector's step-5 call, as at hsy_boot
                ; z80flow: targets external reads=DE,HL,F
                call    BOOT_ENTRY      ; (data-disk default RET NC returns here)
                pop     hl
                ld      (BASENT_REQ), hl
                ret
br3_tmpl:
br_bdos_tmpl:
                push    af
                ld      a, (SLTTBL3)
                and     $0C                 ; the caller's page-1 subslot
                ld      (BR_P1SV), a
                pop     af
                call    WA_SEG_ROM          ; register-transparent
                call    bdos_entry
                push    af
                push    bc
                ld      a, (BR_P1SV)
                jp      WA_SEG_SET          ; the caller's page-1 subslot back, exactly: its pop bc /
                                            ; pop af / ret return for us. D-DISKMSX2HOST: was "RAM again
                                            ; iff the saved bits were 0" -- RAM is subslot 2 on MSX2
                ds      2, 0                ; (the template keeps its 10 B: BR_P1SV stays put)
br_p1sv_tmpl:   db      0
br3_end:

; --- S10.D: the RANDOM record engine's multiply (hk_dget). Moved from below the $7F7C
; pin (runtime.asm) by D-TPAP1, then from the $4E7E hole by D-BOOTREGS: both needed room.

; mul_reclen -- A:HL := HL * GP_RECLEN, a 24-bit product (D-PUTU32: k is 0..65535
; and reclen 1..256, so the product is < 2^24). Two cases, because 256 does not
; fit an 8-bit multiplier: reclen == 256 is k << 8, moved by bytes; reclen < 256
; is 8 MSB-first steps with k FIXED in BC and the product doubling in A:HL.
; Clobbers BC, DE (the sub-ROM copy kept BC; no caller here needs it).
mul_reclen:
                ld      b, h
                ld      c, l                ; BC = k
                ld      de, (GP_RECLEN)
                ld      a, d
                or      a
                jr      z, mr_lt256
                ld      a, b                ; reclen == 256: A:HL = k << 8
                ld      h, c
                ld      l, e                ; 💰 D-AUXOUT: E = 0 here (DE = 256), 1 B
                ret                         ; under `ld l, 0`, for the $5474 veneer's room
mr_lt256:                                   ; A = 0; E = reclen (1..255)
                ld      h, a
                ld      l, a
                ld      d, 8
mr_loop:
                add     hl, hl
                rla                         ; A:HL <<= 1
                sla     e                   ; reclen's top bit -> CF
                jr      nc, mr_skip
                add     hl, bc
                adc     a, 0                ; A:HL += k
mr_skip:
                dec     d
                jr      nz, mr_loop
                ret

bdos_seqread_body:                      ; [bdos_seqread, bsr_have) -> falls into bsr_have
                ; EOF once every file byte has been delivered (BYTESLEFT == 0).
                ld      hl, (BDOS_BYTESLEFT)        ; low word
                ld      de, (BDOS_BYTESLEFT + 2)    ; high word
                ld      a, h
                or      l
                or      d
                or      e
                jp      z, bsr_eof                  ; no bytes left -> EOF (jr->jp: relocated)
                ld      a, (BDOS_RECIDX)
                cp      RECPERSEC
                jp      c, bsr_have     ; records still left in SECTOR_BUF (jr->jp)
                call    fat_read_file_sector
                jp      c, bsr_eof      ; chain ended early -> EOF (jr->jp)
                xor     a
                ld      (BDOS_RECIDX), a    ; back to record 0
                jp      bsr_have

bdos_create_body:                       ; [bdos_create, bdos_create_failpop) — ends in ret
                push    de              ; FCB across fat_mount
                call    fat_mount
                jp      c, bdos_create_failpop      ; jr->jp: relocated
                pop     hl              ; HL = FCB
                call    fcb_clear_tail  ; D-WRBLKSEEK: +16..31 := 0 (HL kept)
                push    hl
                inc     hl              ; HL = FCB+1 = 8.3 name
                call    fat_dir_create  ; find/make a dir slot, write the entry
                pop     hl              ; HL = FCB (pop leaves the flags alone)
                jp      c, bdos_create_err          ; jr->jp: relocated
                call    fmake_fcb_fill  ; D-WRBLKSEEK (b): +20..21, +24, +25 as stock
                ; prime the write iterator: no cluster yet, empty buffer, 0 bytes.
                call    wr_reset            ; D-WRRESET: no cluster, empty buffer, 0 bytes (HL = 0)
                ; Item 5 (spec-diskbasic-option-closure.md): reset the per-open
                ; sequential record counter for the WRITE session too, mirroring
                ; the read-open reset in fopen_fill_body (fat.asm; M33 §2.1). A file
                ; is opened read XOR write, so BDOS_SEQREC is the single per-open
                ; counter shared by wrseq_writeback (read) and wrseq_wr_writeback
                ; (write); write-open must zero it or a prior read session's count
                ; would poison the first WRSEQ position.
                ld      (BDOS_SEQREC), hl
                ld      a, 1
                ld      (BDOS_WRMODE), a    ; file is open for write
                xor     a                   ; A = $00 success
                ret


; --- MSX-DOS-1 kernel _GDATE entry: $553C ($4000 + $153C; M9 date slice) ----
; The kernel BDOS dispatcher ($D831; table $D8BE + 3*C) routes _GDATE ($2A) here
; after paging in the disk ROM via the $F368 hook. CF-3300 and C-BIOS are
; clock-less MSX1, so _GDATE returns the documented MSX-DOS-1 default date
; 1984-01-01 (a Sunday) -- a constant, NOT a reproduction of stock's stored
; day-count -> Y/M/D math. Return contract (observed black-box at the GDATE
; return $CC04): HL=year, D=month, E=day, A=day-of-week(0=Sun), BC=0, F=$44.
; `xor a` yields A=$00 and F=$44 (Z|P/V) exactly; the preceding loads do not
; disturb the flags. CLEAN-ROOM: documented default + black-box return contract;
; no stock bytes. $553C fell inside fac_loop_body; that body is relocated just
; below (label-referenced via fat.asm `jp fac_loop_body`; net-zero -- the tail
; `ds $5FE5 - $` absorbs the shift). See docs/tier2-gdate-spec.md.
; (moved here from runtime.asm by the D-DSKFMT defrag)
; (moved here from kernel.asm by the D-DSKFMT defrag)
; 🎯 D-DISKRUN mb.dsk (Joost 2026-10-27: "Start BASIC (NMS 1200)"): $5E10 IS PINNED.
; mb.dsk's boot code CALLs this fixed address in its step-5 call (scratchpad/mb_5e10.out,
; DE = $7368, HL = $E646 on both machines). On the VG-8020 + NMS 1200 it starts BASIC
; and AUTOEXEC.BAS runs, with no date question (scratchpad/mbtime_run.out: the banner
; at T6, AUTOEXEC.BAS's own `Illegal function call in 20` by T9); the CF-3300 hangs
; there. Here $5E10 was the middle of basent_body and the machine rebooted for ever.
; It is basent_body itself: under boot_call0's DOS_TRY mark that is the boot-loader
; restart ('B','D'), which skips the DOS boot and asks the date only when the disk has
; no AUTOEXEC.BAS -- mb.dsk has one. A pad, then the jump: no new behaviour.
; cr_copy1 / cr_copy2 -- D-WRBLKDIR's third part: fat_dir_update writes FWR_FIRST /
; FWR_BYTES (BDOS_WRFIRST / BDOS_WRBYTES), which still hold the LAST-WRITTEN file's
; values at a close -- so the closed FCB's (wrblk_fcbstate loaded them into
; FAT_FIRSTCLUS / FAT_FILESIZE) go there first. 🔴 Without it the first run gave ALT1
; ALT2's cluster (wrblkalt-acceptance). Split across two holes, for room; cr_copy1 is
; here since D-AUXIN, which needed $546E -- inside cr_copy1's old span -- for AUXIN.
; Entered only by `call` (cr_dir).
cr_copy1:
                ld      hl, (FAT_FIRSTCLUS)
                ld      (BDOS_WRFIRST), hl
                jp      cr_copy2            ; disk/runtime.asm
; fdc_wp_chkrdy -- the write path's NOT READY test, CY / A = 2 or NC (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
fdc_wp_chkrdy:
                ld      a, (FDC_STATUS)
                and     ST_NOTRDY
                ret     z
                ld      a, 2
                scf
                ret
                ds      $553C - $, $00      ; pad to the canonical _GDATE entry
gdate_handler:
                jp      gdate_body          ; D-DOSDATE: the STORED date, not a constant
                ; --- MSX-DOS-1 kernel _SDATE entry: $5552 (D-DOSDATE, 2026-10-01) ------
                ; Measured, not assumed: the first page-1 PC during SDATE on ours is
                ; $5552 (scratchpad/sdatemem.out). It was zero padding here, so ours
                ; NOP-slid into the pinned `jp gtime_body` at $55DB and returned A = 0
                ; for every date, valid or not.
; 📍 D-LOADRUN (2026-11-06): MOVED HERE from the $782B..$7F7C region, which LOAD's
; hdl_run outgrew by 48 B; entered only by call / jp, so it can live in any hole.
; install_f365 — copies f365_iord_tmpl to the fixed address $F365. Lives in the
; free tail (no budget limit) and is reached by a single 3-byte `call` from
; build_resident's cramped pre-$41FD path (see init.asm) instead of inlining the
; ld/ld/ld/ldir sequence there, which overflows that region's canonical-address
; budget (M15 §7.1/§7.2: verified — an inline second copy there makes 3-pass/
; symbol-table assembly silently emit an empty object file, no error printed).
install_f365:
                ld      hl, f365_iord_tmpl
                ld      de, F365_STUB
                ld      bc, f365_iord_end - f365_iord_tmpl
                ldir
                ret
; (setdta_cache_body -- its contract is in the comment block at the old site, the free
; tail; moved here by D-SFIRSTVOL, whose skip needed the bytes there.)
setdta_cache_body:
                ld      (DOS_DTAPTR), de    ; $F23D := DE (runtime DTA pointer)
                ret
                ds      $5552 - $, $00
k_5552:         jp      sdate_body
; (D-BDOSFCB stage C, 2026-11-10: bb_delete / bb_rename / bb_name11, the $F37D BDOS's
; own DELETE and RENAME, are gone -- bdos_tab sends $13 / $17 to the kernel's fdel_body
; and fren_wild, which take the same DE = FCB and match every entry as the CF-3300:
; a '?' in RENAME's new name keeps the entry's own byte, where bb_rename copied it.)

; --- MSX-DOS-1 kernel GTIME/STIME/VERIFY entries: $55DB/$55E6/$55FF (M22a) ----
; While processing BDOS GTIME ($2C) / STIME ($2D) / VERIFY ($2E) the relocated
; kernel CALLs these page-1 disk-ROM entries. All three fell inside the
; fdc_entloop_body/fdc_useslot_body span (now relocated above): STIME executed
; FDC-flavored retry-loop code (wrong exit registers), and GTIME's walk ended in
; a WARM BOOT — killing whatever program was running (the M22a root cause;
; tier2-m22-cpmver-spec.md §4.2). Bodies (gtime_body/stime_body/verify_body,
; free tail) reproduce the pinned oracle contracts (§5.1); none clear $F306 —
; every exit HL is the kernel's own H:=B,L:=A mirror (§5.2, the M20 rule
; inverted for this tier).
; CLEAN-ROOM: published GTIME/STIME/VERIFY contracts (map.grauw.nl, MSX2 TH
; §BDOS) + the black-box-pinned oracle register buffer; no stock CODE decoded.
; 📍 D-RECBIG (2026-11-04): frnd_off and rnd_tab, out of the $5602..$5E10 fill.
; frnd_off -- A:HL := k * GP_RECLEN, k the row's +33..36 (= n - 1 during the I/O,
; rnd_loc); CF = 1 when the product passes 2^24. k's low word goes through
; mul_reclen (exact: < 2^24); its third byte adds k2 * reclen into A, which must
; not carry. IX = the row. Clobbers BC, DE.
frnd_off:
                ld      a, (ix+36)
                or      a
                scf
                ret     nz                  ; k >= 2^24
                ld      l, (ix+33)
                ld      h, (ix+34)
                call    mul_reclen          ; A:HL = k[0..15] * reclen
                ld      b, (ix+35)
                inc     b
                dec     b
                ret     z                   ; k < 2^16: done (CF = 0, mul_reclen's)
                ld      de, (GP_RECLEN)
                dec     d
                scf
                ret     z                   ; reclen 256 and k >= 2^16: past 2^24
fof_add:
                add     a, e
                ret     c                   ; past 2^24
                djnz    fof_add
                ret                         ; CF = 0

; rnd_tab -- HL = DE + 2*A. Clobbers A.
rnd_tab:
                add     a, a
                ld      l, a
                ld      h, 0
                add     hl, de
                ret

; choice_txt -- CHOICE's menu (disk/driver.asm `choice`): the CF-3300's text, 82 bytes
; with the $00. D-DSKFMT, 2026-11-10.
choice_txt:
                db      13, 10, "1 - 1 side"
                db      13, 10, "2 - 2 sides"
                db      13, 10, "3 - 1 side, double track"
                db      13, 10, "4 - 2 sides,double track"
                db      13, 10, 13, 10, 0
; srd_size0 -- D-BDOSUNOP (2026-11-11): Z when the FCB in IX holds a file size of 0
; (+16..19). SEQREAD of an FCB never opened answers 1 (end of file) on the CF-3300;
; ours read a record from the iterator's leftovers (scratchpad/bdosunop_run4.out).
srd_size0:
                ld      a, (ix+16)
                or      (ix+17)
                or      (ix+18)
                or      (ix+19)
                ret
                ds      $55DB - $, $00  ; pad up to the pinned $55DB GTIME entry (M22a)
k_55DB:
                jp      gtime_body      ; $55DB: BDOS $2C GTIME canonical entry (M22a)
; (seldsk_drv_body / curdrv_body -- its contract is in the comment block at the old site, the free
; tail; moved here by D-SFIRSTVOL, whose skip needed the bytes there.)
seldsk_drv_body:
                jp      seldsk_set      ; D-DOSDRVB: the body is in the pin-free span
curdrv_body:
                ld      a, (CURDRV_CELL) ; CURDRV_CELL = $F247 = current-drive index ($00)
                ret
                ds      $55E6 - $, $00  ; pad up to the pinned $55E6 STIME entry (M22a)
k_55E6:
                jp      stime_body      ; $55E6: BDOS $2D STIME canonical entry (M22a)
; 📍 D-COPYALL (2026-11-06): MOVED HERE from the $5602..$5E10 fill, which COPY's
; every-match walk outgrew by 56 B; entered only by call / jp, so it can live
; in any hole.
; dout_row -- A = channel -> HL = its engine row. Clobbers A, DE.
dout_row:
                ld      hl, TXTMAX-(FCH_CEIL+1)*FCH_STATESZ
                ld      de, FCH_STATESZ
drw_lp:
                add     hl, de
                dec     a
                jr      nz, drw_lp
                ret
; verify_body -- BDOS $2E VERIFY (M22a): A = E. Moved here from disk/runtime.asm's
; $782B..$7F7C region (D-DISKCARVE, 2026-11-07); this pad was empty and never
; entered (scratchpad/padwatch_4ede.out).
; (verify_body moved in front of $75A5 by D-VERIFYREAD: it keeps the flag now, and this pad was full)
; (wrseq_body_write moved here by D-RNDFCB from the $7F7C region; entered only by
; driver.asm wrseq_body's `jp nz`.)
; (moved here from runtime.asm by the D-DSKFMT defrag)
cr_copy2:                                   ; (cr_copy1's second half, disk/kernel.asm)
                ld      hl, FAT_FILESIZE
                call    to_wrbytes
                jp      fat_dir_update
                ds      $55FF - $, $00  ; pad up to the pinned $55FF VERIFY entry (M22a)
k_55FF:
                jp      verify_body     ; $55FF: BDOS $2E VERIFY canonical entry (M22a)

; ===== Tier-2: COMMAND.COM-load kernel entries — veneer scaffold (milestone 3a) =====
; The relocated MSX-DOS-1 kernel + COMMAND.COM call back into ~21 disk-ROM entry
; points at fixed page-1 addresses (the de-facto-standard shared-kernel ABI;
; spec-diskrom-kernel.md §5). zerobas exposes each as a 3-byte `jp k_XXXX` veneer
; at its canonical address, with the contract body in this free-tail region.
; This 3a pass places the FIVE free-region entries (no relocation needed); $402D
; (in the $4022-$402F pad) is the sixth non-colliding entry, wired above. Bodies
; are register-preserving stubs for now — our ROM does not yet reach the
; COMMAND.COM-load phase — so this scaffold changes no current behaviour; the 14
; entries that collide with active code arrive in the 3b relocation pass, and
; milestone 4 fills each contract. CLEAN-ROOM: exposing ABI entry points + own
; contract code, never shared-kernel bytes — same legitimacy class as the $4010
; BIOS jump table. Veneer safety confirmed by disk_probe_dosboot_veneer.py.
; --- hk_mki: MKI$'s float coercion (D-MKIRANGE; own code, spec-basic-mksd.md) ---
; Sources: the MK$ contract of docs/spec-basic-mksd.md; the DAC float layout as
; documented in the MSX2 Technical Handbook's Math-Pack section; the range and
; truncation rules are black-box CF-3300 oracle readings. Our own code.
; MKI$ packs main's DE, which `eval` leaves as the argument truncated to 16 bits
; -- right for an INTEGER, and wrong at the edges for a FLOAT: `MKI$(32768)` gave
; -32768 here and Overflow on the CF-3300, `MKI$(-32769)` 32767 against Overflow,
; and `MKI$(-32768.4)` 32767 against -32768 (scratchpad/mkirange_run.out). The
; reference TRUNCATES toward zero (1.5 -> 1, -1.5 -> -1, measured to agree) and
; overflows outside -32768..32767. So for a float this recomputes the value from
; FAC's BCD digits (sign|exp-64, then two digits a byte, high nibble first) and
; stores it over main's DE in STRSCR+1; out of range defers Overflow through
; FPERR (1 -> ERR 6), first error wins, as every other factor refusal does.
; Everything crosses in RAM (hk_mkfloat's ABI). 📍 In the $5602 fill: the tail
; is full.
hk_mki:
                ld      a,(FACTYP)
                cp      2
                jr      z,hkmi_done         ; an INTEGER: main's DE is already right
                call    fac_mag             ; HL = |x| truncated, CF = past 65535
                jr      c,hkmi_ovf
                ld      a,(FAC)
                rla                         ; CF = the sign
                jr      c,hkmi_neg
                bit     7,h                 ; positive: at most 32767
                jr      nz,hkmi_ovf
                jr      hkmi_store
hkmi_neg:
                xor     a                   ; HL = -HL
                sub     l
                ld      l,a
                sbc     a,a
                sub     h
                ld      h,a                 ; (A = H)
                ; negative: magnitude at most 32768. D-DISKREREAD (2026-11-26):
                ; negate FIRST, then check -- |x| 1..32768 is $FFFF..$8000 (bit 15
                ; set), 0 stays 0, 32769..65535 is $7FFF..$0001: overflow; 4 B
                bit     7,h
                jr      nz,hkmi_store
                or      l
                jr      nz,hkmi_ovf
hkmi_store:
                ld      (STRSCR+1),hl       ; low byte then high: MKI$'s packed form
hkmi_done:
                scf                         ; claimed
                ret
hkmi_ovf:
                ld      a,(FPERR)
                or      a
                jr      nz,hkmi_zero        ; an earlier error wins
                inc     a
                ld      (FPERR),a           ; 1 = Overflow -> ERR 6, deferred
hkmi_zero:
                ld      hl,0
                jr      hkmi_store

; fac_mag32 -- DE:HL = |FAC| TRUNCATED toward zero, for a FLOAT FAC: the integer
; digits of FAC's BCD (sign|exp-64, then two digits a byte, high nibble first).
; CF = 1 when the magnitude passes 2^32 - 1 (11+ integer digits, or a carry out
; of DE:HL*10+d); |x| < 1 (zero included) is 0, CF = 0. Split out of hk_mki
; (D-RECBIG, 2026-11-04) and widened for GET / PUT's 32-bit record number
; (rnd_parse; the CF-3300's record is 1 .. 2^32-1, measured 2026-11-04). The digit
; cursor lives in the ALTERNATE set (HL' the byte, C' the nibble, B' the count).
; Clobbers A, BC, BC', DE', HL'.
fac_mag32:
                ld      hl,0
                ld      d,h
                ld      e,l
                ld      a,(FAC)
                and     $7F
                sub     $41                 ; A = integer digits - 1
                ccf
                ret     nc                  ; |x| < 1 (zero included): 0, CF = 0
                cp      10
                ccf
                ret     c                   ; 11+ integer digits: out of range
                inc     a
                exx
                ld      b,a                 ; B' = digits to take
                ld      hl,FAC+1
                ld      c,0                 ; C' bit 0 clear = high nibble next
fmg_dig:
                ld      a,(hl)
                bit     0,c
                jr      nz,fmg_lo
                rrca
                rrca
                rrca
                rrca
                jr      fmg_nib
fmg_lo:
                inc     hl                  ; low nibble: this byte is done
fmg_nib:
                inc     c
                and     $0F
                exx
                push    de
                push    hl                  ; x
                add     hl,hl
                rl      e
                rl      d
                jr      c,fmg_ovf2
                add     hl,hl
                rl      e
                rl      d                   ; 4x
                jr      c,fmg_ovf2
                pop     bc
                add     hl,bc
                ex      de,hl
                pop     bc
                adc     hl,bc
                ex      de,hl               ; 5x
                jr      c,fmg_ovf
                add     hl,hl
                rl      e
                rl      d                   ; 10x
                jr      c,fmg_ovf
                ld      c,a
                ld      b,0
                add     hl,bc               ; + the digit
                jr      nc,fmg_next
                inc     de
                ld      a,d
                or      e
                jr      z,fmg_ovf           ; a carry out of the top
fmg_next:
                exx
                djnz    fmg_dig
                exx
                or      a                   ; CF = 0
                ret
fmg_ovf2:
                pop     bc
                pop     bc                  ; x, discarded
fmg_ovf:
                scf
                ret

; --- hkc_body: COPY's sector loop, IN THIS ROM ------------------------------
; 📍 SITED IN THE $5602..$5FE4 FILL with hkc_resolve (D-DSKFLOCAL, 2026-09-29):
; the tail after runtime.asm is full, and fat_count_free's local body needed
; its room. Entered by hk_copy's absolute `call`, so it can live anywhere.
; D-COPYLOCAL (disk/docs/spec-diskcode-eviction.md §6.6az). The `tnt_copy` body
; verbatim, less its DISKOP_STATUS store; every routine it calls was ALREADY in
; this assembly (basic/fat-prim-body.inc, included above), and every write-cursor
; cell it uses -- FWR_SECIDX / FWR_CLUS / FWR_FIRST / FWR_BYTES / FWR_BUFLEN --
; was already declared disk-local in disk/equates.inc. Only COPY's own three
; cells had to be published.
; Sources: our own FAT12 engine and our own COPY semantics (basic/PROVENANCE.md
; §COPY, docs/spec-basic-copy.md §3); the refusals are black-box readings on the
; CF-3300, not decoded code.
;
;   in : COPY_SRC = the source 8.3 name, DISK_FCB_NAME = the destination
;   out: A = 0 source not found (ERR 53) / 1 copied / 2 mount, full or I-O
;        (load_error) / 3 refused (ERR 5)
;
; The order is forced and each step says by what: mount; hand a wildcard source
; to hkc_resolve, which runs this same path once per match (D-COPYALL; it used
; to be refused with ERR 5); refuse a self-copy (measured, ERR 5); `fat_find`
; the SOURCE FIRST --
; it records the entry location in FWR_DIRSEC/FWR_DIROFF, which the destination's
; create must own afterwards -- and stash its first cluster and size, because the
; destination's delete and create clobber FAT_FIRSTCLUS/FAT_FILESIZE through
; fat_find; delete an old destination (the reference overwrites); create the new
; one and reset the write cursor exactly as fat_io_create does; reopen the
; source; then per sector read, clamp to what is left, and flush. The read
; iterator (FAT_CURCLUS/FAT_CLUSSEC) and the write cursor (FWR_*) are disjoint
; cells; FAT_DBUF is the one shared buffer and that is the point.
; 🔴 `FAT_DBUF`, NOT `FSECTOR_BUF`: the sub-ROM body's comment named the
; per-ROM spelling, which in THIS ROM is main's buffer at $E5C0 rather than ours
; at $E2A0 (§6.6c) -- one address since D-BUFMERGE, the spelling stays neutral.
; The primitives themselves are already neutral (§6.6ax).
hkc_body:
                call    fat_mount
                jp      c,hkc_io            ; A = 2
                ; 🟢 D-COPYWILD (2026-09-28): a '?' SOURCE is RESOLVED, no longer
                ; refused. Measured on the CF-3300 (scratchpad/copywild_run6.out):
                ; ONE match copies (`A1.*` / `A1.T?T`), NO match is 53.
                ; 🟢 D-COPYALL (2026-11-06, Joost: "copy every match like the
                ; reference"): two or more copy EVERY match -- hkc_resolve walks the
                ; directory live (scratchpad/copymulti_run1..4.out). The old line here
                ; said "the reference never returns there"; it did, ~10 s later, and
                ; the probe had stopped looking at 8 s.
                ld      hl,COPY_SRC
                ld      b,11
hkc_wild:       ld      a,(hl)
                cp      '?'                 ; pdfcb turns '*' into '?'s too
                jp      z,hkc_resolve
                inc     hl
                djnz    hkc_wild
hkc_fill:
                ; ...and every '?' in the DESTINATION takes the source's character
                ; at that position -- the CF-3300 does it for a PLAIN source too:
                ; `COPY "A1.TXT" TO "H?.TXT"` makes H1.TXT, where this made a file
                ; literally named `H?.TXT` (copywild_run6.out s_plainq).
                ld      hl,COPY_SRC
                ld      de,DISK_FCB_NAME
                ld      b,11
hkc_df:         ld      a,(de)
                cp      '?'
                jr      nz,hkc_dk
                ld      a,(hl)
                ld      (de),a
hkc_dk:         inc     hl
                inc     de
                djnz    hkc_df
                ld      b,11                ; D-DISKREREAD (2026-11-26): compare BACKWARDS
                                            ; from where hkc_df left HL / DE (+11 each) --
                                            ; "all 11 equal" has no order; 6 B
hkc_same:       dec     hl
                dec     de
                ld      a,(de)              ; CASE-FOLDED (OPEN twice, 2026-10-28): the
                call    toupper_cmp
                jr      nz,hkc_differ
                djnz    hkc_same
                jp      hkc_ill             ; all 11 bytes equal: a self-copy
hkc_differ:     ld      hl,COPY_SRC
                call    fat_find
                jp      c,hkc_nf            ; A = 0
                ld      de,22               ; D-COPYDATE: keep the SOURCE's time/date
                add     hl,de               ; (+22..25) for the destination
                ld      de,COPY_STAMP
                call    copy4               ; (its ldir leaves B = 0)
                ld      hl,FAT_FIRSTCLUS    ; D-DISKREREAD (2026-11-26): first cluster +
                ld      de,COPY_CLUS        ; 32-bit size in ONE block copy (both pairs
                ld      c,6                 ; are 2 + 4 contiguous -- guarded below),
                ldir                        ; not three word copies; 8 B
    IF (FAT_FILESIZE - FAT_FIRSTCLUS - 2) or (COPY_LEFT - COPY_CLUS - 2)
                db      HKC_BLOCK_COPY_NEEDS_CLUS_THEN_SIZE_CONTIGUOUS
    ENDIF
                call    fat_delete          ; an old destination; Cy = 1 "none" is fine
                ld      hl,DISK_FCB_NAME
                call    fat_dir_create
                jp      c,hkc_io
                call    wr_reset            ; D-WRRESET (+ FWR_BUFLEN := 0: a fresh destination)
                ld      hl,(COPY_CLUS)
                ld      (FAT_FIRSTCLUS),hl
                call    fat_open
hkc_loop:
                ld      hl,(COPY_LEFT)
                ld      a,(COPY_LEFT+2)
                or      h
                or      l
                jr      z,hkc_fin
                call    fat_read_file_sector    ; -> FAT_DBUF
                jr      c,hkc_fin           ; chain ended before the size did:
                                            ; stamp what arrived
                ld      de,512
                ld      hl,(COPY_LEFT)
                ld      a,(COPY_LEFT+2)
                or      a
                jr      nz,hkc_n            ; >= 65536 left: a whole sector
                or      a
                sbc     hl,de
                jr      nc,hkc_n            ; >= 512 left: a whole sector
                ld      de,(COPY_LEFT)      ; the tail: n = left
hkc_n:          ld      (FWR_BUFLEN),de
                ld      hl,(COPY_LEFT)      ; left -= n
                or      a
                sbc     hl,de
                ld      (COPY_LEFT),hl
                jr      nc,hkc_nb
                ld      hl,COPY_LEFT+2
                dec     (hl)
hkc_nb:         ld      hl,(FWR_BYTES)      ; FWR_BYTES += n
                add     hl,de
                ld      (FWR_BYTES),hl
                jr      nc,hkc_fl
                ld      hl,FWR_BYTES+2
                inc     (hl)
hkc_fl:         call    fat_flush_data_sector   ; allocates/extends, writes FAT_DBUF
                jp      c,hkc_io
                jr      hkc_loop
hkc_fin:        call    fat_dir_update      ; true size + first cluster
                jp      c,hkc_io
                ; 🔴 D-COPYDATE (2026-10-01): the CF-3300's COPY carries the SOURCE's
                ; time/date to the destination (disk_probe_savedate.py: a source dated
                ; 0 copies as 0), where fat_dir_update just stamped 0821h. Patch the
                ; entry in the buffer fat_dir_update wrote, and write it once more.
                ld      hl,(FWR_DIROFF)
                ld      de,FAT_MBUF+22
                add     hl,de
                ex      de,hl               ; DE -> the entry's +22
                ld      hl,COPY_STAMP
                call    copy4
                ld      de,(FWR_DIRSEC)
                ld      hl,FAT_MBUF
                call    write_sector
                jp      c,hkc_io
                ld      a,1                 ; copied
                ret

; hkc_resolve -- D-COPYALL (2026-11-06): copy EVERY root entry the '?' source
; matches, the way the CF-3300 does (scratchpad/copymulti_run1..4.out, Fable
; agent, 30 cases; Joost's ruling of the same day). The reference walks the root
; directory in slot order and copies each match through the ordinary one-file
; path; the walk is LIVE, so a destination appended AFTER the cursor is visited
; again, and when its name also matches the source pattern the per-position fill
; maps it onto itself -- the self-copy refusal, `Illegal function call`, AFTER
; every real copy (`*.BAS` -> `Z.BAS`, `*.*` -> `N*.*`). A destination that lands
; in a freed slot BEFORE the cursor is never revisited and the same statement
; ends without an error (copymulti_run4.out sm_slot). Both fall out of this walk
; with no special case: it re-reads the directory from the top after each copy
; and takes the first match at or after `next`.
; No new RAM: the two patterns, the cursor and the found flag live in a 24-byte
; frame on the stack for the length of the statement.
;   frame +0..10 the source pattern, +11..21 the destination pattern (as typed,
;   restored before each copy: hkc_fill fills it in place), +22 next slot, +23 1
;   once anything matched (0 at the end -> 53, A = 0).
; Out (to hk_copy, via the frame-freeing ret): A = 0 no match / 1 all copied /
; 2 mount, full or I-O / 3 refused (ERR 5) -- hkc_body's own contract.
; 📍 SITED IN THE $5602..$5FE4 FILL, not beside hkc_body: the tail region after
; runtime.asm reached the ROM's end with D-NOASTO (an empty disk.rom), and code
; placed above `ds $5FE5 - $` shifts nothing downstream. Entered by `jp`, so it
; returns straight to hkc_body's caller.
hkc_resolve:
                ld      hl,-24
                add     hl,sp
                ld      sp,hl               ; SP = the frame
                ex      de,hl
                ld      hl,COPY_SRC
                call    ldir_11
                ld      hl,DISK_FCB_NAME
                ld      c,11
                ldir                        ; +11: the destination pattern
                xor     a
                ld      (de),a              ; +22: next slot = 0
                inc     de
                ld      (de),a              ; +23: nothing matched yet
hkm_walk:       call    fat_dir_start       ; DIRSEC / DIRREM: the whole root
                ld      c,0                 ; C = the slot index of HL's entry
hkm_sec:        call    dirrem_z
                jr      z,hkm_end           ; every root sector scanned
                push    bc
                ld      de,(FAT_DIRSEC)
                call    rd_dbuf
                pop     bc
                jr      c,hkm_io
                ld      hl,FAT_DBUF
                ld      b,16                ; 512 / 32 entries per sector
hkm_ent:        ld      a,(hl)
                or      a
                jr      z,hkm_end           ; $00 = end of directory
                cp      $E5
                jr      z,hkm_next          ; deleted entry
                push    hl
                ld      hl,22+2             ; frame +22, past the pushed HL
                add     hl,sp
                ld      a,c
                cp      (hl)
                pop     hl
                jr      c,hkm_next          ; before the cursor: already visited
                push    bc
                push    hl
                ld      hl,4                ; the frame, past BC and HL
                add     hl,sp
                ex      de,hl               ; DE = the source pattern
                pop     hl
                push    hl
                call    name_cmp            ; trashes A, BC, DE, HL
                ld      a,(hl)              ; on a MATCH name_cmp has walked all 11
                pop     hl                  ; bytes: HL = the attribute (+11); the
                pop     bc                  ; flags are name_cmp's
                jr      nz,hkm_next
                and     $18                 ; volume label / directory: skip
                jr      z,hkm_hit
hkm_next:       ld      de,32
                add     hl,de
                inc     c
                djnz    hkm_ent
                call    dir_nextsec         ; 💰 D-DISKCARVE6: FAT_DIRSEC += 1, FAT_DIRREM -= 1
                jr      hkm_sec
hkm_hit:        ld      a,c                 ; SP = the frame again
                inc     a
                push    hl
                ld      hl,22+2
                add     hl,sp
                ld      (hl),a              ; next = this slot + 1
                inc     hl
                ld      (hl),1              ; something matched
                pop     hl
                ld      de,COPY_SRC
                call    ldir_11
                ld      hl,11
                add     hl,sp
                ld      de,DISK_FCB_NAME
                ld      c,11                ; (B = 0 after the ldir)
                ldir                        ; the destination pattern, afresh
                call    hkc_fill            ; ONE copy: A = 1 ok / 2 / 3 / 0
                dec     a
                jr      z,hkm_walk          ; copied: on from the next slot
                inc     a
                jr      hkm_out             ; the first failure ends the statement
hkm_io:         ld      a,2
                jr      hkm_out
hkm_end:        ld      hl,23
                add     hl,sp
                ld      a,(hl)              ; 1 = copied, 0 = no match -> 53
hkm_out:        ld      hl,24
                add     hl,sp
                ld      sp,hl               ; free the frame (A, flags aside, kept)
                ret


; close_read -- FCLOSE of a file written since FOPEN / FMAKE (D-CLOSESTAMP, 2026-10-01;
; D-COPYDATE, 2026-11-25). The CF-3300 writes the FCB's OWN date and time (+20 / +22)
; into the entry at FCLOSE, and every write stamps the current date into the FCB first
; (probes/bios/spec/xbfile5.asm, scratchpad/XB5_cf.out: a date poked AFTER the write
; reaches the entry, one poked BEFORE it is overwritten by the write, an unwritten close
; writes nothing). So a plain write still dates the file today (D-CLOSESTAMP: FIXD.TXT,
; dated 0, FOPEN + WRRND + FCLOSE reads 0821h, probes/disk/disk_probe_dosdate.py), and
; COMMAND.COM's COPY -- which sets the source's date in the FCB after its last write --
; keeps it. The FCB says whether a write happened (+24's 40h cleared, fcb_mark_written).
; cr_chkdir has just written the entry's size and first cluster through fat_dir_update,
; whose sector is still in FAT_MBUF at FWR_DIROFF: the FCB's time and date go there and
; the sector goes back (fdir_wrback). One exit for all three ends, A := 0 - carry: the
; `add hl, de` of 24 to an FCB address leaves the carry clear for "not written", and
; cr_chkdir's failure and fdir_wrback's I/O error both come with it set.
;   in: DE = the FCB; out: A = $00 ok / $FF I/O error
close_read:
                ld      hl, 24
                add     hl, de              ; Cy = 0
                bit     6, (hl)
                jr      nz, cr_end          ; not written since FOPEN: nothing to do (A 0)
                call    cr_chkdir           ; D-BDOSUNOP: the entry must carry the FCB's name;
                                            ; then D-WRBLKDIR's size + first cluster; HL = +25
                jr      c, cr_end           ; (A $FF)
                ld      de, -3
                add     hl, de              ; HL = the FCB + 22 (its time)
                ex      de, hl
                ld      hl, (FWR_DIROFF)
                ld      bc, FAT_MBUF + 22
                add     hl, bc              ; HL = the entry + 22 (its time)
                ex      de, hl
                ldi
                ldi                         ; entry +22..23 := FCB +22..23, the time
                dec     hl
                dec     hl
                dec     hl
                dec     hl                  ; HL = the FCB + 20
                ldi
                ldi                         ; entry +24..25 := FCB +20..21, the date
                call    fdir_wrback         ; Cy = an I/O error
cr_end:
                sbc     a, a                ; A = $00, or $FF with the carry
                ret

; ===== S10.B (2026-10-06): an OUTPUT channel is an MSX-DOS FCB, in this ROM ==
; spec-diskcode-eviction.md §6.6bc. The CF-3300's channel is a record-size-1
; MSX-DOS FCB moved by 256 B block I/O: the FCB changes only at record
; boundaries, header +6 of the channel's 265 B block is the byte position, and
; the record is the block's +9 (S10.0). Ours is the same shape:
;   row  = DSK_ENGTAB + ch * FCH_STATESZ, main's per-channel engine row, which a
;          DOUT_MODE channel does not otherwise use. +0..36 the FCB (RS = 1, so
;          the random record +33..35 is the committed byte count), +DOUT_BLK
;          the block's address (main's fch_ctx_addr, cached once by
;          dout_open: the block cannot move while the channel is open; the
;          sub-ROM's t_fch_load re-cached it on every switch until S10.E),
;          +DOUT_FLG the flags.
;   block+6 = the position in the record; block+9.. = the record.
; A full record is written AT ONCE, so the FCB's size -- LOF -- moves at byte
; 256, 512, ... as on the CF-3300 (S10.B DESIGN INPUT 2), and a full disk fails
; PRINT# at the record that does not fit. The writer is main's own engine as
; this ROM assembles it (fat_flush_data_sector: allocate, link, write), with the
; channel's write cursor kept in the row between records -- NOT wrblk_body,
; whose re-mount / re-find / directory rewrite on every call made a one-record
; OPEN+PRINT#+CLOSE 17 sector operations against the old engine's 7
; (scratchpad/s10b_wip_measure.out). The directory is written once, at CLOSE.
; Row fields past DOUT_FLG are this ROM's alone: Main reaches these at H_CHOUT / H_CHCLOSE (a byte / a CLOSE) and the
; H_FOPEN selector FOPEN_SEL_MAKE (OPEN); it guarantees the live channel is a
; DOUT_MODE one before it calls the first two.
DOUT_FIRST      equ     DOUT_FLG+1      ; the file's first cluster (0 = none yet)
DOUT_CLUS       equ     DOUT_FLG+3      ; the cluster being filled
DOUT_SECIDX     equ     DOUT_FLG+5      ; the sector in it the next record goes to
DOUT_DIRSEC     equ     DOUT_FLG+6      ; the directory entry: its sector,
DOUT_DIROFF     equ     DOUT_FLG+8      ; and its offset in it
    IF DOUT_DIROFF+2 > FCH_STATESZ
                db      DOUT_ROW_FIELDS_OVERRUN_FCH_STATESZ
    ENDIF
; 📍 In the $5602..$5FE4 fill, beside hk_ochk / close_read.
; ⏱ INTERRUPTS ON AT ENTRY (`ei`), which hk_kill's note said a long body here
; would need. A hook is entered with them OFF; the driver's fdc_di_save /
; fdc_io_done restore the CALLER's state around each sector op, so with this
; the keyboard is scanned between sector ops, as it was when main's engine ran
; each FAT primitive as its own `di / CALSLT / ei` sub-ROM call. Hygiene, NOT a
; measured fix: it was added for diskbasic-acceptance's lost keys in the first
; (wrblk_body) build and MEASURED not to cure them -- that build's 17 sector
; ops a statement were the cause (TODO S10.B). Safe: the ISR is page 0's, and
; htimi_guard skips main's timer work while page 1 is not main.


; dout_live -- IX = the live channel's row. Clobbers A, DE, HL.
dout_live:
                ld      a, (FCH_ACTIVE)
                call    dout_row
                push    hl
                pop     ix
                ret

; dout_pos -- HL = &block+6, the record position. IX = the row. Clobbers A.
dout_pos:
                ld      l, (ix+DOUT_BLK)
                ld      h, (ix+DOUT_BLK+1)
                inc     hl
                inc     hl
                inc     hl
                inc     hl
                inc     hl
                inc     hl
                ret

; dout_open -- the FCB both OPENs build in the live channel's row: drive 0, the
; name, +12.. zero (block, size, random record, flags, cursor), RS = 1, the
; channel's block address (main's fch_ctx_addr) cached with position 0, then the
; disk mounted. IX = the row; CF = the mount failed.
dout_open:
                call    dout_live           ; IX = the row
                push    ix
                pop     de
                xor     a
                ld      (de), a             ; +0: the default drive
                inc     de
                ld      hl, DISK_FCB_NAME
                call    ldir_11
                ex      de, hl
                ld      b, DOUT_DIROFF+2-12 ; +12..: zero
dop_z:
                ld      (hl), a
                inc     hl
                djnz    dop_z
                inc     a
                ld      (ix+14), a          ; RS = 1: a record is a byte, so the
                                            ; random record +33 is a byte offset
                ld      a, (FCH_ACTIVE)
                ld      ix, fch_ctx_addr
                call    calbak              ; HL = the channel's 265 B block
                ei                          ; CALSLT returns DI (cbios slot.asm), and
                                            ; the mount and the directory work below
                                            ; must not run masked: the driver keeps
                                            ; the CALLER's state (fdc_di_save), so a
                                            ; DI here masked the whole OPEN and the
                                            ; keys typed meanwhile were lost
                push    hl
                call    dout_live           ; (calbak consumed IX)
                pop     hl
                ld      (ix+DOUT_BLK), l
                ld      (ix+DOUT_BLK+1), h
                call    dout_pos
                ld      (hl), 0             ; position 0
                jp      fat_mount

; dout_dirpos -- the directory entry fat_dir_create / fat_find just located
; (FWR_DIRSEC/FWR_DIROFF) into the row, for CLOSE.
dout_dirpos:
                ld      hl, (FWR_DIRSEC)
                ld      (ix+DOUT_DIRSEC), l
                ld      (ix+DOUT_DIRSEC+1), h
                ld      hl, (FWR_DIROFF)
                ld      (ix+DOUT_DIROFF), l
                ld      (ix+DOUT_DIROFF+1), h
                ret

; (dout_mirror -- GONE with S10.E, 2026-10-19. It copied the FCB's size (+16)
; and random record (+33) into main's FAT_FILESIZE / FWR_BYTES after every OPEN
; and record write, as the CF-3300's LOF and LOC read its FCB (scratchpad/
; applof_run.out: after OPEN FOR APPEND on a 6 B file LOF 6, LOC 0; once a record
; is written, 256 and 256). LOF and LOC now read the FCB itself, through H_CHIN
; ops 6 / 8 (disk/fat.asm din_lofloc), so nothing in main has to be kept current.)

; hk_fmake -- OPEN FOR OUTPUT (H_FOPEN, FOPEN_SEL_MAKE). FCH_ACTIVE is the
; channel being opened (OPEN stores it); DISK_FCB_NAME the parsed 8.3 name. Creates
; (or truncates) the file through the same fat_dir_create main's fat_io_create
; used. Status 0 = open; otherwise the code is pending in DISKOP_ERR (main's
; oo_fail raises it).
hk_fmake:
                ei
                ld      a, (FCH_ACTIVE)
                or      a
                jr      z, hk_asav          ; no channel: SAVE"name",A (below)
                call    dout_open
                jr      c, hfm_fail
hfm_create:                                 ; (hk_frnd's create joins here: D-DISKREREAD)
                push    ix
                pop     hl
                inc     hl
                call    fat_dir_create      ; HL = the 8.3 name; make or truncate
                jr      c, hfm_fail
hfm_ok:
                call    dout_dirpos         ; (S10.E: no LOF / LOC mirror -- they
hfm_zero:                                   ; (hk_asav ends here: D-DISKREREAD)
                xor     a                   ; read the FCB, din_lofloc)
                jp      hdl_answer          ; status 0, selector disarmed, claimed
hfm_fail:
                xor     a
                ld      (FOPEN_SEL), a      ; disarm, as hdl_answer does
                inc     a
                jp      hk_claim_status     ; status 1, the code pending

; hk_asav -- SAVE"name",A's create, S10 increment 3 (D-ASAVEDOUT). It arrives
; as an OPEN FOR OUTPUT (FOPEN_SEL_MAKE) with NO channel live: main parked the
; live one (fch_park), and OPEN itself always claims its channel first, so
; FCH_ACTIVE = 0 at hk_fmake is SAVE ,A and nothing else. The listing then
; streams through H_CHOUT and ends at H_CHCLOSE, whose no-channel arms below
; feed THIS ROM's sequential writer -- the fat_io_create / fat_io_putbyte /
; fat_io_close bodies hk_dpsave's engine already includes, buffering into
; FAT_DBUF a sector at a time exactly as main's resident copy did. So the file
; is the one main wrote, byte for byte (asavedev-acceptance), and main's
; write engine can go.
; 📍 NO SELECTOR OF ITS OWN, BECAUSE THERE WAS NO BYTE FOR ONE: the first cut
; added `cp FOPEN_SEL_ASAV / jp z,hk_asav` to hk_dpload's chain, and the
; region that chain sits in ($607B..$75A5) had 0 B free -- disk.rom assembled
; EMPTY. This fill had 752. The hook table enters through hk_chout_e /
; hk_chclose_e for the same reason: a live channel goes on into the
; unchanged bodies.
; ⚠️ NOT dout_open's 256 B record writer: that needs a channel row and a
; 265 B block, and SAVE ,A has neither -- the reference's #0 buffer would cost
; 267 B of FRE(0) here. The sequential writer needs no RAM of its own.
; hkf_pat -- D-FILESBARE (2026-10-09): FILES's pattern, or NONE. A non-empty
; pattern holding only blanks and an optional `A:` / `B:` lists the WHOLE disk on
; the CF-3300 -- `FILES " "`, `FILES "A:"`, `"a:"`, `"A: "` -- where the shared
; name builder (pdfcb) answered Bad file name; the empty string `FILES ""` IS 56
; there, and `"Q:"` is 62 (probes/disk/disk_probe_filesbare.py). Found 2026-09-05
; (namspc m.blank / m.drvbare) and deferred for MAIN bytes; FILES has lived in
; this ROM since D-DISKVERB4, and the builder's own rejection stays right for
; KILL / LOAD / SAVE (m.drvkill / m.drvload / m.drvsave). Sited in the fill: the
; FILES region is pinned full. in: STRSCR+1 = the staged pattern, '"'-ended.
; out: FILES_HASPAT 0 (list all) or 1 with DISK_FCB_NAME built.
hkf_pat:
                ld      hl,STRSCR+1
                ld      a,(hl)
                cp      '"'
                jr      z,hkp_name          ; "" -> the builder's 56, as there
                call    hkp_sp              ; leading blanks; A = (HL)
                and     $DF                 ; upper-case a letter
                cp      'A'
                jr      z,hkp_drv
                cp      'B'
                jr      nz,hkp_end
hkp_drv:
                sub     'A'-1               ; D-DSKIB: 1 A:, 2 B: (a name instead: pdfcb rewrites it)
                ld      (DK_FCB_DRV),a
                inc     hl
                ld      a,(hl)
                cp      ':'
                jr      nz,hkp_name         ; `AB.TXT`: a name, not a drive
                inc     hl
                call    hkp_sp
hkp_end:
                ld      a,(hl)
                cp      '"'
                ret     z                   ; nothing left: list it all (HASPAT 0)
hkp_name:
                ld      ix,pdfcb_resume
                call    calbak              ; -> DISK_FCB_NAME (8.3 wildcard)
                ld      a,1
                ld      (FILES_HASPAT),a
                ret
hkp_sp:                                     ; out: A = (HL), the first non-blank
                ld      a,(hl)
                cp      ' '
                ret     nz
                inc     hl
                jr      hkp_sp

; df_first -- D-FILESNL (2026-10-09), the second rule: the SCREEN listing
; STARTS on a fresh row. After `PRINT "AB";` the CF-3300 puts the first name
; under AB, not beside it; zerobas fitted it on the row
; (probes/disk/disk_probe_filesnl.py `midline`). Only before the FIRST entry
; (DISKOP_STATUS still 0): after each later one df_emit's own end-of-row test
; has left the cursor at column 0 or with room. Sited here, in the fill, because
; the FILES walk sits in a region pinned full (disk.rom assembled EMPTY inline).
; Sets DISKOP_STATUS = 1 (at least one entry matched). HL (the entry) survives:
; df_out preserves it.
df_first:
                ld      a,(DISKOP_STATUS)
                or      a
                ret     nz                  ; not the first entry
                inc     a
                ld      (DISKOP_STATUS),a   ; at least one entry matched
                call    op_lfiles
                ret     z                   ; the printer keeps its own head rule
                ld      a,(CSRX)
                dec     a                   ; 0-based column
                ret     z                   ; already at the start of a row
                jp      df_crlf             ; mid-row -> a new line first

hk_asav:
                call    fat_io_create       ; mount, make or truncate, reset the cursor
                jr      c, hfm_fail         ; status 1, the code pending
                ld      a, 4                ; 🐌 D-ASAVEFAT (2026-11-12): hold the FAT sector for
                ld      (FAT_DEFER), a      ; the whole listing, as tokenised SAVE does
                                            ; (D-SAVEFAT); hk_chclose_e writes it at the end
                jr      hfm_zero            ; status 0, selector disarmed, claimed (D-DISKREREAD
                                            ; 2026-11-26: hfm_ok's `xor a / jp hdl_answer`; 2 B)
hk_chout_e:
                push    af
                ld      a, (FCH_ACTIVE)
                or      a
                jr      z, hco_asav         ; no channel: SAVE ,A's listing
                pop     af
                jp      hk_chout            ; a channel: its 256 B record writer
hco_asav:
                pop     af
                ei                          ; as hk_chout's own entry
                or      a
                jr      nz, hco_byte        ; one byte
                ; D-ASAVELINE: A = 0 is main's print_dbwin handing over a whole
                ; 0-terminated window at HL (a listing never carries a $00). Each
                ; byte takes the one-byte path below, a 66 or a DSKIO fault raising
                ; from inside it exactly as before -- the unwind discards the frame.
hco_blk:
                ld      a, (hl)
                or      a
                jr      z, hco_tail         ; NC: the window drained, claimed
                inc     hl
                push    hl
                call    hco_byte            ; returns only on success (CY, claimed)
                pop     hl
                jr      hco_blk
hco_byte:
                call    fat_io_putbyte
                jr      nc, hco_tail
                ; 🔴 D-ASAVEFULLSTAMP's RULE MOVED WITH SAVE ,A. A listing that
                ; fills the disk keeps what is ALREADY ON IT: the CF-3300 stamps
                ; SA.BAS with the 1024 B that fit (scratchpad/savefull_run.out).
                ; That rule was ffds_full, in the SUB-ROM's flush -- which main's
                ; old path reached through its stub, and this ROM's flush does not
                ; carry (it serves the BDOS too, whose answer is not this). The
                ; first build without it left the entry 0/0, and diskfull-
                ; acceptance's `astamp` row said so. Only for 66, not an I/O fault.
                ld      a, (DISKOP_ERR)
                cp      66
                scf
                jr      nz, hco_tail        ; an I/O fault: leave the entry
                ld      hl, (FWR_BYTES)
                ld      de, (FWR_BUFLEN)    ; the full buffer that found no cluster
                or      a
                sbc     hl, de
                ld      (FWR_BYTES), hl
                jr      nc, hcb_nb          ; the borrow into the high word, as a
                ld      hl, (FWR_BYTES + 2) ; `dec` (D-DISKREREAD 2026-11-26: not
                dec     hl                  ; `sbc hl,0`; 2 B -- fat_dir_update
                ld      (FWR_BYTES + 2), hl ; loads its own DE / HL)
hcb_nb:
                                            ; (D-ASAVEFAT: no flush needed here -- a 66
                                            ; comes only from ffds_alloc's failure, whose
                                            ; ffds_undefer -> frl_xfail already wrote the
                                            ; held FAT; knife K2 moved nothing for that)
                call    fat_dir_update      ; size + first cluster, as close does
                ld      a, 66               ; its successful DSKIO cleared the code
                ld      (DISKOP_ERR), a
                scf
hco_tail:
                jp      c, dwr_raise        ; 66 (full) or a DSKIO code, pending
                scf                         ; claimed
                ret
; SAVE ,A's end: the partial sector, then the directory entry (size, first
; cluster). The Ctrl-Z is main's -- the listing's last byte, as before.
hk_chclose_e:
                ld      b, a                ; S10.D: the caller's mode -- main's fdcc_disk
                                            ; crosses with A = FCH_MODE (3 or 4; S10.C: 1 too)
                ld      a, (FCH_ACTIVE)
                or      a
                jp      nz, hk_chclose      ; a channel: Ctrl-Z, record, entry
                ei
                call    sdw_end             ; D-ASAVEFAT: the held FAT out, then fat_io_close
                jr      hco_tail

; hk_fapp -- OPEN FOR APPEND (H_FOPEN, FOPEN_SEL_APND), S10.B increment 2. A
; MISSING file is refused, as main's own APPEND open refused it (D-APPMISS). The
; FCB resumes at the last WHOLE record: random record = the size rounded down to
; 256, and that record's bytes are re-loaded into the block -- a size that is a
; whole number of records re-loads the last record entire, marked full and
; unwritten (DOUT_RFULL), its offset one record back. A trailing Ctrl-Z is
; stepped back onto, so the next byte overwrites it: "first\r\n\x1a" + APPEND
; "second" -> "first\r\nsecond\r\n\x1a" (disk_probe_append.py, the CF-3300).
; The size field keeps the file's size (LOF), the random record is LOC: 6 and 0
; after OPEN FOR APPEND on a 6 B file, as on the CF-3300 (applof_run.out).
hk_fapp:
                call    hk_openfind         ; FAT_FIRSTCLUS / FAT_FILESIZE / the entry
                jr      nc, hfa_found
hfa_nf:                                     ; S10.C: hk_finp's refusal too (disk/fat.asm)
                ld      a, 53               ; not found: refused, `File not found`, as
                ld      (DISKOP_ERR), a     ; main's own APPEND open raised it (its
                jp      hfm_fail            ; df_or_loaderr keyed on fat_find's OP)
hfa_found:
                call    set_first           ; the row's first cluster (D-DISKREREAD)
                ld      hl, (FAT_FILESIZE)
                ld      (ix+16), l
                ld      (ix+17), h          ; the size (a 64 KB file at most, as
                                            ; main's engine took it)
                ld      a, h
                or      l
                jp      z, hfm_ok           ; empty: nothing to resume (jp: hk_asav's
                                            ; block now sits between)
                ld      b, l                ; B = bytes into the last record
                ld      l, 0
                ld      a, b
                or      a
                jr      nz, hfa_rr
                dec     h                   ; a whole last record: re-load it all
hfa_rr:
                call      set_rr          ; the random record: that record's start
                push    bc                  ; [B = the count, 0 = 256]
                ld      a, h
                srl     a                   ; A = the record's sector in the file
                ld      c, 0                ; C = its cluster's index in the chain
                ld      hl, FAT_SECPERCLUS
hfa_div:
                cp      (hl)
                jr      c, hfa_divd
                sub     (hl)
                inc     c
                jr      hfa_div
hfa_divd:
                ld      (ix+DOUT_SECIDX), a
                ld      (FWR_SECIDX), a
                ld      l, (ix+DOUT_FIRST)
                ld      h, (ix+DOUT_FIRST+1)
                ld      a, c
hfa_walk:
                or      a
                jr      z, hfa_clus
                push    af
                call    fat_next_cluster    ; HL = the next cluster
                pop     af
                dec     a
                jr      hfa_walk
hfa_clus:
                ld      (ix+DOUT_CLUS), l
                ld      (ix+DOUT_CLUS+1), h
                ld      (FWR_CLUS), hl
                call    dout_secread        ; FAT_DBUF = the record's sector
                pop     bc
                jp      c, hfm_fail
                ld      hl, FAT_DBUF
                bit     0, (ix+34)          ; the sector's second half?
                jr      z, hfa_src
                inc     h
hfa_src:
                push    hl
                call    rnd_rec             ; D-DISKREREAD (2026-11-26): dout_pos +3
                ex      de, hl              ; DE = the record
                pop     hl                  ; HL = its bytes on disk
                ld      c, b
                ld      b, 0
                dec     c                   ; D-DISKREREAD (2026-11-26): BC = B, or 256
                inc     bc                  ; for B = 0 (C $FF + 1 carries into B); 3 B
                push    bc
                ldir
                pop     bc
                dec     de
                ld      a, (de)             ; the last byte
                cp      $1A
                jr      nz, hfa_pos
                dec     bc                  ; a Ctrl-Z: the next byte goes ON it
hfa_pos:
                call    dout_pos
                ld      (hl), c             ; the position in the record
                ld      a, b
                or      a
                jr      z, hfa_ok           ; under 256: a partial record
                set     DOUT_RFULL, (ix+DOUT_FLG)   ; 256: whole, to be rewritten
hfa_ok:                                     ; D-DISKREREAD (2026-11-26): one jp; 1 B
                jp      hfm_ok

; hk_chout -- PRINT# to an OUTPUT channel (H_CHOUT): A = the byte.
hk_chout:
                ei
                push    af
                call    dout_live
                pop     af
                call    dout_put
                scf                         ; claimed
                ret

; dout_put -- store A in the record at the position; write the record when it
; is full. A full record whose write FAILED is written first, so a full disk
; fails every later PRINT# the same way (D-DISKFULL) and a CLOSE retries it.
; IX = the row. Raises on a failed write (never returns then).
dout_put:
                push    af
                bit     DOUT_RFULL, (ix+DOUT_FLG)
                ld      hl, 256
                call    nz, dout_write      ; the full record, again
                call    dout_pos
                ld      e, (hl)             ; E = the position
                inc     (hl)                ; Z iff the record is now full
                pop     bc                  ; B = the byte
                push    af                  ; [the full flag]
                ld      d, 0
                inc     hl
                inc     hl
                inc     hl                  ; HL = the record
                add     hl, de
                ld      (hl), b
                pop     af
                ret     nz
                set     DOUT_RFULL, (ix+DOUT_FLG)
                ld      hl, 256             ; FALLS INTO dout_write

; dout_write -- write the record's first HL bytes (1..256) at the random record
; (FCB +33, a byte offset: RS = 1), which is a whole number of records, so the
; record is one HALF of a 512 B sector -- the first half when its bit 8 is 0. The
; engine's write cursor (DOUT_CLUS/FIRST/SECIDX) goes into this ROM's FWR_*
; for fat_flush_data_sector, which allocates and links a cluster when one is
; needed and writes FAT_DBUF; a first half is written padded and the cursor
; stepped BACK onto its sector (as fat_detach_channel does), so the second
; half re-reads it and completes it. On success: random record += HL, size :=
; max(size, random record) -- WRBLK's own rule, and the CF-3300's LOF, which
; H_CHIN op 6 reads from here (S10.E: no mirror in main any more) -- and
; DOUT_RFULL clears; out A = 0, CF = 0. On failure raise what is pending --
; 66 from fac_full, or a DSKIO code -- through main's disk_error, the row as
; it was. IX = the row (kept).
dout_write:
                push    hl                  ; [the count]
                ld      l, (ix+DOUT_CLUS)
                ld      h, (ix+DOUT_CLUS+1)
                ld      (FWR_CLUS), hl
                ld      l, (ix+DOUT_FIRST)
                ld      h, (ix+DOUT_FIRST+1)
                ld      (FWR_FIRST), hl
                ld      a, (ix+DOUT_SECIDX)
                ld      (FWR_SECIDX), a
                ld      de, FAT_DBUF
                bit     0, (ix+34)          ; offset bit 8: the sector's second half?
                jr      z, dwr_copy
                call    dout_secread        ; its first half, as written
                jp      c, dwr_fail
                ld      de, FAT_DBUF+256
dwr_copy:
                call    rnd_rec             ; HL = the record (D-DISKREREAD 2026-11-26:
                                            ; dout_pos +3, spelled out here and at hfa_src; 6 B)
                pop     bc
                push    bc
                ldir
                pop     hl                  ; the count
                push    hl
                bit     0, (ix+34)
                jr      z, dwr_len
                inc     h                   ; + the first half
dwr_len:
                ld      (FWR_BUFLEN), hl
                call    fat_flush_data_sector
                pop     bc                  ; BC = the count
                jr      c, dwr_fail
                bit     0, (ix+34)
                jr      nz, dwr_keep        ; the sector is complete: move on
                ld      hl, FWR_SECIDX
                dec     (hl)                ; back onto it, for the second half
dwr_keep:
                ld      hl, (FWR_CLUS)
                ld      (ix+DOUT_CLUS), l
                ld      (ix+DOUT_CLUS+1), h
                ld      hl, (FWR_FIRST)
                ld      (ix+DOUT_FIRST), l
                ld      (ix+DOUT_FIRST+1), h
                ld      a, (FWR_SECIDX)
                ld      (ix+DOUT_SECIDX), a
                ld      l, (ix+33)
                ld      h, (ix+34)
                add     hl, bc
                call      set_rr          ; the random record (64 KB: the size
                                            ; field's high word stays 0, as below)
                ld      e, (ix+16)
                ld      d, (ix+17)
                ex      de, hl
                or      a
                sbc     hl, de              ; size - random record
                jr      nc, dwr_lof         ; the size already reaches it
                ld      (ix+16), e
                ld      (ix+17), d          ; size := the random record
dwr_lof:
                res     DOUT_RFULL, (ix+DOUT_FLG)
                xor     a                   ; A = 0, CF = 0: dout_mirror's old exit
                ret                         ; (S10.E), which no caller reads

; dout_secread -- FAT_DBUF := the sector at (FWR_CLUS, FWR_SECIDX). Cy = I/O.
; S10.D: its address arithmetic is clus_sec now, shared with the RANDOM record
; engine's chain walk (frnd_locate), which carried a second copy of it.
dout_secread:
                ld      hl, (FWR_CLUS)
                ld      a, (FWR_SECIDX)
                call    clus_sec            ; DE = the sector
                jp      rd_dbuf
; clus_sec -- DE = the absolute sector of sector A of cluster HL:
; FIRSTDATA + (cluster - 2) * secPerClus + A. Clobbers A, B, HL.
clus_sec:
                dec     hl
                dec     hl
                ex      de, hl              ; DE = cluster - 2 (a data cluster is >= 2)
                ld      l, a
                ld      h, 0                ; HL = the sector in the cluster
                ld      a, (FAT_SECPERCLUS)
                ld      b, a
cs_mul:
                add     hl, de
                djnz    cs_mul              ; + (cluster - 2) * secPerClus
                ld      de, (FAT_FIRSTDATA)
                add     hl, de
                ex      de, hl
                ret
dwr_fail:
                ; 🔴 D-PRINTFULLSTAMP (2026-10-06): a FAILING CLOSE (DOUT_DEAD is set
                ; before its writes) commits nothing, as the CF-3300's: MSX-DOS 1
                ; writes the FAT at close, so a close that fails leaves the entry
                ; 0/0 AND the clusters free (scratchpad/savefull_run.out: FAT[714]
                ; 000 there, FFF here). Our allocator commits each cluster as it
                ; takes it, so the chain is released here; the entry was never
                ; updated (the directory is written only by a CLOSE that succeeds).
                ; A PRINT# failure keeps the chain: the channel is still open, and
                ; a later CLOSE retries. The pending code (66 / a DSKIO one) is held
                ; across the freeing, whose successful reads clear DISKOP_ERR.
                bit     DOUT_DEAD, (ix+DOUT_FLG)
                jr      z, dwr_raise
                ld      a, (DISKOP_ERR)
                push    af
                ld      l, (ix+DOUT_FIRST)
                ld      h, (ix+DOUT_FIRST+1)
                call    fat_free_chain      ; (its own I/O failure: nothing more to do)
                xor     a
                ld      (ix+DOUT_FIRST), a
                ld      (ix+DOUT_FIRST+1), a
                ld      (ix+DOUT_CLUS), a
                ld      (ix+DOUT_CLUS+1), a
                pop     af
                ld      (DISKOP_ERR), a
dwr_raise:
                ld      ix, disk_error
                ; the code was just stored in DISKOP_ERR, so main's disk_error raises:
                ; an unwind (raise_error), never a return
                ; z80flow: abort unwind
                jp      calbak              ; raises; does not return

; hk_chclose -- CLOSE of a disk channel (H_CHCLOSE), B = its mode. OUTPUT /
; APPEND: Ctrl-Z, the last record, the directory entry; RANDOM (S10.D): the entry
; only; INPUT (S10.C): nothing. A failure raises with
; DOUT_DEAD set and the channel still open, so a re-OPEN is 54 and the NEXT
; CLOSE lands here, clears it and frees the channel without touching the disk
; -- D-DISKFULL's measured CF-3300 face.
hk_chclose:
                ei
                call    dout_live
                ld      a, b
                cp      4
                jr      z, hcc_stamp        ; S10.D: RANDOM -- the entry only, once
                                            ; (D-PUTDIR); a failure raises through
                                            ; dwr_fail, DOUT_DEAD clear, the channel
                                            ; still open -- fdcc_rnd's old face
                dec     a
                jr      z, hcc_done         ; S10.C: INPUT (1) -- nothing to write; the
                                            ; CF-3300 enters $FE62 here too
                                            ; (s10c_hook_run.out), main frees the row
                bit     DOUT_DEAD, (ix+DOUT_FLG)
                jr      nz, hcc_done        ; the failed CLOSE's second: just free it
                set     DOUT_DEAD, (ix+DOUT_FLG)
                ld      a, $1A              ; CP/M text EOF, as the CF-3300's CLOSE
                call    dout_put
                call    dout_pos
                ld      a, (hl)
                or      a
                jr      z, hcc_stamp        ; the Ctrl-Z filled the record: written
                ld      l, a
                ld      h, 0
                call    dout_write          ; the partial last record
hcc_stamp:
                ; the directory entry, once: first cluster, size, the date
                ; (fat_dir_update dates it as the CF-3300 dates a written file)
                ld      l, (ix+DOUT_DIRSEC)
                ld      h, (ix+DOUT_DIRSEC+1)
                ld      (FWR_DIRSEC), hl
                ld      l, (ix+DOUT_DIROFF)
                ld      h, (ix+DOUT_DIROFF+1)
                ld      (FWR_DIROFF), hl
                ld      l, (ix+DOUT_FIRST)
                ld      h, (ix+DOUT_FIRST+1)
                ld      (FWR_FIRST), hl
                push    ix
                pop     hl
                call    close_size_upd      ; the size, then fat_dir_update (carve 18)
                jr      c, dwr_fail
hcc_done:
                scf                         ; claimed; main frees the channel
                ret

; wrblk_fcbstate -- D-BLKIOPERCALL (2026-10-08): WRBLK's per-call state from the
; FCB instead of a mount and a find. The geometry is mounted only when it has
; never been read (RDBLK's own rule, k47b2_seek); M30's allocation hint gets the
; per-operation reset fat_mount gave it; FAT_FIRSTCLUS from +26, FAT_FILESIZE
; from +16..19 (wrblk_fcb_keep writes both after every call, FOPEN / FMAKE before
; the first); and the directory entry for fat_dir_update from +25, the root
; index FOPEN / FMAKE filled -- sector FIRSTROOT + index / 16, offset (index mod
; 16) x 32, close_read's arithmetic.
;   in: IX = the FCB copy; out: CF = the mount failed
wrblk_fcbstate:
                ld      a, (FAT_SECPERCLUS)
                or      a
                jr      nz, wfs_geom
                call    fat_mount
                ret     c
wfs_geom:
                ld      hl, 2
                ld      (FAT_ALLOCHINT), hl ; M30: the per-operation reset
                call    fcb_fatst           ; the chain and the size, from the FCB
                ld      a, (ix+25)          ; the entry's root index
                call    root_pos
                ld      (BDOS_DIRSEC), de
                ld      (BDOS_DIROFF), hl
                or      a                   ; CF clear
                ret
; root_pos -- A = a root entry's index -> DE = its sector (FAT_FIRSTROOT + index / 16),
; HL = its offset in that sector ((index mod 16) x 32). Clobbers A. 💰 D-DSKFMT carve 20
; (2026-11-10): the same arithmetic stood inline in close_read and wfs_geom.
root_pos:
                push    af
                rrca
                rrca
                rrca
                rrca
                and     $0F                 ; A = index / 16
                ld      e, a
                ld      d, 0
                ld      hl, (FAT_FIRSTROOT)
                add     hl, de
                ex      de, hl              ; DE = the entry's sector
                pop     af
                and     $0F
                ld      l, a
                ld      h, 0
                jp      hl_x32            ; HL = (index mod 16) x 32 (tail call:
                                          ; D-DISKREREAD 2026-11-26, 1 B)
; dta_rec / dta_chunk -- SECTOR_BUF + HL := RECSIZE (dta_rec) or BC (dta_chunk) bytes
; from the DTA (BDOS_DTA). Out: HL / DE past the source / the destination, BC = 0.
; 💰 D-DSKFMT carve 22 (2026-11-10): it stood inline in bsw_m34_buffer, wrrnd_body and
; wrblk's chunk copy.
dta_rec:
                ld      bc, RECSIZE
dta_chunk:
                ld      de, SECTOR_BUF
                add     hl, de
                ex      de, hl              ; DE = SECTOR_BUF + HL
                ld      hl, (BDOS_DTA)      ; HL = the caller's bytes
                ldir
                ret



; ===== S10.D (2026-10-15): a RANDOM channel is the same FCB row, and its record
; I/O runs HERE ==============================================================
; D-ROMSIZE step 1 / spec-diskcode-eviction.md S10.D. Until now OPEN ... AS #n,
; GET, PUT and a RANDOM CLOSE ran main's head over the sub-ROM's record engine
; (sub/randio.asm, fatprim rows 15-17) with the channel's state in MAIN's engine
; globals (FWR_FIRST / FWR_BYTES / FWR_DIRSEC / FWR_DIROFF), saved per channel by
; the sub-ROM's t_fch_save. S10.B's lesson holds here too: the record path can
; only move if the state is disk-side FROM OPEN. So a RANDOM channel's row is the
; S10.B FCB row -- name +1..11 (hkk_open_check), size +16..19 (LOF), DOUT_FIRST,
; DOUT_DIRSEC / DOUT_DIROFF (CLOSE's stamp), DOUT_BLK -- filled by hk_frnd and
; kept by hk_dget's engine through IX.
; ⚠️ WHAT DID NOT MOVE WITH S10.D: THE RECORD. FIELD, LSET / RSET, fld_lookup and
; GET / PUT all worked on the LIVE channel's record in FSECTOR_BUF (= this ROM's
; FAT_DBUF, the merged buffer pair), saved to the block's +9 when another channel
; went live by the sub-ROM's t_fch_save / t_fch_load.
; 🏗️ D-FLDDESC (2026-10-17): NOW IT HAS. The record LIVES at the channel's block
; +9 (FCH_HDRSZ), as on the CF-3300 -- where FIELD points each variable's own
; descriptor at block+9+offset (scratchpad/fieldram_run.out) -- and nothing
; copies it on a channel switch. So the engine below reads and writes the
; record through the row's cached block address (rnd_rec), and FAT_MBUF is still
; the sector it overlays; FAT_DBUF is no channel's record any more, which also
; ends every directory / FAT verb's licence to clobber a RANDOM record there.
; The reference's contract at $FE80 (scratchpad/dget_run*.out, CF-3300): main has
; done the "File not OPEN" check and nothing else; the RECORD NUMBER IS PARSED
; HERE (record 0 -> 5, past EOF / -1 -> 55 are raised by the disk side). Ours
; crosses with A / C / FN_RESUME rather than its HL = PTRFIL and a stacked text
; pointer -- own-design ABI, the reference's split.

; hk_frnd -- OPEN "f" AS #n (no FOR clause: RANDOM) -- H_FOPEN, FOPEN_SEL_RAND.
; FCH_ACTIVE is the channel being opened (OPEN stores it), DISK_FCB_NAME the parsed
; 8.3 name. The sub-ROM's fat_rand_open, moved: LOC := 0 (D-LOC: the CF-3300
; reads 0 before any GET / PUT -- dout_open's zeroed +33..35 since D-RECBIG,
; 2026-11-04; it was a separate FCH_RECNOS[ch] store), find the file -- or create
; it (no truncation: RANDOM keeps an existing file). The record (block+9) is NOT touched.
; 📏 D-FLDDESC (2026-10-17): this space-filled the record (fld_fill_record's 512 B,
; then the block's FCH_RECMAX), and the CF-3300 never does -- a fielded B$ still
; reads `  Z` after CLOSE and a re-OPEN (scratchpad/flddesc_run.out r8), a FRESH
; block reads whatever RAM held (159 146 on #1, 255 255 on #2), and CLEAR +
; MAXFILES leave an old `QQ` in place (scratchpad/fldfresh_run.out f1-f3). So the
; fill is gone; a PUT before any LSET writes those bytes, as the reference does.
; hk_fmake's tail: status 0 open; 1 with the code pending.
hk_frnd:
                call    hk_openfind         ; IX = the row; FAT_FIRSTCLUS / ... / the entry
                jp      c, hfm_create       ; D-DISKREREAD (2026-11-26): hk_fmake's create,
                                            ; byte for byte (a new, empty entry -- dout_open
                                            ; zeroed the first cluster and the size); 12 B
                call    set_first           ; the row's first cluster (D-DISKREREAD)
hfr_size:                                   ; (hk_finp joins here: D-DSKFMT carve 14)
                push    ix
                pop     hl
                ld      de, 16
                add     hl, de
                ex      de, hl              ; DE = the FCB's size, +16
                ld      hl, FAT_FILESIZE
                call    copy4                ; the whole 32-bit size (D-PUTU32)
                jp      hfm_ok              ; the entry's place, LOF, status 0
; set_first -- the row's DOUT_FIRST := FAT_FIRSTCLUS (HL = it on return). D-DISKREREAD
; (2026-11-26): it stood inline in hk_frnd and hk_fapp's hfa_found; 2 B.
set_first:
                ld      hl, (FAT_FIRSTCLUS)
                ld      (ix+DOUT_FIRST), l
                ld      (ix+DOUT_FIRST+1), h
                ret

; toupper_cmp -- D-DISKROMHOST step 2b: the run it replaces stood at 3 sites (fat-prim-body.inc, kernel.asm)
toupper_cmp:
                call    toupper             ; names keep the typed case now, and
                ld      c,a                 ; `COPY "a.txt" TO "A.TXT"` IS a self-copy --
                ld      a,(hl)              ; byte-wise it would delete the source and
                call    toupper             ; copy from its freed chain
                cp      c
                ret
; dirrem_z -- D-DISKROMHOST step 2b: the run it replaces stood at 5 sites (fat-prim-body.inc, kernel.asm)
dirrem_z:
                ld      hl,(FAT_DIRREM)
                ld      a,h
                or      l
                ret
; hk_dget -- GET / PUT (H_DGET, $FE80). Main (basic/field.asm gp_common) has
; parsed `#n` and raised 5 / 59 / 52 / 58 / 61 itself, so the channel is OPEN
; and RANDOM.
;   in   A = 0 GET / 1 PUT, C = the channel, FN_RESUME = the cursor past `#n`
;   out  FN_RESUME = the cursor past `, recno`; CF = 1 (claimed)
;   raises (never returns): the evaluator's own (13, 6, ...), 5 record 0, 55 a
;        GET at or past EOF, a pending DSKIO / disk-full code (disk_error)
; ⚠️ THE CHANNEL IS MADE LIVE AFTER THE EVALUATOR, NOT BEFORE: the record is an
; EXPRESSION, and `GET #1,LOF(2)` or a FIELDed variable of another channel in it
; makes THAT channel live (LOF sets FCH_ACTIVE for H_CHIN's op 6 since S10.E; it
; ran fch_select before). Main used to parse first and select second for
; the same reason; the order survives the move. (D-FLDDESC, 2026-10-17: a FIELDed
; variable no longer selects anything when read -- its descriptor points into
; its own block -- so `GET #1,LOF(2)` is the case left, and the order still
; matters for it.)
hk_dget:
                ei                          ; D-HOOKDI: the hook arrives DI
                ld      b, a
                push    bc                  ; [B = GET / PUT, C = the channel]
                ld      hl, (FN_RESUME)
                call    hkp_sp              ; A = the first non-blank
                jr      hdg_cmp             ; over the pinned $5C11 below
                ds      $5C11 - $, $00
; 📌 k_5C11 -- D-ROBOREG (2026-11-13): a CF-3300 disk-kernel address that software CALLS.
; ROBOCOP.DSK's boot code CALSLTs $5C11 (IY $87) with A $F1, BC 0, DE $8780, HL $8680 on
; the CF-3300 as here (scratchpad/roboirq_run9.out); there it returns with no effect our
; own calls could see -- registers out the same for every input, no byte of the caller's
; buffers changed (scratchpad/k5c11_run2.out, k5c11_run3.out). Here it was 13 B into
; hk_dget: an eval of main's text pointer, a Syntax error raised with SAVSTK 0, the stack
; over the hooks and an interrupt storm. A bare `ret`, as the other bodiless canonical
; entries (k_4919 ..).
k_5C11:         ret
hdg_cmp:
                cp      ','
                jr      nz, hdg_next
                inc     hl
                ld      ix, eval
                call    calbak              ; FAC = the record (DE = it, an int)
                ld      (FN_RESUME), hl
                call    rnd_parse           ; GP_RECNO = the record, the CF-3300's rule
                jr      hdg_have
hdg_next:
                ; ✅ D-RECAUTO: NO RECORD NUMBER MEANS THE NEXT ONE -- LOC + 1. The
                ; CF-3300's disk FCB already holds it (+33 = 2 after `GET#1,2`),
                ; and so does ours since D-RECBIG (rnd_loc): the row's +33..34.
                ld      a, c
                call    dout_row            ; HL = channel C's row
                ld      de, 33
                add     hl, de
                ld      de, GP_RECNO
                call    copy4                ; GP_RECNO = LOC (32 bits) ...
                ld      hl, GP_RECNO
                ld      b, 4
hdg_inc:
                inc     (hl)                ; ... + 1
                jr      nz, hdg_have
                inc     hl
                djnz    hdg_inc
hdg_have:
                ld      hl, GP_RECNO
                ld      a, (hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)
                ld      a, 5
                jr      z, hdg_raise        ; 🔴 D-GETREC: record 0 is Illegal function
                                            ; call, RAISED (trappable), GET and PUT
                pop     bc
                push    bc
                ld      a, c
                ld      (FCH_ACTIVE), a     ; the channel live. S10.E (2026-10-19):
                                            ; a STORE, not a call-back to main's
                                            ; fch_select -- the switch it ran
                                            ; (t_fch_load's LOF / LOC mirror) is
                                            ; gone, and FCH_ACTIVE is what this ROM
                                            ; reads. D-FLDDESC: the record does not
                                            ; move -- it is block+9
                ei                          ; the record's I/O runs unmasked on BOTH
                                            ; paths: the call-back's CALSLT returned
                                            ; DI (dout_open's note), and `, recno`'s
                                            ; evaluator call-back above still does
                call    dout_live           ; IX = its row
                call    rnd_loc             ; LOC := n - 1 now: a GET / PUT that
                scf                         ; fails leaves it there (CF-3300: PUT
                ld      b, 4                ; 1E9 is Disk full with LOC 999999999,
hdg_dec:                                    ; `PUT 5:GET 9` past EOF LOC 8)
                ld      a, (de)
                sbc     a, 0
                ld      (de), a
                inc     de
                djnz    hdg_dec
                pop     bc
                ld      a, b
                ld      (GP_FLAGS), a       ; 1 = PUT extends the chain, 0 = GET stops
                djnz    hdg_get             ; B was 0: GET
                call    fat_rand_put
                jr      hdg_fin
hdg_get:
                call    fat_rand_get
hdg_fin:
                call    nc, rnd_loc         ; done: LOC := n
                ld      ix, disk_error
                call    c, calbak           ; D-GETEJECT: the pending code RAISED (70
                                            ; with the disk out); none -> `load error`,
                                            ; which returns -- so this is a call
                scf                         ; claimed
                ret
hdg_raise:
                ld      ix, raise_error     ; A = the code; never returns
                jp      calbak


; frnd_calc -- split the record's offset into GP_SEC (the file sector) and GP_WITHIN
; (the offset in it): offset = k * reclen (frnd_off; CF = 1 past 2^24, which no
; disk holds -- PUT's Disk full, GET's Input past end), reclen =
; FCH_RECLENS[FCH_ACTIVE] (OPEN's LEN=, 1..256) into GP_RECLEN -- and arm
; D-STRADDLE's pass cursor: GP_SRC := the record, GP_LEFT := its length.
; D-FLDDESC: the record is the block's +9 (rnd_rec), not FAT_DBUF. IX = the row.
;   out: A:HL = the offset (GET's EOF bound reads it). Clobbers BC, DE.
frnd_calc:
                ld      a, (FCH_ACTIVE)
                ld      de, FCH_RECLENS
                call    rnd_tab
                ld      e, (hl)
                inc     hl
                ld      d, (hl)
                ld      (GP_RECLEN), de
                ld      (GP_LEFT), de       ; the pass cursor: all of the record...
                call    rnd_rec
                ld      (GP_SRC), hl        ; ...from its first byte (block+9)
                call    frnd_off            ; A:HL = k * reclen
                ret     c                   ; past any disk
                ld      c, a
                ld      e, l
                ld      a, h
                and     1
                ld      d, a
                ld      (GP_WITHIN), de     ; offset & 511
                ld      e, h
                ld      d, c
                srl     d
                rr      e
                ld      (GP_SEC), de        ; offset >> 9: 16 bits on any disk that holds it
                ld      a, c
                or      a                   ; CF = 0
                ret


; ===== D-DOSDATE (2026-10-01): the DOS date, kept as the CF-3300 keeps it ====
; DATE_DAYS ($F33B) is a day count since 1980-01-01 (a Tuesday): 1461 from boot
; (1984-01-01). Measured on the CF-3300 (scratchpad/sdatebad.out): SDATE stores
; 1980-01-01 .. 2099-12-31 with real month lengths and leap years (A = 0) and
; rejects anything else with A = $FF, leaving the count alone; GDATE returns it
; with the day of week; every file stamp is it (Joost 2026-10-01, "Stamp as
; 3300"). Over 1980..2099 every fourth year is a leap year, exactly.

; sdate_body -- in: HL = year, D = month, E = day; out: A = 0 stored / $FF not.
; $F306 is left as the dispatcher set it: A is the result, mirrored into L.
sdate_body:
                ld      bc, -1980
                add     hl, bc
                jr      nc, sd_bad          ; before 1980
                ld      a, h
                or      a
                jr      nz, sd_bad
                ld      a, l
                cp      120
                jr      nc, sd_bad          ; after 2099
                ld      c, a                ; C = year - 1980
                ld      a, d
                dec     a
                cp      12
                jr      nc, sd_bad          ; month not 1..12 (0 wraps to 255)
                ld      b, a                ; B = month - 1
                dec     e                   ; E = day - 1 (day 0 wraps to 255)
                call    month_len           ; A = this month's length (keeps DE)
                dec     a
                cp      e                   ; D-DISKREREAD (2026-11-26): length - 1 < day - 1, tested
                jr      c, sd_bad           ; in E (no push/pop, no D), 3 B -- past the month's last day
                ld      l, e
                ld      h, 0                ; HL = day - 1
sd_mon:
                ld      a, b
                or      a
                jr      z, sd_yr
                dec     b
                call    month_len           ; + every earlier month of this year
                ld      e, a
                ld      d, 0
                add     hl, de
                jr      sd_mon
sd_yr:                                      ; D-DISKREREAD (2026-11-26): one loop tested at the top, not a
                ld      a, c                ; guard plus a bottom test, 2 B
                or      a
                jr      z, sd_store
                dec     c                   ; + every earlier year
                ld      de, 365
                ld      a, c
                and     3
                jr      nz, sd_yadd
                inc     de                  ; 1980 + C is a leap year
sd_yadd:
                add     hl, de
                jr      sd_yr
sd_store:
                ld      (DATE_DAYS), hl
                xor     a
                ret
sd_bad:
                ld      a, $FF
                ret

; gdate_body -- _GDATE's contract (docs/tier2-gdate-spec.md §4): HL = year,
; D = month, E = day, A = day of week (0 = Sunday), BC = 0, F = $44, and $F306
; cleared so the kernel passes HL through (the M20 rule).
gdate_body:
                call    gdate_core
                ld      b, a
                call     clr_f306   ; F = $44 (Z, P/V) from the xor
                ld      a, b                ; A = day of week, flags untouched
                ld      bc, 0
                ret

; gdate_core -- DATE_DAYS as a date. No side effects outside the registers (the
; stamps call it mid-BDOS, where $F306 must stay as the dispatcher set it).
;   out: HL = year, D = month, E = day, A = day of week; trashes BC, F
gdate_core:
                ld      hl, (DATE_DAYS)
                push    hl
                inc     hl
                inc     hl                  ; days + 2: 1980-01-01 was a Tuesday
                ld      e, l
                ld      l, h
                ld      h, 0
                add     hl, hl
                add     hl, hl              ; high byte x 4 (256 = 4 mod 7)
                ld      d, 0
                add     hl, de              ; + low byte: the same remainder mod 7
                ld      de, -7
gc_mod7:
                add     hl, de
                jr      c, gc_mod7
                ld      de, 7
                add     hl, de              ; L = (days + 2) mod 7
                pop     de                  ; DE = days
                push    hl                  ; [day of week]
                ex      de, hl              ; HL = days
                ld      c, 0                ; C = year - 1980
gc_yr:
                ld      de, 365
                ld      a, c
                and     3
                jr      nz, gc_yl
                inc     de
gc_yl:                                      ; D-DISKREREAD (2026-11-26): no `or a` -- CF = 0 from `and 3`
                                            ; (`jr nz` / `inc de` keep it), 1 B
                sbc     hl, de
                jr      c, gc_yrd           ; inside this year
                inc     c
                jr      gc_yr
gc_yrd:
                add     hl, de              ; HL = the day of the year, from 0
                ld      b, 0                ; B = month - 1
gc_mon:
                call    month_len
                ld      e, a
                ld      d, 0
                or      a
                sbc     hl, de
                jr      c, gc_mond          ; inside this month
                inc     b
                jr      gc_mon
gc_mond:
                add     hl, de              ; L = the day of the month, from 0
                inc     l                   ; D-DISKREREAD (2026-11-26): through L, not A (L is
                ld      e, l                ; overwritten next), 1 B -- E = day
                ld      d, b
                inc     d                   ; D = month
                ld      l, c
                ld      h, 0
                ld      bc, 1980
                add     hl, bc              ; HL = year
                pop     bc                  ; C = day of week
                ld      a, c
                ret

; month_len -- A := the length of month B (0 = January) in year 1980 + C.
; Keeps BC, DE, HL.
month_len:
                push    hl
                ld      hl, month_days
                ld      a, l
                add     a, b
                ld      l, a
                jr      nc, ml_nc
                inc     h
ml_nc:
                ld      a, (hl)
                pop     hl
                cp      28
                ret     nz                  ; not February
                bit     0, c
                ret     nz
                bit     1, c
                ret     nz                  ; not a leap year
                inc     a                   ; 29
                ret
month_days:     db      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31

; date_fat_word -- DE := DATE_DAYS as a FAT date word: (year - 1980) << 9 |
; month << 5 | day (1984-01-01 = 0821h, 1999-12-31 = 279Fh). Trashes AF, BC, HL.
date_fat_word:
                call    gdate_core          ; HL = year, D = month, E = day
                ; D-DISKREREAD (2026-11-26): built in place in DE, one rotate of the month serving
                ; both bytes, and year - 1980 from L alone (1980 = $07BC; year - 1980 < 120), 6 B
                ld      a, l
                sub     $BC
                add     a, a                ; (year - 1980) << 1: bits 15..9
                ld      h, a
                ld      a, d
                rrca
                rrca
                rrca                        ; bit 0 = month >> 3, bits 7..5 = month << 5
                ld      l, a
                and     1                   ; month >> 3: bit 8
                or      h
                ld      d, a
                ld      a, l
                and     $E0                 ; (month << 5) & $E0: bits 7..5
                or      e                   ; day: bits 4..0
                ld      e, a
                ret

; ===== D-DOSBASIC (2026-10-02): BASENT, "start BASIC" from MSX-DOS ==========
; COMMAND.COM's BASIC command reaches the disk ROM's $4022 (traced: its handler at
; $D06E, the $F368 hook, then $4022 -- scratchpad/d06e_trace.out). The CF-3300
; then shows a fresh BASIC: the full banner, `Disk BASIC version 1.0`, `Ok`.
; Ours restarts BASIC the way the BIOS starts it: main's cartridge INIT (the
; `AB` header's $4002 word), with the state the BIOS hands it (measured at a
; normal boot, scratchpad/initstate.out: SP $F2EC, pages 0/1 the main ROM, DI),
; hooks refilled with RET as at power-on, and a request flag so the disk INIT it
; reruns keeps the DOS date and does not boot DOS again.

; basent_pending -- Z when a BASENT restart is pending (BASENT_REQ = 'B','A' from
; MSX-DOS, or 'B','D' from a boot loader's fallback -- D-BOOTDATE). Both keep the
; date and skip the DOS boot: skipping is what stops a boot loader that falls back
; to $4022 from looping. Out: HL = the mark (H tells the two apart). Trashes A.
basent_pending:
                ld      hl, (BASENT_REQ)
                ld      a, l
                cp      'B'
                ret     nz
                ld      a, h
                cp      'A'
                ret     z
                cp      'B'                 ; D-BASENTRUN: from a running program
                ret     z
                cp      'D'
                ret

; wrblk_huge -- D-BDOSSEQ (2026-11-10): a WRBLK / WRZER / WRRND whose start RR x RS
; is 1 MB or more -- RR's top byte set and RS >= 16 -- lies past any MSX disk: disk
; full (A = 1), as the CF-3300 answers. wrblk_off24's 24-bit product WRAPPED there,
; and the zero-record WRBLK applied the meaningless size (bdosset_probe.py, after our
; SETRND's RR $080002). Called from wrblk_rs_ok with DE = RS; leaves on the FULL path
; by dropping its own return.
wrblk_huge:
                ld      (WRBLK_RS), de      ; (wrblk_rs_ok's own store, moved here for room)
                ld      a, (ix+35)
                or      a
                ret     z
                ld      a, d
                or      a
                jr      nz, wh_full
                ld      a, e
                cp      16
                ret     c
wh_full:
                pop     hl
                jp      wrblk_full
; (moved here from kernel.asm by the D-DSKFMT defrag)
rdrnd_ex:
                ld      a, (ix+33)
                rla                         ; Cy = r0 bit 7
                ld      a, (ix+34)
                rla                         ; A = (r1 << 1) | (r0 >> 7)
                ld      (ix+12), a
                ld      (ix+14), 0
                ; (c) +28..31 = the record's cluster and its index in the chain (record
                ; 199 of a 200-record file -> $198 / $18 on the CF-3300): rrnd_position
                ; left FAT_CURCLUS on the record's cluster; the index comes from the
                ; record's sector in the file (RR >> 2: four 128-byte records a sector),
                ; through wrblk_fcb_keep's own cluster-cache tail.
                ld      l, (ix+33)
                ld      h, (ix+34)
                srl     h
                rr      l
                srl     h
                rr      l
                ld      (WRBLK_CURSEC), hl
                jp      wfk_cache
; (moved here from runtime.asm by the D-DSKFMT defrag)
; --- dos_clear_screen - OI-3: blank the SCREEN-1 name table + home the cursor -----
; Stock's DOS boot handoff clears the leftover BASIC power-on banner before the DOS
; sign-on via a DIRECT name-table fill of spaces (characterised black-box: 0 CHPUT
; form-feeds, no mode switch -- tier2-oi3-spec.md §2), and homes the cursor (stock's
; CSRY/CSRX = $01/$01 at COMMAND.COM $0100). Ours never cleared, so it inherited BASIC's
; screen (banner ROW10-13) and printed the sign-on from CSRY=$0F -> A> at ROW23. This
; reproduces BOTH halves so ours' A> lands at ROW09 matching stock.
;   (i) FILVRM ($0056): A=byte, BC=length, HL=VRAM addr -> fill VRAM[$1800..$1AFF] with
;       $20 (768 = 32*24 SCREEN-1 name-table cells; namebase=$1800 confirmed by the
;       `screen` probe on BOTH machines). Called via the main-ROM inter-slot path
;       (pg0_mainrom_in/out) exactly as conout_body calls CHPUT: page 0 is RAM here
;       (init.asm did page0_ram_in), so we page the main BIOS ROM back in from EXPTBL[0].
;   (ii) cursor-home: CSRY ($F3DC) := 1, CSRX ($F3DD) := 1 (documented work-area sysvars).
; STAY-DI (OI-3 §5.3): the caller (init.asm) holds DI across the whole page-0-RAM handoff
; until MSXDOS.SYS is entered (init.asm:294); we restore page 0 but do NOT `ei` here, to
; avoid an interrupt firing mid-handoff while $0038 is not yet the BIOS handler.
; MUST preserve IX = $F195 (DRVA_DPB, required into MSXDOS.SYS, §8.31/M18) -- FILVRM does
; not contractually preserve IX -> push/pop guards it.
; CLEAN-ROOM: FILVRM $0056 + the $1800/768 SCREEN-1 name-table geometry + CSRY/CSRX
; $F3DC/$F3DD are all documented MSX BIOS ABI / work-area sysvars (cited, not decoded).
dos_clear_screen:
                push    ix                  ; IX=$F195 (DRVA_DPB) must survive into MSXDOS.SYS
                call    br_mainin_tmpl      ; main BIOS ROM -> page 0 (portable, EXPTBL[0])
                ld      a, $20              ; A = space
                ld      bc, $0300           ; BC = 768 = 32*24 name-table cells
                ld      hl, $1800           ; HL = SCREEN-1 name-table base
                call    $0056               ; FILVRM: VRAM[$1800..$1AFF] := $20
                call    pg0_mainrom_out     ; restore page 0 = RAM (STAY-DI: no ei)
                pop     ix
                ld      hl, $0101           ; D-DISKREREAD (2026-11-26): one word store, not two byte stores
                                            ; (the sole caller, dos_handoff, reloads HL and A next), 2 B
                ld      ($F3DC), hl         ; CSRY := 1, CSRX := 1 (home row/col, 1-based)
                ret
                ds      $5E10 - $, $00  ; pad up to the pinned $5E10 entry
mb_5e10:                        ; D-DISKROOM (2026-11-25): the pinned $5E10 entry FALLS INTO basent_body,
                                ; which follows at once -- it was a `jp` to the next byte (3 B for init_gate's guard)
; basent_body -- $4022. Runs in page 1 (this ROM), so the switch away from it
; happens in a copy of basent_tramp placed in page-3 RAM (SECTOR_BUF: scratch).
; D-BOOTDATE: when MSX-DOS never started (the mark is not DOS_SIG -- a boot code
; that could not open MSXDOS.SYS jumps here from inside dos_handoff, whose mark is
; DOS_TRY), the restart asks the date first, as both references do
; (scratchpad/bootdate_run.out); MSX-DOS's own `BASIC` (DOS_SIG) does not ask.
; 🔴 D-BASENTRUN (2026-10-22): a THIRD caller -- a BASIC program, after the boot is
; over (the mark is neither DOS_SIG nor DOS_TRY). The CF-3300 then shows a fresh
; BASIC and nothing else: no date question, and AUTOEXEC.BAS does NOT run again
; (scratchpad/basent_autoexec.out: our own AUTOEXEC.BAS counts its runs in RAM and
; calls $4022 -- the CF-3300 reads 1, zerobas read 4). MemMan's installer does
; exactly this on a machine with no mapper, and zerobas booted the disk round and
; round. 'B','B' asks for that plain restart; hk_auto clears it.
basent_body:
                di
                ld      hl, (BASENT_REQ)
                ld      a, l
                ld      l, 'B'
                cp      low DOS_SIG         ; 'D': DOS_SIG and DOS_TRY share it
                ld      a, h
                ld      h, 'B'              ; 'B','B': from a running program
                jr      nz, bb_mark
                cp      high DOS_SIG
                ld      h, 'A'
                jr      z, bb_mark          ; 'B','A': from MSX-DOS
                cp      high DOS_TRY
                ld      h, 'D'
                jr      z, bb_mark          ; 'B','D': from a boot loader
                ld      h, 'B'
bb_mark:
                ld      (BASENT_REQ), hl
                ld      a, h                ; (kept across the copy)
                ld      hl, basent_tramp
                ld      de, SECTOR_BUF
                ld      bc, basent_tramp_end - basent_tramp
                ldir
                cp      'B'                 ; D-BASENTRUN: from a running program the
                jp      z, SECTOR_BUF + (bt_keep - basent_tramp) ; hooks STAY: the
                jp      SECTOR_BUF          ; CF-3300 keeps four planted ones across
                                            ; $4022 (scratchpad/basent_hooks.out)
; basent_tramp -- position-independent: no absolute jumps or calls inside it.
basent_tramp:
                ld      hl, $FD9A           ; every hook back to RET, as at power-on:
                ld      de, $FD9B           ; DOS's must not survive into BASIC; the
                ld      bc, $FFC9 - $FD9A   ; ROM INITs main reruns put theirs back
                ld      (hl), $C9
                ldir
                ; D-TPAPTAIL (c): and the two $F368 entries DOS pointed at its page-1
                ; bodies (install_dos_env) -- in BASIC they are BASIC's RAM again. A USR
                ; routine calling $F368 or $F368+3 after A>BASIC: on the CF-3300 the
                ; slots come back as they went in, before a DOS session and after one;
                ; zerobas rewrote slot 3's subslot register (scratchpad/f368use_run.out).
                ; C-BIOS has RETs here before any DOS.
                ld      a, (hl)             ; D-DISKREREAD (2026-11-26): $C9 -- the ldir ended with HL =
                                            ; $FFC9, a cell it just filled with $C9, 1 B
                ld      (WA_JMPTAB), a
                ld      (WA_JMPTAB + 3), a
bt_keep:
                ; 🔴 D-DISKROMHOST step 4(d): and HIMEM back to $F380, as at power-on. The INITs main reruns
                ; claim from it again, and init_gate's guard declines a HIMEM that is not $F380 -- MSX-DOS's
                ; $DF93 (or Disk BASIC's, from a program) left here read as "a cartridge claimed before us",
                ; and A>BASIC came up with no disk (scratchpad/AE_room_dosbasic.out).
                ; Scheme D (2026-11-26): STILL $F380, not $F380 - N, with a foreign claim. Main reruns EVERY
                ; cartridge INIT from here, the foreign one too, and it claims again from what it is handed:
                ; $F380 hands it the power-on state, so it takes its N again and init_gate measures the same N
                ; (the CF-3300 reruns no INIT and keeps $DE77 - N: the same HIMEM). $F380 - N would make a
                ; claimer that subtracts from HIMEM take 2N (scratchpad/AE_schemeD.md).
                ld      hl, $F380
                ld      (HIMEM), hl
                ld      sp, $F2EC           ; the SP the BIOS enters INIT with
                in      a, ($A8)
                and     $F0                 ; pages 0/1 -> primary slot 0, the BIOS
                out     ($A8), a            ; slot; pages 2/3 stay the RAM they are
                ld      hl, ($4002)         ; the main ROM's cartridge INIT
                jp      (hl)
basent_tramp_end:
; --- sav_fits / sav_full: tokenised SAVE's half of D-SAVEFULLSTAMP ------------
; Source: black-box measurement on the CF-3300 (entry, size and file bytes the
; machine wrote; no ROM byte read); own code over our own FAT engine.
; Sited here because basic/sv-savdisk.inc's region (pinned at $75A5) has no
; room. MEASURED on the CF-3300 (scratchpad/savefull_probe.py): a program that
; cannot fit is refused WHOLE -- the $FF marker alone, size 1, one cluster, NO
; Ctrl-Z (BSAVE's refusal has one) -- then 66.
sav_fits:
                ld      hl,(PRGEND)
                ld      de,(TXTTAB)         ; D-TXTTAB: the image starts at TXTTAB
                or      a
                sbc     hl,de
                ld      de,2
                add     hl,de               ; HL = marker + image - 1 (the image is
                                            ; TXTBASE..PRGEND+1), CF = bit 16
                                            ; D-DISKREREAD (2026-11-26): FALLS INTO fat_fits (the include
                                            ; below), not a `jr` to it, 2 B -- A = 0 fits, $FF does not
                include "basic/fatfits-body.inc"     ; fat_fits (D-SAVEFULLSTAMP): here,
                                                     ; not with fat-prim-body (no room)
; (sav_full is bsv_full's tail since D-CALLSTMT's carve, 2026-11-13: basic/sv-bsvdisk.inc)

; (hk_aopen/hk_agetb sited here since D-SAVEFULLSTAMP (2026-10-02): the region
;  pinned at $75A5 needed their bytes for fat_count_upto's early exit)
; --- hk_aopen: open an ASCII program file and prime the read (step 11) -------
; `MERGE` and ASCII `LOAD` arrive here. Main keeps the LINE loop and the
; tokeniser -- that is not a concession, it is what the reference does: its disk
; side never owns MERGE's line loop, the crossing is entered exactly ONCE for a
; 2-line file and once for a 100-line one, and the per-LINE cells sit in a band
; NEITHER measured vendor claims (expansion-protocol.md §8.6a, D-MERGESHAPE).
;
; 🔴 MOUNT AND FIND SEPARATELY, for hk_dpload's reason: their two carries are
; what tells `File not found` from a mount/I-O fault. `fat_io_find` is the
; open minus the mount (D-BLNF; the combined fat_io_open it once split is gone
; since S10.C), and it leaves the stream primed, which is why nothing else is
; needed here.
; ⚠️ DISKOP_STATUS IS WRITTEN ONCE, LAST, on every arm -- it is a SHARED channel
; (main's FAT primitives marshal through the same cell) and pre-setting it is
; what made `NAME ... AS ...` answer Syntax error at an empty drive (§6.6an).
hk_aopen:
                ei                          ; D-ASAVEDI (2026-11-06): the hook arrives DI
                                            ; (D-HOOKDI's class) -- the open's directory /
                                            ; FAT reads then ran fully masked: 258 ms on
                                            ; RUN "P2.BAS" (scratchpad/asavedi_pc_chain2.out)
                call    fat_mount           ; D-DISKREREAD (2026-11-26): each status loaded BEFORE its
                ld      a,3                 ; test (`ld` keeps the carry; neither call takes A), not in
                jr      c,hka_st            ; two stubs, 4 B -- 3: mount / I-O
                call    fat_io_find
                ld      a,1
                jr      c,hka_st            ; 1: not found
                xor     a                   ; 0: open and primed
hka_st:
                ld      (DISKOP_STATUS),a
                scf                         ; CF=1: mine
                ret

; --- hk_agetb: one byte of the open ASCII file (step 11) ---------------------
; Sources: the per-byte shape is MEASURED, not chosen -- disk/docs/
; expansion-protocol.md §8.6a (D-MERGESHAPE) counts $FE8A (H.INDS) entered
; exactly once per byte of an ASCII LOAD or MERGE on both reference vendors,
; and zero times on the tokenised path. The register contract (BC crossing
; intact both ways) is spec-diskbasic-hook-rearchitecture.md's measured ABI.
; Clean-room: entry counts and register file only; no reference ROM byte read.
; 🔴 THE BYTE COMES BACK IN C, NOT IN CF+A, AND chan_gate IS WHY. The gate uses
; CF as the CLAIMED/unclaimed answer (`or a` / `jp (hl)` / `cg_back: ret c`), so
; a handler cannot also return a disposition in it. C carries the byte -- BC
; crosses intact both ways (the measured ABI) -- and DISKOP_STATUS carries EOF.
; Main's dsk_agetbyte turns the pair back into the `A = byte, CF = EOF` contract
; ARL_GETBYTE's other sources already honour.
hk_agetb:
                ei                          ; D-ASAVEDI: a refill is sector I/O (hk_chin's
                                            ; reason, disk/fat.asm)
                call    fat_io_getbyte      ; A = byte, CF set = EOF
                ld      c,a
                sbc     a,a                 ; D-DISKREREAD (2026-11-26): the status from the carry, not
                and     1                   ; a branch, 2 B -- 1 = EOF, 0 = C holds a byte (only CF is
                                            ; read on return: main's cg_back; fopen_cross re-tests A)
                jr      hka_st              ; D-DISKROOM: hka_st's tail is this one, byte for byte

; --- hk_format: CALL FORMAT's geometry menu + tenant dispatch, RUNNING IN THE DISK ROM (docs/spec-basic-nodisk.md §9) ---
; D-FMTHOOK (C6-FORMAT-TO-DISKROM, 2026-10-03). Behind H_FORM ($FFAC): H.FORM in
; the MSX2 TH hook table, and one of the 35 cells the CF-3300's disk ROM claims
; (docs/spec-basic-nodisk.md §5.2). Main's do_format keeps only its DISKSLOT_OK
; gate and the chan_gate_bare crossing; what used to follow the gate in
; basic/format.asm -- the "1=360k 2=720k? " prompt, the re-prompt loop and the
; subrom_call to the sub-ROM page-1 format_tenant -- is here, same behaviour.
;   * The PROMPT goes through CHPUT (hke_pr's loop, BIOS page 0) because its
;     text is in THIS ROM: a call-back to main's print_string would run with
;     this ROM switched out and read main page 1 at the text's address.
;   * The LINE is read by main's read_line through `calbak` (an inter-slot
;     call, MSX2 TH): the wait in CHGET happens with MAIN in page 1, so PLAY
;     and the traps tick exactly as they did while the menu was resident
;     (basic/repl.asm read_line's own rule: a page-1 tenant must never wait).
;   * subrom_call is main's LOW-REGION dispatcher (disk/basic-resident-abi.inc),
;     callable by absolute address while this ROM holds page 1; CALSLT nests,
;     and page 1 comes back to this ROM when the tenant returns.
;   in   nothing (FOPEN_SEL is not consulted; the gate stored nothing)
;   out  FMT_RESULT  = 0 ok / 1 failed (D-DSKFMT: the format is DSKFMT's; the
;                      sub-ROM tenant this once dispatched to retired 2026-11-11)
;        CF = 1 (claimed), which is what chan_gate_bare tests
; 📍 SITED IN THE FILL ABOVE `ds $5FE5 - $` for hkc_resolve's reason: the tail
; after runtime.asm is nearly full, and code here shifts nothing downstream.
hk_format:
                ; 🔴 D-DSKFMT (2026-11-10): CALL FORMAT is the CF-3300's -- the dialogue and
                ; DSKFMT, as MSX-DOS's `format` ($4025); it was a quick format through the
                ; sub-ROM (format_tenant), system area only, "1=360k 2=720k? ".
                ld      hl,fmt_bkey         ; keys: ONE each, waited for in MAIN
                call    fmt_basic
                sbc     a,a
                and     1                   ; 0 done / 1 failed: main's do_format reads it
                ld      (FMT_RESULT),a      ; (basic/format.asm; a failure is its load error --
                scf                         ; the old disposition, TIER 6 to refine)
                ret                         ; claimed
; stmt_hdl -- the ROM header's STATEMENT handler ($4004): ANOTHER BASIC's CALL (the
; CF-3300's own; zerobas-BASIC reaches hk_format through H_FORM). PROCNM = the name,
; 0-terminated, HL = the text after it; out CF = 0 done (HL kept), CF = 1 not ours.
; D-CALLSTMT (2026-11-10): FORMAT only -- CALL SYSTEM's body calls back into
; zerobas-BASIC, so under another BASIC it is still "not ours" (Syntax error).
stmt_hdl:
                push    hl
                ld      hl,PROCNM
                ld      de,fmt_word
                ld      b,7
sh_lp:
                ld      a,(de)
                cp      (hl)
                jr      nz,sh_no
                inc     hl
                inc     de
                djnz    sh_lp
                ld      hl,innoe_body       ; keys through our own CHGET body
                call    fmt_basic
                pop     hl
                or      a                   ; done (a failed format: TIER 6)
                ret
sh_no:
                pop     hl
                scf
                ret
fmt_word:       db      "FORMAT",0
; fmt_basic -- the dialogue from BASIC (HL = the key routine): the work area is the
; free RAM from the arrays' end (STREND) to the STACK, less 256 B for the dialogue's
; own calls. 🔴 Not FRETOP: the stack lies between the arrays and the strings, so
; FRETOP - 64 (the first cut, 2026-11-10) reached over the live stack on a full
; machine; it only worked with much free memory.
fmt_basic:
                push    hl
                ld      hl,-256
                add     hl,sp
                ld      de,(STREND)
                sbc     hl,de               ; the room (CF: none); the add's CF (always set:
                                            ; SP > 256) takes one byte more -- harmless
                jr      nc,fbs_room
                ld      hl,0                ; none: DSKFMT answers 14
fbs_room:
                ld      b,h
                ld      c,l
                pop     hl
                jp      fmt_dlg

; --- hk_system: CALL SYSTEM / _SYSTEM -- back to MSX-DOS (D-CALLSYSTEM; oracle: CF-3300) ---
; Measured on the CF-3300 (scratchpad/callsystem_run.out, callsystem_dos2.out,
; callsystem_swap.out, callsystem_warm.out, callsystem_peek.out):
;   * booted from a data disk: Illegal function call (5);
;   * booted into MSX-DOS, then BASIC: back at A> -- a WARM return (no banner, no
;     date prompt, no AUTOEXEC.BAT; the screen and its mode kept), every open file
;     CLOSED first, MSXDOS.SYS and COMMAND.COM read from the disk again (with
;     MSXDOS.SYS deleted the disk's own boot code says Boot error; without
;     COMMAND.COM, MSXDOS.SYS asks for the DOS disk);
;   * a disk in the drive that does not boot: Syntax error; `SYSTEM("DIR")` or any
;     tail: Syntax error; a program line behaves as a typed one.
; So this runs INIT's own hand-off (init.asm boot_disk_go: steps 4-7) from BASIC,
; with MSX-DOS's cold/warm seed $F340 set non-zero (WARM) and without
; dos_handoff's screen clear. Without the seed DOS starts COLD here -- banner,
; date prompt (scratchpad/callsystem_gate_noseed.out). 🔴 Knife K-CS4 first said the store
; moved NOTHING: its probe run had died on that very date prompt, and the
; harness scored the rows it never printed as unmoved (callsystem_knives_r1.out).
; On the CF-3300 $F340 reads 243 after BOTH kinds of boot (callsystem_peek.out),
; so the cell is DOS's warm flag, not a "booted DOS" mark.
; BASIC's stack ($F380 down, sysvars.inc FN_STK_FLOOR) is already in the area
; INIT's boot stack uses, clear of everything the boot loads, so it stays.
; "Booted into MSX-DOS" is the work-area cell $F346 since D-F346, as on the
; CF-3300: set_ramad clears it, MSXDOS.SYS's $50A9 call sets it to 255, and a
; program may POKE it (it was our own DOS_SIG mark in BASENT_REQ, which $4022
; still uses). 📍 In the fill above `ds $5FE5 - $`, beside hk_format.
;   in   STRSCR+1 = main's cursor at the CALL name (basic/format.asm exc_name)
;   out  never returns: MSX-DOS, or an error raised in main (DISKOP_ERR, disk_error)
hk_system:
                ld      hl,(STRSCR+1)
                ld      de,hsy_name
                ld      b,6
hsy_lp:
                ld      a,(de)
                xor     (hl)
                and     $DF                 ; the same letter, either case
                jr      nz,hsy_syn          ; another name: Syntax error, as before
                inc     hl
                inc     de
                djnz    hsy_lp
hsy_end:
                ld      a,(hl)              ; the name must END the statement
                inc     hl
                cp      ' '
                jr      z,hsy_end
                or      a
                jr      z,hsy_ok
                cp      ':'
                jr      nz,hsy_syn
hsy_ok:
                ; D-F346 (Joost 2026-10-23: "if you boot into basic and not into dos
                ; first, if you do poke &hf346,1 you can do call system"): the CF-3300
                ; goes by the work-area cell $F346 -- 0 after a data-disk boot, 255
                ; once MSX-DOS ran; POKE 1 then CALL SYSTEM boots DOS, POKE 0 after a
                ; DOS boot is Illegal function call (scratchpad/f346_run.out).
                ld      a,(DOS_F346)
                or      a
                ld      a,5                 ; MSX-DOS never booted: Illegal function call
                jr      z,hsy_err
                ld      ix,fch_close_all
                call    calbak              ; close every file, as the CF-3300 does
                di                          ; D-HOOKDI: calbak returns EI now; the
                                            ; DOS boot below has always run masked
                ; The function-key row goes, BEFORE the disk is tried: at A> the
                ; CF-3300 has CRTCNT 24 / CNSDFG 0 (ours had 23 / $FF and the row
                ; still painted), and after a refused disk BASIC is back without it
                ; (scratchpad/callsystem_cells.out, callsystem_swap.out `gone`).
                ; The 5 above keeps it, as there. Documented work-area cells; the
                ; row is blanked here because C-BIOS's ERAFNK is not ours to rely on.
                xor     a
                ld      ($F3DE),a           ; CNSDFG: the key row is hidden
                ld      a,24
                ld      ($F3B1),a           ; CRTCNT: the console gets all 24 rows
                ld      a,($FCAF)           ; SCRMOD: only the two text modes have the row
                ld      hl,($F3B3)          ; TXTNAM (SCREEN 0) + 23 rows of 40
                ld      de,23*40
                ld      bc,40
                or      a
                jr      z,hsy_row
                dec     a
                jr      nz,hsy_rd
                ld      hl,($F3BD)          ; T32NAM (SCREEN 1) + 23 rows of 32
                ld      de,23*32
                ld      c,32
hsy_row:
                add     hl,de
                ld      a,' '
                call    $0056               ; FILVRM: the bottom row, blank
hsy_rd:
                call    boot_rd             ; drive A's sector 0 to $C000, as INIT does
                jr      c,hsy_syn           ; (disk/runtime.asm, D-DISKCARVE3)
                jr      nz,hsy_syn
hsy_boot:
                or      a                   ; Cy = 0: the step-5 call
                ; CALL SYSTEM calls the boot sector it just read: code from the disk, in no image;
                ; the boot protocol hands it Cy, and DE / HL as on the CF-3300 (D-BOOTREGS)
                ; z80flow: targets external reads=DE,HL,F
                call    BOOT_ENTRY
                di
                call    page0_ram_in
                call    lay_page0_env
                ld      ix,DRVA_DPB
                ld      a,1
                ld      ($F340),a           ; MSX-DOS's cold/warm seed: WARM
                call    dos_handoff_keep    ; a DOS disk does not come back
                call    page0_ram_out
                ei
hsy_syn:
                ld      a,2                 ; Syntax error
hsy_err:
                ld      (DISKOP_ERR),a
                ld      ix,disk_error
                jp      calbak              ; raises; does not return
hsy_name:       db      "SYSTEM"

; --- init_gate: SHIFT held at power-on keeps the disk ROM out (D-BOOTKEYS; oracle: CF-3300) ---
; On the CF-3300 SHIFT held from power-on leaves Disk BASIC out entirely: HIMEM
; stays $F380, FRE(0) +5385 B, DSKF is Illegal function call (scratchpad/
; bootkeys_run.out). The ROM header's INIT word points here: SHIFT (keyboard row
; 6 bit 0, low when held -- SNSMAT $0141, MSX2 TH) held -> touch nothing (no
; hooks, no work area, no DOS boot) and leave DISKSLOT_OK ($FB21, DRVTBL's drive count) 0:
; a diskless machine (main's scan writes nothing there since D-DRVTBLNODISK). Else the
; normal INIT, with A (this ROM's slot byte, which init reads) intact.
init_gate:
                push    af
                ld      a, 6
                call    $0141               ; SNSMAT: A = row 6, a 0 bit = held
                rrca                        ; SHIFT -> Cy
                jr      c, ig_init          ; not held: the normal INIT
ig_out:
                pop     af
                xor     a
                ld      (DISKSLOT_OK), a    ; 0: no disk ROM (D-DRVTBLNODISK: main no longer inc's it)
                ret
ig_init:
                ; 🔴 D-DISKROMHOST step 4(d), SCHEME D (2026-11-26, scratchpad/AE_schemeD.md; was THE GUARD
                ; of 2026-11-25, which declined any HIMEM but $F380). A cartridge scanned before us that
                ; claimed N bytes leaves HIMEM = $F380 - N. MEASURED on both references (black box:
                ; scratchpad/AE_wasize.md, AE_wasize_claimN.out, AE_wasize_threshold*.out): they install
                ; anyway, keep every cell of $F1C9-$F37F at its address (written over the claim) and move
                ; every NUMBER they publish -- HIMEM at each claim step, FRE, the $4030 value (and with it
                ; MSX-DOS's $0006), DRVTBL+1 / +3 -- down by exactly N. So do we, for N <= NMAX: our
                ; first own cell below $F1C9 is HOOK_SLOT ($F1C8), which N = 440 reaches. Above NMAX (or a
                ; HIMEM over $F380) decline exactly as SHIFT held does. Cy is SET here (the jr c above),
                ; so the sbc gives HIMEM - $F381 = -N-1, and adding NMAX+1 carries iff 0 <= N <= NMAX.
                ; CLEAN-ROOM: HIMEM ($FC4A, MSX2 TH work area) and the references' published cells,
                ; read as a black box; own code. HL / DE are free: init reads A only.
                ld      hl, (HIMEM)
                ld      de, $F380
                sbc     hl, de              ; HL = -N-1
                ld      de, CLAIM_NMAX + 1
                add     hl, de              ; HL = NMAX - N; Cy: 0 <= N <= NMAX
                jr      nc, ig_out          ; a claim we cannot follow: decline
                ld      de, GETWRK_AREA - CLAIM_NMAX
                add     hl, de
                ld      (CLAIM_D), hl       ; GETWRK_AREA - N (disk/equates.inc)
                xor     a
                ld      (VERIFY_F), a       ; D-VERIFYREAD: VERIFY starts OFF (an own cell, nothing else clears it)
                jp      sltwrk_pub          ; publish SLTWRK, then init (the room is there)

; (calslt_body, CALSLT for a page-0 target under MSX-DOS -- D-DOSMODE40 -- was here;
; D-TPAP1's br_calslt enters the main BIOS's own CALSLT, which covers that case.)

frp_full:                                   ; fat_rand_put's record past any disk (D-RECBIG): Disk
                jp      fac_full            ; full, pending -- fac_full's tail byte for byte (D-DISKROOM)

; (write_sector stood here until D-DISKROOM (2026-11-25) moved it in front of the $7F7C pin,
; disk/runtime.asm: this region needed the bytes for bt_keep's HIMEM reset.)
; bcw_own -- 🔴 D-SEQMIX2 (2026-11-14): FCLOSE ends write mode only for the WRITER'S OWN
; file. BDOS_WRMODE is one global; a copy that closed its SOURCE before the destination's
; last SEQWRITE had that write answered $FF and the record lost (scratchpad/
; seqmix_close2.out: the CF-3300 2 records, zerobas 1). Same file = its FCB's first cluster
; (+26) is the writer's (BDOS_WRFIRST) -- tested BEFORE close_read, whose cr_copy1 rewrites
; BDOS_WRFIRST. In: DE = the FCB. A, F, BC, HL clobbered; DE kept for close_read.
bcw_own:
                ld      hl, 26
                add     hl, de
                ld      a, (hl)
                inc     hl
                ld      h, (hl)
                ld      l, a                ; HL = the closed file's first cluster
                ld      bc, (BDOS_WRFIRST)
                or      a
                sbc     hl, bc
                jr      nz, bcw_x           ; another file: the writer stays open
                xor     a
                ld      (BDOS_WRMODE), a    ; the writer's own file: no longer for write
bcw_x:                                      ; D-DISKREREAD (2026-11-26): one `jp`, reached by `jr`, 1 B
                jp      bdos_close_rd
; fmt_err -- the motor off, A / F kept (FORMAT's error exit; MOVED here by D-DISKCARVE6 step 3).
fmt_err:
                push    af
                call    mtoff
                pop     af
                ret
                ds      $5FE5 - $, $00  ; pad to the first free-region kernel entry
                jp      k_5FE5          ; $5FE5
; --- S10.D: the RANDOM record writer, in the free region after the $5FE5 veneer
; (no canonical entry inside; hk_dget and its helpers are in the fill above
; `ds $5FE5 - $`). IX = the channel's FCB row throughout.

; fat_rand_put -- write the record (block+9 [0..reclen), D-FLDDESC; it was
; FAT_DBUF[0..reclen) while the record travelled) as record GP_RECNO: a
; read-modify-write of each sector it touches, the chain extended as needed, the
; size grown to recno * reclen. 🔴 D-PUTDIR: NO DIRECTORY WRITE -- the CF-3300
; leaves the entry alone until CLOSE (hk_chclose stamps it, once).
; 🎯 D-STRADDLE: a record that crosses a sector boundary is written in TWO
; passes (tests/test_rand_straddle.py builds the expected image independently).
;   in:  GP_RECNO, GP_FLAGS = 1 (hk_dget), IX = the row
;   out: CF = 0 written; CF = 1 disk full / I/O (the code pending in DISKOP_ERR)
fat_rand_put:
                call    frnd_calc           ; GP_SEC, GP_WITHIN, GP_RECLEN, the cursor
                jr      c, frp_full         ; past any disk: Disk full, nothing touched
frp_pass:
                call    frnd_locate         ; GP_PHYS, the chain extended
                ret     c
                ; did the file already have this sector? GP_SEC * 512 < size --
                ; the sub-ROM's `GP_SEC < ceil(size / 512)` (GP_OLDNSEC), asked of
                ; the row's size directly: it does not move until the record is
                ; written (frnd_update_size), so it is still the OLD size here.
                ld      hl, (GP_SEC)
                ld      a, h
                ld      h, l
                ld      l, 0
                sla     h
                rla                         ; A:HL = GP_SEC << 9
                call    rnd_cmp
                jr      c, frp_readold      ; yes: read it first
                call    rh_flush            ; D-PUTCACHE: a held sector out first --
                ret     c                   ; fill_sec is about to overwrite FAT_MBUF
                ld      hl, FAT_MBUF
                call    fill_sec            ; past the old end: start from spaces
                jr      frp_overlay
frp_readold:
                call    rnd_rd
                ret     c
frp_overlay:
                call    frnd_pass_len       ; BC = what fits in this sector
                ld      hl, (GP_WITHIN)
                ld      de, FAT_MBUF
                add     hl, de
                ex      de, hl              ; DE = FAT_MBUF + within
                ld      hl, (GP_SRC)        ; the record, advanced per pass
                ldir
                ld      (GP_SRC), hl
                ; D-PUTCACHE (W4, docs/spec-diskwcache.md): the sector is HELD in
                ; FAT_MBUF, not written -- as the CF-3300, which writes a data sector
                ; once per 512 B the records fill and at CLOSE (scratchpad/fdcput_cf.out).
                ; read_sector writes it before any other read, and serves a re-read
                ; of it (the next PUT / GET in the same sector) from the buffer.
                ld      hl, (GP_PHYS)
                ld      (RND_HSEC), hl
                call    rnd_step
                jr      nz, frp_pass        ; the record's second sector
                ; FALLS INTO frnd_update_size

; frnd_update_size -- size := max(size, recno * reclen), 32 bits, in the row
; (+16..19), where H_CHIN op 6 reads LOF (S10.E; main's mirror of it, dout_mirror,
; is gone): D-LOF §4c, a RANDOM PUT moves LOF live, before the directory knows.
; CF = 0.
frnd_update_size:
                call    frnd_off            ; A:HL = k * reclen (CF = 0: it was
                ld      de, (GP_RECLEN)     ; checked before the write)
                add     hl, de
                adc     a, 0                ; A:HL = (k + 1) * reclen
                call    rnd_cmp
                ccf                         ; (CF must come back 0: hk_dget reads
                ret     nc                  ;  CF = 1 as a failed write) -- the file
                                            ;  already reaches past it
                ld      (ix+16), l
                ld      (ix+17), h
                ld      (ix+18), a
                xor     a                   ; CF = 0 (and A = 0, as dout_mirror's
                ld      (ix+19), a          ; tail left them) -- D-DISKREREAD (2026-11-26): the
                ret                         ; zero from A, 1 B

; rnd_rd -- FAT_MBUF := sector GP_PHYS. CF = the read failed.
rnd_rd:
                ld      de, (GP_PHYS)
                jp      rs_mbuf

; rnd_step -- the next pass of a straddling record: Z = the record is done; else
; GP_WITHIN := 0, GP_SEC += 1, NZ. CF = 0 either way.
rnd_step:
                ld      hl, (GP_LEFT)
                ld      a, h
                or      l
                ret     z
                ld      hl, 0
                ld      (GP_WITHIN), hl
                ld      hl, (GP_SEC)
                inc     hl
                ld      (GP_SEC), hl        ; (inc hl leaves the flags: NZ)
                ret

; fill_sec -- 512 bytes at HL := spaces. fill_sp -- BC+1 bytes at HL := spaces.
; CF = 0. Clobbers BC, DE, HL. (fill_rec, the record's own fill, is beside
; hk_lrset: this fill has no room before the $607B veneer.)
fill_sec:
                ld      bc, 511
fill_sp:
                ld      (hl), ' '
                ld      d, h
                ld      e, l
                inc     de
                ldir
                or      a
                ret

; cpmver_body -- BDOS $0C CPMVER (D-DISKCARVE's move; here since D-CALSLTP0). Entered
; only by `jp` from the BDOS dispatch.
cpmver_body:
                ld      a, $22
                ld      b, 0
                ret
                ds      $607B - $, $00
                jp      k_607B          ; $607B  (BDOS callback; spec §5.2)

; ===== FDC-window P0 fix (2026-07-04): position-free bodies relocated OUT of the
; $7F80-$7FBF FDC register window into this free-region corridor ==================
; The National WD2793 FDC registers ($7FB8-$7FBF) are memory-mapped AND mirrored x8
; across the whole $7F80-$7FBF window, so ROM bytes there read back as register
; values. fdc_useslot_body (create's dir-slot claim) and p0_env_tab's head had drifted
; into that window (~M27), silently breaking create/write persistence. All three
; blocks are reached only by label -- their absolute address is irrelevant -- so they
; live here, in the 5.4 KB of dead $00 pad between the $607B and $75A5 COMMAND.COM-load
; kernel veneers (0 canonical entries, no FDC window). The trailing `ds $75A5 - $`
; still pins k_75A5 exactly -> net-zero canonical addresses. Spec:
; disk/docs/tier2-remediation-spec.md Phase B.

; fdc_entloop_body / fdc_useslot_body — create's directory-slot scan + claim.
; Position-free: reached only by fat.asm's `fdc_entloop`/`fdc_useslot` veneers.
; Kept contiguous, same relative order (M21a-RC-1 convention). Neither falls through
; (each ends in an absolute jp), so the relocation is byte-for-byte behaviour-neutral.
; p0_env_tab — the page-0 RST/CALLF/INT vector template, read by lay_page0_env
; THROUGH the ROM page (`ld hl, p0_env_tab`; pageenv.asm). Formerly pinned at $7FB7,
; straddling the FDC window (its head entries read back as register garbage, tolerated
; only empirically); now fully clear of every window so EVERY entry reads correctly.
; The $0030 entry points DIRECTLY at callf_body_body (M22a: no intermediate veneer).
; (cell, handler) pairs; terminated by a 0 cell. No two JP triples overlap.
p0_env_tab:
                dw      $000C, BR_RDSLT ; RST 8  RDSLT  -- D-TPAP1: page-3 bridges to the
                dw      $0014, BR_WRSLT ; RST 10 WRSLT     main BIOS's own entries
                dw      $001C, BR_CALSLT ; RST 18 CALSLT    (br1_tmpl, pageenv.asm)
                dw      $0024, BR_ENASLT ; ENASLT
                dw      $0030, BR_CALLF ; RST 30 CALLF
                dw      $0005, BR_BDOS  ; BDOS until MSXDOS.SYS lays its own (D-TPAPTAIL:
                                        ; the $F37D stub's road with RAM in page 0); the
                                        ; disk ROM in around it (br3, D-BOOTREGS)
                dw      $0038, INT_H_HIRAM ; maskable-int vector -> A-3 high-RAM handler
                                           ; (NOT page-1 int_h: page 1 is reclaimed by the TPA)
                dw      0               ; end of table

; ===== M28 (tier2-m28-blockrandom-spec.md): $23 FSIZE + $26 WRBLK ===============
; Completes the block/random FCB surface: a real GET FILE SIZE and a real RANDOM
; BLOCK WRITE. Both land here, in the same free-region corridor as the FDC-window
; fix bodies (0 canonical entries, no FDC window) -- net-zero, `ds $75A5 - $` below
; still pins k_75A5 exactly.

; ff_secloop..ff_found — RELOCATED VERBATIM from disk/fat.asm's old $47B9-$4828 span
; (M28 §3.1/§4, [LANDED-B]). $47BE (5 bytes inside the old ff_secloop, the `jr z,
; ff_notfound`) is the kernel's FIXED canonical $26 WRBLK dispatch entry -- a
; collision freed the same way the FDC-window fix and M26 free colliding canonical
; addresses: relocate OUR code, not the kernel's $D8BE dispatch table (not ours to
; edit). Position-free: reached only by fat_find_body's `jp ff_secloop` (runtime.asm)
; and its own internal `jr ff_secloop` back-edge; every ff_* label below is local to
; this block, so the whole unit moves as one byte-identical piece (no jr/jp
; conversions needed -- unlike fdc_entloop_body/fdc_useslot_body, which split into
; two separately-addressed routines, this block keeps its single entry point).
; fsize_body — MSX-DOS-1 kernel FSIZE ($23) canonical entry $501E's real body
; (M28 §3.2). Wired IN PLACE at $501E (dead-$00 pad, no relocation -- [LANDED-B]).
; Entry: DE=$DA40 (kernel 37-byte FCB copy, name pre-filled at copy+1..11),
; A=$25/B=$00/C=$00, ret=$D88A -- same dispatcher class as $4788/$4793 (M26).
; Locates the file (fat_mount + fat_find, reused wholesale from the existing dir
; search) and sets copy+33..35 = ceil(size/128) as a 24-bit little-endian count;
; not found -> A=L=$FF (map.grauw.nl _FSIZE contract). The prior stub returned a
; constant A=L=$03 for every input and never searched or wrote the field at all.
; CLEAN-ROOM: reuses fat_mount/fat_find (our own, pre-existing) + the published
; GET FILE SIZE contract; no stock routine internals decoded.
fsize_body:
                push    de
                pop     ix                  ; IX = kernel FCB-copy pointer ($DA40)
                                            ; D-DISKREREAD (2026-11-26): no `push ix / pop de` --
                                            ; DE still holds it, 3 B
                call    fat_mount
                jr      c, fsize_miss
                push    ix
                pop     hl
                inc     hl                  ; HL -> copy+1, the 11-byte 8.3 name (fat_find's
                                            ; contract; it takes no DE) -- D-DISKREREAD
                                            ; (2026-11-26): straight into HL, no `ex`, 1 B
                call    fat_find            ; Cy=0 found -> FAT_FILESIZE set
                jr      c, fsize_miss
                ; r0..r2 := ceil(FAT_FILESIZE / 128) = (size + 127) >> 7, 24-bit.
                ld      hl, (FAT_FILESIZE)
                ld      de, (FAT_FILESIZE + 2)
                ld      bc, 127
                add     hl, bc
                jr      nc, fsize_noc
                inc     de
fsize_noc:
                ; 32-bit DE:HL >>= 7 (7 iterations of a whole-value logical shift right;
                ; the discarded top byte, D, is always 0 for any file this ROM's FAT12
                ; volumes can hold).
                ld      b, 7
fsize_shift:
                srl     d
                rr      e
                rr      h
                rr      l
                djnz    fsize_shift
                ; r0..r2 = low 24 bits of DE:HL = L, H, E (LE)
                call      set_rr
                ld      (ix+35), e
                xor     a                   ; A = 0 = found
                jr      fsz_t               ; D-DISKREREAD (2026-11-26): one `ld l,a / jp` for both
                                            ; arms, 2 B (ld / jr keep the flags)
fsize_miss:
                ld      a, $FF
fsz_t:
                ld      l, a                ; L := A (A=L=0 found, the pinned success contract;
                                            ; A=L=$FF not-found)
                jp      rrnd_finish         ; jp: rrnd_finish is far below (out of jr range)

; ===========================================================================
; $26 WRBLK — RANDOM BLOCK WRITE (M28 §3.3, [LANDED-A]/[LANDED-B])
; ===========================================================================
; wrblk_body — the real $47BE canonical WRBLK entry. Entry: DE=$DA40 (kernel FCB
; copy), HL = requested record count, A=$26/B=$00/C=$00, ret=$D88A. Contract
; ([LANDED-A]): RS = copy+14..15 (0 -> 128); start record RR = copy+33..35
; (24-bit LE); byte budget = (HL*RS) & $FFFF; CR/EX (copy+32/+12) untouched;
; past-EOF writes grow the file with a CONTIGUOUS FAT chain through the gap (no
; sparse holes; gap bytes stay whatever was already on disk -- no zero-fill);
; size := max(old, new_RR*RS); RR += HL_REQUESTED (not actual -- the write-side
; advance asymmetry vs RDBLK's actual-based advance); return A=0, HL = the
; ORIGINAL requested count (preserved, carries no result). HL=0 is a size-only
; call: RR-at-or-past-EOF just bumps the size field (no data, no chain change);
; RR-before-EOF is the SHRINK path, which stock gets wrong (leaves the FAT
; inconsistent -> a later FCLOSE fails) -- signed off (§6 Q3) to do the SANE
; thing instead: free the tail chain + EOF-mark so FCLOSE keeps succeeding. This
; is an INTENTIONAL, DOCUMENTED divergence from stock's known-broken behaviour,
; not a regression (see tier2-bdos-coverage.md).
; Unlike RDRND/WRRND (which re-find the file for FAT_FIRSTCLUS/FAT_FILESIZE only
; -- they reused the last FOPEN's globals until D-RDRNDMULTI -- and never write
; the directory entry), WRBLK must
; persist size/first-cluster back to disk -- so it re-locates the entry itself
; (fat_mount + fat_find on copy+1..11) to recover FAT_DIRSEC/a dirent offset for
; fat_dir_update, exactly like fsize_body/fopen_fill_body/fren_body already do.
; This also freshly re-confirms FAT_FIRSTCLUS/FAT_FILESIZE (redundant with the
; kernel's own FOPEN for the same file, but harmless) before the per-record loop.
; CLEAN-ROOM: reuses fat_mount/fat_find/fat_alloc_cluster/fat_next_cluster/
; fat_write_fat_entry/fat_dir_update/write_sector (our own, pre-existing) + the
; published RANDOM BLOCK WRITE contract (map.grauw.nl) + the [LANDED-A] oracle
; contract; no stock routine internals decoded.
wrblk_body:
                ld      (WRBLK_REQ), hl     ; capture the requested count FIRST -- everything
                                            ; below clobbers HL (fat_mount/fat_find/the multiply)
                push    de
                pop     ix                  ; IX = kernel FCB-copy pointer ($DA40)
                ; 🔴 D-BLKIOPERCALL (2026-10-08): NO MOUNT AND NO FIND PER CALL. This
                ; re-read the boot sector and walked the root directory on EVERY
                ; WRBLK -- a 32 KB write in 256 B blocks took 8.8x the CF-3300's
                ; (disk_probe_wrblk_alt.py --wseek). Everything they recovered is
                ; in the FCB already (FOPEN / FMAKE fill it, wrblk_fcb_keep keeps
                ; it): wrblk_fcbstate, in the fill beside hk_fmake (this region has
                ; no room).
                call    wrblk_fcbstate
                jp      c, wrblk_ioerr
                ; reseed our own DTA cell (M19 lesson: the kernel's real SETDTA only
                ; ever touches DOS_DTAPTR, never BDOS_DTA).
                call    dta_from_dos
                ; resolve RS := copy+14..15, 0 -> 128 (RECSIZE)
                ld      e, (ix+14)          ; D-DISKREREAD (2026-11-26): straight into E / D, not
                ld      d, (ix+15)          ; through A, 2 B
                ld      a, d
                or      e
                jr      nz, wrblk_rs_ok
                ld      de, RECSIZE
wrblk_rs_ok:
                call    wrblk_huge          ; WRBLK_RS := DE; D-BDOSSEQ: 1 MB on -> full
                ; load RR (24-bit, copy+33..35) into WRBLK_REC
                ld      l, (ix+33)          ; D-DISKREREAD (2026-11-26): the low word through HL
                ld      h, (ix+34)          ; (reloaded just below), 3 B
                ld      (WRBLK_REC), hl
                ld      a, (ix+35)
                ld      (WRBLK_REC + 2), a
                ; HL_requested == 0 ? -> size-only / shrink path (no per-record loop)
                ld      hl, (WRBLK_REQ)
                ld      a, h
                or      l
                jp      z, wrblk_zero_path
                ; 🔴 D-WRBLKRS (2026-10-01): A BYTE TRANSFER, NOT A 128-BYTE-RECORD ONE.
                ; The old loop moved RS bytes a record but POSITIONED as if every
                ; record were 128 B (record-in-sector = REC & 3, sector = REC / 4),
                ; so RS = 1 -- MSX-DOS1's ordinary block-I/O idiom and step 10's
                ; channel shape -- put byte 255 in "sector 63": a 32-cluster chain
                ; for 256 bytes, while the CF-3300 wrote one cluster
                ; (probes/disk/disk_probe_wrblk_alt.py). Now: start = RR * RS,
                ; budget = (HL * RS) & $FFFF (the contract above), one SECTOR a
                ; step. WRBLK_REC holds the 24-bit BYTE offset and WRBLK_CNT the
                ; bytes still to write. RS = 128 writes the same bytes as before, in
                ; fewer sector writes. Nothing here touches WBUF before the loop
                ; (wrblk_off24's note).
                ld      de, (WRBLK_RS)
                ; (wrblk_mul16, its only call, inline since D-DISKROMHOST step 2b: 4 B)  ; HL = (HL * RS) & $FFFF
; wrblk_mul16 -- HL := (HL * DE) & $FFFF, MSB-first shift-and-add. D-WRBLKRS: the
; byte budget, WRBLK's documented `(HL*RS) & $FFFF`. Trashes A, BC, DE.
                ld      b, h
                ld      c, l                ; BC = the multiplicand
                ld      hl, 0
                ld      a, 16
wm16_lp:
                add     hl, hl
                ex      de, hl
                add     hl, hl              ; DE <<= 1, CY = its old MSB
                ex      de, hl
                jr      nc, wm16_no
                add     hl, bc
wm16_no:
                dec     a
                jr      nz, wm16_lp
                ld      (WRBLK_CNT), hl
                call    wrblk_off24         ; WRBLK_REC := (RR * RS) & $FFFFFF
                xor     a
                ld      (WRBLK_CURVALID), a
wrblk_loop:
                ld      hl, (WRBLK_CNT)
                ld      a, h
                or      l
                jr      z, wrblk_loop_done
                ld      hl, (WRBLK_REC + 1) ; D-DISKREREAD (2026-11-26): the shift in HL, not B:A
                srl     h                   ; (wrblk_position_sec takes BC only), 2 B
                rr      l
                ld      b, h
                ld      c, l                ; BC = offset >> 9 = sector-in-file
                call    wrblk_position_sec  ; Cy=0 ok, SECTOR_BUF <- target sector's bytes
                jp      c, wrblk_full
                ld      de, (WRBLK_REC)     ; D-DISKREREAD (2026-11-26): one word load, masked in D;
                ld      a, d                ; `and 1` leaves CF = 0 for the sbc (no `or a`), 3 B
                and     1
                ld      d, a                ; DE = offset & 511 = within
                ld      hl, 512
                sbc     hl, de              ; HL = room left in this sector
                push    de                  ; [within]
                ld      de, (WRBLK_CNT)
                push    hl                  ; D-DISKREREAD (2026-11-26): no `or a` -- CF = 0 from
                sbc     hl, de              ; 512 - within (never borrows; push / ld keep it), 1 B
                pop     hl
                jr      c, wrblk_room       ; room < remaining -> chunk = room
                ex      de, hl              ; else chunk = remaining
wrblk_room:
                ld      b, h
                ld      c, l                ; BC = chunk
                pop     hl                  ; HL = within
                push    bc                  ; [chunk]
                call    dta_chunk           ; SECTOR_BUF + within := the caller's bytes
                ld      (BDOS_DTA), hl
                call    rrnd_sector         ; DE = absolute logical sector to persist
                call    wr_sbuf
                pop     bc                  ; chunk (pop leaves the flags alone)
                jr      c, wrblk_ioerr
                ld      hl, (WRBLK_REC)
                add     hl, bc
                ld      (WRBLK_REC), hl     ; offset += chunk (24-bit)
                jr      nc, wrblk_off_nc
                ld      hl, WRBLK_REC + 2
                inc     (hl)
wrblk_off_nc:
                ld      hl, (WRBLK_CNT)
                or      a
                sbc     hl, bc
                ld      (WRBLK_CNT), hl     ; remaining -= chunk
                jr      wrblk_loop
wrblk_loop_done:
                ; new_RR := RR_start + HL_requested (24-bit + 16-bit)
                ld      l, (ix+33)          ; D-DISKREREAD (2026-11-26): straight into L / H, not
                ld      h, (ix+34)          ; through A (reloaded below), 2 B
                ld      de, (WRBLK_REQ)
                add     hl, de
                call      set_rr
                ld      a, (ix+35)
                adc     a, 0
                ld      (ix+35), a
                ; size := max(old size, new_RR * RS)
                call    wrblk_mul_rr_rs     ; WRBLK_MULACC := (ix+33..35, = new_RR) * RS
                call    wrblk_size_max
                call    wrblk_set_wrfirst
                ; (wrblk_fcb_keep, via wzp_dirupdate) D-WRBLKSEEK: FCB +16/+26/+28/+30, as stock
                ; D-WRBLKDIR (2026-11-08): NO directory update per call -- the CF-3300
                ; touches the entry only at FCLOSE (fdccount_run.out: 72 writes for 64
                ; data sectors; ours wrote the entry every call). The close writes it
                ; from the FCB (bdos_close -> close_read -> cr_dir), which
                ; wrblk_fcb_keep keeps current.
                jr      wzp_dirupdate       ; D-DISKREREAD (2026-11-26): wzp_dirupdate's tail is this
                                            ; one, byte for byte, 7 B
wrblk_zero_path:
                ; HL_requested == 0: compute target := RR (unchanged) * RS, then either
                ; grow-or-hold (target >= old size) or shrink (target < old size). RR
                ; itself is NOT advanced (+= 0, a no-op) per [LANDED-A].
                call    wrblk_mul_rr_rs     ; WRBLK_MULACC := (ix+33..35, = RR unchanged) * RS
                call    mulacc_cmp          ; WRBLK_MULACC vs FAT_FILESIZE (Cy = below)
wzp_decided:
                jr      c, wzp_shrink       ; MULACC < FAT_FILESIZE -> shrink path
                call    wrblk_size_max      ; MULACC >= old size -> grow-or-hold (no chain change)
                call    wrblk_set_wrfirst
                jr      wzp_dirupdate
wzp_shrink:
                call    wrblk_shrink        ; frees the tail chain + EOF-marks; sets
                                            ; BDOS_WRBYTES itself (not via wrblk_size_max)
                jr      c, wrblk_ioerr2
                call    wrblk_set_wrfirst
wzp_dirupdate:
                ; D-WRBLKTRUNC (2026-11-10): the FCB, NOT the entry. This wrote the entry
                ; (the new size) and left FCB +16..19 at the OLD one, and FCLOSE writes the
                ; entry from the FCB (cr_dir): a 20-record file cut to 10 closed at 2560 B
                ; on a 2-cluster chain (D-DOSLADDER D7, scratchpad/dosl_d7_trunc_ours.out).
                ; The CF-3300 sets the FCB's size here and leaves the entry to FCLOSE
                ; (scratchpad/dosl_d7_trunc_stock.out) -- as the record path does.
                call    wrblk_fcb_keep
                xor     a
                jr      wrblk_fin_req       ; D-DISKREREAD (2026-11-26): one `ld hl,(WRBLK_REQ)`, 3 B
wrblk_ioerr:
wrblk_ioerr2:
                ld      a, 2                ; generic FDC I/O error (fdc_read_data convention)
wrblk_fin_req:
                ld      hl, (WRBLK_REQ)     ; (= 0 on the zero path)
wrblk_finish:                              ; shared tail: M20 dispatcher-flag rule, preserve A/HL
                jp      rrnd_finish         ; D-DISKREREAD (2026-11-26): IS rrnd_finish -- clr_f306
                                            ; touches only A / F / $F306, so the push / pop of HL
                                            ; guarded nothing, 5 B

; wrblk_position_ext — position (or extend) the file's iterator to record
; WRBLK_REC (24-bit). M29 (tier2-m29-wrblk-position-cursor-spec.md): keeps an
; incremental cursor (WRBLK_CURVALID/WRBLK_CURSEC) across calls within one
; wrblk_body invocation instead of re-fat_open-ing and re-walking from the head
; every time -- WRBLK_REC is strictly non-decreasing per call (kernel.asm:884),
; so the target sector-in-file is always >= the cursor's last position. First
; call this wrblk_body invocation (or any call once positioned) still walks
; forward via wrblk_read_or_extend_sector exactly as the old from-head walk did;
; only the redundant re-walk of already-visited sectors is elided. Unlike
; rrnd_position, EXTENDS the chain contiguously through any gap instead of
; returning EOF ([LANDED-A]/§6 Q2 full past-EOF extend).
;   out: Cy = 0 ok (SECTOR_BUF holds the target sector's current on-disk bytes --
;        a freshly allocated sector reads back whatever is already on the disk,
;        "uninitialised", no zero-fill, EXCEPT the delta==0 same-sector case,
;        where SECTOR_BUF already holds it from the previous call -- no I/O);
;        Cy = 1 = disk full / I/O error
;        (the old WRBLK_RECSEC record-in-sector output died with D-WRBLKRS;
;        the cell is wrblk_seek's sector-in-cluster now)
wrblk_position_sec:                         ; D-WRBLKRS: in BC = the target SECTOR-in-file
                                            ; (the caller computes it from a BYTE offset;
                                            ; the record -> sector head that assumed 128 B
                                            ; records is gone)
                ld      a, (WRBLK_CURVALID)
                or      a
                jr      z, wpe_fresh
                ; --- cursor valid: incremental path, no fat_open ---------------
                push    bc                  ; save target_sec
                pop     hl                  ; HL = target_sec
                ld      de, (WRBLK_CURSEC)  ; D-DISKREREAD (2026-11-26): no `or a` -- CF = 0 from the
                sbc     hl, de              ; one above (push / pop / ld keep it), 1 B
                                            ; HL = target_sec - WRBLK_CURSEC (>=0, monotonic)
                ld      a, h
                or      l
                ret     z                   ; delta==0 -> already positioned, no I/O: Cy=0 from
                                            ; `or l` -- SECTOR_BUF already holds the target sector
                                            ; from the previous call (D-DISKREREAD: was wpe_same's
                                            ; `or a / ret`, 3 B)
                ; delta >= 1: advance HL steps via wrblk_read_or_extend_sector
                ld      (WRBLK_CURSEC), bc  ; bc still holds target_sec
                ex      de, hl              ; DE = delta
wpe_adv:
                push    de                  ; wrblk_read_or_extend_sector clobbers BC/DE
                                            ; internally (wroe_mul's djnz counter) -- same
                                            ; save/restore rrnd_position uses around
                                            ; fat_read_file_sector for the identical reason
                call    wrblk_read_or_extend_sector
                pop     de
                ret     c
                dec     de
                ld      a, d
                or      e
                jr      nz, wpe_adv
                ret                         ; Cy=0 (wrblk_read_or_extend_sector cleared it)
wpe_fresh:
                call    fat_open            ; iterator -> FAT_FIRSTCLUS, clussec 0
                ld      (WRBLK_CURSEC), bc
                ld      a, 1
                ld      (WRBLK_CURVALID), a
                ; 🔴 D-WRBLKSEEK (2026-10-01): this walked from the head, READING every
                ; sector it passed -- a 32 KB file written in 256 B WRBLKs took 59x the
                ; CF-3300 (disk_probe_wrblk_alt.py --wseek). A file that already has a
                ; cluster now seeks by CLUSTERS from the FCB's running cluster
                ; (wrblk_seek, no data reads); an empty one keeps the head walk, which
                ; allocates its first cluster.
                ld      hl, (FAT_FIRSTCLUS)
                ld      a, h
                or      a
                jr      nz, wpe_seek
                ld      a, l
                cp      2
                jr      c, wpe_head
wpe_seek:
                call    wrblk_seek          ; BC = the wrblk_read_or_extend_sector steps left
                jr      wpe_walk
wpe_head:
                inc     bc                  ; BC = walk-loop count (>=1; sector 0 needs 1 step)
wpe_walk:
                push    bc                  ; wrblk_read_or_extend_sector clobbers BC
                                            ; internally (wroe_mul's djnz counter) -- same
                                            ; save/restore rrnd_position uses around
                                            ; fat_read_file_sector for the identical reason
                call    wrblk_read_or_extend_sector
                pop     bc
                ret     c
                dec     bc
                ld      a, b
                or      c
                jr      nz, wpe_walk
                ret

; wrblk_read_or_extend_sector — like fat.asm's fat_read_file_sector, but when the
; chain runs out (cluster<2 empty-file case, or cluster>=$0FF8 end-of-chain) this
; ALLOCATES a new cluster and links it CONTIGUOUSLY instead of returning EOF. Does
; NOT modify fat_read_file_sector itself (kept byte-identical -- same non-goal
; precedent as M26): a parallel routine, reusing fat_advance/fat_next_cluster/
; fat_alloc_cluster/fat_write_fat_entry. Captures FAT_CURCLUS BEFORE this step's
; own advance/allocate (WRBLK_PREVCLUS) so a freshly allocated cluster links onto
; the TRUE last-good cluster, not a stale EOC/free value fat_advance may have just
; produced; also seeds FAT_FIRSTCLUS when the allocation is the file's very first
; cluster (empty file), so a LATER call in the same wrblk_body loop (re-fat_opens
; every record) walks the newly-grown chain correctly.
;   out: Cy = 0 ok (SECTOR_BUF holds the sector's current on-disk bytes); Cy = 1 =
;        disk full / I/O error
wrblk_read_or_extend_sector:
                ld      hl, (FAT_CURCLUS)
                ld      (WRBLK_PREVCLUS), hl
                ld      a, (FAT_CLUSSEC)
                ld      hl, FAT_SECPERCLUS
                cp      (hl)
                jr      c, wroe_incluster
                call    fat_advance
wroe_incluster:
                ld      hl, (FAT_CURCLUS)
                ld      de, 2
                or      a
                sbc     hl, de
                jr      c, wroe_need_extend ; cluster < 2 (free / empty file)
                add     hl, de              ; D-DISKREREAD (2026-11-26): HL = the cluster again (the
                                            ; sbc did not borrow), not a reload, 2 B
                call    hl_cmp_eoc
                jr      c, wroe_have_valid  ; cluster < $0FF8; else end-of-chain, falling into
                                            ; wroe_need_extend (D-DISKREREAD: one `jr`, not two, 2 B)
wroe_need_extend:
                call    wroe_ext            ; HL = new cluster, claimed + linked, the FAT
                ret     c                   ; sector written ONCE (disk/driver.asm); Cy=1 full / I/O
                ld      de, (WRBLK_PREVCLUS)
                ld      a, d
                or      e
                jr      nz, wroe_have_new
                ld      (FAT_FIRSTCLUS), hl ; file's very first cluster (was empty)
wroe_have_new:
                ld      (FAT_CURCLUS), hl
                xor     a
                ld      (FAT_CLUSSEC), a
wroe_have_valid:
                ; 💰 D-DISKCARVE (2026-11-07): this was a 47-byte PARALLEL COPY of the
                ; shared body's sector computation (basic/fat-prim-body.inc frs_sector:
                ; firstData + (cluster-2)*secPerClus + clussec, read into the sector
                ; buffer -- SECTOR_BUF and FAT_DBUF are one address in the disk build,
                ; and the two were byte-identical, scratchpad/diskcarve_dupspan.out).
                ; Sharing it frees 44 B in front of the $75A5 pin.
                ; D-LOADDIRECT: the shared body now takes its destination on the
                ; stack, so this enters at the TOP. The same work: the cluster is
                ; valid here and FAT_CLUSSEC < FAT_SECPERCLUS (fat_advance or the
                ; new cluster left it 0), so the body's own checks pass through.
                jp      fat_read_file_sector

; wrblk_mul_rr_rs — WRBLK_MULACC(32-bit LE, WBUF+500) := RR(24-bit, ix+33..35) *
; RS(16-bit, WRBLK_RS). Classic LSB-first shift-add multiply, 24 iterations: a
; 32-bit zero-extended copy of RS (WRBLK_MULOP, WBUF+504) WAS doubled each
; iteration and added into WRBLK_MULACC whenever the corresponding bit of a
; shifting 24-bit copy of RR (WRBLK_MULN, WBUF+508) was 1 -- RAM-only state, one
; byte operation at a time. Since carve 35 the same product is computed in
; registers (below); WRBLK_MULOP / WRBLK_MULN are no longer used. Host-unit-tested
; by tests/test_wrblk_fsize.py. Runs after
; fat_find's SECTOR_BUF scan and before fat_dir_update's own WBUF read/write, so
; WBUF's tail is safe transient scratch here (§ equates.inc note).
;   out: WRBLK_MULACC = the 32-bit product; trashes AF, BC, DE, HL
; (wrblk_mul_rr_rs itself and wrblk_off24 -- MOVED by D-DISKCARVE6 to the pads in front of
; $5006 and $4217: call-only, their bytes freed here for the hole in front of $75A5.)
; wmr_core -- DE:HL := RR (ix+33..35) x RS (WRBLK_RS), mod 2^32. 💰 D-DSKFMT carve 35
; (2026-11-10): in REGISTERS -- DE:HL the accumulator, BC = RS -- shift-and-add over
; RR's 24 bits, most significant first; the same result as the RAM-only version it
; replaces (tests/test_wrblk_fsize.py checks it). Trashes A, BC.
wmr_core:
                ld      bc, (WRBLK_RS)
                ld      hl, 0
                ld      d, h
                ld      e, l
                ld      a, (ix+35)
                call    wmr_byte
                ld      a, (ix+34)
                call    wmr_byte
                ld      a, (ix+33)          ; (falls into wmr_byte: its ret is ours)
; wmr_byte -- DE:HL := DE:HL x 256 + A x BC, A's bits most significant first. A
; sentinel 1 (rla after scf) counts the eight: A turns 0 when it has gone out.
wmr_byte:
                scf
                rla                         ; CF = bit 7; the sentinel into bit 0
wmr_bit:
                push    af
                add     hl, hl
                ex      de, hl
                adc     hl, hl
                ex      de, hl              ; DE:HL x 2
                pop     af
                jr      nc, wmr_next
                add     hl, bc
                jr      nc, wmr_next
                inc     de                  ; + BC
wmr_next:
                add     a, a                ; the next bit
                jr      nz, wmr_bit
                ret
; wrzer_body -- BDOS $28 WRZER (D-WRZER, 2026-11-09): WRRND, and the records an
; EXTENSION adds read zero. Measured on the CF-3300 (probes/disk/wrzer.asm,
; scratchpad/wrzer_stock3.out / wrzer_stock4.out): WRZER at RR past the end zero-fills
; every record from the old end of file (ceil(size / 128)) to RR - 1; WRRND leaves
; the disk's bytes there. So: the caller's record through wrrnd_body (it extends
; the file, by WRBLK); then the caller's DTA -- its record is on the disk now -- is
; zeroed and written to each gap record by WRBLK, which positions from the FCB's own
; state (rrnd_position re-finds the file through the DIRECTORY, which WRBLK only
; updates at close: the first cut failed there, A = 2); then RDBLK reads record RR
; back into the DTA, RR is put back and WRRND's success tail runs. R0 >= RR: plain
; wrrnd_body. A failure keeps WRBLK's A (1 full, 2 I/O); the DTA is restored either way.
;   in  DE = the kernel's FCB copy
wrzer_body:
                push    de
                pop     ix
                ld      l, (ix+16)
                ld      h, (ix+17)
                ld      a, (ix+18)
                ld      bc, 127
                add     hl, bc
                adc     a, b                ; B = 0
                add     hl, hl
                rla
                ld      l, h
                ld      h, a                ; HL = R0 = ceil(size / 128)
                ld      c, (ix+33)
                ld      b, (ix+34)          ; BC = RR (records < 65536)
                or      a
                sbc     hl, bc
                jp      nc, wrrnd_body      ; R0 >= RR: no gap (DE = the FCB copy still)
                add     hl, bc
                push    bc                  ; [RR]
                push    hl                  ; [RR][R0]
                call    wrrnd_body          ; the caller's record: extends the file
                pop     bc                  ; BC = R0
                or      a
                jr      nz, wz_out          ; its own error, A kept
                ld      hl, (DOS_DTAPTR)
                ld      e, RECSIZE
wz_z:
                ld      (hl), a             ; A = 0
                inc     hl
                dec     e
                jr      nz, wz_z            ; the caller's DTA: 128 zeros
wz_lp:
                ld      (ix+33), c
                ld      (ix+34), b
                push    bc                  ; [RR][R]
                ld      hl, 1
                push    ix
                pop     de
                call    wrblk_body          ; record R := the zeroed DTA
                pop     bc                  ; [RR]
                or      a
                jr      nz, wz_done
                inc     bc
                pop     hl
                push    hl
                sbc     hl, bc              ; (Cy = 0 from the `or a`)
                jr      nz, wz_lp
wz_done:                                    ; A = 0, or WRBLK's failure
                pop     hl                  ; RR
                push    af
                push    hl                  ; [AF][RR]
                call      set_rr
                push    ix
                pop     de
                push    de                  ; the FCB copy (k47b2_body takes IX over)
                ld      hl, 1
                call    k47b2_body          ; the DTA <- record RR again (the caller's)
                pop     ix
                pop     hl
                call      set_rr          ; RR as the caller gave it
                pop     af                  ; (its flags: Z = success)
                jp      z, wrrnd_ok         ; CR := r0, A = 0, HL = 0
                jp      rrnd_finish
wz_out:
                pop     hl
                ret


; fren_wild -- BDOS $17 FREN (D-FRENWILD, 2026-11-10). The FCB holds the OLD name /
; pattern at +1..11 and the NEW at +17..27. EVERY root entry matching the old pattern
; (name_cmp, with `?`; volume labels and directories skipped) is renamed: new[k] = the
; new pattern's byte, or the entry's own where that byte is `?` -- the CF-3300 turns
; "X?.TMP" -> "Z?.TMP" into X1 -> Z1, X2 -> Z2 (probes/disk/dosl/d5rendel.asm,
; scratchpad/dosl_d5_1.out). A built name that already exists is refused: A = $FF
; (the CF-3300 refuses Y.TMP -> Z1.TMP). Nothing matched: A = $FF; an I/O error: 2.
; ONE pass by entry index (fr_entry), so a renamed entry that still matches is not
; visited again. The built name lives in a 12-byte stack frame (fat_find overwrites
; FAT_DBUF; FAT_MBUF holds the FAT cache): at SP+4 inside the loop. Neither the DTA
; nor SFIRST / SNEXT's BDOS_SRCHIDX is touched. (The old body found the OLD name with
; fat_find -- an EXACT compare -- and copied the new pattern verbatim, `?` included.)
fren_wild:
                push    de
                pop     ix                  ; IX = the FCB
                call    fat_mount
                jp      c, fren_miss
                ld      hl, 0
                ld      (FAT_DIRSEC), hl    ; no root sector held yet (fr_entry)
                push    hl
                push    hl
                push    hl
                push    hl
                push    hl
                push    hl                  ; the frame (12 B): SP+4 = the built name
                push    hl                  ; [cnt]
                push    hl                  ; [idx][cnt]
                                            ; D-DISKREREAD (2026-11-26): no `pop hl / push hl` at the
                                            ; loop head -- HL = the idx on top on BOTH entries (0 here,
                                            ; fw_next's `pop / inc / push`), 2 B
fw_lp:
                call    fr_entry            ; HL = entry idx
                jr      c, fw_end
                ld      a, (hl)
                or      a
                jr      z, fw_end           ; the end mark
                cp      $E5
                jr      z, fw_next          ; deleted
                push    hl
                ld      de, 11
                add     hl, de
                ld      a, (hl)
                and     $18                 ; a volume label or a directory
                push    ix
                pop     de
                inc     de                  ; DE = the old pattern
                pop     hl
                jr      nz, fw_next
                push    hl
                call    name_cmp            ; (D-DISKREREAD: was name_cmp_wild, the same code)
                pop     de                  ; DE = the entry
                jr      nz, fw_next
                ld      hl, 4
                add     hl, sp
                ex      de, hl              ; DE = the frame, HL = the entry
                push    ix
                ld      b, 11
fw_q:
                ld      a, (ix+17)          ; the new pattern's byte ...
                cp      '?'
                jr      nz, fw_q1
                ld      a, (hl)             ; ... or, for `?`, the entry's own
fw_q1:
                ld      (de), a
                inc     hl
                inc     de
                inc     ix
                djnz    fw_q
                pop     ix
                ld      hl, 4
                add     hl, sp
                call    fat_find            ; Cy = 0: the built name exists
                jp      nc, fw_refuse       ; (disk/fat.asm, in the $4C29 hole)
                pop     hl
                push    hl
                call    fr_entry            ; the entry again (fat_find read others)
                jr      c, fw_io
                ex      de, hl
                ld      hl, 4
                add     hl, sp
                call    ldir_11
                ld      de, (FAT_DIRSEC)
                ld      hl, FAT_DBUF
                call    write_sector
                jr      c, fw_io
                pop     hl
                pop     de
                inc     de                  ; one more renamed
                push    de
                push    hl
fw_next:
                pop     hl
                inc     hl
                push    hl
                jr      fw_lp
fw_end:
                pop     hl
                pop     de                  ; DE = how many renamed
                ld      a, d
                or      e                   ; Z: none
                jp      fw_done             ; (disk/runtime.asm, above the FDC window)
fw_io:
                ld      hl, 16
                add     hl, sp
                ld      sp, hl
                jp      fren_ioerr

; seq_k_adv -- BDOS_SEQREC += 1 = K; the kernel FCB copy's +32 CR := K AND $7F and
; +12 EX := K >> 7; HL = K. seq_k30 -- +30 := (K - 1) / records-per-cluster, from
; HL = K. Both stood twice, in wrseq_writeback and wrseq_wr_writeback (D-DSKFMT
; defrag, 2026-11-09). Clobber A, F (seq_k30 also B, C, DE, HL).
seq_k_adv:
                ld      hl, (BDOS_SEQREC)
                inc     hl
                ld      (BDOS_SEQREC), hl
                ld      a, l
                and     $7F
                ld      (ix+32), a
                ld      a, l                ; D-DISKREREAD (2026-11-26): A = low byte of K >> 7 =
                rla                         ; H << 1 | L's bit 7, two rotates, not a 7-step shift
                ld      a, h                ; of a saved HL (HL kept; B no longer clobbered;
                rla                         ; neither caller tests a flag), 7 B
                ld      (ix+12), a
                ret

; recperclus_div -- DE := HL / (A * RECPERSEC), A = FAT_SECPERCLUS on entry (the
; records in a cluster); div_hl_bc -- DE := HL / BC by subtraction (HL ends one BC
; past the remainder, Cy set). The pair stood twice (wrseq_writeback /
; wrseq_wr_writeback) and the divide once more (wrblk_shrink): D-WRZER's carve,
; 2026-11-09. Clobbers A, BC, HL.
recperclus_div:
                ld      c, a
                ld      b, RECPERSEC - 1
rpc_lp:
                add     a, c
                djnz    rpc_lp              ; A = FAT_SECPERCLUS * RECPERSEC
                ld      c, a
                ld      b, 0
div_hl_bc:
                ld      de, 0
dhb_lp:
                or      a
                sbc     hl, bc
                ret     c
                inc     de
                jr      dhb_lp

; wrblk_size_max — BDOS_WRBYTES(32) := max(FAT_FILESIZE, WRBLK_MULACC). Simple
; 32-bit unsigned compare (MSB byte first); copies the winner into BDOS_WRBYTES.
wrblk_size_max:
                call    mulacc_cmp
                ld      hl, WRBLK_MULACC
                jr      nc, to_wrbytes      ; MULACC >= FILESIZE: the new size
                ld      hl, FAT_FILESIZE    ; MULACC < FILESIZE: the old size wins
; to_wrbytes -- BDOS_WRBYTES := the 4 bytes at HL; copy4 -- (DE) := the 4 bytes at HL.
; Clobber BC; HL / DE end past the copy. 💰 D-DSKFMT carve 23 (2026-11-10): the 4-byte
; LDIR stood inline at eleven sites, five of them into BDOS_WRBYTES.
to_wrbytes:
                ld      de, BDOS_WRBYTES
copy4:
                ld      bc, 4
                ldir
                ret
; hl_x128 / hl_x32 -- HL := HL x 128 / x 32 (16 bits, flags as `add hl,hl`'s): MOVED by
; D-DISKCARVE6 to the pad in front of $4462 (disk/driver.asm).
; set_rr -- the FCB copy's random record (IX+33..34) := HL. 💰 D-DSKFMT carves 25 / 26
; (2026-11-10): the shift chains stood seven times, the RR store eight.


; wrblk_shrink — HL_requested==0 and the target size (WRBLK_MULACC, already =
; RR*RS) is BEFORE the file's current end -- shrink to that size ([LANDED-A]/§6
; Q3 "DO THE SANE THING": stock corrupts the FAT here so a later FCLOSE fails;
; ours frees the tail chain + EOF-marks the new last cluster so FCLOSE keeps
; succeeding -- an INTENTIONAL, DOCUMENTED divergence from stock, not a
; regression). Sets BDOS_WRBYTES itself (the target size) -- does NOT go through
; wrblk_size_max, which would wrongly keep the OLD (larger) size.
;   out: Cy = 0 ok, Cy = 1 = I/O error
wrblk_shrink:
                ld      hl, WRBLK_MULACC
                call    to_wrbytes          ; BDOS_WRBYTES := target size; HL = WRBLK_MULACC + 4
                dec     hl                  ; D-DISKREREAD (2026-11-26): walked DOWN from where
                ld      a, (hl)             ; to_wrbytes left HL, no reload, 3 B
                dec     hl
                or      (hl)
                dec     hl
                or      (hl)
                dec     hl
                or      (hl)                ; Z iff target size == 0 (all 4 bytes)
                jr      z, wshrink_empty
                ; keep_sectors = ceil(target_size / 512) = (target_size + 511) >> 9
                ld      hl, (WRBLK_MULACC)
                ld      de, 511
                add     hl, de
                ld      a, (WRBLK_MULACC + 2)
                adc     a, 0                ; A:HL = target_size + 511 (24-bit-safe)
                ld      l, h
                ld      h, a                ; HL = (target_size+511) >> 8
                srl     h
                rr      l                   ; HL = (target_size+511) >> 9 = keep_sectors
                ; keep_clusters = ceil(keep_sectors / SECPERCLUS), repeated subtraction
                ; (SECPERCLUS is a small BPB constant; keep_sectors is bounded to a
                ; floppy's few-thousand-sector range -- cheap either way)
                ld      a, (FAT_SECPERCLUS)
                ld      c, a
                ld      b, 0                ; BC = SECPERCLUS
                push    bc
                pop     de
                dec     de
                add     hl, de              ; HL = keep_sectors + SECPERCLUS - 1
                call    div_hl_bc           ; DE = HL / BC
                ld      (WRBLK_KEEPCNT), de ; keep_clusters (>= 1, since target size > 0)
                ld      hl, (FAT_FIRSTCLUS)
wshrink_walk:
                ld      de, (WRBLK_KEEPCNT)
                dec     de
                ld      a, d
                or      e
                jr      z, wshrink_atlast   ; consumed keep_clusters-1 steps -> HL = last-to-keep
                ld      (WRBLK_KEEPCNT), de
                call    fat_next_cluster
                call    hl_cmp_eoc          ; D-DISKREREAD (2026-11-26): `add hl,de` restores HL and
                add     hl, de              ; carries iff HL < $0FF8 (the fdel loop's idiom), so no
                jr      c, wshrink_walk     ; push / pop and one `jr`, 3 B -- at EOC the chain is
                                            ; shorter than expected: stop defensively (falls through)
wshrink_atlast:
                ; HL = the cluster to KEEP as the file's new (and only surviving) tail.
                push    hl
                call    fat_next_cluster    ; HL := its CURRENT link (the old tail start)
                ld      (WRBLK_NEXTCLUS), hl
                pop     hl                  ; HL = the keep-cluster
                ld      de, EOC
                call    fat_write_fat_entry ; keep-cluster := EOC in every FAT copy
                ret     c                   ; I/O error, CF = 1 (D-DISKREREAD: was wshrink_ioerr)
                ld      hl, (WRBLK_NEXTCLUS)
                jr      wshrink_freeloop
wshrink_empty:
                ld      hl, (FAT_FIRSTCLUS) ; HL = old first cluster (the whole chain to free)
                ld      de, 0               ; D-DISKREREAD (2026-11-26): the zero through DE, no
                ld      (FAT_FIRSTCLUS), de ; push / pop of HL (the freeloop takes HL only), 1 B
                                            ; -- file becomes clusterless
wshrink_freeloop:
                ld      a, h
                or      l
                ret     z                   ; HL == 0 -> nothing to free, CF = 0 from `or l`
                call    hl_cmp_eoc          ; D-DISKREREAD (2026-11-26): `add hl,de` restores HL and
                add     hl, de              ; carries iff HL < $0FF8; the exits are `ret`s (the stack
                ret     nc                  ; is balanced at each), not two stubs, 9 B -- HL >= $0FF8
                                            ; (EOC) -> nothing more to free, CF = 0
                push    hl                  ; HL = cluster to free
                call    fat_next_cluster    ; HL := its current link
                ld      (WRBLK_NEXTCLUS), hl
                pop     hl                  ; HL = the cluster to free (restored)
                call    fwfe_zero ; mark it free ($000)
                ret     c                   ; I/O error, CF = 1
                ld      hl, (WRBLK_NEXTCLUS)
                jr      wshrink_freeloop

; ===== M31: k_47B2 faithful Random Block Read body (tier2-m31-rdblk-randrecord-
; spec.md §3.2) — relocated out of $47B2's cramped span (3b-relocation, the
; M24/M28/M29 idiom); reached only via the k_47B2 veneer's `jp`.
;
; Faithful $27: position to the FCB's random-record field (FCB+33..35), then
; transfer up to HL records of the FCB's record size (FCB+14..15, 0 -> 128),
; zero-padding a final partial record, and report the true delivered count;
; RR is advanced by the delivered count on return (unified contract for both
; boot loaders, at RR=0/RS=1/huge-HL, and any user-level $27 caller, §2/§3.2).
; Register discipline: entry DE = FCB ptr, HL = requested record count. Both
; are needed AFTER we start touching HL/DE for FCB-field reads below, so both
; are parked in dedicated cells immediately (RDBLK_REQ / the FCB ptr survives
; on the stack, popped back for the RR write-back + IY return at the end).
k47b2_body:
                push    de                  ; save entry FCB ptr (RR write-back + IY return)
                ; --- steps 2/3: RDBLK_REQ, record size <- FCB+14..15, 0 -> 128 --
                call    rdblk_rs            ; (driver.asm, shared with bdos_rdblk)
                ; --- step 4: RR start <- FCB+33..35 (24-bit) -> RDBLK_RRSTART -----
                pop     hl                  ; HL = entry FCB ptr (peek without losing it)
                push    hl
                ld      de, 33
                add     hl, de
                ld      de, RDBLK_RRSTART
                ld      bc, 3
                ldir                        ; RDBLK_RRSTART := FCB+33..35
                ; --- step 5: DTA <- (DOS_DTAPTR) -> RDBLK_DST + BDOS_DTA ----------
                ; (M19 lesson: the kernel's real SETDTA only ever touches
                ; DOS_DTAPTR, never our own mini-BDOS's BDOS_DTA cell -- do NOT
                ; trust BDOS_DTA itself here, see k_47B2's original M21b note.)
                call    dta_from_dos
                ld      (RDBLK_DST), hl
                ; --- steps 6+7: POSITION FROM THE FCB (D-RDBLKSEEK, 2026-10-01) ------
                ; 🔴 This was: re-mount (the boot sector), re-find (the root directory)
                ; and fat_open on EVERY call, then skip RR records by FETCHING every
                ; byte before them. A 32 KB file read in 256 B RDBLKs took 2150.91
                ; emulated s against the CF-3300's 14.65 (147x,
                ; disk_probe_wrblk_alt.py --seek). The CF-3300 keeps a running
                ; cluster in the FCB (published layout, read as data after FOPEN /
                ; block 0 / block 5): +26 first cluster, +28 the last cluster
                ; accessed, +30 its index in the chain. So does this now:
                ;   size from +16..19, first cluster from +26 (both filled by FOPEN,
                ;   so two FCBs stay two files -- D-RDBLKMULTI's guarantee, without
                ;   its per-call name lookup), start offset O = RR x RS, target
                ;   cluster index ci = O / (512 x spc); walk the chain from +28/+30
                ;   when that is at or before ci (else from +26), write ci's cluster
                ;   back to +28/+30, prime the iterator at O's sector.
                ; Geometry is mounted only when it has never been read.
                ld      hl, 0
                ld      (RDBLK_DONE), hl    ; no records delivered yet (also the count
                                            ; the EOF exits below write back)
                pop     ix
                push    ix                  ; IX = the FCB (k47b2_seek's input)
                call    k47b2_seek          ; Cy = 1 -> nothing to deliver
                jr      c, k47b2_eof
k47b2_pos_done:
                ; --- step 8: transfer loop (zero-pad variant of rdb_recloop_body) --
                ; "Partial record" (>=1 byte already copied this record) is recovered
                ; by comparing RDBLK_CNT (bytes still owed) against RDBLK_RECSIZE (the
                ; full record size): CNT < RECSIZE iff at least one byte already
                ; landed -- no extra flag cell needed (RDBLK_RRSTART's 3 bytes are the
                ; spec's whole free-tail budget, §3.1).
k47b2_xfer_rec:                             ; D-DISKREREAD (2026-11-26): no `or a` -- CF = 0 on both
                ld      hl, (RDBLK_DONE)    ; entries (the fall-through after `jr c, k47b2_eof`;
                ld      de, (RDBLK_REQ)     ; recdone, reached only by `jr z` after `or l`), 1 B
                sbc     hl, de
                jr      z, k47b2_ok         ; delivered every requested record -> A=0
                ld      hl, (RDBLK_RECSIZE)
                ld      (RDBLK_CNT), hl     ; bytes still to copy for this record
k47b2_xfer_byteloop:
                ld      hl, (RDBLK_CNT)
                ld      a, h
                or      l
                jr      z, k47b2_xfer_recdone       ; whole record copied -> next record
                call    k47b2_nextbyte      ; A = next byte / Cy=1 EOF-or-chain-end
                jr      c, k47b2_xfer_eofcheck
                call    dta_put_cnt
                                            ; (scratchpad/p1dta_run.out)
                jr      k47b2_xfer_byteloop
k47b2_xfer_eofcheck:
                ; EOF (or chain-end, treated the same). Partial record (RDBLK_CNT <
                ; RDBLK_RECSIZE, i.e. a byte already copied this record) -> zero-pad
                ; the remainder and COUNT it as any record; clean boundary (CNT == RECSIZE, no bytes
                ; copied) -> EOF without counting this record.
                ld      hl, (RDBLK_CNT)
                ld      de, (RDBLK_RECSIZE)
                or      a
                sbc     hl, de
                jr      z, k47b2_eof        ; clean boundary -> EOF, uncounted
k47b2_xfer_padloop:
                ld      hl, (RDBLK_CNT)
                ld      a, h
                or      l
                ; 🔴 D-RDBLKPART (2026-11-22): the padded record counts as an ordinary
                ; one -- back to k47b2_xfer_rec, which answers A 0 when it was the last
                ; asked for and A 1 when the next one meets the clean end. It went
                ; straight to k47b2_eof (A 1 always); the VG-8020 + NMS 1200 and the
                ; CF-3300 answer A 1 only when HL comes back SHORT (docs/bdos/BLOCKS.md
                ; R-BK-RDBLK, R-BK-ROUND2, R-BK-FOPENRS; XK4 9: 3 asked over 50 bytes, HL 1, A 1).
                jr      z, k47b2_xfer_recdone
                xor     a
                call    dta_put_cnt
                jr      k47b2_xfer_padloop
k47b2_xfer_recdone:
                ld      hl, (RDBLK_DONE)
                inc     hl
                ld      (RDBLK_DONE), hl
                jr      k47b2_xfer_rec
                ; --- step 9 (8a): one exit, both A values fall through here --------
k47b2_ok:
                xor     a                   ; A = $00 all requested records delivered
                jr      k47b2_return
k47b2_eof:
                ld      a, 1                ; A = $01 EOF-first
k47b2_return:
                ld      e, a                ; E = the 0/1 result, parked here (A itself is
                                            ; clobbered by the 24-bit add below; DE is
                                            ; otherwise dead at this exit) -- restored to
                                            ; A as the very last step before ret.
                ; RR := entry-RR + HL(delivered), write back to FCB+33..35 (24-bit;
                ; carry into the high byte via ADC). At/past EOF, HL=0 -> RR unchanged
                ; (§3.2 step 9). Entry FCB ptr is still on the stack (untouched since
                ; the routine's initial `push de`) -- pop it into IY now: this both
                ; satisfies the IY-return contract AND gives indexed (iy+33/+34/+35)
                ; addressing for the write-back, so the FCB ptr need not be re-derived.
                pop     iy                  ; IY = entry FCB ptr (§5.4/§8.50 IY contract)
                ld      hl, (RDBLK_DONE)    ; HL = records actually delivered
                ld      a, (RDBLK_RRSTART)
                add     a, l
                ld      (iy+33), a
                ld      a, (RDBLK_RRSTART + 1)
                adc     a, h
                ld      (iy+34), a
                ld      a, (RDBLK_RRSTART + 2)
                adc     a, 0
                ld      (iy+35), a
                ; HL = BC = records delivered (the $47B2-boundary count, §1-Q3); HL
                ; itself is already RDBLK_DONE (untouched above -- only A was used for
                ; the 24-bit add), so just copy it to BC.
                ld      b, h
                ld      c, l
                ld      ix, DRVA_DPB        ; IX = $F195 drive-A DPB (§8.50)
                call     clr_f306   ; M20 dispatcher-flag rule
                ld      a, e                ; A = the 0/1 EOF result (restored)
                ret

; k47b2_nextbyte — shared byte-fetch-or-EOF helper (used by BOTH the step-7
; positioning loop and the step-8 transfer loop; folding the BDOS_BYTESLEFT==0
; test + rdblk_getbyte call into one place keeps k47b2_body's two structurally
; identical byte loops small).
;   out: Cy=0, A=the next file byte (BDOS_BYTESLEFT>0; rdblk_getbyte's normal
;        path -- advances BDOS_BYTESLEFT + the sector buffer)
;        Cy=1  BDOS_BYTESLEFT==0 (clean EOF) OR rdblk_getbyte's own chain-end
;        (defensive; caller treats both alike, §3.2 step 7/8)
k47b2_nextbyte:
                ld      hl, BDOS_BYTESLEFT
                ld      a, (hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)
                jp      nz, rdblk_getbyte      ; BDOS_BYTESLEFT != 0 -> fetch for real
                scf                                 ; BDOS_BYTESLEFT == 0 -> Cy=1, clean EOF
                ret
; (k47b2_nextbyte_get -- a `jp rdblk_getbyte` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
                                            ; either way its own `ret` is our `ret` too


; setrnd_body — BDOS $24 SETRND ($50C8's body; $F37D's through bb_setrnd). 🔴 D-BDOSFCB
; residual (2), ruled by Joost 2026-11-10 ("Match the references"): MEASURED on three
; disk systems -- the CF-3300, the VG-8020 + NMS 1200 and the NMS 8250 (MSX2) -- through
; $F37D and, on the CF-3300, through MSX-DOS's $0005 too (scratchpad/setrnd_probe.py,
; setrnd_dos_probe.py): RR := 1 + (RS >> 1) x 65536 -- r0 = 1, r1 = 0, r2..r3 = RS / 2 (for
; RS < 256; for a larger RS only the low byte counts and r3 = 0: D-SETRNDRS, below) --
; whatever EX (+12), CR (+32), +13, +24 or the previous RR, and after any sequential
; reads. It does NOT compute the current record. This body used to (the CP/M rule, cr
; + ex x 128 + s2 x 4096, reading +14 -- MSX-DOS's RS -- as CP/M's S2): an
; "intentional, documented divergence" (M32 §4), now retired by that ruling.
; Entry DE = the FCB (the kernel's copy). Exit A = L = $25, H = 0 (the CF-3300's, through
; MSX-DOS: the dispatcher's value passed through), $F306 cleared (M20 rule).
setrnd_body:
                push    de
                pop     ix
                ; 🔴 D-SETRNDRS (2026-11-17): r2 = RS's LOW BYTE / 2, r3 = 0 -- not the 16-bit RS / 2.
                ; The rule above was measured with RS = 128 and 1 only, where the two coincide;
                ; RS = $0181 / $0200 / $0400 / $1234 / $FFFF on the CF-3300 give r2 = $40 / 0 / 0 /
                ; $1A / $7F and r3 = 0 every time (scratchpad/setrnd_rs_probe.py, F7_rs2.out;
                ; found by bdosf37d-acceptance: RS = $0400 left r3 = 2 here). 4 B shorter.
                ld      a, (ix+14)
                srl     a                   ; A = RS's low byte / 2
                ld      (ix+35), a          ; r2
                ld      hl, 1
                call    set_rr              ; r0 = 1, r1 = 0
                call    clr_f306            ; M20 dispatcher-flag rule (A = 0)
                ld      (ix+36), a          ; r3 -- D-DISKREREAD (2026-11-26): the zero from A
                                            ; (clr_f306 keeps IX), 1 B
                ld      h, a                ; H = $00
                ld      a, $25              ; A = L = $25 (MSX-DOS's, measured)
                ld      l, a
                ret
; bb_setrnd -- SETRND from $F37D: the same RR; A = L = $12 there, on all three references
; (scratchpad/setrnd_run*.out).
bb_setrnd:
                call    setrnd_body
                ld      a, (ix+14)
                rrca
                and     $80
                ld      h, a                ; H = RS's low bit, at bit 7 (RS 1: HL = $8012,
                ld      a, $12              ; scratchpad/setrnd_run2.out)
                ld      l, a
                ret

; fat_have_free_cluster — M34 (tier2-m34-wrseq-diskfull-spec.md §3.2): scan-only,
; non-committing sibling of fat_alloc_cluster (fat.asm). Answers "does a $000
; (free) cluster exist anywhere in FAT copy 0?" WITHOUT claiming one -- no
; fat_write_fat_entry call, FAT_ALLOCHINT untouched. Reuses fat_total_clusters
; (scan bound) + fac_entry_from_wbuf (the shared straddle-correct 12-bit
; unpack) + the same FAT-sector-load bookkeeping fac_loop_body uses (FAT_WRTMP
; = total, FAT_WRTMP2 = cached FAT sector, FAT_BYTEIDX/FAT_PARITY/FAT_FATSEC).
; Always scans from cluster 2 (NOT the M30 FAT_ALLOCHINT) so "none free" is
; answered correctly even when the hint has advanced past a since-deleted
; cluster. Does not touch FAT_CURCLUS/FAT_CLUSSEC (the read iterator's own
; cells) -- fac_entry_from_wbuf doesn't either, so this is safe to call from
; bdos_seqwrite_body mid-sequential-write.
;   out: Cy = 0 a free cluster exists; Cy = 1 none (disk full)
; CLEAN-ROOM: our own routine, mirroring fat_alloc_cluster's own scan structure
; (already our own clean-room code) minus the claim step; Microsoft FAT spec
; §3.2 ($000 = free).
fat_have_free_cluster:
                call    fat_total_clusters  ; DE = total clusters (reads boot sector)
                ld      (FAT_WRTMP), de
                ld      hl, $FFFF
                ld      (FAT_WRTMP2), hl    ; cached-sector = none
                ld      hl, 2               ; always scan from cluster 2 (not the hint)
fhfc_loop:
                ld      de, (FAT_WRTMP)
                push    hl
                or      a
                sbc     hl, de
                pop     hl
                jr      nc, fhfc_none       ; scanned past the last cluster -> none free
                push    hl
                call    fac_cached_entry    ; DE = the entry (CF = a read error: none free)
                pop     hl
                ret     c                   ; D-DISKREREAD (2026-11-26): the read error returns its own
                                            ; CF = 1 (stack balanced after the pop), 2 B
                ld      a, d
                or      e
                ret     z                   ; $000 -> free: Cy = 0 from `or e` (D-DISKREREAD: was
                                            ; fhfc_found's `or a / ret`, 2 B net)
                inc     hl
                jr      fhfc_loop
; (fac_cached_entry, D-DISKCARVE3, is in front of runtime.asm's $7F7C pin since
; D-DISKCARVE5: this region holds D-CALSLTP0's template.)
fhfc_none:
                scf                         ; Cy = 1 -- disk full
                ret

; wrrnd_extend — M36 (tier2-m35-m36-tierc-fixes-spec.md §M36): WRRND ($22)
; past-EOF file-size extension. Called by wrrnd_body (below, in the kernel.asm
; tail) right after the record's write_sector succeeds, BEFORE the CR side
; effect -- growing the file's recorded size when a random-record write lands
; past the previous end-of-file, exactly like stock. PLACEMENT: this
; position-free corridor (0 canonical entries, no FDC window, ample slack),
; not wrrnd_body's own tail (already tight against the $7F80 FDC-window
; guard) -- same relocation idiom as wrseq_writeback/setrnd_body above.
;
; in:  IX = kernel FCB-copy pointer ($DA40); ix+16..19 = current size (LE),
;      ix+33 = r0 (the record just written, 0..255), ix+1 = 11-byte 8.3 name.
; out: ix+16..19 updated in place if the write extended the file; the on-disk
;      directory entry is patched to match (see step 2). Cy=1 = I/O error
;      finding/writing the directory entry (caller must treat as wrrnd_ioerr);
;      Cy=0 otherwise (including the "no extension needed" no-op path).
; Trashes AF/BC/DE/HL; IX preserved (fat_mount/fat_find/write_sector touch no IX).
;
; Step 1: newsize = max(oldsize, (r0+1)*RECSIZE). (r0+1) is computed as a
; 16-bit value BEFORE the x128 shift (r0 <= 255 -> r0+1 <= 256, safe in HL);
; the product is <= 32768 ($8000), so it always fits 16 bits and its own
; (would-be) +18/+19 word is 0 -- no separate high-word compute needed. If
; the CURRENT size's own high word (ix+18/19) is nonzero, oldsize is already
; >= 65536 > any possible newsize, so old wins outright (skip). Otherwise a
; plain 16-bit unsigned compare of newsize against oldsize's low word decides
; it: newsize <= oldsize skips the whole extension (this is what keeps every
; happy-path within-file WRRND, including BDOSX3 rec18-20, byte-unchanged).
wrrnd_extend:
                ld      a, (ix+18)
                or      (ix+19)
                ret     nz                  ; oldsize >= 65536 -- old wins, no extension
                ; D-DISKREREAD (2026-11-26): A = 0 from here. The +18/+19 stores wrote the zeros the
                ; `ret nz` just proved are there; old - new answers both "no extension" cases with
                ; one `ret nc`; and the new size is stored from DE. 16 B (the sole caller, the WRRND
                ; tail, tests only `jr c` and reloads A)
                ld      l, (ix+33)          ; L = r0
                ld      h, a
                inc     hl                  ; HL = r0+1 (<= 256, safe before the x128 shift)
                call    hl_x128            ; HL = (r0+1)*RECSIZE = newsize (<= 32768)
                ld      e, (ix+16)
                ld      d, (ix+17)          ; DE = oldsize (low word; high word already 0 here)
                ex      de, hl              ; HL = oldsize, DE = newsize
                or      a
                sbc     hl, de              ; Cy iff newsize > oldsize
                ret     nc                  ; newsize <= oldsize -- no extension, Cy = 0 (NOT an error)
                ; --- extending: DE = newsize; store into ix+16..17 (+18/+19 are 0 already) ---
                ld      (ix+16), e
                ld      (ix+17), d
                xor     a                   ; Cy = 0
                ret
                ; 🔴 D-RNDLIVE (2026-11-10): the directory entry is NOT written here any
                ; more. The CF-3300 leaves it alone until FCLOSE -- FSIZE of an open file it
                ; grew by random writes answers the entry's OLD size (0 in
                ; probes/disk/dosl/d6rand.asm, scratchpad/dosl_d6_3.out) -- and FCLOSE
                ; writes the size from the FCB (the same rung reads 2048 back after a
                ; close and re-open). M36's persist step wrote it at once.


; --- the disk ROM's banner line (docs/disk-rom-layout.md) -------------------
; Own text, own code: nothing here is derived from any reference ROM. The only
; external facts are documented BIOS/cartridge interface points already used
; throughout this file -- CHPUT ($00A2) and the disk-ROM entry table, MSX2
; Technical Handbook (see disk/PROVENANCE.md). The placement rule this block
; relies on is our own: docs/disk-rom-layout.md.
; 🧭 2026-09-01 (Joost's call). The CF-3300 prints `Disk BASIC version 1.0` from
; its disk ROM, under the main banner; zerobas printed nothing -- `disk/` held no
; text at all but the "AB" signature. NO VERSION NUMBER: the main banner already
; carries `version 0.1`, and a second one would have to be kept in step with it
; for no benefit.
; 🔴 SITED HERE, IN THE PAD BELOW, AND THAT IS THE WHOLE POINT. The disk ROM is a
; PINNED-ADDRESS layout and its appendable tail is 2 B: three attempts to add
; this inline in `init` drove a `ds $XXXX - $` count NEGATIVE and ran pasmo past
; 64 KB. Code placed immediately BEFORE a pad shrinks that pad by exactly what it
; adds and moves no pinned address at all. `make diskmap` prints where the pads
; are; this one is the largest.
; 🟢 CHPUT direct: during the boot scan page 0 is still the main ROM, so this
; needs none of conout_emit_e's page-0 swap. Every register is preserved because
; `init` falls THROUGH into boot_disk.
disk_show_banner:
                push    af
                push    bc
                push    de
                push    hl
                ld      hl, disk_banner
                call    hke_pr              ; D-DISKREREAD (2026-11-26): the print loop IS hke_pr's
                                            ; (CHPUT, 0-terminated; the pops below restore every
                                            ; register and the flags), 9 B
                pop     hl
                pop     de
                pop     bc
                pop     af
                ret
disk_banner:
                db      "zerobas Disk BASIC",13,10,0

; --- (hk_present, D-MKHOOK's spike -- `scf / ret`, "a disk ROM is present" -- is
; GONE: every conversion hook got its own body, and DSKI$ / DSKO$, its last two
; users, run hk_dsk below since D-DSKLOCAL, 2026-10-15.)

; (hk_dsk -- DSKI$ / DSKO$'s handler -- moved in front of the pinned $53A7 entry
; when D-DISKREST slice 3 gave it the parse: this region had no room for it.)

; --- hk_mkfloat: MKS$/MKD$'s conversion, RUNNING IN THE DISK ROM -------------
; D-DISKABI, docs/spec-basic-nodisk.md §13. The FIRST disk-verb body actually to
; live here rather than merely being gated from here -- which is what Joost asked
; for ("the implementation ... should probably be in the disk rom").
;
; It is possible because this is a PAGE-1 ROM: while we are mapped at
; $4000-$7FFF, page 0 still holds slot 0, so main's LOW REGION is callable by
; absolute address. Those addresses arrive through disk/basic-resident-abi.inc,
; GENERATED per build from build/basic-reloc.sym, so a low-region shift cannot
; leave this routine calling stale ones (`make diskrom-abi-check`).
;
; ⚠️ EVERYTHING CROSSES IN RAM, NOT IN REGISTERS. CALSLT is documented to affect
; most of them, so the width arrives in STRSCR, the value in FAC/FACTYP, and the
; result goes back into STRSCR -- all work-area cells, all mapped throughout.
;   in   STRSCR = the width (4 or 8); FAC/FACTYP = the evaluated argument
;   out  STRSCR+1.. = the packed bytes; CF set (handled)
hk_mkfloat:
                ; 🔁 THE WIDEN IS HERE SINCE D-DISKREST slice 2 (2026-10-20). It
                ; used to run main-side because `widen_rhs_operand` reads the live
                ; RHS, which for an INTEGER argument is DE, and CALSLT does not
                ; preserve DE. But main stores DE at STRSCR+1 unconditionally
                ; (D-MKINT), so the int value crosses in RAM: reload it, dispatch
                ; on FACTYP as widen_rhs_operand does (which we cannot call -- its
                ; factyp_is2 is page 1), and widen into ARGA with the low-region
                ; bodies. For a float argument DE is junk and widen_fac_to ignores
                ; it, as before.
                ld      de, (STRSCR+1)
                ld      hl, ARGA
                ld      a, (FACTYP)
                cp      2
                jr      nz, hkm_wfac
                call    widen_int_to
                jr      hkm_wide
hkm_wfac:
                call    widen_fac_to
hkm_wide:
                ld      a, (STRSCR)
                cp      4
                jr      nz, hkm_dbl
                call    round_single_and_pack   ; 6-digit half-up round -> single
                jr      hkm_copy
hkm_dbl:
                call    round_and_finalize      ; exact widen -> double
hkm_copy:
                ld      a, (STRSCR)
                ld      c, a
                ld      b, 0
                ld      hl, FAC
                ld      de, STRSCR + 1
                ldir                        ; the packed bytes ARE the string
                scf
                ret

; --- hk_field: FIELD's body, RUNNING IN THE DISK ROM -----------------------
; docs/spec-basic-nodisk.md §9 for the hook; disk/docs/spec-diskcode-eviction.md
; §7.3-§7.5 for the slice. D-FIELDMOVE, the last member of the channel trio.
;
; 🎯 THIS IS THE THINNEST HANDLER IN THE ROM AND THAT IS THE FINDING, NOT A
; DEFECT. FIELD is a parse, a channel classify and a write into each variable's
; own descriptor (main's FLD_TAB until D-FLDDESC, 2026-10-17);
; none of it is disk work, so what moves is the DECISION to run rather than any
; body. It is here because the alternative -- main gating on H_FIELD and then
; doing the job anyway -- discards the answer of any ROM that claims the cell.
;
; 🔑 THE LOOP IS THE REASON THE ITEM BUNDLE RETURNS A CARRY. main's field_item
; parses ONE `w AS v$`, checks the ERR 50 bound, stores it and reports whether a
; comma followed; the crossings therefore scale with the ITEM COUNT (1 + N),
; unlike LSET/RSET's fixed three. A four-field FIELD is five crossings.
;
;   in   FN_RESUME = the statement cursor, just past the FIELD token
;   out  the statement is DONE; FN_RESUME = the resume cursor
;        CF = 1 (claimed), which is what chan_gate tests
;
; ⚠️ NO FIELD LAYOUT IS KNOWN HERE -- FLD_TAB's was main's, and since D-FLDDESC
; the layout is the variable descriptor's, which is main's too -- so nothing
; needs hand-mirroring in this ROM, the hazard disk/equates.inc's FSECTOR_BUF
; mirror actually caused (D-LRSETMOVE).
; (hk_field's code moved in front of the pinned $53A7 entry when D-DISKREST
; slice 4 gave it FIELD's prologue: this region had no room for it.)

; (hk_lrset -- LSET / RSET's handler -- moved to the hole in front of the pinned
; $53A7 entry when D-DISKREST slice 1 made it parse: ~60 B more than this region had.)



; --- hk_dskf: DSKF's body, RUNNING IN THE DISK ROM --------------------------
; docs/spec-basic-nodisk.md §9 for the hook, and D-DSKFDRV
; (scratchpad/kwt3_dskfchk.py) for the bound below. D-DSKFMOVE, Joost 2026-09-17:
; *"any keyword implemented by the disk rom should go via the hook to the disk
; rom"*.
;
;   in   FAC          = the drive argument, staged by main because chan_gate
;                       clobbers DE building its own return address
;   out  FAC          = the free cluster count (when DISKOP_STATUS = 0)
;        DISKOP_STATUS= 0 counted, else the ERR code main raises: 62 `Bad drive
;                       name` for a byte past the last drive, 5 `Illegal function
;                       call` for a value that is not a byte (D-DSKFRANGE)
;        CF = 1 (claimed), which is what chan_gate tests
;
; 📏 THE BOUND IS MEASURED, NOT JUDGED, and it moved here with the code it
; guards: on the CF-3300 drives 0, 1 and 2 are ACCEPTED (0 and 1 answer 707; 2
; prompts for the phantom B:) while 3, 4, 8 and 9 all answer `Bad drive name` --
; four points above the line and two below it. So the rule is `drive <= 2`.
; 🔴 THE CHECK WAS MISSING ENTIRELY RATHER THAN TOO WIDE when D-DSKFDRV added
; it: this tree answered the same 707 for 0, 1 AND 9, so nothing was validating
; the argument at all. ⚠️ WHAT DRIVE 2 *DOES* IS STILL UNMEASURED -- on the
; reference it prompts and WAITS, and a row that prompts eats its successors, so
; the probe excludes it and this only places the REFUSAL boundary.
;
; 🎯 THE COUNT ITSELF IS A CALL-BACK, and that is the honest shape today: main's
; `fat_count_free` is a 13 B stub that dispatches DISKOP_SEL_FAT_COUNT_FREE to
; the SUB ROM's fatprim tenant, so the walk is neither here nor in main. Moving
; the walk itself is part of the 2394 B question (TODO), not of this verb.
hk_dskf:
                ei                          ; D-HOOKDI: the hook arrives DI (CALSLT);
                                            ; the FAT walk below reads sectors
                ; 🔴 D-DSKFSLOW (2026-09-28): NOTHING TO COUNT WHILE AN ERROR IS
                ; PENDING. The evaluator DEFERS an argument error -- `DSKF()` is 0
                ; with FPERR=Syntax error, `DSKF("A")` FPERR=Type mismatch -- and
                ; raises it at the statement boundary, so this used to walk the
                ; whole FAT first: ~960 ms here where the CF-3300 answers in ~90
                ; (kwtime t6dskf2 / t6dskf13, 10.4-10.8x, over the T2 bar). The
                ; count is skipped and 0 handed back; main raises the SAME error
                ; at the SAME point as before, only without the walk.
                ld      de,0
                ld      a,(FPERR)
                or      a
                jr      nz,hkdf_done        ; a deferred error: count nothing
                ld      de,(FAC)            ; the drive argument
                ; 🔴 D-DSKFRANGE (2026-09-29): A BYTE FIRST, THEN A DRIVE. On the
                ; CF-3300 `DSKF(-1)` / `DSKF(256)` are 5 and `DSKF(9)` is 62
                ; (scratchpad/t6enum_b3.out); this read 62 for all three.
                ld      a,d
                or      a
                ld      a,5                 ; (flags kept) not a byte -> 5
                jr      nz,hkdf_bad
                ld      a,e
                cp      3                   ; 0..2 accepted, 3+ refused
                ld      a,62                ; (flags kept) no such drive -> 62
                jr      nc,hkdf_bad
                ld      a,e                 ; D-DSKIB: the drive DSKF named -- fat_mount's
                ld      (DK_FCB_DRV),a      ; drv_select prompts for B: (2) or back to A:
                call    fat_count_free      ; DE = free clusters -- LOCAL since
                                            ; D-DSKFLOCAL: no crossing to main,
                                            ; and none on to the sub ROM
hkdf_done:
                ld      (FAC),de
                xor     a                   ; counted (falls into hkdf_bad: carve step 4)
hkdf_bad:
                jp      hk_claim_status     ; DISKOP_STATUS := A, CF=1 (the same body)

; --- hk_cv: CVI/CVS/CVD's conversion, RUNNING IN THE DISK ROM ---------------
; docs/spec-basic-nodisk.md §9 (D-MKHOOK) is where these three are established as
; the DISK ROM's verbs rather than ours -- the same section basic/expr.asm's gate
; cites -- and §13 is the ABI hk_mkfloat above follows. The conversion itself is
; measured, not derived: MKS$/MKD$ emit exactly what the variable store holds
; (docs/spec-basic-faczero.md §0, byte-identical on all three machines), so the
; packed bytes ARE the value and the inverse is a copy, not an algorithm.
; D-CVMOVE (Joost, 2026-09-17: *"any keyword implemented by the disk rom should
; go via the hook to the disk rom"*). The inverse of hk_mkfloat above, and it
; lands here for a COMPATIBILITY reason rather than a byte one: until now these
; three hooks were a PRESENCE TEST ONLY -- main did the whole conversion after
; the call returned, so a foreign disk ROM that claimed H_CVI and computed the
; answer had it overwritten on the way out.
;
; ⚠️ EVERYTHING CROSSES IN RAM, NOT IN REGISTERS -- the same ABI hk_mkfloat
; states, for the same reason (CALSLT is documented to affect most of them).
;   in   FACTYP = the width, 2 / 4 / 8 -- the width IS the FACTYP code, so one
;                 cell carries the argument in and the answer's type out
;        STRPTR = the evaluator's DESCRIPTOR pointer, length-checked main-side
;                 (the D-CVITM check stays there: it must fire BEFORE the
;                 descriptor becomes a body pointer)
;   out  width 2:   FAC = the two bytes, FACTYP = 2
;        width 4/8: FAC = the packed value, FACTYP unchanged (= the width)
;        CF set (handled) in both arms
; 🔴 NOTHING CROSSES THROUGH STRSCR, AND THE FIRST CUT OF THIS ROUTINE DID.
; STRSCR is where MKI$/MKS$/MKD$ STAGE THEIR RESULT, so `CVI(MKI$(258))` hands
; this routine a string whose body IS STRSCR+1 -- and marshalling a pointer
; there wrote over the very value being converted. 12 of 32 rows DIFF; the ones
; that stayed green were the arguments that are not staged strings. A ROUND TRIP
; IS WHERE A SHARED SCRATCH BUFFER ALIASES ITSELF.
; 🎯 So the deref happens HERE instead: `pu_deref_body` is in main's LOW REGION
; ($28C1), which stays mapped while this ROM is, so the body can reach it by
; absolute address and nothing has to cross that was not already across.
hk_cv:
                ld      a,(FACTYP)          ; the width
                ld      c,a
                ld      b,0                 ; BC = the byte count for the ldir
                ld      hl,(STRPTR)         ; the descriptor...
                call    pu_deref_body       ; ...and its body (low region: $28C1)
                ld      a,c                 ; ⚠️ RELOAD: the call clobbers A
                cp      2
                jp      nz,hkcv_float       ; (moved out of jr reach: carve step 4)
                ld      e,(hl)              ; low byte
                inc     hl
                ld      d,(hl)              ; high byte -- DE = the int (LE)
                ld      (FAC),de
                ld      a,2                 ; CVI returns an int; a float nested in
                ld      (FACTYP),a          ; the string arg must not stick
                scf
                ret
; (hkcv_float -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $50A9.)

; --- ; --- install_basic_hooks: claim every BASIC-extension hook this ROM answers --
; Table-driven so init.asm costs ONE call: the pad before its $41EF pin is 31 B
; and seven inline installs need 45. docs/spec-basic-nodisk.md §9.
; Clobbers A, BC, DE, HL.
; --- hk_errp: print the disk error codes this ROM hosts (D-DISKERR) ---------
; Layout source: MSX2 Technical Handbook §2 (the hook table -- H.ERRP $FEFD is the
; error-print hook a disk ROM claims; CALLF stub layout as install_hook) and the
; BIOS entry list (CHPUT $00A2). Texts: black-box CF-3300 (`ERROR 68/69/70`).
; Reached from the errmsg tenant's H_ERRP call with A = ERR code, for codes main
; does not host. Prints the text through CHPUT (BIOS, page 0) and returns CF=1;
; a code that is not ours returns CF=0 and main prints `Unprintable error`.
; Texts measured on the CF-3300 (`ERROR 68/69/70` in a program): `Disk write
; protected`, `Disk I/O error`, `Disk offline`. D-DISKFULL (2026-10-02) adds 66,
; `Disk full`, read off the CF-3300's screen on a disk with no free cluster.
; 💰 D-DSKFMT carve 28 (2026-11-10): "Disk " once, then the code's suffix -- the
; (code - 66)-th 0-terminated string after it; 67 is not a disk message (an empty one
; holds its place). No pointer table.
hk_errp:
                sub     66
                cp      70 - 66 + 1
                jr      nc, hke_no          ; not 66..70
                cp      67 - 66
                jr      z, hke_no           ; 67: not ours
                ld      b, a
                inc     b
                ld      hl, dk_disk
                call    hke_pr              ; "Disk "; HL = the first suffix
hke_skip:
                dec     b
                jr      z, hke_pr
hke_skp1:
                ld      a, (hl)
                inc     hl
                or      a
                jr      nz, hke_skp1
                jr      hke_skip
hke_pr:
                ld      a, (hl)
                inc     hl
                or      a
                scf                         ; printed: CF=1 (SCF leaves Z alone)
                ret     z
                call    CHPUT
                jr      hke_pr
hke_no:
                xor     a                   ; CF=0: not ours
                ret
dk_disk:        db      "Disk ", 0
                db      "full", 0           ; 66 (D-DISKFULL; CF-3300 text, disk_probe_diskfull.py)
                db      0                   ; 67: not a disk message
                db      "write protected", 0    ; 68
                db      "I/O error", 0      ; 69
                db      "offline", 0        ; 70

; --- fdc_wp_chkrdy: the write path's NOT READY test (D-DISKERR; the body is in front of $553C) --
; Source: WD2793 data sheet via MSX2 Technical Handbook (type-I status bit 7 =
; NOT READY); the read command reports it on its own, the write command with no
; medium is accepted and completes clean on the emulated FDC (measured, 3/3), so
; the write path asks the seek's status first. Out: CY, A = 2 (not ready) / NC.
; (fdc_wp_chkrdy -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $553C.)

; bdos_dskio -- 🎯 D-DISKVE (2026-11-17): dskio for the BDOS's OWN transfers (write_sector,
; dskio_calslt -> read_sector, RDABS / WRABS; never the published $4010, whose raw codes PHYDIO
; and Disk BASIC expect). Same interface as dskio. On an error, when a disk-error handler is
; installed -- DISKVE ($F323) names a cell holding its address; our init zeroes the high byte
; under BASIC (install_basic_hooks), MSX-DOS's own leaves $D609 on both machines
; (scratchpad/XE3_stock.out, XE3_ours.out) -- the handler is entered as on the CF-3300
; (scratchpad/XE2_ign_stock.out): A = the drive, B = 1, C = DSKIO's code | 1 for a write. Its
; answer in C: 1 Retry (the same transfer again), 2 Abort (a warm boot: the CF-3300 lands at
; COMMAND.COM's prompt, scratchpad/XE4_scr_stock.out), 0 Ignore -- Cy = 0 with A = $FF (dskio's
; own success leaves A = 0), so FMAKE carries on and RDABS / WRABS tell the two apart.
; With no handler: dskio's result, every register as it left it (BASIC's route, unchanged).
bdos_dskio:
                push    hl
                push    de
                push    bc
                push    af
                call    dskio
                jr      nc, bdd_drop
                push    af
                ld      a, (DISKVE + 1)     ; the key: a handler cell outside page 0
                or      a
                jr      nz, bdd_hand
                pop     af
bdd_drop:                                   ; drop the saved inputs, nothing else touched
                inc     sp
                inc     sp
                inc     sp
                inc     sp
                inc     sp
                inc     sp
                inc     sp
                inc     sp
                ret
bdd_hand:
                pop     af
                ld      c, a                ; DSKIO's code
                pop     af                  ; A = the drive, Cy = a write
                push    af
                ld      b, a
                sbc     a, a
                and     1
                or      c
                ld      c, a                ; C = the code | the write bit
                ld      hl, (DISKVE)
                ld      e, (hl)
                inc     hl
                ld      d, (hl)
                ex      de, hl              ; HL = the handler; DE = the cell + 1, as the CF-3300's
                ld      a, b                ; A = the drive
                ld      b, 1
                push    bc
                call    jp_hl
                ld      a, c                ; the answer
                pop     bc                  ; B = 1, C = the code | write: what Ignore returns
                dec     a
                jr      z, bdd_retry        ; 1 Retry
                jp      p, $0000            ; 2 Abort: the warm boot
                pop     hl                  ; 0 Ignore: A = $FF already
                pop     hl
                pop     de
                pop     hl
                or      a                   ; Cy = 0, A = $FF: "ignored"
                ret
bdd_retry:
                pop     af
                pop     bc
                pop     de
                pop     hl
                jr      bdos_dskio

; seldsk_set -- BDOS $0E SELDSK: E = the drive (0 A:, 1 B:) -> A = the logical-drive count
; (DRVCNT, $F347). 🎯 D-DOSDRVB part 1 (2026-11-17): the drive is STORED in $F247 (CURDRV_CELL)
; as on the CF-3300, so CURDRV answers it (scratchpad/XB_stock.out); it only returned the count.
; An E past the count leaves $F247 alone (not measured on the reference). Every register but
; A / F as before.
seldsk_set:
                ld      a, (DRVCNT)
                cp      e
                ret     c
                ret     z               ; E >= the count: not a drive
                push    af
                ld      a, e
                ld      (CURDRV_CELL), a
                pop     af
                ret

; dsel_bdos -- 🎯 D-DOSDRVB part 2 (2026-11-17): fat_geom_read's drive select. A = the drive the
; operation named (DK_FCB_DRV: 0 the default, 1 A:, 2 B:). The default is the CURRENT drive,
; $F247 + 1 -- 0 under BASIC unless a $F37D SELDSK moved it, so BASIC's paths are unchanged.
; With the BIOS in page 0 (BASIC) it is drv_select, the D-DSKIB prompt. Under MSX-DOS (RAM in
; page 0; a machine with its RAM in primary slot 0 would read as BASIC here) the CF-3300 prints,
; after a CR LF even at a line's start, "Insert diskette for drive B:" / "and strike a key when
; ready", CR LF, and waits for a key (scratchpad/XB2_stock.out) -- through the BDOS console
; here, since CHPUT and drv_select's calbak into main's page 1 are not there. Drives past B:
; under MSX-DOS are left alone (only SELDSK's 0 / 1 reach here today).
dsel_bdos:
                or      a
                jr      nz, dsb_named
                ld      a, (CURDRV_CELL)
                inc     a                   ; the default: the current drive
dsb_named:
                push    af
                in      a, ($A8)
                and     3                   ; page 0's primary slot: 0 = the BIOS
                jr      nz, dsb_dos
                pop     af
                jp      drv_select          ; BASIC: D-DSKIB's prompt
dsb_dos:
                pop     af
                cp      3
                ret     nc
                dec     a                   ; 0 A:, 1 B:
                ld      hl, DSK_CURDRV
                cp      (hl)
                ret     z                   ; the drive already holds it: silent
                ld      (hl), a
                push    af
                ld      hl, dsl_text_nl
                call    fmt_pr              ; CR LF first, even at a line's start
                ld      hl, dsl_text_1
                call    fmt_pr
                pop     af
                add     a, 'A'
                ld      e, a
                call    conout_emit_e       ; the drive letter
                ld      hl, dsl_text_2
                call    fmt_pr              ; ":" CR LF "and strike a key when ready" CR LF
                jp      innoe_body          ; the key, no echo

; 🔴 D-ALIASWCELL (2026-09-23): THE RAW-SECTOR BUFFER IS ~~FSECTOR_BUF~~ FWBUF.
; It named FSECTOR_BUF -- main's FILE DATA sector -- so with a file open, DSKI$
; read a raw sector straight over the channel's staged data and DSKO$ wrote the
; channel's data back out over whatever sector it was given. Measured: with
; `chan_gate` re-staging the channel after every crossing, `DSKO$ 0,0` wrote the
; open file's records over SECTOR 0 and the disk lost its boot record entirely.
; 🔬 AND THIS IS THE REFERENCE'S OWN GEOMETRY, NOT AN INVENTION. The CF-3300
; answers this same cell with $EB95, which is its DIRECTORY/RAW sector buffer --
; disjoint from its file-data buffer at $ED95 (scratchpad/refbuf_*.out). FWBUF
; is our directory/metadata buffer, so pointing the cell there puts DSKI$/DSKO$
; on the same tier the reference puts them on.
; ⚠️ NOTHING PINS THE VALUE: every probe and gate row reaches the buffer through
; `PEEK(&HF351)+256*PEEK(&HF352)`, which is the MSX idiom and the point of the
; cell. ⚠️ The buffer stays volatile across a later disk operation -- a re-stage
; may re-read the FAT through FWBUF -- exactly as the reference's raw buffer is
; volatile across directory work.
install_basic_hooks:
                ; 🏗️ D-DISKROMHOST step 2b (2026-11-24, Joost: "do the two-step claim NOW, matching
                ; the reference cartridges"): THE CLAIM IS IN TWO STEPS, AS THE STOCK DISK ROMS
                ; MAKE IT, AND EACH STEP REBUILDS MS-BASIC'S LAYOUT UNDER THE NEW HIMEM
                ; (layout_rebuild below). Measured black box (scratchpad/diskhost2_ref.py ->
                ; AE_diskhost2_ref.out): the CF-3300's INIT lowers HIMEM $F380 -> $F195 (the
                ; VG-8020 + HBD-F1's $F196), its H.RUNC handler to $DF93 (/ $DF94), and both
                ; rewrite MEMSIZ / STKTOP / SAVSTK / FILTAB + its table / NULBUF by MS-BASIC's
                ; rule at each step. This INIT step reserves our cells from INIT_HIMEM up
                ; (DRVA_DPB $F195 is the lowest); hrunc_h makes the second, to the
                ; CF-3300's $DE77 (D-DISKHIMEM; CTRL held: $E48D, D-BOOTKEYS), which keeps
                ; the engine table rows 1..FCH_CEIL reserved (D-FCBSHAPE S2 / D-ENGROW0;
                ; asserted there). Before: ONE step to $DE77 here and no rebuild, so an
                ; MS-BASIC host kept the layout of HIMEM $F380 inside the claim.
                ; Scheme D (D-DISKROMHOST 4 (d)): every HIMEM is passed as an offset from
                ; GETWRK_AREA; layout_rebuild adds CLAIM_D (= GETWRK_AREA - N).
                ld      hl, INIT_HIMEM - GETWRK_AREA
                call    layout_rebuild
                ld      hl, FWBUF           ; D-DSKIO: publish the DSKI$/DSKO$ buffer at the
                ld      (DSKBUF_PTR), hl    ; cell the MSX idiom PEEKs (docs/spec-basic-dskio.md)
                xor     a                   ; D-DISKVE: no disk-error handler under BASIC --
                ld      (DISKVE + 1), a     ; bdos_dskio's key (it held $C9C9 here)
                ld      hl, hook_tab
ibh_lp:
                ld      a, (hl)             ; the hook cell's index ((cell - HOOK_LO) / 5)
                inc     a
                ret     z                   ; $FF terminates the table
                dec     a
                inc     hl
                ld      e, (hl)             ; ...and ITS OWN handler: the table is
                inc     hl                  ; pairs, because MKS$/MKD$ answer with a
                ld      d, (hl)             ; real body while the rest only report
                inc     hl                  ; presence
                push    hl
                ld      l, a
                ld      h, 0
                ld      b, h
                ld      c, l
                add     hl, hl
                add     hl, hl
                add     hl, bc              ; x 5: the MSX hooks are 5 B apart
                ld      bc, HOOK_LO
                add     hl, bc              ; HL = the hook cell
                call    install_hook
                pop     hl
                jr      ibh_lp
; The seven conversion hooks, each one of the 35 slots the CF-3300's disk ROM
; claims (scratchpad/hookdiff_probe.py) and each named in the MSX2 TH table.
; 💰 D-DSKFMT carve 27 (2026-11-10): a row is the cell's INDEX in the hook area (one
; byte; every cell here is an MSX hook, 5 B apart, $FDEF..$FFAC) and the handler --
; 3 B a row, not 4. tools/check_dead_code.py reads the `hookrow` lines.
HOOK_LO         equ     H_DSKO              ; the lowest cell in the table
hookrow         MACRO   cell, handler
    IF (cell - HOOK_LO) - ((cell - HOOK_LO) / 5) * 5
                db      HOOKROW_CELL_NOT_ON_A_5_BYTE_STEP
    ENDIF
    IF cell < HOOK_LO || (cell - HOOK_LO) / 5 > 254
                db      HOOKROW_CELL_OUT_OF_RANGE
    ENDIF
                db      (cell - HOOK_LO) / 5
                dw      handler
                ENDM
hook_tab:
                hookrow H_DSKF, hk_dskf     ; D-DSKFMOVE: body here, not just
                                             ; its presence
                hookrow H_MKI,  hk_mki       ; DE crosses in STRSCR+1; a FLOAT
                                             ; argument is converted HERE (D-MKIRANGE)
                hookrow H_MKS,  hk_mkfloat   ; the float coercion LIVES HERE now
                hookrow H_MKD,  hk_mkfloat
                hookrow H_CVI,  hk_cv       ; D-CVMOVE: the conversion lives HERE
                hookrow H_CVS,  hk_cv       ; now, not just its presence -- so a
                hookrow H_CVD,  hk_cv       ; claiming ROM's answer is not overwritten
                hookrow H_NAME, hk_name      ; D-DISKVERB2: body here too
                hookrow H_KILL, hk_kill      ; D-DISKVERB: the FIRST verb whose
                                             ; BODY lives here, not just its gate
                hookrow H_FILE, hk_files     ; D-DISKVERB4: FILES + LFILES
                hookrow H_LSET, hk_lrset     ; D-LRSETMOVE: both bodies HERE now
                hookrow H_RSET, hk_lrset     ; (one body; LRSET_JUST says which)
                hookrow H_FIELD, hk_field    ; D-FIELDMOVE: the trio is complete --
                                             ; body here, not just its presence
                hookrow H_DSKO, hk_dsk       ; D-DSKLOCAL: the sector I/O runs HERE now
                hookrow H_DSKI, hk_dsk       ; (a sub-ROM tenant behind a presence-only hook before)
                hookrow H_COPY, hk_copy      ; D-DISKVERB3: body here too
                hookrow H_ERRP, hk_errp      ; D-DISKERR: the disk codes' messages live HERE
                hookrow H_FORM, hk_format_gate ; D-FMTHOOK: CALL FORMAT's menu + tenant -- zerobas BASIC's only
                                             ; dispatch live HERE (main keeps the gate)
                hookrow H_SYST, hk_system    ; D-CALLSYSTEM: CALL SYSTEM, back to MSX-DOS
                hookrow H_CHOUT, hk_chout_e  ; S10.B: PRINT# to an OUTPUT channel, a byte
                                             ; (and SAVE ,A's, increment 3: _e below)
                hookrow H_CHCLOSE, hk_chclose_e ; S10.B: its CLOSE (likewise; S10.D:
                                             ; a RANDOM channel's too, A = its mode)
                hookrow H_DGET, hk_dget      ; S10.D: GET / PUT's record I/O
                hookrow H_CHIN, hk_chin      ; S10.C: an INPUT channel's byte / EOF /
                                             ; numeric item (disk/fat.asm)
                hookrow HPHYD, DSKIO_ENTRY  ; so a foreign MSX-BASIC / MSX-DOS host reaches our
                                             ; DSKIO (it was 9 B inline in INIT's pinned pad)
                hookrow H_RUNC, hrunc_h      ; D-DISKROMHOST: phase 2 + the DOS boot (disk/init.asm)
                ; --- H_FOPEN: INSTALLED 2026-09-20 (D-STOPRELATCH) --------
                ; 🏗️ Joost ruled §6.6m, re-affirmed in §6.7's *"we do as the
                ; reference does"*: LOAD arrives at the cell the REFERENCE uses,
                ; and which verb is asking comes from a SELECTOR in RAM (§6.6n).
                ;
                ; 🟢 THREE EXPLANATIONS FOR WHY THIS ROW COULD NOT EXIST WERE
                ; RETRACTED IN TURN, AND THE LAST ONE WAS REAL AND IS FIXED.
                ; §6.6p blamed the CELL ($FE5D is H.NULO, a file-buffer-0
                ; subsystem cell). §6.6t retracted that and blamed the hook
                ; COUNT -- "we cannot install an 18th". §6.6af withdrew that
                ; too: the row carrying it reads RED on the UNMODIFIED ROM at
                ; 15 of 20 timing configurations. §6.6ag then installed the row
                ; and ran a full battery -- 129/132, the only behavioural red
                ; being that same row. §6.6ah fixed the divergence underneath
                ; it (a held Ctrl-STOP now re-fires, `basic/traps.asm`), and
                ; the row is green at 20 of 20 with a 127x margin.
                ;
                ; ⚠️ MAIN DOES NOT CALL THIS YET. The main-ROM side (write
                ; FOPEN_SEL, call the cell, decode DISKOP_STATUS) is the next
                ; slice. Until it lands this row is inert on this machine:
                ; nothing enters $FE5D -- C-BIOS defines the name and never
                ; references it -- so every path behaves exactly as before.
                ; 🔴 AND THE SELECTOR IS WHY IT IS SAFE EVEN IF IT IS ENTERED:
                ; FOPEN_SEL_LOAD is $4C, outside DISKOP_SEL_*'s whole 0..7
                ; range, so a call arriving with the cell merely idle takes
                ; hk_dpload's `ret nz` and answers CF=0 -- "not mine" -- without
                ; touching the drive. That is the fix §6.6p's own footnote
                ; records, and it is what makes landing the row ahead of its
                ; caller a safe intermediate state rather than a half-wired one.
                hookrow H_FOPEN, hk_dpload
                db      $FF                 ; the end

; --- install_hook: write one 5-byte CALLF stub into a hook slot -------------
; Layout source: MSX2 Technical Handbook §2 (inter-slot call / CALLF) --
; `RST 30H (0F7H) ; slot byte ; addr-lo ; addr-hi ; RET (0C9H)`, the same five
; bytes disk/equates.inc cites for HPHYD. CORROBORATED black-box: a diff of the
; hook region between the diskless VG-8020 and the CF-3300 shows exactly that
; shape in all 35 slots the reference disk ROM claims
; (scratchpad/hookdiff_probe.py, docs/spec-basic-nodisk.md §5.2). No stock
; routine internals decoded -- only the RAM cell's own byte layout, which is
; published interface.
; in:  HL = hook address (a RAM cell in $FD9A..$FFE7)
;      DE = target address in THIS ROM
; out: HL advanced past the stub. Clobbers A, HL.
; Sited here rather than in init.asm because the pad before the $41EF pin is only
; 25 B and one inline install already spent all of it.
; (fat_bufinit, the BDOS buffer pair's set-up, is inline in disk/init.asm's init_p2 since
; D-DISKROMHOST: its 13 B here went to hrunc_h below.)
; --- ftc_totsec: total sectors from the boot sector's BPB +19 ----------------
; disk/docs/spec-diskcode-eviction.md §6.4a. Split out of fat_total_clusters
; because that routine then sat in a region pinned by `ds $4B59 - $` with no slack (that pin is gone, 2026-10-15):
; the 3-byte `ld hl,(WBUF + 19)` it replaces could not grow in place. Reached by
; label only, so it is position-free here.
;   in   the boot sector is already in the METADATA buffer
;   out  HL = total sectors (16-bit, BPB +19). Clobbers DE.
; hrunc_h -- D-DISKROMHOST (2026-11-24): the H.RUNC ($FECB) handler INIT claims
; (hook_tab's H_RUNC row): this ROM's phase 2 and the DOS boot, ONCE. The hook goes
; back to five RETs first -- its RST 30h has run already, and the CALLF returns to the
; stub's own RET at $FECF -- so the later H.RUNC calls (NEW, a typed line, RUN, on
; either host) find a plain RET and phase 2 never repeats. A BASENT restart reruns
; INIT, which claims the hook again, and phase 2 then runs once more, as it ran in
; INIT before. Here in the corridor because the INIT region has 1 B left; it took
; fat_bufinit's 13 B, whose body is inline in init_p2 now (disk/init.asm).
; 🏗️ STEP 2b (2026-11-24): before phase 2 it calls the main ROM's start-up-screen routine
; and makes the second HIMEM claim, as the stock cartridges do in their H.RUNC handler.
hrunc_h:
                ld      hl, $C9C9           ; $FECB..$FECE; $FECF is the stub's own RET:
                ld      (H_RUNC), hl        ; all five RET, as the CF-3300 and the
                ld      (H_RUNC + 2), hl    ; VG-8020 + HBD-F1 leave the hook after it
                                            ; (scratchpad/AE_diskhost_*.out, H.RUNC-all-RET)
                ; --- host contract item 4: the routine whose address the HOST's main ROM keeps
                ; at $7D2F (docs/concepts/disk-rom-host.md): read the word with RDSLT from the
                ; main ROM's slot (EXPTBL) and CALSLT it there, as the HBD-F1 / NMS 1200 do
                ; (scratchpad/AE_dosref_zbcalls.out). It sets up the machine's start-up screen
                ; -- TEXT1 37 columns on the VG-8020 and on zerobas EU (STARTUP_SCREEN = INITXT,
                ; basic/program.asm), SCREEN 1 on the Japanese machines -- before MSX-DOS starts.
                ; Our code reads a byte of whatever main ROM hosts it; RDSLT keeps HL and IX.
                ld      hl, STARTUP_WORD + 1
                call    hr_rdmain           ; A = the high byte
                ld      ixh, a
                dec     hl
                call    hr_rdmain           ; A = the low byte
                ld      ixl, a
                call    calbak              ; CALSLT in the main ROM's slot (disk/driver.asm)
                ; --- the claims, in the CF-3300's moments (D-DISKROMHOST step 4 (c), 2026-11-25;
                ; docs/ram-atlas.md, scratchpad/AE_diskhost2_ref.out): H.RUNC claims to DOS_RESV_TOP
                ; ($DF93) -- the HIMEM MSX-DOS runs with -- and only Disk BASIC's start takes the last
                ; piece, to $DE77 (CTRL held at power-on: the one-drive $E48D, D-BOOTKEYS). Disk BASIC
                ; starts when init_p2 RETURNS (no disk, a data disk, the date prompt, a BASENT
                ; restart); MSX-DOS never returns from it, so the DOS path keeps $DF93. CTRL is read
                ; HERE, before init_p2: the key is still down (the scan and this call are a few ms
                ; apart), not after a date prompt; its HIMEM waits on the stack.
                ld      a, 6
                call    $0141               ; SNSMAT row 6, bit 1 low = CTRL held
                and     $02
                ld      hl, DISK_HIMEM - GETWRK_AREA    ; offsets: layout_rebuild adds CLAIM_D
                jr      nz, hrh_him
                ld      hl, CTRL_HIMEM - GETWRK_AREA
hrh_him:
    IF DISK_HIMEM > TXTMAX-FCH_CEIL*FCH_STATESZ
                db      DISK_HIMEM_ABOVE_THE_ENGINE_TABLE   ; rows 1..FCH_CEIL must stay reserved
    ENDIF
    IF CTRL_HIMEM > TXTMAX-FCH_CEIL*FCH_STATESZ
                db      CTRL_HIMEM_ABOVE_THE_ENGINE_TABLE   ; the CTRL boot's too (disk/init.asm)
    ENDIF
                push    hl                  ; Disk BASIC's HIMEM, for when init_p2 returns
                ld      hl, DOS_RESV_TOP - GETWRK_AREA
                call    layout_rebuild      ; the H.RUNC claim: MSX-DOS runs with $DF93 (- N)
                call    init_p2             ; MSX-DOS: no return. Otherwise BASIC is next --
                pop     hl
                jr      layout_rebuild      ; -- so the last claim, and back to H.RUNC's caller
; fat_wclus_x15 -- D-DISKROMHOST step 2b: the run it replaces stood at 2 sites (fat-prim-body.inc)
fat_wclus_x15:
                ld      hl, (FWR_CLUS)
                ld      d, h
                ld      e, l
                srl     d
                rr      e                   ; DE = cluster>>1
                add     hl, de              ; HL = byte offset within the FAT
                ld      a, h
                ret
; dta_put_cnt -- D-DISKROMHOST step 2b: the run it replaces stood at 2 sites (kernel.asm)
dta_put_cnt:
                call    dta_put             ; D-P1DOS: a page-1 DTA under MSX-DOS too
                ld      hl, (RDBLK_CNT)
                dec     hl
                ld      (RDBLK_CNT), hl
                ret
; op_lfiles -- D-DISKROMHOST step 2b: the run it replaces stood at 5 sites (kernel.asm)
op_lfiles:
                ld      a,(DISKOP_OP)
                cp      DISKOP_SEL_LFILES
                ret
hr_rdmain:
                ld      a, (EXPTBL)         ; the main ROM's slot (MSX2 TH: EXPTBL's first byte)
                jp      RDSLT

; --- layout_rebuild: HIMEM := HL, and MS-BASIC's layout under it (D-DISKROMHOST step 2b; black-box oracle) ---
; Cells: MSX2 TH work-area map. Own code; the values below are measured, no reference byte read.
; The rule both references follow at each claim step (scratchpad/AE_diskhost2_ref.out; the
; same rule docs/concepts/disk-rom-host.md item 2 records at HIMEM $F380 and $DF95): the file
; area on top -- FCB #1 at HIMEM - 265, FCB #0 at HIMEM - $212, the pointer table FILTAB =
; FCB #0 - 4 holding the two, NULBUF = FCB #0 + 9 -- then MEMSIZ = FILTAB - 2 (= HIMEM -
; ((MAXFILES+1)*267 + 2)), STKTOP = MEMSIZ - 200, SAVSTK = STKTOP - 2; FRETOP is left alone,
; as the references leave it (BASIC's next CLEAR sets it). MAXFILES 1 and 200 string bytes
; are the values at both calls (INIT and the cold start's H.RUNC). On zerobas the main ROM
; recomputes the same cells from HIMEM at its next clear_vars (D-BOOTCELLS), so this only
; moves an MS-BASIC host's layout out of the claim. Clobbers AF? no: BC, DE, HL.
; 🔴 SCHEME D (D-DISKROMHOST 4 (d), 2026-11-26): HL comes in as HIMEM - GETWRK_AREA, and the
; HIMEM stored is that + CLAIM_D (= GETWRK_AREA - N), so every claim step lands N below the
; constant, as both references' do under a foreign claim of N bytes (scratchpad/AE_wasize.md).
; CLEAN-ROOM: HIMEM and the MS-BASIC layout cells (MSX2 TH work area), measured as a black box.
; (The F of "AF? no" above: its own `add hl,de`s always changed the carry; A is kept.)
layout_rebuild:
                ld      de, (CLAIM_D)
                add     hl, de              ; the HIMEM of this claim step, N below the constant
                ld      (HIMEM), hl
                ld      de, -265
                add     hl, de              ; FCB #1
                push    hl
                add     hl, de              ; FCB #0 (HIMEM - $212)
                push    hl
                ld      bc, 9
                add     hl, bc
                ld      (NULBUF), hl        ; FCB #0 + 9
                ld      de, -13
                add     hl, de              ; FCB #0 - 4
                ld      (FILTAB), hl
                pop     de                  ; the table: FCB #0, FCB #1
                ld      (hl), e
                inc     hl
                ld      (hl), d
                inc     hl
                pop     de
                ld      (hl), e
                inc     hl
                ld      (hl), d             ; HL = FILTAB + 3
                ld      de, -5
                add     hl, de
                ld      (MEMSIZ), hl        ; FILTAB - 2
                ld      de, -200
                add     hl, de
                ld      (STKTOP), hl
                dec     hl
                dec     hl
                ld      (SAVSTK), hl
                ret

; --- sltwrk_pub: our SLTWRK entry, then INIT (D-DISKROMHOST 4 (d) scheme D; owner's ruling 2026-11-26) ---
; CLEAN-ROOM: SLTWRK ($FD09, MSX2 TH work area: 2 B per primary x secondary slot x page) and the value
; the references keep in their own page-1 entry, read as RAM cells only (scratchpad/AE_wasize_ref.out,
; AE_wasize_claimN.out): the CF-3300 $F1BF - N, written at its INIT (already there at its first H.RUNC).
; In: HL = CLAIM_D (GETWRK_AREA - N), the INIT's AF on the stack (A = our slot byte, F000SSPP).
sltwrk_pub:
                ld      de, SLTWRK_BASE - GETWRK_AREA
                add     hl, de
                ex      de, hl              ; DE = SLTWRK_BASE - N
                pop     af
                push    af
                ld      l, a
                and     $03
                rrca
                rrca
                rrca                        ; PP * 32
                ld      h, a
                ld      a, l
                and     $0C
                add     a, a                ; SS * 8
                or      h
                add     a, low (SLTWRK + 2) ; + page 1's word
                ld      l, a
                ld      h, high SLTWRK
                ld      (hl), e
                inc     hl
                ld      (hl), d
                pop     af
                jp      init

; 🏗️ D-FATENG Option 2 (Joost, 2026-09-18): ONE FAT12 ENGINE, ONE SOURCE, TWO
; ROMs -- the same file sub.rom assembles. disk/fat.asm keeps only the
; BDOS-specific half and the pinned M26 trampolines, which resolve to these
; labels BY NAME.
; ⚠️ INCLUDED **HERE**, IN THE FREE CORRIDOR, AND THE POSITION IS NOT A
; PREFERENCE. Placed after fat.asm it pushed kernel.asm past a pin and pasmo
; wrapped the image; appended after runtime.asm it overran the ROM by 1369 B,
; because disk.rom's 8314 B of free space is 32 INTERIOR HOLES, not a tail. This
; corridor was the largest when this was written (2712 B at $6B0D; by 2026-10-15
; the largest hole was ~843 B -- `make basic-reloc`, never this text) and is where install_hook below
; already lives for the same reason.
                ; ⚠️ REPO-ROOT RELATIVE, NOT "../basic/...". pasmo accepts
                ; both, but tools/check_tenant_closure.py's _resolve_include
                ; treats any path containing "/" as repo-root relative -- so
                ; "../basic/..." resolved ABOVE the repo and the body was
                ; SILENTLY DROPPED from the disk closure, leaving five of its
                ; labels looking like unresolved main references. sub/fatprim.asm
                ; uses this form for the same file.
                include "basic/fat-prim-body.inc"
                ; --- fat_delete, for the KILL loop below ----------------
                ; D-KILLLOCAL (disk/docs/spec-diskcode-eviction.md §6.6ay).
                ; The SAME shared body sub/fatprim.asm includes; it is a
                ; separate file only to preserve main's original byte order
                ; (see its header). Its six callees -- fat_mount, fat_find,
                ; fat_next_cluster, fat_write_fat_entry, read_sector,
                ; fatprim_write_sector -- are all in the block above, so this
                ; costs no marshalling either.
                include "basic/fat-delete-body.inc"

                ; --- the FAT STREAM layer, the primitives' caller ----
                ; D-DPLPORT (spec-diskcode-eviction.md §6.6b). Option 2
                ; brought the PRIMITIVE body here; the tokenised LOAD loop
                ; calls `fat_io_getbyte` ONCE PER BYTE, and §6.2c measured
                ; that an inter-slot crossing per byte costs 1.72x the
                ; whole load. So the stream layer is assembled here too --
                ; same file, same one source, second ROM.
                ; ⚠️ It names FAT_DBUF, never FSECTOR_BUF: this ROM fills
                ; $E2A0 while basic-resident-abi.inc binds FSECTOR_BUF to
                ; $E5C0, so the old spelling assembled clean and read a
                ; buffer nobody filled (§6.6c). [D-BUFMERGE, 2026-09-24: the
                ; two are now ONE address; the neutral name stays.] Its 6 bytes of cursor state
                ; are FREAD_OFF/FREAD_LEFT, aliased onto the RDBLK scratch
                ; in init.asm (§6.6f).
                include "basic/fatio-body.inc"
                ; --- the WRITE side, for step 12's hk_dpsave ----------------
                ; D-SAVEPORT. The same four bodies the SUB-ROM save tenant
                ; includes, in the same order, byte-identical: the sequential
                ; write cursor, then the helpers over it, then SAVE's engine.
                ; Beside fat-prim-body.inc above, fat_mount / fat_dir_create /
                ; fat_flush_data_sector / fat_dir_update are IN-PAGE calls here
                ; exactly as they are in the tenant -- which is the whole reason
                ; the move costs no marshalling.
                include "basic/fatiocreate-body.inc" ; fat_io_create
                include "basic/fatiow-body.inc"      ; fat_io_putbyte / fat_io_close
                include "basic/sv-diskwr.inc"        ; disk_write_begin/putbyte/
                                                     ; putword/write_end
                include "basic/sv-savdisk.inc"       ; sav_disk_write .. sav_data

; --- hk_dpsave: the tokenised SAVE write engine, in the disk ROM ------------
; D-SAVEPORT, step 12 of spec-diskcode-eviction.md §6.2, under the same ruling
; as step 9: *"we do as the reference does"*. Reached through the SAME claimed
; cell as LOAD with FOPEN_SEL = FOPEN_SEL_SAVE ($53).
;
; 🎯 WHY THIS IS A FAITHFULNESS MOVE AND NOT A CARVE. The body was ALREADY
; evicted from main page 1 -- `do_save` used to end `jp sv_tenant`, running
; `sav_disk_write` in the SUB-ROM (SUBROM_IDX_SAVE). So this frees no main byte.
; What it buys is §0.0's point (3): before it, `do_save` reached no hook at all,
; so a machine with a FOREIGN disk ROM still saved through OUR engine. Now the
; cell decides, and a cartridge that claims $FE5D gets the verb.
;
; 🔴 THE BODY IS THE SAME SOURCE, NOT A COPY. `basic/sv-savdisk.inc` and the
; helpers below it are included byte-identically by main, sub and disk; the
; names they spell resolve in all three because the four they needed from main
; (PRGEND, DSV_PTR, DSV_END, DISKSLOT_OK) are published through the GENERATED
; basic-resident-abi.inc. Forking the body for a third ROM was the alternative
; and it is exactly what that generator exists to avoid.
;
;   in   FOPEN_SEL = FOPEN_SEL_SAVE; DISK_FCB_NAME = the 8.3 name main parsed
;   out  CF = 1 claimed; DISKOP_STATUS = 0 written / 3 mount, disk-full or I-O
; 🆕 2026-10-19 (D-ROMSIZE step 1): BSAVE to disk shares this prologue and unwind.
; Its engine (basic/sv-bsvdisk.inc) sits in the hole after the pinned $50E0 entry
; -- this region (to $75A5) had 45 B -- and hk_dpbsave there enters at hkds_go
; with HL = bsv_open; SAVE loads HL = sav_disk_write here. hkds_run's `jp (hl)`
; replaced its `jp sav_disk_write`.
hk_dpsave:
                ld      hl,sav_disk_write   ; the engine hkds_run jumps to
hkds_go:                                    ; hk_dpbsave enters here, HL = bsv_open
                ei                          ; D-HOOKDI: the hook arrives DI (CALSLT),
                                            ; and the whole save follows
                xor     a
                ld      (DISKOP_ERR),a      ; D-DISKERR: no DSKIO failure pending
                ld      (DISKOP_STATUS),a   ; assume written; sv_load_error flips it
                call    hkds_run            ; sv_load_error's `ret` lands HERE, from
                                            ; any depth (D-DISKFULL, below)
                ld      a,(DISKOP_STATUS)   ; D-ERRKEEP2: out through the shared
                jp      hk_claim_status     ; tail, so a 68 survives an open file
; (sited here, beside its unwind point, since D-SAVEFULLSTAMP grew the engine
; above past the shared `disk_write_end`'s `jr c,sv_load_error` reach)
sv_load_error:
                ld      a,3                 ; mount / disk full / write / I-O
                ld      (DISKOP_STATUS),a
                ld      sp,(DPSAVE_SP)      ; D-DISKFULL: unwind to hk_dpsave, from
                ret                         ; whatever depth the failure was found at

; sv_load_error -- the disk-local reporter, rebound exactly as the sub-ROM
; tenant rebinds it. Resident it is a zero-byte EQU onto `load_error`, which
; PRINTS; here it must not print, because main's `disk_error` reports ONCE from
; the status this leaves behind -- the same contract step 9's arm already uses.
; No TAPIOF: a disk SAVE never armed the tape.



; --- hk_dpload: the tokenised LOAD loop, in the disk ROM ---------------------
; D-DPLMOVE. Step 9 of disk/docs/spec-diskcode-eviction.md §6.2, unblocked by
; Joost's ruling in §6.6m: the hook is the cell the REFERENCE uses ($FE5D,
; H_FOPEN), and the verb comes from a SELECTOR.
;
;   in   FOPEN_SEL = FOPEN_SEL_LOAD; DISK_FCB_NAME = the 8.3 name main parsed
;   out  CF = 1 claimed; DISKOP_STATUS = 0 loaded / 1 not found / 2 not
;        tokenised (main's ASCII path takes it) / 3 mount or I-O / 4 out of
;        memory.  CF = 0 = "not my selector" -- main carries on as if unhooked.
;
; THE CODE COMES BACK IN RAM, NOT IN A. §6.6h designed this hook as "CF=1 and
; A = a code"; this answers through DISKOP_STATUS instead, because that is what
; every hook already in hook_tab does -- hk_files, hk_kill and hk_copy all put
; their disposition there and main decodes it. CF still rides the CALLF return,
; which chan_gate has always relied on.
; ⚠️ HONESTY ABOUT WHY THIS CHANGED: it was my first guess at §6.6o's red
; `l.load` row and it was WRONG -- the row stayed red. The real cause was that
; disk.rom never wrote DISKOP_ERR at all. This is kept because consistency with
; the other four hooks is worth having, NOT because it fixed anything; a comment
; claiming otherwise would be the same class of unverified claim §6.6l is about.
;
; 🎯 WHY THE LOOP AND NOT JUST THE OPEN. §6.2c MEASURED the alternative: leaving
; the byte loop in main and reaching the stream across the slot costs one
; inter-slot crossing PER BYTE, 1.72x the whole load. `fat_io_getbyte` resolves
; LOCALLY here (the stream layer is included above), so every byte of a 14 KB
; program is an in-page call.
;
; 🔴 NO CALL-BACKS, AND THAT IS A PROPERTY TO KEEP. The body reaches main only
; through RAM -- CLPTR/CLINK/TXTBASE/TXTMAX out of the GENERATED
; basic-resident-abi.inc, so a repack that moves the text ceiling cannot leave
; the bounds check reading the old one. Every decision main has to make it makes
; from A, on its own side. If an arm here ever needs `calbak`, re-read
; FOPEN_SEL's mutual-exclusion note in disk/equates.inc first.
hk_dpload:
                ld      a,(FOPEN_SEL)
                cp      FOPEN_SEL_SAVE
                jr      z,hk_dpsave         ; step 12 (D-SAVEPORT)
                cp      FOPEN_SEL_BSAVE
                jp      z,hk_dpbsave        ; BSAVE to disk (2026-10-19) -- far: in
                                            ; the hole after $50E0, with its engine
                cp      FOPEN_SEL_BLOAD
                jp      z,hk_dpbload        ; BLOAD from disk (2026-10-19) -- far:
                                            ; beside hk_dpbsave, same hole
                cp      FOPEN_SEL_AOPEN
                jp      z,hk_aopen          ; step 11 (D-MERGEPORT) -- far: D-SAVEFULLSTAMP moved it
                cp      FOPEN_SEL_GETB
                jp      z,hk_agetb          ; step 11, once per BYTE -- far, likewise
                cp      FOPEN_SEL_OCHK
                jp      z,hk_ochk           ; D-OPENSAME: OPEN's same-file check
                cp      FOPEN_SEL_MAKE
                jp      z,hk_fmake          ; S10.B: OPEN FOR OUTPUT
                cp      FOPEN_SEL_APND
                jp      z,hk_fapp           ; S10.B increment 2: OPEN FOR APPEND
                cp      FOPEN_SEL_RAND
                jp      z,hk_frnd           ; S10.D: OPEN RANDOM
                cp      FOPEN_SEL_INP
                jp      z,hk_finp           ; S10.C: OPEN FOR INPUT (disk/fat.asm)
                cp      FOPEN_SEL_AUTO
                jp      z,hk_auto           ; AUTOEXEC.BAS's boot probe (2026-10-19)
                cp      FOPEN_SEL_LOAD
                ret     nz                  ; CF=0: not mine. Step 10 (OPEN) adds
                                            ; its arm right here.
                ei                          ; D-LOADDI: the hook arrives through
                                            ; CALSLT, which returns DI (as at
                                            ; hk_dpopen's calbak), and the driver
                                            ; keeps the CALLER's state per sector
                                            ; (fdc_di_save) -- so the WHOLE load ran
                                            ; masked: TIME stood still for 6.9 s,
                                            ; where the CF-3300's runs between
                                            ; sectors (scratchpad/loadclock_run.out)
                ; mount and find SEPARATELY: their two carries are what tells
                ; `file not found` from a mount/I-O fault, which is the whole
                ; reason fatio-body.inc carries the zero-byte `fat_io_find`
                ; label (D-BLNF). DISKOP_OP is not read on this path -- and it
                ; could not be, it is carrying the selector.
                call    fat_mount
                jr      c,hdl_io            ; 3: mount / I-O (`jr` since D-DSKCHG: the loop
                                            ; body that kept it `jp` moved out, D-LOADDIRECT)
                call    fat_io_find
                jr      c,hdl_nf            ; 1: not found
                ; seed the store cursor. ⚠️ HOISTED ABOVE THE MARKER READ for
                ; D-TRUNCLOAD's reason, unchanged by the move: an EOF on the
                ; very first byte (a ZERO-BYTE file) must commit an EMPTY
                ; program like every other EOF here, and cannot with CLPTR
                ; unset. Seeded AFTER the open, exactly where main seeded it,
                ; so a failed load leaves these cells as untouched as before.
                ld      hl,(TXTTAB)         ; D-TXTTAB: at the base as it stands
                ld      (CLPTR),hl
                ld      hl,0                ; D-LOADBASE: A_0 (the SAVING machine's
                ld      (CLINK),hl          ; base) is unknown: 0 = scan line 1
                ld      l,$80               ; D-LOADFAT: FAT_DEFER = $80, LOAD's FAT
                ld      (FAT_DEFER),hl      ; cache on; FAT_FCACHE = 0, nothing in
                                            ; FAT_MBUF yet (the mount just zeroed
                                            ; FAT_DEFER; hdl_answer clears bit 7)
                ; first byte selects the format: $FF = tokenised.
                call    fat_io_getbyte
                jr      c,hdl_ok            ; D-TRUNCLOAD: EOF here is an empty
                                            ; program, not an error
                cp      BASIC_DISK_ID
                jr      nz,hdl_ascii        ; 2: main re-opens and tokenises
                jp      hdl_direct          ; D-LOADDIRECT (disk/runtime.asm): the
                                            ; whole file in place, then hdl_walk
; 🗑️ D-LOADDIRECT (2026-11-07): THE BYTE STREAM IS GONE. hdl_line / hdl_len /
; hdl_body / hdl_get_store parsed the file a byte at a time through
; fat_io_getbyte, with D-LOADRUN's hdl_run (disk/runtime.asm) and D-LOADBASE's
; hdl_scan (disk/driver.asm) beside them; hdl_direct + hdl_walk apply the same
; rules to the file read in place.
hdl_ok:
                xor     a                   ; 0: loaded
                jr      hdl_answer
hdl_nf:
                ld      a,1                 ; 1: file not found
                jr      hdl_answer
hdl_ascii:
                ld      a,2                 ; 2: not tokenised
                jr      hdl_answer
hdl_io:
                ld      a,3                 ; 3: mount / I-O
; hdl_answer: the ONE exit. DISKOP_STATUS carries the code across the hook
; return -- see the header for why it is not A -- and CF=1 says "claimed".
hdl_answer:
                ld      (DISKOP_STATUS),a
                ld      a,(FAT_DEFER)       ; D-LOADFAT: LOAD's FAT cache off; the
                and     $7F                 ; other bits are a write's (another
                ld      (FAT_DEFER),a       ; handler may hold one): kept
                ; 🔴 DISARM THE SELECTOR ON THE WAY OUT. The cell is shared and
                ; $FE5D is reached by callers that are not this verb (§6.6p);
                ; leaving it armed means the NEXT such call falls through into a
                ; FAT mount. Cleared here rather than in main because the side
                ; that CONSUMED the selector is the side that should retire it,
                ; and a main-side clear leaves a window between the two.
                ; 0 is DISKOP_OP's own idle value, and is not the magic.
                xor     a
                ld      (FOPEN_SEL),a
                scf
                ret





; --- hk_kill: KILL's body, RUNNING IN THE DISK ROM --------------------------
; D-DISKVERB, spec-diskbasic-hook-rearchitecture.md phase 2. The first verb whose
; body follows its hook, which is what the reference does on all 35 of its cells
; (D-CFARCH) and what Joost directed.
;
; Main's ex_kill is now the cursor hand-off and the status decode; everything
; between is here. Two things it used to do are GONE rather than moved:
; `chan_gate`'s presence test (we ARE the answer to it) and `diskslot_test`
; (likewise). The diskless ERR 5 is unaffected -- it comes from chan_gate finding
; the cell UNCLAIMED, which is exactly the case where this body does not exist.
;
;   in   FN_RESUME = the statement cursor, just past the KILL token
;   out  DISKOP_STATUS = the dirverb disposition; FN_RESUME = the resume cursor;
;        CF = 1 (claimed), which is what chan_gate tests
;
; 🔴 THERE IS NO `ei` HERE, AND 0c PREDICTED THERE WOULD BE. 0c measured that a
; handler is entered with interrupts OFF and that this stops the clock outright
; (~1 s of disk-ROM work advanced TIME by 3 frames), and concluded the call-back
; ABI should let them back on. So an `ei` was written here -- and then MEASURED,
; with and without, one line apart: `KILL "NOSUCH.XYZ"` against a real disk
; advanced TIME by 2 frames BOTH WAYS (scratchpad/xslot_kill_ei.py).
; It changed nothing, for two reasons worth keeping:
;   * this verb is ~33 ms, so any freeze is below the resolution of the thing
;     being protected; 0c's figure was for a ~1 s window.
;   * `subrom_call` already does `di / call CALSLT / ei` UNCONDITIONALLY, so the
;     moment dirverb_op runs, interrupts come back on regardless of us. The only
;     window an `ei` here would have covered is entry -> the first call-back:
;     a handful of instructions.
; It is not shipped, because a byte whose measured effect is zero should not be.
; ⚠️ THAT IS A STATEMENT ABOUT *THIS* VERB'S SIZE, NOT A GENERAL ONE. A phase-3
; body that does long work in THIS ROM rather than inside a call-back re-opens
; it, and 0c's numbers are the ones to re-read then.
hk_kill:
                call    hk_fname2           ; 💰 D-DISKCARVE7: the name -> DISK_FCB_NAME; may
                                            ; RAISE (ERR 13) and NOT return -- see the ABI
                ; 🎯 EVERY CALL-BACK IS BEHIND US, AND THAT IS THE WHOLE POINT
                ; OF THE SLICE (D-KILLLOCAL, spec-diskcode-eviction.md §6.6ay).
                ; What stood here was `ld a,DISKOP_SEL_KILL / ld ix,dirverb_op /
                ; call calbak` -- a call-back into MAIN that marshalled on to
                ; `sub.rom`'s dirverb tenant, so one KILL crossed
                ; main -> disk.rom -> main -> sub.rom and back. The loop below is
                ; the tenant's body verbatim, running on THIS ROM's fat_mount /
                ; fat_delete, and nothing in the window has to survive a
                ; call-back because no call-back happens in it.
                ; 🔴 `FAT_DBUF`, NEVER `FSECTOR_BUF` -- the body spells the
                ; per-ROM name and this ROM bound it to $E2A0 (§6.6c; one
                ; address since D-BUFMERGE, the name stays). The
                ; sub-ROM tenant's own callees are identical source.
                call    fat_mount       ; LOCAL primitive body
                ld      a,2             ; 2 = mount / DSKIO error -> disk_error
                jr      c,hkk_done
                ; 🔴 D-KILLOPEN (2026-09-28): AN OPEN FILE IS REFUSED, BEFORE
                ; ANYTHING IS DELETED. This loop used to delete whatever matched,
                ; open or not -- `OPEN "K1.TXT" FOR OUTPUT AS #1 : KILL "K1.TXT"`
                ; freed the chain under a live channel. Measured on the CF-3300
                ; (scratchpad/killopen_probe.py): 64 `File still open` for a file
                ; open FOR OUTPUT or FOR INPUT, on the live channel or a saved one,
                ; and through a wildcard; a CLOSEd file deletes. The refusal comes
                ; back the way hkn_exists's 65 does: DISKOP_ERR + status 2, which
                ; main's kill_status already sends to disk_error -- no main byte.
                call    hkk_open_check  ; CF=1: an open file matches the pattern
                jr      c,hkn_open      ; File still open: DISKOP_ERR := 64, status 2, the
                                        ; shared tail -- NAME's arm, the same 9 bytes this
                                        ; carried (D-DISKREREAD 2026-11-26, 9 B)
                ld      c,0             ; C = deleted-any flag
hkk_loop:
                push    bc
                call    fat_delete      ; frees + $E5-marks the FIRST match
                pop     bc
                jr      c,hkk_end       ; no (further) match -> stop
                ld      c,1             ; deleted at least one
                jr      hkk_loop
hkk_end:
                ld      a,c
hkk_done:
                ; 🔴 DISKOP_STATUS IS WRITTEN ONCE, HERE. It is a SHARED
                ; channel -- main's FAT primitives marshal their own disposition
                ; through the same cell -- and pre-setting it is what made NAME
                ; answer `Syntax error` at an empty drive (see hkn_done). The
                ; pre-set that used to stand above was harmless only because the
                ; tenant it guarded overwrote the cell itself.
                jp      hk_claim_status ; 0 none matched / 1 deleted / 2 mount
                                        ; or refused (DISKOP_ERR says which)

; --- hkk_open_check: does an OPEN disk file match DISK_FCB_NAME? (D-KILLOPEN) --
; The RULE is the CF-3300 oracle's, measured (scratchpad/killopen_probe.py); the
; walk below is our own design over our own channel table.
; Walks FCH_MODES[1..15]. For each DISK channel (mode 1..LPT_MODE-1) it matches
; the name in the channel's FCB row (hoc_dout) against the KILL pattern with the
; very name_cmp fat_find uses, so `KILL "K*.TXT"` sees an open K1.TXT as the
; reference does.
; 🧭 S10.C (2026-10-15): EVERY disk mode has its FCB in the row now -- OUTPUT /
; APPEND since S10.B, RANDOM since S10.D, INPUT since S10.C -- so the arm that
; took an INPUT channel's directory position from main's engine globals (live) or
; its saved row (main_FWR_DIRSEC / main_FWR_DIROFF), re-read that directory
; sector and matched the entry is gone, and with it a sector read per open INPUT
; channel on every KILL / NAME / COPY / OPEN.
;   out: CF=1 a match is open; CF=0 none. Clobbers everything.
hkk_open_check:
                ld      b,1                 ; B = channel
hoc_loop:
                ld      a,b
                cp      16                  ; FCH_MODES is [0..FCH_CEIL=15]
                ret     nc                  ; walked them all: none open (CF=0)
                ld      hl,FCH_MODES
                ld      e,b
                ld      d,0
                add     hl,de
                ld      a,(hl)
                or      a
                jr      z,hoc_next          ; closed
                cp      LPT_MODE
                jp      c,hoc_dout          ; a disk channel: its name is in its FCB
                                            ; (mode 2 is never stored, D-MODE2DEAD)
hoc_next:                                   ; a device or cassette channel
                inc     b
                jr      hoc_loop

; ⚠️ STILL CONFLATED, AND IT TRAVELS WITH THE CODE: a DSKIO failure INSIDE
; fat_delete, after the mount and before anything was deleted, returns C = 0 and
; reads as "nothing matched" (ERR 53) rather than as an I-O error. That is
; fat_delete's own contract -- `Cy = 1 not found / mount / I-O error`, one bit for
; three outcomes -- and separating it is a primitive-layer change D-DSKMSG
; declined to make. The mount is split out here exactly as it was in the tenant,
; which is what keeps a disk-offline KILL answering `Disk offline`.

; --- hk_name: NAME's body, RUNNING IN THE DISK ROM (MSX2 TH hook) -----------
; D-DISKVERB2, spec-diskbasic-hook-rearchitecture.md phase 3. The second verb to
; follow its hook, on the pattern hk_kill established.
;
;   in   FN_RESUME = the statement cursor, just past the NAME token
;   out  DISKOP_STATUS, and main's ex_name decodes it; CF = 1 (claimed)
;
; 🎯 NAME'S THREE OUTCOMES ARE ALREADY KILL'S THREE, so main shares kill_status
; rather than keeping NAME's own tails: nm_notfound was `jp df_notfound` (ERR 53)
; = status 0, nm_fail/nm_fail2 were `jp disk_error` = status 2, and success was
; `jp exec_stmt` = status 1. Only the missing-`AS` syntax error needs a fourth
; value, and main tests for it before handing over to the shared decode.
; ⚠️ THE TENANT'S OWN STATUS HAS THE OPPOSITE POLARITY and is translated here,
; not passed through: for NAME_STAMP the dirverb tenant answers 0 = OK, nonzero =
; I/O error, where kill_status reads 0 as `File not found`. Forwarding it raw
; would turn every successful rename into ERR 53.
;
; 🎯 THE `AS` KEYWORD IS READ STRAIGHT OUT OF THE PROGRAM TEXT. That is RAM, and
; RAM is mapped throughout -- so two characters do not need a call-back apiece,
; and skip_spaces/upcase stay where they are.
hk_name:
                call    hk_fname2           ; 💰 D-DISKCARVE7: the name -> DISK_FCB_NAME; may
                                            ; RAISE (ERR 13) and NOT return -- see the ABI
                ld      hl,(FN_RESUME)
                call    skip_sp             ; skip blanks before `AS` (carve 34)
hkn_a:
                call    hkn_up
                cp      'A'
                jr      nz,hkn_noas
                inc     hl
                ld      a,(hl)
                call    hkn_up
                cp      'S'
                jr      nz,hkn_noas
                inc     hl
                ld      (FN_RESUME),hl      ; the NEW name's text starts here
                ; 🔴 D-COPYNAMEOPEN (2026-09-28): an OPEN old file is 64, and it
                ; outranks the new name's 65 -- measured on the CF-3300
                ; (scratchpad/killopen_probe.py n_oldin/n_oldout/n_both).
                ; 🏗️ D-ROMSIZE step 1 (2026-10-19): MOUNT AND FIND ARE THIS ROM'S
                ; OWN NOW, as KILL's and COPY's already were. This used to mount
                ; locally for the open check and then mount AGAIN and find through
                ; call-backs into main's fat_mount / fat_find shims (the sub-ROM's
                ; FAT tenant), because the stamp below read the entry's position
                ; from MAIN's FWR_DIRSEC / DIROFF. A failed local mount went on to
                ; main's, which then reported it; now the one mount reports it --
                ; the same DSKIO read, the same DISKOP_ERR mapping (one engine
                ; body, basic/fat-prim-body.inc), one attempt instead of two. The
                ; open check reads only the channel rows' FCB names (hoc_dout), so
                ; its order against the find no longer matters either.
                call    fat_mount
                jr      c,hkn_io            ; no disk / bad BPB
                call    hkk_open_check      ; CF=1: the OLD name is open
                jr      c,hkn_open
                call    ff_name            ; locate the OLD file: FWR_DIRSEC / DIROFF
                jr      c,hkn_nf
                ld      hl,(FN_RESUME)
                call    fname_ev            ; NEW name -> STRSCR, FN_RESUME moved
                ld      hl,STRSCR + 1
                ld      ix,parse_disk_fcb
                call    calbak              ; new -> DISK_FCB_NAME
                ; 🔴 D-NAMEEXIST (2026-09-28): A NEW NAME THAT ALREADY EXISTS IS
                ; `File already exists` (65), AND IT USED TO BE STAMPED OVER: the
                ; directory then held TWO entries of that name. Measured on the
                ; CF-3300 (scratchpad/t6enum_b8_zb.out, `NAME "N1.TXT" AS
                ; "N2.TXT"` with both present). Look the new name up with the same
                ; fat_find -- but it records its hit in FWR_DIRSEC/DIROFF, which is
                ; exactly where hkn_stamp is about to write, so the OLD file's
                ; location is kept across it and put back. (This ROM's own cells
                ; since 2026-10-19, BDOS_DIRSEC / BDOS_DIROFF; the two call-backs
                ; above touch neither, as COPY_CLUS's note in the ABI says of
                ; COPY's.)
                call    new_taken           ; CF clear = the new name is taken
                jr      nc,hkn_exists
                call    hkn_stamp           ; LOCAL now -- no call-back, no sub-ROM
                jr      c,hkn_io
                ld      a,1                 ; renamed
                jr      hkn_done
hkn_open:                                   ; (KILL's and COPY's open arms too: D-DISKREREAD)
                ld      a,64                ; `File still open` (D-COPYNAMEOPEN)
                jr      hkn_err
hkn_exists:
                ld      a,65                ; `File already exists`, raised by
hkn_err:
                ld      (DISKOP_ERR),a      ; disk_error from status 2 (hkn_io) --
                jr      hkn_io              ; NOT a fall into hkn_nf's status 0
hkn_nf:
                xor     a                   ; 0 = old file not found -> ERR 53
                jr      hkn_done
hkn_io:
                ld      a,2                 ; mount / I-O -> the mapped disk code
                jr      hkn_done
hkn_noas:
                ; 🔴 D-NOASTO (2026-09-29): with no `AS` the CF-3300 looks the OLD
                ; name up FIRST -- `NAME "NOPE.TXT"` is 53 there, `NAME "HI.TXT"` 2
                ; (scratchpad/noasto_run.out). This read 2 for both.
                ; D-NOASTOFOLD: the same lookup COPY's no-TO path makes -- one
                ; helper, LOCAL FAT. (Written when the rename path still found
                ; through main's FAT for its entry-location record; since
                ; 2026-10-19 both paths are this ROM's.)
                ld      hl,DISK_FCB_NAME
                call    hk_srcfind
                jr      c,hkn_done          ; A = 2 (I/O) or 0 (missing -> 53)
hkn_syn:
                ld      a,4                 ; no `AS` -> Syntax error
hkn_done:
                ; 🔴 DISKOP_STATUS IS WRITTEN ONCE, HERE, AFTER EVERY CALL-BACK.
                ; The first cut PRE-SET it to 2 and let the later paths overwrite
                ; it -- which could not work, because main's fat_mount and fat_find
                ; were `ld a,DISKOP_SEL_* / jr fatprim_bounce` shims that marshalled
                ; their own disposition THROUGH THIS SAME CELL (basic/fat.asm; both
                ; gone 2026-10-19 -- the rule stands for the call-backs left).
                ; So a failed mount wrote the FAT primitive's status over mine and
                ; main decoded whatever that happened to be: measured, `NAME "A.BAS"
                ; AS "B.BAS"` at an EMPTY DRIVE answered `Syntax error` instead of
                ; `Disk offline`, because the cell held 4. The cell is a SHARED
                ; channel, not ours; the only safe time to write it is last.
                ; D-ERRKEEP: through the tail KILL shares (header below).
                ; FALLS THROUGH into it.

; --- hk_claim_status: the claimed-hook tail KILL and NAME share (D-ERRKEEP) -----
; A = the verb's DISKOP_STATUS. Stores it, returns CF=1 (claimed). Our own design;
; its history was measured (scratchpad/killopen_probe.py, n_open and the k_* rows).
; 🗑️ S10.E (2026-10-19): IT NO LONGER MARKS "NO CHANNEL LIVE" ON A PENDING CODE.
; That arm (D-ERRKEEP / D-ERRKEEP2, 2026-09-28) existed for MAIN'S RESTORE: with
; a file OPEN, chan_gate reloaded the live channel after the crossing
; (chan_restore_st -> fch_load_ctx -> the sub-ROM's t_fch_load), and that
; crossing's fatprim dispatch zeroed DISKOP_ERR, so a status-2 code reached main
; as 0 and fell to load_error's print-and-continue -- the handler saw `ERR 0
; ERL 0` where the CF-3300 traps 64 / 65. FCH_ACTIVE = 0 made chan_restore
; return at its `ret z`. The
; restore existed only to bring back main's LOF / LOC mirror, which S10.E
; retired (LOF / LOC read the FCB: disk/fat.asm din_lofloc), and chan_gate's
; claimed path now reads DISKOP_STATUS and returns -- nothing runs between this
; tail and main's decode, so the pending code survives with the channel left
; live, as it is on the CF-3300.
hk_claim_status:
                jp      hka_st              ; D-DISKROOM: DISKOP_STATUS := A, CF=1 -- hka_st's tail


; --- hk_copy: COPY's body, BOTH HALVES, in the disk ROM (MSX2 TH hook) ------
; D-DISKVERB3, spec-diskbasic-hook-rearchitecture.md phase 3, third verb.
;
; 🎯 THIS ONE PAYS IN THE SCARCER CURRENCY. COPY's parse half was `copy_parse` in
; the page-0 LOW REGION, which stands at 0 B free -- and `fname_fcb`, its only
; caller's only helper, goes with it now that KILL and NAME no longer route
; through either. Both leave, so the saving is low-region bytes, not page-1 ones.
;
;   in   FN_RESUME = the statement cursor, just past the COPY token
;   out  DISKOP_STATUS, decoded by main's ex_copy; CF = 1 (claimed)
;
; Dispositions are the dirverb tenant's own 0/1/2 plus 3, and main maps 3 to ERR 5
; before handing 0/1/2 to the shared kill_status. A MISSING `TO` folds into 3
; because that is what the reference answers -- ERR 5, measured on the CF-3300,
; the one-argument form PARSING and being refused rather than ERR 2 (D-COPY).
;
; ⚠️ THE ONE 8.3 BUFFER IS REUSED, which is why the source is stashed first:
; DISK_FCB_NAME moves into COPY_SRC so the destination can overwrite it. Getting
; that order wrong copies a file onto itself.
;
; 🧭 D-COPYLOCAL (disk/docs/spec-diskcode-eviction.md §6.6az): BOTH HALVES ARE
; LOCAL NOW. This used to make TWO `ld ix,dirverb_op / call calbak` crossings --
; one to stash eleven bytes, one to run the copy -- each going back into MAIN so
; main could marshal on to `sub.rom`'s tenant. The stash is an `ldir` here and
; the body is `hkc_body` below, so `COPY` reaches no other ROM at all.
hk_copy:
                call    hk_fname2           ; 💰 D-DISKCARVE7: the name -> DISK_FCB_NAME; may
                                            ; RAISE (ERR 13) and NOT return -- see the ABI
                ld      hl,DISK_FCB_NAME    ; stash the SOURCE, LOCAL now
                ld      de,COPY_SRC         ; a main sysvar owned by COPY alone,
                call    ldir_11
                ld      hl,(FN_RESUME)      ; the ldir clobbered HL
                call    skip_sp             ; skip blanks before `TO` (carve 34)
hkc_to:
                cp      TO_TOKEN
                jr      nz,hkc_noto         ; no `TO` -> ERR 5 (measured), not ERR 2
                inc     hl
                call    fname_fcb           ; DESTINATION name -> DISK_FCB_NAME
                ; 🔴 D-COPYNAMEOPEN (2026-09-28): an OPEN source OR destination is
                ; 64 on the CF-3300 (scratchpad/killopen_probe.py c_srcin, c_srcout,
                ; c_dstout, c_wild) and was copied here. Both names are parsed, so
                ; no syntax outcome moves. DISK_FCB_NAME holds the destination and
                ; COPY_SRC the source; hkk_open_check matches DISK_FCB_NAME, so the
                ; source is swapped in for its turn and swapped straight back.
                call    fat_mount           ; LOCAL, for hkk_open_check's reads;
                jr      c,hkc_go            ; on failure hkc_body reports it
                call    hkk_open_check      ; the destination
                jr      c,hkn_open          ; `File still open`: NAME's arm (DISKOP_ERR := 64,
                call    hkc_swap            ; status 2, hk_claim_status) -- hkc_open was the
                call    hkk_open_check      ; same 9 bytes (D-DISKREREAD 2026-11-26, 9 B)
                push    af                  ; (the source)
                call    hkc_swap            ; DISK_FCB_NAME = the destination again
                pop     af
                jr      c,hkn_open
hkc_go:
                call    hkc_body            ; LOCAL: A = 0/1/2/3
                jr      hkc_done
hkc_noto:
                ; 🔴 D-NOASTO (2026-09-29): ...but only if the SOURCE exists. The
                ; CF-3300 looks it up first: `COPY "NOPE.TXT"` is 53 there,
                ; `COPY "HI.TXT"` 5 (scratchpad/noasto_run.out). This read 5 for both.
                ld      hl,COPY_SRC
                call    hk_srcfind          ; D-NOASTOFOLD: shared with NAME's no-AS
                jr      c,hkc_done          ; A = 2 (I/O) or 0 (missing -> 53)
hkc_ref:
                ld      a,3                 ; refused -> ERR 5
hkc_done:
                ; DISKOP_STATUS written ONCE, LAST -- see hk_name for why that is
                ; not a style choice: the cell is a channel main's FAT primitives
                ; marshal through too. ⚠️ hkc_body RETURNS its disposition in A
                ; rather than storing it, for the same reason: the tenant it
                ; replaces wrote the cell itself, and two writers on one channel
                ; is what §6.6aw's empty-drive `Syntax error` was.
                jr      hk_claim_status     ; D-ERRKEEP: stores it, CF = 1

; hkc_swap -- exchange the 11-byte names in DISK_FCB_NAME and COPY_SRC
; (D-COPYNAMEOPEN). Clobbers A, BC, DE, HL.
hkc_swap:
                ld      hl,DISK_FCB_NAME
                ld      de,COPY_SRC
                ld      b,11
hks_lp:
                ld      a,(de)
                ld      c,(hl)
                ld      (hl),a
                ld      a,c
                ld      (de),a
                inc     hl
                inc     de
                djnz    hks_lp
                ret

; hk_srcfind -- D-NOASTOFOLD (2026-09-29): mount, then look the 8.3 name at HL up,
; LOCALLY. NAME's no-AS and COPY's no-TO paths both need exactly this (the
; CF-3300 looks the source up before refusing the missing connective,
; scratchpad/noasto_run.out), and D-NOASTO had written it twice -- the
; duplicate Joost's 2026-09-29 rule forbids. Our own code.
;   out: CF=0 found; CF=1 with A = 2 (mount failed -> I/O) or 0 (missing -> 53)
hk_srcfind:
                ei                          ; D-FWDWIN: as hk_ochk (disk/driver.asm) -- the
                                            ; mount's reads masked otherwise (FW5_stack.out)
                push    hl
                call    fat_mount
                pop     hl
                ld      a,2                 ; (flags kept) mount failure -> I/O
                ret     c
                call    fat_find
                ld      a,0                 ; (flags kept) 0 = not found -> 53
                ret

; --- hk_auto: AUTOEXEC.BAS's silent presence probe (H_FOPEN, FOPEN_SEL_AUTO) ---
; D-ROMSIZE step 1 (2026-10-19). Main's cold start (basic/cload.asm autoexec_run)
; asks here, after the banner and before the REPL, whether to RUN AUTOEXEC.BAS;
; it then loads it the way RUN "file" does (disk_prog_load -> hk_dpload) and
; runs it. The probe was main's -- name staged from a main island, mount and
; find through main's fat_mount / fat_find shims into the sub-ROM's FAT tenant
; -- and is this ROM's own FAT now, where the CF-3300 keeps it. The contract is
; disk/docs/autoexec-bas-spec.md §2: absent, empty (all four size bytes 0) or no
; disk / a bad boot sector are SILENT; only a present, non-empty file runs.
; DISK_FCB_NAME is left holding the name, which is what disk_prog_load opens.
; No `ei`: the old probe ran DI too (a sub-ROM tenant behind CALSLT), and
; cg_back gives main its EI back on the way out.
;   out  DISKOP_STATUS = 0 run it / nonzero skip; CF = 1 (claimed)
hk_auto:
                ld      hl, (BASENT_REQ)    ; D-BASENTRUN: a restart a running program
                ld      de, 'B' * 256 + 'B' ; asked for (basent_body) -- the CF-3300
                or      a                   ; does not run AUTOEXEC.BAS again; the
                sbc     hl, de              ; restart is over, so the mark goes
                ld      a, 1                ; (flags kept) 1 = skip, silently
                jr      nz, hka_probe
                ld      (BASENT_REQ), hl    ; HL = 0
                jr      hka_done
hka_probe:
                call    auto_find           ; CF: no disk / bad BPB / not found
                ld      a,1                 ; (flags kept) 1 = skip, silently
                jr      c,hka_done
                ld      hl,FAT_FILESIZE
                ld      a,(hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)
                inc     hl
                or      (hl)                ; A = 0 iff the file is empty
                sub     1                   ; CF iff empty
                sbc     a,a                 ; $FF = empty: skip; 0 = run it
hka_done:
                jp      hk_claim_status
; auto_find -- is there an AUTOEXEC.BAS? CF = 1: no disk, a bad BPB, or not found;
; CF = 0: found (FAT_FILESIZE holds its size). Shared with boot_date (fat.asm),
; which asks no date when the file is there, whatever its size (D-BOOTDATE).
auto_find:
                ld      hl,hka_name
                ld      de,DISK_FCB_NAME
                push    de
                call    ldir_11
                pop     hl
                jr      hk_srcfind
; The upcased 11-byte 8.3 dirent name, no padding needed: "AUTOEXEC" (8) + "BAS"
; (3) -- disk/docs/autoexec-bas-spec.md §2. (Main's island copy, autoexec_name,
; went with the probe.)
hka_name:       db      "AUTOEXECBAS"



; --- hk_files: FILES and LFILES, in the disk ROM (MSX2 TH hook) -------------
; D-DISKVERB4, spec-diskbasic-hook-rearchitecture.md phase 3, the last of the
; FAT-light four.
;
; 🎯 ONE HOOK, TWO VERBS, AND THE TOKEN SAYS WHICH. H_FILE is the only cell the
; reference gives this pair, so the selector cannot come from the hook. It is
; read from the PROGRAM TEXT instead: FN_RESUME points just past the verb's own
; token, so the byte before it is FILES_TOKEN or LFILES_TOKEN. Same trick as
; NAME's `AS` and COPY's `TO` -- the text is RAM and mapped throughout, so no
; call-back and no new sysvar.
;
;   in   FN_RESUME = the statement cursor, just past the FILES/LFILES token
;   out  DISKOP_STATUS 0/1/2, decoded by the shared kill_status; CF = 1 (claimed)
;
; ⚠️ THE SELECTOR RIDES THE STACK ACROSS THE EVALUATOR, and that is D-FNEXPR2's
; finding carried over rather than re-learned: the filespec is a string
; EXPRESSION, and `FILES INPUT$(5,#1)` reaches the drive through fatprim_bounce,
; whose FIRST instruction is `ld (DISKOP_OP),a`. Writing the selector before the
; evaluator hands the dirverb tenant a FAT-primitive selector. An abort inside
; the evaluator cannot leak the pushed word: raise_error resets SP from SAVSTK.
hk_files:
                ei                          ; D-HOOKDI: the hook arrives DI (CALSLT)
                ld      hl,(FN_RESUME)
                dec     hl
                ld      a,(hl)              ; the verb's own token
                inc     hl
                cp      LFILES_TOKEN
                ld      a,DISKOP_SEL_FILES
                jr      nz,hkf_sel
                ld      a,DISKOP_SEL_LFILES
hkf_sel:
                push    af                  ; the selector, ACROSS the evaluator
                xor     a
                ld      (FILES_HASPAT),a    ; no pattern yet
                ld      (DK_FCB_DRV),a      ; D-DSKIB: and the default drive (A:) until one is typed
                call    skip_sp             ; skip blanks before the filespec (carve 34)
hkf_chk:
                or      a
                jr      z,hkf_run           ; end of statement -> whole directory
                cp      COLON
                jr      z,hkf_run           ; `FILES:...` -> ditto
                call    fname_ev            ; an EXPRESSION; may RAISE (ERR 13)
                call    hkf_pat             ; D-FILESBARE: the name, or none (the fill)
hkf_run:
                pop     af                  ; the selector, asserted only now
                ld      (DISKOP_OP),a       ; op AND sink AND layout, all three
                call    hkf_body            ; LOCAL: writes DISKOP_STATUS itself
                scf                         ; CF=1: the hook is claimed
                ret

; --- hkf_body: the root-directory walk and the 8.3 emit, IN THIS ROM --------
; D-FILESLOCAL (disk/docs/spec-diskcode-eviction.md §6.6ba). `tnt_files` and its
; four helpers, moved verbatim except for the buffer name. This was
; `ld ix,dirverb_op / call calbak` -- back into MAIN so main could `subrom_call`
; on to `sub.rom`, the last of step 13's four round trips.
;
;   in   DISKOP_OP = DISKOP_SEL_FILES | DISKOP_SEL_LFILES (op AND sink AND
;        layout); FILES_HASPAT = 1 if DISK_FCB_NAME holds an 8.3 wildcard
;   out  DISKOP_STATUS = 0 nothing matched / 1 ok / 2 mount or DSKIO error
;
; 🔴 THIS BODY USES `DISKOP_STATUS` AS WORKING STATE, AND THAT IS NOT THE
; §6.6aw VIOLATION IT LOOKS LIKE. The once-and-last rule exists because main's
; FAT primitives are shims that marshal their own disposition through the same
; cell, so a SECOND writer can land between yours. No call-back happens after
; `hkf_run`, so no shim runs, and the cell is exclusively ours for the walk --
; exactly as it was for the tenant. `df_end` READS it back (it must know whether
; anything was emitted), which is why the seed and the df_do_emit store stay.
;
; 🔴 `FAT_DBUF`, NOT `FSECTOR_BUF`. The tenant spelled the per-ROM name in
; CODE at two sites (the sector read and df_entptr's base); in THIS ROM that is
; main's buffer at $E5C0 rather than ours at $E2A0, and it would have assembled
; clean and walked a directory nobody read (§6.6c, §6.6ao). Since D-BUFMERGE
; (2026-09-24) the two names are one address; the neutral one stays.
;
; ⚠️ INTERRUPTS. A hook handler is entered with them OFF and nothing here turns
; them back on: this ROM's `read_sector` is a plain local `call dskio`, where the
; sub-ROM tenant's crossed by CALSLT. Measured either side of the move rather
; than argued -- scratchpad/filesei_probe.py, and §6.6ba carries the numbers.
; Clean-room: our own code, relocated; see basic/files.asm's header for the
; FILES/LFILES provenance and the CF-3300 oracle citations (R-LF1/R-LF2/R-LF7,
; docs/spec-basic-lfiles.md, docs/lptverb-msx1-characterization.md §4.2).
hkf_body:
                ; Seed the matched-any flag at "nothing matched", ALWAYS: the
                ; walk sets it to 1 at df_do_emit the first time it emits, so a
                ; listing that emits nothing raises ERR 53 whether or not the
                ; caller gave a filespec. R-LF7 measured that the CF-3300 answers
                ; `File not found` to a bare FILES over an empty volume too --
                ; the same disposition, not a different one.
                xor     a
                ld      (DISKOP_STATUS),a
                call    fat_mount           ; LOCAL primitive body
                jr      c,hkf_io
                call    fat_dir_start       ; DIRSEC / DIRREM: the whole root
df_secloop:
                call    dirrem_z
                jr      z,df_end            ; scanned every root sector
                ld      de,(FAT_DIRSEC)
                call    rd_dbuf         ; LOCAL primitive body
                jr      c,hkf_io            ; FDC / DSKIO error
                xor     a
                ld      (FILES_ENTIDX),a    ; entry 0..15 within this sector
df_entloop:
                call    df_entptr           ; HL -> current 32-byte dir entry
                ld      a,(hl)
                or      a
                jr      z,df_end            ; $00 = first free slot -> end of dir
                cp      $E5
                jr      z,df_nextent        ; deleted entry
                push    hl
                ld      de,11
                add     hl,de
                ld      a,(hl)              ; attribute byte (+11)
                pop     hl
                and     $18                 ; volume-label | sub-dir -> skip
                jr      nz,df_nextent
                ld      a,(FILES_HASPAT)
                or      a
                jr      z,df_do_emit        ; bare listing -> everything
                push    hl                  ; name_cmp advances HL by 11
                ld      de,DISK_FCB_NAME
                call    name_cmp            ; LOCAL; DE=pattern, HL=entry
                pop     hl                  ; restore entry ptr (flags preserved)
                jr      nz,df_nextent       ; no match -> skip
df_do_emit:
                call    df_first            ; D-FILESNL: the first entry starts a fresh row
                call    df_emit
df_nextent:
                ld      a,(FILES_ENTIDX)
                inc     a
                ld      (FILES_ENTIDX),a
                cp      16                  ; 512 / 32 entries per sector
                jr      c,df_entloop
                call    dir_nextsec         ; 💰 D-DISKCARVE6: FAT_DIRSEC += 1, FAT_DIRREM -= 1
                jr      df_secloop
df_end:
                ; D-DFEND: the SCREEN form leaves the cursor where the last entry
                ; ended (measured three ways on the CF-3300); the PRINTER form
                ; puts the head column back to 0, but ONLY when something was
                ; emitted -- on a no-match both machines print `File not found`,
                ; send nothing, and leave the head parked for R-LP16 to flush.
                ; Zeroing unconditionally fixes one arm and breaks the other.
                call    op_lfiles
                ret     nz                  ; SCREEN: leave the cursor put
                ld      a,(DISKOP_STATUS)
                dec     a                   ; 1 = at least one entry emitted
                ret     nz                  ; nothing emitted -> head never moved
                ld      (LPTPOS),a          ; A is 0 here
                ret
hkf_io:
                ld      a,2
                ld      (DISKOP_STATUS),a   ; -> main's kill_status does load_error
                ret


; df_emit -- print one directory entry's 8.3 name field. HL -> the 32-byte
; directory entry (name at +0..10). Clobbers regs; the entry index lives in RAM
; so the caller re-derives the pointer.
; R-LF1: the printer form has no separator and no wrap -- the wrap decision reads
; CSRX and LINLEN, the SCREEN cursor and the SCREEN width, so on the printer it
; would be reading an unrelated device.
df_emit:
                call    op_lfiles
                jr      z,de_field
                ld      a,(CSRX)
                dec     a                   ; 0-based column (0 = line start): Z = first
                ; D-DFEND: the space is a TRAILING one shared with the printer,
                ; not a separator emitted BEFORE the next field -- the two are
                ; indistinguishable row by row and differ where the cursor comes
                ; to REST, which is the one place the reference could be read.
                ; The `cp 13` is DELIBERATELY UNCHANGED: requiring field+space to
                ; fit is what keeps the trailing space off the last column.
                ; D-DISKREREAD (2026-11-26): the room test is the tail's own (df_room
                ; below: `ret nc` = room, else it falls into df_crlf and wraps), 12 B
                call    nz,df_room          ; mid-line: wrap unless 13 columns are left
de_field:
                ld      b,8                 ; 8 name chars, raw off the disk
de_name:                                    ; D-DISKREREAD (2026-11-26): no push / pop round
                ld      a,(hl)              ; df_out here or in de_ext -- it keeps HL / DE / BC
                call    df_out              ; itself (below), 8 B
                inc     hl
                djnz    de_name
                push    hl                  ; '.' between name and extension -- or a
                ld      a,(hl)              ; SPACE when the extension is blank: D-FILESNOEXT
                inc     hl                  ; (2026-10-27), the CF-3300 lists `Q` with no
                or      (hl)                ; dot, the 12-column field kept
                inc     hl                  ; (scratchpad/filesnoext_run.out). Directory
                or      (hl)                ; bytes are printable, so the OR of the three is
                cp      ' '                 ; exactly ' ' only when all three are spaces
                ld      a,'.'
                jr      nz,de_dot
                ld      a,' '
de_dot:
                call    df_out
                pop     hl
                ld      b,3                 ; 3 extension chars
de_ext:
                ld      a,(hl)
                call    df_out
                inc     hl
                djnz    de_ext
                ; R-LF2, widened by D-DFEND: the trailing space is emitted on
                ; BOTH sinks; only the CR/LF after it stays printer-only.
                ld      a,' '
                call    df_out
                call    op_lfiles
                jr      z,df_crlf           ; the printer: CR/LF after every entry
                ; ✅ D-FILESNL (2026-10-09): THE SCREEN FORM ENDS A FULL ROW AT
                ; ONCE. After a row with no room for another field (3 at WIDTH
                ; 40, 2 at WIDTH 37) the CF-3300 starts a new line, so a PRINT
                ; after FILES begins on its own row; zerobas wrapped only before
                ; the NEXT field, so the cursor stayed at the end of a full last
                ; row (scratchpad/filesnl_run.out). A partly filled row still
                ; leaves the cursor on it, on both. The same `cp 13` as the
                ; pre-check above: field + its trailing space.
                ld      a,(CSRX)
                dec     a                   ; 0-based column
df_room:                                    ; (df_emit's pre-check calls here, D-DISKREREAD)
                ld      b,a
                ld      a,(LINLEN)
                sub     b                   ; columns left on this line
                cp      13                  ; need 12 (field) + 1 (its space)
                ret     nc                  ; room for another field: stay here
                ; fall through -> CR/LF

; df_crlf -- end the current line on the active sink. This is print_crlf's body;
; print_crlf itself is main PAGE 1, and the head/body fork it forced is exactly
; what kept FILES out of the Phase-2 eviction.
df_crlf:
                ld      a,13
                call    df_out
                ld      a,10
                ; fall through -> df_out (its `ret` is this routine's)

; df_out -- emit the byte in A to the active sink: CHPUT for FILES, LPTOUT for
; LFILES. Preserves HL/DE/BC exactly as main's `pchar` does, so it is a drop-in
; for the `call CHPUT` sites this code carried while it was resident.
; ⚠️ A PLAIN `call` REACHES BOTH. Page 0 holds the main ROM while BASIC runs --
; `calbak`'s own `jp CALSLT` ($001C) already depends on it. The MSX-DOS side of
; this ROM pages the BIOS in by hand because THERE page 0 is RAM; that is a
; different environment, not a precedent for this one.
df_out:
                push    hl
                push    de
                push    bc
                ld      c,a                 ; C survives the DISKOP_OP load
                call    op_lfiles
                ld      a,c
                jr      z,dfo_lpt
                call    CHPUT
                jr      dfo_done
dfo_lpt:
                call    LPTOUT
dfo_done:
                pop     bc
                pop     de
                pop     hl
                ret

; --- S10.D: two RANDOM record-engine helpers (disk/kernel.asm hk_dget), at the
; end of this corridor; frnd_locate (in the $77BB fill) calls both.
; (frl_alloc and frnd_next moved to disk/fat.asm's $46CB fill by D-RECSLOW: this
; region's 2 B of slack could not take the FAT scan's new calls.)

; fat_locate -- HL = a cluster -> HL = the absolute FAT sector holding its 12-bit entry;
; FAT_PARITY = cluster & 1, FAT_BYTEIDX = fatofs & $1FF (fatofs = cluster * 3/2, the
; Microsoft FAT spec's 12-bit packing). Clobbers A, DE. 💰 D-DISKCARVE (2026-11-07): the
; same 19 instructions stood inline in getalloc's scan, fat_have_free_cluster's loop and
; fat_write_fat_entry_hot (scratchpad/diskcarve_ngram.out); basic/fat-prim-body.inc keeps
; its own three copies. (The reason given then -- "the sub ROM assembles that body too" --
; no longer holds: only this ROM includes it, D-DISKREREAD 2026-11-26. Its copies could
; call fat_locate; not done yet.)
fat_locate:
                ld      a, l
                and     1
                ld      (FAT_PARITY), a
                ld      e, l
                ld      d, h
                srl     d
                rr      e                   ; DE = cluster >> 1
                add     hl, de              ; HL = fatofs
                ld      a, l
                ld      (FAT_BYTEIDX), a
                ld      a, h
                and     1
                ld      (FAT_BYTEIDX + 1), a    ; byteidx = fatofs & $1FF
                ld      a, h
                srl     a                   ; fatofs >> 9 = the FAT sector offset
                ld      e, a
                ld      d, 0
                ld      hl, (FAT_FATSTART)
                add     hl, de              ; HL = the absolute FAT sector
                ret
; hd_last / hd_end -- hdl_direct's tail (disk/runtime.asm), here for the room: the
; last partial sector through FAT_DBUF, then END into FREAD_OFF and the walk.
hd_last:        ld      hl,(FREAD_LEFT)
                ld      a,h
                or      l
                jr      z,hd_end
                push    de                  ; the partial sector: through the buffer
                call    fat_read_file_sector
                pop     de
                jr      c,hd_end
                ld      hl,FAT_DBUF
                ld      bc,(FREAD_LEFT)
                ldir
hd_end:         ld      (FREAD_OFF),de      ; END (the stream's cell is free now)
                ld      hl,(CLPTR)
                                            ; falls into hdl_walk
; hdl_walk -- D-LOADDIRECT's second half (hdl_direct, disk/runtime.asm, is the
; first): the stream's line rules applied to the file IN PLACE. HL = the line's
; start, FREAD_OFF = END. CLPTR ends where hdl_line's last store would have left
; it: at a line's start for an EOF inside its link or for the $0000 link, at END
; for any other EOF.
hdl_walk:
                ld      (CLPTR),hl          ; an EOF in the link: CLPTR = here
                ex      de,hl
                ld      hl,(FREAD_OFF)
                or      a
                sbc     hl,de
                ld      b,h
                ld      c,l                 ; BC = the bytes left
                ex      de,hl
                ld      e,(hl)
                inc     hl
                ld      d,(hl)              ; DE = the link (read before the test:
                ld      a,b                 ; past END it is never used)
                or      a
                jr      nz,hw_link
                ld      a,c
                cp      2
                jr      c,hw_ok             ; fewer than 2 left: EOF in the link
hw_link:        ld      a,d
                or      e
                jr      z,hw_ok             ; the $0000 link: the program's end
                ld      hl,(CLINK)
                ld      (CLINK),de
                ex      de,hl               ; HL = the link, DE = the last one
                ld      a,d
                or      e
                jr      z,hw_scan           ; the first line: scanned (D-LOADBASE)
                sbc     hl,de               ; (CF = 0) HL = header + body
                ld      a,h
                or      a
                jr      nz,hw_cmp
                ld      a,l
                cp      3
                jr      z,hw_scan           ; a body of $FFFF: scanned, as hdl_line's
                cp      4
                jr      c,hw_eof            ; header + body >= 64 KB: past END
hw_cmp:         ex      de,hl
                ld      h,b
                ld      l,c
                or      a
                sbc     hl,de
                jr      c,hw_eof            ; runs past END
                ld      hl,(CLPTR)
                add     hl,de
                jr      hdl_walk
hw_scan:        ld      hl,-4
                add     hl,bc
                jr      nc,hw_eof           ; fewer than 4 left: EOF in the header
                ld      hl,(CLPTR)
                ld      de,4
                add     hl,de               ; past the header
                ld      de,(FREAD_OFF)
                ld      c,0                 ; C = constant bytes still to pass
hw_sl:          or      a
                sbc     hl,de
                add     hl,de
                jr      z,hw_eof            ; END before its $00
                ld      a,c
                or      a
                ld      a,(hl)
                inc     hl
                jr      z,hw_tok
                dec     c                   ; inside a constant: any byte, $00 too
                jr      hw_sl
hw_tok:         or      a
                jr      z,hdl_walk          ; its $00: HL = the next line
                call    tok_payload
                ld      c,a
                jr      hw_sl
hw_eof:         ld      hl,(FREAD_OFF)
                ld      (CLPTR),hl          ; any other EOF: everything stored
hw_ok:          jp      hdl_ok
; rh_flush -- D-PUTCACHE: write the sector a PUT left in FAT_MBUF (RND_HSEC) and
; forget it. CF / A as fatprim_write_sector's (the tag is cleared either way: a
; failed write is reported once, not retried at every read after). Keeps BC, DE.
rh_flush:
                ld      hl, (RND_HSEC)
                ld      a, h
                or      l
                ret     z                   ; nothing held (CF = 0)
                ei                          ; the write's rotational wait with the ISR
                                            ; running, as the PUT that left it had: this
                                            ; runs inside whatever hook reads next, and a
                                            ; hook arrives through CALSLT masked -- a
                                            ; 352 ms masked gap in L5_stock (D-GCDI's
                                            ; rule; D-LOADDI / D-HOOKDI's reasoning).
                                            ; Only with a sector held: BDOS / DOS paths
                                            ; never hold one, and return above.
                push    bc
                push    de
                ex      de, hl              ; DE = the held sector
                ld      hl, 0
                ld      (RND_HSEC), hl
                call    fws_mbuf
                pop     de
                pop     bc
                ret
; 🔴 D-CALSLTP0 (2026-11-08): br6 -- CALSLT for a page-1 / page-2 target WITH PAGE 0
; LEFT THE CALLER'S RAM. The $001C stub used to enter the BIOS's own CALSLT with the
; BIOS mapped into page 0 (br_go), so the target ran under it: a DOS program's buffer
; below $4000 was the BIOS while the callee wrote it -- GETDPB's DPB went into ROM
; (scratchpad/getdpb_entry.out). The CF-3300 runs the callee with page 0 the
; program's RAM. Page 3 cannot switch itself, so the slot work stays the BIOS's, through
; BR_ENASLT (which maps the BIOS in for the switch only and puts page 0 back): save the
; caller's slot for page p, ENASLT the target, call it, ENASLT the caller's back. The
; save word rides the STACK (a callee that itself CALSLTs cannot clobber it). A page-0
; or page-3 target keeps the old path (the BIOS's CALSLT: page 3 holds this code).
; Interrupts: BR_ENASLT leaves them off, as the CF-3300's CALSLT returns.
; LDIR'd to BR6_RAM by install_dos_env; runs from page-3 RAM -- `jr` within, the
; BR_* / BC_* equates for every absolute reference to itself.
;   in: IYH = the slot, IX = the address, AF / BC / DE / HL for the target
br6_tmpl:
br_cals_tmpl:
                push    af
                ld      a, ixh
                and     $C0
                jr      z, bcs_old          ; page 0: the BIOS's CALSLT, as before
                cp      $C0
                jr      z, bcs_old          ; page 3: likewise (this code lives there)
                push    hl
                push    de
                push    bc
                ld      d, a                ; D = the page's address high byte ($40 / $80)
                rlca
                rlca
                ld      b, a                ; B = p (1 / 2)
                in      a, ($A8)
                call    BC_FIELD
                ld      c, a                ; C = the caller's primary slot for page p
                ld      hl, EXPTBL
                add     a, l
                ld      l, a
                bit     7, (hl)
                jr      z, bcs_have         ; not expanded: the primary is the slot
                ld      a, l
                add     a, SLTTBL - EXPTBL  ; SLTTBL[primary]
                ld      l, a
                ld      a, (hl)
                call    BC_FIELD            ; its subslot for page p
                rlca
                rlca
                or      c
                or      $80
                ld      c, a                ; C = $80 | sub << 2 | primary
bcs_have:
                ld      e, c                ; E = the caller's slot, D = the page
                ld      (BC_TMP), de
                ld      h, d
                ld      a, iyh
                call    BR_ENASLT           ; the target's slot into page p
                pop     bc
                pop     de
                pop     hl
                pop     af                  ; the caller's registers for the target
                ld      iy, (BC_TMP)        ; (IY is CALSLT's input only: free now)
                push    iy                  ; [the save word][the caller's return]
                call    BC_JPIX             ; the target -- page 0 the caller's RAM
                ex      (sp), hl            ; HL = the save word (H page, L slot);
                push    af                  ; the target's HL waits on the stack
                push    bc
                push    de
                ld      a, l
                call    BR_ENASLT           ; the caller's slot back into page p
                pop     de
                pop     bc
                pop     af
                pop     hl                  ; the target's answer, every register
                ret
bcs_old:
                pop     af
                push    hl
                ld      hl, $001C           ; CALSLT, through the BIOS (br_go)
                jp      BR_GO
bc_field_tmpl:                              ; A := (A >> 2B) AND 3 (B = p, kept)
                push    bc
bcf_lp:
                rrca
                rrca
                djnz    bcf_lp
                pop     bc
                and     3
                ret
bc_jpix_tmpl:
                jp      (ix)
bc_tmp_tmpl:    dw      0
br6_end:
; 🔴 D-DSKCHG (2026-11-08): dskchg_body -- DSKCHG ($4013, via dskchg in disk/fat.asm),
; the CF-3300's TIMED answer (probes/disk/dskchg.asm; scratchpad/dskchg_stock2.out):
; within ~2 s of the last sector command B = 1 "unchanged", A = 0, HL / DE kept; after
; it "unknown" -- dc_unk (disk/driver.asm) refreshes the DPB at HL through getdpb, B = 0.
; Cy = 0 unless the disk cannot be read. The SAFE scheme: never "unchanged" long after an
; access. DSK_STAMP is 16-bit (fdc_prep sets it): 8 bits would wrap after ~5 s and say
; "unchanged" after a swap. The cut-off is in SECONDS (100 jiffies = 2 s at our 50 Hz).
; 🔴 The first build (reverted) answered "unknown" WITHOUT the DPB and clobbered HL / DE:
; MSX-DOS calls DSKCHG while booting and no DOS program reached its anchor.
; 💰 D-DISKCARVE6 (2026-11-08): dir_nextsec -- the "next directory sector" step five
; directory walks carried verbatim (scratchpad/diskngram.py: 15 B x5 -- hkm_next,
; ff_skip, fdcr_entloop, df_nextent, dsm_skip). Each site follows it with an
; unconditional `jr`; inc / dec hl set no flags, so nothing read a flag from it.
; 🔴 D-RNDFCB (b) + (c) (2026-11-08): rdrnd_ex -- RDRND's other FCB side effects on the
; CF-3300 (probes/disk/rndfcb.asm, scratchpad/rndfcb_stock.out): +12 = the CP/M extent,
; RR >> 7 (record 199 -> 1), and +14 cleared (wrzer.asm: a record size of 128 read back
; 0). CR (+32 = r0 mod 128) is rdrnd_body's own. IX = the FCB copy. Here for room.
; 💰 D-DISKCARVE7 (2026-11-08): hk_fname2 -- the 17-byte prologue KILL, NAME and COPY
; carried verbatim (scratchpad/diskngram.py, 17 B x3): the file-name EXPRESSION at
; main's staged cursor -> STRSCR, then -> DISK_FCB_NAME (8.3, wildcards). Either calbak
; may RAISE and not return: main's error path resets SP, so this frame's return
; address is dropped with the statement, as the handler's own was.
hk_fname2:
                ld      hl,(FN_RESUME)      ; the cursor main staged for us
; fname_fcb -- a string EXPRESSION at HL -> STRSCR (may RAISE), then -> DISK_FCB_NAME
; (8.3, wildcards); fname_ev -- the expression only. 💰 D-DSKFMT carve 29 (2026-11-10):
; `ld ix,fname_expr / call calbak` stood at four sites, the pair with pdfcb_resume at two.
fname_fcb:
                call    fname_ev
                ld      ix,pdfcb_resume
                jp      calbak
; (fname_ev -- MOVED by D-DISKCARVE6 step 2 to the pad in front of $436C, disk/driver.asm.)
; (skip_sp -- MOVED by D-DISKCARVE6 step 3 to the pad in front of $47B2, disk/fat.asm.)
; seq_flush / seq_dirty -- the start of a random positioning.
; seq_dirty (k47b2_seek, RDBLK -- BASIC's own file reads come through here too): a
; READ-open file's iterator moves, so BDOS_SEQREC := $FFFF makes srd_pos re-position
; the next SEQREAD from the FCB (D-BDOSFCB B2); a WRITE-open one is left alone.
; seq_flush (rrnd_position: the BDOS's RDRND / WRRND only) also, for a WRITE-open file,
; writes the SEQWRITE records still buffered FIRST (D-BDOSSEQ, 2026-11-10): the random
; call reads its sector into that same buffer, and FCLOSE's flush then zero-padded
; over what it wrote (record 1 read back $00 here, $22 on the CF-3300 --
; scratchpad/bdosseq_run2.out). 🔴 NOT from k47b2_seek: BDOS_WRMODE can read 1 there
; while a BASIC output file is open, and flushing then broke BASIC's sequential files
; (the first cut: inpquote-, programs-, kwsweep ... red in the FULL gate,
; scratchpad/bisect_A_inpquote.out). Clobber A, HL; flushing, BC and DE too.
seq_dirty:
                ld      a, (BDOS_WRMODE)
                or      a
                ret     nz
sd_read:
                ld      hl, $FFFF
                ld      (BDOS_SEQREC), hl
                ret
; ===== D-DSKFMT: DSKFMT ($401C), the CF-3300's four formats (2026-11-10) =====
; dskfmt_body -- in A = the choice (1..4, CHOICE's menu), D = the drive (one drive
; here), HL / BC = a work area and its size; out Cy = 0, A = B = 0 (the CF-3300's
; answer, dskfmt_stock*.out); Cy = 1 with A = 12 (bad parameter), 14 (the work area
; is too small) or a DSKIO code. Every track of every side gets a WD2793 Write Track
; (IBM System 34 double density: 9 sectors of 512 B, data $40 -- the CF-3300's fill,
; measured, not the usual $E5: D-FMTFILL), built once in the
; work area and each ID field set per track; then the system area -- the boot
; sector (the BPB: the format's contract, measured per choice; and OUR loader),
; both FATs (the media ID, $FF, $FF) and an empty root -- zeroed in the work area
; and written by ONE DSKIO call. Data in other holes (read by address): fmt_sec
; ($4558), fmt_boot ($4EDE), fmt_tail ($5445). CLEAN-ROOM: the WD2793 data sheet's
; Write Track bytes, the MSX2 TH DSKFMT contract, the measured BPBs; no reference
; ROM byte, and the reference's own boot code is not copied (TODO D-DSKFMT).
FMT_BUF         equ     8                   ; work area: +0 the choice (0..3), +1..7 its row
FMT_SYS         equ     14 * 512            ; the largest system area (1 + 2 x 3 + 7)
FMT_NEED        equ     FMT_BUF + FMT_SYS   ; (more than the track image's 6782 B)
dskfmt_body:
                dec     a
                cp      4
                ld      e, a
                ld      a, 12
                ccf
                ret     c                   ; not a choice: bad parameter
                push    hl
                ld      hl, -FMT_NEED
                add     hl, bc              ; Cy: BC >= FMT_NEED
                pop     ix                  ; IX = the work area
                ld      a, 14
                ccf
                ret     c                   ; not enough memory
                ld      (ix+0), e
                ld      a, e
                add     a, a
                add     a, a
                add     a, a
                sub     e                   ; x 7
                ld      e, a
                ld      d, 0
                ld      hl, fmt_geo
                add     hl, de
                push    ix
                pop     de
                inc     de
                ld      bc, 7
                ldir                        ; +1..7 := the choice's row
                call    fmt_buf
                ex      de, hl              ; DE = the image
                ld      hl, fmt_head
                ld      c, 5
                call    fmt_rle             ; the index gap
                ld      b, 9
fb_sec:
                push    bc
                ld      hl, fmt_sec
                ld      c, 14
                call    fmt_rle             ; a sector
                pop     bc
                djnz    fb_sec
                ld      hl, fmt_tail
                ld      c, 3
                call    fmt_rle             ; fill to the index and past it
                ld      d, b                ; B = 0: track 0
ft_trk:
                ld      e, 0
ft_side:
                call    fmt_track
                jp      c, fmt_err
                ld      a, (ix+0)
                and     1                   ; two sides: choices 2 and 4
                inc     a
                inc     e
                cp      e
                jr      nz, ft_side
                inc     d
                ld      a, 40
                bit     1, (ix+0)           ; 80 tracks: choices 3 and 4
                jr      z, ft_40
                add     a, a
ft_40:
                cp      d
                jr      nz, ft_trk
                ; the system area: zeroed, the boot sector, both FATs' first bytes
                call    fmt_buf
                push    hl
                ld      d, h
                ld      e, l
                inc     de
                ld      (hl), b             ; B = 0
                ld      bc, FMT_SYS - 1
                ldir
                pop     hl
                push    hl
                ex      de, hl
                ld      hl, fmt_boot
                ld      bc, FMT_BOOTLEN
                ldir                        ; sector 0: the template
                pop     hl
                push    hl
                ld      bc, 8
                add     hl, bc
                ld      a, (ix+0)
                add     a, '1'
                ld      (hl), a             ; "MSX_0N", N = the choice
                pop     hl
                push    ix
                pop     de
                inc     de                  ; DE = the row
                ld      bc, fmt_off
fs_bpb:
                push    hl
                ld      a, (bc)
                add     a, l
                ld      l, a
                adc     a, h
                sub     l
                ld      h, a                ; HL = the sector + the field's offset
                ld      a, (de)
                ld      (hl), a
                pop     hl
                inc     de
                inc     bc
                ld      a, c
                cp      low (fmt_off + 7)
                jr      nz, fs_bpb
                inc     h
                inc     h                   ; sector 1: FAT 1
                ld      b, 2
fs_fat:
                ld      a, (ix+5)
                ld      (hl), a             ; the media ID
                inc     hl
                ld      (hl), $FF
                inc     hl
                ld      (hl), $FF
                dec     hl
                dec     hl
                ld      a, (ix+6)
                add     a, a
                add     a, h
                ld      h, a                ; + the FAT's size: FAT 2
                djnz    fs_fat
                call    fmt_buf
                ld      b, FMT_SYS / 512    ; 14 sectors on EVERY choice, as the CF-3300:
                                            ; it zeroes logical 0..13 (the largest system
                                            ; area) even where data starts at 9 or 12 --
                                            ; measured all four choices, from MSX-DOS and
                                            ; from BASIC (D-FMTFILL, scratchpad/fmtfill_ref.out)
                ld      c, (ix+5)           ; the media ID: DSKIO seats the sectors by it
                ld      de, 0
                xor     a
                scf
                jp      dskio               ; A = B = 0, Cy = 0 once written
; (fmt_err -- MOVED by D-DISKCARVE6 step 3 to the pad in front of $5FE5.)
; (fmt_buf -- MOVED by D-DISKCARVE6 to the pad in front of $501E.)
; fmt_track -- Write Track, track D / side E, from the image with each ID field set
; first. Out Cy = 1, A = a DSKIO code (0 write protected, 2 not ready, 10 a fault).
fmt_track:
                push    de
                call    fdc_prep            ; DI, drive A, side E, motor, settle
                call    fdc_seek            ; to track D
                call    fdc_wp_chkrdy
                jr      c, ftk_done
                call    fmt_buf
                ld      bc, 146 + 16
                add     hl, bc              ; sector 1's ID field
                ld      bc, 652 - 2
                ld      a, 1
ftk_id:
                ld      (hl), d             ; C: the track
                inc     hl
                ld      (hl), e             ; H: the side
                inc     hl
                ld      (hl), a             ; R: the sector (N = 2, in the table)
                add     hl, bc
                inc     a
                cp      10
                jr      nz, ftk_id
                call    fmt_buf
                ld      a, $F0              ; WD2793 Write Track
                ld      (FDC_STATUS), a
ftk_busy:
                ld      a, (FDC_STATUS)
                rrca
                jr      nc, ftk_busy        ; until BUSY
ftk_loop:
                ld      a, (FDC_STATUS)
                bit     1, a
                jr      nz, ftk_byte        ; DRQ: the next byte
                rrca
                jr      c, ftk_loop         ; still BUSY
                and     (ST_WP | ST_LOST) >> 1
                jr      z, ftk_done         ; Cy = 0
                and     ST_LOST >> 1
                jr      z, ftk_wp           ; write protected: A = 0
                ld      a, 10               ; write fault
ftk_wp:
                scf
ftk_done:
                pop     de
                jp      fdc_io_done
ftk_byte:
                ld      a, (hl)
                ld      (FDC_DATA), a
                inc     hl
                jr      ftk_loop
; fmt_rle -- C runs (count, byte; a count of 0 is 256) from HL to DE.
fmt_rle:
                ld      b, (hl)
                inc     hl
                ld      a, (hl)
                inc     hl
fr_put:
                ld      (de), a
                inc     de
                djnz    fr_put
                dec     c
                jr      nz, fmt_rle
                ret
; each choice's BPB fields, in fmt_off's order: sectors / cluster, root entries,
; total sectors, the media ID, sectors / FAT, sides (the CF-3300's, measured:
; dskfmt_stock1.out, dskfmt_stock_c2..c4.out)
fmt_geo:
                db      1, 64, $68, $01, $FC, 2, 1
                db      2, 112, $D0, $02, $FD, 2, 2
                db      2, 112, $D0, $02, $F8, 2, 1
                db      2, 112, $A0, $05, $F9, 3, 2
; (fmt_off -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $50E0; its loop tests the pointer's low byte for EQUALITY.)
; the track: the index gap (80 x $4E), sync, the index mark ($F6 writes C2, then
; $FC), gap 1 -- the WD2793 data sheet's Write Track bytes
fmt_head:
                db      80, $4E, 12, $00, 3, $F6, 1, $FC, 50, $4E
; 📍 D-DSKFMT: the FORMAT dialogue lives here, next to DSKFMT (CALL FORMAT's hk_format
; and $4025 both use it); carves 34-36 made the room.
; fmt_bkey -- CALL FORMAT's key: INPUT$'s key source in main (no echo), so the wait
; happens with main in page 1 (a page-1 tenant must not wait: basic/repl.asm).
; (fmt_bkey -- MOVED by D-DISKCARVE6 step 4 to the pad in front of $77B8.)
; fmt_dos -- $4025, MSX-DOS's FORMAT: keys through our own CHGET body (page 0 is RAM
; there); the work area is the TPA from $0100 (COMMAND.COM lives at its top).
fmt_dos:
                ld      hl,innoe_body
                ld      de,$0100
                ld      bc,$1E00
; fmt_dlg -- the dialogue, from either: HL = the key routine, DE / BC = the work area.
; The CF-3300's keys and their order: a drive key (echoed), a choice 1..4 (echoed;
; other keys ignored -- unmeasured, TIER 6), any key to start (not echoed). Its WORDING
; may differ (Joost 2026-11-10): "and strike a key when ready" is the ROM's own swap
; prompt, where the CF-3300 says "Strike a key when ready". Out: Cy / A as DSKFMT.
fmt_dlg:
                ld      (FDC_DEST),hl       ; (free until DSKFMT runs)
                push    de
                push    bc
                ld      hl,fmt_t_drive
                call    fmt_pr
                call    fmt_key
                call    fmt_echo            ; the drive: one drive here, any key is A
                ld      hl,choice_txt
                call    fmt_pr
                ld      hl,fmt_t_q
                call    fmt_pr
fd_choice:
                call    fmt_key
                cp      '1'
                jr      c,fd_choice
                cp      '5'
                jr      nc,fd_choice
                push    af
                call    fmt_echo
                ld      hl,dsl_text_nl
                call    fmt_pr
                ld      hl,dsl_text_2 + 3   ; "and strike a key when ready", CR LF
                call    fmt_pr
                call    fmt_key             ; not echoed
                pop     af
                sub     '0'                 ; A = the choice
                pop     bc
                pop     hl
                ld      d,0                 ; drive A
                call    dskfmt_body
                ret     c
                ld      hl,fmt_t_done
; fmt_pr -- print the 0-terminated string at HL (conout_emit_e: the BIOS paged in by
; itself, so from BASIC and from MSX-DOS alike). Out: CF = 0.
fmt_pr:
                ld      a,(hl)
                or      a
                ret     z
                ld      e,a
                call    conout_emit_e       ; (keeps HL itself: D-DISKREREAD 2026-11-26, 2 B)
                inc     hl
                jr      fmt_pr
; (fmt_key / fmt_echo -- MOVED by D-DISKCARVE6 step 2 to the pad in front of $504E.)
fmt_t_drive:    db      "Drive name? (A,B) ",0
fmt_t_q:        db      "? ",0
fmt_t_done:     db      "Format complete",13,10,0
; cr_chkdir -- D-BDOSUNOP (2026-11-11): FCLOSE writes a file's entry only if the entry at
; the FCB's root index (+25) still carries the FCB's NAME. On the CF-3300, FCLOSE of an
; FCB never opened answers $FF and writes nothing unless the entry at index 0 -- a fresh
; FCB's +25 -- has that name; then 0, and the FCB's fields go there. Ours wrote entry
; +25 whatever its name: closing a never-opened SECOND.DAT, or NOSUCH.XYZ after a failed
; FOPEN, zeroed the FIRST entry's cluster and size, another file's
; (scratchpad/bdosunop_run3.out). in DE = the FCB; out CF = no such entry (or an I/O
; error) -- close_read's $FF -- else cr_dir's.
cr_chkdir:
                push    de
                ld      hl, 25
                add     hl, de
                ld      a, (hl)             ; the root index
                call    root_pos            ; DE = its sector, HL = its offset
                push    hl
                call    rs_dbuf
                pop     hl
                pop     de
                ret     c
                push    de
                ld      bc, SECTOR_BUF
                add     hl, bc              ; HL -> the entry
                ld      b, 11
ccd_lp:
                inc     de                  ; the FCB's name, +1..11
                ld      a, (de)
                cp      (hl)
                inc     hl
                scf
                jr      nz, ccd_out         ; another name: not this file's entry
                djnz    ccd_lp
                or      a
ccd_out:
                pop     de
                ret     c
                jp      cr_dir
; wr_reset -- D-WRRESET (2026-11-11): the sequential writer's state for a new file -- no
; cluster, an empty buffer, 0 bytes. It stood three times (bdos_create_body,
; fat_io_create, COPY's hkc_differ, which lacked only the BUFLEN store -- 0 is right
; for a fresh destination too); one copy gives the disk ROM ~30 B for D-WCACHE.
; out A = 0, HL = 0 (bdos_create_body's next store relies on HL = 0).
wr_reset:
                ; D-DISKREREAD (2026-11-26): WRCLUS .. WRBYTES+3 are 11 contiguous bytes
                ; (disk/equates.inc, guarded below), so one fill loop, 10 B. Out as before:
                ; A = 0, HL = 0 (1654 relies on it), the flags of `xor a`; B = 0 now.
                xor     a
                ld      hl, BDOS_WRCLUS
                ld      b, 11
wrr_lp:
                ld      (hl), a
                inc     hl
                djnz    wrr_lp
                ld      h, a
                ld      l, a
                ret
    IF (BDOS_WRBYTES + 4 - BDOS_WRCLUS - 11) or (BDOS_WRFIRST - BDOS_WRCLUS - 2) or (BDOS_WRSECIDX - BDOS_WRCLUS - 4) or (BDOS_WRBUFLEN - BDOS_WRCLUS - 5)
                db      WR_RESET_NEEDS_WRCLUS_TO_WRBYTES_CONTIGUOUS
    ENDIF
; sdw_end: the close of SAVE (basic/sv-diskwr.inc) and SAVE ,A (hk_chclose_e) under the
; FAT hold (FAT_DEFER bit 2). The order is D-ASAVEEND's (2026-11-12): (1) the buffered
; last sector FIRST, still under the hold -- it often starts a new cluster, and its
; mark + link then join the held FAT sector; (2) that sector out (frl_flush), BEFORE
; (3) the directory entry, whose work runs through FAT_MBUF. It was (2), then
; fat_io_close's (1)+(3): the final allocation re-read the FAT sector just written and
; wrote both copies again -- 3 FDC commands, ~0.4 s of a SAVE ,A
; (scratchpad/asave_fdc_line.out). A failing data write still sends the held sector
; out first (frl_xfail keeps the CF), as ffds_undefer does.
sdw_end:
                ld      hl, (FWR_BUFLEN)
                ld      a, h
                or      l                   ; CF = 0
                call    nz, fat_flush_data_sector
                jp      c, frl_xfail
                call    frl_flush
                ret     c
                jp      fat_dir_update
; wrbytes_add_recsize — BDOS_WRBYTES += RECSIZE, 4-byte little-endian add.
; 📍 D-IFFREAD2 (2026-11-14): MOVED HERE from after bdos_seqwrite_body (its one
; caller, a `call`), for the 10 B the two IFF re-reads need past $782B.
wrbytes_add_recsize:
                ; D-DISKREREAD (2026-11-26): one `adc` loop over all 4 bytes, the addend in A
                ; (RECSIZE, then 0 -- `ld` and 16-bit `inc` keep the carry), 2 B. A = 0 out
                ; now; the one caller (bdos_seqwrite_body) goes straight to
                ; fat_flush_data_sector, which takes no A.
                ld      hl, BDOS_WRBYTES
                ld      a, RECSIZE
                ld      b, 4
                or      a                   ; Cy = 0
wba_loop:
                adc     a, (hl)
                ld      (hl), a
                inc     hl
                ld      a, 0
                djnz    wba_loop
                ret
; (rh_init moved to the pad in front of $4EDE by D-DISKROMHOST: this region's bytes went to hrunc_h.)
; stime_body -- BDOS $2D STIME ($55E6's body, and bdos_tab's row for $F37D): H hour,
; L minute, D second. 🔴 D-F37DSETDATE (2026-11-22): it checks them now -- the
; VG-8020 + NMS 1200 and the CF-3300 refuse hour 24, minute 60 and second 60 with A
; $FF (docs/bdos/SYSTEM.md R-BS-TIME2) and take 23:59:59 (R-BS-DATE3); it took any
; time. An MSX1 keeps no clock: nothing is stored (GTIME still reads 0, R-BS-TIME).
; Out: A = 0 / $FF; BC = HL as before. (Moved here from in front of verify_body.)
stime_body:
                ld      a, 23
                cp      h
                jr      c, st_res           ; hour > 23
                ld      a, 59
                cp      l
                jr      c, st_res           ; minute > 59
                cp      d                   ; second > 59 -> Cy
                jr      c, st_res
                ld      a, 99               ; D-STIMERANGE (2026-11-26): hundredths > 99 -> Cy -- the
                cp      e                   ; CF-3300 answers $FF to 100 under MSX-DOS too (BDOSXT row 11,
                                            ; scratchpad/XT_stock.out); E is STIME's hundredths
st_res:
                sbc     a, a                ; A = $FF refused / 0 taken
                ld      b, h
                ld      c, l
                ret
; --- D-BUFINCTL (2026-11-26): BUFIN's control keys under MSX-DOS, as the CF-3300 ------------------
; Measured, one key between `AB` and `CD` (scratchpad/AE_bufinctl_keys.out: the screen;
; AE_bufinctl_buf.out / _buf2.out: the buffer and the printer): Ctrl-C echoes ^C and the program is
; abandoned (a warm boot here: see cc_break); Ctrl-P / Ctrl-N set / clear the
; printer echo ($F23B 1 / 0, AE_prnflag.out) and are not stored; Ctrl-U and ESC erase the line;
; Ctrl-R is dropped; Ctrl-X is dropped WITH the key after it (`ABD`; ^X^X CD -> `ABCD`); Ctrl-E,
; Ctrl-S (and the rest) are stored and echo as ^E. Under BASIC ($F37D's BUFIN) nothing changes --
; not measured there; DISKVE's high byte is the key, as bdos_dskio's (0 under BASIC).
; CLEAN-ROOM: the published BUFIN contract (map.grauw.nl, MSX2 TH §BDOS: DE = the buffer, +0 max,
; +1 count) and black-box readings -- typed keys in; the buffer, the screen and the printer out,
; plus the work-area cell $F23B. No stock routine read or decoded.
; cinl_ctl -- in: A = a key below $20, not CR / BS (conin_line_body jumps here). Out: back into
; conin_line_body -- cinl_store (A = the key) or cinl_getchar -- or to the Ctrl-C handler.
cinl_ctl:
                ld      c, a
                ld      a, (DISKVE + 1)
                or      a
                ld      a, c
                jp      z, cinl_store       ; BASIC: stored, as before
                cp      $03
                jr      z, cc_break
                cp      $10
                jr      z, cc_prn
                cp      $0E
                jr      z, cc_prn
                cp      $18
                jr      z, cc_skip
                cp      $15
                jr      z, cc_erase
                cp      $1B
                jr      z, cc_erase
                cp      $12
                jp      nz, cinl_store      ; ^E, ^S, ...: stored (cinl_echo shows ^E)
                jp      cinl_getchar        ; ^R: nothing
cc_prn:         sub     $0E                 ; ^N 0, ^P 2 (no borrow: CF = 0)
                rra                         ; ^N 0, ^P 1 -- the CF-3300's values
                ld      (PRN_ECHO), a
                jp      cinl_getchar
cc_skip:        call    cinl_key            ; ^X: the next key goes too
                jp      cinl_getchar
cc_erase:       ld      hl, CONIN_COUNT     ; ^U / ESC: un-echo and drop every stored key
                ld      a, (hl)
                or      a
                jp      z, cinl_getchar
                dec     (hl)
                ld      hl, cinl_bs
                call    fmt_pr
                jr      cc_erase
cc_break:       ld      hl, cc_ctlc
                call    fmt_pr
                jp      $0000               ; the warm boot: COMMAND.COM's A> again, as bdos_dskio's
                                            ; Abort. (Entering the handler BREAKV names, $D76D, from
                                            ; inside the call HUNG after the ^C: AE_bufctl_dosctl1.out.)
cc_ctlc:        db      "^C", 0             ; no CR LF: the warm-started COMMAND.COM starts its own line
                                            ; (with CR LF here: a blank row more than the CF-3300, dosctl3)
; cinl_echo -- conin_line_body's echo of a STORED key (E): under MSX-DOS a control key shows as
; caret + letter (^E, ^S: AE_bufinctl_keys.out); TAB, printable keys and BASIC as before.
cinl_echo:
                ld      a, e
                cp      $20
                jp      nc, conout_body
                cp      $09
                jp      z, conout_body
                ld      a, (DISKVE + 1)
                or      a
                jp      z, conout_body
                push    de
                ld      e, '^'
                call    conout_body
                pop     de
                ld      a, e
                add     a, $40
                ld      e, a
                jp      conout_body
; dskio_verify -- D-VERIFYREAD (2026-11-26): with BDOS VERIFY on (VERIFY_F), read the sector just written back
; and hand its result to the write: the CF-3300 reads back every sector it writes under VERIFY ON (BDOSXE-style
; FDC log, scratchpad/AE_verify1.out: +9 READs to 9 WRITEs; its order WWW RRR, here W R per sector -- the
; count is what the gate scores). The data lands at $4000.. -- this ROM's own page, mapped during DSKIO, so
; the stores are dropped and no buffer is touched. Not measured: what the CF-3300 answers when the read-back
; FAILS (here: that read's error code, as the write's).
; CLEAN-ROOM: the published VERIFY function, the FDC's command register watched; no stock routine read.
; In: HL = the buffer + 512 (fdc_write_phys's). Out: Cy / A = the read's result; HL kept.
verify_body:                                ; BDOS $2E VERIFY ($55FF's and bdos_tab's): A = E, and
                ld      a, e                ; D-VERIFYREAD: E kept for DSKIO's write
                ld      (VERIFY_F), a
                ret
dskio_verify:
                ld      a, (VERIFY_F)
                or      a
                ret     z               ; off: Cy = 0
                push    hl
                ld      hl, (FDC_DEST)
                push    hl
                ld      hl, $4000
                ld      (FDC_DEST), hl
                ld      hl, (FDC_LSEC)
                call    lsec_chs        ; C / D / E for this sector again
                call    fdc_read_phys
                pop     hl
                ld      (FDC_DEST), hl
                pop     hl
                ret
; --- D-DISKMSX2HOST (2026-11-27): THE SUBSLOTS ARE THE HOST'S, NOT "RAM 0, DISK ROM 1" ---------------------
; On C-BIOS MSX2 the RAM is subslot 2 (3-0 is the sub ROM); on MSX2+ the disk ROM sits in 3-3 (3-1 is the music
; ROM). The page-1 switches hard-coded MSX1's layout; these patch the installed RAM copies with the host's bits
; (scratchpad/AE_msx2host_boottrace.out, AE_msx2host_ptrace.out: MSX-DOS restarted / never reached our BDOS).
; CLEAN-ROOM: MSX2 Technical Handbook secondary-slot register ($FFFF, read inverted) and slot byte F000SSPP.
; wa_patch -- install_dos_env's tail: the $F368 / $F36B bodies' page-1 bits := our subslot / the RAM's
wa_patch:
                ld      a, (HOOK_SLOT)
                and     $0C
                ld      (WA_SEG_ROM + (wsr_ld - wa_seg_rom_tmpl) + 1), a
                call    ram_p1bits
                ld      (WA_SEG_RAM + (wsm_ld - wa_seg_ram_tmpl) + 1), a
                jp      install_br6         ; ... and br6 (D-CALSLTP0)
; p1_patch -- install_p1blit's tail: P1_BLIT's flip operand := our page-1 bits XOR the RAM's
p1_patch:
                call    ram_p1bits
                ld      hl, HOOK_SLOT
                xor     (hl)
                and     $0C
                ld      (P1_BLIT + (pb_flip - p1_blit_tmpl) + 1), a
                ret
; ram_p1bits -- A = the RAM's subslot in page-1 position (bits 3-2): page 3's, where the stack lives
; (set_ramad's rule), from the expander of page 3's primary.
ram_p1bits:
                ld      a, ($FFFF)
                cpl
                rrca
                rrca
                rrca
                rrca
                and     $0C
                ret

hk_format_gate:                             ; H_FORM's row (hook_tab), in front of hk_format
                ; C-BIOS integration step 1 (2026-11-27, scratchpad/abireach_probe.py): H_FORM is ALSO C-BIOS's BIOS
                ; FORMAT ($0147: `call H_FORM`), and what follows needs zerobas BASIC -- main's key source through
                ; calbak, BASIC's STREND. With no zerobas BASIC host (its marker, ZB_HOST) refuse at once, Cy = 1:
                ; no crash, nothing written. (fmt_dos is no target: its work area is the TPA from $0100 -- the
                ; CALLER there. A BIOS FORMAT for a BASIC-less host: todo D-BIOSFORMAT.)
                ; CLEAN-ROOM: our own marker cell (MSX2 TH ch.5 sec.7 SLTWRK) and C-BIOS's published source.
                ld      hl,(ZB_HOST)
                ld      de,ZB_HOST_SIG
                or      a
                sbc     hl,de
                scf
                ret     nz
                jp      hk_format
; bb_term -- BDOS $00 TERMINATE through $F37D (D-F37DTERM, 2026-11-26). From a BASIC program the CF-3300
; ends the program SILENTLY -- no further statement, no message, `Ok`, the CLEAR kept (measured by hand, a USR stub
; `ld c,0 / call $F37D / ret`; todo/done/D-F37DTERM.md). Here: what END does to the run -- ONEFLG 0, ENDFLAG 1
; (zerobas's BASIC, through the resident ABI) -- and back with A = 0. Under MSX-DOS (DISKVE's high byte set)
; nothing: A = 0 as before ($00 with no BASIC is the port's open question).
; CLEAN-ROOM: the published BDOS function list and black-box readings of the screen.
bb_term:
                ld      a, (DISKVE + 1)
                or      a
                ld      a, 0
                ret     nz                  ; MSX-DOS: as before
                ld      (ONEFLG), a
                inc     a
                ld      (ENDFLAG), a
                dec     a
                ret
; conout_dirio_out -- BDOS $06 DIRIO with E <> $FF: the char straight to CHPUT. On the CF-3300 it neither
; moves the logical column nor expands TAB (BDOSXL R2a / H / R2l: `XYZ` leaves it 0, from 2 it stays 2,
; a TAB at 36 stays 36). (disk/runtime.asm's conout_body jumps here.) Entry as conout_not_dispatch: caller's HL then AF on the stack.
conout_dirio_out:
                pop     hl
                ld      a, ($F237)
                push    af
                call    conout_emit_e       ; CHPUT (its column step undone below)
                pop     af
                ld      ($F237), a
                pop     af
                ld      a, e
                ret
                ds      $75A5 - $, $00
                jp      k_75A5          ; $75A5

; fcb_mark_written -- what a WRITE does to the FCB on the CF-3300 (D-WRBLKSEEK
; (b), read as data): +20..21 := the date 0821h and +22..23 := time 0 (Joost
; 2026-10-01, "Stamp as 3300"; BDOSX/BDOSX3/BDOSX6 read 0821h after a write to a
; file dated 0), and +24's 40h cleared (40h after FMAKE/FOPEN, 00h after a
; write: --wseek's dump, BDOSX8). in: HL = the FCB; out HL = +24; trashes BC, DE
fcb_mark_written:
                push    hl
                call    date_fat_word       ; DE = the DOS date (D-DOSDATE)
                pop     hl
                ld      bc, 20
                add     hl, bc
                ld      (hl), e
                inc     hl
                ld      (hl), d             ; +20..21 date
                inc     hl
                xor     a                   ; D-DISKREREAD (2026-11-26): A = 0, 1 B -- A / flags
                ld      (hl), a             ; out were date_fat_word's / `add`'s; no caller
                inc     hl                  ; (7632, 7778, 9108, runtime.asm 792) reads them
                ld      (hl), a             ; +22..23 time
                inc     hl
                res     6, (hl)             ; +24
                ret

; fmake_fcb_fill -- FMAKE's FCB fields as the CF-3300 fills them (D-WRBLKSEEK
; (b), read as data after FMAKE): +20..21 the date 0821h (Joost 2026-10-01,
; "Stamp as 3300"), +24 40h, +25 the entry's index in the root directory. Time
; +22..23 stays 0. in: HL = the FCB, FWR_DIRSEC/FWR_DIROFF from fat_dir_create.
; 💰 D-DSKFMT carve 21 (2026-11-10): the date is fcb_mark_written's, which also writes
; +22..23 := 0 -- what they hold already (its one caller, bdos_create_body, clears
; +16..31 first) -- and returns HL = +24.
fmake_fcb_fill:
                call    fcb_mark_written    ; +20..21 the DOS date, +22..23 0; HL = +24
                ld      (hl), $40           ; +24
                inc     hl
                push    hl
                ld      de, (FWR_DIRSEC)
                ld      hl, (FWR_DIROFF)
                call    dir_index
                pop     hl
                ld      (hl), a             ; +25
                ret

; fcb_dirloc -- FOPEN's FCB +25 := the found entry's index (D-WRBLKSEEK (b);
; stock 2Ch for BIG.BIN, ours left 0). Register-only: FWR_DIRSEC/DIROFF are
; BDOS_DIRSEC/DIROFF, an open-for-write file's, and must survive an FOPEN.
;   in: HL = the entry fat_find matched (in SECTOR_BUF), FAT_DIRSEC, IX = the
;   FCB; out: HL kept; trashes A, BC, DE
fcb_dirloc:
                push    hl
                ld      de, SECTOR_BUF
                or      a
                sbc     hl, de              ; HL = the entry's byte offset
                ld      de, (FAT_DIRSEC)
                call    dir_index
                ld      (ix+25), a
                pop     hl
                ret

; dir_index -- A := a root-directory entry's index: (sector - first root
; sector) x 16 + offset / 32. in: DE = its sector, HL = its byte offset (< 512).
dir_index:
                add     hl, hl
                add     hl, hl
                add     hl, hl              ; H = offset / 32
                ex      de, hl              ; HL = the sector, D = offset / 32
                ld      bc, (FAT_FIRSTROOT)
                or      a
                sbc     hl, bc              ; L = the sector's index in the root
                ld      a, l
                add     a, a
                add     a, a
                add     a, a
                add     a, a
                add     a, d
                ret

; fcb_clear_tail -- FMAKE's FCB +16..31 := 0 (D-WRBLKSEEK). WRBLK now positions
; from +28/+30 (wrblk_seek); a program that REUSES an FCB for a new file would
; otherwise walk the OLD file's clusters. Stock after FMAKE: +26..31 are 0.
;   in: HL = the FCB; out: HL kept; trashes A, B, DE
fcb_clear_tail:
                push    hl
                ld      de, 16
                add     hl, de
                ld      b, 16
fct_lp:
                ld      (hl), 0
                inc     hl
                djnz    fct_lp
                pop     hl
                ret

; wrblk_seek -- WRBLK's first positioning in a call, by CLUSTERS (D-WRBLKSEEK).
; The file has a first cluster (the caller checked). Target cluster index ci =
; sector / spc; the walk starts from FCB +28/+30 when that is at or before ci
; (else from FAT_FIRSTCLUS) and reads only FAT sectors. If the chain ends
; first, the iterator stops on the last real cluster marked exhausted, and the
; returned step count lets wrblk_read_or_extend_sector grow it from there.
;   in:  BC = the target sector-in-file, IX = the FCB, fat_open done
;   out: BC = wrblk_read_or_extend_sector steps still to take (>= 1);
;        FAT_CURCLUS/FAT_CLUSSEC set for the first of them
wrblk_seek:
                ld      h, b
                ld      l, c
                ld      a, (FAT_SECPERCLUS)
                dec     a
                and     l
                ld      (WRBLK_RECSEC), a   ; the sector-in-cluster (spc is a power of two)
                call    spc_div             ; HL = ci
                push    hl                  ; [ci]
                call    ci_cache
                jr      nc, ws_in           ; a real cached cluster -> walk from it
ws_first:
                pop     hl
                push    hl                  ; HL = ci steps from the first cluster
                ld      bc, (FAT_FIRSTCLUS)
ws_in:
                ex      de, hl              ; DE = clusters to walk
                ld      h, b
                ld      l, c                ; HL = the cluster
ws_walk:
                ld      a, d
                or      e
                jr      z, ws_at
                push    hl                  ; [the cluster]
                push    de
                call    fat_next_cluster    ; HL := its link (clobbers A/BC/DE)
                pop     de
                ld      a, l                ; D-DISKREREAD (2026-11-26): one 16-bit test,
                add     a, 8                ; frl_have's -- CF iff HL >= $0FF8 (a 12-bit
                ld      a, h                ; entry: the two tests agree on every value), 2 B
                adc     a, $F0
                jr      nc, ws_ok           ; < $0FF8: a real link
                pop     hl                  ; the chain ENDS after this cluster
                ld      (FAT_CURCLUS), hl
                ld      a, (FAT_SECPERCLUS)
                ld      (FAT_CLUSSEC), a    ; exhausted: the next step extends
                dec     de                  ; steps = (DE-1) x spc + sector-in-cluster + 1
                ld      hl, 0
                ld      b, a
ws_mul:
                add     hl, de
                djnz    ws_mul
                ld      a, (WRBLK_RECSEC)
                inc     a
                ld      e, a
                ld      d, 0
                add     hl, de
                jr      ws_tail
ws_ok:
                inc     sp
                inc     sp                  ; drop the saved cluster
                dec     de
                jr      ws_walk
ws_at:
                ld      (FAT_CURCLUS), hl
                ld      a, (WRBLK_RECSEC)
                ld      (FAT_CLUSSEC), a
                ld      hl, 1               ; one step: read the target sector
ws_tail:
                pop     de                  ; drop [ci]
                ld      b, h
                ld      c, l
                ret

; wrblk_fcb_keep -- after a WRBLK, the FCB carries what the CF-3300's does
; (D-WRBLKSEEK, read as data after block 5): +16..19 the size, +26 the first
; cluster, +28/+30 the cluster last written and its index in the chain.
;   in:  IX = the FCB; BDOS_WRBYTES = the new size; FAT_FIRSTCLUS; and, when the
;        call positioned (WRBLK_CURVALID), FAT_CURCLUS / WRBLK_CURSEC
wrblk_fcb_keep:
                call    fcb_size_keep       ; +16..19 (D-DSKFMT carve 12: runtime.asm)
                ld      hl, (FAT_FIRSTCLUS)
                ld      (ix+26), l
                ld      (ix+27), h
                push    ix
                pop     hl
                call    fcb_mark_written    ; +20..23 date/time, +24's 40h cleared
                ld      a, (WRBLK_CURVALID)
                or      a
                ret     z                   ; nothing positioned this call
wfk_cache:                                  ; (D-RNDFCB (c) enters here: rdrnd_ex)
                ld      hl, (FAT_CURCLUS)
                ld      (ix+28), l
                ld      (ix+29), h
                ld      hl, (WRBLK_CURSEC)
                call    spc_div             ; (Cy = 1 out, as before)
                ld      (ix+30), l
                ld      (ix+31), h
                ret

; spc_div -- HL = a sector's index in the file -> HL = its cluster's index in the
; chain (FAT_SECPERCLUS is a power of two). Out A = 0, Cy = 1. It stood three times,
; inline, in wrblk_seek, k47b2_seek and wfk_cache (D-DSKFMT's carve, 2026-11-10).
spc_div:
                ld      a, (FAT_SECPERCLUS)
spd_lp:
                srl     a
                ret     c
                srl     h
                rr      l
                jr      spd_lp

; ci_cache -- HL = ci, a cluster index; IX = the FCB, whose +28..29 hold the last
; cluster accessed and +30..31 its index. Out Cy = 0: walk HL = ci - that index steps
; from BC = that cluster; Cy = 1: walk from the first cluster (the cache is past ci,
; or holds no real cluster). DE = the cached index; A clobbered. It stood twice,
; inline, in wrblk_seek and k47b2_seek (D-DSKFMT's carve, 2026-11-10).
ci_cache:
                ld      e, (ix+30)
                ld      d, (ix+31)
                or      a
                sbc     hl, de              ; HL = ci - the cached index
                ld      c, (ix+28)
                ld      b, (ix+29)          ; BC = the cached cluster
                ret     c                   ; past ci
                ld      a, b
                or      a
                ret     nz                  ; Cy = 0: a real cluster
                ld      a, c
                cp      2                   ; Cy = 1: 0 or 1, no cluster
                ret

; k47b2_seek -- RDBLK's positioning (D-RDBLKSEEK), from k47b2_body's steps 6+7;
; its header there says what and why. Lives here because k47b2_body's region
; had ~10 B of slack before the $75A5 pin.
;   in:  IX = the FCB, RDBLK_RECSIZE resolved
;   out: Cy = 1 nothing to deliver (unreadable disk, or RR x RS past the end);
;        Cy = 0 positioned -- BDOS_BYTESLEFT, FAT_CURCLUS/FAT_CLUSSEC,
;        RDBLK_BUFPOS primed, FCB +28..31 := the cluster holding O and its index
k47b2_seek:
                call    seq_dirty           ; RDBLK moves the read iterator (B2)
                ld      a, (FAT_SECPERCLUS)
                or      a
                jr      nz, k47b2_geom
                call    fat_mount
                ret     c                   ; unreadable disk -> EOF, nothing delivered
k47b2_geom:
                ld      hl, (RDBLK_RECSIZE)
                ld      (WRBLK_RS), hl
                call    wrblk_off24         ; WRBLK_REC = O, the 24-bit start BYTE offset
                ; BDOS_BYTESLEFT := size (FCB+16..19) - O; past the end -> EOF
                ld      a, (ix+16)
                ld      hl, WRBLK_REC
                sub     (hl)
                ld      (BDOS_BYTESLEFT), a
                ld      a, (ix+17)
                inc     hl
                sbc     a, (hl)
                ld      (BDOS_BYTESLEFT + 1), a
                ld      a, (ix+18)
                inc     hl
                sbc     a, (hl)
                ld      (BDOS_BYTESLEFT + 2), a
                ld      a, (ix+19)
                sbc     a, 0
                ld      (BDOS_BYTESLEFT + 3), a
                ret     c                   ; O > size: nothing to deliver
                ; HL := O >> 9 (the sector index in the file; O is 24-bit)
                ld      hl, (WRBLK_REC + 1) ; D-DISKREREAD (2026-11-26): one word load, 5 B
                srl     h
                rr      l
                ; FAT_CLUSSEC := sector-in-cluster, HL := ci (spc is a power of two)
                ld      a, (FAT_SECPERCLUS)
                dec     a
                and     l
                ld      (FAT_CLUSSEC), a
                call    spc_div             ; HL = ci
                ld      (RDBLK_CNT), hl     ; ci, parked: step 8 reloads RDBLK_CNT per
                                            ; record, so it is free until then (no new RAM)
                call    ci_cache            ; the FCB's running cluster, at or before ci?
                jr      nc, k47b2_walk_in   ; a real cached cluster -> walk from it
k47b2_from_first:
                ld      hl, (RDBLK_CNT)      ; walk ci steps from the first cluster
                ld      c, (ix+26)
                ld      b, (ix+27)
k47b2_walk_in:
                ex      de, hl              ; DE = steps to walk
                ld      h, b
                ld      l, c                ; HL = the cluster
k47b2_walk:
                ld      a, d
                or      e
                jr      z, k47b2_walked
                push    de
                call    fat_next_cluster    ; HL := the next link (clobbers A/BC/DE)
                pop     de
                dec     de
                jr      k47b2_walk
k47b2_walked:
                ld      (ix+28), l
                ld      (ix+29), h          ; +28 := the cluster holding O
                ld      (FAT_CURCLUS), hl
                ld      hl, (RDBLK_CNT)
                ld      (ix+30), l
                ld      (ix+31), h          ; +30 := its index in the chain
                ; prime the byte source at O: a whole-sector start refills on the
                ; first byte; otherwise read the sector now and start inside it
                ld      hl, (WRBLK_REC)     ; D-DISKREREAD (2026-11-26): a word load, 3 B
                ld      a, h
                and     1
                ld      h, a                ; HL = O & 511
                or      l
                jr      nz, k47b2_mid
                ld      hl, 512
                ld      (RDBLK_BUFPOS), hl  ; force a sector refill on the first byte
                ret                         ; Cy = 0 (from the `or h` above)
k47b2_mid:
                push    hl
                call    fat_read_file_sector
                pop     hl
                ret     c
                ld      (RDBLK_BUFPOS), hl
                ret                         ; Cy = 0 (fat_read_file_sector's)
; --- S10.D: two of the RANDOM record engine's helpers (disk/kernel.asm hk_dget),
; in the free region before the $77B8 veneer.

; rnd_cmp -- CF = A:HL (24 bits) < the row's size (+16..19, 32 bits). IX = the
; row; A and HL are kept, DE is not.
rnd_cmp:
                push    hl
                ld      e, (ix+16)
                ld      d, (ix+17)
                or      a
                sbc     hl, de
                ld      l, a
                ld      h, 0
                ld      e, (ix+18)
                ld      d, (ix+19)
                sbc     hl, de
                pop     hl
                ret

; 🔴 D-GETEJECT (fat_rand_get's tail): frnd_locate's carry is BOTH "past the
; chain" and a chain-walk READ that failed. A failed DSKIO leaves its code in
; DISKOP_ERR and every successful one clears it, so a pending code is the failure.
frg_eofchk:
                ld      a, (DISKOP_ERR)
                or      a
                scf
                ret     nz                  ; a read failure
                jp      fill_rec            ; a clean end: the record is spaces, CF = 0
                                            ; (all FCH_RECMAX of it: hk_frnd's own
                                            ; fill. It was FAT_DBUF's whole 512 until
                                            ; D-FLDDESC -- at block+9 a 512 B fill
                                            ; would run into the next block)
; srd_pos -- D-BDOSFCB B2 (2026-11-10), SEQREAD's first act (wrseq_rd, IX = the FCB).
; The CF-3300 reads the record the FCB names, EX * 128 + CR: after RDRND RR 6 (CR 6)
; it reads record 6 and leaves CR 7 (scratchpad/bdosfcb_run4.out, row 23). Ours read
; on from its own iterator. BDOS_SEQREC is where that iterator stands ($FFFF after a
; random call: seq_dirty); a different record is re-positioned by rrnd_position's
; path, for every record (D-RRND256: it stopped at 255). Also seeds BDOS_DTA, as
; wrseq_rd did inline.
srd_pos:
                call    dta_from_dos
                ld      a, (ix+12)          ; EX
                ld      h, a
                ld      a, (ix+32)          ; CR (0..127: bit 7 clear)
                add     a, a
                srl     h                   ; H = EX >> 1, CF = EX bit 0
                rra
                ld      l, a                ; HL = EX * 128 + CR
                call    srd_same            ; D-SEQMIX3 (disk/pageenv.asm, for room)
                ret     z                   ; this file's iterator, there already
                ld      (BDOS_SEQREC), hl
                jp      rrnd_pos_hl         ; the whole record (D-RRND256: past 255 too)
; (moved here from kernel.asm by the D-DSKFMT defrag)
; mulacc_cmp -- WRBLK_MULACC vs FAT_FILESIZE, 32-bit unsigned, MSB first: Z = equal,
; Cy = MULACC below. Shared by wrblk_zero_path and wrblk_size_max (the chain stood
; twice, 28 B each -- D-WRZER's carve, 2026-11-09). Clobbers A, HL, DE, B -- D-DISKREREAD
; (2026-11-26): a 4-pass loop, 10 B (16-bit `dec` and `djnz` keep the last `cp`'s flags);
; both callers load DE / BC before they use them (to_wrbytes, wrblk_shrink's).
mulacc_cmp:
                ld      de, WRBLK_MULACC + 3
                ld      hl, FAT_FILESIZE + 3
                ld      b, 4
mc_lp:
                ld      a, (de)
                cp      (hl)
                ret     nz
                dec     de
                dec     hl
                djnz    mc_lp
                ret
; fmt_bkey -- CALL FORMAT's key, through main's CHGET (MOVED here by D-DISKCARVE6 step 4 (2026-11-17)).
fmt_bkey:
                ld      ix,chget_getbyte
                jp      calbak
; hl_cmp_eoc -- D-DISKROMHOST step 2b: the run it replaces stood at 4 sites (kernel.asm). MOVED here, into the pad
; in front of the $77B8 pin, by step 4 (c) (2026-11-25): hrunc_h needed its 7 bytes in the $75A5 region.
hl_cmp_eoc:
                ld      de, $0FF8
                or      a
                sbc     hl, de
                ret
                ds      $77B8 - $, $00
                jp      k_77B8          ; $77B8
; --- S10.D: the RANDOM record engine's chain walk, in the free region between the
; $77B8 and $782B veneers (no canonical entry inside). IX = the channel's row.

; frnd_locate -- GP_PHYS := the absolute sector of file sector GP_SEC, walking the
; chain from the row's first cluster -- and, if GP_FLAGS is 1 (PUT), extending it.
; CF = 1: past the end (GET) or disk full / I/O (the code pending).
; 🔴 FIXED ON THE WAY: the sub-ROM's copy divided GP_SEC by sectors-per-cluster
; on its LOW BYTE ONLY (`ld a,l / cp c`), so a record at or past 128 KB into the
; file (GP_SEC >= 256 on 512 B sectors -- record 513 at LEN=256) walked to the
; WRONG cluster. A 16-bit subtract now, and no bigger.
frnd_locate:
                ld      hl, (GP_SEC)
                ld      a, (FAT_SECPERCLUS)
                ld      c, a
                ld      b, 0
                ld      de, -1              ; DE = the cluster's index in the chain
frl_div:
                inc     de
                or      a
                sbc     hl, bc
                jr      nc, frl_div
                add     hl, bc              ; HL = the sector within that cluster
                ld      a, l
                ld      (GP_SECINCL), a
                ld      (GP_CLIDX), de
                call    frl_cache           ; D-PUTWALK: Z = start from the row's last
                jr      z, frl_have         ; cluster (HL), GP_CLIDX the steps left
                ld      l, (ix+DOUT_FIRST)
                ld      h, (ix+DOUT_FIRST+1)
                ld      a, h
                or      l
                jr      nz, frl_have
                call    frl_alloc           ; an empty file: its first cluster
                ret     c
                ld      (ix+DOUT_FIRST), l
                ld      (ix+DOUT_FIRST+1), h
frl_have:
                ld      (GP_CLUS), hl
                ld      hl, (GP_CLIDX)
                ld      a, h
                or      l
                jp      z, frl_done
                dec     hl
                ld      (GP_CLIDX), hl      ; one step fewer to walk
                ld      hl, (GP_CLUS)
                call    frnd_next           ; HL = the next cluster
                ret     c
                ld      a, l
                add     a, 8
                ld      a, h
                adc     a, $F0              ; CF iff HL >= $0FF8: the end of the chain
                jr      nc, frl_have        ; a real cluster
                ; 🐌 D-RECSLOW (2026-11-05): the chain ends -- extend it by GP_CLIDX + 1
                ; clusters with the FAT sector HELD in FAT_MBUF: every mark and link is
                ; a deferred patch (FAT_DEFER bit 0), and the sector goes to both FAT
                ; copies only when the scan or a link needs another one, and once at the
                ; end. It was a read + two writes per mark and per link, ~6 commands a
                ; cluster: `PUT #1,32767` at LEN=1 issued 451 FDC commands where the
                ; CF-3300 issues none before CLOSE (scratchpad/recslow_fdc.out). No state
                ; outlives this call (frl_flush on every exit), so no other FAT_MBUF user
                ; can see a dirty buffer. No frnd_next after the first new cluster: a
                ; fresh cluster's successor is the end of the chain, and a read would
                ; overwrite the held sector.
                ld      a, (GP_FLAGS)
                or      a
                scf
                ret     z                   ; GET: past the end
                jp      frl_ext             ; PUT: disk/kernel.asm's $4E20 fill
; (frl_walked -- a `jp frl_done` veneer -- retired by D-DSKFMT carve 32: its callers jump there)
; cr_dir -- D-WRBLKDIR (2026-11-08): close_read's write of a written file's entry --
; its size and first cluster, from the FCB, as the per-call update in wrblk_body did.
; The CF-3300 writes the entry at FCLOSE only. Reached only for a file the
; SEQUENTIAL writer did not write (bdos_close sends that one to its own path): WRBLK
; keeps +16 / +26 current (wrblk_fcb_keep), WRRND +16 (wrrnd_extend), FOPEN / FMAKE
; +26. in DE = the FCB; out CF = an I/O error, else HL = the FCB + 25. For room.
cr_dir:
                push    de
                pop     ix                  ; IX = the FCB (wrblk_fcbstate's contract)
                call    wrblk_fcbstate      ; FAT_FIRSTCLUS / FAT_FILESIZE / the entry
                ret     c
                call    cr_copy1            ; -> BDOS_WRFIRST / BDOS_WRBYTES, then
                ret     c                   ; fat_dir_update
                push    ix
                pop     hl
                ld      de, 25
                add     hl, de              ; HL = the FCB + 25
                ret
                ds      $782B - $, $00
                jp      k_782B          ; $782B
; --- contract bodies (stubs; filled in milestone 4; added as each veneer lands) ---
k_402D:         ret
k_41FD:         ret
k_4558:         ret
k_46C8:         ret
; k_47B2 — M31: faithful Random Block Read (tier2-m31-rdblk-randrecord-spec.md
; §3); was M21b's "stream-from-0" simplification (ignored FCB+33..35, always
; read to EOF). The kernel's $27 RDBLK handler ($D887, ret=$C51D) CALLs $47B2
; for BOTH the boot-time COMMAND.COM self-load (DE=$DC5B) and the runtime
; typed-command TPA load (DE=$DA40) AND every running program's user-level $27
; (arbitrary RR/RS/HL, e.g. BDOSX) — one unified body serves all three by
; *contract*, never by caller identity (§2): position to the FCB's
; random-record field (FCB+33..35), transfer up to HL records of the FCB's
; record size (FCB+14..15, 0 -> 128 default), zero-pad a final partial record,
; and report the true delivered count. The boot callers pass RR=0/RS=1/huge-HL
; (§1-Q2), which is just this same contract instantiated at those values (§3.2
; "boot instantiation check") — no branching on caller shape.
;   in:  DE = FCB pointer (kernel work buffer); HL = requested record count
;   out: A = $00 all HL requested records delivered / $01 EOF-first (§1-Q3)
;        HL = BC = records actually delivered
;        FCB+33..35 (RR) := entry-RR + HL (write-back; unchanged when HL=0,
;        i.e. positioned at/past EOF already)
;        IY = the entry DE (kernel work pointer, §5.4/§8.50); IX = the drive-A
;        DPB (§8.50's original boot contract, extended unconditionally, §2).
;        $F306 cleared (Tier-2 dispatcher-flag rule, M20).
; Reuses the byte-granular rdblk_getbyte/fat_open engine (disk/driver.asm) —
; the same machinery bdos_rdblk uses — but with k_47B2's OWN record loop, so
; bdos_rdblk/rdb_recloop_body (the pre-kernel boot MSXDOS.SYS-load path, never
; reached once a program is running, §1-Q1) stay byte-identical.
; CLEAN-ROOM: our own file layer streaming into the caller's own DTA; COMMAND.COM
; is data we copy, never disassembled. Contract from the public map.grauw.nl
; _RDBLK doc + black-box behavioural characterisation (§1).
; 3b-relocation (M24/M28/M29 idiom): the faithful body outgrew the $47B2..
; $4919 span, so this veneer diverts to k47b2_body in the free kernel tail
; (below k_47B2 in this file); every k_* canonical address stays net-zero.
k_47B2:         jp      k47b2_body          ; Tier-2 3b: divert; veneer fills the gap
; (the canonical entries with no body yet share ONE `ret` -- they stood as fifteen;
; D-DSKFMT defrag, 2026-11-09)
k_4919:
k_4935:
k_498C:
k_49B4:
k_4A39:
k_4B59:
k_4BE5:
k_4C25:
k_4E4B:
k_4EDE:
k_5FE5:
k_607B:
k_75A5:
k_77B8:
k_782B:         ret

; p1_blit_tmpl — clean-room blit routine (§8.35), LDIR'd to P1_BLIT ($E77A) by
; build_resident.  Called from dskio (page-1 ROM) via `call P1_BLIT` when the
; DSKIO transfer destination is in page 1 ($4000-$7FFF): the FDC read was
; redirected to SECTOR_BUF; this copies SECTOR_BUF to the original page-1 target
; by briefly switching page-1 sub-slot from disk ROM (3-1) to RAM (3-0).
;
; Runs from page-3 RAM (always-mapped), so the page-1 sub-slot flip is safe.
; The call return address (a page-1 ROM address) is still on the stack while
; page 1 = RAM; sub-slot 1 is restored BEFORE `ret`, so the RET lands in the ROM.
;
; $FFFF secondary register: write = direct sub-slot IDs (bits [3:2] = page-1
; sub-slot); read = complement of the written value (MSX2 TH §2.4).  We CPL
; after reading to recover the "write" form, mask bits 3:2 to 00 (sub-slot 0 =
; RAM), then write; on return CPL to restore original sub-slot 1.
;
; Source: MSX2 TH §2.4 (expanded-slot secondary register semantics); sub-slot
; layout confirmed by black-box differential (set_ramad / page0_ram_in, same
; host).  Clean-room: never reads the stock $EF95 or any proprietary driver.
; Lives in the free tail (LDIR-relocated; moved here from the cramped pre-$41FD
; region so the wa_seg M5.6 additions fit, §8.58). wa_seg_*_tmpl follows it
; immediately so build_resident copies BOTH with one LDIR (RAM-contiguous).
;   in:  HL = source, DE = page-1 destination, BC = the length (D-P1RDBLK:
;        the caller loads them -- dskio's 512-byte sector, RDBLK's single bytes)
;   out: the bytes copied with page 1 = RAM; page-1 sub-slot restored; DE / HL
;        past the copy (LDIR)
; 🔴 D-TPAPTAIL (b), 2026-10-23: THE WINDOW IS ATOMIC NOW. The flip writes $FFFF and
; not the SLTTBL[3] mirror, so an interrupt landing mid-copy whose hooks switch slots
; (BASIC's PLAY / trap hooks call the sub ROM in slot 3-2; the BIOS restores page 1
; from the mirror) would put the disk ROM back under the LDIR and lose the rest of
; the copy. DI around the window, and the caller's interrupt state back after.
;   (A and the flags are not kept; they never were.)
; 🔴 D-IFFREAD2 (2026-11-14): ASKED TWICE, as fdc_di_save (D-IFFREAD). On an NMOS Z80
; (and openMSX) an interrupt ACCEPTED during `ld a,i` reads P/V 0 though interrupts were
; on; only a false 0 is possible, and by the second read the handler has returned with
; EI -- so PE from either read is the truth. One read here would leave the caller masked.
; ONE push carries both: `di`, `ld a,(nn)` and `cpl` leave P/V alone, so the pushed F
; is still the IFF read's. That pays for the second read -- the installed copy has
; 27 B ($EFFE..$F018, FLD_END at $F019), and this is 27.
p1_blit_tmpl:
                ld      a, i                ; P/V = IFF2: were interrupts on?
                jp      pe, P1_BLIT + (pb_iff - p1_blit_tmpl) ; the RAM copy (absolute)
                ld      a, i                ; a false 0 is possible: ask again
pb_iff:         di
                ld      a, ($FFFF)          ; A = inverted sub-slot state (MSX2 TH)
                cpl                         ; A = actual sub-slot write value (P/V kept)
                push    af                  ; A = the sub-slot state, F's P/V = IFF2
pb_flip:        xor     0                   ; page-1 bits: OUR subslot -> the RAM's (install_p1blit
                                            ; patches ours XOR the RAM's; D-DISKMSX2HOST: was
                                            ; `and $F3`, "the RAM is subslot 0" -- the sub ROM on MSX2)
                ld      ($FFFF), a          ; page 1 now maps to RAM
                ldir
                pop     af                  ; A = original sub-slot write value
                ld      ($FFFF), a          ; page 1 back to the disk ROM
                ret     po                  ; they were off: leave them off
                ei
                ret
p1_blit_end:

; wa_seg_*_tmpl — clean-room bodies for the $F368/$F36B work-area segment-switch
; hooks (§8.58), LDIR'd to WA_SEG by build_resident and wired into the $F368 table
; by build_wa_table. The relocated kernel CALLs $F368 to map the disk ROM into
; page 1 (so it can run a page-1 disk-ROM routine) and $F36B to map RAM into page 1
; (so it can read its data living UNDER the page-1 ROM). On this expanded slot 3,
; subslot 1 = disk ROM, subslot 0 = RAM (machine config); the page-1 subslot is
; bits [3:2] of the slot-3 secondary register, $04 = subslot 1, $00 = subslot 0.
;
; Contract (measured stock, §8.58): register-TRANSPARENT (entry regs == exit regs);
; only persistent effect = SLTTBL[3] ($FCC8) and the live secondary register
; ($FFFF) set to $04 (rom) / $00 (ram). We read-modify-WRITE only the page-1 bits
; (mask $F3) so the other pages' subslots are preserved, source the current value
; from the SLTTBL mirror (write-form; $FFFF reads back complemented, so the mirror
; is the safe source), update the mirror then the live register, under DI. No EI:
; faithful -- the kernel calls these with interrupts already off (§8.58 trace).
; Bodies run from page-3 RAM (always mapped), so the page-1 flip never unmaps them.
; Lives in the free tail (LDIR-relocated; ROM position must not disturb canonical
; addresses -- the inline-before-$41FD placement overflowed that gap, §M5.6).
;
; Clean-room: our own standard expanded-slot switch (MSX2 TH §2.4 secondary-slot +
; SLTTBL); the stock $DF57/$DF59 bytes are never read. Straight-line + one PC-
; relative jr, so a plain LDIR relocates it verbatim.
;   in:  -            ; out: page-1 subslot of slot 3 set; all registers preserved
wa_seg_rom_tmpl:                            ; $F368 body: map disk ROM into page 1
                push    af
                push    bc                  ; B is our scratch -> preserve (transparent)
wsr_ld:         ld      a, $04              ; page-1 bits = OUR subslot (install_dos_env patches it
                jr      wa_seg_set_tmpl     ; from HOOK_SLOT; D-DISKMSX2HOST) (relocatable: PC-relative)
wa_seg_ram_tmpl:                            ; $F36B body: map RAM into page 1
                push    af
                push    bc
wsm_ld:         ld      a, $00              ; page-1 bits = the RAM's subslot (patched: ram_p1bits)
wa_seg_set_tmpl:
                di
                ld      b, a                ; B = desired page-1 subslot bits
                ld      a, (SLTTBL3)        ; current slot-3 subslot (write-form mirror)
                and     $F3                 ; clear page-1 bits, keep pages 0/2/3
                or      b                   ; merge new page-1 bits
                ld      (SLTTBL3), a         ; update RAM mirror first
                ld      ($FFFF), a          ; ...then the live secondary-slot register
                pop     bc
                pop     af
                ret
wa_seg_end_tmpl:

; f365_iord_tmpl — clean-room body for the fixed disk-work-area slot-read stub at
; $F365 (M15 OI-1/§7 pin: unbuilt/FF on ours; §7.1 causally BOUNDS the func-9 STROUT
; output blocker to completing `wa_seg` + this stub). Reads the PPI primary-slot-
; select register (port $A8) and returns; the disk-ROM work area's own per-char
; output loop CALLs this fixed address (observed black-box: reader PC = $F365 itself,
; i.e. an executed 2-instruction stub, never disassembled — see docs/tier2-m15-spec.md
; §7.2). Installed at the FIXED address $F365 (not WA_SEG-relative: the kernel calls
; it by that absolute address, like WA_JMPTAB), by install_f365 below.
; CLEAN-ROOM: IN A,(n) reading port $A8 is the documented i8255 PPI primary-slot-
; select register (MSX2 TH ch.2 / i8255 PPI datasheet, docs/allowed-sources.md class
; A) — our own encoding of a public 2-instruction sequence, not read from the ROM.
;   in: - ; out: A = primary-slot register; other registers preserved
f365_iord_tmpl:
                in      a, ($A8)
                ret
f365_iord_end:


; install_resident — copy the page-3-resident BLIT and the two HOOK BODIES to
; their RAM homes (D-ALIASBITE carve, 2026-09-23). Two LDIRs because the two
; blocks are no longer contiguous in RAM: they used to sit at $E77A/$E795, i.e.
; INSIDE main's `FSECTOR_BUF` ($E5C0..$E7BF), and main now RE-READS that buffer
; after a disk-ROM crossing -- which overwrote 54 B of executable code. The
; templates are still adjacent in ROM (wa_seg_rom_tmpl follows p1_blit_end), so
; only the destinations differ.
; ⚠️ Sited HERE and not inline at the call site: the pre-$41FD init region is at
; capacity, and overflowing the `ds $41FD - $` anchor assembles to an EMPTY
; object file with NO error (tools/pad_rom.py is what catches it).
install_resident:
                ; (P1_BLIT is installed on every host now -- install_p1blit, D-P1RDBLK)
                ld      hl, wa_seg_rom_tmpl
                ld      de, WA_SEG
                ld      bc, wa_seg_end_tmpl - wa_seg_rom_tmpl
                ldir
                ret

; res_print_tmpl — clean-room body for the resident $-string print routine, relocated
; to RES_PRINT ($F1C9) by install_res_print (M15 §9.2/§9.3(ii)). The kernel CALLs
; $F1C9 with DE -> a '$'-terminated string. Emits each char via conout_body (the
; proven $5454 CONOUT path func-2/conin_line_body already use) before advancing --
; the M15 root cause was that our first cut consumed the string without emitting it.
; conout_body preserves BC/DE/HL/IX/IY internally, but only across ITS OWN entry
; state; since E is overwritten with the char before the call, DE (the string
; pointer) is saved/restored around the call here. Straight-line + one PC-relative
; jr, so a plain LDIR relocates it verbatim.
; CLEAN-ROOM: derives from the published func-9 STROUT contract ('$'-terminated
; string at DE) + our own conout_body; no stock bytes (never reads $F392/$F2AC/
; $F237/$D88A-region routine bytes, docs/tier2-m15-spec.md §5/§9).
;   in:  DE -> '$'-terminated string ; out: DE past the '$', A=$24, others as Z80 CP
res_print_tmpl:
                ld      a, (de)         ; A = next string byte
                inc     de
                cp      '$'             ; $24 = MS-DOS string terminator
                ret     z               ; done -> return to the kernel
                push    de              ; save the string pointer (E about to change)
                ld      e, a            ; conout_body's ABI: char in E (M10)
                call    conout_body     ; emit via $5454 CONOUT path -> CHPUT
                pop     de              ; restore the string pointer
                jr      res_print_tmpl  ; (relocatable: PC-relative loop)
res_print_end:

; install_res_print — copies res_print_tmpl to RES_PRINT ($F1C9). Lives in the free
; tail (no budget limit) and is reached by a single 3-byte `call` from build_resident's
; cramped pre-$41FD path (init.asm), same rationale as install_f365 (§7.1/§7.2/§9.3).
install_res_print:
                ; 🔴 D-DRVINT (2026-11-08): DRVINT ($FB21 drives on interface 1, $FB22 its
                ; slot) -- the CF-3300 holds 2 / $87 from the prompt on; ours left C-BIOS's
                ; 0 / 0, so a program finding the disk ROM through them to call DSKIO /
                ; DSKCHG / GETDPB direct CALSLTed slot 0 (scratchpad/dskent_drvtbl.out).
                ; DRVCNT ($F347) and DRVTBL+0 ($F348) lie in that order already: one
                ; word. Here, in the $7F7C region, for room (init's callers set both first).
                ld      hl, (DRVCNT)        ; L = the drive count, H = DRVTBL+0 = our slot
                ld      ($FB21), hl
                ld      hl, res_print_tmpl
                ld      de, RES_PRINT
                ld      bc, res_print_end - res_print_tmpl
                ldir
                ret

; seldsk_drv_body — the $50D5 kernel SELDSK-time entry (M17; tier2-m17-spec.md).
; The relocated kernel CALLs $50D5 while processing BDOS SELDSK ($0E) to read the
; logical-drive count. Black-box contract (M17 §2.1, readwatch causal pin): read
; $F347 into A, preserve BC/DE/HL/IX/IY. Reached directly from the fixed $50D5 veneer
; (jp seldsk_drv_body) -- lives here in the free tail like conin_line_body/conout_body.
; CLEAN-ROOM: derives from the observed DATA-cell read contract (return [$F347]) +
; our own code; no stock/kernel bytes decoded (only entry/exit regs + the $F347 read).
;   in: -    ; out: A = ($F347) = drive count ; BC/DE/HL/IX/IY preserved
; (the body is in front of the $55E6 pin since D-SFIRSTVOL)

; curdrv_body — the $50C4 kernel CURDRV-time entry (M18; tier2-m18-spec.md).
; The relocated kernel CALLs $50C4 while processing BDOS CURDRV ($19) to read the
; CURRENT-drive index. Black-box contract (M18 §2.1, readwatch causal pin): read
; $F247 into A, preserve BC/DE/HL/IX/IY. Reached directly from the fixed $50C4 veneer
; (jp curdrv_body) -- lives here in the free tail like seldsk_drv_body.
; CLEAN-ROOM: derives from the observed DATA-cell read contract (return [$F247]) +
; our own code; no stock/kernel bytes decoded (only entry/exit regs + the $F247 read).
;   in: -    ; out: A = ($F247) = current drive ; BC/DE/HL/IX/IY preserved
; (the body is in front of the $55E6 pin since D-SFIRSTVOL)

; ===== M19: runtime directory search (BDOS SFIRST $11 / SNEXT $12) =============
; sfirst_body / snext_body / dirscan_match (name_cmp_wild went to name_cmp, D-DISKREREAD) — the bodies behind the
; $4FB8 (SFIRST) / $5006 (SNEXT) page-1 kernel entries. They walk our own root
; directory (reusing fat_mount + the fat_find ff_secloop/ff_entloop structure),
; match each 8.3 name against the kernel's search-FCB pattern (with the ? wildcard),
; copy the found entry's drive-byte + 32-byte directory image to the runtime DTA,
; and persist the scan position in BDOS_SRCHIDX so SNEXT resumes where the prior
; call stopped. Return A=$00 found / A=$FF exhausted (published SFIRST/SNEXT
; contract; the flags are irrelevant to the caller — M19 §2.4).
;
; PINNED black-box contract (M19 §2/§4, callseq/capture — NO stock CODE decoded):
;   - entry DE -> the kernel's search-FCB copy: drive byte at +0, 11-byte 8.3 name
;     pattern at +1..+11 (?=$3F matches any single char; DIR issues all-? -> lists
;     every entry). Observed DE=$DA40, [$DA40]=80 3F 3F ... (M19 §2.3/§4.5).
;   - the runtime DTA base is the kernel's current-DTA pointer, the fixed work-area
;     word at DOS_DTAPTR ($F23D) (=$D403 here, set via the kernel's own SETDTA just
;     before the search); read fresh each call (M19 §4.1). It receives one drive
;     byte (current drive + 1) then the 32-byte raw dir entry (observed: DTA=
;     01 <32-byte entry>, M19 §4.4).
;   - NO attribute filter: SFIRST/SNEXT surface EVERY non-$E5, non-$00 entry incl.
;     volume-label ($08) and subdir ($10) entries (stock returned the SandStone
;     volume label as match #1; COMMAND.COM's formatter does the label filtering /
;     "nn files" count, not us — M19 §4.4/§7, verified against test.dsk).
; CLEAN-ROOM: our own dir walk over our own fat_* layer + the published FCB /
; directory-entry / SFIRST-SNEXT contract; the stock routine's internals (108/101
; page-1 PCs) were never decoded — only entry/exit registers, call counts, and
; one-sided DATA reads of OUR OWN memory. See docs/tier2-m19-spec.md.

; setdta_cache_body — the $5058 kernel SETDTA-time entry (M19). Caches the new DTA
; pointer (DE) into the disk work-area cell DOS_DTAPTR ($F23D), so the SFIRST/SNEXT
; bodies can read the runtime DTA from a fixed cell (as stock's SFIRST does, PCs
; $4FCC/$4FEF). Register-transparent (like curdrv_body/wa_seg): preserves A/BC/DE/
; HL/IX/IY. Reached from the fixed $5058 veneer (jp setdta_cache_body).
; CLEAN-ROOM: our own DE->$F23D store + the observed entry DE=DTA / write-$F23D
; contract; no stock routine internals decoded. See docs/tier2-m19-spec.md.
;   in:  DE = new DTA pointer ; out: ($F23D) := DE ; all registers preserved
; (the body is in front of the $5552 pin since D-SFIRSTVOL)

; getalloc_body — the $505D kernel GETALLOC-time entry (M20 REVISION 3;
; tier2-m20-spec.md). BDOS $1B GETALLOC: return the volume's allocation geometry.
;
; Walks clusters 2..(rawTotal-1) sector-by-sector into WBUF (the SAME fatofs/byteidx/
; parity math and the SAME shared fac_entry_from_wbuf unpack that fat_alloc_cluster
; uses -- fat.asm; not modified, not called in place, so the Tier-1 write path stays
; byte-unchanged), counting $000 (free) entries instead of stopping at the first one.
; The cluster/free-counter/loop-bound live on the CPU stack across each
; fac_entry_from_wbuf call (it clobbers AF/BC/DE/HL and preserves nothing -- same
; push/pop-around-the-call idiom as fat.asm's own fac_have_sec).
;
; The FIX (Revision 3, see the $505D veneer comment above for the full root-cause
; writeup): clear the dispatcher's HL-passthrough flag ($F306) before `ret`, loading
; the final A value AFTER the clear. Without this the kernel's common BDOS-exit path
; silently overwrites our correct HL with a mirror of our own A/BC.
;
; Any FDC read error takes the fail-safe exit (A=0 BC=$0200 DE=0 HL=0) -- matches
; fat_total_clusters' own "report no free space" error posture.
;
; CLEAN-ROOM: our own FAT scan (fat_mount + read_sector, reusing fat.asm's existing
; fac_entry_from_wbuf unpack) + the published GETALLOC A/BC/DE/HL contract (map.grauw.nl
; MSX-DOS BDOS functions) + the black-box-pinned $F306 dispatcher-flag semantics (no
; stock CODE decoded).
;   out (success): A=(FAT_SECPERCLUS) BC=$0200 DE=total data clusters
;                  HL=free-cluster count ; ($F306):=$00
;   out (fail-safe): A=0 BC=$0200 DE=0 HL=0
getalloc_body:
                call    fat_mount               ; geometry -> FAT_* scratch (SECTOR_BUF)
                jr      c, ga_fail
                call    fat_total_clusters       ; DE = dataClusters + 2 (rawTotal)
                push    de                       ; stack[0] = rawTotal (stays for the scan)
                ld      hl, $FFFF
                ld      (FAT_WRTMP2), hl         ; no FAT sector cached yet (fat_alloc_
                                                 ; cluster's own init, same convention)
                ld      de, 0                    ; DE = free-cluster counter
                ld      hl, 2                    ; HL = cluster under test
ga_scanloop:
                pop     bc                       ; BC = rawTotal (peek: pop then re-push)
                push    bc
                push    hl                       ; save cluster across the compare
                or      a
                sbc     hl, bc                   ; HL = cluster - rawTotal
                pop     hl                       ; restore cluster
                jr      z, ga_scandone           ; cluster == rawTotal -> scan complete
                ; fatofs = cluster + (cluster>>1)  (== floor(cluster*3/2), the same
                ; math fac_loop_body uses -- Microsoft FAT spec §3.2 12-bit packing)
                push    de                       ; save free counter across the call below
                push    hl                       ; save cluster (need it after the call)
                call    fac_cached_entry         ; DE = 12-bit entry value (D-DISKCARVE3; clobbers
                                                 ; AF/BC/DE/HL -- fat.asm's own contract)
                jr      c, ga_rderr              ; (its pops: cluster, counter, rawTotal)
                ld      a, d
                or      e
                pop     hl                       ; restore cluster
                pop     de                       ; restore free counter
                jr      nz, ga_notfree
                inc     de                       ; entry == $000 -> free, bump counter
ga_notfree:
                inc     hl                       ; next cluster
                jr      ga_scanloop
ga_rderr:
                pop     hl                       ; discard saved cluster
                pop     de                       ; discard saved free counter
                pop     hl                       ; discard rawTotal
                jr      ga_fail
ga_scandone:
                ex      de, hl                   ; HL := free-cluster count (final)
                pop     de                       ; DE := rawTotal
                dec     de
                dec     de                       ; DE := total data clusters (final)
                call     clr_f306   ; clear dispatcher HL-passthrough flag
                                                 ; (M20 REVISION 3 fix -- see $505D veneer)
                ld      bc, $0200                ; BC := bytes/sector (final)
                ld      a, (FAT_SECPERCLUS)      ; A := sectors/cluster (final)
                or      a                        ; Cy = 0 (success)
                ret
ga_fail:
                call     clr_f306   ; clear here too (consistent with the
                                                 ; success path; harmless -- A=0 already
                                                 ; forces free_bytes=0 either way)
                ld      bc, $0200                ; BC := 512 (belt-and-braces)
                ld      d, a                     ; DE = HL = 0 from clr_f306's A = 0 --
                ld      e, a                     ; D-DISKREREAD (2026-11-26), 2 B
                ld      h, a
                ld      l, a
                ret

; sfirst_body — BDOS SFIRST ($11): find the FIRST matching root-dir entry.
;   in:  DE -> search FCB (name pattern at DE+1) ; out: A=$00 found / $FF none
; ✂️ D-DISKCARVE3 (2026-11-14): SFIRST IS "CURSOR := 0, THEN SNEXT" (13 B). The cursor is
; reset BEFORE the mount now, so a SFIRST whose mount fails (a disk error, A = $FF) leaves
; it at 0 where it used to keep the old one -- an SNEXT after a failed SFIRST is a scan of
; a disk that just failed either way. fat_mount and the DTA copy do not read it.
sfirst_body:
                ld      hl, 0
                ld      (BDOS_SRCHIDX), hl  ; scan cursor := entry 0
                ; falls into snext_body

; snext_body — BDOS SNEXT ($12): resume from BDOS_SRCHIDX, find the NEXT match.
;   in:  DE -> search FCB (name pattern at DE+1) ; out: A=$00 found / $FF exhausted
snext_body:
                inc     de                  ; DE -> the 11-byte 8.3 pattern (FCB+1)
                ld      (FAT_NAMEPTR), de   ; dirscan_match compares against this
                call    fat_mount           ; (re)parse BPB -> FAT_FIRSTROOT/FAT_FIRSTDATA
                jp      c, ds_none          ; disk error -> no entry
                ; fall through: BDOS_SRCHIDX already points at the next entry.

; dirscan_match — walk the root dir from BDOS_SRCHIDX; on the first entry matching
; the pattern (name_cmp, `?` = any) copy its drive-byte + 32-byte image to the DTA, set
; BDOS_SRCHIDX := match-index + 1, return A=$00. On $00 end-mark or all sectors
; scanned, return A=$FF. Reconstructs the sector cursor from BDOS_SRCHIDX alone
; (idx>>4 = sectors to skip; idx&15 = entry-in-sector) so it never persists
; FAT_DIRSEC/FAT_DIRREM across calls (M19 §4.3/§4.4).
dirscan_match:
                ; sectorsToSkip = BDOS_SRCHIDX >> 4  (16 entries per 512-B sector)
                ld      hl, (BDOS_SRCHIDX)
                ld      b, 4
dsm_shr:
                srl     h
                rr      l
                djnz    dsm_shr             ; HL = idx >> 4 = sector offset
                push    hl                  ; D-DISKREREAD (2026-11-26): kept for DIRREM below, not
                                            ; shifted a second time, 14 B
                ; FAT_DIRSEC := FAT_FIRSTROOT + sectorOffset
                ld      de, (FAT_FIRSTROOT)
                add     hl, de
                ld      (FAT_DIRSEC), hl
                ; FAT_DIRREM := FAT_ROOTSECS - sectorOffset
                call    fat_rootsecs        ; HL = root sectors (D-WCACHE W1; no inputs)
                pop     de                  ; DE = sector offset
                or      a
                sbc     hl, de
                jp      z, ds_none          ; already past the last root sector
                jp      c, ds_none          ; (defensive) offset beyond root
                ld      (FAT_DIRREM), hl
dsm_secloop:
                call    dirrem_z
                jp      z, ds_none          ; scanned every remaining root sector
                ld      de, (FAT_DIRSEC)
                call    rs_dbuf
                jp      c, ds_none          ; FDC error -> exhausted (no entry)
                ; start entry-in-sector = BDOS_SRCHIDX & 15
                ld      a, (BDOS_SRCHIDX)
                and     $0F
                ld      c, a                ; C = entry index within this sector
                ; HL = SECTOR_BUF + entry*32
                ld      hl, SECTOR_BUF
                ld      b, a
                or      a
                jr      z, dsm_athl
dsm_addent:
                ld      de, 32
                add     hl, de
                djnz    dsm_addent
dsm_athl:
                ld      a, 16
                sub     c
                ld      b, a                ; B = entries left in THIS sector (16-C)
dsm_entloop:
                ld      a, (hl)
                or      a
                jr      z, ds_none          ; $00 = end of directory -> exhausted
                cp      $E5
                jr      z, dsm_skip         ; deleted entry -> skip
                push    hl                  ; 🔴 D-SFIRSTVOL (2026-11-08): the VOLUME LABEL
                ld      de, 11              ; (attr bit 3) is not a file -- the CF-3300's
                add     hl, de              ; SFIRST "????????.???" skips it and returns
                bit     3, (hl)             ; the first file (scratchpad/bdosx9_run3.out,
                pop     hl                  ; DTA 1); ours returned the label, and SNEXT
                jr      nz, dsm_skip        ; stayed one entry behind
                ld      de, (FAT_NAMEPTR)
                push    hl
                push    bc
                call    name_cmp            ; Z if the 8.3 name matches the pattern
                pop     bc
                pop     hl
                jr      z, dsm_found
dsm_skip:
                ld      de, 32
                add     hl, de              ; next 32-byte directory entry
                call    dsm_bump            ; BDOS_SRCHIDX += 1 (advance past this one)
                djnz    dsm_entloop
                ; sector done: advance FAT_DIRSEC, dec FAT_DIRREM, next sector.
                call    dir_nextsec         ; 💰 D-DISKCARVE6: FAT_DIRSEC += 1, FAT_DIRREM -= 1
                jr      dsm_secloop
dsm_found:
                call    dsm_p1_pre          ; D-P1SRCH: fill a scratch, blit it after
                ; HL = matched 32-byte directory entry. Build the runtime DTA as the
                ; MSX-DOS SFIRST/SNEXT "found FCB" (drive + unopened FCB), matching
                ; stock byte-for-byte in every field COMMAND.COM's DIR reads (name,
                ; attribute, time, date, first cluster, size). The field layout was
                ; pinned black-box against stock's DTA + a readwatch of the DIR
                ; formatter's DTA reads (M19 §4.4):
                ;   DTA[0]      = drive (current drive + 1)
                ;   DTA[1..11]  = entry[0..10]  (11-byte 8.3 name)
                ;   DTA[12]     = 0  (FCB extent/EX)
                ;   DTA[13]     = entry[11]     (attribute)
                ;   DTA[14]     = 0, DTA[15] computed (below)
                ;   DTA[16..32] = entry[15..31] (raw: the CF-3300 copies 15..21 too)
                ; The attribute at DTA[13] is what lets DIR filter volume-label /
                ; subdir entries and count "nn files" correctly -- writing a verbatim
                ; raw entry (attr at DTA[12]) mis-set that filter (the alternating-
                ; garbage / wrong-count bug). CLEAN-ROOM: our own field copy from our
                ; own dir walk into the published FCB-result layout; no stock code.
                push    hl                  ; save entry ptr
                ld      de, (DOS_DTAPTR)    ; DE = runtime DTA base
                push    de                  ; save DTA base
                ; zero DTA[0..32] (33 bytes) so all gap/unset fields are $00.
                ld      h, d
                ld      l, e                ; HL = DTA base = DE (D-DISKREREAD 2026-11-26:
                ld      (hl), 0             ; the `push hl / pop de` that re-made it went, 2 B)
                inc     de                  ; DE = DTA+1
                ld      bc, 32
                ldir                        ; propagate the $00 across DTA[1..32]
                pop     de                  ; DE = DTA base
                pop     hl                  ; HL = matched dir entry
                ; DTA[0] = drive (current drive + 1)
                ld      a, (CURDRV_CELL)
                inc     a
                ld      (de), a
                inc     de                  ; DE = DTA+1
                ; DTA[1..11] = entry[0..10] (name)
                call    ldir_11             ; HL = entry+11, DE = DTA+12
                ; DTA[12] stays 0; DTA[13] = entry[11] (attr)
                inc     de                  ; DE = DTA+13
                ld      a, (hl)             ; entry[11] = attribute
                ld      (de), a
                ; DTA[14] stays 0; DTA[16..32] = entry[15..31] -- 🔴 D-DOSLADDER D4
                ; (2026-11-09): the CF-3300 copies entry[15..21] raw too (a PC-made
                ; disk's creation / access dates: probes/disk/dosl/d4dir.asm,
                ; scratchpad/dosl_d4_3.out); this left DTA[16..22] zero. DIR never
                ; reads them, which is why the black-box pin above missed it.
                ; D-DISKREREAD (2026-11-26): both pointers walked on from where ldir_11 left
                ; them -- no push / pop of the entry, no DTAPTR + 16 add (DE = DTA+13 here,
                ; and DOS_DTAPTR is the base DE came from), 10 B
                ld      bc, 4
                add     hl, bc              ; HL = entry+15
                inc     de
                inc     de
                inc     de                  ; DE = DTA+16
                ld      bc, 17
                ldir                        ; -> DTA[16..32]
                call    dsm_p1_post         ; D-P1SRCH: the 33 bytes to the real DTA
                call    dsm_bump            ; BDOS_SRCHIDX := match-index + 1
                xor     a                   ; A = $00 = found
                ret
ds_none:
                ld      a, $FF              ; A = $FF = no (more) matching entry
                ret


; (name_cmp_wild -- the FCB `?` compare -- went by D-DISKREREAD (2026-11-26), 17 B: it ran
; the very instructions of name_cmp (basic/fat-prim-body.inc: `?` matches anything, else
; toupper_cmp; Z = match), with one redundant `ld a,(de)`. Its two callers call name_cmp.
; Its "C preserved" note was false: toupper_cmp loads C, and neither caller reads C.)

; --- M24 slice B relocated bodies (tier2-m24-fclose-multicluster-spec.md §6) --

; fdc_read_data_body — relocated fdc_read_data (M26, tier2-m26-spec.md): its
; old span at driver.asm's $435A collided with the real kernel's canonical
; $17 FREN dispatch entry, $4392 (56 bytes into the original 74-byte body --
; the fdc_rd_n1/fdc_rd_n2 status-decode tail). Logic below is byte-for-byte
; unchanged from the original; driver.asm's fdc_read_data is now a 3-byte
; `jp` thunk to here, freeing $4392 for the real `jp fren_body` veneer.
fdc_read_data_body:
                ld      e, 2            ; two 256-byte halves
                ld      b, 0            ; djnz 0 -> 256 iterations
                ; D-FDCDI: the wait for the FIRST byte opens interrupt windows (see
                ; fdc_fdrq in disk/fat.asm for when). INLINE, and two status reads per
                ; window, because a 2DD byte must be taken within 32 us of its DRQ:
                ; a first cut that waited in fdc_fdrq and returned here lost the
                ; first byte on ~1 read in 3 with NO interrupt at all
                ; (scratchpad/fdcseq_win_diag.out) -- the call / ret and the loop setup
                ; between the DRQ and the read were the whole budget.
                ld      a, (FDC_IFF)
                or      a
                jr      z, fdc_rd_wait  ; a masked caller: as before
                cp      1 + 2 * FDC_WINMAX
                jr      nc, fdc_rd_wait ; windows spent: the last try is masked
                in      a, ($A8)        ; page 0 not the BIOS -- MSX-DOS: masked,
                and     3               ; as fdc_fdrq (disk/fat.asm) says why
                jr      nz, fdc_rd_wait
frdw_poll:
                ld      a, (FDC_STATUS)
                bit     1, a
                jr      nz, fdc_rd_byte ; the first DRQ: take the byte at once
                bit     0, a
                jr      z, fdc_rd_status    ; ended without a DRQ
                ld      a, (FDC_STATUS)
                bit     1, a
                jr      nz, fdc_rd_byte
                ei
                nop                     ; a pending interrupt is taken here
                di
                jr      frdw_poll
fdc_rd_blk:
                ld      b, 0            ; djnz 0 -> 256 iterations
fdc_rd_wait:
                ld      a, (FDC_STATUS)
                bit     1, a            ; DRQ?
                jr      nz, fdc_rd_byte
                bit     0, a            ; BUSY?
                jr      nz, fdc_rd_wait
                jr      fdc_rd_status   ; finished with no DRQ -> short/error
fdc_rd_byte:
                ld      a, (FDC_DATA)
                ld      (hl), a
                inc     hl
                djnz    fdc_rd_wait
                dec     e
                jr      nz, fdc_rd_blk
fdc_rd_drain:
                ld      a, (FDC_STATUS) ; all 512 read; wait for command end
                bit     0, a
                jr      nz, fdc_rd_drain
fdc_rd_status:
                ld      a, (FDC_STATUS)
                and     ST_NOTRDY + ST_RNF + ST_CRC + ST_LOST
                ret     z               ; A = 0, Cy = 0 (the `and`) -- D-DISKREREAD (2026-11-26):
                                        ; fdc_rd_ok's `or a / ret` made the same, 3 B
                ld      b, a            ; keep the error bits
                call    fdc_lostwin     ; D-FDCDI: lost data under a window? try again
                jp      nz, fwd_nowin   ; 💰 D-DISKCARVE5: the write path's decoder (disk/
                                        ; fat.asm) -- byte-for-byte the read path's copy
                ld      a, CMD_READ
                ld      (FDC_STATUS), a
                jr      fdc_read_data_body
; (fdc_rd_nowin -- the FDC status -> DSKIO error code decode -- was a byte-for-byte
; copy of fat.asm's fwd_nowin; D-DISKCARVE5 (2026-11-08, scratchpad/diskngram.py)
; jumps there instead.)

; bdos_seqwrite_body — relocated bdos_seqwrite (M26, tier2-m26-spec.md): its old
; span at disk/fat.asm's $46EE collided with the real kernel's canonical $30
; WRABS dispatch address, $4720 -- the displacement byte of the `jr c,
; bsw_full` that used to live there (trace --resync: ours fell through into
; live bdos_seqwrite tail code, silently zeroing BDOS_WRBUFLEN via the
; buffer-drain path before reaching bsw_ok's false-success `ret`). Logic
; below is byte-for-byte unchanged; disk/fat.asm's bdos_seqwrite is now a
; 3-byte `jp` thunk to here, freeing $4720 for the real `jp wrabs_body`
; veneer. Only symbolic callers (`jp bdos_seqwrite` in driver.asm/kernel.asm)
; reference this routine -- no external code jumps into bsw_ok/bsw_full/
; bsw_err/wrbytes_add_recsize's middle, so the whole unit relocates cleanly.
bdos_seqwrite_body:
                ld      a, (BDOS_WRMODE)
                or      a
                jr      z, bsw_err          ; not open for write
                ; --- M34 (tier2-m34-wrseq-diskfull-spec.md §3.1): eager disk-full
                ; onset parity with stock. At a sector start that would need a
                ; NEW cluster, confirm one is available BEFORE buffering/counting
                ; this record -- mirrors ffds_nopad_body's own needs-a-new-cluster
                ; test (kernel.asm ffds_nopad_body) so the real allocation (still
                ; at flush time, unchanged) never fails after we've claimed A=$00.
                ld      hl, (BDOS_WRBUFLEN)
                ld      a, h
                or      l
                jr      nz, bsw_mid         ; mid-sector: cluster already validated
                ld      hl, (BDOS_WRCLUS)
                ld      a, h
                or      l
                jr      z, bsw_first        ; no cluster yet (fresh file)
                ld      a, (BDOS_WRSECIDX)
                ld      hl, FAT_SECPERCLUS
                cp      (hl)
                jr      c, bsw_m34_buffer   ; room in current cluster -> no new one
bsw_m34_needclus:
                call    fat_have_free_cluster ; Cy=0 a $000 cluster exists; Cy=1 none
                jr      c, bsw_full         ; disk full -- BEFORE any buffering (A = 1, Cy = 1:
                jr      bsw_m34_buffer      ; the same 3 bytes there -- D-DISKREREAD 2026-11-26, 1 B)
                ; D-BDOSFCB B3 (2026-11-10): the FIRST cluster is claimed at record 0,
                ; not at the first sector flush -- the CF-3300's FCB has +26 / +28 from
                ; its first SEQWRITE (scratchpad/bdosfcb_run5.out, rows 2-4). The flush
                ; then finds BDOS_WRCLUS set and BDOS_WRSECIDX 0 (FMAKE's) and writes
                ; into it. No free cluster: A = 1 before any buffering, as M34's check.
bsw_first:
                call    fat_alloc_cluster   ; HL = the cluster, marked end-of-chain
                jr      c, bsw_full
                ld      (BDOS_WRFIRST), hl
                ld      (BDOS_WRCLUS), hl
                jr      bsw_m34_buffer
                ; 🔴 D-SEQMIX (2026-11-14): THE WRITER KEEPS NOTHING IN THE SHARED BUFFER
                ; BETWEEN CALLS. A record buffered here was overwritten by another file's
                ; RDBLK (SECTOR_BUF is everyone's), and seq_flush on a later read wrote
                ; whatever another file had left there over this file's sector
                ; (scratchpad/seqmix_run1.out / run3.out). Every record is written through
                ; now; a mid-sector record re-reads the sector as far as it is written
                ; (dout_write's pattern) -- BDOS_WRBUFLEN is "bytes of this sector on disk".
bsw_mid:
                call    dout_secread        ; (FWR_CLUS, FWR_SECIDX) -> FAT_DBUF
                jr      c, bsw_full
bsw_m34_buffer:
                ; copy RECSIZE bytes DTA -> SECTOR_BUF + BDOS_WRBUFLEN
                ld      hl, (BDOS_WRBUFLEN)
                call    dta_rec             ; copy 128 bytes DTA -> the buffer
                ; advance buffered length and total byte count by RECSIZE.
                ld      hl, (BDOS_WRBUFLEN)
                ld      de, RECSIZE
                add     hl, de
                ld      (BDOS_WRBUFLEN), hl
                call    wrbytes_add_recsize ; BDOS_WRBYTES += RECSIZE (4-byte LE)
                ; D-SEQMIX: written through, full or not (the pad zeros the rest).
                call    fat_flush_data_sector
                jr      c, bsw_full         ; disk full / write error
                ld      hl, BDOS_WRBUFLEN+1
                bit     1, (hl)             ; 512 = $0200: the sector is complete
                jr      z, bsw_back
                ld      (hl), 0             ; BUFLEN := 0 (its low byte is 0 at 512)
                jr      bsw_ok
bsw_back:
                ld      hl, BDOS_WRSECIDX
                dec     (hl)                ; back onto the sector for the next record
bsw_ok:
                xor     a                   ; A = $00 success
                ret
bsw_full:
                ld      a, $01              ; disk full (MSX-DOS seq-write code)
                ret
bsw_err:
                ld      a, $FF
                ret

; (wrbytes_add_recsize moved in front of the $75A5 pin by D-IFFREAD2, for room.)

; fdel_body — MSX-DOS-1 kernel FDEL ($13) canonical entry $436C's real body
; (M26, tier2-m26-spec.md sec 2.2). CLEAN-ROOM, single exact-match only
; (same fat_find precedent as fren_body -- BDOSX3 only exercises an exact
; name; '?' wildcard multi-delete is out of scope, confirmed at sign-off).
; PLACEMENT NOTE: this body first landed appended to disk/fat.asm's own end
; (next to fren_ioerr/rdabs_ioerr/wrabs_ioerr -- the last folded into rdabs_ioerr
; by D-DSKFMT carve 17 -- following those three's own
; precedent), but that broke test_gdate.py/test_getdpb.py -- fat.asm's tail
; feeds straight into kernel.asm's own tightly-packed pinned-address
; corridor (GDATE $553C etc, disk/kernel.asm) with no slack for a body this
; size (same "64KB limit passed" class of hazard the FREN slice already hit
; once and worked around by using kernel.asm's OWN free tail instead -- see
; fdc_read_data_body below). Moved here for the same reason.
; Entry is a kernel-side FCB copy pointed to by DE (drive byte + 11-byte
; 8.3 name, zero-filled tail) -- same +1 name-field convention fren_body
; uses. Two things this body must do that fren_body didn't: (a) free the
; file's FAT12 chain (every cluster zeroed in EVERY on-disk FAT copy, via
; fat_next_cluster/fat_write_fat_entry) -- confirmed necessary by black-box
; on-disk $E5 forensics: stock frees both clusters of BDOSX3's 2-cluster
; scratch file, so a bare $E5 stamp alone would under-match stock (leak the
; chain); (b) only THEN stamp the directory entry's first byte $E5 and
; persist it. Order matters: fat_next_cluster/fat_write_fat_entry both use
; SECTOR_BUF as scratch, clobbering the dirent fat_find first located -- so
; the chain-free walk runs BEFORE re-locating the entry (a second fat_find
; call, cheap and reuses already-verified code rather than tracking a
; pointer across a clobbering call). IX holds the FCB-copy pointer across
; every helper call in this body (confirmed by inspection: fat_mount,
; fat_find, fat_next_cluster, fat_write_fat_entry, read_sector, and
; write_sector never touch IX -- only fopen_fill_body/fren_body do, in
; disk/fat.asm), so re-deriving the name pointer for the second fat_find is
; just IX+1 again.
; 🔴 D-FDELWILD (2026-11-10): EVERY MATCH IS DELETED, as on the CF-3300 -- FDEL "Z?.TMP"
; over Z1.TMP and Z2.TMP left neither there and one here (probes/disk/dosl/d5rendel.asm,
; scratchpad/dosl_d5_2.out). fdel_one deletes the first match; this repeats it until
; none is left (a deleted entry is $E5 and no longer matches). $FF only when the
; FIRST pass finds nothing; an I/O error (2) is passed through.
fdel_body:
                push    de
                call    fdel_one
                pop     de
                or      a
                ret     nz                  ; nothing matched ($FF) or an I/O error
fdel_more:
                push    de
                call    fdel_one
                pop     de
                or      a
                jr      z, fdel_more        ; another one gone
                cp      $FF
                ret     nz                  ; an I/O error: A = 2, as fdel_one left it
                jr      fdel_ok             ; D-DISKREREAD (2026-11-26): fdel_relocate's identical
                                            ; success tail (clr_f306, HL = 0), 4 B
fdel_one:
                push    de
                pop     ix                  ; IX = kernel FCB-copy pointer (held throughout)
                call    mnt_find            ; FAT_FIRSTCLUS/FAT_FILESIZE set (a miss -> fren_miss)
                ld      hl, (FAT_FIRSTCLUS)
                ld      a, h
                or      l
                jr      z, fdel_relocate    ; zero-length file: no chain to free
fdel_free_loop:
                push    hl                  ; stack: [cur cluster]
                call    fat_next_cluster    ; HL := next link (cur was consumed as input)
                ex      (sp), hl            ; stack: [next]; HL := cur
                call    fwfe_zero ; zero cur's entry in every on-disk FAT copy  ; free = $000 (Microsoft FAT spec sec 3.2)
                jr      c, fdel_chain_ioerr
                pop     hl                  ; HL := next
                call    hl_cmp_eoc
                jr      nc, fdel_relocate   ; next >= $0FF8 -> chain fully freed
                add     hl, de              ; HL := next again (undo the probe subtraction)
                jr      fdel_free_loop
fdel_chain_ioerr:
                pop     hl                  ; discard the saved "next", keep the stack balanced
                jr      fdel_ioerr
fdel_relocate:
                ; SECTOR_BUF now holds FAT-sector content (clobbered by the chain
                ; walk above, or untouched if this file was zero-length) -- re-run
                ; fat_find for a fresh, valid dirent pointer + FAT_DIRSEC.
                call    mf_find             ; (a miss -> fren_miss)
                ld      (hl), $E5           ; CP/M delete marker (Microsoft FAT spec sec 3.1)
                ld      de, (FAT_DIRSEC)
                call    wr_sbuf
                jr      c, fdel_ioerr
fdel_ok:                                    ; (fdel_body's success exit too)
                call     clr_f306   ; M20 dispatcher-flag rule
                ld      h, a
                ld      l, a                ; HL = $0000 on success (pinned exit value)
                ret
; (fdel_miss -- byte-identical to disk/fat.asm's fren_miss -- removed by D-DISKCARVE,
; 2026-11-07: its three callers jump to fren_miss, A = $FF, CF set, $F306 = 0)
fdel_ioerr:
                jp      fren_ioerr          ; D-DISKROOM: the same body (clr_f306, CF, A = 2)

; fcb_fatst -- FAT_FIRSTCLUS := FCB +26..27, FAT_FILESIZE := FCB +16..19 (IX = the FCB).
; It stood twice, inline, in wfs_geom and rrnd_position (D-DSKFMT's carve, 2026-11-10).
; Out: HL = the size's high word; A, BC, DE untouched.
; mnt_find -- IX = an FCB: fat_mount, then fat_find its name (+1). Found: returns,
; HL = the entry, Cy = 0. Unreadable or not found: DROPS its return address and goes
; to fren_miss (A = $FF, Cy = 1) -- so the caller's own caller gets that answer, as
; the callers' `jp c, fren_miss` did; nothing else may be on the stack. mf_find skips
; the mount. It stood in fopen_fill_body, fdel_one and (the find) fdel_relocate
; (D-DSKFMT's carve, 2026-11-10).
mnt_find:
                push    ix
                pop     de
                call    fat_mount
                jr      c, mf_miss
mf_find:
                push    ix
                pop     hl
                inc     hl                  ; HL -> FCB+1, the 8.3 name (fat_find's contract;
                                            ; it reads no DE and sets it in fat_dir_start) --
                                            ; D-DISKREREAD (2026-11-26): straight into HL, 1 B
                call    fat_find
                ret     nc
mf_miss:
                pop     af                  ; (A and the flags are fren_miss's)
                jp      fren_miss

fcb_fatst:
                ld      l, (ix+26)
                ld      h, (ix+27)
                ld      (FAT_FIRSTCLUS), hl
                ld      l, (ix+16)
                ld      h, (ix+17)
                ld      (FAT_FILESIZE), hl
                ld      l, (ix+18)
                ld      h, (ix+19)
                ld      (FAT_FILESIZE+2), hl
                ret

; rrnd_position / rrnd_sector / rdrnd_body / wrrnd_body — MSX-DOS-1 kernel
; RDRND ($21) / WRRND ($22) canonical entries $4788/$4793's real bodies (M26,
; tier2-m26-spec.md §2.3). CLEAN-ROOM. Entry is the same kernel FCB-copy
; convention as fdel_body/rdabs_body/wrabs_body: DE -> a 37-byte scratch copy
; of the FCB at $DA40; copy+33 = the FCB random-record field r0 (BDOS $18 SETRND
; / direct FCB poke). SCOPE: r0 and r1 (copy+33 / +34: records 0..65535, more
; than a 720 KB disk holds) position the record since D-RRND256 (2026-11-22); it
; was r0 alone at implementation time, so records past 255 went to RR mod 256. PLACEMENT: kernel.asm's free tail (not disk/fat.asm's own end), per the
; FDEL slice's placement lesson (fat.asm's tail starves kernel.asm's own
; pinned-address corridor of slack for a body this size).
;
; rrnd_position seeds the existing sequential-read iterator (FAT_CURCLUS/
; FAT_CLUSSEC/BDOS_RECIDX/BDOS_BYTESLEFT/BDOS_DTA) to point at record r0,
; reusing fat_open + repeated fat_read_file_sector calls (each one reads
; exactly one more sector and advances the cluster chain as needed) rather
; than a dedicated fast-seek -- correct and simple, and the only sector-
; addressing arithmetic (firstData + (cluster-2)*secPerClus + clussec)
; already exists inside fat_read_file_sector/frs_mul_body. rdrnd_body then
; reuses bdos_seqread verbatim for the actual transfer (§4 non-goal: no
; change to bdos_seqread_body itself). wrrnd_body CANNOT reuse
; wrseq_body/bdos_seqwrite the same way -- that engine is append-oriented
; (BDOS_WRMODE-gated, a 512-byte-flush model for a file opened by FMAKE) and
; wrong for overlaying 128 bytes mid-file into a file opened for READ by
; FOPEN -- so it is a small, new read-modify-write body instead: overlay the
; DTA's 128 bytes into the sector rrnd_position already loaded into
; SECTOR_BUF, then persist via write_sector. Because fat_read_file_sector
; does not expose the absolute sector number it just read (only frs_mul_body,
; entered by `jp`, computes it, mid an unrelated read call already committed
; to SECTOR_BUF), wrrnd_body's rrnd_sector helper mirrors that same formula
; independently (does NOT modify frs_mul_body -- §4 non-goal) using the
; POST-call FAT_CURCLUS/FAT_CLUSSEC state: fat_read_file_sector always
; finishes with FAT_CURCLUS already advanced to the sector it read (if
; needed) and FAT_CLUSSEC incremented by exactly one past it -- so
; `FAT_CLUSSEC - 1` safely recovers the clussec of the sector just read, with
; no off-by-one or cluster-boundary hazard (confirmed by reading
; fat_read_file_sector's own body: the increment always follows the read,
; never precedes it, and is never separately normalised/wrapped).
;
; Register contract (capture, tier2-m26-spec.md §2.3): entry A=$25 B=$00
; C=$00 DE=$DA40 SP=$DBFE ret=$D88A (FDEL-dispatcher class, not RDABS's — no
; addr-low fingerprint, C is not the function number); exit A=$00 H=$00
; L=$00 on success. Stock's one clear random-op FCB side effect: CR (user
; FCB+32, mirrored at copy+32) := r0's low byte post-call -- reproduced here
; explicitly (copy+32 := copy+33) since it is a per-call random-op effect,
; not a generic kernel copy-back artifact (contrast fdel_body's DE=$03D0,
; which the kernel already reproduced with no fdel_body code at all).
rrnd_position:
                call    seq_dirty           ; B2 / D-BDOSSEQ: SEQREAD re-positions; a
                                            ; write-open file's buffered records go out
                ld      l, (ix+33)
                ld      h, (ix+34)          ; HL = the record (r0 / r1 -- D-RRND256)
rrnd_pos_hl:                                ; srd_pos: HL = the record
                ; 🔴 D-RRND256 (2026-11-22): the WHOLE record positions it. It was r0 alone
                ; ("0..255, up to a 32640-byte file"), so past record 255 every RDRND /
                ; WRRND / WRZER / RDBLK went to record RR mod 256 -- record 300's write
                ; landed on 44 (probes/bios/spec/xrrnd1.asm, scratchpad/XRRND1_run1.out);
                ; the size and clusters, from the whole RR, looked right. Held on the stack
                ; across mount / find / open (all clobber the registers).
                push    hl                  ; [rec]
                call    dta_from_dos
                                            ; the kernel's real SETDTA only ever
                                            ; touches DOS_DTAPTR, never BDOS_DTA)
                ; 🔴 D-RDRNDMULTI (2026-10-01): find THIS FCB's file first. fat_open
                ; alone re-primed from FAT_FIRSTCLUS/FAT_FILESIZE, i.e. whatever the
                ; LAST fat_find found -- two FCBs read alternately with RDRND both
                ; read the last-found file (disk_probe_wrblk_alt.py --rnd: OUT2.BIN
                ; 12h x 8 where the CF-3300 gives 11h..18h). D-RDBLKMULTI's fix.
                call    fat_mount
                jr      c, rrnd_pos_drop    ; unreadable disk -> both callers' Cy path
                push    ix
                pop     hl
                inc     hl                  ; HL -> copy+1, the 8.3 name (fat_find's contract)
                call    fat_find            ; Cy=0 found -> FAT_FIRSTCLUS/FAT_FILESIZE set
                jr      c, rrnd_pos_drop    ; no such file -> both callers' Cy path
                ; 🔴 D-RNDLIVE (2026-11-10): the chain and the size come from the FCB, not
                ; the directory. The entry is only rewritten at FCLOSE, so after WRRND /
                ; WRBLK / WRZER grew an open file the directory still said its old size
                ; (often 0): RDRND of a record just written answered "past the end" (A = 1)
                ; where the CF-3300 reads it (probes/disk/dosl/d6rand.asm,
                ; scratchpad/dosl_d6_2.out). The FCB's +26 and +16..19 are kept current
                ; by every write (wrblk_fcb_keep, wrrnd_extend) and filled by FOPEN.
                call    fcb_fatst
                call    fat_open            ; reset iterator to file start (FAT_FIRSTCLUS)
                pop     hl                  ; HL = rec
                ld      a, l
                and     3                   ; A = record-in-sector, kept on the stack across
                push    af                  ; the seek loop (fat_read_file_sector clobbers all)
                push    hl                  ; [recsec][rec]
                ; BDOS_BYTESLEFT := FAT_FILESIZE - rec * 128, 24 bits (rec * 128 = C:HL)
                ld      c, h
                srl     c                   ; C = rec >> 9, the product's top byte
                ld      b, 0
                call    hl_x128             ; HL = (rec * 128) AND $FFFF
                ld      de, (FAT_FILESIZE)
                ex      de, hl
                or      a
                sbc     hl, de
                ld      (BDOS_BYTESLEFT), hl
                ld      hl, (FAT_FILESIZE + 2)
                sbc     hl, bc              ; the borrow carried; Cy: rec is past EOF
                jr      nc, rrnd_pos_left
                ld      hl, 0
                ld      (BDOS_BYTESLEFT), hl
rrnd_pos_left:
                ld      (BDOS_BYTESLEFT + 2), hl
                pop     bc
                srl     b
                rr      c
                srl     b
                rr      c
                inc     bc                  ; BC = rec >> 2, the sector in the file, + 1
rrnd_pos_loop:
                push    bc
                call    fat_read_file_sector
                pop     bc
                jr      c, rrnd_pos_drop    ; seek ran off the end of the chain
                dec     bc
                ld      a, b
                or      c
                jr      nz, rrnd_pos_loop
                pop     af                  ; A = record-in-sector
                ld      (BDOS_RECIDX), a
                or      a                   ; Cy = 0 ok
                ret
rrnd_pos_drop:
                pop     hl
                ret                         ; Cy kept

                                            ; arithmetic, which stood here inline (D-DSKFMT
                                            ; defrag, 2026-11-09; both callers reload HL
                                            ; and write_sector sets A / B / C itself)

rdrnd_body:
                push    de
                pop     ix                  ; IX = kernel FCB-copy pointer
                ld      a, (ix+33)          ; CR := r0 MOD 128 FIRST: the CF-3300 sets it
                and     $7F                 ; past the end of the file too (A = 1; probes/
                ld      (ix+32), a          ; disk/wrzer.asm's records 21 / 23 -- D-WRZER)
                call    rrnd_position
                jr      c, rrnd_eof
                call    bdos_seqread_body        ; A=$00 record delivered / $01 EOF
                or      a
                jr      nz, rrnd_finish     ; EOF from seqread -- pass A through, HL untested
                ld      a, (ix+33)          ; CR (copy+32) := r0 MOD 128 (stock's random-op
                and     $7F                 ; side effect; 🔴 D-RNDFCB 2026-11-08: it was raw r0
                ld      (ix+32), a          ; -- record 199 left $C7 where the CF-3300 has $47,
                                            ; scratchpad/rndfcb_stock.out / rndfcb_ours.out)
                call    rdrnd_ex            ; ... and +12 EX := RR >> 7, +14 := 0 (D-RNDFCB (b))
                jr      wrrnd_zero          ; A = 0, HL = $0000, the shared tail -- D-DISKREREAD
                                            ; (2026-11-26): WRRND's identical 5 bytes, 3 B
rrnd_eof:
                ld      a, $01              ; positioned past end-of-file
                jr      rrnd_finish

wrrnd_body:
                push    de
                pop     ix                  ; IX = kernel FCB-copy pointer
                ld      (ix+14), 0          ; D-BDOSFCB: +14 := 0, as RDRND's rdrnd_ex does
                                            ; -- the CF-3300 clears it on an in-file WRRND
                                            ; too (scratchpad/bdosfcb_run2.out, row 12)
                call    rrnd_position
                jr      c, wrrnd_ext        ; past EOF: D-WRRNDEXT grows the file (WRBLK's
                                            ; machinery); M26 had scoped FAT growth out
                ; SECTOR_BUF now holds the target sector (loaded by rrnd_position's seek
                ; loop) -- overlay the caller's DTA record into it at BDOS_RECIDX*RECSIZE.
                ld      a, (BDOS_RECIDX)
                ld      h, a
                ld      l, 0
                srl     h
                rr      l                   ; HL = BDOS_RECIDX * 128
                call    dta_rec             ; the caller's record -> its place in SECTOR_BUF
                call    rrnd_sector         ; DE = absolute logical sector to persist
                call    wr_sbuf
                jr      c, wrrnd_ioerr
                push    ix
                pop     hl
                call    fcb_mark_written    ; +20..23 date/time, +24's 40h cleared (BDOSX3)
                call    wrrnd_extend        ; M36: grow ix+16..19 + the on-disk dirent if this
                                            ; write landed past the previous end-of-file
                jr      c, wrrnd_ioerr
wrrnd_ok:
                ld      a, (ix+33)          ; CR (copy+32) := r0 (stock's random-op side effect)
                ld      (ix+32), a
wrrnd_zero:                                 ; (RDRND's success exit too)
                xor     a
                ld      h, a
                ld      l, a                ; HL = $0000 (pinned exit value, success)
                jr      rrnd_finish
wrrnd_ioerr:
                ld      a, 2                ; generic FDC I/O error (fdc_read_data convention,
                                            ; untested by BDOSX3 -- no negative-path record)
rrnd_finish:                                ; shared tail: M20 dispatcher-flag rule, preserve A
                push    af
                call     clr_f306
                pop     af
                ret
; 🔴 D-WRRNDEXT (2026-11-08): WRRND / WRZER PAST THE END OF THE FILE. rrnd_position
; refuses there (M26 scoped FAT growth out), so under MSX-DOS a random write that
; grows a file answered A = $02 where the CF-3300 writes it (probes/disk/wrzer.asm,
; scratchpad/wrzer_stock3.out). WRBLK already grows a file from the FCB's own state
; (wrblk_fcbstate / wroe_ext) and leaves the sectors an extension skips unwritten --
; WRRND's face on the CF-3300 (the disk's bytes kept). So: one record of 128 bytes
; by wrblk_body -- FCB +14..15 := 0 (WRBLK's 128 default; the CF-3300 leaves 0 there,
; scratchpad/bdosx9_stock.out $04FE) -- with RR put back after (WRBLK advances it,
; WRRND does not), then WRRND's own success tail; a failure keeps WRBLK's A (1 full,
; 2 I/O). IX = the FCB copy (wrrnd_body set it).
wrrnd_ext:                                  ; (a record past 1 MB: wrblk_huge answers
                push    ix                  ; disk full, A = 1, passed on below -- D-BDOSSEQ)
                pop     de                  ; DE = the FCB copy (wrblk_body's input)
                xor     a
                ld      (ix+14), a
                ld      (ix+15), a          ; record size 0 = 128
                ld      l, (ix+33)
                ld      h, (ix+34)
                push    hl                  ; RR (r0 / r1: the record WRRND names)
                ld      hl, 1
                call    wrblk_body          ; A = 0 ok / 1 disk full / 2 I/O
                pop     hl
                call      set_rr          ; RR as it was
                or      a
                jr      nz, rrnd_finish
                jr      wrrnd_ok

