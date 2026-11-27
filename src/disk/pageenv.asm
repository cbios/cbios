; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; page-0 DOS environment: lay_page0_env + inter-slot vector handlers + int_h trampoline (Interface B)
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- step 6: the page-0 MSX-DOS environment (provider-oracle-scope.md §8.4/§8.6)
; With RAM switched into page 0 the page-0 BIOS ROM is gone, so the standard
; inter-slot primitives the boot code + MSXDOS.SYS reach through ($000C RDSLT,
; $0014 WRSLT, $001C CALSLT, $0024 ENASLT, $0030 CALLF) and the maskable-interrupt
; vector ($0038) no longer exist. lay_page0_env writes a JP-vector table into
; those page-0 cells pointing at OUR OWN handlers. The SYNCHRONOUS handlers (RDSLT/
; WRSLT/CALSLT/ENASLT/CALLF) live here in page 1 and are only ever reached while page 1
; is the disk ROM, so they survive every page-0 change. The MASKABLE-INTERRUPT handler
; is the EXCEPTION (A-3): $0038 fires asynchronously, including AFTER COMMAND.COM
; reclaims page 1 as the TPA (wa_seg_ram) — so it must NOT live in page 1. It is
; installed into always-mapped high RAM (INT_H_HIRAM) and $0038 points there.
;
; CLEAN-ROOM: the vector SHAPE is the documented MSX inter-slot layout (MSX2 TH
; ch.2; the §8.6 black-box trace confirmed live cells $000C/$001C/$0024/$0030/
; $0038 in the working boot's page 0). The reference kernel's $DDxx targets are
; opaque and never read/replicated — our targets are our own RAM-resident code.
;
; KEY ADVANTAGE (§8.4): our disk ROM stays in page 1 throughout boot, so the disk
; path needs no real inter-slot switch — the $0030 CALLF handler routes H.PHYD's
; CALLF straight to our DSKIO with a direct call. RDSLT/WRSLT/CALSLT operate on
; the currently-mapped memory (sufficient while the boot's targets — our page-1
; ROM and page-3 work area — stay mapped); ENASLT does a real primary-slot switch.
; If the Tier-1 trap (a3) shows the boot reaches a genuinely-unmapped slot through
; one of these, that handler is deepened then — never by reading the boot code.
lay_page0_env:
                ; A-3: install the maskable-interrupt handler into ALWAYS-MAPPED high RAM
                ; ($DDAE) BEFORE pointing $0038 at it, so the first interrupt after the
                ; COMMAND.COM handoff (page 1 -> RAM) has a live handler instead of $FF
                ; (tier2-a3-spec.md). Interrupts are OFF here; page 1 (LDIR source) is
                ; mapped; page 3 (dest) is RAM. Clobbers A/BC/DE/HL like the loop below.
                ; D-TPAP1: and the slot bridges + the $F368 table (install_dos_env,
                ; kernel.asm) -- every DOS entry, every host.
                call    install_dos_env
                ld      hl, p0_env_tab
lpe_loop:
                ld      e, (hl)
                inc     hl
                ld      d, (hl)
                inc     hl              ; DE = page-0 cell address
                ld      a, d
                or      e
                ret     z               ; cell 0 -> table end
                ld      a, $C3          ; JP opcode
                ld      (de), a
                inc     de
                ld      a, (hl)         ; target low
                ld      (de), a
                inc     hl
                inc     de
                ld      a, (hl)         ; target high
                ld      (de), a
                inc     hl
                jr      lpe_loop
; p0_env_tab (cell, handler pairs; terminated by a 0 cell) now lives in the free
; tail (runtime.asm) — M22a freed $41D6..$41F2 (the table's own span + the
; $0030 entry's 3-byte `jp callf_body_body` veneer) to make room for the BDOS
; $0C CPMVER canonical page-1 entry at $41EF, which the shared kernel calls
; directly. Previously un-wired: the CALL landed on the table's own zero-
; terminator byte and NOP-slid into the CALLF veneer, which misparsed 3 kernel
; CODE bytes as a CALLF operand and warm-booted the machine by GTIME
; (docs/tier2-m22-cpmver-spec.md §4.1). The table's $0030 entry now points
; DIRECTLY at callf_body_body (label reference, position-free); moving the
; table or deleting the veneer changes nothing about lay_page0_env's own loop.
; srd_same -- 🔴 D-SEQMIX3 (2026-11-14), srd_pos's "is the iterator there already?": Z only
; when BDOS_SEQREC is this record AND the iterator's file is this FCB's (its first
; cluster, +26, = FAT_FIRSTCLUS; two files never share one). The iterator is one: with A
; and B open, SEQREAD A after FOPEN B saw CR 0 = BDOS_SEQREC 0 and read on from B's --
; $AA / $BB where the CF-3300 gives A's $11 / $22 (scratchpad/seqmix_inter1.out); the
; re-position (rrnd_pos_hl) finds this FCB's file (D-RDRNDMULTI). In: HL = the record,
; IX = the FCB. Out: Z / NZ, HL kept; DE clobbered. The second `sbc` needs no `or a`:
; the `add hl,de` before it ran on HL = 0, so it cannot carry.
srd_same:
                ld      de, (BDOS_SEQREC)
                or      a
                sbc     hl, de
                add     hl, de              ; (Z from the sbc survives `add hl`)
                ret     nz
                push    hl
                ld      l, (ix+26)
                ld      h, (ix+27)
                ld      de, (FAT_FIRSTCLUS)
                sbc     hl, de
                pop     hl
                ret
                ds      $41EF - $, $00      ; pad (frees the old table tail + callf_body jp)
k_41EF:
                jp      cpmver_body         ; $41EF: BDOS $0C CPMVER canonical entry (M22a)
; (moved here from kernel.asm by the D-DSKFMT defrag, 2026-11-09)
; hkn_up: upcase A. Six bytes of our own rather than a call-back for one
; character -- own-design, and the same fold main's `upcase` does.
hkn_up:
                cp      'a'
                ret     c
                cp      'z' + 1
                ret     nc
                sub     32
                ret
                ds      $41FD - $, $00      ; anchor the canonical address
                jp      k_41FD              ; $41FD: COMMAND.COM-load kernel veneer
; 🐌 D-SAVEFAT (2026-11-06), in this hole because the SAVE engine's region had 11 B:
; a tokenised SAVE is ONE call, so it holds the FAT sector across every cluster's
; mark and link (bit 2 of FAT_DEFER, set by disk_write_begin) and writes it once --
; D-RECSLOW's W3b-within-one-call. The mark's fwe_finish turns bit 0 into "dirty",
; so fdh_rehold re-asserts bit 0 before each mark and link while the hold is on. Any
; flush ends the hold (frl_flush / fat_write_buf_allfats clear the byte): a
; FAT-sector change costs speed, never correctness. A only.
fdh_rehold:
                ld      a, (FAT_DEFER)
                bit     2, a
                ret     z
                or      1
                ld      (FAT_DEFER), a
                ret
; wrblk_off24 -- (MOVED here by D-DISKCARVE6, 2026-11-17) WRBLK_REC (24 bits) := RR x RS, its low three bytes: the same product.
; 💰 D-DSKFMT carve 36 (2026-11-10): it had its own RAM shift-and-add loop.
wrblk_off24:
                call    wmr_core
                ld      (WRBLK_REC), hl
                ld      a, e
                ld      (WRBLK_REC + 2), a
                ret
                ds      $4217 - $, $00      ; pad to rdslt_h (net-zero: stays $4217)

; --- D-TPAP1 (2026-10-23): THE PAGE-0 INTER-SLOT ENTRIES, BRIDGED TO THE MAIN BIOS ---
; CLEAN-ROOM: the entry addresses and their register contract are MSX2 TH ch.2; the
; CF-3300 is a black-box oracle (our own .COM, slot state and answers); own code.
; MSX-DOS runs a program with RAM in page 1 (the CF-3300: slot 3's subslots $00 at
; the program's first instruction), so the $000C / $0014 / $001C / $0024 / $0030
; entries cannot live in this ROM. They used to (rdslt_h .. enaslt_h here, the CALLF
; and CALSLT bodies in the kernel), reading and calling whatever was mapped -- a slot
; argument was never honoured, and CALLF only reached DSKIO. Now each is a stub in
; page-3 RAM that pages the main BIOS into page 0 and enters the BIOS's OWN RDSLT /
; WRSLT / CALSLT / ENASLT / CALLF with the caller's registers; br_back puts page 0
; back. ENASLT of PAGE 0 is done here (br_p0in): the BIOS cannot switch the page it
; runs in. Three pieces -- the int_h_hiram block (runtime.asm), this one, br2
; (fat.asm) -- LDIR'd end to end at INT_H_HIRAM by install_dos_env; `jp` across
; pieces, `jr` within one. Measured with probes/disk/p1vec.asm (scratchpad/
; p1vec_run.out): the CF-3300 runs all five from the TPA.
; D-TPAP1: where each piece of the page-3 slot bridges lands in RAM. All here, ABOVE
; every use: pasmo 0.5.3 assembles a use of a label-derived equ above its own line
; as 0 (D-PASMO053, tools/check_alias_order.py).
BR_MAININ       equ     INT_H_HIRAM + (br_mainin_tmpl - int_h_hiram_tmpl)
BR_P0IN         equ     INT_H_HIRAM + (br_p0in_tmpl - int_h_hiram_tmpl)
BR_BACK         equ     INT_H_HIRAM + (br_back_tmpl - int_h_hiram_tmpl)
BR_BACK_DI      equ     INT_H_HIRAM + (br_back_di_tmpl - int_h_hiram_tmpl)
BR1_RAM         equ     INT_H_HIRAM + (int_h_hiram_end - int_h_hiram_tmpl)
BR_RDSLT        equ     BR1_RAM + (br_rdslt_tmpl - br1_tmpl)
BR_WRSLT        equ     BR1_RAM + (br_wrslt_tmpl - br1_tmpl)
BR_CALSLT       equ     BR1_RAM + (br_calslt_tmpl - br1_tmpl)
BR_ENASLT       equ     BR1_RAM + (br_enaslt_tmpl - br1_tmpl)
BR2_RAM         equ     BR1_RAM + (br1_end - br1_tmpl)
BR_CALLF        equ     BR2_RAM + (br_callf_tmpl - br2_tmpl)
WA_SEG          equ     BR2_RAM + (br2_end - br2_tmpl)  ; the $F368 hook bodies: right after
                                ; br2 (install_dos_env's DE runs on) -- no width here: WA_SEG_ROM
                                ; shares the address and declares none, so one would only DISPUTE
WA_SEG_ROM      equ     WA_SEG + (wa_seg_rom_tmpl - wa_seg_rom_tmpl)   ; = WA_SEG
WA_SEG_RAM      equ     WA_SEG + (wa_seg_ram_tmpl - wa_seg_rom_tmpl)
WA_SEG_SET      equ     WA_SEG + (wa_seg_set_tmpl - wa_seg_rom_tmpl)   ; A = page-1 bits; [bc][af] pushed
BR3_RAM         equ     WA_SEG + (wa_seg_end_tmpl - wa_seg_rom_tmpl)
BR_BDOS         equ     BR3_RAM + (br_bdos_tmpl - br3_tmpl)
BR_P1SV         equ     BR3_RAM + (br_p1sv_tmpl - br3_tmpl)
BR5_RAM         equ     BR3_RAM + (br3_end - br3_tmpl)
BR_GO           equ     BR5_RAM + (br_go_tmpl - br5_tmpl)
BR_FRAME        equ     BR5_RAM + (br_frame_tmpl - br5_tmpl)
BR_TGT          equ     BR5_RAM + (br_tgt_tmpl - br5_tmpl)
BR_FRM          equ     BR5_RAM + (br_frm_tmpl - br5_tmpl)
BR_HL           equ     BR5_RAM + (br_hl_tmpl - br5_tmpl)
BR_CFOP         equ     BR5_RAM + (br_cfop_tmpl - br5_tmpl)
; D-CALSLTP0 (2026-11-08): br6, the page-1 / page-2 CALSLT, right after br5 -- $DF18..,
; inside our own DOS reservation (DOS_RESV_TOP $DF93; $DF1C..$DF92 watched: 0 writes).
BR6_RAM         equ     BR5_RAM + (br5_end - br5_tmpl)
BR_CALS         equ     BR6_RAM + (br_cals_tmpl - br6_tmpl)
BC_FIELD        equ     BR6_RAM + (bc_field_tmpl - br6_tmpl)
BC_JPIX         equ     BR6_RAM + (bc_jpix_tmpl - br6_tmpl)
BC_TMP          equ     BR6_RAM + (bc_tmp_tmpl - br6_tmpl)
br1_tmpl:
br_rdslt_tmpl:
                push    hl
                ld      hl, $000C           ; RDSLT
                jp      BR_GO
br_wrslt_tmpl:
                push    hl
                ld      hl, $0014           ; WRSLT
                jp      BR_GO
br_calslt_tmpl:
                jp      BR_CALS             ; CALSLT: br6 (D-CALSLTP0) -- a page-1 / page-2
                                            ; target with page 0 the caller's RAM
br_enaslt_tmpl:
                push    af
                ld      a, h
                cp      $40
                jr      nc, br_ena_bios_tmpl
                pop     af                  ; page 0: switch it from here
                push    bc
                push    de
                push    hl
                di
                ld      c, a
                call    BR_P0IN
                pop     hl
                pop     de
                pop     bc
                ret
br_ena_bios_tmpl:
                pop     af
                push    hl
                ld      hl, $0024           ; ENASLT, pages 1-2
                jp      BR_GO
br1_end:
; 📍 D-LOADFAT (2026-11-06): MOVED HERE from the $607B..$75A5 region, where LOAD's
; FAT cache needed the room; entered only by call / jp, so it can live in any hole.
; df_entptr -- HL = FAT_DBUF + FILES_ENTIDX*32 (the current dir entry).
; Recomputed from RAM each time so the emit path's register clobber is harmless.
df_entptr:
                ld      a,(FILES_ENTIDX)
                ld      l,a
                ld      h,0
                call    hl_x32            ; *32
                ld      de,FAT_DBUF
                add     hl,de
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; --- gtime_body - the $55DB GTIME canonical entry (M22a) -------------------------
; A := B := C := D := E := 0 — the oracle's pinned constant 00:00:00
; (clockless-MSX1 default; deliberately NOT fed by STIME's stored value, which
; would diverge from the observed oracle, §5.1 r3).
gtime_body:
                xor     a
                ld      b, a
                ld      c, a
                ld      d, a
                ld      e, a
                ret
                ds      $4257 - $, $00      ; net-zero: keep dskio at its canonical offset

