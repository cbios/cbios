; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; free-tail runtime: relocated templates + shared pg0_mainrom_* helpers + conout_body + int_h_body (Interface B)
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- shared page-0 main-ROM inter-slot helpers (CONOUT M8 + int_h A-2) ----------
; pg0_mainrom_in - save the current $A8 to PG_SV_A8 and page the main BIOS ROM into
; page 0. PORTABLE (no machine-specific hardcode): the main-ROM slot is read at runtime
; from EXPTBL[0] ($FCC1) - the standard cell holding the slot id (bit7=expanded,
; [3:2]=subslot, [1:0]=primary). Sets page-0's $A8 primary to that primary and, if the
; slot is expanded, programs its page-0 SECONDARY subslot too. Only the page-0 (and,
; transiently, page-3) $A8 fields move: page 1 (our disk ROM) keeps running this code
; and page 2 (the stack) survives, so call/ret is safe. Caller MUST be DI and MUST pair
; with pg0_mainrom_out. Clobbers A,B,C,D,E,H,L (callers save what they need).
; CLEAN-ROOM: EXPTBL/$FCC1, SLTTBL/$FCC5, the $A8 primary register and the $FFFF
; secondary protocol are documented MSX BIOS ABI (MSX2 TH ch.2/2.4); no oracle bytes.
; NOTE: on the CF-3300 EXPTBL[0]=$00 (unexpanded), so the primary path is exercised+
; validated; the expanded sub-path is spec-derived and NOT reachable here (review queue).
; 💰 D-RNDFCB (2026-11-08): pg0_mainrom_in WAS ONLY `jp br_mainin_tmpl` (D-DISKCARVE had
; made it the bridge template's own page-0 switch, run in place); its callers now call
; br_mainin_tmpl directly and the 3-byte veneer is gone. The contract above is
; br_mainin_tmpl's.

; 💰 D-DISKCARVE3 (2026-11-08; here since D-DISKCARVE5): fac_cached_entry -- the step fat_have_free_cluster and
; getalloc_body both carried verbatim: cluster HL's FAT entry, its sector read into
; WBUF only when it is not the one FAT_WRTMP2 says is cached (FAT_FATSEC follows).
;   in: HL = the cluster ; out: DE = the 12-bit entry, CF = 1 on a read error
;   (HL / A / BC clobbered; fac_entry_from_wbuf reads FAT_BYTEIDX / FAT_PARITY, which
;   fat_locate set, not HL)
fac_cached_entry:
                call    fat_locate          ; 💰 D-DISKCARVE: HL = the entry's FAT sector, PARITY / BYTEIDX set
                ld      de, (FAT_WRTMP2)
                push    hl
                or      a
                sbc     hl, de
                pop     hl
                jr      z, fce_have         ; already loaded -> no re-read
                ld      (FAT_WRTMP2), hl    ; remember the new cached sector
                call    rd_fatsec           ; FAT_FATSEC := HL, read it into FAT_MBUF (Cy)
                ret     c
fce_have:
                jp      fac_entry_from_wbuf ; DE = entry value

; (pg0_mainrom_out is in disk/kernel.asm in front of the $75A5 pin since D-WRRNDEXT,
; whose wrapper needed this region's bytes.)

; --- conout_body - the real $5454 CONOUT (M8/8.67; char-reg fix M10/§8.80) ------
; Emit the char (in E) via the main-ROM CHPUT ($00A2). Reached from the $5454 veneer
; (jp conout_body). Pages the main ROM into page 0 (pg0_mainrom_in), calls CHPUT,
; restores (pg0_mainrom_out). DI spans the window; EI on exit.
;   in:  E = char ; out: A = char, BC/DE/HL/IX/IY preserved
; CHAR REGISTER = E (not A). The $5454 CONOUT contract passes the char in E: the
; relocated MSX-DOS kernel's per-char console output (caller $D88A) loads E=char and
; leaves A=0, so reading A emitted $00/garbage for ALL COMMAND.COM-phase + A>-prompt
; output (banner/date/prompt rendered as constant tiles). The early MSXDOS.SYS sign-on
; (caller $0320) happens to pass the char in BOTH A and E, so the old A-read worked
; there and HID the bug -- the exact "early works, COMMAND fails" discriminator. E is
; the register common to BOTH callers. Black-box proven: $7922-entry callseq shows E
; spelling "MSX-DOS version 1.03" (early) and "Sun 84-01-01"/"A>" (kernel), A=0.
; CLEAN-ROOM: E=char is the MSX-DOS BDOS CONOUT (func 2) ABI convention; no oracle bytes.
;
; M22a: $5454 is ALSO the canonical BDOS $06 DIRIO entry; the kernel routes both
; funcs here (tier2-m22-cpmver-spec.md §4.7/§6.3). C holds the BDOS function code
; ONLY on calls routed through the kernel's common BDOS dispatch (ret=$D88A, the
; same discriminator §3 uses to confirm "called directly by the kernel
; dispatcher" for every other M22a entry) — DIRECT callers that reach $5454
; without going through that dispatch (COMMAND.COM's own column-padding print
; loop, or the early $0320 MSXDOS.SYS sign-on documented above) can leave
; ANYTHING in C, since it's just whatever register a caller-internal loop
; happened to be using. Self-caught in verification: checking C alone made a
; padding loop's counter passing through C=$06 misfire into the DIRIO branch on
; a plain space character, corrupting DIR's screen output (no spec section —
; a plain safety gap in the discriminator, not a contract question). So the
; return address is peeked FIRST (without disturbing the stack) and C is only
; trusted when ret==$D88A; the published DIRIO input-vs-output convention
; (E=$FF is input) is checked only after that.
; The caller's AF is pushed FIRST, before any of this touches flags: the
; original code's first instruction (`ld a,e`) never affected flags, so every
; existing caller relies on ITS OWN entry flags surviving the call untouched
; (the M8 kernel path tests them, e.g. for line-wrap bookkeeping).
conout_body:
                push    af                  ; preserve caller AF/flags BEFORE any check
                push    hl                  ; HL scratch for the return-address peek
                ld      hl, 4
                add     hl, sp              ; HL -> caller's return address (2 pushes = +4)
                ld      a, (hl)
                cp      $8A
                jr      nz, conout_not_dispatch
                inc     hl
                ld      a, (hl)
                cp      $D8
                jr      nz, conout_not_dispatch
                ; ret == $D88A: C reliably holds the BDOS func code here (§4.7).
                ld      a, c
                cp      $06
                jr      nz, conout_not_dispatch
                ld      a, e
                inc     a                   ; E = $FF? (D-DISKREREAD: not `cp $FF`, 1 B --
                jp      nz, conout_dirio_out ; both continuations reload A) -- DIRIO OUTPUT: raw (D-CONCOL)
                pop     hl                  ; restore caller's HL
                pop     af                  ; restore caller's AF for dirio_in_body
                jr      dirio_in_body
; M22b slice 2 (TAB expansion + $F237 logical-column bookkeeping). The per-char emit
; (conout_emit_e) is RELOCATED above the FDC hole $7FB8-$7FBF (it doesn't fit below);
; conout_tab stays HERE, below the hole (jr-reachable), and calls up. This emit tail is
; a shrink vs the old 32-byte inline CHPUT, but p0_env_tab is PINNED at its HEAD address
; $7FB7 (see its tail block) so the shrink does NOT slide its critical $0030/$0038 vector
; entries into the FDC register window -- lay_page0_env reads p0_env_tab THROUGH that
; window, so a corrupted vector there = corrupt page-0 $0038 = boot crash (root-caused
; 2026-07-04). The dispatch preamble above is byte-stable. Contract: char in E, A:=E.
conout_not_dispatch:
                pop     hl                  ; restore caller's HL; caller AF stays on the stack
                ld      a, (PRN_ECHO)       ; D-BUFINCTL: Ctrl-P's printer echo -- the char to the
                dec     a                   ; printer too. EXACTLY 1: under BASIC the cell keeps
                call    z, lstout_body      ; the RAM fill $FF (M27). lstout_body keeps BC/DE/HL.
                ld      a, e                ; char arrives in E ($5454 CONOUT contract)
                cp      $09                 ; TAB? -> expand to the next 8-column stop
                jr      z, conout_tab       ; conout_tab is just below (jr-reachable)
                call    conout_emit_e       ; CHPUT the char + $F237 column bookkeeping (above hole)
                pop     af                  ; restore caller AF/flags (pushed at entry)
                ld      a, e                ; return A := E (the emitted char, §5.1)
                ret
; conout_tab ($09): emit spaces to the next 8-column stop (>=1). conout_emit_e does the
; col++ per space and returns A = the new column, so the loop re-tests A & 7. The stop is
; the LOGICAL column ($F237), not the screen cursor (§5.3 col-34 = 6 spaces). Stock exits
; A=$00 (§8.1, capture at $D88A: AF=$0054, $F237=$08 after a col-1 tab). do{ emit ' ';
; col++ }while(col&7).
conout_tab:
                push    de                  ; preserve caller DE (loop clobbers E with $20)
conout_tab_loop:
                ld      e, $20              ; a space
                call    conout_emit_e       ; CHPUT ' ' + col++ ; returns A = the new column
                and     $07                 ; stop at an 8-column boundary
                jr      nz, conout_tab_loop
                pop     de                  ; restore caller DE
                pop     af                  ; restore caller AF/flags (pushed at conout_body entry)
                ld      a, 0                ; exit A := $00 (stock's post-TAB value, §8.1)
                ret

; --- dirio_in_body - the $5454/$06 DIRIO input direction (M22a) ------------------
; Non-blocking read: CHSNS ($009C) — if a key is queued, CHGET ($009F) it (no
; echo, per the published DIRIO contract) and return A=char; else A=$00. B/C/D/E
; must survive untouched (§5.3); only A changes (HL is overwritten by the
; kernel's own exit mirror regardless, §5.2, so it needs no attention here).
; CLEAN-ROOM: published DIRIO contract (map.grauw.nl) + CHSNS ($009C, the same
; documented MSX BIOS class as CHGET/CHPUT already used here); no oracle bytes.
dirio_in_body:
                push    bc
                push    de
                push    hl
                di
                call    br_mainin_tmpl
                call    $009C               ; CHSNS: A=$00 empty / nonzero ready (BIOS ABI)
                or      a
                jr      z, dirioin_done     ; no key queued: A = 0 already
; 💰 D-DSKFMT carve 15: innoe_body (kernel.asm) jumps in here after its own entry.
dirio_chget:
                call    $009F               ; CHGET: A := char (no echo, DIRIO contract)
; the shared exit of the console bodies: page the BIOS back out, keep A.
dirioin_done:
                push    af                  ; stash result across mainrom_out (clobbers A)
                call    pg0_mainrom_out
                ei
                pop     af
                pop     hl
                pop     de
                pop     bc
                ret

; --- const_body - the $543C CONST canonical entry (M22a) -------------------------
; Non-blocking poll: CHSNS ($009C) -> A=$FF ready / A=$00 empty (the published
; CONST contract; CHSNS itself returns nonzero/zero, normalized to $FF/$00 here).
; B/C/D/E preserved.
; CLEAN-ROOM: published CONST contract (map.grauw.nl) + CHSNS; no oracle bytes.
const_body:
                push    bc
                push    de
                push    hl
                di
                call    br_mainin_tmpl
                call    $009C               ; CHSNS: A=$00 empty / nonzero ready (BIOS ABI)
                add     a, $FF              ; CF := (A != 0)
                sbc     a, a                ; MSX-DOS CONST contract: ready -> A=$FF, else $00
                jr      dirioin_done        ; 💰 carve 15: dirio_in_body's exit


; (dirin_body -- gone or moved, D-DISKCARVE 2026-11-07)

; --- cpmver_body - the $41EF CPMVER canonical entry (M22a) -----------------------
; A := $22 (the published CP/M-2.2-compatibility version constant, map.grauw.nl
; _CPMVER), B := 0. HL is overwritten by the kernel's own exit mirror regardless
; ($F306 stays set — §5.2, the M20 rule inverted for this tier).
; (cpmver_body moved to the $4ECF pad, disk/kernel.asm -- D-DISKCARVE)

login_body:
                ld      a, (DRVCNT)         ; DRVCNT = $F347 = logical-drive count
                ld      b, a                ; B := DRVCNT (loop counter, temporarily)
                ld      hl, 1
                or      a
                jr      z, login_bits_done  ; DRVCNT=0 -> bitmap 0 (defensive)
login_shift:
                add     hl, hl
                djnz    login_shift
login_bits_done:
                dec     hl
                ld      a, l                ; A := (1<<DRVCNT)-1 (fits one byte for DRVCNT<8)
                ld      b, 0                ; B := 0 (LOGIN exit contract)
                ret

; --- stime_body - the $55E6 STIME canonical entry (M22a) -------------------------
; A := 0 (success); B := H (hours); C := L (minutes) — the published STIME
; contract's minimal valid-input path (map.grauw.nl); D/E (seconds/hundredths)
; untouched, matching the pinned oracle (§5.1 r2). Range validation (A:=$FF on
; out-of-range input) is unimplemented — the exerciser never sends invalid
; input and stock's invalid-path was never observed (§8.2 residual).
; (stime_body / gtime_body moved to the $4ECF pad, disk/kernel.asm -- D-DISKCARVE)

; (verify_body -- gone or moved, D-DISKCARVE 2026-11-07)

; --- dskrst_body - the $509F DSKRST canonical entry (M22a) -----------------------
; Reproduces the pinned $50A9 continuation-stub contract (A=0; (W50A9_WRKB):=0;
; DE=IX=$F1AA) by a plain tail-jump into the existing stub — replacing the
; former pad-slide "luck" (§4.4) with an explicit wired entry. HL is overwritten
; by the kernel's own exit mirror regardless ($F306 stays set), so the stub's
; own HL:=$F359 is immaterial here. Real flush semantics are unverifiable (no
; dirty buffers at these call sites) — the register contract is the observable
; surface (§8.4 residual).
; (dskrst_body -- gone or moved, D-DISKCARVE 2026-11-07)

; --- conin_line_body - the $50E0 CONIN line routine (M13; tier2-conin-spec.md v3) ---
; Reimplements the documented BDOS func-$0A buffered-line read, clean-room, from the
; published CHGET ($009F) contract + the CONOUT veneer (echo) + the func-$0A buffer
; layout ([DE+0]=max/[DE+1]=count/[DE+2..]=chars). Reached from the $50E0 veneer
; (jp conin_line_body). NO return register (per the M12d pin) -- the result lives in
; the buffer. One CHGET per char (PIN B: 7 calls for a 6-char + CR line): CR ends the
; line, BS backs up one char (buffer edit only), else echo via conout_body and store.
; D-BUFINCTL (2026-11-26): under MSX-DOS the other control keys go to cinl_ctl (disk/kernel.asm) --
; Ctrl-C / P / N / U / ESC / X / R act as the CF-3300's, a stored one echoes as ^E; under BASIC
; they are stored as before.
; CLEAN-ROOM: CHGET/CHPUT/func-$0A buffer are the published/cross-vendor BDOS ABI; the
; stock $50E0 routine's internals were never read (only its entry regs, black-box).
;   in:  DE = buffer base ; out: buffer filled (func-$0A layout); BC/DE/HL preserved
conin_line_body:
                push    bc
                push    hl
                ld      (CONIN_BUF), de     ; stash buffer base (pg0_mainrom_* clobbers D/E)
                ld      a, (de)
                ld      (CONIN_MAX), a      ; max length = buf[0]
                xor     a
                ld      (CONIN_COUNT), a    ; running fill count starts at 0
cinl_getchar:
                call    cinl_key            ; A = the key read
                cp      $0D                 ; CR -> line done
                jr      z, cinl_cr
                cp      $08                 ; BS -> edit back one char
                jr      z, cinl_bsp
                cp      $20                 ; D-BUFINCTL: the other control keys (disk/kernel.asm)
                jp      c, cinl_ctl
                jr      cinl_store
cinl_bsp:
                ld      hl, CONIN_COUNT
                ld      a, (hl)
                or      a
                jr      z, cinl_getchar     ; nothing to erase -> ignore
                dec     (hl)
                ; 🔴 D-BUFINECHO (2026-11-17): and UN-ECHO it -- BS, space, BS. The CF-3300's
                ; BUFIN erases the character on screen; this left it (`Helz` BS `lo` showed
                ; "Helzlo", `Helz` BS Return "Helz": scratchpad/XK_stock.out, XK2_*.out).
                ld      hl, cinl_bs
                call    fmt_pr
                jr      cinl_getchar
cinl_store:
                ld      c, a                ; C = the char to store/echo
                ld      hl, (CONIN_MAX)     ; L = max, H = current fill count (CONIN_COUNT
                ld      a, l                ; = CONIN_MAX + 1) -- D-DISKREREAD (2026-11-26):
                cp      h                   ; one word load, not two bytes through B: 3 B
                jr      z, cinl_getchar     ; buffer full -> drop the char (no room)
                ld      de, (CONIN_BUF)
                inc     de
                inc     de                  ; DE -> buf[2 + count]
                ld      l, h
                ld      h, 0
                add     hl, de
                ld      (hl), c             ; store the char
                ld      hl, CONIN_COUNT
                inc     (hl)                ; count++
                ld      e, c                ; echo (E = char): conout_body, or ^E for a control
                call    cinl_echo           ; key under MSX-DOS (D-BUFINCTL)
                jr      cinl_getchar
cinl_cr:
                ld      e, a                ; D-BUFINECHO: Return echoes CR -- the CF-3300 leaves
                call    conout_body         ; the cursor at column 0 of the input line (XK_*)
cinl_done:
                ld      de, (CONIN_BUF)
                inc     de                  ; DE -> buf[1] = count
                ld      a, (CONIN_COUNT)
                ld      (de), a
                inc     de                  ; DE -> buf[2] (first char slot)
                ld      l, a
                ld      h, 0
                add     hl, de              ; HL -> buf[2 + count]
                ld      (hl), $0D           ; M16: terminate with CR immediately after the
                                             ; last stored char (buf[2] when count=0) --
                                             ; observed on stock's own buffer content;
                                             ; COMMAND.COM's date-reply parser scans from
                                             ; buf[2] for this rather than trusting count
                                             ; alone (tier2-conin-spec.md §6)
                ld      a, (hl)             ; D-F37DCONIN: A = $0D, as both references answer
                                            ; BUFIN from $F37D (CONIN.md R-BC-BUFIN); was the count
                pop     hl
                pop     bc
                ret
; cinl_key -- one key through CHGET ($009F), the BIOS paged in and out around it. Out: A = B = the key.
cinl_key:
                di                          ; no interrupt while the BIOS is half-mapped
                call    br_mainin_tmpl
                call    $009F               ; CHGET - wait for + return a char in A
                ld      b, a                ; B = the char (pg0_mainrom_out clobbers A)
                call    pg0_mainrom_out     ; restore page 0
                ei
                ld      a, b
                ret
; (cinl_bs -- BackSpace's un-echo -- moved in front of $543C by D-F37DCONIN, for the A = $0D above)

; --- int_h_body - REMOVED 2026-09-09 (D-INTHDEAD) ---------------------------------
; The OLD page-1 $0038 handler (A-2/A-2b), superseded by A-3 and kept "pending
; removal" ever since. Removed on Joost's rule: *"as long as we have proof via the
; oracle we can remove the stale ref"*, and the proof is
; `disk/docs/provider-oracle-scope.md` §8.70 O-2 — a `pctrace --arm 0xDDAE --stock`
; reading of the STOCK machine showing its $0038 handler save registers, switch to a
; private stack, and inter-slot CALSLT to the main-ROM KEYINT ($0038 entry -> body
; $0C3C, calling H.KEYI $FD9A + H.TIMI $FD9F). That is the contract this body
; implemented, it is measured from the oracle, and `int_h_hiram_tmpl` below still
; implements it.
;
; 🔴 IT WAS NOT LINK-DEAD, AND THE FILING SAID IT WAS. `disk/pageenv.asm` still
; carried `jp int_h_body` in the page-1 `int_h` trampoline, so deleting the body
; alone would not have assembled. What made the removal safe is the VECTOR TABLE,
; not the header: `p0_env_tab` (disk/kernel.asm) reads
; `dw $0038, INT_H_HIRAM` — with its own comment "(NOT page-1 int_h: page 1 is
; reclaimed by the TPA)". So $0038 never points at the trampoline, the trampoline
; is unreachable, and both go together. The trampoline's SLOT stays six bytes so
; dskio does not shift.
;
; --- int_h_hiram_tmpl - the LIVE $0038 handler, A-3 + A-5 (tier2-a5-spec.md) ------
; Relocated by a plain LDIR into INT_H_HIRAM ($DDAE) page-3 high RAM during init, so it
; survives COMMAND.COM reclaiming page 1 (wa_seg_ram). RELOCATABLE: straight-line, ONLY a
; PC-relative jr + the FIXED call $0038 (no template-relative call/jp, same rule as
; res_print_tmpl). pg0_mainrom_in/out are INLINED (their `ret z` early-out becomes `jr z`);
; all data refs ($A8/$FFFF, EXPTBL/SLTTBL, PG_SV_A8, $0038) are absolute fixed addresses that
; survive relocation. Pages the main ROM into PAGE 0 only (page 1 = the TPA is never touched),
; calls the main-BIOS KEYINT, restores, EI, RET. BIOS-agnostic (EXPTBL[0]); the expanded-slot
; path is spec-derived (CF-3300 is unexpanded). CLEAN-ROOM: our own code/address; $DDAE is an
; oracle WHERE, never stock's bytes.
;
; A-5 (tier2-a5-spec.md): runs on the CALLER's (interrupted code's) stack — the standard MSX
; interrupt convention — NOT a private stack. The A-2b private stack was a guard against an
; interrupt firing with a corrupt caller SP (the primary derail); A-3 fixed that derail, so
; the caller SP is always valid here. The 48-byte private stack was in fact TOO SMALL for the
; main-ROM KEYINT (~60 B, unbounded via H.TIMI/H.KEYI hooks): KEYINT overflowed it DOWNWARD
; into the WA_SEG trampoline laid out just below it ($E795+), corrupting the $F368/$F36B
; segment switch and hanging the COMMAND.COM handoff (harness, 2026-06-26). Caller stacks
; here (kernel $DBFA, COMMAND.COM $F513…) have ample room, so this is both correct and safe.
int_h_hiram_tmpl:
                push    af
                push    bc
                push    de
                push    hl
                di
                call    BR_MAININ           ; the main BIOS into page 0 (clobbers A..L)
                call    $0038               ; main-ROM KEYINT: ack + H.KEYI + H.TIMI + kb + JIFFY
                di                          ; close KEYINT's internal EI before un-mapping
                ld      a, (PG_SV_A8)       ; page 0: DOS's RAM again
                out     ($A8), a
                pop     hl
                pop     de
                pop     bc
                pop     af
                ei
                ret
; br_mainin / br_p0in (D-TPAP1) -- pg0_mainrom_in's switch, in RAM: the main BIOS
; (br_mainin) or the slot in C (br_p0in, ENASLT's page-0 case) into PAGE 0, the
; caller's $A8 kept in PG_SV_A8. Caller DI; clobbers A..L. The expanded path parks
; page 3 on the target's primary to reach its $FFFF, so from RAM it holds only when
; that primary IS page 3's (the RAM slot 3-0); an expanded BIOS slot elsewhere would
; unmap this code (neither target machine has one).
br_mainin_tmpl:
                ld      a, (EXPTBL)
                ld      c, a
br_p0in_tmpl:
                in      a, ($A8)
                ld      (PG_SV_A8), a       ; save full primary-slot config (restore key)
                ld      a, c
                and     $03                 ; A = the slot's primary
                ld      b, a
                ld      a, (PG_SV_A8)
                and     $FC                 ; clear page-0 primary field
                or      b
                out     ($A8), a            ; primary now in page 0 (pages 1/2/3 kept)
                bit     7, c                ; expanded?
                ret     z
                ld      a, c
                rrca
                rrca
                and     $03                 ; A = subslot S
                ld      e, a
                ld      hl, SLTTBL
                ld      a, b
                add     a, l                ; SLTTBL aligned, P<4 -> no page crossing
                ld      l, a                ; HL = &SLTTBL[P]
                ld      a, (hl)
                and     $FC                 ; clear page-0 subslot field
                or      e
                ld      (hl), a             ; update the SLTTBL[P] mirror
                ld      e, a                ; E = new secondary value for $FFFF
                ld      a, b
                rlca
                rlca
                rlca
                rlca
                rlca
                rlca                        ; P << 6
                ld      d, a
                ld      a, (PG_SV_A8)
                and     $3C                 ; keep pages 1/2 ; clear page-0 + page-3 fields
                or      b                   ; page-0 primary = P
                or      d                   ; page-3 primary = P (reach P's $FFFF expander)
                out     ($A8), a
                ld      a, e
                ld      ($FFFF), a          ; P's secondary: page-0 subslot = S
                ld      a, (PG_SV_A8)
                and     $FC                 ; restore page-3 (& 1/2) primaries; clear page 0
                or      b                   ; page-0 primary = P (now subslot S)
                out     ($A8), a
                ret
; br_back (D-TPAP1) -- where a bridged main-BIOS entry returns (br_go's frame, or
; via the `jp` after BR_CFOP). The caller's $A8 rides on the stack, not in
; PG_SV_A8: a callee may page the BIOS in itself and reuse that cell. Only page 0
; goes back (ENASLT's pages 1-2 stay as the BIOS set them); the interrupt state is
; the callee's, as the BIOS leaves it.   in: stack [saved $A8 in H][caller's return]
br_back_tmpl:
                ex      (sp), hl            ; H = the caller's $A8
                push    af                  ; the callee's answer
                ld      a, i                ; P/V = IFF2
                jp      pe, BR_BACK + (brb_iff - br_back_tmpl) ; the RAM copy (absolute)
                ld      a, i                ; D-IFFREAD2: a false 0 is possible, ask again
brb_iff:        push    af
                di
                ld      a, h
                and     $03
                ld      l, a                ; L = page 0's primary before the call
                in      a, ($A8)
                and     $FC
                or      l
                out     ($A8), a            ; page 0: DOS's RAM again
                ; 🔧 D-SDSBOOT (2026-11-07): ENASLT's A is the $A8 it set -- and the BIOS
                ; set it with page 0 still the BIOS, so the caller would get $FC where
                ; the CF-3300 returns the final value ($FF with every page RAM: a disk's
                ; boot code reads it, scratchpad/sdsboot_ena_zb3.out). The bridged
                ; entry is BR_TGT; RDSLT's and CALSLT's A are left alone.
                ; RDSLT and WRSLT the same way: their D is the $A8 at entry (and WRSLT's
                ; A too) -- the CF-3300 hands back the caller's, the bridge's BIOS call
                ; saw $FC (sdsboot_ena_zb4.out, $C092: DE $FF98 there, $FC98 here).
                ld      l, a                ; L = the $A8 just written
                ld      a, (BR_TGT)
                cp      $24                 ; ENASLT: A
                jr      z, brb_a
                cp      $14                 ; WRSLT: D and A
                jr      z, brb_wr
                cp      $0C                 ; RDSLT: D
                jr      nz, brb_keep
                ld      d, l
                jr      brb_keep
brb_wr:
                ld      d, l
brb_a:
                ld      a, l
                ld      hl, 3
                add     hl, sp              ; [IFF af][callee af]: +3 = the callee's A
                ld      (hl), a
brb_keep:
                pop     af
                jp      po, BR_BACK_DI      ; interrupts were off: leave them off
                pop     af
                pop     hl
                ei
                ret
br_back_di_tmpl:
                pop     af
                pop     hl
                ret
int_h_hiram_end:

; --- dos_handoff — DOS-only $F338 default + the step-7 "load the system" call ---
; COMMAND.COM's startup branches on $F338 (observed: a read of $F338 at PC $C26B):
; 0 = "no AUTOEXEC -> prompt", nonzero -> the wrong path (SELDSK/loop). Stock's disk
; ROM clears $F338 at boot; ours never did. We default it to 0 just before handing to
; the boot sector's step-7 entry. $F338 is dual-purpose (a $C9 RET BASIC hook stub on
; C-BIOS vs $00 for DOS), so we SAVE the host value and RESTORE it if the call returns --
; which only a non-system / data disk does (a real DOS disk JPs into MSXDOS.SYS and never
; returns, so the 0 persists to COMMAND.COM's read). The save uses the stack: on the
; no-return DOS path the push is simply abandoned (MSXDOS.SYS resets SP). Called from
; boot_sig_ok with IX=$F195 and the page-0 env laid; replaces the inline `scf; call
; BOOT_ENTRY` there -- net-zero in that $41FD-pad-packed region. (tier2-f338-default-spec.md)
; Also defaults the date-format config cells $F30D/$F30E (M9 date slice): COMMAND.COM
; reads $F30E at PC $CDA7 to format the boot date; ours leaves them $FF
; (uninit) so the date prints malformed and the prompt loops. Stock's disk ROM defaults
; them to $F30D=01 / $F30E=00 at boot (observed black-box; clean-room, no stock bytes).
; Same DOS-only save/restore discipline as $F338, so a returning data disk leaves the
; host's cells untouched (BIOS-agnostic). (tier2-gdate-spec.md)
; Also defaults the printer-echo state cell $F23B (M27, the LSTOUT-DIR-oddity
; follow-up): the main-BIOS RAM scan leaves it $FF at power-on; stock's disk ROM
; zeroes it once during this same boot phase (black-box-pinned write at $57BE,
; t=3.8s during a real DOS boot -- disk/docs/tier2-m27-lstout-spec.md). Left at
; $FF, COMMAND.COM's own DIR line-end code (and its prompt-cycle save/restore)
; treats it as "a list device is attached" and echoes each line's CR/LF to it
; via BDOS $05 LSTOUT -- 2 extra calls per file (documented since M19/M20 as
; "82 spurious per-file LSTOUT calls during DIR", never root-caused until now).
; A single boot-time poke to $F23B (validated: `callseq --poke 0xF23B:0x00`
; fully re-aligns the call stream, "ALIGNED, NO DIVERGENCE in 90 shared calls")
; is sufficient -- COMMAND.COM's own save/restore keeps it at 0 across the
; whole session once seeded. Same DOS-only save/restore discipline as $F338.
; D-CALLSYSTEM (2026-10-09): CALL SYSTEM (kernel.asm hk_system) enters at
; dos_handoff_keep -- the CF-3300 returns to MSX-DOS without clearing the screen
; (scratchpad/callsystem_warm.out). Both entries mark BASENT_REQ for the duration:
; a disk that returns puts the old value back. D-BOOTDATE (2026-10-21): the mark
; set here is DOS_TRY, no longer DOS_SIG -- a boot code that cannot open
; MSXDOS.SYS jumps to $4022 from INSIDE this call, and $4022 must tell that apart
; from MSX-DOS's own `BASIC` (scratchpad/bootdate_run.out: the reference asks the
; date only in the first case). DOS_SIG is set when MSX-DOS really starts: its
; MSXDOS.SYS calls $50A9 (w50a9_stub, kernel.asm), which no failing boot code does.
dos_handoff:
                call    dos_clear_screen    ; OI-3: blank BASIC banner + home cursor (below)
dos_handoff_keep:
                ld      hl, (BASENT_REQ)    ; D-CALLSYSTEM: the mark, saved ...
                push    hl
                ld      hl, DOS_TRY         ; ... and set: a boot code is being run
                ld      (BASENT_REQ), hl    ; (D-BOOTDATE; $50A9 makes it DOS_SIG)
                ld      a, ($F338)          ; save host $F338 (BASIC hook stub on C-BIOS)
                push    af
                ld      hl, ($F30D)         ; save host $F30D/$F30E (date-format config)
                push    hl
                ld      a, ($F23B)          ; save host $F23B (printer-echo state)
                push    af
                xor     a
                ld      ($F338), a          ; DOS default: $F338 = 0
                ld      ($F23B), a          ; DOS default: $F23B = 0 (no printer echo)
                ld      ($F237), a          ; DOS default: $F237 = 0 (logical console column,
                                            ;   M22b slice 2 -- seed before any output so TAB
                                            ;   expansion + $F237 parity start clean; DOS-console
                                            ;   scratch only, no host save/restore, unlike $F23B.
                                            ;   Safe: p0_env_tab is pinned, so this +3 B doesn't
                                            ;   shift it into the FDC hole)
                ld      hl, $0001
                ld      ($F30D), hl         ; DOS default: $F30D=01, $F30E=00 (date format)
                ld      de, $F368           ; D-BOOTREGS: the boot code is entered with
                ld      hl, $F323           ; DE = $F368, HL = $F323, as on the CF-3300
                scf                         ; Cy = 1 -> step-7 "load the system" entry
                ; the boot sector's step-7 call, as at hsy_boot
                ; z80flow: targets external reads=DE,HL,F
                call    BOOT_ENTRY          ; DOS disk JPs into MSXDOS.SYS (no return)
                pop     af                  ; data disk returned: recover host $F23B
                ld      ($F23B), a
                pop     hl                  ; recover host values
                ld      ($F30D), hl
                pop     af                  ; recover host $F338
                ld      ($F338), a          ; restore the dual-purpose stub for BASIC
                pop     hl                  ; D-CALLSYSTEM: the disk did not boot --
                ld      (BASENT_REQ), hl    ; the mark as it was before
                ret


; ===== M22a: relocated bodies (freed their old low-region spans for the new
; CPMVER/DIRIN/GTIME/STIME/VERIFY canonical entries) + p0_env_tab's new home ===

; fat_find_body — unchanged from its old $5457 slot (kernel.asm), relocated here
; because that slot collided with the $5462 DIRIN canonical entry
; (tier2-m22-cpmver-spec.md §4.6). Position-free: reached only by label from
; fat.asm's `fat_find: jp fat_find_body` veneer.
; ===== fdc_entloop_body / fdc_useslot_body + p0_env_tab: RELOCATED OUT =========
; (2026-07-04, FDC-window P0 fix.) These three position-free blocks used to sit HERE
; in the page-1 tail. The National WD2793 FDC registers ($7FB8-$7FBF, equates.inc)
; are memory-mapped AND mirrored x8 across the WHOLE $7F80-$7FBF window, so bytes
; placed at $7F80-$7FBF read back as FDC register values, not opcodes/data. That is
; exactly where fdc_useslot_body (create's dir-slot claim, $7F8E) landed after ~M27,
; so FMAKE never persisted a directory entry -> create/write silently broke, and
; p0_env_tab's head entries ($7FB7-$7FBF) read back as register garbage. All three
; are reached only by label (fdc_entloop/fdc_useslot veneers; lay_page0_env's
; `ld hl, p0_env_tab`), so they now live in the COMMAND.COM-load free-region corridor
; (kernel.asm, between the $607B and $75A5 kernel veneers -- 0 canonical entries, no
; window). fat_find_body stays here: it ends at $7F5F, below the window. Spec:
; tier2-remediation-spec.md Phase B.
;
; ===== FDC-window guard (TRUE window $7F80-$7FBF, root-caused 2026-07-04) =========
; NOTHING -- executable OR data-read-through-the-page -- may land in $7F80-$7FBF:
; every address there aliases the WD2793 register file (the canonical 8 registers
; $7FB8-$7FBF mirrored x8). The window is now entirely dead $00 pad. The IF turns any
; below-window code that grows INTO the window into a LOUD build error (a bare
; negative `ds` only warns -> a silent empty object).
; hdl_direct -- D-LOADDIRECT (2026-11-07): LOAD the way the CF-3300 does. Its read
; commands land each FULL sector straight at its final address -- HL steps $200 a
; command, one command per sector -- and only the first sector (the $FF marker's)
; and the last partial one pass through a buffer (scratchpad/fdccmd_cf1107.out:
; the `82` / `8A` reads; the `A2` / `AA` run before them is its SAVE). Ours read
; every sector into FAT_DBUF and parsed it there. Read this way, ours misses a
; revolution 11 times in 28 sector gaps (scratchpad/fdccmd_direct.out); the
; CF-3300's LOAD misses 4 of 26.
; Entered with the marker read: FAT_DBUF holds the first sector, FREAD_OFF = 1,
; FREAD_LEFT = the bytes after the marker, CLPTR = TXTTAB, CLINK = 0.
; 🎯 THE SAME ANSWER AS THE STREAM, NOT A NEW READER. When the file ends at or
; below TXTMAX-3, none of hdl_line's TXTMAX tests can fire (the tightest is
; hdl_len's CLPTR < TXTMAX-4 with two link bytes already read), so the stream's
; outcome is a function of the bytes alone -- and the walk below applies the
; stream's own rules to the same bytes in place: link deltas, the first line (and
; a $FFFF delta) scanned to its $00 over tok_payload, every EOF a loaded program
; with CLPTR where the stream's last store left it. A file that does not fit takes
; hdl_line, unchanged. A read that fails is an EOF at that sector, as
; fat_io_getbyte's is. The only new write is the file's tail past its $0000
; link, into the free space below TXTMAX.
hdl_direct:
                ld      hl,(FREAD_LEFT+2)
                ld      a,h
                or      l
                jp      nz,hdl_oom          ; 64 KB or more
                ld      bc,(FREAD_LEFT)
                ld      hl,(CLPTR)
                add     hl,bc               ; the file's end in memory
                jp      c,hdl_oom
                ld      de,TXTMAX+1
                sbc     hl,de
                jp      nc,hdl_oom          ; past TXTMAX: out of memory, nothing read
                ; the first sector's rest: min(511, LEFT) from the buffer
                ld      hl,(FREAD_LEFT)
                ld      bc,511
                or      a
                sbc     hl,bc
                jr      nc,hd_rest
                add     hl,bc
                ld      b,h
                ld      c,l                 ; BC = LEFT, all of it
                ld      hl,0
hd_rest:        ld      (FREAD_LEFT),hl
                ld      hl,FAT_DBUF+1
                ld      de,(CLPTR)          ; DE = the destination, throughout
                ld      a,b
                or      c
                jp      z,hd_end            ; the marker alone: an empty program
                ldir
; ⚠️ ONE DSKIO A SECTOR, ON PURPOSE. D-LOADRUNS (2026-11-07) read each run of
; consecutive clusters with ONE multi-sector DSKIO -- built on a misreading: the
; "B counts down from $1B" it copied was the CF-3300's SAVE, and its LOAD's DSKIO
; count is not visible (BC holds $7FBC there). Measured on its own it was SLOWER
; here (14 KB 3.45 s against 3.07 one call a sector, with the key-scan fix in both, scratchpad/loadclock_keyskip.out / loadclock_ab_persec.out):
; our driver's own per-sector path between two sectors is no shorter than this
; call's, so the run only moved the FAT walk, it did not remove a miss. What made
; the misses was the ~3 ms keyboard-scan interrupt, fixed in C-BIOS (D-KEYSKIP).
hd_full:        ld      hl,(FREAD_LEFT)     ; every full sector: in place
                ld      a,h
                cp      2
                jp      c,hd_last
                dec     h
                dec     h
                ld      (FREAD_LEFT),hl
                push    de
                ex      de,hl
                call    frs_to
                pop     de
                jp      c,hd_end
                inc     d
                inc     d
                jr      hd_full
; (hd_last / hd_end continue in disk/kernel.asm, in front of hdl_walk: this
; region, bounded by the $7F7C pin, has no room for them.)

                ; D-DOSBASIC: zerobas's banner entry, pinned here, OUTSIDE the
                ; standard $4010-$4033 disk-ROM table -- it sat at $4022, which is
                ; BASENT. Main CALSLTs it (basic/interp.asm, `ld ix,$7F7C`).
; frl_cache / frl_done -- D-PUTWALK (2026-11-08): a random channel's chain walk
; starts where the last one ended. frnd_locate (disk/kernel.asm) walked from the
; file's first cluster on EVERY PUT / GET, through frnd_next's FAT read into
; FAT_MBUF -- which evicted the sector D-PUTCACHE holds there, so past the first
; cluster each PUT cost a FAT read, a data re-read and a write
; (scratchpad/fdcput128_zb.out). The row keeps the walk's end where an MSX-DOS FCB
; keeps it -- +28..29 the last cluster accessed, +30..31 its index in the chain (the
; kernel's own BDOS seek, ws_cid, uses the same two fields; dout_open zeroes them,
; and cluster 0 is never a data cluster, so 0 = no cache).
; frl_cache: in GP_CLIDX = the record's cluster index. Out Z + HL = the cluster to
; walk from, GP_CLIDX = the steps left (a hit: the record at or past the cached
; index); NZ = walk from the first cluster, as before. +28..29 is zeroed either way
; and only frl_done sets it again, so a walk that fails leaves no stale cache.
frl_cache:
                ld      c, (ix+30)
                ld      b, (ix+31)          ; BC = the cached index
                ld      e, (ix+28)
                ld      d, (ix+29)          ; DE = the cached cluster
                xor     a                   ; D-DISKREREAD (2026-11-26): through A, 1 B
                ld      (ix+28), a          ; (`ld a,d` below reloads it)
                ld      (ix+29), a
                ld      hl, (GP_CLIDX)
                ld      (ix+30), l
                ld      (ix+31), h          ; the index this walk lands on
                ld      a, d
                or      e
                jr      z, frc_miss         ; nothing cached
                sbc     hl, bc              ; (CF = 0) the steps from the cache
                jr      c, frc_miss         ; the record lies before it
                ld      (GP_CLIDX), hl
                ex      de, hl              ; HL = the cached cluster
                xor     a                   ; Z: a hit
                ret
frc_miss:
                or      1                   ; NZ: from the first cluster
                ret
frl_done:
                ld      hl, (GP_CLUS)
                ld      (ix+28), l
                ld      (ix+29), h          ; the cache: where this walk ended
                ld      a, (GP_SECINCL)
                call    clus_sec            ; (its adds cannot carry: CF = 0)
                ld      (GP_PHYS), de
                ret
; (bdos_close_write, the globals-path FCLOSE, retired by D-SEQMIX 2026-11-14; its
; D-BDOSSEQ lesson -- the size comes from the FCB, a WRRND / WRBLK past the end grows
; +16..19, never BDOS_WRBYTES -- is now the only close rule: close_read / cr_dir.)
; 💰 D-DSKFMT carve 18: BASIC's CLOSE (kernel.asm hcc_stamp) calls in here with HL = its
; channel (the size at +16..19 as in an FCB): out CF = fat_dir_update failed (A = $FF), else
; A = $00.
close_size_upd:
                ld      de, 16
                add     hl, de
                call    to_wrbytes
                call    fat_dir_update      ; write true size + first cluster
                sbc     a, a                ; A = $00 success / $FF (CF kept) --
                ret                         ; D-DISKREREAD (2026-11-26): 5 B
; fcb_size_keep -- FCB (IX) +16..19 := BDOS_WRBYTES, the file's size so far. It stood
; twice, in kernel.asm's wrblk_fcb_keep and wrseq_wr_writeback (D-DSKFMT carve 12,
; 2026-11-10: -11 B). Clobbers HL.
fcb_size_keep:
                ld      hl, (BDOS_WRBYTES)
                ld      (ix+16), l
                ld      (ix+17), h
                ld      hl, (BDOS_WRBYTES + 2)
                ld      (ix+18), l
                ld      (ix+19), h
                ret
; 📍 D-RECBIG (2026-11-04): frnd_pass_len moved out of the $5602..$5E10 fill; D-DSKFMT
; carve 19 (2026-11-10) moved it here from in front of $543C, for bdos_tab's rows.
; frnd_pass_len -- BC := min(GP_LEFT, 512 - GP_WITHIN), and GP_LEFT -= BC: the
; bytes of the record that fit in the sector this pass holds. Clobbers A, DE, HL.
; D-DISKREREAD (2026-11-26): no second `or a` (GP_WITHIN is 0..511 -- frnd_calc's
; offset & 511, or rnd_step's 0 -- so 512 - WITHIN cannot borrow), and one store tail
; for both arms (n into DE, GP_LEFT := 0 through it) -- 6 B.
frnd_pass_len:
                ld      hl, 512
                ld      de, (GP_WITHIN)
                or      a
                sbc     hl, de              ; HL = room in the sector (1..512); CF = 0
                ex      de, hl
                ld      hl, (GP_LEFT)
                sbc     hl, de              ; left - room
                jr      nc, fpl_split       ; left >= room: this pass takes the room
                add     hl, de              ; the rest fits: n = left, none over
                ex      de, hl              ; DE = n
                ld      hl, 0               ; GP_LEFT := 0
fpl_split:
                ld      (GP_LEFT), hl
                ld      b, d
                ld      c, e
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; wrseq_wr_writeback — Item 5 (spec-diskbasic-option-closure.md): the WRITE-side
; twin of wrseq_writeback. Mirrors the SAME FCB-position field set the read
; branch advances, into the FCB in IX (the fixed $DA40 until D-BDOSFCB stage B),
; after a WRITTEN sequential record (wrseq_body calls this only on A=$00). K = BDOS_SEQREC after this
; call's increment (records written so far, 1-based).
;   copy+32 (CR)      := K mod 128
;   copy+12 (EX)      := K div 128            (floppy -> fits a byte, §2.2)
;   copy+28/29 (word) := BDOS_WRCLUS          (the WRITE iterator's current cluster;
;                        NOT FAT_CURCLUS -- that is the READ iterator's cluster)
;   copy+30           := (K-1) div recPerClus, recPerClus = FAT_SECPERCLUS*RECPERSEC
; Placement/register discipline differ deliberately from wrseq_writeback: this
; lives in the $607B-$75A5 free-region corridor (the `ds $75A5 - $` pad below
; absorbs its size -> net-zero canonical addresses), and it is FULLY
; register-transparent -- preserves AF/BC/DE/HL so wrseq_body_write returns
; bdos_seqwrite's exact registers unchanged (keeps the BDOSX3/BDOSX4 WRSEQ
; register snaps byte-identical; only the FCB position memory moves). The
; source cluster is the ONLY logic difference from the read twin; the CR/EX/+30
; arithmetic is identical (deliberate duplication over sharing: the proven read
; routine stays byte-for-byte untouched, and the corridor has ample slack).
wrseq_wr_writeback:
                push    af
                push    bc
                push    de
                push    hl
                ; K := BDOS_SEQREC + 1
                call    seq_k_adv           ; HL = K (+32 CR, +12 EX set; D-DSKFMT defrag)
                push    hl
                ; copy+28/29 := BDOS_WRCLUS (word) -- the write iterator's cluster
                ld      hl, (BDOS_WRCLUS)
                ld      (ix+28), l
                ld      (ix+29), h
                push    ix
                pop     hl
                call    fcb_mark_written    ; +20..23 date/time, +24's 40h cleared (BDOSX8)
                ; copy+30 := (K-1) div recPerClus, recPerClus = FAT_SECPERCLUS*RECPERSEC
                pop     hl                  ; HL = K
                call    seq_k30             ; +30 := (K-1)'s cluster index
                ; copy+16..19 := BDOS_WRBYTES (running file size, 4-byte LE). Stock
                ; maintains the in-memory FCB size on every written record (proven
                ; by the pre-FCLOSE fcbsnap differential, BDOSX8); the read twin
                ; never needed this (FOPEN loads +16..19 from the dirent), but FMAKE
                ; zeroes it, so the write session must grow it here.
                call    fcb_size_keep       ; +16..19 (D-DSKFMT carve 12: runtime.asm)
                ; copy+26/27 := BDOS_WRFIRST (file's first cluster). Also maintained
                ; per stock's post-WRSEQ FCB, and also zero after FMAKE.
                ld      hl, (BDOS_WRFIRST)
                ld      (ix+26), l
                ld      (ix+27), h
                pop     hl
                pop     de
                pop     bc
                pop     af
                ret
; (moved here from kernel.asm by the D-DSKFMT defrag)
; install_dos_env (D-TPAP1) -- at every DOS entry (lay_page0_env), on every host:
; the $0038 handler and the slot bridges to INT_H_HIRAM (three pieces, end to end),
; the $F368 hook bodies to WA_SEG, and $F368 / $F368+3 pointed at them. Built here
; and not at INIT: WA_SEG and INT_H_HIRAM are BASIC's RAM until MSX-DOS starts.
install_dos_env:
                ld      hl, int_h_hiram_tmpl
                ld      de, INT_H_HIRAM
                ld      bc, int_h_hiram_end - int_h_hiram_tmpl
                ldir
                ld      hl, br1_tmpl
                ld      bc, br1_end - br1_tmpl
                ldir
                ld      hl, br2_tmpl
                ld      bc, br2_end - br2_tmpl
                ldir
                ld      a, $C3
                ld      (WA_JMPTAB), a
                ld      (WA_JMPTAB + 3), a
                ld      ($0000), a          ; D-BOOTREGS: the warm boot goes on into the
                ld      hl, $0005           ; BDOS with the caller's C, as on the CF-3300
                ld      ($0001), hl         ; before MSXDOS.SYS (scratchpad/wboot_run.out)
                ld      hl, WA_SEG_ROM
                ld      (WA_JMPTAB + 1), hl
                ld      hl, WA_SEG_RAM
                ld      (WA_JMPTAB + 4), hl
                ld      hl, wa_seg_rom_tmpl ; ... and the hook bodies right after br2
                ld      bc, wa_seg_end_tmpl - wa_seg_rom_tmpl   ; (DE runs on: = WA_SEG)
                ldir
                ld      hl, br3_tmpl        ; ... then br3 (D-BOOTREGS)
                ld      bc, br3_end - br3_tmpl
                ldir
                ld      hl, br5_tmpl        ; ... and br5
                ld      bc, br5_end - br5_tmpl
                ldir
                jp      wa_patch            ; the hook bodies' subslots (D-DISKMSX2HOST), then br6
; ldir_11 -- D-DISKROMHOST step 2b: the run it replaces stood at 7 sites (kernel.asm)
ldir_11:
                ld      bc,11
                ldir                        ; +0: the source pattern
                ret
; dta_from_dos -- D-DISKROMHOST step 2b: the run it replaces stood at 5 sites (fat.asm, kernel.asm)
dta_from_dos:
                ld      hl, (DOS_DTAPTR)
                ld      (BDOS_DTA), hl
                ret
; boot_rd -- ✂️ D-DISKCARVE3 (2026-11-14), here in front of the $7F7C pin for room: read drive A's boot sector (sector 0) to
; BOOT_LOAD ($C000) and test its signature; stood twice, in init.asm's boot_disk_go and in hk_system
; (disk/kernel.asm). Out: CF = the read failed; else Z = $EB or $E9 (A = that byte, NC,
; the flags `cp` left -- what both callers saw before), NZ / NC = not a boot sector.
; Clobbers BC, DE, HL as DSKIO does.
boot_rd:
                xor     a               ; drive A, Cy = 0 -> read
                ld      b, 1            ; one sector
                ld      c, a            ; media byte (ignored, single-drive)
                ld      d, a
                ld      e, a            ; logical sector 0 (the boot sector)
                ld      hl, BOOT_LOAD   ; -> $C000 (page-3 RAM)
                call    dskio
                ret     c
                ld      a, (BOOT_LOAD)  ; first byte = boot signature?
                cp      $EB
                ret     z
                cp      $E9
                ret     z
                or      $FF             ; neither: NZ, NC
                ret
; write_sector -- one sector from HL to logical sector DE of drive A. 📍 D-DISKROOM (2026-11-25): moved here from
; the $5E10..$5FE5 region (disk/kernel.asm), which needed the bytes for bt_keep's HIMEM reset; 10 B -> 8 B.
write_sector:
                ld      bc, $0100       ; B = one sector, C = media 0: not an ID -> the mounted disk's (D-SIDES)
                xor     a               ; drive 0 (ignored); the scf below sets Cy again
                scf                     ; Cy = 1 = WRITE direction (MSX2 TH DSKIO)
                jp      bdos_dskio      ; tail-call (D-DISKVE: DISKVE's handler on an error)
; ce_col -- conout_emit_e's logical column ($F237) after the char in CONOUT_CHAR, the CF-3300's rule
; (D-CONCOL, 2026-11-26; BDOSXL, scratchpad/XL2_stock.out, AE_concol_cap.out): printable +1; CR, FF and
; HOME ($0B) -> 0; BS, cursor-left ($1D) and DEL ($7F) -1 (also at 0: $FF, as the CF-3300); every other
; control code (LF, BEL, ESC ...) leaves it. Was: CR 0, LF unchanged, everything else +1 -- a TAB after
; FF / BS / BEL / ESC landed wrong. DIRIO's output does not count at all: conout_dirio_out below.
; CLEAN-ROOM: the published column cell and black-box readings (BDOSXL's ($F237) per case).
; Entered by conout_emit_e's JP, with its BC / DE / HL on the stack: it pops them and returns for it.
; Out: A = the column.
ce_col:
                ld      hl, $F237
                ld      a, (CONOUT_CHAR)
                cp      $7F
                jr      z, cec_dec          ; DEL: -1
                cp      $20
                jr      nc, cec_inc         ; printable: +1
                cp      $0D
                jr      z, cec_zero         ; CR
                cp      $0C
                jr      z, cec_zero         ; FF
                cp      $0B
                jr      z, cec_zero         ; HOME
                cp      $08
                jr      z, cec_dec          ; BS
                cp      $1D
                jr      nz, cec_done        ; other control codes: unchanged
cec_dec:        dec     (hl)                ; BS, cursor-left, DEL
                jr      cec_done
cec_inc:        inc     (hl)
                jr      cec_done
cec_zero:       ld      (hl), 0
cec_done:       ld      a, (hl)
                pop     hl                  ; conout_emit_e's
                pop     de
                pop     bc
                ret
; (conout_dirio_out: in front of $75A5, disk/kernel.asm -- this region is full)
                ds      $7F7C - $, $00
k_7F7C:         jp      disk_show_banner    ; $7F7C: emit "zerobas Disk BASIC"
                IF ($ > $7F80)
                ENDIF
                ds      $7FC0 - $, $00      ; skip OVER the dead $7F80-$7FBF FDC window
; fw_done -- fren_wild's end (disk/kernel.asm): [idx][cnt] and the frame dropped; A =
; $FF when nothing was renamed, else 0. Here, in the 17 bytes between the FDC window
; and conout_emit_e's $7FD1 pin, for the room in front of $75A5 (D-FRENWILD).
fw_done:                                    ; Z = nothing renamed (fw_end's `or`)
                ld      hl, 12
                add     hl, sp              ; (keeps Z)
                ld      sp, hl              ; the frame dropped
                jp      z, fren_miss        ; nothing matched: $FF
                call     clr_f306   ; M20 dispatcher-flag rule
                ld      h, a
                ld      l, a                ; HL = 0, as the CF-3300 leaves it (BDOSX3 rec 14)
                ret
                ds      $7FD1 - $, $00
; ===== M22b slice 2: conout_emit_e, ABOVE the FDC window =====================
; conout_body's stub + conout_tab (below the window) reach this by absolute call. The
; `ds` above pins it at $7FD1 -- above the window top $7FBF -- so its own instruction
; fetches never read FDC registers. The IF guard below fires loudly if it would ever
; start in/below the window. Spec: tier2-m22b-slice2-reloc-spec.md.
                IF ($ <= $7FBF)
CONOUT_EMIT_IN_HOLE: equ conout_emit_e_would_start_in_or_below_the_FDC_register_window
                ENDIF
; conout_emit_e — CHPUT the char in E via the proven M8/M10 inter-slot window, then
; maintain the logical column at $F237 (CR -> 0, LF -> unchanged, else col++). The
; column work runs INSIDE this routine's HL push/pop guard, so conout_body keeps its
; BC/DE/HL/IX/IY-preserved contract (the kernel console loop holds its string pointer
; in HL -- clobbering it derails the DOS boot). $F237 is a pinned page-1 kernel-ABI
; DATA cell shared across every output path (func-2/func-9/BUFIN-echo, §5.5); using the
; literal address keeps future --mem 0xF100:0x300 capture diffs byte-clean. CLEAN-ROOM:
; the do-while tab rule + column semantics are re-derived from OUR OWN injected TABTEST
; files' observable output + the published _CONOUT contract; no stock bytes decoded.
;   in: E=char ; out: A = the UPDATED $F237 column (NOT the char -- the TAB loop tests
;   A & 7; conout_body's normal path reloads the char via `ld a,e`); BC/DE/HL/IX/IY preserved.
conout_emit_e:
                ld      a, e                ; char arrives in E ($5454 CONOUT contract)
                ld      (CONOUT_CHAR), a    ; stash the char (A is needed for slot work)
                push    bc
                push    de
                push    hl
                di                          ; no interrupt while the BIOS is half-mapped
                call    br_mainin_tmpl
                ld      a, (CONOUT_CHAR)
                call    $00A2               ; CHPUT - emit A; preserves all registers
                call    pg0_mainrom_out     ; restore page 0
                ei
                jp      ce_col              ; the column rule (D-CONCOL) and this routine's exit: A := the
                                            ; updated column. A JP, not a call: no deeper on the stack than
                                            ; before -- a `call` here (2 B more) broke calculus.dsk's boot
                                            ; loader (D-CONCOL's corpus run, scratchpad/AE_concol_diskcorpus.out)

; --- pad to a full 16 KB page ($4000-$7FFF) --------------------------------
                ds      $8000 - $, $00
