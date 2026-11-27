; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; Part of zerobas-disk, included by disk.asm (build with `pasmo -I disk`).
; shared equates: work-area / scratch / constant addresses
; CLEAN-ROOM: every constant, address and algorithm here traces to a public source
; or a black-box oracle probe; nothing is derived from disassembly. See disk/PROVENANCE.md.

; --- System addresses (disk/PROVENANCE.md §INIT / §BDOS) -------------------
; Sources: MSX2 Technical Handbook, work area.
SYSTEM          equ     $F37D   ; SYSTEM sysvar: BDOS entry-point word
; RAMAD0-3: the RAM slot address for each of the four pages (the standard MSX
; work-area cells the disk ROM's INIT populates so MSX-DOS can re-page RAM into
; page 0). MSX2 TH work area; the disk-ROM-INIT responsibility is confirmed by a
; black-box differential (provider-oracle-scope.md §8.8/§8.16). EXPTBL flags which
; primary slots are expanded. (EXPTBL also in basic/sysvars.inc — same address.)
DOS_SIG         equ     'S'*256+'D' ; D-CALLSYSTEM: in BASENT_REQ, 'D','S' = MSX-DOS was booted (MSXDOS.SYS's $50A9 call sets it since D-BOOTDATE, the DOS -> BASIC restart keeps it); CALL SYSTEM refuses (5) without it
DOS_TRY         equ     'T'*256+'D' ; D-BOOTDATE: in BASENT_REQ, 'D','T' = a boot code is being run (dos_handoff), MSX-DOS not yet -- a $4022 now is the boot loader's fallback, not A>BASIC
BASENT_REQ      equ     $F33D   ; D-DOSBASIC: 'B','A' while BASENT restarts BASIC from MSX-DOS, 'B','D' (D-BOOTDATE) from a boot loader's fallback -- disk INIT then keeps the DOS date and skips the DOS boot; 'BA' leaves DOS_SIG (above) in it, 'BD' asks the date and leaves 0 (2 B; a signature, so power-on RAM cannot match)
DATE_DAYS       equ     $F33B   ; the DOS date: a day count since 1980-01-01, where the CF-3300 keeps it (1461 = 1984-01-01 from boot; D-DOSDATE) (2 B)
DOS_F340        equ     $F340   ; disk work-area flag the kernel reads at init (§8.33); $00 = ok (1 B)
DOS_F346        equ     $F346   ; disk work area: MSX-DOS has been booted -- CALL SYSTEM's gate (D-F346) (1 B)
RAMAD0          equ     $F341   ; RAM-slot id per page ($F341-$F344), MSX2 TH work area
HOKVLD          equ     $FB20   ; bit 0: the EXTBIO hook ($FFCA) is valid, MSX2 TH work area (1 B)
EXPTBL          equ     $FCC1   ; expanded-slot flags, 1 byte/primary, bit 7 = expanded
JIFFY           equ     $FC9E   ; MSX software clock, bumped by the timer ISR (MSX2 TH work area; D-DSKCHG)
; D-FILESLOCAL (disk/docs/spec-diskcode-eviction.md §6.6ba): FILES/LFILES emit
; from THIS ROM now, so it needs the list-device entry beside CHPUT ($00A2,
; disk/init.asm) and the two SCREEN cells the wrap decision reads.
; 🔴 ALL THREE ARE PUBLISHED MSX CONSTANTS, NOT OUR CHOICES, which is why
; re-declaring them here cannot drift -- the same rule EXPTBL above is under.
; A BIOS entry is reachable by a plain `call` from a hook handler: page 0 holds
; the main ROM while BASIC runs, which `calbak`'s own `jp CALSLT` ($001C) already
; relies on. That is NOT the MSX-DOS case, where page 0 is RAM and lstout_body
; has to page the BIOS in (disk/kernel.asm) -- do not read one for the other.
LPTOUT          equ     $00A5   ; MSX BIOS: send the character in A to the list
                                ; device (MSX2 TH; same class as CHPUT $00A2)
RDVRM           equ     $004A   ; MSX BIOS: read VRAM at HL -> A -- BSAVE ,S streams
                                ; its data from VRAM through it (2026-10-19; the
                                ; same address basic/sysvars.inc declares)
WRTVRM          equ     $004D   ; MSX BIOS: write A to VRAM at HL -- BLOAD ,S streams
                                ; its data INTO VRAM through it (2026-10-19, hk_dpbload;
                                ; the same address basic/sysvars.inc declares)
CSRX           equ     $F3DD   ; cursor column, 1-based (MSX standard work area;
                                ; same address basic/sysvars.inc declares)
LINLEN          equ     $F3B0   ; current line length = the active width (ditto)
; --- D-DISKVERB: the call-back route out of this ROM into main PAGE 1 -------
; CALSLT is the BIOS's own inter-slot call (MSX2 TH). Page 0 is never swapped
; (spec-diskbasic-hook-rearchitecture.md §2), so it stays mapped while it
; switches page 1 from this ROM to main and back. PRICED: 7 B per call site and
; 0.156 ms per call, ~10% of the hook entry the verb already paid to get here
; (D-XSLOTPRICE 0b).
CALSLT          equ     $001C   ; inter-slot call: IX = target, IYh = slot id
; ⚠️ A PROTOCOL CONSTANT DUPLICATED FROM basic/sysvars.inc, and the two must
; agree: main's kill_status decodes the status this selector produces. Same
; hazard class as the H_* hook equates, which `make hook-equate-check` polices;
; a selector checker is a follow-up, not silence.
DISKOP_SEL_KILL equ     0       ; dirverb: KILL's fat_delete loop (sysvars.inc)
DISKOP_SEL_NAME_STAMP equ 1     ; dirverb: NAME's read+overwrite+write (sysvars.inc)
DISKOP_SEL_COPYSTASH equ 6      ; dirverb: COPY's DISK_FCB_NAME -> COPY_SRC (sysvars.inc)
DISKOP_SEL_COPY equ     7       ; dirverb: COPY's body, sector by sector (sysvars.inc)
; TO_TOKEN is the crunched `TO`, which the VG-8020 produces inside a COPY line
; too (D-COPY). Read straight out of the program text here, as NAME reads `AS`.
TO_TOKEN        equ     $D9     ; basic/sysvars.inc
; D-DISKVERB4: FILES and LFILES share ONE hook cell, so the handler reads the
; verb's own token back out of the program text to tell them apart. Both are
; single-byte. basic/sysvars.inc; oracle-locked (basic/kwtable.inc).
FILES_TOKEN     equ     $B7
LFILES_TOKEN    equ     $BB
COLON           equ     $3A     ; ':' statement separator (basic/sysvars.inc)
DISKOP_SEL_FILES equ    2       ; dirverb: FILES  -> screen, packed rows (sysvars.inc)
DISKOP_SEL_LFILES equ   3       ; dirverb: LFILES -> printer, one per line (sysvars.inc)

; --- Standard hook + entry addresses (disk/PROVENANCE.md §INIT) -------------
; HPHYD is the standard PHYDIO hook a disk ROM installs so a host's physical disk
; I/O reaches the in-slot DSKIO. Source: public MSX hook table
; (fms.komkon.org/MSX/Docs/Hooks.txt) + disk/docs/expansion-protocol.md §2
; (CF-3300 black-box trace: after boot $FFA7 held an RST 30h / slot $87 / RET).
HPHYD           equ     $FFA7   ; PHYDIO hook (5 RAM bytes, default C9)
; DSKIO entry: this ROM base ($4000) + standard disk-ROM offset +$10 (MSX2 TH,
; disk ROM interface). The HPHYD CALLF targets it cross-slot.
DSKIO_ENTRY     equ     $4010   ; our DSKIO ($4000 + $10)

; --- BASIC-extension hooks (D-MKHOOK, docs/spec-basic-nodisk.md) ------------
; On real MSX the MAIN ROM owns the disk keywords' TABLE and the DISK ROM supplies
; their IMPLEMENTATION through these hooks -- which is why a diskless machine
; tokenises `MKI$` and then answers `Illegal function call`. zerobas implemented
; every disk verb BASIC-side, so a diskless build ANSWERED instead: 8 measured
; divergences (probes/basic/basic_probe_nodisk.py).
; Addresses: the MSX2 Technical Handbook hook table (grade B, docs/allowed-sources.md),
; CORROBORATED by a black-box diff of the hook region between the VG-8020 (no disk)
; and the CF-3300 (disk) -- 35 slots carry an `F7 87 <addr> C9` stub there, and
; three of them were already known in-tree ($FD9F H.TIMI, $FFA7 H.PHYD, $FFCA).
; scratchpad/hookdiff_probe.py.
; The seven this ROM claims. Addresses from the TH table, each corroborated by
; the hook diff (all seven are among the 35 slots the CF-3300's disk ROM claims).
H_DSKF          equ     $FE12   ; DSKF
H_MKI           equ     $FE30   ; MKI$ -- the first verb routed this way (D-MKHOOK)
H_MKS           equ     $FE35   ; MKS$
H_MKD           equ     $FE3A   ; MKD$
H_CVI           equ     $FE3F   ; CVI
H_CVS           equ     $FE44   ; CVS
H_CVD           equ     $FE49   ; CVD
H_NAME          equ     $FDF9   ; NAME    (channel verbs, D-CHANHOOK)
H_KILL          equ     $FDFE   ; KILL
H_DSKO          equ     $FDEF   ; DSKO$ (D-DSKIO) -- 🔴 WAS $FDF4 (D-DSKOHOOK
                                ; 2026-09-15). MEASURED on the CF-3300: un-claiming $FDEF
                                ; flips `DSKO$ 0,0` from ERR 70 to ERR 5, un-claiming $FDF4
                                ; changes nothing. ⚠️ THIS EQUATE IS DEFINED TWICE -- here
                                ; and in basic/sysvars.inc -- AND THE TWO MUST AGREE: the
                                ; BASIC side consults the cell and the disk side CLAIMS it,
                                ; so a split makes our own build refuse a verb it can do.
                                ; 14 hook equates are duplicated across the two files;
                                ; `tools/check_hook_equates.py` now refuses a divergence.
H_DSKI          equ     $FE17   ; DSKI$ (D-DSKIO)
H_COPY          equ     $FE08   ; COPY (D-COPY)
H_ERRP          equ     $FEFD   ; error-print hook (D-DISKERR): this ROM prints ERR 68..70
DISKVE          equ     $F323   ; word -> a cell holding the disk-error handler's address (D-DISKVE)
PRN_ECHO        equ     $F23B   ; (1 B) the printer-echo flag: Ctrl-P 1, Ctrl-N 0 (D-BUFINCTL, AE_prnflag.out; M27)
DSKBUF_PTR      equ     $F351   ; word -> the DSKI$/DSKO$ sector buffer; the disk ROM writes it
                                ; at init (basic/sysvars.inc has the same cell and the reading)
; 🟢 FSECTOR_BUF IS NO LONGER MIRRORED HERE (D-LRSETMOVE, 2026-09-17). It was a
; hand copy of basic/sysvars.inc's value, carrying its own warning that *"a drift
; between the two definitions reads garbage there, not silence"* -- and hk_lrset
; needed the same cell, which collided. The fix is the one that removes the
; hazard instead of working around it: it now arrives through
; disk/basic-resident-abi.inc, GENERATED per build from build/basic-reloc.sym and
; verified by `make diskrom-abi-check`, so the two definitions CANNOT drift
; because there is only one.
; ⚠️ `DSKBUF_PTR` above is still a hand copy with the same hazard. Left alone
; deliberately: it is not this slice's subject, and changing a mirrored address
; while moving a verb would put two causes in one commit.
; D-FLDGATE: field.asm's three verbs. Addresses MEASURED on the CF-3300 by
; un-claiming each cell and seeing which verb starts answering ERR 5
; (scratchpad/hookid_probe.py); consecutive, 5 apart, the shape a hook table has.
H_LSET          equ     $FE21   ; LSET
H_RSET          equ     $FE26   ; RSET
H_FIELD         equ     $FE2B   ; FIELD
H_FILE          equ     $FE7B
H_FORM          equ     $FFAC   ; CALL FORMAT (D-FMTHOOK): H.FORM, MSX2 TH hook table;
                                ; in the 35-cell census (docs/spec-basic-nodisk.md §5.2)
; --- H_SYST: CALL SYSTEM (D-CALLSYSTEM, 2026-10-09) ---------------------------
; main's ex_call calls it for every CALL name that is not FORMAT, the name's address
; in STRSCR+1; unclaimed (diskless) its RET keeps main's Syntax error. The reference
; finds CALL handlers through the ROM header, not a hook, so the cell is our own
; choice: H.SETS (MSX2 TH hook table, Disk BASIC 2's SET), which an MSX1 BASIC never
; calls and the CF-3300's disk ROM leaves unclaimed (docs/spec-basic-nodisk.md
; §5.2.2). NOT $FEB7: the reference claims that one and fires it at every OPEN.
H_SYST          equ     $FDF4
H_RUNC          equ     $FECB   ; BASIC clears its program state (NEW / a typed line / RUN / the cold start):
                                ; this ROM's phase 2 and DOS boot run from it (D-DISKROMHOST; MSX2 TH hook table)
; --- H_FOPEN: the cell every file-OPENING verb arrives at (D-DPLMOVE) --------
; 🏗️ RULED BY JOOST 2026-09-19 (spec-diskcode-eviction.md §6.6m): "the reference
; is the better oracle then me, so go with FE5D + selector". "Every disk command
; behind ITS hook" means THE CELL THE REFERENCE ACTUALLY USES -- so where it
; routes several verbs through one cell, so do we.
;
; 🎯 NAMED FOR ITS CLASS, NOT FOR A VERB. D-CHANHOOK measured that of twenty
; unidentified cells exactly one moves OPEN"NOSUCH" and exactly one moves
; MERGE"NOSUCH", and it is the same one; §6.6j added LOAD as the third. Calling
; it H_OPEN or H_LOAD would be a name narrower than its class, which
; spec-diskbasic-hook-rearchitecture.md already declined once.
;
; CLEAN-ROOM: the ADDRESS is a hook cell's, read for SLOT and IDIOM only from
; the 35-cell census (scratchpad/hookdiff_probe.py, expansion-protocol.md §2).
; What the reference's own handler does there is its internal detail: never
; read, never called, and nothing below depends on it.
H_FOPEN         equ     $FE5D

; --- FOPEN_SEL: which verb is asking (main -> disk.rom) ----------------------
; 🔴 IT ALIASES DISKOP_OP, AND THAT IS THE TREE'S OWN IDIOM, NOT A SHORTCUT.
; sysvars.inc already binds DEFT_STATUS and LE_STATUS onto DISKOP_STATUS for
; mutually-exclusive consumers. The last free byte below the channel-context
; table went to DISKOP_OP itself ($E9FB; the five-byte gap at $E9FB..$E9FF is
; spent), so a new cell would have to displace something.
; MUTUAL EXCLUSION, ARGUED: DISKOP_OP is main -> SUB-ROM-TENANT. Between main
; writing FOPEN_SEL and disk.rom reading it there is one inter-slot call and no
; tenant dispatch -- the hook body needs NO call-backs (§6.6h) and disk.rom's
; FAT engine reaches the drive through DSKIO directly, never through a tenant.
; ⚠️ A future arm that DOES call back into main must re-assert the selector
; after it returns, or move to its own cell.
;
; 🔴 AND THE SELECTOR IS NOT READ FROM THE PROGRAM TEXT, WHICH §6.6m FIRST SAID.
; hk_files recovers its verb from the token before FN_RESUME, and that works
; because main stages the cursor past the token and the FILESPEC IS EVALUATED
; INSIDE THE HOOK. LOAD evaluates its filename in MAIN, before the hook is
; reached, so by then FN_RESUME points past the EXPRESSION and the byte in front
; of it is the closing quote, not the verb. Same shape, different side of the
; call -- so the selector crosses in RAM (§6.6n).
FOPEN_SEL       equ     DISKOP_OP
HIMEM           equ     $FC4A   ; the published BASIC ceiling (MSX2 Technical Handbook,
PROCNM          equ     $FD89   ; PUBLISHED: a CALL statement's name, 0-terminated (expansion-ROM STATEMENT handler)
STREND          equ     $F6C6   ; PUBLISHED: the arrays' end -- CALL FORMAT's work area starts here (D-DSKFMT)
FRETOP          equ     $F69B   ; the string heap's low boundary -- the work area ends below it (D-DSKFMT)
                                ; work area) -- D-FCBSHAPE S2 lowers it at init
; D-DISKROMHOST step 2b: MS-BASIC's published layout cells, which a disk ROM rebuilds under each
; HIMEM it claims (docs/concepts/disk-rom-host.md item 2; MSX2 TH work-area map)
MEMSIZ          equ     $F672   ; PUBLISHED: top of string space
STKTOP          equ     $F674   ; PUBLISHED: top of BASIC's stack
SAVSTK          equ     $F6B1   ; PUBLISHED: BASIC's saved stack pointer
FILTAB          equ     $F860   ; PUBLISHED: -> the file pointer table (its next word: NULBUF)
NULBUF          equ     $F862   ; PUBLISHED: -> file #0's buffer
SLTWRK          equ     $FD09   ; PUBLISHED: 2 B per slot x page, a ROM's work-area pointer (MSX2 TH work area)
; D-DISKROMHOST 4 (d) scheme D (owner's ruling 2026-11-26): our slot's page-1 entry holds the work-area
; base the references publish there -- the CF-3300 $F1BF (3-1), the HBD-F1 $F1C0 (1-0), each = its INIT
; HIMEM + $2A, and N lower under a foreign claim of N (scratchpad/AE_wasize_ref.out, AE_wasize_claimN.out).
SLTWRK_BASE     equ     INIT_HIMEM + $2A   ; NOT-A-CELL: $F1BF, the CF-3300's value at N = 0
RDSLT           equ     $000C   ; MSX BIOS: A = the byte at HL in slot A (MSX2 TH)
STARTUP_WORD    equ     $7D2F   ; main ROM: the word naming the machine's start-up-screen routine (item 4)
INIT_HIMEM      equ     $F195   ; INIT's (first) claim: the CF-3300's HIMEM at its H.RUNC entry,
                                ; = DRVA_DPB, our lowest cell above it (scratchpad/AE_diskhost2_ref.out)
SUB_BUILD      equ     0       ; this is the DISK build, not the sub-ROM

FOPEN_SEL_AOPEN equ     $41     ; step 11 (D-MERGEPORT): open an ASCII program
                                ; file and prime the sequential read -- `MERGE`
                                ; and ASCII `LOAD` alike, because on the
                                ; reference they are ONE mechanism (identical
                                ; 47-cell set, expansion-protocol.md §8.6a). The
                                ; selector names the OPERATION, not the verb.
FOPEN_SEL_GETB  equ     $47     ; step 11: hand back ONE byte of the open ASCII
                                ; file. 🔴 A CROSSING PER BYTE IS WHAT THE
                                ; REFERENCE DOES on exactly these paths -- $FE8A
                                ; (H.INDS) is entered once per byte and is
                                ; CLAIMED on both measured vendors -- so this is
                                ; faithfulness, not a concession. Priced at
                                ; 0.156 ms/byte (D-XSLOTPRICE) against the
                                ; reference's own 4.3 ms/byte (§6.2a).
                                ; ⚠️ MAIN RE-ASSERTS THE SELECTOR BEFORE EVERY
                                ; CROSSING, and must: it aliases DISKOP_OP and
                                ; main runs the TOKENISER between byte reads.
FOPEN_SEL_SAVE  equ     $53     ; step 12 (D-SAVEPORT): tokenised SAVE. Same
                                ; rule as LOAD's $4C -- outside DISKOP_SEL_*'s
                                ; 0..7 range, so an idle cell can never read as
                                ; "SAVE is asking".
DISKOP_SEL_DSKO equ     4       ; D-DSKLOCAL: mirror of basic/sysvars.inc's -- main
                                ; passes it in A on DSKO$'s second crossing of
                                ; H_DSKO / H_DSKI (DSKI$ passes DISKOP_SEL_DSKI, 5;
                                ; the gate crossing passes 0)
FOPEN_SEL_MAKE  equ     $4D     ; S10.B (2026-10-06): OPEN FOR OUTPUT -- create
                                ; DISK_FCB_NAME as the live channel's FCB (hk_fmake)
FOPEN_SEL_APND  equ     $50     ; S10.B increment 2: OPEN FOR APPEND (hk_fapp)
FOPEN_SEL_RAND  equ     $52     ; S10.D (2026-10-15): OPEN ... AS #n, RANDOM (hk_frnd)
FOPEN_SEL_INP   equ     $49     ; S10.C (2026-10-15): OPEN FOR INPUT (hk_finp)
FOPEN_SEL_AUTO  equ     $58     ; D-ROMSIZE step 1 (2026-10-19): AUTOEXEC.BAS's boot
                                ; probe (hk_auto) -- stage the name, mount, find,
                                ; test the size; main loads and RUNs on status 0
FOPEN_SEL_BSAVE equ     $42     ; D-ROMSIZE step 1 (2026-10-19): BSAVE to disk
                                ; (hk_dpbsave) -- the 7-byte header + data. The
                                ; CF-3300's BSAVE enters $FE5D exactly once
                                ; (scratchpad/bsave_hook_run.out)
FOPEN_SEL_BLOAD equ     $5A     ; D-ROMSIZE step 1 (2026-10-19): BLOAD from disk
                                ; (hk_dpbload) -- the $FE header, the offset, the
                                ; data into RAM or (`,S`) VRAM. The CF-3300's
                                ; BLOAD enters $FE5D once with DE = 1, HL = the
                                ; program-verb buffer, like LOAD / MERGE, never
                                ; $FE76, no claimed cell per byte
                                ; (spec-diskcode-eviction.md §6.6ab / §6.6v / §8.2)
H_CHIN          equ     $FE8A   ; S10.C: an INPUT channel's byte / EOF / numeric item; S10.E: LOF / LOC
                                ; (hk_chin) -- A = 0 / 2 / 4; out A = the byte,
                                ; CF = the end. The CF-3300 enters $FE8A once per
                                ; byte read (scratchpad/s10c_hook_run.out). S10.E
                                ; (2026-10-19): A = 6 LOF / 8 LOC of any disk
                                ; channel, out DE:HL (din_lofloc)
H_DGET          equ     $FE80   ; S10.D: GET / PUT (hk_dget) -- A = 0 GET / 1 PUT,
                                ; C = the channel, FN_RESUME = the cursor past `#n`
H_CHOUT         equ     $FE85   ; S10.B: one byte out of an OUTPUT channel (A)
H_CHCLOSE       equ     $FE62   ; S10.B: CLOSE of an OUTPUT channel
FOPEN_SEL_OCHK  equ     $4F     ; D-OPENSAME (2026-09-29): OPEN's same-file check
                                ; -- is DISK_FCB_NAME open on a channel? The
                                ; CF-3300 answers `File already open` (54) to a
                                ; second OPEN of one file in every mode
                                ; (scratchpad/opensame_before.out). Outside every
                                ; DISKOP_SEL_* range, like the rest.
FOPEN_SEL_LOAD  equ     $4C     ; 🔴 NOT 0, AND THE REASON IS MEASURED. This
                                ; aliases DISKOP_OP, whose 0 is a LIVE value --
                                ; `DISKOP_SEL_KILL equ 0` -- so a selector of 0
                                ; reads "LOAD is asking" whenever the cell is
                                ; merely idle. And $FE5D IS reached on paths that
                                ; are not ours: claiming it with a 0 selector
                                ; starved the STOP-trap re-arm, because every
                                ; such call fell through into a full FAT mount
                                ; (spec-diskcode-eviction.md §6.6p). $4C is
                                ; outside DISKOP_SEL_*'s whole 0..7 range.
; The on-disk tokenised-BASIC marker, same value basic/sysvars.inc declares.
; A PUBLIC format constant (basic/docs/spec-ascii-saveload.md), not an address --
; duplicated the way BPB_*/DIRENT_*/EOC already are in this file, because it
; cannot drift with a repack the way TXTBASE/TXTMAX can (those go through the
; GENERATED basic-resident-abi.inc instead, and for exactly that reason).
BASIC_DISK_ID   equ     $FF
; The on-disk BSAVE marker, under the same rule (basic/sysvars.inc declares the
; same value): BSAVE's write engine (basic/sv-bsvdisk.inc) runs in this ROM since
; 2026-10-19.
BSAVE_DISK_ID   equ     $FE
; 1..3 are reserved for steps 10-12 (OPEN, MERGE, SAVE): the ruling says they
; route to this same cell and add selector values rather than claiming cells of
; their own. An unrecognised selector returns CF=0 -- "not mine" -- and main
; carries on, which is what makes adding an arm a local change.
   ; FILES

; --- Disk scratch RAM (disk/PROVENANCE.md §Scratch RAM) --------------------
; (GETDPB writes the 18-byte DPB into the CALLER's buffer per MSX2 TH, so the disk
; ROM reserves no local DPB area.)
;
; FDC driver state (7 bytes below the sector buffer)
F37D_STUB       equ     $EFDA   ; D-BDOSBASIC slice 2: the $F37D target, a page-3 stub (28 B)
DSK_CURDRV      equ     $F022   ; D-DSKIB: the logical drive the one physical drive holds now -- 0 A:, 1 B: (1 B)
DSK_MEDIA       equ     $EFF6   ; D-SIDES: the media ID DSKIO seats sectors by (bit 0 two sides, bit 1 8 a track) (1 B)
FDC_IFF         equ     $EFF7   ; saved caller IFF2 across a sector op (1 = was EI) (1 B)
FDC_CNT         equ     $EFF8   ; remaining sector count (1 B)
FDC_LSEC        equ     $EFF9   ; current logical sector (word)
FDC_DEST        equ     $EFFB   ; current transfer address (word)
FDC_TRY         equ     $EFFD   ; read attempt counter (1 B)

; Sector buffer (512 bytes) — DSKIO reads land here for FAT12 parsing.
; 🏗️ D-BUFMERGE (Joost, 2026-09-24: *"(b) One shared buffer"*): this IS main's
; FSECTOR_BUF, and WBUF below IS main's FWBUF -- one data buffer and one
; metadata buffer for both ROMs, the reference's own shape. It is safe because
; every BASIC verb that crosses into this ROM goes through main's `chan_gate`,
; which saves the live channel and re-stages its sector after the crossing
; (the audit: TODO.md, `scratchpad/bufaudit.py`). ⚠️ BDOS ($F37D) and PHYDIO
; called from a user's machine code while a BASIC file is open are NOT
; bracketed and are declared outside the contract -- they already overwrote
; main RAM before the merge. disk.asm asserts both equalities at build time.
; Was $E2A0, on top of main's TEMPPOOL and GFX_* cells.
SECTOR_BUF      equ     $E8AD   ; 512-byte sector buffer ($E8AD-$EAAC) = main FSECTOR_BUF

; --- FAT12 geometry + iterator state (disk/PROVENANCE.md §Scratch RAM) ------
; Filled by fat_mount from the on-disk BPB; the file iterator walks the cluster
; chain using them. All own-choice free page-3 RAM after the sector buffer.
FAT_SECPERCLUS  equ     $ECAD   ; sectors per cluster (byte)
FAT_FATSTART    equ     $ECAE   ; first FAT sector (= reserved sectors) (word)
FAT_FIRSTROOT   equ     $ECB0   ; first root-directory sector (word)
FAT_TOTCLUS     equ     $ECB2   ; total clusters + 2, from fat_mount (word; D-WCACHE W1 --
                                ; was FAT_ROOTSECS, now fat_rootsecs: FIRSTDATA - FIRSTROOT)
FAT_FIRSTDATA   equ     $ECB4   ; first data sector (word)
FAT_CURCLUS     equ     $ECB6   ; current cluster in the open file's chain (word)
FAT_CLUSSEC     equ     $ECB8   ; sector index within current cluster (byte)
FAT_FIRSTCLUS   equ     $ECB9   ; first cluster of the found file (word)
FAT_FILESIZE    equ     $ECBB   ; file size in bytes (4-byte LE)
; numFATs / secPerFAT cached at mount so the write path can sync every FAT copy
; without re-reading the boot sector (which would clobber SECTOR_BUF mid-flush).
FAT_NUMFATS     equ     $ED67   ; number of FAT copies (byte; from BPB +16) (1 B)
FAT_SECPERFAT   equ     $ED68   ; sectors per FAT copy (word; from BPB +22)
; This ROM's own slot byte, captured from A at INIT entry (disk/PROVENANCE.md
; §INIT) and used to build the HPHYD CALLF operand, H.RUNC's and the $F37D
; bridge's inter-slot calls. D-DISKROMHOST step 4 (b), 2026-11-25: INSIDE the RAM
; INIT claims ($F195 up, INIT_HIMEM), so a cartridge scanned after ours cannot
; write over it before H.RUNC -- at $ED6A it lay below that claim until H.RUNC's.
; $F1C8: written by nothing on the DISK machine through an MSX-DOS session,
; BASIC at A> and a Disk BASIC file write (scratchpad/claimuse_probe.py,
; AE_claimuse_zb.out); no named cell (DRVA_DPB ends $F1A7, W50A9_RET_DE $F1AA,
; RES_PRINT starts $F1C9). On both references the whole of $F195-$F37F is the
; disk ROM's own work area (AE_claimuse_cf.out / AE_claimuse_hbd.out), so this is
; ours to arrange.
HOOK_SLOT       equ     $F1C8   ; our slot byte for the HPHYD inter-slot hook (1)
; CLAIM_D -- D-DISKROMHOST step 4 (d), scheme D (2026-11-26, scratchpad/AE_schemeD.md): a cartridge
; scanned BEFORE ours that claimed N bytes (HIMEM = $F380 - N at our INIT) moves every NUMBER the
; references publish by -N and leaves every cell where it is (scratchpad/AE_wasize.md). init_gate
; stores the claim here as the value $4030 hands MSX-DOS, GETWRK_AREA - N (N = 0: $DD0E), so $4030
; stays `ld hl,(CLAIM_D) / ret`; layout_rebuild's callers pass their HIMEM as an offset from
; GETWRK_AREA and it adds this cell. In the INIT-private block below HOOK_SLOT: written by nothing
; else through an MSX-DOS session, BASIC at A> and a Disk BASIC file write (AE_claimuse_zb2.out:
; $F195-$F1C7 unwritten), and below $F1C9, so a foreign claim of N <= 439 (NMAX) cannot reach it.
CLAIM_D         equ     $F1C6   ; $4030's work-area value, GETWRK_AREA - N (2 B)
VERIFY_F        equ     $F1C5   ; (1 B) BDOS VERIFY: 0 off, else on -- verify_body stores E, INIT clears it,
                                ; DSKIO's write reads each sector back while set (D-VERIFYREAD); own cell
CLAIM_NMAX      equ     $F380 - (HOOK_SLOT + 1)   ; NOT-A-CELL: 439, the largest foreign claim followed
; BDOS_SRCHIDX (M19) — persistent root-directory search cursor for the runtime
; SFIRST ($11) / SNEXT ($12) dir-search entries ($4FB8/$5006). Absolute 0-based
; index of the NEXT root-dir entry to examine: SFIRST sets it to (first-match+1),
; each SNEXT resumes there and updates it to (its-match+1). SNEXT reconstructs the
; sector cursor from this index alone (>>4 = sector, &15 = entry-in-sector) so it
; never persists FAT_DIRSEC/FAT_DIRREM across calls (those alias the loader
; fat_find/fat_dir_create path). Own-choice free page-3 RAM in the 2-byte gap
; after HOOK_SLOT, before RRND_RECSEC: $E95E..$E95F ($E55E..$E55F, before WBUF
; $E560, when it was grep-cleaned on 2026-07-01; D-FATFLOOR moved it +$400).
; Word (rootEnts <= 254; a word keeps the >>4 sector math clean).
; DOS-phase-only (dead during BASIC/loader); SFIRST always seeds it before any
; SNEXT reads it, so no init-time build is needed. See disk/PROVENANCE.md §BDOS.
BDOS_SRCHIDX    equ     $ED6B   ; runtime dir-search cursor (word; M19)
; The runtime DTA base (where SFIRST/SNEXT write the found 32-byte dir image) is
; the disk work-area's current-DTA cache at the fixed address $F23D. Stock's
; SETDTA-time routine ($5058) writes the new DTA pointer here and stock's SFIRST
; reads it back (PCs $4FCC/$4FEF); on ours it was never written until the $5058
; veneer (setdta_cache_body) was wired to cache DE here (M19). SFIRST/SNEXT read
; it fresh each call. Black-box pinned (entry DE=DTA, write-target $F23D); no stock
; code decoded. See disk/kernel.asm setdta_cache_body / docs/tier2-m19-spec.md.
DOS_DTAPTR      equ     $F23D   ; disk work-area current-DTA cache (word, LE; M19)
; FAT12 working scratch (transient within a single call)
FAT_PARITY      equ     $ECBF   ; 1 = odd cluster, 0 = even (FAT12 nibble pack) (1 B)
FAT_BYTEIDX     equ     $ECC0   ; byte index within a FAT sector (word, 0..511)
FAT_FATSEC      equ     $ECC2   ; FAT sector currently read (word)
FAT_B0          equ     $ECC4   ; first FAT byte of a 12-bit entry (1 B)
FAT_B1          equ     $ECC5   ; second FAT byte of a 12-bit entry (1 B)
FAT_NAMEPTR     equ     $ECC6   ; -> 11-byte search name (word)
FAT_DIRSEC      equ     $ECC8   ; current root-dir sector being scanned (word)
FAT_DIRREM      equ     $ECCA   ; root-dir sectors remaining to scan (word)

; --- BDOS / FCB layer (disk/PROVENANCE.md §BDOS interface) ------------------
; CP/M-compatible FCB file access reached through the SYSTEM-sysvar vector
; INIT installed. Call number in C, FCB pointer in DE; result in A (MSX2 TH,
; MSX-DOS BDOS call conventions). Names in the FCB are an 11-byte 8.3 field at
; +1 (8 name + 3 ext, space-padded, upper-case) — exactly the layout fat_find
; consumes. The DTA defaults to $0080 (page 0) but is settable via BDOS $1A —
; BLOAD points it at a writable page-3 buffer (page 0 is BIOS ROM under Disk
; BASIC, so the $0080 default would silently fail there).
BDOS_F_STROUT   equ     $09     ; '$'-terminated string output (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_OPEN     equ     $0F     ; FCB Open           (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_SFIRST   equ     $11     ; FCB Search First   (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_SNEXT    equ     $12     ; FCB Search Next    (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_CLOSE    equ     $10     ; FCB Close          (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_SEQRD    equ     $14     ; FCB Sequential Read(MSX2 TH, MSX-DOS BDOS table)
BDOS_F_SEQWR    equ     $15     ; FCB Sequential Write (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_CREATE   equ     $16     ; FCB Create file    (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_SETDTA   equ     $1A     ; Set DTA Address (DE=new DTA) (MSX2 TH, MSX-DOS BDOS table)
BDOS_F_RDBLK    equ     $27     ; Random Block Read (RDBLK) (MSX-DOS BDOS table; the
                                ; MSX-DOS-1 boot loads MSXDOS.SYS through this — a3 §8.9)
DTA_DEFAULT     equ     $0080   ; default Disk Transfer Area (MSX2 TH, BDOS conv.)
RECSIZE         equ     128     ; sequential-read record size (MSX2 TH, FCB seq I/O)
RECPERSEC       equ     4       ; 512 / 128 = records per 512-byte sector (own deriv.)
; BDOS sequential-read position (own choice; free page-3 RAM after FAT scratch).
; Records consumed from SECTOR_BUF so far; RECPERSEC means "buffer exhausted,
; refill on next read". File position otherwise lives in the FAT iterator.
BDOS_RECIDX     equ     $ECCC   ; next 128-byte record within SECTOR_BUF (0..4)
; Settable DTA pointer (BDOS call $1A). Under MSX-DOS the default DTA is page-0
; RAM at $0080; under the combined Disk-BASIC machine page 0 is BIOS ROM, so a
; SeqRead to $0080 silently fails. BDOS $1A lets the caller (BLOAD) point the
; record transfer at a writable buffer it controls. Default preserves MSX-DOS
; compatibility. Own choice for the variable location: free page-3 RAM after the
; BDOS record index. See disk/PROVENANCE.md §BDOS interface.
BDOS_DTA        equ     $ECCD   ; current DTA pointer (word; default DTA_DEFAULT)
; Bytes of the open file still undelivered, seeded from FAT_FILESIZE by Open and
; decremented one record (up to RECSIZE) per Sequential Read. Bounds the partial
; final record and the EOF point by the true file size (own choice for the
; variable location: free page-3 RAM past basic-core's DISK_DTA buffer
; $E4C2..$E541, clear of every other disk and basic region). 4-byte LE.
; See disk/PROVENANCE.md §BDOS interface + §Scratch RAM.
BDOS_BYTESLEFT  equ     $ED4F   ; bytes of the open file not yet delivered (4-byte LE)
; M33 (tier2-m33-m32-fcb-position-spec.md §2.1): records delivered so far by
; Sequential Read (K), seeded 0 by fopen_fill_body, incremented once per
; DELIVERED record (not on EOF) by wrseq_body's read-branch write-back. Every
; nearby free-RAM gap ($E4C2..$E541 basic-core's DISK_DTA, the $E7E8-$E7FF
; WRBLK/M30/M31 gap, the $E762-$E7E7 boot/CONIN scratch chain) is already
; fully claimed by prior milestones (verified by grep across every `equ
; $Exxx`/label-arithmetic RAM address in disk/*.asm and disk/*.inc — no gap
; remained) — so this cell is placed just past DRV_TRAMP ($E800-$E813, the
; last claimed cell before the $F1xx+ fixed BIOS/MSX2-work-area region),
; grep-clean free page-3 RAM. See disk/PROVENANCE.md §Scratch RAM.
BDOS_SEQREC     equ     $EDB3   ; records delivered so far by RDSEQ (K, word)

; --- D-FATENG: the FAT engine's BUFFER BASES, held in RAM ---------------------
; 🏗️ Joost, 2026-09-18, ruling (b) of spec-diskcode-eviction.md §6.4a:
; *"parametrize where it has its buf"*. ONE FAT12 engine serves BDOS and (later)
; Disk BASIC, and the two want different buffers -- so the base is loaded rather
; than assembled in. `ld hl,(DBUF_PTR)` is the SAME 3 BYTES as `ld hl,SECTOR_BUF`
; and costs 6 extra T-states, which is why (b) was affordable at all.
;
; ⚠️ THIS HOME IS PROVISIONAL AND SCOPED TO THE BDOS-ONLY SLICE. $E816 is
; grep-clean in the DISK work area (it is the byte after BDOS_SEQREC, by the same
; method the comment above used), but it lies INSIDE basic-core's FWBUF
; ($E7C0-$E9BF) -- so the moment the engine also serves Disk BASIC these cells
; must move, because the engine would be using that very buffer as its metadata
; scratch. The final home is basic-core's own FAT control block ($E9C0..), which
; this direction FREES when the sub.rom fatprim tenant is deleted.
; 🔴 DO NOT let a later slice read these while BASIC's metadata buffer is live.
DBUF_PTR        equ     $EDB5   ; word -> the 512-byte DATA/sector buffer
MBUF_PTR        equ     $EDB7   ; word -> the 512-byte FAT/dir METADATA buffer

; --- BDOS / FAT12 WRITE-back scratch (disk/PROVENANCE.md §Scratch RAM) -------
; The write side mirrors the read side's "single open file" model: at most one
; file is open for write at a time, and its position lives in these vars. The
; FCB bookkeeping POSITION fields (+12 EX, +28/29 current cluster, +30 cluster
; index, +32 CR) are a MAINTAINED MIRROR as of M33 (tier2-m33-m32-fcb-position-
; spec.md §0/§2) — wrseq_body's read branch writes them back after every
; delivered RDSEQ record so the user FCB tracks true file position byte-for-
; byte (bar +25 dirloc, M22a class, out of scope); the other FCB fields
; (+14/15/16/17/24/25/26/27/31) remain untouched, as before. All own-choice
; free page-3 RAM after BDOS_BYTESLEFT ($E542..$E545), clear of SECTOR_BUF /
; FAT_* / every basic-core region. See disk/PROVENANCE.md §Scratch RAM.
BDOS_WRMODE     equ     $ED53   ; 1 = a file is open for sequential write (byte)
BDOS_WRCLUS     equ     $ED54   ; chain-tail cluster currently being filled (word)
BDOS_WRFIRST    equ     $ED56   ; file's first cluster, 0 until first allocated (word)
BDOS_WRSECIDX   equ     $ED58   ; sector index within the current cluster (byte)
BDOS_WRBUFLEN   equ     $ED59   ; bytes of the current sector in SECTOR_BUF (word, 0..512); a SEQWRITE file's are on disk already (D-SEQMIX)
BDOS_WRBYTES    equ     $ED5B   ; total bytes written so far = final file size (4-byte LE)
BDOS_DIRSEC     equ     $ED5F   ; logical sector holding the open file's dir entry (word)
BDOS_DIROFF     equ     $ED61   ; byte offset of that dir entry within its sector (word)

; --- D-FATENG Option 2: the shared FAT body's names, bound to THIS ROM's cells -
; disk/docs/spec-diskcode-eviction.md §6.4a. basic/fat-prim-body.inc is assembled
; into BOTH sub.rom and disk.rom, one source, and it spells its buffers and
; write-state in basic-core's names. Each ROM binds them to its OWN cells with an
; EQU -- Joost, 2026-09-18: *"keep it a per-ROM equate for now"*. Zero bytes: an
; EQU emits nothing, and each ROM assembles its own instance of the body, so
; there is still exactly ONE engine per ROM.
; ⚠️ A RUNTIME POINTER BECOMES NECESSARY ONLY WHEN disk.rom's ENGINE SERVES BOTH
; BDOS **AND** Disk BASIC -- two contexts inside ONE assembly, which an equate
; cannot express. That is steps 9-12, not now. D-FATENG slices 1-2 built that
; pointer (DBUF_PTR/MBUF_PTR) against disk/fat.asm's own copy, which Option 2
; deletes; it comes back here when the consumers move.
; 🔴 NEUTRAL NAMES, AND FSECTOR_BUF IS WHY. The first cut aliased the body's
; own `FSECTOR_BUF` to our SECTOR_BUF -- but disk/basic-resident-abi.inc ALREADY
; binds FSECTOR_BUF to basic-core's file buffer, because hk_lrset walks BASIC's field
; buffer across the slot. Re-pointing it would have silently broken LSET/RSET.
; The assembler caught it as a redefinition; nothing else would have.
; So the shared body no longer spells basic-core's names at all: it uses
; FAT_DBUF / FAT_MBUF, and each ROM binds those to its own cells.
FAT_DBUF        equ     SECTOR_BUF      ; the body's data/sector buffer
FAT_MBUF        equ     WBUF            ; the body's FAT/dir metadata buffer
FWR_CLUS        equ     BDOS_WRCLUS     ; chain-tail cluster being filled
FWR_FIRST       equ     BDOS_WRFIRST    ; file's first cluster
FWR_SECIDX      equ     BDOS_WRSECIDX   ; sector index within the cluster
FWR_BUFLEN      equ     BDOS_WRBUFLEN   ; bytes buffered
FWR_BYTES       equ     BDOS_WRBYTES    ; total bytes written
FWR_DIRSEC      equ     BDOS_DIRSEC     ; dir entry's sector
FWR_DIROFF      equ     BDOS_DIROFF     ; dir entry's offset within it
FAT_WRTMP       equ     $ED63   ; transient scratch for the FAT12 write helpers (word)
FAT_WRTMP2      equ     $ED65   ; second transient (free-cluster scan cached sector)
; A SECOND 512-byte sector buffer used by the WRITE-BACK FAT/dir helpers. The
; data being accumulated for a Sequential Write lives in SECTOR_BUF; allocating a
; cluster (FAT scan) or stamping the directory entry must read/write OTHER sectors
; without disturbing that in-flight data, so the write-back metadata path uses
; this independent buffer. Own-choice free page-3 RAM after the write scratch.
; D-BUFMERGE: = main's FWBUF (was $E560, whose top 416 B lay inside main's
; FSECTOR_BUF). The 76 B of this ROM's cells that lay inside $E5C0..$E9BF moved
; to $E560..$E5AB, which main keeps free by assertion (basic/sysvars.inc caps its
; cells below $E560 and places its buffers from $E5C0) -- a buffer fill of our
; own would otherwise have overwritten our own state.
; 🏗️ D-FATFLOOR (2026-10-13): the buffer pair went to the diskless floor
; ($E36A / $E56A) and this cell block +$400 to $E960..$E9AB, ABOVE the
; buffers; disk.asm now asserts the block disjoint from them, not below them.
WBUF            equ     $EAAD   ; write-back FAT/dir sector buffer ($EAAD..$ECAC) = main FWBUF
; D-WCACHE W3a: the "defer this FAT write" flag (0 normal, 1 a deferral asked for,
; 2 a deferred write pending) -- set only by ffds_alloc's chain extension, cleared
; by fat_mount, fat_write_buf_allfats and every failure exit. One address in BOTH
; maps: the shared FAT body reads it in disk.rom and in the sub-ROM. MEASURED free
; (scratchpad/ramfree_e5ac.py -> ramfree_e5ac.out: a write watchpoint on
; $E5AC..$E5BF across MSX-DOS, every BASIC disk verb, CALL SYSTEM and DOS again
; saw only its own control POKE); disk.rom's cells end at MBUF_PTR $E5AB, and
; BASIC's map keeps $E560..$E5BF for disk.rom.
FAT_DEFER       equ     $EDB9   ; (1)
; D-LOADFAT (2026-11-06): LOAD's FAT read cache -- the FAT sector number (low byte)
; FAT_MBUF holds, 0 = none yet. Consulted and written ONLY while FAT_DEFER's bit 7
; is set: hk_dpload's tokenised read sets it, a mount zeroes FAT_DEFER and
; hdl_answer clears the bit, so no other reader -- a channel, the directory work,
; MSX-DOS -- ever touches this byte.
FAT_FCACHE      equ     $EDBA   ; (1)
; RND_HSEC -- D-PUTCACHE (W4 for PUT, docs/spec-diskwcache.md): the data sector a
; random-file PUT left in FAT_MBUF UNWRITTEN, 0 = none (sector 0 is the boot sector,
; never a record's). read_sector serves a re-read of it from the buffer and writes it
; out before any other read; rh_flush writes it on demand. Zeroed by INIT.
RND_HSEC        equ     $EDBB   ; (2) word
; D-DSKCHG (2026-11-08): JIFFY at the last sector command (fdc_prep sets it every
; time); DSKCHG's clock -- "unchanged" only within ~2 s of it. 🔴 The first cut put it
; at $EDBB -- RND_HSEC, the cell just above, which basic/sysvars.inc does not declare,
; so the BASIC-side RAM map showed it "unattributed": every sector command then wrote
; JIFFY into D-PUTCACHE's held-sector tag and MSX-DOS stopped booting. basic/sysvars.inc
; holds the same address under the same name.
DSK_STAMP       equ     $EDBD   ; (2)

; Random-record ($21/$22) positioning scratch — transient within a single
; rrnd_position / rrnd_sector call (NO cross-call lifetime, no init needed).
; MUST be RAM: these stash a value across a register-clobbering seek/multiply
; loop, and the earlier ROM-`db` cells silently no-op'd their stores (writes to
; the read-only $4000-$7FFF ROM image), forcing record-in-sector to 0 -> wrong
; record. Own-choice free page-3 RAM immediately after WBUF (PROVENANCE §Scratch RAM).
RRND_RECSEC     equ     $ED6D   ; r0 & 3 (record-in-sector) across the sector-seek loop (1 B)
RRND_CLUSSEC    equ     $ED6E   ; FAT_CLUSSEC-1 across the cluster->sector multiply loop (1 B)

; $26 WRBLK ($47BE, M28 tier2-m28-blockrandom-spec.md) 24-bit positioning scratch --
; transient within a single wrblk_body call (no cross-call lifetime, no init needed).
; 🏗️ D-BUFMERGE (2026-09-24): this block, $E7E8..$E819 below, now sits at
; $E57A..$E5AB, same internal layout, because $E7C0.. is the shared WBUF. The
; placement notes that follow are the ORIGINAL siting, kept as history.
; Own-choice free page-3 RAM in the $E7E8-$E7FF gap (after CONIN_COUNT $E7E7, before
; DRV_TRAMP $E800) -- clear of every other disk region; CONIN's own buffer state is
; dead whenever a BDOS FCB call (WRBLK included) is running, same lifetime-disjoint
; reuse discipline as RRND_RECSEC/RRND_CLUSSEC above (own choice, no oracle bytes).
WRBLK_REC       equ     $ED87   ; D-WRBLKRS: the 24-bit BYTE offset of the next write (3 bytes)
WRBLK_RECSEC    equ     $ED8A   ; wrblk_seek's sector-in-cluster (D-WRBLKSEEK; its old record-in-sector use died with D-WRBLKRS) (1 B)
COPY_STAMP      equ     WRBLK_REC ; $ED87..$ED8A: Disk BASIC COPY's SOURCE time/date (+22..25), carried to the
                                  ; destination as the CF-3300 does (D-COPYDATE). Borrowed: only DOS WRBLK uses
                                  ; these 4 bytes, and it never runs inside a BASIC COPY (4 B)
WRBLK_RS        equ     $ED8B   ; resolved record size (FCB+14..15, 0 -> 128) (word)
DPSAVE_SP       equ     WRBLK_RS ; hk_dpsave's SP, so sv_load_error ABORTS the SAVE (D-DISKFULL).
                                 ; Borrowed like COPY_STAMP: only DOS WRBLK uses it, never inside
                                 ; a BASIC SAVE (word)
WRBLK_CNT       equ     $ED8D   ; D-WRBLKRS: BYTES still to write this call (word, counts down to 0)
WRBLK_REQ       equ     $ED8F   ; the ORIGINAL requested count (word; HL is pinned-preserved
                                ; on exit, per [LANDED-A] -- WRBLK returns HL = requested, not actual)
WRBLK_PREVCLUS  equ     $ED91   ; cluster before the current step's advance/allocate (word);
                                ; needed to link a freshly-allocated cluster onto the true
                                ; last-good cluster (fat_advance may leave FAT_CURCLUS on a
                                ; stale EOC/free value once the chain is exhausted)
WRBLK_KEEPCNT   equ     $ED93   ; shrink path: clusters-to-keep walk countdown (word)
WRBLK_NEXTCLUS  equ     $ED95   ; shrink path: next cluster to free, saved across the
                                ; fat_write_fat_entry call that frees the CURRENT one (word)
; M29 (tier2-m29-wrblk-position-cursor-spec.md) incremental position cursor --
; persists wrblk_position_ext's iterator ACROSS calls within one wrblk_body loop
; (WRBLK_REC is strictly non-decreasing, kernel.asm:913) so a same-sector or
; next-sector call can skip the from-head fat_open+re-walk. Cross-call lifetime
; WITHIN a single wrblk_body invocation only -- wrblk_body clears CURVALID once
; before wrblk_loop.
WRBLK_CURVALID  equ     $ED97   ; byte: 0 = iterator not yet positioned this call (1 B)
WRBLK_CURSEC    equ     $ED98   ; word: sector-in-file the iterator currently sits
                                ; on (meaningful only when CURVALID=1)
; M30 (tier2-m30-alloc-hint-spec.md) FAT allocator next-free scan cursor -- a FAT
; cell (fat_alloc_cluster is reached from WRBLK/WRSEQ/create-then-write, not just
; WRBLK), parked in the $E7E8-$E7FF gap just past the WRBLK cells above only
; because it is the next free byte pair; per-operation lifetime: reset to 2 by
; fat_mount (which runs once at the start of every BDOS file operation, always
; before any fat_alloc_cluster), then advanced past each cluster claimed within
; that operation so a multi-cluster allocation scans forward instead of
; restarting from 2 every time (kills the O(n^2) rescan). Own-choice free
; page-3 RAM; see disk/PROVENANCE.md §Scratch RAM.
FAT_ALLOCHINT   equ     $ED9A   ; next cluster to try in fat_alloc_cluster's scan (word)
; The one-shot RR*RS size multiply (wrblk_mul_rr_rs) runs AFTER fat_find's SECTOR_BUF
; scan and BEFORE fat_dir_update's own WBUF read/write -- no fat_alloc_cluster/
; fat_write_fat_entry call happens during the multiply itself, so WBUF's tail is safe,
; transient scratch for it (avoids growing the tight $E7E8-$E7FF gap; WBUF is a full
; 512-byte sector buffer with ample unused tail at any single instant it's not mid-I/O).
WRBLK_MULACC    equ     WBUF + 500  ; 32-bit product accumulator (LE)
WRBLK_MULOP     equ     WBUF + 504  ; 32-bit shifted-RS operand (LE), doubled each iteration
WRBLK_MULN      equ     WBUF + 508  ; 24-bit shifting copy of RR_start (bit tested each iteration)

; FCB / directory-entry field offsets (Microsoft FAT spec §3.4; MSX2 TH FCB).
DIRENT_FIRSTCLUS equ    26      ; first-cluster word (LE) within a dir entry
DIRENT_FILESIZE equ     28      ; file-size dword (LE) within a dir entry
EOC             equ     $0FFF   ; end-of-chain marker we write (Microsoft FAT spec §3.2)

; --- BPB field offsets within the boot sector (Microsoft FAT spec §3.1) -----
BPB_BYTSPERSEC  equ     11      ; bytes per sector (word LE)
BPB_SECPERCLUS  equ     13      ; sectors per cluster (byte)
BPB_RSVDSECCNT  equ     14      ; reserved sectors incl. boot (word LE)
BPB_NUMFATS     equ     16      ; number of FAT copies (byte)
BPB_ROOTENTCNT  equ     17      ; max root-directory entries (word LE)
BPB_FATSZ16     equ     22      ; sectors per FAT copy (word LE)

; --- FDC: National-style memory-mapped WD2793 (disk/PROVENANCE.md §FDC) -----
; Register addresses + drive-latch bit map: openMSX src/fdc/NationalFDC.cc (GPL,
; allowed for hardware register maps). Status/command bits: WD2793 datasheet.
; The registers are memory-mapped into ROM page 1 at the canonical $7FB8 window;
; the driver runs here (slot 3-1, $4000-$7FFF) and addresses them directly.
; WINDOW EXTENT (root-caused 2026-07-04): the National decode is PARTIAL -- these 8
; registers are MIRRORED x8 across the whole $7F80-$7FBF range, so EVERY address in
; $7F80-$7FBF aliases a register, not just $7FB8-$7FBF. No ROM code/data may live in
; $7F80-$7FBF (enforced by the FDC-window guard in runtime.asm's tail).
FDC_STATUS      equ     $7FB8   ; read = status, write = command
; ($7FB9 = track register — unused: seeks target the track via the Data register.)
FDC_SECTOR      equ     $7FBA   ; sector register
FDC_DATA        equ     $7FBB   ; data register
FDC_CTRL        equ     $7FBC   ; write = drive/side/motor latch; read = IRQ/!DRQ

; Control-latch write bits (NationalFDC.cc writeMem). Single-drive machine: only
; drive A is selected (drive B would be $02 — never used).
CTRL_DRIVE_A    equ     $01     ; select drive 0 (A)
CTRL_SIDE       equ     $04     ; side select (0 = side 0, 1 = side 1)
CTRL_MOTOR      equ     $08     ; motor on

; WD2793 status-register bits (WD2793 datasheet). BUSY (bit 0) and DRQ (bit 1)
; are polled with literal `bit 0,a` / `bit 1,a` in the transfer loops (a `bit`
; index cannot take a mask), so only the error-mask bits below need symbols.
ST_LOST         equ     $04     ; lost data (Type II)
; D-FDCDI: Type II attempts that may wait for their FIRST DRQ with interrupt
; windows (when the caller had interrupts on); the attempt after them waits masked,
; as every attempt used to. FDC_IFF counts them: 1 + 2 x windowed retries used.
FDC_WINMAX      equ     3
ST_CRC          equ     $08     ; CRC error
ST_RNF          equ     $10     ; record not found (Type II)
ST_WP           equ     $40     ; write protect / write fault
ST_NOTRDY       equ     $80     ; drive not ready

; WD2793 commands. Type I flags: head-load $08 + verify $04, step-rate 0 = 6 ms.
CMD_RESTORE     equ     $0C     ; restore to track 0  ($00 + headload + verify)
CMD_SEEK        equ     $1C     ; seek to (Data reg)  ($10 + headload + verify)
CMD_READ        equ     $80     ; read sector, single record
CMD_WRITE       equ     $A0     ; write sector, single record (Type II, $A0 base)
; ($D0 = force interrupt / abort — unused: no command is ever aborted mid-flight.)

; D-DISKHIMEM (2026-10-19): the disk machine's boot HIMEM -- the CF-3300's
; (scratchpad/clearmax_run.out, diskfre_run.out). disk/kernel.asm install_basic_hooks.
DISK_HIMEM      equ     $DE77
; D-BOOTKEYS CTRL RAM (2026-10-27, Joost: "Ctrl boot should behave same as ref"):
; the CF-3300's boot HIMEM with CTRL held -- one drive, a smaller disk work
; area (scratchpad/bootkeys_run.out). disk/init.asm set_ramad.
CTRL_HIMEM      equ     $E48D   ; NOT-A-CELL: a HIMEM value, stored at boot
