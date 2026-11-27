; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause
; --- tok_payload: how many constant bytes follow a program-text byte ----------
; D-LOADBASE (2026-10-22). Shared by the three tokenised loaders -- CLOAD / LOAD
; (sub2/tapeprog.asm ctp_scan, zerobas-sub2 page 0; main's basic/cload.asm until
; D-P0CB's tape-loop slice, 2026-10-24), the tape file skip
; (basic/casmatch-body.inc, zerobas-sub) and the disk LOAD (disk/kernel.asm
; hdl_line) -- each ROM includes its own copy (none can call another's page 1,
; and the tape reader's is on its per-byte path, where a crossing costs time).
; A loader scans the FIRST line of a file for its $00 terminator, because the
; first line's length cannot come from the link words without the SAVING
; machine's text base: the CF-3300 LOADs a program SAVEd with TXTTAB at $C001
; and lists it; zerobas assumed $8001 and loaded garbage
; (scratchpad/txttab_saveload.out). A constant may hold $00 bytes (`256` is
; $1C $00 $01, `&H100` $0C $00 $01; scratchpad/txttab_saveload2.out: the CF-3300
; loads both), so its bytes are passed over by count:
;   $0B octal, $0C hex, $0D line pointer, $0E line number, $1C integer: 2
;   $0F a byte constant: 1      $1D single: 4      $1F double: 8
; (MSX2 TH, the tokenised program format.) A byte after $FF (a function token)
; is never $00, and quoted text cannot be typed with these control codes.
;   in: A = the byte   out: A = its constant's length (0: none)   C clobbered
; CLEAN-ROOM: our own code; the format is the published one (MSX2 TH) and the
; loading behaviour is measured black-box on the CF-3300 (the two .out files
; above). No reference ROM byte read.
tok_payload:
                ld      c,8
                cp      $1F
                jr      z,tpl_c             ; double
                ld      c,4
                cp      $1D
                jr      z,tpl_c             ; single
                ld      c,2
                cp      $1C
                jr      z,tpl_c             ; integer
                cp      $0B
                jr      c,tpl_0             ; below the constant prefixes
                cp      $0F
                jr      c,tpl_c             ; $0B..$0E: two bytes
                ld      c,1                 ; $0F: one byte (D-DISKREREAD 2026-11-26:
                jr      z,tpl_c             ; `ld` keeps the Z of `cp $0F`; 1 B)
tpl_0:
                ld      c,0                 ; $10..$1B (inline digits) and the rest
tpl_c:
                ld      a,c
                ret
