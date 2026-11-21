; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: 0BSD
;
; The cassette routines: TAPION, TAPIN, TAPIOF.
;
; Taken over from zerobas (tape/tape.asm), where they were written as a
; clean-room implementation against a Philips VG-8020 as a black box, with
; every constant derived from the documented format and the Z80 clock and
; tuned until recordings round-tripped; see zerobas's tape/PROVENANCE.md.
;
; MSX cassette signal (FSK): a '0' bit is one cycle of the low tone, a '1'
; bit two cycles of the high tone (1200 / 2400 Hz at 1200 baud; at 2400 baud
; both an octave higher). A byte is a start bit (0), 8 data bits LSB first and
; stop bits (1). A block starts with a leader: a long run of the high tone.
;
; Reading: CAS-in is PSG register 14 bit 7. The reader is not told the baud:
; TAPION measures the leader's half-period and derives the threshold (LOWLIM)
; between a short half (high tone) and a long one (low tone). That works at
; 1200 and 2400 baud, and at the 3744 baud openMSX plays a .cas file at.

CASIN_R14:      equ     14              ; PSG register holding CAS-in in bit 7
MOTOR_ON:       equ     $08             ; PPI bit set/reset: port C bit 4 := 0
MOTOR_OFF:      equ     $09             ; PPI bit set/reset: port C bit 4 := 1

; TAPION's lock: skip CAS_SKIP edges, then average CAS_RUNLEN leader halves
; into LOWLIM (the >>4 below depends on 16), then check that CAS_VERIFY more
; halves are leader too.
; CAS_SKIP is long on purpose: a program's last bytes (CSAVE ends with seven
; $00 bytes) leave about 180 halves of carrier without a gap. A lock that needs
; 256 + 16 unbroken halves cannot complete inside them; it meets the silence
; after them and locks on the real leader (8000 halves or more).
CAS_SKIP:       equ     256
CAS_RUNLEN:     equ     16
CAS_VERIFY:     equ     128

; Motor on / off through the PPI's bit set/reset mode. Changes: A
tape_motor_on:
                ld      a,MOTOR_ON
                jr      tape_motor_out
tape_motor_off:
                ld      a,MOTOR_OFF
tape_motor_out:
                out     (PPI_REGS),a
                ret

;--------------------------------
; Selects PSG register 14, so that IN (PSG_STAT) gives the CAS-in byte, and
; reads the current CAS-in level.
; Output   : D = the level (bit 7: $00 or $80)
; Changes  : AF, D
cas_level:
                ld      a,CASIN_R14
                out     (PSG_REGS),a
                in      a,(PSG_STAT)
                and     $80
                ld      d,a
                ret

;--------------------------------
; Times one CAS-in half-period: counts until the next edge. The low and the
; high level each have their own loop, which tests bit 7 with ADD A,A; the
; cheaper the loop, the more counts per half and the finer the resolution,
; which matters at fast rates (at 3744 baud a leader half is only a few
; counts). The counts are relative: TAPION calibrates LOWLIM with this same
; loop.
; Input    : D = the current level (bit 7: $00 or $80); PSG register 14 selected
; Output   : D = the new level; B = the count to the edge; C set on a timeout
; Changes  : AF, B, D
cas_half:
                ld      b,1
                bit     7,d
                jr      nz,cas_half_hi
cas_half_lo:                            ; low: count until a rising edge
                in      a,(PSG_STAT)
                add     a,a             ; bit 7 -> C
                jr      c,cas_half_hiset
                inc     b
                jp      nz,cas_half_lo
                scf                     ; the count wrapped: timeout
                ret
cas_half_hi:                            ; high: count until a falling edge
                in      a,(PSG_STAT)
                add     a,a
                jr      nc,cas_half_loset
                inc     b
                jp      nz,cas_half_hi
                scf
                ret
cas_half_hiset:
                ld      d,$80
                or      a
                ret
cas_half_loset:
                ld      d,$00
                or      a
                ret

;--------------------------------
; Classifies the last half-period (count in B) as long or short. LOWLIM is in
; quarter counts, so B*4 is compared: at fast rates the transition half at
; each leader/data boundary (about 1.5 x short) and a real long half (2 x)
; differ by too little for a whole-count threshold.
; Output   : C set if long (low tone), clear if short
; Changes  : AF
cas_islong:
                ld      a,b
                add     a,a
                add     a,a             ; B*4
                push    hl
                ld      hl,LOWLIM
                sub     (hl)            ; C if B*4 < LOWLIM: short
                pop     hl
                ccf
                ret

;--------------------------------
; Reads one data bit (its first edge already found).
; Output   : C = the bit
; Changes  : AF, B, D (a timeout mid-byte reads as a 1, it never hangs)
cas_readbit:
                call    cas_half
                call    cas_islong      ; long: a 0
                jr      c,cas_readbit_0
                call    cas_half        ; short: a 1, two high cycles, 4 halves
                call    cas_half
                call    cas_half
                scf
                ret
cas_readbit_0:
                call    cas_half        ; a 0: one low cycle, 2 halves
                or      a
                ret

;--------------------------------
; $00E1 TAPION
; Function : Turns the motor on and locks onto a block's leader, measuring it
;            to set LOWLIM, the threshold TAPIN reads the bits with
; Output   : C set if it failed: CTRL+STOP pressed
; Registers: All
; Remark   : Silence and the motor's spin-up are waited out: openMSX plays
;            about 2 s of silence before a .cas leader and 1 s between blocks,
;            and a tape can have any gap. The previous block's last bytes can
;            also still be playing. So it (1) waits for an edge, testing
;            CTRL+STOP on each flat timeout (a silent tape is waited on, as on
;            the VG-8020); (2) skips CAS_SKIP edges; (3) measures CAS_RUNLEN
;            halves into LOWLIM; (4) checks CAS_VERIFY / 2 more full cycles
;            against it. A flat or a cycle that is not leader anywhere in (2)
;            to (4) starts over at (1). The loops are lean, so that their
;            overhead per half matches TAPIN's.
tapion:
                call    tape_motor_on
                di
                call    cas_level
tapion_wait:
                call    cas_half        ; C: flat (silence, spin-up)
                jr      nc,tapion_skip0 ; an edge: carrier
                call    breakx          ; C: CTRL+STOP (returns at once)
                ret     c
                jr      tapion_wait
tapion_skip0:
                ld      b,CAS_SKIP & $FF ; 0 = 256
tapion_skip:
                push    bc
                call    cas_half
                pop     bc
                jr      c,tapion_wait   ; flat: not a sustained leader
                djnz    tapion_skip
                ld      hl,0            ; the sum of CAS_RUNLEN halves
                ld      c,CAS_RUNLEN
tapion_meas:
                call    cas_half
                jr      c,tapion_wait
                ld      a,l
                add     a,b
                ld      l,a
                jr      nc,tapion_nocy
                inc     h
tapion_nocy:
                dec     c
                jr      nz,tapion_meas
                ; LOWLIM = 1.75 x the average half, in quarter counts: the sum
                ; of 16 halves times 7 / 16. A long half is about 2 x short, the
                ; transition half at a boundary about 1.5 x, so 1.75 x rejects
                ; the latter with margin.
                ld      b,h             ; BC = the sum (D stays the level)
                ld      c,l
                add     hl,hl
                add     hl,hl
                add     hl,hl           ; 8 x
                or      a
                sbc     hl,bc           ; 7 x
                ld      b,4
tapion_avg:
                srl     h
                rr      l
                djnz    tapion_avg      ; / 16
                ld      a,l
                ld      (LOWLIM),a
                ; Verify that it is a leader, not data still playing: data has
                ; no CAS_VERIFY short halves in a row (every byte has a long
                ; start bit). Whole cycles are compared, which are stable where
                ; single halves alternate: a leader cycle is 2 halves, so
                ; cycle * 2 < LOWLIM (below 1.75: a 0-bit cycle is 2.0) and
                ; cycle * 4 >= LOWLIM (0.875 or more: a 1-bit cycle measured
                ; against a lock on long halves is 0.5).
                ld      hl,LOWLIM
                call    cas_half        ; settle: the half the arithmetic ate
                call    cas_half        ; and the next
                ld      c,CAS_VERIFY / 2
tapion_verify:
                call    cas_half
                jr      c,tapion_wait
                ld      e,b             ; the cycle's first half
                call    cas_half
                jr      c,tapion_wait
                ld      a,b
                add     a,e             ; one cycle, in counts
                jr      c,tapion_wait   ; a dropout
                add     a,a             ; cycle * 2
                jr      c,tapion_wait
                cp      (hl)
                jr      nc,tapion_wait  ; 1.75 x or more: a 0 bit
                add     a,a             ; cycle * 4
                cp      (hl)
                jr      c,tapion_wait   ; below 0.875 x: the lock was on long halves
                dec     c
                jr      nz,tapion_verify
                ret                     ; C clear: locked

;--------------------------------
; $00E4 TAPIN
; Function : Reads one byte from the tape (after TAPION)
; Output   : A = the byte; C set on an error: CTRL+STOP, or no start bit
; Registers: All
; Remark   : CTRL+STOP is tested before each byte, as on the VG-8020.
tapin:
                call    breakx          ; C: CTRL+STOP
                ret     c
                call    cas_level
                ld      hl,0            ; the start bit hunt: 64K halves, more
tapin_hunt:                             ; than a whole leader
                call    cas_half
                ret     c
                call    cas_islong      ; a long half: the start bit (a 0)
                jr      c,tapin_start
                dec     hl
                ld      a,h
                or      l
                jr      nz,tapin_hunt
                scf                     ; no start bit
                ret
tapin_start:
                call    cas_half        ; the start bit's second half
                ret     c
                ld      e,0
                ld      c,8
tapin_bit:
                call    cas_readbit
                rr      e               ; LSB first
                dec     c
                jr      nz,tapin_bit
                ld      a,e
                or      a               ; C clear
                ret

;--------------------------------
; $00E7 TAPIOF
; Function : Ends reading from the tape: motor off, interrupts on
; Registers: none (A, F, BC, DE and HL kept, as on the VG-8020)
tapiof:
                push    af
                call    tape_motor_off
                pop     af
                ei
                ret

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
