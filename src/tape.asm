; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: 0BSD
;
; The cassette routines: TAPION, TAPIN, TAPIOF, TAPOON, TAPOUT, TAPOOF, STMOTR.
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
; Writing: CAS-out is PPI port C bit 5, toggled through the PPI's bit set/reset
; mode in timed loops. The rate is the one in the current speed (LOW_), which
; the boot sets to 1200 baud from CS120; 2400 baud when it holds CS240's.
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

CASW_1:         equ     $0B             ; PPI bit set/reset: port C bit 5 := 1
CASW_0:         equ     $0A             ; PPI bit set/reset: port C bit 5 := 0
BREAKX_ENTRY:   equ     $00B7           ; TAPOUT calls BREAKX through its entry:
                                        ; the JP there is part of CAS_BYTECOMP
CASBAUD:        equ     WINWID          ; the write rate for this session, set by
                                        ; TAPOON: 0 = 1200, $FF = 2400 (WINWID is
                                        ; a read cell, idle while writing)

; The half-period counts of the written tones, in djnz iterations. They are
; solved from the VG-8020's own waveform: a watchpoint on the PPI control
; register logged both edges of every cycle of a CSAVE, in T-states, on the
; VG-8020 and on this code. A djnz iteration costs 14 T (with the M1 wait), a
; high half has 20 T of overhead and a plain low half 92 T:
;                 short tone high / low     long tone high / low
;   VG-8020         740 / 756                1492 / 1476
;   this code       748 / 750                1504 / 1498
; The low half of every cycle also holds the ret and the caller's call
; (CAS_DUTY); the last cycle of a bit holds the bit's tail too (CAS_BITCOMP,
; CAS_BITCOMP0), that of a byte the byte's tail (CAS_BYTECOMP), the start bit
; TAPOUT's way to the first data bit (CAS_STARTCOMP), and the leader's last
; cycle the way to the first start bit (CAS_LEADCOMP). Each must
; stay below the smallest count in use (CAS_HHALF24). 2400 baud was not traced:
; its long tone is 1200 baud's short tone, by construction.
; The leader length and this duty are coupled: with this waveform and a 4000
; cycle leader the VG-8020 did not read the header; the reference's leader
; length (below) reads.
CAS_HHALF:      equ     52              ; 1200 baud, high tone (2400 Hz)
CAS_LHALF:      equ     106             ; 1200 baud, low tone (1200 Hz)
CAS_HHALF24:    equ     24              ; 2400 baud, high tone (4800 Hz)
CAS_LHALF24:    equ     CAS_HHALF       ; 2400 baud, low tone: 1200's high tone
CAS_DUTY:       equ     5               ; every low half: ret + the caller's call
CAS_BITCOMP:    equ     13              ; a 1 bit, and the stop bits
CAS_BITCOMP0:   equ     14              ; a 0 bit
CAS_BYTECOMP:   equ     CAS_BITCOMP + 8 ; the byte boundary, with TAPOUT's BREAKX
                                        ; test (113 T: eight iterations)
CAS_STARTCOMP:  equ     12              ; the start bit: TAPOUT's way to bit 0
CAS_LEADCOMP:   equ     23              ; the leader's end: a byte boundary's tail
                                        ; plus TAPOON's own exit and its caller
                                        ; (24), held below CAS_HHALF24
CAS_FLUSHLEN:   equ     32              ; carrier cycles TAPOOF adds at the end

;--------------------------------
; $00F3 STMOTR
; Function : Switches the cassette motor (PPI port C bit 4, 0 = on) through
;            the PPI's bit set/reset mode
; Input    : A - 0: stop; bit 7 set: toggle; otherwise: start
; Output   : A - the PPI command left behind: $08 running, $09 stopped
;            F - $54 when it ends stopped, $10 when a toggle starts it, else
;                the flags AND A gives on the input
; Registers: AF (BC, DE, HL kept)
stmotr:
                and     a               ; Z for 0; the flags of a start
                jr      z,stmotr_off
                jp      p,stmotr_on     ; bit 7 clear: start
                in      a,(GIO_REGS)    ; bit 7 set: toggle
                and     $10             ; Z: running, stop ($54); else start ($10)
                jr      z,stmotr_off
stmotr_on:                              ; the toggle falls through to here
                ld      a,MOTOR_ON
stmotr_out:
                out     (PPI_REGS),a
                ret
stmotr_off:
                ld      a,MOTOR_OFF
                jr      stmotr_out

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
                call    stmotr_on
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
                call    stmotr_off
                pop     af
                ei
                ret

;--------------------------------
; C = the half-period count of the high (cas_short) or low (cas_long) tone at
; the session's rate. A single load, so that the gap between cycles stays small
; enough for 2400 baud to read back.
; Changes  : AF, C
cas_short:
                ld      a,(CASBAUD)
                or      a
                ld      c,CAS_HHALF
                ret     z
                ld      c,CAS_HHALF24
                ret
cas_long:
                ld      a,(CASBAUD)
                or      a
                ld      c,CAS_LHALF
                ret     z
                ld      c,CAS_LHALF24
                ret

;--------------------------------
; Writes one square-wave cycle on CAS-out.
; Input    : C = the half-period count
; Changes  : AF, B
cas_cycle:
                ld      a,CASW_1
                out     (PPI_REGS),a
                ld      b,c
cas_cycle_h:    djnz    cas_cycle_h
                ld      a,CASW_0
                out     (PPI_REGS),a
                ld      a,c
                sub     CAS_DUTY        ; the ret / call land in every low half
                ld      b,a
cas_cycle_l:    djnz    cas_cycle_l
                ret

;--------------------------------
; Writes the last cycle of a bit: its low half is shorter by E, the tail that
; runs after the line goes low and before the next cycle raises it.
; Input    : C = the half-period count, E = the tail
; Changes  : AF, B
cas_cycle_last:
                ld      a,CASW_1
                out     (PPI_REGS),a
                ld      b,c
cas_cyl_h:      djnz    cas_cyl_h
                ld      a,CASW_0
                out     (PPI_REGS),a
                ld      a,c
                sub     e
                ld      b,a
cas_cyl_l:      djnz    cas_cyl_l
                ret

;--------------------------------
; $00EA TAPOON
; Function : Turns the motor on and writes a block's leader
; Input    : A - 0 for a short leader, otherwise a long one
; Output   : C clear
; Registers: AF, BC, HL (DE kept, as on the VG-8020)
; Remark   : The motor first runs silent for 0.567 s, as on the VG-8020. The
;            leader is HEADER x 256 cycles of the high tone (short) or HEADER
;            x 1024 (long): HEADER is 15 at 1200 baud, 31 at 2400.
tapoon:
                push    de
                push    af              ; long / short
                ; The rate, before the motor (its time is free): 2400 baud
                ; when the current speed holds CS240's values, else 1200.
                ld      hl,(LOW_)
                ld      de,(CS240)
                or      a
                sbc     hl,de           ; 0: 2400
                ld      a,h
                or      l
                sub     1               ; C if it was 0
                sbc     a,a             ; $FF: 2400, 0: 1200
                ld      (CASBAUD),a
                di                      ; no jitter in the waveform
                ; One write to port C, as the VG-8020 leaves it: bits 6-7
                ; kept, motor on (bit 4 := 0), CAS-out low (bit 5 := 0), and
                ; keyboard row 7 in the low nibble. Interrupts are off first, so
                ; the key scan cannot rewrite the row under it.
                in      a,(GIO_REGS)
                and     $C0
                or      7
                out     (GIO_REGS),a
                ; 0.567 s of silence: 2 x 33827 x 30 T at 3.58 MHz (30 T: the
                ; MSX adds a wait state to every M1).
                ld      e,2
tapoon_dly1:
                ld      bc,33827
tapoon_dly2:
                dec     bc
                ld      a,b
                or      c
                jr      nz,tapoon_dly2
                dec     e
                jr      nz,tapoon_dly1
                call    cas_short       ; C = the leader's half-period; Z: 1200
                ld      d,15            ; HEADER at 1200 baud
                jr      z,tapoon_hdr
                ld      d,31            ; HEADER at 2400 baud
tapoon_hdr:
                ld      e,0             ; DE = HEADER x 256: short
                pop     af
                or      a
                jr      z,tapoon_last
                sla     d
                sla     d               ; DE = HEADER x 1024: long
tapoon_last:
                dec     de              ; the last cycle comes after the loop
tapoon_emit:
                call    cas_cycle
                dec     de
                ld      a,d
                or      e
                jr      nz,tapoon_emit
                ; The last leader cycle carries the gap to the first start bit:
                ; this routine's end, the caller, and TAPOUT's start up to its
                ; first cycle.
                ld      e,CAS_LEADCOMP
                call    cas_cycle_last
                or      a               ; C clear
                pop     de
                ret

;--------------------------------
; $00ED TAPOUT
; Function : Writes one byte to the tape (after TAPOON)
; Input    : A - the byte
; Output   : C set if CTRL+STOP is down: then the byte is not written
; Registers: All
; Remark   : A start bit (0), 8 data bits LSB first and 2 stop bits (1); a 0
;            is one cycle of the low tone, a 1 two cycles of the high tone.
;            CTRL+STOP is tested before each byte, as on the VG-8020; STOP
;            alone does not break.
tapout:
                di
                push    af
                call    BREAKX_ENTRY    ; C: CTRL+STOP
                jr      c,tapout_brk
                call    cas_long        ; the start bit: a 0
                ld      e,CAS_STARTCOMP
                call    cas_cycle_last
                pop     af
                ld      b,8
tapout_bit:
                rra                     ; the next bit -> C
                push    af
                push    bc
                jr      c,tapout_one
                call    cas_long        ; a 0: one cycle of the low tone
                ld      e,CAS_BITCOMP0
                call    cas_cycle_last
                jr      tapout_next
tapout_one:
                call    cas_short       ; a 1: two cycles of the high tone
                call    cas_cycle
                ld      e,CAS_BITCOMP
                call    cas_cycle_last
tapout_next:
                pop     bc
                pop     af
                djnz    tapout_bit
                call    cas_short       ; 2 stop bits: four high cycles
                call    cas_cycle
                call    cas_cycle
                call    cas_cycle
                ; The last cycle carries the byte boundary: this routine's
                ; end and the next TAPOUT's start up to its first cycle.
                ld      e,CAS_BYTECOMP
                call    cas_cycle_last
                or      a               ; C clear
                ret
tapout_brk:
                pop     de              ; the byte, not written (C kept)
                ret

;--------------------------------
; $00F0 TAPOOF
; Function : Ends writing to the tape: a short carrier, motor off,
;            interrupts on
; Registers: none (A, F, BC, DE, HL kept, as on the VG-8020); CAS-out is left
;            as it was on entry
tapoof:
                push    af
                push    bc
                push    de
                in      a,(GIO_REGS)
                push    af              ; CAS-out as it was
                call    cas_short
                ld      de,CAS_FLUSHLEN
tapoof_lp:
                call    cas_cycle
                dec     de
                ld      a,d
                or      e
                jr      nz,tapoof_lp    ; it ends with CAS-out low
                pop     af
                and     $20
                jr      z,tapoof_lo
                ld      a,CASW_1
                out     (PPI_REGS),a    ; high again, as it was
tapoof_lo:
                pop     de
                pop     bc
                pop     af
                jp      tapiof          ; motor off, interrupts on

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
