; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: 0BSD
;
; The background music player: plays the queues PLAY (or a program, through
; PUTQ and STRTMS) fills, from the timer interrupt.
;
; Taken over from zerobas (basic/playsvc.asm, play_service), measured there
; black-box against a Philips VG-8020 and NMS 8250 (twelve scenarios, every
; PSG write per frame identical on both).
;
; A queue packet, as the player reads it:
;   $FF                      the voice's end
;   [n<<5 | dur 12..8] [dur 7..0] [vol] ([M hi] [M lo]) [tone hi] [tone lo]
;   n = 1 is a rest (vol only); a note has vol, then the envelope period M
;   when vol bit 6 is set, then the tone. vol bit 4: an envelope note.

;--------------------------------
; Function : Plays one interrupt's worth of music. For voices A, B and C, in
;            that order, each only while its MUSICF bit is set: the voice
;            block's first word goes down by one; at 0 the voice reads its
;            next packet. An empty queue leaves the word at 0 (it wraps on
;            the next interrupt). $FF silences the voice (R8+v := 0) and
;            clears its MUSICF bit. A packet sets the word to its duration
;            (0 counts as 65536) and R8+v to vol, the whole byte; a note
;            also restarts the envelope (R13 := vol) when vol bit 4 is set,
;            sets R11 and R12 to M when vol bit 6 is set, and sets the tone,
;            low register first. When every voice has ended, the next round
;            starts as STRTMS does.
; Input    : MUSICF not 0
; Registers: all but SP, alternates too (keyint has saved them)
music:
                ld      hl,VCBA
                ld      iy,(QUEUES)
                ld      bc,$0100+PSG_DATA ; B: the voice's MUSICF bit, C: the data port
music_loop:
                ld      a,(MUSICF)
                and     b
                call    nz,music_voice
                ld      de,VCBB-VCBA
                add     hl,de
                ld      e,6             ; a queue's descriptor
                add     iy,de
                sla     b
                bit     3,b
                jr      z,music_loop
                                        ; every voice at its end: the next round
; $0099 STRTMS (the player falls into it)
; Function : Starts the music PLAY has queued, unless music is playing:
;            when MUSICF is 0 and PLYCNT is not, PLYCNT goes down by one,
;            MUSICF becomes 7 (all three voices) and each voice's first
;            word becomes 1, so that it starts on the next interrupt
; Output   : started: A = 7, F as DEC leaves it on PLYCNT, HL = 1
;            nothing to play: A = 0, F = Z and P/V, HL = PLYCNT
;            playing: A = MUSICF, F as OR A
; Registers: AF, HL
strtms:
                ld      a,(MUSICF)
                or      a
                ret     nz              ; playing
                ld      hl,PLYCNT
                or      (hl)
                ret     z               ; nothing to play
                dec     (hl)
                ld      a,7
                dec     hl              ; MUSICF, right before PLYCNT; F kept
                ld      (hl),a
                ld      hl,1
                ld      (VCBA),hl
                ld      (VCBB),hl
                ld      (VCBC),hl
                ret

music_voice:
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                dec     de
                ld      (hl),d
                dec     hl
                ld      (hl),e
                ld      a,d
                or      e
                ret     nz
                ld      a,(iy+1)        ; get
                cp      (iy+0)          ; put
                ret     z               ; empty: nothing (the word wraps)
                call    music_get
                inc     a
                jr      nz,music_packet
                ld      a,(MUSICF)      ; $FF, the end: D = E = 0, vol 0 and
                xor     b               ; no more than a rest
                ld      (MUSICF),a
                jr      music_vol
music_packet:
                dec     a
                ld      d,a
                and     $1F
                inc     hl
                ld      (hl),a
                dec     hl
                call    music_get
                ld      (hl),a
                call    music_get
                ld      e,a             ; vol
music_vol:
                ld      a,b
                or      $10
                rra                     ; 8 + the voice
                call    music_psg       ; R8+v := vol, first
                ld      a,d
                cp      $40
                ret     c               ; a rest, or the end
                bit     4,e
                ld      a,13
                call    nz,music_psg    ; envelope: R13 := vol
                bit     6,e
                ld      a,11
                call    nz,music_pair   ; M: R11 := low, R12 := high
                ld      a,b
                and     6               ; tone: R2v := low, R2v+1 := high
music_pair:
                push    af
                call    music_get
                ld      d,a             ; high
                call    music_get
                ld      e,a             ; low
                pop     af
                call    music_psg
                inc     a
                ld      e,d
music_psg:
                out     (PSG_REGS),a
                out     (c),e
                ret

; The queue's next byte: the get index steps first (masked with the size),
; then the byte there is read. Changes: AF and the alternate HL
music_get:
                exx
                ld      a,(iy+1)
                inc     a
                and     (iy+3)          ; size
                ld      (iy+1),a
                add     a,(iy+4)
                ld      l,a
                adc     a,(iy+5)
                sub     l               ; the high byte plus the carry
                ld      h,a
                ld      a,(hl)
                exx
                ret
