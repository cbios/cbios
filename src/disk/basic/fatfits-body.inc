; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; fatfits-body.inc -- fat_fits, the free-space verdict of the writers the CF-3300
; refuses whole (D-SAVEFULLSTAMP). Its own file, not a part of fat-prim-body.inc:
; that body sits in a pinned disk.rom region with no room for it, so disk.rom
; includes this one in the free region before $5FE5, and the sub-ROM next to its
; fat-prim-body include. It calls fat_count_free, which both homes assemble.
;
; CLEAN-ROOM: our own code; the behaviour it serves is measured black-box
; (scratchpad/bsvfull_stamp.out, scratchpad/savefull_probe.py).

; fat_fits -- will a file of HL+1 bytes fit in the free clusters? (D-SAVEFULLSTAMP)
;   in:  HL = bytes - 1, CF = its bit 16 (from the caller's `add hl,de`)
;   out: A = 0 fits, $FF does not. Mounts (through fat_count_free).
; For the writers the CF-3300 REFUSES WHOLE -- BSAVE and tokenised SAVE: when the
; image cannot fit it writes only the head and closes (scratchpad/bsvfull_stamp.out,
; savefull_probe.py), so the writer must know BEFORE the data. Call it right
; after the create: the count borrows FAT_DBUF (still empty there) and
; FWR_CLUS/FWR_FIRST, which are re-zeroed here so the stream starts as created.
fat_fits:
                ld      a,h
                rra                         ; A = (bytes - 1) >> 9
                inc     a
                push    af                  ; sectors needed (1..129)
                ld      e,a
                ld      d,0                 ; stop counting at that many: sectors
                call    fat_count_upto      ; >= clusters, so the verdict is the same
                                            ; DE = free (capped); spc now known
                ld      hl,0
                ld      (FWR_CLUS),hl
                ld      (FWR_FIRST),hl
                pop     af
                ld      c,a
                ld      hl,(FAT_SECPERCLUS) ; L = spc (H unread; D-DISKREREAD 2026-11-26, 1 B)
                ld      b,0
ffit_cl:
                inc     b                   ; B = clusters needed = ceil(C / spc)
                ld      a,c
                sub     l
                ld      c,a
                jr      z,ffit_cld
                jr      nc,ffit_cl
ffit_cld:
                xor     a                   ; D-DISKREREAD (2026-11-26): A = 0 and CF iff
                cp      d                   ; D > 0 -- 256+ free: anything fits (both
                ret     c                   ; callers test A alone, `or a`); 2 B
                ld      a,e
                cp      b
                sbc     a,a                 ; $FF: free < needed
                ret
