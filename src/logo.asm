logo_show:
                ; Upload pattern table.
                ld      hl,(CGPBAS)
                ld      bc,8 * logo_patoffset
                add     hl,bc
                ex      de,hl
                ld      hl,logo_patterns
                ld      bc,8 * logo_npatterns
                call    ldirvm

                ; Upload colour table.
                ld      hl,(T32COL)
                ld      bc,logo_patoffset / 8
                add     hl,bc
                call    setwrt
                ld      b,10
                ld      a,$F1
plot_logo_col:
                out     (VDP_DATA),a
                djnz    plot_logo_col
                ld      a,$E1
                out     (VDP_DATA),a
                ld      a,$E1
                out     (VDP_DATA),a

                ; Upload name table.
                ld      hl,(NAMBAS)
                ld      bc,logo_namoffset
                add     hl,bc
                ex      de,hl
                ld      hl,logo_names
                ld      b,logo_namheight
plot_logo_nam:
                push    bc
                push    hl
                push    de
                ld      bc,logo_namwidth
                call    ldirvm
                pop     hl              ; value of DE
                ld      bc,32
                add     hl,bc
                ex      de,hl
                pop     hl              ; value of HL
                ld      bc,logo_namwidth
                add     hl,bc
                pop     bc
                djnz    plot_logo_nam

                ; Select 16x16 sprites.
                ld      a,(RG1SAV)
                or      $02
                ld      b,a
                ld      c,$01
                call    wrtvdp

                ; Upload sprite pattern table.
                ld      hl,logo_spritepat
                ld      de,(PATBAS)
                ld      bc,32
                call    ldirvm

                ; Upload sprite attribute table.
                ld      hl,logo_spriteattr
                ld      de,(ATRBAS)
                ld      bc,8
                call    ldirvm

                ret

logo_patoffset: equ     131
logo_npatterns: equ     91
logo_patterns:
                db      $00,$00,$00,$00,$00,$00,$00,$00
                db      $00,$00,$00,$00,$00,$00,$00,$01
                db      $00,$00,$00,$00,$00,$00,$00,$FF
                db      $00,$00,$00,$00,$00,$00,$00,$C0
                db      $01,$03,$03,$03,$03,$07,$07,$07
                db      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                db      $C0,$80,$80,$80,$80,$00,$00,$00
                db      $00,$01,$07,$0F,$1F,$3F,$7F,$FF
                db      $3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                db      $80,$E0,$F0,$F8,$FC,$FC,$FC,$FC
                db      $0F,$0F,$0F,$0F,$1F,$1F,$1F,$3F
                db      $FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF
                db      $1F,$7F,$FF,$FF,$FF,$FF,$FF,$FF
                db      $80,$E0,$E0,$F0,$F0,$F0,$F0,$E1
                db      $00,$7F,$7F,$7F,$FF,$FF,$FF,$FF
                db      $00,$F0,$F0,$F0,$E0,$E0,$E0,$C0
                db      $00,$03,$0F,$1F,$3F,$7F,$FF,$FF
                db      $7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                db      $00,$00,$00,$01,$03,$07,$0F,$0F
                db      $0F,$3F,$FF,$FF,$FF,$FF,$FF,$FF
                db      $F0,$FC,$FF,$FF,$FF,$FF,$FF,$FF
                db      $00,$00,$00,$00,$80,$80,$80,$80
                db      $00,$01,$01,$03,$03,$03,$07,$07
                db      $E7,$C7,$C7,$8F,$8F,$8F,$1F,$1F
                db      $FC,$FC,$FC,$FC,$FC,$FC,$F8,$F8
                db      $3F,$3F,$3F,$7F,$7F,$7F,$FF,$FF
                db      $FC,$F8,$F8,$F1,$F1,$F1,$E3,$E3
                db      $E1,$E1,$E1,$C3,$C3,$C3,$87,$87
                db      $C1,$C3,$C3,$83,$87,$87,$0F,$0F
                db      $FF,$FF,$FF,$FF,$FF,$FF,$FE,$FE
                db      $CF,$8F,$8F,$1F,$1F,$1F,$3F,$3F
                db      $1F,$1F,$1F,$3F,$3F,$3F,$3F,$7F
                db      $FC,$F8,$F8,$F1,$F1,$F1,$F3,$F0
                db      $FF,$FF,$FF,$FF,$FF,$FF,$FE,$00
                db      $80,$80,$80,$00,$00,$00,$00,$00
                db      $07,$07,$0F,$0F,$0F,$1F,$1F,$1F
                db      $FF,$FF,$FE,$FE,$FE,$FC,$FC,$FC
                db      $1F,$00,$00,$00,$00,$00,$00,$00
                db      $F8,$00,$00,$00,$00,$00,$00,$00
                db      $00,$00,$01,$01,$01,$03,$03,$03
                db      $E3,$E3,$C7,$C7,$C7,$8F,$8F,$8F
                db      $FF,$FF,$FF,$FF,$FF,$FE,$FE,$FE
                db      $87,$87,$0F,$0F,$0F,$1F,$1F,$1F
                db      $0F,$0F,$1F,$1F,$1F,$3F,$3F,$3F
                db      $FE,$FE,$FC,$FC,$FC,$F8,$F8,$F8
                db      $3F,$3F,$7F,$7F,$7F,$FF,$FF,$FF
                db      $F8,$F8,$F0,$F0,$F0,$E0,$E0,$E0
                db      $7F,$7F,$7F,$7F,$3F,$3F,$1F,$0F
                db      $F8,$FC,$FE,$FF,$FF,$FF,$FF,$FF
                db      $00,$00,$00,$80,$C0,$E0,$E0,$F0
                db      $1F,$3F,$3F,$3F,$7F,$7F,$7F,$7F
                db      $FC,$F8,$F8,$F8,$F0,$F0,$F0,$F0
                db      $00,$00,$7F,$7F,$FF,$FF,$FF,$FF
                db      $00,$00,$C0,$C0,$80,$80,$80,$00
                db      $03,$07,$07,$07,$0F,$0F,$0F,$0F
                db      $FF,$FF,$FF,$FF,$FE,$FE,$FE,$FE
                db      $8F,$1F,$1F,$1F,$3F,$3F,$3F,$3F
                db      $FE,$FC,$FC,$FC,$F8,$F8,$F8,$F8
                db      $3F,$7F,$7F,$7F,$FF,$FF,$FF,$FF
                db      $F8,$F1,$F1,$F1,$E3,$E3,$E3,$E3
                db      $E0,$C0,$C0,$C0,$80,$87,$87,$07
                db      $07,$03,$01,$00,$00,$FE,$FE,$FE
                db      $FF,$FF,$FF,$FF,$7F,$3F,$3F,$3F
                db      $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0
                db      $7F,$FF,$FF,$00,$FF,$FF,$00,$FF
                db      $E1,$E1,$E1,$00,$E7,$FF,$00,$FF
                db      $FF,$FF,$FE,$00,$FC,$F8,$00,$F0
                db      $1F,$1F,$1F,$00,$3F,$3F,$00,$7F
                db      $FC,$FC,$FC,$00,$FC,$FF,$00,$FF
                db      $7F,$7F,$7F,$00,$FF,$FF,$00,$FF
                db      $F0,$F0,$F0,$00,$E1,$E1,$00,$C3
                db      $FF,$FF,$FF,$00,$FF,$FF,$00,$FF
                db      $E1,$E1,$E1,$00,$C1,$C1,$00,$81
                db      $C7,$C7,$C7,$00,$CF,$FF,$00,$FF
                db      $FF,$FF,$FE,$00,$FC,$FC,$00,$F0
                db      $0F,$0F,$0F,$00,$0F,$1F,$00,$1F
                db      $E0,$E0,$E0,$00,$C0,$80,$00,$00
                db      $7F,$00,$3F,$1F,$07,$00,$00,$00
                db      $FF,$00,$FF,$FE,$F8,$00,$00,$00
                db      $E0,$00,$80,$00,$00,$00,$00,$00
                db      $7F,$00,$FF,$FF,$00,$00,$00,$00
                db      $FF,$00,$E7,$C7,$01,$00,$00,$00
                db      $FF,$00,$FE,$FC,$F0,$00,$00,$00
                db      $83,$00,$07,$07,$00,$00,$00,$00
                db      $FF,$00,$FF,$FF,$00,$00,$00,$00
                db      $81,$00,$00,$00,$00,$00,$00,$00
                db      $FF,$00,$FF,$7F,$0F,$00,$00,$00
                db      $FF,$00,$FF,$FE,$F0,$00,$00,$00
                db      $0F,$00,$07,$03,$00,$00,$00,$00
                db      $FF,$00,$FF,$FF,$7F,$00,$00,$00
                db      $FE,$00,$F8,$E0,$80,$00,$00,$00

logo_namwidth:  equ     22
logo_namheight: equ     8
logo_namoffset: equ     165
logo_names:
                db      $83,$83,$83,$83,$83,$83,$83,$83,$84,$85,$86,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
                db      $83,$83,$83,$83,$83,$83,$83,$83,$87,$88,$89,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83,$83
                db      $83,$83,$83,$8A,$8B,$8C,$83,$83,$8D,$8E,$8F,$90,$91,$92,$93,$94,$8C,$95,$96,$97,$98,$83
                db      $83,$83,$99,$88,$9A,$9B,$83,$83,$9C,$9D,$88,$9E,$88,$9F,$A0,$A1,$9B,$A2,$A3,$A4,$A5,$83
                db      $83,$83,$A6,$A7,$A8,$A9,$83,$AA,$88,$AB,$AC,$AD,$A7,$AE,$AF,$B0,$B1,$B2,$B3,$B4,$83,$83
                db      $83,$83,$B5,$B6,$B7,$B8,$83,$B9,$BA,$BB,$BC,$B5,$B6,$BD,$BE,$88,$BF,$C0,$C1,$C2,$83,$83
                db      $83,$83,$C3,$C4,$C5,$83,$83,$C6,$C7,$C8,$C9,$CA,$CB,$CA,$CC,$CD,$CE,$C7,$C8,$CF,$83,$83
                db      $83,$83,$D0,$D1,$D2,$83,$83,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$D2,$DB,$DC,$DD,$83,$83,$83

logo_spritepat:
                db      $7F,$7F,$7F,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00
                db      $F0,$F0,$F0,$F0,$E0,$E0,$E0,$00,$00,$00,$00,$00,$00,$00,$00,$00

logo_spriteattr:
                db      $4B,$56,$00,$08,$2F,$8A,$00,$02
