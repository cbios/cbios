; $Id: font.asm,v 1.1.1.1 2004/11/27 02:23:55 mthuurne Exp $
; Fontdata for C-BIOS
;
; Copyright (c) 2002 BouKiCHi.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

B_Font:
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;0
                db      $00,$7C,$44,$7C,$44,$7C,$44,$8C         ;1
                db      $00,$20,$A4,$A4,$20,$30,$48,$84         ;2
                db      $00,$12,$14,$F8,$38,$54,$92,$10         ;3
                db      $00,$10,$7C,$10,$38,$54,$92,$10         ;4
                db      $00,$38,$54,$BA,$10,$38,$54,$FE         ;5
                db      $00,$10,$10,$7C,$10,$10,$10,$FE         ;6
                db      $00,$7C,$44,$44,$7C,$44,$44,$7C         ;7
                db      $00,$40,$7C,$90,$7C,$50,$FC,$10         ;8
                db      $00,$FC,$A4,$A4,$FC,$84,$84,$84         ;9
                db      $00,$04,$EE,$A4,$FE,$AC,$E4,$0C         ;10
                db      $00,$28,$44,$82,$7C,$24,$44,$48         ;11
                db      $00,$24,$CE,$55,$EC,$62,$D2,$44         ;12
                db      $00,$7C,$20,$7C,$44,$7C,$44,$7C         ;13
                db      $00,$1C,$70,$10,$FE,$10,$10,$10         ;14
                db      $00,$FC,$40,$40,$7C,$44,$84,$88         ;15
                db      $00,$00,$7C,$28,$28,$48,$48,$8C         ;16
                db      $10,$10,$10,$10,$FF,$00,$00,$00         ;17
                db      $00,$00,$00,$00,$FF,$10,$10,$10         ;18
                db      $10,$10,$10,$10,$F0,$10,$10,$10         ;19
                db      $10,$10,$10,$10,$1F,$10,$10,$10         ;20
                db      $10,$10,$10,$10,$FF,$10,$10,$10         ;21
                db      $10,$10,$10,$10,$10,$10,$10,$10         ;22
                db      $00,$00,$00,$00,$FF,$00,$00,$00         ;23
                db      $00,$00,$00,$00,$1F,$10,$10,$10         ;24
                db      $00,$00,$00,$00,$F0,$10,$10,$10         ;25
                db      $10,$10,$10,$10,$1F,$00,$00,$00         ;26
                db      $10,$10,$10,$10,$F0,$00,$00,$00         ;27
                db      $81,$42,$24,$18,$18,$24,$42,$81         ;28
                db      $00,$10,$7C,$10,$10,$28,$48,$84         ;29
                db      $00,$10,$7C,$54,$7C,$10,$10,$10         ;30
                db      $00,$10,$10,$54,$54,$94,$10,$30         ;31
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;32
                db      $00,$18,$18,$18,$18,$18,$00,$18         ;33
                db      $00,$6C,$6C,$24,$00,$00,$00,$00         ;34
                db      $00,$24,$7E,$24,$7E,$24,$24,$00         ;35
                db      $14,$3E,$54,$54,$3E,$15,$15,$3E         ;36
                db      $00,$22,$54,$28,$12,$25,$42,$00         ;37
                db      $38,$44,$44,$28,$32,$4A,$7C,$00         ;38
                db      $00,$60,$60,$20,$00,$00,$00,$00         ;39
                db      $00,$08,$10,$10,$10,$10,$08,$00         ;40
                db      $00,$10,$08,$08,$08,$08,$10,$00         ;41
                db      $00,$10,$54,$38,$10,$38,$54,$00         ;42
                db      $00,$00,$10,$10,$7C,$10,$10,$00         ;43
                db      $00,$00,$00,$00,$60,$60,$20,$00         ;44
                db      $00,$00,$00,$7E,$00,$00,$00,$00         ;45
                db      $00,$00,$00,$00,$00,$60,$60,$00         ;46
                db      $00,$02,$04,$08,$10,$20,$40,$00         ;47
                db      $00,$38,$4C,$54,$54,$64,$38,$00         ;48
                db      $00,$10,$30,$10,$10,$10,$38,$00         ;49
                db      $00,$38,$44,$04,$38,$40,$7C,$00         ;50
                db      $00,$78,$04,$38,$04,$04,$78,$00         ;51
                db      $00,$44,$44,$44,$7C,$04,$04,$00         ;52
                db      $00,$7C,$40,$78,$04,$04,$78,$00         ;53
                db      $00,$38,$40,$78,$44,$44,$38,$00         ;54
                db      $00,$7C,$44,$08,$08,$10,$10,$00         ;55
                db      $00,$38,$44,$38,$44,$44,$38,$00         ;56
                db      $00,$38,$44,$44,$3C,$04,$38,$00         ;57
                db      $00,$18,$18,$00,$00,$18,$18,$00         ;58
                db      $00,$18,$18,$00,$00,$18,$18,$08         ;59
                db      $00,$08,$10,$20,$40,$20,$10,$08         ;60
                db      $00,$00,$7C,$00,$7C,$00,$00,$00         ;61
                db      $00,$20,$10,$08,$04,$08,$10,$20         ;62
                db      $00,$38,$44,$04,$18,$10,$00,$10         ;63
                db      $00,$3C,$5A,$6A,$5A,$46,$3C,$00         ;64
                db      $00,$10,$28,$28,$7C,$44,$44,$00         ;65
                db      $00,$78,$44,$78,$44,$44,$78,$00         ;66
                db      $00,$38,$44,$40,$40,$44,$38,$00         ;67
                db      $00,$78,$44,$44,$44,$44,$78,$00         ;68
                db      $00,$7C,$40,$7C,$40,$40,$7C,$00         ;69
                db      $00,$7C,$40,$40,$78,$40,$40,$00         ;70
                db      $00,$38,$44,$40,$4E,$44,$38,$00         ;71
                db      $00,$44,$44,$7C,$44,$44,$44,$00         ;72
                db      $00,$38,$10,$10,$10,$10,$38,$00         ;73
                db      $00,$1C,$08,$08,$08,$48,$30,$00         ;74
                db      $00,$44,$48,$50,$68,$44,$44,$00         ;75
                db      $00,$40,$40,$40,$40,$40,$7C,$00         ;76
                db      $00,$44,$6C,$54,$44,$44,$44,$00         ;77
                db      $00,$44,$64,$54,$54,$4C,$44,$00         ;78
                db      $00,$38,$44,$44,$44,$44,$38,$00         ;79
                db      $00,$78,$44,$44,$78,$40,$40,$00         ;80
                db      $00,$38,$44,$44,$54,$4C,$3C,$06         ;81
                db      $00,$78,$44,$44,$78,$44,$44,$00         ;82
                db      $00,$3C,$40,$38,$04,$44,$38,$00         ;83
                db      $00,$7C,$10,$10,$10,$10,$10,$00         ;84
                db      $00,$44,$44,$44,$44,$44,$38,$00         ;85
                db      $00,$44,$44,$28,$28,$10,$10,$00         ;86
                db      $00,$54,$54,$54,$54,$28,$28,$00         ;87
                db      $00,$44,$28,$10,$10,$28,$44,$00         ;88
                db      $00,$44,$44,$28,$10,$10,$10,$00         ;89
                db      $00,$7C,$04,$08,$10,$20,$7C,$00         ;90
                db      $00,$38,$20,$20,$20,$20,$38,$00         ;91
                db      $00,$44,$28,$7C,$10,$7C,$10,$00         ;92
                db      $00,$38,$08,$08,$08,$08,$38,$00         ;93
                db      $00,$10,$28,$44,$00,$00,$00,$00         ;94
                db      $00,$00,$00,$00,$00,$00,$7C,$00         ;95
                db      $00,$30,$30,$10,$00,$00,$00,$00         ;96
                db      $00,$00,$38,$04,$3C,$44,$3C,$00         ;97
                db      $00,$40,$40,$78,$44,$44,$78,$00         ;98
                db      $00,$00,$38,$44,$40,$44,$38,$00         ;99
                db      $00,$04,$04,$3C,$44,$44,$3C,$00         ;100
                db      $00,$00,$38,$44,$7C,$40,$3C,$00         ;101
                db      $00,$08,$10,$38,$10,$10,$10,$00         ;102
                db      $00,$00,$3C,$44,$3C,$44,$38,$00         ;103
                db      $00,$40,$40,$78,$44,$44,$44,$00         ;104
                db      $00,$10,$00,$30,$10,$10,$38,$00         ;105
                db      $00,$10,$00,$10,$10,$10,$60,$00         ;106
                db      $00,$20,$24,$28,$30,$28,$24,$00         ;107
                db      $00,$10,$10,$10,$10,$10,$10,$00         ;108
                db      $00,$00,$78,$54,$54,$54,$54,$00         ;109
                db      $00,$00,$78,$44,$44,$44,$44,$00         ;110
                db      $00,$00,$38,$44,$44,$44,$38,$00         ;111
                db      $00,$00,$78,$44,$78,$40,$40,$00         ;112
                db      $00,$04,$3C,$44,$3C,$04,$04,$00         ;113
                db      $00,$00,$5C,$60,$40,$40,$40,$00         ;114
                db      $00,$00,$3C,$40,$38,$04,$78,$00         ;115
                db      $00,$20,$78,$20,$20,$20,$18,$00         ;116
                db      $00,$00,$48,$48,$48,$48,$34,$00         ;117
                db      $00,$00,$44,$44,$44,$28,$10,$00         ;118
                db      $00,$00,$54,$54,$54,$54,$28,$00         ;119
                db      $00,$00,$44,$28,$10,$28,$44,$00         ;120
                db      $00,$00,$44,$28,$10,$10,$20,$00         ;121
                db      $00,$00,$7C,$08,$10,$20,$7C,$00         ;122
                db      $00,$08,$10,$10,$20,$10,$10,$08         ;123
                db      $00,$10,$10,$10,$10,$10,$10,$00         ;124
                db      $00,$10,$08,$08,$04,$08,$08,$10         ;125
                db      $00,$32,$4C,$00,$00,$00,$00,$00         ;126
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;127
                db      $00,$18,$3C,$7E,$7E,$18,$3C,$00         ;128
                db      $00,$24,$7E,$7E,$7E,$3C,$18,$00         ;129
                db      $00,$18,$18,$7E,$7E,$18,$3C,$00         ;130
                db      $00,$18,$3C,$7E,$7E,$3C,$18,$00         ;131
                db      $00,$3C,$42,$42,$42,$42,$3C,$00         ;132
                db      $00,$3C,$7E,$7E,$7E,$7E,$3C,$00         ;133
                db      $00,$10,$7C,$20,$78,$54,$28,$3C         ;134
                db      $00,$00,$10,$7C,$10,$7C,$5C,$74         ;135
                db      $00,$00,$00,$00,$48,$44,$44,$24         ;136
                db      $00,$00,$20,$10,$78,$08,$08,$10         ;137
                db      $00,$00,$20,$10,$78,$08,$30,$48         ;138
                db      $00,$00,$10,$7C,$14,$78,$54,$34         ;139
                db      $00,$00,$00,$50,$F8,$54,$48,$20         ;140
                db      $00,$00,$00,$50,$7C,$54,$18,$20         ;141
                db      $00,$00,$00,$10,$1C,$30,$58,$34         ;142
                db      $00,$00,$00,$00,$78,$04,$04,$38         ;143
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;144
                db      $00,$20,$F8,$20,$7C,$AC,$B4,$74         ;145
                db      $00,$00,$88,$88,$84,$84,$A4,$40         ;146
                db      $00,$60,$00,$70,$88,$08,$10,$60         ;147
                db      $00,$60,$10,$F8,$20,$60,$50,$98         ;148
                db      $00,$24,$F4,$20,$78,$A4,$A4,$68         ;149
                db      $00,$48,$E4,$54,$50,$90,$10,$60         ;150
                db      $00,$40,$F8,$20,$F8,$10,$80,$70         ;151
                db      $00,$20,$20,$40,$80,$40,$20,$20         ;152
                db      $00,$88,$88,$BC,$88,$88,$88,$50         ;153
                db      $00,$00,$F8,$08,$00,$00,$80,$78         ;154
                db      $00,$20,$F8,$20,$20,$00,$80,$70         ;155
                db      $00,$80,$80,$80,$80,$80,$88,$70         ;156
                db      $00,$10,$FC,$30,$50,$30,$10,$60         ;157
                db      $00,$48,$FC,$48,$48,$48,$40,$38         ;158
                db      $00,$44,$28,$FC,$30,$40,$40,$38         ;159
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;160
                db      $00,$00,$00,$00,$08,$14,$08,$00         ;161
                db      $00,$60,$40,$40,$00,$00,$00,$00         ;162
                db      $00,$00,$00,$00,$04,$04,$0C,$00         ;163
                db      $00,$00,$00,$00,$10,$08,$08,$00         ;164
                db      $00,$00,$00,$30,$30,$00,$00,$00         ;165
                db      $00,$00,$00,$7C,$04,$7C,$04,$38         ;166
                db      $00,$00,$00,$7C,$04,$14,$10,$20         ;167
                db      $00,$00,$00,$08,$18,$70,$10,$10         ;168
                db      $00,$00,$00,$10,$7C,$44,$04,$18         ;169
                db      $00,$00,$00,$00,$38,$10,$10,$7C         ;170
                db      $00,$00,$00,$08,$7C,$18,$28,$48         ;171
                db      $00,$00,$00,$20,$7C,$24,$24,$20         ;172
                db      $00,$00,$00,$00,$38,$08,$08,$7C         ;173
                db      $00,$00,$00,$7C,$04,$7C,$04,$7C         ;174
                db      $00,$00,$00,$54,$54,$04,$04,$18         ;175
                db      $00,$00,$00,$40,$3C,$00,$00,$00         ;176
                db      $00,$7C,$04,$14,$14,$14,$20,$40         ;177
                db      $00,$04,$08,$18,$30,$50,$10,$10         ;178
                db      $00,$10,$7C,$44,$44,$04,$08,$10         ;179
                db      $00,$00,$7C,$10,$10,$10,$10,$7C         ;180
                db      $00,$08,$7C,$08,$18,$28,$48,$18         ;181
                db      $00,$10,$7C,$14,$24,$24,$44,$08         ;182
                db      $00,$10,$7C,$10,$7C,$10,$10,$10         ;183
                db      $00,$20,$3C,$24,$44,$08,$08,$30         ;184
                db      $00,$20,$3C,$28,$48,$08,$10,$20         ;185
                db      $00,$00,$7C,$04,$04,$04,$04,$7C         ;186
                db      $00,$28,$7C,$28,$28,$08,$08,$30         ;187
                db      $00,$64,$04,$64,$04,$04,$08,$70         ;188
                db      $00,$7C,$04,$04,$08,$10,$28,$44         ;189
                db      $00,$20,$7C,$24,$20,$20,$20,$1C         ;190
                db      $00,$44,$44,$44,$04,$04,$08,$30         ;191
                db      $00,$1C,$24,$24,$7C,$04,$08,$10         ;192
                db      $00,$04,$78,$10,$7C,$10,$10,$20         ;193
                db      $00,$54,$54,$54,$04,$04,$08,$30         ;194
                db      $00,$38,$00,$7C,$10,$10,$10,$20         ;195
                db      $00,$20,$20,$20,$38,$24,$20,$20         ;196
                db      $00,$10,$7C,$10,$10,$10,$10,$20         ;197
                db      $00,$00,$38,$00,$00,$00,$00,$7C         ;198
                db      $00,$7C,$04,$48,$30,$10,$28,$44         ;199
                db      $00,$10,$7C,$08,$38,$54,$54,$10         ;200
                db      $00,$08,$08,$08,$08,$10,$10,$60         ;201
                db      $00,$08,$08,$48,$44,$44,$44,$44         ;202
                db      $00,$40,$78,$40,$40,$40,$40,$3C         ;203
                db      $00,$00,$7C,$04,$04,$08,$08,$30         ;204
                db      $00,$00,$20,$50,$48,$04,$04,$00         ;205
                db      $00,$10,$7C,$10,$10,$54,$54,$10         ;206
                db      $00,$00,$7C,$04,$48,$30,$10,$08         ;207
                db      $00,$60,$1C,$60,$1C,$00,$60,$1C         ;208
                db      $00,$10,$20,$28,$48,$44,$7C,$04         ;209
                db      $00,$04,$04,$28,$10,$18,$24,$40         ;210
                db      $00,$7C,$20,$20,$7C,$20,$20,$1C         ;211
                db      $00,$20,$FC,$24,$24,$10,$10,$10         ;212
                db      $00,$78,$08,$08,$08,$08,$08,$7C         ;213
                db      $00,$7C,$04,$04,$7C,$04,$04,$7C         ;214
                db      $00,$38,$00,$7C,$04,$04,$04,$38         ;215
                db      $00,$48,$48,$48,$48,$08,$08,$30         ;216
                db      $00,$48,$48,$48,$48,$48,$48,$8C         ;217
                db      $00,$40,$40,$40,$40,$44,$48,$70         ;218
                db      $00,$7C,$44,$44,$44,$44,$44,$7C         ;219
                db      $00,$7C,$44,$44,$04,$04,$04,$38         ;220
                db      $00,$64,$04,$04,$04,$08,$08,$70         ;221
                db      $00,$00,$50,$28,$00,$00,$00,$00         ;222
                db      $00,$20,$50,$20,$00,$00,$00,$00         ;223
                db      $00,$20,$78,$20,$5C,$40,$40,$9C         ;224
                db      $00,$20,$7C,$20,$78,$84,$04,$18         ;225
                db      $00,$00,$38,$C4,$04,$04,$04,$78         ;226
                db      $00,$00,$3C,$C8,$10,$20,$20,$18         ;227
                db      $00,$84,$48,$30,$20,$40,$40,$3C         ;228
                db      $00,$88,$64,$60,$90,$38,$50,$30         ;229
                db      $00,$40,$5C,$80,$80,$80,$80,$5C         ;230
                db      $00,$80,$44,$78,$D8,$A8,$BC,$58         ;231
                db      $00,$48,$D8,$68,$48,$D8,$EC,$58         ;232
                db      $00,$00,$38,$54,$94,$A4,$A4,$68         ;233
                db      $00,$08,$BC,$88,$88,$B8,$AC,$B4         ;234
                db      $00,$00,$6C,$A8,$48,$48,$48,$30         ;235
                db      $00,$00,$70,$00,$20,$10,$94,$A4         ;236
                db      $00,$00,$00,$30,$48,$88,$84,$04         ;237
                db      $00,$00,$9C,$88,$BC,$98,$AC,$58         ;238
                db      $00,$20,$FC,$20,$FC,$70,$A8,$64         ;239
                db      $00,$00,$E0,$28,$78,$AC,$A8,$50         ;240
                db      $00,$28,$F4,$20,$60,$A4,$64,$38         ;241
                db      $00,$84,$74,$48,$B4,$94,$AC,$48         ;242
                db      $00,$20,$F8,$40,$FC,$40,$40,$38         ;243
                db      $00,$90,$5C,$74,$D4,$48,$40,$20         ;244
                db      $00,$90,$B8,$D4,$94,$1C,$10,$20         ;245
                db      $00,$20,$38,$20,$70,$A8,$A8,$60         ;246
                db      $00,$40,$20,$80,$B8,$C8,$08,$30         ;247
                db      $00,$90,$88,$88,$88,$08,$10,$20         ;248
                db      $00,$78,$10,$30,$C8,$38,$48,$30         ;249
                db      $00,$40,$E8,$58,$68,$48,$C8,$4C         ;250
                db      $00,$78,$08,$30,$48,$84,$04,$38         ;251
                db      $00,$40,$E0,$58,$64,$44,$C4,$58         ;252
                db      $00,$10,$10,$20,$30,$50,$68,$CC         ;253
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;254
                db      $00,$00,$00,$00,$00,$00,$00,$00         ;255

